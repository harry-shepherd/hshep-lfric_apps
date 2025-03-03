! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Calculates the total PM10 and PM2.5 concentrations and for each aerosol type

module calc_pm_diags_mod

use um_types, only: real_umphys

implicit none

! Global parameters available to other routines.
! Note that all variables whose name starts with 'denom_' are parameters
! related to the volume distribution and were calculated offline by the
! standalone program calc_pm_params.f90.
!
! Parameters for sulphate
! What is advected is sulphur. Therefore, it is necessary to convert m.m.r
! of sulphur to m.m.r of ammonium sulphate by multiplying by the ratio of
! their molecular weights: [(NH4)2]SO4 / S = 132 / 32 =  4.125
real(kind=real_umphys), parameter :: s_conv_fac      = 4.125
real(kind=real_umphys), parameter :: denom_su_ait    = 0.832154093803605184e-20
real(kind=real_umphys), parameter :: denom_su_acc    = 0.317222975403968212e-16
!
! Parameters for black carbon (soot)
real(kind=real_umphys), parameter :: denom_bc_fr     = 0.132771498349419138e-16
real(kind=real_umphys), parameter :: denom_bc_ag     = 0.132771498349419138e-16
!
! Parameters for OCFF
real(kind=real_umphys), parameter :: denom_ocff_fr   = 0.231243545609857872e-16
real(kind=real_umphys), parameter :: denom_ocff_ag   = 0.399588846813834411e-16
!
! Parameters for biogenic aerosol (SOA)
real(kind=real_umphys), parameter :: denom_soa       = 0.293507300762206618e-16

! Paremeter for unit conversion, from kg to micrograms
real(kind=real_umphys), parameter :: kg_to_micg        = 1.0e9

character(len=*), parameter, private :: ModuleName='CALC_PM_DIAGS_MOD'

contains

subroutine calc_pm_diags (                                                     &
  ! Arguments in
  ! Parallel variables
  off_x, off_y,                                                                &
  !   Model dimensions
  row_length, rows,                                                            &
  model_levels,                                                                &
  salt_dim1, salt_dim2, salt_dim3,                                             &
  !   Logicals in
  l_sulpc_so2, l_soot, l_biomass, l_ocff, l_use_biogenic, l_dust,              &
  l_nitrate, l_use_seasalt_pm,                                                 &
  !   Data fields in
  p_theta_levels, t,                                                           &
  so4_ait, so4_acc, soot_new, soot_agd, bmass_new, bmass_agd,                  &
  ocff_new, ocff_agd, biogenic, sea_salt_film, sea_salt_jet,                   &
  dust_1, dust_2, dust_3, dust_4, dust_5, dust_6, nitr_acc,                    &
  ! Arguments out
  pm10, pm2p5, pm10_so4, pm2p5_so4, pm10_bc, pm2p5_bc, pm10_bb, pm2p5_bb,      &
  pm10_ocff, pm2p5_ocff, pm10_soa, pm2p5_soa,  pm10_ss, pm2p5_ss,              &
  conc_dust, conc_dust_surf, pm10_dust, pm2p5_dust, pm10_nitr, pm2p5_nitr)

use planet_constants_mod, only: r
use dust_parameters_mod, only: l_twobin_dust, pwsdiag_sfc_em

use yomhook, only: lhook, dr_hook
use parkind1, only: jprb, jpim
use pws_diags_mod, only: pws_dustmmr1_em, pws_dustmmr2_em,                     &
                         pws_dustmmr3_em, pws_dustmmr4_em,                     &
                         pws_dustmmr5_em, pws_dustmmr6_em,                     &
                         flag_dustmmr_em

implicit none


!--------------------------------------------------------------------------
! Description :
! Calculates the PM10 and PM2.5 mass concentrations (in microgram/m3)
! Method :
!     Calculation of the PM10 and PM2.5 mass concentrations (in
!     microgram/m3) by using the cumulative volume (from 0 to
!     cut-off diameter) of a log-normal, with median radius
!     (or gemetric mean radius) r_bar, geometric standard deviation
!     sigma and total number Ntot.
!     The contributions by the different aerosol species and modes
!     are added up to get the total PM10 & PM2.5 concs (microgram/m3).
!
!     Formulation derived from eqs. (7.51) & (7.51a) of Seinfeld
!     and Pandis (1998) using method of (7.39). Also, checked with
!     "The Mechanics of Inhaled Pharmaceutical Aerosols: An
!     Introduction", by Warren A. Finlay, Academic Press, 2001
!
!     (1) Cumulative_volume for particles with diameter between
!     0 and d_cut (assuming spherical particles):
!            V(d_cut) = (A*Ntot/2.0) + (A*Ntot/2.0) *
!                   erf((alog(d_cut)-B)/(sqrt(2.0)*alog(sigma)))
!     with
!            A = (pi/6.0)*exp(3.0*alog(dbar)+4.5*(alog(sigma))**2)
!            B = alog (volume median diameter) =
!              = alog(d_bar)+3.0*(alog(sigma))**2
!     See also note (3) for the calculation of Ntot

!     (2) Known V(d_cut) in m3, the mass concentration (ug / m3) of
!         particles with diameter lower than d_cut is:
!            V(d_cut) * rho_particle * 1e9
!            (need to multiply by 1e9 because rho is in kg/m3)

!     (3) Sea salt diagnostics are aerosol number (N) while the
!     diagnostics for the other aerosol species are m.m.r.
!     In those cases one can calculate Ntot with the relationship:
!
!                            rho_air           1
!      N (m-3) = m.m.r. * ------------  * -------------, with
!                         rho_particle     V_particle

!      V_particle = (4*PI/3) * (r_bar**3) * exp (4.5*(ln (sigma))**2),
!      because 2*r_bar is the geometric diameter

!                                          3 * rho_air
!      We finally use:  N (m-3) = m.m.r. * -------------
!                                            denom
!                      (where denom = 3*rho_particle*V_particle)

!     (4) The size distribution of mineral dust is currently not lognormal.
!         Six size bins are used instead, covering
!         3.162e-8 to 1.000e-7 m (bin 1), 1.000e-8 to 3.162e-7 m (bin 2),
!         3.162e-7 to 1.000e-6 m (bin 3), 1.000e-6 to 3.162e-6 m (bin 4),
!         3.162e-6 to 1.000e-5 (bin 5), and 1.000e-5 to 3.162e-5 m (bin 6).
!         Consequently, PM2.5 concentrations are derived from m.m.r. of
!         bins 1 to 3 and 19.3941% of bin 4, and PM10 concentrations are
!         derived from m.m.r. of bins 1 to 4 and 39.8317% of bin 5:
!            m.m.r. * rho_air (kg/m3) * 1e9 --> ug / m3
!         We also calculate a total concentration by using the total
!         m.m.r. across all 6 bins.

!     (5) All contributions are finally added to get the total
!         PM10 and PM2.5 mass concentrations

! Limitations:
!      This code currently ignores hygroscopic growth of aerosol

!  Calculation of total concentration is also possible, but
!  much simpler since no cut-off diameter is required. This is
!  currently only done for mineral dust.
!
!  Called by Aero_Ctl

! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: Aerosols

! Code Description:
!   Language:  Fortran 90
!   This code is written to UMDPi 003 programming standards.
!--------------------------------------------------------------------------

! Model dimensions
integer, intent(in) :: off_x      ! Size of small halo in i
integer, intent(in) :: off_y      ! Size of small halo in j.
integer, intent(in) :: row_length
integer, intent(in) :: rows
integer, intent(in) :: model_levels
!dimensions of seasalt array
integer, intent(in) :: salt_dim1
integer, intent(in) :: salt_dim2
integer, intent(in) :: salt_dim3
!
logical, intent(in) :: l_sulpc_so2            !T if Sulphur Cycle required
logical, intent(in) :: l_soot                 !T if SOOT modelling required
logical, intent(in) :: l_biomass              !T if biomass modelling reqd
logical, intent(in) :: l_ocff                 !T if OCFF modelling required
logical, intent(in) :: l_use_biogenic         !T if clim. of biog. aeros. used
logical, intent(in) :: l_dust                 !T if mineral dust used
logical, intent(in) :: l_nitrate              !T if nitrate modelling required
logical, intent(in) :: l_use_seasalt_pm       !T if want to include SS in PM

! Data fields in
! pressure on theta levels
real(kind=real_umphys), intent(in) ::                                          &
                    p_theta_levels(1-off_x:row_length+off_x,                   &
                                   1-off_y:rows+off_y, model_levels)
! Temp (K) on theta levels
real(kind=real_umphys), intent(in) ::  t(row_length,rows,model_levels)

! mmr S in AITKEN and ACCUMULATION modes
real(kind=real_umphys), intent(in) ::                                          &
                    so4_ait(1-off_x:row_length+off_x, 1-off_y:rows+off_y,      &
                               model_levels)
real(kind=real_umphys), intent(in) ::                                          &
                    so4_acc(1-off_x:row_length+off_x, 1-off_y:rows+off_y,      &
                            model_levels)

! mmr fresh and aged soot
real(kind=real_umphys), intent(in) ::                                          &
                    soot_new(1-off_x:row_length+off_x, 1-off_y:rows+off_y,     &
                                model_levels)
real(kind=real_umphys), intent(in) ::                                          &
                    soot_agd(1-off_x:row_length+off_x, 1-off_y:rows+off_y,     &
                                model_levels)

!mmr fresh and aged smoke
real(kind=real_umphys), intent(in) ::                                          &
                       bmass_new(1-off_x:row_length+off_x, 1-off_y:rows+off_y, &
                                 model_levels)
real(kind=real_umphys), intent(in) ::                                          &
                       bmass_agd(1-off_x:row_length+off_x, 1-off_y:rows+off_y, &
                                 model_levels)

! mmr fresh and aged OCFF
real(kind=real_umphys), intent(in) ::                                          &
                       ocff_new(1-off_x:row_length+off_x, 1-off_y:rows+off_y,  &
                                model_levels)
real(kind=real_umphys), intent(in) ::                                          &
                       ocff_agd(1-off_x:row_length+off_x, 1-off_y:rows+off_y,  &
                                model_levels)
! mmr biogenics
real(kind=real_umphys), intent(in) ::                                          &
                       biogenic(row_length, rows, model_levels)

! Sea-salt aerosol number densities
real(kind=real_umphys), intent(in) ::                                          &
                       sea_salt_film(salt_dim1,salt_dim2,salt_dim3)
real(kind=real_umphys), intent(in) ::                                          &
                       sea_salt_jet (salt_dim1,salt_dim2,salt_dim3)

! mmr dust in 5 divisions
real(kind=real_umphys), intent(in) ::                                          &
                       dust_1(1-off_x:row_length+off_x, 1-off_y:rows+off_y,    &
                              model_levels)
real(kind=real_umphys), intent(in) ::                                          &
                       dust_2(1-off_x:row_length+off_x, 1-off_y:rows+off_y,    &
                              model_levels)
real(kind=real_umphys), intent(in) ::                                          &
                       dust_3(1-off_x:row_length+off_x, 1-off_y:rows+off_y,    &
                              model_levels)
real(kind=real_umphys), intent(in) ::                                          &
                       dust_4(1-off_x:row_length+off_x, 1-off_y:rows+off_y,    &
                              model_levels)
real(kind=real_umphys), intent(in) ::                                          &
                       dust_5(1-off_x:row_length+off_x, 1-off_y:rows+off_y,    &
                              model_levels)
real(kind=real_umphys), intent(in) ::                                          &
                       dust_6(1-off_x:row_length+off_x, 1-off_y:rows+off_y,    &
                              model_levels)

! mmr Nitrate in ACC
real(kind=real_umphys), intent(in) ::                                          &
                       nitr_acc(1-off_x:row_length+off_x, 1-off_y:rows+off_y,  &
                                model_levels)

! Output arguments
!PM10  (micrograms m-3)
real(kind=real_umphys), intent(out) ::                                         &
                     pm10       (row_length, rows, model_levels)
!PM2.5 (micrograms m-3)
real(kind=real_umphys), intent(out) ::                                         &
                     pm2p5      (row_length, rows, model_levels)
!PM concentrations due to sulphate
real(kind=real_umphys), intent(out) ::                                         &
                     pm10_so4   (row_length, rows, model_levels)
real(kind=real_umphys), intent(out) ::                                         &
                     pm2p5_so4  (row_length, rows, model_levels)
!PM concs. due to black carbon
real(kind=real_umphys), intent(out) ::                                         &
                     pm10_bc    (row_length, rows, model_levels)
real(kind=real_umphys), intent(out) ::                                         &
                     pm2p5_bc   (row_length, rows, model_levels)
!PM concs. due to biomass burning aerosol
real(kind=real_umphys), intent(out) ::                                         &
                     pm10_bb    (row_length, rows, model_levels)
real(kind=real_umphys), intent(out) ::                                         &
                     pm2p5_bb   (row_length, rows, model_levels)
!PM concs. due to OCFF
real(kind=real_umphys), intent(out) ::                                         &
                     pm10_ocff  (row_length, rows, model_levels)
real(kind=real_umphys), intent(out) ::                                         &
                     pm2p5_ocff (row_length, rows, model_levels)
!PM concs. due to SOA
real(kind=real_umphys), intent(out) ::                                         &
                     pm10_soa   (row_length, rows, model_levels)
real(kind=real_umphys), intent(out) ::                                         &
                     pm2p5_soa  (row_length, rows, model_levels)
!PM concs. due to sea-salt aerosol
real(kind=real_umphys), intent(out) ::                                         &
                     pm10_ss   (row_length, rows, model_levels)
real(kind=real_umphys), intent(out) ::                                         &
                     pm2p5_ss  (row_length, rows, model_levels)
! concs. due to mineral dust
real(kind=real_umphys), intent(out) ::                                         &
                     conc_dust (row_length, rows, model_levels)
real(kind=real_umphys), intent(out) :: conc_dust_surf (row_length, rows)
real(kind=real_umphys), intent(out) ::                                         &
                     pm10_dust (row_length, rows, model_levels)
real(kind=real_umphys), intent(out) ::                                         &
                     pm2p5_dust(row_length, rows, model_levels)
!PM concs. due to nitrate
real(kind=real_umphys), intent(out) ::                                         &
                     pm10_nitr (row_length, rows, model_levels)
real(kind=real_umphys), intent(out) ::                                         &
                     pm2p5_nitr(row_length, rows, model_levels)


! Local parameters for PM10 and PM2.5 calculations
! Parameters related to the volume distribution were
! calculated offline by the program calc_pm_params.f90
! All densities are in kg/m3
! Parameters for sulphate
real(kind=real_umphys), parameter :: rho_su          = 1769.0     ! density
real(kind=real_umphys), parameter :: a_param_su_ait  = 0.156803107933598664e-23
real(kind=real_umphys), parameter :: erf_su_ait_d1   = 1.00000000000000000
real(kind=real_umphys), parameter :: erf_su_ait_d2   = 1.00000000000000000
real(kind=real_umphys), parameter :: a_param_su_acc  = 0.597744442065136312e-20
real(kind=real_umphys), parameter :: erf_su_acc_d1   = 1.00000000000000000
real(kind=real_umphys), parameter :: erf_su_acc_d2   = 0.999999999970596409
! Parameters for Black Carbon (soot)
real(kind=real_umphys), parameter :: rho_bc          = 1900.0     ! density
real(kind=real_umphys), parameter :: a_param_bc_fr   = 0.232932453244595951e-20
real(kind=real_umphys), parameter :: erf_bc_fr_d1    = 0.999998972736938496
real(kind=real_umphys), parameter :: erf_bc_fr_d2    = 0.996102525359877422
real(kind=real_umphys), parameter :: a_param_bc_ag   = 0.232932453244595951e-20
real(kind=real_umphys), parameter :: erf_bc_ag_d1    = 0.999998972736938496
real(kind=real_umphys), parameter :: erf_bc_ag_d2    = 0.996102525359877422
! Parameters for biomass burning aerosol
real(kind=real_umphys), parameter :: rho_bb          = 1350.0     ! density
real(kind=real_umphys), parameter :: denom_bb_fr     = 0.231243545609857872e-16
real(kind=real_umphys), parameter :: a_param_bb_fr   = 0.570971717555206423e-20
real(kind=real_umphys), parameter :: erf_bb_fr_d1    = 1.00000000000000000
real(kind=real_umphys), parameter :: erf_bb_fr_d2    = 1.00000000000000000
real(kind=real_umphys), parameter :: denom_bb_ag     = 0.399588846813834411e-16
real(kind=real_umphys), parameter :: a_param_bb_ag   = 0.986639127935394024e-20
real(kind=real_umphys), parameter :: erf_bb_ag_d1    = 1.00000000000000000
real(kind=real_umphys), parameter :: erf_bb_ag_d2    = 0.999999999999999667
! Parameters for OCFF
real(kind=real_umphys), parameter :: rho_ocff        = 1350.0     ! density
real(kind=real_umphys), parameter :: a_param_ocff_fr = 0.570971717555206423e-20
real(kind=real_umphys), parameter :: erf_ocff_fr_d1  = 1.00000000000000000
real(kind=real_umphys), parameter :: erf_ocff_fr_d2  = 1.00000000000000000
real(kind=real_umphys), parameter :: a_param_ocff_ag = 0.986639127935394024e-20
real(kind=real_umphys), parameter :: erf_ocff_ag_d1  = 1.00000000000000000
real(kind=real_umphys), parameter :: erf_ocff_ag_d2  = 0.999999999999999667
! Parameters for biogenic aerosol (SOA)
real(kind=real_umphys), parameter :: rho_soa         = 1300.0     ! density
real(kind=real_umphys), parameter :: a_param_soa     = 0.752582822467193671e-20
real(kind=real_umphys), parameter :: erf_soa_d1      = 1.00000000000000000
real(kind=real_umphys), parameter :: erf_soa_d2      = 0.999999724269635237
! Parameters for sea-salt
real(kind=real_umphys), parameter :: rho_ss          = 2165.0     ! density
real(kind=real_umphys), parameter :: a_param_ss_fi   = 0.267438839195005737e-19
real(kind=real_umphys), parameter :: erf_ss_fi_d1    = 0.999969448922318982
real(kind=real_umphys), parameter :: erf_ss_fi_d2    = 0.955514879308947629
real(kind=real_umphys), parameter :: a_param_ss_je   = 0.363956958194679524e-16
real(kind=real_umphys), parameter :: erf_ss_je_d1    = 0.191596825012604527
real(kind=real_umphys), parameter :: erf_ss_je_d2    = -0.921169668112190365
! Parameters for nitrate
real(kind=real_umphys), parameter :: rho_ni          = 1725.0     ! density
!What is advected is nitrogen. Therefore,
!necessary to convert m.m.r. of N to ammonium
!nitrate by multiplying by ratio of molecular weights:
!NH4NO3 / N =  80 / 14 =  5.714
real(kind=real_umphys), parameter :: n_conv_fac      = 5.714
real(kind=real_umphys), parameter :: denom_ni_acc    = 0.309332748768708366e-16
real(kind=real_umphys), parameter :: a_param_ni_acc  = 0.597744442065136312e-20
real(kind=real_umphys), parameter :: erf_ni_acc_d1   = 1.00000000000000000
real(kind=real_umphys), parameter :: erf_ni_acc_d2   = 0.999999999970596409
! Parameters for six-bin dust
real(kind=real_umphys), parameter :: dust5_pm10_frac  = 0.398317
                                               ! Fraction of bin 5 <10 microns
real(kind=real_umphys), parameter :: dust4_pm2p5_frac = 0.193941
                                               ! Fraction of bin 4 <2.5 microns
! Parameters for two-bin dust
real(kind=real_umphys), parameter :: dust1_pm2p5_frac_twobin = 0.405559
                                               ! Fraction of bin 1 < 2.5 microns
real(kind=real_umphys), parameter :: dust2_pm10_frac_twobin  = 0.507086
                                               ! Fraction of bin 2 < 10 microns

real(kind=real_umphys) :: rho_air    ! air density
real(kind=real_umphys) :: n_tot      ! total nr. aerosol conc.
real(kind=real_umphys) :: dust_sum_mmr ! working sum of must MMRs

integer :: i, j         ! loop counters (horizontal field indices)
integer :: k            ! loop counter  (vertical index)
character (len=* ), parameter :: RoutineName='CALC_PM_DIAGS'

integer(kind=jpim), parameter :: zhook_in  = 0
integer(kind=jpim), parameter :: zhook_out = 1
real(kind=jprb)               :: zhook_handle

if (lhook) call dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! Add contributions from the different aerosol species and modes
! to calculate pm10 & pm2.5 concentrations

!initialise pm10 & pm2.5 concentrations
!$OMP PARALLEL DEFAULT(SHARED) private(i,j,k,n_tot,rho_air)

!$OMP do SCHEDULE(STATIC)
do k = 1, model_levels
  do j = 1, rows
    do i = 1, row_length
      pm10       (i,j,k) = 0.0
      pm2p5      (i,j,k) = 0.0
      pm10_so4   (i,j,k) = 0.0
      pm2p5_so4  (i,j,k) = 0.0
      pm10_bc    (i,j,k) = 0.0
      pm2p5_bc   (i,j,k) = 0.0
      pm10_bb    (i,j,k) = 0.0
      pm2p5_bb   (i,j,k) = 0.0
      pm10_ocff  (i,j,k) = 0.0
      pm2p5_ocff (i,j,k) = 0.0
      pm10_soa   (i,j,k) = 0.0
      pm2p5_soa  (i,j,k) = 0.0
      pm10_ss    (i,j,k) = 0.0
      pm2p5_ss   (i,j,k) = 0.0
      pm10_dust  (i,j,k) = 0.0
      conc_dust  (i,j,k) = 0.0
      pm2p5_dust (i,j,k) = 0.0
      pm10_nitr  (i,j,k) = 0.0
      pm2p5_nitr (i,j,k) = 0.0
    end do
  end do
end do
!$OMP end do

!$OMP do SCHEDULE(STATIC)
do k = 1, model_levels
  do j = 1, rows
    do i = 1, row_length

      rho_air = p_theta_levels(i, j, k)/(r * t(i, j, k))

      if (l_sulpc_so2) then
        ! includes conversion of m.m.r. of sulphur to ammonium
        n_tot = 3.0*(so4_ait(i, j, k)*s_conv_fac)*rho_air / denom_su_ait

        pm10_so4(i, j, k) =                                                    &
          ((a_param_su_ait * n_tot / 2.0)                                      &
          +(a_param_su_ait * n_tot / 2.0)                                      &
          * erf_su_ait_d1) * rho_su * kg_to_micg

        pm2p5_so4(i, j, k) =                                                   &
          ((a_param_su_ait * n_tot/2.0)                                        &
          +(a_param_su_ait * n_tot/2.0)                                        &
          * erf_su_ait_d2) * rho_su * kg_to_micg

        n_tot = 3.0*(so4_acc(i, j, k)*s_conv_fac)*rho_air / denom_su_acc

        pm10_so4(i, j, k) = pm10_so4(i, j, k) +                                &
          ((a_param_su_acc * n_tot / 2.0)                                      &
          +(a_param_su_acc * n_tot / 2.0)                                      &
          * erf_su_acc_d1) * rho_su * kg_to_micg

        pm2p5_so4(i, j, k) = pm2p5_so4(i, j, k) +                              &
          ((a_param_su_acc * n_tot/2.0)                                        &
          +(a_param_su_acc * n_tot/2.0)                                        &
          * erf_su_acc_d2) * rho_su * kg_to_micg

        pm10  (i, j, k) = pm10_so4 (i, j, k)
        pm2p5 (i, j, k) = pm2p5_so4(i, j, k)
      end if


      if (l_soot) then
        n_tot = 3.0 * soot_new(i, j, k) * rho_air / denom_bc_fr

        pm10_bc(i, j, k) =                                                     &
          ((a_param_bc_fr * n_tot / 2.0)                                       &
          +(a_param_bc_fr * n_tot / 2.0)                                       &
          * erf_bc_fr_d1) * rho_bc * kg_to_micg

        pm2p5_bc(i, j, k) =                                                    &
          ((a_param_bc_fr * n_tot / 2.0)                                       &
          +(a_param_bc_fr * n_tot / 2.0)                                       &
          * erf_bc_fr_d2) * rho_bc * kg_to_micg

        n_tot = 3.0 * soot_agd (i, j, k) * rho_air / denom_bc_ag

        pm10_bc(i, j, k) =  pm10_bc(i, j, k) +                                 &
          ((a_param_bc_ag * n_tot / 2.0)                                       &
          +(a_param_bc_ag * n_tot / 2.0)                                       &
          * erf_bc_ag_d1) * rho_bc * kg_to_micg

        pm2p5_bc(i, j, k) = pm2p5_bc(i, j, k) +                                &
          ((a_param_bc_ag * n_tot / 2.0)                                       &
          +(a_param_bc_ag * n_tot / 2.0)                                       &
          * erf_bc_ag_d2) * rho_bc * kg_to_micg

        pm10  (i, j, k) = pm10  (i, j, k) + pm10_bc (i, j, k)
        pm2p5 (i, j, k) = pm2p5 (i, j, k) + pm2p5_bc(i, j, k)
      end if


      if (l_biomass) then
        n_tot = 3.0 * bmass_new(i, j, k) * rho_air / denom_bb_fr

        pm10_bb(i, j, k) =                                                     &
          ((a_param_bb_fr * n_tot / 2.0)                                       &
          +(a_param_bb_fr * n_tot / 2.0)                                       &
          * erf_bb_fr_d1) * rho_bb * kg_to_micg

        pm2p5_bb(i, j, k) =                                                    &
          ((a_param_bb_fr * n_tot / 2.0)                                       &
          +(a_param_bb_fr * n_tot / 2.0)                                       &
          * erf_bb_fr_d2) * rho_bb * kg_to_micg

        n_tot = 3.0 * bmass_agd(i, j, k) * rho_air / denom_bb_ag

        pm10_bb(i, j, k) =  pm10_bb(i, j, k) +                                 &
          ((a_param_bb_ag * n_tot / 2.0)                                       &
          +(a_param_bb_ag * n_tot / 2.0)                                       &
          * erf_bb_ag_d1) * rho_bb * kg_to_micg

        pm2p5_bb(i, j, k) = pm2p5_bb(i, j, k) +                                &
          ((a_param_bb_ag * n_tot / 2.0)                                       &
          +(a_param_bb_ag * n_tot / 2.0)                                       &
          * erf_bb_ag_d2) * rho_bb * kg_to_micg

        pm10  (i, j, k) = pm10  (i, j, k) + pm10_bb (i, j, k)
        pm2p5 (i, j, k) = pm2p5 (i, j, k) + pm2p5_bb(i, j, k)
      end if


      if (l_ocff) then
        n_tot = 3.0 * ocff_new(i, j, k) * rho_air / denom_ocff_fr

        pm10_ocff(i, j, k) =                                                   &
          ((a_param_ocff_fr * n_tot / 2.0)                                     &
          +(a_param_ocff_fr * n_tot / 2.0)                                     &
          * erf_ocff_fr_d1) * rho_ocff * kg_to_micg

        pm2p5_ocff(i, j, k) =                                                  &
          ((a_param_ocff_fr * n_tot/2.0)                                       &
          +(a_param_ocff_fr * n_tot/2.0)                                       &
          * erf_ocff_fr_d2) * rho_ocff * kg_to_micg

        n_tot = 3.0 * ocff_agd(i, j, k) * rho_air / denom_ocff_ag

        pm10_ocff(i, j, k) =  pm10_ocff(i, j, k) +                             &
          ((a_param_ocff_ag * n_tot / 2.0)                                     &
          +(a_param_ocff_ag * n_tot / 2.0)                                     &
          * erf_ocff_ag_d1) * rho_ocff * kg_to_micg

        pm2p5_ocff(i, j, k) = pm2p5_ocff(i, j, k) +                            &
          ((a_param_ocff_ag * n_tot/2.0)                                       &
          +(a_param_ocff_ag * n_tot/2.0)                                       &
          * erf_ocff_ag_d2) * rho_ocff * kg_to_micg

        pm10  (i, j, k) = pm10  (i, j, k) + pm10_ocff (i, j, k)
        pm2p5 (i, j, k) = pm2p5 (i, j, k) + pm2p5_ocff(i, j, k)
      end if

      if (l_use_biogenic) then
        n_tot = 3.0 * biogenic(i, j, k) * rho_air / denom_soa

        pm10_soa(i, j, k) =                                                    &
          ((a_param_soa * n_tot / 2.0)                                         &
          +(a_param_soa * n_tot / 2.0)                                         &
          * erf_soa_d1) * rho_soa * kg_to_micg

        pm2p5_soa(i, j, k) =                                                   &
          ((a_param_soa * n_tot / 2.0)                                         &
          +(a_param_soa * n_tot / 2.0)                                         &
          * erf_soa_d2) * rho_soa * kg_to_micg

        pm10  (i, j, k) = pm10  (i, j, k) + pm10_soa (i, j, k)
        pm2p5 (i, j, k) = pm2p5 (i, j, k) + pm2p5_soa(i, j, k)
      end if

      if (l_use_seasalt_pm) then
        n_tot = sea_salt_film(i, j, k)

        pm10_ss(i, j, k) =                                                     &
          ((a_param_ss_fi * n_tot / 2.0)                                       &
          +(a_param_ss_fi * n_tot / 2.0)                                       &
          * erf_ss_fi_d1) * rho_ss * kg_to_micg

        pm2p5_ss(i, j, k) =                                                    &
          ((a_param_ss_fi * n_tot / 2.0)                                       &
          +(a_param_ss_fi * n_tot / 2.0)                                       &
          * erf_ss_fi_d2) * rho_ss * kg_to_micg

        n_tot = sea_salt_jet(i, j, k)

        pm10_ss(i, j, k) =  pm10_ss(i, j, k) +                                 &
          ((a_param_ss_je * n_tot / 2.0)                                       &
          +(a_param_ss_je * n_tot / 2.0)                                       &
          * erf_ss_je_d1) * rho_ss * kg_to_micg

        pm2p5_ss(i, j, k) = pm2p5_ss(i, j, k) +                                &
          ((a_param_ss_je * n_tot / 2.0)                                       &
          +(a_param_ss_je * n_tot / 2.0)                                       &
          * erf_ss_je_d2) * rho_ss * kg_to_micg

        pm10  (i, j, k) = pm10  (i, j, k) + pm10_ss (i, j, k)
        pm2p5 (i, j, k) = pm2p5 (i, j, k) + pm2p5_ss(i, j, k)
      end if

      if (l_dust) then
        if (l_twobin_dust) then   ! two-bin dust
          conc_dust(i, j, k) =                                                 &
             ((dust_1(i, j, k)                                                 &
             + dust_2(i, j, k))                                                &
             * rho_air * kg_to_micg)

          pm10_dust(i, j, k) =                                                 &
             ((dust_1(i, j, k)                                                 &
             + (dust2_pm10_frac_twobin * dust_2(i, j, k)))                     &
             * rho_air * kg_to_micg)

          pm2p5_dust(i,j, k) =                                                 &
             ((dust1_pm2p5_frac_twobin  * dust_1(i, j, k))                     &
             * rho_air * kg_to_micg)
        else ! Six-bin dust
          conc_dust(i, j, k) =                                                 &
             ((dust_1(i, j, k)                                                 &
             + dust_2(i, j, k)                                                 &
             + dust_3(i, j, k)                                                 &
             + dust_4(i, j, k)                                                 &
             + dust_5(i, j, k)                                                 &
             + dust_6(i, j, k))                                                &
             * rho_air * kg_to_micg)

          pm10_dust(i, j, k) =                                                 &
             ((dust_1(i, j, k)                                                 &
             + dust_2(i, j, k)                                                 &
             + dust_3(i, j, k)                                                 &
             + dust_4(i, j, k)                                                 &
             + (dust5_pm10_frac * dust_5(i, j, k)))                            &
             * rho_air * kg_to_micg)

          pm2p5_dust(i, j, k) =                                                &
            ((dust_1(i, j, k)                                                  &
            + dust_2(i, j, k)                                                  &
            + dust_3(i, j, k)                                                  &
            + (dust4_pm2p5_frac * dust_4(i, j, k)))                            &
            * rho_air * kg_to_micg)

        end if ! l_twobin_dust
        pm10  (i, j, k) = pm10  (i, j, k) + pm10_dust (i, j, k)
        pm2p5 (i, j, k) = pm2p5 (i, j, k) + pm2p5_dust(i, j, k)

        if (k == 1) then
          ! surface dust concentration diagnostics:
          if (flag_dustmmr_em) then
            ! the surface concentration is a weighted average
            ! of the end of timestep model-level concentration, and an estimate
            ! after emission:
            if (l_twobin_dust) then   ! two-bin dust
              dust_sum_mmr = ((pws_dustmmr1_em(i,j) + pws_dustmmr2_em(i,j)) *  &
                              pwsdiag_sfc_em) +                                &
                             ((dust_1(i, j, k) + dust_2(i, j, k)) *            &
                              (1.0 - pwsdiag_sfc_em))
            else ! Six-bin dust
              dust_sum_mmr = ((pws_dustmmr1_em(i,j) + pws_dustmmr2_em(i,j) +   &
                               pws_dustmmr3_em(i,j) + pws_dustmmr4_em(i,j) +   &
                               pws_dustmmr5_em(i,j) + pws_dustmmr6_em(i,j) ) * &
                              pwsdiag_sfc_em) +                                &
                             ((dust_1(i, j, k) + dust_2(i, j, k) +             &
                               dust_3(i, j, k) + dust_4(i, j, k) +             &
                               dust_5(i, j, k) + dust_6(i, j, k)) *            &
                              (1.0 - pwsdiag_sfc_em))

            end if
            conc_dust_surf(i,j) = dust_sum_mmr * rho_air * kg_to_micg
          else
            ! surface dust conc is the same as the model level diagnostic:
            conc_dust_surf(i,j) = conc_dust(i,j,1)
          end if
        end if
      end if

      if (l_nitrate) then
        !includes  conversion of  m.m.r. of N to ammonium nitrate
        n_tot = 3.0*(nitr_acc(i, j, k)*n_conv_fac)*rho_air / denom_ni_acc

        pm10_nitr(i, j, k) =                                                   &
          ((a_param_ni_acc * n_tot / 2.0)                                      &
          +(a_param_ni_acc * n_tot / 2.0)                                      &
          * erf_ni_acc_d1) * rho_ni * kg_to_micg

        pm2p5_nitr(i, j, k) =                                                  &
          ((a_param_ni_acc * n_tot/2.0)                                        &
          +(a_param_ni_acc * n_tot/2.0)                                        &
          * erf_ni_acc_d2) * rho_ni * kg_to_micg

        pm10  (i, j, k) = pm10  (i, j, k) + pm10_nitr (i, j, k)
        pm2p5 (i, j, k) = pm2p5 (i, j, k) + pm2p5_nitr(i, j, k)
      end if

    end do
  end do
end do
!$OMP end do
!$OMP end PARALLEL

if (lhook) call dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
return
end subroutine calc_pm_diags

end module calc_pm_diags_mod

