! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Declares variables and allocatable arrays which are local to atm_step
!  (and below)
!
!  Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: top_level

module atm_step_local

use def_ukca_radaer_presc, only: t_ukca_radaer_presc
use def_easyaerosol, only: t_easyaerosol_rad, t_easyaerosol_cdnc
use um_types, only: real_umphys

implicit none

logical :: l_do_rims

integer :: cycleno ! Number of cycles (iterations) for iterative SISL

integer :: tr_size ! size of a single tracer - FLUME variable

logical :: l_tracer ! T if *any* tracer variables present

logical :: l_print_l2norms = .false. ! diagnostic printing of l2norms

logical :: l_alloc_wvar  ! Switch turns on local allocation of bl_w_var

integer :: first_constant_r_rho_level    ! 1st rho level with r constant
integer :: first_constant_r_rho_level_m1 ! Max (1,first_constant_r_rho_level)

integer :: i_start
integer :: i_stop
integer :: j_start
integer :: j_stop
integer :: j_begin
integer :: j_end

integer :: lambda_start ! pointer for start of lambda_p/lambda_u on this pe

! used in interpolation code - no. of levels to check for whether the departure
! point lies inside the orography.
integer :: check_bottom_levels

integer :: rhc_row_length
integer :: rhc_rows

!  Dummy variables for SCM Diagnostics,
!  for passing down to atmos_physics1 and 2 routines
integer, parameter :: nscmdpkgs = 15
logical :: l_scmdiags(nscmdpkgs)
logical :: l_flux_bc    ! T if prescribed surface fluxes to be used

logical, parameter :: set_halos = .true.
logical, parameter :: do_not_set_halos = .false.
                   ! logicals used to set up the calls to tr_set_phys.

logical, parameter :: couple_app=.true.   ! this parameter can be used
                   ! to force a dynamics only run even with the full physics
                   ! suite executing. For testing purposes. If false it will
                   ! zero all source terms coming from atmos_physics1 &
                   ! atmos_physics2 (as far as dynamics is concerned - mostly).
                   ! It could be set to l_physics, however that would exclude
                   ! the eg_idl_forcing contribution which is enabled by
                   ! disabling physics in the namelist and setting the
                   ! appropriate IDEALISED namelist logical.

real :: max_q_star, min_q_star    ! local diagnostics

real :: increment_factor ! For calculating value of LBC at next TL
real :: solver_tolerance

! Variables required for call to SET_LATERAL_BOUNDARIES
integer :: lbc_size        ! size of a single level of LBC
logical :: l_do_halos      ! update the halos?
logical :: l_do_boundaries ! update the boundaries?

! -------------------------------------------------

integer :: nd_o3  ! Total size of ozone array supplied

! Code to do with tropopause diagnostics from O3_to_3D
! Declare logicals for Stash Diagnostics used in call to O3_to_3D
logical :: l_o3_trop_level   ! stash code 2,280
logical :: l_o3_trop_height  ! stash code 2,281
logical :: l_t_trop_level    ! stash code 2,282
logical :: l_t_trop_height   ! stash code 2,283

real, pointer :: resp_s_acc_gb_um(:,:) => null()
! (target varies according to l_TRIFFID)

integer :: dim_cs1=1 ! soil C dimension: 1 for single, 4 for 4-pool

integer :: co2_dim_len ! For dimension 3-D CO2 field to be passed
integer :: co2_dim_row !     to NI_bl_ctl
integer :: co2_dim_lev !     and NI_rad_ctl

! Array dimensions for sea-salt aerosol
integer :: salt_dim1
integer :: salt_dim2
integer :: salt_dim3

! Array dimensions for Aero_Ctl
integer :: aero_dim1
integer :: aero_dim2
integer :: aero_dim3

! Declare the tropopause variables output for O3_to_3D as allocatable
real, allocatable:: o3_trop_level(:,:)
real, allocatable:: t_trop_level(:,:)
real, allocatable:: o3_trop_height(:,:)
real, allocatable:: t_trop_height(:,:)

!3d ozone (expanded from zonal) for radiation
real, allocatable:: ozone3D(:,:,:)

!  stashworki = stashwork for section i
real, allocatable:: stashwork1(:),stashwork2(:),stashwork3(:),                 &
                    stashwork4(:),stashwork5(:),stashwork6(:),                 &
                    stashwork8(:),stashwork9(:),stashwork12(:),                &
                    stashwork13(:),stashwork14(:),stashwork17(:),              &
                    stashwork19(:),stashwork26(:),stashwork30(:),              &
                    stashwork10(:),stashwork18(:),stashwork35(:),              &
                    stashwork39(:),stashwork21(:),stashwork20(:),              &
                    stashwork53(:),stashwork54(:),stashwork33(:),              &
                    stashwork34(:),stashwork38(:), stashwork50(:)

! local dynamic arrays for PC2

real, allocatable :: t_inc_pres(:,:,:)
      ! Temperature increment due to adiabatic temperature forcing (K)
real, allocatable :: q_inc_pres(:,:,:)
      ! Vapour increment due to adiabatic temperature forcing (kg kg-1)
real, allocatable :: qcl_inc_pres(:,:,:)
      ! Liquid increment due to adiabatic temperature forcing (kg kg-1)
real, allocatable :: qcf_inc_pres(:,:,:)
      ! Ice increment due to adiabatic temperature forcing (kg kg-1)
real, allocatable :: qcf2_inc_pres(:,:,:)
      ! Ice increment due to adiabatic temperature forcing (kg kg-1)
real, allocatable :: cf_inc_pres(:,:,:)
      ! Total cloud fraction increment due to adiabatic forcing (no units)
real, allocatable :: cfl_inc_pres(:,:,:)
      ! Liquid cloud fraction increment due to adiabatic forcing (no units)
real, allocatable :: cff_inc_pres(:,:,:)
      ! Ice cloud fraction increment due to adiabatic forcing (no units)
real, allocatable :: t_dini(:,:,:)
      ! Temperature increment due to initiation (K)
real, allocatable :: q_dini(:,:,:)
      ! Vapour increment due to initiation (kg kg-1)
real, allocatable :: qcl_dini(:,:,:)
      ! Liquid increment due to initiation (kg kg-1)
real, allocatable :: qcf_dini(:,:,:)
      ! Ice increment due to initiation (kg kg-1)
real, allocatable :: qcf2_dini(:,:,:)
      ! Ice increment due to initiation (kg kg-1)
real, allocatable :: cf_dini(:,:,:)
      ! Total cloud fraction increment due to initiation (no units)
real, allocatable :: cfl_dini(:,:,:)
      ! Liquid cloud fraction increment due to initiation(no units)
real, allocatable :: cff_dini(:,:,:)
      ! Ice cloud fraction increment due to initiation (no units)
real, allocatable :: sskew_dini(:,:,:)
!       Skewness of mixture of two s-distributions on levels in bimodal
!       initiation (in-cloud only)
real, allocatable :: svar_turb_dini(:,:,:)
!       Variance of turbulence-based uni-modal s-distribution on levels in
!       bimodal initiation
real, allocatable :: svar_bm_dini(:,:,:)
!       Variance of mixture of two s-distributions in bimodal initiation
!       (in-cloud only)

real, allocatable :: rhts(:,:,:), qtts(:,:,:), tlts(:,:,:), ptts(:,:,:)

! Extra variables needed for cycling
! Vars ending in _phys1 are copies holding the value the original
! variable had after exiting phys1.
! Vars ending in _np1 are tn+1 estimates holding the value the original
! variable had at the end of the last cycle (provided that CycleNo>1).
! obtained from the last
! cycle when CycleNo>1.

real, allocatable :: ti_phys1(:,:,:)
real, allocatable :: cca_phys1(:,:,:), area_cld_frac_phys1(:,:,:)
real, allocatable :: e_trb_phys1(:,:,:), tsq_trb_phys1(:,:,:),                 &
                     qsq_trb_phys1(:,:,:), cov_trb_phys1(:,:,:)
real, allocatable :: bulk_cld_frac_phys1(:,:,:), bulk_cld_liq_phys1(:,:,:)
real, allocatable :: bulk_cld_fr_phys1(:,:,:)

real, allocatable :: wetrho_r_sq_np1(:,:,:)

real, allocatable :: u_np1(:,:,:)
real, allocatable :: v_np1(:,:,:)
real, allocatable :: w_np1(:,:,:)
real, allocatable :: dryrho_np1(:,:,:)
real, allocatable :: rho_np1(:,:,:)

real, allocatable :: etadot_np1(:,:,:)
real, allocatable :: thetav_np1(:,:,:)
real, allocatable :: exner_np1(:,:,:)
real, allocatable :: exner_surf_np1(:,:)
real, allocatable :: m_v_np1(:,:,:)
real, allocatable :: m_cl_np1(:,:,:)
real, allocatable :: m_cf_np1(:,:,:)
real, allocatable :: m_r_np1(:,:,:)
real, allocatable :: m_gr_np1(:,:,:)
real, allocatable :: m_cf2_np1(:,:,:)


real, allocatable:: z0msea_phys1(:,:), zh_phys1(:,:), ddmfx_phys1(:,:),        &
                                     ti_gb_phys1(:,:)
real, allocatable:: deep_flag_phys1(:,:), past_precip_phys1(:,:),              &
                                     past_conv_ht_phys1(:,:)
real, allocatable:: conv_prog_dtheta_phys1(:,:,:)
real, allocatable:: conv_prog_dq_phys1(:,:,:)
real, allocatable:: conv_prog_flx_phys1(:,:,:)
real, allocatable:: conv_prog_precip_phys1(:,:,:)
real, allocatable:: t_land_ctile_phys1(:,:)
real, allocatable:: t_sea_ctile_phys1(:,:)
real, allocatable:: t_sice_ctile_phys1(:,:,:)
real, allocatable:: t_surf_phys1(:,:), t_sf_tile_phys1(:,:)
real, allocatable:: snow_tile_phys1(:,:), dolr_phys1(:,:)
real, allocatable:: rho_lbc_real_tend(:,:)

integer, allocatable :: ccb_phys1(:,:), cct_phys1(:,:)

! Variables required for ice category selection
real, pointer :: p_ti(:,:,:)        => null()
real, pointer :: p_ice_fract(:,:,:) => null()
real, pointer :: p_ice_thick(:,:,:) => null()
real, pointer :: p_tstar_sice(:,:,:) => null()
real, pointer :: p_snodep_sice(:,:,:) => null()
real, pointer :: p_ice_thick_rad(:,:,:) => null()
real, pointer :: p_ice_fract_rad(:,:,:) => null()

! increment diagnostics:
real, allocatable :: u_incr_diagnostic(:,:,:)
real, allocatable :: v_incr_diagnostic(:,:,:)
real, allocatable :: t_incr_diagnostic(:,:,:)
real, allocatable :: q_incr_diagnostic(:,:,:)
real, allocatable :: qcl_incr_diagnostic(:,:,:)
real, allocatable :: qcf_incr_diagnostic(:,:,:)
real, allocatable :: qrain_incr_diagnostic(:,:,:)
real, allocatable :: qgraup_incr_diagnostic(:,:,:)
real, allocatable :: qcf2_incr_diagnostic(:,:,:)
real, allocatable :: cf_incr_diagnostic(:,:,:)
real, allocatable :: cfl_incr_diagnostic(:,:,:)
real, allocatable :: cff_incr_diagnostic(:,:,:)
real, allocatable :: w_incr_diagnostic(:,:,:)

! Allocatable arrays for use in AC_CTL call
real, allocatable :: work_q(:,:,:), work_qcl(:,:,:), work_qcf(:,:,:)

! Workspace defined as allocatable arrays, since they each communicate
! fields between near adjacent calls and only need to use memory for
! a subset of the total routine.
real, allocatable :: exner_prime(:,:,:) ! soln to helmholtz solver
real, allocatable :: dtheta_dr_term(:,:,:)
real, allocatable :: depart_lambda(:,:,:), depart_phi(:,:,:)
real, allocatable :: depart_r_theta(:,:,:), depart_r_w(:,:,:)

real, allocatable :: rhcpt(:,:,:)

real, allocatable :: tgrad_bm(:,:,:)
real, allocatable :: z_theta_bm(:,:,:)
real, allocatable :: tau_dec_bm(:,:,:)
real, allocatable :: tau_hom_bm(:,:,:)
real, allocatable :: tau_mph_bm(:,:,:)
real, allocatable :: ri_bm(:,:,:)
real, allocatable :: mix_len_bm(:,:,:)
real, allocatable :: zhsc_bm(:,:)
real, allocatable :: dzh_bm(:,:)
real, allocatable :: bl_type_7_bm(:,:)

! Allocatable arrays for prognostic tnuc declared here
real(kind=real_umphys),allocatable :: dust_tot_nd(:,:,:)
real(kind=real_umphys),allocatable :: tnuc_nlcl(:,:)
real(kind=real_umphys),allocatable :: tnuc_new(:,:,:)
! ----------------------------------------------------------------

logical ::  gather   ! Convert to sea_ice points within BL code
parameter ( gather = .true. ) ! (was l_compress_seaice)

integer :: n_y_arrays    ! = 1 for global, 3 for LAM
integer :: n_yw_arrays   ! = 1 for global, 2 for LAM
integer :: n_yd_arrays   ! = 1 for global, 3 for LAM
integer :: n_ydw_arrays  ! = 1 for global, 2 for LAM
!
integer :: info          ! icode return from umFlush

real  ::   constant
real  ::   h_print     ! needed for printing idealised orography

logical :: l_update_lbcs, l_apply_lbcs, l_balance, gcr_zero_guess_it

integer :: itemp
integer :: gi

! Oxidant mass-mixing ratios and concentrations, for use in sulphur
! cycle.
real, allocatable :: o3_mmr(:,:,:), hno3_mmr(:,:,:), h2o2_mmr(:,:,:)
real, allocatable :: oh_conc(:,:,:), ho2_conc(:,:,:)

logical :: l_physics_store

! Local variables for using the aerosol climatology for NWP

! Internal array of mass-mixing ratios
real, allocatable :: arcl(:,:,:,:)

! Number of requested species within the climatology
integer :: n_arcl_species

! Corresponding number of requested components
integer :: n_arcl_compnts

! Local structures for UKCA_RADAER prescriptions
type (t_ukca_radaer_presc) :: ukca_radaer_presc_sw
type (t_ukca_radaer_presc) :: ukca_radaer_presc_lw
type (t_ukca_radaer_presc) :: ukca_radaer_presc_aod

! Local structures for using EasyAerosol climatologies
type (t_easyaerosol_rad)  :: easyaerosol_sw
type (t_easyaerosol_rad)  :: easyaerosol_lw
type (t_easyaerosol_cdnc) :: easyaerosol_cdnc


logical, save :: first_atmstep_call = .true.
logical, save :: iau_in_initial     = .false. ! Flag to test within atm_step if IAU ran in initial

end module atm_step_local

