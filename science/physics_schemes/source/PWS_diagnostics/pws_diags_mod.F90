! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
!
! Purpose: Interface for pws diagnostics (migrated from FieldCalc utility)
!
! Programming standard: Unified Model Documentation Paper No. 3
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: PWS_diagnostics

module pws_diags_mod

use um_types, only: real_umphys

implicit none

! Arrays containing PWS diagnostics and their respective flags are
! declared in this module.
!
! Diag arrays should follow the naming convention
! type, allocatable :: pws_[name](:,:)
!
! Diag flags should follow the naming convention
! logical :: flag_[name] = .false.
!
! Wind speed at 10m B-grid
real(kind=real_umphys), allocatable :: pws_wind_speed_10mb(:,:)
logical           :: flag_windspeed_10m = .false.
! Precipitation symbol
real(kind=real_umphys), allocatable :: pws_precip_sym(:,:)
real(kind=real_umphys), allocatable :: pws_precip_sym_ls_rain(:,:)
real(kind=real_umphys), allocatable :: pws_precip_sym_ls_snow(:,:)
real(kind=real_umphys), allocatable :: pws_precip_sym_conv_rain(:,:)
real(kind=real_umphys), allocatable :: pws_precip_sym_conv_snow(:,:)
real(kind=real_umphys), allocatable :: pws_precip_sym_t1p5m(:,:)
logical           :: flag_precip_sym = .false.
! Snow probability
real(kind=real_umphys), allocatable :: pws_snow_prob(:,:)
logical           :: flag_snow_prob = .false.
! Convective cloud depth, base and top (ICAO ht)
real(kind=real_umphys), allocatable :: pws_conv_cld_dep(:,:)
real(kind=real_umphys), allocatable :: pws_conv_icao_base(:,:)
real(kind=real_umphys), allocatable :: pws_conv_icao_top(:,:)
logical           :: flag_conv_cld_dep = .false.
logical           :: flag_conv_cld_base = .false.
logical           :: flag_conv_cld_top = .false.

! Flags for Divergence and Relative Vorticity on various pressure levels
logical           :: flag_divergence = .false.
logical           :: flag_rel_vorticity = .false.
! Maxwinds
real(kind=real_umphys), allocatable :: pws_max_wind_ub(:,:)
real(kind=real_umphys), allocatable :: pws_max_wind_vb(:,:)
real(kind=real_umphys), allocatable :: pws_max_wind_pb(:,:)
real(kind=real_umphys), allocatable :: pws_max_wind_base(:,:)
real(kind=real_umphys), allocatable :: pws_max_wind_top(:,:)
real(kind=real_umphys), allocatable :: pws_max_wind_icao(:,:)
logical           :: flag_max_wind_ub = .false.
logical           :: flag_max_wind_vb = .false.
logical           :: flag_max_wind_pb = .false.
logical           :: flag_max_wind_base = .false.
logical           :: flag_max_wind_top = .false.
logical           :: flag_max_wind_icao = .false.
! Output p levels for diags on p levels
logical           :: flag_windspeed_plev = .false.
! Thermal advection on p levels B-grid
logical           :: flag_thermal_advec = .false.
! Thickness 1000-500mb and/or 1000-850mb
real(kind=real_umphys), allocatable :: pws_thickness(:,:)
real(kind=real_umphys), allocatable :: pws_geopht_1000(:,:)
real(kind=real_umphys), allocatable :: pws_geopht_850(:,:)
real(kind=real_umphys), allocatable :: pws_geopht_500(:,:)
logical           :: flag_thickness_500 = .false.
logical           :: flag_thickness_850 = .false.
! Tropopause diagnostics
real(kind=real_umphys), allocatable :: pws_tropopause_ht(:,:)
real(kind=real_umphys), allocatable :: pws_tropopause_press(:,:)
real(kind=real_umphys), allocatable :: pws_tropopause_temp(:,:)
real(kind=real_umphys), allocatable :: pws_tropopause_icao(:,:)

logical           ::  flag_tropopause_ht = .false.
logical           ::  flag_tropopause_temp = .false.
logical           ::  flag_tropopause_press = .false.
logical           ::  flag_tropopause_icao = .false.

! Icing potential
real(kind=real_umphys), allocatable :: pws_icing_pot_diag(:,:,:)
real(kind=real_umphys), allocatable :: icing_pot_press(:)
logical           :: flag_icing_pot = .false.
integer           :: icing_pot_press_levs

! Freezing Levels
real(kind=real_umphys), allocatable :: pws_freezing_ht(:,:)
real(kind=real_umphys), allocatable :: pws_freezing_press(:,:)
real(kind=real_umphys), allocatable :: pws_freezing_icao(:,:)
! zenith total delay
real(kind=real_umphys), allocatable :: pws_zen_tot_delay(:,:)
real(kind=real_umphys), allocatable :: h_rho_levels(:,:,:)
real(kind=real_umphys), allocatable :: h_theta_levels(:,:,:)
logical           :: flag_zenithdelay = .false.

logical           :: flag_freezing_ht = .false.
logical           :: flag_freezing_press = .false.
logical           :: flag_freezing_icao = .false.
logical           :: flag_isotherm_ms20_ht = .false.
logical           :: flag_isotherm_ms20_press = .false.
logical           :: flag_isotherm_ms20_icao = .false.
logical           :: flag_isotherm_ms70_ht = .false.
logical           :: flag_isotherm_ms70_press = .false.
logical           :: flag_isotherm_ms70_icao = .false.

! In cloud turb potential.
real(kind=real_umphys), allocatable :: pws_cloudturb_pot_diag(:,:,:)
logical           :: flag_cloudturb_pot = .false.
integer           :: pot_press_levs
real(kind=real_umphys), allocatable :: pot_press(:)

! WAFC caturb potential
real(kind=real_umphys), allocatable :: pws_wafc_cat_diag(:,:,:)
real(kind=real_umphys), allocatable :: wafc_cat_press(:)
logical           :: flag_wafc_cat = .false.
integer           :: wafc_cat_press_levs

real(kind=real_umphys), allocatable :: pws_gwd_stress_lev_u(:,:,:)
real(kind=real_umphys), allocatable :: pws_gwd_stress_lev_v(:,:,:)

! Cat turb
real(kind=real_umphys), allocatable :: pws_wind_ub_200(:,:)
real(kind=real_umphys), allocatable :: pws_wind_vb_200(:,:)
real(kind=real_umphys), allocatable :: pws_wind_ub_250(:,:)
real(kind=real_umphys), allocatable :: pws_wind_vb_250(:,:)
real(kind=real_umphys), allocatable :: pws_wind_ub_300(:,:)
real(kind=real_umphys), allocatable :: pws_wind_vb_300(:,:)
real(kind=real_umphys), allocatable :: uwind(:,:)
real(kind=real_umphys), allocatable :: vwind(:,:)
real(kind=real_umphys), allocatable :: pws_cat_turb(:,:,:)
real(kind=real_umphys), allocatable :: pws_max_cat(:,:)
real(kind=real_umphys), allocatable :: pws_max_cat_press(:,:)
logical           :: flag_cat_turb = .false.
logical           :: flag_max_cat = .false.
logical           :: flag_max_cat_press = .false.
integer           :: cat_press_levs
real(kind=real_umphys), allocatable :: cat_press(:)

! Mountain wave turbulence
real(kind=real_umphys), allocatable :: pws_mtn_wave_turb(:,:)
logical           :: flag_mtn_wave_turb = .false.

! Dust concentrations
real(kind=real_umphys), allocatable :: pws_dustconc_tot (:,:,:)
real(kind=real_umphys), allocatable :: pws_dustconc_surf(:,:)
real(kind=real_umphys), allocatable :: pws_dustconc_5000(:,:)
logical           :: flag_dustconc_surf = .false.
logical           :: flag_dustconc_5000 = .false.
! Model level 1 dust mass-mixing-ratios (in bins), within timestep
! After emission:
real(kind=real_umphys), allocatable :: pws_dustmmr1_em (:,:)
real(kind=real_umphys), allocatable :: pws_dustmmr2_em (:,:)
real(kind=real_umphys), allocatable :: pws_dustmmr3_em (:,:)
real(kind=real_umphys), allocatable :: pws_dustmmr4_em (:,:)
real(kind=real_umphys), allocatable :: pws_dustmmr5_em (:,:)
real(kind=real_umphys), allocatable :: pws_dustmmr6_em (:,:)
logical           :: flag_dustmmr_em = .false.

! Contrail forecasts
real(kind=real_umphys), allocatable :: pws_contrail_bot(:,:)
real(kind=real_umphys), allocatable :: pws_contrail_top(:,:)
logical           :: flag_contrail_bot = .false.
logical           :: flag_contrail_top = .false.

! Visibility-related arrays
real(kind=real_umphys), allocatable :: pws_bl_1p5m_vis_tot(:,:)
real(kind=real_umphys), allocatable :: pws_bl_1p5m_vis_land(:,:)
real(kind=real_umphys), allocatable :: pws_bl_1p5m_vis_ssi(:,:)
real(kind=real_umphys), allocatable :: pws_bl_1p5m_temp(:,:)
real(kind=real_umphys), allocatable :: pws_bl_1p5m_temp_land(:,:)
real(kind=real_umphys), allocatable :: pws_bl_1p5m_temp_ssi(:,:)
real(kind=real_umphys), allocatable :: pws_1p5m_vis_tot(:,:)
real(kind=real_umphys), allocatable :: pws_1p5m_vis_dust(:,:)
real(kind=real_umphys), allocatable :: pws_1p5m_vis_land(:,:)
real(kind=real_umphys), allocatable :: pws_1p5m_vis_ssi(:,:)
logical           :: flag_1p5m_vis_tot = .false.
logical           :: flag_1p5m_vis_dust = .false.
logical           :: flag_1p5m_vis_land = .false.
logical           :: flag_1p5m_vis_ssi = .false.

! Inverse Richardson Number related arrays
real(kind=real_umphys), allocatable :: pws_inv_richardson(:,:,:)
real(kind=real_umphys), allocatable :: inv_richardson_press(:)
logical           :: flag_inv_richardson = .false.
integer           :: inv_richardson_press_levels

! Updraught Helicity and related velocity arrays
real(kind=real_umphys), allocatable :: pws_upd_helicity_5k(:,:)
real(kind=real_umphys), allocatable :: pws_pcd_mlev_u(:,:,:)
real(kind=real_umphys), allocatable :: pws_pcd_mlev_v(:,:,:)
real(kind=real_umphys), allocatable :: pws_pcd_mlev_w(:,:,:)
logical           :: flag_upd_helicity_5k = .false.

! CAPE/CIN diagnostics  arrays and flags
real(kind=real_umphys), allocatable :: pws_mlcape(:,:)  ! mixed layer CAPE
real(kind=real_umphys), allocatable :: pws_mlcin(:,:)   ! mixed layer CIN
real(kind=real_umphys), allocatable :: pws_mucape(:,:)  ! max CAPE
real(kind=real_umphys), allocatable :: pws_mucin(:,:)   ! CIN for max CAPE
real(kind=real_umphys), allocatable :: pws_sbcape(:,:)  ! surface CAPE
real(kind=real_umphys), allocatable :: pws_sbcin(:,:)   ! CIN for surface CAPE
real(kind=real_umphys), allocatable :: pws_mlelz(:,:)   ! Height LNB for MLCAPE
real(kind=real_umphys), allocatable :: pws_mulplz(:,:)  ! height for MUCAPE
real(kind=real_umphys), allocatable :: pws_muelz(:,:)   ! Height LNB for MUCAPE
real(kind=real_umphys), allocatable :: pws_eibasez(:,:) ! base inflow for MUCAPE
real(kind=real_umphys), allocatable :: pws_eitopz(:,:)  ! top inflow for MUCAPE

real(kind=real_umphys), allocatable :: pws_bl_1p5m_q(:,:) ! BL 1.5m q
logical           :: flag_mlcape = .false.    ! mixed layer ascent required
logical           :: flag_mucape = .false.    ! ascents to find max CAPE
logical           :: flag_sbcape = .false.    ! surface based ascent

! cut off limits to be used in tropopause calculations.
real(kind=real_umphys), parameter :: heightcut_top = 22000.0
                                           !  arbritary limits for high
real(kind=real_umphys), parameter :: heightcut_bot = 4500.0
                                           !  and low trop levels for search
real(kind=real_umphys), parameter :: tempcut= 243.0
                                           !  max temp allowed for tropopause
integer         :: tropo_model_top         !  model level that is close to
                                           !  heightcut_top. It is imdi when
                                           !  model level not identified.

character(len=*), parameter, private :: ModuleName='PWS_DIAGS_MOD'

contains

subroutine pws_diags_alloc()


use atm_fields_bounds_mod,  only: tdims, udims, vdims
use dust_parameters_mod,    only: l_twobin_dust, pwsdiag_sfc_em
use missing_data_mod,       only: rmdi
use stash_array_mod,        only: sf, stlist, stindex, stash_levels
use um_stashcode_mod, only:                                                    &
     stashcode_pws_sec, stashcode_pws_windspeed10m,                            &
     stashcode_pws_windspeedplev,                                              &
     stashcode_pws_divergence,stashcode_pws_rel_vorticity,                     &
     stashcode_pws_snow_prob, stashcode_pws_conv_cld_dep,                      &
     stashcode_pws_conv_icao_base, stashcode_pws_conv_icao_top,                &
     stashcode_pws_precip_sym,stashcode_pws_max_wind_ub,                       &
     stashcode_pws_max_wind_vb, stashcode_pws_max_wind_pb,                     &
     stashcode_pws_max_wind_base, stashcode_pws_max_wind_top,                  &
     stashcode_pws_max_wind_icao, stashcode_pws_thickness500,                  &
     stashcode_pws_thickness850, stashcode_pws_tropopause_ht,                  &
     stashcode_pws_tropopause_temp, stashcode_pws_tropopause_press,            &
     stashcode_pws_tropopause_icao, stashcode_pws_freezing_ht,                 &
     stashcode_pws_freezing_press, stashcode_pws_freezing_icao,                &
     stashcode_pws_isotherm_ms20_ht, stashcode_pws_isotherm_ms20_press,        &
     stashcode_pws_isotherm_ms20_icao, stashcode_pws_isotherm_ms70_ht,         &
     stashcode_pws_isotherm_ms70_press,stashcode_pws_isotherm_ms70_icao,       &
     stashcode_pws_dustconc_surf, stashcode_pws_zenithdelay,                   &
     stashcode_pws_dustconc_5000, stashcode_pws_1p5m_vis_tot,                  &
     stashcode_pws_1p5m_vis_dust, stashcode_pws_1p5m_vis_land,                 &
     stashcode_pws_1p5m_vis_ssi, stashcode_pws_thickness850,                   &
     stashcode_pws_cat_turb, stashcode_pws_max_cat,                            &
     stashcode_pws_max_cat_press,                                              &
     stashcode_pws_contrail_bot, stashcode_pws_contrail_top,                   &
     stashcode_pws_cloudturb_pot_diag, stashcode_pws_icing_pot_diag,           &
     stashcode_pws_wafc_caturb, stashcode_pws_mtn_wave_turb,                   &
     stashcode_pws_cloudturb_pot_diag,                                         &
     stashcode_pws_inv_richardson,                                             &
     stashcode_pws_upd_helicity_5k, stashcode_pws_mlcape, stashcode_pws_mlcin, &
     stashcode_pws_mucape, stashcode_pws_mucin, stashcode_pws_sbcape,          &
     stashcode_pws_sbcin, stashcode_pws_mlelz, stashcode_pws_mulplz,           &
     stashcode_pws_muelz, stashcode_pws_eibasez, stashcode_pws_eitopz

use yomhook,  only: lhook, dr_hook
use parkind1, only: jprb, jpim

implicit none

integer :: im_index,isl,ni,k
integer :: i,j

integer(kind=jpim), parameter :: zhook_in  = 0
integer(kind=jpim), parameter :: zhook_out = 1
real(kind=jprb)               :: zhook_handle

character(len=*), parameter :: RoutineName='PWS_DIAGS_ALLOC'

if (lhook) call dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

im_index = 1

flag_windspeed_10m = sf(stashcode_pws_windspeed10m, stashcode_pws_sec)

if (flag_windspeed_10m) then

  allocate(pws_wind_speed_10mb                                                 &
          (udims%i_start:udims%i_end, vdims%j_start:vdims%j_end))

!$OMP PARALLEL do SCHEDULE(STATIC) DEFAULT(none) private(i,j)                  &
!$OMP SHARED(udims,vdims,pws_wind_speed_10mb)
  do j = vdims%j_start,vdims%j_end
    do i = udims%i_start,udims%i_end
      pws_wind_speed_10mb(i,j) = rmdi
    end do
  end do
!$OMP end PARALLEL do

end if


flag_precip_sym    = sf(stashcode_pws_precip_sym, stashcode_pws_sec)

if (flag_precip_sym) then

  allocate(pws_precip_sym                                                      &
          (tdims%i_start:tdims%i_end, tdims%j_start:tdims%j_end))
  allocate(pws_precip_sym_ls_rain                                              &
          (tdims%i_start:tdims%i_end, tdims%j_start:tdims%j_end))
  allocate(pws_precip_sym_ls_snow                                              &
          (tdims%i_start:tdims%i_end, tdims%j_start:tdims%j_end))
  allocate(pws_precip_sym_conv_rain                                            &
          (tdims%i_start:tdims%i_end, tdims%j_start:tdims%j_end))
  allocate(pws_precip_sym_conv_snow                                            &
          (tdims%i_start:tdims%i_end, tdims%j_start:tdims%j_end))
  allocate(pws_precip_sym_t1p5m                                                &
          (tdims%i_start:tdims%i_end, tdims%j_start:tdims%j_end))

!$OMP PARALLEL do SCHEDULE(STATIC) DEFAULT(none) private(i,j)                  &
!$OMP SHARED(tdims,pws_precip_sym)
  do j = tdims%j_start,tdims%j_end
    do i = tdims%i_start,tdims%i_end
      pws_precip_sym(i,j) = 0.0 !  set to no rain and then alter
                                !  if precip present.
    end do
  end do
!$OMP end PARALLEL do

end if

flag_max_wind_ub   = sf(stashcode_pws_max_wind_ub,  stashcode_pws_sec)
flag_max_wind_vb   = sf(stashcode_pws_max_wind_vb,  stashcode_pws_sec)
flag_max_wind_pb   = sf(stashcode_pws_max_wind_pb,  stashcode_pws_sec)
flag_max_wind_base = sf(stashcode_pws_max_wind_base,stashcode_pws_sec)
flag_max_wind_top  = sf(stashcode_pws_max_wind_top, stashcode_pws_sec)
flag_max_wind_icao = sf(stashcode_pws_max_wind_icao,stashcode_pws_sec)

if (flag_max_wind_ub .or. flag_max_wind_vb .or. flag_max_wind_pb               &
     .or. flag_max_wind_base .or. flag_max_wind_top                            &
     .or. flag_max_wind_icao ) then
  allocate(pws_max_wind_ub                                                     &
          (udims%i_start:udims%i_end, vdims%j_start:vdims%j_end))
  allocate(pws_max_wind_vb                                                     &
          (udims%i_start:udims%i_end, vdims%j_start:vdims%j_end))
  allocate(pws_max_wind_pb                                                     &
          (udims%i_start:udims%i_end, vdims%j_start:vdims%j_end))

!$OMP PARALLEL do SCHEDULE(STATIC) DEFAULT(none) private(i,j)                  &
!$OMP SHARED(udims,vdims,pws_max_wind_ub,pws_max_wind_vb,pws_max_wind_pb)
  do j = vdims%j_start,vdims%j_end
    do i = udims%i_start,udims%i_end
      pws_max_wind_ub(i,j) = rmdi
      pws_max_wind_vb(i,j) = rmdi
      pws_max_wind_pb(i,j) = rmdi
    end do
  end do
!$OMP end PARALLEL do

  if (flag_max_wind_base .or. flag_max_wind_top) then
    allocate(pws_max_wind_base                                                 &
            (udims%i_start:udims%i_end, vdims%j_start:vdims%j_end))
    allocate(pws_max_wind_top                                                  &
            (udims%i_start:udims%i_end, vdims%j_start:vdims%j_end))

!$OMP PARALLEL do SCHEDULE(STATIC) DEFAULT(none) private(i,j)                  &
!$OMP SHARED(udims,vdims,pws_max_wind_base,pws_max_wind_top)
    do j = vdims%j_start,vdims%j_end
      do i = udims%i_start,udims%i_end
        pws_max_wind_base(i,j) = rmdi
        pws_max_wind_top(i,j)  = rmdi
      end do
    end do
!$OMP end PARALLEL do
  end if

  if (flag_max_wind_icao) then
    allocate(pws_max_wind_icao                                                 &
            (udims%i_start:udims%i_end, vdims%j_start:vdims%j_end))

!$OMP PARALLEL do SCHEDULE(STATIC) DEFAULT(none) private(i,j)                  &
!$OMP SHARED(udims,vdims,pws_max_wind_icao)
    do j = vdims%j_start,vdims%j_end
      do i = udims%i_start,udims%i_end
        pws_max_wind_icao(i,j) = rmdi
      end do
    end do
!$OMP end PARALLEL do
  end if
end if

flag_windspeed_plev= sf(stashcode_pws_windspeedplev,stashcode_pws_sec)

flag_thickness_500 = sf(stashcode_pws_thickness500, stashcode_pws_sec)

if (flag_thickness_500) then
  allocate(pws_geopht_500                                                      &
          (tdims%i_start:tdims%i_end, tdims%j_start:tdims%j_end))

!$OMP PARALLEL do SCHEDULE(STATIC) DEFAULT(none) private(i,j)                  &
!$OMP SHARED(tdims,pws_geopht_500)
  do j = tdims%j_start,tdims%j_end
    do i = tdims%i_start,tdims%i_end
      pws_geopht_500(i,j) = rmdi
    end do
  end do
!$OMP end PARALLEL do
end if

flag_thickness_850 = sf(stashcode_pws_thickness850, stashcode_pws_sec)
flag_snow_prob    = sf(stashcode_pws_snow_prob, stashcode_pws_sec)
! Snow probability calculation (Boyden Method) also needs
! 850mb & 1000mb geopotential heights

if (flag_thickness_850 .or. flag_snow_prob) then
  allocate(pws_geopht_850                                                      &
          (tdims%i_start:tdims%i_end, tdims%j_start:tdims%j_end))
!$OMP PARALLEL do SCHEDULE(STATIC) DEFAULT(none) private(i,j)                  &
!$OMP SHARED(tdims,pws_geopht_850)
  do j = tdims%j_start,tdims%j_end
    do i = tdims%i_start,tdims%i_end
      pws_geopht_850(i,j) = rmdi
    end do
  end do
!$OMP end PARALLEL do
end if

if (flag_thickness_850 .or. flag_thickness_500 .or. flag_snow_prob) then
  allocate(pws_thickness                                                       &
          (tdims%i_start:tdims%i_end, tdims%j_start:tdims%j_end))
  allocate(pws_geopht_1000                                                     &
          (tdims%i_start:tdims%i_end, tdims%j_start:tdims%j_end))
!$OMP PARALLEL do SCHEDULE(STATIC) DEFAULT(none) private(i,j)                  &
!$OMP SHARED(tdims,pws_geopht_1000,pws_thickness)
  do j = tdims%j_start,tdims%j_end
    do i = tdims%i_start,tdims%i_end
      pws_thickness(i,j) = rmdi
      pws_geopht_1000(i,j) = rmdi
    end do
  end do
!$OMP end PARALLEL do
end if

if (flag_snow_prob) then
  allocate(pws_snow_prob                                                       &
          (tdims%i_start:tdims%i_end, tdims%j_start:tdims%j_end))
!$OMP PARALLEL do SCHEDULE(STATIC) DEFAULT(none) private(i,j)                  &
!$OMP SHARED(tdims,pws_snow_prob)
  do j = tdims%j_start,tdims%j_end
    do i = tdims%i_start,tdims%i_end
      pws_snow_prob(i,j) = rmdi
    end do
  end do
!$OMP end PARALLEL do
end if

flag_conv_cld_dep = sf(stashcode_pws_conv_cld_dep,stashcode_pws_sec)
flag_conv_cld_base= sf(stashcode_pws_conv_icao_base,stashcode_pws_sec)
flag_conv_cld_top = sf(stashcode_pws_conv_icao_top,stashcode_pws_sec)
if (flag_conv_cld_dep .or. flag_conv_cld_base .or.                             &
                           flag_conv_cld_top) then
  allocate(pws_conv_cld_dep                                                    &
          (tdims%i_start:tdims%i_end, tdims%j_start:tdims%j_end))
  allocate(pws_conv_icao_base                                                  &
          (tdims%i_start:tdims%i_end, tdims%j_start:tdims%j_end))
  allocate(pws_conv_icao_top                                                   &
          (tdims%i_start:tdims%i_end, tdims%j_start:tdims%j_end))
!$OMP PARALLEL do SCHEDULE(STATIC) DEFAULT(none) private(i,j)                  &
!$OMP SHARED(tdims,pws_conv_cld_dep,pws_conv_icao_base,pws_conv_icao_top)
  do j = tdims%j_start,tdims%j_end
    do i = tdims%i_start,tdims%i_end
      pws_conv_cld_dep(i,j) = rmdi
      pws_conv_icao_base(i,j) = rmdi
      pws_conv_icao_top(i,j) = rmdi
    end do
  end do
!$OMP end PARALLEL do
end if


flag_cat_turb      = sf(stashcode_pws_cat_turb, stashcode_pws_sec)
flag_max_cat       = sf(stashcode_pws_max_cat, stashcode_pws_sec)
flag_max_cat_press = sf(stashcode_pws_max_cat_press, stashcode_pws_sec)

if (flag_cat_turb .or. flag_max_cat .or. flag_max_cat_press) then

  isl=stindex(1,16,20,im_index)

  if (isl >  0) then
    ni = -stlist(10,isl)
    cat_press_levs = stash_levels(1,ni)

    allocate (cat_press(cat_press_levs))


    do k = 1,cat_press_levs
      cat_press(k) = stash_levels(k+1,ni)/1000.0
      ! ***** levels are stored as integers so divide by a thousand **
    end do
  else
    cat_press_levs = 1
  end if

  ! ----------Extract required pressures for Potn_vort on press ----


  allocate(pws_wind_ub_250                                                     &
          (udims%i_start:udims%i_end, vdims%j_start:vdims%j_end))
  allocate(pws_wind_vb_250                                                     &
          (udims%i_start:udims%i_end, vdims%j_start:vdims%j_end))
  allocate(pws_wind_ub_300                                                     &
          (udims%i_start:udims%i_end, vdims%j_start:vdims%j_end))
  allocate(pws_wind_vb_300                                                     &
          (udims%i_start:udims%i_end, vdims%j_start:vdims%j_end))

  allocate(pws_cat_turb                                                        &
          (udims%i_start:udims%i_end, vdims%j_start:vdims%j_end,cat_press_levs))

  allocate(pws_max_cat                                                         &
          (udims%i_start:udims%i_end, vdims%j_start:vdims%j_end))

  allocate(pws_max_cat_press                                                   &
          (udims%i_start:udims%i_end, vdims%j_start:vdims%j_end))

  allocate(uwind                                                               &
          (udims%i_start:udims%i_end, vdims%j_start:vdims%j_end))

  allocate(vwind                                                               &
          (udims%i_start:udims%i_end, vdims%j_start:vdims%j_end))

  do k = 1,cat_press_levs
!$OMP PARALLEL do SCHEDULE(STATIC) DEFAULT(none) private(i,j)                  &
!$OMP SHARED(udims,vdims,k,pws_cat_turb)
    do j = vdims%j_start,vdims%j_end
      do i = udims%i_start,udims%i_end
        pws_cat_turb(i,j,k)=0.0
      end do
    end do
!$OMP end PARALLEL do
  end do
!$OMP PARALLEL do SCHEDULE(STATIC) DEFAULT(none) private(i,j)                  &
!$OMP SHARED(udims,vdims,pws_max_cat,pws_max_cat_press,pws_wind_ub_250,        &
!$OMP pws_wind_vb_250,pws_wind_ub_300,pws_wind_vb_300,uwind,vwind)
  do j = vdims%j_start,vdims%j_end
    do i = udims%i_start,udims%i_end
      pws_max_cat(i,j) = 0.0
      pws_max_cat_press(i,j) = 0.0
      pws_wind_ub_250(i,j) = 0.0
      pws_wind_vb_250(i,j) = 0.0
      pws_wind_ub_300(i,j) = 0.0
      pws_wind_vb_300(i,j) = 0.0
      uwind(i,j) = 0.0
      vwind(i,j) = 0.0
    end do
  end do
!$OMP end PARALLEL do


end if

flag_divergence    = sf(stashcode_pws_divergence, stashcode_pws_sec)
flag_rel_vorticity = sf(stashcode_pws_rel_vorticity,stashcode_pws_sec)
flag_mtn_wave_turb = sf(stashcode_pws_mtn_wave_turb,stashcode_pws_sec)


if (flag_divergence .or. flag_rel_vorticity .or. flag_cat_turb .or.            &
    flag_mtn_wave_turb) then

  allocate(pws_wind_ub_200                                                     &
          (udims%i_start:udims%i_end, vdims%j_start:vdims%j_end))
  allocate(pws_wind_vb_200                                                     &
          (udims%i_start:udims%i_end, vdims%j_start:vdims%j_end))

!$OMP PARALLEL do SCHEDULE(STATIC) DEFAULT(none) private(i,j)                  &
!$OMP SHARED(udims,vdims,pws_wind_ub_200,pws_wind_vb_200)
  do j = vdims%j_start,vdims%j_end
    do i = udims%i_start,udims%i_end
      pws_wind_ub_200(i,j) = 0.0
      pws_wind_vb_200(i,j) = 0.0
    end do
  end do
!$OMP end PARALLEL do

end if


flag_tropopause_ht =sf(stashcode_pws_tropopause_ht, stashcode_pws_sec)
flag_tropopause_temp =sf(stashcode_pws_tropopause_temp, stashcode_pws_sec)
flag_tropopause_press =sf(stashcode_pws_tropopause_press, stashcode_pws_sec)
flag_tropopause_icao =sf(stashcode_pws_tropopause_icao, stashcode_pws_sec)

if (flag_tropopause_ht .or. flag_tropopause_temp .or.                          &
    flag_tropopause_press .or. flag_tropopause_icao   ) then
  allocate(pws_tropopause_ht                                                   &
          (tdims%i_start:tdims%i_end, tdims%j_start:tdims%j_end))
  allocate(pws_tropopause_temp                                                 &
          (tdims%i_start:tdims%i_end, tdims%j_start:tdims%j_end))
  allocate(pws_tropopause_press                                                &
          (tdims%i_start:tdims%i_end, tdims%j_start:tdims%j_end))
  allocate(pws_tropopause_icao                                                 &
          (tdims%i_start:tdims%i_end, tdims%j_start:tdims%j_end))
!$OMP PARALLEL do SCHEDULE(STATIC) DEFAULT(none) private(i,j)                  &
!$OMP SHARED(tdims,pws_tropopause_ht,pws_tropopause_press,pws_tropopause_temp, &
!$OMP pws_tropopause_icao)
  do j = tdims%j_start,tdims%j_end
    do i = tdims%i_start,tdims%i_end
      pws_tropopause_ht(i,j) = rmdi
      pws_tropopause_press(i,j) = rmdi
      pws_tropopause_temp(i,j) = rmdi
      pws_tropopause_icao(i,j) = rmdi
    end do
  end do
!$OMP end PARALLEL do
end if

flag_icing_pot = sf(stashcode_pws_icing_pot_diag,stashcode_pws_sec)

if (flag_icing_pot) then

  isl=stindex(1,stashcode_pws_icing_pot_diag,20,im_index)

  if (isl >  0) then
    ni = -stlist(10,isl)
    icing_pot_press_levs = stash_levels(1,ni)
    allocate(icing_pot_press(icing_pot_press_levs))

    do k = 1,icing_pot_press_levs
      icing_pot_press(k) = stash_levels(k+1,ni)/1000.0
      ! ***** levels are stored as integers so divide by a thousand **
      ! next multiply by 100 to convert to pascals as stash uses hPa.
      icing_pot_press(k) = icing_pot_press(k)*100.0
    end do
  else
    allocate(icing_pot_press(icing_pot_press_levs))
    icing_pot_press_levs = 1
  end if

  allocate( pws_icing_pot_diag                                                 &
          ( tdims%i_start:tdims%i_end, tdims%j_start:tdims%j_end,              &
                                           icing_pot_press_levs))
  do k=1,icing_pot_press_levs
!$OMP PARALLEL do SCHEDULE(STATIC) DEFAULT(none) private(i,j)                  &
!$OMP SHARED(tdims,k,pws_icing_pot_diag)
    do j = tdims%j_start,tdims%j_end
      do i = tdims%i_start,tdims%i_end
        pws_icing_pot_diag(i,j,k) = rmdi
      end do
    end do
!$OMP end PARALLEL do
  end do

end if


flag_cloudturb_pot = sf(stashcode_pws_cloudturb_pot_diag,stashcode_pws_sec)

isl=stindex(1,stashcode_pws_cloudturb_pot_diag,20,im_index)

if (flag_cloudturb_pot) then

  if (isl >  0) then
    ni = -stlist(10,isl)
    pot_press_levs = stash_levels(1,ni)
    allocate(pot_press(pot_press_levs))

    do k = 1,pot_press_levs
      pot_press(k) = stash_levels(k+1,ni)/1000.0
      ! ***** levels are stored as integers so divide by a thousand **
    end do
  else
    pot_press_levs = 1
  end if

  allocate( pws_cloudturb_pot_diag                                             &
        (tdims%i_start:tdims%i_end, tdims%j_start:tdims%j_end, pot_press_levs))
  do k=1,pot_press_levs
!$OMP PARALLEL do SCHEDULE(STATIC) DEFAULT(none) private(i,j)                  &
!$OMP SHARED(tdims,k,pws_cloudturb_pot_diag)
    do j = tdims%j_start,tdims%j_end
      do i = tdims%i_start,tdims%i_end
        pws_cloudturb_pot_diag(i,j,k) = 0.0
      end do
    end do
!$OMP end PARALLEL do
  end do

end if


flag_mtn_wave_turb = sf(stashcode_pws_mtn_wave_turb,stashcode_pws_sec)

if (flag_mtn_wave_turb) then

  allocate( pws_mtn_wave_turb                                                  &
        (udims%i_start:udims%i_end, vdims%j_start:vdims%j_end))

!$OMP PARALLEL do SCHEDULE(STATIC) DEFAULT(none) private(i,j)                  &
!$OMP SHARED(udims,vdims,pws_mtn_wave_turb)
  do j = vdims%j_start,vdims%j_end
    do i = udims%i_start,udims%i_end
      pws_mtn_wave_turb(i,j) = 0.0
    end do
  end do
!$OMP end PARALLEL do

end if


flag_contrail_bot     = sf(stashcode_pws_contrail_bot,  stashcode_pws_sec)
flag_contrail_top     = sf(stashcode_pws_contrail_top,  stashcode_pws_sec)

if (flag_contrail_bot .or. flag_contrail_top) then

  allocate(pws_contrail_bot                                                    &
          (tdims%i_start:tdims%i_end, tdims%j_start:tdims%j_end))
  allocate(pws_contrail_top                                                    &
          (tdims%i_start:tdims%i_end, tdims%j_start:tdims%j_end))
!$OMP PARALLEL do SCHEDULE(STATIC) DEFAULT(none) private(i,j)                  &
!$OMP SHARED(tdims,pws_contrail_bot,pws_contrail_top)
  do j = tdims%j_start,tdims%j_end
    do i = tdims%i_start,tdims%i_end
      pws_contrail_bot(i,j) = 0.0
      pws_contrail_top(i,j) = 0.0
    end do
  end do
!$OMP end PARALLEL do

end if

flag_wafc_cat = sf(stashcode_pws_wafc_caturb,stashcode_pws_sec)

if (flag_wafc_cat) then

  isl=stindex(1,stashcode_pws_wafc_caturb,stashcode_pws_sec,im_index)

  if (isl >  0) then
    ni = -stlist(10,isl)
    wafc_cat_press_levs = stash_levels(1,ni)
    allocate(wafc_cat_press(wafc_cat_press_levs))

    do k = 1,wafc_cat_press_levs
      wafc_cat_press(k) = stash_levels(k+1,ni)/1000.0
      ! ***** levels are stored as integers so divide by a thousand **
    end do
  else
    wafc_cat_press_levs = 1
    allocate(wafc_cat_press(wafc_cat_press_levs))
  end if

  allocate( pws_wafc_cat_diag                                                  &
          ( udims%i_start:udims%i_end, vdims%j_start:vdims%j_end,              &
                                           wafc_cat_press_levs))
  do k = 1, wafc_cat_press_levs
!$OMP PARALLEL do SCHEDULE(STATIC) DEFAULT(none) private(i,j)                  &
!$OMP SHARED(udims,vdims,k,pws_wafc_cat_diag)
    do j = vdims%j_start,vdims%j_end
      do i = udims%i_start,udims%i_end
        pws_wafc_cat_diag(i,j,k) = rmdi
      end do
    end do
!$OMP end PARALLEL do
  end do
end if

if (flag_wafc_cat .or. flag_mtn_wave_turb) then
  allocate( pws_gwd_stress_lev_u                                               &
        (udims%i_start:udims%i_end, udims%j_start:udims%j_end, 0:udims%k_end))
  allocate( pws_gwd_stress_lev_v                                               &
        (vdims%i_start:vdims%i_end, vdims%j_start:vdims%j_end, 0:vdims%k_end))

!$OMP PARALLEL do SCHEDULE(STATIC) DEFAULT(none) private(i,j,k)                &
!$OMP SHARED(udims,pws_gwd_stress_lev_u,pws_gwd_stress_lev_v)
  do k = 0,udims%k_end
    do j = udims%j_start,udims%j_end
      do i = udims%i_start,udims%i_end
        pws_gwd_stress_lev_u(i,j,k) = rmdi
      end do
    end do
  end do
!$OMP end PARALLEL do
!$OMP PARALLEL do SCHEDULE(STATIC) DEFAULT(none) private(i,j,k)                &
!$OMP SHARED(vdims,pws_gwd_stress_lev_u,pws_gwd_stress_lev_v)
  do k = 0,vdims%k_end
    do j = vdims%j_start,vdims%j_end
      do i = vdims%i_start,vdims%i_end
        pws_gwd_stress_lev_v(i,j,k) = rmdi
      end do
    end do
  end do
!$OMP end PARALLEL do

end if



flag_freezing_ht      = sf(stashcode_pws_freezing_ht,  stashcode_pws_sec)
flag_freezing_press   = sf(stashcode_pws_freezing_press,  stashcode_pws_sec)
flag_freezing_icao    = sf(stashcode_pws_freezing_icao,  stashcode_pws_sec)
flag_isotherm_ms20_ht = sf(stashcode_pws_isotherm_ms20_ht,                     &
                                               stashcode_pws_sec)
flag_isotherm_ms20_press  = sf(stashcode_pws_isotherm_ms20_press,              &
                                               stashcode_pws_sec)
flag_isotherm_ms20_icao   = sf(stashcode_pws_isotherm_ms20_icao,               &
                                               stashcode_pws_sec)
flag_isotherm_ms70_ht     = sf(stashcode_pws_isotherm_ms70_ht,                 &
                                               stashcode_pws_sec)
flag_isotherm_ms70_press  = sf(stashcode_pws_isotherm_ms70_press,              &
                                               stashcode_pws_sec)
flag_isotherm_ms70_icao   = sf(stashcode_pws_isotherm_ms70_icao,               &
                                               stashcode_pws_sec)

if (flag_freezing_ht         .or. flag_freezing_press      .or.                &
    flag_freezing_icao       .or. flag_isotherm_ms20_ht    .or.                &
    flag_isotherm_ms20_press .or. flag_isotherm_ms20_icao  .or.                &
    flag_isotherm_ms70_ht    .or. flag_isotherm_ms70_press .or.                &
    flag_isotherm_ms70_icao ) then

  allocate(pws_freezing_ht                                                     &
          (tdims%i_start:tdims%i_end, tdims%j_start:tdims%j_end))
  allocate(pws_freezing_press                                                  &
          (tdims%i_start:tdims%i_end, tdims%j_start:tdims%j_end))
  allocate(pws_freezing_icao                                                   &
          (tdims%i_start:tdims%i_end, tdims%j_start:tdims%j_end))
!$OMP PARALLEL do SCHEDULE(STATIC) DEFAULT(none) private(i,j)                  &
!$OMP SHARED(tdims,pws_freezing_ht,pws_freezing_press,pws_freezing_icao)
  do j = tdims%j_start,tdims%j_end
    do i = tdims%i_start,tdims%i_end
      pws_freezing_ht(i,j) = rmdi
      pws_freezing_press(i,j) = rmdi
      pws_freezing_icao(i,j) = rmdi
    end do
  end do
!$OMP end PARALLEL do

end if


flag_dustconc_surf = sf(stashcode_pws_dustconc_surf, stashcode_pws_sec)
flag_dustconc_5000 = sf(stashcode_pws_dustconc_5000, stashcode_pws_sec)

if (flag_dustconc_surf .or. flag_dustconc_5000) then
  allocate(pws_dustconc_tot                                                    &
          (tdims%i_start:tdims%i_end,                                          &
           tdims%j_start:tdims%j_end,                                          &
                       1:tdims%k_end))      ! 1:model_levels
end if

if (flag_dustconc_surf) then
  allocate(pws_dustconc_surf                                                   &
          (tdims%i_start:tdims%i_end, tdims%j_start:tdims%j_end))
!$OMP PARALLEL do SCHEDULE(STATIC) DEFAULT(none) private(i,j)                  &
!$OMP SHARED(tdims,pws_dustconc_surf)
  do j = tdims%j_start,tdims%j_end
    do i = tdims%i_start,tdims%i_end
      pws_dustconc_surf(i,j) = rmdi
    end do
  end do
!$OMP end PARALLEL do
end if

if (flag_dustconc_5000) then
  allocate(pws_dustconc_5000                                                   &
          (tdims%i_start:tdims%i_end, tdims%j_start:tdims%j_end))
!$OMP PARALLEL do SCHEDULE(STATIC) DEFAULT(none) private(i,j)                  &
!$OMP SHARED(tdims,pws_dustconc_5000)
  do j = tdims%j_start,tdims%j_end
    do i = tdims%i_start,tdims%i_end
      pws_dustconc_5000(i,j) = rmdi
    end do
  end do
!$OMP end PARALLEL do
end if

flag_1p5m_vis_tot = sf(stashcode_pws_1p5m_vis_tot, stashcode_pws_sec)

if (flag_1p5m_vis_tot) then
  allocate(pws_1p5m_vis_tot                                                    &
          (tdims%i_start:tdims%i_end, tdims%j_start:tdims%j_end))
!$OMP PARALLEL do SCHEDULE(STATIC) DEFAULT(none) private(i,j)                  &
!$OMP SHARED(tdims,pws_1p5m_vis_tot)
  do j = tdims%j_start,tdims%j_end
    do i = tdims%i_start,tdims%i_end
      pws_1p5m_vis_tot(i,j) = rmdi
    end do
  end do
!$OMP end PARALLEL do
end if

flag_1p5m_vis_dust = sf(stashcode_pws_1p5m_vis_dust, stashcode_pws_sec)

if (flag_1p5m_vis_tot .or. flag_1p5m_vis_dust) then
  allocate(pws_bl_1p5m_vis_tot                                                 &
          (tdims%i_start:tdims%i_end, tdims%j_start:tdims%j_end))
!$OMP PARALLEL do SCHEDULE(STATIC) DEFAULT(none) private(i,j)                  &
!$OMP SHARED(tdims,pws_bl_1p5m_vis_tot)
  do j = tdims%j_start,tdims%j_end
    do i = tdims%i_start,tdims%i_end
      pws_bl_1p5m_vis_tot(i,j) = rmdi
    end do
  end do
!$OMP end PARALLEL do
end if

flag_1p5m_vis_land = sf(stashcode_pws_1p5m_vis_land, stashcode_pws_sec)

if (flag_1p5m_vis_land) then

  allocate(pws_1p5m_vis_land                                                   &
  (tdims%i_start:tdims%i_end, tdims%j_start:tdims%j_end))
!$OMP PARALLEL do SCHEDULE(STATIC) DEFAULT(none) private(i,j)                  &
!$OMP SHARED(tdims,pws_1p5m_vis_land)
  do j = tdims%j_start,tdims%j_end
    do i = tdims%i_start,tdims%i_end
      pws_1p5m_vis_land(i,j) = rmdi
    end do
  end do
!$OMP end PARALLEL do

  allocate(pws_bl_1p5m_vis_land                                                &
          (tdims%i_start:tdims%i_end, tdims%j_start:tdims%j_end))
!$OMP PARALLEL do SCHEDULE(STATIC) DEFAULT(none) private(i,j)                  &
!$OMP SHARED(tdims,pws_bl_1p5m_vis_land)
  do j = tdims%j_start,tdims%j_end
    do i = tdims%i_start,tdims%i_end
      pws_bl_1p5m_vis_land(i,j) = rmdi
    end do
  end do
!$OMP end PARALLEL do
end if

flag_1p5m_vis_ssi = sf(stashcode_pws_1p5m_vis_ssi, stashcode_pws_sec)
if (flag_1p5m_vis_ssi) then
  allocate(pws_1p5m_vis_ssi                                                    &
  (tdims%i_start:tdims%i_end, tdims%j_start:tdims%j_end))
!$OMP PARALLEL do SCHEDULE(STATIC) DEFAULT(none) private(i,j)                  &
!$OMP SHARED(tdims,pws_1p5m_vis_ssi)
  do j = tdims%j_start,tdims%j_end
    do i = tdims%i_start,tdims%i_end
      pws_1p5m_vis_ssi(i,j) = rmdi
    end do
  end do
!$OMP end PARALLEL do

  allocate(pws_bl_1p5m_vis_ssi                                                 &
          (tdims%i_start:tdims%i_end, tdims%j_start:tdims%j_end))
!$OMP PARALLEL do SCHEDULE(STATIC) DEFAULT(none) private(i,j)                  &
!$OMP SHARED(tdims,pws_bl_1p5m_vis_ssi)
  do j = tdims%j_start,tdims%j_end
    do i = tdims%i_start,tdims%i_end
      pws_bl_1p5m_vis_ssi(i,j) = rmdi
    end do
  end do
!$OMP end PARALLEL do
end if

if (flag_1p5m_vis_dust .or. flag_1p5m_vis_tot .or.                             &
    flag_1p5m_vis_ssi .or. flag_1p5m_vis_land) then
  allocate(pws_1p5m_vis_dust                                                   &
          (tdims%i_start:tdims%i_end, tdims%j_start:tdims%j_end))
!$OMP PARALLEL do SCHEDULE(STATIC) DEFAULT(none) private(i,j)                  &
!$OMP SHARED(tdims,pws_1p5m_vis_dust)
  do j = tdims%j_start,tdims%j_end
    do i = tdims%i_start,tdims%i_end
      pws_1p5m_vis_dust(i,j) = rmdi
    end do
  end do
!$OMP end PARALLEL do
end if

! set up the store for the dust MMRs, if requried:
if (flag_1p5m_vis_tot .or. flag_1p5m_vis_dust .or. flag_dustconc_surf .or.     &
    flag_1p5m_vis_ssi .or. flag_1p5m_vis_land) then
  if (pwsdiag_sfc_em > 0.0) then
    flag_dustmmr_em = .true.
    if (l_twobin_dust) then
      allocate(pws_dustmmr1_em                                                 &
           (tdims%i_start:tdims%i_end, tdims%j_start:tdims%j_end))
      allocate(pws_dustmmr2_em                                                 &
           (tdims%i_start:tdims%i_end, tdims%j_start:tdims%j_end))
    else
      allocate(pws_dustmmr1_em                                                 &
           (tdims%i_start:tdims%i_end, tdims%j_start:tdims%j_end))
      allocate(pws_dustmmr2_em                                                 &
           (tdims%i_start:tdims%i_end, tdims%j_start:tdims%j_end))
      allocate(pws_dustmmr3_em                                                 &
           (tdims%i_start:tdims%i_end, tdims%j_start:tdims%j_end))
      allocate(pws_dustmmr4_em                                                 &
           (tdims%i_start:tdims%i_end, tdims%j_start:tdims%j_end))
      allocate(pws_dustmmr5_em                                                 &
           (tdims%i_start:tdims%i_end, tdims%j_start:tdims%j_end))
      allocate(pws_dustmmr6_em                                                 &
           (tdims%i_start:tdims%i_end, tdims%j_start:tdims%j_end))
    end if
  end if
end if

flag_zenithdelay = sf(stashcode_pws_zenithdelay, stashcode_pws_sec)

if (flag_zenithdelay) then

  allocate(pws_zen_tot_delay(tdims%i_start:tdims%i_end,                        &
                             tdims%j_start:tdims%j_end))
!$OMP PARALLEL do SCHEDULE(STATIC) DEFAULT(none) private(i,j)                  &
!$OMP SHARED(tdims,pws_zen_tot_delay)
  do j = tdims%j_start,tdims%j_end
    do i = tdims%i_start,tdims%i_end
      pws_zen_tot_delay(i,j) = 0.0
    end do
  end do
!$OMP end PARALLEL do

end if

flag_inv_richardson = sf(stashcode_pws_inv_richardson, stashcode_pws_sec)
if (flag_inv_richardson) then

  isl = stindex(1, stashcode_pws_inv_richardson, stashcode_pws_sec, im_index)
  if (isl > 0) then
    ni = - stlist(10, isl)
    inv_richardson_press_levels = stash_levels(1, ni)
    allocate(inv_richardson_press(inv_richardson_press_levels))
    do k = 1, inv_richardson_press_levels
      inv_richardson_press(k) = stash_levels(k+1,ni)/1000.0
      ! Levels are integers so divide by one thousand. Then multiply by 100
      ! to convert hPa to Pa.
      inv_richardson_press(k) = inv_richardson_press(k)  * 100.0
    end do
  else
    inv_richardson_press_levels = 1
    allocate(inv_richardson_press(inv_richardson_press_levels))
  end if

  allocate(pws_inv_richardson(tdims%i_start:tdims%i_end,                       &
                              tdims%j_start:tdims%j_end,                       &
                              inv_richardson_press_levels))
  do k = 1, inv_richardson_press_levels
!$OMP PARALLEL do SCHEDULE(STATIC) DEFAULT(none) private(i,j)                  &
!$OMP SHARED(tdims,k,pws_inv_richardson)
    do j = tdims%j_start,tdims%j_end
      do i = tdims%i_start,tdims%i_end
        pws_inv_richardson(i,j,k) = rmdi
      end do
    end do
!$OMP end PARALLEL do
  end do

end if

flag_upd_helicity_5k = sf(stashcode_pws_upd_helicity_5k, stashcode_pws_sec)

if (flag_upd_helicity_5k) then
  allocate(pws_pcd_mlev_u                                                      &
          (tdims%i_start:tdims%i_end,                                          &
           tdims%j_start:tdims%j_end,                                          &
                       1:tdims%k_end))      ! 1:model_levels
  allocate(pws_pcd_mlev_v                                                      &
          (tdims%i_start:tdims%i_end,                                          &
           tdims%j_start:tdims%j_end,                                          &
                       1:tdims%k_end))      ! 1:model_levels
  allocate(pws_pcd_mlev_w                                                      &
          (tdims%i_start:tdims%i_end,                                          &
           tdims%j_start:tdims%j_end,                                          &
                       1:tdims%k_end))      ! 1:model_levels

  allocate(pws_upd_helicity_5k                                                 &
          (tdims%i_start:tdims%i_end, tdims%j_start:tdims%j_end))
!$OMP PARALLEL do SCHEDULE(STATIC) DEFAULT(none) private(i,j)                  &
!$OMP SHARED(tdims,pws_upd_helicity_5k)
  do j = tdims%j_start,tdims%j_end
    do i = tdims%i_start,tdims%i_end
      pws_upd_helicity_5k(i,j) = rmdi
    end do
  end do
!$OMP end PARALLEL do
end if

! Control of convective CAPE and CIN diags
! All diagnostics requiring a mixed layer ascent
flag_mlcape = sf(stashcode_pws_mlcape, stashcode_pws_sec) .or.                 &
              sf(stashcode_pws_mlcin, stashcode_pws_sec)  .or.                 &
              sf(stashcode_pws_mlelz, stashcode_pws_sec)

! All diagnostics requiring a most unstable layer ascent
flag_mucape = sf(stashcode_pws_mucape, stashcode_pws_sec) .or.                 &
              sf(stashcode_pws_mucin, stashcode_pws_sec)  .or.                 &
              sf(stashcode_pws_muelz, stashcode_pws_sec)  .or.                 &
              sf(stashcode_pws_mulplz, stashcode_pws_sec) .or.                 &
              sf(stashcode_pws_eibasez, stashcode_pws_sec) .or.                &
              sf(stashcode_pws_eitopz, stashcode_pws_sec)

! All diagnostics requiring a near surface ascent
flag_sbcape = sf(stashcode_pws_sbcape, stashcode_pws_sec) .or.                 &
              sf(stashcode_pws_sbcin, stashcode_pws_sec)  .or.                 &
              flag_mucape

if (flag_mlcape) then
  allocate( pws_mlcape(tdims%i_start:tdims%i_end, tdims%j_start:tdims%j_end) )
  allocate( pws_mlcin(tdims%i_start:tdims%i_end, tdims%j_start:tdims%j_end) )
  allocate( pws_mlelz(tdims%i_start:tdims%i_end, tdims%j_start:tdims%j_end) )
end if

if (flag_mucape) then
  allocate( pws_mucape(tdims%i_start:tdims%i_end, tdims%j_start:tdims%j_end) )
  allocate( pws_mucin(tdims%i_start:tdims%i_end, tdims%j_start:tdims%j_end) )
  allocate( pws_mulplz(tdims%i_start:tdims%i_end, tdims%j_start:tdims%j_end) )
  allocate( pws_muelz(tdims%i_start:tdims%i_end, tdims%j_start:tdims%j_end) )
  allocate( pws_eibasez(tdims%i_start:tdims%i_end, tdims%j_start:tdims%j_end) )
  allocate( pws_eitopz(tdims%i_start:tdims%i_end, tdims%j_start:tdims%j_end) )
end if

if (flag_sbcape) then
  allocate( pws_sbcape(tdims%i_start:tdims%i_end, tdims%j_start:tdims%j_end) )
  allocate( pws_sbcin(tdims%i_start:tdims%i_end, tdims%j_start:tdims%j_end) )
end if

! Need space for 1.5m q for either set of diagnostics
if (flag_mucape .or. flag_sbcape) then
  allocate( pws_bl_1p5m_q(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end))
end if

! Allocation of space for 1.5m temperature needed by visibility and CAPE CIN
! diagnostics
if (flag_1p5m_vis_tot .or. flag_1p5m_vis_dust .or.                             &
     flag_mucape .or. flag_sbcape) then
  allocate(pws_bl_1p5m_temp(tdims%i_start:tdims%i_end,                         &
                            tdims%j_start:tdims%j_end))
!$OMP PARALLEL do SCHEDULE(STATIC) DEFAULT(none) private(i,j)                  &
!$OMP SHARED(tdims,pws_bl_1p5m_temp)
  do j = tdims%j_start,tdims%j_end
    do i = tdims%i_start,tdims%i_end
      pws_bl_1p5m_temp(i,j) = rmdi
    end do
  end do
!$OMP end PARALLEL do
end if

if (flag_1p5m_vis_land) then
  allocate(pws_bl_1p5m_temp_land(tdims%i_start:tdims%i_end,                    &
                                 tdims%j_start:tdims%j_end))
!$OMP PARALLEL do SCHEDULE(STATIC) DEFAULT(none) private(i,j)                  &
!$OMP SHARED(tdims,pws_bl_1p5m_temp_land)
  do j = tdims%j_start,tdims%j_end
    do i = tdims%i_start,tdims%i_end
      pws_bl_1p5m_temp_land(i,j) = rmdi
    end do
  end do
!$OMP end PARALLEL do
end if

if (flag_1p5m_vis_ssi) then
  allocate(pws_bl_1p5m_temp_ssi(tdims%i_start:tdims%i_end,                     &
                                tdims%j_start:tdims%j_end))
!$OMP PARALLEL do SCHEDULE(STATIC) DEFAULT(none) private(i,j)                  &
!$OMP SHARED(tdims,pws_bl_1p5m_temp_ssi)
  do j = tdims%j_start,tdims%j_end
    do i = tdims%i_start,tdims%i_end
      pws_bl_1p5m_temp_ssi(i,j) = rmdi
    end do
  end do
!$OMP end PARALLEL do
end if

if (lhook) call dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

end subroutine pws_diags_alloc

!-------------------------------------------------------------

subroutine pws_diags_dealloc()

use yomhook, only: lhook, dr_hook
use parkind1, only: jprb, jpim

implicit none

integer(kind=jpim), parameter :: zhook_in  = 0
integer(kind=jpim), parameter :: zhook_out = 1
real(kind=jprb)               :: zhook_handle

character(len=*), parameter :: RoutineName='PWS_DIAGS_DEALLOC'

if (lhook) call dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

if (allocated(pws_bl_1p5m_temp)) deallocate(pws_bl_1p5m_temp)
if (allocated(pws_bl_1p5m_q))    deallocate(pws_bl_1p5m_q)
if (allocated(pws_bl_1p5m_temp_land)) deallocate(pws_bl_1p5m_temp_land)
if (allocated(pws_bl_1p5m_temp_ssi)) deallocate(pws_bl_1p5m_temp_ssi)
if (allocated(pws_sbcin))     deallocate(pws_sbcin)
if (allocated(pws_sbcape))    deallocate(pws_sbcape)
if (allocated(pws_eitopz))     deallocate(pws_eitopz)
if (allocated(pws_eibasez))     deallocate(pws_eibasez)
if (allocated(pws_muelz))     deallocate(pws_muelz)
if (allocated(pws_mulplz))     deallocate(pws_mulplz)
if (allocated(pws_mucin))     deallocate(pws_mucin)
if (allocated(pws_mucape))    deallocate(pws_mucape)
if (allocated(pws_mlelz))     deallocate(pws_mlelz)
if (allocated(pws_mlcin))     deallocate(pws_mlcin)
if (allocated(pws_mlcape))    deallocate(pws_mlcape)

if (allocated(pws_wind_speed_10mb))    deallocate(pws_wind_speed_10mb)
if (allocated(pws_precip_sym))         deallocate(pws_precip_sym)
if (allocated(pws_precip_sym_ls_rain)) deallocate(pws_precip_sym_ls_rain)
if (allocated(pws_precip_sym_ls_snow)) deallocate(pws_precip_sym_ls_snow)
if (allocated(pws_precip_sym_conv_rain)) deallocate(pws_precip_sym_conv_rain)
if (allocated(pws_precip_sym_conv_snow)) deallocate(pws_precip_sym_conv_snow)
if (allocated(pws_precip_sym_t1p5m))   deallocate(pws_precip_sym_t1p5m)
if (allocated(pws_max_wind_ub))        deallocate(pws_max_wind_ub)
if (allocated(pws_max_wind_vb))        deallocate(pws_max_wind_vb)
if (allocated(pws_max_wind_pb))        deallocate(pws_max_wind_pb)
if (allocated(pws_max_wind_base))      deallocate(pws_max_wind_base)
if (allocated(pws_max_wind_top))       deallocate(pws_max_wind_top)
if (allocated(pws_max_wind_icao))      deallocate(pws_max_wind_icao)
if (allocated(pws_thickness))          deallocate(pws_thickness)
if (allocated(pws_geopht_1000))        deallocate(pws_geopht_1000)
if (allocated(pws_geopht_500))         deallocate(pws_geopht_500)
if (allocated(pws_geopht_850))         deallocate(pws_geopht_850)
if (allocated(pws_snow_prob))          deallocate(pws_snow_prob)
if (allocated(pws_conv_cld_dep))       deallocate(pws_conv_cld_dep)
if (allocated(pws_conv_icao_base))     deallocate(pws_conv_icao_base)
if (allocated(pws_conv_icao_top))      deallocate(pws_conv_icao_top)
if (allocated(uwind))                  deallocate(uwind)
if (allocated(vwind))                  deallocate(vwind)
if (allocated(pws_wind_ub_300))        deallocate(pws_wind_ub_300)
if (allocated(pws_wind_vb_300))        deallocate(pws_wind_vb_300)
if (allocated(pws_wind_ub_250))        deallocate(pws_wind_ub_250)
if (allocated(pws_wind_vb_250))        deallocate(pws_wind_vb_250)
if (allocated(pws_wind_ub_200))        deallocate(pws_wind_ub_200)
if (allocated(pws_wind_vb_200))        deallocate(pws_wind_vb_200)
if (allocated(pws_max_cat_press))      deallocate(pws_max_cat_press)
if (allocated(pws_max_cat))            deallocate(pws_max_cat)
if (allocated(cat_press))              deallocate(cat_press)
if (allocated(pws_cat_turb))           deallocate(pws_cat_turb)
if (allocated(pws_contrail_top))       deallocate(pws_contrail_top)
if (allocated(pws_contrail_bot))       deallocate(pws_contrail_bot)
if (allocated(pot_press))              deallocate(pot_press)
if (allocated(pws_cloudturb_pot_diag)) deallocate(pws_cloudturb_pot_diag)
if (allocated(wafc_cat_press))         deallocate(wafc_cat_press)
if (allocated(pws_wafc_cat_diag))      deallocate(pws_wafc_cat_diag)
if (allocated(pws_gwd_stress_lev_u))   deallocate(pws_gwd_stress_lev_u)
if (allocated(pws_gwd_stress_lev_v))   deallocate(pws_gwd_stress_lev_v)
if (allocated(pws_mtn_wave_turb))      deallocate(pws_mtn_wave_turb)
if (allocated(pws_tropopause_icao))    deallocate(pws_tropopause_icao)
if (allocated(pws_tropopause_press))   deallocate(pws_tropopause_press)
if (allocated(pws_tropopause_temp))    deallocate(pws_tropopause_temp)
if (allocated(pws_tropopause_ht))      deallocate(pws_tropopause_ht)
if (allocated(pws_freezing_ht))        deallocate(pws_freezing_ht)
if (allocated(pws_freezing_press))     deallocate(pws_freezing_press)
if (allocated(pws_freezing_icao))      deallocate(pws_freezing_icao)
if (allocated(icing_pot_press))        deallocate(icing_pot_press)
if (allocated(pws_icing_pot_diag))     deallocate(pws_icing_pot_diag)
if (allocated(pws_dustconc_surf))      deallocate(pws_dustconc_surf)
if (allocated(pws_dustconc_5000))      deallocate(pws_dustconc_5000)
if (allocated(pws_dustconc_tot))       deallocate(pws_dustconc_tot)
if (allocated(pws_1p5m_vis_tot))       deallocate(pws_1p5m_vis_tot)
if (allocated(pws_1p5m_vis_land))      deallocate(pws_1p5m_vis_land)
if (allocated(pws_1p5m_vis_dust))      deallocate(pws_1p5m_vis_dust)
if (allocated(pws_1p5m_vis_ssi))       deallocate(pws_1p5m_vis_ssi)
if (allocated(pws_bl_1p5m_vis_tot))    deallocate(pws_bl_1p5m_vis_tot)
if (allocated(pws_bl_1p5m_vis_land))   deallocate(pws_bl_1p5m_vis_land)
if (allocated(pws_bl_1p5m_vis_ssi))    deallocate(pws_bl_1p5m_vis_ssi)
if (allocated(pws_dustmmr6_em))        deallocate(pws_dustmmr6_em)
if (allocated(pws_dustmmr5_em))        deallocate(pws_dustmmr5_em)
if (allocated(pws_dustmmr4_em))        deallocate(pws_dustmmr4_em)
if (allocated(pws_dustmmr3_em))        deallocate(pws_dustmmr3_em)
if (allocated(pws_dustmmr2_em))        deallocate(pws_dustmmr2_em)
if (allocated(pws_dustmmr1_em))        deallocate(pws_dustmmr1_em)
if (allocated(pws_zen_tot_delay))      deallocate(pws_zen_tot_delay)
if (allocated(pws_inv_richardson))     deallocate(pws_inv_richardson)
if (allocated(inv_richardson_press))   deallocate(inv_richardson_press)
if (allocated(pws_upd_helicity_5k))    deallocate(pws_upd_helicity_5k)
if (allocated(pws_pcd_mlev_w))         deallocate(pws_pcd_mlev_w)
if (allocated(pws_pcd_mlev_v))         deallocate(pws_pcd_mlev_v)
if (allocated(pws_pcd_mlev_u))         deallocate(pws_pcd_mlev_u)

if (lhook) call dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

end subroutine pws_diags_dealloc


end module pws_diags_mod
