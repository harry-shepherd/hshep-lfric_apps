! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Module containing variables local to atmos_physics2
!
module atmos_physics2_alloc_mod

!
implicit none
save
!
! Description:
!   Contains all variables in atmos_physics2 calculated using
!   start-of-timestep quantities, and which therefore do not need
!   re-calculating at the second ENDGAME semi-lagrangian cycle
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: top_level
!
! Code description:
!   Language: Fortran 90.
!   This code is written to UM programming standards version 8.3.
!
! things from communication section at top
! for surface current interpolation
real, allocatable, target :: uhalo(:,:), vhalo(:,:)
! winds on p-grid
real, allocatable :: u_p(:,:,:),v_p(:,:,:)
! Land fraction on land tiles.
real, allocatable :: fland(:)
! Land fraction on all points
real, allocatable, target :: flandg(:,:)
! extended halo winds for coastal tiling
real, allocatable, target :: u_px(:,:,:), v_px(:,:,:),                         &
                             u_0_px(:,:), v_0_px(:,:)
! extended halo blending height (blendht) when prognostic
real, allocatable, target :: blendht_px(:,:)
! extended halo blending height wind components on p-columns
! (passed from bdy_expl1 in bdy_layr with l_jules_call to bdy_expl3
!  in bdy_layr with not.l_jules_call)
real, allocatable, target :: u_blend_px(:,:)
real, allocatable, target :: v_blend_px(:,:)
!
! things from ni_bl_ctl argument list
! out variables required in IMP_SOLVER
real, allocatable :: alpha1_sea(:,:),alpha1_sice(:,:,:),                       &
 ashtf_prime_sea(:,:),ashtf_prime(:,:,:),bq_gb(:,:,:),bt_gb(:,:,:),            &
 dtrdz_charney_grid(:,:,:),rdz_charney_grid(:,:,:),dtrdz_u(:,:,:),             &
 dtrdz_v(:,:,:),rdz_u(:,:,:),rdz_v(:,:,:),u_s(:,:),fb_surf(:,:)
! out diagnostics (done after implicit solver)
! e_sea, fqw, ftl, h_sea
real, allocatable :: rib_gb(:,:),vshr(:,:),shallowc(:,:),                      &
 cu_over_orog(:,:),bl_type_1(:,:),bl_type_2(:,:),bl_type_3(:,:),               &
 bl_type_4(:,:),bl_type_5(:,:),bl_type_6(:,:),bl_type_7(:,:),                  &
 z0m_eff_gb(:,:), zhnl(:,:)
! out data required for tracer mixing :
real, allocatable :: rho_aresist(:,:),aresist(:,:),resist_b(:,:)
!out variables required for mineral dust scheme
real, allocatable :: r_b_dust(:,:,:),dust_flux(:,:,:),                         &
 dust_emiss_frac(:,:),u_s_t_tile(:,:,:),u_s_t_dry_tile(:,:,:),                 &
 u_s_std_surft(:,:),we_lim(:,:,:),t_frac(:,:,:),zrzi(:,:,:),                   &
 we_lim_dsc(:,:,:),t_frac_dsc(:,:,:),zrzi_dsc(:,:,:),zhsc(:,:)
integer, allocatable :: kent(:,:),kent_dsc(:,:),k_blend_tq(:,:),               &
 k_blend_uv(:,:)
! out additional variables for MOSES II
! ftl_surft, le_tile, radnet_sice, radnet_tile, fqw_surft, epot_surft
! fqw_ice, ftl_ice
real, allocatable :: rho_aresist_surft(:,:), aresist_surft(:,:),               &
 resist_b_surft(:,:), alpha1(:,:), ashtf_prime_surft(:,:),                     &
 fracaero_t(:,:), fracaero_s(:,:),                                             &
 resfs(:,:), resft(:,:), rhokh_surft(:,:) ,rhokh_sice(:,:,:),                  &
 rhokh_sea(:,:), z0hssi(:,:), z0mssi(:,:),                                     &
 chr1p5m(:,:), chr1p5m_sice(:,:), smc_soilt(:),                                &
 npp_gb(:), resp_s_tot_soilt(:), resp_w_pft(:,:),                              &
 gc_surft(:,:), canhc_surft(:,:), wt_ext_surft(:,:,:), flake(:,:),             &
 tile_frac(:,:)
integer, allocatable :: surft_index(:,:),surft_pts(:)
! out additional variables for JULES
! dtstar_surft
! dtstar
real, allocatable :: hcons_soilt(:),emis_soil(:)
! bottom block
! t1_sd, q1_sd, ntml, cumulus, nbdsc, ntdsc
! ntpar, nlcl, zhpar, zlcl, l_shallow, wstar, wthvs, delthvu
! out variables required by convection
real, allocatable :: uw0(:,:),vw0(:,:), taux_p(:,:,:), tauy_p(:,:,:)
! and from bdy_expl3
real, allocatable, target :: rhokm(:,:,:)
real, allocatable :: rhokm_u(:,:,:),rhokm_v(:,:,:),                            &
 cdr10m_u(:,:),cdr10m_v(:,:),flandg_u(:,:),flandg_v(:,:)
!
! arrays for blending height wind levels
integer, allocatable :: k_blend_u(:,:), k_blend_v(:,:)

! conditional arrays, only needed with endgame saving
! for saving conv_diag
real, allocatable :: conv_diag_reals(:,:,:)
integer, allocatable :: conv_diag_ints(:,:,:)
logical, allocatable :: conv_diag_logs(:,:,:)
! for saving ni_bl_ctl
real, allocatable :: bl_ctl_2d(:,:,:),bl_ctl_3d(:,:,:,:),                      &
 tile_save(:,:,:),land_save(:,:),sice_save(:,:,:,:),sea_save(:,:)
integer, allocatable :: bl_ctl_int2d(:,:,:)
logical, allocatable :: bl_ctl_log2d(:,:,:)
! for saving bdy_expl3
real, allocatable :: bdy_expl3_u3d(:,:,:),bdy_expl3_v3d(:,:,:),                &
                     bdy_expl3_u2d(:,:,:), bdy_expl3_v2d(:,:,:)
!
character(len=*), parameter, private :: ModuleName='ATMOS_PHYSICS2_ALLOC_MOD'

contains
subroutine atmos_physics2_alloc(land_pts,nsurft,ndiv,ndivh,npft,               &
             ntype,sm_levels,bl_levels,nice_use)
!
use parkind1, only: jpim, jprb       !DrHook
use yomhook,  only: lhook, dr_hook   !DrHook
use jules_sea_seaice_mod, only: l_use_dtstar_sea
use bl_option_mod,     only: l_quick_ap2, l_calc_tau_at_p
use stash_array_mod, only: stindex
use turb_diff_mod, only: l_leonard_term
use leonard_incs_mod, only: leonard_incs_alloc
use atm_fields_bounds_mod, only: pdims,pdims_s,udims,udims_s,                  &
                                 vdims,vdims_s
use model_domain_mod, only: model_type, mt_single_column


implicit none
!
integer :: land_pts,bl_levels,nsurft,ndiv,ndivh,npft,ntype,                    &
           sm_levels,nice_use
!
integer(kind=jpim), parameter :: zhook_in  = 0
integer(kind=jpim), parameter :: zhook_out = 1
real(kind=jprb)               :: zhook_handle

character(len=*), parameter :: RoutineName='ATMOS_PHYSICS2_ALLOC'
!
if (lhook) call dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)
!
! things from communication section
allocate(uhalo(udims_s%i_start:udims_s%i_end,                                  &
               udims_s%j_start:udims_s%j_end))
allocate(vhalo(vdims_s%i_start:vdims_s%i_end,                                  &
               vdims_s%j_start:vdims_s%j_end))
allocate(u_p(pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end,              &
             pdims%k_start:pdims%k_end))
allocate(v_p(pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end,              &
             pdims%k_start:pdims%k_end))
allocate(fland(land_pts)) ! Land fraction on land tiles.
allocate(flandg(pdims_s%i_start:pdims_s%i_end,pdims_s%j_start:pdims_s%j_end))
allocate(u_px(pdims_s%i_start:pdims_s%i_end,                                   &
              pdims_s%j_start:pdims_s%j_end,bl_levels))
allocate(v_px(pdims_s%i_start:pdims_s%i_end,                                   &
              pdims_s%j_start:pdims_s%j_end,bl_levels))
allocate(u_0_px(pdims_s%i_start:pdims_s%i_end,pdims_s%j_start:pdims_s%j_end))
allocate(v_0_px(pdims_s%i_start:pdims_s%i_end,pdims_s%j_start:pdims_s%j_end))
allocate(blendht_px(pdims_s%i_start:pdims_s%i_end,                             &
                    pdims_s%j_start:pdims_s%j_end))
allocate(u_blend_px(pdims_s%i_start:pdims_s%i_end,                             &
                    pdims_s%j_start:pdims_s%j_end))
allocate(v_blend_px(pdims_s%i_start:pdims_s%i_end,                             &
                    pdims_s%j_start:pdims_s%j_end))
!
! things from ni_bl_ctl argument list
! out variables required in IMP_SOLVER
allocate(alpha1_sea(pdims%i_start:pdims%i_end,                                 &
                    pdims%j_start:pdims%j_end))
allocate(alpha1_sice(pdims%i_start:pdims%i_end,                                &
                     pdims%j_start:pdims%j_end,nice_use))
allocate(ashtf_prime_sea(pdims%i_start:pdims%i_end,                            &
                   pdims%j_start:pdims%j_end))
allocate(ashtf_prime(pdims%i_start:pdims%i_end,                                &
               pdims%j_start:pdims%j_end,nice_use))
allocate(bq_gb(pdims%i_start:pdims%i_end,                                      &
               pdims%j_start:pdims%j_end,bl_levels))
allocate(bt_gb(pdims%i_start:pdims%i_end,                                      &
               pdims%j_start:pdims%j_end,bl_levels))
allocate(dtrdz_charney_grid(pdims%i_start:pdims%i_end,                         &
                            pdims%j_start:pdims%j_end,bl_levels))
allocate(rdz_charney_grid(pdims%i_start:pdims%i_end,                           &
                          pdims%j_start:pdims%j_end,bl_levels))
allocate(dtrdz_u(udims%i_start:udims%i_end,udims%j_start:udims%j_end,          &
                 bl_levels))
allocate(dtrdz_v(vdims%i_start:vdims%i_end,vdims%j_start:vdims%j_end,          &
                 bl_levels))
allocate(rdz_u(udims%i_start:udims%i_end,udims%j_start:udims%j_end,            &
         2:bl_levels))
allocate(rdz_v(vdims%i_start:vdims%i_end,vdims%j_start:vdims%j_end,            &
         2:bl_levels))
allocate(k_blend_tq(pdims%i_start:pdims%i_end,                                 &
                    pdims%j_start:pdims%j_end))
allocate(k_blend_uv(pdims_s%i_start:pdims_s%i_end,                             &
                    pdims_s%j_start:pdims_s%j_end))
allocate(k_blend_u(udims%i_start:udims%i_end,                                  &
                   udims%j_start:udims%j_end))
allocate(k_blend_v(vdims%i_start:vdims%i_end,                                  &
                   vdims%j_start:vdims%j_end))
allocate(u_s(pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end))
! Friction velocity
allocate(fb_surf(pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end))
! Surface buoyancy flux over density (m^2/s^-3)
! out diagnostics (done after implicit solver)
allocate(rib_gb(pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end))
allocate(vshr(pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end))
allocate(shallowc(pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end))
         ! indicator of shallow cumulus
allocate(cu_over_orog(pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end))
         ! indicator of cumulus over steep orography
allocate(bl_type_1(pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end))
         ! stable bl indicator
allocate(bl_type_2(pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end))
         ! decoupled scu over stable bl indicator
allocate(bl_type_3(pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end))
         ! well mixed bl indicator
allocate(bl_type_4(pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end))
         ! decoupled scu over well mixed bl indicator
allocate(bl_type_5(pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end))
         ! decoupled scu over cumulus bl indicator
allocate(bl_type_6(pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end))
         ! cumulus bl indicator
allocate(bl_type_7(pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end))
         ! shear driven bl indicator
allocate(z0m_eff_gb(pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end))
allocate(zhnl(pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end))
! out data required for CLASSIC aerosol mixing and deposition :
allocate(rho_aresist(pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end))
allocate(aresist(pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end))
allocate(resist_b(pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end))
!out variables required for mineral dust scheme
allocate(r_b_dust(pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end,ndiv))
         ! surface layer resist for dust
allocate(dust_flux(pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end,ndiv))
         ! dust emissions (kg m-2 s-1)
allocate(dust_emiss_frac(land_pts,nsurft))
         ! out fraction of tile can emit dust
allocate(u_s_t_tile(land_pts,nsurft,ndivh))
         !out threshold frict. vel
allocate(u_s_t_dry_tile(land_pts,nsurft,ndivh))
         !out dry soil value
allocate(u_s_std_surft(land_pts,nsurft))
         !out friction velocity
allocate(kent(pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end))
         !  grid-level of SML inversion
allocate(kent_dsc(pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end))
         !  grid-level of DSC inversion
allocate(we_lim(pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end,3))
         !  rho*entrainment rate implied by placing of subsidence
allocate(zrzi(pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end,3))
         !  (z-z_base)/(z_i-z_base)
allocate(t_frac(pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end,3))
         !  a fraction of the timestep
allocate(we_lim_dsc(pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end,3))
allocate(zrzi_dsc(pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end,3))
allocate(t_frac_dsc(pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end,3))
allocate(zhsc(pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end))
         !  Top of decoupled layer
! out additional variables for MOSES II
allocate(rho_aresist_surft(land_pts,nsurft))
         ! RHOSTAR*CD_STD*VSHR on land tiles
allocate(aresist_surft(land_pts,nsurft))
         ! 1/(CD_STD*VSHR) on land tiles for CLASSIC aerosol scheme
allocate(resist_b_surft(land_pts,nsurft))
         ! (1/CH-1/CD_STD)/VSHR on land tiles for CLASSIC aerosol scheme
allocate(alpha1(land_pts,nsurft))
         ! Mean gradient of saturated specific humidity with respect
         ! to temperature between the bottom model layer and tile surfaces
allocate(ashtf_prime_surft(land_pts,nsurft))
         !Coefficient to calculate surface heat flux into land tiles.
allocate(fracaero_t(land_pts,nsurft))
         ! Total fraction of surface moisture flux subject only to aerodynamic
allocate(fracaero_s(land_pts,nsurft))
         ! Fraction of surface moisture flux subject only to aerodynamic
         ! from the frozen part of the tile only
allocate(resfs(land_pts,nsurft))
         ! Combined soil, stomatal and aerodynamic resistance
         ! factor for fraction (1-FRACA) of snow-free land tiles.
allocate(resft(land_pts,nsurft))
         ! Total resistance factor FRACA+(1-FRACA)*RESFS for
         !     snow-free land, 1 for snow.
allocate(rhokh_surft(land_pts,nsurft))
         ! Surface exchange coefficients for land tiles
allocate(rhokh_sice(pdims%i_start:pdims%i_end,                                 &
                                    pdims%j_start:pdims%j_end,nice_use))
         ! Surface exchange coefficients for sea-ice
allocate(rhokh_sea(pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end))
         ! Surface exchange coefficients for sea
allocate(z0hssi(pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end))
allocate(z0mssi(pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end))
         ! Roughness lengths over sea (m)
allocate(chr1p5m(land_pts,nsurft))
         ! Ratio of coefffs for calculation of 1.5m temp for land tiles.
allocate(chr1p5m_sice(pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end))
         ! CHR1P5M for sea and sea-ice (leads ignored).
allocate(smc_soilt(land_pts))
allocate(npp_gb(land_pts))
         ! Net primary productivity (kg C/m2/s)
allocate(resp_s_tot_soilt(land_pts))
         ! out total RESP_S over pools
allocate(resp_w_pft(land_pts,npft))
         ! Wood maintenance respiration (kg C/m2/s)
allocate(gc_surft(land_pts,nsurft))
         ! "Stomatal" conductance to evaporation for land tiles (m/s)
allocate(canhc_surft(land_pts,nsurft))
         ! Areal heat capacity of canopy for land tiles (J/K/m2)
allocate(wt_ext_surft(land_pts,sm_levels,nsurft))
         ! Fraction of evapotranspiration which is extracted from each
         !    soil layer by each tile.
allocate(flake(land_pts,nsurft))
         ! Lake fraction
allocate(surft_index(land_pts,ntype))
allocate(surft_pts(ntype))
allocate(tile_frac(land_pts,nsurft))
         ! Tile fractions including snow cover in the ice tile
! out additional variables for JULES
allocate(hcons_soilt(land_pts))
allocate(emis_soil(land_pts))
! bottom block
allocate(uw0(pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end))
         ! U-component of surface wind stress (P-grid)
allocate(vw0(pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end))
         ! V-component of surface wind stress (P-grid)

! Set switch for whether to calculate wind stresses on the theta-grid
! (for diagnostics)
if ( .not. model_type == mt_single_column ) then
  ! The arrays taux_p, tauy_p need to be allocated now if the
  ! diagnostics are requested later, even if not in the first timestep
  ! (this routine, where we allocate taux_p, tauy_p,
  ! is only called in the first timestep).  Therefore check
  ! stindex to see if the diagnostics have been requested at all:
  if ( .not. ( stindex(1,560,3,1)==0 .and. stindex(1,561,3,1)==0 ) ) then
    l_calc_tau_at_p = .true.
  end if
  ! Note: if taux_p, tauy_p were only required for diagnostic
  ! purposes, it would make more sense to only allocate them on
  ! timesteps when the diagnostics are actually requested (in which
  ! case the allocate would need to test stashflags and be done in an area
  ! of code that gets called every timestep).
  ! However, the main purpose of taux_p, tauy_p is for use by
  ! convection schemes (e.g. comorph) called from
  ! other_conv_ctl.  As such, their most common use will require them
  ! to be allocated and set for all timesteps, so it makes sense to just
  ! allocate them once from this routine.
  ! Note: l_calc_tau_at_p is set to true if needed by the convection scheme
  ! in cv_set_dependent_switches.
end if

! Allocate arrays for wind-stress fields on p-grid, if requested
if ( l_calc_tau_at_p ) then
  allocate( taux_p ( pdims%i_start:pdims%i_end,                                &
                     pdims%j_start:pdims%j_end,                                &
                     0:bl_levels-1 ) )
  allocate( tauy_p ( pdims%i_start:pdims%i_end,                                &
                     pdims%j_start:pdims%j_end,                                &
                     0:bl_levels-1 ) )
         ! Wind stresses on theta-levels, on p-grid
else
  allocate( taux_p(1,1,1) )
  allocate( tauy_p(1,1,1) )
end if

! and from bdy_expl3
allocate(cdr10m_u(udims%i_start:udims%i_end,udims%j_start:udims%j_end))
allocate(cdr10m_v(vdims%i_start:vdims%i_end,vdims%j_start:vdims%j_end))
allocate(flandg_u(udims%i_start:udims%i_end,udims%j_start:udims%j_end))
         ! Land frac (on U-grid)
allocate(flandg_v(vdims%i_start:vdims%i_end,vdims%j_start:vdims%j_end))
         ! Land frac (on V-grid)
allocate(rhokm(pdims_s%i_start:pdims_s%i_end,                                  &
               pdims_s%j_start:pdims_s%j_end, 0:bl_levels-1))
allocate(rhokm_u(udims%i_start:udims%i_end,                                    &
                 udims%j_start:udims%j_end, 0:bl_levels-1))
allocate(rhokm_v(vdims%i_start:vdims%i_end,                                    &
                 vdims%j_start:vdims%j_end, 0:bl_levels-1))
!
! If using the Leonard terms, need to allocate the increment arrays
if (l_leonard_term)  call leonard_incs_alloc()
!
! if we are optimising the 2nd ENDGAME cycle, then we need to allocate
! these arrays to save output
if (l_quick_ap2) then
  ! save conv_diag output
  allocate(conv_diag_reals(pdims%i_start:pdims%i_end,                          &
                           pdims%j_start:pdims%j_end,14)) ! reals
  allocate(conv_diag_ints(pdims%i_start:pdims%i_end,                           &
                          pdims%j_start:pdims%j_end,4))   ! integers
  allocate(conv_diag_logs(pdims%i_start:pdims%i_end,                           &
                          pdims%j_start:pdims%j_end,4))   ! logicals
  ! save bl_ctl output
  allocate(bl_ctl_2d(pdims%i_start:pdims%i_end,                                &
                     pdims%j_start:pdims%j_end,5))        ! 2d reals
  allocate(bl_ctl_int2d(pdims%i_start:pdims%i_end,                             &
                        pdims%j_start:pdims%j_end,1))     ! 2d integers
  allocate(bl_ctl_log2d(pdims%i_start:pdims%i_end,                             &
                        pdims%j_start:pdims%j_end,2))     ! 2d logicals
  allocate(bl_ctl_3d(pdims%i_start:pdims%i_end,                                &
                     pdims%j_start:pdims%j_end,bl_levels,3)) !3d reals
  allocate(tile_save(land_pts,nsurft,4))        ! land reals on tiles
  allocate(land_save(land_pts,1))               ! land reals
  allocate(sice_save(pdims%i_start:pdims%i_end,                                &
                     pdims%j_start:pdims%j_end, nice_use, 4)) ! sea-ice fields
  if (l_use_dtstar_sea) then
    allocate(sea_save(pdims%i_start:pdims%i_end,                               &
                      pdims%j_start:pdims%j_end))          ! open sea fields
  end if
  ! save bdy_expl3 output
  allocate(bdy_expl3_u3d(udims%i_start:udims%i_end,                            &
                         udims%j_start:udims%j_end,0:bl_levels-1))
           ! 3d reals on u-points
  allocate(bdy_expl3_v3d(vdims%i_start:vdims%i_end,                            &
                         vdims%j_start:vdims%j_end,0:bl_levels-1))
           ! 3d reals on v-points
  allocate(bdy_expl3_u2d(udims%i_start:udims%i_end,                            &
                         udims%j_start:udims%j_end,2))
           ! 2d reals on u-points
  allocate(bdy_expl3_v2d(vdims%i_start:vdims%i_end,                            &
                         vdims%j_start:vdims%j_end,2))
           ! 2d reals on v-points
end if !l_quick_ap2
!
if (lhook) call dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
!
return
end subroutine atmos_physics2_alloc
end module atmos_physics2_alloc_mod
