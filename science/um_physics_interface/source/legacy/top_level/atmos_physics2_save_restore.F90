! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Description:
!  Routines to save convection and other variables in routine atmos_physics2
!  on 1st ENDGame cycle when l_quick_ap2=.true. for conv_diag, bl_ctl etc
!  and restore them for 2nd ENDGame cycle.

! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: top_level

module atmos_physics2_save_restore_mod

implicit none

character(len=*), parameter, private :: ModuleName =                           &
                                       'ATMOS_PHYSICS2_SAVE_RESTORE_MOD'

contains

subroutine ap2_init_conv_diag( rows, row_length, ntml, ntpar, nlcl, cumulus,   &
    l_shallow, l_mid_level, delthvu, ql_ad, zhpar, dzh, qcl_inv_top, zlcl,     &
    zlcl_uv, conv_type, no_cumulus, w_max, w_copy, L_cape_opt_345)

use nlsizes_namelist_mod, only: model_levels
use cv_run_mod,  only: cape_bottom, cape_top
use bl_option_mod, only: zero
use yomhook, only: lhook, dr_hook
use parkind1, only: jprb, jpim

use um_types, only: r_bl

implicit none

! Variables passed in through the argument list...
integer, intent(in) ::                                                         &
  row_length,                                                                  &
  rows

integer, intent(out) ::                                                        &
  ntml      (row_length, rows),                                                &
  ntpar     (row_length, rows),                                                &
  nlcl      (row_length, rows),                                                &
  conv_type (row_length, rows)

real(r_bl), intent(in) ::                                                      &
  w_copy        (row_length, rows, 0:model_levels)

real(r_bl), intent(out) ::                                                     &
  zhpar         (row_length, rows),                                            &
  zlcl          (row_length, rows),                                            &
  zlcl_uv       (row_length, rows),                                            &
  delthvu       (row_length, rows),                                            &
  ql_ad         (row_length, rows),                                            &
  w_max         (row_length, rows),                                            &
  dzh           (row_length, rows),                                            &
  qcl_inv_top   (row_length, rows)

logical, intent(in) ::                                                         &
  L_cape_opt_345

logical, intent(out) ::                                                        &
  cumulus      (row_length, rows),                                             &
  no_cumulus   (row_length, rows),                                             &
  l_shallow    (row_length, rows),                                             &
  l_mid_level  (row_length, rows)

! Local variables...
integer :: i, j, k  ! loop indices
character(len=*), parameter ::  RoutineName = 'AP2_INIT_CONV_DIAG'
! Dr Hook
!==============================
integer(kind=jpim), parameter :: zhook_in  = 0
integer(kind=jpim), parameter :: zhook_out = 1
real(kind=jprb) :: zhook_handle

if (lhook) call dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! Initialise conv_diag output arrays

!$OMP PARALLEL do DEFAULT(none) private(i,j) SCHEDULE(STATIC)                  &
!$OMP SHARED(rows, row_length, ntml, ntpar, nlcl, conv_type, cumulus,          &
!$OMP no_cumulus, l_shallow, l_mid_level, delthvu, ql_ad, zhpar, dzh,          &
!$OMP qcl_inv_top, zlcl, zlcl_uv, w_max)
do j = 1, rows
  do i = 1, row_length
    ntml(i,j)       = 1
    ntpar(i,j)      = 1
    nlcl(i,j)       = 1
    cumulus(i,j)    = .false.
    no_cumulus(i,j) = .false.
    l_shallow(i,j)  = .false.
    l_mid_level(i,j)= .false.
    conv_type(i,j)  = 0
    delthvu(i,j)    = zero
    ql_ad(i,j)      = zero
    zhpar(i,j)      = zero
    dzh(i,j)        = zero
    qcl_inv_top(i,j)= zero
    zlcl(i,j)       = zero
    zlcl_uv(i,j)    = zero
    ! Initialise the w_max array.
    w_max(i,j)      = zero
  end do
end do
!$OMP end PARALLEL do

if (L_cape_opt_345) then
!$OMP PARALLEL DEFAULT(none) private(i,j,k)                                    &
!$OMP SHARED(rows, row_length, cape_bottom, cape_top,                          &
!$OMP        w_copy, w_max)
  !   Find w_max for each column. The w_max array is initialised just
  !   before the start of the OpenMP parallel region.
  do k =  cape_bottom, cape_top
!$OMP do SCHEDULE(STATIC)
    do j = 1, rows
      do i = 1, row_length
        w_max(i,j) = max(w_max(i,j), w_copy(i,j,k))
      end do
    end do
!$OMP end do NOWAIT
  end do
!$OMP end PARALLEL
end if  ! L_cape_opt_345

if (lhook) call dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

return
end subroutine ap2_init_conv_diag


subroutine ap2_save_conv_diag( rows, row_length, zh, zhpar, zlcl, zlcl_uv,     &
    delthvu, ql_ad, cin_undilute, cape_undilute, wstar, wthvs, entrain_coef,   &
    qsat_lcl, dzh, qcl_inv_top, ntml, ntpar, nlcl, conv_type, cumulus,         &
    l_shallow, l_congestus, l_congestus2 )

use atmos_physics2_alloc_mod, only: conv_diag_reals, conv_diag_ints,           &
                                    conv_diag_logs

use yomhook, only: lhook, dr_hook
use parkind1, only: jprb, jpim

implicit none

! Variables passed in through the argument list...
integer, intent(in) ::                                                         &
  row_length,                                                                  &
  rows,                                                                        &
  ntml      (row_length, rows),                                                &
  ntpar     (row_length, rows),                                                &
  nlcl      (row_length, rows),                                                &
  conv_type (row_length, rows)

real, intent(in) ::                                                            &
  zh            (row_length, rows),                                            &
  zhpar         (row_length, rows),                                            &
  zlcl          (row_length, rows),                                            &
  zlcl_uv       (row_length, rows),                                            &
  delthvu       (row_length, rows),                                            &
  ql_ad         (row_length, rows),                                            &
  cin_undilute  (row_length, rows),                                            &
  cape_undilute (row_length, rows),                                            &
  wstar         (row_length, rows),                                            &
  wthvs         (row_length, rows),                                            &
  entrain_coef  (row_length, rows),                                            &
  qsat_lcl      (row_length, rows),                                            &
  dzh           (row_length, rows),                                            &
  qcl_inv_top   (row_length, rows)

logical, intent(in) ::                                                         &
  cumulus      (row_length, rows),                                             &
  l_shallow    (row_length, rows),                                             &
  l_congestus  (row_length, rows),                                             &
  l_congestus2 (row_length, rows)

! Local variables...
integer :: i, j  ! loop indices
character(len=*), parameter ::  RoutineName = 'AP2_SAVE_CONV_DIAG'
! Dr Hook
!==============================
integer(kind=jpim), parameter :: zhook_in  = 0
integer(kind=jpim), parameter :: zhook_out = 1
real(kind=jprb) :: zhook_handle

if (lhook) call dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! save outputs for second EG cycle
! N.B. any new out data added to conv_diag will need to be saved here and
! restored. Array sizes will also need to be changed in atmos_physics2_alloc.

!$OMP PARALLEL do DEFAULT(none) private(j,i) SCHEDULE(STATIC)                  &
!$OMP SHARED(rows,row_length,zh,zhpar,zlcl,zlcl_uv,delthvu,ql_ad,              &
!$OMP        cin_undilute,cape_undilute,wstar,wthvs,entrain_coef,              &
!$OMP        qsat_lcl,dzh,qcl_inv_top,ntml,ntpar,nlcl,conv_type,               &
!$OMP        cumulus,l_shallow,l_congestus,l_congestus2,                       &
!$OMP        conv_diag_reals,conv_diag_ints,conv_diag_logs)
do j = 1, rows
  do i = 1, row_length
    conv_diag_reals(i,j,1) = zh(i,j)
    conv_diag_reals(i,j,2) = zhpar(i,j)
    conv_diag_reals(i,j,3) = zlcl(i,j)
    conv_diag_reals(i,j,4) = zlcl_uv(i,j)
    conv_diag_reals(i,j,5) = delthvu(i,j)
    conv_diag_reals(i,j,6) = ql_ad(i,j)
    conv_diag_reals(i,j,7) = cin_undilute(i,j)
    conv_diag_reals(i,j,8) = cape_undilute(i,j)
    conv_diag_reals(i,j,9) = wstar(i,j)
    conv_diag_reals(i,j,10)= wthvs(i,j)
    conv_diag_reals(i,j,11)= entrain_coef(i,j)
    conv_diag_reals(i,j,12)= qsat_lcl(i,j)
    conv_diag_reals(i,j,13)= dzh(i,j)
    conv_diag_reals(i,j,14)= qcl_inv_top(i,j)

    conv_diag_ints(i,j,1) = ntml(i,j)
    conv_diag_ints(i,j,2) = ntpar(i,j)
    conv_diag_ints(i,j,3) = nlcl(i,j)
    conv_diag_ints(i,j,4) = conv_type(i,j)

    conv_diag_logs(i,j,1) = cumulus(i,j)
    conv_diag_logs(i,j,2) = l_shallow(i,j)
    conv_diag_logs(i,j,3) = l_congestus(i,j)
    conv_diag_logs(i,j,4) = l_congestus2(i,j)
  end do
end do
!$OMP end PARALLEL do

if (lhook) call dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

return
end subroutine ap2_save_conv_diag


subroutine ap2_restore_conv_diag(rows, row_length, zh, zhpar, zlcl, zlcl_uv,   &
    delthvu, ql_ad, cin_undilute, cape_undilute, wstar, wthvs, entrain_coef,   &
    qsat_lcl, dzh, qcl_inv_top, ntml, ntpar, nlcl, conv_type, cumulus,         &
    l_shallow, l_congestus, l_congestus2 )

use atmos_physics2_alloc_mod, only: conv_diag_reals, conv_diag_ints,           &
                                    conv_diag_logs

use yomhook, only: lhook, dr_hook
use parkind1, only: jprb, jpim

implicit none

! Variables passed in through the argument list...
integer, intent(in) ::                                                         &
  row_length,                                                                  &
  rows

integer, intent(out) ::                                                        &
  ntml      (row_length, rows),                                                &
  ntpar     (row_length, rows),                                                &
  nlcl      (row_length, rows),                                                &
  conv_type (row_length, rows)

real, intent(out) ::                                                           &
  zh            (row_length, rows),                                            &
  zhpar         (row_length, rows),                                            &
  zlcl          (row_length, rows),                                            &
  zlcl_uv       (row_length, rows),                                            &
  delthvu       (row_length, rows),                                            &
  ql_ad         (row_length, rows),                                            &
  cin_undilute  (row_length, rows),                                            &
  cape_undilute (row_length, rows),                                            &
  wstar         (row_length, rows),                                            &
  wthvs         (row_length, rows),                                            &
  entrain_coef  (row_length, rows),                                            &
  qsat_lcl      (row_length, rows),                                            &
  dzh           (row_length, rows),                                            &
  qcl_inv_top   (row_length, rows)

logical, intent(out) ::                                                        &
  cumulus      (row_length, rows),                                             &
  l_shallow    (row_length, rows),                                             &
  l_congestus  (row_length, rows),                                             &
  l_congestus2 (row_length, rows)

! Local variables...
integer :: i, j  ! loop indices
character(len=*), parameter ::  RoutineName = 'AP2_RESTORE_CONV_DIAG'
! Dr Hook
!==============================
integer(kind=jpim), parameter :: zhook_in  = 0
integer(kind=jpim), parameter :: zhook_out = 1
real(kind=jprb) :: zhook_handle

if (lhook) call dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! Restore outputs on second EG cycle

!$OMP PARALLEL do DEFAULT(none) private(j,i) SCHEDULE(STATIC)                  &
!$OMP SHARED(rows,row_length,zh,zhpar,zlcl,zlcl_uv,delthvu,ql_ad,              &
!$OMP        cin_undilute,cape_undilute,wstar,wthvs,entrain_coef,              &
!$OMP        qsat_lcl,dzh,qcl_inv_top,ntml,ntpar,nlcl,conv_type,               &
!$OMP        cumulus,l_shallow,l_congestus,l_congestus2,                       &
!$OMP        conv_diag_reals,conv_diag_ints,conv_diag_logs)
do j = 1, rows
  do i = 1, row_length
    zh(i,j)            = conv_diag_reals(i,j,1)
    zhpar(i,j)         = conv_diag_reals(i,j,2)
    zlcl(i,j)          = conv_diag_reals(i,j,3)
    zlcl_uv(i,j)       = conv_diag_reals(i,j,4)
    delthvu(i,j)       = conv_diag_reals(i,j,5)
    ql_ad(i,j)         = conv_diag_reals(i,j,6)
    cin_undilute(i,j)  = conv_diag_reals(i,j,7)
    cape_undilute(i,j) = conv_diag_reals(i,j,8)
    wstar(i,j)         = conv_diag_reals(i,j,9)
    wthvs(i,j)         = conv_diag_reals(i,j,10)
    entrain_coef(i,j)  = conv_diag_reals(i,j,11)
    qsat_lcl(i,j)      = conv_diag_reals(i,j,12)
    dzh(i,j)           = conv_diag_reals(i,j,13)
    qcl_inv_top(i,j)   = conv_diag_reals(i,j,14)

    ntml(i,j)          = conv_diag_ints(i,j,1)
    ntpar(i,j)         = conv_diag_ints(i,j,2)
    nlcl(i,j)          = conv_diag_ints(i,j,3)
    conv_type(i,j)     = conv_diag_ints(i,j,4)

    cumulus(i,j)       = conv_diag_logs(i,j,1)
    l_shallow(i,j)     = conv_diag_logs(i,j,2)
    l_congestus(i,j)   = conv_diag_logs(i,j,3)
    l_congestus2(i,j)  = conv_diag_logs(i,j,4)
  end do
end do
!$OMP end PARALLEL do

if (lhook) call dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

return
end subroutine ap2_restore_conv_diag


subroutine ap2_save_bl_ctl( rows, row_length, bl_levels, ntiles,               &
    land_points, gs, zh, dzh, z0msea, wstar, wthvs, ntml, cumulus, l_shallow,  &
    fqw, ftl, rhokh, dtstar_sea, radnet_sice, fqw_ice,                         &
    ftl_ice, dtstar_sice, ftl_tile, fqw_tile, epot_tile, dtstar_tile,          &
    l_use_dtstar_sea )

use atmos_physics2_alloc_mod, only: bl_ctl_2d, bl_ctl_int2d, bl_ctl_log2d,     &
                                    bl_ctl_3d, sea_save, sice_save,            &
                                    tile_save, land_save
use jules_sea_seaice_mod, only: nice_use

use yomhook, only: lhook, dr_hook
use parkind1, only: jprb, jpim

implicit none

! Variables passed in through the argument list...
integer, intent(in) ::                                                         &
  row_length, rows, bl_levels,                                                 &
  ntiles, land_points,                                                         &
  ntml      (row_length, rows)

real, intent(in) ::                                                            &
  gs    (land_points),                                                         &
  zh    (row_length, rows),                                                    &
  dzh   (row_length, rows),                                                    &
  z0msea(row_length, rows),                                                    &
  wstar (row_length, rows),                                                    &
  wthvs (row_length, rows),                                                    &
  fqw  (row_length, rows, bl_levels),                                          &
  ftl  (row_length, rows, bl_levels),                                          &
  rhokh(row_length, rows, bl_levels),                                          &
  dtstar_sea  (row_length, rows),                                              &
  radnet_sice (row_length, rows, nice_use),                                    &
  fqw_ice     (row_length, rows, nice_use),                                    &
  ftl_ice     (row_length, rows, nice_use),                                    &
  dtstar_sice (row_length, rows, nice_use),                                    &
  ftl_tile    (land_points, ntiles),                                           &
  fqw_tile    (land_points, ntiles),                                           &
  epot_tile   (land_points, ntiles),                                           &
  dtstar_tile (land_points, ntiles)

logical, intent(in) ::                                                         &
  l_use_dtstar_sea,                                                            &
  cumulus  (row_length, rows),                                                 &
  l_shallow(row_length, rows)

! Local variables...
integer :: i, j, k  ! loop indices
character(len=*), parameter ::  RoutineName = 'AP2_SAVE_BL_CTL'
! Dr Hook
!==============================
integer(kind=jpim), parameter :: zhook_in  = 0
integer(kind=jpim), parameter :: zhook_out = 1
real(kind=jprb) :: zhook_handle

if (lhook) call dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! Save outputs for second EG cycle
! N.B. any new out data added to ni_bl_ctl which gets subsequently modified
! (i.e. is INOUT to conv_ctl or imp_ctl) will need to be saved here and
! restored. Array sizes will also need to be changed in atmos_physics2_alloc.

!$OMP PARALLEL DEFAULT(none) private(j,i,k)                                    &
!$OMP SHARED(rows,row_length,bl_levels,nice_use,bl_ctl_2d,bl_ctl_int2d,        &
!$OMP        bl_ctl_log2d,bl_ctl_3d,sea_save,sice_save,zh,dzh,z0msea,wstar,    &
!$OMP        wthvs,ntml,cumulus,l_shallow,fqw,ftl,rhokh,                       &
!$OMP        l_use_dtstar_sea, dtstar_sea,radnet_sice,                         &
!$OMP        fqw_ice,ftl_ice,dtstar_sice,land_points,land_save,gs,             &
!$OMP        ntiles, tile_save, ftl_tile, fqw_tile, epot_tile, dtstar_tile )

!$OMP do SCHEDULE(STATIC)
do j = 1, rows
  do i = 1, row_length
    bl_ctl_2d(i,j,1)=zh(i,j)    !INOUT to bl_ctl, gets reset in atm_step
    bl_ctl_2d(i,j,2)=dzh(i,j)   !INOUT to bl_ctl
    bl_ctl_2d(i,j,3)=z0msea(i,j)!INOUT to bl_ctl, gets reset in atm_step
    bl_ctl_2d(i,j,4)=wstar(i,j) !INOUT to conv_ctl, needs reset
    bl_ctl_2d(i,j,5)=wthvs(i,j) !INOUT to conv_ctl, needs reset

    bl_ctl_int2d(i,j,1)=ntml(i,j)!INOUT to conv_ctl, needs reset

    bl_ctl_log2d(i,j,1)=cumulus(i,j)
    !INOUT to conv_ctl, needs reset
    bl_ctl_log2d(i,j,2)=l_shallow(i,j)
    !INOUT to conv_ctl, needs reset
  end do
end do
!$OMP end do NOWAIT

!$OMP do SCHEDULE(STATIC)
do k = 1, bl_levels
  do j = 1, rows
    do i = 1, row_length
      bl_ctl_3d(i,j,k,1)=fqw(i,j,k)  !INOUT to imp_ctl, needs reset
      bl_ctl_3d(i,j,k,2)=ftl(i,j,k)  !INOUT to imp_ctl, needs reset
      bl_ctl_3d(i,j,k,3)=rhokh(i,j,k)!INOUT to imp_ctl, needs reset
    end do
  end do
end do
!$OMP end do NOWAIT

if (l_use_dtstar_sea) then
!$OMP do SCHEDULE(STATIC)
  do j = 1, rows
    do i = 1, row_length
      sea_save(i,j)=dtstar_sea(i,j) !INOUT to imp_ctl, needs reset
    end do
  end do
!$OMP end do NOWAIT
end if

do k = 1, nice_use
!$OMP do SCHEDULE(STATIC)
  do j = 1, rows
    do i = 1, row_length
      sice_save(i,j,k,1)=radnet_sice(i,j,k)!INOUT to imp_ctl, needs reset
      sice_save(i,j,k,2)=fqw_ice(i,j,k)    !INOUT to imp_ctl, needs reset
      sice_save(i,j,k,3)=ftl_ice(i,j,k)    !INOUT to imp_ctl, needs reset
      sice_save(i,j,k,4)=dtstar_sice(i,j,k)!INOUT to imp_ctl, needs reset
    end do
  end do
!$OMP end do NOWAIT
end do

!$OMP do SCHEDULE(STATIC)
do i = 1, land_points
  land_save(i,1) = gs(i) !INOUT to bl_ctl, gets reset in atm_step
end do
!$OMP end do NOWAIT

do j = 1, ntiles
!$OMP do SCHEDULE(STATIC)
  do i = 1, land_points
    tile_save(i,j,1)=ftl_tile(i,j)     !INOUT to imp_ctl, needs reset
    tile_save(i,j,2)=fqw_tile(i,j)     !INOUT to imp_ctl, needs reset
    tile_save(i,j,3)=epot_tile(i,j)    !INOUT to imp_ctl, needs reset
    tile_save(i,j,4)=dtstar_tile(i,j)  !INOUT to imp_ctl, needs reset
  end do
!$OMP end do NOWAIT
end do
!$OMP end PARALLEL

if (lhook) call dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

return
end subroutine ap2_save_bl_ctl


subroutine ap2_restore_bl_ctl( rows, row_length, bl_levels, ntiles,            &
    land_points, gs, zh, dzh, z0msea, wstar, wthvs, ntml, cumulus, l_shallow,  &
    fqw, ftl, rhokh, dtstar_sea, radnet_sice, fqw_ice,                         &
    ftl_ice, dtstar_sice, ftl_tile, fqw_tile, epot_tile, dtstar_tile,          &
    l_use_dtstar_sea )

use atmos_physics2_alloc_mod, only: bl_ctl_2d, bl_ctl_int2d, bl_ctl_log2d,     &
                                    bl_ctl_3d, sea_save, sice_save,            &
                                    tile_save, land_save
use jules_sea_seaice_mod, only: nice_use

use yomhook, only: lhook, dr_hook
use parkind1, only: jprb, jpim

implicit none

! Variables passed in through the argument list...
integer, intent(in) ::                                                         &
  row_length, rows, bl_levels,                                                 &
  ntiles, land_points

integer, intent(out) ::                                                        &
  ntml      (row_length, rows)

real, intent(out) ::                                                           &
  gs    (land_points),                                                         &
  zh    (row_length, rows),                                                    &
  dzh   (row_length, rows),                                                    &
  z0msea(row_length, rows),                                                    &
  wstar (row_length, rows),                                                    &
  wthvs (row_length, rows),                                                    &
  fqw  (row_length, rows, bl_levels),                                          &
  ftl  (row_length, rows, bl_levels),                                          &
  rhokh(row_length, rows, bl_levels),                                          &
  dtstar_sea  (row_length, rows),                                              &
  radnet_sice (row_length, rows, nice_use),                                    &
  fqw_ice     (row_length, rows, nice_use),                                    &
  ftl_ice     (row_length, rows, nice_use),                                    &
  dtstar_sice (row_length, rows, nice_use),                                    &
  ftl_tile    (land_points, ntiles),                                           &
  fqw_tile    (land_points, ntiles),                                           &
  epot_tile   (land_points, ntiles),                                           &
  dtstar_tile (land_points, ntiles)

logical, intent(in) ::                                                         &
  l_use_dtstar_sea

logical, intent(out) ::                                                        &
  cumulus  (row_length, rows),                                                 &
  l_shallow(row_length, rows)

! Local variables...
integer :: i, j, k  ! loop indices
character(len=*), parameter ::  RoutineName = 'AP2_RESTORE_BL_CTL'
! Dr Hook
!==============================
integer(kind=jpim), parameter :: zhook_in  = 0
integer(kind=jpim), parameter :: zhook_out = 1
real(kind=jprb) :: zhook_handle

if (lhook) call dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! Restore outputs for second EG cycle
! N.B. any new out data added to ni_bl_ctl which gets subsequently modified
! (i.e. is INOUT to conv_ctl or imp_ctl) will be restored here.
! Array sizes will also need to be changed in atmos_physics2_alloc.

!$OMP PARALLEL DEFAULT(none) private(j,i,k)                                    &
!$OMP SHARED(rows,row_length,bl_levels,nice_use,bl_ctl_2d,bl_ctl_int2d,        &
!$OMP        bl_ctl_log2d,bl_ctl_3d,sea_save,sice_save,zh,dzh,z0msea,wstar,    &
!$OMP        wthvs,ntml,cumulus,l_shallow,fqw,ftl,rhokh,                       &
!$OMP        l_use_dtstar_sea, dtstar_sea,radnet_sice,                         &
!$OMP        fqw_ice,ftl_ice,dtstar_sice,land_points,gs,land_save,             &
!$OMP        ntiles, tile_save, ftl_tile, fqw_tile, epot_tile, dtstar_tile)

!$OMP do SCHEDULE(STATIC)
do j = 1, rows
  do i = 1, row_length
    zh(i,j)     = bl_ctl_2d(i,j,1)
    dzh(i,j)    = bl_ctl_2d(i,j,2)
    z0msea(i,j) = bl_ctl_2d(i,j,3)
    wstar(i,j)  = bl_ctl_2d(i,j,4)
    wthvs(i,j)  = bl_ctl_2d(i,j,5)

    ntml(i,j) = bl_ctl_int2d(i,j,1)

    cumulus(i,j)   = bl_ctl_log2d(i,j,1)
    l_shallow(i,j) = bl_ctl_log2d(i,j,2)
  end do
end do
!$OMP end do NOWAIT

!$OMP do SCHEDULE(STATIC)
do k = 1, bl_levels
  do j = 1, rows
    do i = 1, row_length
      fqw(i,j,k)   = bl_ctl_3d(i,j,k,1)
      ftl(i,j,k)   = bl_ctl_3d(i,j,k,2)
      rhokh(i,j,k) = bl_ctl_3d(i,j,k,3)
    end do
  end do
end do
!$OMP end do NOWAIT

if (l_use_dtstar_sea) then
!$OMP do SCHEDULE(STATIC)
  do j = 1, rows
    do i = 1, row_length
      dtstar_sea(i,j) = sea_save(i,j)
    end do
  end do
!$OMP end do NOWAIT
end if

do k = 1, nice_use
!$OMP do SCHEDULE(STATIC)
  do j = 1, rows
    do i = 1, row_length
      radnet_sice(i,j,k) = sice_save(i,j,k,1)
      fqw_ice(i,j,k)     = sice_save(i,j,k,2)
      ftl_ice(i,j,k)     = sice_save(i,j,k,3)
      dtstar_sice(i,j,k) = sice_save(i,j,k,4)
    end do
  end do
!$OMP end do NOWAIT
end do

!$OMP do SCHEDULE(STATIC)
do i = 1, land_points
  gs(i) = land_save(i,1) !INOUT to bl_ctl, gets reset in atm_step
end do
!$OMP end do NOWAIT

do j = 1, ntiles
!$OMP do SCHEDULE(STATIC)
  do i = 1, land_points
    ftl_tile(i,j)    = tile_save(i,j,1)
    fqw_tile(i,j)    = tile_save(i,j,2)
    epot_tile(i,j)   = tile_save(i,j,3)
    dtstar_tile(i,j) = tile_save(i,j,4)
  end do
!$OMP end do NOWAIT
end do
!$OMP end PARALLEL

if (lhook) call dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

return
end subroutine ap2_restore_bl_ctl


subroutine ap2_save_bdy_expl( bl_levels,                                       &
    taux, tauy, taux_land, taux_ssi, tauy_land, tauy_ssi )

use atm_fields_bounds_mod, only: udims, vdims
use atmos_physics2_alloc_mod, only: bdy_expl3_u3d, bdy_expl3_v3d,              &
                                    bdy_expl3_u2d, bdy_expl3_v2d

use yomhook, only: lhook, dr_hook
use parkind1, only: jprb, jpim

implicit none

! Variables passed in through the argument list...
integer, intent(in) ::                                                         &
  bl_levels

real, intent(in) ::                                                            &
  taux(udims%i_start:udims%i_end,udims%j_start:udims%j_end,0:bl_levels-1),     &
  tauy(vdims%i_start:vdims%i_end,vdims%j_start:vdims%j_end,0:bl_levels-1),     &
  taux_land(udims%i_start:udims%i_end,udims%j_start:udims%j_end),              &
                               ! Taux over land part of grid box.
  taux_ssi(udims%i_start:udims%i_end,udims%j_start:udims%j_end),               &
                               ! Taux over sea part of grid box.
  tauy_land(vdims%i_start:vdims%i_end,vdims%j_start:vdims%j_end),              &
                               ! Tauy over land part of grid box.
  tauy_ssi(vdims%i_start:vdims%i_end,vdims%j_start:vdims%j_end)
                               ! Tauy over sea part of grid box.

! Local variables...
integer :: i, j, k  ! loop indices
character(len=*), parameter ::  RoutineName = 'AP2_SAVE_BDY_EXPL'
! Dr Hook
!==============================
integer(kind=jpim), parameter :: zhook_in  = 0
integer(kind=jpim), parameter :: zhook_out = 1
real(kind=jprb) :: zhook_handle

if (lhook) call dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! save outputs for second EG cycle
! N.B. any new out data added to bdy_expl3 will need to be saved here and
! restored below. Array sizes will need to be increased in atmos_physics2_alloc
!$OMP PARALLEL DEFAULT(none) private( i, j, k )                                &
!$OMP SHARED( bl_levels, udims, vdims, bdy_expl3_u3d, taux,                    &
!$OMP         bdy_expl3_v3d, tauy, bdy_expl3_u2d, taux_land,                   &
!$OMP         taux_ssi, bdy_expl3_v2d, tauy_land, tauy_ssi )
!$OMP do SCHEDULE(STATIC)
do k = 0, bl_levels-1
  do j = udims%j_start, udims%j_end
    do i = udims%i_start, udims%i_end
      bdy_expl3_u3d(i,j,k)=taux(i,j,k)    !INOUT to imp_ctl, needs reset
    end do
  end do

  do j = vdims%j_start, vdims%j_end
    do i = vdims%i_start, vdims%i_end
      bdy_expl3_v3d(i,j,k)=tauy(i,j,k)    !INOUT to imp_ctl, needs reset
    end do
  end do
end do
!$OMP end do NOWAIT

!$OMP do SCHEDULE(STATIC)
do j = udims%j_start, udims%j_end
  do i = udims%i_start, udims%i_end
    bdy_expl3_u2d(i,j,1)=taux_land(i,j)   !INOUT to imp_ctl, needs reset
    bdy_expl3_u2d(i,j,2)=taux_ssi(i,j)    !INOUT to imp_ctl, needs reset
  end do
end do
!$OMP end do NOWAIT

!$OMP do SCHEDULE(STATIC)
do j = vdims%j_start, vdims%j_end
  do i = vdims%i_start, vdims%i_end
    bdy_expl3_v2d(i,j,1)=tauy_land(i,j)   !INOUT to imp_ctl, needs reset
    bdy_expl3_v2d(i,j,2)=tauy_ssi(i,j)    !INOUT to imp_ctl, needs reset
  end do
end do
!$OMP end do
!$OMP end PARALLEL

if (lhook) call dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

return
end subroutine ap2_save_bdy_expl


subroutine ap2_restore_bdy_expl( bl_levels,                                    &
    taux, tauy, taux_land, taux_ssi, tauy_land, tauy_ssi )

use atm_fields_bounds_mod, only: udims, vdims
use atmos_physics2_alloc_mod, only: bdy_expl3_u3d, bdy_expl3_v3d,              &
                                    bdy_expl3_u2d, bdy_expl3_v2d

use yomhook, only: lhook, dr_hook
use parkind1, only: jprb, jpim

implicit none

! Variables passed in through the argument list...
integer, intent(in) ::                                                         &
  bl_levels

real, intent(out) ::                                                           &
  taux(udims%i_start:udims%i_end,udims%j_start:udims%j_end,0:bl_levels-1),     &
  tauy(vdims%i_start:vdims%i_end,vdims%j_start:vdims%j_end,0:bl_levels-1),     &
  taux_land(udims%i_start:udims%i_end,udims%j_start:udims%j_end),              &
                               ! Taux over land part of grid box.
  taux_ssi(udims%i_start:udims%i_end,udims%j_start:udims%j_end),               &
                               ! Taux over sea part of grid box.
  tauy_land(vdims%i_start:vdims%i_end,vdims%j_start:vdims%j_end),              &
                               ! Tauy over land part of grid box.
  tauy_ssi(vdims%i_start:vdims%i_end,vdims%j_start:vdims%j_end)
                               ! Tauy over sea part of grid box.

! Local variables...
integer :: i, j, k  ! loop indices
character(len=*), parameter ::  RoutineName = 'AP2_RESTORE_BDY_EXPL'
! Dr Hook
!==============================
integer(kind=jpim), parameter :: zhook_in  = 0
integer(kind=jpim), parameter :: zhook_out = 1
real(kind=jprb) :: zhook_handle

if (lhook) call dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! Restore outputs for second EG cycle
! N.B. any new out data added to bdy_expl3 will need to be restored here.
! Array sizes will need to be increased in atmos_physics2_alloc
!$OMP PARALLEL DEFAULT(none) private( i, j, k )                                &
!$OMP SHARED( bl_levels, udims, vdims, bdy_expl3_u3d, taux,                    &
!$OMP         bdy_expl3_v3d, tauy, bdy_expl3_u2d, taux_land,                   &
!$OMP         taux_ssi, bdy_expl3_v2d, tauy_land, tauy_ssi )
!$OMP do SCHEDULE(STATIC)
do k = 0, bl_levels-1
  do j = udims%j_start, udims%j_end
    do i = udims%i_start, udims%i_end
      taux(i,j,k)=bdy_expl3_u3d(i,j,k)
    end do
  end do

  do j = vdims%j_start, vdims%j_end
    do i = vdims%i_start, vdims%i_end
      tauy(i,j,k)=bdy_expl3_v3d(i,j,k)
    end do
  end do
end do
!$OMP end do NOWAIT

!$OMP do SCHEDULE(STATIC)
do j = udims%j_start, udims%j_end
  do i = udims%i_start, udims%i_end
    taux_land(i,j)=bdy_expl3_u2d(i,j,1)
    taux_ssi(i,j)=bdy_expl3_u2d(i,j,2)
  end do
end do
!$OMP end do NOWAIT
!$OMP do SCHEDULE(STATIC)
do j = vdims%j_start, vdims%j_end
  do i = vdims%i_start, vdims%i_end
    tauy_land(i,j)=bdy_expl3_v2d(i,j,1)
    tauy_ssi(i,j)=bdy_expl3_v2d(i,j,2)
  end do
end do
!$OMP end do
!$OMP end PARALLEL

if (lhook) call dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

return
end subroutine ap2_restore_bdy_expl


end module atmos_physics2_save_restore_mod
