! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************
!
!  Global data module for switches/options concerned with the idealised models.

module idealise_run_mod

  ! Description:
  !   Module containing runtime logicals/options used by the idealised code.
  !
  ! Method:
  !   All switches/options which are contained in the &IDEALISED
  !   namelist in the IDEALISE control file are declared in this module.
  !   Default values have been declared where appropriate.
  !
  !   Any routine wishing to use these options may do so with the 'Use'
  !   statement.
  !
  ! Code Owner: Please refer to the UM file CodeOwners.txt
  ! This file belongs in section: Idealised
  !
  ! Code Description:
  !   Language: FORTRAN 90
  !   This code is written to UMDP3 programming standards.
  !
  ! Declarations:

use atmos_max_sizes, only: model_levels_max
use model_domain_mod, only: output_grid_stagger,                               &
                            FH_GridStagger_Endgame

use horiz_grid_mod, only:  Nxi1L, Nxi1V, Nxi2L, Nxi2V,                         &
                            delta_xi1_H, delta_xi1_L,                          &
                            delta_xi2_H, delta_xi2_L

use planet_suite_mod, only: tforce_number, trelax_number,                      &
                            nsteps_consv_print

use profiles_mod,     only: num_data_max, num_time_max, num_prof_max,          &
                            num_theta_relax_times, num_theta_relax_heights,    &
                            num_mv_relax_times, num_mv_relax_heights,          &
                            num_uv_relax_times, num_uv_relax_heights,          &
                            theta_relax_timescale, mv_relax_timescale,         &
                            uv_relax_timescale,                                &
                            theta_relax_height, theta_relax_time,              &
                            theta_relax_data,                                  &
                            mv_relax_height, mv_relax_time,                    &
                            mv_relax_data,                                     &
                            uv_relax_height, uv_relax_time,                    &
                            u_relax_data, v_relax_data,                        &
                            num_theta_inc_times, num_theta_inc_heights,        &
                            theta_inc_field_type,                              &
                            num_mv_inc_times,    num_mv_inc_heights,           &
                            num_uv_inc_times,    num_uv_inc_heights,           &
                            theta_inc_time,      theta_inc_height,             &
                            mv_inc_time,         mv_inc_height,                &
                            uv_inc_time,         uv_inc_height,                &
                            theta_inc_data,      mv_inc_data,                  &
                            u_inc_data,          v_inc_data,                   &
                            num_w_force_times,   num_w_force_heights,          &
                            w_force_time, w_force_height, w_force_data,        &
                            num_uv_geo_times,   num_uv_geo_heights,            &
                            uv_geo_time, uv_geo_height,                        &
                            u_geo_data, v_geo_data


use surface_flux_mod, only: IdlSurfFluxSeaOption, IdlSurfFluxseaParams,        &
                            num_surface_flux_times, surface_flux_time,         &
                            sh_flux, lh_flux, time_varying, num_surf_max,      &
                            IdlSSTOption, num_sst_times, sst_time, sst_data,   &
                            sst_varying


use local_heat_mod,   only: local_heat_option, local_heat_xoffset,             &
                            local_heat_yoffset, local_heat_amp,                &
                            local_heat_sigma, local_heat_base,                 &
                            local_heat_top, local_heat_period, analytic


use missing_data_mod, only: rmdi, imdi
use yomhook,  only: lhook, dr_hook
use parkind1, only: jprb, jpim

implicit none
save

! ROSE guidelines suggest that we should not use default values for
! variables defined below but code will not work in places without them.
! This needs to be revisited.

! Values used to size arrays
integer,parameter:: max_num_profile_data = 1
integer,parameter:: max_num_force_times  = 1

! Non-namelist variables for the idealised model:
logical :: zero_orography = .false.     ! True if all points have zero orog.

! ===============================================>>

! Options that do not belong here (run_dyntest?):
logical :: L_shallow = .false.
logical :: L_const_grav = .true. !.false.

! General idealised options:
logical :: L_vert_Coriolis = .false.
logical :: L_fixed_lbcs = .false.
logical :: L_force_lbc = .false.

! Orography specification:
integer :: grow_steps = imdi

! Surface Characteristics:
real    :: T_surface = rmdi
real    :: p_surface = rmdi
real    :: roughlen_z0m = rmdi
real    :: roughlen_z0h = rmdi
logical :: L_spec_z0 = .false.

! Fixed latitude Coriolis terms:
real    :: f_plane = rmdi
real    :: ff_plane = rmdi

! latitude and lontitude for radiation
real    :: area_latitude = rmdi
real    :: area_longitude = rmdi

! Forcing profiles (New Dynamics only):
integer :: num_tforce_times = imdi
integer :: num_qforce_times = imdi
integer :: num_uvforce_times = imdi
integer :: num_pforce_times = imdi
real    :: tforce_time_interval = rmdi
real    :: qforce_time_interval = rmdi
real    :: uvforce_time_interval = rmdi
real    :: pforce_time_interval = rmdi
real    :: tforce_data(max_num_profile_data, max_num_force_times) = rmdi
real    :: qforce_data(max_num_profile_data, max_num_force_times) = rmdi
real    :: uforce_data(max_num_profile_data, max_num_force_times) = rmdi
real    :: vforce_data(max_num_profile_data, max_num_force_times) = rmdi
real    :: p_surface_data(max_num_force_times) = rmdi

! LBC forcing data
real    :: tforce_data_modlev(model_levels_max, max_num_force_times) = rmdi
real    :: qforce_data_modlev(model_levels_max, max_num_force_times) = rmdi
real    :: uforce_data_modlev(model_levels_max, max_num_force_times) = rmdi
real    :: vforce_data_modlev(model_levels_max, max_num_force_times) = rmdi


logical :: L_geo_for = .false.

! Held-Suarez (New Dynamics Only):
integer :: SuHe_fric = imdi
real    :: SuHe_newtonian_timescale_ka = rmdi
real    :: SuHe_newtonian_timescale_ks = rmdi
real    :: SuHe_pole_equ_deltaT = rmdi
real    :: SuHe_static_stab = rmdi
real    :: SuHe_sigma_cutoff = rmdi
real    :: base_frictional_timescale = rmdi

! Held-Suarez
! All the above parameters are hard coded, may want to re-integrate
! if want proper control of them again.
logical :: L_HeldSuarez = .false.
logical :: L_HeldSuarez1_drag = .false.

! 2d like simulations - Used for controlling local heating
!                     - if .true. then only varies in x direction
!                     - In future may control bubble anomalies as well
!                     - indicates a "2d like" simulation.
logical :: l_ideal_2d = .false.

!----------------------------------------------------------------------------
! Define namelist &IDEALISED read in from IDEALISE control file.
!----------------------------------------------------------------------------

namelist/Idealised/                                                            &

! Options that do not belong here:
L_shallow, L_const_grav,                                                       &

! How do I get rid of these?
Nxi1L, Nxi1V, Nxi2L, Nxi2V,                                                    &
delta_xi1_H, delta_xi1_L, delta_xi2_H, delta_xi2_L,                            &

! General idealised options:
L_vert_Coriolis, L_fixed_lbcs, L_force_lbc,                                    &

! Surface Characteristics:
T_surface, p_surface, L_spec_z0, roughlen_z0m, roughlen_z0h,                   &

! Initial profiles:
f_plane, ff_plane,                                                             &
area_latitude, area_longitude,                                                 &

! Other forcing options:
IdlSurfFluxSeaOption, IdlSurfFluxSeaParams, IdlSSTOption,                      &
L_geo_for,                                                                     &
num_uv_geo_times, num_uv_geo_heights, uv_geo_height, uv_geo_time,              &
u_geo_data, v_geo_data,                                                        &

! Idealised forcing
num_theta_relax_heights, num_theta_relax_times, num_mv_relax_heights,          &
num_mv_relax_times, num_uv_relax_heights, num_uv_relax_times,                  &
theta_relax_timescale, mv_relax_timescale, uv_relax_timescale,                 &
theta_relax_height, theta_relax_time, theta_relax_data,                        &
mv_relax_height, mv_relax_time, mv_relax_data,                                 &
uv_relax_height, uv_relax_time, u_relax_data, v_relax_data,                    &
num_theta_inc_times, num_theta_inc_heights, theta_inc_field_type,              &
num_mv_inc_times, num_mv_inc_heights, num_uv_inc_times, num_uv_inc_heights,    &
theta_inc_time, theta_inc_height, mv_inc_time, mv_inc_height,                  &
uv_inc_time, uv_inc_height, theta_inc_data, mv_inc_data,                       &
u_inc_data, v_inc_data,                                                        &
num_w_force_times, num_w_force_heights, w_force_time, w_force_height,          &
w_force_data,                                                                  &
num_surface_flux_times, surface_flux_time, sh_flux, lh_flux,                   &
num_sst_times, sst_time, sst_data,                                             &

! 2d like simulation
l_ideal_2d,                                                                    &

! local heating or 2d like
local_heat_option, local_heat_xoffset, local_heat_yoffset, local_heat_amp,     &
local_heat_sigma, local_heat_base, local_heat_top, local_heat_period,          &

! Planetary modelling options:
tforce_number, trelax_number,                                                  &
nsteps_consv_print,                                                            &

! Held-Suarez (New Dynamics only):
SuHe_newtonian_timescale_ka, SuHe_newtonian_timescale_ks,                      &
SuHe_pole_equ_deltaT, SuHe_static_stab,                                        &
SuHe_sigma_cutoff, SuHe_fric, base_frictional_timescale,                       &

! Held-Suarez
L_HeldSuarez, L_HeldSuarez1_drag

!----------------------------------------------------------------------------

!DrHook-related parameters
integer(kind=jpim), parameter, private :: zhook_in  = 0
integer(kind=jpim), parameter, private :: zhook_out = 1

character(len=*), parameter, private :: ModuleName='IDEALISE_RUN_MOD'

contains
subroutine check_nlist_idealised()

use dynamics_testing_mod,   only: problem_number
use problem_mod,            only: idealised_problem
use chk_opts_mod, only: chk_var, def_src
use bl_option_mod, only: flux_bc_opt,interactive_fluxes

use umPrintMgr, only: umprint, ummessage
implicit none

character(len=*), parameter       :: RoutineName = 'CHECK_NLIST_IDEALISED'
real(kind=jprb)                   :: zhook_handle

if (lhook) call dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)
def_src = RoutineName

! Only check if running with specified fluxes
if (flux_bc_opt /= interactive_fluxes) then
  ! Surface forcing
  call chk_var(IdlSurfFluxSeaOption,'IdlSurfFluxSeaOption','[1:5]')

  if (IdlSurfFluxSeaOption == time_varying) then
    call chk_var(num_surface_flux_times,'num_surface_flux_times','[1:100]')
    ! Not checking data arrays as could be a mix of values and missing data
  end if

end if

! Only check if running an idealised problem i.e. number 3
if (problem_number == idealised_problem) then

  ! latitude and longitude
  call chk_var(area_latitude,'area_latitude','[-90.0:90.0]')
  call chk_var(area_longitude,'area_longitude','[0.0:360.0]')

  ! Local heating option
  call chk_var(local_heat_option,'local_heat_option','[0,1]')

  ! Check local heating variables if option 1 (analytic) or above chosen
  ! Limits same as in meta-data
  if (local_heat_option >= analytic) then
    call chk_var(local_heat_xoffset,'local_heat_xoffset','[0.0:1.0]')
    if (.not. l_ideal_2d) then  ! not checking if 2d like
      call chk_var(local_heat_yoffset,'local_heat_yoffset','[0.0:1.0]')
    end if
    call chk_var(local_heat_amp,'local_heat_amp','[0.0:1000.0]')
    call chk_var(local_heat_sigma,'local_heat_sigma','[0.0:10000.0]')
    call chk_var(local_heat_base,'local_heat_base','[0.0:20000.0]')
    call chk_var(local_heat_top,'local_heat_top','[0.0:80000.0]')
    call chk_var(local_heat_period,'local_heat_period','[0.0:86400.0]')
  end if

  ! Subsidence forcing - limited by array dimension
  call chk_var(num_w_force_times,'num_w_force_times','[0:100]')

  ! Only check subsidence forcing if it is being used.
  if (num_w_force_times > 0) then
    ! Number of values restricted by array size
    call chk_var(num_w_force_heights,'num_w_force_heights','[0:100]')
    ! Check that number of heights * number of times is not greater >100
    if (num_w_force_heights*num_w_force_times > 100) then
      write(umMessage,'(A,I0)')                                                &
      'Array size is limited to 100, you need a branch if you want ',          &
       num_w_force_heights*num_w_force_times
      call umPrint(umMessage,src=RoutineName)
    end if
    ! Not checking data arrays as can have valid missing data values if
    ! using only part of the input array.
  end if

  ! Time and vertical-varying geostrophic forcing
  if (num_uv_geo_times > 0) then
    ! Number of values restricted by array size
    call chk_var(num_uv_geo_heights,'num_w_force_heights','[0:100]')
    ! Check that number of heights * number of times is not greater >100
    if (num_uv_geo_heights*num_uv_geo_times > 100) then
      write(umMessage,'(A,I0)')                                                &
      'Array size is limited to 100, you need a branch if you want ',          &
       num_w_force_heights*num_w_force_times
      call umPrint(umMessage,src=RoutineName)
    end if
    ! Not checking data arrays as can have valid missing data values if
    ! using only part of the input array.
  end if

  if (IdlSSTOption == 2) then
    ! Number of values restricted by array size
    call chk_var(num_sst_times,'num_sst_times','[0:100]')

  end if


end if

def_src = ''
if (lhook) call dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

end subroutine check_nlist_idealised

subroutine print_nlist_idealised()
use umPrintMgr, only: umPrint
implicit none
integer :: i
character(len=50000) :: lineBuffer
real(kind=jprb) :: zhook_handle

character(len=*), parameter :: RoutineName='PRINT_NLIST_IDEALISED'
integer :: k

if (lhook) call dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

call umPrint('Contents of namelist idealised',                                 &
    src='idealise_run_mod')

! Options that do not belong here:
write(lineBuffer,*)' L_shallow = ',L_shallow
call umPrint(lineBuffer,src='idealise_run_mod')
write(lineBuffer,*)' L_const_grav = ',L_const_grav
call umPrint(lineBuffer,src='idealise_run_mod')

! How do I get rid of these?
write(lineBuffer,*)' Nxi1L = ',Nxi1L
call umPrint(lineBuffer,src='idealise_run_mod')
write(lineBuffer,*)' Nxi1V = ',Nxi1V
call umPrint(lineBuffer,src='idealise_run_mod')
write(lineBuffer,*)' Nxi2L = ',Nxi2L
call umPrint(lineBuffer,src='idealise_run_mod')
write(lineBuffer,*)' Nxi2V = ',Nxi2V
call umPrint(lineBuffer,src='idealise_run_mod')
write(lineBuffer,*)' delta_xi1_H = ',delta_xi1_H
call umPrint(lineBuffer,src='idealise_run_mod')
write(lineBuffer,*)' delta_xi1_L = ',delta_xi1_L
call umPrint(lineBuffer,src='idealise_run_mod')
write(lineBuffer,*)' delta_xi2_H = ',delta_xi2_H
call umPrint(lineBuffer,src='idealise_run_mod')
write(lineBuffer,*)' delta_xi2_L = ',delta_xi2_L
call umPrint(lineBuffer,src='idealise_run_mod')

! General idealised options:
write(lineBuffer,*)' L_vert_Coriolis = ',L_vert_Coriolis
call umPrint(lineBuffer,src='idealise_run_mod')
write(lineBuffer,*)' L_fixed_lbcs = ',L_fixed_lbcs
call umPrint(lineBuffer,src='idealise_run_mod')
write(lineBuffer,*)' L_force_lbc = ',L_force_lbc
call umPrint(lineBuffer,src='idealise_run_mod')

! Surface Characteristics:
write(lineBuffer,*)' T_surface = ',T_surface
call umPrint(lineBuffer,src='idealise_run_mod')
write(lineBuffer,*)' p_surface = ',p_surface
call umPrint(lineBuffer,src='idealise_run_mod')
write(lineBuffer,*)' L_spec_z0 = ',L_spec_z0
call umPrint(lineBuffer,src='idealise_run_mod')
write(lineBuffer,*)' roughlen_z0m = ',roughlen_z0m
call umPrint(lineBuffer,src='idealise_run_mod')
write(lineBuffer,*)' roughlen_z0h = ',roughlen_z0h
call umPrint(lineBuffer,src='idealise_run_mod')

! Initial profiles:
write(lineBuffer,*)' f_plane = ',f_plane
call umPrint(lineBuffer,src='idealise_run_mod')
write(lineBuffer,*)' ff_plane = ',ff_plane
call umPrint(lineBuffer,src='idealise_run_mod')

! Lat long:
write(lineBuffer,'(A,F9.3)')' area_latitude  = ',area_latitude
call umPrint(lineBuffer,src='idealise_run_mod')
write(lineBuffer,'(A,F9.3)')' area_longitude = ',area_longitude
call umPrint(lineBuffer,src='idealise_run_mod')

! Other forcing options:
write(lineBuffer,*)' IdlSurfFluxSeaOption = ',IdlSurfFluxSeaOption
call umPrint(lineBuffer,src='idealise_run_mod')
write(lineBuffer,*)' IdlSurfFluxSeaParams = ',IdlSurfFluxSeaParams
call umPrint(lineBuffer,src='idealise_run_mod')
! Time varying surface fluxes
if (IdlSurfFluxSeaOption ==  time_varying) then
  write(lineBuffer,'(A,I0)')' num_surface_flux_times = ',                      &
                              num_surface_flux_times
  call umPrint(lineBuffer,src='idealise_run_mod')
  write(lineBuffer,'(A)')' I    time (s)   sh (W/m2)  lh (W/m2)'
  call umPrint(lineBuffer,src='idealise_run_mod')

  do i = 1,  num_surface_flux_times
    write(lineBuffer,'(I4,F16.0,2F16.3)') i,surface_flux_time(i),              &
                                        sh_flux(i),lh_flux(i)
    call umPrint(lineBuffer,src='idealise_run_mod')
  end do
end if

write(lineBuffer,'(A,I6)')' IdlSSTOption = ',IdlSSTOption
call umPrint(lineBuffer,src='idealise_run_mod')
if (IdlSSTOption ==  sst_varying) then
  write(lineBuffer,'(A,I0)')' num_sst_times = ', num_sst_times
  call umPrint(lineBuffer,src='idealise_run_mod')
  do i = 1,  num_sst_times
    write(lineBuffer,'(I4,F16.0,F16.3)') i,sst_time(i),sst_data(i)
    call umPrint(lineBuffer,src='idealise_run_mod')
  end do
end if

write(lineBuffer,*)' L_geo_for = ',L_geo_for
call umPrint(lineBuffer,src='idealise_run_mod')

! Idealised forcing profiles
write(lineBuffer,'(A,I0)')' num_theta_relax_heights = ',                       &
                            num_theta_relax_heights
call umPrint(lineBuffer,src='idealise_run_mod')
write(lineBuffer,'(A,I0)')' num_theta_relax_times = ',                         &
                            num_theta_relax_times
call umPrint(lineBuffer,src='idealise_run_mod')
write(lineBuffer,'(A,I0)')' num_mv_relax_heights = ',                          &
                            num_mv_relax_heights
call umPrint(lineBuffer,src='idealise_run_mod')
write(lineBuffer,'(A,I0)')' num_mv_relax_times = ',                            &
                            num_mv_relax_times
call umPrint(lineBuffer,src='idealise_run_mod')
write(lineBuffer,'(A,I0)')' num_uv_relax_heights = ',                          &
                            num_uv_relax_heights
call umPrint(lineBuffer,src='idealise_run_mod')
write(lineBuffer,'(A,I0)')' num_uv_relax_times = ',                            &
                            num_uv_relax_times
call umPrint(lineBuffer,src='idealise_run_mod')
write(lineBuffer,'(A,I0)')' num_theta_inc_heights = ',                         &
                            num_theta_inc_heights
call umPrint(lineBuffer,src='idealise_run_mod')
write(lineBuffer,'(A,I0)')' num_theta_inc_times = ',                           &
                            num_theta_inc_times
call umPrint(lineBuffer,src='idealise_run_mod')
write(lineBuffer,'(A,I0)')' theta_inc_field_type = ',                          &
                            theta_inc_field_type
call umPrint(lineBuffer,src='idealise_run_mod')
write(lineBuffer,'(A,I0)')' num_mv_inc_heights = ',                            &
                            num_mv_inc_heights
call umPrint(lineBuffer,src='idealise_run_mod')
write(lineBuffer,'(A,I0)')' num_mv_inc_times = ',                              &
                            num_mv_inc_times
call umPrint(lineBuffer,src='idealise_run_mod')
write(lineBuffer,'(A,I0)')' num_uv_inc_heights = ',                            &
                            num_uv_inc_heights
call umPrint(lineBuffer,src='idealise_run_mod')
write(lineBuffer,'(A,I0)')' num_uv_inc_times = ',                              &
                            num_uv_inc_times
call umPrint(lineBuffer,src='idealise_run_mod')
write(lineBuffer,'(A,I0)')' num_w_force_times = ',                             &
                            num_w_force_times
call umPrint(lineBuffer,src='idealise_run_mod')
write(lineBuffer,'(A,I0)')' num_w_force_heights = ',                           &
                            num_w_force_heights
call umPrint(lineBuffer,src='idealise_run_mod')
if (num_w_force_times > 0) then
  write(lineBuffer,'(A)') ' k height w_force_data'
  call umPrint(lineBuffer,src='idealise_run_mod')
  do k=1,num_w_force_heights
    write(lineBuffer,'(I4,F10.1,F12.6)') k, w_force_height(k), w_force_data(k)
    call umPrint(lineBuffer,src='idealise_run_mod')
  end do
end if

! Time and vertical- varying geostrophic forcing
if (num_uv_geo_times > 0) then
  write(lineBuffer,'(A)') ' k height u_geo_data'
  call umPrint(lineBuffer,src='idealise_run_mod')
  do k=1,num_uv_geo_heights
    write(lineBuffer,'(I4,F10.1,F12.6,F12.6)') k, uv_geo_height(k),            &
                       u_geo_data(k), v_geo_data(k)
    call umPrint(lineBuffer,src='idealise_run_mod')
  end do
end if
! Idealised forcing data arrays not printed, as they can be very large

! 2d like
write(lineBuffer,'(a,l1)')' l_ideal_2d = ',l_ideal_2d
call umPrint(lineBuffer,src='idealise_run_mod')
write(lineBuffer,'(a,i12)')' local_heat_option = ',local_heat_option
call umPrint(lineBuffer,src='idealise_run_mod')
! Only print local heating settings if in use.
! Already checked values within known ranges so can use format statements.
if (local_heat_option > 0) then
  write(lineBuffer,'(a,f7.4)')' local_heat_xoffset = ',local_heat_xoffset
  call umPrint(lineBuffer,src='idealise_run_mod')
  if (.not. l_ideal_2d) then  ! local_heat_yoffset not used
    write(lineBuffer,'(a,f7.4)')' local_heat_yoffset = ',local_heat_yoffset
    call umPrint(lineBuffer,src='idealise_run_mod')
  end if
  write(lineBuffer,'(a,f8.2)')' local_heat_amp = ',local_heat_amp
  call umPrint(lineBuffer,src='idealise_run_mod')
  write(lineBuffer,'(a,f10.2)')' local_heat_sigma = ',local_heat_sigma
  call umPrint(lineBuffer,src='idealise_run_mod')
  write(lineBuffer,'(a,f10.2)')' local_heat_base = ',local_heat_base
  call umPrint(lineBuffer,src='idealise_run_mod')
  write(lineBuffer,'(a,f10.2)')' local_heat_top = ',local_heat_top
  call umPrint(lineBuffer,src='idealise_run_mod')
  write(lineBuffer,'(a,f8.0)')' local_heat_period = ',local_heat_period
  call umPrint(lineBuffer,src='idealise_run_mod')
end if

! Planetary modelling options:
write(lineBuffer,*)'tforce_number = ',tforce_number
call umPrint(lineBuffer,src='idealise_run_mod')
write(lineBuffer,*)'trelax_number = ',trelax_number
call umPrint(lineBuffer,src='idealise_run_mod')
write(lineBuffer,*)'nsteps_consv_print = ',nsteps_consv_print
call umPrint(lineBuffer,src='idealise_run_mod')

! Held-Suarez (New Dynamics only):
write(lineBuffer,*)' SuHe_newtonian_timescale_ka = ',                          &
                                                SuHe_newtonian_timescale_ka
call umPrint(lineBuffer,src='idealise_run_mod')
write(lineBuffer,*)' SuHe_newtonian_timescale_ks = ',                          &
                                                SuHe_newtonian_timescale_ks
call umPrint(lineBuffer,src='idealise_run_mod')
write(lineBuffer,*)' SuHe_pole_equ_deltaT = ',SuHe_pole_equ_deltaT
call umPrint(lineBuffer,src='idealise_run_mod')
write(lineBuffer,*)' SuHe_static_stab = ',SuHe_static_stab
call umPrint(lineBuffer,src='idealise_run_mod')
write(lineBuffer,*)' SuHe_sigma_cutoff = ',SuHe_sigma_cutoff
call umPrint(lineBuffer,src='idealise_run_mod')
write(lineBuffer,*)' SuHe_fric = ',SuHe_fric
call umPrint(lineBuffer,src='idealise_run_mod')
write(lineBuffer,*)' base_frictional_timescale = ',base_frictional_timescale
call umPrint(lineBuffer,src='idealise_run_mod')

! Held-Suarez
write(lineBuffer,*)' L_HeldSuarez = ',L_HeldSuarez
call umPrint(lineBuffer,src='idealise_run_mod')
write(lineBuffer,*)' L_HeldSuarez1_drag = ',L_HeldSuarez1_drag
call umPrint(lineBuffer,src='idealise_run_mod')

call umPrint('- - - - - - end of namelist - - - - - -',                        &
    src='idealise_run_mod')

if (lhook) call dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

end subroutine print_nlist_idealised


end module idealise_run_mod
