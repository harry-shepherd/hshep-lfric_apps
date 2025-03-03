! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
! Input control for dynamics.

! Description:
!   Module containing input switches/settings and the check_run_dyntest
!   routine for logic checking the selected settings. Used by the
!   dynamics to enable testing and some idealised running.
!   Each switch would usually not be used by the everyday user
!   and is defaulted to the common setting.
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: Dynamics
!
! Method:
!   Switches are initialised to false and read in from the
!   namelists. The module may then be used directly where the switches
!   are needed within the dynamics code.

! Code Description:
!   Language: FORTRAN 90
!   This code is written to UMDP3

module dynamics_testing_mod

use errormessagelength_mod, only: errormessagelength
use gcr_input_mod,          only: GCR_Diagnostics
use missing_data_mod,       only: rmdi, imdi
use var_end_mod,            only: lambda_p_end, phi_p_end, lambda_u_end,       &
                                  phi_v_end, dlambda_p_end, dphi_p_end,        &
                                  dlambda_u_end, dphi_v_end
use var_input_mod,          only: lam_var, phi_var, var_ratio, lam_ratio,      &
                                  phi_ratio, phi_frac, lam_frac

use yomhook,  only: lhook, dr_hook
use parkind1, only: jprb, jpim

implicit none

! ===============================================
logical :: L_adjust_wet   = .false.           ! (To be retired)
logical :: L_free_slip    = .false.           ! (To be retired)
real    :: uv_limit       = rmdi               ! (To be retired)
integer :: trap_option    = imdi               ! (To be retired)
logical :: L_trap_theta   = .false.           ! (To be retired)
logical :: L_trap_w       = .false.           ! (To be retired)
real    :: Cw_max         = rmdi               ! (To be retired)
integer :: Cw_test_lev    = imdi               ! (To be retired)
real    :: max_thinc      = rmdi               ! (To be retired)
! ===============================================

! the following general users need not alter.
! power users may like to alter these via the namelist run_dyntest

logical :: L_Physics           = .false.  ! T: physics to be included
logical :: L_dynamics_only     = .false.  ! T: run without physics
logical :: L_Run_With_Physics2 = .false.  ! T: physics2 to be included
logical :: L_exclude_Physics2  = .false.  ! T: physics2 to be excluded
logical :: L_perturb_IC_theta  = .false.  ! T: perturb theta on ts1
logical :: L_Backwards         = .false.  ! F Integrate backwards without
                                        ! physics
logical :: L_dry               = .false.  ! T run with no moisture

logical :: L_idealised_data    = .false.  ! T run idealised problem

logical :: L_hydrostatic_EG    = .false.  ! choose to run hydrostatic in EG.

logical :: L_prognostic_level0 = .false.  ! F: Use constant extrapolation
                                          !    in vertical for thetavd and
                                          !    moisture on level 0.

! trapping
logical :: L_trap_uv    = .false.  ! .true. trap excessive u,v

! Type of problem to be solved
integer :: problem_number = imdi
! Seed for random numbers used to perturb initial condition
! cf. L_perturb_IC_theta
integer :: IntRand_seed = imdi

namelist/RUN_Dyntest/                                                          &
    L_dynamics_only, L_Backwards, L_hydrostatic_EG,                            &
    L_exclude_Physics2, L_perturb_IC_theta,                                    &
    L_dry,                                                                     &
    L_trap_uv, L_idealised_data,                                               &
    GCR_Diagnostics,                                                           &
    lambda_p_end,  phi_p_end, lambda_u_end,  phi_v_end,                        &
    dlambda_p_end,  dphi_p_end, dlambda_u_end,  dphi_v_end,                    &
    lam_var, phi_var,                                                          &
    var_ratio, lam_ratio, phi_ratio, lam_frac, phi_frac,                       &
    problem_number, IntRand_seed

! DrHook-related parameters
integer(kind=jpim), parameter, private :: zhook_in  = 0
integer(kind=jpim), parameter, private :: zhook_out = 1

character(len=*), parameter, private :: ModuleName='DYNAMICS_TESTING_MOD'

contains

subroutine check_run_dyntest()

! Description:
!   Subroutine to apply logic checks and set variables based on the
!   options selected in the run_dyntest namelist.

use dynamics_input_mod,     only: Ih

implicit none

real(kind=jprb)               :: zhook_handle

character(len=*), parameter :: RoutineName='CHECK_RUN_DYNTEST'

if (lhook) call dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! ---------------------------------------------
! Dynamics logic control - New Dynamics and ENDGAME
! ---------------------------------------------

if (l_dynamics_only) then
  l_physics           = .false.
else
  l_physics           = .true.
end if

if (l_exclude_physics2) then
  l_run_with_physics2 = .false.
else
  l_run_with_physics2 = .true.
end if

if (L_Backwards) L_Physics = .false.

! ---------------------------------------------
! Dynamics logic control - ENDGAME Only
! ---------------------------------------------

if (l_hydrostatic_EG) then
  Ih =0.0
else
  Ih =1.0
end if

if (lhook) call dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
return
end subroutine check_run_dyntest

subroutine print_nlist_run_dyntest()
use umPrintMgr, only: umPrint
implicit none
character(len=50000) :: lineBuffer
real(kind=jprb)               :: zhook_handle

character(len=*), parameter :: RoutineName='PRINT_NLIST_RUN_DYNTEST'

if (lhook) call dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

call umPrint('Contents of namelist run_dyntest',                               &
    src='dynamics_testing_mod')

write(lineBuffer,'(''L_dynamics_only = '',L1)') L_dynamics_only
call umPrint(lineBuffer,src='dynamics_testing_mod')
write(lineBuffer,'(''L_Backwards = '',L1)') L_Backwards
call umPrint(lineBuffer,src='dynamics_testing_mod')
write(lineBuffer,'(''L_hydrostatic_EG = '',L1)') L_hydrostatic_EG
call umPrint(lineBuffer,src='dynamics_testing_mod')
write(lineBuffer,'(''L_exclude_Physics2 = '',L1)') L_exclude_Physics2
call umPrint(lineBuffer,src='dynamics_testing_mod')
write(lineBuffer,'(''L_perturb_IC_theta = '',L1)') L_perturb_IC_theta
call umPrint(lineBuffer,src='dynamics_testing_mod')
write(lineBuffer,'(''L_dry = '',L1)') L_dry
call umPrint(lineBuffer,src='dynamics_testing_mod')
write(lineBuffer,'(''L_trap_uv = '',L1)') L_trap_uv
call umPrint(lineBuffer,src='dynamics_testing_mod')
write(lineBuffer,'(''L_idealised_data = '',L1)') L_idealised_data
call umPrint(lineBuffer,src='dynamics_testing_mod')
write(lineBuffer,'(''GCR_Diagnostics = '',I0)') GCR_Diagnostics
call umPrint(lineBuffer,src='dynamics_testing_mod')
write(lineBuffer,'(''lambda_p_end = '',E15.7)') lambda_p_end
call umPrint(lineBuffer,src='dynamics_testing_mod')
write(lineBuffer,'(''phi_p_end = '',E15.7)') phi_p_end
call umPrint(lineBuffer,src='dynamics_testing_mod')
write(lineBuffer,'(''lambda_u_end = '',E15.7)') lambda_u_end
call umPrint(lineBuffer,src='dynamics_testing_mod')
write(lineBuffer,'(''phi_v_end = '',E15.7)') phi_v_end
call umPrint(lineBuffer,src='dynamics_testing_mod')
write(lineBuffer,'(''dlambda_p_end = '',E15.7)') dlambda_p_end
call umPrint(lineBuffer,src='dynamics_testing_mod')
write(lineBuffer,'(''dphi_p_end = '',E15.7)') dphi_p_end
call umPrint(lineBuffer,src='dynamics_testing_mod')
write(lineBuffer,'(''dlambda_u_end = '',E15.7)') dlambda_u_end
call umPrint(lineBuffer,src='dynamics_testing_mod')
write(lineBuffer,'(''dphi_v_end = '',E15.7)') dphi_v_end
call umPrint(lineBuffer,src='dynamics_testing_mod')
write(lineBuffer,'(''lam_var = '',I0)') lam_var
call umPrint(lineBuffer,src='dynamics_testing_mod')
write(lineBuffer,'(''phi_var = '',I0)') phi_var
call umPrint(lineBuffer,src='dynamics_testing_mod')
write(lineBuffer,'(''var_ratio = '',E15.7)') var_ratio
call umPrint(lineBuffer,src='dynamics_testing_mod')
write(lineBuffer,'(''lam_ratio = '',E15.7)') lam_ratio
call umPrint(lineBuffer,src='dynamics_testing_mod')
write(lineBuffer,'(''phi_ratio = '',E15.7)') phi_ratio
call umPrint(lineBuffer,src='dynamics_testing_mod')
write(lineBuffer,'(''lam_frac = '',E15.7)') lam_frac
call umPrint(lineBuffer,src='dynamics_testing_mod')
write(lineBuffer,'(''phi_frac = '',E15.7)') phi_frac
call umPrint(lineBuffer,src='dynamics_testing_mod')
write(lineBuffer,'(''problem_number = '',I0)') problem_number
call umPrint(lineBuffer,src='dynamics_testing_mod')
write(lineBuffer,'(''IntRand_seed = '',I0)') IntRand_seed
call umPrint(lineBuffer,src='dynamics_input_mod')

call umPrint('- - - - - - end of namelist - - - - - -',                        &
    src='dynamics_testing_mod')

if (lhook) call dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

end subroutine print_nlist_run_dyntest


end module dynamics_testing_mod
