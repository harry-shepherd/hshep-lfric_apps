! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!  PE_Helmholtz solver parameters.

module gcr_input_mod

use missing_data_mod, only: rmdi, imdi

implicit none

! Description:
!          PE_Helmholtz parameters constant for run
!          set by namelist input
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: dynamics_solver
!
! Code Description:
!   Language: FORTRAN 90
!   This code is written to UMDP3 v6 programming standards.

!----------------------------------------------
! the following are not read in by a namelist and fixed
!----------------------------------------------

! Speed up solver convergence when dynamics-physics cycling is used.
! not used in any jobs as FAST is slow in real jobs.
! true use 'faster' non reproducible code
logical, parameter :: L_gcr_fast_x        = .false.
logical, parameter :: GCR_zero_init_guess = .true. ! True if initial guess zero

integer, parameter ::  max_numcycles = 5  ! Max number of dynamics cycles
integer, parameter ::  GCR_Restart_value  = 2
                                 ! No. of iterations before restarts

! Switch controlling diagnostic output.
integer, parameter ::  no_GCR_diags = 0        ! 0 = none
integer, parameter ::  initial_and_final = 1   ! 1 = initial and final residuals
integer, parameter ::  ini_fin_itr= 2          ! 2 = all
integer, parameter ::  iteration_count= 3      ! 3 = iteration count processing

real, parameter    ::  G_term_tol  = 0.900     ! tolerance for vertical G term

!----------------------------------------------
! the following are not read in by a namelist but
! are set dependent upon othe namelist inputs.
!----------------------------------------------
logical  ::  GCR_use_tol_abs  = .false.
integer  ::  GCR_n_ADI_pseudo_timesteps = imdi
                                 ! Number of ADI pseudo timesteps to perform.

integer  ::  GCR_its_switch(max_numcycles) = imdi ! Iterations analysis switch
integer  ::  GCR_max_its(max_numcycles)    = imdi ! Max its this period
integer  ::  GCR_min_its(max_numcycles)    = imdi ! Min its this period
integer  ::  GCR_max_time(max_numcycles)   = imdi ! max timestep number
integer  ::  GCR_min_time(max_numcycles)   = imdi ! min timestep number
integer  ::  GCR_sum_its(max_numcycles)    = imdi ! Sum its over period

real  ::  GCR_tol_res = rmdi
real  ::  GCR_tol_res2= rmdi
real  ::  GCR_tol_abs = rmdi
real  ::  GCR_tol_abs2= rmdi

!----------------------------------------------
! the following are read in by a namelist:
! either run_dyn or run_dyntest
!----------------------------------------------
logical  :: L_GCR_cycle_opt       = .false. ! (To be retired)
logical  :: GCR_use_residual_tol  = .false. ! (To be retired)
logical  :: GCR_adi_add_full_soln = .false. ! (To be retired)

integer  ::  GCR_max_iterations   = imdi
integer  ::  GCR_precon_option    = imdi
integer  ::  GCR_Diagnostics      = initial_and_final  ! Initial and final norms

real  ::  GCR_tol                 = rmdi
real  ::  GCR_tol2                = rmdi    ! (To be retired)
real  ::  GCR_ADI_pseudo_timestep = rmdi    ! (To be retired)


end module gcr_input_mod
