! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
! Input control for dynamics.

! Description:
!   Module containing input switches/settings as used by the
!   semi_lagrangian code and the check_run_dyn routine for logic
!   checking the selected settings.
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: Dynamics

! Method:
!   Switches are initialised to false and read in from the
!   namelists. The module may then be used directly where the switches
!   are needed within the dynamics code.

! Code Description:
!   Language: FORTRAN 90
!   This code is written to UMDP3

module dynamics_input_mod

use eg_alpha_ramp_mod,      only: alpha_relax_type
use errormessagelength_mod, only: errormessagelength
use gcr_input_mod, only:                                                       &
    L_GCR_cycle_opt,GCR_zero_init_guess,GCR_use_residual_tol,                  &
    GCR_adi_add_full_soln,GCR_use_tol_abs,L_gcr_fast_x,                        &
    GCR_max_iterations,GCR_diagnostics,GCR_precon_option,                      &
    GCR_n_ADI_pseudo_timesteps,GCR_Restart_value,GCR_tol_res,                  &
    GCR_tol,GCR_tol_res2,GCR_tol_abs,GCR_tol_abs2,GCR_tol2,                    &
    GCR_ADI_pseudo_timestep,G_term_tol
use lbc_input_mod,          only: L_LBC_balance
use missing_data_mod,       only: rmdi, imdi

use yomhook,  only: lhook, dr_hook
use parkind1, only: jprb, jpim
use helmholtz_const_matrix_mod, only:                                          &
    solver_precision, l_tri_sor_vl

use imbnd_data, only: l_ib_noslip, l_use_ib_mask, ib_filename,                 &
                      ib_alpha => alpha, ib_beta => beta,                      &
                      ib_damping => damping
implicit none

! ------------------------------------------------
! Parameters not read in by the run_dyn namelist.
! ------------------------------------------------


! T: reset polar values to mean value every polar_reset_timesteps
logical, parameter :: L_polar_reset   = .false.
logical, parameter :: L_interp_depart = .false.
                                      ! interpolate to find u,v departure pts
logical, parameter :: L_qwaterload    = .true.
                                      ! true if using adding waterloading terms
logical, parameter :: L_fint_theta    = .false. ! true:  fully-interpolating
                                      ! semi-lagrangian theta advection will be
                                      ! used false: standard non-interpolating
                                      ! in the vertical

! interval for resetting polar to mean
integer, parameter :: polar_reset_timesteps = 1

real, parameter :: tol_sc_fact  = 1.0      ! linear solver scaling factor
real, parameter :: T_surf_ref   = 290.0    ! SI Reference surface temperature
real, parameter :: p_surf_ref   = 1.0e5    ! SI Reference surface pressure
real, parameter :: extrp_weight = 1.5      ! time interpolation weight for first
                                           ! SL iteration.
real, parameter :: eccentricity = 0.0      ! Eccentricity of planet, considered
                                           ! as an oblate spheroid

integer :: NumCycles    = 2    !  Parameters for dynamics-physics cycling
                               ! only valid value for ENDGame.

! This logical is used to activate the Immersed Boundary Method (IBM)
! implimentation used to represent orography via a feedback forcing
! mechanism.

logical :: l_use_ib_method = .false.

! ===============================================
logical :: L_new_tdisc    = .false.                      ! (To be retired)
! ===============================================

! Multigrid related parameters

! Solver to use (2=multigrid, 3 = BiCGStab preconditioned with Multigrid)
integer, parameter :: solver_multigrid = 2
integer, parameter :: solver_bicgstab_multigrid = 3

! -------------------------------
! Items read in by run_dyn namelist.
! -------------------------------

! ENDGAME only namelist items
logical :: l_dry_static_adjust  =.false. ! Enforces static stability on
                                         ! theta field at end of outer loop

logical :: L_inc_solver         =.false. ! Flag for incremental solver
logical :: L_conserv_smooth_lap =.false. ! to use the option of redistributing
                                          ! mass based on only the Laplacian in
                                          ! the mass conservation routine

logical :: L_accel_convergence  =.false. ! accelerated convergence switches
logical :: L_init_Fnm1          =.false. ! Fnm1 initialisation
logical :: l_fast_vert_adjust   =.false. ! Switch to use simplified
                                         ! adjust_vert_bound code
logical :: l_sl_bc_correction   =.false. ! Flag for using corrected treatment
                                         ! of upper and lower boundaries in
                                         ! semi-Lagrangian advection scheme.
logical :: l_advect_log_theta = .false.  ! Flag for advecting log(theta)

! Logicals and settings for the multigrid scheme

logical :: l_use_mg = .false.         ! use the multigrid solver
logical :: semicoarsening = .true.    ! use semicoarsening for the lat-lon grid?
logical :: full_multigrid = .false.   ! Start multigrid iteration by FMG cycle

real    :: relaxation_factor = rmdi     ! Overrelaxation factor
real    :: relaxation_factor_fmg = rmdi ! Overrelaxation factor for FMG

real    :: mg_tolerance = rmdi          ! multigrid solution tolerance

! Number of steps for pre- and post-smoothing and on the coarsest level
integer :: n_presmooth = imdi
integer :: n_postsmooth = imdi
integer :: n_presmooth_fine = imdi
integer :: n_postsmooth_fine = imdi
integer :: n_coarsesmooth = imdi

integer :: maxiter = imdi        ! Maximum number of iterations
integer :: maxlevel = imdi       ! Maximum number of multigrid levels
integer :: norm_type = imdi      ! Type of norm (1=L2_norm, 2= Linf_norm)
integer :: solver = imdi         ! Solver to use (2 = Multigrid,
                                 ! 3 = BiCGStab preconditioned with Multigrid)
integer :: coarsegrid_solver = imdi ! Solver to use on coarsest grid
                                 !  (1 = smoother, 2 = BiCGStab)


! Logicals and options for ZLF scheme
! also ensure there is error checking if needed on these variables
! in chk_run_dyn
!
! The following logicals/integer parameters are for controlling the ZLF
! scheme to restore mass conservation at the advection stage.
!
! L_conservation_(*)_zlf=T/F = use/not ZLF scheme for (*) variables
!
! zlf_conservation_(*)_option number specifies the mass conservation
! scheme to be used with the ZLF scheme
! zlf_conservation_(*)_option = 0 (ZLF consevation off )
! zlf_conservation_(*)_option = 1 (OCF scheme )
! zlf_conservation_(*)_option = 2 (ADAS scheme)
! .
integer, parameter :: apply_ocf  = 1
integer, parameter :: apply_adas = 2
integer :: zlf_conservation_moist_option = imdi
integer :: zlf_conservation_tracers_option = imdi
integer :: zlf_conservation_theta_option = imdi

real    :: zlf_maximum_height            = rmdi

integer :: eg_vert_damp_profile = imdi    ! Vertical damping flag
real :: eta_s                   = rmdi    ! Height (in eta) above
                                          ! which to apply damping

real :: Ih                      = rmdi    ! Hydrostatic switch
                                          !(1=nonhydrostatic; 0=hydrostatic)

logical :: L_RK_dps             =.true.  ! Runge-Kutta departure point scheme
logical :: L_eliminate_rho      =.true.  ! Diagnostic rho
integer :: InnIts               = 2       ! Number of inner iterations
real :: eg_vert_damp_coeff      = rmdi
real :: damp_height             = rmdi

integer :: polar_cap_rows       = 0       ! Number of rows in polar cap

logical :: l_viscosity          = .false. ! Include explicit Viscosity
real :: horiz_viscosity         = rmdi    ! Horizontal Viscosity Coeff
real :: vert_viscosity          = rmdi    ! Vertical Viscosity Coeff

! Conservation schemes for total mass of dry air
integer :: conserve_dry_mass       = imdi
integer, parameter ::                                                          &
         not_conserved           = 0 ! No conservation of dry air
integer, parameter ::                                                          &
         constant_factor         = 1 ! Adjust dryrho by a constant factor
integer, parameter ::                                                          &
         linear_factor_IE        = 2 ! Adjust dryrho and theta_vd by a linear
                                     ! function of height, preserving total
                                     ! gravitational potential energy and
                                     ! total internal energy (approximately).
integer, parameter ::                                                          &
         linear_factor           = 3 ! Apply linear function to dryrho only.

! ----------------------
! namelist run_dyn
! ----------------------

namelist/RUN_Dyn/                                                              &
 GCR_max_iterations,GCR_precon_option,                                         &
 GCR_tol,                                                                      &
 conserve_dry_mass, l_sl_bc_correction,                                        &
 l_advect_log_theta, alpha_relax_type, polar_cap_rows,                         &
 eg_vert_damp_profile, eta_s, damp_height, eg_vert_damp_coeff,                 &
 l_viscosity, horiz_viscosity, vert_viscosity,                                 &
 zlf_conservation_moist_option,                                                &
 zlf_conservation_tracers_option,                                              &
 zlf_conservation_theta_option,                                                &
 zlf_maximum_height,                                                           &
 l_use_mg, semicoarsening, full_multigrid,                                     &
 relaxation_factor,relaxation_factor_fmg, mg_tolerance,                        &
 n_presmooth, n_postsmooth, n_presmooth_fine, n_postsmooth_fine,               &
 n_coarsesmooth, maxiter, maxlevel, norm_type,                                 &
 solver, coarsegrid_solver, solver_precision, l_dry_static_adjust,             &
 l_use_ib_method, l_ib_noslip, l_use_ib_mask, ib_alpha, ib_beta, ib_damping,   &
 ib_filename, l_tri_sor_vl


!DrHook-related parameters
integer(kind=jpim), parameter, private :: zhook_in  = 0
integer(kind=jpim), parameter, private :: zhook_out = 1

character(len=*), parameter, private :: ModuleName='DYNAMICS_INPUT_MOD'

contains

subroutine check_run_dyn()

! Description:
!   Subroutine to apply logic checks and set variables based on the
!   options selected in the run_dyn namelist.

use bl_option_mod,        only: off, l_quick_ap2, i_bl_vn, i_bl_vn_1a
use chk_opts_mod,         only: chk_var
use ereport_mod,          only: ereport
use model_domain_mod,     only: model_type, mt_global
use precon_constants_mod, only: vert_plus_xyz_ADI_precon,                      &
                                vert_plus_xz_ADI_precon,no_precon
use um_parcore,           only: mype

implicit none

real(kind=jprb)               :: zhook_handle

integer                       :: ErrorStatus   ! used for ereport
character (len=errormessagelength)     :: cmessage      ! used for ereport
character (len=*), parameter  :: RoutineName = 'CHECK_RUN_DYN'

if (lhook) call dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! ---------------------------------------------
! Dynamics logic control - New Dynamics and ENDGAME
! ---------------------------------------------

! Skip all calculations in atmos_physics2 which are identical between
! the first and subsequent iterations
! Not compatable with 1a BL scheme as advection of TKE will differ
! between each outer loop
if (i_bl_vn /= i_bl_vn_1a) l_quick_ap2 = .true.

if (GCR_use_residual_Tol) then
  GCR_use_tol_abs=.false.
  GCR_tol_res = GCR_tol
  GCR_tol_res2 = GCR_tol2
else
  GCR_use_tol_abs=.true.
  GCR_tol_abs = GCR_tol
  GCR_tol_abs2 = GCR_tol2
end if


if (GCR_precon_option /= no_precon   .and.                                     &
    GCR_precon_option /= vert_plus_xyz_ADI_precon   .and.                      &
    GCR_precon_option /= vert_plus_xz_ADI_precon) then
  cmessage =                                                                   &
  'An invalid preconditioner option for ENDGame has been requested'
  ErrorStatus = 25
  call ereport(RoutineName, ErrorStatus, cmessage)
end if

call chk_var( zlf_conservation_moist_option, 'zlf_conservation_moist_option',  &
              [off,apply_ocf,apply_adas] )
call chk_var( zlf_conservation_tracers_option,                                 &
              'zlf_conservation_tracers_option',[off,apply_ocf,apply_adas] )
call chk_var( zlf_conservation_theta_option, 'zlf_conservation_theta_option',  &
              [off,apply_ocf,apply_adas] )

if (L_use_mg ) then
  if ( model_type /= mt_global ) then
    cmessage = 'Multigrid cannot be used with a non-global domain'
    ErrorStatus = 26
    call ereport(RoutineName, ErrorStatus, cmessage)
  end if

  ! check values for multigrid variables only if multigrid active
  call chk_var( coarsegrid_solver,  'coarsegridsolver',  '[1,2]' )
  call chk_var( maxiter,  'maxiter',  '[0:999]' )
  call chk_var( maxlevel,  'maxlevel',  '[1:9]' )
  call chk_var( mg_tolerance,  'mg_tolerance',  '[0.0:1.0]' )
  call chk_var( n_presmooth,  'n_presmooth',  '[0:10]' )
  call chk_var( n_postsmooth,  'n_postsmooth',  '[0:10]' )
  call chk_var( n_presmooth_fine,  'n_presmooth_fine',  '[0:10]' )
  call chk_var( n_postsmooth_fine,  'n_postsmooth_fine',  '[0:10]' )
  call chk_var( n_coarsesmooth,  'n_coarsesmooth',  '[0:10]' )
  call chk_var( norm_type,  'norm_type',  '[1,2]' )
  call chk_var( relaxation_factor,  'relaxation_factor',  '[0.0:1.0]' )
  call chk_var( relaxation_factor_fmg,  'relaxation_factor_fmg',  '[0.0:1.0]' )
  call chk_var( solver,  'solver',  '[2,3]' )

end if

if ( l_use_ib_method ) then
  call chk_var( ib_alpha, 'ib_alpha', '[0.0:1.0]' )
  call chk_var( ib_beta,  'ib_beta',  '[0.0:1.0]' )
  if ( l_use_ib_mask ) then
    call chk_var( ib_damping, 'ib_damping', '[0.0:1.0]' )
  end if
  if ( trim(ib_filename) == "" .and. mype == 0 ) then
    cmessage = 'Immersed Boundary file must be specified'
    ErrorStatus = 26
    call ereport(RoutineName, ErrorStatus, cmessage)
  end if
end if

call chk_var( solver_precision,  'solver_precision',  '[32,64]' )

if (lhook) call dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

return
end subroutine check_run_dyn

subroutine print_nlist_run_dyn()

use umPrintMgr, only: umPrint

implicit none
character(len=50000) :: lineBuffer
real(kind=jprb)               :: zhook_handle

character(len=*), parameter :: RoutineName='PRINT_NLIST_RUN_DYN'

if (lhook) call dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

call umPrint('Contents of namelist run_dyn',                                   &
    src='dynamics_input_mod')

write(lineBuffer,"(A,I3)") ' polar_cap_rows = ', polar_cap_rows
call umPrint(lineBuffer,src='dynamics_input_mod')
write(lineBuffer,*) ' GCR_max_iterations = ',GCR_max_iterations
call umPrint(lineBuffer,src='dynamics_input_mod')
write(lineBuffer,*) ' GCR_precon_option = ',GCR_precon_option
call umPrint(lineBuffer,src='dynamics_input_mod')
write(lineBuffer,*) ' GCR_tol = ',GCR_tol
call umPrint(lineBuffer,src='dynamics_input_mod')
write(lineBuffer,'(A,I0)') ' conserve_dry_mass = ',conserve_dry_mass
call umPrint(lineBuffer,src='dynamics_input_mod')
write(lineBuffer,*) ' l_sl_bc_correction = ',l_sl_bc_correction
call umPrint(lineBuffer,src='dynamics_input_mod')
write(lineBuffer,'(A,L1)') ' l_dry_static_adjust = ',l_dry_static_adjust
call umPrint(lineBuffer,src='dynamics_input_mod')
write(lineBuffer,*) ' alpha_relax_type = ',alpha_relax_type
call umPrint(lineBuffer,src='dynamics_input_mod')
write(lineBuffer,*) ' eg_vert_damp_profile = ',eg_vert_damp_profile
call umPrint(lineBuffer,src='dynamics_input_mod')
write(lineBuffer,'(A,ES17.10)') ' eg_vert_damp_coeff = ',eg_vert_damp_coeff
call umPrint(lineBuffer,src='dynamics_input_mod')
write(lineBuffer,"(A,F16.3)") ' damp_height = ',damp_height
call umPrint(lineBuffer,src='dynamics_input_mod')
write(lineBuffer,'(A,L1)') ' l_viscosity = ',l_viscosity
call umPrint(lineBuffer,src='dynamics_input_mod')
write(lineBuffer,"(A,F16.3)") ' horiz_viscosity = ',horiz_viscosity
call umPrint(lineBuffer,src='dynamics_input_mod')
write(lineBuffer,"(A,F16.3)") ' vert_viscosity = ',vert_viscosity
call umPrint(lineBuffer,src='dynamics_input_mod')
write(lineBuffer,'(A,I0)') 'zlf_conservation_moist_option = ',                 &
                            zlf_conservation_moist_option
call umPrint(lineBuffer,src='dynamics_input_mod')
write(lineBuffer,'(A,I0)') 'zlf_conservation_tracers_option = ',               &
                            zlf_conservation_tracers_option
call umPrint(lineBuffer,src='dynamics_input_mod')
write(lineBuffer,'(A,I0)') 'zlf_conservation_theta_option = ',                 &
                            zlf_conservation_theta_option
call umPrint(lineBuffer,src='dynamics_input_mod')
write(lineBuffer,'(A,E16.3)') 'zlf_maximum_height = ',zlf_maximum_height
call umPrint(lineBuffer,src='dynamics_input_mod')

call umPrint(lineBuffer,src='dynamics_input_mod')
write(lineBuffer,'(A,L1)') ' l_advect_log_theta = ',l_advect_log_theta

write(lineBuffer,'(A,L1)') 'l_use_mg = ',l_use_mg
call umPrint(lineBuffer,src='dynamics_input_mod')
write(lineBuffer,'(A,L1)') 'semicoarsening = ',semicoarsening
call umPrint(lineBuffer,src='dynamics_input_mod')
write(lineBuffer,'(A,L1)') 'full_multigrid = ',full_multigrid
call umPrint(lineBuffer,src='dynamics_input_mod')
write(lineBuffer,"(A,F21.8)") ' relaxation_factor = ',relaxation_factor
call umPrint(lineBuffer,src='dynamics_input_mod')
write(lineBuffer,"(A,F21.8)") ' relaxation_factor_fmg = ',relaxation_factor_fmg
call umPrint(lineBuffer,src='dynamics_input_mod')
write(lineBuffer,"(A,F25.12)") ' mg_tolerance = ',mg_tolerance
call umPrint(lineBuffer,src='dynamics_input_mod')

write(lineBuffer,'(A,I0)') ' n_presmooth  = ',n_presmooth
call umPrint(lineBuffer,src='dynamics_input_mod')
write(lineBuffer,'(A,I0)') ' n_postsmooth  = ',n_postsmooth
call umPrint(lineBuffer,src='dynamics_input_mod')
write(lineBuffer,'(A,I0)') ' n_presmooth_fine  = ',n_presmooth_fine
call umPrint(lineBuffer,src='dynamics_input_mod')
write(lineBuffer,'(A,I0)') ' n_postsmooth_fine  = ',n_postsmooth_fine
call umPrint(lineBuffer,src='dynamics_input_mod')
write(lineBuffer,'(A,I0)') ' n_coarsesmooth  = ',n_coarsesmooth
call umPrint(lineBuffer,src='dynamics_input_mod')
write(lineBuffer,'(A,I0)') ' maxiter = ',maxiter
call umPrint(lineBuffer,src='dynamics_input_mod')
write(lineBuffer,'(A,I0)') ' maxlevel = ',maxlevel
call umPrint(lineBuffer,src='dynamics_input_mod')
write(lineBuffer,'(A,I0)') ' norm_type = ',norm_type
call umPrint(lineBuffer,src='dynamics_input_mod')
write(lineBuffer,'(A,I0)') ' solver = ',solver
call umPrint(lineBuffer,src='dynamics_input_mod')
write(lineBuffer,'(A,I0)') ' coarsegrid_solver = ',coarsegrid_solver
call umPrint(lineBuffer,src='dynamics_input_mod')
write(lineBuffer,'(A,I0)') ' solver_precision = ',solver_precision
call umPrint(lineBuffer,src='dynamics_input_mod')
write(lineBuffer,'(A,L1)') ' l_tri_sor_vl = ',l_tri_sor_vl
call umPrint(lineBuffer,src='dynamics_input_mod')

write(lineBuffer,'(A,L1)') 'l_use_ib_method = ',l_use_ib_method
call umPrint(lineBuffer,src='dynamics_input_mod')
write(lineBuffer,'(A,L1)') 'l_ib_noslip = ',l_ib_noslip
call umPrint(lineBuffer,src='dynamics_input_mod')
write(lineBuffer,'(A,L1)') 'l_use_ib_mask = ',l_use_ib_mask
call umPrint(lineBuffer,src='dynamics_input_mod')
write(lineBuffer,'(A,A)') 'ib_filename = ',trim(ib_filename)
call umPrint(lineBuffer,src='dynamics_input_mod')
write(lineBuffer,"(A,F21.8)") ' ib_alpha = ',ib_alpha
call umPrint(lineBuffer,src='dynamics_input_mod')
write(lineBuffer,"(A,F21.8)") ' ib_beta = ',ib_beta
call umPrint(lineBuffer,src='dynamics_input_mod')
write(lineBuffer,"(A,F21.8)") ' ib_damping = ',ib_damping
call umPrint(lineBuffer,src='dynamics_input_mod')


call umPrint('- - - - - - end of namelist - - - - - -',                        &
    src='dynamics_input_mod')

if (lhook) call dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

end subroutine print_nlist_run_dyn


end module  dynamics_input_mod
