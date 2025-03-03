! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
!  Parameters and variables for Incremental Analysis Update (IAU) scheme

module IAU_mod

! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: ac_assimilation
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to UMDP3 version 8.1 programming standards.
!
! Declarations:

use errormessagelength_mod, only: errormessagelength
use filenamelength_mod, only: filenamelength
use missing_data_mod, only: imdi, rmdi
use nlsizes_namelist_mod, only: len_fixhd
use um_types, only: real_32

use yomhook,  only: lhook, dr_hook
use parkind1, only: jprb, jpim

implicit none

!-------------------------------------------------------------------------------
! [1]: Global constants.
!-------------------------------------------------------------------------------

integer, parameter :: MaxNum_IAU_incs = 9999 ! Max number of increment files

! Data for fields that may be read from the increment files:
integer, parameter :: IAU_NumFldCodes = 28
integer, parameter :: IAU_FldCodes(IAU_NumFldCodes) =                          &
                        [  2        ,  3        ,  4       ,                   &
                            9        ,  10       ,  12      ,                  &
                            20       ,  24       ,  90      ,                  &
                            150      ,  253      ,  254     ,                  &
                            255      ,  407      ,  431     ,                  &
                            432      ,  433      ,  434     ,                  &
                            435      ,  436      ,  480     ,                  &
                            16207    ,  18001    ,  101     ,                  &
                            34001    ,  34004    ,  233     ,                  &
                            384       ]

character(len=8), parameter ::                                                 &
                      IAU_FldDescs(IAU_NumFldCodes) =                          &
                        [ 'u       ', 'v       ', 'theta   ',                  &
                           'soilm   ', 'q       ', 'qCF     ',                 &
                           'TSoil   ', 'sfctemp ', 'aerosol ',                 &
                           'w       ', 'rho     ', 'qCL     ',                 &
                           'exner   ', 'p       ', 'dust1   ',                 &
                           'dust2   ', 'dust3   ', 'dust4   ',                 &
                           'dust5   ', 'dust6   ', 'ozone   ',                 &
                           'qT      ', 'qT      ', 'SO2     ',                 &
                           'ukca_O3 ', 'ukca_NO2', 'sfctTile',                 &
                           'SnowTemp' ]

! TimeID choices, copied from VAR:
integer, parameter :: TimeID_Once     = 0 ! Instantaneous increment
integer, parameter :: TimeID_Constant = 3 ! Constant error tendency

! Call frequency codes:
integer, parameter :: CallFreq_Never              = 0
integer, parameter :: CallFreq_EveryCallIncsToAdd = 1
integer, parameter :: CallFreq_EndInc1Window      = 2
integer, parameter :: CallFreq_LastIAUCall        = 3

! Filter type codes:
integer, parameter :: FilterType_Uniform    = 1
integer, parameter :: FilterType_Triangular = 2
integer, parameter :: FilterType_LancWin    = 3
integer, parameter :: FilterType_Dolph      = 4

!-------------------------------------------------------------------------------
! [2]: Global type definitions.
!-------------------------------------------------------------------------------

! Structure to hold IAU increment data:
type :: IAU_inc_type

  character(filenamelength) :: FilePath

  logical :: FieldsToAdd    ! Any fields to be added to the model?

  logical :: TendencyIncs   ! Are increments (per second) tendencies?

  logical :: Contains_q_qCL_or_qCF ! Are there any increments to q, qCL or qCF?
  logical :: Contains_qT           ! Are there any increments to qT?
  logical :: Contains_sfctemp      ! Are there any increments to sfctemp?
  logical :: Contains_soilm        ! Are there any increments to soilm?

  integer       :: InsertionStartTS ! Start timestep for insertion
  integer       :: InsertionEndTS   ! End   timestep for insertion
  real, pointer :: Weights(:)       ! Insertion weights

  integer          :: FixHd(Len_FixHd)
  integer          :: Len1Lookup
  integer          :: Len2Lookup
  integer, pointer :: Lookup(:,:)

  logical :: StoreFields  ! Store the increment fields between IAU calls?
  logical :: StorePacked  ! Store them 32-bit-packed?
  logical :: FieldsStored ! Are the fields currently stored?

  integer                    :: flds_len     ! Required length of storage array
  real,              pointer :: flds     (:) ! Storage array for increments
  real(kind=real_32), pointer :: flds32bit(:) ! 32-bit alternative

end type IAU_inc_type

!-------------------------------------------------------------------------------
! [3]: Global variables.
!-------------------------------------------------------------------------------

type (IAU_inc_type), allocatable :: IAU_incs(:) ! IAU increments

integer :: IAU_LocFldLens(IAU_NumFldCodes) ! Local lengths of fields that may
                                           ! be read from the increment files

real :: q_min ! Minimum value allowed for q after addition of q increments

integer :: IAU_FirstCallTS = -1 ! Timestep number for first call to IAU
integer :: IAU_LastCallTS  = -1 ! Timestep number for last  call to IAU

integer :: ukca_O3_index  ! Index of O3  field in the tracer_ukca array
integer :: ukca_NO2_index ! Index of NO2 field in the tracer_ukca array

!-------------------------------------------------------------------------------
! [3.1]: Namelist variables.
!-------------------------------------------------------------------------------

logical :: l_iau = .false.  ! Activate IAU scheme?

integer :: Num_IAU_incs = imdi ! Number of increment files

logical :: L_IAU_SpecifyInc1Filter = .false. ! Specify filter details for first
                                            ! increment? Was true

! Filter details for first increment:
integer :: IAU_FilterType    = imdi   ! Filter type 1- uniform, 2- triangular,
                                      !             3- Lanczos, 4- Dolph
integer :: IAU_StartMin      = imdi   ! Start minute of filtering period
                                      ! Was 0
integer :: IAU_EndMin        = imdi   ! End   minute of filtering period
                                      ! Was 0
integer :: IAU_ApexMin       = imdi   ! Apex minute for triangular filter
                                      ! Was 0
real    :: IAU_Cutoff_period = rmdi   ! Filter cutoff period in hours - was 0.0
real    :: IAU_SBE_period    = rmdi   ! Stop band edge period in hours - was 0.0

! Switches:
logical :: L_IAU_IncDiags       = .false. ! Write increment diagnostics?
logical :: L_IAU_CalcExnerIncs  = .false. ! Use p increments to calculate
                                          ! exner increments?
logical :: L_IAU_CalcThetaIncs  = .false. ! Calculate theta increments using
                                          ! exner and q increments?
logical :: L_IAU_CalcRhoIncs    = .false. ! Calculate rho increments using
                                          ! exner, theta and (possibly) q
                                          ! increments?
logical :: L_IAU_IncTStar       = .false. ! Add level-one temperature
                                          ! increments to surface temperature
                                          ! and top-level soil temperature?
logical :: L_IAU_IncTStar_tile  = .false. ! Add level-one temperature
                                          ! increments to surface temperature
                                          ! on land-surface tiles
                                          ! Was true
logical :: L_IAU_IncTSurf_Snow  = .false. ! Add level-one temperature
                                          ! increments to snow temperature
logical :: L_IAU_UseSfctempIncs = .false. ! Include surface temperature
                                          ! increments and add to TStar
                                          ! and top-level soil temperature?
logical :: L_IAU_UseSoilmIncs  = .false.  ! Add soil moisture increments
                                          ! to soil-levels on land-surface?
logical :: L_IAU_UseTSoilIncs  = .false.  ! Add deep soil temperature
                                          ! increments to soil-levels
                                          ! on land-surface?
logical ::  L_IAU_UseSfcTempTileIncs  = .false. ! Add skin temperature on tiles
                                                ! increments to land-surface?
logical ::  L_IAU_UseTSnowTileIncs    = .false. ! Add snow temperature on tiles
                                                ! increments to multilayer snow?
logical :: L_IAU_ResetPoles     = .false. ! Reset polar rows of relevant
                                          ! increment fields to their mean
                                          ! values? Was true
logical :: L_IAU_RemoveSS       = .false. ! Remove supersaturation wrt water?
logical :: L_IAU_DumpTS0State   = .false. ! Write out model state
                                          ! immediately after timestep-zero
                                          ! call to IAU scheme
logical :: L_IAU_ApplyQTCorrections = .false.     ! Apply corrections for
                                                  ! processing of qT incs.
logical :: L_IAU_Add_qT_prime_to_q = .false. ! Replace q with qT_plus,
                                                  ! bypassing Var_DiagCloud.
logical :: L_IAU_IncrementIce   = .false. ! Diagnose ice cloud increments as
                                          ! well as humidity and liquid cloud
                                          ! increments from qT increments?
logical :: L_IAU_ScaleCloud     = .false. ! If diagnosing humidity and cloud
                                          ! increments from qT increments,
                                          ! scale increments to be within
                                          ! physical bounds?
logical :: L_IAU_LimitUpperThetaIncs = .false. ! Apply limits to size of
                                               ! upper-level theta increments?
logical :: L_IAU_SetOzoneMin    = .false.      ! Reset ozone to oz_min if ozone
                                               ! was negative?
logical :: L_IAU_SMCChecks      = .false.      ! Apply limits and masks to SMC
                                               ! incs?
logical :: L_IAU_TSoilLandIceMask = .false.    ! Mask out TSoil incs at land-ice
                                               ! points?
logical :: L_IAU_IgnoreTsoilSnowCheck = .false.! Ignore the snowdepth check when
                                               ! applying increments to tsoil.
                                               ! Only applies to direct
                                               ! increments.
                                               ! (l_iau_usetsoilincs=.true.)

! Parameters for use with L_IAU_LimitUpperThetaIncs:
real :: IAU_LimitUpperThetaIncs_pBound = rmdi ! Pressure boundary - was 200.0
real :: IAU_LimitUpperThetaIncs_maxInc = rmdi ! Maximum absolute value of
                                               ! increment after multiplication
                                               ! by IAU weight - was 100.0

! Parameters and switches for use with QLimits:
integer :: IAU_QLimitsCallFreq  = imdi    ! Call frequency
                                          !  - was CallFreq_EndInc1Window
logical :: L_IAU_QLimitsDiags   = .false. ! Write diagnostics?
logical :: L_IAU_RmNonTropQIncs = .false. ! Remove non-trop q increments?
real    :: IAU_trop_min_RH      = rmdi    ! Lower limit to apply to trop RH
                                          ! Was 0.0
real    :: IAU_nonTrop_max_q    = rmdi    ! Upper limit to apply to non-trop q
                                          ! Was 3.0E-06
real    :: IAU_nonTrop_min_q    = rmdi    ! Lower limit to apply to non-trop q
                                          ! Was 1.0E-06
real    :: IAU_nonTrop_max_RH   = rmdi    ! Upper limit to apply to non-trop RH
                                          ! Was 0.1
real    :: IAU_trop_min_p       = rmdi    ! Minimum tropospheric pressure
                                          ! Was 1.0E+04
real    :: IAU_trop_max_PV      = rmdi    ! Maximum tropospheric abs(PV)
                                          ! Was 2.5E-06
real    :: IAU_nonTrop_max_p    = rmdi    ! Maximum non-trop     pressure
                                          ! Was 4.0E+04

! Want to make this switch obsolete ASAP:
logical :: L_IAU_IgnoreTopLevThetaIncs = .false.

! Parameters and switches for use within Var_DiagCloud itself.
! (Currently Var_DiagCloud is called only from IAU, so it makes sense to
! control them via the IAU namelist.)

real :: DiagCloud_Tol_q = rmdi  ! Was 0.0
  ! Tolerance for qcl==0 and qcf==0 tests in VarDiag_Cloud.
  ! (Tests suggest that a value of 1.0e-14 is more-or-less optimal.)

real :: DiagCloud_Tol_FM = rmdi  ! Was 0.0
  ! Tolerance for (1-FM)==0  tests in VarDiag_Cloud.
  ! (Tests suggest that a value of 1.0e-28 is more-or-less optimal.)

logical :: DiagCloud_ApplyCompLimits = .false. ! Was true
  ! If true, take measures to avoid machine precision issues in Var_DiagCloud.
  ! (Once we're confident that this option has no nasty side-effects, we should
  ! remove the switch and always use it.)

integer :: DiagCloud_NumMaxLoops = imdi  ! Was 150
  ! Maximum number of loops for convergence in Var_DiagCloud.

real    :: DiagCloud_QN_CompRegimeLimit = rmdi ! Was 20.0
  ! When the magnitude of QN gets much beyond this parameter, the standard
  ! calculations for qCL_calc in Var_DiagCloud give deviations from its upper
  ! or lower bound that are highly sensitive to rounding. In the case where QN
  ! is negative, this makes the iterative algorithm used to calculate cloud
  ! quantities extremely sensitive to small changes in the input data, and when
  ! the IncrementIce option is used often leads to non-convergence. To get
  ! around this, when ApplyCompBounds is set to true, and the magnitude of QN
  ! exceeds the above limit, we set qCL_calc directly to the appropriate
  ! bounding value.
  !
  ! QN < -QN_CompRegimeLimit occurs at points very far from any saturation.
  ! QN >  QN_CompRegimeLimit occurs at points very far from any unsaturation.
  !
  ! The parameter value was determined experimentally by determining when the
  ! path through the code starts becoming affected by least-significant-bit
  ! changes to the LS states. The current value is appropriate for 64-bit
  ! reals.

!-------------------------------------------------------------------------------
! [4]: IAU namelist.
!-------------------------------------------------------------------------------

namelist / IAU_nl /                                                            &
l_iau,                                                                         &
Num_IAU_incs,                                                                  &
L_IAU_SpecifyInc1Filter,                                                       &
IAU_FilterType,                                                                &
IAU_StartMin,                                                                  &
IAU_EndMin,                                                                    &
IAU_ApexMin,                                                                   &
IAU_Cutoff_period,                                                             &
IAU_SBE_period,                                                                &
L_IAU_IncDiags,                                                                &
L_IAU_CalcExnerIncs,                                                           &
L_IAU_CalcThetaIncs,                                                           &
L_IAU_CalcRhoIncs,                                                             &
L_IAU_IncTStar,                                                                &
L_IAU_IncTStar_tile,                                                           &
L_IAU_IncTSurf_Snow,                                                           &
L_IAU_UseSoilmIncs,                                                            &
L_IAU_UseTSoilIncs,                                                            &
L_IAU_UseSfctempIncs,                                                          &
L_IAU_UseSfcTempTileIncs,                                                      &
L_IAU_UseTSnowTileIncs,                                                        &
L_IAU_ResetPoles,                                                              &
L_IAU_RemoveSS,                                                                &
L_IAU_DumpTS0State,                                                            &
L_IAU_ApplyQTCorrections,                                                      &
L_IAU_Add_qT_prime_to_q,                                                       &
L_IAU_IncrementIce,                                                            &
L_IAU_ScaleCloud,                                                              &
L_IAU_LimitUpperThetaIncs,                                                     &
L_IAU_SetOzoneMin,                                                             &
L_IAU_SMCChecks,                                                               &
L_IAU_TSoilLandIceMask,                                                        &
L_IAU_IgnoreTsoilSnowCheck,                                                    &
IAU_LimitUpperThetaIncs_pBound,                                                &
IAU_LimitUpperThetaIncs_maxInc,                                                &
IAU_QLimitsCallFreq,                                                           &
L_IAU_QLimitsDiags,                                                            &
L_IAU_RmNonTropQIncs,                                                          &
IAU_trop_min_RH,                                                               &
IAU_nonTrop_max_q,                                                             &
IAU_nonTrop_min_q,                                                             &
IAU_nonTrop_max_RH,                                                            &
IAU_trop_min_p,                                                                &
IAU_trop_max_PV,                                                               &
IAU_nonTrop_max_p,                                                             &
L_IAU_IgnoreTopLevThetaIncs,                                                   &
DiagCloud_Tol_q,                                                               &
DiagCloud_Tol_FM,                                                              &
DiagCloud_ApplyCompLimits,                                                     &
DiagCloud_NumMaxLoops,          & ! must be greater than 50
DiagCloud_QN_CompRegimeLimit


!DrHook-related parameters
integer(kind=jpim), parameter, private :: zhook_in  = 0
integer(kind=jpim), parameter, private :: zhook_out = 1

character(len=*), parameter, private :: ModuleName='IAU_MOD'

contains


end module IAU_mod
