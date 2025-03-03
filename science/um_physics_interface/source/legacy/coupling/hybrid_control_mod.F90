! *****************************COPYRIGHT****************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt which you should
! have received as part of this distribution.
! *****************************COPYRIGHT****************************************
module hybrid_control_mod
!
! Description: The hybrid variables which are likely to be shared,
!              and the hybrid I/O subroutines.
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: Coupling
!
!--------------------------------------------------------------------
use errormessagelength_mod, only: errormessagelength
use missing_data_mod,       only: rmdi, imdi

use yomhook,                only: lhook, dr_hook
use parkind1,               only: jprb, jpim

implicit none

private

! Variables and subroutine tstmsk_hybrid_ukca outside #if !defined(LFRIC)
public :: cpl_hybrid_in, cpl_hybrid_out,                                       &
          cpl_hybrid_stats_in, cpl_hybrid_stats_out,                           &
          cpl_tstep_in, cpl_tstep_out,                                         &
          hybrid_data, hybrid_in, hybrid_info, hybrid_out,                     &
          i_activate_strip_ukca, ip_activate_run_in_snr,                       &
          ip_activate_scale, ip_activate_unchange,                             &
          l_hyb_theta_adj, l_hyb_theta_adj_prnt,                               &
          l_hybrid_overw_in, l_hybrid_overw_out,                               &
          l_hybrid_stats_in, l_hybrid_stats_out,                               &
          l_junior, l_senior, l_strip_ukca,                                    &
          n_cpl_hybrid_in, n_cpl_hybrid_out,                                   &
          n_cpl_hybrid_stats_in, n_cpl_hybrid_stats_out,                       &
          n_cpl_total_in, n_cpl_total_out,                                     &
          n_cpl_tstep_in, n_cpl_tstep_out,                                     &
          qmin_strat, scale_activ_aerosol, tstmsk_hybrid_ukca


! For hybrid configuration, indicate if this is Snr or Jnr.
logical                     :: l_senior = .false.
logical                     :: l_junior = .false.

! Variables connected with namelists HYBRID_SNR2JNR and HYBRID_JNR2SNR.
! Determine if UKCA code in Snr is no longer run
logical                     :: l_strip_ukca = .false.
! Determine if we want stats on hybrid coupled fields
logical                     :: l_hybrid_stats_in, l_hybrid_stats_out
! Determine if the hybrid fields should overwrite the native fields
logical                     :: l_hybrid_overw_in, l_hybrid_overw_out
! Maintain global potential temperature on Jnr
logical                     :: l_hyb_theta_adj = .false.
! Determine if potential temperature adjustment stats should be displayed
logical                     :: l_hyb_theta_adj_prnt = .false.
! A fairly arbitary limit on the number of 3D fields in each
! STASH coupling array. Limit is (max_stash_cpl - 1).
integer, parameter          :: max_stash_cpl = 60
! The number of outgoing and incoming coupling fields
integer                     :: n_cpl_hybrid_out, n_cpl_hybrid_in
! The number of outgoing and incoming coupling fields for stats only
integer                     :: n_cpl_hybrid_stats_out,                         &
                               n_cpl_hybrid_stats_in
! The total number of outgoing and incoming coupling fields
integer                     :: n_cpl_total_out, n_cpl_total_in
! The STASH codes for hybrid coupling fields
integer                     :: cpl_hybrid_in(max_stash_cpl),                   &
                               cpl_hybrid_out(max_stash_cpl)
! The STASH codes for hybrid coupling fields only used for stats comparison
integer                     :: cpl_hybrid_stats_in(max_stash_cpl),             &
                               cpl_hybrid_stats_out(max_stash_cpl)
! A fairly arbitary limit on the number of overrides to the standard
! coupling frequency between hybrid components which are possible
integer, parameter          :: max_cpl_tstep = 20
! The coupling frequency overrides from namelist (two entries per override)
integer                     :: cpl_tstep_in(2 * max_cpl_tstep),                &
                               cpl_tstep_out(2 * max_cpl_tstep)
! The number of fields with non-default coupling frequencies
integer                     :: n_cpl_tstep_in,                                 &
                               n_cpl_tstep_out
! Determine treatement of UKCA-ACTIVATE when stripping UKCA from UM
integer                     :: i_activate_strip_ukca = imdi
! Options for i_activate_strip_ukca
! Not changing the activated aerosol received from Junior
integer, parameter          :: ip_activate_unchange = 0
! Scaling the activated aerosol received from Junior
integer, parameter          :: ip_activate_scale = 1
! Running ACTIVATE code in Senior
integer, parameter          :: ip_activate_run_in_snr = 2

! Minimum threshold for water vapour. There are other minimum values for q
! in the code, such as qmin_conv in module cv_run_mod, but these are
! typically ~1.0e-8 and are thought to be too large when considering the
! stratosphere, as well as the troposphere.
real, parameter             :: qmin_strat = 1.0e-10
! Storage array for all hybrid coupling fields (similar to D1 array)
real, target, allocatable   :: hybrid_data(:)
! If i_activate_strip_ukca = ip_activate_scale this is the scaling for
! activated aerosol used
real                        :: scale_activ_aerosol = rmdi

! Description of the hybrid fields which are passed
type :: hybrid_info
  logical :: l_overw = .false.
  logical :: l_vector = .false.
  logical :: l_lower_bnd = .false.
  logical :: l_upper_bnd = .false.
  integer :: stash_code
  integer :: d1_addr
  integer :: gridtype
  integer :: fld_type
  integer :: halotype
  integer :: levs
  integer :: i_size
  integer :: j_size
  integer :: tc = -1
  integer :: i_start = -999
  integer :: i_end   = -999
  integer :: j_start = -999
  integer :: j_end   = -999
  integer :: k_start = -999
  integer :: k_end   = -999
  integer :: length
  integer :: cpl_freq
  real    :: lower_bnd
  real    :: upper_bnd
  real, pointer :: field_data(:,:,:)
end type hybrid_info

type (hybrid_info), allocatable, target :: hybrid_in(:)
type (hybrid_info), allocatable, target :: hybrid_out(:)

character(len=*), parameter :: ModuleName='HYBRID_CONTROL_MOD'

! Dr Hook variables
integer(kind=jpim), parameter :: zhook_in  = 0
integer(kind=jpim), parameter :: zhook_out = 1

contains

!**********************************************************************
!**********************************************************************
logical function tstmsk_hybrid_ukca(isec, item)
!======================================================================
! Description: Determines if UKCA field should be in D1 array for
!              Senior hybrid model.
! Note: this routine is small and can be called often, which is why
! Dr Hook is not called for this routine.
!======================================================================
! Modules
use um_stashcode_mod, only: stashcode_cdnc, stashcode_n_activ_sum

implicit none

! Arguments
! Section number
integer, intent(in) :: isec
! Item number
integer, intent(in) :: item

! Local variables
! The STASH code
integer :: stashc

! Determine STASH code
stashc = 1000 * isec + item

! Determine if field should be in D1
if ( (stashc == stashcode_cdnc) .or. (stashc == stashcode_n_activ_sum) ) then
  ! CDNC and activated aerosol are always needed by Senior UM
  tstmsk_hybrid_ukca = .true.
else
  ! Otherwise, the only fields we want in Senior UM are those passed
  ! from Junior UM.
  if ( (n_cpl_hybrid_in > 0) .and.                                             &
       (count(cpl_hybrid_in(1:n_cpl_hybrid_in) == stashc) > 0) ) then
    tstmsk_hybrid_ukca = .true.
  else
    if ( (n_cpl_hybrid_stats_in > 0) .and.                                     &
         (count(cpl_hybrid_stats_in(1:n_cpl_hybrid_stats_in) ==                &
                stashc) > 0) ) then
      tstmsk_hybrid_ukca = .true.
    else
      tstmsk_hybrid_ukca = .false.
    end if
  end if
end if

end function tstmsk_hybrid_ukca

end module hybrid_control_mod
