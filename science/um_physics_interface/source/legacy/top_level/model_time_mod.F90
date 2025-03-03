! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Description: Derived model time/step information including start/end
!              step numbers and frequencies (in steps) of interface field
!              generation, boundary field updating, ancillary field
!              updating; and assimilation start/end times.
!              NB: Last three are set by IN_BOUND, INANCCTL, IN_ACCTL.
!              Also contains current time/date information, current
!              step number (echoed in history file) and steps-per-group.
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: top_level
!
! Code Description:
!   Language: FORTRAN 90

module model_time_mod

use submodel_mod, only: internal_id_max
use control_max_sizes, only: max_n_intf_a

implicit none

integer :: i_year               ! Current model time (years)
integer :: i_month              ! Current model time (months)
integer :: i_day                ! Current model time (days)
integer :: i_hour               ! Current model time (hours)
integer :: i_minute             ! Current model time (minutes)
integer :: i_second             ! Current model time (seconds)
integer :: i_day_number         ! Current model time (day no)
integer :: previous_time(7)     ! Model time at previous step
integer :: iau_dtresetstep      ! Data time reset step for IAU run

integer :: basis_time_days  ! Integral no of days to basis time
integer :: basis_time_secs  ! No of seconds-in-day at basis time

logical :: l_c360dy ! Use 360 day calendar

real    :: forecast_hrs         ! Hours since Data Time (ie T+nn)
real    :: data_minus_basis_hrs ! Data time - basis time (hours)

integer :: stepim(internal_id_max)  ! Step no since basis time
integer :: groupim(internal_id_max) ! Number of steps per group

! Finish step number this run
integer :: target_end_stepim(internal_id_max)

! Timestep length in secs
real    :: secs_per_stepim(internal_id_max)

! Frequency of interface field generation in steps
integer :: interface_stepsim(max_n_intf_a,internal_id_max)

! Start steps for interface field generation
integer :: interface_fstepim(max_n_intf_a,internal_id_max)

! End steps for interface field generation
integer :: interface_lstepim(max_n_intf_a,internal_id_max)

! Frequency of  updating boundary fields in steps
integer :: boundary_stepsim(internal_id_max)

! No of steps from boundary data prior to basis time to model basis time
integer :: bndary_offsetim(internal_id_max)

! Lowest frequency for updating of ancillary fields in steps
integer :: ancillary_stepsim(internal_id_max)

! Start steps for assimilation
integer :: assim_firststepim(internal_id_max)

! Number of assimilation steps to analysis
integer :: assim_stepsim(internal_id_max)

! Number of assimilation steps after analysis
integer :: assim_extrastepsim(internal_id_max)

end module model_time_mod
