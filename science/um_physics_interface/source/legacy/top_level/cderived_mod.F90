! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: Top_level

module cderived_mod

implicit none

! Description:
!   This file contains declarations for derived constants and
!   grid definition coords in radians within the atmospheric model.

!   The derived constants are calculated in the routine SETCONA.
!
      ! No of cloud types ie low/med/high
integer, parameter :: NUM_CLOUD_Types = 3

! derived constants:
integer :: LOW_BOT_Level      ! Bottom level of lowest cloud type
integer :: LOW_TOP_Level      ! Top      "    "   "       "    "
integer :: MED_BOT_Level      ! Bottom   "    "  med      "    "
integer :: MED_TOP_Level      ! Top      "    "   "       "    "
integer :: HIGH_BOT_Level     ! Bottom   "    "  top      "    "
integer :: HIGH_TOP_Level     ! Top      "    "   "       "    "

! height values to split model levels into l/m/h cloud
real ::    h_split(NUM_CLOUD_Types+1)

logical :: elf                ! T if atmosphere model on LAM grid

! Grid definition co-ordinates in radians
real:: Delta_lambda       ! EW (x) grid spacing in radians
real:: Delta_phi          ! NS (y) grid spacing in radians
real:: Base_phi           ! Lat of first theta point in radians
real:: Base_lambda        ! Long of first theta point in radians
real:: lat_rot_NP         ! Real lat of 'pseudo' N pole in radians
real:: long_rot_NP        ! Real long of 'pseudo' N pole in radians

contains

subroutine h_split_defaults()

! height values to split model levels into l/m/h cloud

implicit none

! Default settings for h_split
!  low:middle:high cloud model levels =(1)->(2):(2)->(3):(3)->(4)
h_split(1) =   111.0        ! ICAO 1000mb height (m)
h_split(2) =  1949.0        ! ICAO  800mb height (m)
h_split(3) =  5574.0        ! ICAO  500mb height (m)
h_split(4) = 13608.0        ! ICAO  150mb height (m)

end subroutine h_split_defaults

end module cderived_mod
