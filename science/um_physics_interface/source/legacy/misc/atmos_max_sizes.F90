! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: Misc
!
! Description
!   This module provides parameters giving the maximum likely sizes
!   of key UM resolution variables, useful for sizing static arrays.

module atmos_max_sizes

implicit none

! Maximum sector size for I/O

#if defined(UTILIO)
! Small execs needs a larger value to cope with some of the wet
! model types
integer, parameter :: row_length_max   = 8000 ! maximum row length
integer, parameter :: rows_max         = 6000 ! max no of rows
#else
integer, parameter :: row_length_max   = 4096 ! maximum row length
integer, parameter :: rows_max         = 3200 ! max no of rows
#endif

! maximum permitted size of a halo
integer, parameter :: Max_Halo_Size    = 20
integer, parameter :: model_levels_max = 250 ! max no of total levels


integer, parameter :: horiz_dim_max=max(row_length_max,rows_max)
integer, parameter :: Max2DFieldSize   = row_length_max*rows_max
integer, parameter :: MaxHaloArea      = horiz_dim_max*Max_Halo_Size

end module atmos_max_sizes
