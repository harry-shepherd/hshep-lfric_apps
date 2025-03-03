! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!  Magic numbers  defining decompositions

module decomp_params

! Description:
!    This data module contains magic numbers defining decompositions
!    for MPP components
!
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: MPP

implicit none


integer, parameter :: max_decomps           = 3

! not set decomp
integer, parameter :: decomp_unset          = 0

! dead decomps that are still referenced, but not actually used
integer, parameter :: decomp_standard_ocean = -1
integer, parameter :: decomp_nowrap_ocean   = -2

! decomps for rcf
integer, parameter :: decomp_rcf_input      = 1
integer, parameter :: decomp_rcf_output     = 2

! decomps for atmos
integer, parameter :: decomp_standard_atmos = 1
integer, parameter :: decomp_standard_wave  = 2

!decomps for small execs
integer, parameter :: decomp_smexe          = 3

! Magic number to retrieve grid information from
! decomp arrays
integer, parameter :: gl_row_length = 1
integer, parameter :: gl_num_rows   = 2
integer, parameter :: gl_num_levels = 3

end module decomp_params
