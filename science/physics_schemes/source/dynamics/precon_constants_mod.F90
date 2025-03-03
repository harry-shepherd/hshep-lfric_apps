! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
! GCR solver names

! Description:
!   Module containing GCR parameter options
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: Dynamics

! Method:
! dynamics code options (magic number names)

! Code Description:
!   Language: FORTRAN 90
!   This code is written to UMDP3

module precon_constants_mod

implicit none

integer, parameter ::      no_precon                = 0
integer, parameter ::      vert_precon              = 1
integer, parameter ::      vert_plus_xyz_ADI_precon = 2
integer, parameter ::      xyz_ADI_precon           = 3
integer, parameter ::      vert_plus_xz_ADI_precon  = 4
integer, parameter ::      xz_ADI_precon            = 5
integer, parameter ::      Dufort_Frankel_precon    = 6

end module precon_constants_mod
