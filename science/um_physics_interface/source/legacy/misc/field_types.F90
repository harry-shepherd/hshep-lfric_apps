! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
module field_types

implicit none

! FLDTYPE definitions for the different field types recognised on the
! decomposition
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: Misc

integer, parameter :: Nfld_max           = 8

! maximum number of field types

integer, parameter :: fld_type_unknown   = -1
! non-standard grid

integer, parameter :: fld_type_0         = 0
! field is a 0D or 1D array

integer, parameter :: fld_type_p         = 1
! grid on P points

integer, parameter :: fld_type_u         = 2
! grid on U points

integer, parameter :: fld_type_v         = 3
! grid on V points

integer, parameter :: fld_type_r         = 7
! grid on river points

integer, parameter :: fld_type_w         = 8
! Only for Endgame

end module field_types
