! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

module dgnstcs_glue_conv_mod

implicit none

character(len=*), parameter, private :: ModuleName = 'DGNSTCS_GLUE_CONV_MOD'

contains

! Output dummy SCM diagnostics from routine glue_conv_6a - STUB version non-SCM

subroutine dgnstcs_glue_conv_6a( nlev )

implicit none

! Description:
!   Stub version of dummy scmoutput routine, purely for non-SCM compilations.

! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: scm

! Code Description:
!   Language: Fortran 90

! Variables passed in through the argument ist...
integer, intent(in) :: nlev     ! No. of model layers


return
end subroutine dgnstcs_glue_conv_6a

end module dgnstcs_glue_conv_mod
