! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
! Input control for dynamics.

! Description:
!   Module containing input settings as used for lbcs and trapping
!
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: Dynamics

! Method:
!   Switches are initialised to false and read in from the
!   namelist. The module may then be used directly where the switches
!   are needed within the dynamics semi_lagrangian code.

! Code Description:
!   Language: FORTRAN 90
!   This code is written to UMDP3

module lbc_input_mod

use missing_data_mod, only: imdi

implicit none

logical :: L_LBC_balance = .true.   ! T: impose vertically balanced Exners
                                    !    and rho in LBCs (set w=0)
                                    ! F: leave Exners alone

logical :: L_blend         ! default is .true. for blending lbcs
                           ! for moisture, tracers and w fields

end module  lbc_input_mod
