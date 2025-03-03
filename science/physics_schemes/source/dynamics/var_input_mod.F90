! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
! Input control for dynamics.

! Description:
!   Module containing input settings as used for variable resolution
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

module var_input_mod

use missing_data_mod, only: rmdi, imdi

implicit none

integer :: lam_var = imdi   ! number of variable res. lambda intervals
integer :: phi_var = imdi   ! number of variable res. phi intervals

real :: var_ratio  = rmdi   ! grid-stretcing ratio for var grid
real :: lam_ratio  = rmdi   ! scaling of original grid to high res grid
real :: phi_ratio  = rmdi   ! scaling of original grid to high res grid
real :: lam_frac   = rmdi   ! proportion of reg. points in West of domain
real :: phi_frac   = rmdi   ! proportion of reg. points in South of domain

end module  var_input_mod
