! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: dynamics_advection
module problem_mod

implicit none
! Description: module containing problem_number
!  for use in setting problem types
!
integer, parameter :: standard=0
integer, parameter :: dynamical_core=2
integer, parameter :: idealised_problem=3
integer, parameter :: idealised_planet=5

end module problem_mod
