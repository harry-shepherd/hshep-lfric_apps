! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: Dynamics

module eg_alpha_mod

implicit none

real, save :: alpha_u        = 0.55
real, save :: alpha_v        = 0.55
real, save :: alpha_w        = 0.55
real, save :: alpha_rho      = 0.55
real, save :: alpha_p
real, save :: alpha_theta    = 0.55

real, save :: tau_u        = 0.55
real, save :: tau_v        = 0.55
real, save :: tau_w        = 0.55
real, save :: tau_rho      = 0.55
real, save :: tau_p
real, save :: tau_theta    = 0.55

logical, save :: alpha_changed = .false.

end module
