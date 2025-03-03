! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!   var_end_mod

module var_end_mod

use missing_data_mod, only: rmdi

implicit none

! Description:  End values for variable resolution grid
!
! Method:   These are calculated in set_var_grid via setcona.
!
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: top_level
!
! Code Description:
!   Language: FORTRAN 90
!   This code is written to UMDP3 v6 programming standards.

real  :: lambda_p_end   = rmdi
real  :: lambda_u_end   = rmdi
real  :: phi_p_end      = rmdi
real  :: phi_v_end      = rmdi
real  :: dlambda_p_end  = rmdi
real  :: dlambda_u_end  = rmdi
real  :: dphi_p_end     = rmdi
real  :: dphi_v_end     = rmdi

end module var_end_mod
