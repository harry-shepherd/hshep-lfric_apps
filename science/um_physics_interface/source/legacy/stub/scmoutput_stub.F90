! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

module scmoutput_mod

implicit none

contains


! Stub version of Single-Column Model diagnostic output routine

subroutine scmoutput                                                           &
  ( x, sname, lname, units, timprof, domprof, streams, sname2                  &
  , calling_routine )


implicit none


! Description:
!   SCM output stub routine for non-scm model configurations
!
! Method:
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: scm
!
! Code Description:
!   Language: Fortran77 with some bits of Fortran90

integer, intent(in) :: domprof         ! Domain profile for the diagnostic
real,    intent(in) :: x( * )            ! Input variable
! NOTE: with scmoutput inside a module, the input array x needs to be
! declared with assumed size (*).  Declaring it with assumed shape (:), (:,:)
! or (:,:,:) causes compile errors in the full UM wherever different-ranked
! arrays are passed into scmoutput.
! In the actual SCM version of this routine, we don't run into this problem,
! because in that case scmoutput knows what shape all the arrays passed to it
! should have (so x is declared as an automatic array, for which fortran
! allows rank mis-matches in argument lists).

character(len=*), intent(in) :: sname  ! Diag Short name, (unique)
character(len=*), intent(in) :: lname  ! Diag Long name
character(len=*), intent(in) :: units  ! Diag Units
character(len=*), intent(in) :: sname2 ! Short name of another, previously
                                       ! defined diagnostic which will be
                                       ! used in the construction of this
                                       ! one according to the time profile

character(len=*), intent(in) :: calling_routine
                                       ! Routine that has called scmoutput

integer, intent(in) :: timprof         ! Diag time profile
integer, intent(in) :: streams         ! An encoded integer specifying
                                       ! which output streams the diagnostic
                                       ! is to go to

return

end subroutine scmoutput


end module scmoutput_mod
