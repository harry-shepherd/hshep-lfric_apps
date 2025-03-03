module helmholtz_const_matrix_mod
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
!
! Description: Contains the ENDGame departure points
!
!
! Method:
!
! Documentation: ENDGame formulation version 1.01
!
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: Dynamics
!
! Code description:
!   Language: Fortran 90.
!   This code is written to UM programming standards version 8.3.

use um_types, only: real_64,real_32
use missing_data_mod, only: imdi
implicit none

integer, parameter :: double_precision_solver = 64
integer, parameter :: single_precision_solver = 32
integer            :: solver_precision = imdi

! If set to true, the tri_sor pre-conditioner uses a special routine
! optimised to take advantage of the length of the vector registers
! of the underlying hardware.

logical            :: l_tri_sor_vl = .false.

! Size of vector register for real_32 and real_64
#if defined (VCTLEN_R32)
integer, parameter :: vector_length_real32 = vctlen_r32
#else
! The default value is 256bit vector register
integer, parameter :: vector_length_real32 = 8
#endif

#if defined (VCTLEN_R64)
integer, parameter :: vector_length_real64 = vctlen_r64
#else
! The default value is 256bit vector register
integer, parameter :: vector_length_real64 = 4
#endif


type :: solver_precision_type
  real(kind=real_64), allocatable :: dbl(:,:,:)
  real(kind=real_32), allocatable :: single(:,:,:)
end type solver_precision_type

! Horizontal coordinates
type(solver_precision_type), target, save ::                                   &
         Hlm_Lp,                                                               &
         Hlm_Ldiag,                                                            &
         Hlm_Ln,                                                               &
         Hlm_Ls,                                                               &
         Hlm_Le,                                                               &
         Hlm_Lw,                                                               &
         Hlm_Lu,                                                               &
         Hlm_Ld,                                                               &
         Hlm_Ek,                                                               &
         Hlm_Fk,                                                               &
         Hlm_Ck,                                                               &
         Hu_k, Hd_k


! Red-black ordered versions
type(solver_precision_type), target, save ::                                   &
         tHlm_Ln,                                                              &
         tHlm_Ls,                                                              &
         tHlm_Le,                                                              &
         tHlm_Lw,                                                              &
         tHlm_Lu,                                                              &
         tHlm_Ld,                                                              &
         tHu_k, tHd_k


character(len=*), parameter, private :: ModuleName='HELMHOLTZ_CONST_MATRIX_MOD'

contains

subroutine init_helmholtz_const_matrix()

use atm_fields_bounds_mod, only: pdims
use ereport_mod,           only: Ereport

use parkind1, only: jpim, jprb       !DrHook
use yomhook,  only: lhook, dr_hook   !DrHook

implicit none

integer(kind=jpim), parameter :: zhook_in  = 0
integer(kind=jpim), parameter :: zhook_out = 1
real(kind=jprb)               :: zhook_handle

character(len=*), parameter :: RoutineName='INIT_HELMHOLTZ_CONST_MATRIX'

integer :: ierr

if (lhook) call dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

if (solver_precision == double_precision_solver) then

  allocate (   Hlm_Lp%dbl   (pdims%i_start:pdims%i_end,                        &
                                pdims%j_start:pdims%j_end,                     &
                                pdims%k_start:pdims%k_end),                    &
               Hlm_Ldiag%dbl(pdims%i_start:pdims%i_end,                        &
                                pdims%j_start:pdims%j_end,                     &
                                pdims%k_start:pdims%k_end),                    &
               Hlm_Ln%dbl   (pdims%i_start:pdims%i_end,                        &
                                pdims%j_start:pdims%j_end,                     &
                                pdims%k_start:pdims%k_end),                    &
               Hlm_Ls%dbl   (pdims%i_start:pdims%i_end,                        &
                                pdims%j_start:pdims%j_end,                     &
                                pdims%k_start:pdims%k_end),                    &
               Hlm_Le%dbl   (pdims%i_start:pdims%i_end,                        &
                                pdims%j_start:pdims%j_end,                     &
                                pdims%k_start:pdims%k_end),                    &
               Hlm_Lw%dbl   (pdims%i_start:pdims%i_end,                        &
                                pdims%j_start:pdims%j_end,                     &
                                pdims%k_start:pdims%k_end),                    &
               Hlm_Lu%dbl   (pdims%i_start:pdims%i_end,                        &
                                pdims%j_start:pdims%j_end,                     &
                                pdims%k_start:pdims%k_end),                    &
               Hlm_Ld%dbl   (pdims%i_start:pdims%i_end,                        &
                                pdims%j_start:pdims%j_end,                     &
                                pdims%k_start:pdims%k_end),                    &
               Hlm_Ek%dbl   (pdims%i_start:pdims%i_end,                        &
                                pdims%j_start:pdims%j_end,                     &
                                pdims%k_start:pdims%k_end),                    &
               Hlm_Fk%dbl   (pdims%i_start:pdims%i_end,                        &
                                pdims%j_start:pdims%j_end,                     &
                                pdims%k_start:pdims%k_end),                    &
               Hlm_Ck%dbl   (pdims%i_start:pdims%i_end,                        &
                                pdims%j_start:pdims%j_end,                     &
                                0:pdims%k_end),                                &
               Hu_k%dbl     (pdims%i_start:pdims%i_end,                        &
                                pdims%k_start:pdims%k_end,                     &
                                pdims%j_start:pdims%j_end),                    &
               Hd_k%dbl     (pdims%i_start:pdims%i_end,                        &
                                pdims%k_start:pdims%k_end,                     &
                                pdims%j_start:pdims%j_end),                    &
               stat=ierr)

  if (ierr/=0) call Ereport("init_helmholtz_const_matrix",ierr,                &
                              "Unable to allocate double precision.")

  allocate (  tHlm_Ln%dbl   (pdims%i_start:pdims%i_end,                        &
                                pdims%k_start:pdims%k_end,                     &
                                pdims%j_start:pdims%j_end),                    &
              tHlm_Ls%dbl   (pdims%i_start:pdims%i_end,                        &
                                pdims%k_start:pdims%k_end,                     &
                                pdims%j_start:pdims%j_end),                    &
              tHlm_Le%dbl   (pdims%i_start:pdims%i_end,                        &
                                pdims%k_start:pdims%k_end,                     &
                                pdims%j_start:pdims%j_end),                    &
              tHlm_Lw%dbl   (pdims%i_start:pdims%i_end,                        &
                                pdims%k_start:pdims%k_end,                     &
                                pdims%j_start:pdims%j_end),                    &
              tHlm_Lu%dbl   (pdims%i_start:pdims%i_end,                        &
                                pdims%k_start:pdims%k_end,                     &
                                pdims%j_start:pdims%j_end),                    &
              tHlm_Ld%dbl   (pdims%i_start:pdims%i_end,                        &
                                pdims%k_start:pdims%k_end,                     &
                                pdims%j_start:pdims%j_end),                    &
              tHu_k%dbl     (pdims%i_start:pdims%i_end,                        &
                                pdims%k_start:pdims%k_end,                     &
                                pdims%j_start:pdims%j_end),                    &
              tHd_k%dbl     (pdims%i_start:pdims%i_end,                        &
                                pdims%k_start:pdims%k_end,                     &
                                pdims%j_start:pdims%j_end),                    &
              stat=ierr)

  if (ierr/=0) call Ereport("init_helmholtz_const_matrix",ierr,                &
                            "Unable to allocate red-black double precision.")

else

  allocate (   Hlm_Lp%single   (pdims%i_start:pdims%i_end,                     &
                                pdims%j_start:pdims%j_end,                     &
                                pdims%k_start:pdims%k_end),                    &
               Hlm_Ldiag%single(pdims%i_start:pdims%i_end,                     &
                                pdims%j_start:pdims%j_end,                     &
                                pdims%k_start:pdims%k_end),                    &
               Hlm_Ln%single   (pdims%i_start:pdims%i_end,                     &
                                pdims%j_start:pdims%j_end,                     &
                                pdims%k_start:pdims%k_end),                    &
               Hlm_Ls%single   (pdims%i_start:pdims%i_end,                     &
                                pdims%j_start:pdims%j_end,                     &
                                pdims%k_start:pdims%k_end),                    &
               Hlm_Le%single   (pdims%i_start:pdims%i_end,                     &
                                pdims%j_start:pdims%j_end,                     &
                                pdims%k_start:pdims%k_end),                    &
               Hlm_Lw%single   (pdims%i_start:pdims%i_end,                     &
                                pdims%j_start:pdims%j_end,                     &
                                pdims%k_start:pdims%k_end),                    &
               Hlm_Lu%single   (pdims%i_start:pdims%i_end,                     &
                                pdims%j_start:pdims%j_end,                     &
                                pdims%k_start:pdims%k_end),                    &
               Hlm_Ld%single   (pdims%i_start:pdims%i_end,                     &
                                pdims%j_start:pdims%j_end,                     &
                                pdims%k_start:pdims%k_end),                    &
               Hlm_Ek%single   (pdims%i_start:pdims%i_end,                     &
                                pdims%j_start:pdims%j_end,                     &
                                pdims%k_start:pdims%k_end),                    &
               Hlm_Fk%single   (pdims%i_start:pdims%i_end,                     &
                                pdims%j_start:pdims%j_end,                     &
                                pdims%k_start:pdims%k_end),                    &
               Hlm_Ck%single   (pdims%i_start:pdims%i_end,                     &
                                pdims%j_start:pdims%j_end,                     &
                                0:pdims%k_end),                                &
               Hu_k%single     (pdims%i_start:pdims%i_end,                     &
                                pdims%k_start:pdims%k_end,                     &
                                pdims%j_start:pdims%j_end),                    &
               Hd_k%single     (pdims%i_start:pdims%i_end,                     &
                                pdims%k_start:pdims%k_end,                     &
                                pdims%j_start:pdims%j_end),                    &
               stat=ierr)

  if (ierr/=0) call Ereport("init_helmholtz_const_matrix",ierr,                &
                              "Unable to allocate single precision.")

  allocate (  tHlm_Ln%single   (pdims%i_start:pdims%i_end,                     &
                                pdims%k_start:pdims%k_end,                     &
                                pdims%j_start:pdims%j_end),                    &
              tHlm_Ls%single   (pdims%i_start:pdims%i_end,                     &
                                pdims%k_start:pdims%k_end,                     &
                                pdims%j_start:pdims%j_end),                    &
              tHlm_Le%single   (pdims%i_start:pdims%i_end,                     &
                                pdims%k_start:pdims%k_end,                     &
                                pdims%j_start:pdims%j_end),                    &
              tHlm_Lw%single   (pdims%i_start:pdims%i_end,                     &
                                pdims%k_start:pdims%k_end,                     &
                                pdims%j_start:pdims%j_end),                    &
              tHlm_Lu%single   (pdims%i_start:pdims%i_end,                     &
                                pdims%k_start:pdims%k_end,                     &
                                pdims%j_start:pdims%j_end),                    &
              tHlm_Ld%single   (pdims%i_start:pdims%i_end,                     &
                                pdims%k_start:pdims%k_end,                     &
                                pdims%j_start:pdims%j_end),                    &
              tHu_k%single     (pdims%i_start:pdims%i_end,                     &
                                pdims%k_start:pdims%k_end,                     &
                                pdims%j_start:pdims%j_end),                    &
              tHd_k%single     (pdims%i_start:pdims%i_end,                     &
                                pdims%k_start:pdims%k_end,                     &
                                pdims%j_start:pdims%j_end),                    &
              stat=ierr)

  if (ierr/=0) call Ereport("init_helmholtz_const_matrix",ierr,                &
                            "Unable to allocate red-black single precision.")

end if

if (lhook) call dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

end subroutine init_helmholtz_const_matrix
end module helmholtz_const_matrix_mod
