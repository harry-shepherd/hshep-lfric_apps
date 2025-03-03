! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

module eg_alpha_ramp_mod

use missing_data_mod, only: imdi

implicit none

integer, save :: alpha_relax_type = imdi
integer, save :: alpha_relax_int  = 1

integer, parameter :: alpha_relax_type_constant = 1
integer, parameter :: alpha_relax_type_sqrt     = 2
integer, parameter :: alpha_relax_type_coslaw   = 3
integer, parameter :: alpha_relax_type_step     = 4


character(len=*), parameter, private :: ModuleName='EG_ALPHA_RAMP_MOD'

contains

subroutine eg_alpha_ramp()

use conversions_mod, only: pi
use eg_alpha_mod,    only: alpha_u, alpha_v, alpha_w, alpha_rho, alpha_p,      &
                           alpha_changed, tau_u, tau_v, tau_w, tau_rho,        &
                           tau_p
use ereport_mod,     only: ereport
use errormessagelength_mod, only: errormessagelength
use nlstcall_nrun_as_crun_mod, only: l_nrun_as_crun
use timestep_mod,    only: timestep_number
use um_parcore,      only: mype
use umPrintMgr,      only: umPrint, printstatus, prstatus_normal, ummessage

use yomhook,         only: lhook, dr_hook
use parkind1,        only: jprb, jpim

implicit none
!
! Description:
!
!
!
! Method:
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: dynamics_advection
!
! Code description:
!   Language: Fortran 90.
!   This code is written to UM programming standards version 8.3.


logical, save :: first_call = .true.
real,    save :: alpha_star,beta_star

character(len=errormessagelength)            :: Cmessage
character(len=15)             :: Routine = 'eg_ramp_alpha'
integer                       :: icode

integer(kind=jpim), parameter :: zhook_in  = 0
integer(kind=jpim), parameter :: zhook_out = 1
real(kind=jprb)               :: zhook_handle

character(len=*), parameter :: RoutineName='EG_ALPHA_RAMP'

if (lhook) call dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

if (first_call) then

  alpha_star = alpha_u
  beta_star  = 1.0-alpha_u
  first_call = .false.

  if (l_nrun_as_crun) then
    alpha_relax_type = alpha_relax_type_constant
    cmessage = 'l_nrun_as_crun is true:' //                                    &
               'Overriding value of alpha_relax_type (now set to 1=constant)'
    icode = -1
    call ereport(RoutineName, icode, cmessage)
  end if

end if

select case (alpha_relax_type)

case (alpha_relax_type_sqrt)

  alpha_u     = beta_star*1.0/dble(timestep_number)**.5 + alpha_star

  alpha_v     = alpha_u
  alpha_w     = alpha_u
  alpha_rho   = alpha_u
  alpha_p     = alpha_u

  alpha_changed = .true.

case (alpha_relax_type_constant)

  !   nothing to be done here, we simply use the alphas from the namelist

case (alpha_relax_type_coslaw)

  if (timestep_number <  alpha_relax_int) then

    alpha_u   = beta_star*cos(pi*dble(timestep_number)/(2.0*dble(alpha_relax_int)))**2 + alpha_star

  else

    alpha_u   = alpha_star

  end if

  alpha_v     = alpha_u
  alpha_w     = alpha_u
  alpha_rho   = alpha_u
  alpha_p     = alpha_u
  alpha_changed = .true.

case (alpha_relax_type_step)

  if (timestep_number <= alpha_relax_int) then

    alpha_u   = 1.0

  else

    alpha_u   = alpha_star

  end if

  alpha_v     = alpha_u
  alpha_w     = alpha_u
  alpha_rho   = alpha_u
  alpha_p     = alpha_u

  alpha_changed = .true.

case DEFAULT

  icode = 1
  cmessage = 'unknown alpha relax type'
  call Ereport(Routine,icode,cmessage)

end select

if ( alpha_changed ) then
  tau_u   = alpha_u
  tau_v   = alpha_v
  tau_w   = alpha_w
  tau_rho = alpha_rho
  tau_p   = alpha_p
end if


if ( mype == 0 .and. PrintStatus > PrStatus_Normal) then
  write(umMessage,fmt='(A,E22.15)') 'ALPHA:', alpha_u
  call umPrint(umMessage,src='eg_alpha_ramp_mod')
end if

if (lhook) call dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

end subroutine
end module
