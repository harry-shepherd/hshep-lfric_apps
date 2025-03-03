! *****************************COPYRIGHT********************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT********************************
!
!  Data module for switches for UM inputs,carbon options namelist
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: Carbon

module carbon_options_mod

use missing_data_mod, only: imdi, rmdi
implicit none

! Carbon cycle
! Include surface emissions
logical :: l_co2_emits        = .false.

! Specification of CO2 absorption
! 1 => Simple method with fixed value.
! 2 => Complex method allowing linear and/or exponential variation.
! 3 => From the interactive carbon cycle.
integer :: i_co2_opt = imdi

namelist /carbon_options/  i_co2_opt, l_co2_emits

!===========================================================================
! items not set in namelist
!===========================================================================
! Carbon cycle
! Interactive 3D CO2 field for use with carbon cycle model
! This is set by the check_carbon_options routine based on the value of
! i_co2_opt
logical :: l_co2_interactive  = .false.

character(len=*), parameter, private :: ModuleName='CARBON_OPTIONS_MOD'

contains

subroutine print_nlist_carbon_options()
use umPrintMgr, only: umPrint
implicit none
character(len=50000) :: lineBuffer

call umPrint('Contents of namelist carbon_options',                            &
    src='carbon_options_mod')

write(lineBuffer,'(A,I0)') ' i_co2_opt = ',i_co2_opt
call umPrint(lineBuffer,src='carbon_options_mod')
write(lineBuffer,'(A,L1)') ' l_co2_emits = ',l_co2_emits
call umPrint(lineBuffer,src='carbon_options_mod')

call umPrint('- - - - - - end of namelist - - - - - -',                        &
    src='carbon_options_mod')

end subroutine print_nlist_carbon_options

!-----------------------------------------------------------------------

subroutine check_carbon_options()
! Description:
!   Subroutine to apply logic controls and set control variables based on the
!   options selected in the carbon_options namelist.
implicit none
! Set logicals based on integer choice in namelist
if (i_co2_opt == 3) then
  l_co2_interactive = .true.
end if
return
end subroutine check_carbon_options

!-----------------------------------------------------------------------


!-----------------------------------------------------------------------

end module carbon_options_mod
