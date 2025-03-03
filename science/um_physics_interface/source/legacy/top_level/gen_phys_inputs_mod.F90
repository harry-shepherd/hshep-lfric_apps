! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************
!
! Description:
!   Module containing general physics runtime options
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: top_level
!
! Code Description:
!   Language: FORTRAN 95

module gen_phys_inputs_mod

use errormessagelength_mod, only: errormessagelength

implicit none

! Check conservation of moisture by micro-physics, convection, boundary
! layer and methane oxidation.
! See check_dmoist_inc routine (or meta-data) for more details.
logical :: l_check_moist_inc = .false.

! Methane oxidation
logical :: l_use_methox = .false.

! Use mixing ratio in atmos_physics1, atmos_physics2 and end of atm_step_4A
logical :: l_mr_physics = .false.

! Switch for leads temperature -
! If false, leads are set to TFS (freezing temperature of sea water).
! If true, leads temperatures are prognostic values.
logical :: l_leads_temp_prog = .false.

! Switch to use volume-based interpolation of rho to theta-levels is true
! Note that this is only valid when the rho levels lie halfway between the
! theta levels
logical :: l_vol_interp_rho

namelist /gen_phys_inputs/ l_use_methox, l_mr_physics, l_leads_temp_prog,      &
                           l_check_moist_inc, l_vol_interp_rho

character(len=*), parameter, private :: ModuleName='GEN_PHYS_INPUTS_MOD'

contains

!------------------------------------------------------------------------------

subroutine print_nlist_gen_phys_inputs()

use umPrintMgr, only: umPrint
implicit none
character(len=50000) :: lineBuffer

call umPrint('Contents of namelist gen_phys_inputs',                           &
     src='gen_phys_inputs_mod')

write(lineBuffer,'(A,l1)') 'l_use_methox= ',l_use_methox
call umPrint(lineBuffer,src='gen_phys_inputs_mod')
write(lineBuffer,'(A,l1)') 'l_mr_physics  = ',l_mr_physics
call umPrint(lineBuffer,src='gen_phys_inputs_mod')
write(lineBuffer,'(A,l1)') 'l_leads_temp_prog  = ',l_leads_temp_prog
call umPrint(lineBuffer,src='gen_phys_inputs_mod')
write(lineBuffer,'(A,l1)') 'l_check_moist_inc  = ',l_check_moist_inc
call umPrint(lineBuffer,src='gen_phys_inputs_mod')
write(lineBuffer,'(A,l1)') 'l_vol_interp_rho  = ',l_vol_interp_rho
call umPrint(lineBuffer,src='gen_phys_inputs_mod')

call umPrint('- - - - - - end of namelist - - - - - -',                        &
     src='gen_phys_inputs_mod')

end subroutine print_nlist_gen_phys_inputs

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

end module gen_phys_inputs_mod
