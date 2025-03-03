! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Sizes for many of the UM's main, dynamic data arrays
!
module nlsizes_namelist_mod

! Description:
!  Module-based interface to the nlsizes namelist and associated declarations.
!  Contains the sizes needed for the dynamic allocation of the main data arrays
!  within the model.
!
! Code Description:
!  Language: FORTRAN 95.
!  This code is written to UMDP3 v10.3 programming standards.
!
!  Code Owner: Please refer to the UM file CodeOwners.txt
!  This file belongs in section: Grids
!

use missing_data_mod,   only: imdi
use filenamelength_mod, only: filenamelength

! Util code has no dependency on JULES, but also doesn't need sm_levels
! so choosing to exclude this use statement.
#if !defined(UTILIO)
use jules_soil_mod,         only: sm_levels
#endif

implicit none

private :: imdi

character (len=*), private, parameter :: ModuleName  = "NLSIZES_NAMELIST_MOD"

save

! Main sizes of fields for each submodel

! Start of variables read in from nlsizes
integer :: global_row_length = imdi ! Points per global row
integer :: global_rows       = imdi ! Number of global (theta) rows
integer :: model_levels      = imdi ! Number of model levels
integer :: land_field        = imdi ! Number of land points in field
integer :: cloud_levels      = imdi ! Number of cloud-levels
integer :: st_levels         = imdi ! Number of soil temperature levels
integer :: bl_levels         = imdi ! Number of boundary-layer-levels
integer :: ozone_levels      = imdi ! Number of ozone-levels
integer :: river_row_length  = imdi ! Number of river routing points per row
integer :: river_rows        = imdi ! Number of global river routing rows

character(len=filenamelength) :: vert_lev  = 'unset' ! Vertical levels file
character(len=filenamelength) :: var_grid  = 'unset' ! Variable resolution
                                                     ! grid file
! End of variables read in from nlsizes

namelist /nlsizes/                                                             &
  global_row_length, global_rows, land_field,                                  &
  model_levels, cloud_levels, st_levels,                                       &
  bl_levels, ozone_levels, vert_lev, var_grid,                                 &
  river_row_length, river_rows

integer :: row_length           ! Number of points per local row
integer :: rows                 ! Number of local (theta) rows

integer :: ntiles               ! Number of land surface tiles


! Physics-related sizes for atmosphere submodel
integer :: tpps_ozone_levels    ! Number of tropopause-ozone-levels


! Dynamics-related sizes for atmosphere submodel
integer :: tr_levels            ! Number of tracer-levels
integer :: tr_vars              ! Number of passive tracers
integer :: tr_lbc_vars          ! Number of tracers in lbcs
integer :: tr_ukca              ! Number of UKCA tracers
integer :: tr_ukca_actv         ! Number of active UKCA tracers (same as
                                ! tr_ukca, unless this is the Senior UM
                                ! in the hybrid model when it is 0)
integer :: tr_lbc_ukca          ! Number of UKCA tracer lbcs


! For Small executables
integer :: tot_levels


! Grid related sizes for data structure
! Data structure sizes for atmosphere submodel
integer :: a_prog_lookup        ! Number of prognostic fields
integer :: a_prog_len           ! Total length of prognostic fields
integer :: a_len_inthd          ! Length of integer header
integer :: a_len_realhd         ! Length of real header
integer :: a_len2_levdepc       ! Number of level-dependent arrays
integer :: a_len2_rowdepc       ! Number of row-dependent arrays
integer :: a_len2_coldepc       ! Number of column-dependent arrays
integer :: a_len2_flddepc       ! Number of field arrays
integer :: a_len_extcnst        ! Number of extra scalar constants
integer :: a_len_cfi1           ! Length of compressed field index 1
integer :: a_len_cfi2           ! Length of compressed field index 2
integer :: a_len_cfi3           ! Length of compressed field index 3


! Data structure sizes for atmosphere interface file control routines
integer :: n_intf_a              ! Number of atmosphere interface areas
integer :: max_intf_model_levels ! Max. number of model levs in all areas
integer :: max_lbcrow_length     ! Max. number of lbc row length in all areas
integer :: max_lbcrows           ! Max. number of lbc rows in all areas


! Data structure sizes for atmosphere boundary file control routines:
! Sizes applicable to all configurations (dumps/fieldsfile)
integer :: pp_len_inthd          ! Length of PP file integer header
integer :: pp_len_realhd         ! Length of PP file real    header

integer :: aocpl_row_length      ! Atmos row length
integer :: aocpl_p_rows          ! Atmos number of p rows


! Data structure sizes for atmosphere submodel (config dependent)
integer :: a_len2_lookup         ! Total no of fields (incl diags)
integer :: a_len_data            ! Total no of words of data
integer :: a_len_d1              ! Total no of words in atmos D1


! Size of main data array for this configuration
integer :: len_tot               ! Length of D1 array
integer :: n_obj_d1_max          ! Number of objects in D1 array

! global (ie. dump version) of *_len_data
integer :: global_a_len_data

integer :: global_land_field     ! Global number of land points
integer :: local_land_field      ! Local  number of land points

! Fundamental data sizes :
! Fundamental parameter  sizes of data structure
! Sizes applicable to all configurations (history file)

! Length of history file in dump
integer, parameter :: len_dumphist = 0

! Sizes applicable to all configurations (dumps/fieldsfile)
! Length of dump fixed header
integer, parameter :: len_fixhd = 256

! Size of a single LOOKUP header
integer, parameter :: len1_lookup     = 64
integer, parameter :: mpp_len1_lookup = 2

! Size of compressed LBC LOOKUP (only used internally and
! contains just the items which change between each set of LBCs
integer, parameter :: len1_lbc_comp_lookup = 8

! Sizes applicable to all configurations (stash)
! Moved to typstsz.h

integer :: intf_len2_levdepc    ! 1st dim of interface out lev dep constants
integer :: intf_len2_rowdepc    ! 2nd dim of interface out row dep constants
integer :: intf_len2_coldepc    ! 2nd dim of interface out col dep constants

! sub-model atmosphere   :
! Data structure sizes derived from grid size
integer :: a_len1_levdepc       ! 1st dim of level  dep constants
integer :: a_len1_rowdepc       ! 1st dim of row    dep constants
integer :: a_len1_coldepc       ! 1st dim of column dep constants
integer :: a_len1_flddepc       ! 1st dim of field  dep constants

! Data structure sizes for atmosphere interface file control routines
integer :: intf_lookupsa        ! No of interface lookups.

! sub-model atmosphere   : derived sizes
! derived from model grid/levels. Arakawa B-grid

! Size of fields on THETA grid ...
integer :: theta_field_size     ! ... with no halos
integer :: theta_off_size       ! ... with simple halos
integer :: theta_halo_size      ! ... with extended halos

! Size of fields on u grid ...
integer :: u_field_size         ! ... with no halos
integer :: u_off_size           ! ... with simple halos
integer :: u_halo_size          ! ... with extended halos

! Size of fields on v grid ...
integer :: v_field_size         ! ... with no halos
integer :: v_off_size           ! ... with simple halos
integer :: v_halo_size          ! ... with extended halos

integer :: n_rows               ! Number of V-rows
integer :: n_cca_lev            ! Number of CCA levels


contains

!-----------------------------------------------------------------------

subroutine print_nlist_nlsizes()
use umPrintMgr, only: umPrint
implicit none
character(len=50000) :: lineBuffer

call umPrint('Contents of namelist nlsizes',                                   &
  src='nlsizes_namelist_mod')

write(lineBuffer,'(A,I0)')' global_row_length = ', global_row_length
call umPrint(lineBuffer,src='nlsizes_namelist_mod')
write(lineBuffer,'(A,I0)')' global_rows       = ', global_rows
call umPrint(lineBuffer,src='nlsizes_namelist_mod')
write(lineBuffer,'(A,I0)')' land_field        = ', land_field
call umPrint(lineBuffer,src='nlsizes_namelist_mod')
write(lineBuffer,'(A,I0)')' model_levels      = ', model_levels
call umPrint(lineBuffer,src='nlsizes_namelist_mod')
write(lineBuffer,'(A,I0)')' cloud_levels      = ', cloud_levels
call umPrint(lineBuffer,src='nlsizes_namelist_mod')
write(lineBuffer,'(A,I0)')' st_levels         = ', st_levels
call umPrint(lineBuffer,src='nlsizes_namelist_mod')
write(lineBuffer,'(A,I0)')' bl_levels         = ', bl_levels
call umPrint(lineBuffer,src='nlsizes_namelist_mod')
write(lineBuffer,'(A,I0)')' ozone_levels      = ', ozone_levels
call umPrint(lineBuffer,src='nlsizes_namelist_mod')
write(lineBuffer, '(A,A)')' vert_lev          = ', trim(vert_lev)
call umPrint(lineBuffer,src='nlsizes_namelist_mod')
write(lineBuffer, '(A,A)')' var_grid          = ', trim(var_grid)
call umPrint(lineBuffer,src='nlsizes_namelist_mod')
write(lineBuffer,'(A,I0)')' river_row_length  = ', river_row_length
call umPrint(lineBuffer,src='nlsizes_namelist_mod')
write(lineBuffer,'(A,I0)')' river_rows        = ', river_rows
call umPrint(lineBuffer,src='nlsizes_namelist_mod')

call umPrint('- - - - - - end of namelist - - - - - -',                        &
  src='nlsizes_namelist_mod')

end subroutine print_nlist_nlsizes

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

subroutine check_nlsizes()
! Description:
!   Subroutine to apply logic and range checking on variables based on
!   the options set in the nlsizes namelist

use atmos_max_sizes,        only: row_length_max, rows_max,                    &
                                  model_levels_max
use chk_opts_mod,           only: chk_var, def_src
use errormessagelength_mod, only: errormessagelength
use ereport_mod,            only: ereport
use umPrintMgr,             only: newline
use parkind1,               only: jprb, jpim
use yomhook,                only: lhook, dr_hook
implicit none

character (len=errormessagelength) :: chk_string
character (len=*), parameter :: RoutineName = "CHECK_NLSIZES"
character (len=errormessagelength) :: cmessage
integer :: icode
logical :: pass_log(9) = .false.

integer(kind=jpim), parameter :: zhook_in  = 0
integer(kind=jpim), parameter :: zhook_out = 1
real(kind=jprb)               :: zhook_handle

if (lhook) call dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

def_src = ModuleName//':'//RoutineName

write(chk_string, '(A3,I0,A1)')'[2:',row_length_max,']'
call chk_var(global_row_length, "global_row_length", trim(chk_string),         &
             report_pass=pass_log(1) )
if (modulo(global_row_length, 2) /= 0) then
  write(cmessage, '(A,I0,A)') "global_row_length = ", global_row_length,       &
       newline // 'global_row_length must be even'
  icode = 10
  call ereport (def_src, icode, cmessage)
end if

write(chk_string, '(A3,I0,A1)')'[1:',rows_max,']'
call chk_var(global_rows, "global_rows", trim(chk_string),                     &
             report_pass=pass_log(2) )

write(chk_string, '(A3,I0,A1)')'[1:',model_levels_max,']'
call chk_var(model_levels, "model_levels", trim(chk_string) ,                  &
              report_pass=pass_log(3) )

call chk_var(land_field, "land_field", "[-99,>=0]", report_pass=pass_log(4) )

write(chk_string, '(A,I0,A)')'[1:',model_levels,']'
call chk_var(bl_levels, "bl_levels", chk_string, report_pass=pass_log(5) )
call chk_var(cloud_levels, "cloud_levels", chk_string, report_pass=pass_log(6) )
call chk_var(ozone_levels, "ozone_levels", chk_string, report_pass=pass_log(7) )

call chk_var(st_levels, "st_levels", "[>=1]", report_pass=pass_log(8) )
write(chk_string, '(A,I0,A)')'[==',st_levels,']'
#if !defined(UTILIO)
call chk_var(sm_levels, "sm_levels", chk_string, report_pass=pass_log(9) )
#endif

! var_grid not yet checked here. A string detailing the path to the Variable
! resolution grid file. Only used by recon for variable resolution dumps
! If required, value is checked before use

! vert_lev not yet checked here. A string detailing the path to the vertical
! levels namelist file. Only used by recon
! If required, value is checked before use

if (any(.not. pass_log)) then
  icode = 20
  write(cmessage, '(I0,A)') count(.not. pass_log),                             &
       ' errors have been found in the nlsizes namelist'                       &
       // newline //                                                           &
       'Please see output for details and then correct the namelist'
  call ereport(def_src, icode, cmessage)
end if

def_src = ''
if (lhook) call dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
end subroutine check_nlsizes

end module nlsizes_namelist_mod
