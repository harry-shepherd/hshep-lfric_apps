! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: MPP
!
! Provides variables that describe the current decomposition, and a routine to
! switch between decompositions


module UM_ParVars

use Field_Types, only:                                                         &
  nfld_max, fld_type_p, fld_type_u, fld_type_v, fld_type_r

use UM_ParParams, only:                                                        &
  pnorth, psouth, peast, pwest, Ndim_max, Nhalo_max, halo_type_extended,       &
  halo_type_single

use decomp_params, only: decomp_unset, max_decomps

use UM_ParCore, only: mype, nproc

use ereport_mod, only: ereport

use Atmos_Max_Sizes, only: Max_Halo_Size, row_length_max, rows_max


use yomhook, only: lhook, dr_hook
use parkind1, only: jprb, jpim

use errormessagelength_mod, only: errormessagelength

implicit none

private

integer(kind=jpim), parameter, private :: zhook_in  = 0
integer(kind=jpim), parameter, private :: zhook_out = 1

! Parameters and globals required by the UM and other execs

integer, public :: current_decomp_type=decomp_unset ! current decomposition type
integer, public :: Offx                   ! standard halo size in East-West
integer, public :: Offy                   ! standard halo size in North-South
integer, public :: halo_i                 ! extended halo size in East-West
integer, public :: halo_j                 ! extended halo size in North-South

integer, public :: gc_proc_row_group      ! GID for procs along a proc row
integer, public :: gc_proc_col_group      ! GID for procs along a proc col
integer, public :: gc_all_proc_group      ! GID for all procs

! logicals indicating if a processor is at the edge of the LPG
logical, public :: at_extremity(4)

! array with the tids of the four neighbours in the horizontal plane
integer, public :: neighbour(4)

! array with the tids of the eight neighbours in the horizontal plane,
! i.e. include the corners.
integer, public :: full_neighbour(8)

! position of personal data in global data (in terms of standard
! Fortran array notation
integer, public :: datastart(Ndim_max)

integer, public :: first_comp_pe       ! top left pe in LPG
integer, public :: last_comp_pe        ! bottom right pe in LPG
integer, public :: halosize(Ndim_max,NHalo_max)        ! available halo sizes
integer, target, public :: glsize(Ndim_max,Nfld_max)   ! global data size
integer, target, public :: blsize(Ndim_max,Nfld_max)   ! personal data size
integer, public :: lasize(Ndim_max,Nfld_max,NHalo_max) ! local data size

! Generalised version of datastart for *all* fieldtypes
integer, public :: datastart_f(Ndim_max,Nfld_max)

! size of the LPG in each dimension
integer, public :: gridsize(Ndim_max)

! position of this process in the LPG 0,1,2,...,nproc_x-1 etc.
integer, public :: gridpos(Ndim_max)

! domain type
integer, public :: sb_model_type

! type of boundary (cyclic or static) in each direction
integer, public :: bound(Ndim_max)

integer, public :: datastartr(Ndim_max)

! Which processor column a given point is in: 0 -> nproc_x-1
integer, public :: g_pe_index_EW(1-Max_Halo_Size:row_length_max+Max_Halo_Size)

! Which processor row a given point is in: 0 -> nproc_y-1
integer, public :: g_pe_index_NS(1-Max_Halo_Size:rows_max+Max_Halo_Size)

integer, public :: nproc_x=1    ! number of processors in x-direction
integer, public :: nproc_y=1    ! number of processors in y-direction


! Short cut names that are used by some rcf code.
integer, pointer, public :: glsizep(:)=>null() ! global u data size
integer, pointer, public :: glsizeu(:)=>null() ! global u data size
integer, pointer, public :: glsizev(:)=>null() ! global v data size
integer, pointer, public :: glsizer(:)=>null() ! global river-routing data size
integer, pointer, public :: blsizep(:)=>null() ! personal p data area
integer, pointer, public :: blsizeu(:)=>null() ! personal u data area
integer, pointer, public :: blsizev(:)=>null() ! personal v data area
integer, pointer, public :: blsizer(:)=>null() ! personal river-routing data
                                               !   area

logical, public ::                                                             &
    atSouth,             &! process at the bottom of the LPG
    atNorth,             &! process at the top of the LPG
    atWest,              &! process at the left of the LPG
    atEast                ! process at the right of the LPG
! NB: None of the above logicals are mutually exclusive

character(len=*), parameter, private :: ModuleName='UM_PARVARS'


end module UM_ParVars
