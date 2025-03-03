! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: MPP.

module UM_ParParams

implicit none

!   Description:
!   Two sets of parameters are set up -
!     i)  for the MPP-UM itself.
!     ii) for the interface to the Message Passing Software.
!

!=================================================================
! Parameters needed for the MPP-UM
!=================================================================

! maximum number of spatial dimensions
integer,parameter :: Ndim_max           = 3 ! 3d data

! number of different halo types
integer,parameter :: NHalo_max          = 3 ! for N.D. atmos. model

integer,parameter :: halo_type_single   = 1
integer,parameter :: halo_type_extended = 2
integer,parameter :: halo_type_no_halo  = 3

! Used in addressing to indicate if calculation is for a local or
! global (ie. disk dump) size
integer,parameter :: local_data         = 1
integer,parameter :: global_dump_data   = 2

!=================================================================
! Parameters needed for the Message Passing Software
!=================================================================

! Processor addresses in the neighbour array
integer,parameter :: NoDomain    = -1
integer,parameter :: PNorth      = 1
integer,parameter :: PEast       = 2
integer,parameter :: PSouth      = 3
integer,parameter :: PWest       = 4
! Processor addresses for the corner cases
integer,parameter :: PNorthEast  = 5
integer,parameter :: PSouthEast  = 6
integer,parameter :: PSouthWest  = 7
integer,parameter :: PNorthWest  = 8

integer,parameter :: bc_static   = 1 ! Static boundary conditions
integer,parameter :: bc_cyclic   = 2 ! Cyclic boundary conditions
integer,parameter :: bc_overpole = 3 ! Transfer over pole

end module UM_ParParams
