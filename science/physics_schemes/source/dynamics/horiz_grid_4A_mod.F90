! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!

module horiz_grid_mod
implicit none

!
! Description: Contains the horizontal ENDGame grid.
!
!
! Method:
!
! Documentation: ENDGame formulation version 1.01
!
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: dynamics
!
! Code description:
!   Language: Fortran 90.
!   This code is written to UM programming standards version 8.3.

! Co-ordinate system
! True = Cartesian grid, False = Long-Lat grid
logical, save :: cartesian_grid = .false.

! Horizontal coordinates

type :: horzg_t
  real, pointer ::                                                             &
    xi1_p(:)=>null(),                                                          &
    xi1_u(:)=>null(),                                                          &
    xi2_p(:)=>null(),                                                          &
    xi2_v(:)=>null()
end type horzg_t

real,save, target, allocatable ::                                              &
  xi1_p(:),                                                                    &
  xi1_u(:),                                                                    &
  xi2_p(:),                                                                    &
  xi2_v(:)

real,save, allocatable ::                                                      &
  glob_xi1_p(:),                                                               &
  glob_xi1_u(:),                                                               &
  glob_xi2_p(:),                                                               &
  glob_xi2_v(:)

real,save, allocatable ::                                                      &
  glob_rdxi1_p(:),                                                             &
  glob_rdxi1_u(:),                                                             &
  glob_rdxi2_p(:),                                                             &
  glob_rdxi2_v(:)


! Horizontal & Vertical coordinates

real,save, allocatable ::                                                      &
  csxi1_p(:),                                                                  &
  csxi1_u(:),                                                                  &
  csxi2_p(:),                                                                  &
  csxi2_v(:)

real,save, allocatable ::                                                      &
  snxi1_p(:),                                                                  &
  snxi1_u(:),                                                                  &
  snxi2_p(:),                                                                  &
  snxi2_v(:)

real, save, allocatable ::                                                     &
  phi_at_p(:,:,:),                                                             &
  phi_at_u(:,:,:),                                                             &
  phi_at_v(:,:,:),                                                             &
  phi_at_eta(:,:,:)

!
! Linear Interpolation weights
!

real, save, allocatable ::                                                     &
  intw_u2p(:,:),                                                               &
  intw_v2p(:,:),                                                               &
  intw_p2u(:,:),                                                               &
  intw_p2v(:,:),                                                               &
  intw_rho2w(:,:),                                                             &
  intw_w2rho(:,:)

! Area of grid cell on lower boundary
real, save, allocatable :: cell_area_surface(:,:)

! Constant resolution parameters
real, save :: base_xi1  = 0.0
real, save :: base_xi2  = 0.0
real, save :: delta_xi1 = 0.0
real, save :: delta_xi2 = 0.0

! Variable resolution parameters
integer, save :: Nxi1L = 0
integer, save :: Nxi1V = 0
integer, save :: Nxi2L = 0
integer, save :: Nxi2V = 0
real, save :: delta_xi1_H = 0.0
real, save :: delta_xi1_L = 0.0
real, save :: delta_xi2_H = 0.0
real, save :: delta_xi2_L = 0.0

end module horiz_grid_mod
