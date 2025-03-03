! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
! Immersed Boundary data structures

! Description:
!   Module containing data used by the Immersed Boundary Method (IBM)
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: Dynamics

! Method:
!     Sets default parameters, and datastructures required to
!     run the Immersed Boundary code.

! Code Description:
!   Language: FORTRAN 90
!   This code is written to UMDP3

module imbnd_data
use filenamelength_mod, only: filenamelength
use missing_data_mod,   only: rmdi, imdi

implicit none

! Derived type for storing the (x,y,z) positions of
! the immersed boundary as well as the 3 components of
! the normal vector - currently Cartesian.

type :: ib_surf_data
  sequence
  real :: x
  real :: y
  real :: z
  real :: ni
  real :: nj
  real :: nk
end type

! This parameter sets the possible (maximum) number of
! cells touched by the discrete delta function about an
! immersed boundary point

integer, parameter                             :: Nl = 32

logical                                        :: l_ib_noslip   = .false.
logical                                        :: l_use_ib_mask  = .false.
character (len=filenamelength)                 :: ib_filename   = ""

real                                           :: alpha = rmdi, beta = rmdi
real                                           :: damping = rmdi

real,    parameter                             :: min_ib_ht = 5.0

! Dirac delta function formulas

integer, parameter                             :: simple   = 1
integer, parameter                             :: smoothx3 = 2
integer, parameter                             :: del_type = smoothx3

! These are used as array ranges based upon the delta function
! width

integer                                     :: ib_u_i_start, ib_u_i_end
integer                                     :: ib_v_i_start, ib_v_i_end
integer                                     :: ib_w_i_start, ib_w_i_end

integer                                     :: ib_u_j_start, ib_u_j_end
integer                                     :: ib_v_j_start, ib_v_j_end
integer                                     :: ib_w_j_start, ib_w_j_end

integer                                     :: ib_k_end, ib_halo

! This variable the number of immersed points and is used
! to allocate the subsequent arrays

integer                                     :: n_p_pnts
integer                                     :: n_ib_pnts_rd

! The list of immersed surface points are stored in this
! derived type

type(ib_surf_data), allocatable             :: ib_surf(:)
type(ib_surf_data), allocatable             :: rd_ib_dataset(:)

! These arrays store the number of u,v,w points effected by
! each immersed boundary (IB)  point.

integer, allocatable                        :: No_u_ib_pnts(:)
integer, allocatable                        :: No_v_ib_pnts(:)
integer, allocatable                        :: No_w_ib_pnts(:)

! These are the gid points, on the staggered grid, effected
! by each IB point.

integer, allocatable                        :: is_u(:,:), js_u(:,:), ks_u(:,:)
integer, allocatable                        :: is_v(:,:), js_v(:,:), ks_v(:,:)
integer, allocatable                        :: is_w(:,:), js_w(:,:), ks_w(:,:)

logical, allocatable                        :: interior_pnt(:,:,:)

! The values of the distrete delta function for the u,v,w velocity
! components are stored in this array

real,    allocatable                        :: D_u(:,:), D_v(:,:), D_w(:,:)

! velocities on the immersed surface and integrated history
! go in these arrays - one for each velocity component is
! required if using no-slip boundary condition

real,    allocatable                        :: u_s(:), us_int(:)
real,    allocatable                        :: v_s(:), vs_int(:)
real,    allocatable                        :: w_s(:), ws_int(:)

! When l_use_ib_mask is .true. these arrays contain the velocity
! mask: they are .true. in the fluid and .false. inside the orography.
! Note - they are also 1 on the distributed surface!

logical, allocatable                        :: u_mask(:,:,:)
logical, allocatable                        :: v_mask(:,:,:)
logical, allocatable                        :: w_mask(:,:,:)

private :: delta
contains

function dirac(x,y,z, dx,dy,dz)
implicit none
real,                          intent(in)   :: x, y, z
real,                          intent(in)   :: dx, dy, dz
real                                        :: dirac

dirac = delta(x,dx)*delta(y,dy)*delta(z,dz)

end function dirac

function delta(x,h)
implicit none

real,                       intent(in)   :: x,h
real                                     :: delta
real                                     :: s, r

delta = 0.0

select case(del_type)
case (simple)
  s = abs(x/h)
  if ( s < 1.0 ) delta = (1.0 - s)
case (smoothx3)
  r = x/h
  s = abs(r)
  if ( s < 0.5 ) then
    delta = (1.0 + sqrt(1.0-3.0*r**2))/3.0
  else
    if ( s <= 1.5 ) then
      delta = sqrt(1.0 - 3.0*(1.0 - s)**2)
      delta = ( 5.0 - 3.0*s - delta )/6.0
    end if
  end if
end select

end function delta

end module imbnd_data
