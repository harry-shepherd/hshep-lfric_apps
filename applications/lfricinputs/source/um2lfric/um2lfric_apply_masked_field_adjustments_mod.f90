! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file LICENCE
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
module um2lfric_apply_masked_field_adjustments_mod

! Intrinsic modules
use, intrinsic :: iso_fortran_env, only : int64, real64

implicit none

private

public :: um2lfric_apply_masked_field_adjustments


contains

subroutine um2lfric_apply_masked_field_adjustments(stashcode, src, dst)

use lfricinp_stashmaster_mod,              only: land_compressed,      &
                                                 grid,                 &
                                                 get_stashmaster_item, &
                                                 p_points_values_over_sea
use um2lfric_masked_field_adjustments_mod, only: land_field_adjustments, &
                                                 maritime_field_adjustments

implicit none

! Arguments
integer(kind=int64), intent(in) :: stashcode
real(kind=real64), intent(in) :: src(:,:)
real(kind=real64), intent(in out) :: dst(:)

! Get grid code from stashmaster
select case (get_stashmaster_item(stashcode, grid))

  case(land_compressed)
    call land_field_adjustments%apply_masked_adjustment_src_2d_dst_1d(src,     &
                                                                      dst)
  case(p_points_values_over_sea)
    call maritime_field_adjustments%apply_masked_adjustment_src_2d_dst_1d(src, &
                                                                      dst)

end select

end subroutine um2lfric_apply_masked_field_adjustments

end module um2lfric_apply_masked_field_adjustments_mod

