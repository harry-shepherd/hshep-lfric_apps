! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Description:
!
!  UM module for calculating humidity-related fields for UKCA.
!
! Part of the UKCA model, a community model supported by the
! Met Office and NCAS, with components provided initially
! by The University of Cambridge, University of Leeds and
! The Met. Office.  See www.ukca.ac.uk
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: UKCA_UM
!
! Code Description:
!   Language:  FORTRAN 2003
!   This code is written to UMDP3 programming standards.
!
! ----------------------------------------------------------------------

module atmos_ukca_humidity_mod

implicit none
private

character(len=*), parameter :: ModuleName='ATMOS_UKCA_HUMIDITY_MOD'

public :: atmos_ukca_humidity

contains

subroutine atmos_ukca_humidity(row_length, rows, model_levels,                 &
                               t_theta_levels, p_theta_levels, q, rh,          &
                               qsatmr_wat, qsatmr_ice_wat, svp)

use qsat_mod,              only: qsat, qsat_wat_mix
use planet_constants_mod,  only: repsilon

use ereport_mod,            only: ereport
use errormessagelength_mod, only: errormessagelength

use parkind1,              only: jpim, jprb      ! DrHook
use yomhook,               only: lhook, dr_hook  ! DrHook

implicit none

! Subroutine arguments
integer, intent(in) :: row_length
integer, intent(in) :: rows
integer, intent(in) :: model_levels
real, intent(in) :: t_theta_levels(row_length, rows, model_levels)
  ! Temperature on theta levels (K)
real, intent(in) :: p_theta_levels(row_length, rows, model_levels)
  ! Pressure on theta levels (Pa)
real, optional, intent(in) :: q(row_length, rows, model_levels)
  ! Specific humidity
real, optional, intent(out) :: rh(row_length, rows, model_levels)
  ! Relative humidity
real, optional, intent(out) :: qsatmr_wat(row_length, rows, model_levels)
  ! Saturation mixing ratio with respect to liquid water irrespective of
  ! temperature
real, optional, intent(out) :: qsatmr_ice_wat(row_length, rows, model_levels)
  ! Saturation mixing ratio with respect to liquid water (T > 0C) or
  ! ice (T < 0C)
  !!!! NOTE: the the actual return value is currently the equivalent saturation
  !!!! specific humidity. See note below about corrections needed to this
  !!!! subroutine.
real, optional, intent(out) :: svp(row_length, rows, model_levels)
  ! Saturation vapour pressure with respect to liquid water
  ! irrespective of temperature

! Local variables
integer :: k
real, allocatable :: qsmr_wat(:,:,:)
  ! sat mixing ratio with respect to liquid water irrespective of temperature

! ErrorStatus
integer :: errcode                               ! Error flag
character(len=errormessagelength) :: cmessage    ! Error return message

integer (kind=jpim), parameter :: zhook_in  = 0  ! DrHook tracing entry
integer (kind=jpim), parameter :: zhook_out = 1  ! DrHook tracing exit
real    (kind=jprb)            :: zhook_handle   ! DrHook tracing

character(len=*), parameter :: RoutineName='ATMOS_UKCA_HUMIDITY'

! End of header

if (lhook) call dr_hook(ModuleName//':'//RoutineName, zhook_in, zhook_handle)

if (present(rh) .and. (.not. present(q))) then
  errcode = 1
  write(cmessage, '(A)') 'Missing argument q for calculating relative humidity'
  call ereport(RoutineName, errcode, cmessage)
end if

!!!! NOTE: Corrections are required to calculations below which currently treat
!!!! specific humidity and water vapour mixing ratio as interchangeable.
!!!! The necessary corrections are flagged here and a related issue requiring
!!!! correction in subroutine ukca_main1 is similarly flagged and should be
!!!! addressed at the same time.

allocate(qsmr_wat(row_length, rows, model_levels))

!$OMP PARALLEL do SCHEDULE(STATIC) DEFAULT(none) private(k)                    &
!$OMP SHARED(model_levels,qsmr_wat,t_theta_levels,                             &
!$OMP p_theta_levels,rh,q,qsatmr_wat,qsatmr_ice_wat,svp,repsilon)
do k = 1, model_levels
  ! Calculate saturation mixing ratio with respect to water.
  call qsat_wat_mix(qsmr_wat(:,:,k),                                           &
                    t_theta_levels(:,:,k), p_theta_levels(:,:,k),              &
                    size(t_theta_levels(:,1,k)), size(t_theta_levels(1,:,k)))
  ! Calculate relative humidity fraction
  !!!! CORRECTION NEEDED: numerator should be a mixing ratio.
  if (present(rh)) rh(:,:,k) = q(:,:,k) / qsmr_wat(:,:,k)
  ! Copy saturation mixing ratio w.r.t. water from local array.
  if (present(qsatmr_wat)) qsatmr_wat(:,:,k) = qsmr_wat(:,:,k)
  ! Calculate saturation mixing ratio with respect to
  ! ice for T < 0C or water for T > 0C.
  !!!! CORRECTION NEEDED: qsat returns saturation specific humidity but
  !!!! saturation mixing ratio is required so qsat_mix should be
  !!!! called instead.
  if (present(qsatmr_ice_wat))                                                 &
    call qsat(qsatmr_ice_wat(:,:,k),                                           &
              t_theta_levels(:,:,k), p_theta_levels(:,:,k),                    &
              size(t_theta_levels(:,1,k)),size(t_theta_levels(1,:,k)))
  ! Derive saturation vapour pressure from saturated mixing ratio
  !!!! CORRECTION NEEDED: the formula below is that for deriving SVP from
  !!!! specific humidity but qsmr_wat is a mixing ratio.
  if (present(svp))                                                            &
    svp(:,:,k) = qsmr_wat(:,:,k) * p_theta_levels(:,:,k) /                     &
                 (repsilon + qsmr_wat(:,:,k) * (1.0 - repsilon))
end do
!$OMP end PARALLEL do

deallocate(qsmr_wat)

if (lhook) call dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
return

end subroutine atmos_ukca_humidity

end module atmos_ukca_humidity_mod

