! DECLARE_ATM_FIELDS_MOD
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: top_level

! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

module atm_fields_mod

!JULES type definitions
use crop_vars_mod, only: crop_vars_type, crop_vars_data_type
use p_s_parms,     only: psparms_type, psparms_data_type
use top_pdm,       only: top_pdm_type, top_pdm_data_type
use fire_vars_mod, only: fire_vars_type, fire_vars_data_type
use ancil_info,    only: ainfo_type, ainfo_data_type
use trif_vars_mod, only: trif_vars_type, trif_vars_data_type
use soil_ecosse_vars_mod, only: soil_ecosse_vars_type,                         &
                                soil_ecosse_vars_data_type
use aero,          only: aero_type, aero_data_type
use urban_param_mod, only: urban_param_type, urban_param_data_type
use prognostics, only: progs_type, progs_data_type
use trifctl,       only: trifctl_type, trifctl_data_type
use coastal,       only: coastal_data_type, coastal_type
use jules_vars_mod, only: jules_vars_data_type, jules_vars_type
use fluxes_mod, only: fluxes_data_type, fluxes_type
use lake_mod, only: lake_data_type, lake_type
use jules_forcing_mod, only: forcing_data_type, forcing_type
use jules_rivers_mod, only: rivers_data_type, rivers_type
! use veg3_parm_mod, only: in_dev
! use veg3_field_mod, only: in_dev
use jules_chemvars_mod, only: chemvars_data_type, chemvars_type
use water_resources_vars_mod, only: water_resources_data_type,                 &
                                    water_resources_type
use jules_wtrac_type_mod,   only: jls_wtrac_type, jls_wtrac_data_type

! UM type definitions
use water_tracers_mod, only: wtrac_type, sfc_wtrac_type

use free_tracers_inputs_mod, only: n_wtrac

implicit none

!     TRACERS are a special case
      ! dummy array used to prevent crashing when there are no tracers
real, target  :: dummy_field(1) = [-1.0]
real, target  :: dummy_fld2(1,1) = -1.0
real, target  :: dummy_fld3(1,1,1) = -1.0
real, target  :: dummy_fld4(1,1,1,1) = -1.0
real, pointer :: tracer(:,:,:,:)
real, pointer :: tracer_ukca(:,:,:,:)
real, pointer :: tracer_ukca_lbc(:,:,:)
real, pointer :: tracer_ukca_lbc_tend(:,:,:)
real, pointer :: tracer_lbc(:,:,:)
real, pointer :: tracer_lbc_tend(:,:,:)
!     Add a specific pointer for ozone tracer and cariolle parameters
real, pointer :: ozone_tracer(:,:,:)
real, pointer :: o3_prod_loss(:,:)
real, pointer :: o3_p_l_vmr(:,:)
real, pointer :: o3_vmr(:,:)
real, pointer :: o3_p_l_temp(:,:)
real, pointer :: o3_temp(:,:)
real, pointer :: o3_p_l_colo3(:,:)
real, pointer :: o3_colo3(:,:)
! 1: Array Variables (dimensions are resolution dependent.)

! Oxidant concentrations from UKCA for use in HadGEM sulphur cycle
real, pointer :: oh_ukca(:,:,:)
real, pointer :: h2o2_ukca(:,:,:)
real, pointer :: ho2_ukca(:,:,:)
real, pointer :: o3_ukca(:,:,:)
real, pointer :: hno3_ukca(:,:,:)

! Used for ACTIVATE in the hybrid resolution model
real, pointer :: tke_activ_hyb(:,:,:)

! 1.1: Data variables stored in primary space.

real, pointer :: sst(:,:)

real, pointer :: DryRho(:,:,:)
real, pointer :: EtaDot(:,:,:)
real, pointer :: ThetaV(:,:,:)
real, pointer :: psi_w_surf(:,:)
real, pointer :: psi_w_lid(:,:)
real, pointer :: m_v(:,:,:)
real, pointer :: m_cl(:,:,:)
real, pointer :: m_cf(:,:,:)
real, pointer :: m_cf2(:,:,:)
real, pointer :: m_gr(:,:,:)
real, pointer :: m_r(:,:,:)
real, pointer :: exner_surf(:,:)
real, pointer :: exner(:,:,:)

real, pointer :: u(:,:,:)      ! u component of wind
real, pointer :: v(:,:,:)      ! v component of wind
real, pointer :: w(:,:,:)      ! w component of wind
real, pointer :: wetrho_r_sq_n(:,:,:)    ! Density
real, pointer :: rho(:,:,:)    ! Density
real, pointer :: theta(:,:,:)  ! Potential temperature
real, pointer :: q(:,:,:)        ! Specific humidity
real, pointer :: qcl(:,:,:)      ! qcl
real, pointer :: qcf(:,:,:)      ! qcf
real, pointer :: qcf2(:,:,:)     ! second ice
real, pointer :: qrain(:,:,:)    ! rain
real, pointer :: qgraup(:,:,:)   ! graupel

! Atmospheric Electricity / Lightning scheme
real, pointer :: flash_pot(:,:,:) ! lightning flash potential

! Boundary layer scheme w-variance for turbulent production
! of mixed phase clouds
real, pointer :: bl_w_var(:,:,:) ! bl w-variance

! TKE based turbulence scheme
real, pointer :: e_trb(:,:,:)     ! TKE
real, pointer :: tsq_trb(:,:,:)   ! Self covariance of thetal'
real, pointer :: qsq_trb(:,:,:)   ! Self coveriance of qw'
real, pointer :: cov_trb(:,:,:)   ! Correlation between thetal' and qw'
real, pointer :: zhpar_shcu(:,:)  ! Height of mixed layer used to
                                  ! evaluate the non-grad buoy flux

!PV-tracers
real, pointer :: dPV_rad(:,:,:)
real, pointer :: dPV_sw(:,:,:)
real, pointer :: dPV_lw(:,:,:)
real, pointer :: dPV_mic(:,:,:)
real, pointer :: dPV_gwd(:,:,:)
real, pointer :: dPV_ph1(:,:,:)
real, pointer :: dPV_conv(:,:,:)
real, pointer :: dPV_bl(:,:,:)
real, pointer :: dPV_stph(:,:,:)
real, pointer :: dPV_cld(:,:,:)
real, pointer :: dPV_iau(:,:,:)
real, pointer :: dPV_nud(:,:,:)
real, pointer :: dPV_tot(:,:,:)
real, pointer :: dEPS_I(:,:,:)
real, pointer :: dPV_sol(:,:,:)
real, pointer :: dPV_mass(:,:,:)
real, pointer :: dPV_0(:,:,:)
real, pointer :: dPV_conv_d(:,:,:)
real, pointer :: dPV_conv_f(:,:,:)
real, pointer :: dPV_bl_d(:,:,:)
real, pointer :: dPV_bl_f(:,:,:)
real, pointer :: dPV_PC2c(:,:,:)

!Theta tracers
real, pointer :: dtheta_0(:,:,:)
real, pointer :: dtheta_bl(:,:,:)
real, pointer :: dtheta_bl_mix(:,:,:)
real, pointer :: dtheta_bl_LH(:,:,:)
real, pointer :: dtheta_conv(:,:,:)
real, pointer :: dtheta_mic(:,:,:)
real, pointer :: dtheta_rad(:,:,:)
real, pointer :: dtheta_SW(:,:,:)
real, pointer :: dtheta_LW(:,:,:)
real, pointer :: dtheta_slow(:,:,:)
real, pointer :: dtheta_cld(:,:,:)
real, pointer :: dtheta_PC2c(:,:,:)

! Exner pressure on rho levels
real, pointer :: exner_rho_levels(:,:,:)

real, pointer :: u_adv(:,:,:) ! Advective u component of wind
real, pointer :: v_adv(:,:,:) ! Advective v component of wind
real, pointer :: w_adv(:,:,:) ! Advective w component of wind
real, target, allocatable :: U_ADV_nodump(:)
real, target, allocatable :: V_ADV_nodump(:)
real, target, allocatable :: W_ADV_nodump(:)

! Convective prognostics
real, pointer :: conv_prog_dtheta(:,:,:) ! Time-smoothed conv. theta increment
real, pointer :: conv_prog_dq(:,:,:)     ! Time-smoothed conv. humidity incr
real, pointer :: conv_prog_flx(:,:,:)    ! Time-smoothed conv. mass flux
real, pointer :: conv_prog_precip(:,:,:) ! Convection prognostic recent precip

! Total precipitation at start of timestep (for input to gw_ussp source calc.)
real, pointer :: totalppn(:,:)

! Stochastic physics fields for BL perturbations
real, pointer :: bl_pert_rand_fld(:,:)
real, pointer :: bl_pert_flag(:,:)

! 1.2: Data variables stored in secondary space.
real, pointer :: p(:,:,:)                  ! Pressure on rho levels

! Pressure on theta levels
real, pointer :: p_theta_levels(:,:,:)

! Exner pressure on theta levels
real, pointer :: exner_theta_levels(:,:,:)

! 1.3: Cloud Fields
real, pointer :: cca(:,:,:)                ! Convective cloud amount
real, pointer :: cf_area(:,:,:)            ! Area Cloud Fraction
real, pointer :: cf_bulk(:,:,:)            ! Bulk Cloud Fraction
real, pointer :: cf_liquid(:,:,:)          ! Liquid cloud fraction
real, pointer :: cf_frozen(:,:,:)          ! Frozen cloud fraction
real, pointer :: precfrac(:,:,:)           ! Precipitation fraction

! 1.4: Soil Ancillary fields
real, pointer :: deep_soil_temp(:,:)   ! Deep soil temperature

real, pointer :: smcl(:,:)   ! Soil moisture content in layers
real, pointer :: sthu(:,:)   ! Unfrozen soil moisture fraction
real, pointer :: sthf(:,:)   ! Frozen soil moisture fraction

! 1.5: Radiation Increments
real, pointer :: sw_incs(:,:,:)    ! SW radiation increments
real, pointer :: lw_incs(:,:,:)    ! LW radiation increments
! PAR radiation increment
real, pointer :: dirpar(:,:)

! 1.6: Ozone
real, pointer :: o3(:)          ! Ozone
!  tropopause-based ozone
real, pointer :: tppsozone(:)
! 1.7: Tracer and aerosol fields
real, pointer :: murk_source(:,:,:)    ! multi-level murk source
real, pointer :: murk(:,:,:)           ! multi-level murk concent
real, pointer :: dust_div1(:,:,:)      ! dust mmr, division 1
real, pointer :: dust_div2(:,:,:)      ! dust mmr, division 2
real, pointer :: dust_div3(:,:,:)      ! dust mmr, division 3
real, pointer :: dust_div4(:,:,:)      ! dust mmr, division 4
real, pointer :: dust_div5(:,:,:)      ! dust mmr, division 5
real, pointer :: dust_div6(:,:,:)      ! dust mmr, division 6
real, pointer :: so2(:,:,:)            ! sulphur dioxide gas
real, pointer :: dms(:,:,:)            ! dimethyl sulphide gas
real, pointer :: so4_aitken(:,:,:)     ! Aitken mode sulphate aer
real, pointer :: so4_accu(:,:,:)       ! accumulation mode sulpha
real, pointer :: so4_diss(:,:,:)       ! dissloved  sulphate aero
real, pointer :: h2o2(:,:,:)           ! hydrogen peroxide mmr
real, pointer :: nh3(:,:,:)            ! ammonia gas mmr
real, pointer :: soot_new(:,:,:)       ! fresh soot mmr
real, pointer :: soot_agd(:,:,:)       ! aged soot mmr
real, pointer :: soot_cld(:,:,:)       ! soot in cloud mmr
real, pointer :: bmass_new(:,:,:)      ! fresh biomass mmr
real, pointer :: bmass_agd(:,:,:)      ! aged biomass mmr
real, pointer :: bmass_cld(:,:,:)      ! cloud biomass mmr
real, pointer :: ocff_new(:,:,:)       ! fresh OCFF mmr
real, pointer :: ocff_agd(:,:,:)       ! aged OCFF mmr
real, pointer :: ocff_cld(:,:,:)       ! OCFF in cloud mmr
real, pointer :: so2_natem(:,:,:)      ! natural SO2 emissions
real, pointer :: oh(:,:,:)             ! hydroxyl radical ancilla
real, pointer :: ho2(:,:,:)            ! hydrogen dioxide ancilla
real, pointer :: h2o2_limit(:,:,:)     ! limiting H2O2 ancillary
real, pointer :: o3_chem(:,:,:)        ! ozone for chemistry anci
real, pointer :: nitr_acc(:,:,:)       ! accumulation ammonium nitrate
real, pointer :: nitr_diss(:,:,:)      ! dissolved ammonium nitrate
real, pointer :: co2(:,:,:)            ! 3D CO2 FIELD
! 1.7a: GLOMAP_CLIM aerosol section 54
real, pointer :: gc_nd_nuc_sol(:,:,:)  ! GLOMAP_CLIM NUC (Sol) number
real, pointer :: gc_nuc_sol_su(:,:,:)  ! GLOMAP_CLIM NUC (Sol) SO4
real, pointer :: gc_nuc_sol_oc(:,:,:)  ! GLOMAP_CLIM NUC (Sol) OC
real, pointer :: gc_nd_ait_sol(:,:,:)  ! GLOMAP_CLIM AIT (Sol) number
real, pointer :: gc_ait_sol_su(:,:,:)  ! GLOMAP_CLIM AIT (Sol) SO4
real, pointer :: gc_ait_sol_bc(:,:,:)  ! GLOMAP_CLIM AIT (Sol) BC
real, pointer :: gc_ait_sol_oc(:,:,:)  ! GLOMAP_CLIM AIT (Sol) OC
real, pointer :: gc_nd_acc_sol(:,:,:)  ! GLOMAP_CLIM ACC (Sol) number
real, pointer :: gc_acc_sol_su(:,:,:)  ! GLOMAP_CLIM ACC (Sol) SO4
real, pointer :: gc_acc_sol_bc(:,:,:)  ! GLOMAP_CLIM ACC (Sol) BC
real, pointer :: gc_acc_sol_oc(:,:,:)  ! GLOMAP_CLIM ACC (Sol) OC
real, pointer :: gc_acc_sol_ss(:,:,:)  ! GLOMAP_CLIM ACC (Sol) SS
real, pointer :: gc_nd_cor_sol(:,:,:)  ! GLOMAP_CLIM COR (Sol) number
real, pointer :: gc_cor_sol_su(:,:,:)  ! GLOMAP_CLIM COR (Sol) SO4
real, pointer :: gc_cor_sol_bc(:,:,:)  ! GLOMAP_CLIM COR (Sol) BC
real, pointer :: gc_cor_sol_oc(:,:,:)  ! GLOMAP_CLIM COR (Sol) OC
real, pointer :: gc_cor_sol_ss(:,:,:)  ! GLOMAP_CLIM COR (Sol) SS
real, pointer :: gc_nd_ait_ins(:,:,:)  ! GLOMAP_CLIM AIT (Ins) number
real, pointer :: gc_ait_ins_bc(:,:,:)  ! GLOMAP_CLIM AIT (Ins) BC
real, pointer :: gc_ait_ins_oc(:,:,:)  ! GLOMAP_CLIM AIT (Ins) OC
! 1.8: Multi-level user ancillary fields
real, pointer :: user_mult1(:)     ! multi-level user ancilla
real, pointer :: user_mult2(:)     ! multi-level user ancilla
real, pointer :: user_mult3(:)     ! multi-level user ancilla
real, pointer :: user_mult4(:)     ! multi-level user ancilla
real, pointer :: user_mult5(:)     ! multi-level user ancilla
real, pointer :: user_mult6(:)     ! multi-level user ancilla
real, pointer :: user_mult7(:)     ! multi-level user ancilla
real, pointer :: user_mult8(:)     ! multi-level user ancilla
real, pointer :: user_mult9(:)     ! multi-level user ancilla
real, pointer :: user_mult10(:)    ! multi-level user ancilla
real, pointer :: user_mult11(:)    ! multi-level user ancilla
real, pointer :: user_mult12(:)    ! multi-level user ancilla
real, pointer :: user_mult13(:)    ! multi-level user ancilla
real, pointer :: user_mult14(:)    ! multi-level user ancilla
real, pointer :: user_mult15(:)    ! multi-level user ancilla
real, pointer :: user_mult16(:)    ! multi-level user ancilla
real, pointer :: user_mult17(:)    ! multi-level user ancilla
real, pointer :: user_mult18(:)    ! multi-level user ancilla
real, pointer :: user_mult19(:)    ! multi-level user ancilla
real, pointer :: user_mult20(:)    ! multi-level user ancilla

! 1.9: Fields carried forward from previous version.
! Lateral Boundary Conditions
real, pointer :: orog_lbc(:)                         ! Orography LBC
real, pointer :: u_lbc(:,:)                          ! U LBC
real, pointer :: v_lbc(:,:)                          ! V LBC
real, pointer :: w_lbc(:,:)                          ! W LBC
real, pointer :: rho_lbc(:,:)                        ! RHO LBC
real, pointer :: theta_lbc(:,:)                      ! Theta LBC
real, pointer :: q_lbc(:,:)                          ! Q LBC
real, pointer :: qcl_lbc(:,:)                        ! QCL LBC
real, pointer :: qcf_lbc(:,:)                        ! QCF LBC
real, pointer :: qcf2_lbc(:,:)                       ! 2nd Ice LBC
real, pointer :: qrain_lbc(:,:)                      ! Rain LBC
real, pointer :: qgraup_lbc(:,:)                     ! Graupel LBC
real, pointer :: cf_bulk_lbc(:,:)                    ! CF_BULK LBC
real, pointer :: cf_liquid_lbc(:,:)                  ! CF_LIQUID_LBC
real, pointer :: cf_frozen_lbc(:,:)                  ! CF_FROZEN_LBC
real, pointer :: exner_lbc(:,:)                      ! Exner LBC
real, pointer :: u_adv_lbc(:,:)                      ! U_ADV LBC
real, pointer :: v_adv_lbc(:,:)                      ! V_ADV LBC
real, pointer :: w_adv_lbc(:,:)                      ! W_ADV LBC
real, pointer :: murk_lbc(:,:)                       ! Murk aerosol LBC
real, pointer :: dust_div1_lbc(:,:)      ! dust mmr, division 1 LBC
real, pointer :: dust_div2_lbc(:,:)      ! dust mmr, division 2 LBC
real, pointer :: dust_div3_lbc(:,:)      ! dust mmr, division 3 LBC
real, pointer :: dust_div4_lbc(:,:)      ! dust mmr, division 4 LBC
real, pointer :: dust_div5_lbc(:,:)      ! dust mmr, division 5 LBC
real, pointer :: dust_div6_lbc(:,:)      ! dust mmr, division 6 LBC
real, pointer :: so2_lbc(:,:)            ! sulphur dioxide gas LBC
real, pointer :: dms_lbc(:,:)            ! dimethyl sulphide gas LBC
real, pointer :: so4_aitken_lbc(:,:)     ! Aitken mode sulphate aerosol LBC
real, pointer :: so4_accu_lbc(:,:)       ! accumulation mode sulphate LBC
real, pointer :: so4_diss_lbc(:,:)       ! dissolved sulphate aero LBC
real, pointer :: nh3_lbc(:,:)            ! ammonia gas LBC
real, pointer :: soot_new_lbc(:,:)       ! fresh soot LBC
real, pointer :: soot_agd_lbc(:,:)       ! aged soot LBC
real, pointer :: soot_cld_lbc(:,:)       ! soot in cloud LBC
real, pointer :: bmass_new_lbc(:,:)      ! fresh biomass LBC
real, pointer :: bmass_agd_lbc(:,:)      ! aged biomass LBC
real, pointer :: bmass_cld_lbc(:,:)      ! cloud biomass LBC
real, pointer :: ocff_new_lbc(:,:)       ! fresh fossil fuel LBC
real, pointer :: ocff_agd_lbc(:,:)       ! aged fossil fuel LBC
real, pointer :: ocff_cld_lbc(:,:)       ! cloud fossil fuel LBC
real, pointer :: nitr_acc_lbc(:,:)       ! accumulation ammonium nitrate LBC
real, pointer :: nitr_diss_lbc(:,:)      ! dissolved ammonium nitrate LBC

! Lateral Boundary Condition tendencies
real, pointer :: u_lbc_tend(:,:)                     ! U LBC  tendencies
real, pointer :: v_lbc_tend(:,:)                     ! V LBC tendencies
real, pointer :: w_lbc_tend(:,:)                     ! W LBC tendencies
real, pointer :: rho_lbc_tend(:,:)                   ! RHO LBC tendencies
real, pointer :: theta_lbc_tend(:,:)                 ! Theta LBC tendencies
real, pointer :: q_lbc_tend(:,:)                     ! Q LBC tendencies
real, pointer :: qcl_lbc_tend(:,:)                   ! QCL LBC tendencies
real, pointer :: qcf_lbc_tend(:,:)                   ! QCF LBC tendencies
real, pointer :: qcf2_lbc_tend(:,:)                  ! 2nd Ice
real, pointer :: qrain_lbc_tend(:,:)                 ! Rain LBC tendencies
real, pointer :: qgraup_lbc_tend(:,:)                ! Graupel
real, pointer :: cf_bulk_lbc_tend(:,:)               ! CF_BULK LBC tend'cies
real, pointer :: cf_liquid_lbc_tend(:,:)             ! CF_LIQUID_LBC t'cies
real, pointer :: cf_frozen_lbc_tend(:,:)             ! CF_FROZEN_LBC t'cies
real, pointer :: exner_lbc_tend(:,:)                 ! Exner LBC tendencies
real, pointer :: u_adv_lbc_tend(:,:)                 ! U_ADV LBC tendencies
real, pointer :: v_adv_lbc_tend(:,:)                 ! V_ADV LBC tendencies
real, pointer :: w_adv_lbc_tend(:,:)                 ! W_ADV LBCtendencies
real, pointer :: murk_lbc_tend(:,:)                  ! Murk aerosol LBC tend
real, pointer :: dust_div1_lbc_tend(:,:)      ! dust mmr, division 1 LBC tend
real, pointer :: dust_div2_lbc_tend(:,:)      ! dust mmr, division 2 LBC tend
real, pointer :: dust_div3_lbc_tend(:,:)      ! dust mmr, division 3 LBC tend
real, pointer :: dust_div4_lbc_tend(:,:)      ! dust mmr, division 4 LBC tend
real, pointer :: dust_div5_lbc_tend(:,:)      ! dust mmr, division 5 LBC tend
real, pointer :: dust_div6_lbc_tend(:,:)      ! dust mmr, division 6 LBC tend
real, pointer :: so2_lbc_tend(:,:)            ! sulphur dioxide gas LBC tend
real, pointer :: dms_lbc_tend(:,:)            ! dimethyl sulphide gas LBC tend
real, pointer :: so4_aitken_lbc_tend(:,:)     ! Aitken mode sulphate aerosol LBC
real, pointer :: so4_accu_lbc_tend(:,:)       ! accumulation mode sulphate LBC
real, pointer :: so4_diss_lbc_tend(:,:)       ! dissolved sulphate aero LBC
real, pointer :: nh3_lbc_tend(:,:)            ! ammonia gas LBC tend
real, pointer :: soot_new_lbc_tend(:,:)       ! fresh soot LBC tend
real, pointer :: soot_agd_lbc_tend(:,:)       ! aged soot LBC tend
real, pointer :: soot_cld_lbc_tend(:,:)       ! soot in cloud LBC tend
real, pointer :: bmass_new_lbc_tend(:,:)      ! fresh biomass LBC tend
real, pointer :: bmass_agd_lbc_tend(:,:)      ! aged biomass LBC tend
real, pointer :: bmass_cld_lbc_tend(:,:)      ! cloud biomass LBC tend
real, pointer :: ocff_new_lbc_tend(:,:)       ! fresh fossil fuel LBC tend
real, pointer :: ocff_agd_lbc_tend(:,:)       ! aged fossil fuel LBC tend
real, pointer :: ocff_cld_lbc_tend(:,:)       ! cloud fossil fuel LBC tend
real, pointer :: nitr_acc_lbc_tend(:,:)       ! accm'n ammonium nitrate LBC tend
real, pointer :: nitr_diss_lbc_tend(:,:)      ! dissolved amm nitrate LBC tend

! 2: Scalar Variables

! 2.1: Data variables stored in primary space.
real, pointer :: tstar(:,:)            ! Surface temperature
real, pointer :: tstar_anom(:)         ! Surface temperature anomaly
!   2.15: Fields for coastal tiling
real, pointer :: frac_land(:)          ! Land fraction in grid box
real, pointer :: tstar_land(:,:)       ! Land surface temperature
real, pointer :: tstar_sea(:,:)        ! Sea surface temperature
real, pointer :: tstar_sice(:,:,:)     ! Sea-ice surface temperature
real, pointer :: tstar_sice_cat(:,:,:) ! Sea-ice category surface temp

!   Set pointer for sea surface freezing temperature
real, pointer :: sstfrz(:,:)

! Set pointers for sea-ice and land albedos
real, pointer :: sice_alb(:,:)         ! Sea-ice albedo
real, pointer :: land_alb(:,:)         ! Mean land albedo

! 2.2: Data variables stored in secondary space.

real, pointer :: pstar(:,:)          ! Surface pressure

! 2.3: Cloud fields

real, pointer :: cclwp(:,:)          ! Convective cloud liquid water path
real, pointer :: deep_flag(:,:)      ! Deep convection flag
real, pointer :: past_precip(:,:)    ! Past convective precipitation
real, pointer :: past_conv_ht(:,:)   ! Past convective height

! 2.4: Boundary layer fields

real, pointer :: zh(:,:)             ! Boundary layer depth

real, pointer :: ddmfx(:,:)          ! Convective dowdraught
                                     ! mass-flux at cloud base

! Standard deviation of turbulent fluctuations of layer 1 temperature
real, pointer :: t1_sd(:,:)

! Standard deviation of turbulent fluctuations of layer 1 humidity
real, pointer :: q1_sd(:,:)

! Decoupled screen-level temperature
real, pointer :: TScrnDcl_SSI(:,:)
real, pointer :: TScrnDcl_TILE(:,:)
real, pointer :: tStbTrans(:,:)

! Convective cold pools
real, pointer :: ux_ccp  (:,:)
real, pointer :: uy_ccp  (:,:)
real, pointer :: um_ccp  (:,:)
real, pointer :: g_ccp   (:,:)
real, pointer :: h_ccp   (:,:)
real, pointer :: riso_ccp(:,:)
real, pointer :: rdir_ccp(:,:)

! 2.4: Soil Ancillary fields (all land_field, single level)

real, pointer :: sat_soilw_suction(:) ! Saturated soil water suction
real, pointer :: therm_cap    (:)     ! Thermal capacity
real, pointer :: therm_cond   (:)     ! Thermal conductivity
real, pointer :: vol_smc_crit (:)     ! Vol smc at critical point
real, pointer :: vol_smc_wilt (:)     ! Vol smc at wilting point
real, pointer :: vol_smc_sat  (:)     ! Vol smc at saturation
real, pointer :: sat_soil_cond(:)     ! Saturated soil conductivity
real, pointer :: clapp_horn   (:)     ! Clapp-Hornberger B coefficient
real, pointer :: z0m_soil     (:)     ! Bare soil momentum roughness length

! 2.5: Other surface fields
real, pointer :: canopy_water(:) ! Canopy Water                    (l_f, 1 lev)
real, pointer :: gs(:)           ! Gridbox mean canopy conductance (l_f, 1 lev)
real, pointer :: z0(:,:)         ! Roughness length, used for sea points

! 2.6: Orographic Ancillary fields

real, pointer :: orography(:,:)     ! Orographic height
real, pointer :: orog_sd(:)       ! Standard Deviation of orography
real, pointer :: orog_sil(:)      ! Silhouette area of orography
real, pointer :: orog_ho2(:)      ! Peak to trough height/(2*sqrt2)
real, pointer :: orog_grad_x(:)
real, pointer :: orog_grad_y(:)
real, pointer :: orog_unfilt(:,:)
real, pointer :: orog_grad_xx(:)  ! Orographic gradient xx
real, pointer :: orog_grad_xy(:)  ! Orographic gradient xy
real, pointer :: orog_grad_yy(:)  ! Orographic gradient yy
real, pointer :: orog_f1(:)  !
real, pointer :: orog_f2(:)  !
real, pointer :: orog_f3(:)  !
real, pointer :: orog_amp(:)  !

! 2.7: Sea/Sea Ice

real, pointer :: u_sea(:,:)           ! Surface current (u component)
real, pointer :: v_sea(:,:)           ! Surface current (v component)
real, pointer :: u_0_p(:,:)           ! Surace  current (u) on p-grid
real, pointer :: v_0_p(:,:)           ! Surface current (v) on p-grid
real, pointer :: ice_fract_cat(:,:,:) ! Sea ice fraction on categories
real, pointer :: ice_thick_cat(:,:,:) ! Sea ice thickness on categories
real, pointer :: ti_cat(:,:,:)        ! Sea ice temperature on categories
real, pointer :: ice_k_cat(:,:,:)     ! Sea ice effect cond on categories
real, pointer :: chloro_sea(:,:)      ! Sea near surface sea chlorophyll
real, pointer :: ice_fraction(:,:,:)  ! Sea ice fraction
real, pointer :: ice_thickness(:,:,:) ! Sea ice depth
real, pointer :: ti(:,:,:)            ! Sea ice temperature
real, pointer :: pond_depth_cat(:,:,:)  ! Meltpond depth on categories
real, pointer :: pond_frac_cat(:,:,:)   ! Meltpond fraction on categories
real, pointer :: z0m_sice_fmd(:,:)    ! Sea ice form drag roughness length
real, pointer :: z0m_sice_skin(:,:)   ! Sea ice skin roughness length

! 2.8: Snow

real, pointer :: snodep(:,:)      ! Snow depth on land
real, pointer :: snodep_sea(:,:,:)! Snow depth on sea ice (theta_pts_sea_only)
real, pointer :: snodep_sea_cat(:,:,:) ! Snow depth on sea ice catagories
real, pointer :: catch_snow(:,:)       ! Coniferous canopy snow capacity
real, pointer :: snow_grnd(:,:)   ! Snow below canopy
real, pointer :: snsoot(:,:)      ! Snow soot content

! 2.9: aerosol emission fields, including mineral dust parent soil props

real, pointer :: soil_clay(:,:)                    ! soil clay fraction
real, pointer :: soil_silt(:,:)                    ! soil silt fraction
real, pointer :: soil_sand(:,:)                    ! soil sand fraction
real, pointer :: dust_mrel1(:,:)                   ! soil rel mass, div 1
real, pointer :: dust_mrel2(:,:)                   ! soil rel mass, div 2
real, pointer :: dust_mrel3(:,:)                   ! soil rel mass, div 3
real, pointer :: dust_mrel4(:,:)                   ! soil rel mass, div 4
real, pointer :: dust_mrel5(:,:)                   ! soil rel mass, div 5
real, pointer :: dust_mrel6(:,:)                   ! soil rel mass, div 6
real, pointer :: dust_psti(:,:)                    ! dust pref source term input

real, pointer :: so2_em(:,:)        ! sulphur dioxide emission
real, pointer :: dms_em(:,:)        ! dimethyl sulphide emission
real, pointer :: so2_hilem(:,:)     ! high level SO2 emissions
real, pointer :: nh3_em(:,:)        ! ammonia gas surface emiss
real, pointer :: soot_em(:,:)       ! fresh soot surface emissions
real, pointer :: soot_hilem(:,:)    ! fresh soot high lev emissions
real, pointer :: bmass_em(:,:)      ! fresh bmass surface emissions
real, pointer :: bmass_hilem(:,:)   ! fresh bmass high lev emissions
real, pointer :: ocff_em(:,:)       ! fresh OCFF surface emissions
real, pointer :: ocff_hilem(:,:)    ! fresh OCFF high lev emissions
real, pointer :: dms_conc(:,:)      ! seawater dimethyl sulphide conc.
! Minimum and maximum heights for injection of fresh bmass high lev emissions
real, pointer :: bmass_hilem_h1 (:,:)
real, pointer :: bmass_hilem_h2 (:,:)

! 2.10: User ancillary fields
real, pointer :: user_anc1(:)         ! user ancillary field 1
real, pointer :: user_anc2(:)         ! user ancillary field 2
real, pointer :: user_anc3(:)         ! user ancillary field 3
real, pointer :: user_anc4(:)         ! user ancillary field 4
real, pointer :: user_anc5(:)         ! user ancillary field 5
real, pointer :: user_anc6(:)         ! user ancillary field 6
real, pointer :: user_anc7(:)         ! user ancillary field 7
real, pointer :: user_anc8(:)         ! user ancillary field 8
real, pointer :: user_anc9(:)         ! user ancillary field 9
real, pointer :: user_anc10(:)        ! user ancillary field 10
real, pointer :: user_anc11(:)        ! user ancillary field 11
real, pointer :: user_anc12(:)        ! user ancillary field 12
real, pointer :: user_anc13(:)        ! user ancillary field 13
real, pointer :: user_anc14(:)        ! user ancillary field 14
real, pointer :: user_anc15(:)        ! user ancillary field 15
real, pointer :: user_anc16(:)        ! user ancillary field 16
real, pointer :: user_anc17(:)        ! user ancillary field 17
real, pointer :: user_anc18(:)        ! user ancillary field 18
real, pointer :: user_anc19(:)        ! user ancillary field 19
real, pointer :: user_anc20(:)        ! user ancillary field 20

!   2.11: Store arrays for energy correction calculation
real, pointer :: net_flux(:,:)                   ! Net energy flux
real, pointer :: net_mflux(:,:)                  ! Net moisture flux

!   2.12: Tiled Vegetation and Triffid fields
real, pointer :: frac_typ(:,:)      ! Fractions of surface type
real, pointer :: frac_con1(:)       ! Fractions of surface type
real, pointer :: frac_con2(:)       ! Fractions of surface type
real, pointer :: frac_con3(:)       ! Fractions of surface type
real, pointer :: frac_con4(:)       ! Fractions of surface type
real, pointer :: frac_con5(:)       ! Fractions of surface type
real, pointer :: frac_con6(:)       ! Fractions of surface type
real, pointer :: frac_con7(:)       ! Fractions of surface type
real, pointer :: frac_con8(:)       ! Fractions of surface type
real, pointer :: frac_con9(:)       ! Fractions of surface type

real, pointer :: lai_pft(:,:)       ! LAI of plant functional types
real, pointer :: canht_pft(:,:)     ! Canopy hght of plant func types
real, pointer :: disturb_veg(:)     ! Disturbed fraction of vegetation (1-lev)
real, pointer :: disturb_veg_prev(:) ! Previous Disturbed fraction of vegetation
real, pointer :: pasture_frac_d1(:)      ! Pasture fraction of vegetation
real, pointer :: pasture_frac_prev_d1(:) ! Previous pasture fraction
real, pointer :: agr_crop_frac_d1(:)     ! Crop fraction of vegetation
real, pointer :: agr_crop_frac_prev_d1(:)! Previous crop fraction
real, pointer :: wood_prod_fast_d1(:)  ! Wood product pool (fast)
real, pointer :: wood_prod_med_d1(:)   ! Wood product pool (medium)
real, pointer :: wood_prod_slow_d1(:)  ! Wood product pool (slow)

real, pointer :: soil_alb(:)        ! Snow-free albedo of bare soil    (1-lev)

real, pointer :: obs_alb_sw(:)      ! Observed snow-free SW albedo
real, pointer :: obs_alb_vis(:)     ! Observed snow-free VIS albedo
real, pointer :: obs_alb_nir(:)     ! Observed snow-free NIR albedo

real, pointer :: soil_nitro1(:)     ! Soil organic nitrogen content DPM
real, pointer :: soil_nitro2(:)     ! Soil organic nitrogen content RPM
real, pointer :: soil_nitro3(:)     ! Soil organic nitrogen content BIO
real, pointer :: soil_nitro4(:)     ! Soil organic  nitrogen content HUM
real, pointer :: soil_nitrolyr_p1(:,:) ! Soil layered nitrogen content DPM
real, pointer :: soil_nitrolyr_p2(:,:) ! Soil layered nitrogen content RPM
real, pointer :: soil_nitrolyr_p3(:,:) ! Soil layered nitrogen content BIO
real, pointer :: soil_nitrolyr_p4(:,:) ! Soil layered nitrogen content HUM

real, pointer :: soil_inorgnit(:)   ! Soil inorganic nitrogen content
real, pointer :: soil_inorgnitlyr(:,:) ! Soil inorganic layered N content
real, pointer :: soil_inorgnitavailpft(:,:,:) ! Soil inorg layered N avail pft
real, pointer :: nitrogen_deposition_d1(:) ! Nitrogen deposition on land

real, pointer :: soil_carb(:,:)     ! Soil carbon content
real, pointer :: soil_carb1(:,:)    ! Soil carbon content DPM
real, pointer :: soil_carb2(:,:)      ! Soil carbon content RPM
real, pointer :: soil_carb3(:,:)      ! Soil carbon content BIO
real, pointer :: soil_carb4(:,:)      ! Soil carbon content HUM
real, pointer :: soil_carblyr_p1(:,:)  ! Soil organic layered carbon DPM
real, pointer :: soil_carblyr_p2(:,:)  ! Soil organic layered carbon RPM
real, pointer :: soil_carblyr_p3(:,:)  ! Soil organic layered carbon BIO
real, pointer :: soil_carblyr_p4(:,:)  ! Soil organic layered carbon HUM

real, pointer :: resp_s_acclyr_p1(:,:)  ! Accumulated respiration DPM
real, pointer :: resp_s_acclyr_p2(:,:)  ! Accumulated respiration RPM
real, pointer :: resp_s_acclyr_p3(:,:)  ! Accumulated respiration BIO
real, pointer :: resp_s_acclyr_p4(:,:)  ! Accumulated respiration HUM

real, pointer :: npp_pft_acc(:,:)     ! Accumulated NPP on PFTs
real, pointer :: g_lf_pft_acc(:,:)    ! Accum. leaf turnover rate PFTs
real, pointer :: g_phlf_pft_acc(:,:)  ! Accumulated phenological leaf
                                      !           turnover rate on PFTs
real, pointer :: rsp_w_pft_acc(:,:)   ! Accum. wood respiration on PFTs
real, pointer :: resp_s_acc_1pool(:,:)    ! Accumulated soil respiration
real, pointer :: resp_s_acc_4pool(:,:)   ! Acc. soil respiration 4 pool model

real, pointer :: can_water_tile(:,:)  ! Canopy water content on tiles
real, pointer :: catch_tile(:,:)      ! Canopy capacity on tiles
real, pointer :: infil_tile(:,:)      ! Max infiltration rate on tiles
real, pointer :: rgrain_tile(:,:)     ! Snow grain size on tiles
real, pointer :: snodep_tile(:,:)     ! Snow depth on tiles
real, pointer :: tstar_tile(:,:)      ! Surface temperature on tiles
real, pointer :: tsurf_elev_surft(:,:)! Temperature of elevated
                                      ! subsurface tiles (K)
real, pointer :: z0_tile(:,:)         ! Surface roughness on tiles
real, pointer :: z0h_tile(:,:)        ! Snow-free thermal roughness
                                      ! on tiles
real, pointer :: dolr_field(:,:)      ! TOA - surface upward LW at
                            ! radiation timestep
real, pointer :: lw_down(:,:)         ! Surface downward LW at
                            ! radiation timestep
! (changed from SW_TILE b/c of naming conflicts)
real, pointer :: sw_tile_rts(:,:)     ! Surface net SW on land tiles at
                            ! radiation timestep

! MORUSES - new urban two-tile scheme
real, pointer :: hgt(:)      ! Building height (m)
real, pointer :: hwr(:)      ! Height to width
real, pointer :: wrr(:)      ! Width ratio
real, pointer :: disp(:)     ! Displacement height (m)
real, pointer :: ztm(:)      ! Eff roughness length of momentum (m)
real, pointer :: albwl(:)    ! Wall albedo
real, pointer :: albrd(:)    ! Road albedo
real, pointer :: emisw(:)    ! Wall emmissivity
real, pointer :: emisr(:)    ! Road emmissivity

!   2.14: Carbon cycle fields
real, pointer :: triffid_co2_d1(:)
                 ! TRIFFID-derived CO2 fluxes for passing to the
                 ! atmosphere in emissions-driven runs (kgC/m2/yr):
                 ! exudates + wood product pool flux + harvest flux
real, pointer :: co2flux(:,:)   ! Ocean CO2 flux (Kg CO2/m2/s1)
real, pointer :: co2_emits(:,:) ! Surface CO2 emissions (Kg CO2/m2/s)

real, pointer :: soil_thickness(:)

! Definition of height(i,j,k) = zsea(k) + C(k)*zorog(i,j)
! All 1d (in the vertical)
real, pointer :: zseak_theta(:) ! zsea(k) on theta levels
real, pointer :: Ck_theta(:)    ! C(k)    on theta levels
real, pointer :: zseak_rho(:)   ! zsea(k) on rho levels
real, pointer :: Ck_rho(:)      ! C(k)    on rho levels

!   2.16: Fields for large-scale hydrology scheme. All 1d (land_field).

real, pointer :: ti_mean(:)          !Mean topographic index
real, pointer :: ti_sig(:)           !Standard dev. in topographic index
real, pointer :: fexp(:)             !Exponential decay in soil
!                                  ! saturated conductivity
real, pointer :: gamma_int(:)        !Integrated gamma function
real, pointer :: water_table(:)      !Water table depth
real, pointer :: fsfc_sat(:)         !Surface saturation fraction
real, pointer :: f_wetland(:)        !Wetland fraction

real, pointer :: sthzw(:)
real, pointer :: a_fsat(:)
real, pointer :: c_fsat(:)
real, pointer :: a_fwet(:)
real, pointer :: c_fwet(:)

!   2.17: Fields for River routing.
real, pointer :: riv_sequence(:)   ! River sequence
real, pointer :: riv_direction(:)  ! River direction
real, pointer :: riv_storage(:)    ! River water storage
real, pointer :: riv_number(:)     ! River outflow number
real, pointer :: tot_surfroff(:)   ! Accumulated surface runoff
real, pointer :: tot_subroff(:)    !     "       sub-surface runoff
real, pointer :: riv_inlandatm(:)       ! inland basin outflow

! Fields for water conservation correction due to lake evaporation:
real, pointer :: acc_lake_evap(:)  ! Acc lake evaporation

! Fields for RFM river routing (river routing 2A)
real, pointer :: riv_iarea(:)      ! Drainage area
real, pointer :: riv_slope(:)      ! Grid-cell slope
real, pointer :: riv_flowobs1(:)   ! Initial values of flow
real, pointer :: riv_inext(:)      ! Flow direction (x)
real, pointer :: riv_jnext(:)      ! Flow direction (y)
real, pointer :: riv_land(:)       ! Land-type (land/river/sea)
real, pointer :: riv_substore(:)   ! Subsurface storage
real, pointer :: riv_surfstore(:)  ! Surface storage
real, pointer :: riv_flowin(:)     ! Surface inflow
real, pointer :: riv_bflowin(:)    ! Subsurface inflow

! Fields used when coupling using OASIS.
real, pointer :: c_solar(:,:)       ! CPL solar radn
real, pointer :: c_blue(:,:)        ! CPL blue radn
real, pointer :: c_longwave(:,:)    ! CPL lw radn
real, pointer :: c_taux(:,:)        ! CPL taux
real, pointer :: c_tauy(:,:)        ! CPL tauy
real, pointer :: c_w10(:,:)         ! CPL 10m wind
real, pointer :: c_sensible(:,:)    ! CPL sensible ht flx
real, pointer :: c_sublim(:,:,:)    ! CPL Sublimation rate
real, pointer :: c_evap(:,:)        ! CPL Evaporation
real, pointer :: c_fcondtopn(:,:,:) ! CPL Fcondtop
real, pointer :: c_topmeltn(:,:,:)  ! CPL Topmelt
real, pointer :: c_penabs_radn(:,:,:)  ! CPL penetrative-absorbed radiation
real, pointer :: c_tstar_sicen(:,:,:)  ! CPL Sea ice surface temp
real, pointer :: c_lsrain(:,:)
real, pointer :: c_lssnow(:,:)
real, pointer :: c_cvrain(:,:)
real, pointer :: c_cvsnow(:,:)
real, pointer :: c_riverout(:,:)
real, pointer :: c_calving(:,:)
real, pointer :: c_mslp(:,:)
real, pointer :: c_surf_CO2(:,:)    ! CPL pp of CO2
real, pointer :: c_dust_dep(:,:)    ! CPL the total dust deposition
real, pointer :: c_riverout_trip(:)  ! River outflow on the TRIP grid
real, pointer :: c_sst_perts(:,:)

! Fields used when coupling to wave model using OASIS
real, pointer :: c_u10m_w(:,:)
real, pointer :: c_v10m_w(:,:)
real, pointer :: c_rho_air(:,:)
real, pointer :: charnock_w(:,:)

!   2.18: JULES variables
real, pointer :: blendht(:,:)        ! Blending height (m)
real, pointer :: snowdepth(:,:)      ! Snow depth on ground on tiles (m)
real, pointer :: rho_snow_grnd(:,:)  ! Snowpack bulk density (kg/m3)
real, pointer :: nsnow(:,:)          ! Number of snow layers on ground on tiles
real, pointer :: ds(:,:,:)           ! Snow layer thickness (m)
real, pointer :: sice(:,:,:)         ! Snow layer ice mass on tiles (Kg/m2)
real, pointer :: sliq(:,:,:)         ! Snow layer liquid mass on tiles (Kg/m2)
real, pointer :: tsnowlayer(:,:,:)   ! Snow layer temperature (K)
real, pointer :: rho_snow(:,:,:)     ! Snow layer densities (kg/m3)
real, pointer :: rgrainl(:,:,:)      ! Snow layer grain size on tiles (microns)
!  FLake lake scheme (all 1d, land_field)
real, pointer :: lake_depth(:)
real, pointer :: lake_fetch(:)
real, pointer :: lake_t_mean(:)
real, pointer :: lake_t_mxl(:)
real, pointer :: lake_t_ice(:)
real, pointer :: lake_h_mxl(:)
real, pointer :: lake_h_ice(:)
real, pointer :: lake_shape(:)
real, pointer :: lake_g_dt(:)
! Irrigation demand scheme
real, pointer :: sthu_irr(:,:) ! Unfrozen soil moisture in irrigated fraction
real, pointer :: frac_irr(:)   ! Irrigated fraction
! Land field filter for the hybrid resolution model
real, pointer :: filter_land_hyb(:)

! Aerosol climatologies
real, pointer :: arclbiog_bg(:,:,:) ! Biogenic aerosol climatology
real, pointer :: arclbiom_fr(:,:,:) ! Biomass burning (fresh) aerosol clim
real, pointer :: arclbiom_ag(:,:,:) ! Biomass burning (aged) aerosol clim
real, pointer :: arclbiom_ic(:,:,:) ! Biomass burning (in-cloud) aerosol clim
real, pointer :: arclblck_fr(:,:,:) ! Black carbon (fresh) aerosol clim
real, pointer :: arclblck_ag(:,:,:) ! Black carbon (aged) aerosol clim
real, pointer :: arclsslt_fi(:,:,:) ! Sea salt (film mode) aerosol clim
real, pointer :: arclsslt_jt(:,:,:) ! Sea salt (jet mode) aerosol clim
real, pointer :: arclsulp_ac(:,:,:) ! Sulphate (accumulation mode) aero clim
real, pointer :: arclsulp_ak(:,:,:) ! Sulphate (Aitken mode) aerosol clim
real, pointer :: arclsulp_di(:,:,:) ! Sulphate (dissolved) aerosol clim
real, pointer :: arcldust_b1(:,:,:) ! Dust (bin 1) aerosol climatology
real, pointer :: arcldust_b2(:,:,:) ! Dust (bin 2) aerosol climatology
real, pointer :: arcldust_b3(:,:,:) ! Dust (bin 3) aerosol climatology
real, pointer :: arcldust_b4(:,:,:) ! Dust (bin 4) aerosol climatology
real, pointer :: arcldust_b5(:,:,:) ! Dust (bin 5) aerosol climatology
real, pointer :: arcldust_b6(:,:,:) ! Dust (bin 6) aerosol climatology
real, pointer :: arclocff_fr(:,:,:) ! Org carbon fossil fuel (fresh) aero clim
real, pointer :: arclocff_ag(:,:,:) ! Org carbon fossil fuel (aged) aero clim
real, pointer :: arclocff_ic(:,:,:) ! Org carb fossil fuel (in-cloud) aero clim
real, pointer :: arcldlta_dl(:,:,:) ! Delta aerosol climatology

! Convective Cloud Fields
real, pointer :: lcbase(:,:)
real, pointer :: ccw_rad(:,:,:)
real, pointer :: cca_dp(:,:,:)
real, pointer :: cca_md(:,:,:)
real, pointer :: cca_sh(:,:,:)

! INFERNO Ignition Ancillaries
real, pointer :: flash_rate_ancil(:,:)
real, pointer :: pop_den_ancil(:,:)
real, pointer :: wealth_index_ancil(:,:)

! Temperature fields for thermal acclimation of photosynthesis.
real, pointer :: t_growth_gb_d1(:)
real, pointer :: t_home_gb_d1(:)

! Water tracer fields
type(wtrac_type), pointer :: wtrac(:)
type(sfc_wtrac_type), pointer :: wtrac_sfc(:)

!==============================================================================
! JULES TYPES
!JULES data comes as pairs of TYPES, a 'data' type and a 'pointer' type
!Instances of the pointer TYPES are always required to for passing fields
!to JULES.
!Instances of the data TYPES are not always required, depending on whether the
!required data is available elsewhere, eg D1.
!The declarations here should generally (but not exactly) match jules_fields_mod
!in the JULES repo
!These will be USEd into atm_step, and as required for initialisation

!TYPES to hold the data
type(crop_vars_data_type), target :: crop_vars_data
type(psparms_data_type), target :: psparms_data
type(top_pdm_data_type), target :: top_pdm_data
type(fire_vars_data_type), target :: fire_vars_data
type(ainfo_data_type), target :: ainfo_data
type(trif_vars_data_type), target :: trif_vars_data
type(soil_ecosse_vars_data_type), target :: soil_ecosse_vars_data
type(aero_data_type), target :: aero_data
type(urban_param_data_type), target :: urban_param_data
type(progs_data_type), target :: progs_data
type(trifctl_data_type), target :: trifctl_data
type(coastal_data_type), target :: coastal_data
type(jules_vars_data_type), target :: jules_vars_data
type(fluxes_data_type), target :: fluxes_data
type(lake_data_type), target :: lake_data
type(forcing_data_type), target :: forcing_data
type(rivers_data_type), target :: rivers_data
!type(in_dev), target :: veg3_parm_data
!type(in_dev), target :: veg3_field_data
type(chemvars_data_type), target :: chemvars_data
type(water_resources_data_type), target :: water_resources_data
type(jls_wtrac_data_type), target :: wtrac_jls_data

!TYPES passed to JULES. These may point at the data types above
!but this should be transparent
type(crop_vars_type) :: crop_vars
type(psparms_type) :: psparms
type(top_pdm_type) :: toppdm
type(fire_vars_type) :: fire_vars
type(ainfo_type) :: ainfo
type(trif_vars_type) :: trif_vars
type(soil_ecosse_vars_type) :: soilecosse
type(aero_type) :: aerotype
type(urban_param_type) :: urban_param
type(progs_type) :: progs
type(trifctl_type) :: trifctltype
type(coastal_type) :: coast
type(jules_vars_type) :: jules_vars
type(fluxes_type) :: fluxes
type(lake_type) :: lake_vars
type(forcing_type) :: forcing
type(rivers_type) :: rivers
!type(in_dev) :: veg3_parm
!type(in_dev) :: veg3_field
type(chemvars_type) :: chemvars
type(water_resources_type) :: water_resources
type(jls_wtrac_type) :: wtrac_jls

end module atm_fields_mod
