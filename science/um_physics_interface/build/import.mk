##############################################################################
# (c) Crown copyright 2025 Met Office. All rights reserved.
# The file LICENCE, distributed with this code, contains details of the terms
# under which the code may be used.
##############################################################################
export PROJECT_SOURCE = $(APPS_ROOT_DIR)/science/um_physics_interface/source

.PHONY: import-um_physics_interface
import-um_physics_interface:
    # Extract and Psyclone the interface code
	$Q$(MAKE) $(QUIET_ARG) -f $(LFRIC_BUILD)/extract.mk \
			  SOURCE_DIR=$(PROJECT_SOURCE)
	$Q$(MAKE) $(QUIET_ARG) -f $(LFRIC_BUILD)/psyclone/psyclone.mk \
	          SOURCE_DIR=$(PROJECT_SOURCE) \
	          OPTIMISATION_PATH=$(OPTIMISATION_PATH)
    # Extract and Psyclone the physics schemes
	$Q$(MAKE) $(QUIET_ARG) -f $(LFRIC_BUILD)/extract.mk \
          SOURCE_DIR=$(APPS_ROOT_DIR)/science/physics_schemes/source
	$Q$(MAKE) $(QUIET_ARG) -f $(LFRIC_BUILD)/psyclone/psyclone.mk \
          SOURCE_DIR=$(APPS_ROOT_DIR)/science/physics_schemes/source
