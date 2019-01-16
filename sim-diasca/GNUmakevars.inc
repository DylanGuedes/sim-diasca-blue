# Prerequisite: SIM_DIASCA_TOP must be set.

# Project section.

# PROJECT_NAME should be defined on a per-project basis.
ifndef PROJECT_NAME
	   PROJECT_NAME = Sim-Diasca
endif


#SIM_DIASCA_VERSION := 2.1.2-beta-$(date '+%Y%m%d-%Hh%M')
#SIM_DIASCA_VERSION := 2.2.2-for-RELEASE
#SIM_DIASCA_VERSION := 2.2.3-for-T1-and-T2
#SIM_DIASCA_VERSION := 2.2.3-for-esl-work-t3
SIM_DIASCA_VERSION := 2.2.11-rc4


# PROJECT_VERSION should be defined on a per-project basis.
ifndef PROJECT_VERSION
	   PROJECT_VERSION = $(SIM_DIASCA_VERSION)
endif

ifndef PROJECT_RELEASE_DATE
	   # To be set only at release time:
	   PROJECT_RELEASE_DATE = "(not set yet)"
	   #PROJECT_RELEASE_DATE = "Tuesday, April 7, 2015"
endif


ifndef PACKAGE_NAME
	   PACKAGE_NAME = sim-diasca
endif


ifndef PACKAGE_TOP
	   PACKAGE_TOP = $(SIM_DIASCA_TOP)
endif


ifndef INSTALLATION_PREFIX
	   INSTALLATION_PREFIX := $$HOME/Software/Sim-Diasca/Sim-Diasca-$(SIM_DIASCA_VERSION)
endif


# Better defined here, as depends on the Sim-Diasca version number:
SIM_DIASCA_RELEASE_BASENAME := Sim-Diasca-$(SIM_DIASCA_VERSION)
SIM_DIASCA_RELEASE_BASE = $(ROOT_TOP)/../$(SIM_DIASCA_RELEASE_BASENAME)

SIM_DIASCA_RELEASE_ARCHIVE_ZIP := $(SIM_DIASCA_RELEASE_BASENAME).zip
SIM_DIASCA_RELEASE_ARCHIVE_BZ2 := $(SIM_DIASCA_RELEASE_BASENAME).tar.bz2
SIM_DIASCA_RELEASE_ARCHIVE_XZ  := $(SIM_DIASCA_RELEASE_BASENAME).tar.xz

SIM_DIASCA_RELEASES := $(SIM_DIASCA_RELEASE_ARCHIVE_ZIP) \
					   $(SIM_DIASCA_RELEASE_ARCHIVE_BZ2) \
					   $(SIM_DIASCA_RELEASE_ARCHIVE_XZ)


ifndef VM_NAME

VM_NAME := sim_diasca_debug

endif


ifndef VM_TEST_NAME

VM_TEST_NAME := sim_diasca_test

endif


# Starting from this layer, nodes are not started as distributed ones, as, for
# the purpose of run uniqueness (see SII), Sim-Diasca has to change the name of
# the user node at runtime, hence this non-distributed flag:
#
ifndef NODE_NAMING
  NODE_NAMING="--nn"
endif


WORK_ARCHIVE_FILE  := \
  $(ARCHIVE_ROOT)/$(date +'%Y%m%d')-$(PROJECT_NAME)-work-archive.tar.xz



# Directory section.

VERSION_DIR = $(SIM_DIASCA_TOP)

FRAMEWORK_DIR = $(SIM_DIASCA_TOP)/src

CORE_BASE_DIR := $(FRAMEWORK_DIR)/core

CORE_SRC_DIR := $(CORE_BASE_DIR)/src


BINDINGS_BASE_DIR          := $(CORE_SRC_DIR)/bindings
CASE_SUPPORT_BASE_DIR      := $(CORE_SRC_DIR)/case-support
DATAFLOW_BASE_DIR          := $(CORE_SRC_DIR)/dataflow
DATA_MANAGEMENT_BASE_DIR   := $(CORE_SRC_DIR)/data-management
DEPLOYMENT_BASE_DIR        := $(CORE_SRC_DIR)/deployment
INSTANCE_CREATION_BASE_DIR := $(CORE_SRC_DIR)/instance-creation
PLUGINS_BASE_DIR           := $(CORE_SRC_DIR)/plugins
RANDOM_BASE_DIR            := $(CORE_SRC_DIR)/random
RESILIENCE_BASE_DIR        := $(CORE_SRC_DIR)/resilience
SCENARIO_BASE_DIR          := $(CORE_SRC_DIR)/scenario
SCHEDULING_BASE_DIR        := $(CORE_SRC_DIR)/scheduling

JAVA_BINDING_BASE_DIR   := $(BINDINGS_BASE_DIR)/java
PYTHON_BINDING_BASE_DIR := $(BINDINGS_BASE_DIR)/python

DATA_EXCHANGE_BASE_DIR := $(DATA_MANAGEMENT_BASE_DIR)/data-exchange
DATA_STORAGE_BASE_DIR  := $(DATA_MANAGEMENT_BASE_DIR)/data-storage
MONITORING_BASE_DIR    := $(DATA_MANAGEMENT_BASE_DIR)/monitoring
RESULT_BASE_DIR        := $(DATA_MANAGEMENT_BASE_DIR)/result-management

MOCKUP_BASE_DIR            := $(DATAFLOW_BASE_DIR)/mockup-support
DATAFLOW_BINDINGS_BASE_DIR := $(DATAFLOW_BASE_DIR)/bindings

DATAFLOW_JAVA_BINDING_BASE_DIR   := $(DATAFLOW_BINDINGS_BASE_DIR)/java
DATAFLOW_PYTHON_BINDING_BASE_DIR := $(DATAFLOW_BINDINGS_BASE_DIR)/python

MOCKUP_TEST_BASE_DIR := $(MOCKUP_BASE_DIR)/tests

PERFORMANCE_BASE_DIR := $(MONITORING_BASE_DIR)/performance
PROBE_BASE_DIR       := $(MONITORING_BASE_DIR)/probe


GENERIC_BASE_DIR := $(CORE_BASE_DIR)/generic
MESH_BASE_DIR    := $(CORE_BASE_DIR)/mesh
SPACE_BASE_DIR   := $(CORE_BASE_DIR)/space


MODELS_DIR = $(FRAMEWORK_DIR)/models

EQUIPMENT_BASE_DIR   := $(MODELS_DIR)/equipment
RELIABILITY_BASE_DIR := $(MODELS_DIR)/reliability

FAILURE_BASE_DIR     := $(RELIABILITY_BASE_DIR)/failure
REPARATION_BASE_DIR  := $(RELIABILITY_BASE_DIR)/reparation

SPATIAL_BASE_DIR := $(FRAMEWORK_DIR)/spatial-support



# Source section.

CORE_SRC := $(CORE_BASE_DIR)

DATA_EXCHANGER_SRC      := $(DATA_EXCHANGE_BASE_DIR)
DATA_LOGGER_SRC         := $(DATA_STORAGE_BASE_DIR)

PERFORMANCE_TRACKER_SRC := $(PERFORMANCE_BASE_DIR)
PROBE_SRC               := $(PROBE_BASE_DIR)
RESULT_SRC              := $(RESULT_BASE_DIR)

BINDINGS_SRC          := $(BINDINGS_BASE_DIR)
CASE_SUPPORT_SRC      := $(CASE_SUPPORT_BASE_DIR)
DATAFLOW_SRC          := $(DATAFLOW_BASE_DIR)
DEPLOYMENT_SRC        := $(DEPLOYMENT_BASE_DIR)
PLUGINS_SRC           := $(PLUGINS_BASE_DIR)
RANDOM_SRC            := $(RANDOM_BASE_DIR)
RESILIENCE_SRC        := $(RESILIENCE_BASE_DIR)
SCENARIO_SRC          := $(SCENARIO_BASE_DIR)
SCHEDULING_SRC        := $(SCHEDULING_BASE_DIR)
INSTANCE_CREATION_SRC := $(INSTANCE_CREATION_BASE_DIR)


JAVA_BINDING_SRC   := $(BINDINGS_BASE_DIR)/java/src
PYTHON_BINDING_SRC := $(BINDINGS_BASE_DIR)/python/src

DATAFLOW_BINDINGS_SRC := $(DATAFLOW_BINDINGS_BASE_DIR)

DATAFLOW_JAVA_BINDING_SRC   := $(DATAFLOW_JAVA_BINDING_BASE_DIR)/src
DATAFLOW_PYTHON_BINDING_SRC := $(DATAFLOW_PYTHON_BINDING_BASE_DIR)/src

MOCKUP_SRC      := $(MOCKUP_BASE_DIR)
MOCKUP_TEST_SRC := $(MOCKUP_TEST_BASE_DIR)


GENERIC_SRC := $(GENERIC_BASE_DIR)/src
MESH_SRC    := $(MESH_BASE_DIR)/src
SPACE_SRC   := $(SPACE_BASE_DIR)/src


EQUIPMENT_SRC   := $(EQUIPMENT_BASE_DIR)/src
RELIABILITY_SRC := $(RELIABILITY_BASE_DIR)/src


FAILURE_SRC     := $(FAILURE_BASE_DIR)/src
REPARATION_SRC  := $(REPARATION_BASE_DIR)/src


SPATIAL_SRC := $(SPATIAL_BASE_DIR)/src



# Include path section.


VERSION_INC             = -I$(VERSION_DIR)

CORE_INC                = -I$(CORE_SRC)

BINDINGS_INC            = -I$(BINDINGS_SRC)
CASE_SUPPORT_INC        = -I$(CASE_SUPPORT_SRC)
DATAFLOW_INC            = -I$(DATAFLOW_SRC)
DATA_EXCHANGE_INC       = -I$(DATA_EXCHANGER_SRC)
DATA_STORAGE_INC        = -I$(DATA_LOGGER_SRC)
DEPLOYMENT_INC          = -I$(DEPLOYMENT_SRC)
INSTANCE_CREATION_INC   = -I$(INSTANCE_CREATION_SRC)
PERFORMANCE_TRACKER_INC = -I$(PERFORMANCE_TRACKER_SRC)
PLUGINS_INC             = -I$(PLUGINS_SRC)
PROBE_INC               = -I$(PROBE_SRC)
RANDOM_INC              = -I$(RANDOM_SRC)
RESILIENCE_INC          = -I$(RESILIENCE_SRC)
RESULT_INC              = -I$(RESULT_SRC)
SCENARIO_INC            = -I$(SCENARIO_SRC)
SCHEDULING_INC          = -I$(SCHEDULING_SRC)

DATAFLOW_BINDINGS_INC = -I$(DATAFLOW_BINDINGS_SRC)

MOCKUP_INC            = -I$(MOCKUP_SRC)

GENERIC_INC = -I$(GENERIC_SRC)
MESH_INC    = -I$(MESH_SRC)
SPACE_INC   = -I$(SPACE_SRC)

EQUIPMENT_INC   = -I$(EQUIPMENT_SRC)
FAILURE_INC     = -I$(FAILURE_SRC)
REPARATION_INC  = -I$(REPARATION_SRC)

SPATIAL_INC = -I$(SPATIAL_SRC)


# $(SCENARIO_INC) not existing (yet):
INC += $(VERSION_INC) $(CORE_INC)                                            \
	$(BINDINGS_INC) $(CASE_SUPPORT_INC) $(DATAFLOW_INC)                      \
	$(DATA_EXCHANGE_INC) $(DATA_STORAGE_INC) $(DEPLOYMENT_INC)               \
	$(INSTANCE_CREATION_INC) $(PERFORMANCE_TRACKER_INC) $(PLUGINS_INC)       \
	$(PROBE_INC) $(RANDOM_INC) $(RESILIENCE_INC) $(RESULT_INC)               \
	$(SCENARIO_INC) $(SCHEDULING_INC)                                        \
	$(DATAFLOW_BINDINGS_INC) $(MOCKUP_INC)                                   \
	$(GENERIC_INC) $(MESH_INC) $(SPACE_INC)                                  \
	$(EQUIPMENT_INC) $(FAILURE_INC) $(REPARATION_INC)                        \
	$(SPATIAL_INC)



# BEAM path section.

CORE_BEAM := $(CORE_BASE_DIR)/src

BINDINGS_BEAM            := $(BINDINGS_BASE_DIR)
CASE_SUPPORT_BEAM        := $(CASE_SUPPORT_BASE_DIR)
DATAFLOW_BEAM            := $(DATAFLOW_BASE_DIR)
DATA_EXCHANGE_BEAM       := $(DATA_EXCHANGE_BASE_DIR)
DATA_STORAGE_BEAM        := $(DATA_STORAGE_BASE_DIR)
DEPLOYMENT_BEAM          := $(DEPLOYMENT_BASE_DIR)
INSTANCE_CREATION_BEAM   := $(INSTANCE_CREATION_BASE_DIR)
PERFORMANCE_TRACKER_BEAM := $(PERFORMANCE_BASE_DIR)
PROBE_BEAM               := $(PROBE_BASE_DIR)
PLUGINS_BEAM             := $(PLUGINS_BASE_DIR)
RANDOM_BEAM              := $(RANDOM_BASE_DIR)
RESILIENCE_BEAM          := $(RESILIENCE_BASE_DIR)
RESULT_BEAM              := $(RESULT_BASE_DIR)
SCENARIO_BEAM            := $(SCENARIO_BASE_DIR)
SCHEDULING_BEAM          := $(SCHEDULING_BASE_DIR)

JAVA_BINDING_BEAM   := $(JAVA_BINDING_SRC)
PYTHON_BINDING_BEAM := $(PYTHON_BINDING_SRC)

DATAFLOW_BINDINGS_BEAM := $(DATAFLOW_BINDINGS_BASE_DIR)

DATAFLOW_JAVA_BINDING_BEAM   := $(DATAFLOW_JAVA_BINDING_SRC)
DATAFLOW_PYTHON_BINDING_BEAM := $(DATAFLOW_PYTHON_BINDING_SRC)

MOCKUP_BEAM      := $(MOCKUP_BASE_DIR)
MOCKUP_TEST_BEAM := $(MOCKUP_TEST_BASE_DIR)


GENERIC_BEAM := $(GENERIC_BASE_DIR)/src
MESH_BEAM    := $(MESH_BASE_DIR)/src
SPACE_BEAM   := $(SPACE_BASE_DIR)/src

EQUIPMENT_BEAM  := $(EQUIPMENT_BASE_DIR)/src
FAILURE_BEAM    := $(FAILURE_BASE_DIR)/src
REPARATION_BEAM := $(REPARATION_BASE_DIR)/src


SPATIAL_BEAM := $(SPATIAL_BASE_DIR)/src


# Used to be 'BEAM_DIRS := ... $(BEAM_DIRS)', but it led to multiple duplicates
# (multiple inclusions?)
#
# ($(SCENARIO_BEAM) does not exist yet)
#
BEAM_DIRS += $(CORE_BEAM)                                                     \
	$(BINDINGS_BEAM) $(CASE_SUPPORT_BEAM) $(DATAFLOW_BEAM)                    \
	$(DATA_EXCHANGE_BEAM) $(DATA_STORAGE_BEAM) $(DEPLOYMENT_BEAM)             \
	$(INSTANCE_CREATION_BEAM) $(PERFORMANCE_TRACKER_BEAM) $(PLUGINS_BEAM)     \
	$(PROBE_BEAM) $(RANDOM_BEAM) $(RESILIENCE_BEAM) $(RESULT_BEAM)            \
	$(SCHEDULING_BEAM)                                                        \
	$(BINDINGS_BEAM) $(JAVA_BINDING_BEAM) $(PYTHON_BINDING_BEAM)              \
	$(DATAFLOW_BINDINGS_BEAM) $(DATAFLOW_JAVA_BINDING_BEAM)                   \
	$(DATAFLOW_PYTHON_BINDING_BEAM) $(MOCKUP_BEAM) $(MOCKUP_TEST_BEAM)        \
	$(GENERIC_BEAM) $(MESH_BEAM) $(SPACE_BEAM) $(EQUIPMENT_BEAM)              \
	$(FAILURE_BEAM) $(REPARATION_BEAM)                                        \
	$(SPATIAL_BEAM)


# To compile a module (ex: a plugin) using a behaviour (ex: sim_diasca_plugin),
# the BEAM of that behaviour must be found:
#
ERLANG_COMPILER_OPT += -pa $(PLUGINS_BEAM)


# Best option to avoid various warnings (same as for
# trace_disabled/{1,2,3,4,5}):
#
ERLANG_COMPILER_OPT += "+{nowarn_unused_function,[ {get_spatial_message,2} ]}"


# Specifying the root directory of Sim-Diasca allows to be able to find
# afterwards files (like the node cleaner script) regardless from where the
# interpreter is run.
INTERNAL_OPTIONS := --sim-diasca-root $(SIM_DIASCA_TOP)     \
				--sim-diasca-version $(SIM_DIASCA_VERSION)  \


# The script to convert spreadsheets into DUMF files:
SPREADSHEETS_TO_DUMF_SCRIPT := $(MOCKUP_BASE_DIR)/mockup_spreadsheet_to_dumf.escript


# Must be available from the user's path:
LOGMX := $$(which logmx.sh 2>/dev/null)

PROJECT_CSS = $(SIM_DIASCA_TOP)/tools/css/SimDiasca-docutils.css


# For later reuse in the next layer of the software stack:
SIM_DIASCA_PLT_FILE := $(SIM_DIASCA_TOP)/sim-diasca.plt


# The configuration file to list the potential distributed hosts:
SIM_DIASCA_HOST_FILE := "sim-diasca-host-candidates.txt"



# Documentation section.

TECH_MANUAL := SimDiasca-technical-manual-english.pdf
TECH_MANUAL_DIR := technical-manual

DEV_GUIDE := SimDiasca-developer-guide-english.pdf
DEV_GUIDE_DIR := developer-guide

MOD_GUIDE := SimDiasca-modeller-guide-english.pdf
MOD_GUIDE_DIR := modeller-guide

COUPLING_HOWTO := SimDiasca-coupling-howto-english.pdf
COUPLING_HOWTO_DIR := coupling-howto

DATAFLOW_HOWTO := SimDiasca-dataflow-howto-english.pdf
DATAFLOW_HOWTO_DIR := dataflow-howto



# This is the merged PLT of the level just below in the software stack.
# For Sim-Diasca, it is the PLT of 'Traces':
ifndef PREDECESSOR_PLT
   PREDECESSOR_PLT = $(TRACES_PLT_FILE)
endif


# TRACES_TOP is expected to be a symbolic link pointing to the actual Traces
# package to be used.
#
# This link should be created by the 'links' target of the root makefile, so
# that a layer is able to find the layer just above it without being itself
# modified.
#
TRACES_TOP = $(SIM_DIASCA_TOP)/../traces
