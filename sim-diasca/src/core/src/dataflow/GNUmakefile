SIM_DIASCA_TOP = ../../../..

.PHONY: stats

MODULES_DIRS = bindings mockup-support tests

include $(SIM_DIASCA_TOP)/GNUmakesettings.inc


class_DataflowBlock.beam: class_DataflowBlock.erl           \
						  class_DataflowBlock_defines.hrl   \
						  class_DataflowBlock_functions.hrl \


stats:
	@$(MAKE_CODE_STATS) $(PWD)
