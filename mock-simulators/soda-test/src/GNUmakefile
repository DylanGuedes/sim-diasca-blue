MOCK_SIMULATORS_TOP = ../..


.PHONY: all demo demo-batch batch run test-platform-integration


all:


demo:
	@$(MAKE) soda_stochastic_integration_run


demo-batch:
	@$(MAKE) soda_stochastic_integration_run CMD_LINE_OPT="--batch"


batch: demo-batch

run: demo


test-deterministic:
	@$(MAKE) soda_deterministic_integration_run CMD_LINE_OPT="--batch"
	@$(MAKE) soda_deterministic_integration_run


test-platform-integration:
	@$(MAKE) soda_platform_integration_run CMD_LINE_OPT="--batch -start-verbatim-options --simulation-instance-id 944"


include $(MOCK_SIMULATORS_TOP)/GNUmakesettings.inc
