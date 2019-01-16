# Prerequisite: SIM_DIASCA_TOP must be set.

# Add all Sim-Diasca-specific rules below.


.PHONY: .PHONY-local


.PHONY-local: default-sim-diasca-rule make-config-links                \
			  make-config-links-recurse clean clean-probe clean-test   \
			  clean-test-logs clean-test-images clean-source-archives  \
			  clean-results clean-outputs


# Now, to accommodate the SII, the user node is launched in non-distributed mode
# (i.e. no more '--ln $@' in the rules):

%_run: %_sim.beam %.beam %_run_dependencies
	@echo "     Running simulation $@ (two prerequisites) with $(ERL_PARAMETERIZED_LAUNCHER)"
	@$(ERL_PARAMETERIZED_LAUNCHER) --eval $$(echo $@ | sed 's|_run$$|_sim:run()|1') $(CMD_LINE_OPT) $(INTERNAL_OPTIONS)


%_run: %_sim.beam %_run_dependencies
	@echo "     Running simulation $@ (one prerequisite) with $(ERL_PARAMETERIZED_LAUNCHER)"
	@$(ERL_PARAMETERIZED_LAUNCHER) --eval $$(echo $@ | sed 's|_run$$|_sim:run()|1') $(CMD_LINE_OPT) $(INTERNAL_OPTIONS)


%_run: %_sim.beam %.beam
	@echo "     Running simulation $@ (two prerequisites) with $(ERL_PARAMETERIZED_LAUNCHER)"
	@$(ERL_PARAMETERIZED_LAUNCHER) --eval $$(echo $@ | sed 's|_run$$|_sim:run()|1') $(CMD_LINE_OPT) $(INTERNAL_OPTIONS)



%_case: %.beam %_case.beam
		@# Even just a comment is needed here, to force rebuild.


%_case: %_case.beam
		@# Even just a comment is needed here, to force rebuild.



# To run executions labelled as (simulation) cases, rather than as mere tests:
%_run: %_case
	@echo "     Running simulation case $^"
	@$(ERL_PARAMETERIZED_LAUNCHER) --eval $$(echo $@ | sed 's|_run$$|_case:run()|1') $(CMD_LINE_OPT) $(INTERNAL_OPTIONS)



# The first non-generic, therefore default, rule:
default-sim-diasca-rule: all


# Ensures that, as soon a directory contains a test, there is an host
# configuration file, creating a symbolic link to the centralised version
# thereof.
#
# Run the clean-host-candidate-files target first, if wanting to rely on
# symbolic links pointing to the same centralised file.
#
make-config-links:
	@if [ ! -e "$(SIM_DIASCA_HOST_FILE)" ] ; then \
	if /bin/ls *_test.erl 1>/dev/null 2>&1 ; then \
	echo "  Adding link to centralised host file in $$(pwd)" ; \
	/bin/ln -sf $(SIM_DIASCA_TOP)/conf/$(SIM_DIASCA_HOST_FILE) ; fi ; fi


make-config-links-recurse:
	@for m in $(MODULES_DIRS) ; do \
	( cd $$m && $(MAKE) -s make-config-links make-config-links-recurse ) ; \
	done



clean: clean-probe clean-test

clean-probe:
	-@if [ $$(basename $$(pwd)) != "doc" ]; then /bin/rm -f *_Probe*.png *_probe*.png *_probe*.p *_probe*.dat *_Probe*.png *_Probe*.p *_Probe*.dat *-generated.png; fi


clean-test: clean-test-logs clean-test-images clean-source-archives


clean-test-logs:
	-@/bin/rm -f *_test.log


# *test*.png was removing images that were to be kept:
clean-test-images:
	-@if [ $$(basename $$(pwd)) != "doc" ]; then \
	/bin/rm -f *Test*.png; fi


# Code archive files may be created by tests:
clean-source-archives:
	-@/bin/rm -f *.sdar



# This target is quite slow to evaluate and a bit dangerous (actually it is not
# called by default, see the 'clean-all-results' target in the root
# GNUmakefile):
#
clean-results:
	@echo "    Cleaning all results for user $$USER in $$(pwd)"
	@for d in *-on-*-at-*-by-$$USER-* ; do /bin/rm -rf $$d ; done


clean-outputs: clean-results clean-traces
	@for m in $(MODULES_DIRS) ; do \
	( cd $$m && $(MAKE) -s clean-outputs ) ; \
	done
