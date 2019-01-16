SIM_DIASCA_TOP = ../..


.PHONY: all full-doc-local clean-local


OVERALL_DOCUMENT_SOURCE = $(MOD_GUIDE)


# Default do-nothing target:
all:
	@echo "  Run 'make full-doc' to generate the "\
	"modeller guide ($(MOD_GUIDE))."


full-doc: full-doc-local


# We need to rely on images generated in other layers (ex: mock-simulators):
#
# (soda-ordering-interacting-FSM.png is a file generated in mock-simulators, yet
# we need it; we do not put the symlink in GIT as it would be dead as long as
# the PNG is not generated, and this would block the release preparation which
# attempts to follow symlinks; hence we recreate it iff needed)
#
full-doc-local:
	-@ln -sf $(SIM_DIASCA_TOP)/../mock-simulators/soda-test/doc/soda-ordering-interacting-FSM.png
	@cd $(SIM_DIASCA_TOP)/.. && $(MAKE) -s doc



clean: clean-local

# Just a link:
clean-local:
	-@/bin/rm -f soda-ordering-interacting-FSM.png


# Root specified as we want to catch the doc in src/core as well:
#
DOCUTILS_TOP = $(SIM_DIASCA_TOP)


include $(SIM_DIASCA_TOP)/GNUmakesettings.inc
