SIM_DIASCA_TOP = ..

DOC_ELEMENTS := technical-manual developer-guide modeller-guide coupling-howto \
				dataflow-howto


.PHONY: all release-doc doc-release doc-package $(DOC_ELEMENTS) info-doc \
		clean clean-doc


MODULES_DIRS = $(DOC_ELEMENTS) training

DOC_PACKAGE_PREFIX  = Sim-Diasca-$(SIM_DIASCA_VERSION)-doc


DOC_PACKAGE_ARCHIVE_ZIP = $(DOC_PACKAGE_PREFIX).zip
DOC_PACKAGE_ARCHIVE_BZ2 = $(DOC_PACKAGE_PREFIX).tar.bz2
DOC_PACKAGE_ARCHIVE_XZ  = $(DOC_PACKAGE_PREFIX).tar.xz


# Default do-nothing target:
all:
	@echo "  Run 'make doc-package' if wanting to generate a documentation \
	package (or 'make full-doc' to generate only the technical manual)."


doc:


# Alternate named targets:
release-doc: doc-package

doc-release: doc-package


# XZ target not activated, not usual enough:
doc-package: $(DOC_ELEMENTS)
	-@/bin/rm -rf $(DOC_PACKAGE_PREFIX)
	@/bin/mkdir $(DOC_PACKAGE_PREFIX)
	@/bin/cp $(TECH_MANUAL_DIR)/tmp-rst/$(TECH_MANUAL) $(DOC_PACKAGE_PREFIX)
	@/bin/cp $(DEV_GUIDE_DIR)/tmp-rst/$(DEV_GUIDE) $(DOC_PACKAGE_PREFIX)
	@/bin/cp $(MOD_GUIDE_DIR)/tmp-rst/$(MOD_GUIDE) $(DOC_PACKAGE_PREFIX)
	@/bin/cp $(COUPLING_HOWTO_DIR)/tmp-rst/$(COUPLING_HOWTO) \
	$(DOC_PACKAGE_PREFIX)
	@/bin/cp $(DATAFLOW_HOWTO_DIR)/tmp-rst/$(DATAFLOW_HOWTO) $(DOC_PACKAGE_PREFIX)
	@tar cvjf $(DOC_PACKAGE_ARCHIVE_BZ2) $(DOC_PACKAGE_PREFIX)
	@zip -r $(DOC_PACKAGE_ARCHIVE_ZIP) $(DOC_PACKAGE_PREFIX)
	@echo "   $(DOC_PACKAGE_ARCHIVE_BZ2) and $(DOC_PACKAGE_ARCHIVE_ZIP) "\
	"generated in sim-diasca/doc"
	-@/bin/rm -rf $(DOC_PACKAGE_PREFIX)


technical-manual:
	@( cd $(TECH_MANUAL_DIR) && $(MAKE) -s full-doc )


developer-guide:
	@( cd $(DEV_GUIDE_DIR) && $(MAKE) -s full-doc )


modeller-guide:
	@( cd $(MOD_GUIDE_DIR) && $(MAKE) -s full-doc )


coupling-howto:
	@( cd $(COUPLING_HOWTO_DIR) && $(MAKE) -s full-doc )


dataflow-howto:
	@( cd $(DATAFLOW_HOWTO_DIR) && $(MAKE) -s full-doc )


info-doc:
	@echo "  Use 'make full-doc' to generate the Sim-Diasca Technical Manual"\
	" (in english)."
	@echo "DOC_PACKAGE_PREFIX      = $(DOC_PACKAGE_PREFIX)"
	@echo "DOC_PACKAGE_ARCHIVE_ZIP = $(DOC_PACKAGE_ARCHIVE_ZIP)"
	@echo "DOC_PACKAGE_ARCHIVE_BZ2 = $(DOC_PACKAGE_ARCHIVE_BZ2)"
	@echo "DOC_PACKAGE_ARCHIVE_XZ  = $(DOC_PACKAGE_ARCHIVE_XZ)"


clean: clean-doc


# Not using DOC_PACKAGE_* as any version must be removed:
clean-doc:
	-@/bin/rm -f Sim-Diasca-*-doc.zip Sim-Diasca-*-doc.tar.bz2 \
		Sim-Diasca-*-doc.tar.xz
	-@/bin/rm -rf Sim-Diasca-*-doc


# Root specified as we want to catch the doc in src/core as well:
DOCUTILS_TOP = $(SIM_DIASCA_TOP)


include $(SIM_DIASCA_TOP)/GNUmakesettings.inc
