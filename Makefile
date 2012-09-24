EMACS=emacs
# EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs
# EMACS=/Applications/Emacs23.app/Contents/MacOS/Emacs
# EMACS=/Applications/Aquamacs.app/Contents/MacOS/Aquamacs
# EMACS=/Applications/Macmacs.app/Contents/MacOS/Emacs
# EMACS=/usr/local/bin/emacs
# EMACS=/opt/local/bin/emacs
# EMACS=/usr/bin/emacs
EMACS_FLAGS=-Q --batch
TESTS=

CURL=curl
WORK_DIR=$(shell pwd)
AUTOLOADS_FILE=$(shell basename `pwd`)-loaddefs.el
TEST_DIR=ert-tests
TEST_DATADIR=pcache
TEST_DEP_1=ert
TEST_DEP_1_URL=http://bzr.savannah.gnu.org/lh/emacs/emacs-24/download/head:/ert.el-20110112160650-056hnl9qhpjvjicy-2/ert.el
TEST_DEP_2=pcache
TEST_DEP_2_URL=https://raw.github.com/sigma/pcache/fa8f863546e2e8f2fc0a70f5cc766a7f584e01b6/pcache.el
TEST_DEP_3=persistent-soft
TEST_DEP_3_URL=https://raw.github.com/rolandwalker/persistent-soft/374a63e3cf116f5d2902aa8b253b8c9de298f0a4/persistent-soft.el
TEST_DEP_4=ucs-utils
TEST_DEP_4_URL=https://raw.github.com/rolandwalker/ucs-utils/cf38ef555fc30d9aefaf3675ebd969948b71496a/ucs-utils.el
TEST_DEP_4a=ucs-utils-6.0-delta
TEST_DEP_4a_URL=https://raw.github.com/rolandwalker/ucs-utils/cf38ef555fc30d9aefaf3675ebd969948b71496a/ucs-utils-6.0-delta.el
TEST_DEP_5=font-utils
TEST_DEP_5_URL=https://raw.github.com/rolandwalker/font-utils/2740e21b3768bcd811a6009aa55a22b81cce9936/font-utils.el

build :
	$(EMACS) $(EMACS_FLAGS) --eval             \
	    "(progn                                \
	      (setq byte-compile-error-on-warn t)  \
	      (batch-byte-compile))" *.el

test-dep-1 :
	@cd $(TEST_DIR)                                      && \
	$(EMACS) $(EMACS_FLAGS)  -L . -L .. -l $(TEST_DEP_1) || \
	(echo "Can't load test dependency $(TEST_DEP_1).el, run 'make downloads' to fetch it" ; exit 1)

test-dep-2 :
	@cd $(TEST_DIR)                                   && \
	$(EMACS) $(EMACS_FLAGS)  -L . -L .. --eval           \
	    "(progn                                          \
	      (setq package-load-list '(($(TEST_DEP_2) t)))  \
	      (when (fboundp 'package-initialize)            \
	       (package-initialize))                         \
	      (require '$(TEST_DEP_2)))"                  || \
	(echo "Can't load test dependency $(TEST_DEP_2).el, run 'make downloads' to fetch it" ; exit 1)

test-dep-3 :
	@cd $(TEST_DIR)                                   && \
	$(EMACS) $(EMACS_FLAGS)  -L . -L .. --eval           \
	    "(progn                                          \
	      (setq package-load-list '(($(TEST_DEP_2) t)    \
	                                ($(TEST_DEP_3) t)))  \
	      (when (fboundp 'package-initialize)            \
	       (package-initialize))                         \
	      (require '$(TEST_DEP_3)))"                  || \
	(echo "Can't load test dependency $(TEST_DEP_3).el, run 'make downloads' to fetch it" ; exit 1)

test-dep-4 :
	@cd $(TEST_DIR)                                   && \
	$(EMACS) $(EMACS_FLAGS)  -L . -L .. --eval           \
	    "(progn                                          \
	      (setq package-load-list '(($(TEST_DEP_2) t)    \
	                                ($(TEST_DEP_3) t)    \
	                                ($(TEST_DEP_4) t)))  \
	      (when (fboundp 'package-initialize)            \
	       (package-initialize))                         \
	      (require '$(TEST_DEP_4)))"                  || \
	(echo "Can't load test dependency $(TEST_DEP_4).el, run 'make downloads' to fetch it" ; exit 1)

test-dep-5 :
	@cd $(TEST_DIR)                                   && \
	$(EMACS) $(EMACS_FLAGS)  -L . -L .. --eval           \
	    "(progn                                          \
	      (setq package-load-list '(($(TEST_DEP_2) t)    \
	                                ($(TEST_DEP_3) t)    \
	                                ($(TEST_DEP_5) t)))  \
	      (when (fboundp 'package-initialize)            \
	       (package-initialize))                         \
	      (require '$(TEST_DEP_5)))"                  || \
	(echo "Can't load test dependency $(TEST_DEP_5).el, run 'make downloads' to fetch it" ; exit 1)


downloads :
	$(CURL) '$(TEST_DEP_1_URL)'  > $(TEST_DIR)/$(TEST_DEP_1).el
	$(CURL) '$(TEST_DEP_2_URL)'  > $(TEST_DIR)/$(TEST_DEP_2).el
	$(CURL) '$(TEST_DEP_3_URL)'  > $(TEST_DIR)/$(TEST_DEP_3).el
	$(CURL) '$(TEST_DEP_4_URL)'  > $(TEST_DIR)/$(TEST_DEP_4).el
	$(CURL) '$(TEST_DEP_4a_URL)' > $(TEST_DIR)/$(TEST_DEP_4a).el
	$(CURL) '$(TEST_DEP_5_URL)'  > $(TEST_DIR)/$(TEST_DEP_5).el

autoloads :
	$(EMACS) $(EMACS_FLAGS) --eval                       \
	    "(progn                                          \
	      (setq generated-autoload-file \"$(WORK_DIR)/$(AUTOLOADS_FILE)\") \
	      (update-directory-autoloads \"$(WORK_DIR)\"))"

test-autoloads : autoloads
	@$(EMACS) $(EMACS_FLAGS) -l "./$(AUTOLOADS_FILE)" || echo "failed to load autoloads: $(AUTOLOADS_FILE)"

test : build test-dep-1 test-dep-2 test-dep-3 test-dep-4 test-dep-5 test-autoloads
	@cd $(TEST_DIR)                                   && \
	(for test_lib in *-test.el; do                       \
	    $(EMACS) $(EMACS_FLAGS) -L . -L .. -l cl -l $(TEST_DEP_1) -l $$test_lib --eval \
	    "(flet ((ert--print-backtrace (&rest args)       \
	      (insert \"no backtrace in batch mode\")))      \
	       (ert-run-tests-batch-and-exit '(and \"$(TESTS)\" (not (tag :interactive)))))" || exit 1; \
	done)

clean :
	@rm -f $(AUTOLOADS_FILE) *.elc *~ */*.elc */*~ $(TEST_DIR)/$(TEST_DEP_1).el $(TEST_DIR)/$(TEST_DEP_2).el \
	    $(TEST_DIR)/$(TEST_DEP_3).el $(TEST_DIR)/$(TEST_DEP_4).el $(TEST_DIR)/$(TEST_DEP_4a).el              \
	    $(TEST_DIR)/$(TEST_DEP_5).el
	@rm -rf '$(TEST_DIR)/$(TEST_DATADIR)'
