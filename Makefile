EMACS=emacs
# EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs
# EMACS=/Applications/Emacs23.app/Contents/MacOS/Emacs
# EMACS=/Applications/Aquamacs.app/Contents/MacOS/Aquamacs
# EMACS=/Applications/Macmacs.app/Contents/MacOS/Emacs
# EMACS=/usr/local/bin/emacs
# EMACS=/opt/local/bin/emacs
# EMACS=/usr/bin/emacs
EMACS_FLAGS=-Q --batch

WORK_DIR=$(shell pwd)

build :
	$(EMACS) $(EMACS_FLAGS) -f batch-byte-compile *.el

test :
	@echo no tests defined

autoloads :
	$(EMACS) $(EMACS_FLAGS) --eval '(let ((generated-autoload-file "$(WORK_DIR)/loaddefs.el")) (update-directory-autoloads "$(WORK_DIR)"))'

clean :
	@rm -f loaddefs.el *.elc *~ */*.elc */*~
