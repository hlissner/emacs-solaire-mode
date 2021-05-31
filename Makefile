##
# Project Title
#
# @file
# @version 0.1

all: autoloads compile test

autoloads:
	@emacs -batch \
        --eval '(setq generated-autoload-file (expand-file-name "solaire-mode-autoloads.el"))' \
		-f batch-update-autoloads .

compile:
	@emacs -batch -f batch-byte-compile solaire-mode.el

test:
	@emacs -batch -L . -l solaire-mode-test.el -f ert-run-tests-batch

clean:
	@rm -vf *.elc *-autoloads.el *~

# end
