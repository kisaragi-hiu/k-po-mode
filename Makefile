.cask: Cask
	cask install

ELC := $(patsubst %.el,%.elc,$(wildcard *.el))

$(ELC): .cask $(wildcard *.el)
	cask build

compile: $(ELC)

test:
	cask emacs -batch -L . -f buttercup-run-discover

.PHONY: test compile
