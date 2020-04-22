MODULE = yaml-path.el
EMACS = ${HOME}/.nix-profile/bin/emacs

all: README.md

README.md: make-readme-markdown.el $(MODULE)
	$(EMACS) --script $< < $(MODULE) > $@

make-readme-markdown.el:
	wget -O $@ https://raw.github.com/mgalgs/make-readme-markdown/master/make-readme-markdown.el

.INTERMEDIATE: make-readme-markdown.el
