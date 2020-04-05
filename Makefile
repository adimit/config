# TODO:
# - download git repos for xmonad & taffybar
# - compile xmonad & taffybar
# - link configuration files for emacs, fish, git, dunst, kitty, xmonad, tmux, XCompose, Xdefaults

XMONAD_REPO = xmonad/xmonad
XMONAD_CONTRIB_REPO = xmonad/xmonad-contrib
TAFFYBAR_REPO = xmonad/my-taffybar/taffybar
GIT_REPOS = ${XMONAD_REPO} ${XMONAD_CONTRIB_REPO} ${TAFFYBAR_REPO}
EMACS_CONFIG = ~/.emacs.d
FISH_CONFIG = ~/.config/fish/config.fish
GIT_CONFIG = ~/.gitconfig
KITTY_CONFIG = ~/.config/kitty/kitty.conf
DUNST_CONFIG = ~/.config/dunst/dunstrc
XMONAD_CONFIG = ~/.xmonad

.PHONY: install
install: build links

${EMACS_CONFIG}:
	ln -s ${PWD}/newmacs ${EMACS_CONFIG}

${FISH_CONFIG}:
	ln -s ${PWD}/config.fish ${FISH_CONFIG}

${GIT_CONFIG}:
	ln -s ${PWD}/gitconfig ${GIT_CONFIG}

${DUNST_CONFIG}:
	ln -s ${PWD}/dunstrc ${DUNST_CONFIG}

${KITTY_CONFIG}:
	ln -s ${PWD}/kitty.conf ${KITTY_CONFIG}

${XMONAD_CONFIG}:
	ln -s ${PWD}/xmonad ${XMONAD_CONFIG}

.PHONY: links
links: ${EMACS_CONFIG} ${FISH_CONFIG} ${GIT_CONFIG} ${DUNST_CONFIG} ${KITTY_CONFIG} ${XMONAD_CONFIG}

${XMONAD_REPO}:
	cd xmonad && git clone "git@github.com:xmonad/xmonad"

${XMONAD_CONTRIB_REPO}:
	cd xmonad && git clone "git@github.com:xmonad/xmonad-contrib"

${TAFFYBAR_REPO}:
	cd xmonad/my-taffybar && git clone "git@github.com:taffybar/taffybar.git"

.PHONY: build
build: ${GIT_REPOS}
	cd xmonad && stack install
