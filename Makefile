# TODO:
# - link configuration files for emacs, fish, git, dunst, kitty, xmonad, tmux, XCompose, Xdefaults

EXECUTABLE_NAMES = /tmux /seafile-applet /git /fish /pass /vlc /htop /kitty /compton /signal-desktop /dunst /nitrogen /offlineimap
EXECUTABLES = $(EXECUTABLE_NAMES:/%=/usr/bin/%)
X_TOUCHPAD_CONFIGURATION = /etc/X11/xorg.conf.d/50-touchpad.conf
XMONAD = ~/.local/bin/xmonad
TAFFYBAR = ~/.local/bin/my-taffybar
XMONAD_REPO = xmonad/xmonad
XMONAD_CONTRIB_REPO = xmonad/xmonad-contrib
TAFFYBAR_REPO = xmonad/my-taffybar/taffybar
EMACS_CONFIG = ~/.emacs.d
FISH_CONFIG = ~/.config/fish/config.fish
GIT_CONFIG = ~/.gitconfig
OFFLINEIMAPRC = ~/.offlineimaprc
OFFLINEIMAPPY = ~/.offlineimap.py
KITTY_CONFIG = ~/.config/kitty/kitty.conf
DUNST_CONFIG = ~/.config/dunst/dunstrc
XMONAD_CONFIG = ~/.xmonad
MKDOTFILE = ~/bin/mkdotfile
STACK = /usr/local/bin/stack
MU = ~/.local/bin/mu
MU_REPOSITORY = ~/var/src/mu
FOREIGN_SOURCE = ~/var/src
XMONAD_XSESSION = /usr/share/xsessions/xmonad.desktop
XMONAD_START_FILE = ~/.local/bin/start-xmonad

.PHONY: install
install: links ${XMONAD} ${TAFFYBAR} ${MU} ${XMONAD_XSESSION} ${XMONAD_START_FILE} ${EXECUTABLES} ${X_TOUCHPAD_CONFIGURATION}

${X_TOUCHPAD_CONFIGURATION}:
	sudo cp 50-touchpad.conf ${X_TOUCHPAD_CONFIGURATION}

/usr/bin/signal-desktop:
	sudo dnf -y copr enable luminoso/Signal-Desktop
	sudo dnf install -y signal-desktop

/usr/bin/seafile-applet:
	sudo dnf install -y seafile-client

/usr/bin/%:
	sudo dnf -y install $*

${XMONAD_XSESSION}:
	sudo cp xmonad/xmonad.desktop ${XMONAD_XSESSION}

${XMONAD_START_FILE}:
	cp xmonad/start-xmonad ${XMONAD_START_FILE}
	chmod +x ${XMONAD_START_FILE}

${EMACS_CONFIG}:
	ln -s ${PWD}/newmacs ${EMACS_CONFIG}

${FISH_CONFIG}:
	mkdir -p ${HOME}/.config/fish
	ln -s ${PWD}/config.fish ${FISH_CONFIG}

${GIT_CONFIG}:
	ln -s ${PWD}/gitconfig ${GIT_CONFIG}

${DUNST_CONFIG}:
	mkdir -p ${HOME}/.config/dunst
	ln -s ${PWD}/dunstrc ${DUNST_CONFIG}

${KITTY_CONFIG}:
	mkdir -p ${HOME}/.config/kitty
	ln -s ${PWD}/kitty.conf ${KITTY_CONFIG}

${XMONAD_CONFIG}:
	ln -s ${PWD}/xmonad ${XMONAD_CONFIG}

.PHONY: links
links: ${EMACS_CONFIG} ${FISH_CONFIG} ${GIT_CONFIG} ${DUNST_CONFIG} ${KITTY_CONFIG} ${XMONAD_CONFIG} ${MKDOTFILE} ${OFFLINEIMAPRC} ${OFFLINEIMAPPY}

${OFFLINEIMAPRC}:
	ln -s ${PWD}/offlineimaprc ${OFFLINEIMAPRC}

${OFFLINEIMAPPY}:
	ln -s ${PWD}/offlineimap.py ${OFFLINEIMAPPY}

xmonad:
	mkdir -p xmonad

${XMONAD_REPO}: xmonad
	cd xmonad && git clone "git@github.com:xmonad/xmonad"

${XMONAD_CONTRIB_REPO}: xmonad
	cd xmonad && git clone "git@github.com:xmonad/xmonad-contrib"

${TAFFYBAR_REPO}: xmonad
	cd xmonad/my-taffybar && git clone "git@github.com:taffybar/taffybar.git"

${MKDOTFILE}: ~/bin
	ln -s ${HOME}/config/bin/mkdotfile ${HOME}/bin

.PHONY: build
build: ${STACK} ${XMONAD_REPO} ${XMONAD_CONTRIB_REPO} ${TAFFYBAR_REPO}
	sudo dnf install -y gobject-introspection-devel libX11-devel libXrandr-devel libXinerama-devel libXScrnSaver-devel libXft-devel cairo-devel cairo-gobject-devel gdk-pixbuf2-devel pango-devel libdbusmenu-devel atk-devel gtksourceview3-devel libdbusmenu-gtk3-devel
	cd xmonad && stack install

${STACK}:
	curl -sSL https://get.haskellstack.org/ | sh

${MU}: ${MU_REPOSITORY}
	sudo dnf install -y guile-devel texinfo html2text xdg-utils guile22-devel gmime30-devel xapian-core-devel webkit2gtk3-devel
	cd ${MU_REPOSITORY} && \
	./autogen.sh --prefix=${HOME}/.local && \
	make -j5 && \
	make install

${MU_REPOSITORY}: ${FOREIGN_SOURCE}
	cd ${FOREIGN_SOURCE} && git clone https://github.com/djcb/mu.git

${FOREIGN_SOURCE}:
	mkdir -p ~/var/src

~/bin:
	mkdir -p ~/bin
