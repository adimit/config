# TODO:
# - link configuration files for emacs, fish, git, dunst, kitty, xmonad, tmux, XCompose, Xdefaults

EXECUTABLE_NAMES = /tmux /seafile-applet /git /fish /pass /vlc /htop /kitty /compton /dunst /nitrogen /offlineimap /lollypop /flac /oggenc /picard /gimp /npm /chromium-browser /jq /ledger /curl /sqlite3
EXECUTABLES = $(EXECUTABLE_NAMES:/%=/usr/bin/%)
NPM_EXECUTABLES = /tsc /eslint /prettier
NPM_BINARIES = $(NPM_EXECUTABLES:/%=/home/aleks/.local/bin/%)
X_TOUCHPAD_CONFIGURATION = /etc/X11/xorg.conf.d/50-touchpad.conf
LOCAL = ${HOME}/.local
XMONAD = ${LOCAL}/bin/xmonad
TAFFYBAR = ${LOCAL}/bin/my-taffybar
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
STACK = /usr/local/bin/stack
FOREIGN_SOURCE = ~/var/src
MU = ${LOCAL}/bin/mu
MU_REPOSITORY = ${FOREIGN_SOURCE}/mu
EMACS = ${LOCAL}/bin/emacs
EMACS_REPOSITORY = ${FOREIGN_SOURCE}/emacs
XMONAD_XSESSION = /usr/share/xsessions/xmonad.desktop
XMONAD_START_FILE = ${LOCAL}/bin/start-xmonad
RUST = ${HOME}/.cargo/bin/rustc
WASM_PACK = ${HOME}/.cargo/bin/wasm-pack
CARGO_GENERATE = ${HOME}/.cargo/bin/cargo-generate
RUST_ANALYZER = ${HOME}/.local/bin/rust-analyzer
FONTS = ${HOME}/.fonts
FIRA_CODE = ${FONTS}/FiraCode
FIRA_GO = ${FONTS}/FiraGo
BITTER = ${FONTS}/Bitter
INSTALL_CMD=sudo apt install -y
HLEDGER = ${LOCAL}/bin/hledger
NPM_PREFIX = npm set prefix ${LOCAL}

.PHONY: install
install: links ${EXECUTABLES} ${FOREIGN_SOURCE} ${XMONAD} ${EMACS} ${MU} ${XMONAD_XSESSION} ${XMONAD_START_FILE} ${X_TOUCHPAD_CONFIGURATION} ${RUST} ${WASM_PACK} ${CARGO_GENERATE} ${RUST_ANALYZER} ${FIRA_CODE} ${FIRA_GO} ${NPM_BINARIES} ${BITTER} ${HLEDGER}

${X_TOUCHPAD_CONFIGURATION}:
	sudo mkdir -p $$(dirname ${X_TOUCHPAD_CONFIGURATION})
	sudo cp 50-touchpad.conf ${X_TOUCHPAD_CONFIGURATION}

/usr/bin/chromium-browser:
#${INSTALL_CMD} chromium
	${INSTALL_CMD} chromium-browser

/usr/bin/oggenc:
	${INSTALL_CMD} vorbis-tools

/usr/bin/seafile-applet:
# ${INSTALL_CMD} seafile-client
	${INSTALL_CMD} seafile-gui

/usr/bin/%:
	${INSTALL_CMD} $*

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
links: ${EMACS_CONFIG} ${FISH_CONFIG} ${GIT_CONFIG} ${DUNST_CONFIG} ${KITTY_CONFIG} ${XMONAD_CONFIG} ${OFFLINEIMAPRC} ${OFFLINEIMAPPY}

${OFFLINEIMAPRC}:
	ln -s ${PWD}/offlineimaprc ${OFFLINEIMAPRC}

${OFFLINEIMAPPY}:
	ln -s ${PWD}/offlineimap.py ${OFFLINEIMAPPY}

${XMONAD_REPO}:
	mkdir -p xmonad
	cd xmonad &&  test -d xmonad || git clone "git@github.com:xmonad/xmonad"

${XMONAD_CONTRIB_REPO}: ${XMONAD_REPO}
	cd xmonad && test -d xmonad-contrib || git clone "git@github.com:xmonad/xmonad-contrib"

${TAFFYBAR_REPO}: ${XMONAD_REPO}
	cd xmonad/my-taffybar && test -d taffybar || git clone "git@github.com:taffybar/taffybar.git"

${HLEDGER}: ${STACK}
	stack install hledger hledger-web

${XMONAD}: ${STACK} ${XMONAD_REPO} ${XMONAD_CONTRIB_REPO} ${TAFFYBAR_REPO}
	# sudo dnf install -y gobject-introspection-devel libX11-devel libXrandr-devel libXinerama-devel libXScrnSaver-devel libXft-devel cairo-devel cairo-gobject-devel gdk-pixbuf2-devel pango-devel libdbusmenu-devel atk-devel gtksourceview3-devel libdbusmenu-gtk3-devel
	${INSTALL_CMD} gobject-introspection libx11-dev libxrandr-dev libxinerama-dev libxss-dev libxft-dev libcairo2-dev libcairo-gobject2 libgdk-pixbuf2.0-dev libsdl-pango-dev libdbusmenu-gtk3-dev libgtksourceview-3.0-dev libgirepository1.0-dev
	cd xmonad && stack install

${STACK}:
	curl -sSL https://get.haskellstack.org/ | sh

${MU}: ${MU_REPOSITORY}
	# sudo dnf install -y guile-devel texinfo html2text xdg-utils guile22-devel gmime30-devel xapian-core-devel webkit2gtk3-devel
	${INSTALL_CMD} guile-3.0-dev texinfo html2text xdg-utils guile-2.2-dev libgmime-3.0-dev libxapian-dev libwebkit2gtk-4.0-dev

	cd ${MU_REPOSITORY} && \
	./autogen.sh --prefix=${LOCAL} && \
	make -j5 && \
	make install

${MU_REPOSITORY}:
	cd ${FOREIGN_SOURCE} && git clone https://github.com/djcb/mu.git

${EMACS_REPOSITORY}:
	cd ${FOREIGN_SOURCE} && git clone https://git.savannah.gnu.org/git/emacs.git

${EMACS}: ${EMACS_REPOSITORY}
	# sudo dnf builddep -y emacs
	# sudo dnf install -y jansson-devel
	sudo apt build-dep -y emacs
	${INSTALL_CMD} libjansson-dev
	cd ${EMACS_REPOSITORY} && git checkout emacs-27 && ./autogen.sh && ./configure --prefix=${LOCAL} --with-xwidgets && make -j5 && make install

${FOREIGN_SOURCE}:
	mkdir -p ~/var/src

~/bin:
	mkdir -p ~/bin

${RUST}:
	curl --proto '=https' --tlsv1.2 -sSf 'https://sh.rustup.rs' > /tmp/install_rust.sh && sh /tmp/install_rust.sh -y && rm -f /tmp/install_rust.sh

${WASM_PACK}:
	curl 'https://rustwasm.github.io/wasm-pack/installer/init.sh' -sSf | PATH=$$HOME/.cargo/bin:$$PATH sh

${CARGO_GENERATE}:
	# sudo dnf install -y openssl-devel
	${INSTALL_CMD} libssl-dev
	cargo install cargo-generate

${RUST_ANALYZER}:
	curl -L https://github.com/rust-analyzer/rust-analyzer/releases/latest/download/rust-analyzer-linux -o ${RUST_ANALYZER}
	chmod +x ${RUST_ANALYZER}

${FONTS}:
	mkdir -p ${FONTS}

FONT_TMP_FILE = /tmp/fonts.zip

${FIRA_CODE}:
	curl -L 'https://github.com/tonsky/FiraCode/releases/download/5.2/Fira_Code_v5.2.zip' -o ${FONT_TMP_FILE}
	mkdir -p ${FIRA_CODE}
	unzip ${FONT_TMP_FILE} -d ${FIRA_CODE}
	rm -f ${FONT_TMP_FILE}
	fc-cache -fv ${FONTS}

${FIRA_GO}:
	curl -L 'https://bboxtype.com/downloads/FiraGO/Download_Folder_FiraGO_1001.zip' -o ${FONT_TMP_FILE}
	mkdir -p ${FIRA_GO}
	unzip ${FONT_TMP_FILE} -d ${FIRA_GO}
	rm -f ${FONT_TMP_FILE}
	fc-cache -fv ${FONTS}

${BITTER}:
	curl -L 'https://fonts.google.com/download?family=Bitter' -o ${FONT_TMP_FILE}
	mkdir -p ${BITTER}
	unzip ${FONT_TMP_FILE} -d ${BITTER}
	rm -f ${FONT_TMP_FILE}
	fc-cache -fv

${LOCAL}/bin/tsc:
	${NPM_PREFIX}
	npm i -g typescript

${LOCAL}/bin/eslint:
	${NPM_PREFIX}
	npm i -g eslint

${LOCAL}/bin/prettier:
	${NPM_PREFIX}
	npm i -g prettier
