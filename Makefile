ifneq (,$(realpath /usr/bin/dpkg))
OS=Debianish
INSTALL_CMD=sudo apt install -y
else ifneq (,$(realpath /usr/bin/dnf))
OS=Fedora
INSTALL_CMD=sudo dnf install -y
else
$(error Could not find dpkg or dnf)
endif

EXECUTABLE_NAMES = /tmux /seafile-applet /git /fish /pass /vlc /htop /kitty /compton /dunst /nitrogen /offlineimap /lollypop /flac /oggenc /picard /gimp /npm /chromium-browser /jq /ledger /curl /sqlite3 /stalonetray /i3lock
EXECUTABLES = $(EXECUTABLE_NAMES:/%=/usr/bin/%)
NPM_EXECUTABLES = /tsc /eslint /prettier
NPM_BINARIES = $(NPM_EXECUTABLES:/%=/home/aleks/.local/bin/%)
X_TOUCHPAD_CONFIGURATION = /etc/X11/xorg.conf.d/50-touchpad.conf
LOCAL = ${HOME}/.local
XMONAD = ${LOCAL}/bin/xmonad
XMONAD_REPO = xmonad/xmonad
XMOBAR_REPO = xmonad/xmobar
XMONAD_CONTRIB_REPO = xmonad/xmonad-contrib
XMOBAR_CONFIG = ~/.xmobarrc
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
HLEDGER = ${LOCAL}/bin/hledger
NPM_PREFIX = npm set prefix ${LOCAL}

.PHONY: install
install: links ${EXECUTABLES} ${FOREIGN_SOURCE} ${XMONAD} ${EMACS} ${MU} ${XMONAD_XSESSION} ${XMONAD_START_FILE} ${X_TOUCHPAD_CONFIGURATION} ${RUST} ${WASM_PACK} ${CARGO_GENERATE} ${RUST_ANALYZER} ${FIRA_CODE} ${FIRA_GO} ${NPM_BINARIES} ${BITTER} ${HLEDGER}

${X_TOUCHPAD_CONFIGURATION}:
	sudo mkdir -p $$(dirname ${X_TOUCHPAD_CONFIGURATION})
	sudo cp 50-touchpad.conf ${X_TOUCHPAD_CONFIGURATION}

ifeq (${OS},Fedora)
CHROMIUM_PACKAGE=chromium
else
CHROMIUM_PACKAGE=chromium-browser
endif
/usr/bin/chromium-browser:
	${INSTALL_CMD} ${CHROMIUM_BROWSER}

/usr/bin/oggenc:
	${INSTALL_CMD} vorbis-tools

ifeq (${OS},Fedora)
SEAFILE_PACKAGE=chromium
else
SEAFILE_PACKAGE=chromium-browser
endif
/usr/bin/seafile-applet:
	${INSTALL_CMD} ${SEAFILE_PACKAGE}

/usr/bin/%:
	${INSTALL_CMD} $*

${XMONAD_XSESSION}:
	sudo cp xmonad/xmonad.desktop ${XMONAD_XSESSION}

${XMONAD_START_FILE}:
	ln -s ${PWD}/xmonad/start-xmonad ${XMONAD_START_FILE}

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
links: ${EMACS_CONFIG} ${FISH_CONFIG} ${GIT_CONFIG} ${DUNST_CONFIG} ${KITTY_CONFIG} ${XMONAD_CONFIG} ${OFFLINEIMAPRC} ${OFFLINEIMAPPY} ${XMOBAR_CONFIG}

${XMOBAR_CONFIG}:
	ln -s ${PWD}/xmonad/xmobarrc ${XMOBAR_CONFIG}

${OFFLINEIMAPRC}:
	ln -s ${PWD}/offlineimaprc ${OFFLINEIMAPRC}

${OFFLINEIMAPPY}:
	ln -s ${PWD}/offlineimap.py ${OFFLINEIMAPPY}

${XMONAD_REPO}:
	cd xmonad && test -d xmonad || git clone "git@github.com:xmonad/xmonad"

${XMONAD_CONTRIB_REPO}: ${XMONAD_REPO}
	cd xmonad && test -d xmonad-contrib || git clone "git@github.com:xmonad/xmonad-contrib"

${XMOBAR_REPO}: ${XMONAD_REPO}
	cd xmonad && test -d xmobar || git clone "git@github.com:jaor/xmobar"

${HLEDGER}: ${STACK}
	stack install hledger hledger-web

ifeq (${OS},Fedora)
XMONAD_DEPENDENCIES=libX11-devel libXrandr-devel libXinerama-devel libXScrnSaver-devel libXft-devel libXpm-devel
else
XMONAD_DEPENDENCIES=libx11-dev libxrandr-dev libxinerama-dev libxss-dev libxft-dev libasound2-dev libxpm-dev
endif
${XMONAD}: ${STACK} ${XMONAD_REPO} ${XMONAD_CONTRIB_REPO} ${XMOBAR_REPO}
	${INSTALL_CMD} ${XMONAD_DEPENDENCIES}
	cd xmonad && stack install

${STACK}:
	curl -sSL https://get.haskellstack.org/ | sh

ifeq (${OS},Fedora)
MU_DEPENDENCIES=guile-devel texinfo html2text xdg-utils guile22-devel gmime30-devel xapian-core-devel webkit2gtk3-devel
else
MU_DEPENDENCIES=guile-3.0-dev texinfo html2text xdg-utils guile-2.2-dev libgmime-3.0-dev libxapian-dev libwebkit2gtk-4.0-dev
endif
${MU}: ${MU_REPOSITORY}
	${INSTALL_CMD} ${MU_DEPENDENCIES}
	cd ${MU_REPOSITORY} && \
	./autogen.sh --prefix=${LOCAL} && \
	make -j5 && \
	make install

${MU_REPOSITORY}:
	cd ${FOREIGN_SOURCE} && git clone https://github.com/djcb/mu.git

${EMACS_REPOSITORY}:
	cd ${FOREIGN_SOURCE} && git clone https://git.savannah.gnu.org/git/emacs.git

ifeq (${OS},Fedora)
EMACS_DEPENDENCIES=jansson-devel
else
EMACS_DEPENDENCIES=libjansson-dev
endif
${EMACS}: ${EMACS_REPOSITORY}
ifeq (${OS},Fedora)
	sudo dnf builddep -y emacs
else
	sudo apt build-dep -y emacs
endif
	${INSTALL_CMD} ${EMACS_DEPENDENCIES}
	cd ${EMACS_REPOSITORY} && git checkout emacs-27 && ./autogen.sh && ./configure --prefix=${LOCAL} --with-xwidgets && make -j5 && make install

${FOREIGN_SOURCE}:
	mkdir -p ~/var/src

~/bin:
	mkdir -p ~/bin

${RUST}:
	curl --proto '=https' --tlsv1.2 -sSf 'https://sh.rustup.rs' > /tmp/install_rust.sh && sh /tmp/install_rust.sh -y && rm -f /tmp/install_rust.sh

${WASM_PACK}:
	curl 'https://rustwasm.github.io/wasm-pack/installer/init.sh' -sSf | PATH=$$HOME/.cargo/bin:$$PATH sh

ifeq (${OS},Fedora)
CARGO_DEPENDENCIES=openssl-devel
else
CARGO_DEPENDENCIES=libssl-dev
endif
${CARGO_GENERATE}:
	${INSTALL_CMD} ${CARGO_DEPENDENCIES}
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
