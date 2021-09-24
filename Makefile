ifneq (,$(realpath /usr/bin/dpkg))
OS=Debianish
INSTALL_CMD=sudo apt install -y
else ifneq (,$(realpath /usr/bin/dnf))
OS=Fedora
INSTALL_CMD=sudo dnf install -y
else
$(error Could not find dpkg or dnf)
endif

EXECUTABLE_NAMES = /tmux /seafile-applet /git /fish /pass /vlc /htop /kitty /compton /dunst /nitrogen /offlineimap /lollypop /flac /oggenc /picard /gimp /npm /chromium-browser /jq /ledger /curl /sqlite3 /stalonetray /i3lock /psql /javac /rg /virtualenv /syncthing /pip3 /dot /latex /xelatex /dvipng /scrot /biber /R /black /stellarium /pipenv /anki /inkscape /mvn /tlp
EXECUTABLES = $(EXECUTABLE_NAMES:/%=/usr/bin/%)
NPM_EXECUTABLES = /tsc /eslint /prettier
NPM_BINARIES = $(NPM_EXECUTABLES:/%=${HOME}/.local/bin/%)
X_TOUCHPAD_CONFIGURATION = /etc/X11/xorg.conf.d/50-touchpad.conf
LOCAL = ${HOME}/.local
CARGO_LOCAL = ${HOME}/.cargo
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
ALACRITTY = ${CARGO_LOCAL}/bin/alacritty
FD = ${CARGO_LOCAL}/bin/fd
CASK = ${LOCAL}/bin/cask

.PHONY: install
install: links ${EXECUTABLES} ${FOREIGN_SOURCE} ${XMONAD} ${EMACS} ${MU} ${XMONAD_XSESSION} ${XMONAD_START_FILE} ${X_TOUCHPAD_CONFIGURATION} ${RUST} ${WASM_PACK} ${CARGO_GENERATE} ${RUST_ANALYZER} ${FIRA_CODE} ${FIRA_GO} ${NPM_BINARIES} ${BITTER} ${HLEDGER} ${ALACRITTY} ${FD}

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

/usr/bin/psql:
	${INSTALL_CMD} postgresql-client

/usr/bin/javac:
	${INSTALL_CMD} openjdk-14-jdk

/usr/bin/rg:
	${INSTALL_CMD} ripgrep

/usr/bin/virtualenv:
	${INSTALL_CMD} python3-virtualenv

/usr/bin/syncthing:
ifeq (${OS},Fedora)
else
	sudo curl -s -o /usr/share/keyrings/syncthing-archive-keyring.gpg https://syncthing.net/release-key.gpg
	echo "deb [signed-by=/usr/share/keyrings/syncthing-archive-keyring.gpg] https://apt.syncthing.net/ syncthing stable" | sudo tee /etc/apt/sources.list.d/syncthing.list
	printf "Package: *\nPin: origin apt.syncthing.net\nPin-Priority: 990\n" | sudo tee /etc/apt/preferences.d/syncthing
	sudo apt update
endif
	${INSTALL_CMD} syncthing

/usr/bin/pip3:
	${INSTALL_CMD} python3-pip python3-dev

/usr/bin/dot:
	${INSTALL_CMD} graphviz

/usr/bin/latex:
	${INSTALL_CMD} texlive

/usr/bin/xelatex:
	${INSTALL_CMD} texlive-xetex texlive-fonts-extra

/usr/bin/R:
	${INSTALL_CMD} r-base r-cran-ggplot2

/usr/bin/mvn:
	${INSTALL_CMD} maven

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

${CASK}:
	cd /var/tmp && git clone "https://github.com/cask/cask" && cd cask && make install

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
EMACS_DEPENDENCIES=libjansson-dev libxpm-dev libgif-dev libjpeg-dev libpng-dev libtiff-dev libx11-dev libncurses5-dev automake autoconf texinfo libgtk2.0-dev libwebkit2gtk-4.0-dev libgccjit-10-dev gcc-10 g++-10 libgccjit0 libgccjit-10-dev libjansson4 libjansson-dev libgtk-3-dev libmagick++-dev fonts-symbola fonts-noto-color-emoji
endif
${EMACS}: ${EMACS_REPOSITORY}
ifeq (${OS},Fedora)
	sudo dnf builddep -y emacs
else
	sudo add-apt-repository -y ppa:ubuntu-toolchain-r/ppa
	sudo apt build-dep -y emacs
endif
	${INSTALL_CMD} ${EMACS_DEPENDENCIES}
	cd ${EMACS_REPOSITORY} && ./autogen.sh && ( CC=/usr/bin/gcc-10 CXX=/usr/bin/gcc-10 ./configure --prefix=${LOCAL} --with-xwidgets --with-cairo --with-modules --without-compress-install --with-x-toolkit=yes --with-gnutls --without-gconf --without-toolkit-scroll-bars --without-xaw3d --without-gsettings --with-mailutils --with-native-compilation --with-json --with-harfbuzz --with-imagemagick --with-jpeg --with-png --with-rsvg --with-tiff --with-wide-int --without-xft --with-xml2 --with-xpm CFLAGS="-O3 -mtune=native -march=native -fomit-frame-pointer" && make -j5 && make install)

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
	( PATH=$$HOME/.cargo/bin:$$PATH cargo install cargo-generate )

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

ifeq (${OS},Fedora)
DEPENDENCIES=cmake freetype-devel fontconfig-devel libxcb-devel libxkbcommon-devel g++
else
DEPENDENCIES=cmake pkg-config libfreetype6-dev libfontconfig1-dev libxcb-xfixes0-dev libxkbcommon-dev python3
endif
${ALACRITTY}:
	${INSTALL_CMD} ${DEPENDENCIES}
	( PATH=$$HOME/.cargo/bin:$$PATH cargo install alacritty )

${FD}:
	( PATH=$$HOME/.cargo/bin:$$PATH cargo install fd-find )
