#!/bin/bash
APT_PACKAGES=(fd-find ripgrep libtool-bin cmake tidy)
BREW_PACKAGES=(fd ripgrep readline openssl zlib postgresql sqlite ruby-build rbenv libffi libgccjit gcc)
NPM_PACKAGES=(vscode-css-languageserver-bin vscode-html-languageserver-bin typescript-language-server @tailwindcss/language-server@0.0.4 stylelint js-beautify import-js)
GEMS=(solargraph bundler)
DOOM_GIT=(https://github.com/hlissner/doom-emacs)
DOTFILES_GIT=(https://github.com/otavioschwanck/doom_emacs_dotfiles.git)

get_bash_profile () {
  if test -f ".zshrc"; then
    BASH_PROFILE=(.zshrc)
  else
    BASH_PROFILE=(.bashrc)
  fi
}
get_machine_type () {
  unameOut="$(uname -s)"
  case "${unameOut}" in
    Linux*)     machine=Linux;;
    Darwin*)    machine=Mac;;
    CYGWIN*)    machine=Cygwin;;
    MINGW*)     machine=MinGw;;
    *)          machine="UNKNOWN:${unameOut}"
  esac
  echo "Proceeding instalation for OS ${machine}"
}

ask_question () {
  echo "Do you wish to install $1?"
  select yn in "Yes" "No"; do
    case $yn in
      Yes ) echo "Installing $1";$2;break;;
      No ) echo "Not installing $1";break;;
    esac
  done
}

# Install base packages
install_packages_linux () {
  echo "================= INSTALLING PACKAGES ================="
  sudo apt-get -qq update
  sudo apt-get -qq install ${APT_PACKAGES[*]} -y
  sudo npm install -s -g ${NPM_PACKAGES[*]} -y
}

install_packages_mac () {
  echo "================= INSTALLING PACKAGES ================="
  brew install ${BREW_PACKAGES[*]}
  brew link libpq --force
}

prompt_ruby_versions () {
  echo "Which ruby versions would you like to install? Use spaces to install more than one"
  echo "Example: 2.7.1 3.0.1 3.1.1"
  echo "Leave blank to not install any"
  IFS= read -rp 'Ruby Verions: ' USER_RUBY_INPUT
  IFS=' '
  read -a RUBY_VERSIONS <<< "$USER_RUBY_INPUT"
}

install_ruby_linux () {
  echo "================= INSTALLING RUBY ================="
  curl -fsSL https://github.com/rbenv/rbenv-installer/raw/HEAD/bin/rbenv-installer | bash
  git clone https://github.com/rbenv/ruby-build.git
  cat ruby-build/install.sh
  PREFIX=/usr/local sudo ./ruby-build/install.sh
  echo "gem: --no-document" > ~/.gemrc
  prompt_ruby_versions
  for i in "${RUBY_VERSIONS[@]}"; do rbenv install $i -s; echo "Installed ruby version $i"; done
}

install_ruby_mac () {
  echo "================= INSTALLING RUBY ON MAC ================="
  echo "gem: --no-document" > ~/.gemrc
  prompt_ruby_versions
  for i in "${RUBY_VERSIONS[@]}"; do rbenv install $i -s; echo "Installed ruby version $i"; done
}

install_fonts () {
  echo "================= INSTALLING FONTS ================="
  if [ -d ~/$FONTS_LIBRARY ]; then
    echo "Fonts folder located, proceeding to install fonts"
  else
    mkdir ~/$FONTS_LIBRARY
  fi
  cd; wget -q https://github.com/ryanoasis/nerd-fonts/releases/download/v2.1.0/JetBrainsMono.zip
  unzip -q -o JetBrainsMono.zip -d ~/$FONTS_LIBRARY
  cd; rm JetBrainsMono.zip
}

install_gems () {
  echo "================= INSTALLING GEMS ================="
  if [ "$(which rbenv)" = "" ]
  then
    echo "Rbenv not found"
  else
    cd ~/.rbenv/versions/; RUBY_VERSION=(*);rbenv local "$RUBY_VERSION"; rbenv global "$RUBY_VERSION"; cd
    echo "Ruby version $RUBY_VERSION was set as global"
    echo 'eval "$(rbenv init -)"' >> ~/$BASH_PROFILE
    eval "$(rbenv init -)"
    rbenv local $RUBY_VERSION
  fi
  for i in $GEMS; do gem install $i; done
}

install_emacs_linux () {
  echo "================= INSTALLING EMACS ================="
  # Install Emacs using snap
  sudo snap install emacs --classic
}

install_emacs_mac () {
  echo "================= INSTALLING EMACS ================="
  brew tap d12frosted/emacs-plus
  brew install emacs-plus
}

install_emacs_doom () {
  echo "================= INSTALLING DOOM ================="
  git clone --quiet --depth 1 $DOOM_GIT ~/.emacs.d
  git clone --quiet $DOTFILES_GIT ~/.doom.d
  ~/.emacs.d/bin/doom sync
}

linux_workflow () {
  FONTS_LIBRARY=".fonts"
  ask_question "base packages for emacs" install_packages_linux
  if [ "$(which emacs)" = "" ]
  then
    'We did not find a working emacs client within your system, proceeding to ask for instalation'
    ask_question "Emacs client" install_emacs_linux
  fi
  if [ "$(which rbenv)" = "" ]
  then
    'We did not find a working rbenv within your system, proceeding to ask for instalation'
    ask_question "Ruby on Rails with Rbenv" install_ruby_linux
  fi
}

mac_workflow () {
  FONTS_LIBRARY="Library/Fonts"
  ask_question "base packages for emacs" install_packages_mac
  if [ "$(which emacs)" = "" ]
  then
    'We did not find a working emacs client within your system, proceeding to ask for instalation'
    ask_question "Emacs client" install_emacs_mac
  fi
  if [ "$(which rbenv)" = "" ]
  then
    'We did not find a working rbenv within your system, proceeding to ask for instalation'
    ask_question "Ruby on Rails with Rbenv" install_ruby_mac
  fi
}

# SCRIPT QUESTIONAIRE
get_machine_type
get_bash_profile
case "${machine}" in
  Linux)     linux_workflow;;
  Mac)       mac_workflow;;
  *)         echo "OS not recognized"
esac

install_fonts
install_gems
install_emacs_doom

echo "Script finished!"
