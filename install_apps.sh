sudo add-apt-repository ppa:c2d4u.team/c2d4u4.0+ -y
sudo add-apt-repository ppa:jonathonf/vim -y
sudo apt install --no-install-recommends software-properties-common dirmngr
wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | sudo tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc
sudo add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/"
sudo apt update
sudo apt upgrade -y
sudo apt install \
terminator tree ssh zsh curl git vim fzf ripgrep \
autojump zsh-syntax-highlighting zsh-autosuggestions \
r-base-core r-cran-tidyverse  \
r-cran-kableextra r-cran-styler \
r-cran-shiny r-cran-rmarkdown r-cran-tidyverse r-cran-knitr \
texlive-science zathura qutebrowser firefox  -y
