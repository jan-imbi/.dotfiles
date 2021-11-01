echo 'deb https://cloud.r-project.org/bin/linux/debian bullseye-cran40/' | sudo tee /etc/apt/sources.list -a
wget -O jranke.asc "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0xe19f5f87128899b192b1a2c2ad5f960a256a04af"
sudo apt-key add jranke.asc
sudo apt-get update
sudo apt-get upgrade
sudo apt install vim nano wget perl git emacs x11-xkb-utils
sudo apt install -t bullseye-cran40 r-base
ssh-keygen -t ed25519 -C "meis@imbi.uni-heidelberg.de"
eval "$(ssh-agent -s)"
ssh-add ~/.ssh/id_ed25519

git clone git@github.com:jan-imbi/.dotfiles.git
cd .dotfiles/
sh deploy.sh
cd /usr/share/fonts
sudo wget https://github.com/be5invis/Iosevka/releases/download/v10.3.4/ttc-iosevka-10.3.4.zip
sudo unzip ttc-iosevka-10.3.4.zip
cd ~