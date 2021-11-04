wget -O jranke.asc "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0xe19f5f87128899b192b1a2c2ad5f960a256a04af"
sudo apt-key add jranke.asc
sudo apt-get update
sudo apt-get upgrade
sudo apt install -t bullseye-cran40 r-base
cd .dotfiles/
sh deploy.sh
cd /usr/share/fonts
sudo wget https://github.com/be5invis/Iosevka/releases/download/v10.3.4/ttc-iosevka-10.3.4.zip
sudo unzip ttc-iosevka-10.3.4.zip
cd ~
sh .dotfiles/install-texlive.sh
