sudo apt-get update
sudo apt-get upgrade
sudo apt install vim nano wget perl git emacs x11-xkb-utils gnupg tmux docker

ssh-keygen -t ed25519 -C "meis@imbi.uni-heidelberg.de"
eval "$(ssh-agent -s)"
ssh-add ~/.ssh/id_ed25519
git clone git@github.com:jan-imbi/.dotfiles.git
