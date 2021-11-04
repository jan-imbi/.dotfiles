wget https://mirror.ctan.org/systems/texlive/tlnet/install-tl-unx.tar.gz
tar -xf install-tl-unx.tar.gz
rm install-tl-unx.tar.gz
cd ./install-tl-*
echo 'selected_scheme scheme-full' > temp.profile
sudo perl install-tl -profile temp.profile
cd ~
rm -rf install-tl-*
echo 'PATH=/usr/local/texlive/2021/bin/x86_64-linux:$PATH' >> .bashrc
