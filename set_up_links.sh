#!/bin/zsh

# since the install process creates a .config directory move it temporarily
mv ~/.config ~/.config.tmp

# create links to hidden files from ~/Dropbox/dotfiles directories
ff=(".zshrc" ".viminfo" ".vimrc" ".local" ".vim" ".vimplugins" ".config" ".Rprofile")
for P in "${ff[@]}"
do
echo "create  a link for Dropbox/dotfiles version of $P in Home"
    ln -v -s "$HOME/Dropbox/dotfiles/$P" "$HOME/$P"
done

# copy the original ".config" files into new linked .config
cp -R ~/config.tmp/* ~/.config

# create new directories (links) for working files from Dropbox
dd=("sandbox" "bin" "docs" "prj" "work" "ssh" "shr")
for P in "${dd[@]}"
do
    echo "create  a link for Dropbox/dotfiles version of echo $P in Home"
    ln -v -s "$HOME/Dropbox/$P" "$HOME/$P"
done
