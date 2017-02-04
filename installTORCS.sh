mkdir torcs
cd torcs
wget http://prdownloads.sourceforge.net/torcs/torcs-1.3.4.tar.bz2?download
wget https://sourceforge.net/projects/cig/files/SCR%20Championship/Server%20Linux/
tar xfvj torcs-1.3.4.tar.bz2 
mv scr-linux-patch.tgz torcs-1.3.4/
cd torcs-1.3.4
tar -xvzf scr-linux-patch.tgz
cd scr-patch
sh do_patch.sh
cd ..

#once everything is unpacked, start installing
sudo apt-get --assume-yes install libplib-dev
sudo apt-get --assume-yes install libopenal1 libopenal-dev
sudo apt-get install libalut0 libalut-dev
sudo apt-get install libxxf86vm-dev libpng-dev
.configure
make
sudo make install
sudo make datainstall

