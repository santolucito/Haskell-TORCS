#an untested script that is roughly models the install process for torcs and the patch on linux

mkdir torcs
cd torcs
#best to download these by hand
#wget http://prdownloads.sourceforge.net/torcs/torcs-1.3.4.tar.bz2?download
#wget https://sourceforge.net/projects/cig/files/SCR%20Championship/Server%20Linux/2.1/scr-linux-patch.tgz/download
tar xfvj torcs-1.3.4.tar.bz2 
mv scr-linux-patch.tgz torcs-1.3.4/
cd torcs-1.3.4
tar -xvzf scr-linux-patch.tgz
cd scr-patch
sh do_patch.sh
cd ..

#once everything is unpacked, start installing
sudo apt-get --assume-yes install libplib-dev
sudo apt-get --assume-yes install libopenal1 libopenal-dev libvorbis-dev libxrender-dev libxrandr-dev
sudo apt-get install libalut0 libalut-dev
sudo apt-get install libxxf86vm-dev libpng-dev
./configure
#comment out line 70 in src/modules/simu/simuv2/simu.cpp
make
sudo make install
sudo make datainstall

