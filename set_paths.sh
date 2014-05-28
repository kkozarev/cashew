#!/usr/bin/env bash
# sets up environmentals and paths

#set CORWAV to the directory where analyzed events will be stored
CORWAV="/Volumes/Backscratch/Users/kkozarev/corwav/"
#set CORWAV_DATA to the directory where the local copy of data will live
CORWAV_DATA="/Volumes/Itch/Users/kkozarev/corwav/"
#set CORWAV_TRUNK to the directory of the CorWav code (or local copy of the repository)
CORWAV_TRUNK="${HOME}/svn/corwav/trunk/"
#set CORWAV_WEB to the directory where the web database page is.
CORWAV_WEB="/var/www/personal/kkozarev/public_html/"



#=======================NO USER INPUT BELOW!===================================

#Add the global vars to .bashrc and .bash_profile
echo "export CORWAV=\"$CORWAV\"" >> ~/.bashrc
echo "export CORWAV_DATA=\"$CORWAV_DATA\"" >> ~/.bashrc
echo "export CORWAV_TRUNK=\"$CORWAV_TRUNK\"" >> ~/.bashrc
echo "export CORWAV_WEB=\"$CORWAV_WEB\"" >> ~/.bashrc
echo "export CORWAV=\"$CORWAV\"" >> ~/.bash_profile
echo "export CORWAV_DATA=\"$CORWAV_DATA\"" >> ~/.bash_profile
echo "export CORWAV_TRUNK=\"$CORWAV_TRUNK\"" >> ~/.bash_profile
echo "export CORWAV_WEB=\"$CORWAV_WEB\"" >> ~/.bash_profile

#Add the global vars to .cshrc
echo "setenv CORWAV $CORWAV" >> ~/.cshrc.user
echo "setenv CORWAV_DATA $CORWAV_DATA" >> ~/.cshrc.user
echo "setenv CORWAV_TRUNK $CORWAV_TRUNK" >> ~/.cshrc.user
echo "setenv CORWAV_WEB $CORWAV_WEB" >> ~/.cshrc.user

idl_path='!PATH'"=expand_path('+${CORWAV_TRUNK}',/all_dirs)+':'"+'!PATH'
echo $idl_path >> ~/.idl_startup
echo "" >> ~/.idl_startup
#
#eof
#
