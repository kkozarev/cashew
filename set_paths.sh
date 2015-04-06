#!/usr/bin/env bash
# sets up environmentals and paths

#set CORWAV to the directory where analyzed events will be stored
CORWAV="${HOME}/corwav/"
#set CORWAV_DATA to the directory where the local copy of data will live
CORWAV_DATA="${HOME}/AIA_data/"
#set CORWAV_TRUNK to the directory of the CorWav code (or local copy of the repository)
CORWAV_TRUNK="${HOME}/git/corwav/"
#set CORWAV_WEB to the directory where the web database page is.
CORWAV_WEB="${HOME}/"



#=======================NO USER INPUT BELOW!===================================

#Add the global vars to .bashrc and .bash_profile
echo ""
echo "#Global variables for use with the CorWav package" >> ~/.bashrc
echo "export CORWAV=\"$CORWAV\"" >> ~/.bashrc
echo "export CORWAV_DATA=\"$CORWAV_DATA\"" >> ~/.bashrc
echo "export CORWAV_TRUNK=\"$CORWAV_TRUNK\"" >> ~/.bashrc
echo "export CORWAV_WEB=\"$CORWAV_WEB\"" >> ~/.bashrc
echo "export SSW_INSTR=\" \$SSW_INSTR IRIS HESSI ETHZ STEREO XRAY SWAP PFSS ONTOLOGY CHIANTI AIA MKIT NRH NORH\"" >> ~/.bashrc

echo ""
echo "#Global variables for use with the CorWav package" >> ~/.bash_profile
echo "export CORWAV=\"$CORWAV\"" >> ~/.bash_profile
echo "export CORWAV_DATA=\"$CORWAV_DATA\"" >> ~/.bash_profile
echo "export CORWAV_TRUNK=\"$CORWAV_TRUNK\"" >> ~/.bash_profile
echo "export CORWAV_WEB=\"$CORWAV_WEB\"" >> ~/.bash_profile
echo "export SSW_INSTR=\" \$SSW_INSTR IRIS HESSI ETHZ STEREO XRAY SWAP PFSS ONTOLOGY CHIANTI AIA MKIT NRH NORH\"" >> ~/.bash_profile

#Add the global vars to .cshrc
echo ""
echo "#Global variables for use with the CorWav package" >> ~/.cshrc.user
echo "setenv CORWAV $CORWAV" >> ~/.cshrc.user
echo "setenv CORWAV_DATA $CORWAV_DATA" >> ~/.cshrc.user
echo "setenv CORWAV_TRUNK $CORWAV_TRUNK" >> ~/.cshrc.user
echo "setenv CORWAV_WEB $CORWAV_WEB" >> ~/.cshrc.user
echo "setenv SSW_INSTR \" \$SSW_INSTR IRIS HESSI ETHZ STEREO XRAY SWAP PFSS ONTOLOGY CHIANTI AIA MKIT NRH NORH\"" >> ~/.cshrc.user

idl_path='!PATH'"=expand_path('+${CORWAV_TRUNK}',/all_dirs)+':'"+'!PATH'
echo $idl_path >> ~/.idl_startup
echo "" >> ~/.idl_startup
#
#eof
#
