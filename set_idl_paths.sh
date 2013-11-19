#!/usr/bin/env bash
# sets up environmentals and paths 


CORWAV_TRUNK="${HOME}/svn/corwav/trunk/"
idl_path='!PATH'"=expand_path('+${CORWAV_TRUNK}',/all_dirs)+':'"+'!PATH'
echo $idl_path >> ~/.idl_startup
echo "" >> ~/.idl_startup
#
#eof
#
