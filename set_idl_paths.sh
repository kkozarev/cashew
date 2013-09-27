#!/usr/bin/env bash
# sets up paths for CorWav

CORWAV_TRUNK="${HOME}/svn/corwav/trunk"
idl_path='!PATH'"=expand_path('+${CORWAV_TRUNK}',/all_dirs)+':'"+'!PATH'
#idl_path='!PATH'"=expand_path(\'+${CORWAV_TRUNK}\',/all_dirs)+\':\'"+'!PATH'
#idl_path="${idl_path}+:${CORWAV_TRUNK}/old"
#idl_path="${idl_path}+:${CORWAV_TRUNK}/batch"
#idl_path="${idl_path}+:${CORWAV_TRUNK}/radio"
#idl_path="${idl_path}+:${CORWAV_TRUNK}/examples"
#idl_path="${idl_path}+:${CORWAV_TRUNK}/studies"
#idl_path="${idl_path}+:${CORWAV_TRUNK}/tests"
#idl_path="${idl_path}+:${CORWAV_TRUNK}/temp"
#idl_path="${idl_path}+:${CORWAV_TRUNK}/dem"
#idl_path="${idl_path}+:${CORWAV_TRUNK}/external"
#idl_path="${idl_path}+:${CORWAV_TRUNK}/mhammer"
#idl_path="${idl_path}+:${CORWAV_TRUNK}/pfss_shock"
#idl_path="${idl_path}+:${CORWAV_TRUNK}/yaftawave"
#idl_path="${idl_path}+:${CORWAV_TRUNK}/pfss_shock"

echo $idl_path >> ~/.idl_startup
echo "" >> ~/.idl_startup
#
#eof
#
