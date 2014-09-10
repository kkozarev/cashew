#!/bin/tcsh

setenv IDL_VER "71"

setenv SSW "/proj/DataCenter/ssw"                                                                                           
setenv SSWDB "/proj/DataCenter/sswdb"

if ( -d "/usr/local/etc/profile.d" ) then
  # re-source the profile.d environment for idl-only
  if ( -r "/usr/local/etc/profile.d/49-exelis-idl.csh" ) then
    source /usr/local/etc/profile.d/49-exelis-idl.csh
  endif
else
  # re-source the environment for idl for non-profile.d machines
  ssxg_setup_exelis_idl
endif

setenv SSW_INSTR    "GEN TRACE SXT CDS CHIANTI SUMER BINARIES SOT XRT EIS EIT MDI SOT AIA IRIS SECCHI PFSS HESSI ETHZ STEREO XRAY SWAP ONTOLOGY CHIANTI MKIT NRH NORH"

source $SSW/gen/setup/setup.ssw /quiet

if ( ! -d "/usr/local/etc/profile.d" ) then
   ssxg_setup_browser
else
   if ( -r "/usr/local/etc/profile.d/60-ssxg_setup_browser.csh" ) then
      source /usr/local/etc/profile.d/60-ssxg_setup_browser.csh
   endif
endif

setenv CORWAV /data/george/corwav/
setenv CORWAV_DATA /data/george/corwav/
setenv CORWAV_TRUNK /home/kkozarev/git/corwav/
setenv CORWAV_WEB /var/www/projects/cashew/public_html/
setenv SSW_INSTR " $SSW_INSTR IRIS HESSI ETHZ STEREO XRAY SWAP PFSS ONTOLOGY CHIANTI AIA MKIT NRH NORH"

#chgrp -R corwav /data/george/corwav
chmod -R g+w /data/george/corwav/*

cd $CORWAV_TRUNK/web
setenv IDL_STARTUP $CORWAV_TRUNK/web/driver.pro

sswidl
