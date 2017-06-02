pro pfss_restore, restfile, refresh=refresh,loud=loud, bfield_struct=bfield_struct
;+
;   Name: pfss_restore
;
;   Purpose: routine to restore input pfss 'save' file and update pfss common
;
;   Input Parameters:
;      restfile - the pfss file to restore - (.sav or .h5)
;
;   Output Parameters:
;      NONE - (all output via common block)
;
;   Keyword Parameters:
;      refresh - if set, force restore even if same as last restored file 
;      bfield_struct - optional output HDF5 pfss structure
;      loud - if set, more details
;
;   Side Effects:
;      Updates pfss common block: pfss_data_block
;
;   Common Blocks:

;      YES: pfss_data_block - common used by pfss_viewer et al
;           pfss_restore_blk - restore file from last call
;
;   History:
;      5-Dec-2004 - S.L.Freeland
;     26-Jan-2004 - S.L.Freeland - add /RELAX to 
;     25-sep-2006 - S.L.Freeland - use pfss_data_block.pro for common define
;     11-oct-2012 - S.L.Freeland - allow pfss Version 2 / HDF5 format
;-

common pfss_restore_blk, lastrestfile

@pfss_data_block

loud=keyword_set(loud)

if not data_chk(restfile,/string) then begin 
   box_message,'Need an input pfss (idl) save file name...., bailing'
   return
endif 
loud=keyword_set(loud)

if n_elements(lastrestfile) eq 0 then lastrestfile=''  ; init local common 
restoreit=keyword_set(refresh) or restfile(0) ne lastrestfile

if not file_exist(restfile(0)) and restoreit then begin 
   if strpos(restfile(0),'http') eq 0 then begin 
      break_url,restfile(0),IP,subdir,file
      outdir=get_temp_dir()
      locfile=concat_dir(outdir,file)
      if not file_exist(locfile) then begin
         box_message,'Retrieving pfss Bfield file via http...'
         sock_copy,restfile(0),out_dir=outdir,use_network=1, $ 
            prog=is_member(!d.name,'win,x,mac',/wc,/ignore_case)
 
         restfile=locfile         ; url -> local name
      endif else begin
         box_message,'File> ' + locfile +' already local...'
         restfile=locfile
      endelse
   endif
   if not file_exist(restfile(0)) then begin 
      box_message,'Cannot find restore file> ' + restfile(0) + ' ,bailing..'
      return
   endif
endif

if restoreit then begin 
   box_message,['Restoring file>> ',restfile,'...Please be patient']
   extension=(ssw_strsplit(restfile(0),'.',/tail))(0)
   case extension of
      'sav': restore, file=restfile, /relax 
      'h5': begin
         bfield_struct=h5_parse(restfile)
         if not tag_exist(bfield_struct,'ssw_pfss_extrapolation') then begin 
            box_message,'Unexpected HDF5 output, bailing...'
            return ; !!! Early Exit 
         endif
         help,out=out
         sscom=where(strpos(out,'(PFSS_DATA_BLOCK)') ne -1,ccnt) ; allow evolution
         cvars=ssw_strsplit(out(sscom),' ',/head)
         for v=0,n_elements(cvars) -1 do begin 
            ;vst=gt_tagval(bfield_struct.ssw_pfss_extrapolation._data,cvars(v),missing=-1, found=found)
            css=(tag_index(bfield_struct.ssw_pfss_extrapolation._data,cvars(v)))(0)
            if loud then  print,cvars(v) + ' >> ' + (['special case or not',''])(css ne -1) + ' found' 
            if css(0) ne -1 then begin 
               estat=execute(cvars(v)+'=bfield_struct.ssw_pfss_extrapolation._data.(css)')
            endif 
         endfor
         ; special cases not covered with above tag->var loop
         now=bfield_struct.ssw_pfss_extrapolation._data.model_date
         if required_tags(bfield_struct.ssw_pfss_extrapolation._data,'phiat_re,phiat_im,phibt_re,phibt_im') then begin 
            phiat=dcomplex(bfield_struct.ssw_pfss_extrapolation._data.phiat_re,bfield_struct.ssw_pfss_extrapolation._data.phiat_im)
            phibt=dcomplex(bfield_struct.ssw_pfss_extrapolation._data.phibt_re,bfield_struct.ssw_pfss_extrapolation._data.phibt_im)
         endif
      endcase
      else: box_message,'Unknown file extension>> .'+extension
   endcase
   lastrestfile=restfile(0)
   box_message,'Done with restore...'
endif else if restfile(0) eq lastrestfile and loud then $
   box_message,['Same file as last time so not re-restored',$
               'Use /REFRESH to force re-restore'] 

return
end
