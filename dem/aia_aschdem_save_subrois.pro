pro test_aia_aschdem_save_subrois
 ;You can run for one event, like this.
  one=1
  if one eq 1 then begin
     event=load_events_info(label='110511_01')
     aia_aschdem_save_subrois,event
  endif
  
  
;Alternatively, run for all events
  all=0
  if all eq 1 then begin
     events=load_events_info()
     for ev=0,n_elements(events)-1 do begin
        event=events[ev]
        aia_aschdem_save_subrois,event
     endfor
  endif
end


pro aia_aschdem_save_subrois,event, path=path, wav=wav
;PURPOSE:
;This procedure will load all the Aschwanden DEM results, select the
;regions used for the ionization and save the datacubes in separate
;.sav files in the same folder.
;
;CATEGORY:
;AIA/DEM
;
;INPUTS:
;
;KEYWORDS:
;
;OUTPUTS:
;
;DEPENDENCIES:
;
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 02/2013
;

if not keyword_set(path) then path=event.savepath
label=event.label
date=event.date
if not keyword_set(wav) then wav='193'
dempath=event.aschdempath
ionizpath=event.ionizationpath
ionfile=ionizpath+'rois_'+date+'_'+label+'_'+wav+'.sav'
if find_file(ionfile) eq '' then begin
   print,''
   print,'File '+ionfile+' does not exist. Quitting...'
   print,''
   return
endif

;Strings for the different wave bands
waves=['131','171','193','211','335','94']
nwaves=n_elements(waves)

restore, ionfile
roistartx=roi_subindex[0].roistart_x
roistarty=roi_subindex[0].roistart_y
roiendx=roi_subindex[0].roiend_x
roiendy=roi_subindex[0].roiend_y

nx=roiendx[0]-roistartx[0]+1
ny=roiendy[0]-roistarty[0]+1
nregions=n_elements(roistartx)

roi_subdata=0
roi_subindex=0


;Find all of the files from the Aschwanden DEM run.
demfiles=file_search(dempath,'*_map.sav')
if demfiles[0] eq '' then begin
   print,''
   print,'File '+ionfile+' does not exist. Quitting...'
   print,''
   return
endif
nfiles=n_elements(demfiles)
ntimes=nfiles

;For the Weber DEM calculations, I am taking data from three times - before, during, after the
;event(here shown as 193A times):
;2011-05-11T02:17:07.84
;2011-05-11T02:24:43.84
;2011-05-11T02:32:07.84
;For the Aschwanden calculations, I am just taking the files and
;saving the subframes that correspond to the ionization/DEM regions.
;after. The files are called
;demfiles=['AschDEM_20110511_021727_teem_map.sav','AschDEM_20110511_022603_teem_map.sav','AschDEM_20110511_023202_teem_map.sav']


;The data structure which will hold the information for DEM.
chidata=dblarr(nregions,ntimes,nx,ny)
emdata=dblarr(nregions,ntimes,nx,ny)
sigdata=dblarr(nregions,ntimes,nx,ny)
tedata=dblarr(nregions,ntimes,nx,ny)
times=strarr(ntimes)

for t=0,ntimes-1 do begin
   restore, demfiles[t]
   times[t]=dateobs
   for r=0,nregions-1 do begin
      chidata[r,t,*,*]=reform(chi_map[roistartx[r]:roiendx[r],roistarty[r]:roiendy[r]])
      emdata[r,t,*,*]=reform(em_map[roistartx[r]:roiendx[r],roistarty[r]:roiendy[r]])
      sigdata[r,t,*,*]=reform(sig_map[roistartx[r]:roiendx[r],roistarty[r]:roiendy[r]])
      tedata[r,t,*,*]=reform(te_map[roistartx[r]:roiendx[r],roistarty[r]:roiendy[r]])
   endfor
endfor


save,filename=dempath+'aschdem_'+date+'_'+label+'_teem_map_subrois.sav',chidata,emdata,sigdata,tedata,times,waves



end
