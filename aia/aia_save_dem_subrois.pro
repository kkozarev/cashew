pro test_aia_save_dem_subrois
 ;You can run for one event, like this.
  one=1
  if one eq 1 then begin
     event=load_events_info(label='110511_01')
     aia_save_dem_subrois,event
  endif
  
  
;Alternatively, run for all events
  all=0
  if all eq 1 then begin
     events=load_events_info()
     for ev=0,n_elements(events)-1 do begin
        event=events[ev]
        aia_save_dem_subrois,event
     endfor
  endif
end


pro aia_save_dem_subrois,event, path=path, wav=wav
;PURPOSE:
;This procedure will load all the Aschwanden DEM results, select the
;regions used for the ionization and save the datacubes in separate
;.sav files in the same folder. The procedure is a newer, more general
;version of aia_aschdem_save_subrois. It allows for general shapes and
;orientations of the subregions.
;
;CATEGORY:
;AIA/DEM
;
;INPUTS:
;    event - an event structure
;KEYWORDS:
;
;OUTPUTS:
;
;DEPENDENCIES:
;
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 04/2014
;

if not keyword_set(path) then path=event.savepath
label=event.label
date=event.date
if not keyword_set(wav) then wav='193'
dempath=event.aschdempath
ionizpath=event.ionizationpath
ionfile=ionizpath+'rois_'+date+'_'+label+'.sav'
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


;roistartx=roi_subindex[0].roistart_x
;roistarty=roi_subindex[0].roistart_y
;roiendx=roi_subindex[0].roiend_x
;roiendy=roi_subindex[0].roiend_y

;nx=roiendx[0]-roistartx[0]+1
;ny=roiendy[0]-roistarty[0]+1
nregions=n_elements(roi_positions)
npix=roi_positions.npix
maxpix=max(npix)


;Find all of the files from the Aschwanden DEM run.
demfiles=file_search(dempath,'aschdem_'+date+'_'+label+'*_teem_map.sav')
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
;saving the subframes that correspond to the ionization/DEM regions
;after. The files are called
;demfiles=['AschDEM_20110511_021727_teem_map.sav','AschDEM_20110511_022603_teem_map.sav','AschDEM_20110511_023202_teem_map.sav']


;The data structure which will hold the information for DEM.

chidata=dblarr(nregions,ntimes,maxpix)
emdata=dblarr(nregions,ntimes,maxpix)
sigdata=dblarr(nregions,ntimes,maxpix)
tedata=dblarr(nregions,ntimes,maxpix)
times=strarr(ntimes)

for t=0,ntimes-1 do begin
   restore, demfiles[t]
   times[t]=dateobs
   for r=0,nregions-1 do begin
      arrind=roi_positions[r].posind[*,0:npix[r]-1]

      chidata[r,t,0:npix[r]-1]=reform(chi_map[arrind[0,*],arrind[1,*]])
      emdata[r,t,0:npix[r]-1]=reform(em_map[arrind[0,*],arrind[1,*]])
      sigdata[r,t,0:npix[r]-1]=reform(sig_map[arrind[0,*],arrind[1,*]])
      tedata[r,t,0:npix[r]-1]=reform(te_map[arrind[0,*],arrind[1,*]])
   endfor
endfor


save,filename=dempath+'dem_'+date+'_'+label+'_teem_map_subrois.sav',chidata,emdata,sigdata,tedata,times,waves,npix



end
