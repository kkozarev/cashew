pro save_weber_subrois
;This procedure will load the AIA data, select the
;regions used for the ionization and save the datacubes in separate
;.sav files in the same folder. This was written to prepare subrois
;for Mark Weber to analyze.

path='/Volumes/Backscratch/Users/kkozarev/AIA/events/'
event='37'
dempath=path+event+'/dem/aschwanden/'
ionizpath=path+event+'/ionization/'

waves=['094','131','171','193','211','335']
nwaves=n_elements(waves)

;I am taking data from three times - before, during, after the
;event(here shown as 193A times):
;2011-05-11T02:17:07.84
;2011-05-11T02:24:43.84
;2011-05-11T02:32:07.84
ntimes=3
tind=[0,37,74] ;indices for the right times

for w=0,nwaves-1 do begin
   for t=0,ntimes-1 do begin
      restore, ionizpath+'rois_'+event+'_'+waves[w]+'.sav'
      if w eq 0 and t eq 0 then begin
         roistartx=roi_subindex[0].roistart_x
         roistarty=roi_subindex[0].roistart_y
         roiendx=roi_subindex[0].roiend_x
         roiendy=roi_subindex[0].roiend_y
         
         nx=roiendx[0]-roistartx[0]+1
         ny=roiendy[0]-roistarty[0]+1
         nregions=n_elements(roistartx)
         
         ;The data structure which will hold the information for DEM.
         demdata=dblarr(nregions,ntimes,nwaves,nx,ny)
      endif
      for r=0,nregions-1 do begin
         demdata[r,t,w,*,*]=reform(roi_subdata[r,*,*,tind[t]])
      endfor
   endfor
endfor

save, filename='demdata_20110511_event_forMark.sav',ntimes,nregions,nx,ny,waves,nwaves,demdata


end
