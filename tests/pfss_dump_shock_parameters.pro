pro test_pfss_dump_shock_parameters
  labels=['140708_01','131212_01','130517_01','130423_01','120915_01','120526_01',$
          '120424_01','110607_01','110211_02','110125_01']
  for ev=0,n_elements(labels)-1 do begin
     label=labels[ev]
     event=load_events_info(label=label)
     pfss_dump_shock_parameters,event
  endfora
end

pro pfss_dump_shock_parameters,event
;This is a quick and dirty program (for now) to extract shock information for specific field lines 
;Need Vshock, ThetaBN, density jump,time,line index
close,/all

;Definitions
  wav='193'
  evnum=event.label
  label=event.label
  sts=event.st
  std=event.et
  date=event.date
  eventname='AIA_'+date+'_'+evnum+'_'+wav
  savepath=event.pfsspath
  datapath=savepath
  pfsspath=event.pfsspath



;Load the data files

  ;Load the annulusplot analysis file
  wavefile=event.annuluspath+'annplot_'+date+'_'+label+'_'+wav+'_analyzed_radial.sav'
  print, wavefile
  if not file_exist(wavefile) then begin
     print,'The annplot file is not properly set or does not exist. Quitting.'
     return
  endif
  ;Find a file to load with the latest results of applying the CSGS model
  csgsfile=find_latest_file(event.pfsspath+'csgs_results_*') 
  if csgsfile eq '' then begin
     print,'The CSGS file is not properly set or does not exist. Quitting.'
     return
  endif
  
  
  print,''
  print,'Loading data...'

  ;Load the Annulusplot analysis results
  print, 'Loading shock info file '+wavefile
  restore,wavefile
  
  ;Load the CSGS model results
  print ,'Loading CSGS File '+csgsfile
  restore,csgsfile
  
  ;Load regions 6,3,7,8 of the DEM results, get the density compression
  ;regions=[6,3,7,8]
  ;regions=strtrim(string(regions),2)
  regfiles=file_basename(find_files('aschdem_'+event.date+'_'+event.label+'_'+'teem_normalized_series_r*.sav',event.aschdempath))
  if regfiles[0] eq '' then begin
     print,'No DEM files of type '+'aschdem_'+event.date+'_'+event.label+'_'+'teem_normalized_series_r*.sav are present. Quitting...'
     return
  endif
  nregions=n_elements(regfiles)
  regions=strarr(nregions)
  for rr=0,nregions-1 do $
     regions[rr]=strmid(regfiles[rr],strpos(regfiles[rr],'.')-2,2)
  denscomp=fltarr(nregions)
  
  for rr=0,nregions-1 do begin
     reg=regions[rr]
     restore,event.aschdempath+'aschdem_'+event.date+'_'+event.label+'_'+'teem_normalized_series_r'+reg+'.sav'
     denscomp[rr]=denscompress
  endfor


  
  ;Compute the instantaneous shock speed
  vshock=deriv(time,radiusfitlines)
  
  ;Make an array of the field line indices and where they occur in crossPoints
  ;The point is to order the parameters by field line first, then by time.
  linidhistogram=histogram(crossPoints.linid)
  tmp2=where(linidhistogram gt 0)
  tmp2=tmp2[where(tmp2 gt 0)]
;This variable holds the indices of the crossing lines in crossPoints
  crLinInd=intarr(n_elements(tmp2),nsteps)
  
;Write the file with information ordered by field line index
  savname=savepath+'line_shock_info.txt'
  openw,lun,savname,/get_lun
  printf,lun,'#Shock crossing magnetic field lines for event '+event.label
  printf,lun,'#These are results from running the PFSS-CSGS models on AIA data.'
  printf,lun,'# LineId    Time[s]   Vshock[km/s]   ThetaBN[deg]   n1/n2    |B|[G]'
  for mm=0,n_elements(tmp2)-1 do begin
     for tt=0,nsteps-1 do begin
        dind=tt/ceil(nsteps*1./nregions)
        ncrosses=allcrosses[tt]
        lind=tmp2[mm]
        ff=where(crossPoints[tt,*].linid eq lind)
        if ff[0] eq -1 then continue
        ff=ff[0]
        crLinInd[mm,tt]=ff
        line='   '
        line+=strtrim(string(lind,format='(i7)'),2)+'       '
        line+=strtrim(string(time[tt],format='(f7.2)'),2)+'       '
        line+=strtrim(string(vshock[tt],format='(f7.2)'),2)+'       '
        line+=strtrim(string(crosspoints[tt,ff].thbn,format='(f7.2)'),2)+'       '
        line+=strtrim(string(denscomp[dind],format='(f7.3)'),2)+'       '
        line+=strtrim(string(crosspoints[tt,ff].bmag,format='(f7.3)'),2)
        printf,lun,line
     endfor
  endfor
  close,/all

  
;Write the file
;THIS VERSION SAVES THE INFORMATION TIME STEP BY TIME STEP.
;  savname=savepath+'line_shock_info2.txt'
;  openw,lun,savname,/get_lun
;  printf,lun,'#This file contains information about shock crossing magnetic field lines.'
;  printf,lun,'#  Time[s]   LineId   Vshock[km/s]   ThetaBN[deg]   n1/n2   |B|[G]'
;  ;HERE GOES THE LOOPS OVER THE TIME STEPS AND FIELD LINES.
;  ;Start simple - give them just one field line first.
;  for tt=0,nsteps-1 do begin
;     ;This index is for the shock density jump, based on how many regions we
;     ;use in the radial direction
;     dind=tt/ceil(nsteps*1./nregions)
;     ncrosses=allcrosses[tt]
;     pind=reform(crossPoints[tt,0:ncrosses-1].linid)
;     stop
;     for ff=0,ncrosses-1 do begin
;       line=''
;        npt=pfssLines[pind[ff]].npts
;        line+=strtrim(string(time[tt],format='(f7.2)'),2)+'       '
;        line+=strtrim(string(pind[ff],format='(i7)'),2)+'       '
;        line+=strtrim(string(vshock[tt],format='(f7.2)'),2)+'       '
;        line+=strtrim(string(crosspoints[tt,ff].thbn,format='(f7.2)'),2)+'       '
;        line+=strtrim(string(denscomp[dind],format='(f7.3)'),2)+'       '
;        line+=strtrim(string(crosspoints[tt,ff].bmag,format='(f7.3)'),2)
;        printf,lun,line
;     endfor
;  endfor
;  close,/all

end
