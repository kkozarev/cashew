pro test_pfss_shock_update_crosspoints_tags
  labels=['110607_01','131212_01']
  for ev=0,n_elements(labels)-1 do begin
     label=labels[ev]
     event=load_events_info(label=label)
     pfss_shock_update_crosspoints_tags,event,/hires
     pfss_shock_update_crosspoints_tags,event,/lores
  endfor
end

pro pfss_shock_update_crosspoints_tags,event,lores=lores,hires=hires
;just a small program to open, add a couple of tags, and close the
;shock-crossing points structures. This should be done only once,
;though it shouldn't make a difference/overwrite stuff. I hope not.
  cashewtime=generate_struct('cashewtime')
  newcrossPt={time:cashewtime,rpx:0.0D,rpy:0.0D,rpz:0.0D,px:0.0D,py:0.0D,pz:0.0D,thbn:0.0,vshock:0.0,$
              linid:0L,bmag:0.0D,open:0,density:0.0D,temperature:0.0,shockjump:0.0}
  
  if keyword_set(hires) then begin
     csgsfile=event.mfhcpath+event.csgs.hires.map_savename
  endif else begin
     csgsfile=event.mfhcpath+event.csgs.lores.map_savename
  endelse
  
  if csgsfile eq '' or not file_exist(csgsfile) then begin
     print,'The CSGS file is not properly set or does not exist. Quitting.'
     return
  endif

  
  ;Read in the CSGS results file
  restore,csgsfile
  nmaxcrosses=max(allcrosses)
  newcrossPts=replicate(newcrossPt,nsteps,nmaxcrosses)

  
  for step=0,nsteps-1 do begin
     ncrosses=allcrosses[step]
     for cross=0,ncrosses-1 do begin
        newcrosspts[step,cross].rpx=crosspoints[step,cross].rpx
        newcrosspts[step,cross].rpy=crosspoints[step,cross].rpy
        newcrosspts[step,cross].rpz=crosspoints[step,cross].rpz
        newcrosspts[step,cross].px=crosspoints[step,cross].px
        newcrosspts[step,cross].py=crosspoints[step,cross].py
        newcrosspts[step,cross].pz=crosspoints[step,cross].pz
        newcrosspts[step,cross].thbn=crosspoints[step,cross].thbn
        newcrosspts[step,cross].linid=crosspoints[step,cross].linid
        newcrosspts[step,cross].bmag=crosspoints[step,cross].bmag
        newcrosspts[step,cross].open=crosspoints[step,cross].open
        newcrosspts[step,cross].shockjump=crosspoints[step,cross].shockjump
     endfor
  endfor
  crosspoints=newcrosspts
  
  save,filename=csgsfile,$
       ALLCROSSES,CARRLAT,CARRLON,CROSSPOINTS,DT,NSTEPS,NMAXCROSSES,$
       RADIUS,RADIUSFITLINES,ROTATIONANGLES,SC,SUBINDEX,SUNCENTER,$
       TIME,VERTEX_LIST,VERT_ROTMAT,VERT_TRANSMAT
  
  print,''
  print,'Done with event '+event.label
  print,''

end



