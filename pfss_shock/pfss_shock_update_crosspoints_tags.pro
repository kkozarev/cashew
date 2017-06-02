pro pfss_shock_update_crosspoints_tags
;just a small program to open, add a couple of tags, and close the
;shock-crossing points structures. This should be done only once,
;though it shouldn't make a difference/overwrite stuff. I hope not.
  
  
  newcrossPt={rpx:0.0D,rpy:0.0D,rpz:0.0D,px:0.0D,py:0.0D,pz:0.0D,thbn:0.0D,$
              linid:0L,bmag:0.0D,open:0,density:0.0D,temperature:0.0D,shockjump:0.0D}
  
  
  events=load_events_info()
  for ev=0,n_elements(events)-1 do begin
     event=events[ev]
     if file_exist(event.pfsspath+event.csgs.lores.map_savename) then begin
        restore,event.pfsspath+event.csgs.lores.map_savename
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
           endfor
        endfor
        crosspoints=newcrosspts
        
        save,filename=event.pfsspath+event.csgs.lores.map_savename,$
             ALLCROSSES,CARRLAT,CARRLON,CROSSPOINTS,DT,NSTEPS,NMAXCROSSES,$
             RADIUS,RADIUSFITLINES,ROTATIONANGLES,SC,SUBINDEX,SUNCENTER,$
             TIME,VERTEX_LIST,VERT_ROTMAT,VERT_TRANSMAT
     endif
     
     ;The same for the high resolution data
     if file_exist(event.pfsspath+event.csgs.hires.map_savename) then begin
        restore,event.pfsspath+event.csgs.hires.map_savename
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
           endfor
        endfor
        crosspoints=newcrosspts
        
        save,filename=event.pfsspath+event.csgs.hires.map_savename,$
             ALLCROSSES,CARRLAT,CARRLON,CROSSPOINTS,DT,NSTEPS,NMAXCROSSES,$
             RADIUS,RADIUSFITLINES,ROTATIONANGLES,SC,SUBINDEX,SUNCENTER,$
             TIME,VERTEX_LIST,VERT_ROTMAT,VERT_TRANSMAT
     endif
     print,''
     print,'Done with event '+event.label
     print,''
  endfor
end
  
  
  
