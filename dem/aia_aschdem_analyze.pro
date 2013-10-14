pro batch_aia_aschdem_analyze

;The actual events and AIA channels which to measure...
  er=[6]

  evindex=['05','06','13','19','20','32','37','38']
  evdate=['20110125','20110127','20110211','20110307',$
          '20110308','20110427','20110511','20110529']
  begstep=[25,30,10,30,20,5,35,1] ;these are the initial steps for which to run the algorithm
  endstep=[100,100,100,85,90,100,110,110] ;these are the final steps for which to run the algorithm
  
  
  aia_aschdem_analyze,evindex[6],evdate[6],begstep[6],endstep[6]
  
end



pro aia_aschdem_analyze,events,evdate,begstep,endstep,basepath=basepath
;This procedure will analyze the results from the ASCHWANDEN DEM
;calculations for the May 11, 2011 event.
  if not keyword_set(basepath) then basepath='/Volumes/Backscratch/Users/kkozarev/AIA/studies/2011events/'
  
  for ii=0,n_elements(events)-1 do begin
     event='e'+events[ii]
     
     inpath=basepath+event+'/dem/aschwanden/'
     infile='AschDEM_teem_map_subrois.sav'

;Restore the DEM results
     restore, inpath+infile
     
     ntimes=endstep[ii]-begstep[ii]+1
     nregions=n_elements(emdata[*,0,0,0])
     nx=n_elements(emdata[0,0,*,0])
     ny=nx
     ntot=1.0*nx*ny
     
;Get the time values in JD
     tm=anytim2jd(times[begstep[ii]:endstep[ii]])
     tm=1.D*tm.int+tm.frac
     
     
;Separate the high and low Electron Temperature values
     te=fltarr(nregions,ntimes)
     teh=te
     tel=te
     ;This is the average value
     for r=0,nregions-1 do for t=begstep[ii],endstep[ii] do begin
        te[r,t-begstep[ii]]=total(tedata[r,t,*,*])/ntot
        ;tel[r,t-begstep[ii]]=min(tedata[r,t,*,*])
        ;teh[r,t-begstep[ii]]=max(tedata[r,t,*,*])
     endfor
     hi=where(te[2,*] gt 6.35)
     lo=where(te[2,*] lt 6.35)
     teh=te[*,hi]
     tel=te[*,lo]
     
     
;Separate the high and low Emission Measure values
     em=fltarr(nregions,ntimes)
     emh=em
     eml=em
     chi=fltarr(nregions,ntimes)
     sig=fltarr(nregions,ntimes)
     for r=0,nregions-1 do begin
        for t=begstep[ii],endstep[ii] do begin
           em[r,t-begstep[ii]]=total(emdata[r,t,*,*])/ntot
           ;eml[r,t-begstep[ii]]=min(emdata[r,t,*,*])
           ;emh[r,t-begstep[ii]]=max(emdata[r,t,*,*])
           chi[r,t-begstep[ii]]=sqrt(total(chidata[r,t,*,*]^2))
           sig[r,t-begstep[ii]]=sqrt(total(sigdata[r,t,*,*]^2))
        endfor
     endfor
     emh=em[*,hi]
     eml=em[*,lo]
   
;get the high and low times
     tmh=tm[hi]
     tml=tm[lo]
     
     
     dummy = LABEL_DATE(DATE_FORMAT=['%H:%I'])
     xtit='Time of '+evdate[ii]
     ytit='log(T)'
     set_plot,'ps'
     !P.font=0
     !P.position=[0.16,0.12,0.85,0.93]
     loadct,0,/silent
     
     xrange=[min(tm),max(tm)]
     for r=0,nregions-1 do begin


;Plot the original data
        rst=strtrim(string(r+1),2)
        fname=inpath+'aschdem_teem_normalized_series_r'+rst
        device,file=fname+'.eps',/inches,xsize=8.0,ysize=8.0,$
               /encaps,/color,/helvetica

        mnt=min(tel[r,*])
        mxt=max(teh[r,*])
        mne=min(eml[r,*])
        mxe=max(emh[r,*])
 
        tit='log(T!De!N) and EM(grey), R'+rst+', '+evdate[ii]
        plot,tm,te[r,*],$
             title=tit,$
             xtitle=xtit,ytitle=ytit,$
             xrange=xrange,yrange=[mnt,mxt],$
             /ynozero,thick=8,background=255,$
             color=0,xstyle=1,ystyle=8,$
             XTICKUNITS = ['Time'], $  
             XTICKFORMAT='LABEL_DATE',$
             xthick=4,ythick=4,ytickformat='(f6.3)',$
             charthick=2,charsize=1.6
        oploterror,tm,te[r,*],te[r,*]-tel[r,*],/lobar,thick=4
        oploterror,tm,te[r,*],teh[r,*]-te[r,*],/hibar,thick=4
        
        AXIS, YAXIS=1, YRANGE=[mne,mxe], /SAVE, color=100,$
              charthick=2,charsize=1.6,ythick=4,ytitle="EM"
        
        oplot,tm,em[r,*],color=100,thick=8
        oploterror,tm,em[r,*],em[r,*]-eml[r,*],/lobar,thick=4,color=100
        oploterror,tm,em[r,*],emh[r,*]-em[r,*],/hibar,thick=4,color=100

        device,/close
        exec='convert -flatten '+fname+'.eps '+fname+'.png ; rm -rf '+fname+'.eps '
        spawn,exec
     endfor


;Plots of the High values
     tmp=(tmh-tm[0])*1440.
     xrange=[min(tmh),max(tmh)]
     for r=0,nregions-1 do begin

        tmp=emh[r,*] ;*teh[r,0]/(emh[r,0])
        mint=min(teh[r,*]);/teh[r,0]
        mine=min(emh[r,*]);/emh[r,0]
        mn=min([mint,mine])
        
        maxt=max(teh[r,*]);/teh[r,0]
        maxe=max(emh[r,*]);/emh[r,0]
        
        rst=strtrim(string(r+1),2)
        fname=inpath+'aschdem_teem_normalized_series_high_r'+rst
        device,file=fname+'.eps',/inches,xsize=8.0,ysize=8.0,$
               /encaps,/color,/helvetica
;timind[0]:timind[1]
        tmp=emh[r,*] ;*teh[r,0]/(emh[r,0])
        mint=min(teh[r,*]);/teh[r,0]
        mine=min(emh[r,*]);/emh[r,0]
        mn=min([mint,mine])
        
        maxt=max(teh[r,*]);/teh[r,0]
        maxe=max(emh[r,*]);/emh[r,0]
        mx=max([maxt,maxe])
        
        tit='log(T!De!N) and EM(grey), R'+rst+', '+evdate[ii]
        plot,tmh,teh[r,*],$
             title=tit,$
             xtitle=xtit,ytitle=ytit,$
             xrange=xrange,yrange=[mint,maxt],$
             /ynozero,thick=8,background=255,$
             color=0,xstyle=1,ystyle=8,$
             XTICKUNITS = ['Time'], $  
             XTICKFORMAT='LABEL_DATE',$
             xthick=4,ythick=4,ytickformat='(f6.3)',$
             charthick=2,charsize=1.6
        
        AXIS, YAXIS=1, YRANGE=[mine,maxe], /SAVE, color=100,$
              charthick=2,charsize=1.6,ythick=4,ytitle="EM"
        oplot,tmh,emh[r,*],color=100,thick=8
        device,/close
        exec='convert -flatten '+fname+'.eps '+fname+'.png ; rm -rf '+fname+'.eps '
        spawn,exec
     endfor
     
     
     
;Plots of the Low values
;tmp=(tml-tm[0])*1440.
     xrange=[min(tml),max(tml)]
     for r=0,nregions-1 do begin
        tmp=emh[r,*]            ;*teh[r,0]/(emh[r,0])
        mint=min(teh[r,*]);/teh[r,0]
        mine=min(emh[r,*]);/emh[r,0]
        mn=min([mint,mine])
        
        maxt=max(teh[r,*]);/teh[r,0]
        maxe=max(emh[r,*]);/emh[r,0]
        
        rst=strtrim(string(r+1),2)
        fname=inpath+'aschdem_teem_normalized_series_low_r'+rst
        device,file=fname+'.eps',/inches,xsize=8.0,ysize=8.0,$
               /encaps,/color,/helvetica
        
        tmp=eml[r,*]            ;*teh[r,0]/(emh[r,0])
        mint=min(tel[r,*]);/teh[r,0]
        mine=min(eml[r,*]);/emh[r,0]
        mn=min([mint,mine])
        
        maxt=max(tel[r,*]);/teh[r,0]
        maxe=max(eml[r,*]);/emh[r,0]
        mx=max([maxt,maxe])
        
        tit='log(T!De!N) and EM(grey), R'+rst+', '+evdate[ii]
        plot,tml,tel[r,*],$
             title=tit,$
             xtitle=xtit,ytitle=ytit,$
             xrange=xrange,yrange=[mint,maxt],$
             /ynozero,thick=8,background=255,$
             color=0,xstyle=1,ystyle=8,$
             XTICKUNITS = ['Time'], $  
             XTICKFORMAT='LABEL_DATE',$
             xthick=4,ythick=4,ytickformat='(f6.3)',$
             charthick=2,charsize=1.6
        AXIS, YAXIS=1, YRANGE=[mine,maxe], /SAVE, color=100,$
              charthick=2,charsize=1.6,ythick=4,ytitle="EM"
        oplot,tml,eml[r,*],color=100,thick=8
        device,/close
        exec='convert -flatten '+fname+'.eps '+fname+'.png ; rm -rf '+fname+'.eps '
        spawn,exec
        
     endfor

  endfor  

set_plot,'x'     
     
end
