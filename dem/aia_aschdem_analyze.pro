pro test_aia_dem_analyze

;The actual events and AIA channels which to measure...
  ;begstep=[25,30,10,30,20,5,35,1] ;these are the initial steps for which to run the algorithm
  ;endstep=[100,100,100,85,90,100,110,110] ;these are the final steps for which to run the algorithm
  trange='2011-05-11T'+['02:15:00','02:31:00']
  event=load_events_info(label='110511_01')
  aia_dem_analyze,event,trange=trange
end



pro aia_dem_analyze,event,trange=trange,savepath=savepath
;This procedure will analyze the results from the ASCHWANDEN DEM
;calculations.
  if not keyword_set(savepath) then savepath=event.savepath
  evlabel=event.label
  evdate=event.date
  
  inpath=event.aschdempath
  infile='dem_'+evdate+'_'+evlabel+'_teem_map_subrois.sav'
  
;Restore the DEM results
  restore, inpath+infile
  ntimes=n_elements(times)
  nregions=n_elements(emdata[*,0,0])
  
  
;Get the time values in JD
  tm=anytim2jd(times)
  tm=1.D*tm.int+tm.frac
  if not keyword_set(trange) then trang=[0,ntimes-1] else begin    
     trjd=anytim2jd(trange)
     trjd=1.D*trjd.int+trjd.frac
     tr0ind=min(where(tm ge trjd[0]))
     if tr0ind eq -1 then tr0ind=0
     tr1ind=min(where(tm ge trjd[1]))
     if tr1ind eq -1 then tr1ind=ntimes-1
                                ;trang=[times[tr0ind],times[tr1ind]]
     trang=[tr0ind,tr1ind]
  endelse
  begstep=trang[0]
  endstep=trang[1]
  tm=tm[begstep:endstep]
  tottimes=ntimes
  ntimes=endstep-begstep+1
  
  
te=fltarr(nregions,ntimes)
  ;This is the average value
  for r=0,nregions-1 do begin
     ntot=npix[r]
     for t=begstep,endstep do begin
        te[r,t-begstep]=total(tedata[r,t,*])/ntot
     endfor
  endfor
  
  
;Obtain the ChiSq, EM, and Sigma values
  em=fltarr(nregions,ntimes)
  chi=fltarr(nregions,ntimes)
  sig=fltarr(nregions,ntimes)
  stderr=fltarr(nregions,ntimes)
  for r=0,nregions-1 do begin
     ntot=npix[r]
     for t=begstep,endstep do begin
        em[r,t-begstep]=total(emdata[r,t,*])/ntot
;           chi[r,t-begstep]=sqrt(total(chidata[r,t,*,*]^2))
        chi[r,t-begstep]=total(chidata[r,t,*])/ntot
        sig[r,t-begstep]=sqrt(total(sigdata[r,t,*]^2)/(ntot-1))
        stderr[r,t-begstep]=sig[r,t-begstep]/sqrt(ntot)
     endfor
  endfor
  
  
  
  dummy = LABEL_DATE(DATE_FORMAT=['%H:%I'])
  set_plot,'ps'
  !P.font=0
  !P.position=[0.18,0.12,0.83,0.93]
  loadct,0,/silent
  
  xrange=[min(tm),max(tm)]
                                ;xrange=[minmax(tm[begstep:endstep])]
                                ;mnt=min(te)
                                ;mxt=max(te)
                                ;mine=min(em)
                                ;maxe=max(em)
  
  
  for r=0,nregions-1 do begin
     ntot=npix[r]
     mnt=min(te[r,*])
     mxt=max(te[r,*],mxt_ind)
     mine=min(em[r,*])
     maxe=max(em[r,*],maxe_ind)
     
                                ;Find the background level and maximum
                                ;of the emission measure, get
                                ; density compression from them.
     rng=10
     embckg=avg(em[r,0:rng-1])
     denscompress=10^(maxe-embckg)
       ;Find the background level and maximum
       ;of the peak temperature, get
       ; their difference
     tebckg=avg(te[r,0:rng-1])
     tdiff=10^mxt-10^tebckg
     
     
;Plot the Emission Measure associated with the temperature maximum
     rst=strtrim(string(r+1),2)
     if rst lt 10 then rst='0'+rst
     fname=inpath+'aschdem_'+evdate+'_'+evlabel+'_teem_normalized_series_r'+rst
     device,file=fname+'.eps',/inches,xsize=8.0,ysize=8.0,$
               /encaps,/color,/helvetica
     
     ;tit='log(T!DMAX!N)(black) and log(EM!DMAX!N)(grey), R'+rst ;+', '+event.label
     tit='log(EM), R'+rst       ;+', '+event.label
     xtit='Time of '+evdate
     ytit="log(EM)"
     plot,tm,em[r,*],$
          title=tit,xticks=3,$
          xtitle=xtit,ytitle=ytit,$
          xrange=xrange,yrange=[mine,maxe],$
          /ynozero,thick=8,background=255,$
          color=0,xstyle=1,ystyle=8,$
          XTICKUNITS = ['Time'], $  
          XTICKFORMAT='LABEL_DATE',$
          xthick=4,ythick=4,ytickformat='(f6.2)',$
          charthick=2,charsize=1.8,xminor=4,/nodata
     
     oplot,tm,em[r,*],color=0,thick=8
     oplot,[tm[0],tm[maxe_ind]],[embckg,embckg],color=0,linestyle=2,thick=8
     oplot,[tm[maxe_ind],tm[maxe_ind]],[embckg-(maxe-embckg)*0.1,embckg+(maxe-embckg)*0.1],$
           color=0,thick=12
     xyouts,!p.position[0]+0.02,!p.position[3]-0.06,$
            'EM!DW!N/EM!D0!N='+strtrim(string(denscompress,format='(f6.2)'),2),$
            color=0,charthick=2.4,charsize=2.2,/norm
templot=1
if templot gt 0 then begin     
;Plot the Temperature time series
        AXIS, YAXIS=1,YRANGE=[mnt,mxt], /SAVE, color=140,$
              charthick=2,charsize=1.8,ythick=4,ytitle='log(T!DMAX!N)'
        
        
        oplot,tm,te[r,*],thick=8,color=140
        oploterror,tm,te[r,*],stderr[r,*],thick=4,/nohat,color=140
        oplot,[tm[0],tm[mxt_ind]],[tebckg,tebckg],color=140,linestyle=2,thick=8
        oplot,[tm[mxt_ind],tm[mxt_ind]],[tebckg-(mxt-tebckg)*0.1,tebckg+(mxt-tebckg)*0.1],$
              color=140,thick=12
        xyouts,!p.position[0]+0.02,!p.position[3]-0.11,$
               'T!Dmax,W!N-T!Dmax,0!N='+strtrim(string(tdiff,format='(e8.2)'),2)+' K',$
               color=140,charthick=2.4,charsize=2.2,/norm
endif        

        device,/close
        exec='convert -flatten '+fname+'.eps '+fname+'.png ; rm -rf '+fname+'.eps '
        spawn,exec
        
        
                                ;Save the information to a file
        temp=reform(te[r,*])
        emiss=reform(em[r,*])
        save,filename=fname+'.sav',denscompress,mnt,mxt,mine,maxe,embckg,tdiff,tm,tebckg,temp,emiss
        
     endfor
  
  set_plot,'x'     
  
end
