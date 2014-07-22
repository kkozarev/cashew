pro test_pfss_shock_plot_thetabn_stats
;Testing the shock crossing angles plotting procedure
event=load_events_info(label='110511_01')

pfss_shock_plot_thetabn_stats,event
end

pro pfss_shock_plot_thetabn_stats,event
;PURPOSE:
; This procedure will plot a time series of different ranges of
; thetaBN for various shock-field angle points.
;
;CATEGORY:
;PFSS_Shock
;
;INPUTS:
;       event - an event structure
;
;KEYWORDS:
;  oplot
;
;OUTPUTS:
;
; 
;DEPENDENCIES:
;
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 02/22/2014
;
  set_plot,'x'
  close,/all
  savepath=event.pfsspath
  infile=find_latest_file(event.pfsspath+'csgs_results_*')
  if infile[0] eq '' then begin
     print,'The file to load is not properly set or does not exist. Quitting.'
     return
  endif
  print,''
  print,'Loading file '+infile
  print,''
  restore,infile
  
;DEBUG
;  nlins=n_elements(pfsslines)
;  tmpind=indgen(fix(nlins/10))*10
;  pfsslines[tmpind].open=1
;DEBUG
  

  ;Get the times right
  strtime=subindex[*].date_obs
  tm=anytim2jd(strtime)
  tm=1.D*tm.int+tm.frac
  ntimes=n_elements(tm)
  RSUN=subindex[0].rsun_ref/1000. ;Solar radius in km.
  
  xrng=[min(tm),max(tm)]
  yrng=[0,max(allcrosses)*1.001]
  
  ;th=crossPoints.thbn
  ;minth=min(th[where(th gt 0.0 and finite(th))])
  ;maxth=max(th[where(th gt 0.0 and finite(th))])
  
  ;The angle value ranges
  ;rangevals=[15.0,30.0,45.0,60.0,75.0,90.0]
  rangevals=[30,50,70,90]
  nranges=n_elements(rangevals)
  valarr=lonarr(ntimes,nranges)
  valranges=lonarr(ntimes,nranges,2)
  linekinds=intarr(ntimes,nranges,2)
  for tt=0,ntimes-1 do begin
     ;First take care of the first range
     res=where(crossPoints[tt,*].thbn gt 0.0 and crossPoints[tt,*].thbn le rangevals[0])
     if res[0] eq -1 then valarr[tt,0]=0 else valarr[tt,0]=n_elements(res)
     valranges[tt,0,0]=10
     valranges[tt,0,1]=valarr[tt,0]

     ;Find the number of open and closed field lines for every angle range
     if res[0] gt -1 then begin
        tmpind=crosspoints[tt,res].linid
        openinds=reform(pfsslines[tmpind].open)
        tmp=where(openinds eq 1)
        if tmp[0] eq -1 then linekinds[tt,0,0]=0 else linekinds[tt,0,0]=n_elements(tmp)
        linekinds[tt,0,1]=n_elements(openinds) - linekinds[tt,0,0]
     endif
     
     for rr=1,nranges-1 do begin
        res=where(crossPoints[tt,*].thbn gt rangevals[rr-1] and crossPoints[tt,*].thbn le rangevals[rr])
        if res[0] eq -1 then valarr[tt,rr]=0 else valarr[tt,rr]=n_elements(res)
        valranges[tt,rr,0]=valranges[tt,rr-1,1]
        valranges[tt,rr,1]=valranges[tt,rr,0]+valarr[tt,rr]
        
     ;Find the number of open and closed field lines for every angle range
     if res[0] gt -1 then begin
        tmpind=crosspoints[tt,res].linid
        openinds=reform(pfsslines[tmpind].open)
        tmp=where(openinds eq 1)
        if tmp[0] eq -1 then linekinds[tt,rr,0]=0 else linekinds[tt,rr,0]=n_elements(tmp)
        linekinds[tt,rr,1]=n_elements(openinds) - linekinds[tt,rr,0]
     endif
     endfor
  endfor


;+======================================================================
  cthick=6
  chsize=2.2
  dummy = LABEL_DATE(DATE_FORMAT=['%H:%I'])
  xtit='Time of '+event.date
  ytit='# Shock-Interacting Fieldlines'
  ;thlet='!4' + String("150B) + '!X'
  thlet='!9'+String("161B)+'!X'
  
  !P.position=[0.18,0.14,0.86,0.92]
  !P.charthick=4
  !P.font=0
  ;wdef,0,1000,800
  set_plot,'ps'
  fname=savepath+'thetabn_stats_'+event.date+'_'+event.label+'_time_crossings'
  device,file=fname+'.eps',/inches,xsize=10.0,ysize=8.0,$
         /encaps,/color,/helvetica
  loadct,0,/silent
  PLOT, tm, allcrosses, PSYM = 10, $ 
        TITLE = thlet+'!DBN!N Evolution', $
        XTITLE =xtit, $
        YTITLE = ytit, $
        XTICKUNITS = ['Time'],XTICKFORMAT='LABEL_DATE',$
        xticklen=-0.01,$
        xrange=xrng,yrange=yrng,xticks=4, $
        xstyle=1,ystyle=1,color=0,background=255,$
        xthick=4,ythick=4,thick=4,charsize=chsize,charthick=chthick,/nodata

  loadct,13,/silent
  cols=findgen(nranges)*255./(nranges*1.0)
  rvst=strtrim(string(rangevals,format='(i3)'),2)
  for rr=0,nranges-1 do begin
     bar_plot,valranges[*,rr,1],background=255,colors=fltarr(ntimes)+cols[rr],$
              baselines=valranges[*,rr,0],/overplot
     if rr eq 0 then sttr=' 0!Uo!N<'+thlet+'!DBN!N<'+rvst[0]+'!Uo!N' $
     else sttr=rvst[rr-1]+'!Uo!N'+'<'+thlet+'!DBN!N<'+rvst[rr]+'!Uo!N'
     xyouts,!p.position[0]+0.02,!p.position[3]-0.05-0.05*rr,sttr,charsize=chsize,$
            charthick=chthick,color=cols[rr],/norm
  endfor
  
;Plot the percentage of open field lines for the last two angle intervals
;DEBUG
  axis,ystyle=1,yaxis=1,ythick=4,yrange=[0,100],/save,$
       ytitle='% Open Crossing Lines in Range',charsize=chsize,charthick=chthick
  for rr=0,nranges-1 do begin
     totranglin=linekinds[*,rr,0]+linekinds[*,rr,1]
     open_percent=(linekinds[*,rr,0]/(1.*totranglin))*100.
     if cols[rr] eq 0 then bckcol=255 else bckcol=0
     oplot,tm,open_percent,thick=9,color=bckcol
     oplot,tm,open_percent,thick=8,color=cols[rr]
  endfor
;DEBUG
  
  ;tvlct,rr,gg,bb,/get
  ;image=tvrd(true=1)
  device,/close
  exec='convert -flatten '+fname+'.eps '+fname+'.png ; rm -rf '+fname+'.eps '
  spawn,exec
  ;write_png,fname,image,rr,gg,bb
  set_plot,'x'
;-======================================================================



  
;+======================================================================
  cthick=6
  chsize=2.2
  dummy = LABEL_DATE(DATE_FORMAT=['%H:%I'])
  xtit='Time of '+event.date
  ytit='# Shock-Interacting Fieldlines'
  ;thlet='!4' + String("150B) + '!X'
  thlet='!9'+String("161B)+'!X'
  
  !P.position=[0.16,0.14,0.94,0.92]
  !P.charthick=4
  !P.font=0
  ;wdef,0,1000,800
  set_plot,'ps'
  fname=savepath+'thetabn_stats_'+event.date+'_'+event.label+'_time'
  device,file=fname+'.eps',/inches,xsize=10.0,ysize=9.0,$
         /encaps,/color,/helvetica
  loadct,0,/silent
  PLOT, tm, allcrosses, PSYM = 10, $ 
        TITLE = thlet+'!DBN!N Evolution', $
        XTITLE =xtit, $
        YTITLE = ytit, $
        XTICKUNITS = ['Time'],XTICKFORMAT='LABEL_DATE',$
        xticklen=-0.01,$
        xrange=xrng,yrange=yrng,xticks=4, $
        xstyle=1,ystyle=1,color=0,background=255,$
        xthick=4,ythick=4,thick=4,charsize=chsize,charthick=chthick,/nodata

  loadct,13,/silent
  cols=findgen(nranges)*255./(nranges*1.0)
  rvst=strtrim(string(rangevals,format='(i3)'),2)
  for rr=0,nranges-1 do begin
     bar_plot,valranges[*,rr,1],background=255,colors=fltarr(ntimes)+cols[rr],$
              baselines=valranges[*,rr,0],/overplot
     if rr eq 0 then sttr=' 0!Uo!N<'+thlet+'!DBN!N<'+rvst[0]+'!Uo!N' $
     else sttr=rvst[rr-1]+'!Uo!N'+'<'+thlet+'!DBN!N<'+rvst[rr]+'!Uo!N'
     xyouts,!p.position[0]+0.02,!p.position[3]-0.05-0.05*rr,sttr,charsize=chsize,$
            charthick=chthick,color=cols[rr],/norm
  endfor
  
  ;tvlct,rr,gg,bb,/get
  ;image=tvrd(true=1)
  device,/close
  exec='convert -flatten '+fname+'.eps '+fname+'.png ; rm -rf '+fname+'.eps '
  spawn,exec
  ;write_png,fname,image,rr,gg,bb
  set_plot,'x'
;-======================================================================
  

;+======================================================================
  xtit='Shock Nose Distance, R!Ds!N'
  rad=radiusfitlines/rsun*event.geomcorfactor+1
  xrng=[min(rad),max(rad)]
  ;wdef,0,1000,800
  set_plot,'ps'
  fname=savepath+'thetabn_stats_'+event.date+'_'+event.label+'_distance'
  device,file=fname+'.eps',/inches,xsize=10.0,ysize=8.0,$
         /encaps,/color,/helvetica
  loadct,0,/silent
  PLOT, rad, allcrosses, PSYM = 10, $ 
        TITLE = thlet+'!DBN!N Evolution', $
        XTITLE =xtit, $
        YTITLE = ytit, $
        xticklen=-0.01,xtickformat='(f4.2)',$
        xrange=xrng,yrange=yrng,xticks=4, $
        xstyle=1,ystyle=1,color=0,background=255,$
        xthick=3,ythick=3,thick=4,charsize=chsize,charthick=chthick,/nodata
  
  loadct,13,/silent
  cols=findgen(nranges)*255./(nranges*1.0)
  rvst=strtrim(string(rangevals,format='(i3)'),2)
  for rr=0,nranges-1 do begin
     bar_plot,valranges[*,rr,1],background=255,colors=fltarr(ntimes)+cols[rr],$
              baselines=valranges[*,rr,0],/overplot
     if rr eq 0 then sttr=' 0!Uo!N<'+thlet+'!DBN!N<'+rvst[0]+'!Uo!N' $
     else sttr=rvst[rr-1]+'!Uo!N'+'<'+thlet+'!DBN!N<'+rvst[rr]+'!Uo!N'
     xyouts,!p.position[0]+0.02,!p.position[3]-0.05-0.05*rr,sttr,charsize=chsize,$
            charthick=chthick,color=cols[rr],/norm
  endfor
  
  
  
  ;tvlct,rr,gg,bb,/get
  ;image=tvrd(true=1)
  device,/close
  exec='convert -flatten '+fname+'.eps '+fname+'.png ; rm -rf '+fname+'.eps '
  spawn,exec
  ;write_png,fname,image,rr,gg,bb
  set_plot,'x'
;-======================================================================

end
