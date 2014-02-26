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
  for tt=0,ntimes-1 do begin
     ;First take care of the first range
     res=where(crossPoints[tt,*].thbn gt 0.0 and crossPoints[tt,*].thbn le rangevals[0])
     if res[0] eq -1 then valarr[tt,0]=0 else valarr[tt,0]=n_elements(res)
     valranges[tt,0,0]=10
     valranges[tt,0,1]=valarr[tt,0]
     
     for rr=1,nranges-1 do begin
        res=where(crossPoints[tt,*].thbn gt rangevals[rr-1] and crossPoints[tt,*].thbn le rangevals[rr])
        if res[0] eq -1 then valarr[tt,rr]=0 else valarr[tt,rr]=n_elements(res)
        valranges[tt,rr,0]=valranges[tt,rr-1,1]
        valranges[tt,rr,1]=valranges[tt,rr,0]+valarr[tt,rr]
     endfor
  endfor

  
;+======================================================================
  cthick=2.8
  dummy = LABEL_DATE(DATE_FORMAT=['%H:%I'])
  xtit='Time of '+event.date
  ytit='# Shock-Interacting Fieldlines'
  thlet='!4' + String("110B) + '!X'
  !P.position=[0.16,0.14,0.79,0.92]
  !P.charthick=2
  !P.font=-1
  wdef,0,1000,800
  loadct,0,/silent
  PLOT, tm, allcrosses, PSYM = 10, $ 
        TITLE = thlet+'!DBN!N Evolution', $
        XTITLE =xtit, $
        YTITLE = ytit, $
        XTICKUNITS = ['Time'],XTICKFORMAT='LABEL_DATE',$
        xticklen=-0.01,$
        xrange=xrng,yrange=yrng,xticks=4, $
        xstyle=1,ystyle=1,color=0,background=255,$
        xthick=3,ythick=3,thick=3,charsize=3,charthick=cthick,/nodata
  
  loadct,13,/silent
  cols=findgen(nranges)*255./(nranges*1.0)
  rvst=strtrim(string(rangevals,format='(i3)'),2)
  for rr=0,nranges-1 do begin
     bar_plot,valranges[*,rr,1],background=255,colors=fltarr(ntimes)+cols[rr],$
              baselines=valranges[*,rr,0],/overplot
     if rr eq 0 then sttr=' 0!Uo!N<'+thlet+'!DBN!N<'+rvst[0]+'!Uo!N' $
     else sttr=rvst[rr-1]+'!Uo!N'+'<'+thlet+'!DBN!N<'+rvst[rr]+'!Uo!N'
     xyouts,!p.position[2]+0.01,!p.position[3]-0.05-0.05*rr,sttr,charsize=3,$
            charthick=cthick+0.2,color=cols[rr],/norm
  endfor
  
  tvlct,rr,gg,bb,/get
  image=tvrd(true=1)
  fname=savepath+'thetabn_stats_time_'+event.date+'_'+event.label+'.png'
  write_png,fname,image,rr,gg,bb
  
;-======================================================================
  

;+======================================================================
  xtit='Shock Nose Distance, R!Ds!N'
  rad=radiusfitlines/rsun*event.geomcorfactor+1
  xrng=[min(rad),max(rad)]
  loadct,0,/silent
  PLOT, rad, allcrosses, PSYM = 10, $ 
        TITLE = thlet+'!DBN!N Evolution', $
        XTITLE =xtit, $
        YTITLE = ytit, $
        xticklen=-0.01,xtickformat='(f4.2)',$
        xrange=xrng,yrange=yrng,xticks=4, $
        xstyle=1,ystyle=1,color=0,background=255,$
        xthick=3,ythick=3,thick=3,charsize=3,charthick=cthick,/nodata
  
  loadct,13,/silent
  cols=findgen(nranges)*255./(nranges*1.0)
  rvst=strtrim(string(rangevals,format='(i3)'),2)
  for rr=0,nranges-1 do begin
     bar_plot,valranges[*,rr,1],background=255,colors=fltarr(ntimes)+cols[rr],$
              baselines=valranges[*,rr,0],/overplot
     if rr eq 0 then sttr=' 0!Uo!N<'+thlet+'!DBN!N<'+rvst[0]+'!Uo!N' $
     else sttr=rvst[rr-1]+'!Uo!N'+'<'+thlet+'!DBN!N<'+rvst[rr]+'!Uo!N'
     xyouts,!p.position[2]+0.01,!p.position[3]-0.05-0.05*rr,sttr,charsize=3,$
            charthick=cthick+0.2,color=cols[rr],/norm
  endfor
  
  tvlct,rr,gg,bb,/get
  image=tvrd(true=1)
  fname=savepath+'thetabn_stats_distance_'+event.date+'_'+event.label+'.png'
  write_png,fname,image,rr,gg,bb
;-======================================================================

  
end
