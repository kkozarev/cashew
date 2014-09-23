pro test_pfss_shock_plot_thetabn_stats
;Testing the shock crossing angles plotting procedure
event=load_events_info(label='paper')
pfss_shock_plot_thetabn_stats,event,/hires
end

pro pfss_shock_plot_thetabn_stats,event,lores=lores,hires=hires
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
  ;infile=find_latest_file(event.pfsspath+'csgs_results_*')
  infile=file_search(event.pfsspath+'csgs_results_'+event.date+'_'+event.label+'_lores.sav')
  if keyword_set(hires) then infile=file_search(event.pfsspath+'csgs_results_'+event.date+'_'+event.label+'_hires.sav')
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
  linekinds=intarr(ntimes,nranges,2)
  radstats=replicate({mean:0.D,max:0.D,min:0.D},ntimes,nranges)
  for tt=0,ntimes-1 do begin
     ;First take care of the first range
     res=where(crossPoints[tt,*].thbn gt 0.0 and crossPoints[tt,*].thbn le rangevals[0])
     if res[0] gt -1 then begin 
        valarr[tt,0]=n_elements(res)
        valranges[tt,0,0]=0
        valranges[tt,0,1]=valarr[tt,0]
     ;Find the radial positions of all points in the range
        points_radpos=reform(sqrt(crosspoints[tt,res].rpx^2+crosspoints[tt,res].rpy^2+crosspoints[tt,res].rpz^2))*event.geomcorfactor
        radstats[tt,0].mean=mean(points_radpos)
        tmp=minmax(points_radpos)
        radstats[tt,0].max=tmp[1]
        radstats[tt,0].min=tmp[0]
;Find the number of open and closed field lines for every angle range
;        tmpind=crosspoints[tt,res].linid
        openinds=crosspoints[tt,res].open
;        openinds=reform(pfsslines[tmpind].open)
        tmp=where(openinds eq 1)
        if tmp[0] eq -1 then linekinds[tt,0,0]=0 else linekinds[tt,0,0]=n_elements(tmp)
        linekinds[tt,0,1]=n_elements(openinds) - linekinds[tt,0,0]
     endif else begin
        valarr[tt,0]=0
        valranges[tt,0,0]=0
        valranges[tt,0,1]=0
        radstats[tt,0].mean=0.
        radstats[tt,0].max=0.
        radstats[tt,0].min=0.
        linekinds[tt,0,0]=0
        linekinds[tt,0,0]=0
     endelse
           
     
;Then loop over the others
     for rr=1,nranges-1 do begin
        res=where(crossPoints[tt,*].thbn gt rangevals[rr-1] and crossPoints[tt,*].thbn le rangevals[rr])
        if res[0] gt -1 then begin 
           valarr[tt,rr]=n_elements(res)
           valranges[tt,rr,0]=valranges[tt,rr-1,1]
           valranges[tt,rr,1]=valranges[tt,rr,0]+valarr[tt,rr]
           
 ;Find the radial positions of all points in the range
           points_radpos=reform(sqrt(crosspoints[tt,res].rpx^2+crosspoints[tt,res].rpy^2+crosspoints[tt,res].rpz^2))*event.geomcorfactor
           radstats[tt,rr].mean=mean(points_radpos)
           tmp=minmax(points_radpos)
           radstats[tt,rr].max=tmp[1]
           radstats[tt,rr].min=tmp[0]
        
 ;Find the number of open and closed field lines for every angle range
           
 ;tmpind=crosspoints[tt,res].linid
 ;openinds=reform(pfsslines[tmpind].open)
           openinds=crosspoints[tt,res].open
           tmp=where(openinds eq 1)
           if tmp[0] eq -1 then linekinds[tt,rr,0]=0 else linekinds[tt,rr,0]=n_elements(tmp)
           linekinds[tt,rr,1]=n_elements(openinds) - linekinds[tt,rr,0]
        endif else begin
           valarr[tt,rr]=0
           valranges[tt,rr,0]=0
           valranges[tt,rr,1]=0
           radstats[tt,rr].mean=0.
           radstats[tt,rr].max=0.
           radstats[tt,rr].min=0.
           linekinds[tt,rr,0]=0
           linekinds[tt,rr,0]=0
        endelse
     endfor
  endfor
  
 

;+======================================================================
  cthick=6
  chsize=2.2
  dummy = LABEL_DATE(DATE_FORMAT=['%H:%I'])
  xtit='Time of '+event.date
  ytit='# Shock-Interacting Fieldlines'
  thlet='!9'+String("161B)+'!X'
  ;thlet='!4' + String("150B) + '!X'

  
  !P.position=[0.16,0.14,0.94,0.92]
  !P.charthick=4
  !P.font=0
  ;wdef,0,1000,800
  set_plot,'ps'
  resolution='lores'
  if keyword_set(hires) then resolution='hires'
  fname=savepath+'thetabn_stats_'+event.date+'_'+event.label+'_'+resolution+'_time'
  device,file=fname+'.eps',/inches,xsize=10.0,ysize=8.0,$
         /encaps,/color,/helvetica
  loadct,0,/silent
  PLOT, tm, allcrosses, PSYM = 10, $ 
        TITLE = thlet+'!DBN!N Evolution', $
        XTITLE =xtit, $
        YTITLE = ytit, $
        XTICKUNITS = ['Time'],XTICKFORMAT='LABEL_DATE',$
        xticklen=-0.01,yminor=1,xminor=1,$
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
  cthick=6
  chsize=2.2
  dummy = LABEL_DATE(DATE_FORMAT=['%H:%I'])
  xtit='Time of '+event.date
  ytit='# Shock-Interacting Fieldlines'
  thlet='!9'+String("161B)+'!X'
  ;thlet='!4' + String("150B) + '!X'

  
  !P.position=[0.18,0.14,0.86,0.92]
  !P.charthick=4
  !P.font=0
  ;wdef,0,1000,800
  set_plot,'ps'
  fname=savepath+'thetabn_stats_'+event.date+'_'+event.label+'_'+resolution+'_time_crossings'
  device,file=fname+'.eps',/inches,xsize=10.0,ysize=8.0,$
         /encaps,/color,/helvetica
  loadct,0,/silent
  PLOT, tm, allcrosses, PSYM = 10, $ 
        TITLE = thlet+'!DBN!N Evolution', $
        XTITLE =xtit, $
        YTITLE = ytit, $
        XTICKUNITS = ['Time'],XTICKFORMAT='LABEL_DATE',$
        xticklen=-0.01,xminor=1,yminor=1,$
        xrange=xrng,yrange=yrng,xticks=4,$
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
;  axis,xstyle=1,xaxis=0,xthick=4,xrange=xrng,$
;       xtitle=xtit,charsize=chsize,charthick=chthick,xticklen=-0.01,$
;       XTICKUNITS = ['Time'],XTICKFORMAT='LABEL_DATE',color=0
;  axis,xstyle=1,xaxis=0,xthick=4,xrange=xrng,$
;       xtitle=xtit,charsize=chsize,charthick=chthick,xticklen=-0.01,$
;       XTICKUNITS = ['Time'],XTICKFORMAT='(A1)',color=0,/save
;  axis,ystyle=1,yaxis=0,ythick=4,yrange=yrng,$
;       ytitle=ytit,charsize=chsize,charthick=chthick,color=0
  
  maxopenlines=max(linekinds[*,*,0])
  if maxopenlines lt 5 then maxopenlines=5
  axis,ystyle=1,yaxis=1,ythick=4,yrange=[0,maxopenlines],/save,$
       ytitle='# Open Crossing Lines in Range',charsize=chsize,$
       charthick=chthick,color=0,yticks=5,ytickformat='(i5)'
  
  for rr=0,nranges-1 do begin
     ;totranglin=linekinds[*,rr,0]+linekinds[*,rr,1]
     ;open_percent=(linekinds[*,rr,0]/(1.*totranglin))*100.
     if cols[rr] eq 0 then bckcol=255 else bckcol=0
     LOADCT,0,/SILENT
     ;oplot,tm,smooth(linekinds[*,rr,0],2,/edge_trunc),thick=10,color=bckcol
     oplot,tm,linekinds[*,rr,0],thick=10,color=bckcol
     LOADCT,13,/SILENT
     ;oplot,tm,smooth(linekinds[*,rr,0],2,/edge_trunc),thick=8,color=cols[rr]
     oplot,tm,linekinds[*,rr,0],thick=8,color=cols[rr]
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
  cthick=6
  chsize=2.2
  dummy = LABEL_DATE(DATE_FORMAT=['%H:%I'])
  xtit='Time of '+event.date
  thlet='!9'+String("161B)+'!X'
  ytit='Radial Distance, R!Dsun!N'
  ;thlet='!4' + String("150B) + '!X'
  
  
  !P.position=[0.18,0.14,0.92,0.92]
  !P.charthick=4
  !P.font=0
  ;wdef,0,1000,800
  set_plot,'ps'
  fname=savepath+'thetabn_stats_'+event.date+'_'+event.label+'_'+resolution+'_time_radpos'
  device,file=fname+'.eps',/inches,xsize=8.0,ysize=8.0,$
         /encaps,/color,/helvetica
  loadct,0,/silent
  PLOT, tm, allcrosses, PSYM = 10, $ 
        TITLE = 'Open-line crossings by '+thlet+'!DBN!N Range', $
        XTITLE =xtit, $
        YTITLE = ytit, $
        XTICKUNITS = ['Time'],XTICKFORMAT='LABEL_DATE',$
        xticklen=-0.01,xminor=1,yminor=1,$
        xrange=xrng,yrange=[1.1,1.55],xticks=4,$
        xstyle=1,ystyle=1,color=0,background=255,$
        xthick=4,ythick=4,thick=4,charsize=chsize,charthick=chthick,/nodata
  
  loadct,13,/silent
  cols=findgen(nranges)*255./(nranges*1.0)
  rvst=strtrim(string(rangevals,format='(i3)'),2)
 ; for rr=0,nranges-1 do begin
 ;    bar_plot,valranges[*,rr,1],background=255,colors=fltarr(ntimes)+cols[rr],$
 ;             baselines=valranges[*,rr,0],/overplot
;     if rr eq 0 then sttr=' 0!Uo!N<'+thlet+'!DBN!N<'+rvst[0]+'!Uo!N' $
;     else sttr=rvst[rr-1]+'!Uo!N'+'<'+thlet+'!DBN!N<'+rvst[rr]+'!Uo!N'
;     xyouts,!p.position[0]+0.02,!p.position[3]-0.05-0.05*rr,sttr,charsize=chsize,$
;            charthick=chthick,color=cols[rr],/norm
 ; endfor
  
  for rr=0,nranges-1 do begin
     ;totranglin=linekinds[*,rr,0]+linekinds[*,rr,1]
     ;open_percent=(linekinds[*,rr,0]/(1.*totranglin))*100.
     if cols[rr] eq 0 then bckcol=255 else bckcol=0
     LOADCT,0,/SILENT
     oplot,tm,radstats[*,rr].min,thick=10,color=bckcol,linestyle=2
     oplot,tm,radstats[*,rr].mean,thick=10,color=bckcol
     oplot,tm,radstats[*,rr].max,thick=10,color=bckcol,linestyle=2
     LOADCT,13,/SILENT
     oplot,tm,radstats[*,rr].min,thick=8,color=cols[rr],linestyle=2
     oplot,tm,radstats[*,rr].mean,thick=8,color=cols[rr]
     oplot,tm,radstats[*,rr].max,thick=8,color=cols[rr],linestyle=2
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
  fname=savepath+'thetabn_stats_'+event.date+'_'+event.label+'_'+resolution+'_distance'
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





end
