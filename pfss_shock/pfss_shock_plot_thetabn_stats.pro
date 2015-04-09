pro test_pfss_shock_plot_thetabn_stats
;Testing the shock crossing angles plotting procedure
  labels=['paper','130423_01','140708_01']
  for ev=0,n_elements(labels)-1 do begin
      label=labels[ev]
      event=load_events_info(label=label)
      pfss_shock_plot_thetabn_stats,event,/hires
  endfor
end

pro load_typeII,event,filename,tIItime,tIIdens,tIIrad,lanes,lsi
;load and plot the Learmonth radio backbone positions
  close,/all
  line=''
  t2str={time:0.0,freq:0.0,factor:0.0}
  openr,lun,filename,/get_lun
  lanes=''
  lsi=0
  cc=0.
  while not eof(lun) do begin
     readf,lun,line
     if strpos(line,'%%') gt -1 then begin
        if lanes[0] eq '' then lanes=line else lanes=[lanes,line]
        if cc gt 0 then lsi=[lsi,cc]
        continue
     endif else begin
        res=strsplit(line,'  ',/extract)
        if cc eq 0 then data=float([res[0],res[1],res[2]]) $
        else data=[[data],[res[0],res[1],res[2]]]
        cc++
     endelse
     
  endwhile
  close,/all
  nl=n_elements(lanes)
  for i=0,nl-1 do begin
     res=strsplit(lanes[i],'%%',/extract)
     lanes[i]=strtrim(res[0],2)
  endfor
  tIItime=reform(data[0,*]) ;time in seconds since the start of day
  nt=n_elements(tIItime)
  ;tIIdens=reform((8980.*data[1,*]/data[2,*])^2) ;density in 1/cm^3
  
;Calculate the density, convert to heights.
 ; tIIdens=reform((data[1,*]/data[2,*]*1.e6/8980.)^2) ;density in 1/cm^3
  tIIdens=reform((data[1,*]*1.e6/8980.)^2) ;density in 1/cm^3
;Use the 1-fold Newkirk model
  a=1.0
  n0=4.2e4
  rs=6.958e5
  RSUN=6.955e5                  ;Solar radius in km 
  tIIrad=4.32/alog10(tIIdens/n0/a)
  
  
  ;Get the JD time from the tII plots time
  hh=strtrim(string(fix(tIItime/3600.)),2)
  tmp=where(hh lt 10)
  if tmp[0] gt -1 then hh[tmp]='0'+hh[tmp]
  mm=strtrim(string(fix((tIItime/3600.-hh)*60.)),2)
  tmp=where(mm lt 10)
  if tmp[0] gt -1 then mm[tmp]='0'+mm[tmp]
  ss=strtrim(string(fix(tIItime-hh*3600.-mm*60.)),2)
  tmp=where(ss lt 10)
  if tmp[0] gt -1 then ss[tmp]='0'+ss[tmp]
  yyyy=strarr(nt)+strmid(event.date,0,4)
  mn=strarr(nt)+strmid(event.date,4,2)
  dd=strarr(nt)+strmid(event.date,6,2)
  
  ;Convert the time to JD and pass on
  tIItime=julday(mn,dd,yyyy,hh,mm,ss)
end


pro pfss_shock_plot_thetabn_stats,event,lores=lores,hires=hires,typeII=typeII,force=force
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
;Modified by KAK, 12/04/2014 - Added advanced statistical information
;                              plotting capability
;

  set_plot,'x'
  close,/all
  savepath=event.pfsspath
  ;infile=find_latest_file(event.pfsspath+'csgs_results_*')
  tIIfilename='vasili_out.txt'
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
  
;load the Learmonth radio data  
  if keyword_set(typeII) then load_typeII,event,tIIfilename,tIItime,tIIdens,tIIrad,lanes,lsi

  
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
  savnam='thetabn_stats_'+event.date+'_'+event.label+'_'+resolution+'_time'
  fname=savepath+savnam
  if file_test(fname) and not keyword_set(force) then begin
     print,'File '+savnam
     print,'already exists. To overwrite, rerun with /force.'
     print,'----'
  endif else begin
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
  endelse
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
  savnam='thetabn_stats_'+event.date+'_'+event.label+'_'+resolution+'_time_crossings'
  fname=savepath+savnam
  if file_test(fname) and not keyword_set(force) then begin
     print,'File '+savnam
     print,'already exists. To overwrite, rerun with /force.'
     print,'----'
  endif else begin
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
  endelse
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
  savnam='thetabn_stats_'+event.date+'_'+event.label+'_'+resolution+'_time_radpos'
  fname=savepath+savnam
  if file_test(fname) and not keyword_set(force) then begin
     print,'File '+savnam
     print,'already exists. To overwrite, rerun with /force.'
     print,'----'
  endif else begin
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
endelse
;-======================================================================

  ;Calculate the position of the shock nose
  shocknosepos=(radiusfitlines/6.958e5 + 1.)*event.geomcorfactor

  
;+======================================================================
;- - - - - - - - - - - - - - - - - - - - - -
; Here, calculate the binned crossings (binned by radial position)
;- - - - - - - - - - - - - - - - - - - - - -
  rangevals=[50.,90.]
  nranges=n_elements(rangevals-1)
  valarr=lonarr(ntimes,nranges)
  valranges=lonarr(ntimes,nranges,2)
  linekinds=intarr(ntimes,nranges,2)
  allpoints_radpos=fltarr(ntimes,max(allcrosses))
  nclosed=intarr(ntimes)

  radstats=replicate({mean:0.D,max:0.D,min:0.D},ntimes,nranges)
  for tt=0,ntimes-1 do begin
     ;First take care of the first range
     res=where(crossPoints[tt,*].thbn gt rangevals[0] and crossPoints[tt,*].thbn le rangevals[1])
     if res[0] gt -1 then begin
        valarr[tt,0]=n_elements(res)
        valranges[tt,0,0]=0
        valranges[tt,0,1]=valarr[tt,0]
        
;Find the number of open and closed field lines for every angle range
        closedinds=reform(crosspoints[tt,res].open)
        tmp=where(closedinds eq 0)
        if tmp[0] gt -1 then closedinds=tmp else continue
;        openinds=reform(pfsslines[tmpind].open)
        
     ;Find the radial positions of all points in the range
        points_radpos=reform(sqrt(crosspoints[tt,res].rpx^2+crosspoints[tt,res].rpy^2 $
                                  +crosspoints[tt,res].rpz^2))*event.geomcorfactor
        
; If the following line is commented
; out, we take all crossings, not just
; the closed-field ones.
        ;points_radpos=points_radpos[closedinds]
        
        nclosed[tt]=n_elements(points_radpos)
        
        allpoints_radpos[tt,0:n_elements(points_radpos)-1]=points_radpos
        radstats[tt,0].mean=mean(points_radpos)
        tmp=minmax(points_radpos)
        radstats[tt,0].max=tmp[1]
        radstats[tt,0].min=tmp[0]
                
        if tmp[0] eq -1 then linekinds[tt,0,0]=0 else linekinds[tt,0,0]=n_elements(tmp)
        linekinds[tt,0,1]=n_elements(closedinds) - linekinds[tt,0,0]
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
  endfor
;- - - - - - - - - - - - - - - - - - - - - -

  cthick=6
  chsize=2.2
  dummy = LABEL_DATE(DATE_FORMAT=['%H:%I'])
  xtit='Time of '+event.date
  thlet='!9'+String("161B)+'!X'
  ytit='Radial Distance, R!Dsun!N'
  ;thlet='!4' + String("150B) + '!X'
  
  !P.position=[0.18,0.14,0.9,0.92]
  !P.charthick=4
  !P.font=0
  ;wdef,0,1000,800
  set_plot,'ps'
  savnam='thetabn_stats_'+event.date+'_'+event.label+'_'+resolution+'_time_radpos_binned'
  fname=savepath+savnam
  if file_test(fname) and not keyword_set(force) then begin
     print,'File '+savnam
     print,'already exists. To overwrite, rerun with /force.'
     print,'----'
  endif else begin
  device,file=fname+'.eps',/inches,xsize=8.0,ysize=8.0,$
         /encaps,/color,/helvetica
  loadct,0,/silent
  xrng=[min(tm),max(tm)]
  if keyword_set(typeII) then yrnge=[1.0,1.8] else yrnge=[1.0,max(shocknosepos)*1.1]
  PLOT, tm, allcrosses, PSYM = 10, $ 
        TITLE = 'tII lanes and 50!Uo!N<'+thlet+'!DBN!N<90!Uo!N Crossings', $
        XTITLE =xtit, $
        YTITLE = ytit, $
        XTICKUNITS = ['Time'],XTICKFORMAT='LABEL_DATE',$
        xticklen=-0.01,xminor=1,yminor=1,$
        xrange=xrng,yrange=yrnge,xticks=4,$
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

  ;Plot binned crossing radii
  for tt=0,ntimes-1 do begin
    if nclosed[tt] gt 0 then begin 
    tmp=reform(allpoints_radpos[tt,0:nclosed[tt]-1])
     nbins=40.
     binsize=(max(tmp)-min(tmp))/nbins
     crosshist=histogram(tmp,nbins=nbins)
     tmprad=min(tmp)+findgen(nbins)*binsize+binsize/2.
     LOADCT,0,/SILENT
     for bb=0,nbins-1 do begin
        plots,tm[tt],tmprad[bb],color=255,psym=sym(1),$
              symsize=1.7*crosshist[bb]/(max(crosshist)*1.0)
        plots,tm[tt],tmprad[bb],color=0,psym=sym(1),$
              symsize=1.6*crosshist[bb]/(max(crosshist)*1.0)     
endfor
endif
  endfor
  
  for rr=0,nranges-1 do begin
     ;totranglin=linekinds[*,rr,0]+linekinds[*,rr,1]
     ;open_percent=(linekinds[*,rr,0]/(1.*totranglin))*100.
     if cols[rr] eq 0 then bckcol=255 else bckcol=0
     LOADCT,0,/SILENT
     oplot,tm,radstats[*,rr].min,thick=8,color=bckcol,linestyle=0
     ;oplot,tm,radstats[*,rr].mean,thick=10,color=bckcol
     oplot,tm,radstats[*,rr].max,thick=8,color=bckcol,linestyle=0
     
     LOADCT,13,/SILENT
     oplot,tm,radstats[*,rr].min,thick=6,color=cols[rr],linestyle=2
     ;oplot,tm,radstats[*,rr].mean,thick=8,color=cols[rr]
     oplot,tm,radstats[*,rr].max,thick=6,color=cols[rr],linestyle=2
  endfor
  
  oplot,tm,shocknosepos,thick=10,linestyle=0
  
  ;oplot,tIItime,tIIrad,psym=2,symsize=2
 
  if keyword_set(typeII) then begin
     loadct,13,/silent
     nl=n_elements(lsi)
     for i=0,nl-2 do begin
        oplot,tIItime[lsi[i]:lsi[i+1]-1],tIIrad[lsi[i]:lsi[i+1]-1],psym=2,$
              symsize=1,color=160.*(i+1)/(nl-2.)
        xyouts,0.2,0.87-0.04*i,lanes[i],/norm,color=160.*(i+1)/(nl-2.),charthick=3,charsize=1.8
     endfor
     oplot,tIItime[lsi[nl-1]:*],tIIrad[lsi[nl-1]:*],psym=2,symsize=1,color=255
     xyouts,0.2,0.87-0.04*i,lanes[i],/norm,color=255,charthick=3,charsize=1.8
  endif

  device,/close
  exec='convert -flatten '+fname+'.eps '+fname+'.png ; rm -rf '+fname+'.eps '
  spawn,exec
  set_plot,'x'
endelse
;-======================================================================



;+======================================================================
  xtit='Shock Nose Distance, R!Ds!N'
  rad=radiusfitlines/rsun*event.geomcorfactor+1
  xrng=[min(rad),max(rad)]
  ;wdef,0,1000,800
  set_plot,'ps'
  savnam='thetabn_stats_'+event.date+'_'+event.label+'_'+resolution+'_distance'
  fname=savepath+savnam
  if file_test(fname) and not keyword_set(force) then begin
     print,'File '+savnam
     print,'already exists. To overwrite, rerun with /force.'
     print,'----'
  endif else begin
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
endelse
;-======================================================================





end
