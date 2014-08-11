pro test_aia_annulus_analyze_tangential
;Procedure to run and test aia_annulus_analyze_tangential

;You can run for one event, like this.
  one=1
  if one eq 1 then begin
     wav='193'
     rrange=[1.11,1.34]
     event=load_events_info(label='paper')
     aia_annulus_analyze_tangential,event,wave=wav;,rrange=rrange ;,/interactive
  endif
  
  
;Alternatively, run for all events
  all=0
  if all eq 1 then begin
     events=load_events_info()
     wavelengths=['193','211']
     rrange=[1.1,1.37]
;n_elements(events)-1
     for ev=0,n_elements(events)-1 do begin
        event=events[ev]
        for w=0,n_elements(wavelengths)-1 do begin
           wavelength=wavelengths[w]
           aia_annulus_analyze_tangential,event,wave=wavelength,rrange=rrange ;,/interactive
        endfor
     endfor
  endif

end



;+============================================================================
function jmap_filter_maxima_tangential,time, mind, ht_km, height, mymaxima,fitrange=fitrange,outliers=outliers
;PURPOSE:
; Filters the position maxima to make them smooth and physical
;
;CATEGORY:
; AIA/Kinematics
;
;INPUTS:
;      TIME - time array, must be in seconds!
;      HEIGHT - the y-array in the jmap, must be in km!
;      MYMAXIMA - the array of maxima indices from jmap_find_maxima
;      
;
;KEYWORDS:
;      FITRANGE - the index range the user picks for fitting
;                  kinematics
;      OUTLIERS - on output, contains an array containing -1 for
;                 outliers, 1 for all other values
;OUTPUTS:
;      Returns the  
; 
;DEPENDENCIES:
;
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 11/29/2013
  RSUN=6.96e5                   ;Solar radius in km.
  DIST_FACTOR=RSUN*!PI/180.
  vlimit=[10.0,1500.0]                 ;Position derivative max limit, in km/s.
  maxinds=mymaxima
  

  ;Set the time range so we only look at the user-selected time range.
  if keyword_set(fitrange) then tr=fitrange else tr=[0,n_elements(time)-1]
  rind=reform(maxinds[0,tr[0]:tr[1]].ind)

;Get the ranges of the time and radial distance, as well as their changes
  ht=ht_km[rind]

;  ht=height[reform(maxinds[0,tr[0]:tr[1]].ind)]
  nr=n_elements(ht)
  dr=ht[1:nr-1]-ht[0:nr-2]

  tm=time[tr[0]:tr[1]]
  nt=n_elements(tm)
  dt=tm[1:nt-1]-tm[0:nt-2]
  
  ;Here, go through all the points, and check the speeds, fixing the
  ;positions if necessary
  change=0
  for tt=0,nt-2 do begin
     v=(ht[tt+1]-ht[tt])/(tm[tt+1]-tm[tt])
    
     if v lt vlimit[0] then begin
        ht[tt+1]=ht[tt]+(tm[tt+1]-tm[tt])*vlimit[0]
        change=1
     endif else begin
        if v ge vlimit[1] then begin
           ht[tt+1]=ht[tt]+(tm[tt+1]-tm[tt])*vlimit[1]
           change=1
        endif
     endelse
     maxinds[0,fitrange[0]+tt+1].rad=ht_km[maxinds[0,fitrange[0]+tt+1].ind]/(DIST_FACTOR*height)


;If there has been a change in the position, find the nearest larger height and
;assign its index to the maxinds structure.
     if change ne 0 then begin
        maxinds[0,fitrange[0]+tt+1].ind=min(where(ht_km-ht[tt+1] gt 0.))
        maxinds[0,fitrange[0]+tt+1].rad=ht[tt+1]/(DIST_FACTOR*height)
     endif
     
    change=0 
  endfor
  return,maxinds
end
;-============================================================================




;+============================================================================
pro annulus_fit_maxima_tangential,event,indata,datastruct,time,yarr,constrain=constrain,$
                                  auto=auto, gradient=gradient, y_rsun_array=y_rsun_array

  RSUN=6.96e5  ;Solar radius in km.
  nlatmeas=n_elements(datastruct.imgtit)
  nt=n_elements(time)
  yarray=yarr
  

  ;The info to pass to the position fitting routine for the fitting parameters
  parinfo = replicate({value:0.D, limited:[0,0], limits:[0.D,0.D]}, 3)
  
  xmargin=0.001
  ymargin=0.02
  chars=4
  TIME_FACTOR=60.
  DIST_FACTOR=RSUN*!PI/180.
  parinfo[0].value=5.0*DIST_FACTOR
  parinfo[0].limited=[1,1]
  parinfo[0].limits=[1.0,20.0]*DIST_FACTOR
  
  
  parinfo[1].value=100.
  parinfo[1].limited=[1,1]
  parinfo[1].limits=[0.0,2000.0]
  parinfo[2].value=10.
  parinfo[2].limited[0]=1
  parinfo[2].limits[0]=-1500.0
  
  yrngOrig=datastruct.yfitrange
  yrng = datastruct.yfitrange
  !p.multi=datastruct.multi
  device,window_state=win_open
  
  
;LOOP OVER MEASUREMENTS!
  for mind=0, nlatmeas-1 do begin
;  for mind = 2, 2    do begin
     yrng = yrngOrig
     
     wave_frontedge=0
     wave_backedge=wave_frontedge
     data=indata[*,*,mind]
     height=datastruct.lat_heights[mind]
     
     if n_elements(data[0,*])-1 lt yrng[1] then yrng[1] = n_elements(data[0,*])-1

     ht_km=yarr*DIST_FACTOR*height
     if mind eq 0 then wdef,datastruct.winind,datastruct.winsize[0],datastruct.winsize[1]
  
     !P.position=[0.18,0.17,0.9,0.9]
     fitrange=intarr(2)
                                ;
     
     maxRadIndex = min(where(data[0,*] eq 0.0))
     if maxRadIndex eq -1 then begin
        maxRadIndex = n_elements(yarray)-1
     endif
     
;     help, maxRadIndex
     
;Find start and end positions
     if keyword_set(auto) then begin
        if keyword_set(gradient) then begin

           bin_aia_data, time.jd, yarray, data, yrng, binData=binData
           
           make_gradient_map_tangential, time.jd, yarray, binData, yrng, intensityData=intensityData

           mask_gradient_data, time.jd, yarray, data, yrng, intensityData, correctData=correctData

           data = correctData

        endif
           
;;         filter_tangential_data, data[*, yrng[0]:yrng[1]], time, yarray, yrng=yrng, maxYInd=maxYInd, maxFrontEdge=maxFrontEdge
;; ;        datastruct.yfitrange = yrng
        maxYInd = yrng[1]
        maxFrontEdge = yrng[1]
        
        aia_jmap_find_maxima,data,time.relsec,yarray,mymaxima=mymaxima,allmaxima=allmaxima,$
                             yrange=[yarr[datastruct.yfitrange[0]],yarr[maxYInd]],$
                             numplotmax=3

        print, mymaxima
        
        find_start_end_tangential, data[*, yrng[0]:yrng[1]], time, startInd=startInd, endInd=endInd, maxYind=maxYInd
      
        
;Exit if a good start position is not found
        if startInd eq -1 then return
        if endInd eq -1 then return
        if startInd eq endInd then return
        
        print, "Initial start index: ", startInd
        print, "Initial end index: ", endInd
        
        fitrange=[startInd, endInd]
           
     endif else begin
        
        aia_plot_jmap_data,time.jd,yarray[yrng[0]:yrng[1]],data[*,yrng[0]:yrng[1]],$
                           min=-40,max=50,fitrange=fitrange,$
                           title=datastruct.imgtit[mind],$
                           xtitle=datastruct.xtitle,ytitle=datastruct.ytitle

        
        
        aia_jmap_find_maxima,data,time.relsec,yarray,mymaxima=mymaxima,allmaxima=allmaxima,$
                             yrange=[yarr[datastruct.yfitrange[0]],yarr[datastruct.yfitrange[1]]],$
                             numplotmax=3

     endelse
     
     datastruct.xfitrange=fitrange
     sp=datastruct.xfitrange[0]
     ep=datastruct.xfitrange[1]
 ;Search for the edges of the wave
     wave_frontedge=replicate({rad:0.0D,yind:0L,xind:0L},ep-sp)
     wave_backedge=wave_frontedge
     
;To restore the plot information and overplot on them, do
     datastruct.plotinfo[mind].p=!P
     datastruct.plotinfo[mind].x=!X
     datastruct.plotinfo[mind].y=!Y

     ;;                            ;Fit the maxima and overplot them...
     ;; aia_jmap_find_maxima,data,time.relsec,yarray,mymaxima=mymaxima, allmaxima=allmaxima,$
     ;;                      yrange=[yarr[datastruct.yfitrange[0]],yarr[datastruct.yfitrange[1]]],$
     ;;                      numplotmax=3

;  jmap_find_maxima,data[*,yrng[0]:yrng[1]],time,yarray[yrng[0]:yrng[1]],$
;                   mymaxima=mymaxima,numplotmax=3               
     tmp=reform(mymaxima[0,*].ind)
     datastruct.maxinds[0,*]=reform(mymaxima[0,*].ind)
     
;Filter the maxima positions here for physicality
                                ;outliers=1
      maxinds=jmap_filter_maxima_tangential(time.relsec, mind, ht_km[yrng[0]:maxYInd], height, mymaxima,fitrange=datastruct.xfitrange) ;,outliers=outliers
     ;; mymaxima = maxinds 


     good_ind_pos=where(maxinds.ind ge 0)
     good_max_inds=maxinds[good_ind_pos]
     device,window_state=win_open
;  oplot,time.jd,reform(yarray[datastruct.maxinds[mind,*]]),psym=1,color=200,thick=4,symsize=2
     
     loadct,8,/silent
;oplot,time[sp:ep],yarray[datastruct.maxinds[mind,sp:ep]],psym=1,color=200,thick=4,symsize=2
;oplot,time.jd[sp+good_ind_pos],yarray[datastruct.maxinds[mind,sp+good_ind_pos]],psym=1,color=200,thick=4,symsize=2
;oplot,[time[sp].jd,time[sp].jd],[yarray[0],yarray[n_elements(yarray)-1]],color=255
     
     find_wave_edge_tangential, data, yarray, yrng, time, fitrange, mymaxima, mind,$
                                maxRadIndex, datastruct=datastruct, wave_frontedge=wave_frontedge,$
                                wave_backedge=wave_backedge, maxYInd=maxYInd
     print, "Printing front edge in main program"
     print, wave_frontedge

                                ;for ii=sp,ep do begin
                                ;  if maxinds[ii-sp] gt 0.0 then begin
     
     
     
;; ;+--------------------------------------------------------------
;; ;Find the front edge of the wave
;;         oldv=1
;;         if oldv gt 0 then begin
;; ;OLD VERSION, SEARCHING DOWN FROM INTENSITY PEAK
;;            y=reform(datastruct.bdiff[ii,mymaxima[0,ii].ind:*])
;;            y=smooth(y,4,/edge_truncate)
;;            np=n_elements(y)
;;            tmp=min(where(y le 0.2*max(y)))
;;            if tmp[0] eq -1 then tmp=np-1
;;            wave_frontedge[ii-sp].val=yarray[mymaxima[0,ii].ind+tmp]
;;            wave_frontedge[ii-sp].ind=mymaxima[0,ii].ind+tmp
;;            datastruct.frontinds[mind,ii]=mymaxima[0,ii].ind+tmp
;;         endif
        
;;         newv=0
;;         if newv gt 0 then begin
;; ;NEW VERSION,SEARCHING UP FROM BACKGROUND 
;;            maxind=mymaxima[0,ii].ind-datastruct.yfitrange[0]
;;            ylim=datastruct.yfitrange[1]
;;            y=reform(datastruct.bdiff[ii,maxind:ylim])
;;            y=smooth(y,4,/edge_truncate)
;;            np=n_elements(y)
;;            y=reverse(y)
;;                                 ;y[where(y le 0.0)]=1.0e-10
;;            bind=20
;;            bckg=abs(avg(y[0:bind-1]))
;;            tmp=min(where(y gt (y[np-1]-bckg)*0.2)) ;look for 20% increase above background
;;            tmp=np-tmp                              ;since data is reversed, reverse the index as well.
;;            if tmp[0] eq -1 then tmp=0
;;            wave_frontedge[ii-sp].val=yarray[mymaxima[0,ii].ind+tmp]
;;            wave_frontedge[ii-sp].ind=mymaxima[0,ii].ind+tmp
;;            datastruct.frontinds[mind,ii]=mymaxima[0,ii].ind+tmp
;;         endif
;; ;---------------------------------------------------------------
        
        
;; ;+--------------------------------------------------------------
;; ;Find the back edge of the wave
;;         oldv=1
;;         if oldv gt 0 then begin
;; ;OLD VERSION, SEARCHING DOWN FROM INTENSITY PEAK
;;            y=reform(datastruct.bdiff[ii,0:mymaxima[0,ii].ind])
;;            y=smooth(y,4,/edge_truncate)
;;            np=n_elements(y)
;;            y=reverse(y,1)       ;reverse the array so the search is the same
;;            tmp=min(where(y le 0.2*max(y)))
;;            if tmp[0] eq -1 then tmp=np-1
;;            wave_backedge[ii-sp].val=yarray[mymaxima[0,ii].ind-tmp]
;;            wave_backedge[ii-sp].ind=mymaxima[0,ii].ind-tmp
;;            datastruct.backinds[mind,ii]=mymaxima[0,ii].ind-tmp
;;         endif
        
;;         newv=0
;;         if newv gt 0 then begin    
;; ;NEW VERSION, SEARCHING UP FROM BACKGROUND
;;            y=reform(datastruct.bdiff[ii,0:maxind])
;;            y=reverse(y,1)
;;            y=smooth(y,4,/edge_truncate)
;;            np=n_elements(y)
;;            tmp=min(where(y gt 2.*bckg)) ;look for 20% increase above background
;;            stop
;;            if tmp[0] eq -1 then tmp=np-1
;;            wave_backedge[ii-sp].val=yarray[mymaxima[0,ii].ind+tmp]
;;            wave_backedge[ii-sp].ind=mymaxima[0,ii].ind+tmp
;;            datastruct.backinds[mind,ii]=mymaxima[0,ii].ind+tmp
;;         endif
;; ;---------------------------------------------------------------    
        
 
;DEBUG    
;For now, don't overplot the back edge  
;        oplot,[time[ii],time[ii]],$
;              [wave_backedge[ii-sp].val,wave_frontedge[ii-sp].val],$
;              color=200,thick=1
;END DEBUG
;     endif 
;  endfor
  loadct,0,/silent
 
if keyword_set(auto) then begin
     ; Correct start and end positions with maxima data
     
     startCorr = 0
     endCorr = 0
     
     find_corr_start_tangential, data, time, yarray, datastruct, ht_km, height, fitrange, yrng, mind,$
                      maxRadIndex, startInd=startInd, mymaxima=mymaxima,$
                      wave_frontedge=wave_frontedge, startCorr=startCorr, constrain=constrain,$
                      wave_backedge=wave_backedge, maxYInd=maxYInd

     help, wave_frontedge

     find_corr_end_tangential, data, time, yarray, startInd=startInd, endInd=endInd, wave_frontedge=wave_frontedge,$
                   maxRadIndex=maxRadIndex, endCorr=endCorr, maxFrontEdge=maxFrontEdge

     help, wave_frontedge

     print, "Corrected start index: ", startInd
     print, "Corrected end index: ", endInd

     if endCorr le startCorr then begin
        endCorr = n_elements(wave_frontedge)-1
     endif
     
     print, "Before correct"
     print, wave_frontedge

     ; Correct the positioning of the wave edges
     wave_frontedge = wave_frontedge[0:endCorr]
     wave_backedge = wave_backedge[0:endCorr]

     print, "After correct"
     print, wave_frontedge

     ; Plot new corrected region of interest 
     aia_plot_jmap_data,time.jd,yarray[yrng[0]:yrng[1]],data[*,yrng[0]:yrng[1]],$
                        min=-40,max=50,fitrange=fitrange,$
                        title=datastruct.imgtit[mind],$
                        xtitle=datastruct.xtitle,ytitle=datastruct.ytitle, /auto, startInd=startInd, endInd=endInd  

  endif
  
  datastruct.xfitrange=fitrange
  sp=datastruct.xfitrange[0]
  ep=datastruct.xfitrange[1]
;Search for the edges of the wave
  
  
;To restore the plot information and overplot on them, do
  datastruct.plotinfo[mind].p=!P
  datastruct.plotinfo[mind].x=!X
  datastruct.plotinfo[mind].y=!Y
  
 tmp=reform(mymaxima[0,*].ind)
 datastruct.maxinds[0,*]=reform(mymaxima[0,*].ind)

  ;Filter the maxima positions here for physicality
  if keyword_set(constrain) then begin
     maxinds=jmap_filter_maxima_tangential(time.relsec, mind, ht_km, height, allmaxima,fitrange=datastruct.xfitrange) ;,outliers=outliers
     mymaxima=maxinds
  endif
  
  device,window_state=win_open
  dt=(time[1:n_elements(time)-1].jd-time[0:n_elements(time)-2].jd)/2.
  dt = [dt, dt[0]]
 
  print, reform(yarray[datastruct.maxinds[0, sp]])
  oplot,time.jd-dt,reform(yarray[datastruct.maxinds[0,*]]),psym=1,color=200,thick=4,symsize=2
  loadct,8,/silent
   
;  oplot, time[sp:ep].jd-dt[sp:ep], data[sp:ep, mymaxima], psym=1, color=200, thick=4, symsize=2

  oplot,time[sp:ep].jd-dt[sp:ep],reform(mymaxima[0,sp:ep].rad),psym=1,color=200,thick=4,symsize=2
  
  for ii=sp,ep do begin
;     oplot, [time[ii].jd - dt[ii]], [yarray[mymaxima[ii-sp]]], psym=1, color=200, thick=4, symsize=2
;DEBUG    
;Overplot the front edge 

     oplot,[time[ii].jd-dt[ii],time[ii].jd-dt[ii]],[mymaxima[0,ii].rad,wave_frontedge[ii-sp].rad],$
           color=200,thick=2

     oplot,[time[ii].jd-dt[ii],time[ii].jd-dt[ii]],[mymaxima[0,ii].rad,wave_backedge[ii-sp].rad],$
           color=200,thick=2
;END DEBUG
  endfor

loadct, 0, /silent

; No fitting for now
;test = get_kbrd(1)
;continue
  
;--------------------------------------------------
;Do second order polynomial fitting for the wave fronts
  print,''
  print,'Fitting a second-order polynomial to the wave front edge positions...'
  dist=reform(wave_frontedge[0:ep-sp].rad)*DIST_FACTOR*height
  ;dist=reform(wave_frontedge[good_ind_pos].val)*DIST_FACTOR*height
  time_good=time[sp:ep].relsec-time[sp].relsec
  dist=smooth(dist,2)
  bootstrap_sdo,dist,time_good,fit_line, p1, p2, p3, s1, s2, s3,parinfo=parinfo, /linear
  print,''
  print,''
  wave_fits=p1[0] + p2[0] * (time_good)+ 0.5 * p3[0] * (time_good)^2
  wave_fits=wave_fits/DIST_FACTOR/height
  datastruct.fitparams[mind,0].front=p1[0]
  datastruct.fitparams[mind,1].front=p2[0]
  datastruct.fitparams[mind,2].front=p3[0]
  datastruct.fitsigma[mind,0].front=s1[0]
  datastruct.fitsigma[mind,1].front=s2[0]
  datastruct.fitsigma[mind,2].front=s3[0]
;  wave_rads=rad_data.fitparams[0,0].front+rad_data.fitparams[0,1].front*wave_times+0.5*(rad_data.fitparams[0,2].front)^2
;--------------------------------------------------
  ;oplot,time[sp:ep],wave_fits,thick=3
  oplot,time.jd[sp+good_ind_pos],wave_fits,thick=3
  
  
;; ;--------------------------------------------------
;; ;Do second order polynomial fitting for the maxima
;;   print,''
;;   print,'Fitting a second-order polynomial to the wave peak positions...'
;;   dist=reform(ht_km[mymaxima[0,sp:ep].ind])*height
;;   ;dist=reform(ht_km[mymaxima[0,sp+good_ind_pos].ind])
;;   time_good=time[sp:ep].relsec-time[sp].relsec
;;   dist=smooth(dist,2)
;;   ;bootstrap_sdo,dist,time_good,fit_line, p1, p2, p3, s1, s2, s3,parinfo=parinfo
;;   print,''
;;   print,''
;;   wave_fits=p1[0] + p2[0] * (time_good)+ 0.5 * p3[0] * (time_good)^2
;;   wave_fits=wave_fits/DIST_FACTOR;RSUN*180./!PI
;;   datastruct.fitparams[mind,0].max=p1[0]
;;   datastruct.fitparams[mind,1].max=p2[0]
;;   datastruct.fitparams[mind,2].max=p3[0]
;;   datastruct.fitsigma[mind,0].max=s1[0]
;;   datastruct.fitsigma[mind,1].max=s2[0]
;;   datastruct.fitsigma[mind,2].max=s3[0]
;; ;--------------------------------------------------
;;   ;oplot,time[sp:ep],wave_fits,thick=3
;;   oplot,time.jd[sp+good_ind_pos],wave_fits,thick=3
  
;;   ;--------------------------------------------------
;; ;Do second order polynomial fitting for the wave back edges
;;   print,''
;;   print,'Fitting a second-order polynomial to the wave back edge positions...'
;;   dist=reform(wave_backedge[0:ep-sp].rad)*DIST_FACTOR*height
;;   ;dist=reform(wave_backedge[good_ind_pos].val)*DIST_FACTOR*height
;;   time_good=time[sp:ep].relsec-time[sp].relsec
;;   dist=smooth(dist,2)
;;   ;bootstrap_sdo,dist,time_good,fit_line, p1, p2, p3, s1, s2, s3,parinfo=parinfo
;;   print,''
;;   print,''
;;   wave_fits=p1[0] + p2[0] * (time_good)+ 0.5 * p3[0] * (time_good)^2
;;   wave_fits=wave_fits/DIST_FACTOR/height
;;   datastruct.fitparams[mind,0].back=p1[0]
;;   datastruct.fitparams[mind,1].back=p2[0]
;;   datastruct.fitparams[mind,2].back=p3[0]
;;   datastruct.fitsigma[mind,0].back=s1[0]
;;   datastruct.fitsigma[mind,1].back=s2[0]
;;   datastruct.fitsigma[mind,2].back=s3[0]
;; ;--------------------------------------------------
  ;oplot,time[sp:ep],wave_fits,thick=3
  ;; oplot,time.jd[sp+good_ind_pos],wave_fits,thick=3
  
  
  ;Print out the results of the fitting
;initial position angle/Radial position
  ;r0=yarray[mymaxima[0,sp].ind]
  r0=datastruct.fitparams[mind,0].front/DIST_FACTOR/height
  sr0=datastruct.fitsigma[mind,0].front/DIST_FACTOR/height
  tmpstr=datastruct.kinquantity[0]+' = '+strtrim(string(r0,format='(f9.2)'),2)+datastruct.kinunit[mind,0]     ;+' +/-'+strtrim(string(sr0,format='(f11.3)'),2)+datastruct.kinunit[mind,0]
  datastruct.kinvalue[mind,0].max=r0
  datastruct.kinsigma[mind,0].max=s1[0]
  print,tmpstr
  xyouts,!x.window[0]+xmargin,!P.position[3]-ymargin,tmpstr,/norm,charsize=1.4,color=255
  
;final height
;  rf=yarray[mymaxima[0,ep].ind]
  rf=wave_frontedge[ep-sp].rad
  tmpstr=datastruct.kinquantity[1] +' = '+strtrim(string(rf,format='(f9.2)'),2)+datastruct.kinunit[1]
  datastruct.kinvalue[mind,1].max=rf
  print,tmpstr
  xyouts,!x.window[0]+xmargin,!P.position[3]-2*ymargin,tmpstr,/norm,charsize=1.4,color=255
  
;average speed
  vavg=datastruct.fitparams[mind,1].front
  errvavg=datastruct.fitsigma[mind,1].front

  tmpstr='V!Davg!N = '+strtrim(string(vavg,format='(f9.2)'),2)+datastruct.kinunit[2];' +/-'+strtrim(string(errv0,format='(f9.2)'),2)+datastruct.kinunit[2]
  datastruct.kinvalue[mind,2].max=vavg
  datastruct.kinsigma[mind,2].max=errvavg
  print,tmpstr
  xyouts,!x.window[0]+xmargin,!P.position[3]-3*ymargin,tmpstr,/norm,charsize=1.4,color=255

  
;; ;final speed
;;   accel=datastruct.fitparams[mind,2].front
;;   vf=(v0+accel*(time[ep].relsec-time[sp].relsec))
;;   tmpstr=datastruct.kinquantity[3]+' = '+strtrim(string(vf,format='(f9.2)'),2)+datastruct.kinunit[3]
;;   datastruct.kinvalue[mind,3].max=vf
;;   print,tmpstr
;;   xyouts,!x.window[0]+xmargin,!P.position[3]-4*ymargin,tmpstr,/norm,charsize=1.4,color=255
  
;; ;acceleration
;;   accel=accel
;;   erraccel=datastruct.fitsigma[mind,2].front

;;   tmpstr=datastruct.kinquantity[4]+' = '+strtrim(string(accel,format='(f9.2)'),2)+datastruct.kinunit[4] ;' +/-'+strtrim(string(erraccel,format='(f9.2)'),2)+datastruct.kinunit[4]
;;   datastruct.kinvalue[mind,4].max=accel
;;   datastruct.kinsigma[mind,4].max=erraccel
;;   print,tmpstr
;;   print,''
;;   xyouts,!x.window[0]+xmargin,!P.position[3]-5*ymargin,tmpstr,/norm,charsize=1.4,color=255
endfor

  return

end
;-============================================================================



;+============================================================================
pro aia_annulus_analyze_tangential,event,datapath=datapath,savepath=savepath,$
                                   thrange=thrange,interactive=interactive,wave=wave,rrange=rrange,$
                                   constrain=constrain, gradient=gradient, auto=auto
;PURPOSE:
;Procedure to analyze the speeds of radial and tangential expansion of a
;wave and/or a filament.
;Uses output from aia_annulus_plot.pro, a procedure deprojecting AIA
;data onto a rectangular grid, where the X-axis is latitude along the
;limb, and the Y-axis is radial distance.
;
;CATEGORY:
; AIA/Kinematics
;
;INPUTS:
;      EVENT - load an event information structure
;
;KEYWORDS:
;      DATAPATH:
;      SAVEPATH:
;      THRANGE:
;      INTERACTIVE:
;      WAVE:
;
;OUTPUTS:
;
; ;DEPENDENCIES:
;
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 08/07/2013
;   2013/11/19, Kamen Kozarev - Integrated the event structure,
;               updated and streamlined the procedure

;Restore the file with the deprojected data
  date=event.date
  
  if not keyword_set(wave) then wav='193' else wav=wave
  if not keyword_set(savepath) then savepath=event.savepath+'annulusplot/' ;'./'
  if not keyword_set(datapath) then datapath=savepath ;'./'

  fname=event.annplot.savename+wav+'.sav'
  restore, datapath+fname
  ;Create the time structure
  time=get_time(ind_arr.date_obs)
  nsteps=n_elements(subprojdata[*,0,0])
  ncols=n_elements(subprojdata[0,*,0])
  nrows=n_elements(subprojdata[0,0,*])
  projdata=0.0
  RSUN=ind_arr[0].rsun_ref/1000. ;Solar radius in km.
  KMPX=ind_arr[0].IMSCL_MP*ind_arr[0].RSUN_REF/(1000.0*ind_arr[0].RSUN_OBS)
  
  if not keyword_set(interactive) then begin
     nsteps=n_elements(subprojdata[*,0,0])
     ncols=n_elements(subprojdata[0,*,0])
     nrows=n_elements(subprojdata[0,0,*])
  endif
  
;Pixel coordinates in arcseconds from the center of the sun. How do I convert them to km?
  y_arcsec_array=(res/ind_arr[0].cdelt1*findgen(nrows)+r_in)
  y_rsun_array=y_arcsec_array/ind_arr[0].rsun_obs  ;*event.geomcorfactor
  y_km_array = y_rsun_array * RSUN

;The X-angular array (distance along the limb from the pole).
  if keyword_set(interactive) then x_deg_array=findgen(ncols)*ang_step $
  else x_deg_array=findgen(ncols)*ang_step+thrang[0]*180./!PI
  
;The relative times
  DT=anytim(ind_arr[1].date_obs)-anytim(ind_arr[0].date_obs) ;The imaging cadence
  
;  rad_times=findgen(nsteps)*dt/60./60.
;  lat_times=findgen(nsteps)*dt/60.
  
  set_plot,'x'
  loadct,0,/silent
  tvlct,ct_rr,ct_gg,ct_bb,/get
  !p.font=-1
  !P.position[3]=0.8
  !p.position[1]=0.1
  !P.background=255
  !P.color=0
  !P.charsize=1.6
  wdef,0,1200,800
  if keyword_set(interactive) then begin
  
     img=reform(projdata[fix(nsteps/2),*,*]-projdata[fix(nsteps/2)-1,*,*])
     plot_image, img, xtitle = '!5Theta [degrees from solar north]', $
                 ytitle = '!5Radius [arcsec from Sun center]', $
                 title = 'Annulusplot, AIA/'+wav+' '+date, max = 50, $
                 origin = [0,0], charthick = 1.2, charsize=3, $
                 scale = [ang_step, res/ind_arr[0].cdelt1], $
                 pos = [0.1, 0.1, 0.95, 0.95], min = -40
  endif else begin
     
     img=reform(subprojdata[fix(nsteps/3),*,*]-subprojdata[fix(nsteps/3)-1,*,*])
     plot_image, img, xtitle = '!5Theta [degrees from solar north]', $
                 ytitle = '!5Radius [arcsec from Sun center]', $
                 title = 'Annulusplot, AIA/'+wav+' '+date, max = 50, $
                 origin = [thrang[0]*180./!PI,r_in], charthick = 1.2, charsize=3,$
                 scale = [ang_step, res/ind_arr[0].cdelt1], $
                 pos = [0.1, 0.1, 0.95, 0.95], min = -40, font_name='Hershey 5'
  endelse
  
  
  htlimits=aia_rad_height_limits(degarray=x_deg_array)
  oplot,x_deg_array,htlimits,color=255,thick=2;,/data
  
  limb=ind_arr[0].rsun_obs ;Limb position in arcsec
  limbind=min(where(y_arcsec_array ge limb))
  ;oplot,thrang*180./!PI,[limb,limb],linestyle=2,thick=2,color=255 ;Overplot the limb position
  
  if not keyword_set(thrange) then begin
     if keyword_set(interactive) then begin
        print,''
        print,'On the image, select the MIN(Theta) to include'
        cursor,x,y,/down,/data
        oplot,[x,x],[0,nrows-1],thick=2
        thrang[0]=x
        print,x
        
        print,''
        print,'On the image, select the MAX(Theta) to include'
        cursor,x,y,/down,/data
        oplot,[x,x],[0,nrows-1],thick=2
        thrang[1]=x
        print,x
     endif    
  endif else begin
     thrang=thrange
  endelse
  
  if not keyword_set(interactive) then begin
     if not keyword_set(centerlat) then begin
        if event.arlon lt 0.0 then arlat=270.0+event.arlat $
        else arlat=90.0-event.arlat
     endif else arlat=centerlat
     
     ;Find the radial outward limit/edge for which there's data
     yradlimit=aia_rad_height_limits(angle=arlat)
     yradlimind=min(where(y_arcsec_array ge yradlimit))
     
     oplot,[arlat,arlat],[ind_arr[0].rsun_obs,yradlimit],thick=2

     

     ;The index of the AR latitude
     arxcentind=min(where(x_deg_array ge arlat))
     
     lat_heights=(limb+[0.25,0.45,0.65]*(yradlimit-limb))/ind_arr[0].rsun_obs;*event.geomcorfactor
     ;print,lat_heights
     nlatmeas=n_elements(lat_heights)
     htind=intarr(nlatmeas)
     goodlats=fltarr(nlatmeas,ncols)-10.
     for ii=0,nlatmeas-1 do begin
        ;Find the indices of the tangential measurement heights
        htind[ii]=min(where(y_rsun_array ge lat_heights[ii]))
        
        ;Find where the lat_heights are larger than the radial limits 'htlimits'.
        tt=where(htlimits-lat_heights[ii]*ind_arr[0].rsun_obs ge 0.)
        if tt[0] ne -1 then goodlats[ii,tt]=y_arcsec_array[htind[ii]]
        plots,x_deg_array,goodlats[ii,*],psym=1,thick=1,symsize=0.1
        ;Overplot the tangential measurements location
        ;oplot,thrang*180./!PI,[y_arcsec_array[htind[ii]],y_arcsec_array[htind[ii]]],thick=2
     endfor
     
     
    
  endif else begin
     print,''
     print,'On the image, select the latitude of the AR center:'
     cursor,x,y,/down,/data
     oplot,[x,x],[0,nrows-1],thick=2
     print,x
     arcenter=x
     
     uinput=0.0
     while uinput le 0.0 or uinput gt 5 do begin
        read,uinput,prompt='How many heights for tangential measurements (5 max)? '
        wait,0.1
     endwhile
     nlatmeas=uinput
     if nlatmeas gt 5 then nlatmeas=5
     ;Arrays that hold the measurement heights/indices thereof
     lat_heights=dblarr(nlatmeas)
     htind=dblarr(nlatmeas)
     goodlats=fltarr(nlatmeas,ncols)-10.

     for ii=0,nlatmeas-1 do begin
        print,''
        print,'Select height for tangential measurement #'+strtrim(string(ii+1),2)
        cursor,x,y,/down,/data
        print,y
        ;oplot,[0,ncols-1],[y,y],thick=2
        lat_heights[ii]=y
        htind[ii]=min(where(yangular_array ge lat_heights[ii]))

        ;Find where the lat_heights are larger than the radial limits 'htlimits'.
        tt=where(htlimits-lat_heights[ii]*ind_arr[0].rsun_obs ge 0.)
        if tt[0] ne -1 then goodlats[ii,tt]=y_arcsec_array[htind[ii]]
        plots,x_deg_array,goodlats[ii,*],psym=1,thick=1,symsize=0.1

     endfor
     
  endelse
  ;The middle height index
  midind=fix(median(indgen(nlatmeas)))
;Save the overview plot
  write_png,savepath+'annplot_'+date+'_'+event.label+'_'+wav+'_overview_plot.png',tvrd(/true),ct_rr,ct_gg,ct_bb
  wdel,0
  
  if keyword_set(interactive) then begin
     subfovind=where(new_theta[*,0] ge thrang[0]*!PI/180.0 and new_theta[*,0] le thrang[1]*!PI/180.0)
     plotimg=img[subfovind,*]
     
;  plot_image, plotimg, xtitle = '!5Theta [degrees from solar north]', $
;              ytitle = '!5Radius [arcsec from Limb]', $
;              charsize = 1.5, title = 'AIA deprojected image', max = 50, $
;              origin = [thrang[0]*180./!PI,r_in], charthick = 1.1, $
;              scale = [ang_step, res/ind_arr[0].cdelt1], $
;              pos = [0.1, 0.1, 0.95, 0.95], min = -40
     
     
;The index of the AR center in the SUB-FOV images
     arxcentind=min(where(new_theta[*,0] ge (arlat-thrang[0])*!PI/180.0))
     
;Get just the data to analyze
     subfovdata=projdata[*,subfovind,*]
     subncols=n_elements(subfovdata[0,*,0])
     andata=projdata[*,subfovind,htind]
  endif else begin
     subfovdata=subprojdata
     subncols=ncols
     andata=subprojdata[*,*,htind]
  endelse
  
  lat_deg_array=findgen(arxcentind)*ang_step ;The tangential angle positions  
  
;DEBUG
  ;make sure there are no negative values in the data.
  ind=where(subfovdata le 0.0)
  if ind[0] ne -1 then subfovdata[ind]=1.0e-10 ;Set to very nearly zero  
;DEBUG  

;Set the Tangential data
;  tmp=subfovdata[*,*,htind]
  tmpdata=subfovdata[*,*,htind]  
  for ii=0,nlatmeas-1 do begin
;To improve the S/N for tangential data, sum five rows of data
;for each height.
     tmp=subfovdata[*,*,htind[ii]-2:htind[ii]+2]
     ;Set the pixels that are outside the detector FOV
     tt=where(goodlats[ii,*] lt 0.)
     ;if tt[0] ne -1 then tmp[*,tt,ii]=-1.0e-10
     if tt[0] ne -1 then tmp[*,tt,*]=1.0e-10
     tmpdata[*,*,ii]=total(tmp,3)
  endfor
  ;left_lat_data=reform(tmp[*,0:arxcentind-1,*])
  ;right_lat_data=reform(tmp[*,arxcentind:subncols-1,*])
  left_lat_data=reform(tmpdata[*,0:arxcentind-1,*]) 
  right_lat_data=reform(tmpdata[*,arxcentind:subncols-1,*])


  
  ;Save the Tangential data into a structure
  lat_data_left={$
                type:'tangential',$
                lat_heights:lat_heights,$
                wav:wav,$
                data:left_lat_data,bdiff:left_lat_data,winind:1,$
                savename:'annplot_'+date+'_'+event.label+'_'+wav+'_tangential.png',$
                plotinfo:replicate({p:!P, x:!X, y:!Y},nlatmeas),$
                kinquantity:['Th!D0!N','Th!D1!N','V!Dth,0!N','V!Dth,1!N','a'],$
                kinunit:['!Uo!N','!Uo!N',' km/s',' km/s',' km/s!U2!N'],$
                kinvalue:replicate({front:0.0, max:0.0, back:0.0},nlatmeas,5),$
                kinsigma:replicate({front:0.0, max:0.0, back:0.0},nlatmeas,5),$
                fitparams:replicate({front:0.0, max:0.0, back:0.0},nlatmeas,3),$
                fitsigma:replicate({front:0.0, max:0.0, back:0.0},nlatmeas,3),$
                maxinds:intarr(nlatmeas,nsteps),$
                frontinds:intarr(nlatmeas,nsteps),$
                backinds:intarr(nlatmeas,nsteps),$
                time:time.relsec,$
                xfitrange:[0,0],yfitrange:[0,arxcentind-1],$ ;The indices of the start/end fitting times/heights
                scale:[dt/60.,ang_step],origin:[0,0],difforigin:[0,0],$
                xtitle:'Minutes since start',ytitle:'Degrees from AR center',$
                imgtit:strarr(nlatmeas),$
                winsize:[1200,800],multi:[0,nlatmeas,1]$
                }
  for rr=0,nlatmeas-1 do $
     if rr eq midind then $
        lat_data_left.imgtit[rr]='AIA/'+wav+' Left Tangential Positions | Start at '+ind_arr[0].date_obs+'!C R = '+$
     strtrim(string(lat_heights[rr],format='(f5.3)'),2)+' R!Ds!N' $
     else lat_data_left.imgtit[rr]='R = '+ strtrim(string(lat_heights[rr],format='(f5.3)'),2)+' R!Ds!N'

  
  lat_data_right={$
                 type:'tangential',$
                 wav:wav,$
                 lat_heights:lat_heights,$
                 data:right_lat_data,bdiff:right_lat_data,winind:2,$
                 savename:event.annplot.analyzed_savename+wav+'_tangential_right.png',$
                 plotinfo:replicate({p:!P, x:!X, y:!Y},nlatmeas),$
                 kinquantity:['Th!D0!N','Th!D1!N','V!Dth,0!N','V!Dth,1!N','a'],$
                 kinunit:['!Uo!N','!Uo!N',' km/s',' km/s',' km/s!U2!N'],$
                 kinvalue:replicate({front:0.0, max:0.0, back:0.0},nlatmeas,5),$
                 kinsigma:replicate({front:0.0, max:0.0, back:0.0},nlatmeas,5),$
                 fitparams:replicate({front:0.0, max:0.0, back:0.0},nlatmeas,3),$
                 fitsigma:replicate({front:0.0, max:0.0, back:0.0},nlatmeas,3),$
                 maxinds:intarr(nlatmeas,nsteps),$
                 frontinds:intarr(nlatmeas,nsteps),$
                 backinds:intarr(nlatmeas,nsteps),$
                 time:time.relsec,$
                 xfitrange:[0,0],yfitrange:[0,arxcentind-1],$
                 scale:[dt/60.,ang_step],origin:[0,0],difforigin:[0,0],$
                 xtitle:'Minutes since start',ytitle:'Degrees from AR center',$
                 imgtit:strarr(nlatmeas),$
                 winsize:[1200,800],multi:[0,nlatmeas,1]$
                 }
  for rr=0,nlatmeas-1 do $
     if rr eq midind then $
        lat_data_right.imgtit[rr]='AIA/'+wav+' Right Tangential Positions | Start at '+ind_arr[0].date_obs+'!C R = '+$
     strtrim(string(lat_heights[rr],format='(f5.3)'),2)+' R!Ds!N' $
     else lat_data_right.imgtit[rr]='R = '+strtrim(string(lat_heights[rr],format='(f5.3)'),2)+' R!Ds!N'

;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;
;JMAP PLOTTING
;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;
      
  sp=56
  ep=90
    ;Record the start/end time indices in the data structures
  lat_data_left.xfitrange=[sp,ep]
  lat_data_right.xfitrange=[sp,ep]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Plot the Left Tangential Data!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  !p.multi=lat_data_left.multi
  tmpdata=lat_data_left.data
 
  nbase=5
  base=total(tmpdata[0:nbase-1,*,*],1)/(nbase*1.0)
  for tt=0,nsteps-1 do tmpdata[tt,*,*]=reform(tmpdata[tt,*,*]-base)
 
;Plot the Left Tangential positions
  ;wdef,lat_data_left.winind,lat_data_left.winsize[0],lat_data_left.winsize[1]
  for rr=0,nlatmeas-1 do begin
     tmp=reform(tmpdata[*,*,rr])
     ;REVERSE THE IMAGE COLUMNS SO THE AR IS ALWAYS IN THE BOTTOM
     tmp=reverse(tmp,2)
     ;DESPIKE THE IMAGE
     tmp=despike_gen(tmp)
     tmpdata[*,*,rr]=tmp
     lat_data_left.bdiff[*,*,rr]=tmp
     ;glind=where(goodlats[rr,*] ge 0.0)   
     
    ; dat=reform(tmp)
    ; dat2=gmascl(dat,gamma=4)
    ; dat2=double(dat2)
  endfor
;Everything else is here
  annulus_fit_maxima_tangential,event,lat_data_left.bdiff,lat_data_left,time,lat_deg_array,$
                                constrain=constrain, auto=auto, gradient=gradient, y_rsun_array=y_rsun_array
  lat_data_left.plotinfo[0].p=!P
  lat_data_left.plotinfo[0].x=!X
  lat_data_left.plotinfo[0].y=!Y
  write_png,savepath+lat_data_left.savename,tvrd(/true),ct_rr,ct_gg,ct_bb
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Plot the Right Tangential Data!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;!p.multi=lat_data_right.multi
  tmpdata=lat_data_right.data
  nbase=5
  base=total(tmpdata[0:nbase-1,*,*],1)/(nbase*1.0)
  for tt=0,nsteps-1 do tmpdata[tt,*,*]=reform(tmpdata[tt,*,*]-base)
  
;Plot the Right Tangential positions
  ;wdef,lat_data_right.winind,lat_data_right.winsize[0],lat_data_right.winsize[1]
  for rr=0,nlatmeas-1 do begin
     tmp=reform(tmpdata[*,*,rr])
     ;DESPIKE THE IMAGE
     tmp=despike_gen(tmp)
     lat_data_right.bdiff[*,*,rr]=tmp
     
     ;dat=reform(tmp)
     ;dat2=gmascl(dat,gamma=4)
     ;dat2=double(dat2)
  endfor
     ;Everything else is here

  annulus_fit_maxima_tangential,event,lat_data_right.bdiff,lat_data_right,time,lat_deg_array,$
                                constrain=constrain, auto=auto, gradient=gradient, y_rsun_array=y_rsun_array
                                
  lat_data_right.plotinfo[0].p=!P
  lat_data_right.plotinfo[0].x=!X
  lat_data_right.plotinfo[0].y=!Y
  write_png,savepath+lat_data_right.savename,tvrd(/true),ct_rr,ct_gg,ct_bb
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  save,filename=savepath+event.annplot.analyzed_savename+wav+'_analyzed_tangential.sav',lat_data_left,lat_data_right,ind_arr,nsteps,$
         ncols,nrows,wav,htind,yradlimind,lat_deg_array,y_arcsec_array,$
         y_rsun_array,rad_times,time
 end





;-============================================================================

