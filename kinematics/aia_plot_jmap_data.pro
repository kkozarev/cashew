pro aia_plot_jmap_data,times,rad,data,xrange=xrange,yrange=yrange,ct=ct,min=min,max=max,fitrange=fitrange,title=title,_extra=extra,startInd=startInd,endInd=endInd, auto=auto
!P.position=[0.15,0.15,0.94,0.9]
;This procedure will plot the jmap-extracted profiles with x- and y-axes
  if keyword_set(ct) then loadct,ct,/silent
  nt=n_elements(times)
  nr=n_elements(rad)
  yrange=[min(rad),max(rad)]

  date_label=label_date(DATE_FORMAT='%H:%I')
  if not keyword_set(title) then title=''
  if not keyword_set(xtitle) then xtitle=''
  if not keyword_set(ytitle) then ytitle=''
  
  xrange=[times[0],times[nt-1]]
  plot,times,rad,$
       xrange=xrange,$
       yrange=yrange,$
       ytickformat='(f6.2)',$
       xtickformat='LABEL_DATE',$
       xtickunit='Time',xtitle='',ytitle='',$
       /nodata,_extra=extra,$
       xstyle=1,ystyle=1,$
       charsize=3,charthick=2,$
       xticks=6,yticks=6,color=255
  
  for t=0,nt-2 do begin
     xmin=times[t]
     xmax=times[t+1]
     for r=0,nr-2 do begin
        ymin=rad[r]
        ymax=rad[r+1]
        color=255.0*(data[t,r]-min)/(max-min) ; Scale Colors
        ;color=data[t,r]
        if color lt 0 then color = 0
        if color gt 255 then color = 255
        polyfill,[xmin,xmax,xmax,xmin],[ymin,ymin,ymax,ymax],color=color,/data
        
     endfor
  endfor
  
  xyouts,!P.position[0]+0.15,!P.position[3]+0.02, title,color=0,charsize=3,/norm
  AXIS, YAXIS=0,YRANGE=yrange, /SAVE, color=0,ythick=3,yticks=6,charsize=3,_extra=extra,ystyle=1,ytickformat='(f6.2)'
  AXIS, YAXIS=1,YRANGE=yrange, /SAVE, color=0,ythick=3,yticks=6,ytickformat='(A1)',ystyle=1
  AXIS, XAXIS=0,XRANGE=xrange, /SAVE, color=0,xthick=3,xticks=6,xtickformat='LABEL_DATE',xtickunit='Time',charsize=3,_extra=extra,xstyle=1
  AXIS, XAXIS=1,XRANGE=xrange, /SAVE, color=0,xthick=3,xticks=6,xtickformat='(A1)',xstyle=1
 
  if keyword_set(fitrange) then begin


     ;; yrng=yrange

     ;; totalPixVals=dblarr(nt)
     ;; for tt=0,nt-1 do begin
     ;;    dat=data[tt,yrng[0]:yrng[1]]
     ;;    ind=where(dat gt 0.0)
     ;;    if ind[0] gt -1 then tmp=total(dat[ind]) else tmp=0
     ;;    totalPixVals[tt]=tmp
     ;; endfor
    
     ;; stop
     
     
     loadct, 0, /silent
     
     if ~(keyword_set(auto)) then begin
        print,''
        print,'Select starting point:'
        cursor,x,y,/down,/data
        ;plots,x,y,psym=5,symsize=2,thick=2,color=100
        sp=min(where((times-x) gt 0.0))
        dt=(times[sp]-times[sp-1])/2.
        oplot,[times[sp]-dt,times[sp]-dt],yrange,color=255

        print,'Select ending point:'
        cursor,x,y,/down,/data
        ;plots,x,y,psym=5,symsize=2,thick=2,color=200
        ep=min(where((times-x) gt 0.0))
        dt=(times[ep]-times[ep-1])/2.
        oplot,[times[ep]-dt,times[ep]-dt],yrange,color=255
     endif 
     
     if keyword_set(startInd) then begin
        loadct, 1, /silent
        print, "Plotting automatically detected start position"
        sp = startInd
        dt=(times[sp]-times[sp-1])/2.
        oplot,[times[startInd]-dt,times[startInd]-dt],yrange,thick=3,color=100
     endif
   
     if keyword_set(endInd) then begin
        loadct, 1, /silent
        print, "Plotting automatically detected start position"
        ep = endInd
        dt=(times[ep]-times[ep-1])/2.
        oplot,[times[endInd]-dt,times[endInd]-dt],yrange,thick=3,color=100
     endif

     loadct, 0, /silent

     fitrange=[sp,ep]

  endif
;endif else begin
;     sp=min(where((rad_times-timerange[0]) gt 0.0))
;     ep=min(where((rad_times-timerange[1]) gt 0.0))
;     oplot,[rad_times[sp],rad_times[sp]],[y_rsun_array[htind[0]],y_rsun_array[yradlimind]],color=255
;     oplot,[rad_times[ep],rad_times[ep]],[y_rsun_array[htind[0]],y_rsun_array[yradlimind]],color=255 
;  endelse


end
