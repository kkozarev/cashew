pro aia_plot_jmap_data,times,rad,data,xrange=xrange,yrange=yrange,ct=ct,min=min,max=max,fitrange=fitrange,_extra=extra
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
       ytickformat='(f4.2)',$
       xtickformat='LABEL_DATE',$
       xtickunit='Time',xtitle='',ytitle='',$
       /nodata,_extra=extra,$
       xstyle=1,ystyle=1,$
       charsize=3,charthick=2,$
       xticks=6,yticks=6,color=255
;       ytitle=ytitle,$
;       title=title,$
  
  for t=0,nt-2 do begin
     xmin=times[t]
     xmax=times[t+1]
     for r=0,nr-2 do begin
        ymin=rad[r]
        ymax=rad[r+1]
        color=255.0*(data[t,r]-min)/(max-min) ; Scale Colors
        if color lt 0 then color = 0
        if color gt 255 then color = 255
        polyfill,[xmin,xmax,xmax,xmin],[ymin,ymin,ymax,ymax],color=color,/data
        
     endfor
  endfor
  
  xyouts,!P.position[0]+0.15,!P.position[3]+0.02,extra.title,color=0,charsize=3,/norm
  AXIS, YAXIS=0,YRANGE=yrange, /SAVE, color=0,ythick=3,yticks=6,charsize=3,_extra=extra,ystyle=1,ytickformat='(f4.2)'
  AXIS, YAXIS=1,YRANGE=yrange, /SAVE, color=0,ythick=3,yticks=6,ytickformat='(A1)',ystyle=1
  AXIS, XAXIS=0,XRANGE=xrange, /SAVE, color=0,xthick=3,xticks=6,xtickformat='LABEL_DATE',xtickunit='Time',charsize=3,_extra=extra,xstyle=1
  AXIS, XAXIS=1,XRANGE=xrange, /SAVE, color=0,xthick=3,xticks=6,xtickformat='(A1)',xstyle=1
 
  if keyword_set(fitrange) then begin
     print,''
     print,'Select starting point:'
     cursor,x,y,/down,/data
     ;plots,x,y,psym=5,symsize=2,thick=2,color=100
     sp=min(where((times-x) gt 0.0))
     oplot,[times[sp],times[sp]],yrange,color=255
     
     
     print,'Select ending point:'
     cursor,x,y,/down,/data
     ;plots,x,y,psym=5,symsize=2,thick=2,color=200
     ep=min(where((times-x) gt 0.0))
     oplot,[times[ep],times[ep]],yrange,color=255

     fitrange=[sp,ep]
  endif
;endif else begin
;     sp=min(where((rad_times-timerange[0]) gt 0.0))
;     ep=min(where((rad_times-timerange[1]) gt 0.0))
;     oplot,[rad_times[sp],rad_times[sp]],[y_rsun_array[htind[0]],y_rsun_array[yradlimind]],color=255
;     oplot,[rad_times[ep],rad_times[ep]],[y_rsun_array[htind[0]],y_rsun_array[yradlimind]],color=255 
;  endelse


end
