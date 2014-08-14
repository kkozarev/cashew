pro bin_aia_data, time, yarray, data, yrng, binData=binData


binData = data

; Compute thresholds from background average
backgroundMean = mean(data[0:5, 100:n_elements(data[0,*])-1])
backgroundStdev = stddev(data[0:5, 100:n_elements(data[0,*])-1])

print, "Background average is: "
print, backgroundMean

print, "Background stdev is"
print, backgroundStdev


;; for t=0, n_elements(data[*,0])-1 do begin
;;    for r=0, n_elements(data[0,*])-1 do begin
;;       bindata[t,r] = 0.0
;;       if data[t, r] lt 0.0 then begin
;;          binData[t, r] = -10.0
;;       endif else if data[t, r] gt 50.0 && data[t,r] lt 75.0 then begin
;;          binData[t,r] = 25.0
;;       endif else if data[t,r] gt 75.0 then begin
;;          binData[t,r] = 150.0
;;       endif
;;    endfor
;; endfor 

firstBin = backgroundMean +3*backgroundStdev
secondBin = backgroundMean + 4*backgroundStdev
thirdBin = backgroundMean + 6*backgroundStdev
fourthBin = backgroundMean + 8*backgroundStdev


for t=0, n_elements(data[*,0])-1 do begin
   for r=0, n_elements(data[0,*])-1 do begin
      bindata[t,r] = 0.0
      if data[t, r] lt backgroundMean then begin
         binData[t, r] = -1.0
      endif else if data[t, r] gt backgroundMean && data[t,r] lt firstBin then begin
         binData[t,r] = 1.0
      endif else if data[t,r] gt firstBin && data[t,r] lt secondBin then begin
         binData[t,r] = 5.0
      endif else if data[t,r] gt secondBin && data[t,r] lt thirdBin then begin
         binData[t,r] = 15.0
      endif else if data[t,r] gt thirdBin && data[t,r] lt fourthBin then begin
         binData[t,r] = 35.0
      endif else begin
         binData[t,r] = 70.0
      endelse 
   endfor
endfor 


;; min=-40
;; max=50


;; nt=n_elements(time)
;; nr=n_elements(yarray)
;; yrange=[min(yarray),max(yarray)]

;;   date_label=label_date(DATE_FORMAT='%H:%I')
;;   if not keyword_set(title) then title='110511 Binned AIA 193 Tangential Data at R = 1.086'
;;   if not keyword_set(xtitle) then xtitle='Time (UT)'
;;   if not keyword_set(ytitle) then ytitle='Degrees from Active Region'

;;   xrange=[time[0],time[nt-1]]
;;   plot,time,yarray,$
;;        xrange=xrange,$
;;        yrange=yrange,$
;;        ytickformat='(f6.2)',$
;;        xtickformat='LABEL_DATE',$
;;        xtickunit='Time',xtitle=xtitle,ytitle=ytitle,$
;;        /nodata,$
;;        xstyle=1,ystyle=1,$
;;        charsize=3,charthick=2,$
;;        xticks=6,yticks=6,color=255

;;   for t=0,nt-2 do begin
;;      xmin=time[t]
;;      xmax=time[t+1]
;;      for r=0,nr-2 do begin
;;         ymin=yarray[r]
;;         ymax=yarray[r+1]
;;         color=255.0*(binData[t,r]-min)/(max-min) ; Scale Colors                                         
;;                                 ;color=data[t,r]                                                                                                                                                                                     
;;         if color lt 0 then color = 0
;;         if color gt 255 then color = 255
;;         polyfill,[xmin,xmax,xmax,xmin],[ymin,ymin,ymax,ymax],color=color,/data

;;      endfor
;;   endfor

;;   xyouts,!P.position[0]+0.02,!P.position[3]+0.02, title,color=0,charsize=3,/norm
;;    AXIS, YAXIS=0,YRANGE=yrange, /SAVE, color=0,ythick=3,yticks=6,charsize=3,_extra=extra,ystyle=1,ytickformat='(f6.2)', ytitle=ytitle
;;    AXIS, YAXIS=1,YRANGE=yrange, /SAVE, color=0,ythick=3,yticks=6,ytickformat='(A1)',ystyle=1
;;    AXIS, XAXIS=0,XRANGE=xrange, /SAVE, color=0,xthick=3,xticks=6,xtickformat='LABEL_DATE',xtickunit='Time',charsize=3,_extra=extra,xstyle=1, xtitle=xtitle
;;    AXIS, XAXIS=1,XRANGE=xrange, /SAVE, color=0,xthick=3,xticks=6,xtickformat='(A1)',xstyle=1

;  cgImage, binData

end
