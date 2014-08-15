pro bin_aia_data, time, yarray, data, yrng, binData=binData
  
;PURPOSE                                                                                             
;Procedure to assign AIA data to constant valued bins in order
;to enable the gradient code to work correctly. Bins are determined
;by computing a quiet average in the upper left corner of the data
;and basing the bins off of number of standard deviations above
;the mean                                                                                                    
;INPUTS 
;     DATA - annulus data from aia_annulus_analyze_radial.pro                                         
;     TIME - array of times to corresponding annulus data                                             
;     YARRAY - array of y-axis used in the kinematics data
;     YRNG - indices indicating the current range of valid yarray data
;
;OUTPUTS                                                                                              
;     BINDATA - Binned version of DATA ready for gradient



binData = data

; Compute thresholds from background average in upper
; left corner over 5 time steps
backgroundMean = mean(data[0:5, 100:n_elements(data[0,*])-1])
backgroundStdev = stddev(data[0:5, 100:n_elements(data[0,*])-1])

print, "Background average is: "
print, backgroundMean

print, "Background stdev is"
print, backgroundStdev

firstBin = backgroundMean +3*backgroundStdev
secondBin = backgroundMean + 4*backgroundStdev
thirdBin = backgroundMean + 6*backgroundStdev
fourthBin = backgroundMean + 8*backgroundStdev

; Assign data to corresponding bins
; The assigned values were chosen somewhat arbitrarily, but with the
; general scale in mind that small gradients should be valued less 
; than large gradients. There might be a better way to do this. 
; Ultimately, pixel intensities exceeding 8 standard deviations 
; should receive a high value after binning the data to ensure
; the corresponding gradient value is similarly high. Data near the
; mean should not have large gradients and are thus assigned smaller
; differences between each bin
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

; Plot binned data and return
cgImage, binData

end
