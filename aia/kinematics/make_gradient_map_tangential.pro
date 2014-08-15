pro make_gradient_map_tangential, times, rad, data, yrng, intensityData=intensityData
 
;PURPOSE
;Compute the average x-gradient over a specified number of points
;from data passed in through the data variable. Generally the data
;needs to be binned using bin_aia_data before taking the gradient
;
;INPUTS
;     DATA - annulus data from aia_annulus_analyze_radial.pro
;     TIMES - array of times to corresponding annulus data
;     RAD - array of radii used in the data
;     YRNG - array of indices indicating range of valid rad data
;OUTPUTS
;     INTENSITYDATA - array containing computed gradient scores

 
  ; Just take gradient in the x direction for tangential plots

  intensityData = dblarr(n_elements(times), n_elements(rad))

  ; Select a number of points for a window to average over
  nPts = 10
  window = indgen(nPts)
  start = nPts

  for t=1, n_elements(data[*,0])-1 do begin
     for r=0, n_elements(data[0,*])-1  do begin
        meanDiff = 0
        currentPixel = data[t, r]
        skip = 0
        ; Take surrounding pixels and create an intensity score
        if t eq 0 then begin
           ; Average over the specified window
           for i = 0, n_elements(window)-1 do begin
              rightDiff = abs(data[t+i,r]-data[t+i+1,r])
              print, rightDiff
              meanDiff = meanDiff+rightDiff
           endfor
           meanDiff = meanDiff / n_elements(window)
           intensityData[t,r] = meanDiff
        endif else if t eq n_elements(times)-1 then begin 
           ; Average over the specified window
           for i = 0, n_elements(window)-1 do begin
              leftDiff = abs(data[t,r]-data[t-i-1, r])
              meanDiff = meanDiff+leftDiff
           endfor
           meanDiff = meanDiff / n_elements(window)
           intensityData[t,r] = meanDiff
        endif else begin
           nIterations = 0
           meanDiff = 0
           ; Average over the specified window
           for i=0, n_elements(window)-1 do begin
              if t - i le 1 then break
              nIterations++
              leftDiff = abs(data[t,r]-data[t-i-1,r])
              meanDiff = meanDiff + leftDiff
           endfor
           meanDiff = meanDiff / nIterations
           intensityData[t,r] = meanDiff
        endelse

     endfor
  endfor

  intensityData = intensityData
  cgimage, intensityData[*, yrng[0]:yrng[1]]
  plotData = intensityData[*, yrng[0]:yrng[1]]

;; min=-40
;; max=50


;; nt=n_elements(times)
;; nr=n_elements(rad)
;; yrange=[min(rad),max(rad)]

;;   date_label=label_date(DATE_FORMAT='%H:%I')
;;   if not keyword_set(title) then title='110511 AIA 193 Gradient Data at R = 1.086'
;;   if not keyword_set(xtitle) then xtitle='Time (UT)'
;;   if not keyword_set(ytitle) then ytitle='Degrees from Active Region'

;;   xrange=[times[0],times[nt-1]]
;;   plot,times,rad,$
;;        xrange=xrange,$
;;        yrange=yrange,$
;;        ytickformat='(f6.2)',$
;;        xtickformat='LABEL_DATE',$
;;        xtickunit='Times',xtitle=xtitle,ytitle=ytitle,$
;;        /nodata,$
;;        xstyle=1,ystyle=1,$
;;        charsize=3,charthick=2,$
;;        xticks=6,yticks=6,color=255

;;   for t=0,nt-2 do begin
;;      xmin=times[t]
;;      xmax=times[t+1]
;;      for r=0,nr-2 do begin
;;         ymin=rad[r]
;;         ymax=rad[r+1]
;;         color=255.0*(plotData[t,r]-min)/(max-min) ; Scale Colors                                                                                                   
;;                                 ;color=data[t,r]
;;                                 ;\
                                                                                                                                                                  
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
 
;; stop

end
         
