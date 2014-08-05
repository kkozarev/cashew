pro filter_tangential_data, data, time, yarray, yrng=yrng, maxYInd=maxYInd, maxFrontEdge=maxFrontEdge

dat=data

ind = where(dat lt 0.0)
if ind[0] gt -1 then dat[ind] = 0.0

totalAnglePixVals = dblarr(n_elements(yarray))
for tt=0, n_elements(yarray)-1 do begin
   totalAnglePixVals[tt] = total(dat[*,tt])
endfor

smoothAnglePixVals = smooth(totalAnglePixVals, 50, /edge_truncate)
tmp = lindgen(n_elements(smoothAnglePixVals))

;cgPlot, smoothAnglePixVals, yarray, /window

gfit = gaussfit(tmp, smoothAnglePixVals, coeff, nterms=6)

cgPlot, tmp, smoothAnglePixVals, /window
cgPlot, tmp, gfit, col='blue', /window, /overPlot

gaussHeight = coeff[0]
gaussCenter = coeff[1]
gaussStdev = coeff[2]

dataCutoff = gaussCenter+2.5*gaussStdev
dataCutoff = round(dataCutoff)

frontEdgeCutoff = gaussCenter+1.75*gaussStdev

maxFrontEdge = round(frontEdgeCutoff)
maxYInd = dataCutoff



;; maxima = get_local_maxima(smoothAnglePixVals, yarray)
;; minind = lclxtrem(smoothAnglePixVals-smooth(smoothAnglePixVals, 20, /edge_truncate), 10)

;; trueMax  = maxima[0]

;; upperMaxInd = where(maxima.rad gt 30)
;; upperMaxima = maxima[upperMaxInd]
;; upperMax = upperMaxima[0]

;; if upperMax.val gt 750 then begin
;;    upperMinind = where(minind gt trueMax.ind)
;;    minind = minind[upperMinind]
;;    upperMinind = where(minind lt upperMax.ind)
;;    minind = minind[upperMinind]
;;    upperMinInd = where(smoothAnglePixVals[minind] eq min(smoothAnglePixVals[minind])) 
;;    upperMin = minind[upperMinInd]

;;    print, "Cutting off data at: "
;;    print, yarray[upperMin]

;;    yrng[1] = upperMin
;; endif


end
