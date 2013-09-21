pro yaftawave_fit_circle, index,data,inmasks,infeatures,label
;The procedure will determine to which points to fit circles.
;Kamen Kozarev - 09/2011
  ;ARc=[index[0].arx0,index[0].ary0]
  nframes=n_elements(inmasks[0,0,*])

  ;get the coordinates of the farthest pixels from the AR center...
  ;farpix=yaftawave_return_farthest_pixels(inmasks, label, ARc,numpoints=10)
  ;farpix=yaftawave_return_extreme_pixels(inmasks, label, ARc,numpoints=10)

  ;Extract the feature, for cleaner plotting
  yaftawave_extract_features,inmasks,infeatures,label,outmasks,outfeatures
 ;The loop over all frames
  for nf=0,nframes-1 do begin
     mm=smooth(reform(outmasks[*,*,nf]),10)
     mm[where(mm gt 0.0)]=label
     outmasks[*,*,nf]=mm
     
     tvscl,data[*,*,nf]
     tvmask,outmasks[*,*,nf],label

     ; Determine the points, to which to
     ; fit the circle. The idea is to make
     ; this iteratively, until the farthest N pixels remain...
     ;Loop over some number of steps, increment by 10 pixels or so.
;Search from the outside, start from the solar radius in pixels, and
;reduce the radius, checking every shell between consecutive circles
;whether there are a minimum of N pixels there. The first shell where
;there are, take the first N pixels you can find, and fit a circle,
;then the next N pixels, fit a circle, etc. Then, compute a
;chi-squared for every fit and find the best one...


     findextremepixels,outmasks[*,*,nf],label,excols
     xx=reform(excols[0,*])
     ;xx=xx[0:99]
     yy=reform(excols[1,*])
     ;yy=yy[0:99]

     ;xx=reform(farpix[0,*,nf])
     ;yy=reform(farpix[1,*,nf])
     aia_autofit_circle,xx,yy,circpos,pp
     print, pp
     plots,circpos[0,*],circpos[1,*],/device,thick=3
     wait,0.4
  endfor

;stop





end
