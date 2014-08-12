pro find_wave_edge, data, yarray, yrng, time, fitrange, mymaxima, mind,$
                         maxRadIndex, datastruct=datastruct, wave_frontedge=wave_frontedge,$
                         wave_backedge=wave_backedge
;PURPOSE
;Procedure to find the edges of the EUV wave using a Gaussian fit
;At each timestep a Gaussian is fitted to the radial data and the
;wave edges are defined at +/- one sigma
;
;INPUTS
;     DATA - annulus data from aia_annulus_analyze_radial.pro
;     YARRAY - array of radii used in the data
;     YRNG - range through which yarray contains valid data
;     FITRANGE - list containing [startInd, endInd]
;     MYMAXIMA - structure containing maxima within the fitrange
;     TIME - array of times to corresponding annulus data
;
;OUTPUTS
;     WAVE_FRONTEDGE - Structure storing the positions of the front of
;                      the wave
;     WAVE_BACKEDGE - Structure storing the positions of the back of
;                     the wave

  plot = 0
  debug = 0

  loadct, 0

  sp=fitrange[0]
  ep=fitrange[1]

  rad = yarray[yrng[0]:yrng[1]]
  dat = data[*,yrng[0]:yrng[1]]

  nSigma = 1.0

;  cgPlot, yarray, data[0,*]

  ; Determine the wave edge for each timestep within
  ; the region of interest 
  for ii=sp, ep do begin
     
     alt=0

     newtime = time.jd
     caldat, newtime[ii], m, d, y, h, m, s 

     col = dat[ii,*]
     colSmooth = smooth(col, 8, /edge_truncate)     

; Filter out the negative data
     average = mean(colSmooth)
     negInd = where(colSmooth lt 0) 
     if negInd[0] ne -1.0 then begin
        colSmooth[negInd] = average 
     endif
     
     ;gfit1 = gaussfit(rad, colSmooth, coeff, nterms=3)
     ;gfit2 = gaussfit(rad, colSmooth, coeff, nterms=4)
     ;gfit3 = gaussfit(rad, colSmooth, coeff, nterms=5)

     ; Fit a Gaussian to determine front and back edges
     gfit4 = mpfitpeak(rad, colSmooth, coeff, nterms=4)

     if plot eq 1 then begin
        print, h, ":", m
        cgPlot, rad, colSmooth, col='brown'
        
        print, "COEFF"
        print, coeff
        print, "One Sigma"
        print, coeff[1]+nSigma*coeff[2]

        ;cgPlot, rad, gfit1, /overPlot, col='blue'
        ;cgPlot, rad, gfit2, /overPlot, col='green'
        ;cgPlot, rad, gfit3, /overPlot, col='red'
         cgPlot, rad, gfit4, /overPlot, col='cyan'
     endif

     gaussHeight = coeff[0]
     gaussCenter = coeff[1]
     gaussStdev = coeff[2]

     ; If the Gaussian fit is bad, use the searching down from
     ; intensity peak method instead
     if gaussHeight lt 0 then alt=1
     if gaussHeight gt 200 then alt=1

     if alt eq 0 then begin
        ; Calculate front and back edges
        frontEdge = gaussCenter+nsigma*gaussStdev
        backEdge = gaussCenter-nsigma*gaussStdev

        ; Chop off the front edge at the last location of viable data
        if frontEdge gt yarray[maxRadIndex] then frontEdge = yarray[maxRadIndex]        
        if frontEdge lt mymaxima[mind, ii].rad then frontEdge = mymaxima[mind,ii].rad
        
        wave_frontedge[ii-sp].rad = frontEdge
        wave_frontedge[ii-sp].yind = mymaxima[mind,ii].ind
        wave_frontedge[ii-sp].xind = ii

        wave_backedge[ii-sp].rad = backEdge
        wave_backedge[ii-sp].yind = mymaxima[mind, ii].ind
        wave_backedge[ii-sp].xind = ii

        ;; print, "Printing new version backedge"
        ;; print, mymaxima[mind, ii].rad

        if plot eq 1 then begin
           
           print, h, ":", m
           
           cgPlot, [frontEdge, frontEdge], [-10,40], /Overplot
           cgPlot, [backEdge, backEdge], [-10,40], /Overplot

           ;; ;OLD VERSION, SEARCHING DOWN FROM INTENSITY PEAK
           ;; y=reform(datastruct.bdiff[ii,mymaxima[mind,ii].ind:*])
           ;; y=smooth(y,2,/edge_truncate)
           ;; np=n_elements(y)
           ;; tmp=min(where(y le 0.25*max(y)))
           ;; if tmp[0] eq -1 then tmp=np-1
           ;; wave_frontedge[ii-sp].rad=yarray[mymaxima[mind,ii].ind+tmp]
           ;; wave_frontedge[ii-sp].ind=mymaxima[mind,ii].ind+tmp
           ;; datastruct.frontinds[mind,ii]=wave_frontedge[ii-sp].ind
           ;; cgPlot, [wave_frontedge[ii-sp].rad,
           ;; wave_frontedge[ii-sp].rad], [-10,40], col='green',
           ;; /Overplot

        endif
        ; If Gaussian fit fails, use old method
     endif else begin
        ;OLD VERSION, SEARCHING DOWN FROM INTENSITY PEAK
        y=reform(datastruct.bdiff[ii,mymaxima[mind,ii].ind:*])
        y=smooth(y,2,/edge_truncate)
;        cgPlot, y
        np=n_elements(y)
        tmp=min(where(y le 0.25*max(y)))
        if tmp[0] eq -1 then tmp=np-1
        wave_frontedge[ii-sp].rad=yarray[mymaxima[mind,ii].ind+tmp]
        wave_frontedge[ii-sp].yind=mymaxima[mind,ii].ind+tmp
        wave_frontedge[ii-sp].xind=ii
        datastruct.frontinds[mind,ii]=wave_frontedge[ii-sp].yind
        
        if wave_frontedge[ii-sp].rad lt mymaxima[mind, ii].rad then begin
           wave_frontedge[ii-sp].rad = mymaxima[mind, ii].rad
        endif

        ;Find the back edge of the wave
        
;OLD VERSION, SEARCHING DOWN FROM INTENSITY PEAK
        y=reform(datastruct.bdiff[ii,0:mymaxima[mind,ii].ind])
        y=smooth(y,2,/edge_truncate)
        np=n_elements(y)
        y=reverse(y,1)          ;reverse the array so the search is the same
       ; cgPlot, y, color='blue'
        tmp=min(where(y le 0.25*mymaxima[mind,ii].rad))
        if tmp[0] eq -1 then tmp=np-1

        wave_backedge[ii-sp].yind=mymaxima[mind,ii].ind-tmp
        wave_backedge[ii-sp].xind = ii
        wave_backedge[ii-sp].rad = yarray[mymaxima[mind,ii].ind-tmp]

        ;; print, "Printing maxima in old version"
        ;; print, mymaxima[mind,ii].rad-tmp
        
        datastruct.backinds[mind,ii]=mymaxima[mind,ii].ind-tmp

        if plot eq 1 then begin
           cgPlot, [wave_frontedge[ii-sp].rad, wave_frontedge[ii-sp].rad], [-10,40], col='green', /Overplot
           cgPlot, [wave_backedge[ii-sp].rad, wave_backedge[ii-sp].rad], [-10,40], col='green', /Overplot
        endif

     endelse

     ; Get keyboard input to iterate through plots
     ; during plot mode
     if plot eq 1 then begin
        test = get_kbrd(1)
     endif

     nearestStart =value_locate(rad, wave_frontedge[ii-sp].rad)
     nearestEnd = value_locate(rad, wave_backedge[ii-sp].rad)

;     if nearestStart gt n_elements(y

     datastruct.avgIntense[ii] = mean(col[nearestEnd:nearestStart])
     
  endfor
  
end
