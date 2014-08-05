pro find_wave_edge_tangential, data, yarray, yrng, time, fitrange, mymaxima, mind,$
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

  print, wave_frontedge

;  cgPlot, yarray, data[0,*]

  ; Determine the wave edge for each timestep within
  ; the region of interest 
  for ii=sp, ep-1 do begin
     
     alt=0

     newtime = time.jd
     caldat, newtime[ii], m, d, y, h, m, s 

     col = dat[ii,*]
     colSmooth = smooth(col, 25, /edge_truncate)    

     ; Fit a Gaussian to determine front and back edges
     gfit4 = gaussfit(rad, colSmooth, coeff, nterms=4)

     if plot eq 1 then begin
        print, h, ":", m
        cgPlot, rad, colSmooth, col='brown', /window
        
        print, "COEFF"
        print, coeff
        print, "One Sigma"
        print, coeff[1]+nSigma*coeff[2]

         cgPlot, rad, gfit4, /overPlot, col='cyan', /window
      endif

     if debug eq 1 then begin
        print, "COEFF"
        print, coeff
        print, "One Sigma"
        print, coeff[1]+nSigma*coeff[2]

        print, "Maximum value is: "
        print, mymaxima[0, ii].rad
     endif

     gaussHeight = coeff[0]
     gaussCenter = coeff[1]
     gaussStdev = coeff[2]

     ; Calculate front and back edges
     frontEdge = gaussCenter+nsigma*gaussStdev
     backEdge = gaussCenter-nsigma*gaussStdev
     
     ; If the Gaussian fit is bad, use the searching down from
     ; intensity peak method instead
     if gaussHeight gt 300 then alt = 1

     ; Try excluding some initial points in case there are really
     ; negative values preventing the Gaussian fit from working
     if gaussHeight lt 0 then begin
        ; Set initial negative values to the mean
        average = mean(colSmooth[0, *])
        for i = 0, n_elements(rad)-1 do begin
           if colSmooth[0, i] lt 0 then begin
              colSmooth[0, i] = average
           endif else begin
              break
           endelse
        endfor

        ; Refit with new data
        gfit4 = gaussfit(rad, colSmooth, coeff, nterms=4)
        if plot eq 1 then begin
           cgPlot, rad, gfit4, /overPlot, col='red', /window
        endif
        
        gaussHeight = coeff[0]
        gaussCenter = coeff[1]
        gaussStdev = coeff[2]

        ; Calculate front and back edges
        frontEdge = gaussCenter+nsigma*gaussStdev
        backEdge = gaussCenter-nsigma*gaussStdev
        
        if gaussHeight lt 0 then alt = 1
     endif
        
     if frontEdge lt 0 then alt=1

     if alt eq 0 then begin
        
        ; Chop off the front edge at the last location of viable data
        if frontEdge gt yarray[maxRadIndex] then begin
           print, "Chopping front edge"
           frontEdge = yarray[maxRadIndex]        
        endif

        if frontEdge lt mymaxima[0, ii].rad then begin
           print, "Front edge behind maxima, assigning max value"
           frontEdge = mymaxima[0,ii].rad
        endif
        
        wave_frontedge[ii-sp].rad = frontEdge
        wave_frontedge[ii-sp].yind = mymaxima[0,ii].ind
        wave_frontedge[ii-sp].xind = ii

        wave_backedge[ii-sp].rad = backEdge
        wave_backedge[ii-sp].yind = mymaxima[0, ii].ind
        wave_backedge[ii-sp].xind = ii

        ;; print, "Printing new version backedge"
        ;; print, mymaxima[mind, ii].rad

        if plot eq 1 then begin
           
           print, h, ":", m
           
           cgPlot, [frontEdge, frontEdge], [-10,40], /Overplot, /window
           cgPlot, [backEdge, backEdge], [-10,40], /Overplot, /window

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
        y=reform(datastruct.bdiff[ii,mymaxima[0,ii].ind:*])
        y=smooth(y,2,/edge_truncate)
;        cgPlot, y
        np=n_elements(y)
        tmp=min(where(y le 0.25*max(y)))
        if tmp[0] eq -1 then tmp=np-1
        if plot eq 1 || debug eq 1 then begin
           print, "Old version value: "
           print, yarray[mymaxima[0,ii].ind+tmp]
        endif
        wave_frontedge[ii-sp].rad=yarray[mymaxima[0,ii].ind+tmp]
        wave_frontedge[ii-sp].yind=mymaxima[0,ii].ind+tmp
        wave_frontedge[ii-sp].xind = ii
        datastruct.frontinds[mind,ii]=wave_frontedge[ii-sp].yind
        
        if wave_frontedge[ii-sp].rad lt mymaxima[0, ii].rad then begin
           wave_frontedge[ii-sp].rad = mymaxima[0, ii].rad
        endif

        ;Find the back edge of the wave
        
;OLD VERSION, SEARCHING DOWN FROM INTENSITY PEAK
        y=reform(datastruct.bdiff[ii,0:mymaxima[0,ii].ind])
        y=smooth(y,2,/edge_truncate)
        np=n_elements(y)
        y=reverse(y,1)          ;reverse the array so the search is the same
       ; cgPlot, y, color='blue'
        tmp=min(where(y le 0.25*mymaxima[0,ii].rad))
        if tmp[0] eq -1 then tmp=np-1

        wave_backedge[ii-sp].yind=mymaxima[0,ii].ind-tmp
        wave_backedge[ii-sp].xind = ii
        wave_backedge[ii-sp].rad = yarray[mymaxima[0,ii].ind-tmp]

        ;; print, "Printing maxima in old version"
        ;; print, mymaxima[mind,ii].rad-tmp
        
        datastruct.backinds[mind,ii]=mymaxima[0,ii].ind-tmp

        if plot eq 1 then begin
           cgPlot, [wave_frontedge[ii-sp].rad, wave_frontedge[ii-sp].rad], [-10,40], col='green', /Overplot, /window
           cgPlot, [wave_backedge[ii-sp].rad, wave_backedge[ii-sp].rad], [-10,40], col='green', /Overplot, /window
        endif

     endelse

     ; Get keyboard input to iterate through plots
     ; during plot mode
     if plot eq 1 then begin
        test = get_kbrd(1)
     endif
     
  endfor

  print, wave_frontedge

  
end
