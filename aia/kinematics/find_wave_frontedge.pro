pro find_wave_frontedge, data, yarray, yrng, time, fitrange, mymaxima, mind,$
                         maxRadIndex, datastruct=datastruct, wave_frontedge=wave_frontedge
        

  plot = 0

  loadct, 0

  sp=fitrange[0]
  ep=fitrange[1]

  rad = yarray[yrng[0]:yrng[1]]
  dat = data[*,yrng[0]:yrng[1]]

  nSigma = 2.0

;  cgPlot, yarray, data[0,*]
  for ii=sp, ep do begin

     alt=0

     newtime = time.jd
     caldat, newtime[ii], m, d, y, h, m, s 

     col = dat[ii,*]

           ;color=255.0*(col-min)/(max-min) ; Scale Colors

;           col=reverse(col)           
;           yarray = reverse(yarray)

     colSmooth = smooth(col, 8, /edge_truncate)     
     
     
     gfit1 = gaussfit(rad, colSmooth, coeff, nterms=3)
     ;; gfit2 = gaussfit(rad, colSmooth, coeff, nterms=4)
     ;; gfit3 = gaussfit(rad, colSmooth, coeff, nterms=5)
     ;; gfit4 = gaussfit(rad, colSmooth, coeff, nterms=6)


     if plot eq 1 then begin
        print, h, ":", m
        cgPlot, rad, colSmooth, col='brown'
        
        print, "COEFF"
        print, coeff
        

        

        print, "two Sigma"
        print, coeff[1]+nSigma*coeff[2]

     
        cgPlot, rad, gfit1, /overPlot, col='blue'
        ;; cgPlot, rad, gfit2, /overPlot, col='green'
        ;; cgPlot, rad, gfit3, /overPlot, col='red'
        ;; cgPlot, rad, gfit4, /overPlot, col='cyan'
     endif

     gaussHeight = coeff[0]
     gaussCenter = coeff[1]
     gaussStdev = coeff[2]

     if gaussHeight lt 0 then alt=1
     if gaussHeight gt 200 then alt=1

     if alt eq 0 then begin
        edge = gaussCenter+nsigma*coeff[2]

        if edge gt yarray[maxRadIndex] then edge = yarray[maxRadIndex]        
        if edge lt mymaxima[mind, ii].rad then edge = mymaxima[mind,ii].rad
        
        wave_frontedge[ii-sp].rad = edge
        wave_frontedge[ii-sp].ind = mymaxima[mind,ii].ind

        if plot eq 1 then begin
           cgPlot, [edge, edge], [-10,40], /Overplot

        ;; ;OLD VERSION, SEARCHING DOWN FROM INTENSITY PEAK
        ;; y=reform(datastruct.bdiff[ii,mymaxima[mind,ii].ind:*])
        ;; y=smooth(y,2,/edge_truncate)
        ;; np=n_elements(y)
        ;; tmp=min(where(y le 0.25*max(y)))
        ;; if tmp[0] eq -1 then tmp=np-1
        ;; wave_frontedge[ii-sp].rad=yarray[mymaxima[mind,ii].ind+tmp]
        ;; wave_frontedge[ii-sp].ind=mymaxima[mind,ii].ind+tmp
        ;; datastruct.frontinds[mind,ii]=wave_frontedge[ii-sp].ind
        ;; cgPlot, [wave_frontedge[ii-sp].rad, wave_frontedge[ii-sp].rad], [-10,40], col='green', /Overplot
           
        endif

     endif else begin
        ;OLD VERSION, SEARCHING DOWN FROM INTENSITY PEAK
        y=reform(datastruct.bdiff[ii,mymaxima[mind,ii].ind:*])
        y=smooth(y,2,/edge_truncate)
        np=n_elements(y)
        tmp=min(where(y le 0.25*max(y)))
        if tmp[0] eq -1 then tmp=np-1
        wave_frontedge[ii-sp].rad=yarray[mymaxima[mind,ii].ind+tmp]
        wave_frontedge[ii-sp].ind=mymaxima[mind,ii].ind+tmp
        datastruct.frontinds[mind,ii]=wave_frontedge[ii-sp].ind
        
        if wave_frontedge[ii-sp].rad lt mymaxima[mind, ii].rad then begin
           wave_frontedge[ii-sp].rad = mymaxima[mind, ii].rad
        endif

        if plot eq 1 then begin
           cgPlot, [wave_frontedge[ii-sp].rad, wave_frontedge[ii-sp].rad], [-10,40], /Overplot
        endif
     endelse


        


     if plot eq 1 then begin
        test = get_kbrd(1)
     endif
     
  endfor
  
end
