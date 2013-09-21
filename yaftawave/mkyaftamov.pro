pro mkyaftamov
;Just testing technology here... THIS IS A TEMPORARY PROCEDURE!
;DELETE AFTER USE!
;DEVICE, BYPASS_TRANSLATION=0

  ypath='/home/kkozarev/algoTests/yafta/YAFTA_10'
  add_path,ypath
  testpath='/home/kkozarev/algoTests/yafta/'
  restore,testpath+'normalized_AIA_20110125_05_211_subdata_noradial.sav'
  restore,testpath+'YAFTATEST_HANNING_output.sav'
  
  baseim=subdata[*,*,0]
  nx = n_elements(subdata[*,0,0])
  ny = n_elements(subdata[0,*,0])
  nt = n_elements(subdata[0,0,*])
  
  ps = 1                        ; set to 1 to generat .ps at each step
  if ps eq 1 then set_plot,'ps'
  
  for i = 1,nt-1 do begin
     loadct,0
     filenum = STRtrim(STRING(1000 + fix(i), FORMAT = '(I4)'), 2)
     filename = STRCOMPRESS(testpath+'yaftamov_' + filenum, /REMOVE_ALL)
     mask1=all_masks[*,*,i-1]
     ;features1=features[i]
     ind=subindex[i]
     img2 = subdata[*,*,i]-baseim
     mom=moment(img2)
     meanv=mom[0]
     resim=aia_hide_disk(subindex[i],img2,value=meanv)
     
     
     ;The Hanning noise removal filter is applied here:
     ;===========================================
     image=resim
     imageSize = [nx,ny]
     transform = SHIFT(FFT(image), (imageSize[0]/2), (imageSize[1]/2))
     mask = HANNING(imageSize[0], imageSize[1])  
     maskedTransform = transform*mask
     inverseTransform = FFT(SHIFT(maskedTransform,(imageSize[0]/2), (imageSize[1]/2)), /INVERSE)
     img2=REAL_PART(inverseTransform)
     mom=moment(img2)
     threshold=sqrt(mom[1])

     if ps eq 1 then begin
        set_plot,'ps'
        device,/color,filename=STRCOMPRESS(filename+'.eps',/re),/inches,xsize=10,ysize=10
     endif
     if ps eq 0 then wdef,0,1024
     display_yafta,img2,$;/psfine,/interpolate
                   tit='STEP '+strtrim(string(i),2)+'  ('+ind.origin+'/'+ind.instrume+$
                   '/'+ind.wave_str+'  '+ind.date_obs+')',$
                   /aspect
     plot_edges,mask1,thick=6
     ;plot_labels,features1,thick=4
     
    ;================================
    ; Write graphic output to .ps file, then convert to png
    ;=========================
     if ps eq 1 then begin
        device,/close
        
        exec='convert -flatten '+filename+'.eps '+filename+'.png'
        print,exec
        spawn,exec
    endif
     stop
  endfor
  
  set_plot,'x'
  
end
