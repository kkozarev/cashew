pro aia_test_hanning
  ;ypath='/home/kkozarev/algoTests/yafta/YAFTA_10'
  ;add_path,ypath
  restore,'/home/kkozarev/algoTests/yafta/normalized_AIA_20110125_05_211_subdata_noradial.sav'
  

  for ii=10,n_elements(subindex)-1 do begin
     
     img=subdata[*,*,ii]-subdata[*,*,0]
     mom=moment(img)
     stdv=sqrt(mom[1])
     meanv=mom[0]
     resim=aia_hide_disk(subindex[ii],img,value=meanv)
     
     
;###################################################
     image=resim
     imageSize = [1024,1024]
     displaysize=imagesize
     
;Initialize the display:
    ; DEVICE, DECOMPOSED = 0  
    ; LOADCT, 0
     
;Create a window and display the original image:
     WINDOW, 0, XSIZE = displaySize[0], YSIZE = displaySize[1], TITLE ='Original Image'  
     TVSCL, image
     stop
;Determine the forward Fourier transformation of the image:
     transform = SHIFT(FFT(image), (imageSize[0]/2), (imageSize[1]/2))
     
;Create another window and display the power spectrum:
     WINDOW, 1, TITLE = 'Surface of Forward FFT'  
     SHADE_SURF, (2.*ALOG10(ABS(transform))), /XSTYLE, /YSTYLE, /ZSTYLE, TITLE = 'Power Spectrum', XTITLE = 'Mode', YTITLE = 'Mode', ZTITLE = 'Amplitude', CHARSIZE = 3
    stop
     
;Use a Hanning mask to filter out the noise:
     mask = HANNING(imageSize[0], imageSize[1])  
     maskedTransform = transform*mask
     
;Create another window and display the masked power spectrum:
     WINDOW, 2, TITLE = 'Surface of Filtered FFT'  
     SHADE_SURF, (2.*ALOG10(ABS(maskedTransform))),/XSTYLE, /YSTYLE, /ZSTYLE, TITLE = 'Masked Power Spectrum', XTITLE = 'Mode', YTITLE = 'Mode', ZTITLE = 'Amplitude', CHARSIZE = 3 
     stop
     
;Apply the inverse transformation to the masked frequency domain image:
     inverseTransform = FFT(SHIFT(maskedTransform,(imageSize[0]/2), (imageSize[1]/2)), /INVERSE)
     smim=REAL_PART(inverseTransform)
     
;Create another window and display the results of the inverse transformation:
     WINDOW, 3, XSIZE = displaySize[0], YSIZE = displaySize[1], TITLE = 'Hanning Filtered Image'  
     TVSCL, smim
     stop
     
     nim=smim
     mom=moment(nim)
     stdv=sqrt(mom[1])
     nim[where(nim gt stdv)]*=10.0
     nim[where(nim le stdv)]/=100.0
     
     
;Create another window and display the results of the inverse transformation:
     WINDOW, 4, XSIZE = displaySize[0], YSIZE = displaySize[1], TITLE = 'Threshold Enhanced Image'  
;TV, smooth(nim,4,/edge_truncate)
     tv,nim
     stop
     
     min_size=400
     mom=moment(nim)
     stdv=sqrt(mom[1])
     contiguous_mask,nim,mask,threshold=stdv*0.9,/unipolar
     create_features,nim, mask, features, min_size=min_size,vx=vx, vy=vy, peakthreshold=peakthreshold , dx=dx
     
     window,5,XSIZE = displaySize[0], YSIZE = displaySize[1], TITLE = 'YAFTA image'
     display_yafta,nim,/aspect
     plot_edges,mask
     plot_labels,features
     stop
     
;Plotting into a JPG file
     ;filenum = STRMID(STRING(1000 + fix(ii), FORMAT = '(I4)'),1)
     ;fileid = STRCOMPRESS('feature_' + filenum, /REMOVE_ALL)
     ;image = tvread(FILENAME = fileid, /JPEG, QUALITY = 100, /NODIALOG)
     
     ;delvar,mask,features
     
  endfor
  
end
