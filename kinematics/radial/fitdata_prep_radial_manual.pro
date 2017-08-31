pro fitdata_prep_radial_manual,rad_data,plotinfo

  
;----------------------------
  diffdata=rad_data.diffdata
  ny=n_elements(diffdata[0,*])
  ntimes=n_elements(diffdata[*,0])
  ;Compute the diff data background
  data_background=mean(diffdata[0:14,ny-15:ny-1])
  yrng=rad_data.radfitrange
  time=rad_data.time
  
  
;----------------------------
;ADDITIONAL PROCESSING WITH HISTOGRAM SELECTION AND ATROUS sharpening
  loadct,39,/silent
  wdef,0,1200,800
  !p.multi=[0,2,2]
  position=[0.09,0.55,0.48,0.94]
  !p.position=position
  aia_plot_jmap_data,time.jd,rad_data.y_rsun_array[yrng[0]:yrng[1]],$
                     diffdata[*,yrng[0]:yrng[1]],$
                     min=-20,max=50,title=plotinfo.imgtit,xtitle=plotinfo.xtitle,$
                     ytitle=plotinfo.ytitle,position=position,charsize=2
  xyouts,0.11,0.85,'1',charsize=4,/norm,color=255
  
;------------------------------  
  ;subtract an X-smoothed version of the data
  xsmdata=diffdata-smooth(diffdata,[16,1],/edge_truncate)
;Average out each radial distance/row of the image.
;Subtract this average profile to get rid of horizontal striping
  ;avgprof=fltarr(ny)
  ;for rr=0,ny-1 do avgprof[rr]=mean(xsmdata[*,rr])
  ;for tt=0,ntimes-1 do xsmdata[tt,*]-=avgprof
  
  ;wdef,1,1000,800
  position=[0.58,0.55,0.97,0.94] 
  !p.position=position
  aia_plot_jmap_data,time.jd,rad_data.y_rsun_array[yrng[0]:yrng[1]],$
                     xsmdata[*,yrng[0]:yrng[1]],$
                     min=-20,max=50,title='',xtitle=plotinfo.xtitle,$
                     ytitle=plotinfo.ytitle,position=position,charsize=2
  xyouts,0.6,0.85,'2',charsize=4,/norm,color=255
  current_data=xsmdata
;------------------------------    


  ;------------------------------
  ;Select from the histogram of values the ones closest to the EUV wave.
  ;sm=smooth(xsmdata_atrous,[14,1],/edge_truncate)
  histdata=current_data
  ;Generate the pixel values histogram
  res=histogram(histdata,locations=loc)
  ;Fit a Gaussian to the histogram
  ress=mpfitpeak(loc,res,coeff,/gaussian)
;How many sigmas from the middle of the pixel distribution to extend the valid signal
  sigfactor=70
  rdrange=[coeff[1]-sigfactor*coeff[2],coeff[1]+sigfactor*coeff[2]]
  ;print,coeff[1]-sigfactor*coeff[2],coeff[1]+sigfactor*coeff[2]
  beyond_datarange=where((histdata lt rdrange[0]) or (histdata gt rdrange[1]))
  if beyond_datarange[0] ne -1 then histdata[beyond_datarange]=data_background
  
;  wdef,2,1000,800
;  aia_plot_jmap_data,time.jd,rad_data.y_rsun_array[yrng[0]:yrng[1]],$
;                     histdata[*,yrng[0]:yrng[1]],$
;                     min=-20,max=50,title='histogram(X-smoothed data)',xtitle=plotinfo.xtitle,$
;                     ytitle=plotinfo.ytitle
  current_data=histdata
;------------------------------

 
;------------------------------  
  ;Perform an atrous filtering of the images:
  coefficients=[0,0,6,0,0,0,0,0]
  ;on the X-smoothed data
  xsmdata=current_data
  xsmdata_atrous=cashew_atrous_multiscale_one_level(xsmdata,coeffs=coefficients,/denoise)
  ;on the original data
  diffdata_atrous=cashew_atrous_multiscale_one_level(rad_data.data,coeffs=coefficients,/denoise)
  
  ;wdef,3,1000,800
  position=[0.09,0.11,0.48,0.5]
  !p.position=position
  aia_plot_jmap_data,time.jd,rad_data.y_rsun_array[yrng[0]:yrng[1]],$
                     xsmdata_atrous[*,yrng[0]:yrng[1]],$
                     min=-20,max=50,title='',xtitle=plotinfo.xtitle,$
                     ytitle=plotinfo.ytitle,position=position,charsize=2
  xyouts,0.11,0.41,'3',charsize=4,/norm,color=255
current_data=xsmdata_atrous
;------------------------------


  ;------------------------------
  ;Select from the histogram of values the ones closest to the EUV wave.
  ;sm=smooth(xsmdata_atrous,[14,1],/edge_truncate)
  histdata=current_data
  ;Generate the pixel values histogram
  res=histogram(histdata,locations=loc)
  ;Fit a Gaussian to the histogram
  ress=mpfitpeak(loc,res,coeff,/gaussian)
;How many sigmas from the middle of the pixel distribution to extend the valid signal
  sigfactor=70
  rdrange=[coeff[1]-sigfactor*coeff[2],coeff[1]+sigfactor*coeff[2]]
  ;print,coeff[1]-sigfactor*coeff[2],coeff[1]+sigfactor*coeff[2]
  beyond_datarange=where((histdata lt rdrange[0]) or (histdata gt rdrange[1]))
  if beyond_datarange[0] ne -1 then histdata[beyond_datarange]=data_background
  
;  wdef,4,1000,800
;  aia_plot_jmap_data,time.jd,rad_data.y_rsun_array[yrng[0]:yrng[1]],$
;                     histdata[*,yrng[0]:yrng[1]],$
;                     min=-20,max=50,title='histogram(atrous(histogram(X-smoothed data)))',xtitle=plotinfo.xtitle,$
;                     ytitle=plotinfo.ytitle
  current_data=histdata
;------------------------------


;------------------------------   
  ;Perform fft-reverse fft on the atrous-processed images to get rid of horizontal striping.
 ; fft_trans=fft(histdata_atrous)
 ; tmp=fft_trans
 ; nx=n_elements(fft_trans[*,0])
 ; ny=n_elements(fft_trans[0,*])
 ; tmp[nx-50:nx-1,*]=0
 ; tmpinv=fft(tmp,/inverse)
  
 ; aia_plot_jmap_data,time.jd,rad_data.y_rsun_array[yrng[0]:yrng[1]],$
 ;                    tmpinv[*,yrng[0]:yrng[1]],$
 ;                    min=-20,max=50,title='Inverse FFT of histogram-atrous',xtitle=plotinfo.xtitle,$
 ;                    ytitle=plotinfo.ytitle
;------------------------------     


;------------------------------
  ;Subtract a Y-smoothed image from the resulting image  
  ;wdef,5,1000,800
  position=[0.58,0.11,0.97,0.5]
  !p.position=position
  histdata=current_data
  sm=histdata[*,yrng[0]:yrng[1]]-smooth(histdata,[1,10],/edge_truncate)
  sm_histdata=histdata-sm
  aia_plot_jmap_data,time.jd,rad_data.y_rsun_array[yrng[0]:yrng[1]],$
                     sm_histdata[*,yrng[0]:yrng[1]],$
                     min=-20,max=50,title='',xtitle=plotinfo.xtitle,$
                     ytitle=plotinfo.ytitle,position=position,charsize=2
  xyouts,0.6,0.41,'4',charsize=4,/norm,color=255
  current_data=sm_histdata
;------------------------------
  

  print,''
  uinput=''
  valid=0
  while valid eq 0 do begin
     read,uinput,prompt='Choose the best quality J-map (1-4) to measure the front:'
     if uinput eq '1' or uinput eq '2' or uinput eq '3' or uinput eq '4' then valid=1
  endwhile
  case uinput of
     '1': rad_data.data=diffdata
     '2': rad_data.data=xsmdata
     '3': rad_data.data=xsmdata_atrous
     '4': rad_data.data=sm_histdata
  endcase
  
 ; rad_data.data=current_data
  
  
  ;bin_aia_data_radial,rad_data,binData=binData
  ;make_gradient_map_radial, rad_data, binData, intensityData=intensityData
  ;mask_gradient_data_radial, rad_data, intensityData, correctData = correctData
 
;----------------------------  
wait,1
wdel,0
;wdel,1
;wdel,2
;wdel,3
;wdel,4
;wdel,5
end
