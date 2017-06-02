pro fitdata_prep_radial,rad_data,plotinfo

  
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
  wdef,0,1000,800
  aia_plot_jmap_data,time.jd,rad_data.y_rsun_array[yrng[0]:yrng[1]],$
                     diffdata[*,yrng[0]:yrng[1]],$
                     min=-20,max=50,title=plotinfo.imgtit,xtitle=plotinfo.xtitle,$
                     ytitle=plotinfo.ytitle
  
  ;Select from the histogram of values the ones closest to the EUV wave.
  sm=smooth(diffdata,[14,1],/edge_truncate)
  histdata=diffdata-sm
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
  
;Average out each radial distance/row of the image.
;Subtract this average profile to get rid of horizontal striping
  avgprof=fltarr(ny)
  for rr=0,ny-1 do avgprof[rr]=mean(histdata[*,rr])
  for tt=0,ntimes-1 do histdata[tt,*]-=avgprof
  
  wdef,1,1000,800
  aia_plot_jmap_data,time.jd,rad_data.y_rsun_array[yrng[0]:yrng[1]],$
                     histdata[*,yrng[0]:yrng[1]],$
                     min=-20,max=50,title='After applying histogram',xtitle=plotinfo.xtitle,$
                     ytitle=plotinfo.ytitle
  
  ;Perform an atrous filtering of the images:
  coefficients=[0,7,0,0,0,0,0,0]
  ;on the histogram-reduced data
  histdata_atrous=cashew_atrous_multiscale_one_level(histdata,coeffs=coefficients,/denoise)
  ;on the original data
  diffdata_atrous=cashew_atrous_multiscale_one_level(rad_data.data,coeffs=coefficients,/denoise)
  
  wdef,2,1000,800
  aia_plot_jmap_data,time.jd,rad_data.y_rsun_array[yrng[0]:yrng[1]],$
                     histdata_atrous[*,yrng[0]:yrng[1]],$
                     min=-20,max=50,title='After atrous on histogram data',xtitle=plotinfo.xtitle,$
                     ytitle=plotinfo.ytitle
  
 ; wdef,3,1000,800
 ; aia_plot_jmap_data,time.jd,rad_data.y_rsun_array[yrng[0]:yrng[1]],$
 ;                    diffdata_atrous[*,yrng[0]:yrng[1]],$
 ;                    min=-20,max=50,title='After atrous on original data',xtitle=plotinfo.xtitle,$
 ;                    ytitle=plotinfo.ytitle
  
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
  
  ;Subtract a smooth image with horizontal stripes from the resulting image
  wdef,3,1000,800
  sm=smooth(histdata_atrous,[14,1],/edge_truncate)
  aia_plot_jmap_data,time.jd,rad_data.y_rsun_array[yrng[0]:yrng[1]],$
                     histdata_atrous[*,yrng[0]:yrng[1]]-sm[*,yrng[0]:yrng[1]],$
                     min=-20,max=50,title='atrous(histdata)-smooth',xtitle=plotinfo.xtitle,$
                     ytitle=plotinfo.ytitle
  
  rad_data.data=histdata_atrous-sm

  
  ;bin_aia_data_radial,rad_data,binData=binData
  ;make_gradient_map_radial, rad_data, binData, intensityData=intensityData
  ;mask_gradient_data_radial, rad_data, intensityData, correctData = correctData
  ;stop
;----------------------------  
wait,1
wdel,0
wdel,1
wdel,2
wdel,3
end
