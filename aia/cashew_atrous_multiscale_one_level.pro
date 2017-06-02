pro test_cashew_atrous_multiscale_one_level
;This procedure will test the technique below.

;First, need a function to determine the number of scales, in order
;for the user to feed the correct number. Alternatively, the procedure
;below will augment/trim the coefficient matrix, where necessary.

;Restore some test array - bdiff - sized 1024x1024x75 steps
  ;inpath='/home/kkozarev/git/corwav/tests/'
  ;outpath=inpath
  ;fname='atrous_test_bdiff.sav'
                                ;restore,inpath+fname
  wav='193'
  event=load_events_info(label='151104_01')
  restore,event.annuluspath+replace_string(event.annplot.savename,'WAV',wav)
  
  
  stop
  image=float(bdiff[*,0:799,20])
  ;wdef,0,1024,1024
  ;for ii=0,73 do tv,bytscl(cashew_atrois_multiscale_two_level(float(bdiff[*,*,ii])),min=-50,max=50) 
  image_recomposed=cashew_atrous_multiscale_one_level(image,/denoise)
  wdef,0,n_elements(image[*,0]),n_elements(image[0,*])
  tv,bytscl(image_recomposed,min=-100,max=100)
  stop 
  
end



function cashew_atrous_multiscale_one_level,image,coeffs=coeffs,makeplots=makeplots,denoise=denoise
;This procedure applies the multiscale one-level atrous decomposition-recomposition method described in 
;Stenborg & Cobelli (2003) for coronal wave applications.
;It uses IDL procedures developed by Erik Rosolowsky in 2003:
;https://github.com/low-sky/idl-low-sky/tree/master/wavelet
;
;Written by Kamen Kozarev, 05/26/2016
;Number of pixels in the square arrays
  npix_x=n_elements(image[*,0])
  npix_y=n_elements(image[0,*])

;;----------------------------------------------------------
;FIRST LEVEL DECOMPOSITION
  atrous,image,decomposition=image_atrous_level1,n_scales=nscales_level1
  ;stop
  
  if keyword_set(denoise) then begin
;DENOISING OF COMPONENTS - USE THE MULTIRESOLUTION SUPPORT ROUTINE MRS
;The algorithm below first computes an array containing the maximum
;scale id at which each pixel is significant. Then, it loops over each
;second level scale, finds the pixels for which that scale is the
;largest significant, and sets their values to zero at all higher
;scales of the second-level decomposition array.
     ;Get the pixel significance for the scales
     image_support_level1=mrs(image_atrous_level1)
     for ii=0,nscales_level1-1 do begin
        maxscalemap=where(image_support_level1 eq ii)
        if maxscalemap[0] eq -1 then continue
        for kk=ii+1,nscales_level1-1 do begin
           tmp_img=image_atrous_level1[*,*,kk]
           tmp_img[maxscalemap]=0.0
           image_atrous_level1[*,*,kk]=tmp_img
        endfor
     endfor
  endif

  ;stop

;----------------------------------------------------------
;THE RECONSTRUCTION STRATEGY - SETTING THE WEIGHT COEFFICIENTS
  if keyword_set(makeplots) then begin
     wdef,0,npix_x/4.,npix_y/4.
;Make plots of the data at all scales of each level
     for ii=0,nscales_level1 do begin
        tvscl,rebin(sqrt(image_atrous_level1[*,*,ii]),npix_x/4.,npix_y/4.)
        xyouts,5,240,'Scale '+strtrim(string(ii),2),$
               /device,charsize=1.5,charthick=1.5
        write_png,'data_atrous_l1scale'+strtrim(string(ii),2)+'.png',$
                  tvrd(/true)
     endfor
  endif

;----------------------------------------------------------
;Set or import the coefficients
  coefficients=intarr(nscales_level1+1)
  if not keyword_set(coeffs) then begin
     coefficients=[0,50,50,0,0,0,0,0]
  endif else begin
;If the elements of the coefficients array is less/more than expected,
;pad/trim it - working at the end (the large scales).
     while n_elements(coeffs) lt nscales_level1+1 do coeffs=[coeffs,0]
     while n_elements(coeffs) gt nscales_level1+1 do coeffs=coeffs[0:n_elements(coeffs)-2]
     coefficients=coeffs
  endelse
;----------------------------------------------------------
;RECOMPOSITION OF FINAL IMAGES
  ;print,coefficients
  ;stop
  image_recomposed=fltarr(npix_x,npix_y)
  for ii=0,nscales_level1 do image_recomposed+=reform(image_atrous_level1[*,*,ii]*coefficients[ii])
  
  return,image_recomposed
  
end
