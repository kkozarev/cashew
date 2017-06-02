pro test_cashew_atrous_multiscale_two_level
;This procedure will test the technique below.

;First, need a function to determine the number of scales, in order
;for the user to feed the correct number. Alternatively, the procedure
;below will augment/trim the coefficient matrix, where necessary.

;Restore some test array - bdiff - sized 1024x1024x75 steps
  test1=0
  if test1 gt 0 then begin
     inpath='/home/kkozarev/git/corwav/tests/'
     outpath=inpath
     fname='atrous_test_bdiff.sav'
     restore,inpath+fname
     
     image=float(bdiff[*,0:799,20])
;for ii=0,73 do tv,bytscl(cashew_atrois_multiscale_two_level(float(bdiff[*,*,ii])),min=-50,max=50) 
     image_recomposed=cashew_atrous_multiscale_two_level(image,/denoise)
     wdef,0,n_elements(image[*,0]),n_elements(image[0,*])
     tv,bytscl(image_recomposed,min=-50,max=50)
     stop
  endif
  
  test2=1
  if test2 gt 0 then begin
     label=['test_cashew']
     event=load_events_info(label=label)
     wav='193'
     fname=replace_string(event.annplot.savename,'WAV',wav)
     restore, event.annuluspath+fname
     ntimes=n_elements(projdata[*,0,0])
     nx=n_elements(projdata[0,*,0])
     ny=n_elements(projdata[0,0,*])
     
     ;Give the function our own coefficients
     ;coefficients=intarr(8,8)
     ;coefficients[0,*]=[20,0,0,0,0,0,0,0]
     ;coefficients[1,*]=[30,2,2,1,1,0,0,0]
     ;coefficients[2,*]=[30,2,2,1,1,0,0,0]
     ;coefficients[3,*]=[25,2,2,1,1,0,0,0]
     ;coefficients[4,*]=[20,2,2,1,1,0,0,0]
     ;coefficients[5,*]=[0,0,0,0,0,0,0,0]
     ;coefficients[6,*]=[0,0,0,0,0,0,0,0]
     ;coefficients[7,*]=[0,0,0,0,0,0,0,0]
     
     coefficients=intarr(8,8)
     coefficients[0,*]=[0,0,0,0,0,0,0,0]
     coefficients[1,*]=[10,2,2,1,1,0,0,0]
     coefficients[2,*]=[20,2,2,1,1,0,0,0]
     coefficients[3,*]=[30,2,2,1,1,0,0,0]
     coefficients[4,*]=[20,2,2,1,1,0,0,0]
     coefficients[5,*]=[0,0,0,0,0,0,0,0]
     coefficients[6,*]=[0,0,0,0,0,0,0,0]
     coefficients[7,*]=[0,0,0,0,0,0,0,0]
     
     ;bdiff=reform(projdata[40,*,*]-projdata[0,*,*])
     ;image_recomposed=cashew_atrous_multiscale_two_level(bdiff,/denoise,coeffs=coefficients)
     
     ;tv,bytscl(image_recomposed,min=-50,max=50)
     ;stop
     
     atrous_bdiff=projdata
     ;wdef,0,nx,ny
     for ii=0,ntimes-1 do begin
        startsectime=systime(/seconds)
        bdiff=reform(projdata[ii,*,*]-projdata[0,*,*])
        image_recomposed=cashew_atrous_multiscale_two_level(bdiff,/denoise,coeffs=coefficients)
        atrous_bdiff[ii,*,*]=image_recomposed
        ;tv,bytscl(image_recomposed,min=-50,max=50)
        endsectime=systime(/seconds)
        print,'Step '+strtrim(string(ii+1),2)+': atrous algorithm took '+strtrim(string(endsectime-startsectime),2)+' seconds.'
        ;stop
     endfor
     fname='atrous_bdiff.sav'
     save,filename=event.annuluspath+fname,atrous_bdiff,annulus_info,ind_arr
     
  endif
  
end



function cashew_atrous_multiscale_two_level,image,coeffs=coeffs,makeplots=makeplots,denoise=denoise
;This procedure applies the multiscale two-level atrous decomposition-recomposition method described in 
;Stenborg & Cobelli (2003) for coronal wave applications.
;It uses IDL procedures developed by Erik Rosolowsky in 2003:
;https://github.com/low-sky/idl-low-sky/tree/master/wavelet
;
;Written by Kamen Kozarev, 05/26/2016

;Number of pixels in the square arrays
  npix_x=n_elements(image[*,0])
  npix_y=n_elements(image[0,*])

  
;----------------------------------------------------------
;FIRST LEVEL DECOMPOSITION
  atrous,image,decomposition=image_atrous_level1,n_scales=nscales_level1
                                ;image_allscales[*,*,0,0]=image_atrous_level1[*,*,0]

;----------------------------------------------------------
;SECOND LEVEL DECOMPOSITION AND POPULATION OF THE MULTILEVEL SCALE ARRAYS
  for ii=0,nscales_level1 do begin
     ;Do the second level decomposition for each first level scale
     atrous,image_atrous_level1[*,*,ii],decomposition=image_atrous_level2,n_scales=nscales_level2
     
     if ii eq 0 then begin
        ;This array will hold the two-level scales
        image_allscales=fltarr(npix_x,npix_y,nscales_level1+1,nscales_level2+1)
                                ;image_allscales[*,*,0,0]=image_atrous_level1[*,*,0]
                                ;return
     endif
     
;----------------------------------------------------------
  if keyword_set(denoise) then begin
;DENOISING OF COMPONENTS - USE THE MULTIRESOLUTION SUPPORT ROUTINE MRS
;The algorithm below first computes an array containing the maximum
;scale id at which each pixel is significant. Then, it loops over each
;second level scale, finds the pixels for which that scale is the
;largest significant, and sets their values to zero at all higher
;scales of the second-level decomposition array.
        tmp_image_support=mrs(image_atrous_level2)

        for jj=0,nscales_level2 do begin
           
;Then, do it for the image array
           maxscalemap=where(tmp_image_support eq jj)
           if maxscalemap[0] eq -1 then continue
           for kk=jj+1,nscales_level2 do begin
              tmp_img=image_atrous_level2[*,*,kk]
              tmp_img[maxscalemap]=0.0
              image_atrous_level2[*,*,kk]=tmp_img
           endfor
        endfor
     endif
     image_allscales[*,*,ii,*]=image_atrous_level2
  endfor
  

;----------------------------------------------------------
;THE RECONSTRUCTION STRATEGY - SETTING THE WEIGHT COEFFICIENTS
  if keyword_set(makeplots) then begin
     wdef,0,npix_x/4.,npix_y/4.

;Make plots of the image at all scales of each level
     for ii=0,nscales_level1 do begin
        for jj=0,nscales_level2 do begin
           tvscl,rebin(image_allscales[*,*,ii,jj],npix_x/4.,npix_y/4.)
           xyouts,5,240,'L1 Scale '+strtrim(string(ii),2)+' | L2 Scale '+strtrim(string(ii),2),$
                  /device,charsize=1.5,charthick=1.5
           write_png,'image_atrous_l1scale'+strtrim(string(ii),2)+'_l2scale'+strtrim(string(jj),2)+'.png',$
                     tvrd(/true)
        endfor
     endfor
  endif
  
  
  coefficients=intarr(nscales_level1+1,nscales_level2+1)
  if not keyword_set(coeffs) then begin   
     coefficients[0,*]=[15,0,0,0,0,0,0,0]
     coefficients[1,*]=[16,2,2,1,1,0,0,0]
     coefficients[2,*]=[25,2,2,1,1,0,0,0]
     coefficients[3,*]=[16,2,2,1,1,0,0,0]
     coefficients[4,*]=[10,2,2,1,1,0,0,0]
     coefficients[5,*]=[0,0,0,0,0,0,0,0]
     coefficients[6,*]=[0,0,0,0,0,0,0,0]
     coefficients[7,*]=[0,0,0,0,0,0,0,0]
  endif else begin
;If the elements of the coefficients array is less/more than expected,
;pad/trim it - working at the end (the large scales).
     nel1=n_elements(coeffs[*,0])
     nel2=n_elements(coeffs[0,*])
     if nel1 lt nscales_level1+1 then begin
        tmparr=intarr(nscales_level1+1,nel2)
        tmparr[0:nel1-1,*]=coeffs
        coeffs=tmparr
     endif
     for ii=0,nscales_level1 do begin
        coeff=reform(coeffs[ii,*])
        while n_elements(coeff) lt nscales_level2+1 do coeff=[coeff,0]
        coefficients[ii,*]=coeff[0:nscales_level2]
     endfor
  endelse



;----------------------------------------------------------
;RECOMPOSITION OF FINAL IMAGES
  image_recomposed=fltarr(npix_x,npix_y)
  for ii=0,nscales_level1 do for jj=0,nscales_level2 do image_recomposed+=reform(image_allscales[*,*,ii,jj]*coefficients[ii,jj])

  return,image_recomposed

end
