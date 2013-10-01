pro test_aia_annulus_analyze
;Procedure to run and test aia_analyze_deprojected

path='/home/kkozarev/Desktop/AIA/pro/'
fname=path+'aia_deprojected_annulus_20110511_193.sav'
aia_annulus_analyze,fname,outpath=path,/interactive

end



pro aia_annulus_analyze,fname,outpath=outpath,thrange=thrange,interactive=interactive
;PURPOSE:
;Procedure to analyze the speeds of radial and lateral expansion of a
;wave and/or a filament.
;Uses output from aia_annulus_plot.pro, a procedure deprojecting AIA
;data onto a rectangular grid, where the X-axis is latitude along the
;limb, and the Y-axis is radial distance.
;
;CATEGORY:
; AIA/Kinematics
;
;INPUTS:
;
;KEYWORDS:
; 
;
;OUTPUTS:
;
; 
;DEPENDENCIES:
;
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 08/07/2013

;Restore the file with the deprojected data
restore, fname
nsteps=n_elements(projdata[*,0,0])
ncols=n_elements(projdata[0,*,0])
nrows=n_elements(projdata[0,0,*])

;What is this? Arcseconds from somewhere. How do I convert them to km?
yangular_array=res/ind_arr[0].cdelt1*findgen(nrows)


;The X-angular array (distance along the limb from the pole).
xangarr=ncols*ang_step


img=reform(projdata[fix(nsteps/2),*,*]-projdata[fix(nsteps/2)-1,*,*])
plot_image, img, xtitle = '!5Theta [degrees from solar north]', $
            ytitle = '!5Radius [arcsec from Sun center]', $
            charsize = 1.5, title = 'AIA deprojected image', max = 20, $
            origin = [0, 0], charthick = 1.1, $
            scale = [ang_step, res/ind_arr[0].cdelt1], $
            pos = [0.1, 0.1, 0.95, 0.95], min = -50

if not keyword_set(thrange) then begin
   thrang=fltarr(2)
   if not keyword_set(interactive) then begin
      thrang=[0.,360.]
   endif else begin
      
      print,''
      print,'On the image, select the MIN(Theta) to include'
      cursor,x,y,/down,/data
      oplot,[x,x],[0,nrows-1]
      thrang[0]=x
      print,x
      
      print,''
      print,'On the image, select the MAX(Theta) to include'
      cursor,x,y,/down,/data
      oplot,[x,x],[0,nrows-1]
      thrang[1]=x
      print,x
   endelse
   
endif else begin
   thrang=thrange
endelse


if not keyword_set(interactive) then begin
   arcenter=60.0
   lat_heights=[150,200,250]
   htind=dblarr(3)
   for ii=0,2 do $
      htind[ii]=min(where(yangular_array ge lat_heights[ii]))
endif else begin
   print,''
   print,'On the image, select the latitude of the AR center:'
   cursor,x,y,/down,/data
   oplot,[x,x],[0,nrows-1]
   print,x
   arcenter=x
   
   
   uinput=0.0
   while uinput le 0.0 or uinput gt 5 do $
      read,uinput,prompt='How many heights for lateral measurements (5 max)? '
   
   lat_heights=dblarr(uinput)
   htind=dblarr(uinput)
   
   for ii=0,uinput-1 do begin
      print,''
      print,'Select height for lateral measurement #'+strtrim(string(ii+1),2)
      cursor,x,y,/down,/data
      print,y
      oplot,[0,ncols-1],[y,y]
      lat_heights[ii]=y
      htind[ii]=min(where(yangular_array ge lat_heights[ii]))
   endfor
   
endelse
   
   subfovind=where(new_theta[*,0] ge thrang[0]*!PI/180.0 and new_theta[*,0] le thrang[1]*!PI/180.0)
   plotimg=img[subfovind,*]
   
   
   plot_image, plotimg, xtitle = '!5Theta [degrees from solar north]', $
               ytitle = '!5Radius [arcsec from Limb]', $
               charsize = 1.5, title = 'AIA deprojected image', max = 20, $
            origin = [thrang[0], 100], charthick = 1.1, $
               scale = [ang_step, res/ind_arr[0].cdelt1], $
               pos = [0.1, 0.1, 0.95, 0.95], min = -50
   
   
;The index of the AR center in the SUB-FOV images
   arxcentind=min(where(new_theta[*,0] ge (arcenter-thrang[0])*!PI/180.0))
   
   
;Get just the data to analyze
   subfovdata=projdata[*,subfovind,*]
   subncols=n_elements(subfovdata[0,*,0])
   andata=projdata[*,subfovind,htind]
   
   leftdata=subfovdata[*,0:arxcentind-1,htind]
   rightdata=subfovdata[*,arxcentind:subncols-1,htind]
   stop
   
   tmp=reform(andata[*,150:349,1])
   for ss=1,nsteps-1 do $
      tmp[ss,*]=reform(andata[ss,150:349,0])-reform(andata[ss-1,150:349,1])
   
   stop
   
end

