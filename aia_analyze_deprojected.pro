pro test_aia_analyze_deprojected
;Procedure to run and test aia_analyze_deprojected

path='./'
fname=path+'aia_deprojected_annulus_20110511_193.sav'

aia_analyze_deprojected,fname,outpath=path


end



pro aia_analyze_deprojected,fname,outpath=outpath,thrange=thrange
; Procedure to analyze the speeds of radial and lateral expansion of a
;wave and/or a filament.
;Uses output from aia_annulus_plot.pro, a procedure deprojecting AIA
;data onto a rectangular grid, where the X-axis is latitude along the
;limb, and the Y-axis is radial distance.
;Written by Kamen Kozarev, 08/07/2013

;Restore the file with the deprojected data
restore, fname
nsteps=n_elements(projdata[*,0,0])
ncols=n_elements(projdata[0,*,0])
nrows=n_elements(projdata[0,0,*])

;What is this? Arcseconds from somewhere. How do I convert them to kms?
yangular_array=res/ind_arr[0].cdelt1*findgen(nrows)

img=reform(projdata[fix(nsteps/2),*,*]-projdata[fix(nsteps/2)-1,*,*])
plot_image, img, xtitle = '!5Theta [degrees from solar north]', $
            ytitle = '!5Radius [arcsec from Sun centre]', $
            charsize = 1.5, title = 'AIA deprojected image', max = 20, $
            origin = [0, 0], charthick = 1.1, $
            scale = [ang_step, res/ind_arr[0].cdelt1], $
            pos = [0.1, 0.1, 0.95, 0.95], min = -50

if not keyword_set(thrange) then begin
   thrang=fltarr(2)
   
   print,''
   print,'Select the MIN(Theta) to include'
   cursor,x,y,/down,/data
   oplot,[x,x],[0,nrows-1]
   thrang[0]=x
   print,x
   
   print,''
   print,'Select the MAX(Theta) to include'
   cursor,x,y,/down,/data
   oplot,[x,x],[0,nrows-1]
   thrang[1]=x
   print,x
   
endif else begin
   thrang=thrange
endelse


print,''
print,'Select the longitude of the AR center:'
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


subfovind=where(new_theta[*,0] ge thrang[0]*!PI/180.0 and new_theta[*,0] le thrang[1]*!PI/180.0)
plotimg=img[subfovind,*]


plot_image, plotimg, xtitle = '!5Theta [degrees from solar north]', $
            ytitle = '!5Radius [arcsec from Sun centre]', $
            charsize = 1.5, title = 'AIA deprojected image', max = 20, $
            origin = [thrang[0], 0], charthick = 1.1, $
            scale = [ang_step, res/ind_arr[0].cdelt1], $
            pos = [0.1, 0.1, 0.95, 0.95], min = -50


;The index of the AR center in the SUB-FOV images
arxcentind=min(where(new_theta[*,0] ge (arcenter-thrang[0])*!PI/180.0))


;Get just the data to analyze
newdata=projdata[*,subfovind,*]
subncols=n_elements(newdata[0,*,0])
leftdata=newdata[*,0:arxcentind-1,htind]
rightdata=newdata[*,arxcentind:subncols-1,htind]
stop


end
