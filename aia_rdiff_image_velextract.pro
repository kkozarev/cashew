pro aia_rdiff_image_velextract,data,indices,framerange,proflocs,nms,checkbad=checkbad,titprefix=titprefix,outpath=outpath
;a quick program to extract the velocity/acceleration of the
;CME/shock from a set of images. The point selection on the images is
;done by hand.


;INPUT
;avgprofile - the average of the radial profiles 
;indices - a corresponding array of data indices
;framerange - the first and last frame for which to measure the
;             transient's position
;proflocs - the position of the profiles, for reference.
;nms - since measuring the points is done manually, this variable has
;      the number of measurements the user has to perform.

;checkbad (optional) - if set, prompt the user to approve each profile
;                      before selecting profile peak
;titprefix (optional) - set to the name of the transient (CME,shock,
;                       etc) (string)
;outpath (optional) - set to the location where the plots of velocity
;                     and position vs. time should be saved. 

;OPTIONAL OUTPUT
;
;if the keyword outpath is set, saves a plot of position vs. time and
;a plot of velocity vs. time for the examined averaged profile.
;

if not keyword_set(titprefix) then titprefix='CME' 
wav=strtrim(string(indices[0].wavelnth),2)


!P.font=-1
loadct,0,/silent 
;tvlct,red,green,blue,/get
!P.position=[0.13,0.1,0.93,0.93]

;Assuming a solar radius of 6.96e5 km, equivalent to 944.76 arcsec(in
;193 A),
;and a platescale of 0.6 "/px (all this info taken from the
;fits headers), the conversion is 442.02 km/px.
kmpx=indices[0].IMSCL_MP*indices[0].RSUN_REF/(1000.0*indices[0].RSUN_OBS)


nprofs=n_elements(data[*,0,0])


init=framerange[0] ;the first frame for which to record the position of the feature.
fin=framerange[1]

peakpos=fltarr(nms,fin-init+1,2)
sigmapos=fltarr(fin-init+1)
sigmavel=fltarr(fin-init+1)
fitchisq=fltarr(fin-init+1)
time=fltarr(fin-init+1)

cmemoments=fltarr(fin-init+1,3) ;moments of the position of the feature

jdfrac=dblarr(nprofs)

bad=0

;Convert the time to fractional JD
for i=0,nprofs-1 do begin
tmp=anytim2jd(reform(indices[i].date_obs))
jdfrac[i]=tmp.frac
endfor
relmintime=(jdfrac-jdfrac[0])*86400.0
time=relmintime[init:fin]


;create the average for the base difference
avg=dblarr(1024,1024)
avg_exptime=average(indices[*].exptime)
avg=data[0,*,*]*avg_exptime/indices[0].exptime
for i=1,9 do avg=avg+data[i,*,*]*avg_exptime/indices[i].exptime
avg=avg/10.0


wdef,0,1024

for n=0,nms-1 do begin
    print,'============================================='
    print,'=================  Trial #'+strtrim(string(n+1),2)+'  ======================'
    print,'============================================='
    for i=init,fin do begin
        rundiffim=data[i,*,*]*avg_exptime/indices[i].exptime-data[i-1,*,*]/indices[i-1].exptime
        ;basediffim=data[i,*,*]*avg_exptime/indices[i].exptime-avg
        tv,rundiffim+smooth(rundiffim,10)*4
        ;tv,basediffim+smooth(basediffim,10)*4
        plots,proflocs[10,0,*],proflocs[10,1,*],/device,thick=4
        if n eq 0 and keyword_set(outpath) then write_png,outpath+'rundiff0612_'+wav+'_'+strtrim(string(i+1000),2)+'.png',tvrd()

        
        wait,0.2
        
        print,'Click on the feature maximum:'
        cursor,x,y,/device
        print, 'You chose x: '+strtrim(string(x),2)+'  y: '+strtrim(string(y),2)
        plots,x,y,psym=2,symsize=2,thick=3,/device
;plots,[0,npix*kmpx],[y/2,y/2]
        
        peakpos[n,i-init,0]=x*kmpx
        peakpos[n,i-init,1]=y*kmpx
        
        wait,0.4
    endfor
endfor


;record the radial position and standard deviation from the measurements
for i=0,fin-init do begin
    sigmapos[i]=stddev(sqrt(peakpos[*,i,0]^2+peakpos[*,i,1]^2))
    cmemoments[i,0]=average(sqrt(peakpos[*,i,0]^2+peakpos[*,i,1]^2))
endfor



;calculate the radial velocity
cmemoments[*,1]=deriv(time,cmemoments[*,0])
sigmavel=derivsig(time,cmemoments[*,0],0.0,sigmapos)
;calculate the radial acceleration
cmemoments[*,2]=deriv(time,cmemoments[*,1])


;=====================================================
;plot the results
;=====================================================

;=========================================================
;plot the position as a function of time.
xrange=[min(time)/60.0-0.1,max(time)/60.0+0.1]
min=min(cmemoments[*,0])
max=max(cmemoments[*,0])
yrange=[min/1.1,max*1.1]


reltime=strmid(indices[init].date_obs,11,8)
date=strmid(indices[init].date_obs,0,10)

plot, time/60.0,cmemoments[*,0],$
      psym=7,symsize=1,$
      title=titprefix+' position versus time, AIA/'+wav+' A',$
      xtitle='Time relative to '+reltime+'UT, '+date+', [min]', $
      ytitle='Radial distance, km',$
      thick=4,xthick=2,ythick=2,charsize=1.8,$
      xrange=xrange,xstyle=1,$
      yrange=yrange,ystyle=1    ;,$
                                ;background=0,color=255


;overplot the fit to the user-selected peaks in the profiles
posparams=poly_fit(time/60.0,cmemoments[*,0],2,yfit=posfit,yband=posfiterr,measure_errors=sigmapos,sigma=possigma)
oplot, time/60.0, posfit
oploterr, time/60.0,cmemoments[*,0],sigmapos,psym=7

xyouts,0.15,0.87,'r = a + bt + ct!U2!N',charsize=1.8,/normal
xyouts,0.15,0.85,'a = '+strtrim(string(posparams[0]),2)+' +/- '+strtrim(string(possigma[0]),2),charsize=1.8,/normal
xyouts,0.15,0.83,'b = '+strtrim(string(posparams[1]),2)+' +/- '+strtrim(string(possigma[1]),2),charsize=1.8,/normal
xyouts,0.15,0.81,'c = '+strtrim(string(posparams[2]),2)+' +/- '+strtrim(string(possigma[2]),2),charsize=1.8,/normal


;overplot the fitted position as a function of time,including errors.
;oplot, time/60.0,gfitmoments[*,0],psym=7,symsize=1
;Use half the FWHM of the Gaussian fit as the error in position
;oploterr,time/60.0, gfitmoments[*,0],siggfitmoments[*,0]

;overplot the fit to the estimated Gaussian peaks of the profiles
;res=linfit(time/60.0,gfitmoments[*,0],yfit=posfit)
;oplot, time/60.0, posfit,linestyle=2


if keyword_set(outpath) then write_png,outpath+'radPosTime_'+wav+'.png',tvrd()
;=========================================================
   
   ;stop
   wait,2.0
   
   
   
;=========================================================
;plot the velocity as a function of time
min=min(cmemoments[*,1])
max=max(cmemoments[*,1])
yrange=[min/1.1,max*1.1]

plot, time/60.0,cmemoments[*,1],$
  psym=4,symsize=1,$
  title=titprefix+' velocity versus time, AIA/'+wav+' A',$
  xtitle='Time relative to '+reltime+'UT, '+date+', [min]', $
  ytitle='Apparent velocity, km/s',$
  thick=4,xthick=2,ythick=2,charsize=1.8,$
  xrange=xrange,xstyle=1,$
  yrange=yrange,ystyle=1;,$
;  background=0,color=255

oploterr, time/60.0,cmemoments[*,1],sigmavel,psym=7

velparams=poly_fit(time/60.0,cmemoments[*,1],1,yfit=velfit,sigma=velsigma,measure_errors=sigmavel)
;res=linfit(time/60.0,cmemoments[*,1],yfit=velfit)
oplot, time/60.0, velfit

xyouts,0.15,0.87,'v = a + bt',charsize=1.8,/normal
xyouts,0.15,0.85,'a = '+strtrim(string(velparams[0]),2)+' +/- '+strtrim(string(velsigma[0]),2),charsize=1.8,/normal
xyouts,0.15,0.83,'b = '+strtrim(string(velparams[1]),2)+' +/- '+strtrim(string(velsigma[1]),2),charsize=1.8,/normal
;xyouts,0.15,0.81,'c = '+strtrim(string(velparams[2]),2)+' +/- '+strtrim(string(velsigma[2]),2),charsize=1.8,/normal

;oploterr,time/60.0, velfit,yband

;overplot the fitted velocity as a function of time
;oplot, time/60.0,gfitmoments[*,1],psym=7,symsize=1
;overplot the errorbars for the uncertainty in the Gaussian fitted velocity
;oploterr,time/60.0,gfitmoments[*,1],siggfitmoments[*,1]

;overplot the fit to the velocity points determined from the Gaussian fits.
;res=linfit(time/60.0,gfitmoments[*,1],yfit=velfit)
;oplot, time/60.0, velfit,linestyle=2


if keyword_set(outpath) then write_png,outpath+'radVelTime_'+wav+'.png',tvrd()
;=========================================================

if keyword_set(outpath) then save,velparams,velsigma,posparams,possigma,posfit,posfiterr,velfit,cmemoments,$
  filename=outpath+'vel_pos_extract_params.sav'


end
