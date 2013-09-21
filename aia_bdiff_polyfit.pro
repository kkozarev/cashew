pro aia_bdiff_polyfit,indices,framerange,proflocs,peakpos,nms,checkbad=checkbad,titprefix=titprefix,outpath=outpath
;Using the hand measurements of the wave positions, perform second-
;and third-order polynomial fitting to obtain velocities and
;accelerations.

set_plot,'x'

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


;=============================================================================
;0. Constants, Definitions, Data Preparation


if not keyword_set(titprefix) then titprefix='CME Wave'
wav=strtrim(string(indices[0].wavelnth),2)
date=strmid(indices[0].date_obs,0,4)+strmid(indices[0].date_obs,5,2)+strmid(indices[0].date_obs,8,2)

measnum=3 ; This variable controls along how many profiles to measure the wave speed.

rsun=indices[0].RSUN_REF/1000.0


;loadct,8,/silent 
;tvlct,red,green,blue,/get
;!P.position=[0.13,0.1,0.93,0.93]

;Assuming a solar radius of 6.96e5 km, equivalent to 944.76 arcsec(in
;193 A),
;and a platescale of 0.6 "/px (all this info taken from the
;fits headers), the conversion is 442.02 km/px.
kmpx=indices[0].IMSCL_MP*indices[0].RSUN_REF/(1000.0*indices[0].RSUN_OBS)

;number of time steps
nprofs=n_elements(indices[*])



init=framerange[0] ;the first frame for which to record the position of the feature.
fin=framerange[1]

;peakpos=reform(fltarr(measnum,nms,fin-init+1,2))
sigmapos=fltarr(measnum,fin-init+1)
sigmavel=fltarr(measnum,fin-init+1)
fitchisq=fltarr(measnum,fin-init+1)
time=fltarr(fin-init+1)

fitparams=dblarr(measnum,3)
fitparamssigma=dblarr(measnum,3)

fitparams3d=dblarr(measnum,4)
fitparamssigma3d=dblarr(measnum,4)

cmemoments=fltarr(measnum,fin-init+1,3) ;moments of the position of the feature
cmemoments3d=fltarr(measnum,fin-init+1,4) ;includes jerk

jdfrac=dblarr(nprofs)

bad=0

;Convert the time to fractional JD
for i=0,nprofs-1 do begin
tmp=anytim2jd(reform(indices[i].date_obs))
jdfrac[i]=tmp.frac
endfor

time=jdfrac[init:fin]
relmintime=(jdfrac-jdfrac[0])*86400.0
time=(time-time[0])*86400.0
fitlines=dblarr(measnum,n_elements(time))
fitlines3d=fitlines
;print,floor(max(time))

proforigin=[proflocs[0,0,0]*kmpx,proflocs[0,1,0]*kmpx]

;=============================================================================



;==================================================================================
;2. Record the mean and standard deviation of the radial position from
;the measurements, then perform bootstrapping analysis and get good v0
;and a information

for p=0,measnum-1 do begin

;First, get mean and standard deviation of the positions
   for i=0,fin-init do begin
      res=moment(sqrt((peakpos[p,*,i,0]-proforigin[0])^2+(peakpos[p,*,i,1]-proforigin[1])^2),sdev=sdev)
      sigmapos[p,i]=sdev
      cmemoments[p,i,0]=reform(res[0])
      cmemoments3d[p,i,0]=reform(res[0])
   ;  sigmapos[p,i]=stddev(sqrt(peakpos[p,*,i,0]^2+peakpos[p,*,i,1]^2))
   ;  cmemoments[p,i,0]=average(sqrt((peakpos[p,*,i,0]-proforigin[0])^2+(peakpos[p,*,i,1]-proforigin[1])^2))
   endfor


;Next, feed the positions and standard deviations into the
;bootstrapping code by David Long.

;second-order fit - x = p[0] + p[1] * (t) + 0.5D * p[2] * (t)^2
bootstrap_sdo, reform(cmemoments[p,*,0]),time,error=reform(sigmapos[p,*]),fit_line,p1,p2,p3,s1,s2,s3
fitparams[p,*]=reform([p1[0],p2[0],p3[0]])
fitparamssigma[p,*]=[s1,s2,s3]
fitlines[p,*]=p1[0]+p2[0]*time+p3[0]*0.5*time^2


;third-order fit - x = p[0] + p[1] * (t) + 0.5D * p[2] * (t)^2 +  1./6. * p[3] * (t)^3
bootstrap_sdo_cubic, reform(cmemoments3d[p,*,0]),time,error=reform(sigmapos[p,*]),fit_line3d,p1,p2,p3,p4,s1,s2,s3,s4
fitparams3d[p,*]=reform([p1[0],p2[0],p3[0],p4[0]])
fitparamssigma3d[p,*]=[s1,s2,s3,s4]
fitlines3d[p,*]=p1[0]+p2[0]*time+p3[0]*0.5*time^2+p4[0]*(1./6.)*time^3



cmemoments[p,*,1]=p2[0] + time*p3[0]
cmemoments[p,*,2]=fltarr(fin-init+1)+p3[0]

cmemoments3d[p,*,1]=p2[0] + time*p3[0]+p4[0]*0.5*time^2
cmemoments3d[p,*,2]=p4[0]*time
cmemoments3d[p,*,3]=fltarr(fin-init+1)+p4[0]
endfor
;==================================================================================


;stop


 

;=================================================================================
;3. Plot the resulting positions and velocities, show the parameters of the fit
wdef,0,900,600
!P.position=[0.12,0.15,0.95,0.93]
!P.color=0
!P.font=1
!P.background=255
!P.thick=2
!P.charsize=2.5
!P.charthick=3


;3.1 Plot the position as a function of time.
xrange=[min(time)/60.0-0.1,max(time)/60.0+0.1]
reltime=strmid(indices[init].date_obs,11,8)

min=min(cmemoments[*,*,0])/rsun+1
max=max(cmemoments[*,*,0])/rsun+1
yrange=[min/1.1,max*1.1]

for p=0,measnum-1 do begin

   plot, time/60.0,cmemoments[p,*,0]/rsun+1,$
                        psym=p+5,symsize=1,$
                        title=titprefix+' position versus time, AIA/'+wav+' A',$
                        xtitle='Time relative to '+reltime+'UT, '+date+', [min]', $
                        ytitle='Radial distance, R!Ds!N',$
                        xthick=2,ythick=2,$
                        xrange=xrange,xstyle=1,$
                        yrange=yrange,ystyle=1
   oploterr, time/60.0,cmemoments3d[p,*,0]/rsun+1,sigmapos[p,*]/rsun,psym=5+p
   oplot, time/60.0,fitlines3d[p,*]/rsun+1,linestyle=2
   oplot, time/60.0,fitlines[p,*]/rsun+1

   plots,[0.7,0.77],[0.85,0.85],/normal
   plots,[0.7,0.77],[0.81,0.81],linestyle=1,/normal
   xyouts,0.78,0.845,'2!Und!N-order fit',/normal
   xyouts,0.78,0.805,'3!Urd!N-order fit',/normal

   xyouts,0.15,0.87,'r = r!D0!N + v!D0!N * t + 1/2 * a * t!U2!N + 1/6 * j * t!U3!N',/normal
   xyouts,0.15,0.83,'r!D0!N = '+strtrim(string(fitparams3d[p,0]/rsun+1,format='(f15.4)'),2)+' +/- '+strtrim(string(fitparamssigma3d[p,0]/rsun,format='(f15.4)'),2) + ' R!Ds!N',/normal
   xyouts,0.15,0.80,'v!D0!N = '+strtrim(string(fitparams3d[p,1],format='(f15.2)'),2)+' +/- '+strtrim(string(fitparamssigma3d[p,1],format='(f15.2)'),2) + ' km/s',/normal
   xyouts,0.15,0.77,'a!D0!N = '+strtrim(string(fitparams3d[p,2],format='(f15.2)'),2)+' +/- '+strtrim(string(fitparamssigma3d[p,2],format='(f15.2)'),2) + ' km/s!U2!N',/normal
   xyouts,0.15,0.74,'j = '+strtrim(string(fitparams3d[p,3],format='(f15.2)'),2)+' +/- '+strtrim(string(fitparamssigma3d[p,3],format='(f15.2)'),2) + ' km/s!U3!N',/normal


   xyouts,0.65,0.37,'r = r!D0!N + v!D0!N * t + 1/2 * a * t!U2!N',/normal
   xyouts,0.65,0.33,'r!D0!N = '+strtrim(string(fitparams[p,0]/rsun+1,format='(f15.4)'),2)+' +/- '+strtrim(string(fitparamssigma[p,0]/rsun,format='(f15.4)'),2) + ' R!Ds!N',/normal
   xyouts,0.65,0.30,'v!D0!N = '+strtrim(string(fitparams[p,1],format='(f15.2)'),2)+' +/- '+strtrim(string(fitparamssigma[p,1],format='(f15.2)'),2) + ' km/s',/normal
   xyouts,0.65,0.27,'a = '+strtrim(string(fitparams[p,2],format='(f15.2)'),2)+' +/- '+strtrim(string(fitparamssigma[p,2],format='(f15.2)'),2) + ' km/s!U2!N',/normal


   if keyword_set(outpath) then write_png,outpath+'radPosTime_'+wav+'_profile_'+strtrim(string(p),2)+'.png',tvrd()

endfor


;3.2. Plot the velocity as a function of time

loadct,0,/silent


;The second-order version:
min=min(cmemoments[*,*,1])
max=max(cmemoments[*,*,1])
yrange=[min/1.1,max*1.1]

for p=0,measnum-1 do begin
   
   if p eq 0 then plot, time/60.0,cmemoments[p,*,1],$ ;psym=p+5,symsize=1,$
                        title=titprefix+' derived velocity versus time, AIA/'+wav+' A',$
                        xtitle='Time relative to '+reltime+'UT, '+date+', [min]', $
                        ytitle='Apparent velocity, km/s',$
                        xthick=2,ythick=2,$
                        xrange=xrange,xstyle=1,$
                        yrange=yrange,ystyle=1
   
   if p gt 0 then oplot,time/60.0,cmemoments[p,*,1],linestyle=p;,psym=5+p,symsize=1

   plots,[0.77,0.81],[0.87-p*0.02,0.87-p*0.02],linestyle=p,/normal
   xyouts,0.815,0.87-p*0.02,'Profile '+strtrim(string(p+1),2),/normal,charsize=1.8
   
endfor

if keyword_set(outpath) then write_png,outpath+'radVelTime_'+wav+'_profiles.png',tvrd()



;The third-order version:
min=min(cmemoments3d[*,*,1])
max=max(cmemoments3d[*,*,1])
yrange=[min/1.1,max*1.1]

for p=0,measnum-1 do begin
   
   if p eq 0 then plot, time/60.0,cmemoments3d[p,*,1],$ ;psym=p+5,symsize=1,$
                        title=titprefix+' 3rd-order derived velocity versus time, AIA/'+wav+' A',$
                        xtitle='Time relative to '+reltime+'UT, '+date+', [min]', $
                        ytitle='Apparent velocity, km/s',$
                        xthick=2,ythick=2,$
                        xrange=xrange,xstyle=1,$
                        yrange=yrange,ystyle=1
   
   if p gt 0 then oplot,time/60.0,cmemoments3d[p,*,1],linestyle=p;,psym=5+p,symsize=1

   plots,[0.77,0.81],[0.87-p*0.02,0.87-p*0.02],linestyle=p,/normal
   xyouts,0.815,0.87-p*0.02,'Profile '+strtrim(string(p+1),2),/normal,charsize=1.8
   
endfor

if keyword_set(outpath) then write_png,outpath+'radVelTime_3d_'+wav+'_profiles.png',tvrd()



;3.2. Plot the acceleration as a function of time - third-order version

loadct,0,/silent


min=min(cmemoments3d[*,*,2])
max=max(cmemoments3d[*,*,2])
yrange=[min/1.1,max*1.1]

for p=0,measnum-1 do begin
   
   if p eq 0 then plot, time/60.0,cmemoments3d[p,*,2],$ ;psym=p+5,symsize=1,$
                        title=titprefix+' derived acceleration versus time, AIA/'+wav+' A',$
                        xtitle='Time relative to '+reltime+'UT, '+date+', [min]', $
                        ytitle='Acceleration, km/s!U2!N',$
                        thick=4,xthick=2,ythick=2,charsize=1.8,$
                        xrange=xrange,xstyle=1,$
                        yrange=yrange,ystyle=1
   
   if p gt 0 then oplot,time/60.0,cmemoments3d[p,*,2],linestyle=p;,psym=5+p,symsize=1

   plots,[0.77,0.81],[0.87-p*0.02,0.87-p*0.02],linestyle=p,/normal
   xyouts,0.815,0.87-p*0.02,'Profile '+strtrim(string(p+1),2),/normal,charsize=1.8
   
endfor

if keyword_set(outpath) then write_png,outpath+'radAccTime_3d_'+wav+'_profiles.png',tvrd()



;=========================================================

if keyword_set(outpath) then save,time,fitparams,fitparams3d,fitparamssigma,fitparamssigma3d,peakpos,cmemoments,cmemoments3d,framerange,nms,$
  filename=outpath+'vel_pos_extract_params_3d_'+date+'_'+wav+'.sav'


;save all the measured positions into a .sav file
;if keyword_set(outpath) then save,peakpos,filename=outpath+'bdiff_positions_'+date+'_'+wav+'.sav'

set_plot,'x'
end
