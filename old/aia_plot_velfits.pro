pro aia_plot_velfits,date,wav,peakr,framerange,cmemoments,cmemoments3d,time,index,sigmapos,fitlines,fitlines3d,fitparams,fitparams3d,fitparamssigma,fitparamssigma3d,outpath=outpath


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
init=framerange[0]
fin=framerange[1]
kmpx=index[0].IMSCL_MP*index[0].RSUN_REF/(1000.0*index[0].RSUN_OBS)
rsun=index[0].RSUN_REF/1000.0

measnum=n_elements(peakr[*,0,0])
nms=n_elements(peakr[0,*,0])
titprefix='CME Wave'

;3.1 Plot the position as a function of time.
xrange=[min(time)/60.0-0.1,max(time)/60.0+0.1]
reltime=strmid(index[init].date_obs,11,8)

min=min(cmemoments[*,*,0])/rsun
max=max(cmemoments[*,*,0])/rsun
yrange=[min/1.1,max*1.1]

for p=0,measnum-1 do begin
   
   plot, time/60.0,cmemoments[p,*,0]/rsun,$
                        psym=p+5,symsize=1,$
                        title=titprefix+' position versus time,AIA/'+wav+' A (gcor)',$
                        xtitle='Time relative to '+reltime+'UT, '+date+', [min]', $
                        ytitle='Radial distance, R!Ds!N',$
                        xthick=2,ythick=2,$
                        xrange=xrange,xstyle=1,$
                        yrange=yrange,ystyle=1
   oploterr, time/60.0,cmemoments3d[p,*,0]/rsun,sigmapos[p,*]/rsun,psym=5+p
   oplot, time/60.0,fitlines3d[p,*]/rsun,linestyle=2
   oplot, time/60.0,fitlines[p,*]/rsun

   plots,[0.7,0.77],[0.85,0.85],/normal
   plots,[0.7,0.77],[0.81,0.81],linestyle=1,/normal
   xyouts,0.78,0.845,'2!Und!N-order fit',/normal
   xyouts,0.78,0.805,'3!Urd!N-order fit',/normal

   xyouts,0.15,0.87,'r = r!D0!N + v!D0!N * t + 1/2 * a * t!U2!N + 1/6 * j * t!U3!N',/normal
   xyouts,0.15,0.83,'r!D0!N = '+strtrim(string(fitparams3d[p,0]/rsun,format='(f15.4)'),2)+' +/- '+strtrim(string(fitparamssigma3d[p,0]/rsun,format='(f15.4)'),2) + ' R!Ds!N',/normal
   xyouts,0.15,0.80,'v!D0!N = '+strtrim(string(fitparams3d[p,1],format='(f15.2)'),2)+' +/- '+strtrim(string(fitparamssigma3d[p,1],format='(f15.2)'),2) + ' km/s',/normal
   xyouts,0.15,0.77,'a!D0!N = '+strtrim(string(fitparams3d[p,2],format='(f15.2)'),2)+' +/- '+strtrim(string(fitparamssigma3d[p,2],format='(f15.2)'),2) + ' km/s!U2!N',/normal
   xyouts,0.15,0.74,'j = '+strtrim(string(fitparams3d[p,3],format='(f15.2)'),2)+' +/- '+strtrim(string(fitparamssigma3d[p,3],format='(f15.2)'),2) + ' km/s!U3!N',/normal


   xyouts,0.65,0.37,'r = r!D0!N + v!D0!N * t + 1/2 * a * t!U2!N',/normal
   xyouts,0.65,0.33,'r!D0!N = '+strtrim(string(fitparams[p,0]/rsun,format='(f15.4)'),2)+' +/- '+strtrim(string(fitparamssigma[p,0]/rsun,format='(f15.4)'),2) + ' R!Ds!N',/normal
   xyouts,0.65,0.30,'v!D0!N = '+strtrim(string(fitparams[p,1],format='(f15.2)'),2)+' +/- '+strtrim(string(fitparamssigma[p,1],format='(f15.2)'),2) + ' km/s',/normal
   xyouts,0.65,0.27,'a = '+strtrim(string(fitparams[p,2],format='(f15.2)'),2)+' +/- '+strtrim(string(fitparamssigma[p,2],format='(f15.2)'),2) + ' km/s!U2!N',/normal


   if keyword_set(outpath) then write_png,outpath+'radPosTime_'+wav+'_gcor_profile_'+strtrim(string(p),2)+'.png',tvrd()

endfor


;3.2. Plot the velocity as a function of time

loadct,0,/silent


;The second-order version:
min=min(cmemoments[*,*,1])
max=max(cmemoments[*,*,1])
yrange=[min/1.1,max*1.1]

for p=0,measnum-1 do begin
   
   if p eq 0 then plot, time/60.0,cmemoments[p,*,1],$ ;psym=p+5,symsize=1,$
                        title=titprefix+' derived velocity versus time, AIA/'+wav+' A (gcor)',$
                        xtitle='Time relative to '+reltime+'UT, '+date+', [min]', $
                        ytitle='Apparent velocity, km/s',$
                        xthick=2,ythick=2,$
                        xrange=xrange,xstyle=1,$
                        yrange=yrange,ystyle=1
   
   if p gt 0 then oplot,time/60.0,cmemoments[p,*,1],linestyle=p;,psym=5+p,symsize=1

   plots,[0.77,0.81],[0.87-p*0.02,0.87-p*0.02],linestyle=p,/normal
   xyouts,0.815,0.87-p*0.02,'Profile '+strtrim(string(p+1),2),/normal,charsize=1.8
   
endfor

if keyword_set(outpath) then write_png,outpath+'radVelTime_'+wav+'_gcor_profiles.png',tvrd()



;The third-order version:
min=min(cmemoments3d[*,*,1])
max=max(cmemoments3d[*,*,1])
yrange=[min/1.1,max*1.1]

for p=0,measnum-1 do begin
   
   if p eq 0 then plot, time/60.0,cmemoments3d[p,*,1],$ ;psym=p+5,symsize=1,$
                        title=titprefix+' 3rd-order derived velocity versus time, AIA/'+wav+' A (gcor)',$
                        xtitle='Time relative to '+reltime+'UT, '+date+', [min]', $
                        ytitle='Apparent velocity, km/s',$
                        xthick=2,ythick=2,$
                        xrange=xrange,xstyle=1,$
                        yrange=yrange,ystyle=1
   
   if p gt 0 then oplot,time/60.0,cmemoments3d[p,*,1],linestyle=p;,psym=5+p,symsize=1

   plots,[0.77,0.81],[0.87-p*0.02,0.87-p*0.02],linestyle=p,/normal
   xyouts,0.815,0.87-p*0.02,'Profile '+strtrim(string(p+1),2),/normal,charsize=1.8
   
endfor

if keyword_set(outpath) then write_png,outpath+'radVelTime_3d_'+wav+'_gcor_profiles.png',tvrd()



;3.2. Plot the acceleration as a function of time - third-order version

loadct,0,/silent


min=min(cmemoments3d[*,*,2])
max=max(cmemoments3d[*,*,2])
yrange=[min/1.1,max*1.1]

for p=0,measnum-1 do begin
   
   if p eq 0 then plot, time/60.0,cmemoments3d[p,*,2],$ ;psym=p+5,symsize=1,$
                        title=titprefix+' derived acceleration versus time, AIA/'+wav+' A (gcor)',$
                        xtitle='Time relative to '+reltime+'UT, '+date+', [min]', $
                        ytitle='Acceleration, km/s!U2!N',$
                        thick=4,xthick=2,ythick=2,charsize=1.8,$
                        xrange=xrange,xstyle=1,$
                        yrange=yrange,ystyle=1
   
   if p gt 0 then oplot,time/60.0,cmemoments3d[p,*,2],linestyle=p;,psym=5+p,symsize=1

   plots,[0.77,0.81],[0.87-p*0.02,0.87-p*0.02],linestyle=p,/normal
   xyouts,0.815,0.87-p*0.02,'Profile '+strtrim(string(p+1),2),/normal,charsize=1.8
   
endfor

if keyword_set(outpath) then write_png,outpath+'radAccTime_3d_'+wav+'_gcor_profiles.png',tvrd()



;=========================================================

if keyword_set(outpath) then save,time,fitparams,fitparams3d,fitparamssigma,fitparamssigma3d,geomcorpeakpos,cmemoments,cmemoments3d,framerange,nms,$
  filename=outpath+'vel_pos_extract_params_3d_gcor_'+date+'_'+wav+'.sav'


;save all the measured positions into a .sav file
;if keyword_set(outpath) then save,geomcorpeakpos,filename=outpath+'bdiff_positions_'+date+'_'+wav+'.sav'

set_plot,'x'
end
