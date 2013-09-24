pro test_radio_read_data
  radio_read_data
end




pro radio_read_data,path=path
;PURPOSE:
;This program reads radio data sent by Vasili Lobzin from USydney.
;;The file contains time in seconds from the start of the day versus
;frequency, for 06/12 and 06/13, and for both the fundamental and
;first harmonic
;
;CATEGORY:
;Radio/Culgoora_Learmonth
;
;INPUTS:
;
;KEYWORDS:
;
;OUTPUTS:
;
;DEPENDENCIES:
; MULTIPLE FUNCTIONS IN THIS FILE. ALSO, IT RELIES ON SPECIFIC DATA
; FILES - NEED TO GENERALIZE IT!
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 01/31/2011
;02/07/2011 Kamen Kozarev - added functions which convert between
;                           frequency and radial distance (f2r) and
;compute averages of the positions and standard deviations, for a
;given time (radioavg).

if not keyword_set(path) then path='/home/kkozarev/Desktop/temp/radiodata/newfit/'
plotit=0
FUNDAMENTAL=0
HARMONIC=1

fp2d=fltarr(4,3)
fs2d=fltarr(4,3)
fp3d=fltarr(4,4)
fs3d=fltarr(4,4)

rsun=6.95508e5

;0. Read raw frequency vs. time data
;NOTE: 'UP' means fundamental frequency, 'LOW' means harmonic, since
;Lobzin inverts the emission.
;NOTE: the data are 2-by-n element arrays, where the first column is
;seconds since the start of the day, the second is frequency in MHz
wdef,0,800,500
!P.font=1
!P.color=0
!P.background=255
!P.thick=2.4

;==============================
;20100612 lower (AKA harmonic)
;==============================
nl=file_lines(path+'20100612lower/data.txt')
data0612low=fltarr(2,nl)
openr,lun,path+'20100612lower/data.txt',/get_lun
readf,lun,data0612low
free_lun,lun
;reform the data so that we have radial positions instead of frequency.
dt=f2r(data0612low,HARMONIC)
;reform the data so that we have a single position for a single time,
;and some uncertainty...
low0612=radioavg(dt)

if plotit gt 0 then begin
plot,low0612.time,low0612.ravg,/ynozero,psym=2
oploterr,low0612.time,low0612.ravg,low0612.rdev,psym=2
oplot,dt[0,*],dt[1,*],/psym
stop
endif


;second-order bootstrap fitting - x = p[0] + p[1] * (t) + 0.5D * p[2] * (t)^2
bootstrap_sdo, low0612.ravg,low0612.time-low0612[0].time,error=low0612.rdev,fit_line,p1,p2,p3,s1,s2,s3
fp2d[0,*]=reform([p1[0],p2[0],p3[0]])
fs2d[0,*]=[s1,s2,s3]
fl0612low_2d=fit_line

;third-order bootstrap fitting  - x = p[0] + p[1] * (t) + 0.5D * p[2] * (t)^2 + (1./6.)*p[3] * (x)^3
bootstrap_sdo_cubic, low0612.ravg,low0612.time-low0612[0].time,error=low0612.rdev,fit_line,p1,p2,p3,p4,s1,s2,s3,s4
fp3d[0,*]=reform([p1[0],p2[0],p3[0],p4[0]])
fs3d[0,*]=[s1,s2,s3,s4]
fl0612low_3d=fit_line


plot,low0612.time-low0612[0].time,low0612.ravg/rsun,/ynozero,psym=2,$
     title='radio fits, 06122010 - harmonic emission',xtitle='Seconds since '+str(low0612[0].time)+' sec',$
     ytitle='Rsun',ythick=2,xthick=2,symsize=2,charsize=2.6,charthick=3
oploterr,low0612.time-low0612[0].time,low0612.ravg/rsun,low0612.rdev/rsun,psym=2
oplot, fl0612low_2d/rsun,linestyle=0
oplot, fl0612low_3d/rsun,linestyle=2
write_png,path+'low0612fits.png',tvrd()



;=================================



;================================
;20100612 upper (AKA fundamental)
;================================
nl=file_lines(path+'20100612upper/data.txt')
data0612up=fltarr(2,nl)
openr,lun,path+'20100612upper/data.txt',/get_lun
readf,lun,data0612up
free_lun,lun
;reform the data so that we have radial positions instead of frequency.
dt=f2r(data0612up,FUNDAMENTAL)
;reform the data so that we have a single position for a single time,
;and some uncertainty...
up0612=radioavg(dt)

if plotit gt 0 then begin
plot,up0612.time,up0612.ravg,/ynozero,psym=2
oploterr,up0612.time,up0612.ravg,up0612.rdev,psym=2
oplot,dt[0,*],dt[1,*],/psym
stop
endif

;second-order bootstrap fitting - x = p[0] + p[1] * (t) + 0.5D * p[2] * (t)^2
bootstrap_sdo, up0612.ravg,up0612.time-up0612[0].time,error=up0612.rdev,fit_line,p1,p2,p3,s1,s2,s3
fp2d[1,*]=reform([p1[0],p2[0],p3[0]])
fs2d[1,*]=[s1,s2,s3]
fl0612up_2d=fit_line

;third-order bootstrap fitting  - x = p[0] + p[1] * (t) + 0.5D * p[2] * (t)^2 + (1./6.)*p[3] * (x)^3
bootstrap_sdo_cubic, up0612.ravg,up0612.time-up0612[0].time,error=up0612.rdev,fit_line,p1,p2,p3,p4,s1,s2,s3,s4
fp3d[1,*]=reform([p1[0],p2[0],p3[0],p4[0]])
fs3d[1,*]=[s1,s2,s3,s4]
fl0612up_3d=fit_line


plot,up0612.time-up0612[0].time,up0612.ravg/rsun,/ynozero,$
     title='radio fits, 06122010 - fundamental emission',xtitle='Seconds since '+str(up0612[0].time)+' sec',$
     ytitle='Rsun',ythick=2,xthick=2,symsize=2,charsize=2.6,charthick=3
oploterr,up0612.time-up0612[0].time,up0612.ravg/rsun,up0612.rdev/rsun,psym=2
oplot, fl0612up_2d/rsun,linestyle=0
oplot, fl0612up_3d/rsun,linestyle=2
write_png,path+'up0612fits.png',tvrd()

;=============================



;=========================
;20100613 lower (harmonic)
;=========================
nl=file_lines(path+'20100613lower/data.txt')
data0613low=fltarr(2,nl)
openr,lun,path+'20100613lower/data.txt',/get_lun
readf,lun,data0613low
free_lun,lun
;reform the data so that we have radial positions instead of frequency.
dt=f2r(data0613low,HARMONIC)
;reform the data so that we have a single position for a single time,
;and some uncertainty...
low0613=radioavg(dt)

if plotit gt 0 then begin
plot,low0613.time,low0613.ravg,/ynozero,psym=2
oploterr,low0613.time,low0613.ravg,low0613.rdev,psym=2
oplot,dt[0,*],dt[1,*],/psym
stop
endif


;second-order bootstrap fitting - x = p[0] + p[1] * (t) + 0.5D * p[2] * (t)^2
bootstrap_sdo, low0613.ravg,low0613.time-low0613[0].time,error=low0613.rdev,fit_line,p1,p2,p3,s1,s2,s3
fp2d[2,*]=reform([p1[0],p2[0],p3[0]])
fs2d[2,*]=[s1,s2,s3]
fl0613low_2d=fit_line

;third-order bootstrap fitting  - x = p[0] + p[1] * (t) + 0.5D * p[2] * (t)^2 + (1./6.)*p[3] * (t)^3
bootstrap_sdo_cubic, low0613.ravg,low0613.time-low0613[0].time,error=low0613.rdev,fit_line,p1,p2,p3,p4,s1,s2,s3,s4
fp3d[2,*]=reform([p1[0],p2[0],p3[0],p4[0]])
fs3d[2,*]=[s1,s2,s3,s4]
fl0613low_3d=fit_line


plot,low0613.time-low0613[0].time,low0613.ravg/rsun,/ynozero,psym=2,$
     title='radio fits, 06132010 - harmonic emission',xtitle='Seconds since '+str(low0613[0].time)+' sec',$
     ytitle='Rsun',ythick=2,xthick=2,symsize=2,charsize=2.6,charthick=3
oploterr,low0613.time-low0613[0].time,low0613.ravg/rsun,low0613.rdev/rsun,psym=2
oplot, fl0613low_2d/rsun,linestyle=0
oplot, fl0613low_3d/rsun,linestyle=2
write_png,path+'low0613fits.png',tvrd()


;==========================


;==========================
;20100613 upper (harmonic)
;==========================
nl=file_lines(path+'20100613upper/data.txt')
data0613up=fltarr(2,nl)
openr,lun,path+'20100613upper/data.txt',/get_lun
readf,lun,data0613up
free_lun,lun
;reform the data so that we have radial positions instead of frequency.
dt=f2r(data0613up,HARMONIC)
;reform the data so that we have a single position for a single time,
;and some uncertainty...
up0613=radioavg(dt)

if plotit gt 0 then begin
plot,up0613.time,up0613.ravg,/ynozero,psym=2
oploterr,up0613.time,up0613.ravg,up0613.rdev,psym=2
oplot,dt[0,*],dt[1,*],/psym
stop
endif


;second-order bootstrap fitting - x = p[0] + p[1] * (t) + 0.5D * p[2] * (t)^2
bootstrap_sdo, up0613.ravg,up0613.time-up0613[0].time,error=up0613.rdev,fit_line,p1,p2,p3,s1,s2,s3
fp2d[3,*]=reform([p1[0],p2[0],p3[0]])
fs2d[3,*]=[s1,s2,s3]
fl0613up_2d=fit_line

;third-order bootstrap fitting  - x = p[0] + p[1] * (t) + 0.5D * p[2] * (t)^2 + (1./6.)*p[3] * (t)^3
bootstrap_sdo_cubic, up0613.ravg,up0613.time-up0613[0].time,error=up0613.rdev,fit_line,p1,p2,p3,p4,s1,s2,s3,s4
fp3d[3,*]=reform([p1[0],p2[0],p3[0],p4[0]])
fs3d[3,*]=[s1,s2,s3,s4]
fl0613up_3d=fit_line


plot,up0613.time-up0613[0].time,up0613.ravg/rsun,/ynozero,psym=2,$
     title='radio fits, 06132010 - harmonic fundamental emission',xtitle='Seconds since '+str(up0613[0].time)+' sec',$
     ytitle='Rsun',ythick=2,xthick=2,symsize=2,charsize=2.6,charthick=3
oploterr,up0613.time-up0613[0].time,up0613.ravg/rsun,up0613.rdev/rsun,psym=2
oplot, fl0613up_2d/rsun,linestyle=0
oplot, fl0613up_3d/rsun,linestyle=2
write_png,path+'up0613fits.png',tvrd()


;========================


;stop

;2. Plot and save the results
save,filename=path+'save_2d_3d_radio_fits.sav',fp2d,fs2d,fp3d,fs3d,low0612,low0613,up0612,up0613,$
     fl0612up_2d,fl0612up_3d,fl0612low_2d,fl0612low_3d,fl0613up_2d,fl0613up_3d,fl0613low_2d,fl0613low_3d

end
;-=======================================================================




;+=======================================================================
function radioavg,datain
;This function will read in a 2-by-n element array, where the first column is
;seconds since the start of the day, the second is frequency in MHz.
;Since there is emission at multiple frequencies at a given time, we will have to reform the data
;to find the averages of the points with overlapping times.

;OUTPUT
;      The function returns an array of structures, each containing
;      the time of the observation in seconds since start of day,
;      average frequency of the observation, standard deviation, and
;      the number of points over which the averaging was performed


;Define a structure that will hold the averages of the points.
rst={time:0.0,ravg:0.0D,rdev:0.0D,nmeas:0}
nl=n_elements(datain[0,*])
dst=replicate(rst,nl)

t=0
pi=0
for i=0, nl-1 do begin
   if i lt pi then continue
   ind=where(datain[0,*] eq datain[0,i],step)
   if step gt 1 then begin
      tmp=moment(datain[1,ind],sdev=sdev)
   endif else begin
      tmp=datain[1,i]
      sdev=1.0e-6
   endelse
   dst[t].time=datain[0,i]
   dst[t].ravg=tmp[0]
   dst[t].rdev=sdev
   dst[t].nmeas=step
   pi=i+step
   t++
endfor

return,dst[0:t-1]
end



function f2r, din, type
;This function returns the same format as the input, but with
;frequency transformed into radial distance in Rs...
;It takes the type of emission - type=0 is fundamental, type=1 is
;                                harmonic.
;This function uses the Newkirk model as specified by Lobzin in
;private communication.
rsun=6.95508e5

nl=n_elements(din[0,*])
dout=din
for i=0,nl-1 do dout[1,i]=(0.5*9.95)/alog(din[1,i]/((type+1)*1.84))
dout[1,*]*=rsun
return,dout
end


pro plotrads,data,fp2d,fp3d,fs2d,fs3d,freqind,date,xrange=xrange,yrange=yrange
rsun=6.95508e5

if freqind eq 0 then freqstr='FUNDAMENTAL'
if freqind eq 1 then freqstr='HARMONIC'

relhour=strtrim(string(fix(data[0].time/3600)),2)
relmin=strtrim(string(fix(data[0].time/60 - relhour*60)),2)
relsec=strtrim(string(fix(data[0].time mod 60)),2)
reltime=relhour+':'+relmin+':'+relsec
time=(data.time-data[0].time)


if keyword_set(xrange) then xrange=xrange else xrange=[0,1.2]

if keyword_set(yrange) then yrange=yrange else yrange=[1.3,1.5]

fitlines=fp2d[0]+fp2d[1]*time+0.5*fp2d[2]*time^2
fitlines3d=fp3d[0]+fp3d[1]*time+0.5*fp3d[2]*time^2+(1./6.)*fp3d[3]^3

plot, time/60.0,data.ravg/rsun,$
                        psym=5,symsize=1,$
                        title='Shock vs. time, mtype II, '+freqstr,$
                        xtitle='Time relative to '+reltime+'UT, '+date+', [min]', $
                        ytitle='Radial distance, R!Ds!N',$
                        xthick=2,ythick=2,$
                        xrange=xrange,xstyle=1,$
                        yrange=yrange,ystyle=1
   oploterr, time/60.0,data.ravg/rsun,data.rdev/rsun,psym=5
   oplot, time/60.0,fitlines/rsun
   oplot, time/60.0,fitlines3d/rsun,linestyle=2	

   plots,[0.7,0.77],[0.85,0.85],/normal
   plots,[0.7,0.77],[0.81,0.81],linestyle=2,/normal
   xyouts,0.78,0.845,'2!Und!N-order fit',/normal
   xyouts,0.78,0.805,'3!Urd!N-order fit',/normal

   xyouts,0.15,0.87,'r = r!D0!N + v!D0!N * t + 1/2 * a * t!U2!N + 1/6 * j * t!U3!N',/normal
   xyouts,0.15,0.83,'r!D0!N ='+strtrim(string(fp3d[0]/rsun,format='(f15.4)'),2)+' +/- '+$
	strtrim(string(fs3d[0]/rsun,format='(f15.4)'),2) + ' R!Ds!N',/normal
   xyouts,0.15,0.80,'v!D0!N ='+strtrim(string(fp3d[1],format='(f15.2)'),2)+$
	' +/- '+strtrim(string(fs3d[1],format='(f15.2)'),2) + ' km/s',/normal
   xyouts,0.15,0.77,'a!D0!N ='+strtrim(string(fp3d[2],format='(f15.2)'),2)+$
	' +/- '+strtrim(string(fs3d[2],format='(f15.2)'),2) + ' km/s!U2!N',/normal
   xyouts,0.15,0.74,'j ='+strtrim(string(fp3d[3],format='(f15.2)'),2)+' +/- '+$
	 strtrim(string(fs3d[3],format='(f15.2)'),2) + ' km/s!U3!N',/normal


   xyouts,0.65,0.37,'r = r!D0!N + v!D0!N * t + 1/2 * a * t!U2!N',/normal
   xyouts,0.65,0.33,'r!D0!N ='+strtrim(string(fp2d[0]/rsun,format='(f15.4)'),2)+' +/- '+$
	 strtrim(string(fs2d[0]/rsun,format='(f15.4)'),2) + ' R!Ds!N',/normal
   xyouts,0.65,0.30,'v!D0!N ='+strtrim(string(fp2d[1],format='(f15.2)'),2)+' +/- '+$
	   strtrim(string(fs2d[1],format='(f15.2)'),2) + ' km/s',/normal
   xyouts,0.65,0.27,'a ='+strtrim(string(fp2d[2],format='(f15.2)'),2)+' +/- '+$
	   strtrim(string(fs2d[2],format='(f15.2)'),2) + ' km/s!U2!N',/normal

end
