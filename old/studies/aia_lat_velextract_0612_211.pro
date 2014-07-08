pro aia_lat_velextract_0612_211,restart=restart,justplot=justplot

path='/home/kkozarev/Desktop/AIA/limbCMEs/'
outpath='/home/kkozarev/Desktop/temp/newvel/'

!P.font=-1
loadct,8,/silent
!P.position=[0.13,0.1,0.93,0.93]

xrange=[3072,4095]
yrange=[2548,3571]
nms=3
measnum=3
wav='211'
date='06122010'
arlong=43.0;heliographic longitude of the associated AR
init=8
fin=35
mmdd=strmid(date,0,4)
titprefix='CME Wave'
framerange=[init,fin]
offset=reform([xrange[0],yrange[0]])

if keyword_set(justplot) then begin
   restore,outpath+'vel_pos_extract_params_3d_gcor_'+mmdd+'_'+wav+'_2.sav'
   goto,justplot
endif


;peakpos=reform(fltarr(measnum,nms,fin-init+1,2))
peakr=fltarr(measnum,nms,fin-init+1)

peakpos=reform(fltarr(measnum,nms,fin-init+1,2))
sigmapos=fltarr(measnum,fin-init+1)
sigmavel=fltarr(measnum,fin-init+1)
fitchisq=fltarr(measnum,fin-init+1)
time=fltarr(fin-init+1)
fitparams=dblarr(measnum,3)
fitparamssigma=dblarr(measnum,3)
fitparams3d=dblarr(measnum,4)
fitparamssigma3d=dblarr(measnum,4)
cmemoments=fltarr(measnum,fin-init+1,3) ;moments of the position of the feature
cmemoments3d=fltarr(measnum,fin-init+1,4) ;moments of the position of the feature


;get the profile
restore,'/home/kkozarev/Desktop/temp/profiles/proflocs_new_'+mmdd+'.sav'

flarepos=[proflocs[0,0,0],proflocs[0,1,0]]


if keyword_set(restart) then begin
   restore,outpath+'vel_pos_extract_params_3d_gcor_'+mmdd+'_'+wav+'_1.sav'
   goto, restart
endif

;get the data
restore,path+date+'/results/'+wav+'/regionData_'+mmdd+'_'+wav+'.sav'
data=totaldata
index=indices

;sun center is at
cenx=index[0].X0_MP - offset[0]
ceny=index[0].X0_MP - offset[1]
;rsun=6.95508e5
rsun=index[0].RSUN_REF/1000.0 ;solar radius in km
kmpx=index[0].IMSCL_MP*index[0].RSUN_REF/(1000.0*index[0].RSUN_OBS) ;km to pixel ratio.


;res=aia_oplot_radials(indices[40],data[40,*,*],offset=offset,flarepos=flarepos)



;=========================================================================

;number of time steps
nprofs=n_elements(data[*,0,0])

;Convert the time to fractional JD
jdfrac=dblarr(nprofs)
for i=0,nprofs-1 do begin
tmp=anytim2jd(reform(index[i].date_obs))
jdfrac[i]=tmp.frac
endfor
time=jdfrac[init:fin]
relmintime=(jdfrac-jdfrac[0])*86400.0
time=(time-time[0])*86400.0
;--------------------------------


;create the average for the base difference
avg=dblarr(1024,1024)
avg_exptime=average(index[*].exptime)
avg=data[0,*,*]*avg_exptime/index[0].exptime
for i=1,9 do avg=avg+data[i,*,*]*avg_exptime/index[i].exptime
avg=avg/10.0
;--------------------------------


wdef,0,1024


;=========================================================================
;Do the measurements!
;=========================================================================
for p=0,measnum-1 do begin
   print,';;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;'
   print,';;;;;;;;;;;;;;  Measurement along profile #'+strtrim(string(p+1),2)+'  ;;;;;;;;;;;;;;;;;'
   print,';;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;'
   print,''
   for n=0,nms-1 do begin
      print,'============================================='
      print,'=================  Trial #'+strtrim(string(n+1),2)+'  ======================'
      print,'============================================='
      for i=init,fin do begin
         basediffim=reform(data[i,*,*]*avg_exptime/index[i].exptime-avg)
                                ;basediffim+smooth(basediffim,4)*4
         im=basediffim+smooth(basediffim,20,/edge_truncate)*4


;Get the position for this click.
         res=aia_oplot_radials(index[i],im,offset=offset,flarepos=flarepos,color=0,/radcirc)
         wait,0.2
         print,'Click on the feature maximum:'
         cursor,x,y,/data
         plots,x,y,psym=2,symsize=2,thick=3,/data
         wait,0.2
         dist=sqrt((x-cenx)^2+(y-ceny)^2)*kmpx/rsun
         dist/=sin(arlong*!PI/180.0)
         print,'You selected r: '+strtrim(string(dist),2)+' Rs'
         peakr[p,n,i-init]=dist*rsun
         
         measlat=0
         if measlat eq 1 then begin
            dd=fltarr(2)
            print,index[i].date_obs
            for o=0,1 do begin
               res=aia_oplot_radials(index[i],im,offset=offset,flarepos=flarepos,color=0,/radcirc)
               wait,0.2
               print,'Click on the feature maximum perp to the nearest radial:'
               cursor,x,y,/device
               plots,x,y,psym=2,symsize=2,thick=3,/device
               wait,0.1
;Compare it with the center of the sun's position, convert the distance to km.
               dist=sqrt((x-cenx)^2+(y-ceny)^2)*kmpx/rsun
               dd[o]=dist/sin(arlong*!PI/180.0)
               if o eq 1 then begin
                  ravg=(dd[0]+dd[1])/2
                  rerr=(dd[1]-dd[0])/2
                  print, 'Ravg = '+strtrim(string(ravg),2)+' +/- '+strtrim(string(rerr),2)+ 'Rs'
               endif
             endfor
         endif
      endfor
   endfor
endfor

;=========================================================================

;save the results before starting to plot stuff...
save,time,index,peakr,framerange,fin,init,nms,measnum,date,wav,$
     filename=outpath+'vel_pos_extract_params_3d_gcor_'+mmdd+'_'+wav+'_1.sav' 

stop

;==================================================================================
;2. Record the mean and standard deviation of the radial position from
;the measurements, then perform bootstrapping analysis and get good v
;and a information



restart:


for p=0,measnum-1 do begin

;First, get mean and standard deviation of the positions
   for i=0,fin-init do begin
      res=moment(reform(peakr[p,*,i]),sdev=sdev)
      sigmapos[p,i]=sdev
      cmemoments[p,i,0]=reform(res[0])
      cmemoments3d[p,i,0]=cmemoments[p,i,0]
   endfor

;Next, feed the positions and standard deviations into the
;bootstrapping code by David Long, and using the following functional
;form: x = p[0] + p[1] * (t) + 0.5D * p[2] * (t)^2
bootstrap_sdo, reform(cmemoments[p,*,0]),time,error=reform(sigmapos[p,*]),fit_line,p1,p2,p3,s1,s2,s3
fitparams[p,*]=reform([p1[0],p2[0],p3[0]])
fitparamssigma[p,*]=[s1,s2,s3]

if p eq 0 then fitlines=dblarr(measnum,n_elements(fit_line))
fitlines[p,*]=fit_line
cmemoments[p,*,1]=p2[0] + time*p3[0]
cmemoments[p,*,2]=fltarr(fin-init+1)+p3[0]

;third-order fit - x = p[0] + p[1] * (t) + 0.5D * p[2] * (t)^2 +  1./6. * p[3] * (t)^3
bootstrap_sdo_cubic, reform(cmemoments3d[p,*,0]),time,error=reform(sigmapos[p,*]),fit_line3d,p1,p2,p3,p4,s1,s2,s3,s4
fitparams3d[p,*]=reform([p1[0],p2[0],p3[0],p4[0]])
fitparamssigma3d[p,*]=[s1,s2,s3,s4]
if p eq 0 then fitlines3d=dblarr(measnum,n_elements(fit_line3d))
fitlines3d[p,*]=fit_line3d
cmemoments3d[p,*,1]=p2[0] + time*p3[0]+p4[0]*0.5*time^2
cmemoments3d[p,*,2]=p4[0]*time
cmemoments3d[p,*,3]=fltarr(fin-init+1)+p4[0]

endfor
;==================================================================================


;save the results before starting to plot stuff...
save,time,index,fitparams,fitparams3d,fitparamssigma,fitlines,fitlines3d,$
     fitparamssigma3d,peakr,cmemoments,$
     cmemoments3d,framerange,fin,init,nms,measnum,sigmapos,date,wav,$
     filename=outpath+'vel_pos_extract_params_3d_gcor_'+mmdd+'_'+wav+'_2.sav'


justplot:

;==================================================================================
aia_plot_velfits,date,wav,peakr,framerange,cmemoments,cmemoments3d,time,index,sigmapos,fitlines,fitlines3d,fitparams,fitparams3d,fitparamssigma,fitparamssigma3d,outpath=outpath



end
