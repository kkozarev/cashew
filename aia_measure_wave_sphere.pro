pro test_aia_measure_wave_sphere
;this procedure tests aia_measure_wave_sphere

  evindex=11
  profrange=[11,31]
  datapath='/Volumes/Backscratch/Users/kkozarev/AIA/events/'
  wavelength=['193','211']
;  evnums=['05','06','07','13','19','20','23','32','37','38','41','113','112']
;  sts=['2011/01/25 11:56:00','2011/01/27 11:50:00','2011/01/28 00:45:00',$
;       '2011/02/11 12:30:00','2011/03/07 19:35:00','2011/03/08 03:30:00',$
;       '2011/03/12 15:20:00','2011/04/27 02:05:00','2011/05/11 02:10:00',$
;       '2011/05/29 10:00:00','2012/01/05 06:56:00','2010/06/13 05:35:00',$
;       '2010/06/12 00:55:00']

  wav=wavelength[0]
  evnum=evnums[evindex]
  st=sts[evindex]
  
  
end

pro aia_measure_wave_sphere,wav=wav,evnum=evnum,profrange=profrange,st=st,datapath=datapath
;PURPOSE:
; Measure the radius of a wave event interactively
;CATEGORY:
; AIA/Kinematics
;
;INPUTS:
;
;KEYWORDS:
; wav
; evnum
; profrange
; st
; datapath
;
;OUTPUTS:
;
; 
;DEPENDENCIES:
; aia_oplot_radial, aiamancirclefit,  bootstrap_sdo
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 11/2011


;+========================================
  ;User Input Parameters
  if not keyword_set(wav) then wav='193'
  if not keyword_set(st) then st='2010/06/13 05:35:00'
  ;The range of images to measure - this is currently
  ;determined by visual inspection of the subdataset
  if not keyword_set(profrange) then profrange=[11,31] ;The range of images to record
  init=profrange[0]
  fin=profrange[1]
  if not keyword_set(datapath) then datapath='/Volumes/Backscratch/Users/kkozarev/AIA/events/'
  if not keyword_set(evnum) then evnum='113'
;-========================================


;+========================================
;0. Load the subdata

  std=strsplit(st,'/ :',/extract)
  date=std[0]+std[1]+std[2]
  eventname='AIA_'+date+'_'+evnum+'_'+wav
  print,'Loading '+wav+' channel AIA data for event #'+evnum
  restore,datapath+evnum+'/normalized_'+eventname+'_subdata.sav'
  ntimes=n_elements(subdata[0,0,*])
                                ;fin=n_elements(subindex)-1
  nprofs=fin-init+1             ;The number of measurements
;-========================================
  
  
  
  loadct,9,/silent
  
;First, find the starting and ending time steps of the feature.
                                ;for i=20,40 do tvscl,subdata[*,*,i]-subdata[*,*,0]
  
                                ;stop
  
;+========================================
;1. Constants and definitions
  maxruns=3
  kmpx=subindex[0].IMSCL_MP*subindex[0].RSUN_REF/(1000.0*subindex[0].RSUN_OBS)
  avg=dblarr(1024,1024)
  centcirc=fltarr(maxruns,2,nprofs)
  frontcirc=fltarr(maxruns,2,nprofs)
  frontcircgcor=fltarr(maxruns,2,nprofs)
  radius=fltarr(maxruns,nprofs)
  frontcircmoments=fltarr(nprofs,3) ;moments of the position of the wave front
  centcircmoments=fltarr(nprofs,3)  ;moments of the position of the wave front
  radiusmoments=fltarr(nprofs,3) ;moments of the position of the wave front
  frontpossigma=fltarr(nprofs) ;store the error in the position measurement here
  centpossigma=fltarr(nprofs) ;for the center
  radiussigma=fltarr(nprofs) ;for the radius
  strtime=strarr(nprofs)
  jdfrac=dblarr(nprofs)
;-========================================


;+========================================
;1. Measure the positions and infer the radius and position of the
;center of the circle.

;Create the average for the base difference
  avg_exptime=average(subindex[*].exptime)
  avg=subdata[*,*,0]*avg_exptime/subindex[0].exptime
  for i=1,4 do avg=avg+subdata[*,*,i]*avg_exptime/subindex[i].exptime
  avg=avg/5.0

  geomcorfactor=1.0/sin(subindex[0].ARLON*!PI/180.0)

  wdef,0,1024
  nrun=0
  done=0
  mousebutton=0
  special_point=[0,0]

  print,''
  print,"Here is a preview of the image sequence, twice!"
  print,''
  
 ; init=15
 ; fin=18
  
  for j=0,1 do begin
     for i=init,fin do begin
        ind=strtrim(string(i),2)
        if ind lt 100 then ind='0'+ind
        if ind lt 10 then ind='0'+ind
        im=subdata[*,*,i]-subdata[*,*,i-1]
        scimage=bytscl(im,max=10,min=-20)
        tv,scimage
        xyouts,0.01,0.97,subindex[i].date_obs+' / '+wav,charsize=3,charthick=3,color=200
        xyouts,0.94,0.97,ind,charsize=3,charthick=3,color=200
        wait,0.3
     endfor
  endfor

  stop
  
  while done eq 0 do begin
     for i=init,fin do begin
        ind=strtrim(string(i),2)
        if ind lt 100 then ind='0'+ind
        if ind lt 10 then ind='0'+ind
                                ;im=subdata[*,*,i]*avg_exptime/subindex[i].exptime-avg
        im=subdata[*,*,i]-subdata[*,*,i-1]
        scimage=bytscl(im,max=30,min=-50)
        tv,scimage
        xyouts,0.01,0.97,subindex[i].date_obs+' / '+wav,charsize=3,charthick=3,color=200
        xyouts,0.94,0.97,ind,charsize=3,charthick=3,color=200
        print,'Select points along the wave front. Please pick the radial direction first!'
        aia_oplot_radial,subindex[0],[subindex[0].arx0,subindex[0].ary0]
        aiamancirclefit,circlepos,p,special_point=special_point,np=5
        ;if mousebutton eq 4 then begin
        ;   print,'The user has aborted at frame '+ind
        ;   print,'No more measurements will be made for this event!'
        ;   done=1
        ;   break
        ;endif
        
        radius[nrun,i-init]=p[0]*kmpx
        centcirc[nrun,0,i-init]=(p[2]-subindex[0].arx0)*kmpx
        centcirc[nrun,1,i-init]=(p[3]-subindex[0].ary0)*kmpx
        strtime[i-init]=subindex[i].date_obs
        tmp=anytim2jd(reform(strtime[i-init]))
        jdfrac[i-init]=tmp.frac
        frontcirc[nrun,0,i-init]=abs(special_point[0]-subindex[0].arx0)*kmpx
        frontcirc[nrun,1,i-init]=abs(special_point[1]-subindex[0].ary0)*kmpx
        ;Perform geometric correction for the plane-of-sky projection
        frontcircgcor[nrun,0,i-init]=frontcirc[nrun,0,i-init]*geomcorfactor
        frontcircgcor[nrun,1,i-init]=frontcirc[nrun,1,i-init]*geomcorfactor
        
        wait,0.6
     endfor
     

     if nrun lt maxruns-1 then begin
        uinput=''
        read,uinput,prompt='Do you want to make another measurement? n for no/exit, y for yes: '
        if uinput eq 'n' then done=1
     endif else done=1
     nrun++
  endwhile
;-========================================
  centcirc=centcirc[0:nrun-1,*,*]
  frontcirc=frontcirc[0:nrun-1,*,*]
  frontcircgcor=frontcircgcor[0:nrun-1,*,*]
  radius=radius[0:nrun-1,*]
  time=(jdfrac-jdfrac[0])*86400.0
  
  ;Calculate the error in the measurements
  for i=init,fin do begin
     ;The front of the circle
     res=moment(sqrt(frontcircgcor[*,0,i-init]^2+frontcircgcor[*,1,i-init]^2),sdev=sdev)
     frontpossigma(i-init)=sdev
     frontcircmoments[i-init,0]=reform(res[0])
     ;The circle center
     res=moment(sqrt(centcirc[*,0,i-init]^2+centcirc[*,1,i-init]^2),sdev=sdev)   
     centpossigma(i-init)=sdev
     centcircmoments[i-init,0]=reform(res[0])
     ;The circle radius
     res=moment(sqrt(radius[*,i-init]^2+radius[*,i-init]^2),sdev=sdev)   
     radiussigma(i-init)=sdev
     radiusmoments[i-init,0]=reform(res[0])
  endfor



  ;second-order position fit - x = p[0] + p[1] * (t) + 0.5D * p[2] * (t)^2
  ;The front of the circle
  bootstrap_sdo, reform(frontcircmoments[*,0]),time,error=reform(frontpossigma),$
                 fit_line,p1,p2,p3,s1,s2,s3
  frontfitparams=reform([p1[0],p2[0],p3[0]])
  frontfitparamssigma=[s1,s2,s3]
  frontfitlines=p1[0]+p2[0]*time+p3[0]*0.5*time^2
  frontcircmoments[*,1]=p2[0]+time*p3[0]
  frontcircmoments[*,2]=fltarr(nprofs)+p3[0]
  
  ;The circle center
  bootstrap_sdo, reform(centcircmoments[*,0]),time,error=reform(centpossigma),$
                 fit_line,p1,p2,p3,s1,s2,s3
  centfitparams=reform([p1[0],p2[0],p3[0]])
  centfitparamssigma=[s1,s2,s3]
  centfitlines=p1[0]+p2[0]*time+p3[0]*0.5*time^2
  centcircmoments[*,1]=p2[0]+time*p3[0]
  centcircmoments[*,2]=fltarr(nprofs)+p3[0]

  ;The circle radius
  bootstrap_sdo, reform(radiusmoments[*,0]),time,error=reform(radiussigma),$
                 fit_line,p1,p2,p3,s1,s2,s3
  radiusfitparams=reform([p1[0],p2[0],p3[0]])
  radiusfitparamssigma=[s1,s2,s3]
  radiusfitlines=p1[0]+p2[0]*time+p3[0]*0.5*time^2
  radiusmoments[*,1]=p2[0]+time*p3[0]
  radiusmoments[*,2]=fltarr(nprofs)+p3[0]


  arcoords=[subindex[0].arx0,subindex[0].ary0]
  nruns=nrun

  save,filename=datapath+evnum+'/'+eventname+'_shocklocations.sav',kmpx,nruns,subindex,$
       centcirc,frontcirc,frontcircgcor,radius,time,jdfrac,init,fin,nprofs,$
       frontfitparams, frontfitparamssigma, frontfitlines, frontcircmoments, frontpossigma,$
       centfitparams,centfitparamssigma,centfitlines,centcircmoments,centpossigma,$
       radiusfitparams,radiusfitparamssigma,radiusfitlines,radiusmoments,radiussigma

       
  ;stop
  
  
  
end
