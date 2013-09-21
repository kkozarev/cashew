pro batch_circlefit
;get the velocities for the 0612 and 0613 events
path='/home/kkozarev/Desktop/AIA/limbCMEs/'

loadct,8,/silent
;First the 0612 event
init= 35;8
fin= 60;37
dim1=1024
dim2=dim1
frontcircle=dblarr(fin-init+1,3)
backcircle=dblarr(fin-init+1,3)

;load the profile location
;restore,path+'06122010/results/proflocs_0612.sav'
restore,path+'06132010/results/proflocs_0613.sav'

nprofiles=n_elements(proflocs[*,0,0])
proflen=n_elements(proflocs[0,0,*])
;193 A
;restore,path+'06122010/results/193/regionData_0612_193.sav'
restore,path+'06132010/results/193/regionData_0613_193.sav'
nframes=n_elements(totaldata[*,0,0])


;create the average for the base difference
avg=dblarr(dim1,dim2)
avg_exptime=average(indices[*].exptime)
avg=totaldata[0,*,*]*avg_exptime/indices[0].exptime
for i=1,9 do avg=avg+totaldata[i,*,*]*avg_exptime/indices[i].exptime
avg=avg/10.0

wdef,0,dim1,dim2

;fit circles to the base difference image
for n=init,fin do begin
   basediffim=(totaldata[n,*,*]*avg_exptime/indices[i].exptime*1.0-avg);/(avg*1.0)*100.0
   tv,basediffim+smooth(basediffim,10)*4
   plots,proflocs[0,0,*],proflocs[0,1,*],/device,thick=3
   plots,proflocs[nprofiles-1,0,*],proflocs[nprofiles-1,1,*],/device,thick=3

   ;avgprof=fltarr(proflen)
   ;avgprof=basediffim[proflocs[0,0,*],proflocs[0,1,*]]
   ;for i=1,nprofiles-1 do avgprof=avgprof+basediffim[proflocs[i,0,*],proflocs[i,1,*]]
   ;avgprof=avgprof/(nprofiles*1.0)
   ;plot,smooth(avgprof,40,/edge_truncate),yrange=[-10,10]
   ;xyouts,0.13,0.9,indices[n].date_obs,/normal
   ;wait,0.5

   print,'Select points along wave front between the two radial profiles'
   aiamancirclefit,circlepos,p
   frontcircle[n-init,0]=p[0] ;circle radius
   frontcircle[n-init,1]=p[2] ;x-position of circle center
   frontcircle[n-init,2]=p[3] ;y-position of circle center

;   print,'Select points along wave back between the two radial profiles'
;   aiamancirclefit,circlepos,p
;   backcircle[n-init,0]=p[0]
;   backcircle[n-init,1]=p[2]
;   backcircle[n-init,2]=p[3]

   wait,0.6
endfor

stop
;aia_bdiff_image_velextract,totaldata,indices,[5,37],proflocs,10,$
;                           outpath=path+'06122010/results/193/bdiffim/'
;stop
;211 A
;restore,path+'06122010/results/211/regionData_0612_211.sav'
;aia_bdiff_image_velextract,totaldata,indices,[5,37],proflocs,10,$
;                           outpath=path+'06122010/results/211/bdiffim/'

;stop



;Then, do the 0613 event
restore,path+'06132010/results/proflocs_0613.sav'

;193 A
restore,path+'06132010/results/193/regionData_0613_193.sav'
aia_bdiff_image_velextract,totaldata,indices,[35,60],proflocs,10,$
                           outpath=path+'06132010/results/193/bdiffim/'

stop

;211 A
restore,path+'06132010/results/211/regionData_0613_211.sav'
aia_bdiff_image_velextract,totaldata,indices,[35,60],proflocs,10,$
                           outpath=path+'06132010/results/211/bdiffim/'


end
