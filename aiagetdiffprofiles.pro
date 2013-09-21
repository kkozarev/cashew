pro aiagetdiffprofiles,data,indices,proflocs,outProfile,avgProfile,basediffProfile,imProfile,outpath=outpath
;A procedure for obtaining the radial profiles from specific
;wavelengths of AIA data.
;Kamen Kozarev, September 2010

;INPUTS
;data - a 3D datacube of the region for which to extract profiles.
;indices - an array of indices to accompany the data
;proflocs - the locations of the profiles
;outpath (optional) - output path for results (string)

;OUTPUT
;outProfile - the info collected along all the profiles
;
;avgProfile - the average of the summed profiles


;===========================================================
;Constants and definitions
;===========================================================

nfiles=n_elements(data[*,0,0])
nprofiles=n_elements(proflocs[*,0,0])
profRadius=n_elements(proflocs[0,0,*])
outProfile = fltarr(nprofiles,nfiles-1,profRadius)
totProfile = fltarr(nfiles-1,profRadius)
avgProfile = fltarr(nfiles-1,profRadius)
imProfile = fltarr(nfiles-1,profRadius)
basediffProfile = fltarr(nfiles,profRadius)

wav=strtrim(string(indices[0].wavelnth),2)
;===========================================================

avg=dblarr(1024,1024)
avg_exptime=average(indices[*].exptime)
avg=data[0,*,*]*avg_exptime/indices[0].exptime
for i=1,9 do avg=avg+data[i,*,*]*avg_exptime/indices[i].exptime
avg=avg/10.0



;===========================================================
;3. Load the other files, plot the percentage intensity difference
;===========================================================
for i=0,nfiles-1 do begin
   
   tmp1=data[i,*,*]*avg_exptime/indices[i].exptime
   diffim2=float(tmp1 - avg)/float(avg)
   basediffProfile[i,*]=diffim2[proflocs[0,0,*],proflocs[0,1,*]]
   for n = 1,nprofiles-1 do basediffProfile[i,*]=basediffProfile[i,*] + $
      diffim2[proflocs[n,0,*],proflocs[n,1,*]]
   basediffProfile[i,*]=basediffProfile[i,*]/float(nprofiles)


   if i ge 1 then begin
   ;make a percentage difference image
       diffim=smooth(float(tmp1-tmp0)/float(tmp0),3)
      ;diffim=tmp1
      ;diffim2=float(tmp1 - avg)/float(avg)

;Determine the difference profiles
       outProfile[0,i-1,*]= diffim[proflocs[0,0,*],proflocs[0,1,*]]
       totProfile[i-1,*] = outProfile[0,i-1,*]
       imProfile[i-1,*]=tmp1[proflocs[0,0,*],proflocs[0,1,*]]
       for n = 1,nprofiles-1 do begin
          outProfile[n,i-1,*] = diffim[proflocs[n,0,*],proflocs[n,1,*]]
                                ;plot,outProfile[n,i-1,*]
                                ;wait,0.5
          totProfile[i-1,*] = totProfile[i-1,*] + outProfile[n,i-1,*]
          imProfile[i-1,*]=imProfile[i-1,*]+tmp1[proflocs[n,0,*],proflocs[n,1,*]]
       endfor
       
       imProfile[i-1,*]=float(imProfile[i-1,*])/float(nprofiles)
       avgProfile[i-1,*]=float(totProfile[i-1,*])/float(nprofiles)
       plot,smooth(avgProfile[i-1,*],30,/edge_truncate),yrange=[-0.1,0.2]
       xyouts,0.13,0.87,'frame '+strtrim(string(i-1),2),/normal
       wait,0.6

   endif
   
   tmp0=tmp1
   
;stop
   
endfor

stop

;===========================================================


;===========================================================
;4. Optionally, save the percentage changes in the profiles between consecutive images
if keyword_set(outpath) then begin
   mm=strmid(indices[0].date_obs,5,2)
   dd=strmid(indices[0].date_obs,8,2)
   save,outProfile,avgProfile,totProfile,filename=outpath+'profiles_'+mm+dd+'_'+wav+'.sav'
endif
;===========================================================


end
