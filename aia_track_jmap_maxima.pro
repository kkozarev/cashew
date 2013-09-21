pro test_aia_track_jmap_maxima

;Load the time height data and any additional information
;datafile='restore_me_007.sav'
;infofile='info_007.sav'

path='/home/kkozarev/Desktop/AIA/pro/e37/'
datafile=path+'restore_me_37_193_r000.sav'
infofile=path+'info_37_193_r001.sav'


radrange=[1.28,1.65] ;radial extent of the measurement
numplotmax=2 ;number of maxima to track
dynrange=[-90,45]

aia_track_jmap_maxima,datafile,infofile,path=path,numplotmax=numplotmax,allmaximind=allmaximind,allmaxima=allmaxima,nmax=nmax,allgfits=allgfits,time=time,distance=distance,dynrange=dynrange,radrange=radrange;/gaussfit,

end


function gaussianfit,x,aa
zz=(x-aa[1])/aa[2]
return,[[aa[0]*exp(-0.5*zz^2)+aa[3]+x*aa[4]+x^2*aa[5]],$  ;function
        [exp(-0.5*zz^2)],$                  ;function derivative wrt aa[0]
        [aa[0]/aa[2]*zz*exp(-0.5*zz^2)],$   ;function derivative wrt aa[1]
        [aa[0]/aa[2]*zz^2*exp(-0.5*zz^2)],$ ;function derivative wrt aa[2]
        [1.0],$                             ;function derivative wrt aa[3]
        [x],$                               ;function derivative wrt aa[4]
        [x^2]]                              ;function derivative wrt aa[5]
end



PRO aia_track_jmap_maxima,datafile,infofile,gaussfit=gaussfit,radrange=radrange,path=path,numplotmax=numplotmax,allmaximind=allmaximind,allmaxima=allmaxima,nmax=nmax,allgfits=allgfits,time=time,distance=distance,dynrange=dynrange,refine=refine
;Adapt the Callisto routine to detect AIA maxima.

!P.font=1
!p.position=[0.1,0.13,0.93,0.92]

if not keyword_set(path) then path = './'
restore,datafile
restore,infofile
data_subindex=data_subindex[plotted_frame_initial:plotted_frame_final]
wav=strtrim(string(data_subindex[0].wavelnth),2)
if not keyword_set(dynrange) then dynrange=[-100,100]

;How many maxima to track
if not keyword_set(numplotmax) then numplotmax=2

;Plot the time-height map
polyfill_process, data_thin_wave, data_subindex, data_rotation_angle,data_date,data_evnum,time=time,rad=rad,dynrange=dynrange

distance=rad
loadct,0,/silent
tvlct,rr,gg,bb,/get
tvlct,reverse(rr),reverse(gg),reverse(bb)

ntimes=n_elements(time)
;dtime=time[1:ntimes-1]-time[0:ntimes-2]
nrad=n_elements(rad)

if not keyword_set(radrange) then begin
   radrange=[1.08,max(rad)]
endif else begin
   if radrange[0] lt min(rad) then radrange[0]=min(rad)
   if radrange[1] gt max(rad) then radrange[1]=max(rad)
endelse
radrange=double(radrange)

oplot,[min(time),max(time)],[radrange[0],radrange[0]],thick=3,linestyle=2,color=0
oplot,[min(time),max(time)],[radrange[1],radrange[1]],thick=3,linestyle=2,color=0
data=data_thin_wave
index=data_subindex[0:ntimes-1]
timrng=[0,ntimes-1]

;Find the radial range index
radrng=[min(where(rad-radrange[0] ge -1.e-6)),min(where(rad-radrange[1] ge -1.0e-6))]

;Part 2. Find and fit the time-height maxima
wdef,1,900,500
allgfits=fltarr(100,5,timrng[1]-timrng[0]+1)
allmaxima=fltarr(100,timrng[1]-timrng[0]+1)
allmaximind=fltarr(100,timrng[1]-timrng[0]+1)
mymaxima=fltarr(100,timrng[1]-timrng[0]+1)
mymaximind=fltarr(100,timrng[1]-timrng[0]+1)
nmax=intarr(timrng[1]-timrng[0]+1)


;THE TIME LOOP!
for timind=timrng[0],timrng[1] do begin
   
;Smoothing the array should be an optional input
   arr=smooth(reform(data[timind,radrng[0]:radrng[1]]),2,/edge_truncate)
;arr=reform(data[timind,radrng[0]:radrng[1]])
   plot,rad[radrng[0]:radrng[1]],arr,xrange=radrange,yrange=[min(arr),max(arr)],/xs,$
        charsize=2,charthick=3,xtitle = 'Radial height, Rs' ;,thick=2
;Plot the mean value
;oplot,radrange,[mean(arr),mean(arr)],thick=2
   
   
;This function finds the local minima
   minpxsize=25
   ind=lclxtrem(arr,minpxsize)  ;minpxsize should be a keyword
   
;Record the maxima in each of the intervals set by the minima locations.
   maxima=fltarr(100)
   indmaxima=intarr(100)
   gfit=fltarr(100,5)
   cc=0
   
   for ii=0,n_elements(ind)-1 do begin
                                ;plot the ranges of the local maximum intervals
      oplot,[rad[ind[ii]+radrng[0]],rad[ind[ii]+radrng[0]]],[min(data),max(data)]
                                ;find the local maximum in every interval
      if ii lt n_elements(ind)-1 then begin
         locarr=arr[ind[ii]:ind[ii+1]]
         locy=rad[ind[ii]+radrng[0]:ind[ii+1]+radrng[0]]
         locmax=max(locarr)
         lmaxind=ind[ii]+!C+radrng[0]
         
                                ;This is a filter that lets through only the biggest maxima.
                                ;locmax lt mean(arr) or 
         if (ind[ii+1]-ind[ii] le 6) then continue
         maxima[cc]=locmax
         indmaxima[cc]=lmaxind
         
                                ;Do a Gaussian fit to the maxima profiles to get an initial estimate
         if keyword_set(gaussfit) then begin
            res=gaussfit(locy,locarr,aa,nterms=6)
                                ;Follow up with a Levenberg-Marquardt fitting algorightm
            res=lmfit(locy,locarr,aa,/double,function_name='gaussianfit',sigma=sigma)
            if cc eq 0 then print,aa[1],rad[lmaxind]
            gfit[cc,0]=aa[1]                  ;X-location (radial) of the peak
            gfit[cc,1]=max(res)               ;Y-location (intensity) of the peak
            gfit[cc,2]=2*sqrt(2*alog(2)*aa[2]^2) ;The FWHM of the gaussian fit
            if gfit[cc,2] eq 'Inf' or gfit[cc,2] eq 'NaN' then gfit[cc,2]=1.0e-10
            gfit[cc,3]=sigma[1] ;The error in radial position of the maximum
            gfit[cc,4]=sigma[0] ;The error in the fitted peak value
            
            zz=(locy-aa[1])/aa[2]
                                ;6-term fit
            oplot,locy,aa[0]*exp(-0.5*zz^2)+aa[3]+locy*aa[4]+locy^2*aa[5],linestyle=2,thick=2
                                ;5-term fit
                                ;oplot,locy,aa[0]*exp(-0.5*zz^2)+aa[3]+locy*aa[4],linestyle=2,thick=2
                                ;4-term fit
                                ;oplot,locy,aa[3]+aa[0]*exp(-0.5*zz^2),linestyle=2,thick=2
            plots,rad[lmaxind],locmax,psym=2,symsize=2,thick=2,/data
                                ;The FWHM of the gaussian fit is 2*sqrt(2*alog(2)*aa[2]^2)
         endif
         cc++
      endif
   endfor
   
   nmax[timind]=cc
   maxima=maxima[0:cc-1]
   indmaxima=indmaxima[0:cc-1]
   gfit=gfit[0:cc-1,*]
   
;Order the local maxima by size
   sort=reverse(sort(maxima))
   ordmaxima=maxima[sort]
   ordindmaxima=indmaxima[sort]
   gfit=gfit[sort,*]
   
;Save to the overall maximum catalog
   allmaximind[0:cc-1,timind-timrng[0]]=ordindmaxima
   allmaxima[0:cc-1,timind-timrng[0]]=ordmaxima
   allgfits[0:cc-1,*,timind-timrng[0]]=gfit
   
;Set an initial threshold in pixels for how much the maximum could have moved
   inithresh=10
   thresh=fltarr(numplotmax)+inithresh
   
   if timind eq timrng[0] then begin
      mymaximind[0:cc-1,timind-timrng[0]]=ordindmaxima
      mymaxima[0:cc-1,timind-timrng[0]]=ordmaxima
      
   endif else begin
                                ;1. Check if the Nth maximum is within +/-thresh/2.0 pixels of the position of the
                                ;Nth maximum from last step.
                                ;2. If yes, record as the same maximum from last time, calculate the speed
                                ;of the maximum.
                                ;3. If not, search for a maximum that is within +/-thresh/2.0 pixels this
                                ;time and assume that is the maximum we want.
      
                                ;go over the first numplotmax maxima that the code has found
                                ;Check if the pixel difference with the previous location is larger than the threshold
      
                                ;goto,jump
      
      for ii=0,numplotmax-1 do begin
         ci=ii
         if abs(ordindmaxima[ii] - mymaximind[ii,timind-timrng[0]-1]) gt 2*thresh[ii] then begin
                                ;If yes, go and find another maximum
                                ;within that range and move it to the
                                ;current maximum position
                                ;for tt=ci,nmax[timind]-1 do begin
            dmax=ordindmaxima[ii] - mymaximind[ii+1:nmax[timind]-1,timind-timrng[0]-1]
            
                                ;check if there's a closer maximum - if not, keep this one.
            mmm=-1.0
            mult=1.0
            while mmm lt 0 do begin
               if mult*thresh[ii] gt 3*inithresh then begin
                  mmm=ii
                  break
               endif
               
               tmp=where(dmax le mult*thresh[ii] and dmax gt 0.0)
               if tmp[0] ge 0 then mmm=min(tmp) else mult++
            endwhile
            
            mymaximind[ii,timind-timrng[0]]=ordindmaxima[mmm]
            mymaxima[ii,timind-timrng[0]]=ordmaxima[mmm]
            
         endif else begin
                                ;Otherwise record the position for the same maximum
            mymaximind[ii,timind-timrng[0]]=ordindmaxima
            mymaxima[ii,timind-timrng[0]]=ordmaxima
            if thresh[ii] gt inithresh then $
               thresh=mymaximind[ii,timind-timrng[0]]-mymaximind[ii,timind-timrng[0]-1]
                                ;print,thresh[ii]
                                ;stop
         endelse
                                ;Update the threshold because the speeds will vary
                                ;if ii ge 3 then thresh=mean(deriv(mymaximind[ii-3:ii,timind-timrng[0]-1]))
      endfor
                                ;stop
   endelse
   
;jump:
   
;for ii=0,n_elements(sort)-1 do begin
                                ;oplot,[rad[ordindmaxima[ii]],rad[ordindmaxima[ii]]],$
         ;[min(data),max(data)],linestyle=3,thick=3
   ;print,ordindmaxima[ii],ordmaxima[ii]
;endfor

endfor



;Overplot the time-height diagram again, with the maxima as points.
wset,0
polyfill_process, data_thin_wave, data_subindex, data_rotation_angle,data_date,data_evnum,dynrange=dynrange
loadct,39,/silent
;plot horizontal lines for the range of radial heights
oplot,[min(time),max(time)],[radrange[0],radrange[0]],thick=3,linestyle=2,color=0
oplot,[min(time),max(time)],[radrange[1],radrange[1]],thick=3,linestyle=2,color=0

colors=[255,190,250,60,30]
for timind=timrng[0], timrng[1] do begin
   tmp=where(allmaximind[*,timind-timrng[0]] eq 0)
   nmaxpts=min(tmp)
   ;stop
;Plot the maxima   
   for mm=0,nmaxpts-1 do begin
      if mm eq numplotmax then break
      if timind lt ntimes-1 then dt = time[timind+1]-time[timind]
      ;Plot the data maxima with errors
      if not keyword_set(gaussfit) then begin

;         plots,time[timind]+dtime[timind]/2.0,rad[mymaximind[mm,timind-timrng[0]]],$
;               color=colors[mm],psym=1,symsize=1,thick=4
         plots,time[timind]+dt/2.0,rad[allmaximind[mm,timind-timrng[0]]],$
               color=colors[mm],psym=1,symsize=1,thick=4
         oploterror,time[timind]+dt/2.0,dt,$
                    rad[allmaximind[mm,timind-timrng[0]]],0.0,color=colors[mm],thick=2,/nohat
      endif else begin
         ;Plot the maxima of the gaussian fits, together with the errors
         plots,time[timind]+dt/2.0,allgfits[mm,0,timind-timrng[0]],$
               color=colors[mm],psym=1,symsize=1,thick=4
         oploterror,time[timind]+dt/2.0,dt,allgfits[mm,0,timind-timrng[0]],$
                    allgfits[mm,3,timind-timrng[0]],color=colors[mm],thick=2,/nohat
      endelse
   endfor
endfor

;Plot the legend for the different maxima
for mm=0,numplotmax-1 do begin
   if mm le 1 then polyfill,[0.953,0.99,0.99,0.953],[0.785,0.785,0.815,0.815]-mm*0.05,/norm,color=0
   plots,0.96,0.8-mm*0.05,psym=1,symsize=1,thick=4,/norm,color=colors[mm]
   xyouts,0.961,0.79-mm*0.05,' #'+strtrim(string(mm+1),2),color=colors[mm],/norm,charsize=1.6
endfor




;+============================================================================
;Fit the maxima positions for determination of kinematics.
;=========================================================

uinput=0.0
while uinput le 0.0 or uinput gt numplotmax do $
read,uinput,prompt='Which set of maxima would you like to fit? (1-'+strtrim(string(numplotmax),2)+')'
uinput--


print,''
print,'Select starting point:'
cursor,x,y,/down,/data
plots,x,y,psym=5,symsize=2,thick=2,color=100
sp=min(where(fix(time-x) gt 0.0))-1
print,sp,time[sp]


print,'Select ending point:'
cursor,x,y,/down,/data
plots,x,y,psym=5,symsize=2,thick=2,color=200
ep=min(where(fix(time-x) gt 0.0))-1

;record the image
image=tvrd(/true)
savname='e'+data_evnum+'_'+data_date+'_'+wav+'emission_maxima_all.png'
if keyword_set(outpath) then savname=outpath+savname
write_png,savname,image


;Search for the edges of the wave
wave_frontedge=dblarr(ep-sp+1)
wave_backedge=dblarr(ep-sp+1)
for ii=sp,ep do begin
   x=rad[allmaximind[uinput,ii]:*]
   ;y=smooth(reform(data[ii,allmaximind[uinput,ii]:*]),40,/edge_truncate)
   
if keyword_set(refine) then begin
   if ii gt sp then begin
      if rad[allmaximind[uinput,ii]] lt wave_backedge[ii-sp-1] or rad[allmaximind[uinput,ii]] lt rad[allmaximind[uinput,ii-1]] - 20./data_subindex[0].r_sun then begin
                                ;Refine the positions of the maxima
         print,''
         print,'Refining the positions of the maximum...'
         
         tmp=rad[allmaximind[0:nmax[ii]-1,ii]] 
         res=where(tmp ge wave_backedge[ii-sp-1] and tmp le wave_frontedge[ii-sp-1])
         if res[0] eq -1 then begin
            cnt=0
            print,'res[0]=-1'
            while res[0] eq -1 do begin
               res=where(tmp ge wave_backedge[ii-sp-1]-cnt*2.0 and tmp le wave_frontedge[ii-sp-1]+cnt*2.0)
               cnt++
            endwhile
            
         endif else begin
            tmp=rad[allmaximind[res,ii]]
            maxy=max(tmp,ind)
            maxind=allmaximind[res[ind],ii]
            allmaximind[uinput,ii]=maxind
            allmaxima[uinput,ii]=maxy
         endelse
      endif
        
   endif
endif


;The front edge
   y=reform(data[ii,allmaximind[uinput,ii]:*])
   tmp=min(where(y le 0.2*max(y)))

   wave_frontedge[ii-sp]=rad[allmaximind[uinput,ii]+tmp]
   ;plots,time[ii]+dt/2.0,rad[allmaximind[uinput,ii]+tmp],$
   ;      color=colors[uinput],psym=4,symsize=1,thick=1

;The back edge
   y=reform(data[ii,0:allmaximind[uinput,ii]])
   tmp=max(where(y le 0.2*max(y)))
   wave_backedge[ii-sp]=rad[tmp]
   ;plots,time[ii]+dt/2.0,rad[tmp],$
   ;      color=colors[uinput],psym=4,symsize=1,thick=1
   
   oplot,[time[ii]+dt/2.0,time[ii]+dt/2.0],[wave_backedge[ii-sp],wave_frontedge[ii-sp]],color=colors[uinput],thick=1
endfor





;Overplot just the points to be fitted.
polyfill_process, data_thin_wave, data_subindex, data_rotation_angle,data_date,data_evnum,dynrange=dynrange
loadct,39,/silent
for ii=sp,ep do begin
   plots,time[ii]+dt/2.0,rad[allmaximind[uinput,ii]],$
               color=colors[uinput],psym=1,symsize=1,thick=4
         oploterror,time[ii]+dt/2.0,dt,$
                    rad[allmaximind[uinput,ii]],0.0,color=colors[uinput],thick=2,/nohat
   oplot,[time[ii]+dt/2.0,time[ii]+dt/2.0],[wave_backedge[ii-sp],wave_frontedge[ii-sp]],color=colors[uinput],thick=1
endfor



;Do fitting for the maximum
print,''
print,'Fitting a second-order polynomial to the wave peak positions...'
raddist=rad[allmaximind[uinput,sp:ep]]
times=time[sp:ep]
bootstrap_sdo,raddist,times,fit_line, p1, p2, p3, s1, s2, s3

wave_rad_model=p1[0] + p2[0] * (time[sp:ep])+ 0.5 * p3[0] * (time[sp:ep])^2
oplot,time[sp:ep]+dt/2.0,wave_rad_model,thick=3
;initial height
r0=rad[allmaximind[uinput,sp]]
print,'R0 = '+strtrim(string(r0),2)+' +/-'+strtrim(string(s1[0],format='(f9.5)'),2)+' Rs'
;final height
rf=rad[allmaximind[uinput,ep]]
print,'Rf = '+strtrim(string(rf),2)+' Rs'
;initial speed
v0=p2[0]*6.955e5
errv0=s2[0]*6.955e5
print,'V0 = '+strtrim(string(v0),2)+' +/-'+strtrim(string(errv0),2)+' km/s'
;final speed
tmp=p2[0]+p3[0]*(time[sp:ep]-time[sp])
vf=tmp[n_elements(tmp)-1]*6.955e5
print,'Vf = '+strtrim(string(vf),2)+ ' km/s'
accel=p3[0]*6.955e8
erraccel=s3[0]*6.955e8
print,'a = '+strtrim(string(accel),2)+' +/-'+strtrim(string(erraccel),2)+' m/s^2'
print,''




;record the image
image=tvrd(/true)
savname='e'+data_evnum+'_'+data_date+'_'+wav+'emission_maxima.png'
if keyword_set(outpath) then savname=outpath+savname
write_png,savname,image

;Save the output:
wave_times=time[sp:ep]
wave_rads=reform(rad[allmaximind[uinput,sp:ep]])
wave_indices=reform(allmaximind[uinput,sp:ep])
ind=data_subindex[sp:ep]
wave_data=data_thin_wave[sp:ep,allmaximind[uinput,sp]:allmaximind[uinput,ep]]
savname='e'+data_evnum+'_'+data_date+'_'+wav+'_jmap_measurements.sav'
if keyword_set(outpath) then savname=outpath+savname
save,filename=savname,time,rad,ind,wave_times,wave_rads,wave_indices,wave_data,wave_frontedge,wave_backedge,r0,rf,v0,vf,errv0,accel,erraccel,wave_rad_model



;+============================================================================
;Plot a time series of the FWHM of the fits
;==========================================

cols=[0,120,100,160,200,230]
if keyword_set(gaussfit) then begin
   wdef,2,900,500
   loadct,0,/silent
   yrange=[min(allgfits[0:numplotmax-1,2,*]),max(allgfits[0:numplotmax-1,2,*])]
   yrange[1]=[0.1]
   xrange=[min(time),max(time)]
   
   plot,time,allgfits[0,2,*],xrange=xrange,yrange=yrange,/xs,$
        charsize=2,charthick=3,xtitle = 'Time [sec]',ytitle='FWHM [Rs]',$
        title='FWHM of fitted peaks',/nodata,background=255,color=0
   for mm=uinput,uinput do begin
      col=cols[mm]
      oplot,time,allgfits[mm,2,*],psym=10,color=col,thick=2
      ;oploterror,time,time[1:ntimes-1]-time[0:ntimes-2],allgfits[0,2,*],fltarr(ntimes),$
      ;           /nohat,color=col,thick=2
      
   endfor
   
   image=tvrd(/true)
   savname='e'+data_evnum+'_'+data_date+'_'+wav+'_gaussfit_fwhm_timeseries.png'
   if keyword_set(outpath) then savname=outpath+savname
   write_png,savname,image
endif
;-============================================================================




;+============================================================================
;Plot a time series of the Fitted Peaks Intensities
;==================================================

if keyword_set(gaussfit) then begin
   wdef,3,900,500
   loadct,0,/silent
   yrange=[min(allgfits[0:numplotmax-1,1,*]),max(allgfits[0:numplotmax-1,1,*])]
   xrange=[min(time),max(time)]
   
   plot,time,allgfits[0,1,*],xrange=xrange,yrange=yrange,/xs,/ys,$
        charsize=2,charthick=3,xtitle = 'Time [sec]',ytitle=' [ADU]',$
        title='Fitted Peaks Intensities',/nodata,background=255,color=0
   
   for mm=uinput,uinput do begin
      col=cols[mm]
      oplot,time,allgfits[mm,1,*],psym=10,color=col,thick=2
      ;oploterror,time,allgfits[mm,1,*],allgfits[mm,4,*],$
      ;           /nohat,color=col,thick=2
;,time[1:ntimes-1]-time[0:ntimes-2]
   endfor
image=tvrd(/true)
savname='e'+data_evnum+'_'+data_date+'_'+wav+'_gaussfit_peak_intensities_timeseries.png'
if keyword_set(outpath) then savname=outpath+savname
write_png,savname,image
endif else begin

;Plot a time series of the Maxima intensities
   wdef,3,900,500
   loadct,0,/silent
   yrange=[min(allmaxima[0:numplotmax-1,*]),max(allmaxima[0:numplotmax-1,*])]
   xrange=[min(time),max(time)]
   
   plot,time,allmaxima[0,*],xrange=xrange,yrange=yrange,/xs,/ys,$
        charsize=2,charthick=3,xtitle = 'Time [sec]',ytitle=' [ADU]',$
        title='Emission Peak Intensities',/nodata,background=255,color=0
  
   
   for mm=uinput,uinput do begin
      col=cols[mm]
      oplot,time,allmaxima[mm,*],psym=10,color=col,thick=2
      ;oploterror,time,allgfits[mm,1,*],allgfits[mm,4,*],$
      ;           /nohat,color=col,thick=2
;,time[1:ntimes-1]-time[0:ntimes-2]
   endfor
image=tvrd(/true)
savname='e'+data_evnum+'_'+data_date+'_'+wav+'peak_intensities_timeseries.png'
if keyword_set(outpath) then savname=outpath+savname
write_png,savname,image
endelse
;-============================================================================


end





;+
; NAME:
;     badpar
; PURPOSE: (one line)
;     Validate an input parameter against valid entries.
; DESCRIPTION:
;
;     This is a general parameter checking function for validating input
;     quantities in other procedures and functions.  This routine will
;     generate an error message indicating what is wrong with the item.
;
;     Example of use:
;
;     pro foo,array
;     if badpar(array,[4,5],2,CALLER='foo') then return
;        .
;        . code for foo .
;        .
;     end
;
;
;     This would cause an immediate return to the routine that called foo
;     with an error message if the input was not either floating or double
;     and 2 dimensional.
;
;     As of IDL v3.0, these are the recognized type codes (see 1-218 in
;        reference guide).
;
;        Type
;        Code     Data Type
;        ----    -----------------------------
;          0      Undefined
;          1      Byte
;          2      Integer
;          3      Longword integer
;          4      Floating point
;          5      Double-precision floating
;          6      Complex floating
;          7      String
;          8      Structure
;
; CATEGORY:
;  Utility
; CALLING SEQUENCE:
;     val = badpar(param,goodtype,goodrank)
; INPUTS:
;     param    - IDL variable to validate.
;     goodtype - Scalar or vector of type codes that are valid.
;     goodrank - Scalar or vector of valid ranks.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD PARAMETERS:
;     CALLER   - String identifying the calling routine.
;     DEFAULT  - Value to return in param if undefined and undefined allowed.
;     DIMEN    - Dimensions of variable.
;     NPTS     - Total number of elements in variable.
;     RANK     - Rank of variable.
;     TYPE     - Type of variable.
; OUTPUTS:
;     Return value is true if the parameter is bad.  False if good.
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;     3/24/93 - Written by Marc W. Buie, Lowell Observatory.
;     4/27/93 - MWB, added TYPE and DEFAULT keywords.
;-
function badpar,param,goodtype,goodrank, $
            CALLER=caller, DEFAULT=default, DIMEN=dimen, $
            NPTS=npts, RANK=rank, TYPE=type

errval = 0

sz = size(param)

rank = sz[0]
type = sz[rank+1]
npts = sz[rank+2]

err1=''
err2=''

if rank eq 0 then dimen=0 else dimen = sz[1:rank]

z=where(goodtype eq type, count)
if count eq 0 then begin
   case type of
      0 :    err1 = 'Undefined variable is not allowed.'
      1 :    err1 = 'Byte variable type is not allowed.'
      2 :    err1 = 'Integer variable type is not allowed.'
      3 :    err1 = 'Longword integer variable type is not allowed.'
      4 :    err1 = 'Floating point variable type is not allowed.'
      5 :    err1 = 'Double-precision floating point variable type is not allowed.'
      6 :    err1 = 'Complex floating point variable type is not allowed.'
      7 :    err1 = 'String variable type is not allowed.'
      8 :    err1 = 'Structure variable type is not allowed.'
      else : err1 = 'Unrecognized variable type code.  Impossible!'
   endcase
   errval=1
endif

if type ne 0 then begin
   z=where(goodrank eq rank, count)
   if count eq 0 then begin
      case rank of
         0 :    err2 = 'Scalar variables are not allowed.'
         1 :    err2 = 'Vector variables are not allowed.'
         2 :    err2 = '2-D variables are not allowed.'
         3 :    err2 = '3-D variables are not allowed.'
         4 :    err2 = '4-D variables are not allowed.'
         5 :    err2 = '5-D variables are not allowed.'
         6 :    err2 = '6-D variables are not allowed.'
         7 :    err2 = '7-D variables are not allowed.'
         8 :    err2 = '8-D variables are not allowed.'
         else : err2 = 'Unrecognized variable rank.  Impossible!'
      endcase
      errval=1
   endif
endif

if errval then begin
   if not keyword_set(caller) then caller = ''
   print,caller,'Illegal variable encountered.'
   if err1 ne '' then print,err1
   if err2 ne '' then print,err2
   return,errval
endif

if type eq 0 then begin
   szd = size(default)
   if szd[szd[0]+1] ne 0 then begin
      param = default
      sz    = size(param)
      rank  = sz[0]
      type  = sz[rank+1]
      npts  = sz[rank+2]
   endif
endif

return,errval

end


;+
; NAME:
;  lclxtrem
; PURPOSE:
;  Find local minima or maxima in a 1-d vector.
; DESCRIPTION:
;
; CATEGORY:
;  Spectroscopy
; CALLING SEQUENCE:
;  idx=lclxtrem(vec,width,[/MAXIMA])
; INPUTS:
;  vec - Input vector of data points.
;  width - size of zone to search, minima (or maxima) separated by less than
;            width are never returned.  (Default = 5)
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;  MAXIMA  - Flag, if set, causes program to search for local maxima, the default
;            is to search for local minima
; OUTPUTS:
;  Returns indicies into vec that give local extrema.
; KEYWORD OUTPUT PARAMETERS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;  97/12/5, Written by Marc W. Buie, Lowell Observatory
;
;-
FUNCTION lclxtrem,vec,in_width,MAXIMA=maxima

   IF badpar(vec,[2,3,4,5],1,caller="LCLXTREM: (vec)") THEN return,-1
   IF badpar(in_width,[0,2,3,4,5],0,caller="LCLXTREM: (width)",default=5) THEN return,-1
   IF badpar(maxima,[0,1,2,3],0,caller="LCLXTREM: (MAXIMA)", $
                                                          default=0) THEN return,-1

   n=n_elements(vec)

   width=fix(in_width/2)*2+1
   wa=lindgen(width)
   idx=lonarr(n-width+1)
   FOR i=0,n_elements(idx)-1 DO BEGIN
      IF maxima THEN BEGIN
         z=where(vec[wa+i] eq max(vec[wa+i]))
      ENDIF ELSE BEGIN
         z=where(vec[wa+i] eq min(vec[wa+i]))
      ENDELSE
      idx[i]=z[0]+i
   ENDFOR

   idx=idx[uniq(idx)]

   FOR i=0,n_elements(idx)-2 DO BEGIN
      IF maxima THEN BEGIN
         rmin = min(vec[idx[i]:idx[i+1]])
         IF rmin eq vec[idx[i]] THEN BEGIN
            j=i
            old=idx[j]
            REPEAT BEGIN
               idx[j] = idx[i+1]
               if j ne 0 then j=j-1
            ENDREP UNTIL j eq 0 or idx[j] ne old
         ENDIF ELSE IF rmin eq vec[idx[i+1]] THEN BEGIN
            idx[i+1] = idx[i]
         ENDIF

      ENDIF ELSE BEGIN
         rmax = max(vec[idx[i]:idx[i+1]])
         IF rmax eq vec[idx[i]] THEN BEGIN
            j=i
            old=idx[j]
            REPEAT BEGIN
               idx[j] = idx[i+1]
               if i gt 0 then j=j-1 else j=0
            ENDREP UNTIL j eq 0 or idx[j] ne old
         ENDIF ELSE IF rmax eq vec[idx[i+1]] THEN BEGIN
            idx[i+1] = idx[i]
         ENDIF
      ENDELSE
   ENDFOR

   idx=idx[uniq(idx)]

   return,idx
END
