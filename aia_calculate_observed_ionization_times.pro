;+============================================================================
pro test_calculate_observed_ionization_times
;This procedure runs the aia_calculate_observed_ionization_times procedure for multiple events
;Kamen Kozarev, 10/31/2011
;This is made for the 2011 events - needs to be updated.

;loop over the events
;The ratios of wavelengths: 
; 1) 193/211
; 2) 193/335
; 3) 211/335
;more to come later...
ratios=[1,2,3]
nratios=n_elements(ratios)
numrois=5
;This array contains the observed ionization timescales for various
;events and wavelength ratios
ionizTimes=fltarr(nevents,nratios,numrois)

evlabels=['05','06','13','19','20','32','37','38']
for e=0,n_elements(evlabels)-1 do begin   
   event=load_events_info(evlabels[ev])
   for r=0,nratios-1 do begin
      aia_calculate_ionization_times,event.label,ratios[r],iontimes,inpath=event.savepath
      ionizTimes[e,r,*]=iontimes
   endfor
endfor
end
;-============================================================================



;+============================================================================
pro aia_calculate_ionization_times, event, ratio, obsIonizTimes,inpath=inpath
;PURPOSE:
;This procedure allows a user to determine ionization timescales
;based on the method used in Ma et al. (2011) - by inspecting time series
;of the ratios of intensities of two different AIA channels.
;This procedure is meant to work with the results from applying the
;batch_define_rois procedure.
;
;CATEGORY:
; AIA/Ionization
;
;INPUTS:
;
;KEYWORDS:
; 
;
;OUTPUTS:
;
; 
;DEPENDENCIES:
;
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 10/31/2011
;

;Constants and definitions:
if not keyword_set(inpath) then inpath='/Volumes/Backscratch/Users/kkozarev/AIA/events/'
wave=['193','211']

;The ratios of wavelengths:
; 1) 193/211
; 2) 193/335
; 3) 211/335
; ... (add more ratios)
;Load data for specific ratios.
   case ratio of
      1: wave=['193','211']
      2: wave=['193','335']
      3: wave=['211','335']
   endcase

nwaves=n_elements(wave)

;0. Load the ionization data for the desired event - data cubes (M px by M px by N steps) for P
;positions off the solar limb for the two EUV channels.
;roi_subdata=fltarr(NUMROI,M,M,N)
;roi_subindex=fltarr(N)
restore,inpath+event+'/ionization/rois_'+event+'_'+wave[0]+'.sav'
index1=roi_subindex
data1=roi_subdata

restore,inpath+event+'/ionization/rois_'+event+'_'+wave[1]+'.sav'
index2=roi_subindex
data2=roi_subdata

;1. For every subroi, determine the ionization time scales for all
;desired ratios
numroi=n_elements(data1[*,0,0,0])
nsteps=n_elements(data1[0,0,0,*])
nx=n_elements(data1[0,*,0,0])
ny=nx
timeseries=fltarr(nwaves,numroi,nsteps)
ratios=fltarr(numroi,nsteps)
obsIonizTimes=fltarr(numroi)


;1.1. Average the data cubes over all pixels - obtain a 1D array of N
;steps, do this for all wavelengths.
for r=0,numroi-1 do begin
   timeseries[0,r,*]=average(reform(data1[r,*,*,*]),[1,2])
   timeseries[1,r,*]=average(reform(data2[r,*,*,*]),[1,2])
   ratios[r,*]=reform(timeseries[0,r,*]/timeseries[1,r,*])
endfor


;1.2. Get the times of the timesteps in seconds from the first step.
;Convert the time to fractional JD
jdfrac=fltarr(nwaves,nsteps)
for t=0,nsteps-1 do begin
   tmp=anytim2jd(reform(index1[t].date_obs))
   jdfrac[0,t]=tmp.frac
   tmp=anytim2jd(reform(index2[t].date_obs))
   jdfrac[1,t]=tmp.frac
endfor
relmintime=min([jdfrac[0,0],jdfrac[1,0]],minind)

;Find the relative time to the first observation in either channel, in
;seconds.
times=(jdfrac[minind,*]-relmintime)*86400.

;2. Calculate the ionization timescales for the different SUBROIs.

xrange=[min(times)/60.0-0.1,max(times)/60.0+0.1]

if minind eq 0 then minind=index1 else minind=index2
reltime=strmid(minind[0].date_obs,11,8)
date=strmid(minind[0].date_obs,0,10)
csize=2.4

;Plot the ratio'd time series for every ROI. Smooth if need be...
wdef,0,800
!P.font=1
print,''
for r=0,numroi-1 do begin
   yrange=[min(ratios[r,*]),max(ratios[r,*])]	

   plot, times/60.0,ratios[r,*],$
         psym=0+5,symsize=1,$
         title='Flux ratio time series, AIA '+wave[0]+'/'+wave[1],$
         xtitle='Time relative to '+reltime+'UT, '+date+', [min]', $
         ytitle='AIA Intensity ratio',$
         thick=4,xthick=2,ythick=2,charsize=csize,charthick=3,$
         xrange=xrange,xstyle=1,$
         yrange=yrange,ystyle=1 ,background=255,color=0
  
;Ask the user to select the start of the event by clicking on the
;graph.
   print,'SUBROI #'+string(r+1)
   print,'Select the start of the ratio increase on the plot:'
   cursor,x,y,/data
   startTime=x*60.0

; Overplot a vertical line on that position.
   plots,[x,x],[yrange[0],yrange[1]],/data,color=0,linestyle=2,thick=2
   wait,0.3

;Find the peak of the ratio time series automatically. 
   tmp=max(ratios[r,*],ind)
   peaktime=times[ind]
   plots,[peaktime/60.0,peaktime/60.0],[yrange[0],yrange[1]],/data,color=0,linestyle=2,thick=2

;------------------------------------------------------------------------------------------
;Plot the flux ratio in a separate png plot.
   set_plot,'ps'
   ;set_plot,'x'
   fname='ionization_ratio_'+event+'_subroi'+strtrim(string(r+1),2)+'_'+wave[0]+'_'+wave[1]
   ;tvlct,rr,gg,bb,/get
device,file=inpath+event+'/ionization/'+fname+'.eps',$
          /inches,xsize=10.0,ysize=10.0,$
          /encaps,/color,/helvetica
   
   plot, times/60.0,ratios[r,*],$
         psym=0+5,symsize=1,$
         title='Flux ratio, '+event+'; ROI# '+strtrim(string(r+1),2)+'; AIA '+wave[0]+'/'+wave[1],$
         xtitle='Time relative to '+reltime+'UT, '+date+', [min]', $
         ytitle='AIA Intensity ratio',$
         thick=4,xthick=2,ythick=2,charsize=csize,charthick=3,$
         xrange=xrange,xstyle=1,$
         yrange=yrange,ystyle=1 ,background=255,color=0
   plots,[x,x],[yrange[0],yrange[1]],/data,color=0,linestyle=2,thick=2
   plots,[peaktime/60.0,peaktime/60.0],[yrange[0],yrange[1]],/data,color=0,linestyle=2,thick=2
   
   xyouts,0.7,0.3,'Ionization timescale: '+strtrim(string(peaktime-starttime),2)+' sec'
   
   ;image=tvrd(true=1)
   ;write_png,fname+'.png',image,rr,gg,bb
   device,/close
   set_plot,'x'
   exec='convert -flatten '+inpath+event+'/ionization/'+fname+'.eps '+inpath+event+'/ionization/'+fname+'.png'
   spawn,exec
;------------------------------------------------------------------------------------------

;The time difference is the observed ionization time scale.
   obsIonizTimes[r]=peaktime-starttime
   print,'The observed ionization timescale (in seconds) is:'
   print,obsIonizTimes[r]
   print,''
   wait,0.5
endfor	;The SUBROI loop

end
;-============================================================================
