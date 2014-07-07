;+============================================================================
pro test_aia_measure_ionization_times
;This procedure runs the aia_calculate_observed_ionization_times procedure for multiple events
;Kamen Kozarev, 10/31/2011
;This is made for the 2011 events - needs to be updated.

;loop over the events
;The ratios of wavelengths: 
; 1) 211/193
; 2) 335/193
; 3) 335/211
;more to come later...
  ratios=[1]
  nratios=n_elements(ratios)
;This array contains the observed ionization timescales for various
;events and wavelength ratios
  
;You can run for one event, like this.
  one=1
  if one eq 1 then begin
     event=load_events_info(label='test')
     for r=0,nratios-1 do begin
        iontimes=aia_measure_ionization_times(event,ratio=ratios[r])
        if r eq 0 then ionizTimes=fltarr(nratios,n_elements(iontimes))
        ionizTimes[r,*]=iontimes
     endfor
  endif
  
  
;Alternatively, run for all events
  all=0
  if all eq 1 then begin
     events=load_events_info()
     nevents=n_elements(events)
     for ev=0,n_elements(nevents)-1 do begin   
        event=events[ev]
        for r=0,nratios-1 do begin
           iontimes=aia_measure_ionization_times(event,ratio=ratios[r])
           if r eq 0 then ionizTimes=fltarr(nevents,nratios,n_elements(iontimes))
           ionizTimes[e,r,*]=iontimes
        endfor
     endfor
  endif
  
  stop
end
;-============================================================================

 

;+============================================================================
function aia_measure_ionization_times, event, ratio=ratio,inpath=inpath
;PURPOSE:
;This procedure allows a user to determine ionization timescales
;based on the method used in Ma et al. (2011) - by inspecting time series
;of the ratios of intensities of two different AIA channels.
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
if not keyword_set(inpath) then inpath=event.ionizationpath
wave=['211','193']
label=event.label
date=event.date
;The ratios of wavelengths:

; ... (add more ratios)
;Load data for specific ratios.
if not keyword_set(ratio) then ratio=1
case ratio of
   1: wave=['211','193']
   2: wave=['335','193']
   
;   3: wave=['335','211']
endcase

nwaves=n_elements(wave)

;0. Load the ionization data for the desired event - data cubes (M px by M px by N steps) for P
;positions off the solar limb for the two EUV channels.
;roi_subdata=fltarr(NUMROI,M,M,N)
;roi_subindex=fltarr(N)
restore,inpath+'rois_'+date+'_'+label+'_'+wave[0]+'.sav'
index1=roi_subindex
data1=roi_subdata

restore,inpath+'rois_'+date+'_'+label+'_'+wave[1]+'.sav'
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
date=event.date ;strmid(minind[0].date_obs,0,10)
csize=2.4

;Plot the ratio'd time series for every ROI. Smooth if need be...
wdef,0,800
!P.font=1
temps=[2.2,2.4,2.6,2.8,3.0]*1.e6

for r=0,numroi-1 do begin
   
   print,''
   print,'************************************'
   print,'Theoretical ionization time scales:'
   rdens=get_coronal_density(roi_radheight[r])
   for ii=0,n_elements(temps)-1 do begin
      iontmp=aia_theoretical_ionization_times(temps[ii],rdens,wave[0])
      print,'T = '+strtrim(string(temps[ii],format='(e10.2)'),2) + $
            ' K        t = '+strtrim(string(iontmp,format='(f7.2)'),2)+' s'
   endfor


   yrange=[min(ratios[r,*])/1.02,max(ratios[r,*])*1.02]	

   plot, times/60.0,ratios[r,*],$
         psym=0+5,symsize=1,$
         title='Flux ratio time series, AIA '+wave[0]+'/'+wave[1]+' R'+strtrim(string(r+1),2),$
         xtitle='Time relative to '+reltime+'UT, '+date+', [min]', $
         ytitle='AIA Intensity ratio',$
         thick=4,xthick=2,ythick=2,charsize=csize,charthick=3,$
         xrange=xrange,xstyle=1,$
         yrange=yrange,ystyle=1 ,background=255,color=0
  
;Ask the user to select the start of the event by clicking on the
;graph.
   print,'SUBROI #'+strtrim(string(r+1),2)
   print,'Select the start of the ratio increase on the plot:'
   cursor,x,y,/data,/down
   xind=min(where(times/60. ge x))
   startTime=x*60.0

; Overplot a vertical line on that position.
   plots,[x,x],[yrange[0],yrange[1]],/data,color=0,linestyle=2,thick=2
   wait,0.3

;Find the peak of the ratio time series automatically. 
   ;tmp=max(ratios[r,xind:*],ind)
   
   ;Or, do it manually again.
   print,'Select the peak of the ratio increase on the plot:'
   cursor,x,y,/data,/down
   xind=min(where(times/60. ge x))
   peakTime=x*60.0
   plots,[peakTime/60.0,peakTime/60.0],[yrange[0],yrange[1]],/data,color=0,linestyle=2,thick=2
   
;------------------------------------------------------------------------------------------
;Plot the flux ratio in a separate png plot.
   set_plot,'ps'
   ;set_plot,'x'
   fname='ionization_ratio_'+date+'_'+label+'_r'+strtrim(string(r+1),2)+'_'+wave[0]+'_'+wave[1]
   ;tvlct,rr,gg,bb,/get
   device,file=inpath+fname+'.eps',$
          /inches,xsize=10.0,ysize=10.0,$
          /encaps,/color,/helvetica
   
   plot, times/60.0,ratios[r,*],$
         psym=0+5,symsize=1,$
         title='Flux ratio, '+label+'; ROI# '+strtrim(string(r+1),2)+'; AIA '+wave[0]+'/'+wave[1],$
         xtitle='Time relative to '+reltime+'UT, '+date+', [min]', $
         ytitle='AIA Intensity ratio',$
         thick=4,xthick=2,ythick=2,charsize=csize,charthick=3,$
         xrange=xrange,xstyle=1,$
         yrange=yrange,ystyle=1 ,background=255,color=0
   plots,[x,x],[yrange[0],yrange[1]],/data,color=0,linestyle=2,thick=2
   plots,[peaktime/60.0,peaktime/60.0],[yrange[0],yrange[1]],/data,color=0,linestyle=2,thick=2
   
   xyouts,0.7,0.3,'Observed Ionization timescale: '+strtrim(string(peaktime-starttime),2)+' sec'
   
   ;image=tvrd(true=1)
   ;write_png,fname+'.png',image,rr,gg,bb
   device,/close
   set_plot,'x'
   exec='convert -flatten '+inpath+fname+'.eps '+inpath+fname+'.png; rm '+inpath+fname+'.eps'
   spawn,exec
;------------------------------------------------------------------------------------------

;The time difference is the observed ionization time scale.
   obsIonizTimes[r]=peaktime-starttime
   print,''
   print,'The observed ionization timescale is '+ strtrim(string(obsIonizTimes[r]),2)+ ' seconds.'
   print,''
   wait,0.5
   print,'************************************'
   print,''
endfor	;The SUBROI loop

save,filename=inpath+'ionization_'+date+'_'+label+'_'+wave[0]+'_'+wave[1]+'_timescales.sav',obsIonizTimes,wave,times,ratios

return,obsIonizTimes

end
;-============================================================================
