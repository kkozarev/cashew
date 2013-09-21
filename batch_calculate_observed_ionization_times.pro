pro batch_calculate_observed_ionization_times
;This procedure runs the aia_calculate_observed_ionization_times procedure for multiple events
;This procedure is meant to work with the results from applying the
;batch_define_rois procedure, for the 2011 AIA event study.
;Kamen Kozarev, 10/31/2011


evindex=['05','06','13','19','20','32','37','38']
;eventsToRun=[0,1,2,3,4,5,6,7]
eventsToRun=[6]
nevents=n_elements(eventsToRun)
numrois=5

;loop over the events
;The ratios of wavelengths: 
; 1) 193/211
; 2) 193/335
; 3) 211/335
;more to come later...
ratios=[1,2,3]
nratios=n_elements(ratios)

;This array contains the observed ionization timescales for various
;events and wavelength ratios
ionizTimes=fltarr(nevents,nratios,numrois)

for e=0,nevents-1 do begin
   event=eventsToRun[e]
   eventName='e'+evindex[event]
   for r=0,nratios-1 do begin
      aia_calculate_observed_ionization_times,eventName,ratios[r],iontimes
      ionizTimes[e,r,*]=iontimes
   endfor
endfor

end
