pro generate_kinematics_plots

;PURPOSE
;Procedure to run kinematics processing on all of the events
;listed in events.json. Control where the script starts processing
;by changing the starting location of ev in the for loop


rrange=[1.1,1.37]
wav=['193','211']

events=load_events_info()
for ev=5, n_elements(events)-1 do begin
   print, "CURRENT ITERATION", ev

   event=events[ev]
   print, "Current event is: ", event.label
   if event.label eq 'test' then continue
   if event.web eq 1 then begin
      for w=0, n_elements(wav)-1 do begin
         print, "Current wavelength is: ", wav[w]
         aia_annulus_analyze_radial, event, wave=wav[w], rrange=rrange, /constrain, /auto
      endfor
   endif
endfor

end
