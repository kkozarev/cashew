pro run_aschdem_models
;This procedure will run the pfss_return_field procedure, obtaining high-
;and low-resolution PFSS models for all events.
  events=load_events_info()
  for ev=0,n_elements(events)-1 do begin
     event=events[ev]
     date=event.st
     if ev eq 8 then continue
     aia_cfa_teem_run,event,/remove_aec
     stop
  endfor
  
end
