pro run_pfss_models
;This procedure will run the pfss_return_field procedure, obtaining high-
;and low-resolution PFSS models for all events.
  events=load_events_info()
  for ev=8,n_elements(events)-1 do begin
     event=events[ev]
     date=event.st
     aia_carrington_latlon,event,lat,lon
     aclon=lon+event.arlon
     aclat=lat+event.arlat
     if event.label ne 'paper' then $
        if event.date eq '20110511' then continue ;skip May 11, 2011 - we have that one
     box=[aclon-90.,aclat-90.,aclon+90.,aclat+90.]
     pfss_return_field,event,box=box,/save
  endfor
  
end
