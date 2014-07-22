pro generate_event_movies

  events=load_events_info()
  movie_types=['araw','arun']
  wav=['193','211']

  for ev=0, n_elements(events)-1 do begin
     skip = 0
     event=events[ev]
     if event.label eq 'test' then continue     
     for w=0, n_elements(wav)-1 do begin
        for mt=0, n_elements(movie_types)-1 do begin
           aia_make_movies, event, movie_type=movie_types[mt],wav=wav[w],/force              
        endfor                                                         
     endfor
  endfor
  
  
end
