pro generate_annulus_plots

events=load_events_info()
annulusplot=['araw','arun']
wav=['193','211']
for ev=0, n_elements(events)-1 do begin
   skip = 0
   event=events[ev]
   if event.label eq 'test' then continue
   ;event=load_events_info(label='120915_01')
   
   for i=0, n_elements(annulusplot)-1 do begin
      for j=0, n_elements(wav)-1 do begin
         path = event.annuluspath+annulusplot[i]+'/'+wav[j]
         print, path
         if dir_exist(path) then begin
            spawn, 'rm -f '+path+'/*.png'
         endif else begin
            print, "No data found, skipping..."
            skip = 1 
         endelse
      endfor
   endfor
   if skip eq 0 then begin
      for w=0, n_elements(wav)-1 do aia_annulus_plot, event,/raw,/run,wav=wav[w]
   endif
endfor


end
