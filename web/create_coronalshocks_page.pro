pro test_create_coronalshocks_page
  path='/var/www/personal/kkozarev/www/'
  fname='coronalshocks.content'
  create_coronalshocks_page,path+fname
  
end



pro create_coronalshocks_page, fname
  close,/all
;Load all the events info
  events=load_events_info()
  nev=n_elements(events)
  
;Order the events by time
  tmpind=sort(anytim(events.st,/sec))
  events=events[tmpind]
  
  
;Write the first part of the HTML
  openw,lun,fname,/get_lun
  printf,lun,"<table border=1 cellpadding=1 cellspacing=0 width=920 style='table-layout:fixed'>"
  printf,lun,"<col width=55>"
  printf,lun,"<col width=40>"
  printf,lun,"<col width=40>"
  printf,lun,"<col width=40>"
  printf,lun,"<col width=30>"
  printf,lun,"<col width=30>"
  printf,lun,"<col width=45>"
  printf,lun,"<col width=40>"
  printf,lun,"<col width=45>"
  printf,lun,"<col width=45>"
  printf,lun,"<col width=35>"
  printf,lun,"<col width=40>"
  printf,lun,"<col width=40>"
  printf,lun,"<col width=45>"
  printf,lun,"<col width=50>"
  printf,lun,"<col width=45>"
  printf,lun,"<col width=65>"
  
  printf,lun,"<tr height=25>"
  printf,lun,'<th align="center" colspan=19 height=25 width=920>OFF-LIMB CORONAL BRIGHT FRONTS DATABASE</th>'
  printf,lun,"</tr>"
  
  printf,lun,'<tr height=1>'
  printf,lun,'<th rowspan=2>Date</th>'
  printf,lun,'<th rowspan=2>Start (UT)</th>'
  printf,lun,'<th rowspan=2>End (UT)</th>'
  printf,lun,'<th rowspan=2>Flare Class</th>'
  printf,lun,'<th rowspan=2>X (&quot;)</th>'
  printf,lun,'<th rowspan=2>Y (&quot;)</th>'
  printf,lun,'<th rowspan=2>Filament Erupt</th>'
  printf,lun,'<th rowspan=2>Loop Erupt</th>'
  printf,lun,'<th colspan=3 width=144>Metric Radio Spectra</th>'
  printf,lun,'<th colspan=2 width=79>EUV Raw / Run Diff. Movies</th>'
  printf,lun,'<th colspan=2 width=85>EUV Annulus plot Raw / Run Diff. Movies</th>'
  printf,lun,'<th colspan=2 width=96>Kinematics</th>'
  printf,lun,'<th rowspan=2><a href="http://solar.physics.montana.edu/martens/squaw-valley/Aschwanden-solar-phys.pdf" target="_blank">TEEM DEM</a> Movies</th>'
  printf,lun,'<th rowspan=2>PFSS / Shock Model</th>'
  printf,lun,'</tr>'
 
 printf,lun,'<tr height=40 align="center">'
 printf,lun,'<th height=40><a href="http://www.e-callisto.org" target="_blank">eCallisto</a></th>'
 printf,lun,'<th><a href="http://secchirh.obspm.fr/" target="_blank">Nancay</a></th>'
 printf,lun,'<th width=45><a href="http://www.ips.gov.au/World_Data_Centre/1/9" target="_blank">IPS</a></th>'
 printf,lun,'<th colspan=2 width=80>AIA/193</th>'
 printf,lun,'<th colspan=2 width=80>AIA/193</th>'
 printf,lun,'<th width=45>Radial</th>'
 printf,lun,'<th width=45>Tangential</th>'
 printf,lun,'</tr>'
 
;Write the second part, containing the data for each event
  for ev=0,n_elements(events)-1 do begin
     event=events[ev]
     tmp=strsplit(event.st,' ',/extract)
     dt=tmp[0]
     st=strmid(tmp[1],0,5)
     tmp=strsplit(event.et,' ',/extract)
     et=strmid(tmp[1],0,5)
     printf,lun,'<tr height=25 align="center">'
     printf,lun,'<td>'+dt+'</td>'
     printf,lun,'<td>'+st+'</td>'
     printf,lun,'<td>'+et+'</td>'
     if event.flareclass eq '' then printf,lun,'<td>&nbsp;</td>' else $
        printf,lun,'<td><a href="http://www.solarmonitor.org/goes_pop.php?date='+event.date+'&amp;type=xray" target="_blank">'+event.flareclass+'</a></td>'
     printf,lun,'<td>'+strtrim(string(event.coordx),2)+'</td>'
     printf,lun,'<td>'+strtrim(string(event.coordy),2)+'</td>'
     if event.filament eq 1 then printf,lun,'<td>Yes</td>' else $
        printf,lun,'<td>No</td>'
     if event.loop eq 1 then printf,lun,'<td>Yes</td>' else $
        printf,lun,'<td>No</td>'
;This will be replaced by self-generated Callisto plots soon.
     if event.callisto_lookup eq '' then printf,lun,'<td>&nbsp;</td>' else $
        printf,lun,'<td><a href='+event.callisto_lookup+' target="_blank">eCallisto</a></td>'

     if event.nrh_lookup eq '' then printf,lun,'<td>&nbsp;</td>' else $
        printf,lun,'<td><a href='+event.nrh_lookup+' target="_blank">NRH</a></td>'
     if event.ips_lookup eq '' then printf,lun,'<td>&nbsp;</td>' else $
        printf,lun,'<td><a href='+event.ips_lookup+' target="_blank">IPS</a></td>'
 
     ;The Raw movie
     movname='events/'+event.label+'/movies/raw_193_'+event.label+'.mp4'
     if file_exist(event.webpath+'movies/raw_193_'+event.label+'.mp4') then $
        printf,lun,'<td><a href="'+movname+'" target="_blank">RAW</a></td>' else $
           printf,lun,'<td>&nbsp;</td>'
     ;The Running difference movie
     movname='events/'+event.label+'/movies/run_193_'+event.label+'.mp4'
     if file_exist(event.webpath+'movies/run_193_'+event.label+'.mp4') then $
        printf,lun,'<td><a href="'+movname+'" target="_blank">RDIFF</a></td>' else $
           printf,lun,'<td>&nbsp;</td>'
     ;The Annulusplot Raw movie
     movname='events/'+event.label+'/movies/araw_193_'+event.label+'.mp4'
     if file_exist(event.webpath+'movies/araw_193_'+event.label+'.mp4') then $
        printf,lun,'<td><a href="'+movname+'" target="_blank">ARAW</a></td>' else $
           printf,lun,'<td>&nbsp;</td>'
     ;The Annulusplot Running difference movie
     movname='events/'+event.label+'/movies/arun_193_'+event.label+'.mp4'
     if file_exist(event.webpath+'movies/arun_193_'+event.label+'.mp4') then $
        printf,lun,'<td><a href="'+movname+'" target="_blank">ARDIFF</a></td>' else $
           printf,lun,'<td>&nbsp;</td>'
     
     ;The deprojected kinematics radial figure
     pngname='events/'+event.label+'/annulusplot/annplot_'+event.date+'_'+event.label+'_193_radial.png'
     if file_exist(event.webpath+'annulusplot/annplot_'+event.date+'_'+event.label+'_193_radial.png') then $
        printf,lun,'<td><a href="'+pngname+'" target="_blank">PNG</a></td>' else $
           printf,lun,'<td>&nbsp;</td>'
     ;The deprojected kinematics tangential figure
     printf,lun,'<td>&nbsp;</td>'
     
     ;The Aschwanden DEM movie
     movname='events/'+event.label+'/movies/aschdem_'+event.date+'_'+event.label+'.mp4'
     if file_exist(event.webpath+'movies/aschdem_'+event.date+'_'+event.label+'.mp4') then $
        printf,lun,'<td><a href="'+movname+'" target="_blank">DEM</a></td>' else $
           printf,lun,'<td>&nbsp;</td>'
     
     ;The PFSS/Shock movie
     movname='events/'+event.label+'/movies/aia_pfss_shock_'+event.date+'_'+event.label+'.mp4'
     if file_exist(event.webpath+'movies/aia_pfss_shock_'+event.date+'_'+event.label+'.mp4') then $
        printf,lun,'<td><a href="'+movname+'" target="_blank">PSHOCK</a></td>' else $
           printf,lun,'<td>&nbsp;</td>'
     
     printf,lun,'</tr>'
  endfor
  printf,lun,'</table>'
  close,/all
end



;pro write_event_row, event, fname
;Write the HTML code for the event to the HTML file.
  
  
;end
