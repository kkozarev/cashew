pro create_candidate_events
  
  line=''
  cc=0
  
  openr,lun,'event_candidates_dates.txt',/get_lun
  while not EOF(lun) do begin
     readf,lun,line
     cc++
  endwhile
  nev=cc
  cc=0
  close,/all
  
  events=replicate({st:'',et:'',date:'',label:''},nev)
  openr,lun,'event_candidates_dates.txt',/get_lun
  while not EOF(lun) do begin
     readf,lun,line
     res=strsplit(line,'\t\n\f -',/extract)
     events[cc].date=res[0]
     events[cc].st=events[cc].date+' '+strtrim(res[1],2)+':00'
     events[cc].et=events[cc].date+' '+strtrim(res[2],2)+':00'
     events[cc].label=strmid(strjoin(strsplit(events[cc].date,'/',/extract)),2,6)
     if cc gt 0 then begin
        if events[cc-1].label eq events[cc].label+'_01' then $
           events[cc].label+='_02' else  events[cc].label+='_01'
     endif else begin
        events[cc].label+='_01'
     endelse
     cc++
  endwhile
  
  
  
openw,lun,'event_candidates.json',/get_lun
for ee=0,nev-1 do begin
printf,lun,'{'
printf,lun,'"label": "'+events[ee].label+'",'
printf,lun,'"coordX": 0,'
printf,lun,'"coordY": 0,'
printf,lun,'"st": "'+events[ee].st+'",'
printf,lun,'"et": "'+events[ee].et+'",'
printf,lun,'"aiafov": [1024,1024],'
printf,lun,'"flareclass": "",'
printf,lun,'"typeII": false,'
printf,lun,'"loop": false,'
printf,lun,'"filament": false,'
printf,lun,'"comment": "",'
printf,lun,'"web": false,'
printf,lun,'"rstn_lookup": "",'
printf,lun,'"callisto_lookup": "",'
printf,lun,'"nrh_lookup": ""'
printf,lun,'}'
printf,lun,''
endfor
close,/all
  
end
