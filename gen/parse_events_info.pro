pro test_parse_events_info
trunk=GETENV('CORWAV_TRUNK')
parse_events_info,trunk+'dat/events.json'

end



pro parse_events_info, fname, labels=labels, coordX=coordX, coordY=coordY, sts=sts, ets=ets, typeII=typeII, loop=loop, filament=filament, comment=comment, flareclass=flareclass, aiafov=aiafov, nrh_lookup=nrh_lookup, callisto_lookup=callisto_lookup, ips_lookup=ips_lookup,web=web
;PURPOSE:
;
;This procedure will parse the json object containing wave event information
;
;CATEGORY:
; AIA/Kinematic
;
;INPUTS:
;  fname - the name of the file to parse
;KEYWORDS:
; The keywords are all the parameters, which will be passed back to
; load_events_info()
;OUTPUTS:
;
; 
;DEPENDENCIES:
;
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 01/16/2014
;
  close,/all
  inObj=0
  line=''
  cc=0 ; a counter for the number of events
  
  openr,lun,fname,/get_lun
  while not eof(lun) do begin
     readf, lun, line

;THIS WILL NEED TO BE CHANGED IN CASE THE CURLY BRACKETS ARE NOT ON
;SEPARATE LINES FROM THE ACTUAL ELEMENTS. SUBSTITUTE 'EQ' WITH STH. ELSE!
     if line eq '{' then begin
        inObj=1
        continue
     endif
     if line eq '}' then begin
        inObj=0
        cc++
        continue
     endif
     
     ;Do the parsing of the lines using regex:
     tmp=strsplit(line,'"[ /t]*:',/extract,/regex)
     ;Exclude the empty lines
     if n_elements(tmp) eq 1 then continue
     tmp[0]=strtrim(strjoin(strsplit(tmp[0],'"',/extract),''),2)
     tmp[1]=strsplit(tmp[1],',$',/extract,/regex)
     tmp[1]=strtrim(strjoin(strsplit(tmp[1],'"',/extract),''),2)
     
     if inObj eq 1 then begin
        case tmp[0] of
           'label': if cc eq 0 then labels=tmp[1] else labels=[labels,tmp[1]]
           'st': if cc eq 0 then sts=tmp[1] else sts=[sts,tmp[1]]
           'et': if cc eq 0 then ets=tmp[1] else ets=[ets,tmp[1]]
           'coordX': if cc eq 0 then coordX=tmp[1] else coordX=[coordX,tmp[1]]
           'coordY': if cc eq 0 then coordY=tmp[1] else coordY=[coordY,tmp[1]]
           'aiafov': if cc eq 0 then aiafov=strsplit(tmp[1],'[],',/extract) else aiafov=[[aiafov],[strsplit(tmp[1],'[],',/extract)]]
           'flareclass': if cc eq 0 then flareclass=tmp[1] else flareclass=[flareclass,tmp[1]]
           'typeII': if cc eq 0 then typeII=tmp[1] else typeII=[typeII,tmp[1]]
           'loop': if cc eq 0 then loop=tmp[1] else loop=[loop,tmp[1]]
           'filament': if cc eq 0 then filament=tmp[1] else filament=[filament,tmp[1]]
           'comment': if cc eq 0 then comment=tmp[1] else comment=[comment,tmp[1]]
           'web': if cc eq 0 then web=tmp[1] else web=[web,tmp[1]]
           'ips_lookup': if cc eq 0 then ips_lookup=tmp[1] else ips_lookup=[ips_lookup,tmp[1]]
           'callisto_lookup': if cc eq 0 then callisto_lookup=tmp[1] else callisto_lookup=[callisto_lookup,tmp[1]]
           'nrh_lookup': if cc eq 0 then nrh_lookup=tmp[1] else nrh_lookup=[nrh_lookup,tmp[1]]
           else:
        endcase
     endif  
  endwhile
  close,/all

  coordX=fix(coordX)
  coordY=fix(coordY)
  aiafov=fix(aiafov)
  
  ;FIX THE BOOLEAN ELEMENTS
  ind=where(typeII eq 'true')
  if ind[0] ne -1 then typeII[ind]=1
  ind=where(typeII eq 'false')
  if ind[0] ne -1 then typeII[ind]=0
  typeII=fix(typeII)
  ind=where(loop eq 'true')
  if ind[0] ne -1 then loop[ind]=1
  ind=where(loop eq 'false')
  if ind[0] ne -1 then loop[ind]=0
  loop=fix(loop)
  ind=where(filament eq 'true')
  if ind[0] ne -1 then filament[ind]=1
  ind=where(filament eq 'false')
  if ind[0] ne -1 then filament[ind]=0
  filament=fix(filament)
  ind=where(web eq 'true')
  if ind[0] ne -1 then web[ind]=1
  ind=where(web eq 'false')
  if ind[0] ne -1 then web[ind]=0
  web=fix(web)


;Finally, printous for test purposes.
;DEBUG
;  print,'|'+labels+'|'
;  print,'|'+sts+'|'
;  print,'|'+ets+'|'
;  print,coordX
;  print,coordY
;  print,aiafov
;  print,flareclass
;  print,typeII
;  print,loop
;  print,filament
;  print,'|'+comment+'|'
;  print,'|'+ips_lookup+'|'
;  print,'|'+callisto_lookup+'|'
;  print,'|'+nrh_lookup+'|'
;DEBUG

end
