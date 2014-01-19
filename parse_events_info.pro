pro test_parse_events_info

parse_events_info,'dat/events.json'

end



pro parse_events_info, fname, labels=labels, coordX=coordX, coordY=coordY, sts=sts, ets=ets, typeII=typeII, loop=loop, filament=filament, comment=comment, flareclass=flareclass, aiafov=aiafov, nrh_lookup=nrh_lookup, callisto_lookup=callisto_lookup, ips_lookup=ips_lookup
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
     if line eq '{' then inObj=1
     if line eq '}' then begin
        inObj=0
        cc++
     endif

; First, remove the quotation marks and white space:
     line=strjoin(strsplit(strtrim(line,2),'":',/extract,/regex),',')
     line=strjoin(strsplit(strtrim(line,2),'"',/extract),'')
     ;print,line
     ;Then, separate into a name and a value, divide and conquer-style
     tmp=strsplit(line,',[]',/extract)

;           'st': if cc eq 0 then sts=tmp[1]+':'+tmp[2]+':'+tmp[3] else sts=[sts,tmp[1]+':'+tmp[2]+':'+tmp[3]]
;           'et': if cc eq 0 then ets=tmp[1]+':'+tmp[2]+':'+tmp[3] else ets=[ets,tmp[1]+':'+tmp[2]+':'+tmp[3]]     

;THIS NEEDS TO BE DEBUGGED!!!!
;WRITE SEPARATE IF STATEMENTS FOR THE DIFFERENT TYPES OF ELEMENTS,
;SINCE THEY NEED TO BE PARSED DIFFERENTLY!!!

     if inObj eq 1 then begin
        case tmp[0] of
           'label': if cc eq 0 then labels=tmp[1] else labels=[labels,tmp[1]]
           'st': if cc eq 0 then sts=tmp[1] else sts=[sts,tmp[1]]
           'et': if cc eq 0 then ets=tmp[1] else ets=[ets,tmp[1]]
           'coordX': if cc eq 0 then coordX=tmp[1] else coordX=[coordX,tmp[1]]
           'coordY': if cc eq 0 then coordY=tmp[1] else coordY=[coordY,tmp[1]]
           'aiafov': if cc eq 0 then aiafov=tmp[2] else aiafov=[aiafov,tmp[2]]
           'flareclass': if cc eq 0 then flareclass=tmp[1] else flareclass=[flareclass,tmp[1]]
           'typeII': if cc eq 0 then typeII=tmp[1] else typeII=[typeII,tmp[1]]
           'loop': if cc eq 0 then loop=tmp[1] else loop=[loop,tmp[1]]
           'filament': if cc eq 0 then filament=tmp[1] else filament=[filament,tmp[1]]
           'comment': if cc eq 0 then comment=tmp[1] else comment=[comment,tmp[1]]
           'ips_lookup': if cc eq 0 then ips_lookup=tmp[1] else ips_lookup=[ips_lookup,tmp[1]]
           'callisto_lookup': if cc eq 0 then callisto_lookup=tmp[1] else callisto_lookup=[callisto_lookup,tmp[1]]
           'nrh_lookup': if cc eq 0 then nrh_lookup=tmp[1] else nrh_lookup=[nrh_lookup,tmp[1]]
           else:
        endcase
     endif  
  endwhile
  close,/all
  labels=strtrim(labels,1)
  sts=strtrim(sts,1)
  ets=strtrim(ets,1)
  coordX=strtrim(coordX,1)
  coordX=fix(coordX)
  coordY=strtrim(coordY,1)
  coordY=fix(coordY)
  ;aiafov=strtrim(aiafov,1)
  comment=strtrim(comment,1)
  flareclass=strtrim(flareclass,1)
  ips_lookup=strtrim(ips_lookup,1)
  callisto_lookup=strtrim(callisto_lookup,1)
  nrh_lookup=strtrim(nrh_lookup,1)
  typeII=strtrim(typeII,2)
  loop=strtrim(loop,2)
  filament=strtrim(filament,2)
  
  
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
  
  ind=where(aiafov eq 1024)
  if ind[0] ne -1 then aiafov[ind]=1
  ind=where(aiafov eq 2048)
  if ind[0] ne -1 then aiafov[ind]=2
  aiafov=fix(aiafov)
  
;Finally, printous for test purposes.
;DEBUG
  print,'|'+labels+'|'
  print,'|'+sts+'|'
  print,'|'+ets+'|'
  print,coordX
  print,coordY
  print,aiafov
  print,flareclass
  print,typeII
  print,loop
  print,filament
  print,'|'+comment+'|'
  print,'|'+ips_lookup+'|'
  print,'|'+callisto_lookup+'|'
  print,'|'+nrh_lookup+'|'
;DEBUG

end
