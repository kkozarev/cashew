pro create_MWACME_events
  
  line=''
  cc=0
  inpath=getenv("HOME")+'/git/mwacme/dat'
  stop
  infile=inpath+'Limb_CME_observations.csv'
  tmp=read_csv(infile)
  nev=n_elements(tmp.field01)
  events=replicate({label:'',date:'',c2st:'',apspeed:'',spspeed:'',accel:'',mpa:'',$
                    source:'',flare:'',flarest:'',lascomov:'',nrhmov:'',nrhspec:'',aiardiffmov:'',radiospec:'',notes:''},nev)
  events[*].date=tmp.field01
  events[*].c2st=events[*].date+' '+strtrim(tmp.field02,2)
  for ev=0,nev-1 do begin
     res=strsplit(events[ev].date,'/',/extract)
     if res[0] lt 10 then res[0]='0'+res[0]
     if res[1] lt 10 then res[1]='0'+res[1]
     events[ev].label=res[2]+res[0]+res[1]
  endfor
  events[*].apspeed=tmp.field03
  events[*].spspeed=tmp.field04
  events[*].accel=tmp.field05
  events[*].mpa=tmp.field06
  events[*].source=tmp.field07
  events[*].flare=tmp.field08
  events[*].flarest=tmp.field09
  events[*].lascomov=tmp.field10
  events[*].notes=tmp.field11
  events[*].nrhmov=tmp.field12
  events[*].nrhspec=tmp.field13
  events[*].aiardiffmov=tmp.field14
  events[*].radiospec=tmp.field15
  id=sort(events.label)
  events=events[id]
  
  openw,lun,inpath+'CME_events.json',/get_lun
  printf,lun,'['
  for ee=0,nev-1 do begin
     printf,lun,'{'
     printf,lun,'"label": "'+events[ee].label+'",'
     printf,lun,'"date": "'+events[ee].date+'",'
     printf,lun,'"C2st": "'+events[ee].c2st+'",'
     printf,lun,'"source": "'+events[ee].source+'",'
     printf,lun,'"flareclass": "'+events[ee].flare+'",'
     printf,lun,'"flarest": "'+events[ee].flarest+'",'
     printf,lun,'"apspeed": "'+strtrim(events[ee].apspeed,2)+'",'
     printf,lun,'"spspeed": "'+strtrim(events[ee].spspeed,2)+'",'
     printf,lun,'"accel": "'+strtrim(events[ee].accel,2)+'",'
     printf,lun,'"mpa": "'+strtrim(events[ee].mpa,2)+'",'
     printf,lun,'"lascomov": "'+events[ee].lascomov+'",'
     printf,lun,'"nrhmov": "'+events[ee].nrhmov+'",'
     printf,lun,'"nrhspec": "'+events[ee].nrhspec+'",'
     printf,lun,'"aiardiffmov": "'+events[ee].aiardiffmov+'",'
     printf,lun,'"radiospec": "'+events[ee].radiospec+'",'
     printf,lun,'"notes": "'+events[ee].notes+'"'
     if ee eq nev-1 then printf,lun,']' else printf,lun,'},'
  endfor
  close,/all
  
end
