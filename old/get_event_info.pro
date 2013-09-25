function get_event_info,filename
; A function which reads a coronal shock event nugget and returns a structure containing the info
  
  event={designation:'e01', $
             st:'2011/01/01 00:00:00',$
             et:'2011/01/01 00:01:00',$
             coordx:0.0,$
             coordy:0.0,$
             flareclass:'N/A',$
             typeII:0,$
             typeIII:0,$
             path:'/Volumes/PLUME/AIA_data/studies/2011events/e01/'}

;read and parse the file contents
  openr,lun,filename,/get_lun
  line=''
  incomment=0
  indata=0
  
  while not eof(lun) do begin
     readf,lun,line

     if line eq '<comment>' then incomment=1
     if line eq '<ecomment>' then incomment=0
     if line eq '<data>' then indata=1
     if line eq '<edata>' then indata=0
     if incomment eq 1 then continue
     
     if indata eq 1 then begin
        ;print,line
        ;stop
        
        
        case line of
           '<designation>': begin
              readf,lun,line
              event.designation=line
           end
           '<st>':  begin 
              readf,lun,line
              event.st=line
           end
           '<et>': begin
              readf,lun,line
              event.et=line
           end
           '<coordx>': begin
              readf,lun,line
              event.coordx=line
           end
           '<coordy>': begin
              readf,lun,line
              event.coordy=line
           end
           '<flareclass>': begin
              readf,lun,line
              event.flareclass=line
           end
           '<typeII>': begin
              readf,lun,line
              event.typeII=line
           end
           '<typeII>': begin
              readf,lun,line
              event.typeIII=line
           end
           '<path>': begin
              readf,lun,line
              event.path=line
           end           

           else:
           
        endcase
     endif
     
  endwhile
  close,lun
  
  return,event
  
end
