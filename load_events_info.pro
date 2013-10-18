function load_events_info,printlabels=printlabels,label=label
;PURPOSE:
;
;This procedure will load the information for one or a list of events into an
;array of structures and return it.
;
;CATEGORY:
; AIA/Kinematic
;
;INPUTS:
;
;KEYWORDS:
; printlabels - if set, print just the labels of the events
; label - if set, return the information about the event with this
;         label or an error message if it doesn't exist
;OUTPUTS:
;
; 
;DEPENDENCIES:
;
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 09/30/2013
;

;----------------------------------------------------------------------------------------
; THIS IS THE LIST OF 'GOOD' EVENTS THAT MICHAEL HAMMER CREATED IN 08/2013
;----------------------------------------------------------------------------------------
  labels=['05','07','13','37','0804W','0809W','1020W','1109E','0424E','0526W','0728E','0915W','1007E','0423W','0501E','0517E']
;arcsecond coordinates
  coordX=[-955,777,-1073,785,564,883,1005,-752,-1019,899,-745,806,-932,999,-865,-576]
  coordY=[-248,391,11,399,186,269,374,580,247,416,-662,422,378,-399,237,192]
  sts=['2011/01/25 11:56:00',$
       '2011/01/28 00:45:00',$
       '2011/02/11 12:30:00',$
       '2011/05/11 02:10:00',$
       '2011/08/04 03:50:00',$
       '2011/08/09 08:00:00',$
       '2011/10/20 03:05:00',$
       '2011/11/09 13:00:00',$
       '2012/04/24 07:25:00',$
       '2012/05/26 20:30:00',$
       '2012/07/28 20:35:00',$
       '2012/09/15 22:50:00',$
       '2012/10/07 20:15:00',$
       '2013/04/23 18:05:00',$
       '2013/05/01 02:15:00',$
       '2013/05/17 08:40:00']
 
ets=['2011/01/25 12:26:00',$
     '2011/01/28 01:15:00',$
     '2011/02/11 13:00:00',$
     '2011/05/11 02:40:00',$
     '2011/08/04 04:20:00',$
     '2011/08/09 08:20:00',$
     '2011/10/20 03:35:00',$
     '2011/11/09 13:30:00',$
     '2012/04/24 07:55:00',$
     '2012/05/26 21:00:00',$
     '2012/07/28 21:05:00',$
     '2012/09/15 23:20:00',$
     '2012/10/07 20:45:00',$
     '2013/04/23 18:35:00',$
     '2013/05/01 02:45:00',$
     '2013/05/17 09:15:00']
typeII=[0,0,0,1,1,1,0,1,1,0,1,1,0,0,0,1]
loop=[1,0,1,1,1,0,1,0,0,0,1,1,1,1,1,1]
filament=[0,0,1,1,0,0,0,1,1,0,0,0,0,0,1,1]
comment=['Loop catches up to initial shock wave',$
         'Lateral shock wave not visible in the radial direction',$
         'Shock wave tends southward',$
         'Huge shock wave originates inside the limb with clear lateral expansion',$
         'One of the largest shock waves I have seen, but inside limb',$
         'Large shock wave inside the limb propagates southward',$
         'Distinguished wave radially and laterally in front of filament',$
         'Faint, far inside the limb, possibly a loop in front of filament',$
         'Filament eruption & tornado, but there is definitely a wave',$
         'Shock behind limb, seen easier laterally, but still visible radially',$
         'Originates inside the limb and movie could be better, but clear wave',$
         'Wave fades radially very quickly with a loop immediately following',$
         'Double shock wave laterally with quick radial speed as well',$
         'Shock wave immediately in front of loop with clear circular expansion',$
         'Clear lateral expansion precedes filament and loop',$
         'A nice event on the eastern limb with serious NRH emission and type II bursts']
flareclass=['','M1.3','','B8.1','M9.3','X6.9','M1.6','M1.1','C3.7','','M6.1','B9.6','C1.2','','','M3.2']
datapath='/Volumes/Backscratch/Users/kkozarev/AIA_data/'
savepath='/Volumes/Backscratch/Users/kkozarev/AIA/events/'
;----------------------------------------------------------------------------------------


nevents=n_elements(labels)
event={label:'',st:'',et:'',coordX:0,coordY:0,date:'',arlon:0.,arlat:0.,flareclass:'',$
       typeII:0,loop:0,filament:0,comment:'',datapath:'',savepath:''}
events=replicate(event,nevents)

for ev=0,nevents-1 do begin
events[ev].label=labels[ev]
events[ev].coordX=coordX[ev]
events[ev].coordY=coordY[ev]
;An attempt to get the AR coordinates in degrees
tmp=strsplit(sts[ev],' ',/extract)
dat=tmp[0]
events[ev].date=dat
res=arcmin2hel(coordX[ev]/60.,coordY[ev]/60.,date=dat)
events[ev].arlat=res[0]
events[ev].arlon=res[1]
events[ev].st=sts[ev]
events[ev].et=ets[ev]
events[ev].typeII=typeII[ev]
events[ev].loop=loop[ev]
events[ev].filament=filament[ev]
events[ev].comment=comment[ev]
events[ev].flareclass=flareclass[ev]
events[ev].datapath=datapath
events[ev].savepath=savepath+events[ev].label+'/'

endfor

if keyword_set(printlabels) then print,events.label
if keyword_set(label) then begin
   ind=where(events.label eq label)
   if ind eq -1 then begin
      print,''
      print,'Event "'+label+'" does not exist! Quitting...' 
      print,''
   endif else begin
      single_event=events[ind]   
      return, single_event
   endelse
endif else begin
   return,events
endelse

end
