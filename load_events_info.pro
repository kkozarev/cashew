function load_events_info,printlabels=printlabels,label=label,quiet=quiet
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
; quiet - don't produce any messages
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
  labels=['110125_01','110128_01','110211_02','110511_01','110804_01','110809_01','111020_01','111109_01','120424_01','120526_01','120728_01','120915_01','121007_01','130423_01','130501_01','130517_01','131106_01','131029_01']
  
  if keyword_set(label) then begin
     tmp=where(labels eq label)
     if tmp[0] eq -1 then begin
        if not keyword_set(quiet) then begin
           print,''
           print,'Event "'+label+'" does not exist! Quitting...' 
           print,''
        endif
        return,-1
     endif
  endif
  
;AR arcsecond coordinates
  coordX=[-955,777,-1073,785,564,883,1005,-752,-1019,899,-745,806,-932,999,-865,-576,-576,990]
  coordY=[-248,391,11,399,186,269,374,580,247,416,-662,422,378,-399,237,192,-269,38]
  hemisphere=strarr(n_elements(labels))
  hemisphere[where(coordX lt 0.)]='E'
  hemisphere[where(coordX ge 0.)]='W'
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
       '2013/05/17 08:40:00',$
       '2013/11/06 13:40:00',$
       '2013/10/29 21:40:00']
 
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
     '2013/05/17 09:15:00',$
     '2013/11/06 14:10:00',$
     '2013/10/29 22:10:00']
typeII=[0,0,0,1,1,1,0,1,1,0,1,1,0,0,0,1,1,1]
loop=[1,0,1,1,1,0,1,0,0,0,1,1,1,1,1,1,0,1]
filament=[0,0,1,1,0,0,0,1,1,0,0,0,0,0,1,1,1,1]
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
         'A nice event on the eastern limb with serious NRH emission and type II bursts',$
         'Eastern event with interesting type III and type II bursts',$
         'Western event, nice coronal wave running parallel to the limb, X2.3 flare']
flareclass=['','M1.3','','B8.1','M9.3','X6.9','M1.6','M1.1','C3.7','','M6.1','B9.6','C1.2','','','M3.2','M3.8','X2.3']
aiafov=[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2]
datapath='/Volumes/Backscratch/Users/kkozarev/AIA_data/'
savepath='/Volumes/Backscratch/Users/kkozarev/AIA/events/'
nrhpath='/Volumes/Backscratch/Users/kkozarev/NRH/NRH_data/'
rhessipath='/Volumes/Backscratch/Users/kkozarev/RHESSI/RHESSI_data/'
;----------------------------------------------------------------------------------------


nevents=n_elements(labels)
event={label:'',st:'',et:'',coordX:0,coordY:0,aiafov:intarr(2),hemisphere:'',date:'',arlon:0.,arlat:0.,$
       flareclass:'',typeII:0,loop:0,filament:0,comment:'',datapath:'',savepath:'',moviepath:'',nrhpath:'',rhessipath:''}
events=replicate(event,nevents)

for ev=0,nevents-1 do begin
events[ev].label=labels[ev]
events[ev].coordX=coordX[ev]
events[ev].coordY=coordY[ev]
events[ev].hemisphere=hemisphere[ev]
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
events[ev].moviepath=events[ev].savepath+'movies'+'/'
events[ev].nrhpath=nrhpath
events[ev].rhessipath=rhessipath

;Field of view, in pixels
events[ev].aiafov=[1024,1024]
if aiafov[ev] eq 2 then events[ev].aiafov=[1024,2048]
endfor

if keyword_set(printlabels) then print,events.label
if keyword_set(label) then begin
   ind=where(events.label eq label)
   single_event=events[ind]   
   return, single_event
endif else begin
   return,events
endelse

end
