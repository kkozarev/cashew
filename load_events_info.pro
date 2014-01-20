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
;11/18/2013, Kamen Kozarev - added various folder elements

;----------------------------------------------------------------------------------------
; THIS IS THE LIST OF 'GOOD' EVENTS THAT MICHAEL HAMMER CREATED IN 08/2013
;- Continuously updated list of events.
;----------------------------------------------------------------------------------------
  basepath=GETENV('CORWAV');Usually, it will be /Volumes/Backscratch/Users/kkozarev/corwav/ or similar
  webbasepath=GETENV('CORWAV_WEB')

;DEBUG
parse_events_info,'dat/events.json', labels=labels, coordX=coordX, coordY=coordY, sts=sts, ets=ets, typeII=typeII, loop=loop, filament=filament, comment=comment, flareclass=flareclass, aiafov=aiafov, nrh_lookup=nrh_lookup, callisto_lookup=callisto_lookup, ips_lookup=ips_lookup
;DEBUG
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

  hemisphere=strarr(n_elements(labels))
  hemisphere[where(coordX lt 0.)]='E'
  hemisphere[where(coordX ge 0.)]='W'

;DATAPATHS
aia_datapath=basepath+'AIA_data/'
nrh_datapath=basepath+'NRH_data/'
ips_datapath=basepath+'IPS_data/'
callisto_datapath=basepath+'Callisto_data/'
rhessi_datapath=basepath+'RHESSI_data/'
euvi_datapath=basepath+'EUVI_data/'
swap_datapath=basepath+'SWAP_data/'
pfss_datapath=basepath+'PFSS_data/'

;SAVEPATHS
savepath=basepath+'events/' ;The base savepath
webpath=webbasepath+'events/'
;FILENAMES

;----------------------------------------------------------------------------------------


nevents=n_elements(labels)
event={label:'',st:'',et:'',coordX:0,coordY:0,aiafov:intarr(2),hemisphere:'',date:'',$
       arlon:0.,arlat:0.,geomcorfactor:0.,flareclass:'',typeII:0,loop:0,filament:0,comment:'',$
       aia_datapath:'',nrh_datapath:'',rhessi_datapath:'',ips_datapath:'',callisto_datapath:'',$
       ips_lookup:'',callisto_lookup:'',nrh_lookup:'',$
       euvi_datapath:'',swap_datapath:'',pfss_datapath:'',savepath:'',webpath:'',$
       moviepath:'',radiopath:'',nrhpath:'',ipspath:'',callistopath:'',annuluspath:'',pfsspath:'',$
       swappath:'',ionizationpath:'',aschdempath:'',weberpath:'',particlespath:'',$
      euvipath:'',dempath:'',pngpath:'',yaftawavepath:'',kinematicspath:''}
events=replicate(event,nevents)

for ev=0,nevents-1 do begin
events[ev].label=labels[ev]
events[ev].coordX=coordX[ev]
events[ev].coordY=coordY[ev]
events[ev].hemisphere=hemisphere[ev]
;An attempt to get the AR coordinates in degrees
tmp=strsplit(sts[ev],' ',/extract)
dat=tmp[0]
tmp=strsplit(dat,'/',/extract)
events[ev].date=tmp[0]+tmp[1]+tmp[2]
res=arcmin2hel(coordX[ev]/60.,coordY[ev]/60.,date=dat)
events[ev].arlat=res[0]
events[ev].arlon=res[1]
events[ev].geomcorfactor=1.0/sin(events[ev].arlon*!PI/180.)
;if ev eq 3 then print,res[0]
events[ev].st=sts[ev]
events[ev].et=ets[ev]
events[ev].typeII=typeII[ev]
events[ev].loop=loop[ev]
events[ev].filament=filament[ev]
;Field of view, in pixels
events[ev].aiafov=aiafov[*,ev]
events[ev].comment=comment[ev]
events[ev].flareclass=flareclass[ev]
events[ev].nrh_lookup=nrh_lookup[ev]
events[ev].callisto_lookup=callisto_lookup[ev]
events[ev].ips_lookup=ips_lookup[ev]

;datapaths
events[ev].aia_datapath=aia_datapath
events[ev].nrh_datapath=nrh_datapath
events[ev].callisto_datapath=callisto_datapath
events[ev].rhessi_datapath=rhessi_datapath
events[ev].ips_datapath=ips_datapath
events[ev].swap_datapath=swap_datapath
events[ev].euvi_datapath=euvi_datapath
events[ev].pfss_datapath=pfss_datapath

;savepaths
events[ev].savepath=savepath+events[ev].label+'/'
events[ev].webpath=webpath+events[ev].label+'/'
events[ev].moviepath=events[ev].savepath+'movies/'
events[ev].radiopath=events[ev].savepath+'radio/'
events[ev].nrhpath=events[ev].radiopath+'NRH/'
events[ev].ipspath=events[ev].radiopath+'IPS/'
events[ev].callistopath=events[ev].radiopath+'Callisto/'
events[ev].annuluspath=events[ev].savepath+'annulusplot/'
events[ev].pfsspath=events[ev].savepath+'pfss/'
events[ev].swappath=events[ev].savepath+'swap/'
events[ev].ionizationpath=events[ev].savepath+'ionization/'
events[ev].euvipath=events[ev].savepath+'euvi/'
events[ev].dempath=events[ev].savepath+'dem/'
events[ev].aschdempath=events[ev].dempath+'aschwanden/'
events[ev].weberpath=events[ev].dempath+'weber/'
events[ev].pngpath=events[ev].savepath+'png/'
events[ev].yaftawavepath=events[ev].savepath+'yaftawave/'
events[ev].kinematicspath=events[ev].savepath+'kinematics/'
events[ev].particlespath=events[ev].savepath+'particles/'

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
