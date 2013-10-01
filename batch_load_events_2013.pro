pro batch_load_events_2013
;PURPOSE:
;
;This is the new format for loading big bunches of AIA data for
;multiple events and wavelengths.
;
;CATEGORY:
; AIA/Batch
;
;INPUTS:
;
;KEYWORDS:
; 
;
;OUTPUTS:
;
; 
;DEPENDENCIES:
;
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 06/2013
;

  
  path='/Volumes/Backscratch/Users/kkozarev/AIA_data/'
  wave=['171','193','211','335','094','131','304']
  
  evnums=['01','06','07','13','19','20','23','32','37','38','41','113','112']
  coordX=[999,729,777,-1073,812,-1069,771,-835,785,-843,864,883,633]
  coordY=[-399,-76,100,-70,750,-150,163,545,399,-200,113,-422,390]


;The ending times are not inclusive in the minutes - that is, if the
;ending time for an event is '2011/01/25 12:26:00', an image taken at
;12:26:10 won't be read in.
  ets=['2013/04/23 18:30:00','2011/01/27 12:21:00','2011/01/28 01:16:00',$
       '2011/02/11 13:01:00','2011/03/07 20:06:00','2011/03/08 04:01:00',$
       '2011/03/12 15:51:00','2011/04/27 02:26:00','2011/05/11 02:41:00',$
       '2011/05/29 10:31:00','2012/01/05 07:10:00','2010/06/13 05:46:00',$
       '2010/06/12 01:04:00']
  
  sts=['2011/05/11 02:20:00']
  ets=['2011/05/11 02:23:00']

  ;choose which of all the events to load data for
  ;events2run=[0,1,2,3,4,5,6,7,8,9] 
  events2run=[0]
  waves2run=[1]

  for w=0,n_elements(waves2run)-1 do begin
     wav=wave[waves2run[w]]
     for j=0,n_elements(events2run)-1 do begin
        ev=events2run[j]
        evnum=evnums[ev]
        coords=[coordX[ev],coordY[ev]]
        st=sts[ev]
        et=ets[ev]
        print,''
        print,'Loading '+wav+' channel AIA data for event #'+evnum+' between '+st+' and '+et
        print,''
        std=strsplit(st,'/ :',/extract)
        savepath=path+'e'+evnum+'/'
        if not file_exist(savepath) then begin
           exec='mkdir '+savepath
           spawn,exec
        endif
        savefile=savepath+'normalized_AIA_'+std[0]+std[1]+std[2]+'_'+evnum+'_'+wav
        aia_load_event,st,et,coords,wav,index,data,/remove_aec
        
        save,filename=savefile+'.sav',index,data

        newcoords=aia_autoselect_subroi(index[0],coords)
        subdata=aia_inspect_data(index,data,autoregion=newcoords)
        subindex=aia_update_subdata_index(index,[newcoords[0],newcoords[1]],1024,coords)
        
        save,filename=savefile+'_subdata'+'.sav',subindex,subdata
     endfor
  endfor
end
