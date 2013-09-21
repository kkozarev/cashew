pro batch_load_events_newformat
;This is the new format for loading big bunches of AIA data for
;multiple events and wavelengths.
;Kamen Kozarev, 10/2011
  
  path='/Volumes/Backscratch/Users/kkozarev/AIA_data/studies/2011events/'
  wave=['171','193','211','335','094','131','304']
  
  evnums=['05','06','07','13','19','20','23','32','37','38','41','113','112']
  coordX=[-955,729,777,-1073,812,-1069,771,-835,785,-843,864,883,633]
  coordY=[-50,-76,100,-70,750,-150,163,545,399,-200,113,-422,390]
  sts=['2011/01/25 11:56:00','2011/01/27 11:50:00','2011/01/28 00:45:00',$
       '2011/02/11 12:30:00','2011/03/07 19:35:00','2011/03/08 03:30:00',$
       '2011/03/12 15:20:00','2011/04/27 02:05:00','2011/05/11 02:10:00',$
       '2011/05/29 10:00:00','2012/01/05 06:56:00','2010/06/13 05:35:00',$
       '2010/06/12 00:55:00']

;The ending times are not inclusive in the minutes - that is, if the
;ending time for an event is '2011/01/25 12:26:00', an image taken at
;12:26:10 won't be read in.
  ets=['2011/01/25 12:27:00','2011/01/27 12:21:00','2011/01/28 01:16:00',$
       '2011/02/11 13:01:00','2011/03/07 20:06:00','2011/03/08 04:01:00',$
       '2011/03/12 15:51:00','2011/04/27 02:26:00','2011/05/11 02:41:00',$
       '2011/05/29 10:31:00','2012/01/05 07:10:00','2010/06/13 05:46:00',$
       '2010/06/12 01:04:00']
  
  ;choose which of all the events to run
  ;events2run=[0,1,2,3,4,5,6,7,8,9]
  events2run=[8]
  waves2run=[4,5,6]

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
        savefile=path+'e'+evnum+'/normalized_AIA_'+std[0]+std[1]+std[2]+'_'+evnum+'_'+wav
        aia_load_event,st,et,wav,index,data
        ;save,filename=savefile+'.sav',index,data
        ;restore,savefile+'.sav'

        newcoords=aia_autoselect_subroi(index[0],coords)
        subdata=aia_inspect_data(index,data,autoregion=newcoords)
        subindex=aia_update_subdata_index(index,[newcoords[0],newcoords[1]],1024,coords)
        save,filename=savefile+'_subdata'+'.sav',subindex,subdata
     endfor
  endfor
end
