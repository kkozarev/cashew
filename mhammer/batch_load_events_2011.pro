pro batch_load_events_2011
;This is the new format for loading big bunches of AIA data for
;multiple events and wavelengths.
;Kamen Kozarev, 10/2013

; (0) Set Path
; (1) Enter Event Number
; (2) Enter Coordinates
; (3) Enter Start Time
; (4) Enter End Time
; (5) Select Events to Run
; (6) Select Wavelengths to Run
   
  path='/data/tokyo/kkozarev/2011events/'
  wave=['171','193','211','335','094','131','304']
  
  evnums=['0915W','0728E','0902E','1007E','1231E']
  evnums=['12','42','43','44','45']
  evnums=['50','51','52','53']
  coordX=[806,$
          -745,$
          -949,$
          -932,$
          -842]
  coordX=[-1073,$
          879,$
          883,$
          883,$
          883]
  coordX=[564,$
          1005,$
          -801,$
          -752]
  coordY=[422,$
          -662,$
          575,$
          378,$
          454]
  coordY=[11,$
          240,$
          269,$
          150,$
          50]
  coordY=[186,$
          374,$
          168,$
          580]

  sts=['2013/04/23 18:30:00','2011/01/27 12:21:00','2011/01/28 01:16:00',$
       '2011/02/11 13:01:00','2011/03/07 20:06:00','2011/03/08 04:01:00',$
       '2011/03/12 15:51:00','2011/04/27 02:26:00','2011/05/11 02:41:00',$
       '2011/05/29 10:31:00','2012/01/05 07:10:00','2010/06/13 05:46:00',$
       '2010/06/12 01:04:00'] ; Example Events (Start Times)
  sts=['2012/09/15 22:50:00',$
       '2012/07/28 20:35:00',$
       '2012/09/02 00:45:00',$
       '2012/10/07 20:15:00',$
       '2012/12/31 03:15:00']
  sts=['2011/02/11 09:50:00',$
       '2011/08/08 18:00:00',$
       '2011/08/09 08:00:00',$
       '2011/08/09 08:00:00',$
       '2011/08/09 08:00:00']
  sts=['2011/08/04 03:50:00',$
       '2011/10/20 03:05:00',$
       '2011/09/22 10:25:00',$
       '2011/11/09 13:00:00']

;The ending times are not inclusive in the minutes - that is, if the
;ending time for an event is '2011/01/25 12:26:00', an image taken at
;12:26:10 won't be read in.
  ets=['2013/04/23 18:30:00','2011/01/27 12:21:00','2011/01/28 01:16:00',$
       '2011/02/11 13:01:00','2011/03/07 20:06:00','2011/03/08 04:01:00',$
       '2011/03/12 15:51:00','2011/04/27 02:26:00','2011/05/11 02:41:00',$
       '2011/05/29 10:31:00','2012/01/05 07:10:00','2010/06/13 05:46:00',$
       '2010/06/12 01:04:00'] ; Example Events (End Times)
  ets=['2012/09/15 23:20:00',$
       '2012/07/28 21:05:00',$
       '2012/09/02 01:15:00',$
       '2012/10/07 20:45:00',$
       '2012/12/31 03:45:00']
  ets=['2011/02/11 10:32:00',$
       '2011/08/08 18:20:00',$
       '2011/08/09 08:20:00',$
       '2011/08/09 08:20:00',$
       '2011/08/09 08:20:00']
  ets=['2011/08/04 04:20:00',$
       '2011/10/20 03:35:00',$
       '2011/09/22 10:55:00',$
       '2011/11/09 13:30:00']
  
  ;choose which of all the events to load data for
  ;events2run=[0,1,2,3,4,5,6,7,8,9] 
  events2run=[0,1,2,3]
  waves2run=[5,6]

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
        aia_load_event,st,et,wav,index,data
        ;save,filename=savefile+'.sav',index,data

        newcoords=aia_autoselect_subroi(index[0],coords)
        subdata=aia_inspect_data(index,data,autoregion=newcoords)
        subindex=aia_update_subdata_index(index,[newcoords[0],newcoords[1]],1024,coords)
        save,filename=savefile+'_subdata'+'.sav',subindex,subdata
     endfor
  endfor
end

