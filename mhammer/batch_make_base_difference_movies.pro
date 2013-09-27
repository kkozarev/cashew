pro batch_make_base_difference_movies, CHECK_EXPOSURES = check_exposures
;This procedure will make png files for movies of wave events
;Kamen Kozarev, 01/2012
  
  ;path='/data/tokyo/kkozarev/2011events/'
  path='/Volumes/Scratch/Users/mhammer/2012events/'
  wave=['171','193','211','335','094','131','304']
  wave_exposure_times=[2.0,2.0,2.9,2.9,2.9,2.9,2.9]
  
  ;evnums=['0423W','0501E','0623E'] ; Good 2013
  evnums=['0915W','0728E','1007E'] ; Good 2012
  ;evnums=['04','05','07','11','13','18',$
  ;        '19','20','29','37','38']
  ;evnums=['12','42','45']
  evnums=['50','51','52','53',$
          '0313W','0517W','0526W','0326E','0424E']

  ;sts=['2013/04/23 18:05:00',$ ; Good Events 2013
  ;     '2013/05/01 20:35:00',$
  ;     '2013/06/23 20:40:00']
  sts=['2012/09/15 22:50:00',$ ; Good Events 2012
       '2012/07/28 20:35:00',$
       '2012/10/07 20:15:00']
  ;sts=['2011/01/18 00:00:00','2011/01/25 00:00:00','2011/01/28 00:00:00',$
  ;     '2011/02/11 00:00:00','2011/02/11 00:00:00','2011/03/05 00:00:00',$
  ;     '2011/03/07 00:00:00','2011/03/08 00:00:00','2011/04/07 00:00:00',$
  ;     '2011/05/11 00:00:00','2011/05/29 00:00:00']
  ;sts=['2011/02/11 09:50:00',$
  ;     '2011/08/08 18:00:00',$
  ;     '2011/08/09 08:00:00']

  ;choose which of all the events to run
  ;events2run=[1,2,3,4,6,9]
  events2run=[1]
  waves2run=[1,2]

  ;loadct,9,/silent
  wdef,4,1024

  for w=0,n_elements(waves2run)-1 do begin
     wav=wave[waves2run[w]]
     wav_exposure_time=wave[waves2run[w]]
     for j=0,n_elements(events2run)-1 do begin
        
        loadct,8,/silent ; set color table
        tvlct,rr,gg,bb,/get

        ev=events2run[j]
        evnum=evnums[ev]
        st=sts[ev]
        print,''
        print,'Loading '+wav+' channel AIA data for event #'+evnum
        print,''
        std=strsplit(st,'/ :',/extract)
        infname='normalized_AIA_'+std[0]+std[1]+std[2]+'_'+evnum+'_'+wav
        infile=path+'e'+evnum+'/'+infname
        svfile=path+'e'+evnum+'/base/'+wav+'/'+infname
        restore,infile+'_subdata.sav'
        nsteps=n_elements(subindex)
        ;stop

        ; Set up base
        base = subdata[*,*,0]
        for i=1,4 do begin
           base = base + subdata[*,*,i]
        endfor
        base = base / 5

        ; Remove Bad Exposures
        ; There should be a parameter that says whether or not
        ; you should check if bad exposures need to be removed
        ; or if bad exposures have already been removed

        check_exposures, good_exposures, $
           SUBDATA = subdata, SUBINDEX = subindex
 
        for i=0,nsteps-1 do begin
           if good_exposures[i] eq 1 then begin
              ind=strtrim(string(i),2)
              if ind lt 100 then ind='0'+ind
              if ind lt 10 then ind='0'+ind
              ;im=subdata[*,*,i]*basetime/subindex[i].exptime - base
              ;scimage=bytscl(im,max=30,min=-50)
              scimage = (bytscl((subdata[*,*,i])-(subdata[*,*,0]), max=30, min=-50)+0)
              ;stop
              ;tv,smooth(scimage,6)
              tvscl,scimage
              xyouts,0.01,0.97,subindex[i].date_obs+' / '+wav,charsize=3,charthick=3,color=200
              xyouts,0.94,0.97,ind,charsize=3,charthick=3,color=200

              image=tvrd(true=1)
              write_png,svfile+'_subdata_base_'+ind+'.png',image,rr,gg,bb
           endif ; Good Exposure endif
        endfor
       ;stop
     endfor
  endfor
end ; EOF
