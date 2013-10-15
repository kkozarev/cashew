pro batch_make_movies
;This procedure will make png files for movies of wave events
;Kamen Kozarev, 01/2012
  
  path	='/Volumes/Backscratch/Users/kkozarev/AIA/events/'
  wave=['171','193','211','335','094','131','304']
  
  evnums=['05','06','07','13','19','20','23','32','37','38','41']

  sts=['2011/01/25 11:56:00','2011/01/27 11:50:00','2011/01/28 00:45:00',$
       '2011/02/11 12:30:00','2011/03/07 19:35:00','2011/03/08 03:30:00',$
       '2011/03/12 15:20:00','2011/04/27 02:05:00','2011/05/11 02:10:00',$
       '2011/05/29 10:00:00','2012/01/05 06:56:00']

  ;choose which of all the events to run
  ;events2run=[1,2,3,4,6,9]
  events2run=[8]
  waves2run=[1]
  wdef,0,1024

  for w=0,n_elements(waves2run)-1 do begin
     wav=wave[waves2run[w]]
     for j=0,n_elements(events2run)-1 do begin
        tvlct,rr,gg,bb,/get
        ;TVLCT, Reverse(rr), Reverse(gg), Reverse(bb)
        ev=events2run[j]
        evnum=evnums[ev]
        st=sts[ev]
        print,''
        print,'Loading '+wav+' channel AIA data for event #'+evnum
        print,''
        std=strsplit(st,'/ :',/extract)
        infname='normalized_AIA_'+std[0]+std[1]+std[2]+'_'+evnum+'_'+wav
        infile=path+evnum+'/'+infname
        restore,infile+'_subdata.sav'
        nsteps=n_elements(subindex)
        ;stop
        
        for i=70,90 do begin
           ind=strtrim(string(i),2)
           if ind lt 100 then ind='0'+ind
           if ind lt 10 then ind='0'+ind
           im=subdata[*,*,i]
           scimage=alog(im+abs(min(im))+1)
           ;scimage=1/(1+(0.5/(im+abs(min(im))+1))^0.18)
          ; TVLCT, Reverse(rr), Reverse(gg), Reverse(bb)
           tvscl,scimage
           ;if i eq 85 then stop
           xyouts,0.01,0.97,subindex[i].date_obs+' / '+wav,charsize=3,charthick=3,color=200
           xyouts,0.94,0.97,ind,charsize=3,charthick=3,color=200
           image=tvrd(true=1)

           write_png,infile+'_subdata_'+ind+'.png',image,rr,gg,bb
        endfor
       
     endfor
  endfor
end
