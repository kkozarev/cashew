pro batch_rad_diff_data

wav=['211','193','335','171']
nwav=n_elements(wav)

date0612=['2010','06','12']
date0613=['2010','06','13']

path='/home/kkozarev/Desktop/AIA/limbCMEs/'


for i=0,nwav-1 do begin
   if i gt 0 then make_aia_raddiff_0612,wav=wav[i],radData0612,indices0612,$
                           outpath=path+date0612[1]+date0612[2]+date0612[0]+'/results/'+wav[i]+'/'
   if i eq 0 then begin
      restore, '/home/kkozarev/Desktop/AIA/limbCMEs/06122010/results/211/radData_0612_211.sav'
      radData0612=radData
      indices0612=indices
   endif
   aia_make_diff_rad,wav[i],date0612,radData0612,indices0612,diffradData0612,$
                      outpath=path+date0612[1]+date0612[2]+date0612[0]+'/results/'+wav[i]+'/'

   
   if i gt 0 then make_aia_raddiff_0613,wav=wav[i],radData0613,indices0613,$
                           outpath=path+date0613[1]+date0613[2]+date0613[0]+'/results/'+wav[i]+'/'

   if i eq 0 then begin
      restore, '/home/kkozarev/Desktop/AIA/limbCMEs/06132010/results/211/radData_0613_211.sav'
      radData0613=radData
      indices0613=indices
   endif
   aia_make_diff_rad,wav[i],date0613,radData0613,indices0613,diffradData0613,$
                      outpath=path+date0613[1]+date0613[2]+date0613[0]+'/results/'+wav[i]+'/'

endfor




end
