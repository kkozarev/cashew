pro batch_prep_maps

wav=['211','193','335','171']
nwav=n_elements(wav)

date0612=['2010','06','12']
date0613=['2010','06','13']

path='/home/kkozarev/Desktop/AIA/limbCMEs/'

for i=0,nwav-1 do begin 
  aiaprepmaps_0612,wav[i],maps0612,$
                    outpath=path+date0612[1]+date0612[2]+date0612[0]+'/results/'+wav[i]+'/'


   aia_make_diff_maps,wav[i],date0612,maps0612,diffmaps0612,$
                      outpath=path+date0612[1]+date0612[2]+date0612[0]+'/results/'+wav[i]+'/'


aiaprepmaps_0613,wav[i],maps0613,$
                    outpath=path+date0613[1]+date0613[2]+date0613[0]+'/results/'+wav[i]+'/'


   aia_make_diff_maps,wav[i],date0613,maps0613,diffmaps0613,$
                      outpath=path+date0613[1]+date0613[2]+date0613[0]+'/results/'+wav[i]+'/'

endfor

end


