pro test_threshold

;This procedure will test the sensitivity of the threshold for yafta processing.

pngpath='/home/kkozarev/algoTests/yafta/test/'
f211_e5='/Volumes/PLUME/AIA_data/studies/2011events/normalized_AIA_20110125_05_211_subdata.sav'
f193_e5='/Volumes/PLUME/AIA_data/studies/2011events/normalized_AIA_20110125_05_193_subdata.sav'
f211_e13='/Volumes/PLUME/AIA_data/studies/2011events/normalized_AIA_20110211_13_211_subdata.sav'
f193_e13='/Volumes/PLUME/AIA_data/studies/2011events/normalized_AIA_20110211_13_193_subdata.sav'
fnames=[[f193_e5,f193_e13],[f211_e5,f211_e13]]
evindex=['05','13']



for ev=0,1 do begin
   for wav=0,1 do begin
      
;Load the data
      restore,fnames[ev,wav]
      lim=[1,n_elements(subindex)-1]
      
      baseim=subdata[*,*,0]
      subdata=subdata[*,*,lim[0]:lim[1]]
      subindex=subindex[lim[0]:lim[1]]
      
      for nn=1,2 do begin
         tnum=strtrim(string(nn),2)
         print,'Test #'+tnum
         print,''
         
         lev=0.1+0.2*nn
         evname='test_thresh_'+tnum+'_'+'e'+evindex[ev]
         yaftawave_track_features,subindex,subdata,baseim,features,allmasks,$
                                  level=lev,eventname=evname,ps=pngpath+evname
         features=0
         allmasks=0
         
      endfor
      
   endfor
endfor
end
