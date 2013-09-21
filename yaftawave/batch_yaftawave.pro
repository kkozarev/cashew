pro batch_yaftawave

;This procedure will test the sensitivity of the threshold for yafta processing.

;pngpath='/home/kkozarev/algoTests/yafta/test/test100311/'
inpath='/Volumes/PLUME/AIA_data/studies/2011events/'
pngpath=inpath
e05w193='normalized_AIA_20110125_05_193_subdata.sav'
e05w211='normalized_AIA_20110125_05_211_subdata.sav'
e06w193='normalized_AIA_20110127_06_193_subdata.sav'
e06w211='normalized_AIA_20110127_06_211_subdata.sav'
e13w193='normalized_AIA_20110211_13_193_subdata.sav'
e13w211='normalized_AIA_20110211_13_211_subdata.sav'
e19w193='normalized_AIA_20110307_19_193_subdata.sav'
e19w211='normalized_AIA_20110307_19_211_subdata.sav'
e20w193='normalized_AIA_20110308_20_193_subdata.sav'
e20w211='normalized_AIA_20110308_20_211_subdata.sav'
e32w193='normalized_AIA_20110427_32_193_subdata.sav'
e32w211='normalized_AIA_20110427_32_211_subdata.sav'
e37w193='normalized_AIA_20110511_37_193_subdata.sav'
e37w211='normalized_AIA_20110511_37_211_subdata.sav'
e38w193='normalized_AIA_20110529_38_193_subdata.sav'
e38w211='normalized_AIA_20110529_38_211_subdata.sav'


fnames=[[e05w193,e06w193,e13w193,e19w193,e20w193,e32w193,e37w193,e38w193],$
        [e05w211,e06w211,e13w211,e19w211,e20w211,e32w211,e37w211,e38w211]]
evindex=['05','06','13','19','20','32','37','38']
begstep=[25,30,10,30,20,5,35,1] ;these are the initial steps for which to run the algorithm
endstep=[100,100,100,85,90,100,110,110] ;these are the final steps for which to run the algorithm

for ev=3,n_elements(evindex)-1 do begin
   event='e'+evindex[ev]
   for wav=0,1 do begin
      
;Load the data
      restore,inpath+event+'/'+fnames[ev,wav]

      lim=[begstep[ev]+1,endstep[ev]]
      baseim=subdata[*,*,begstep[ev]]
      subdata=subdata[*,*,lim[0]:lim[1]]
      subindex=subindex[lim[0]:lim[1]]
      
      ;use a threshold of 0.2
      evname='e'+evindex[ev]+'_'
         yaftawave_track_features,subindex,subdata,baseim,features,allmasks,$
                                  level=0.2,eventname=evname,$
                                  ps=pngpath+event+'/png/'+evname,$
                                  min_size=2500
         features=0
         allmasks=0
      
   endfor
endfor
end
