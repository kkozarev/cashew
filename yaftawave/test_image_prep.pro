pro test_img_prep
set_plot,'x'

path='/Volumes/Backscratch/Users/kkozarev/AIA/events/'
enum='32'
date='20110427'
wav='193'
file='normalized_AIA_'+date+'_'+enum+'_'+wav+'_subdata.sav'
restore,path+'e'+enum+'/'+file
start_step=25
nx = n_elements(subdata[*,0,0])
ny = n_elements(subdata[0,*,0])
nt = n_elements(subdata[0,0,*])
end_step=nt-1
baseavgnsteps=10

;prepare base image
baseim=fltarr(1024,1024)
for i=start_step-baseavgnsteps,start_step-1 do baseim += subdata[*,*,i]
baseim /=(1.0*baseavgnsteps)

dx=subindex[0].IMSCL_MP*subindex[0].RSUN_REF/(1000.0*subindex[0].RSUN_OBS)
min_size = 2000; only track features containing min_size pixels or more

for t = start_step,end_step do begin
   wav=strtrim(string(subindex[t].wavelnth),2)
   print, "Tracking step:",string(t+1)
   filenum = STRMID(STRING(1000 + fix(t+1), FORMAT = '(I4)'), 1)
   filename = STRCOMPRESS('ywave_'+'AIA_'+wav+'_'+ filenum, /REMOVE_ALL)
   ind=subindex[t]
   
   
   
   ;Do a sequence of operations on each image
   origim=subdata[*,*,t]  ;original image
   im=origim
   imdespike=despike_gen(im)  ;despiked image
   im=imdespike
   imsmooth=smooth(im,8)  ;smoothed image
   im=imsmooth
   diffim = im-baseim ;base difference image
   im=diffim
   mom=moment(im)
   meanv=mom[0]
   nodiskim=aia_hide_disk(ind,im,value=meanv) ;image with disk removed
   im=nodiskim
   
   ;Enhance brighter features
   mom=moment(im)
   stdv=sqrt(mom[1])
   level=0.6
   thresh=stdv*level
   imenhance=im
   imenhance[where(imenhance gt thresh)]*=100.0
   imenhance[where(imenhance le thresh)]/=100.0
   im=imenhance
   

   freqim=fft(im,-1)
   power=shift(alog(abs(freqim)),512,512)

   filter = 1 / (1 + 1.0*(50/freqim)^(2))
   testim1=fft(freqim*filter,1)
   tv,testim1-baseim
   wait,0.3
   
   filter = 1 / (1 + 1.0*(freqim/100.0)^(2*1.0))
   testim2=fft(freqim*filter,1)

   tv,testim2-baseim
   wait,0.3

   filter = 1 / (1 + 1.0*(freqim/1000.0)^(2*1.0))
   testim3=fft(freqim*filter,1)
   tv,testim3-baseim






   stop
endfor

end
