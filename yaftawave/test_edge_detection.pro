pro test_edge_detection
set_plot,'x'

path='/Volumes/Backscratch/Users/kkozarev/AIA/events/'
enum='37'
date='20110511'
wav='193'
file='normalized_AIA_'+date+'_'+enum+'_'+wav+'_subdata.sav'
restore,path+enum+'/'+file
start_step=25
nx = n_elements(subdata[*,0,0])
ny = n_elements(subdata[0,*,0])
nt = n_elements(subdata[0,0,*])
end_step=nt-1
baseavgnsteps=10

;prepare base image
baseim=fltarr(nx,ny)
for i=start_step-baseavgnsteps,start_step-1 do baseim += subdata[*,*,i]
baseim /=(1.0*baseavgnsteps)

dx=subindex[0].IMSCL_MP*subindex[0].RSUN_REF/(1000.0*subindex[0].RSUN_OBS)
min_size = 2000; only track features containing min_size pixels or more

;for t = start_step,end_step do begin
;   wav=strtrim(string(subindex[t].wavelnth),2)
;   print, "Tracking step:",string(t+1)
;  filenum = STRMID(STRING(1000 + fix(t+1), FORMAT = '(I4)'), 1)
;  filename = STRCOMPRESS('ywave_'+'AIA_'+wav+'_'+ filenum, /REMOVE_ALL)
;   ind=subindex[t]
   
t=80
;Do a sequence of operations on each image
   origim=subdata[*,*,t]  ;original image
   im=origim
   imdespike=despike_gen(im)    ;despiked image
   im=imdespike
   imsmooth=smooth(im,8)  ;smoothed image
   im=imsmooth
   

   rdiffim=im-subdata[*,*,t-1]
   bdiffim = im-baseim ;base difference image
   im=rdiffim

   ;remove the disk so the algorithm doesn't get confused.
   mom=moment(im)
   meanv=mom[0]
   resim=aia_hide_disk(subindex[t],im,value=meanv)
   im=resim
   
;Start with the difference image
   wdef,0,1024
   tv,im
   stop

;Apply Shift_Diff filter
   shiftimage=shift_diff(im)
   ;tv,shiftimage
   ;stop
   tv,bytscl(shiftimage,min=-1,max=1)
   ;tv,bytscl(smooth(shiftimage,16,/edge_truncate),min=-1,max=1)
   stop
   
;Apply Laplacian filter
   lapimage=laplacian(im)
   tv,lapimage
   stop

;Apply Emboss filter
   embossimage=emboss(im)
   tv,embossimage
   stop


;Apply Roberts filter
   robimage=roberts(im)
   tv,robimage
   stop

;Apply Sobel filter
   sobimage=sobel(im)
   tv,sobimage
   stop
   
;Apply Prewitt filter
   prewimage=prewitt(im)
   tv,prewimage
   stop
   
;Apply Edge_dog filter
   edgedogimage=edge_dog(im)
   tv,edgedogimage
   stop


;Apply Histogram filter
   hist=bytscl(histogram(im))
   isoim=im
   ;iso_res=image_threshold(im,threshold=t,/byimage)
   ;isoim[where(iso_res eq 0)]=0
   tv,isoim
   stop
   
;endfor



end
