
pro test_UV_disk_wave_detection

res=file_search('/Users/kkozarev/Downloads/aia.lev1.1600A_2011-05-11T02*.fits')
read_sdo,res,ind,dat,/uncomp_delete
;aia_prep,ind,dat,index,data

nx=n_elements(dat[*,0,0])
ny=n_elements(dat[0,*,0])
nt=n_elements(dat[0,0,*])
dd=intarr(nx,ny,nt-1)

;Make running difference images
for i=1,nt-1 do dd[*,*,i-1]=dat[*,*,i]-dat[*,*,i-1]
;Rebin to 1024x1024 px
md=intarr(nx/4,ny/4,nt-1)
for i=0,nt-2 do md[*,*,i]=rebin(dd[*,*,i],nx/4,ny/4)

wdef,0,1024
tvscl,md[*,*,0]

;This is for the May 11, 2011 event:
for i=0,13 do tv,bytscl(dd[2472:3495,2072:3095,i],50,100)

end
