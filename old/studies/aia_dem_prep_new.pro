pro getpoints,xx,yy
;Allows the user to interactively select points on an image.
;Returns the x and y indices.

print,'Select points. If you want to stop, hit q.'
xx=0
yy=0
repeat begin
    char=''
    cursor,x,y,/data
    ;print,x,y
    xx=[xx,x]
    yy=[yy,y]
    plots,x,y,psym=2,symsize=2,/data
    wait,0.5
    char=get_kbrd(0)
endrep until char eq 'q'
xx=xx[1:*]
yy=yy[1:*]
print,xx
print,yy

end



pro aia_dem_prep_new,noselect=noselect

; This program will prepare files for running DEM on them, and save them in a .sav file as a structure (?).
;This file is for 0613 event.

;Update 03/2011 - This version is for regions that have been selected
;                 by hand, using the polyfillv() function.



;INPUTS
inpath='/Volumes/PLUME/AIA_data/2010/06/13/H0500/'
outpath='/home/kkozarev/Desktop/AIA/limbCMEs/06132010/DEM/new/'

fl1=['AIA20100613_053709_0094.fits','AIA20100613_053711_0131.fits','AIA20100613_053700_0171.fits',$
     'AIA20100613_053708_0193.fits','AIA20100613_053702_0211.fits','AIA20100613_053705_0335.fits']

fl2=['AIA20100613_053909_0094.fits','AIA20100613_053911_0131.fits','AIA20100613_053900_0171.fits',$
     'AIA20100613_053908_0193.fits','AIA20100613_053902_0211.fits','AIA20100613_053905_0335.fits']

;l1=['AIA20100613_053702_0211.fits']

;l2=['AIA20100613_053902_0211.fits']

xran=[3072,4095]
yran=[824,1847]

;END INPUTS

if keyword_set(noselect) then begin
restore,outpath+'demData_full_06132010.sav'
endif


;===================================================================
;1. First, load all EUV data for the specified period, and prep them
;to 1.5 level

for i=0,n_elements(fl1)-1 do begin
   if i eq 0 then begin
      files1=find_file(inpath+fl1[i])
      files2=find_file(inpath+fl2[i])
   endif else begin
      files1=[files1,find_file(inpath+fl1[i])]
      files2=[files2,find_file(inpath+fl2[i])]
   endelse
endfor


temparr=fltarr(4096,4096,6)

;The first time step:
;read
read_sdo,files1,index,data,/uncomp_delete
;prep to 1.5 level
aia_prep,index,data,index1,data1

;normalize to 1 sec exposure
for i=0,n_elements(files1)-1 do temparr[*,*,i]=data1[*,*,i]*1.0/index1[i].exptime

fdata1=temparr[xran[0]:xran[1],yran[0]:yran[1],*]

;data1norm=temparr
;reorder indices so wavelength is in increasing order
;fdata1=data1norm[*,*,sort(index1.wavelnth)]


;The second time step:
;read
read_sdo,files2,index,data,/uncomp_delete
;prep to 1.5 level
aia_prep,index,data,index2,data2
for i=0,n_elements(files2)-1 do temparr[*,*,i]=data2[*,*,i]*1.0/index2[i].exptime
fdata2=temparr[xran[0]:xran[1],yran[0]:yran[1],*]
;data2norm=temparr
;reorder indices so wavelength is in increasing order
;fdata2=data2norm[*,*,sort(index2.wavelnth)]

endif

wdef,0,1024
aia_lct,rr,gg,bb,wave=211
im=fdata2[*,*,4]-fdata1[*,*,4]
plot_image,alog(im)



;Check for the regions' positions...
;Region 1
if not keyword_set(noselect) then begin
   print,'SELECT POINTS FOR REGION 1'
   getpoints,xx,yy
   res1=polyfillv(xx,yy,n_elements(im[*,0,0]),n_elements(im[0,*,0]))
endif

nf=n_elements(fdata1[0,0,*])
for i=0,nf-1 do begin
    if i eq 0 then begin
        reg1=fltarr(n_elements(res1),nf)
        reg2=reg1
    endif
    mask=reform(fdata1[*,*,i])
    reg1[*,i]=mask[res1]
    mask=(fdata2[*,*,i])
    reg2[*,i]=mask[res1]
endfor
reg1_t1=reg1
reg1_t2=reg2
im[res1]=max(im)
plot_image,alog(im)

;stop

;Region 2
if not keyword_set(noselect) then begin
   print,'SELECT POINTS FOR REGION 2'
   getpoints,xx,yy
   res2=polyfillv(xx,yy,n_elements(im[*,0,0]),n_elements(im[0,*,0]))
endif

for i=0,nf-1 do begin
    if i eq 0 then begin
        reg1=fltarr(n_elements(res2),nf)
        reg2=reg1
    endif
    mask=fdata1[*,*,i]
    reg1[*,i]=mask[res2]

    mask=fdata2[*,*,i]
    reg2[*,i]=mask[res2]
endfor
reg2_t1=reg1
reg2_t2=reg2
im[res2]=max(im)
plot_image,alog(im)

;stop


;Region 3
if not keyword_set(noselect) then begin
   print,'SELECT POINTS FOR REGION 3'
   getpoints,xx,yy
   res3=polyfillv(xx,yy,n_elements(im[*,0,0]),n_elements(im[0,*,0]))
endif


for i=0,nf-1 do begin
    if i eq 0 then begin
        reg1=fltarr(n_elements(res3),nf)
        reg2=reg1
    endif
    mask=fdata1[*,*,i]
    reg1[*,i]=mask[res3]

    mask=fdata2[*,*,i]
    reg2[*,i]=mask[res3]
endfor
reg3_t1=reg1
reg3_t2=reg2
im[res3]=max(im)
plot_image,alog(im)

;stop



;Region 4
if not keyword_set(noselect) then begin
   print,'SELECT POINTS FOR REGION 4'
   getpoints,xx,yy
   res4=polyfillv(xx,yy,n_elements(im[*,0,0]),n_elements(im[0,*,0]))
endif

for i=0,nf-1 do begin
    if i eq 0 then begin
        reg1=fltarr(n_elements(res4),nf)
        reg2=reg1
    endif
    mask=fdata1[*,*,i]
    reg1[*,i]=mask[res4]

    mask=fdata2[*,*,i]
    reg2[*,i]=mask[res4]
endfor
reg4_t1=reg1
reg4_t2=reg2
im[res4]=max(im)
plot_image,alog(im)

;stop



;im=fdata2[*,*,4]
;im[res1]=max(im)
;im[res2]=max(im)
;im[res3]=max(im)
;plot_image,alog(im)

write_png,outpath+'aia_dem_regions_new.png',tvrd(),rr,gg,bb


;stop


;Finally, save the data in the DEM prep folder
save,files1,files2,index1,index2,fdata1,fdata2,res1,res2,res3,res4,$
      filename=outpath+'demData_full_06132010.sav'
save,index1,index2,reg4_t1,reg4_t2,reg3_t1,reg3_t2,reg2_t1,reg2_t2,reg1_t1,reg1_t2,$
      filename=outpath+'demData_regions_06132010.sav'

end
