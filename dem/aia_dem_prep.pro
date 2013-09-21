pro aia_dem_prep
; This program will prepare files for running DEM on them, and save
; them in a .sav file as a structure. This file is for 0613 event.



;INPUTS
inpath='/data/SDO/AIA/level1/2010/06/13/H0005/'
outpath='/home/kkozarev/Desktop/AIA/limbCMEs/06132010/DEM/'

fl1=['AIA20100613_053700_0171.fits','AIA20100613_053702_0211.fits',$
	'AIA20100613_053705_0335.fits','AIA20100613_053708_0193.fits','AIA20100613_053709_0094.fits',$
	'AIA20100613_053711_0131.fits']
fl2=['AIA20100613_053900_0171.fits','AIA20100613_053902_0211.fits',$
	'AIA20100613_053905_0335.fits','AIA20100613_053908_0193.fits','AIA20100613_053909_0094.fits',$
	'AIA20100613_053911_0131.fits']

xran=[3072,4095]
yran=[824,1847]


;END INPUTS

;0. Get the AIA temperature response


;1. First, load all EUV data for the specified period, and prep them
;to 1.5 level

for i=0,5 do begin
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
for i=0,5 do temparr[*,*,i]=data1[*,*,i]*1.0/index1[i].exptime
data1norm=temparr
;reorder indices so wavelength is in increasing order
fdata1=data1norm[*,*,sort(index1.wavelnth)]


;The second time step:
;read
read_sdo,files2,index,data,/uncomp_delete
;prep to 1.5 level
aia_prep,index,data,index2,data2
for i=0,5 do temparr[*,*,i]=data2[*,*,i]*1.0/index2[i].exptime
data2norm=temparr
;reorder indices so wavelength is in increasing order
fdata2=data2norm[*,*,sort(index2.wavelnth)]

wdef,0,1024
aia_lct,rr,gg,bb,wave=211
;plot_image,sqrt(fdata2[xran[0]:xran[1],yran[0]:yran[1],4]),min=sqrt(0),max=sqrt(100)
plot_image,sqrt(fdata2[*,*,4]),min=sqrt(0),max=sqrt(100)

;first region 
reg1x=[670,709]
reg1y=[700,739]
oplot,[xran[0]+reg1x[0],xran[0]+reg1x[1]],[yran[0]+reg1y[0],yran[0]+reg1y[0]],color=0
oplot,[xran[0]+reg1x[0],xran[0]+reg1x[0]],[yran[0]+reg1y[0],yran[0]+reg1y[1]],color=0
oplot,[xran[0]+reg1x[1],xran[0]+reg1x[1]],[yran[0]+reg1y[0],yran[0]+reg1y[1]],color=0
oplot,[xran[0]+reg1x[0],xran[0]+reg1x[1]],[yran[0]+reg1y[1],yran[0]+reg1y[1]],color=0
reg1_t1=fdata1[xran[0]+reg1x[0]:xran[0]+reg1x[1],yran[0]+reg1y[0]:yran[0]+reg1y[1],*]
reg1_t2=fdata2[xran[0]+reg1x[0]:xran[0]+reg1x[1],yran[0]+reg1y[0]:yran[0]+reg1y[1],*]

;second region
reg2x=[770,809]
reg2y=[500,539]
oplot,[xran[0]+reg2x[0],xran[0]+reg2x[1]],[yran[0]+reg2y[0],yran[0]+reg2y[0]],color=0
oplot,[xran[0]+reg2x[0],xran[0]+reg2x[0]],[yran[0]+reg2y[0],yran[0]+reg2y[1]],color=0
oplot,[xran[0]+reg2x[1],xran[0]+reg2x[1]],[yran[0]+reg2y[0],yran[0]+reg2y[1]],color=0
oplot,[xran[0]+reg2x[0],xran[0]+reg2x[1]],[yran[0]+reg2y[1],yran[0]+reg2y[1]],color=0
reg2_t1=fdata1[xran[0]+reg2x[0]:xran[0]+reg2x[1],yran[0]+reg2y[0]:yran[0]+reg2y[1],*]
reg2_t2=fdata2[xran[0]+reg2x[0]:xran[0]+reg2x[1],yran[0]+reg2y[0]:yran[0]+reg2y[1],*]

;third region
reg3x=[730,769]
reg3y=[300,339]
oplot,[xran[0]+reg3x[0],xran[0]+reg3x[1]],[yran[0]+reg3y[0],yran[0]+reg3y[0]],color=0
oplot,[xran[0]+reg3x[0],xran[0]+reg3x[0]],[yran[0]+reg3y[0],yran[0]+reg3y[1]],color=0
oplot,[xran[0]+reg3x[1],xran[0]+reg3x[1]],[yran[0]+reg3y[0],yran[0]+reg3y[1]],color=0
oplot,[xran[0]+reg3x[0],xran[0]+reg3x[1]],[yran[0]+reg3y[1],yran[0]+reg3y[1]],color=0
reg3_t1=fdata1[xran[0]+reg3x[0]:xran[0]+reg3x[1],yran[0]+reg3y[0]:yran[0]+reg3y[1],*]
reg3_t2=fdata2[xran[0]+reg3x[0]:xran[0]+reg3x[1],yran[0]+reg3y[0]:yran[0]+reg3y[1],*]

;fourth region
reg4x=[560,599]
reg4y=[200,239]
oplot,[xran[0]+reg4x[0],xran[0]+reg4x[1]],[yran[0]+reg4y[0],yran[0]+reg4y[0]],color=0
oplot,[xran[0]+reg4x[0],xran[0]+reg4x[0]],[yran[0]+reg4y[0],yran[0]+reg4y[1]],color=0
oplot,[xran[0]+reg4x[1],xran[0]+reg4x[1]],[yran[0]+reg4y[0],yran[0]+reg4y[1]],color=0
oplot,[xran[0]+reg4x[0],xran[0]+reg4x[1]],[yran[0]+reg4y[1],yran[0]+reg4y[1]],color=0
reg4_t1=fdata1[xran[0]+reg4x[0]:xran[0]+reg4x[1],yran[0]+reg4y[0]:yran[0]+reg4y[1],*]
reg4_t2=fdata2[xran[0]+reg4x[0]:xran[0]+reg4x[1],yran[0]+reg4y[0]:yran[0]+reg4y[1],*]

;fifth region
reg5x=[340,379]
reg5y=[180,219]
oplot,[xran[0]+reg5x[0],xran[0]+reg5x[1]],[yran[0]+reg5y[0],yran[0]+reg5y[0]],color=0
oplot,[xran[0]+reg5x[0],xran[0]+reg5x[0]],[yran[0]+reg5y[0],yran[0]+reg5y[1]],color=0
oplot,[xran[0]+reg5x[1],xran[0]+reg5x[1]],[yran[0]+reg5y[0],yran[0]+reg5y[1]],color=0
oplot,[xran[0]+reg5x[0],xran[0]+reg5x[1]],[yran[0]+reg5y[1],yran[0]+reg5y[1]],color=0
reg5_t1=fdata1[xran[0]+reg5x[0]:xran[0]+reg5x[1],yran[0]+reg5y[0]:yran[0]+reg5y[1],*]
reg5_t2=fdata2[xran[0]+reg5x[0]:xran[0]+reg5x[1],yran[0]+reg5y[0]:yran[0]+reg5y[1],*]

;write_png,outpath+'aia_dem_regions_211.png',tvrd(),rr,gg,bb


stop


;Finally, save the data in the DEM prep folder
save,index1,index2,fdata1,fdata2,$
      filename=outpath+'demData_full_06132010.sav'
save, index1,index2,reg5_t1,reg5_t2,reg4_t1,reg4_t2,reg3_t1,reg3_t2,reg2_t1,reg2_t2,reg1_t1,reg1_t2,$
      filename=outpath+'demData_regions_06132010.sav'

end
