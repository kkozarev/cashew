pro aia_map_contour_oplot
;a quick program that overplots contours of 211 and 193 AIA images

date='0612' ;0613

;==============================================================================
; 1. Definitions, constants, loading data
path='/home/kkozarev/Desktop/AIA/limbCMEs/'+date+'2010/results/'
outpath='/home/kkozarev/Desktop/AIA/limbCMEs/'+date+'2010/results/contours_193_211/'
restore,path+'211/diffMapData_'+date+'_211.sav'
diff211=diffmaps
d211=diff211[*].data
restore,path+'193/diffMapData_'+date+'_193.sav'
diff193=diffmaps
d193=diff193[*].data
diffmaps=0
ntimes=n_elements(diff211)
;==============================================================================



;==============================================================================
; 2. Plotting territory
loadct,8,/silent
set_plot,'z'
wdef,0,1024
tvlct,r,g,b,/get


if date eq '0612' then begin
frames=[5,70]
minval=0
maxval=4
nlevels=6
endif

if date eq '0613' then begin
frames=[20,80]
minval=0
maxval=4
nlevels=6
endif

for i=frames[0],frames[1] do begin
contour,smooth(d211[*,*,i],100,/edge_truncate),min_value=minval,max_value=maxval,nlevels=nlevels,thick=1.6
contour,smooth(d193[*,*,i],100,/edge_truncate),min_value=minval,max_value=maxval,nlevels=nlevels,color=130,/overplot,thick=1.6
xyouts,0.16,0.93,diff211[i].id+' '+diff211[i].time,/normal,charsize=2,charthick=2
xyouts,0.16,0.9,diff193[i].id+' '+diff193[i].time,/normal,charsize=2,charthick=2,color=130


wait,0.4
fname=outpath+'contour_'+date+'_193_211_'+strtrim(string(1000+i),2)+'.png'
write_png,fname,tvrd(),r,g,b
endfor
;==============================================================================

set_plot,'x'
end
