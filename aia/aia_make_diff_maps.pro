pro aia_make_diff_maps,wav,date,maps,diffmaps,outpath=outpath
;PURPOSE:
;A procedure to create base difference maps
;(map tutorial available at http://www.sipwork.org/?p=42 ), and then
;make difference maps. Save the difference and original maps for
;future use. Also make plots and save them for making a movie.
;
;CATEGORY:
; AIA/General
;
;INPUTS:
;wav (string) - wavelength, in the format '000', like '193'
;date (string array) - date, in format ['2010','06','12']
;indata (type map)- an array of maps
;
;KEYWORDS:
;outpath - the destination, in which to save the difference maps, in
;          the format diffMapData_0612_193.sav
;
;OUTPUTS:
;diffmaps (type map) - an array of base difference maps
; 
;DEPENDENCIES:
;
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev with code help from David Long, 08/11/2010.
;

set_plot,'x'
nmaps=n_elements(maps)

avg=maps[0]
;1. Create an average map from the first 10 images
for i=1,9 do begin
avg.data=avg.data+maps[i].data
endfor
avg.data=avg.data/10.0



;2. Make the difference maps
set_plot,'z'
loadct,9,/silent
tvlct,r,g,b,/get
wdef,0,1024

for i=0,nmaps-1 do begin
ss=diff_map(reform(maps[i]),avg)
if i eq 0 then diffmaps=replicate(ss,nmaps)
diffmaps[i]=ss
plot_map, diffmaps[i], dmin = -70, dmax = 20, /isotropic, /limb, lthick = 1, grid_spacing = 20, gthick = 1, gstyle = 0

;if i eq 25 then begin
;plot_map, diffmaps[i], dmin = -200, dmax = 100, /isotropic, /limb, lthick = 1, grid_spacing = 20, gthick = 1, gstyle = 0
;stop
;endif
fname='mapbdiff_'+date[1]+date[2]+'_'+wav+'_'+strtrim(string(i+1000),2)+'.png'
write_png,outpath+'mapbdiff/'+fname,tvrd(),r,g,b
;wait,0.4
endfor

;===========================================================
;3. Save all the data so I can go back and change the profiles
if keyword_set(outpath) then begin
   save,diffmaps,filename=outpath+'diffMapData_'+date[1]+date[2]+'_'+wav+'.sav'
   endif
;===========================================================

set_plot,'x'
end
