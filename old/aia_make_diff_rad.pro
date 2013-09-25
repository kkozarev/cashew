pro aia_make_diff_rad,wav,date,radData,radIndices,diffradData,outpath=outpath
;A procedure to create base difference images from AIA images 
;processed with a radial intensity filter.
;Also make plots and save them for making a movie.
;Procedure by Kamen Kozarev with code help from David Long, 08/11/2010.

;INPUT
;wav (string) - wavelength, in the format '000', like '193'
;date (string array) - date, in format ['2010','06','12']
;indata (type map)- an array of maps

;OUTPUT
;diffmaps (type map) - an array of base difference maps


;KEYWORDS
;outpath - the destination, in which to save the difference maps, in
;          the format diffMapData_0612_193.sav

set_plot,'x'
nframes=n_elements(radData[*,0,0])
dim=n_elements(radData[0,*,0])
diffradData=dblarr(nframes,dim,dim)

avg=radData[0,*,*]
;1. Create an average map from the first 10 images
for i=1,9 do begin
avg=avg+radData[i,*,*]
endfor
avg=avg/10.0



;2. Make the difference maps
set_plot,'z'
loadct,9,/silent
tvlct,r,g,b,/get
wdef,0,1024

for i=0,nframes-1 do begin
diffradData[i,*,*]=radData[i,*,*]-avg
tv,(diffradData[i,*,*]*0.4+smooth(diffradData[i,*,*],3))*6


fname='radbdiff_'+date[1]+date[2]+'_'+wav+'_'+strtrim(string(i+1000),2)+'.png'
write_png,outpath+'radbdiff/'+fname,tvrd(),r,g,b
;wait,0.4
endfor

;===========================================================
;3. Save all the data so I can go back and change the profiles
if keyword_set(outpath) then begin
   save,diffradData,radIndices,filename=outpath+'diffradData_'+date[1]+date[2]+'_'+wav+'.sav'
   endif
;===========================================================

set_plot,'x'
end
