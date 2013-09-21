pro aia_prep_diff_synoptic;,wav,date,outpath=outpath;, totalData,indices,inpath,outpath=outpath,wav,date,hours,hmins,xrange,yrange,totalData,indices
  
;This procedure reads in a sequence of AIA fits images and saves
;running difference images of the entire disc binned to 1024 by 1024
;pixels.

;!NB The user needs to know what he/she is doing...
;Kamen Kozarev, September 2010
;

;INPUTS
;inpath - basic input path for data (string)
;outpath (optional) - output path for results (string)
;date - the date, in the format [yyyy,mm,dd] (string)
;wav - wavelength of AIA channel (string)
;hours - directory name for the specific hours (string)
;hmins - search string for the right files in the format [hhm], (string)
;        where 'm' is the decimal of the minutes. For example,
;        if we want all data between 05:30 and 06:00, then set
;        hmins=['053','054','055']
;   !NB this setup of hours and hmins only works for data
;   within a single hour so far!!!
;xrange - the x-range of pixels to extract from the original data (int)
;yrange - the y-range of pixels to extract from the original data (int)

;OUTPUT
;

;OPTIONAL OUTPUT
;If the keyword outpath is set, the program saves the file 
;regionData_mmdd_wav.sav, where wav isthe wavelength of AIA 
;channel and mmdd is the month and day (for example, 
;regionData_0613_171.sav)

time=systime()
tim1=systime(/seconds)

;===========================================================
;Constants and definitions
;===========================================================
wav='211'
date=['2010','11','07']
inpath='/data/SDO/AIA/level1/'
;outpath='/home/kkozarev/Desktop/AIA/limbCMEs/06132010/results/'+wav+'/'
outpath='/home/kkozarev/Desktop/synoptic/'

stride=4

print,'Reading the AIA '+wav+' channel fits files for '+date[0]+'/'+date[1]+'/'+date[2]
print,'Reading started: '+time


;hours=['H0005']
;hmins=['053','054']
;xrange=[3072,4095]
;yrange=[824,1847]
;xrange=[520,1220]
;yrange=[-800,-100]

dim=1024
dirpath=inpath+date[0]+'/'+date[1]+'/'+date[2]+'/'

;Create the list of files
for h=0,23 do begin
   hour='H'+strtrim(string(h),2)+'00'
   if h lt 10 then hour='H0'+strtrim(string(h),2)+'00'
   if h eq 0 then begin file=find_file(dirpath+hour+'/*'+wav+'.fits')
   endif else begin
      file=[file,find_file(dirpath+hour+'/*'+wav+'.fits')]
   endelse
endfor
nfiles=n_elements(file)



;===========================================================

;===========================================================
;2. Load the files
;===========================================================
set_plot,'z'
loadct,8,/silent
tvlct,r,g,b,/get
wdef,0,dim

xlength=0.5
ylength=0.025
x0=0.00
y0=1.0-ylength

read_sdo,file[0],index,tmp,/uncomp_delete

prev=rebin(tmp/index.exptime*2.90,dim,dim)

for i=1,nfiles-1,stride do begin
   read_sdo,file[i],index,tmp,/uncomp_delete
   data=rebin(tmp/index.exptime*2.90,dim,dim)
   diff=data-prev
   tv,diff+smooth(diff,10)*4
   polyfill,[x0,x0+xlength,x0+xlength,x0,x0], $
            [y0,y0,y0+ylength,y0+ylength,y0],color=0
   xyouts,x0+xlength/100.0,y0+ylength/4.0,'AIA/'+wav+'  '+index.date_obs,color=255,charsize=2,charthick=2

   fname='bdiff_'+date[1]+date[2]+date[2]+'_'+wav+'_'+strtrim(string(i+1000),2)+'.png'
   write_png,outpath+fname,tvrd(),r,g,b

   prev=data
   print,'Read frame #'+strtrim(string(i+1),2)+' out of '+strtrim(string(nfiles),2)
endfor
;===========================================================

time=systime()
tim2=systime(/seconds)
print,'Reading ended:  '+time
print,'This operation took '+strtrim(string((tim2-tim1)/60.0),2)+' minutes'

end
