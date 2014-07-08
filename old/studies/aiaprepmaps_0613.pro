pro aiaprepmaps_0613,wav,maps,outpath=outpath;, totalData,indices,inpath,outpath=outpath,wav,date,hours,hmins,xrange,yrange,totalData,indices

;This procedure reads in a sequence of AIA fits images and saves a
;subregion as a data cube.
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


;===========================================================
;Constants and definitions
;===========================================================
inpath='/data/SDO/AIA/level1/'
outpath='/home/kkozarev/Desktop/AIA/limbCMEs/06132010/results/'+wav+'/'
date=['2010','06','13']
;wav='171'
hours=['H0005']
hmins=['053','054']
;xrange=[3072,4095]
;yrange=[824,1847]
xrange=[520,1220]
yrange=[-800,-100]

dim=1024
dirpath=inpath+date[0]+'/'+date[1]+'/'+date[2]+'/'

;Currently, this only works for a single hour
for h=0,n_elements(hours)-1 do begin
   for m=0,n_elements(hmins)-1 do begin
      if h eq 0 and m eq 0 then begin file=find_file(dirpath+hours[h]+'/*_'+hmins[m]+'*_0'+wav+'.fits')
      endif else begin
         file=[file,find_file(dirpath+hours[h]+'/*_'+hmins[m]+'*_0'+wav+'.fits')]
      endelse
   endfor
endfor



nfiles=n_elements(file)

mreadfits, file[0], index, tmp
index2map,index,tmp,map
sub_map,map,smap,xrange=xrange,yrange=yrange
maps=replicate(smap,nfiles)
maps[0]=smap
;===========================================================


;===========================================================
;2. Load the files
;===========================================================
for i=1,nfiles-1 do begin
   mreadfits,file[i],index,tmp
   index2map,index,tmp,map
   sub_map,map,smap,xrange=xrange,yrange=yrange
   maps[i]=smap
   print,'Read frame #'+strtrim(string(i+1),2)+' out of '+strtrim(string(nfiles),2)
endfor
;===========================================================


;===========================================================
;3. Save all the data so I can go back and change the profiles
if keyword_set(outpath) then begin
   save,maps,filename=outpath+'mapData_'+date[1]+date[2]+'_'+wav+'.sav'
   endif
;===========================================================

end
