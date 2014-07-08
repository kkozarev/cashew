pro aiaprepsubregion_0612,wav
;, inpath,outpath=outpath,wav,date,hours,hmins,xrange,yrange,totalData,indices


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
;hmins - search string array for the right files in the format [hhm], (string)
;        where 'm' is the decimal of the minutes. For example,
;        if we want all data between 05:50 and 06:10, then set
;        hmins=[['055','N/A'],['060','061']]
;        The array has dimensions nh(number of hours),nm(number of minutes).
;        If the number of minutes in the different hours differs, set nm=max(nm)
;        and fill with 'N/A' strings.
;xrange - the x-range of pixels to extract from the original data (int)
;yrange - the y-range of pixels to extract from the original data (int)

;OUTPUT
;

;OPTIONAL OUTPUT
;If the keyword outpath is set, the program saves the file 
;regionData_mmdd_wav.sav, where wav isthe wavelength of AIA 
;channel and mmdd is the month and day (for example, 
;regionData_0613_171.sav)

;EXAMPLE
;datapath='/data/SDO/AIA/level1/'
;outpath='/home/kkozarev/Desktop/AIA/limbCMEs/06132010/test/'
;aiaprepsubregion,datapath,outpath=outpath,'171',['2010','06','13'],$
;                 ['H0005'],['054'],[3072,4095],[1024,2047],data,indices

;===========================================================
;Constants and definitions
;===========================================================
inpath='/home/kkozarev/Desktop/AIA/limbCMEs/20100612/'+wav+'A/'
outpath='/home/kkozarev/Desktop/AIA/limbCMEs/06122010/results/'+wav+'/'
date=['2010','06','12']
;wav='193'
hours=['00','01']
hmins=[['5'],['0']]
xrange=[3072,4095]
yrange=[2548,3571]


dim=1024
;dirpath=inpath+date[0]+'/'+date[1]+'/'+date[2]+'/'

;Currently, this only works for a single hour
;for h=0,n_elements(hours)-1 do begin
;   for m=0,n_elements(hmins[*,0])-1 do begin
;      if m eq 0 then begin file=find_file(dirpath+hours[h]+'/*_'+hmins[m,h]+'*_0'+wav+'.fits')
;      endif else begin
;         file=[file,find_file(dirpath+hours[h]+'/*_'+hmins[m,h]+'*_0'+wav+'.fits')]
;      endelse
;   endfor
;endfor


;======SPECIAL IMPLEMENTATION FOR 06122010 EVENT=======================
dirpath=inpath
for h=0,n_elements(hours)-1 do begin
   for m=0,n_elements(hmins[*,0])-1 do begin
      if m eq 0 and h eq 0 then begin
    ;print,dirpath+'aia_test.lev1.'+wav+'A_'+date[0]+'-'+date[1]+'-'+date[2]+'T'+hours[h]+'_'+hmins[m,h]+'*.fits'
	file=find_file(dirpath+'aia_test.lev1.'+wav+'A_'+date[0]+'-'+date[1]+'-'+date[2]+'T'+hours[h]+'_'+hmins[m,h]+'*.fits')
      endif else begin
	file=[file,find_file(dirpath+'aia_test.lev1.'+wav+'A_'+date[0]+'-'+date[1]+'-'+date[2]+'T'+hours[h]+'_'+hmins[m,h]+'*.fits')]
      endelse
   endfor
endfor
;======SPECIAL IMPLEMENTATION FOR 06122010 EVENT=======================

nfiles=n_elements(file)
totalData=intarr(nfiles,dim,dim)

times=strarr(nfiles)

mreadfits,file[0],index,data
indices=replicate(index,nfiles)
;===========================================================

;wdef,0,1024
;tvscl,sqrt(data[xrange[0]:xrange[1],yrange[0]:yrange[1]])
;stop

;===========================================================
;2. Load the files
;===========================================================
for i=0,nfiles-1 do begin
   mreadfits,file[i],index,data
   tmp1=data[xrange[0]:xrange[1],yrange[0]:yrange[1]]
   totalData[i,*,*]=tmp1
   ;tvscl,sqrt(tmp1)
   copy_struct_inx,index,indices,index_to=i
   print,'Read frame #'+strtrim(string(i+1),2)+' out of '+strtrim(string(nfiles),2)
   times[i]=index.date_obs
endfor
;===========================================================


;===========================================================
;3. Save all the data so I can go back and change the profiles
if keyword_set(outpath) then begin
   save,totalData,indices,times,filename=outpath+'regionData_'+date[1]+date[2]+'_'+wav+'.sav'
   endif
;===========================================================

end
