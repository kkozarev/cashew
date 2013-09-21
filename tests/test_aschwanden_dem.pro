pro test_aschwanden_dem

wave_ =['131','171','193','211','335','94'] 
nwave =n_elements(wave_)
workdir ='./'
;path='/Data/SDO/AIA/level1/2011/05/11/H0200/'
cfaarc='/Data/SDO/AIA/level1/'
outpath='/Volumes/PLUME/AIA_data/studies/2011events/e37/dem/aschwanden/'

io=3 ;   (0=screen, 3=color postscript file)
ct=3 ;   (IDL color table) 
nsig=3 ;   (contrast in number of standard deviations)
nsm=7 ;   (smoothing boxcar of limb profiles)
;st='2011-05-11 02:10:00'
;et='2011-05-11 02:40:47'
st='2011-05-11 02:21:36'
et='2011-05-11 02:40:47'

fileset ='AschDEM'
arcoords=[785,399] ; The AR coordinates in arcseconds for selecting a subframe
te_range=[0.5,10]*1.e6           ;   ([K], valid temperature range for DEM solutions) 
tsig=0.1*(1+1*findgen(10))       ;   (values of Gaussian logarithmic temperature widths)
q94=6.7                          ;   (correction factor for low-temperature 94 A response) 
fov=[0.5,0.5,1,1]*1.25           ;   (field-of-view [x1,y1,x2,y2] in solar radii) 
npix=1                           ;   (macropixel size=8x8 pixels, yields 512x512 map) 

;print,st
;print,''
;Loop over the data, augmenting the time strings by 12 seconds every time.
for i=0,18 do begin
 
   st=aia_augment_timestring(st,12)
   ;if i eq 0 then print,st
   files=''
   tmp1=st
   tmp2=aia_augment_timestring(st,12)
   eet=tmp2
   print,tmp1
   print,tmp2
   print,''
   
   for w=0,n_elements(wave_)-1 do begin
      if w eq 0 then files=cfa_search(cfaarc,tmp1,tmp2,wave_[w]) else files=[files,cfa_search(cfaarc,tmp1,tmp2,wave_[w])]
   endfor
   
   tmpstr=tmp1[0]+tmp1[1]+tmp1[2]+'_'+tmp1[3]+tmp1[4]+tmp1[5]
   datstr=tmp1[0]+tmp1[1]+tmp1[2]
   teem_fname=outpath+fileset+'_'+tmpstr+'_'+'teem'
   table_fname=outpath+fileset+'_'+datstr+'_'+'teem'
   teem_table=table_fname+'_table.sav' ;   (savefile that contains DEM loopup table)
   teem_map=teem_fname+'_map'
   ;print,teem_fname
   ;stop
   

   
   if i eq 1000 then begin


;aia_teem_table,wave_,tsig,te_range,q94,fileset,teem_table
      aia_cfa_teem_table,files,wave_,tsig,te_range,q94,teem_table
      
;aia_coalign_test,fileset,wave_,io,ct,nsig,nsm,h_km,dx,dy
      aia_cfa_coalign_test,st,eet,table_fname,wave_,io,ct,nsig,nsm,h_km,dx,dy
   endif
   
   
   aia_cfa_teem_map,files,arcoords,wave_,npix,teem_table,teem_map
;stop
   aia_cfa_teem_disp,teem_map,te_range,st

endfor
print,''
print,tmp2
print,et

end
;_________________________________________________________________________
;_________________________________________________________________________





;_________________________________________________________________________
;_________________________________________________________________________
function aia_augment_timestring,oldtime,nsec
;a simple procedure that takes a date/time string, like '2011-05-11
;02:26:00' and adds a certain number of SECONDS to the time,
;taking care to augment it properly. It is only accurate to within a
;day change...
;_________________________________________________________________________
newtime=''
nsec=fix(nsec)

;Record the times in string arrays
st=strsplit(oldtime,' /:,.-T',/extract)
if n_elements(st) eq 4 then st=[st,'00']
if n_elements(st) eq 5 then st=[st,'00']


;The seconds
minsec=0
dsec=nsec mod 60
newsec=st[5]+dsec
while newsec ge 60 do begin
   minsec++
   newsec-=60
endwhile
newsec=strtrim(string(newsec),2)
if newsec lt 10 then newsec='0'+newsec


;The minutes
hrmin=0
dmin=nsec/60
newmin=st[4]+dmin+minsec
while newmin ge 60 do begin
   hrmin++
   newmin-=60
endwhile
newmin=strtrim(string(newmin),2)
if newmin lt 10 then newmin='0'+newmin


;The hours
dayhr=0
dhr=(st[4]+dmin)/60
newhr=st[3]+dhr+hrmin
if newmin eq 0 then newhr++
while newhr ge 24 do begin
   dayhr++
   newhr-=24
endwhile
newhr=strtrim(string(newhr),2)
if newhr lt 10 then newhr='0'+newhr



;The days
dday=(st[3]+dhr)/24
newday=st[2]+dday+dayhr
newday=strtrim(string(newday),2)
if newday lt 10 then newday='0'+newday


newtime=st[0]+'-'+st[1]+'-'+newday+' '+newhr+':'+newmin+':'+newsec

return, newtime
end
