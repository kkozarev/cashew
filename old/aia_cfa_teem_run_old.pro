;+==============================================================================
pro test_aia_cfa_teem_run
  ;test/batch run the AschDEM code
  fileset ='AschDEM'
  evnums=['05','06','07','13','19','20','23','32','37','38','41','113','112']
  coordX=[-955,729,777,-1073,812,-1069,771,-835,785,-843,864,883,633]
  coordY=[-50,-76,100,-70,750,-150,163,545,399,-200,113,-422,390]
  sts=['2011/01/25 11:56:00','2011/01/27 11:50:00','2011/01/28 00:45:00',$
       '2011/02/11 12:30:00','2011/03/07 19:35:00','2011/03/08 03:30:00',$
       '2011/03/12 15:20:00','2011/04/27 02:05:00','2011/05/11 02:10:00',$
       '2011/05/29 10:00:00','2012/01/05 06:56:00','2010/06/13 05:35:00',$
       '2010/06/12 00:55:00']

;The ending times are not inclusive in the minutes - that is, if the
;ending time for an event is '2011/01/25 12:26:00', an image taken at
;12:26:10 won't be read in.
  ets=['2011/01/25 12:27:00','2011/01/27 12:21:00','2011/01/28 01:16:00',$
       '2011/02/11 13:01:00','2011/03/07 20:06:00','2011/03/08 04:01:00',$
       '2011/03/12 15:51:00','2011/04/27 02:26:00','2011/05/11 02:41:00',$
       '2011/05/29 10:31:00','2012/01/05 07:10:00','2010/06/13 05:46:00',$
       '2010/06/12 01:04:00']
  
  er=8
  
  outpath='/Volumes/Backscratch/Users/kkozarev/AIA/events/'+evnums[er]+'/dem/aschwanden/'
  aia_cfa_teem_run,st=sts[er],et=ets[er],arcoords=[coordX[er],coordY[er]],outpath=outpath,/remove_aec
  
end
;-==============================================================================



;+==============================================================================
pro aia_cfa_teem_run,st=st,et=et,arcoords=arcoord,outpath=outpath,workdir=workdir,fileset=fileset,cfaarc=cfaarc,remove_aec=remove_aec
;PURPOSE:
;Program which runs Marcus Aschwanden's DEM code on an AIA datacube
;
;CATEGORY:
; DEM/AschDEM
;
;INPUTS:
;
;KEYWORDS:
; 
;
;OUTPUTS:
;
; 
;DEPENDENCIES:
; aia_load_data, aia_augment_timestring, aia_cfa_coalign_test
; aia_cfa_teem_table, aia_file_search, aia_cfa_teem_map, aia_cfa_teem_disp
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 07/2013
;10/01/2013, KAK - added remove_aec keyword that does not load AEC exposures.
;
set_plot,'x'
resolve_routine,'aia_load_data',/either,/compile_full_file,/no_recompile

if not keyword_set(fileset) then fileset ='AschDEM'
 
;INPUTS
;
if not keyword_set(outpath) then $
   outpath='/Volumes/Backscratch/Users/kkozarev/testAIA/dem/'
if not keyword_set(st) then st='2011/05/11 02:10:00'
if not keyword_set(et) then et='2011/05/11 02:11:47'
if not keyword_set(arcoords) then arcoords=[785,399] ; The AR coordinates in arcseconds for selecting a subframe

;Wavelengths ordered by temperature?
wave =['131','171','193','211','335','94'] 
nwave =n_elements(wave)
if not keyword_set(workdir) then workdir ='./'
if not keyword_set(cfaarc) then cfaarc='/Data/SDO/AIA/level1/'

io=3 ;   (0=screen, 3=color postscript file)
ct=3 ;   (IDL color table) 
nsig=3 ;   (contrast in number of standard deviations)
nsm=7 ;   (smoothing boxcar of limb profiles)



te_range=[0.5,10]*1.e6           ;   ([K], valid temperature range for DEM solutions) 
tsig=0.1*(1+1*findgen(10))       ;   (values of Gaussian logarithmic temperature widths)
q94=6.7                          ;   (correction factor for low-temperature 94 A response) 
fov=[0.5,0.5,1,1]*1.25           ;   (field-of-view [x1,y1,x2,y2] in solar radii) 
npix=1                           ;   (macropixel size=8x8 pixels, yields 512x512 map) 

i=0
;Loop over the data, augmenting the time strings by 12 seconds every
;time.
t_aug=12
if keyword_set(remove_aec) then t_aug=24
files=''
tmp1=st
tmp2=aia_augment_timestring(st,t_aug)


eet=tmp2
print,''

while anytim(tmp2) le anytim(et) do begin
   for w=0,n_elements(wave)-1 do begin
                                ;,/check171
      if w eq 0 then files=aia_file_search(tmp1,tmp2,wave[w],path=cfaarc,remove_aec=remove_aec) else $
         files=[files,aia_file_search(tmp1,tmp2,wave[w],path=cfaarc,remove_aec=remove_aec)]
   endfor
   
   ;tmpstr=tmp1[0]+tmp1[1]+tmp1[2]+'_'+tmp1[3]+tmp1[4]+tmp1[5]
   ;datstr=tmp1[0]+tmp1[1]+tmp1[2]
   tmpstr=strmid(tmp1,0,4)+strmid(tmp1,5,2)+strmid(tmp1,8,2)+'_'+strmid(tmp1,11,2)+strmid(tmp1,14,2)+strmid(tmp1,17,2)
   datstr=strmid(tmp1,0,4)+strmid(tmp1,5,2)+strmid(tmp1,8,2)
   teem_fname=outpath+fileset+'_'+tmpstr+'_'+'teem'
   table_fname=outpath+fileset+'_'+datstr+'_'+'teem'
   teem_table=table_fname+'_table.sav' ; (savefile that contains DEM loopup table)
   teem_map=teem_fname+'_map'
   
   if i eq 0 then begin
     ;aia_teem_table,wave,tsig,te_range,q94,fileset,teem_table
      aia_cfa_teem_table,files,wave,tsig,te_range,q94,teem_table

     ;aia_coalign_test,fileset,wave,io,ct,nsig,nsm,h_km,dx,dy
      aia_cfa_coalign_test,st,eet,table_fname,wave,io,ct,nsig,nsm,h_km,dx,dy
      i=1
   endif
   
   aia_cfa_teem_map,files,arcoords,wave,npix,teem_table,teem_map

   aia_cfa_teem_disp,teem_map,te_range,st

   st=aia_augment_timestring(st,t_aug)
   files=''
   tmp1=st
   tmp2=aia_augment_timestring(st,t_aug)
   eet=tmp2
   print,tmp1
   print,tmp2
   print,''

endwhile

end
;-==============================================================================
