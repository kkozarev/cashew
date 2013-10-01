
;+====================================================================
pro test_callisto_plot_spectrum
  
;Run the program like this to combine four files at different times
;and frequency ranges
;Order the files like this - rows are time, cols are frequency (from high freq to low)
;files = [['BIR_20111105_101500_02.fit.gz','BIR_20111105_103000_02.fit.gz'],$
;         ['BIR_20111105_101500_01.fit.gz','BIR_20111105_103000_01.fit.gz']]
;station='Birr'
;timerange='2011-NOV-05 10:' + ['20:30','36:00']
;freqrange=[40,160]

files = [['SSRT_20110511_021500_59.fit.gz','SSRT_20110511_023000_59.fit.gz']]
files='/home/kkozarev/svn/corwav/trunk/radio/callisto/'+files
station='SSRT'
freqrange=[45,160]
timerange='2011-MAY-11 02:' + ['20:00','32:00']
numplotmax=2


files = ['ALMATY_20110809_080000_59.fit']
files='/home/kkozarev/svn/corwav/trunk/radio/callisto/'+files
timerange='2011-AUG-09 08:' + ['02:30','04:30']
freqrange=[200,450]
station='Almaty'
width_cutoff=0.5

callisto_plot_spectrum,files,timerange=timerange,station=station,$
                                numplotmax=numplotmax,/fit,freqrange=freqrange,width_cutoff=width_cutoff,/refine

stop


files = ['ALMATY_20110809_080000_59.fit']
files='/home/kkozarev/svn/corwav/trunk/radio/callisto/'+files
timerange='2011-AUG-09 08:' + ['02:00','04:30']
freqrange=[47,450]
station='Almaty'

;plot this to see everything in the file (frequency and time)
callisto_plot_spectrum,files

stop

;run this to zoom in on a range of frequencies and times
callisto_plot_spectrum,files,timerange=timerange,station=station,freqrange=freqrange,plot=plot;,fit=fit
end
;-====================================================================


;+====================================================================
PRO callisto_plot_spectrum,files,timerange=timerange,station=station,freqrange=freqrange,fit=fit,plot=plot,numplotmax=numplotmax,width_cutoff=width_cutoff,refine=refine
set_plot,'x'
;plot and analyze spectrograms of Callisto data.
; EXTERNAL CALLS:
; jmap_find_maxima, elimwrongchannels, constbacksub, spectro_plot
;
; MODIFICATION HISTORY:
; 2013/09/11, Written by Kamen Kozarev, SAO


  resolve_routine,'jmap_find_maxima',/either,/compile_full_file,/no_recompile
  
loadct,9,/silent
tvlct,rr,gg,bb,/get
tvlct,reverse(rr),reverse(gg),reverse(bb)
!P.font=1
!p.position=[0.1,0.13,0.93,0.92]

nfiles=n_elements(files)
ntf=n_elements(files[*,0])
nff=n_elements(files[0,*])

if not keyword_set(station) then station=''
if not keyword_set(numplotmax) then numplotmax=3
if not keyword_set(width_cutoff) then widthcutoff=0.2 else widthcutoff=width_cutoff

;Read the radio files
for ff=0,nff-1 do begin
   for tf=0,ntf-1 do begin
      if find_file(files[tf,ff]) eq '' then begin
         print,''
         print,'Error: File "'+ files[tf,ff]+'" was not found. Exiting...'
         print,''
         return
      endif
      radio_spectro_fits_read,files[tf,ff],z1,x1,y1
      if tf eq 0 then begin
         z=z1
         x=x1
      endif else begin
         z=[z,z1]
         x=[x,x1]
      endelse
   endfor
   if ff eq 0 then y=y1 else y=[y,y1]
   if ff eq 0 then zz=z else zz=[[zz],[z]]
endfor

;Fix the frequencies
if nff gt 1 then elimwrongchannels, zz, x,y

time=x
frequency=y
tmp=anytim(x[0],out_style='YY/MM/DD')
tt=strsplit(tmp,'/,',/extract)
data_date=tt[0]+tt[1]+tt[2]



;Do a constant background subtraction on the intensities
zb=constbacksub(zz, /auto)


;Select the frequency ranges
if keyword_set(freqrange) then freqrng=freqrange*1.0 else $
   freqrng = [frequency[n_elements(frequency)-1],frequency[0]]*1.0
frng=[min(where(freqrng[1]-frequency gt 0.0)),min(where(freqrng[0]-frequency gt 0.0))]

;Select the time ranges
if keyword_set(timerange) then $
   timrng=[max(where(anytim(timerange[0])-time gt 0.0)),max(where(anytim(timerange[1])-time gt 0.0))] $
else $
   timrng=[0,n_elements(time)-1]


window,0,xsize=900,ysize=600; for XWIN only!
spectro_plot, zb, time,frequency ,/xs, /ys, $
xrange = timerange, $
yrange = [freqrng[0],freqrng[1]], ytitle = 'Frequency [MHz]', $
title='Callisto spectrum '+station,charsize=2.4,charthick=3
image=tvrd(/true)
write_png,'radio_spectrum.png',image


;======================================
;Part 2. Find and fit the spectrum maxima


;Get the maxima. 
;the output array allmaxima[num(maxima),num(timerange)] contains the frequencies
jmap_find_maxima,zb,time,frequency,xrange=anytim(timerange),yrange=freqrng,$
                 allmaxima=allmaxima,mymaxima=mymaxima,nmax=nmax,$
                 numplotmax=numplotmax,/flipyaxes

wset,0
colors=[255,190,250,60,30]

;+========================================================
;Plot the maxima
spectro_plot, zb, time, frequency ,/xs, /ys, $
xrange = timerange, $
yrange = [freqrng[0],freqrng[1]], ytitle = 'Frequency [MHz]', $
title='Callisto spectrum '+station,charsize=2,charthick=3
loadct,39,/silent


for timind=timrng[0], timrng[1] do begin
   tmp=where(allmaxima[*,timind-timrng[0]].ind eq 0)
   nmax=min(tmp)
   
   for mm=0,nmax-1 do begin
      if mm eq numplotmax then break
      plots,time[timind],frequency[allmaxima[mm,timind-timrng[0]].ind],$
            color=colors[mm],psym=1,symsize=1,thick=4
      allmaxima[mm,timind-timrng[0]].val=frequency[allmaxima[mm,timind-timrng[0]].ind]
   endfor
endfor

;Maxima legend overplotting
for mm=0,numplotmax-1 do begin
   if mm le 1 then polyfill,[0.933,0.97,0.97,0.933],[0.785,0.785,0.815,0.815]-mm*0.05,/norm,color=0
   plots,0.94,0.8-mm*0.05,psym=1,symsize=1,thick=4,/norm,color=colors[mm]
   xyouts,0.941,0.79-mm*0.05,' #'+strtrim(string(mm+1),2),color=colors[mm],/norm,charsize=1.6
endfor
;-========================================================




;+========================================================
;Plot the smart maxima too
smartmax=0
if smartmax gt 0 then begin
   window,2,xsize=900,ysize=600 ; for XWIN only!
   loadct,9,/silent
   spectro_plot, zb, time,y ,/xs, /ys, $
                 xrange = timerange, $
                 yrange = [freqrng[0],freqrng[1]], ytitle = 'Frequency [MHz]', $
                 title='Callisto spectrum '+station,charsize=2,charthick=3
   loadct,39,/silent
   for timind=timrng[0], timrng[1] do begin
      tmp=where(mymaxima[*,timind-timrng[0]].ind eq 0)
      nmax=min(tmp)
      
      for mm=0,nmax-1 do begin
         if mm eq numplotmax then break
         plots,time[timind],frequency[mymaxima[mm,timind-timrng[0]].ind],$
               color=colors[mm],psym=1,symsize=1,thick=4
      endfor
   endfor
   
   for mm=0,numplotmax-1 do begin
      if mm le 1 then polyfill,[0.933,0.97,0.97,0.933],[0.785,0.785,0.815,0.815]-mm*0.05,/norm,color=0
      plots,0.94,0.8-mm*0.05,psym=1,symsize=1,thick=4,/norm,color=colors[mm]
      xyouts,0.941,0.79-mm*0.05,' #'+strtrim(string(mm+1),2),color=colors[mm],/norm,charsize=1.6
   endfor
   
   
   image=tvrd(/true)
   write_png,'emission_maxima.png',image
endif

;-=========================================================



if keyword_set(fit) then begin
;+========================================================
;Fit the maxima positions for determination of kinematics.
;=========================================================
  dt=time[1]-time[0]

  uinput=0.0
  while uinput le 0.0 or uinput gt numplotmax do $
     read,uinput,prompt='Which set of maxima would you like to fit? (1-'+strtrim(string(numplotmax),2)+')'
  uinput--
  
  
  print,''
  print,'Select starting point:'
  cursor,x,y,/down,/data
  plots,x,y,psym=5,symsize=2,thick=2,color=100
  sp=min(where(fix(time-x) gt 1.0e-6))-1
  print,sp,anytim(time[sp],out_style='STIME'),y
  
  
  print,'Select ending point:'
  cursor,x,y,/down,/data
  plots,x,y,psym=5,symsize=2,thick=2,color=200
  ep=min(where(fix(time-x) gt 1.0e-6))-1
  print,ep,anytim(time[ep],out_style='STIME'),y

;record the image
  image=tvrd(/true)
  savname=data_date+'_'+'emission_maxima_all.png'
  if keyword_set(outpath) then savname=outpath+savname
  write_png,savname,image
  

  
;Search for the edges of the wave
  tmp={val:0.0D,ind:0L}
  wave_frontedge=replicate(tmp,ep-sp+1);dblarr(ep-sp+1)
  wave_backedge=replicate(tmp,ep-sp+1)
  ;wave_backedge=dblarr(ep-sp+1)
  for ii=sp,ep do begin

;Find the front edge of the wave
     y=reform(zb[ii,allmaxima[uinput,ii-timrng[0]].ind:*])
     tmp=min(where(y le widthcutoff*max(y)))
     wave_frontedge[ii-sp].val=frequency[allmaxima[uinput,ii-timrng[0]].ind+tmp]
     wave_frontedge[ii-sp].ind=allmaxima[uinput,ii-timrng[0]].ind+tmp
     ;plots,time[ii]+dt/2.0,frequency[tmp],$
     ;      color=colors[uinput],psym=4,symsize=1,thick=1
     ;print,wave_frontedge[ii-sp].val 
     
;Find the back edge of the wave
     y=reform(zb[ii+sp,0:allmaxima[uinput,ii-timrng[0]].ind])
     tmp=max(where(y le widthcutoff*max(y)))
     ;stop
     wave_backedge[ii-sp].val=frequency[tmp]
     wave_backedge[ii-sp].ind=tmp


     if keyword_set(refine) and ii gt sp then begin

        if frequency[allmaxima[uinput,ii-timrng[0]].ind] lt wave_backedge[ii-sp-1].val then begin
          ;Refine the positions of the maxima
           print,''
           print,'Refining the maximum...'
           
           back=wave_backedge[ii-sp-1].ind
           front=wave_frontedge[ii-sp-1].ind
           
           jmap_return_maxima,zb[ii,back:front],frequency[back:front],$
                            allmaxima=allmaxima,numplotmax=numplotmax
           
           stop
 
           
           
           tmp=frequency[allmaxima[0:nmax[ii-sp]-1,ii-timrng[0]].ind] 
           res=where(tmp ge wave_backedge[ii-sp-1].val and tmp le wave_frontedge[ii-sp-1].val)
           if res[0] eq -1 then begin
              cnt=0
              print,'res[0]=-1'
              while res[0] eq -1 do begin
                 res=where(tmp ge wave_backedge[ii-sp-1].val-cnt*2.0 and tmp le wave_frontedge[ii-sp-1].val+cnt*2.0)
                 cnt++
              endwhile
              
           endif else begin
              tmp=frequency[allmaxima[res,ii-timrng[0]].ind]
              maxy=max(tmp,ind)
              maxind=allmaxima[res[ind],ii-timrng[0]].ind
              allmaxima[uinput,ii-timrng[0]].ind=maxind
              allmaxima[uinput,ii-timrng[0]].val=maxy
           endelse
        endif
     endif
     
     
     
     oplot,[time[ii]+dt/2.0,time[ii]+dt/2.0],[wave_backedge[ii-sp].val,wave_frontedge[ii-sp].val],$
           color=colors[uinput],thick=1
  endfor
endif
;-==============================================


stop
end
;-====================================================================
