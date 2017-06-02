pro test_aia_aschdem_plot_em_ratios
;Test procedure for aia_cfa_teem_plot_em_ratios
event=load_events_info(label='paper')
aia_aschdem_plot_em_ratios,event

end


pro aia_aschdem_plot_em_ratios,event
;Find the files first
  path=event.aschdempath
  fileset=file_basename(file_search(path+'aschdem_'+event.date+'_'+event.label+'*teem_tot.sav'))
  nfiles=n_elements(fileset)
  for ff=0,nfiles-1 do begin
     res=strsplit(fileset[ff],'_',/extract)
     if n_elements(res) eq 6 then dateobs=res[3] else dateobs=res[4]
     
     infname=path+fileset[ff] ;'aschdem_'+event.date+'_'+event.label+'_'+dateobs+'_teem_map.sav'
     outfname=path+'aschdem_'+event.date+'_'+event.label+'_'+dateobs+'_teem_em_ratios.png'
     basefname=path+fileset[0:4]
     aia_aschdem_plot_em_ratios_main,infname,outfname,basefname,dateobs
  endfor

end



pro aia_aschdem_plot_em_ratios_main,infname,outfname,basefname,dateobs
;+
; Project     : AIA/SDO
;
; Name        : AIA_ASCHDEM_PLOT_EM_RATIOS 
;
; Category    : Display EM base ratios
;		previously calculted with AIA_CFA_TEEM_MAP.PRO
;
; Explanation : plots EM base ratios. Based on aia_cfa_teem_disp.pro
;
; Syntax      : IDL>aia_aschdem_em_ratios,event
;
; Inputs      : fileset  = initial part of filename
;		te_range(2) = min and max of valid DEM temperature range [K]
;		dateobs  = date and time of image
;               teem_fname = the filename to save the output in.
;
; Outputs     : png-file
;
; History     :  29-Jun-2014 - Kamen Kozarev - based on aia_cfa_teem_disp.pro
;
; Contact     : kkozarev@cfa.harvard.edu
;-

;________________________DISPLAY EM BASE RATIOS________________________
set_plot,'z'

nbase=n_elements(basefname)
for bb=0,nbase-1 do begin
   restore,basefname[bb]
   bmap=10^emlog
   if bb eq 0 then begin
      te_range=minmax(10^telog) ;   ([K], valid temperature range for DEM solutions)
      basemap=bmap
   endif else begin
      basemap+=bmap
   endelse
endfor
basemap/=(1.0D*nbase)


restore,infname
ind	=where(emlog ne 0)
;statistic,te_map(ind),te_avg,te_sig
statistic,emlog(ind),em_avg,em_sig
nsig	=3
em1	=em_avg-nsig*em_sig
em2	=em_avg+nsig*em_sig
;te1	=alog10(te_range(0))
;te2	=alog10(te_range(1))
dim	=size(emlog)
nx	=dim(1)
ny	=dim(2)
inmap=10^emlog

;rebin small images
;nx	=nx_
;ny	=ny_
;zoom	=1
;if (nx_ lt 512) then zoom=long(512/nx_+0.5)
;if (zoom ge 2) then begin
; nx	=nx_*zoom
; ny	=ny_*zoom
; em_map =rebin(em_map,nx,ny)
; te_map =rebin(te_map,nx,ny)
;endif

;temperature scale
;i3	=long(nx*0.95)
;for j=0,ny-1 do te_map(i3:nx-1,j)=te1+(te2-te1)*float(j)/float(ny-1)

;display
;window,0,xsize=nx,ysize=ny
device, set_resolution=[nx*1.2,ny], SET_PIXEL_DEPTH=24, DECOMPOSED=0

!p.font=0
tvlct,rr,gg,bb,/get
loadct,0,/silent
!p.position=[0,0,1,1]
!x.range=[0,2.*nx]
!y.range=[0,ny]
!x.style=1
!y.style=1
!p.background=255
plot,[0,0],[0,0],xticks=1,yticks=1,xminor=1,yminor=1
ct=13
rmin= 0.9;min(inmap/(basemap*1.0))
rmax= 1.4;max(inmap/(basemap*1.0))
loadct,ct,/silent
tv,bytscl(sqrt(inmap/(1.*basemap)),min=rmin,max=rmax);,0,0  ;
;loadct,5
;tv,bytscl(te_map,min=te1,max=te2),nx,0
;t1str	=string(te1,'(f3.1)')
;t2str	=string(te2,'(f3.1)')
;t3str	='6.0' &q3=(6.0-te1)/(te2-te1)
;t4str	='6.5' &q4=(6.5-te1)/(te2-te1)
;xyouts,1.95*nx,0.01*ny,t1str,size=2,color=255
;xyouts,1.95*nx,0.97*ny,t2str,size=2,color=0
;xyouts,1.95*nx,  q3*ny,t3str,size=2,color=255
;xyouts,1.95*nx,  q4*ny,t4str,size=2,color=255
;loadct,0,/silent
;xyouts,0.85,0.98,'EM BASE RATIO',charsize=1.2,color=255,charthick=1,/norm
;xyouts,0.85,0.96,dateobs,charsize=1.2,color=255,charthick=1,/norm
;loadct,ct,/silent
fcolorbar, MIN=rmin,MAX=rmax,Divisions=8, $
           Color=0,VERTICAL=1,RIGHT=1, TITLE='EM BASE RATIO  '+dateobs,$
           CHARSIZE=2,charthick=2,format='(f4.2)',Position=[0.905, 0.03, 0.94, 0.97]

;PNG file
image_tv=tvrd(true=1)
write_png,outfname,image_tv,rr,gg,bb
print,'file written : ',outfname

loadct,0,/silent
set_plot,'x'
;stop
end
