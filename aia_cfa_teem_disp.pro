pro aia_cfa_teem_disp,teem_fname,te_range,dateobs

;+
; Project     : AIA/SDO
;
; Name        : AIA_TEEM_DISP  
;
; Category    : Display of EM and Te map
;		previously calculted with AIA_TEEM_MAP.PRO
;
; Explanation : plots TE and EM maps
;
; Syntax      : IDL>aia_teem_disp,teem_fname,te_range,dateobs
;
; Inputs      : fileset  = initial part of filename 
;               wave_	 = strarr(6) with wavelengths in Angstroem
;		te_range(2) = min and max of valid DEM temperature range [K]
;		dateobs  = date and time of image
;               teem_fname = the filename to save the output in.
;
; Outputs     : png-file
;
; History     :  3-Mar-2011, Version 1 written by Markus J. Aschwanden
;                20-Mar-2012, Modified version for use at CfA by
;                Kamen A. Kozarev
;
; Contact     : aschwanden@lmsal.com
;-

;________________________DISPLAY TE+EM MAP________________________
teem_map=teem_fname+'.sav'
restore,teem_map
ind	=where(em_map ne 0)
statistic,te_map(ind),te_avg,te_sig
statistic,em_map(ind),em_avg,em_sig
nsig	=3
em1	=em_avg-nsig*em_sig
em2	=em_avg+nsig*em_sig
te1	=alog10(te_range(0))
te2	=alog10(te_range(1))
dim	=size(em_map)
nx_	=dim(1)
ny_	=dim(2)

;rebin small images
nx	=nx_
ny	=ny_
zoom	=1
if (nx_ lt 512) then zoom=long(512/nx_+0.5)
if (zoom ge 2) then begin
 nx	=nx_*zoom
 ny	=ny_*zoom
 em_map =rebin(em_map,nx,ny)
 te_map =rebin(te_map,nx,ny)
endif
 
;temperature scale
i3	=long(nx*0.95)
for j=0,ny-1 do te_map(i3:nx-1,j)=te1+(te2-te1)*float(j)/float(ny-1)

;display
window,0,xsize=nx*2,ysize=ny
tvlct,rr,gg,bb,/get
loadct,3
!p.position=[0,0,1,1]
!x.range=[0,2.*nx]
!y.range=[0,ny]
!x.style=1
!y.style=1
plot,[0,0],[0,0]
tv,bytscl(em_map,min=em1,max=em2),0,0
loadct,5
tv,bytscl(te_map,min=te1,max=te2),nx,0
t1str	=string(te1,'(f3.1)')
t2str	=string(te2,'(f3.1)')
t3str	='6.0' &q3=(6.0-te1)/(te2-te1)
t4str	='6.5' &q4=(6.5-te1)/(te2-te1)
xyouts,1.95*nx,0.01*ny,t1str,size=2,color=255
xyouts,1.95*nx,0.97*ny,t2str,size=2,color=0
xyouts,1.95*nx,  q3*ny,t3str,size=2,color=255
xyouts,1.95*nx,  q4*ny,t4str,size=2,color=255
xyouts,1.05*nx,0.95*ny,'log(T)',size=4,color=255
xyouts,0.05*nx,0.95*ny,'log(EM)',size=4,color=255
xyouts,0.01*nx,0.01*ny,dateobs,size=2,color=255

;PNG file
image_tv=tvrd(true=1)
write_png,teem_fname+'.png',image_tv,rr,gg,bb
print,'file written : ',teem_fname+'.png'

end
