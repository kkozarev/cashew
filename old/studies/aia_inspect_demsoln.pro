pro dem_avg,dt1,dt2,dmean_t1,dmean_t2
;average all DEM solutions over all pixels for each region before and
;after the wave shows up...
xsize=40
ysize=xsize


for xx=0,xsize-1 do begin
   for yy=0,ysize-1 do begin
      DEM_T1 = *dt1[xx,yy]
      DEM_T2 = *dt2[xx,yy]
      if xx eq 0 and yy eq 0 then begin
         dmean_t1 = avg(DEM_T1, 1)
         dmean_t2 = avg(DEM_T2, 1)
      endif else begin
         dmean_t1 += avg(DEM_T1, 1)
         dmean_t2 += avg(DEM_T2, 1)
      endelse
   endfor
endfor
dmean_t1/=(xsize*ysize)
dmean_t2/=(xsize*ysize)

end


pro dem_avg_good,dt1,dt2,mt1,mt2,dmean_t1,dmean_t2
;average all GOOD DEM solutions over all pixels for each region before and
;after the wave shows up. GOOD DEM solution is one with a zero chi-squared.
;mt1 and mt2 are the masks which tell us where the solution is zero
;chi-squared.

;xsize=40
;ysize=xsize


ssg1=where(mt1 EQ 1)
ng1=n_elements(ssg1)
ssg2=where(mt2 EQ 1)
ng2=n_elements(ssg2)

;First time
for p=0,ng1-1 do begin
   DEM_T1=*dt1[ssg1[p]]
   if p eq 0 then begin
      dmean_t1 = avg(DEM_T1,1)
   endif else begin
      dmean_t1 += avg(DEM_T1,1)
   endelse
endfor

;Second time
for p=0,ng2-1 do begin
   DEM_T2=*dt2[ssg2[p]]
   if p eq 0 then begin
      dmean_t2 = avg(DEM_T2,1)
   endif else begin
      dmean_t2 += avg(DEM_T2,1)
   endelse
endfor

dmean_t1/=(ng1*1.0)
dmean_t2/=(ng2*1.0)

end


pro aia_inspect_demsoln
;Inspect the results from the DEM analysis done by Mark Weber for the
;06/13/2010 event

!P.charsize=2
!P.charthick=2
!P.font=1
!P.position=[0.14,0.16,0.95,0.9]
lthick=6
athick=6

inpath='/home/kkozarev/Desktop/AIA/limbCMEs/06132010/DEM/'
soln='DEMsolns_regions_06132010.sav'
regs='demData_regions_06132010.sav'
restore,inpath+soln
restore,inpath+regs

;wdef, 4, 800, 600
set_plot,'ps'
!P.font=0
!P.color=0
!P.background=255
loadct,6,/silent

;---------------------------REG1-----------------------------
;dem_avg,DEM1_T1,DEM1_T2,dmean_t1,dmean_t2
fname='avg_demsoln_reg1'
device,file=inpath+fname+'.eps',/inches,xsize=9.0,ysize=9,$
  /encaps,/color,/helvetica
dem_avg_good,DEM1_T1,DEM1_T2,MASK1_T1,MASK1_T2,dmean_t1,dmean_t2
plot, Tgrid, dmean_t1, psym=10, thick=lthick, /ytype, yr=[1e16,1e21],$
      xtitle='logT',ytitle='DEM',title='Mean DEM - REG1',xthick=athick,ythick=athick
oplot, Tgrid, dmean_t2, psym=10, thick=lthick,linesty=2

;integrate the DEM to get the total emission measure.
res1=int_tabulated(10^tgrid,dmean_t1,/double)
res2=int_tabulated(10^tgrid,dmean_t2,/double)
print,''
;The ratio of EM is the same as the ratio of densities
print,'Reg1: n2/n1 = '+strtrim(string(sqrt(res2/res1)),2)
device,/close
exec='convert -flatten '+inpath+fname+'.eps'+' '+inpath+fname+'.png'
spawn,exec
;write_png,inpath+'avg_demsoln_reg1.png',tvrd()
;stop


;---------------------------REG2-----------------------------
;dem_avg,DEM2_T1,DEM2_T2,dmean_t1,dmean_t2
fname='avg_demsoln_reg2'
device,file=inpath+fname+'.eps',/inches,xsize=9.0,ysize=9,$
  /encaps,/color,/helvetica
dem_avg_good,DEM2_T1,DEM2_T2,MASK2_T1,MASK2_T2,dmean_t1,dmean_t2
plot, Tgrid, dmean_t1, psym=10, thick=lthick, /ytype, yr=[1e16,1e21],$
      xtitle='logT',ytitle='DEM',title='Mean DEM - REG2',xthick=athick,ythick=athick
oplot, Tgrid, dmean_t2, psym=10, thick=lthick,linesty=2

;integrate the DEM to get the total emission measure.
res1=int_tabulated(10^tgrid,dmean_t1,/double)
res2=int_tabulated(10^tgrid,dmean_t2,/double)
print,''
print,'Reg2: n2/n1 = '+strtrim(string(sqrt(res2/res1)),2)
;write_png,inpath+'avg_demsoln_reg2.png',tvrd()
device,/close
exec='convert -flatten '+inpath+fname+'.eps'+' '+inpath+fname+'.png'
spawn,exec
;stop

;---------------------------REG3-----------------------------
fname='avg_demsoln_reg3'
device,file=inpath+fname+'.eps',/inches,xsize=9.0,ysize=9.0,$
  /encaps,/color,/helvetica
dem_avg_good,DEM3_T1,DEM3_T2,MASK3_T1,MASK3_T2,dmean_t1,dmean_t2
plot, Tgrid, dmean_t1, psym=10, thick=lthick, /ytype, yr=[1e16,1e21],$
      xtitle='logT',ytitle='DEM',title='Mean DEM - REG3',xthick=athick,ythick=athick
oplot, Tgrid, dmean_t2, psym=10, thick=lthick,linesty=2

;integrate the DEM to get the total emission measure.
res1=int_tabulated(10^tgrid,dmean_t1,/double)
res2=int_tabulated(10^tgrid,dmean_t2,/double)
print,''
print,'Reg3: n2/n1 = '+strtrim(string(sqrt(res2/res1)),2)
;write_png,inpath+'avg_demsoln_reg3.png',tvrd()
;stop
device,/close
exec='convert -flatten '+inpath+fname+'.eps'+' '+inpath+fname+'.png'
spawn,exec

;---------------------------REG4-----------------------------
fname='avg_demsoln_reg4'
device,file=inpath+fname+'.eps',/inches,xsize=9.0,ysize=9.0,$
  /encaps,/color,/helvetica
dem_avg_good,DEM4_T1,DEM4_T2,MASK4_T1,MASK4_T2,dmean_t1,dmean_t2
plot, Tgrid, dmean_t1, psym=10, thick=lthick, /ytype, yr=[1e16,1e21],$
      xtitle='logT',ytitle='DEM',title='Mean DEM - REG4',xthick=athick,ythick=athick
oplot, Tgrid, dmean_t2, psym=10, thick=lthick,linesty=2

;integrate the DEM to get the total emission measure.
res1=int_tabulated(10^tgrid,dmean_t1,/double)
res2=int_tabulated(10^tgrid,dmean_t2,/double)
print,''
print,'Reg4: n2/n1 = '+strtrim(string(sqrt(res2/res1)),2)
;write_png,inpath+'avg_demsoln_reg4.png',tvrd()
;stop
device,/close
exec='convert -flatten '+inpath+fname+'.eps'+' '+inpath+fname+'.png'
spawn,exec

;---------------------------REG5-----------------------------
fname='avg_demsoln_reg5'
device,file=inpath+fname+'.eps',/inches,xsize=9.0,ysize=9.0,$
  /encaps,/color,/helvetica
dem_avg_good,DEM5_T1,DEM5_T2,MASK5_T1,MASK5_T2,dmean_t1,dmean_t2
plot, Tgrid, dmean_t1, psym=10, thick=lthick, /ytype, yr=[1e16,1e21],$
      xtitle='logT',ytitle='DEM',title='Mean DEM - REG5',xthick=athick,ythick=athick
oplot, Tgrid, dmean_t2, psym=10, thick=lthick,linesty=2

;integrate the DEM to get the total emission measure.
res1=int_tabulated(10^tgrid,dmean_t1,/double)
res2=int_tabulated(10^tgrid,dmean_t2,/double)
print,''
print,'Reg5: n2/n1 = '+strtrim(string(sqrt(res2/res1)),2)
;write_png,inpath+'avg_demsoln_reg5.png',tvrd()
;stop
device,/close
exec='convert -flatten '+inpath+fname+'.eps'+' '+inpath+fname+'.png'
spawn,exec

set_plot,'x'
end
