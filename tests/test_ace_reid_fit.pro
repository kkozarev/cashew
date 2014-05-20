
FUNCTION MYFUNCT, X, P
; The independent variable is X
; Parameter values are passed in "P"

fit1= (p[1]/abs(X-p[7]))*exp(-(p[2]/abs(X-p[7]))-(abs(X-p[7])/p[3]))
fit1[where(X-p[7] le 0.0)]=p[0]
fit2= (p[4]/(abs(X-p[8])))*exp(-(p[5]/abs((X-p[8])))-(abs((X-p[8]))/p[6]))
fit2[where(X-p[8] le 0.0)]=p[0]

YMOD =p[0] + fit1 + fit2
;plot,X,YMOD,/ylog,yrange=[1.0e-5,3.0e-2]

;stop

return, YMOD
END

pro test_ace_reid_fit
;Load ACE SIS data, make a double Reid profile fit to the data for the
;two events of June 12 and June 13, 2010.
;Kamen Kozarev, 10/2010
set_plot,'z'
close,/all
loadct,39,/silent
tvlct,red,green,blue,/get
!P.font=-1

;datafile='/Volumes/Transcend/research/AIA_work/06132010/presentation/otherObservations/ACE_SIS_Data.txt'
;datafile='/home/kamen/temp/mpfittest/ACE_SIS_Data.txt'
path='./'
datafile=path+'ACE_SIS_Data.txt'
openr,lun,datafile,/get_lun


;Array containing geometric mean energy of He SIS channels, MeV/n
energy=[4.03215,5.39038,6.68489,8.41777,11.4932,15.6229,22.9592,34.7696]



ii=0
aa=''
while ~EOF(lun) do begin
readf,lun,aa
if aa eq 'BEGIN DATA' then ii=-1
ii=ii+1
endwhile
close,lun


ntimes=ii
nergs=8
data=dblarr(nergs+1,ii)
a=dblarr(nergs,9)
fitparams=dblarr(nergs,9)
paramerr=dblarr(nergs,9)

openr,lun,datafile,/get_lun
mm=0
while ~EOF(lun) do begin
if mm eq 1 then begin
   readf,lun,data
   break
endif
readf,lun,aa
if aa eq 'BEGIN DATA' then mm=1
endwhile
close,/all




dt=162. ;time offset that puts the beginning of the event at zero...

;
time=data[0,*]-dt
;ind=where(dx gt 0 and dx lt 6)
timind=where(time gt 0)
time=reform(time[timind])


;Determine the input parameters for the different energies:
    a[0,0]=1.0e-6
    a[0,1]=2.0e-1
    a[0,2]=1.6D
    a[0,3]=0.8D
    a[0,4]=3.0e-2
    a[0,5]=0.7D
    a[0,6]=0.45D

    a[1,0]=1.0e-6
    a[1,1]=2.0e-1
    a[1,2]=1.7D
    a[1,3]=0.6D
    a[1,4]=2.0e-2
    a[1,5]=0.7D
    a[1,6]=0.3D

    a[2,0]=1.0e-6
    a[2,1]=6.0e-2
    a[2,2]=1.4D
    a[2,3]=0.7D
    a[2,4]=8.0e-3
    a[2,5]=0.6D
    a[2,6]=0.4D

    a[3,0]=1.0e-6
    a[3,1]=3.2e-2
    a[3,2]=1.2D
    a[3,3]=0.8D
    a[3,4]=3.0e-2
    a[3,5]=1.4D
    a[3,6]=0.2D

    a[4,0]=1.0e-6
    a[4,1]=2.0e-2
    a[4,2]=1.0D
    a[4,3]=0.5D
    a[4,4]=1.0e-2
    a[4,5]=1.1D
    a[4,6]=0.2D

    a[5,0]=1.0e-6
    a[5,1]=1.1e-2
    a[5,2]=1.0D
    a[5,3]=0.45D
    a[5,4]=6.0e-3
    a[5,5]=1.0D
    a[5,6]=0.2D

    a[6,0]=1.0e-6
    a[6,1]=2.0e-2
    a[6,2]=1.2D
    a[6,3]=0.2D
    a[6,4]=2.5e-3
    a[6,5]=0.8D
    a[6,6]=0.2D

    a[7,0]=1.0e-7
    a[7,1]=9.0e-3
    a[7,2]=1.0D
    a[7,3]=0.2D
    a[7,4]=5.0e-3
    a[7,5]=1.4D
    a[7,6]=0.2D

    a[*,7]=0.85D            ;The time shifts for the first function,days
    a[*,8]=1.9D           ;The time shift for the second function,days
;============================================================================




;============================================================================
;Here attempt fitting of the reid profiles
;Want a functional form like
;f = a[0] + (a[1]/t) * exp(-(a[2]/t)-(t/a[3]) + (a[4]/t) * exp(-(a[5]/t)-(t/a[6]))
;============================================================================
for nrg=0,nergs-1 do begin
    ind=timind
    dx=time
    dy=reform(data[nrg+1,ind])
    smdy=smooth(dy,6,/edge_truncate)

    ;ain contains the start fitting parameters for this particular energy
    ain=reform(a[nrg,*])

;This value constrains the percentage range around the 
;given input parameters. MPFIT is constrained to find a fit
;for every parameter between value-value*fitrange and
;value+value*fitrange
    fitrange=0.1

;Input the params and ranges, set fixed values.
    parinfo = replicate({value:0.D, fixed:0, limited:[0,0], $
                     limits:[0.D,0]}, 9)
    for i=0,6 do begin
        parinfo[i].limits[0]=ain[i]-fitrange*ain[i]
        parinfo[i].limits[1]=ain[i]+fitrange*ain[i]
    endfor
    parinfo[7].fixed=1
    parinfo[8].fixed=1
    ind=[1,2,3,4,5,6]
    parinfo[ind].limited[*]=1
    parinfo[ind].value=ain[ind]
    
    parinfo[ind].limits[0]=ain[ind]-fitrange*ain[ind]
    parinfo[ind].limits[1]=ain[ind]+fitrange*ain[ind]
    
    p=mpfitfun('myfunct',dx,smdy,sqrt(smdy)+1.0e-8,ain,parinfo=parinfo)
    
;============================================================================


    
;============================================================================
;Plotting territory.
;============================================================================   

wdef,0,1024,1024
    yrange=[1.0e-5,3.0e-2]
;      yrange=yrange,$
    print,'Case #'+strtrim(string(nrg+1),2)
    plot,dx,smdy,$
         /ylog,$
         xthick=2,$
         ythick=2,$
         thick=3,$
         charsize=1.6,$
         charthick=1.8,$
         xtitle='Days relative to DOY '+strtrim(string(dt),2),$
         ytitle='Flux [MeV sr cm!U2!N s]!U-1!N',$
         title='ACE He flux at E='+strtrim(string(energy[nrg]),2)+' Mev/nuc',$
         color=0,background=255
    
;plotting the result
    fit1= (p[1]/abs(dX-p[7]))*exp(-(p[2]/abs(dX-p[7]))-(abs(dX-p[7])/p[3]))
    fit1[where(dX-p[7] le 0.0)]=a[nrg,0]
    fit2= (p[4]/(abs(dX-p[8])))*exp(-(p[5]/abs((dX-p[8])))-(abs((dX-p[8]))/p[6]))
    fit2[where(dX-p[8] le 0.0)]=a[nrg,0]
    
    oplot,dx,p[0]+fit1,color=0,thick=3
    oplot,dx,p[0]+fit2,color=0,thick=3
    oplot,dX,p[0] + fit1 + fit2,linestyle=2,color=230,thick=3

write_png,path+'ACE_He_fitted_E'+strtrim(string(nrg),2)+'.png',tvrd(),red,green,blue

;============================================================================


    
;============================================================================
;Here plot the spectra of the data as well of the fitted functions.





;============================================================================

    
    
;print, startparams[0:6]
    print, p[0:6]

fitparams[nrg,*]=p
;paramerr[nrg,*]=perror

stop

endfor

set_plot,'x'


end
