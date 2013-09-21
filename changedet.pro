pro changedet, data,ht,hx,alldata
;This program will calculate the change in time and space of an AIA
;datacube. In particular, we want to determine the change of the EUV
;wave. The data must be in the format fltarr(nx,ny,nt).
;The procedure implements formulas from the paper
;'A Framework for Diagnosing Changes in Evolving Data Streams' by
;Charu Aggarwal
;
;01/2011 Kamen Kozarev


;TEST
;Make up some test data cube
nx=100
ny=100
nt=20
data=fltarr(nx,ny,nt)
for t=0,nt-1 do for i=0,nx-1 do for j=0,ny-1 do $
  data[i,j,t]=cos((i-5*t)/10.0)*cos(j/10.0)
;the cosine 2D pattern moves 5 pixels to the right every frame.
;TEST


nx=n_elements(data[*,0,0])
ny=n_elements(data[0,*,0])
nt=n_elements(data[0,0,*])
print,nx,ny,nt



;Definitions and constants
hy=hx

;The forward time slice density function
F=fltarr(nx,ny,nt)
;The backward time slice density function
R=fltarr(nx,ny,nt)
;The velocity density
V=fltarr(nx,ny,nt)

d={dx:0.0D,dy:0.0D}
dv=replicate(d,nx,ny,nt)

alldata=fltarr(3,nx,ny,nt)


;Start the loops

;The time loop
for t=ht-1,nt-ht-1 do begin
        
;The X-loopp
   for i=0,nx-1 do begin
      
;The Y-loop
      for j=0,ny-1 do begin
      ;forward - convolve data[i,j,t:t+ht-1] with the kernel
      ;reverse - convolve data[i,j,t-ht-1:t] with the kernel
      ;Think about edge effects...
         
;The forward density calculation
         for s=0,ht-1 do begin
            Kx=(1/sqrt(2*!PI)*hx)*exp(-(data[i,j,t]-data[i,j,t+s])^2/(2*hx^2))
            Ky=(1/sqrt(2*!PI)*hy)*exp(-(data[i,j,t]-data[i,j,t+s])^2/(2*hy^2))
            if s eq 0 then begin 
               F[i,j,t]=(1-(t-(t-s))/ht)*Kx*Ky
            endif else begin 
               F[i,j,t]+=(1-(t-(t-s))/ht)*Kx*Ky
            endelse
         endfor
                                ;F[i,j,t]*=Cf
         
;The backward density calculation
         for s=0,ht-1 do begin
            ;if t lt ht then 
            Kx=(1/sqrt(2*!PI)*hx)*exp(-(data[i,j,t]-data[i,j,t-s])^2/(2*hx^2))
            Ky=(1/sqrt(2*!PI)*hy)*exp(-(data[i,j,t]-data[i,j,t-s])^2/(2*hy^2))
            if s eq 0 then begin
               R[i,j,t]=(1-((t-s)-t)/ht)*Kx*Ky
            endif else begin
               R[i,j,t]+=(1-(t-(t-s))/ht)*Kx*Ky
            endelse
         endfor
                                ;R[i,j,t]*=Cr                        
      endfor
   endfor
                                ;The normalization
   F[*,*,t] /= total(F[*,*,t])
   R[*,*,t] /= total(R[*,*,t])
   V[*,*,t] = (F[*,*,t] - R[*,*,t])/ht
   
   for i=0,nx-2 do begin
      for j=0,ny-2 do begin
; Find the interpolated value along both dimensions.
         vx_int=0.01*v[i+1,j,t]+0.99*v[i,j,t]
         vy_int=0.01*v[i,j+1,t]+0.99*v[i,j,t]
         
;then compute the spatial velocity gradient according to the paper.
         dv[i,j,t].dx=(vx_int-v[i,j,t])/0.01
         dv[i,j,t].dy=(vy_int-v[i,j,t])/0.01
      endfor
   endfor
   
endfor 

;Perform a search for DATA COAGULATIONS: WHAT ARE THE CRITERIA?
;1. A SPATIAL SCALE
;2. A TEMPORAL SCALE
;3. A SIGNAL THRESHOLD
;4. CONNECTIVITY REQUIREMENTS BETWEEN NEIGHBORING PIXELS



alldata[0,*,*,*]=v[*,*,*]
alldata[1,*,*,*]=dv[*,*,*].dx
alldata[2,*,*,*]=dv[*,*,*].dy

stop


;save,filename='/home/kkozarev/Desktop/AIA/limbCMEs/changedet/smdata_changedet.sav',alldata



end
