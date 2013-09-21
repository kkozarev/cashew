pro test_square_contour
;Test a plotting technique which is like a filled contour, but with colorful squares instead.
;Kamen Kozarev 11/2011

nx=20
ny=20
expf=30

;This will eventually be the data to plot.
nc=randomn(10,nx,ny)

loadct,13,/silent
wdef,0,nx*expf,ny*expf

symz=expf*nx/(100)
;+0.5*symz
for i=0,nx-1 do $
    for j=0,ny-1 do plots,(i*expf),(j*expf),$
    psym=sym(5),$
    symsize=symz,$
    color=nc[i,j]*255,$
    /device

end
