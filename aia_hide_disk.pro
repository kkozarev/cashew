function aia_hide_disk, index, data, value=value
;This function calls the make_circ procedure in order to set all the pixel values on the AIA solar disk to some value (default is zero).
;External calls:
;                      make_circ.pro           
;Kamen Kozarev, 07/20/2011

if keyword_set(value) then pixval=value else pixval=0

nx=n_elements(data[*,0,0])
ny=n_elements(data[0,*,0])
nt=n_elements(index)
newdata=fltarr(nx,ny,nt)

for t=0,nt-1 do newdata[*,*,t]=make_circ(data[*,*,t],index[0].R_sun+9,cent=[index[0].x0_mp,index[0].y0_mp],val=pixval)

return, newdata
end
