pro concentric_pixel_search, index, inmask, infeature, label
; Determine the points, to which to fit the circle. The idea is to make
; this iteratively, until the farthest N pixels remain...
;Loop over some number of steps, increment by 10 pixels or so.
;Search from the outside, start from the solar radius in pixels, and
;reduce the radius, checking every shell between consecutive circles
;whether there are a minimum of N pixels there. The first shell where
;there are, take the first N pixels you can find, and fit a circle,
;then the next N pixels, fit a circle, etc. Then, compute a
;chi-squared for every fit and find the best one...

index=reform(index[0])

nx=index.naxis1
ny=index.naxis1

x0=index.X0_MP
y0=index.y0_MP
Rs=index.rsun_obs/index.imscl_mp

rstep=0.01 * Rs; The search step/shell thickness, in Rsun
rout=1.2 * Rs ;The initial outer circle radius.
rin=rout-rstep

while rin ge Rs do begin

;Create the concentric circles
res1=make_circ(fltarr(nx,ny),rout,center=[x0,y0],val=100)
res2=make_circ(fltarr(nx,ny),rin,center=[x0,y0],val=200)
res=res1+res2
shell=where(res eq 100)
tv,res
tvmask,inmask,label
;stop

;get the feature pixel addresses - should be 1D.
;This assumes that the feature has been isolated using yaftawave_extract_features!
featurepos=long(strsplit(infeature.mask_str,/extract))

print,rin/Rs

ind=0
for jj=0,n_elements(featurepos)-1 do begin
   in=where(shell eq featurepos[jj])
   if in ne -1 then ind=[ind,in]
   if n_elements(ind) ge 401 then begin
      ind=ind[1:*]
      goto,d100
   endif
endfor

rout-=rstep
rin=rout-rstep
endwhile

d100:
spos=array_indices(fltarr(nx,ny),shell[ind])
loadct,6
plots,reform(spos[0,*]),reform(spos[1,*]),/device,psym=1,color=40

stop

aia_autofit_circle,reform(spos[0,*]),reform(spos[1,*]),circpos,pp
stop

end
