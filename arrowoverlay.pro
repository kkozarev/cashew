pro arrowoverlay,vmaps
;A procedure, which overlays arrows on the results from the change
;detection algorithm, showing the spatial velocity of the change.


v=reform(vmaps[0,*,*,*])
dx=reform(vmaps[1,*,*,*])
dy=reform(vmaps[2,*,*,*])

nt=n_elements(vmaps[0,0,0,*])

;smoothing parameter
smt=20

loadct,6
wdef,0,1024
tvlct,rr,gg,bb,/get

for t=0,nt-1 do begin
;Smooth the current frame
s=reform(smooth(v[*,*,t],smt,/edge_truncate))
sx=reform(smooth(dx[*,*,t],smt,/edge_truncate))
sy=reform(smooth(dy[*,*,t],smt,/edge_truncate))
sm=sqrt(sx^2+sy^2)
sm=sm[smt:1023,smt:1023]

tvscl,s

;to plot over the image, at a pixel [x0,y0], normalize the
for x0=20,1023,smt do begin
	for y0=20,1023,smt do begin
		arrow,x0,y0,x0+sx[x0,y0]*100/max(sm),y0+sy[x0,y0]*100/max(sm),hsize=4,hthick=2,thick=2,color=255
	endfor
endfor

;for x0=0,1023,10 do for y0=0,1023,10 do arrow,x0,y0,x0+sx[x0,y0]*8.0e9,y0+sy[x0,y0]*8.0e9,hsize=6,hthick=4,thick=4

;where 1.0e9 is a random multiplicative constant to make the arrows a pleasing length.

;This is not perfect - it would be good to smooth and show the larger picture...

write_png,'smooth_arrows_'+strtrim(string(t+1000),2)+'.png',tvrd(),rr,gg,bb
;stop
endfor

stop

end
