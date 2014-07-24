;+
;         
; NAME: MAKE_TEST_DATA.PRO
;          
; USAGE: test_image = make_test_data()
;          
; PURPOSE: Generates a 2D fltarr containing Gaussian intensity 
;     variations, meant to crudely mimic magnetic features in 
;     magnetograms, as test data for feature identification methods. 
;
;     By default, images contain both intensity enhancements and 
;     deficits; but images that contain only enhancements can also
;     be generated, by setting /UNIPOLAR.
;
;     Some distinguishable intensity features will be contiguous 
;     with other features, for testing gradient-based identification 
;     methods.  Features' separations can be increased by choosing 
;     larger NX & NY values.
;                   
; OUTPUT: IMG = 2D fltarr w/ intensity variations to mimic features
;
; REQUIRED ARGUMENTS: None.
;                   
; OPTIONAL ARGUMENTS/KEYWORDS:
;   NFEAT = # of Gaussian intensity features in IMG; DEFAULT = 10       
;   NX = # of horizontal pixels; DEFAULT = 12*NFEAT
;   NY = # of vertical pixels; DEFAULT = 8*NFEAT
;   UNIPOLAR = set this keyword to create test data containing only
;      intensity enhancements (no negative image values) 
;
; SIDE EFFECTS/ROUTINES CALLED: Causes floating underflow error.  
;   Calls CHECK_MATH.PRO to clear.
;            
; HISTORY: BTW, 21 May 2008: Written.
;                   
;-
function make_test_data, nfeat=nfeat, nx=nx, ny=ny, $
                         unipolar=unipolar

if not(keyword_set(nfeat)) then nfeat = 10

if not(keyword_set(nx)) then nx = 12*(nfeat + 1)
if not(keyword_set(ny)) then ny =  8*(nfeat + 1)

npix = float(nx)*ny
aspect_ratio = float(nx)/ny     ; Ratio of X to Y

nfeat_x = ceil(sqrt(nfeat*aspect_ratio))
nfeat_y = ceil(sqrt(nfeat/aspect_ratio)) 

dx = floor(float(nx)/nfeat_x)
dy = floor(float(ny)/nfeat_y)

xc = floor((findgen(nfeat_x)+0.5)*dx)#replicate(1.,nfeat_y)
yc = replicate(1.,nfeat_x)#floor((findgen(nfeat_y)+0.5)*dy)

max_rad = max([ceil(0.5*min([dx,dy])),3])
min_rad = 2.
delta_r = max_rad - min_rad

if keyword_set(unipolar) then begin
    radii = min_rad + delta_r*(findgen(nfeat)+1.)/nfeat 
    amplitudes = alog(findgen(nfeat) + 1) + 1
endif else begin
    half_nfeat = ceil(0.5*nfeat)
    radii = min_rad + delta_r*(findgen(half_nfeat)+1.)/half_nfeat 
    radii = [radii,reverse(radii)]
    amplitudes = alog(findgen(half_nfeat) + 1) + 1
    amplitudes = [amplitudes,reverse(-amplitudes)]
endelse

img = fltarr(nx,ny)

x2d = findgen(nx)#replicate(1,ny)
y2d = replicate(1,nx)#findgen(ny)

for i = 0,nfeat-1 do begin
    
    exp_arg = -((xc(i) - x2d)^2 + (yc(i) - y2d)^2)/(radii(i)^2)
    
    if (not(keyword_set(unipolar)) and (i gt floor(0.5*nfeat)-1)) then $
      coeff = -1. else coeff = 1.
    
    img = img + alog(radii(i))*coeff*exp(exp_arg)
    
endfor

error_trap = check_math()
if ((error_trap)mod(32) ne 0) then begin ; It's *not* a floating underflow...
    message,'Non-trivial math error found.',/info 
    stop                        ; ... so better halt & investigate! 
endif

return, img

end


