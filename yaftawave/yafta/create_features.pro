;+
;
; NAME: create_features.pro
;
; USAGE: create_features, img, mask, features, min_size=4, dx=7.25e+7
; 
; PURPOSE: From a data array and corresponding mask partitioned 
;          into features (by, e.g., RANKDOWN.PRO or IDL's 
;          LABEL_REGION.PRO with a wrapper routine), this routine 
;          creates a structure for each feature, and returns an
;          array of such structures.
;
; REQUIRED ARGUMENTS: 
;     IMG = 2-D data array, N_x by N_y 
;     MASK = (N_x, N_y) array partitioned into features by labels
;     FEATURES = name of array of features' structures to be returned.
;
; OPTIONAL ARGUMENTS/KEYWORDS:
;     VX = (N_x, N_y) array of x-comp of local correlation 
;          tracking (LCT) velocities  
;     VY = (N_x, N_y) array of y-comp of LCT vels
;     DX = pixel size in data units; DEFAULT: dx = 1
;     MIN_SIZE = min. # of pixels required for inclusion in
;          FEATURES; DEFAULT = 10
;     PEAKTHRESHOLD = min. peak field strength for inclusion in 
;          FEATURES; DEFAULT = 0 (then RANKDOWN's THRESHOLD is the
;          effective PEAKTHRESHOLD)
;     SILENT = set to suppress printing of messages
;
; SIDE EFFECTS/ROUTINES CALLED:
;     - Modifies MASK array: removes features with < MIN_SIZE pixels,
;       renames other features in MASK.
; 
; HISTORY: BTW, 23 March 2005
;     12 APR 2005 --- BTW: Change .b to .maxb field to structure.
;     28 APR 2005 --- BTW: Made .src and .trm fields longword integer.
;     09 May 2005 --- BTW: Added string-valued .alt_label field, for 
;  		      use with dual-mask (convex & contiguous) tracking.
;		      MATCH_MASKS.PRO fills in this field.
;     14 May 2005 --- BTW: Created .*err fields; to compute errors in 
;		      these quantities, multiply .*err fields by the 
;		      noise estimate, sigma_N, squared. 
;     19 May 2005 --- BTW: Included a factor of 2 in .XY and .XYERR fields.
;     27 Jun 2005 --- BTW: Bug found on DEC architectures.  IDL will 
;		      not concatenate anonymous the structures used in   
;		      this code.  To work around, replace this code
;        feature = {label:label_j,alt_label:'',size:size_j, $
;                     with 
;        feature = {feature,label:label_j,alt_label:'',size:size_j, $
;     07 May 2007 --- BTW: Fixed incorrect indexing for computing  
;                     velocity moments when LCT flows are also input.	
;     21 May 2008 --- BTW: Fixed bug that incorrectly neglected creating 
;                     a feature for highest (absolute) label.
;     05 Aug 2008 --- BTW: Added SILENT keyword; can now pass
;                     min_size=0
;
;- 

pro create_features, img, mask, features, min_size=min_size, $
                     vx=vx, vy=vy, dx=dx, peakthreshold=peakthreshold, $
                     silent=silent

nx = n_elements(img(*,0))
ny = n_elements(img(0,*))

if not(keyword_set(dx)) then begin
    dx = 1. ; dx should contain physical units of pixel size.
    if not(keyword_set(silent)) then $ 
      print,'% CREATE_FEATURES: Assuming unit pixel size.'
endif
if not(keyword_set(vx)) then begin
    vx = fltarr(nx,ny)
    if not(keyword_set(silent)) then $ 
      print,'% CREATE_FEATURES: Assuming no input V_x flow field.'
endif
if not(keyword_set(vy)) then begin
    vy = fltarr(nx,ny)
    if not(keyword_set(silent)) then $ 
      print,'% CREATE_FEATURES: Assuming no input V_y flow field.'
endif
if (n_elements(min_size) eq 0) then begin
    min_size = 10
    if not(keyword_set(silent)) then $ 
      print,'% CREATE_FEATURES: Setting min. feature size to 10 pixels.'
endif
if (n_elements(peakthreshold) eq 0) then begin
    peakthreshold = 0
    if not(keyword_set(silent)) then $ 
      print,'% CREATE_FEATURES: No peak threshold set.'
endif

orig_mask = mask      ; save input mask --- nice to have for debugging
n_lab = max(abs(mask))
new_mask = fltarr(nx,ny)

for j = 1,n_lab do begin
    where_j = (where(abs(mask) eq j, size_j))
    peakfield = max(abs(img(where_j)))
    
    if ((size_j ge min_size) and (peakfield gt peakthreshold)) then begin

        xs = (where_j)MOD(nx)  + .5 ; col. indices, convert to pix ctr
        ys = FLOOR(where_j/nx) + .5 ; row  indices, in pixel centers
        
        ; Spatial Moment Info
        ;=====================
        tot_j = total(img(where_j))
        max_j = max(abs(img(where_j)))
        x_j  = total(xs  *img(where_j))/(tot_j)
        x2_j = total((xs - x_j)^2*img(where_j))/(tot_j)
        y_j  = total(ys  *img(where_j))/(tot_j)
        y2_j = total((ys - y_j)^2*img(where_j))/(tot_j)
        xy_j = 2*total((xs - x_j)*(ys - y_j)*img(where_j))/(tot_j)
        sign_j = fix(tot_j/abs(tot_j))

	; Error Info
	;===========
        xerr_j = total((xs - x_j)^2)/tot_j^2 ; needs to be multi- ...
        yerr_j = total((ys - y_j)^2)/tot_j^2 ;  plied by sigma_N^2
        x2err_j = total(((xs - x_j)^2 - x2_j^2)^2)/(tot_j)^2 
        y2err_j = total(((ys - y_j)^2 - y2_j^2)^2)/(tot_j)^2
        xyerr_j = 2*total(((xs - x_j)*(ys - y_j) - x2_j*y2_j)^2)/(tot_j)^2

        ; Velocity Moment Info
        ;=====================
        vx_j  = total(  vx(where_j)        *img(where_j))/(tot_j)
        vx2_j = total( (vx(where_j)-vx_j)^2*img(where_j))/(tot_j)
        vy_j  = total(  vy(where_j)        *img(where_j))/(tot_j)
        vy2_j = total( (vy(where_j)-vy_j)^2*img(where_j))/(tot_j)
        vxvy_j =total( (vx(where_j)-vx_j)  * $
                       (vy(where_j)-vy_j)  *img(where_j))/(tot_j)

       tot_j = abs(tot_j)*dx^2 ; Make flux absolute, in physical units.
    
        ; Store this feature's mask in a string, retrieve via:
        ; addresses = long(strsplit(mask_str,/extract))
        ;=======================================================
        mask_str=strjoin(string(where_j))

        ; Pre-matched feature label = # of features defined, + 1  
        ;============================================
        label_j = n_elements(features)+1

        ; Make a structure for each feature
        ;==================================
        feature = {label:label_j,alt_label:'',size:size_j, $
                   step:0L,src:0L,trm:0L, $ ; tracking info
                   sign:sign_j, phi:tot_j, maxb:max_j, $ ; field params
                   x:x_j, y:y_j, x2:x2_j, y2:y2_j, xy:xy_j, $ ; flux moments 
		   xerr:xerr_j,yerr:yerr_j,$ ; info for computing errors
		   x2err:x2err_j,y2err:y2err_j,xyerr:xyerr_j,$ more err info
                   vx:vx_j,vx2:vx2_j,vy:vy_j,vy2:vy2_j,vxvy: vxvy_j,$ ; vel. 
                   mask_str:mask_str} ; one string of pixel addresses
 

        ; Make an array of features' structures
        ;======================================
        if (label_j eq 1) then features = feature $ 
          else features = [features,feature]

        ; Generate new mask w/feature sizes > min_size
        ;======================================
        new_mask(where_j) = label_j*sign_j

      endif
endfor

; If input mask lacked any non-zero pixels, let user know.
if (n_lab lt 1) then if not(keyword_set(silent)) then $
      message, 'CREATE_FEATURES: Input mask was blank.',/info

; Replace the old mask with the new (truncated) one
mask = new_mask 

end





