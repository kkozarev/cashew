; This collection of IDL commands is meant to be used in combination
; with the tutorial text in YAFTA_ReadMe.

; if necessary, get YAFTA results to analyze
if (n_elements(features) eq 0) then restore,'YAFTA_output.sav'

; Plot unsigned flux vs. time for Feature Label 8
index8 = where(features.label eq 8, n8) 
if n8 ne 0 then flux8 = features(index8).phi 
if n8 ne 0 then step8 = features(index8).step 
if n8 ne 0 then plot, step8, flux8  

; Do the same, but with less code.
index8 = where(features.label eq 8, n8) 
if n8 ne 0 then feat8 = features(index8) 
if n8 ne 0 then plot, feat8.step, feat8.phi  

; Now plot signed flux vs. time
if n8 ne 0 then plot, feat8.step, feat8.phi*feat8.sign  

; Find instances of flux appearance.
apps = where(features.src lt 0, n_apps)  
if n_apps ne 0 then appflux = total(features(apps).phi)  

; Compare appeared flux to total flux
notfirst = features(where(features.step gt 0)) 
if n_apps ne 0 then flux = total(notfirst.phi) 
if n_apps ne 0 then print, appflux, flux 

; Find instances of flux fragmentation.
where_frag = where(features.label ne features.src and $ 
                   features.src gt 0, n_frags) 
if n_frags ne 0 then frags = features(where_frag) 
if n_frags ne 0 then fragflux = total(frags.phi)/n_frags 

; Compare flux from fragments to all flux
avgflux= total(features.phi)/n_elements(features.phi) 
print,'Avg. Unsigned Flux in All Features, in Fragments:' 
print,avgflux,fragflux 

; Reconstruct a feature's mask
mask_str = features(13).mask_str  
addresses = long(strsplit(mask_str,/extract)) 
newmask = all_masks(*,*,0)
newmask(*,*) = 0 
newmask(addresses) = features(13).label 
display_yafta, newmask, /aspect 
plot_edges, newmask 

; Dilate the reconstructed mask
dilmask = newmask
dilmask(addresses) = 10 
s1 = replicate(1,3,3) ; dilates to nearest neighbors 
dilate1 = 9*fix(dilate(dilmask, s1)) 
s2 = replicate(1,5,5) ; dilates to 2 nearest neighbors 
dilate2 = 8*fix(dilate(dilmask, s2)) 
display_yafta, dilmask+dilate1+dilate2, /asp 
plot_edges, newmask 

end


