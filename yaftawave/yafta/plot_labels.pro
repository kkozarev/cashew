;+
;
; NAME: plot_labels.pro
;
; USAGE: plot_labels,features
; 
; PURPOSE: Uses IDL's XYOUTS.PRO to overlay features' labels, colored
;  coded by label, on images (e.g., those created by DISPLAY.PRO).
;
; ARGUMENTS: FEATURES = array of structures of each feature, generated
;    by CREATE_FEATURES.PRO
;  
; OPTIONAL ARGUMENTS/KEYWORDS:
;    CT= color table to for color coding features' labels; DEFAULT=13
;    DCOLOR= color shift between labels {i,i+1}; DEFAULT=23
;    MIN_COL= min. color to use (excludes colors); DEFAULT=30
;    MAX_COL= max. color to use (excludes colors); DEFAULT=255
;    THICK= thickness of characters used in labels; ; DEFAULT=2
;    SIZE= size of characters used in labels; ; DEFAULT=2
;    
; SIDE EFFECTS/ROUTINES CALLED: LOADCT.PRO, XYOUTS.PRO
; 
; HISTORY: Written 27 March 2005, BTW
;   05/31/05, BTW: Changed not(keyword_set(ct)) to (n_elements(ct) eq 0) 
;
;- 

pro plot_labels, features, ct=ct,dcolor=dcolor, $
                 min_col=min_col,max_col=max_col, $
                 thick=thick,size=size

n = n_elements(features.label)

if (n_elements(ct) eq 0) then ct=13
if not(keyword_set(dcolor)) then dcolor=23
if not(keyword_set(min_col)) then min_col=30
if not(keyword_set(max_col)) then max_col=255
if not(keyword_set(size)) then size=2
if not(keyword_set(thick)) then thick=2

for nn=0,n_elements(features)-1 do begin
   xoffset=30
   if features[nn].label ge 10 then xoffset+=20
   if features[nn].label ge 100 then xoffset+=20
   polyfill,[features[nn].x-xoffset,features[nn].x+10,features[nn].x+10,features[nn].x-xoffset],$
            [features[nn].y-10,features[nn].y-10,features[nn].y+30,features[nn].y+30],color=0
endfor
loadct,ct ,/silent
xyouts, features.x-.2, features.y-.8, string(features.label),align = 1., $
  color=(dcolor*features.label)mod(max_col-min_col) + min_col, $
  chars=size,charthick=thick 

loadct,0 ,/silent

end

