pro aia_inspect_map,map,submap=submap,automatic=automatic
;PURPOSE:
;This procedure converts the full 4k AIA mapcube to 1k mapcube and shows a
;movie to the user, allowing an inspection of the data. 
;Optionally, it can allow the user to select a subregion, and return it.
;
;CATEGORY:
; AIA/General
;
;INPUTS:
; map - the original mapcube
;
;KEYWORDS:
; automatic - Automatically select a subregion, do
;                     not show a movie of the data. The program expects the 
;                     pixel coordinates for the upper left(x1,y1) and lower
;                     right(x2,y2) corners of the ROI to be supplied, like   
;                     [x1,y1,x2,y2].
;
;OUTPUTS:
; submap - if a variable is provided, return a submap of
;                   the datacube.
; 
;DEPENDENCIES:
; roiselect, sub_map, plot_map
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 02/2010
;

nt=n_elements(map)



;make the difference map
diffmap=diff_map(map[1],map[0])
for nn=2,nt-1 do begin
dmap=diff_map(map[nn],map[0])
diffmap=[[diffmap],[dmap]]
endfor
ndiff=nt-1

;if i eq 0 then diffmaps=replicate(ss,nmaps)


;make a movie and show it until the user is satisfied
if not keyword_set(automatic) then begin
c=0
print,''
uinput=''
;wdef,0,1024
while c eq 0 do begin
   for t=0,ndiff-1 do plot_map, diffmap[t], dmin = -70, dmax = 20, /isotropic, $
                             /limb, lthick = 1, grid_spacing = 20, gthick = 1, gstyle = 0
   l1:read,uinput,prompt='Watch again? y for yes, n for no: '
   if uinput eq 'n' then begin
      c=1
   endif else begin
      if uinput eq 'y' then begin
         print,'Here is the movie again'
      endif else begin
         print,"Please answer with 'y' for yes and 'n' for no."
         goto,l1
      endelse
   endelse
endwhile
endif

;JUST IN CASE YOU WANT TO SAVE THE MAPS IN PNG FILES...:
;date=strsplit(map[0].time,' -:.',/extract)
;set_plot,'z'
;loadct,9,/silent
;tvlct,r,g,b,/get
;wdef,0,1024
;for t=0,ndiff-1 do begin
;plot_map, diffmap[t], dmin = -70, dmax = 20, /isotropic, $
;          /limb, lthick = 1, grid_spacing = 20, gthick = 1, gstyle = 0
;fname='mapbdiff_'+date[0]+date[1]+'_'+wav+'_'+strtrim(string(i+1000),2)+'.png'
;write_png,outpath+'mapbdiff/'+fname,tvrd(),r,g,b
;endfor
;set_plot,'x'


;select the ROI of interest
if keyword_set(submap) then begin
   ;print, 'Select the region of interest.'
   ;plot_map, diffmap[nt/2], dmin = -70, dmax = 20, /isotropic, /limb, lthick = 1, grid_spacing = 20, gthick = 1, gstyle = 0
   ;roiselect,x,y
   ;x*=4
   ;y*=4

   sub_map,map,submap,/plot;,xrange=x,yrange=y

endif





end
