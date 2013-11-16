function aia_inspect_data,index,data,subcor=subcor,autoregion=autoregion,bdiff=bdiff,bratio=bratio,event=event
;PURPOSE:
;This procedure converts the full 4k AIA data to 1k cube and shows a
;movie to the user, allowing an inspection of the data. 
;Optionally, it can allow the user to select a subregion, and return it.
;
;CATEGORY:
; AIA/General
;
;INPUTS:
;      index - the index array
;      data - the datacube
;
;OPTIONAL INPUT:
;         autoregion - Automatically select a subregion, do
;                     not show a movie of the data. The program expects the 
;                     pixel coordinates for the upper left(x1,y1) and lower
;                     right(x2,y2) corners of the ROI to be supplied, like   
;                     [x1,y1,x2,y2].
;         event - the event structure
;
;KEYWORDS:
;         bdiff - make base difference images for the movies. Not used
;                 in the automatic mode.
;         bratio - make base ratio images for the movies. Not used
;                 in the automatic mode.
;         
;
;OUTPUT:
;	  If the autoregion keyword is set, returns 
;
;OPTIONAL OUTPUT:
;         subcor - if a variable is provided, return the upper left(x1,y1)
;                  and lower right(x2,y2) ROI pixel coordinates, 
;                  like [x1,y1,x2,y2].
;
;
;Kamen Kozarev 02/2010

result=0
subcor=intarr(4)
if keyword_set(event) then fov=event.aiafov else fov=[1024,1024]

;Rebin the data to X-Y pixels
nt=n_elements(index)
dat=fltarr(fov[0],fov[1],nt)
for t=0, nt-1 do dat[*,*,t]=rebin(reform(data[*,*,t]),fov[0],fov[1])


;make a movie and show it until the user is satisfied
if not keyword_set(autoregion) then begin
c=0
uinput=''
print,''
wdef,0,fov[0],fov[1]
   while c eq 0 do begin
      if keyword_set(bdiff) then begin
         for t=0,nt-1 do tvscl,dat[*,*,t]-dat[*,*,0]
      endif else begin
         if keyword_set(bratio) then begin
            for t=0,nt-1 do tvscl,dat[*,*,t]/(1.0*dat[*,*,0])
         endif else begin
         ;the default sqrt(data) movie...
            for t=0,nt-1 do tvscl,dat[*,*,t]
         endelse
      endelse

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

   print, 'Select the region of interest.'
   tvscl,dat[*,*,nt/2]-dat[*,*,0]
   roiselect,x,y
   x*=4
   y*=4
   result=data[x[0]:x[1],y[0]:y[1],*]
   if keyword_set(subcor) then begin
      subcor=fix([x[0],x[1],y[0],y[1]])
endif

endif else begin
      ;select the ROI of interest
      x=[autoregion[0],autoregion[2]]
      y=[autoregion[1],autoregion[3]]
      result=data[x[0]:x[1],y[0]:y[1],*]
endelse

return,result
end
