pro roiselect_square,xrang,yrang,roisize=roisize,roiname=roiname
;This procedure helps the user select a 50x50 pixel subregion of the image
;It returns the x- and y- indices of the rectangular region, ordered,
;in the DEVICE coordinate system.

;Input: None.
;Optional Input:
;               roisize - size in pixels of the region
;               roiname - if provided, will print the name of the
;                         region on top of its location
;Output: xrang,yrang - ordered pairs of the x-and y-ranges of the ROI

;Kamen Kozarev, Oct 2011

  if not keyword_set(roisize) then roisize=50
  
  yes=0
  no=1
  uinput=''
  
;The question is: Are you satisfied with the region selected
  response=no
  xsize=roisize
  ysize=roisize
  
  print,''
  print,'This program helps you select a RECTANGULAR region of interest.'
  print,'The resulting indices will be in DEVICE coordinates.'
;The master loop - keep asking the user to select regions.
  while response eq no do begin
     
     rs=strtrim(string(roisize),2)
     print,''
     print,'1. Please select the center of the desired '+rs+'x'+rs+' pixel ROI:'
     cursor,xc,yc,/device,/down
     
     print,'Center selected.'
     plots,xc,yc,psym=4,/device
     
     xrange=[xc-xsize/2,xc+xsize/2-1]
     yrange=[yc-ysize/2,yc+ysize/2-1]
     
     
     plots,[xrange[0],xrange[1]],[yrange[0],yrange[0]],/device,thick=3,color=255
     plots,[xrange[0],xrange[0]],[yrange[0],yrange[1]],/device,thick=3,color=255
     plots,[xrange[1],xrange[1]],[yrange[0],yrange[1]],/device,thick=3,color=255
     plots,[xrange[0],xrange[1]],[yrange[1],yrange[1]],/device,thick=3,color=255
     if keyword_set(roiname) then xyouts,xc-xsize/3.0,yc-ysize/4.0,roiname,/device,$
                                         charsize=3,charthick=4,color=255

     j1: read,uinput,prompt='Are you happy with this selection? y for yes/exit, n for no: '
     if uinput eq 'y' then begin
        response=yes
        print,''
        print,'Bye bye...'
        print,''
        xrang=xrange
        yrang=yrange
     endif else begin
        if uinput eq 'n' then begin
           print,"Okay, let's try it again:"
           plots,[xrange[0],xrange[1]],[yrange[0],yrange[0]],/device,thick=3,color=120
           plots,[xrange[0],xrange[0]],[yrange[0],yrange[1]],/device,thick=3,color=120
           plots,[xrange[1],xrange[1]],[yrange[0],yrange[1]],/device,thick=3,color=120
           plots,[xrange[0],xrange[1]],[yrange[1],yrange[1]],/device,thick=3,color=120
           if keyword_set(roiname) then xyouts,xc-xsize/3.0,yc-ysize/4.0,roiname,/device,$
                                               charsize=3,charthick=4,color=120
        endif else begin
           print,"Please answer with 'y' for yes and 'n' for no."
           goto, j1
        endelse
     endelse
  endwhile
  
end
