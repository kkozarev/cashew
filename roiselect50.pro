pro roiselect50,xrange,yrange
;This procedure helps the user select a 50x50 pixel subregion of the image
;It returns the x- and y- indices of the rectangular region, ordered,
;in the DEVICE coordinate system.

;Input: None.
;Output: xrange,yrange - ordered pairs of the x-and y-ranges of the ROI

;Kamen Kozarev, Jan 2011


yes=0
no=1
uinput=''

;The question is: Are you satisfied with the region selected
response=no
xsize=50
ysize=50

print,''
print,'This program helps you select a RECTANGULAR region of interest.'
print,'The resulting indices will be in DEVICE coordinates.'
;The master loop - keep asking the user to select regions.
while response eq no do begin

    print,''
    print,'1. Please select the center of the desired 50x50 pixel ROI:'
    cursor,xc,yc,/device
    
    print,'Center selected.'
    plots,xc,yc,psym=4,/device
    wait,0.1
    
    xrange=[xc-xsize/2,xc+xsize/2-1]
    yrange=[yc-ysize/2,yc+ysize/2-1]
    

    plots,[xrange[0],xrange[1]],[yrange[0],yrange[0]],/device
    plots,[xrange[0],xrange[0]],[yrange[0],yrange[1]],/device
    plots,[xrange[1],xrange[1]],[yrange[0],yrange[1]],/device
    plots,[xrange[0],xrange[1]],[yrange[1],yrange[1]],/device
    
    j1: read,uinput,prompt='Are you happy with this selection? y for yes/exit, n for no: '
    if uinput eq 'y' then begin
        response=yes
        print,''
        print,'Bye bye...'
        print,''
    endif else begin
        if uinput eq 'n' then begin
            print,"Okay, let's try it again:"
        endif else begin
            print,"Please answer with 'y' for yes and 'n' for no."
            goto, j1
        endelse
    endelse
endwhile

end
