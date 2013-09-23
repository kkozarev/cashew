pro roiselect,xrange,yrange
;PURPOSE:
;This procedure helps the user select a subregion of the image
;It returns the x- and y- indices of the rectangular region, ordered,
;in the DEVICE coordinate system.
;
;CATEGORY:
;AIA/General
;
;INPUTS:
;
;KEYWORDS:
;
;OUTPUTS:
;
;DEPENDENCIES:
;
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 01/2011
;


yes=0
no=1
uinput=''

;The question is: Are you satisfied with the region selected
response=no


print,''
print,'This program helps you select a RECTANGULAR region of interest.'
print,'The resulting indices will be in DEVICE coordinates.'
;The master loop - keep asking the user to select regions.
while response eq no do begin

    print,''
    print,'1. Please select the position of one edge:'
    cursor,x0,y0,/device
    
    print,'First edge selected.'
    plots,x0,y0,psym=4,/device
    wait,0.1
    
    print,''
    print,'2. Please select the position of an opposite edge:'
    cursor,x1,y1,/device
    
    print,'Second edge selected.'
    plots,x1,y1,psym=4,/device
    
    xrange=[x0,x1]
    xrange=xrange[sort(xrange)]
    yrange=[y0,y1]
    yrange=yrange[sort(yrange)]

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
