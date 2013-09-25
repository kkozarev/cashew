pro aiasetprofiles,data,index,proflocs,outpath=outpath,angres=angres,latrange=latrange
;A procedure for selecting the radial profiles from AIA data.
;Kamen Kozarev, September 2010
;NOTE: This will be replaced with an automatic procedure once I figure
;out how to obtain the flare location automatically from GOES/STEREO
;flare lists...

;INPUTS
;data - an image
;index - the index of the image
;
;outpath - output path for results (string)
 
;OUTPUT
;proflocs - an array containing the positions of the different
;           profiles.

;OPTIONAL OUTPUT
;If outpath is specified,the program saves the radial profiles in 
;file outpath+'proflocs_date.sav', where date is the date 
;of the event in MMDD format (for example, outpath+proflocs_0613.sav)



;===========================================================
;Constants and definitions
;===========================================================
!P.font=0
labelpos=[0.1,0.95]
cthick=2
csize=2

if not keyword_set(latrange) then latrange=20
if not keyword_set(angres) then angres=0.5
nprofiles = round(latRange/float(angres))

dim=1024
;dirpath='/data/SDO/AIA/level1/2010/06/13/'
;hours=['H0005']
;file=find_file(dirpath+hours[0]+'/*_053*_0193.fits')
;file=[file,find_file(dirpath+hours[0]+'/*_054*_0193.fits')]
;file=[file,find_file(dirpath+hours[0]+'/*_055*_0193.fits')]
nfiles=n_elements(data[*,0,0])

tobs=index.date_obs
date=strmid(tobs,5,2)+strmid(tobs,8,2)


;===========================================================




;===========================================================
;1. Pick the main radial profile.
;===========================================================

;These are the start and end points for the profile.
wdef,0,dim,dim
tvscl,sqrt(data)

xpos=fltarr(2)
ypos=fltarr(2)

print,'Click on the origin of the profile:'
cursor,x,y,/device
print, 'You chose x: '+strtrim(string(x),2)+'  y: '+strtrim(string(y),2)
xpos[0]=x
ypos[0]=y
plots,x,y,psym=2,symsize=2,/device

wait,0.5
print,'Click on the end of the profile:'
cursor,x,y,/device
print, 'You chose x: '+strtrim(string(x),2)+'  y: '+strtrim(string(y),2)
xpos[1]=x
ypos[1]=y
plots,x,y,psym=2,symsize=2,/device


nx=abs(floor(xpos[1]-xpos[0]))+1
print,'nx: '+strtrim(string(nx),2)


;find the slope
slope=float(ypos[1]-ypos[0])/float(xpos[1]-xpos[0])
initangle=atan(float(ypos[1]-ypos[0])/float(xpos[1]-xpos[0]))*180/!PI
profRadius=floor(sqrt((xpos[1]-xpos[0])^2+(ypos[1]-ypos[0])^2))
proflocs=fltarr(nprofiles,2,profRadius)

;calculate the intensity at those points
;NB!!! There will be some error in specifying the pixel locations correctly.
;This must be accounted for accurately.


;profx=indgen(nx)+xpos[0]
;profy=fltarr(nx)

;for i = 0, nx-1 do profy[i] = ypos[0] + slope*float(profx[i]-xpos[0])
;===========================================================



;===========================================================
;2. Create the other profiles around the primary one.
;===========================================================
xEndPts=intarr(2)
yEndPts=intarr(2)

xEndPts[0]=xpos[0]
yEndPts[0]=ypos[0]

for n = 0,nprofiles-1 do begin

    ;The angle is:
    angle = (initangle-latRange/2.0+n*angres)*!PI/180.
    ;First, determine the initial x and y for the profile.
    xEndPts[1] = xEndPts[0] + round(profRadius*cos(angle))
    yEndPts[1] = yEndPts[0] + round(profRadius*sin(angle))
    ;slope = float(yEndPts[1] - yEndPts[0])/float(xEndPts[1] - xEndPts[0])
    plots,xEndPts[1],yEndPts[1],psym=2,/device

    for r = 0, profradius-1 do begin
        ;x = xEndPts[0] + round(r*cos(angle))
        ;y = yEndPts[0] + round(r*sin(angle))
        proflocs[n,0,r]=xEndPts[0] + round(r*cos(angle))
        proflocs[n,1,r]=yEndPts[0] + round(r*sin(angle))
        plots, proflocs[n,0,r], proflocs[n,1,r], psym=3, /device
        ;outProf[r] = outProf[r] + tmp0[x,y]
        ;profArray[n,r] = tmp0[x,y]
    endfor

endfor
;===========================================================



;Save the radial profile locations
if keyword_set(outpath) then save,proflocs,filename=outpath+'proflocs_'+date+'.sav'

end
