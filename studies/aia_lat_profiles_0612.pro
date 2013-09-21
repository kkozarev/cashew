pro aia_lat_profiles_0612
;This procedure will add more angular extent to the profiles we are 
;using for AIA, so I can do lateral measurements of wave speeds.
;01/31/2011
;NOTE - this is a hack, not a sustainable development...

path='/home/kkozarev/Desktop/AIA/limbCMEs/'
outpath='/home/kkozarev/Desktop/temp/profiles/'

xrange=[3072,4095]
yrange=[2548,3571]

;0. Get the profiles that exist already
restore,path+'06122010/results/proflocs_0612.sav'
proflocs_old=proflocs
;Also get some data for display purposes...
restore,path+'06122010/results/193/regionData_0612_193.sav'


tobs=indices[0].date_obs
date=strmid(tobs,5,2)+strmid(tobs,8,2)




wdef,0,1024
tvscl,sqrt(totaldata[20,*,*])

;stop

;1. Create new profiles and rerecord...
angres=1
latrange=120
nprofiles = round(latRange/float(angres))

xpos=[proflocs[0,0,0],proflocs[10,0,1063]]
ypos=[proflocs[0,1,0],proflocs[10,1,1063]]

plots,xpos[0],ypos[0],psym=2,symsize=2,/device,color=0
plots,xpos[1],ypos[1],psym=2,symsize=2,/device


;This is from the old profile-creating routine:

nx=abs(floor(xpos[1]-xpos[0]))+1
print,'nx: '+strtrim(string(nx),2)


;find the slope
slope=float(ypos[1]-ypos[0])/float(xpos[1]-xpos[0])
initangle=atan(float(ypos[1]-ypos[0])/float(xpos[1]-xpos[0]))*180/!PI
profRadius=floor(sqrt((xpos[1]-xpos[0])^2+(ypos[1]-ypos[0])^2))
proflocs=fltarr(nprofiles,2,profRadius)

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
        ;plots, proflocs[n,0,r], proflocs[n,1,r], psym=3, /device
        ;outProf[r] = outProf[r] + tmp0[x,y]
        ;profArray[n,r] = tmp0[x,y]
    endfor

endfor
;===========================================================


;stop

n=16
for r = 0, profradius-1 do plots, proflocs[n,0,r], proflocs[n,1,r], psym=3, /device
n=nprofiles/2-10
for r = 0, profradius-1 do plots, proflocs[n,0,r], proflocs[n,1,r], psym=3, /device
n=nprofiles/2
for r = 0, profradius-1 do plots, proflocs[n,0,r], proflocs[n,1,r], psym=3, /device
n=nprofiles/2+9
for r = 0, profradius-1 do plots, proflocs[n,0,r], proflocs[n,1,r], psym=3, /device
n=nprofiles-10
for r = 0, profradius-1 do plots, proflocs[n,0,r], proflocs[n,1,r], psym=3, /device

;plot the three original profiles...
pnum=[0,10,19]
for i=0,2 do plots,proflocs_old[pnum[i],0,*],proflocs_old[pnum[i],1,*],/device,thick=4



;xyouts,180,30,'n=16',/device,charsize=2
;xyouts,750,50,'n=70',/device,charsize=2 
;xyouts,850,120,'n=80',/device,charsize=2
;xyouts,900,220,'n=90',/device,charsize=2
;xyouts,850,930,'n=159',/device,charsize=2

stop


if keyword_set(outpath) then save,proflocs,filename=outpath+'proflocs_new_'+date+'.sav'

end
