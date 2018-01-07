pro test_compare_radial_lateral_kinematics
;A little program to test the compare_radial_lateral_kinematics procedure
  events = ['151104_01']
  ;events = ['131212_01']
  nevents=n_elements(events)
  for ev=0,nevents-1 do begin
     event_label=events[ev]
     event=load_events_info(label=event_label)
     print,event.savepath
     compare_radial_lateral_kinematics,event,/ps 
  endfor

end



function convert_lateral_to_radial_positions,lat_data,full=full
;Convert the latitudinal positions to perceived radial distance
  
  sp=lat_data.timefitrange[0]
  ep=lat_data.timefitrange[1]
  
  if not keyword_set(full) then begin
     gamma = lat_data.wave_frontedge[sp:ep].lat * !PI/180.
     gammastdv = lat_data.wave_frontedge[sp:ep].stdv * !PI/180.
  endif else begin
     lats = lat_data.wave_frontedge.lat
     lats[where(lats lt 0)] = 0.
     gamma = lats * !PI/180.
     gammastdv = lat_data.wave_frontedge.stdv * !PI/180.
     ;stop
  endelse
  radius = lat_data.radius
  frontrad = radius*sqrt(2*(1-cos(gamma))) ; + 1.
  frontstdv = abs( ( frontrad * abs(sin(gamma) * gammastdv) ) / ( 2. * (1 - cos(gamma))))
  frontrad +=1.
  
  if not keyword_set(full) then begin
     gamma = lat_data.wave_peak[sp:ep].lat * !PI/180.
     gammastdv = lat_data.wave_peak[sp:ep].stdv * !PI/180.
  endif else begin
     lats = lat_data.wave_peak.lat
     lats[where(lats lt 0)] = 0.
     gamma = lats * !PI/180.
     gammastdv = lat_data.wave_peak.stdv * !PI/180.
  endelse
  radius = lat_data.radius
  peakrad = radius*sqrt(2*(1-cos(gamma))) ;+ 1.
  peakstdv = abs( ( peakrad * abs(sin(gamma) * gammastdv) ) / ( 2. * (1 - cos(gamma))))
  peakrad +=1.

  if not keyword_set(full) then begin
     gamma = lat_data.wave_backedge[sp:ep].lat * !PI/180.
     gammastdv = lat_data.wave_backedge[sp:ep].stdv * !PI/180.
  endif else begin
     lats = lat_data.wave_backedge.lat
     lats[where(lats lt 0)] = 0.
     gamma = lats * !PI/180.
     gammastdv = lat_data.wave_backedge.stdv * !PI/180.
  endelse
  radius = lat_data.radius
  backrad = radius*sqrt(2*(1-cos(gamma))) ;+ 1.
  backstdv = abs( ( backrad * abs(sin(gamma) * gammastdv) ) / ( 2. * (1 - cos(gamma))))
  backrad +=1.
  
  rad_from_lat = {front:frontrad, frontstdv:frontstdv, peak:peakrad, peakstdv:peakstdv, back:backrad, backstdv:backstdv}
  return,rad_from_lat
end




function find_average_kinematics,rad_data,lat_data
;Function for finding the average radial (measured and inferred)
;positions, speeds, and accelerations between the radial
;and lateral data
  LEFTDIR=0
  RIGHTDIR=1
  lat_radii=lat_data[uniq(lat_data.radius,sort(lat_data.radius))].radius
  numrad=n_elements(lat_radii)
  numdirs=n_elements(uniq(lat_data.type,sort(lat_data.type)))
  lat_dirs=indgen(numdirs)
  ntimes=n_elements(rad_data.time)
  postemplate={front:dblarr(ntimes),peak:dblarr(ntimes),back:dblarr(ntimes)}
  allpos=replicate(postemplate,1+numdirs*numrad)
  allpos[0].front=rad_data.wave_frontedge.rad
  allpos[0].peak=rad_data.wave_peak.rad
  allpos[0].back=rad_data.wave_backedge.rad

  allspeed=replicate(postemplate,1+numdirs*numrad)
  allspeed[0].front=rad_data.savgolfits.front.speed
  allspeed[0].peak=rad_data.savgolfits.peak.speed
  allspeed[0].back=rad_data.savgolfits.back.speed

  allaccel=replicate(postemplate,1+numdirs*numrad)
  allaccel[0].front=rad_data.savgolfits.front.accel
  allaccel[0].peak=rad_data.savgolfits.peak.accel
  allaccel[0].back=rad_data.savgolfits.back.accel
  
  minsp = 1000
  maxsp = -1000
  for rr=0,numrad-1 do begin
     for did=0,numdirs-1 do begin
        dir=lat_dirs[did]
        latindex=dir+rr*numdirs
        
        ;POSITIONS
        rad_from_lat = convert_lateral_to_radial_positions(lat_data[latindex],/full)
        tmp=where(rad_from_lat.front eq 1.0)
        if tmp[0] ne -1 then rad_from_lat.front[tmp]=0.
        tmp=where(rad_from_lat.peak eq 1.0)
        if tmp[0] ne -1 then rad_from_lat.peak[tmp]=0.
        tmp=where(rad_from_lat.back eq 1.0)
        if tmp[0] ne -1 then rad_from_lat.back[tmp]=0.
        allpos[1+latindex].front=rad_from_lat.front
        allpos[1+latindex].peak=rad_from_lat.peak
        allpos[1+latindex].back=rad_from_lat.back
        
        ;SPEEDS
        allspeed[1+latindex].front=lat_data[latindex].savgolfits.front.speed
        allspeed[1+latindex].peak=lat_data[latindex].savgolfits.peak.speed
        allspeed[1+latindex].back=lat_data[latindex].savgolfits.back.speed
        
        ;ACCELERATIONS
        allaccel[1+latindex].front=lat_data[latindex].savgolfits.front.accel
        allaccel[1+latindex].peak=lat_data[latindex].savgolfits.peak.accel
        allaccel[1+latindex].back=lat_data[latindex].savgolfits.back.accel
     endfor
  endfor
  
  minsp = min([min(lat_data.timefitrange[0]),rad_data.timefitrange[0]])
  maxep = max([max(lat_data.timefitrange[1]),rad_data.timefitrange[1]])
  
  avgpos = {front:fltarr(ntimes),peak:fltarr(ntimes),back:fltarr(ntimes),sp:minsp,ep:maxep}
  avgspeed = {front:fltarr(ntimes),peak:fltarr(ntimes),back:fltarr(ntimes),sp:minsp,ep:maxep}
  avgaccel = {front:fltarr(ntimes),peak:fltarr(ntimes),back:fltarr(ntimes),sp:minsp,ep:maxep}
  
  avglatpos = {front:fltarr(ntimes),peak:fltarr(ntimes),back:fltarr(ntimes),sp:minsp,ep:maxep}
  avglatspeed = {front:fltarr(ntimes),peak:fltarr(ntimes),back:fltarr(ntimes),sp:minsp,ep:maxep}
  avglataccel = {front:fltarr(ntimes),peak:fltarr(ntimes),back:fltarr(ntimes),sp:minsp,ep:maxep}
  
  for tt=0,ntimes-1 do begin
     ;POSITIONS
     frontpos=allpos[*].front[tt]
     tmp=where(frontpos ne 0.)
     if tmp[0] ne -1 then frontpos=frontpos[tmp]
     avgpos.front[tt]=mean(frontpos)
     ;lateral only
     frontpos=allpos[1:*].front[tt]
     tmp=where(frontpos ne 0.)
     if tmp[0] ne -1 then frontpos=frontpos[tmp]
     avglatpos.front[tt]=mean(frontpos)
     
     peakpos=allpos[*].peak[tt]
     tmp=where(peakpos ne 0.)
     if tmp[0] ne -1 then peakpos=peakpos[tmp]     
     avgpos.peak[tt]=mean(peakpos)
     ;lateral only
     peakpos=allpos[1:*].peak[tt]
     tmp=where(peakpos ne 0.)
     if tmp[0] ne -1 then peakpos=peakpos[tmp]     
     avglatpos.peak[tt]=mean(peakpos)
     
     backpos=allpos[*].back[tt]
     tmp=where(backpos ne 0.)
     if tmp[0] ne -1 then backpos=backpos[tmp]  
     avgpos.back[tt]=mean(backpos)
     ;lateral only
     backpos=allpos[1:*].back[tt]
     tmp=where(backpos ne 0.)
     if tmp[0] ne -1 then backpos=backpos[tmp]  
     avglatpos.back[tt]=mean(backpos)
     
     
     ;SPEEDS
     frontspeed=allspeed[*].front[tt]
     tmp=where(frontspeed ne 0.)
     if tmp[0] ne -1 then frontspeed=frontspeed[tmp]
     avgspeed.front[tt]=mean(frontspeed)
     ;lateral only
     frontspeed=allspeed[1:*].front[tt]
     tmp=where(frontspeed ne 0.)
     if tmp[0] ne -1 then frontspeed=frontspeed[tmp]
     avglatspeed.front[tt]=mean(frontspeed)
     
     peakspeed=allspeed[*].peak[tt]
     tmp=where(peakspeed ne 0.)
     if tmp[0] ne -1 then peakspeed=peakspeed[tmp]     
     avgspeed.peak[tt]=mean(peakspeed)
     ;lateral only
     peakspeed=allspeed[1:*].peak[tt]
     tmp=where(peakspeed ne 0.)
     if tmp[0] ne -1 then peakspeed=peakspeed[tmp]     
     avglatspeed.peak[tt]=mean(peakspeed)
     
     backspeed=allspeed[*].back[tt]
     tmp=where(backspeed ne 0.)
     if tmp[0] ne -1 then backspeed=backspeed[tmp]  
     avgspeed.back[tt]=mean(backspeed)
     ;lateral only
     backspeed=allspeed[1:*].back[tt]
     tmp=where(backspeed ne 0.)
     if tmp[0] ne -1 then backspeed=backspeed[tmp]  
     avglatspeed.back[tt]=mean(backspeed)

     
     ;ACCELERATIONS
     frontaccel=allaccel[*].front[tt]
     tmp=where(frontaccel ne 0.)
     if tmp[0] ne -1 then frontaccel=frontaccel[tmp]
     avgaccel.front[tt]=mean(frontaccel)
     ;lateral only
     frontaccel=allaccel[1:*].front[tt]
     tmp=where(frontaccel ne 0.)
     if tmp[0] ne -1 then frontaccel=frontaccel[tmp]
     avglataccel.front[tt]=mean(frontaccel)
     
     peakaccel=allaccel[*].peak[tt]
     tmp=where(peakaccel ne 0.)
     if tmp[0] ne -1 then peakaccel=peakaccel[tmp]     
     avgaccel.peak[tt]=mean(peakaccel)
     ;lateral only
     peakaccel=allaccel[1:*].peak[tt]
     tmp=where(peakaccel ne 0.)
     if tmp[0] ne -1 then peakaccel=peakaccel[tmp]     
     avglataccel.peak[tt]=mean(peakaccel)

     backaccel=allaccel[*].back[tt]
     tmp=where(backaccel ne 0.)
     if tmp[0] ne -1 then backaccel=backaccel[tmp]  
     avgaccel.back[tt]=mean(backaccel)
     ;lateral only
     backaccel=allaccel[1:*].back[tt]
     tmp=where(backaccel ne 0.)
     if tmp[0] ne -1 then backaccel=backaccel[tmp] 
     avglataccel.back[tt]=mean(backaccel)
  endfor
  avgkinematics={avgpos:avgpos,avgspeed:avgspeed,avgaccel:avgaccel,avglatpos:avglatpos,avglatspeed:avglatspeed,avglataccel:avglataccel}
  return,avgkinematics
end


function find_comparison_plot_ranges, rad_data, rad_stdv, lat_data, lat_stdv, speed=speed, accel=accel
;Calculate the appropriate X- and Y-axis plotting ranges
  
  lat_radii=lat_data[uniq(lat_data.radius,sort(lat_data.radius))].radius
  numrad=n_elements(lat_radii)
  numdirs=n_elements(uniq(lat_data.type,sort(lat_data.type)))
  lat_dirs=indgen(numdirs)
  ntimes=n_elements(rad_data.time)
;-------------Calculate the time (X) and radial position (Y) ranges of the plot------------- 
  miny={front:1.e9,peak:1.e9,back:1.e9}
  maxy={front:-1.e9,peak:-1.e9,back:-1.e9}
  minx=1.e12
  maxx=-1.e12
  radsp=rad_data.timefitrange[0]
  radep=rad_data.timefitrange[1]
  radtimejd=rad_data.time.jd

  if keyword_set(speed) then begin
        front_quantity = rad_data.savgolfits.front[radsp:radep].speed
        front_qstdev = rad_stdv.front[radsp:radep].speed
        peak_quantity = rad_data.savgolfits.peak[radsp:radep].speed
        peak_qstdev = rad_stdv.peak[radsp:radep].speed
        back_quantity = rad_data.savgolfits.back[radsp:radep].speed
        back_qstdev = rad_stdv.back[radsp:radep].speed
  endif else begin
     if keyword_set(accel) then begin
        front_quantity = rad_data.savgolfits.front[radsp:radep].accel
        front_qstdev = rad_stdv.front[radsp:radep].accel
        peak_quantity = rad_data.savgolfits.peak[radsp:radep].accel
        peak_qstdev = rad_stdv.peak[radsp:radep].accel
        back_quantity = rad_data.savgolfits.back[radsp:radep].accel
        back_qstdev = rad_stdv.back[radsp:radep].accel
     endif else begin
        front_quantity = rad_data.wave_frontedge[radsp:radep].rad
        front_qstdev = rad_data.wave_frontedge[radsp:radep].stdv
        peak_quantity = rad_data.wave_peak[radsp:radep].rad
        peak_qstdev = rad_data.wave_peak[radsp:radep].stdv
        back_quantity = rad_data.wave_backedge[radsp:radep].rad
        back_qstdev = rad_data.wave_backedge[radsp:radep].stdv
     endelse
  endelse
  
  ;First the radial data
  quantity_front_max = max(front_quantity+front_qstdev)
  quantity_front_min = min(front_quantity[where(front_quantity ne 0.)]-front_qstdev)
  if quantity_front_min lt miny.front then miny.front = quantity_front_min
  if quantity_front_max gt maxy.front then maxy.front = quantity_front_max
  
  quantity_peak_max = max(peak_quantity + peak_qstdev)
  quantity_peak_min = min(peak_quantity[where(peak_quantity ne 0.)]-peak_qstdev)
  if quantity_peak_min lt miny.peak then miny.peak = quantity_peak_min
  if quantity_peak_max gt maxy.peak then maxy.peak = quantity_peak_max
  
  quantity_back_max = max(back_quantity+back_qstdev)
  quantity_back_min = min(back_quantity[where(back_quantity ne 0.)]-back_qstdev)
  if quantity_back_min lt miny.back then miny.back = quantity_back_min
  if quantity_back_max gt maxy.back then maxy.back = quantity_back_max
  
  if radtimejd[radsp] lt minx then minx = radtimejd[radsp]
  if radtimejd[radep] gt maxx then maxx = radtimejd[radep]
  
  ;Second the lateral data
  for rr=0,numrad-1 do begin
     for did=0,numdirs-1 do begin
        dir=lat_dirs[did]
        latindex=dir+rr*numdirs
        
        latsp=lat_data[latindex].timefitrange[0]
        latep=lat_data[latindex].timefitrange[1]
        lattimejd=lat_data[latindex].time.jd
        if keyword_set(speed) then begin
           front_quantity = lat_data[latindex].savgolfits.front[latsp:latep].speed
           front_qstdev = lat_stdv[latindex].front[latsp:latep].speed
           peak_quantity = lat_data[latindex].savgolfits.peak[latsp:latep].speed
           peak_qstdev = lat_stdv[latindex].peak[latsp:latep].speed
           back_quantity = lat_data[latindex].savgolfits.back[latsp:latep].speed
           back_qstdev = lat_stdv[latindex].back[latsp:latep].speed
        endif else begin
           if keyword_set(accel) then begin
              front_quantity = lat_data[latindex].savgolfits.front[latsp:latep].accel
              front_qstdev = lat_stdv[latindex].front[latsp:latep].accel
              peak_quantity = lat_data[latindex].savgolfits.peak[latsp:latep].accel
              peak_qstdev = lat_stdv[latindex].peak[latsp:latep].accel
              back_quantity = lat_data[latindex].savgolfits.back[latsp:latep].accel
              back_qstdev = lat_stdv[latindex].back[latsp:latep].accel
           endif else begin
              rad_from_lat = convert_lateral_to_radial_positions(lat_data[latindex])
              front_quantity = rad_from_lat.front
              front_qstdev = rad_from_lat.frontstdv
              peak_quantity = rad_from_lat.peak
              peak_qstdev = rad_from_lat.peakstdv
              back_quantity = rad_from_lat.back
              back_qstdev = rad_from_lat.backstdv
           endelse
        endelse
        quantity_front_max = max(front_quantity + front_qstdev)
        quantity_front_min = min(front_quantity[where(front_quantity ne 0.)]-front_qstdev)
        if quantity_front_min lt miny.front then miny.front = quantity_front_min
        if quantity_front_max gt maxy.front then maxy.front = quantity_front_max
        
        quantity_peak_max = max(peak_quantity + peak_qstdev)
        quantity_peak_min = min(peak_quantity[where(peak_quantity ne 0.)]-peak_qstdev)
        if quantity_peak_min lt miny.peak then miny.peak = quantity_peak_min
        if quantity_peak_max gt maxy.peak then maxy.peak = quantity_peak_max
        
        quantity_back_max = max(back_quantity + back_qstdev)
        quantity_back_min = min(back_quantity[where(back_quantity ne 0.)]-back_qstdev)
        if quantity_back_min lt miny.back then miny.back = quantity_back_min
        if quantity_back_max gt maxy.back then maxy.back = quantity_back_max
        
        if lattimejd[latsp] lt minx then minx = lattimejd[latsp]
        if lattimejd[latep] gt maxx then maxx = lattimejd[latep]
     endfor
  endfor
  xrange={minx:minx,maxx:maxx}
  yrange={miny:miny,maxy:maxy}

  return,{xrange:xrange,yrange:yrange}
end




pro plot_radlat_comparison, event, rad_data, rad_stdv, lat_data, lat_stdv, avg_kinematics, speed=speed, accel=accel, ps=ps
  ;Function for plotting the comparison between radial and lateral measurements

  dtsec=(rad_data.time[1].relsec-rad_data.time[0].relsec)
  radsp=rad_data.timefitrange[0]
  radep=rad_data.timefitrange[1]
  radtimejd=rad_data.time.jd

  lat_radii=lat_data[uniq(lat_data.radius,sort(lat_data.radius))].radius
  numrad=n_elements(lat_radii)
  numdirs=n_elements(uniq(lat_data.type,sort(lat_data.type)))
  lat_dirs=indgen(numdirs)
  ;dirstr=['Left','Right']
  dirstrtypes=lat_data[uniq(lat_data.type,sort(lat_data.type))].type
  dirstr=strarr(n_elements(dirstrtypes))
  for ii=0,n_elements(dirstrtypes)-1 do dirstr[ii]=(strsplit(dirstrtypes[ii],'_',/extract))[1]
  
  avgsp = avg_kinematics.avgpos.sp
  avgep = avg_kinematics.avgpos.ep
  avgrlsp = avg_kinematics.avglatpos.sp
  avgrlep = avg_kinematics.avglatpos.ep
  if keyword_set(speed) then begin
     plotpng=event.annuluspath+'compared_rad_lat_kinematics_speed_'+strtrim(event.label)+'.png'
     ploteps=event.annuluspath+'compared_rad_lat_kinematics_speed_'+strtrim(event.label)+'.eps'
     plotranges=find_comparison_plot_ranges(rad_data, rad_stdv, lat_data, lat_stdv,/speed)
  endif else begin
     if keyword_set(accel) then begin
        plotranges=find_comparison_plot_ranges(rad_data, rad_stdv, lat_data, lat_stdv,/accel)
        plotpng=event.annuluspath+'compared_rad_lat_kinematics_accel_'+strtrim(event.label)+'.png'
        ploteps=event.annuluspath+'compared_rad_lat_kinematics_accel_'+strtrim(event.label)+'.eps'
     endif else begin
        plotranges=find_comparison_plot_ranges(rad_data, rad_stdv, lat_data, lat_stdv)
        plotpng=event.annuluspath+'compared_rad_lat_kinematics_pos_'+strtrim(event.label)+'.png'
        ploteps=event.annuluspath+'compared_rad_lat_kinematics_pos_'+strtrim(event.label)+'.eps'
     endelse
  endelse
  
  xrnge=[plotranges.xrange.minx,plotranges.xrange.maxx]
  miny=plotranges.yrange.miny
  maxy=plotranges.yrange.maxy
  
;-------------Window creation--------------
  set_plot,'x'
  npanels=3
  if keyword_set(ps) then begin
     !p.font = 0
     set_plot,'ps'
     device,file=ploteps,/inches,xsize=9,ysize=12,$
            /encaps,/color,/helvetica
     !p.multi = [0, npanels, 1]
  endif else begin
     wdef, 1,1000, 1200
     loadct,0,/silent
     tvlct,rr,gg,bb,/get
     tvlct,reverse(rr),reverse(gg),reverse(bb)
     !p.multi = [0, npanels, 1]
     !p.font = -1
  endelse

  dummy = LABEL_DATE(DATE_FORMAT=['%H:%I'])
  
  ;Parameters of the plot panels
  x0=0.16
  x1=0.96
  y1=0.96
  y0=0.06
  panelysize=(y1-y0)/(1.*npanels) ;0.17
  panelxsize=x1-x0
  strdy=0.02
  yoffset=0.01
  
  linethick=4
  avglinethick=linethick+3
  
  ;colors
  green=150
  red=255
  blue=100
  purple=35
  black=0
  cfront=red
  cpeak=green
  cback=blue
  crad=black
  clat=[[green,purple],[red,blue]]
  
;----------------------PLOT THE FRONT QUANTITIES---------------------------
  if keyword_set(speed) then begin
     quantity = rad_data.savgolfits.front[radsp:radep].speed
     qstdev = rad_stdv.front[radsp:radep].speed
     title = "Wave speeds, "+strtrim(event.label,2)
     ytitle = "Speed [km/s]"
  endif else begin
     if keyword_set(accel) then begin
        quantity = rad_data.savgolfits.front[radsp:radep].accel
        qstdev = rad_stdv.front[radsp:radep].accel
        title = "Wave accelerations, "+strtrim(event.label,2)
        ytitle = "Acceleration [m/s!U2!N]"
     endif else begin
        quantity = rad_data.wave_frontedge[radsp:radep].rad
        qstdev = rad_data.wave_frontedge[radsp:radep].stdv
        title = "Wave positions, "+strtrim(event.label,2)
        ytitle = "Radial position [R!Ds!N]"
     endelse
  endelse
  
  !p.position = [x0,y1-1*panelysize,x0+panelxsize,y1-0*panelysize]
  yrnge = [miny.front,maxy.front]
  loadct, 0, /silent
  
  plot, radtimejd[radsp:radep], quantity,/xstyle, thick = linethick, $
        title= title,$
        charsize = 3, color = 0, background = 255, ytitle = ytitle,$
        xrange = xrnge, yrange = yrnge, /nodata, /ynozero, /ystyle, xthick=linethick, ythick=linethick, XTICKFORMAT="(A1)", $
        XTICKUNITS = ['Time']
  xyouts, !p.position[0]+0.012, !p.position[3]-yoffset-strdy, 'Front', color = crad, /normal, charsize = 2, charthick = 2
  loadct, 13, /silent
  ;From radial measurements
  oplot, radtimejd[radsp:radep], quantity, thick=linethick,color = crad
  oplot, radtimejd[radsp:radep], quantity,psym=sym(1),symsize=0.8,color=crad
  oploterr, radtimejd[radsp:radep], quantity, qstdev, errcolor = crad, errthick = 4, /noconnect
  xyouts, !p.position[2]-0.22, !p.position[1]+yoffset+(numrad*numdirs)*strdy, 'Radial', $
          color = crad, /normal, charsize = 1.3, charthick = 1.5
  
  ;From lateral measurements
  for rr=0,numrad-1 do begin
     ;for dir=LEFTDIR,RIGHTDIR do begin
     for did=0,numdirs-1 do begin
        dir=lat_dirs[did]
        latindex=dir+rr*numdirs
        lattimejd=lat_data[latindex].time.jd
        latsp=lat_data[latindex].timefitrange[0]
        latep=lat_data[latindex].timefitrange[1]

        if keyword_set(speed) then begin
           quantity = lat_data[latindex].savgolfits.front[latsp:latep].speed
           qstdev = lat_stdv[latindex].front[latsp:latep].speed
        endif else begin
           if keyword_set(accel) then begin
              quantity = lat_data[latindex].savgolfits.front[latsp:latep].accel
              qstdev = lat_stdv[latindex].front[latsp:latep].accel
           endif else begin
              rad_from_lat = convert_lateral_to_radial_positions(lat_data[latindex])
              quantity = rad_from_lat.front
              qstdev = rad_from_lat.frontstdv
           endelse
        endelse
        
        
        oplot, lattimejd[latsp:latep], quantity, thick=linethick,color = clat[rr,dir]
        oplot, lattimejd[latsp:latep], quantity,psym=sym(1),symsize=0.8,color=clat[rr,dir]
        oploterr, lattimejd[latsp:latep], quantity, qstdev, errcolor = clat[rr,dir], errthick = 4, /noconnect
        xyouts, !p.position[2]-0.22, !p.position[1]+yoffset+(numrad*numdirs-1-latindex)*strdy, $
                'Lat/'+dirstr[dir]+', '+strtrim(string(lat_data[latindex].radius,format='(f5.2)'),2)+' R!Ds!N', $
                color = clat[rr,dir], /normal, charsize = 1.3, charthick = 1.5
     endfor
  endfor
  
  if keyword_set(speed) then begin
     allavg = avg_kinematics.avgspeed.front[avgsp:avgep]
     allavg_lat = avg_kinematics.avglatspeed.front[avgsp:avgep]
  endif else begin
     if keyword_set(accel) then begin
        allavg = avg_kinematics.avgaccel.front[avgsp:avgep]
        allavg_lat = avg_kinematics.avglataccel.front[avgsp:avgep]
     endif else begin
        allavg = avg_kinematics.avgpos.front[avgsp:avgep]
        allavg_lat = avg_kinematics.avglatpos.front[avgrlsp:avgrlep]
     endelse
  endelse

  ;The average of all measurements
  loadct,0,/silent 
  oplot, radtimejd[avgsp:avgep], allavg, thick=avglinethick,color=120,linestyle=2
  xyouts, !p.position[2]-0.22, !p.position[1]+yoffset+(numrad*numdirs+1)*strdy, 'Average', $
          color = 120, /normal, charsize = 1.3, charthick = 1.5
  
  ;The average of the lateral measurements
  loadct,13,/silent
  oplot, radtimejd[avgrlsp:avgrlep], allavg_lat, thick=linethick,color=210
  xyouts, !p.position[2]-0.22, !p.position[1]+yoffset+(numrad*numdirs+2)*strdy, 'Average (Lat only)', $
          color = 210, /normal, charsize = 1.3, charthick = 1.5
;----------------------------------

  
;----------------------PLOT THE PEAK QUANTITIES---------------------------
  if keyword_set(speed) then begin
     quantity = rad_data.savgolfits.peak[radsp:radep].speed
     qstdev = rad_stdv.peak[radsp:radep].speed
     title = "Wave speeds, "+strtrim(event.label,2)
     ytitle = "Speed [km/s]"
  endif else begin
     if keyword_set(accel) then begin
        quantity = rad_data.savgolfits.peak[radsp:radep].accel
        qstdev = rad_stdv.peak[radsp:radep].accel
        title = "Wave accelerations, "+strtrim(event.label,2)
        ytitle = "Acceleration [m/s!U2!N]"
     endif else begin
        quantity = rad_data.wave_peak[radsp:radep].rad
        qstdev = rad_data.wave_peak[radsp:radep].stdv
        title = "Wave positions, "+strtrim(event.label,2)
        ytitle = "Radial position [R!Ds!N]"
     endelse
  endelse
  
  !p.position = [x0,y1-2*panelysize,x0+panelxsize,y1-1*panelysize]
  yrnge = [miny.peak,maxy.peak]
  loadct, 0, /silent
  plot, radtimejd[radsp:radep], quantity,/xstyle, thick = linethick, $
        charsize = 3, color = 0, background = 255, ytitle =ytitle,$
        xrange = xrnge,yrange = yrnge,/nodata,/ynozero,/ystyle, xthick=linethick,ythick=linethick, $
        XTICKFORMAT="(A1)", XTICKUNITS = ['Time']
  xyouts, !p.position[0]+0.012, !p.position[3]-yoffset-strdy, 'Peak', color = crad, /normal, $
          charsize = 2, charthick = 2
  loadct, 13, /silent
  
  ;From radial measurements
  oplot, radtimejd[radsp:radep],  quantity, thick=linethick,color = crad
  oplot, radtimejd[radsp:radep], quantity,psym=sym(1),symsize=0.8,color=crad
  oploterr, radtimejd[radsp:radep], quantity, qstdev, errcolor = crad, errthick = 4, /noconnect

                                ;From lateral measurements
  for rr=0,numrad-1 do begin
                                ;for dir=LEFTDIR,RIGHTDIR do begin
     for did=0,numdirs-1 do begin
        dir=lat_dirs[did]
        latindex=dir+rr*numdirs
        lattimejd=lat_data[latindex].time.jd
        latsp=lat_data[latindex].timefitrange[0]
        latep=lat_data[latindex].timefitrange[1]
        if keyword_set(speed) then begin
           quantity = lat_data[latindex].savgolfits.peak[latsp:latep].speed
           qstdev = lat_stdv[latindex].peak[latsp:latep].speed
        endif else begin
           if keyword_set(accel) then begin
              quantity = lat_data[latindex].savgolfits.peak[latsp:latep].accel
              qstdev = lat_stdv[latindex].peak[latsp:latep].accel
           endif else begin
              rad_from_lat = convert_lateral_to_radial_positions(lat_data[latindex])
              quantity = rad_from_lat.peak
              qstdev = rad_from_lat.peakstdv
           endelse
        endelse
        oplot, lattimejd[latsp:latep],  quantity, thick=linethick,color = clat[rr,dir]
        oplot, lattimejd[latsp:latep], quantity,psym=sym(1),symsize=0.8,color=clat[rr,dir] 
        oploterr, lattimejd[latsp:latep], quantity, qstdev, errcolor = clat[rr,dir], errthick = 4, /noconnect
     endfor
  endfor

  if keyword_set(speed) then begin
     allavg = avg_kinematics.avgspeed.peak[avgsp:avgep]
     allavg_lat = avg_kinematics.avglatspeed.peak[avgsp:avgep]
  endif else begin
     if keyword_set(accel) then begin
        allavg = avg_kinematics.avgaccel.peak[avgsp:avgep]
        allavg_lat = avg_kinematics.avglataccel.peak[avgsp:avgep]
     endif else begin
        allavg = avg_kinematics.avgpos.peak[avgsp:avgep]
        allavg_lat = avg_kinematics.avglatpos.peak[avgrlsp:avgrlep]
     endelse
  endelse
  ;The average of all measurements
  loadct,0,/silent
  oplot, radtimejd[avgsp:avgep], allavg,thick=avglinethick,color=120,linestyle=2

  ;The average of the lateral measurements
  loadct,13,/silent
  oplot, radtimejd[avgrlsp:avgrlep], allavg_lat,thick=linethick,color=210

  
;----------------------PLOT THE BACK POSITIONS---------------------------
    if keyword_set(speed) then begin
     quantity = rad_data.savgolfits.back[radsp:radep].speed
     qstdev = rad_stdv.back[radsp:radep].speed
     title = "Wave speeds, "+strtrim(event.label,2)
     ytitle = "Speed [km/s]"
  endif else begin
     if keyword_set(accel) then begin
        quantity = rad_data.savgolfits.back[radsp:radep].accel
        qstdev = rad_stdv.back[radsp:radep].accel
        title = "Wave accelerations, "+strtrim(event.label,2)
        ytitle = "Acceleration [m/s!U2!N]"
     endif else begin
        quantity = rad_data.wave_backedge[radsp:radep].rad
        qstdev = rad_data.wave_backedge[radsp:radep].stdv
        title = "Wave positions, "+strtrim(event.label,2)
        ytitle = "Radial position [R!Ds!N]"
     endelse
  endelse
  
  !p.position = [x0,y1-3*panelysize,x0+panelxsize,y1-2*panelysize]
  yrnge = [miny.back,maxy.back]
  loadct, 0, /silent
  plot, radtimejd[radsp:radep], quantity,/xstyle, thick = linethick, $
        charsize = 3, color = 0, background = 255, ytitle =ytitle,$
        xrange = xrnge, yrange = yrnge, /nodata, /ynozero,/ystyle, xthick=linethick,ythick=linethick, XTICKFORMAT='LABEL_DATE', $
        XTICKUNITS = ['Time']
  xyouts, !p.position[0]+0.012, !p.position[3]-yoffset-strdy, 'Back', color = crad, /normal, charsize = 2, charthick = 2
  loadct, 13, /silent

  ;From radial measurements
  oplot, radtimejd[radsp:radep], quantity, thick=linethick, color = crad
  oplot, radtimejd[radsp:radep], quantity,psym=sym(1),symsize=0.8,color=crad
  oploterr, radtimejd[radsp:radep], quantity, qstdev, errcolor = crad, errthick = 4, /noconnect
  
  ;From lateral measurements
  for rr=0,numrad-1 do begin
     ;for dir=LEFTDIR,RIGHTDIR do begin
     for did=0,numdirs-1 do begin
        dir=lat_dirs[did]
        latindex=dir+rr*numdirs
        lattimejd=lat_data[latindex].time.jd
        if keyword_set(speed) then begin
           quantity = lat_data[latindex].savgolfits.back[latsp:latep].speed
           qstdev = lat_stdv[latindex].back[latsp:latep].speed
        endif else begin
           if keyword_set(accel) then begin
              quantity = lat_data[latindex].savgolfits.back[latsp:latep].accel
              qstdev = lat_stdv[latindex].back[latsp:latep].accel
           endif else begin
              rad_from_lat = convert_lateral_to_radial_positions(lat_data[latindex])
              quantity = rad_from_lat.back
              qstdev = rad_from_lat.backstdv
           endelse
        endelse
        
        latsp=lat_data[latindex].timefitrange[0]
        latep=lat_data[latindex].timefitrange[1]
        oplot, lattimejd[latsp:latep], quantity, thick=linethick, color = clat[rr,dir]
        oplot, lattimejd[latsp:latep], quantity,psym=sym(1),symsize=0.8,color=clat[rr,dir]
        oploterr, lattimejd[latsp:latep], quantity, qstdev, $
                  errcolor = clat[rr,dir], errthick = 4, /noconnect
     endfor
  endfor

  if keyword_set(speed) then begin
     allavg = avg_kinematics.avgspeed.back[avgsp:avgep]
     allavg_lat = avg_kinematics.avglatspeed.back[avgsp:avgep]
  endif else begin
     if keyword_set(accel) then begin
        allavg = avg_kinematics.avgaccel.back[avgsp:avgep]
        allavg_lat = avg_kinematics.avglataccel.back[avgsp:avgep]
     endif else begin
        allavg = avg_kinematics.avgpos.back[avgsp:avgep]
        allavg_lat = avg_kinematics.avglatpos.back[avgrlsp:avgrlep]
     endelse
  endelse
  
  ;The average of all measurements
  loadct,0,/silent
  oplot, radtimejd[avgsp:avgep], allavg,thick=avglinethick,color=120,linestyle=2
  
  ;The average of the lateral measurements
  loadct,13,/silent
  oplot, radtimejd[avgrlsp:avgrlep], allavg_lat,thick=linethick,color=210
  
;----------------------Save the plot-----------------
  !p.multi=0
  if keyword_set(ps) then begin
     device,/close
     ;here do conversions to PNG files.
     exec='convert -flatten '+ploteps+' '+plotpng + '; rm -rf '+ploteps
     spawn,exec
     set_plot,'x'
  endif else begin
     write_png, plotpng, tvrd(/true)
     wdelete, 1           
  endelse  
end



pro compare_radial_lateral_kinematics,event,ps=ps
;Procedure to compare the averaged radial and lareral kinematics
;measurements/calculations directly.
;This currently works for the averaged manual measurements.
;
;CATEGORY:
; AIA/Kinematics
;
;INPUTS:
;      EVENT - the event structure
;
;KEYWORDS:  
;     PS - if set, write EPS and PNG files. Default is PNG from X-window.
;OUTPUTS:
;Plots comparing the radial and lateral measurements
;
;DEPENDENCIES:
;
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 30/05/2017
;
  wav = '193'
  ;dirstr=['Left','Right']
  
;-------------Restore the lateral data-------------
  lat_avg_fname=event.annuluspath+replace_string(event.annplot.analyzed.lateral.avg_savename,'WAV',wav)
  if not file_exist(lat_avg_fname) then begin
     print,''
     print,'File not present: '+lat_avg_fname
     print,'Run the lateral kinematics routine first.'
     return
  endif
  
  restore,lat_avg_fname
  tmp=where(lat_data.status eq 1)
  if tmp[0] ne -1 then begin
     lat_data=lat_data[tmp]
     lat_stdv=lat_stdv[tmp]
  endif else begin
     print, ''
     print,'No good data in file '+rad_avg_fname
     print, 'Quitting.'
     return
  endelse
  ;lat_radii=lat_data[uniq(lat_data.radius,sort(lat_data.radius))].radius
  ;numdirs=n_elements(uniq(lat_data.type,sort(lat_data.type)))
  ;lat_dirs=indgen(numdirs)
  ;numrad=n_elements(lat_radii) ;determine the number of radial measurements from the total, dividing by 2 directions
  ;dirstrtypes=lat_data[uniq(lat_data.type,sort(lat_data.type))].type
  ;dirstr=strarr(n_elements(dirstrtypes))
  ;for ii=0,n_elements(dirstrtypes)-1 do dirstr[ii]=(strsplit(dirstrtypes[ii],'_',/extract))[1]
  
;-------------Restore the radial data-------------
  rad_avg_fname=event.annuluspath+replace_string(event.annplot.analyzed.radial.avg_savename,'WAV',wav)
  if not file_exist(rad_avg_fname) then begin
     print,''
     print,'File not present: '+rad_avg_fname
     print,'Run the radial kinematics routine first.'
     return
  endif
  restore,rad_avg_fname

  
  ;The calculations are done in the find_average_kinematics function.
  avg_kinematics = find_average_kinematics(rad_data,lat_data)
  
  
  ;Do the plots for positions, speeds, and accelerations
  plot_radlat_comparison, event, rad_data, rad_stdv, lat_data, lat_stdv, avg_kinematics, ps=ps
  plot_radlat_comparison, event, rad_data, rad_stdv, lat_data, lat_stdv, avg_kinematics, ps=ps,/speed
  plot_radlat_comparison, event, rad_data, rad_stdv, lat_data, lat_stdv, avg_kinematics, ps=ps,/accel
  
end
