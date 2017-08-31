pro test_compare_radial_lateral_kinematics
;A little program to test the compare_radial_lateral_kinematics procedure
  events=['110607_01','131212_01']
  nevents=n_elements(events)
  for ev=0,nevents-1 do begin
     event_label=events[ev]
     event=load_events_info(label=event_label)
     compare_radial_lateral_kinematics,event,/ps 
  endfor

end


function convert_lateral_to_radial_positions,lat_data
  ;----------Convert the latitudinal positions to perceived radial distance-----------
  
  sp=lat_data.timefitrange[0]
  ep=lat_data.timefitrange[1]

  gamma = lat_data.wave_frontedge[sp:ep].lat * !PI/180.
  gammastdv = lat_data.wave_frontedge[sp:ep].stdv * !PI/180.
  radius = lat_data.radius
  frontrad = radius*sqrt(2*(1-cos(gamma))) ; + 1.
  frontstdv = abs( ( frontrad * abs(sin(gamma) * gammastdv) ) / ( 2. * (1 - cos(gamma))))
  frontrad +=1.
  
  gamma = lat_data.wave_peak[sp:ep].lat * !PI/180.
  gammastdv = lat_data.wave_peak[sp:ep].stdv * !PI/180.
  radius = lat_data.radius
  peakrad = radius*sqrt(2*(1-cos(gamma))) ;+ 1.
  peakstdv = abs( ( peakrad * abs(sin(gamma) * gammastdv) ) / ( 2. * (1 - cos(gamma))))
  peakrad +=1.
  
  gamma = lat_data.wave_backedge[sp:ep].lat * !PI/180.
  gammastdv = lat_data.wave_backedge[sp:ep].stdv * !PI/180.
  radius = lat_data.radius
  backrad = radius*sqrt(2*(1-cos(gamma))) ;+ 1.
  backstdv = abs( ( backrad * abs(sin(gamma) * gammastdv) ) / ( 2. * (1 - cos(gamma))))
  backrad +=1.
  
  rad_from_lat = {front:frontrad, frontstdv:frontstdv, peak:peakrad, peakstdv:peakstdv, back:backrad, backstdv:backstdv}

  return,rad_from_lat
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
  LEFTDIR=0
  RIGHTDIR=1
  dirstr=['Left','Right']
  
;-------------Restore the lateral data-------------
  lat_avg_fname=event.annuluspath+replace_string(event.annplot.analyzed.lateral.avg_savename,'WAV',wav)
  if not file_exist(lat_avg_fname) then begin
     print,''
     print,'File not present: '+lat_avg_fname
     print,'Run the lateral kinematics routine first.'
     return
  endif
  
  restore,lat_avg_fname
  lat_ind_arr=ind_arr
  lat_annulus_info=annulus_info
  latsp=lat_data[0,0].timefitrange[0]
  latep=lat_data[0,0].timefitrange[1]
  lattimejd=lat_data[0,0].time.jd
  numrad=n_elements(lat_data)/2. ;determine the number of radial measurements from the total, dividing by 2 directions

  
;-------------Restore the radial data-------------
  rad_avg_fname=event.annuluspath+replace_string(event.annplot.analyzed.radial.avg_savename,'WAV',wav)
  if not file_exist(rad_avg_fname) then begin
     print,''
     print,'File not present: '+rad_avg_fname
     print,'Run the radial kinematics routine first.'
     return
  endif
  restore,rad_avg_fname
  rad_ind_arr=ind_arr
  rad_annulus_info=annulus_info
  radsp=rad_data.timefitrange[0]
  radep=rad_data.timefitrange[1]
  radtimejd=rad_data.time.jd
  
;-------------Calculate the time (X) and radial position (Y) ranges of the plot------------- 
  miny={front:1.e3,peak:1.e3,back:1.e3}
  maxy={front:-1.e3,peak:-1.e3,back:-1.e3}
  minx=1.e12
  maxx=-1.e12
  
  ;First the radial data
  pos_array_front = rad_data.wave_frontedge[radsp:radep].rad
  pos_array_front_max = max(pos_array_front+rad_data.wave_frontedge[radsp:radep].stdv)
  pos_array_front_min = min(pos_array_front[where(pos_array_front ne 0.)]-rad_data.wave_frontedge[radsp:radep].stdv)
  if pos_array_front_min lt miny.front then miny.front = pos_array_front_min
  if pos_array_front_max gt maxy.front then maxy.front = pos_array_front_max
  
  pos_array_peak = rad_data.wave_peak[radsp:radep].rad
  pos_array_peak_max = max(pos_array_peak+rad_data.wave_peak[radsp:radep].stdv)
  pos_array_peak_min = min(pos_array_peak[where(pos_array_peak ne 0.)]-rad_data.wave_peak[radsp:radep].stdv)
  if pos_array_peak_min lt miny.peak then miny.peak = pos_array_peak_min
  if pos_array_peak_max gt maxy.peak then maxy.peak = pos_array_peak_max
  
  pos_array_back = rad_data.wave_backedge[radsp:radep].rad
  pos_array_back_max = max(pos_array_back+rad_data.wave_backedge[radsp:radep].stdv)
  pos_array_back_min = min(pos_array_back[where(pos_array_back ne 0.)]-rad_data.wave_backedge[radsp:radep].stdv)
  if pos_array_back_min lt miny.back then miny.back = pos_array_back_min
  if pos_array_back_max gt maxy.back then maxy.back = pos_array_back_max
  
  if radtimejd[radsp] lt minx then minx = radtimejd[radsp]
  if radtimejd[radep] gt maxx then maxx = radtimejd[radep]
  
  ;Second the lateral data
  for rr=0,numrad-1 do begin
     for dir=LEFTDIR,RIGHTDIR do begin
        rad_from_lat = convert_lateral_to_radial_positions(lat_data[rr,dir])
        lattimejd=lat_data[rr,dir].time.jd
        pos_array_front_max = max(rad_from_lat.front + rad_from_lat.frontstdv)
        pos_array_front_min = min(rad_from_lat.front[where(rad_from_lat.front ne 0.)]-rad_from_lat.frontstdv)
        if pos_array_front_min lt miny.front then miny.front = pos_array_front_min
        if pos_array_front_max gt maxy.front then maxy.front = pos_array_front_max
        
        pos_array_peak_max = max(rad_from_lat.peak + rad_from_lat.peakstdv)
        pos_array_peak_min = min(rad_from_lat.peak[where(rad_from_lat.peak ne 0.)]-rad_from_lat.peakstdv)
        if pos_array_peak_min lt miny.peak then miny.peak = pos_array_peak_min
        if pos_array_peak_max gt maxy.peak then maxy.peak = pos_array_peak_max
        
        pos_array_back_max = max(rad_from_lat.back + rad_from_lat.backstdv)
        pos_array_back_min = min(rad_from_lat.back[where(rad_from_lat.back ne 0.)]-rad_from_lat.backstdv)
        if pos_array_back_min lt miny.back then miny.back = pos_array_back_min
        if pos_array_back_max gt maxy.back then maxy.back = pos_array_back_max
        
        if lattimejd[latsp] lt minx then minx = lattimejd[latsp]
        if lattimejd[latep] gt maxx then maxx = lattimejd[latep]
     endfor
  endfor
  xrnge=[minx,maxx]
  
  
  set_plot,'x'
  plotpng=event.annuluspath+'compared_rad_lat_kinematics_pos_'+strtrim(event.label)+'.png'
  ploteps=event.annuluspath+'compared_rad_lat_kinematics_pos_'+strtrim(event.label)+'.eps'
  
;-------------Window creation--------------
  npanels=3
  if keyword_set(ps) then begin
     ;!p.position=[0.15,0.12,0.93,0.9]
     !p.font=0
     set_plot,'ps'
     device,file=ploteps,/inches,xsize=9,ysize=12,$
            /encaps,/color,/helvetica
     !p.multi =[0, npanels, 1]
  endif else begin
     wdef, 1,1000, 1200
     loadct,0,/silent
     tvlct,rr,gg,bb,/get
     tvlct,reverse(rr),reverse(gg),reverse(bb)
     !p.multi =[0, npanels, 1]
     !p.font=-1
  endelse
  
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
  
;----------------------PLOT THE FRONT POSITIONS---------------------------
  !p.position = [x0,y1-1*panelysize,x0+panelxsize,y1-0*panelysize]
  yrnge = [miny.front,maxy.front]
  loadct, 0, /silent
  plot, radtimejd[radsp:radep], rad_data.wave_peak[radsp:radep].rad,/xstyle, thick = linethick, $
        title= "Wave positions, "+strtrim(event.label,2),$
        charsize = 3, color = 0, background = 255, ytitle ="Radial position [R!Ds!N]",$
        xrange = xrnge, yrange = yrnge, /nodata, /ynozero, /ystyle, xthick=linethick, ythick=linethick, XTICKFORMAT="(A1)", $
        XTICKUNITS = ['Time']
  loadct, 13, /silent
  ;From radial measurements
  oplot, radtimejd[radsp:radep], rad_data.wave_frontedge[radsp:radep].rad, thick=linethick,color = crad
  oplot, radtimejd[radsp:radep], rad_data.wave_frontedge[radsp:radep].rad,psym=sym(1),symsize=0.8,color=crad
  oploterr, radtimejd[radsp:radep], rad_data.wave_frontedge[radsp:radep].rad, rad_data.wave_frontedge[radsp:radep].stdv, $
            errcolor = crad, errthick = 4, /noconnect
  xyouts, !p.position[2]-0.22, !p.position[1]+yoffset, 'Radial ', $
          color = crad, /normal, charsize = 1.3, charthick = 1.5
  
  ;From lateral measurements
  for rr=0,numrad-1 do begin
     for dir=LEFTDIR,RIGHTDIR do begin
        rad_from_lat = convert_lateral_to_radial_positions(lat_data[rr,dir])
        oplot, lattimejd[latsp:latep], rad_from_lat.front, thick=linethick,color = clat[rr,dir]
        oplot, lattimejd[latsp:latep], rad_from_lat.front,psym=sym(1),symsize=0.8,color=clat[rr,dir]
        oploterr, lattimejd[latsp:latep], rad_from_lat.front, rad_from_lat.frontstdv, $
                  errcolor = clat[rr,dir], errthick = 4, /noconnect
        ccc=dir+rr*2+1
        xyouts, !p.position[2]-0.22, !p.position[1]+yoffset+ccc*strdy, 'Lat/'+dirstr[rr]+', '+strtrim(string(lat_data[rr,dir].radius,format='(f5.2)'),2)+' R!Ds!N', $
                color = clat[rr,dir], /normal, charsize = 1.3, charthick = 1.5
     endfor
  endfor
        
  xyouts, !p.position[0]+0.012, !p.position[3]-yoffset-strdy, 'Front', color = crad, /normal, charsize = 2, charthick = 2

  
;----------------------PLOT THE PEAK POSITIONS---------------------------
  !p.position = [x0,y1-2*panelysize,x0+panelxsize,y1-1*panelysize]
  yrnge = [miny.peak,maxy.peak]
  loadct, 0, /silent
  plot, radtimejd[radsp:radep], rad_data.wave_peak[radsp:radep].rad,/xstyle, thick = linethick, $
        charsize = 3, color = 0, background = 255, ytitle ="Radial position [R!Ds!N]",$
        xrange = xrnge,yrange = yrnge,/nodata,/ynozero,/ystyle, xthick=linethick,ythick=linethick, XTICKFORMAT="(A1)", $
        XTICKUNITS = ['Time']
  loadct, 13, /silent
  
  ;From radial measurements
  oplot, radtimejd[radsp:radep],  rad_data.wave_peak[radsp:radep].rad, thick=linethick,color = crad
  oplot, radtimejd[radsp:radep], rad_data.wave_peak[radsp:radep].rad,psym=sym(1),symsize=0.8,color=crad
  oploterr, radtimejd[radsp:radep], rad_data.wave_peak[radsp:radep].rad, rad_data.wave_peak[radsp:radep].stdv, $
            errcolor = crad, errthick = 4, /noconnect

  ;From lateral measurements
  for rr=0,numrad-1 do begin
     for dir=LEFTDIR,RIGHTDIR do begin
        rad_from_lat = convert_lateral_to_radial_positions(lat_data[rr,dir])
        oplot, lattimejd[latsp:latep],  rad_from_lat.peak, thick=linethick,color = clat[rr,dir]
        oplot, lattimejd[latsp:latep], rad_from_lat.peak,psym=sym(1),symsize=0.8,color=clat[rr,dir] 
        oploterr, lattimejd[latsp:latep], rad_from_lat.peak, rad_from_lat.peakstdv, $
                  errcolor = clat[rr,dir], errthick = 4, /noconnect
        
     endfor
  endfor
  xyouts, !p.position[0]+0.012, !p.position[3]-yoffset-strdy, 'Peak', color = crad, /normal, charsize = 2, charthick = 2
  
  
;----------------------PLOT THE BACK POSITIONS---------------------------
  !p.position = [x0,y1-3*panelysize,x0+panelxsize,y1-2*panelysize]
  yrnge = [miny.back,maxy.back]
  loadct, 0, /silent
  plot, radtimejd[radsp:radep], rad_data.wave_peak[radsp:radep].rad,/xstyle, thick = linethick, $
        charsize = 3, color = 0, background = 255, ytitle ="Radial position [R!Ds!N]",$
        xrange = xrnge, yrange = yrnge, /nodata, /ynozero,/ystyle, xthick=linethick,ythick=linethick, XTICKFORMAT='LABEL_DATE', $
        XTICKUNITS = ['Time']
  loadct, 13, /silent

  ;From radial measurements
  oplot, radtimejd[radsp:radep], rad_data.wave_backedge[radsp:radep].rad, thick=linethick, color = crad
  oplot, radtimejd[radsp:radep], rad_data.wave_backedge[radsp:radep].rad,psym=sym(1),symsize=0.8,color=crad
  oploterr, radtimejd[radsp:radep], rad_data.wave_backedge[radsp:radep].rad, rad_data.wave_backedge[radsp:radep].stdv, $
            errcolor = crad, errthick = 4, /noconnect
  
  ;From lateral measurements
  for rr=0,numrad-1 do begin
     for dir=LEFTDIR,RIGHTDIR do begin
        rad_from_lat = convert_lateral_to_radial_positions(lat_data[rr,dir])
        oplot, lattimejd[latsp:latep], rad_from_lat.back, thick=linethick, color = clat[rr,dir]
        oplot, lattimejd[latsp:latep], rad_from_lat.back,psym=sym(1),symsize=0.8,color=clat[rr,dir]
        oploterr, lattimejd[latsp:latep], rad_from_lat.back, rad_from_lat.backstdv, $
                  errcolor = clat[rr,dir], errthick = 4, /noconnect
     endfor
  endfor
  xyouts, !p.position[0]+0.012, !p.position[3]-yoffset-strdy, 'Back', color = crad, /normal, charsize = 2, charthick = 2
  loadct, 0, /silent

  
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
