pro test_spiral
;Little program testing how to draw a spiral.
  resolve_routine,'sym',/either,/compile_full_file
  winsize=1000
  center=[winsize/2.,winsize/2.]
  a=100 ;initial radius, in pixels
  b=4. ;how far apart the spiral arms are, in what quantity?
  start_angle=0. ;Starting angle, counting from 12 o'clock.
  nturns=12


  !P.color=0
  !p.background=255
  wdef,0,winsize
  loadct,0,/silent
  nmonths=12.
  months=["January","February","March","April","May","June",$
          "July","August","September","October","November","December"]

;  for nn=0,0 do begin
  spiral,start_angle,nturns,a,b,center,x_spiral,y_spiral ;,/ccw
  maxrad=ceil(max(sqrt((x_spiral-center[0])^2+(y_spiral-center[1])^2)))
  npoints=n_elements(x_spiral)
;     if nn eq 0 then begin
;        xrange=[min(x_spiral),max(x_spiral)]
;        yrange=[min(y_spiral),max(y_spiral)]
;     endif
  xrange=[0,winsize-1]
  yrange=[0,winsize-1]
  plot,x_spiral,y_spiral,$
       xrange=xrange,$
       yrange=yrange,$
       ystyle=1,$
       xstyle=1,$
       thick=2
;     wait,0.5
;  endfor
  
;Next thing would be to figure out how to split the spiral and plot
;single segments of it, in different colors. 12. is the number of
;months
  loadct,13,/silent
  nseg=nturns*nmonths*1.
  segpts=npoints/(nseg*1.0)
  x_seg=x_spiral[0:segpts-2]
  y_seg=y_spiral[0:segpts-2]
  oplot,x_seg,y_seg,thick=6,color=10
  
  for ss=0,nseg-1 do begin
     st=ss*segpts
     en=(ss+1)*segpts-2
     ;col=10+ss/(nseg-2)*245.
     if ss eq 0 then col=abs(randomn(10L,nmonths))*245.
     
     if en ge npoints then en=npoints-2
     x_seg=x_spiral[st:en]
     y_seg=y_spiral[st:en]
     oplot,x_seg,y_seg,thick=10,color=col[ss mod nmonths]
  endfor
  

  ;Write out the month names
  for mm=0.,nmonths-1 do begin
     ;The position angle for the months is formed by three parts:
     ;!PI/2. is the 90-degree CCW rotation from the start (positive X) to the positive Y
     ;2.*!PI*(1.-mm/nmonths) is the angular rotation for each month, in the CW by 2PI/nmonths
     ;-!PI/nmonths is a slight correction to bring the word to the center of the segment
     pos_angle=!PI/2. + 2.*!PI*(1.-mm/nmonths) - !PI/nmonths
     ;The text orientation angle is the position angle plus a 90-deg CW rotation
     orient_angle=(pos_angle-!PI/2.)*180./!PI

     ;The radius of the circle, on which the text is positioned.
     textrad=maxrad+20
     
     xpos=center[0]+(textrad)*cos(pos_angle)
     ypos=center[0]+(textrad)*sin(pos_angle)
     xyouts,xpos,ypos,months[mm],orientation=orient_angle,charsize=2.6,charthick=3,alignment=0.5
  endfor
  loadct,0,/silent
  
  
  
;Next test is to plot the spiral with a shading, that is with
;connected segments in similar but slightly different colors.
  wdef,1,winsize
  loadct,9,/silent
  nseg=nturns*nmonths*1.
  segpts=npoints/(nseg*1.0)
  x_seg=x_spiral[0:segpts-2]
  y_seg=y_spiral[0:segpts-2]
  oplot,x_seg,y_seg,thick=6,color=10
  for ss=0,nseg-1 do begin
     st=ss*segpts
     en=(ss+1)*segpts-2
     col=10+ss/(nseg-1)*240.
     ;if ss eq 0 then col=abs(randomu(10L,nmonths,/uniform))*245.
     
     if en ge npoints then en=npoints-2
     x_seg=x_spiral[st:en]
     y_seg=y_spiral[st:en]
     oplot,x_seg,y_seg,thick=10,color=col
  endfor
  ;Write out the month names
  for mm=0.,nmonths-1 do begin
     ;The position angle for the months is formed by three parts:
     ;!PI/2. is the 90-degree CCW rotation from the start (positive X) to the positive Y
     ;2.*!PI*(1.-mm/nmonths) is the angular rotation for each month, in the CW by 2PI/nmonths
     ;- 0.5*!PI/nmonths is a slight correction to bring the word to the center of the segment
     pos_angle=!PI/2.+2.*!PI*(1.-mm/nmonths) - !PI/nmonths
     ;The text orientation angle is the position angle plus a 90-deg CW rotation.
     orient_angle=(pos_angle-!PI/2.)*180./!PI

     ;The radius of the circle, on which the text is positioned.
     textrad=maxrad+20
     
     xpos=center[0]+(textrad)*cos(pos_angle)
     ypos=center[0]+(textrad)*sin(pos_angle)
     xyouts,xpos,ypos,months[mm],orientation=orient_angle,charsize=2.6,charthick=3,alignment=0.5
  endfor
  loadct,0,/silent

end


pro spiral,start_angle,nturns,a,b,center,x_spiral,y_spiral,ccw=ccw,npoints=npoints
;Draw a very customized spiral
  if not keyword_set(npoints) then npoints=10000
  ;outputs are x_spiral,y_spiral
  x_spiral=dblarr(npoints)
  y_spiral=dblarr(npoints)
;Rotate the start angle to 12 o'clock, turn into radians
  start_angle+=90
  start_angle*=!PI/180.
  for i=0L,npoints-1L do begin
     ;angle is such that at i=npoints-1 the angle is start_angle+N_turns*2*!PI
     angle=start_angle+i*nturns*2.*!PI/(npoints-1.)

     x_spiral[i]=center[0]-(a+b*angle)*cos(angle)
     if keyword_set(ccw) then $
        x_spiral[i]=center[0]+(a+b*angle)*cos(angle)
     y_spiral[i]=center[1]+(a+b*angle)*sin(angle)
  endfor

end
