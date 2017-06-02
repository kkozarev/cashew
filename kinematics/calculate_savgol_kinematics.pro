pro test_calculate_savgol_kinematics
;A program to test calculate_savgol_kinematics

  ;Get the example time array
  n = 30 ; number of points
  dt = 12.  ; sampling interval
  time = dt*FINDGEN(n)
  
  ;Calculate the example function, representative of EUV wave dynamics
  y0=150000.
  v0=300.
  a=0.55
  input_array = y0 + v0 * (time - time[0]) + 0.5 * a * (time - time[0])^2 + 50*RANDOMN(127, n)

  
  calculate_savgol_kinematics, time, input_array, distance=distance, speed=speed, acceleration=acceleration,/plot
  
  
end


pro calculate_savgol_kinematics, time, input_array, distance=distance, speed=speed, acceleration=acceleration, plot=plot
;PURPOSE:
; Calculates the time-dependent position, speed, and acceleration of input_array.
;
;CATEGORY:
; AIA/Kinematics
;
;INPUTS:
;      TIME - time array, must be in seconds!
;      INPUT_ARRAY - the kinematic positions array
;      
;
;KEYWORDS:
;      DISTANCE - if supplied, returns in it the Savitzky-Golay-smoothed array
;      SPEED - if supplied, returns in it the time-dependent speed
;      ACCELERATION if supplied, returns in it the time-dependent acceleration
;      PLOT - if set, plots the positions, speeds, and accelerations on three different plots
;OUTPUTS:  
; 
;DEPENDENCIES:
;
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 02/2016
  
  
  n=n_elements(time)
  dt=time[1]-time[0]
  origarr=input_array
  sghw=ceil(n/10.)              ; Savitzky-Golay Half Window - set to 10% of the number of points, but at least 3.
  if sghw lt 4 then sghw=4
  nextend=10 ;how many factors of sghw to extend the array to get rid of edge effects - at least 2
  if nextend lt 2 then nextend=2
  bign=n+2*nextend*sghw           ;
  
  degree=2 ;The degree of the fitting polynomial
  
  ;Regularize the grid and use interpol to fill the empty locations.
  regtime=findgen(n)*max(time)/(n-1)
  input_array=interpol(input_array,time,regtime)
  time=regtime
  
  
  ;First, do a second-order poly-fit to the beginning and ending of the time series.
  ;Fit the first [sghw] points
  fit=poly_fit(time[0:sghw-1],input_array[0:sghw-1],2)
  begy0=fit[0]
  begv0=fit[1]
  bega=fit[2]
  ;Make extrapolation
  begtime=reverse(-dt*(findgen(nextend*sghw)+1))
  begfit=input_array[0]+begv0*(begtime)+bega*begtime^2
  
  ;Fit the last [sghw] points
  fit=poly_fit(time[n-sghw:n-1]-time[n-sghw],input_array[n-sghw:n-1],2) 
  endy0=fit[0]
  endv0=fit[1]
  enda=fit[2]
  ;Make extrapolation
  endtime=time[n-sghw]+dt*(findgen(nextend*sghw))
  endfit=endy0+endv0*(endtime-endtime[0])+enda*(endtime-endtime[0])^2
  endtime=endtime[sghw:n_elements(endfit)-1]
  endfit=endfit[sghw:n_elements(endfit)-1]
  
  

  ;Create a new array that includes the extrapolated points. Interpolate on it.  
  fittime=[begtime,time,endtime]
  bigy1=[begfit,input_array,endfit]
  
  ;Make the smooth array dist
  order=0
  distSavgol=SAVGOL(sghw, sghw, order, degree)
  fulldist=CONVOL(bigy1, distSavgol, /EDGE_TRUNCATE)
  distance=fulldist[nextend*sghw:bign-nextend*sghw-1]
  
  ;Calculate the time-dependent speeds
  order=1
  speedSavgol=SAVGOL(sghw, sghw, order, degree)*(FACTORIAL(order)/(dt^order))
  speed=CONVOL(fulldist, speedSavgol, /EDGE_TRUNCATE)
  speed=speed[nextend*sghw:bign-nextend*sghw-1]
  
  ;Calculate the time-dependent accelerations
  order=2
  accelSavgol= SAVGOL(sghw, sghw, order, degree)*(FACTORIAL(order)/(dt^order))
  accel=CONVOL(fulldist, accelSavgol, /EDGE_TRUNCATE)
  acceleration=accel[nextend*sghw:bign-nextend*sghw-1]

  if keyword_set(plot) then begin
     
     ; Display the first plots and define the layout
     tvlct,rr,gg,bb,/get
     tvlct,rr,gg,bb
     !p.multi=0
     !p.background=255
     !p.color=0
     !p.thick=2
     !x.thick=2
     !y.thick=2
     !p.charsize=2
     !p.charthick=2
     !p.position=[0.15,0.15,0.9,0.9]
     wdef,0,1000,600
     
     PLOT,time, origarr,/ynozero,title='Distance Savitzky-Golay test',ytit='Distance',xtit='Time [sec]',psym=2,symsize=2
     oplot,time,distance,linestyle=2
     
     wdef,1,1000,600
     plot, time, speed,/ynozero,title='Speed Savitzky-Golay test',ytit='Speed',xtit='Time [sec]'
     
     wdef,2,1000,600
     
     plot, time, acceleration,/ynozero,title='Acceleration Savitzky-Golay test',ytit='Acceleration',xtit='Time [sec]',ystyle=1
     
  endif
  
END
