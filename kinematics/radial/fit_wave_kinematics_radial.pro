;+============================================================================
pro fit_wave_kinematics_radial,rad_data,ind_arr,front=front,peak=peak,back=back
;PERFORM THE TWO KINDS OF KINEMATICS FITTING PROCEDURES - 2nd ORDER
;POLYNOMIAL AND SAVITZKY-GOLAY!
  if (not keyword_set(front)) and (not keyword_set(peak)) and (not keyword_set(back)) then begin
     print,'No fitting done. Rerun with one of keywords /front, /peak, /back. Quitting.'
     return
  endif
  ;This is the time to use for the fitting, in seconds
  sp=rad_data.timefitrange[0]
  ep=rad_data.timefitrange[1]
  time_good=rad_data.time[sp:ep].relsec-rad_data.time[sp].relsec
  dtsec=(rad_data.time[1].relsec-rad_data.time[0].relsec)
  RSUN=ind_arr[0].rsun_ref/1000. ;Solar radius in km.
  
    ;The info to pass to the position fitting routine for the fitting parameters
  parinfo = replicate({value:0.D, limited:[0,0], limits:[0.D,0.D]}, 3)
  parinfo[0].value=1.0;*DIST_FACTOR
  parinfo[0].limited=[1,1]
  parinfo[0].limits=[1.01,1.2];*DIST_FACTOR
  
  parinfo[1].value=100.
  parinfo[1].limited=[1,1]
  parinfo[1].limits=[0.0,2000.0]
  parinfo[2].value=10.
  parinfo[2].limited[0]=1
  parinfo[2].limits[0]=-1500.0
  
  if keyword_set(front) then begin
     print,''
     print,'Fitting  a second-order polynomial and a Savitzky-Golay filter to the wave front edge positions...' 
     ;Do the fitting of the front edge positions
     dist=reform(rad_data.wave_frontedge[sp:ep].rad)
  endif
  if keyword_set(peak) then begin
     print,''
     print,'Fitting a second-order polynomial and a Savitzky-Golay filter to the wave peak positions...' 
     ;Do the fitting of the front edge positions
     dist=reform(rad_data.wave_peak[sp:ep].rad)
  endif
  if keyword_set(back) then begin
     print,''
     print,'Fitting a second-order polynomial and a Savitzky-Golay filter to the wave back edge positions...' 
     ;Do the fitting of the front edge positions
     dist=reform(rad_data.wave_backedge[sp:ep].rad)
  endif
  
;Check if the values are zero or if there are repeating values
  zeros=where(dist eq 0,complement=nonzero)
  if zeros[0] ne -1 then begin
     dist=dist[nonzero]
     time_good=time_good[nonzero]
  endif
  ;repeating=where(dist eq shift(dist,1),complement=notrepeating)
  ;if repeating[0] ne -1 then begin
  ;   dist=dist[notrepeating]
  ;   time_good=time_good[notrepeating]
  ;endif
  
  ;This is very important - record the overall time index range for this fit.
  ;if keyword_set(front) then begin
  ;   rad_data.kinfittimerange.front=sp+minmax(notrepeating)
  ;   stfit=rad_data.kinfittimerange.front[0]
  ;   etfit=rad_data.kinfittimerange.front[1]
  ;endif
  ;if keyword_set(peak) then begin
  ;   rad_data.kinfittimerange.peak=sp+minmax(notrepeating)
  ;   stfit=rad_data.kinfittimerange.peak[0]
  ;   etfit=rad_data.kinfittimerange.peak[1]
  ;endif
  ;if keyword_set(back) then begin
  ;   rad_data.kinfittimerange.back=sp+minmax(notrepeating)
  ;   stfit=rad_data.kinfittimerange.back[0]
  ;   etfit=rad_data.kinfittimerange.back[1]
  ;endif
  
  ;This is very important - record the overall time index range for this fit.
  if keyword_set(front) then begin
     rad_data.kinfittimerange.front=sp+minmax(nonzero)
     stfit=rad_data.kinfittimerange.front[0]
     etfit=rad_data.kinfittimerange.front[1]
  endif
  if keyword_set(peak) then begin
     rad_data.kinfittimerange.peak=sp+minmax(nonzero)
     stfit=rad_data.kinfittimerange.peak[0]
     etfit=rad_data.kinfittimerange.peak[1]
  endif
  if keyword_set(back) then begin
     rad_data.kinfittimerange.back=sp+minmax(nonzero)
     stfit=rad_data.kinfittimerange.back[0]
     etfit=rad_data.kinfittimerange.back[1]
  endif
  
  if n_elements(dist) le 4 then begin
     print, "Not enough data to smooth, exiting..."
     return
  endif
  newtimegood=time_good
  bootstrap_sdo,dist*RSUN,newtimegood,fit_line, p1, p2, p3, s1, s2, s3,parinfo=parinfo
  wave_fits=p1[0] + p2[0] * (time_good)+ 0.5 * p3[0] * (time_good)^2
  wave_fits/=RSUN
  if keyword_set(front) then begin
     rad_data.fitparams[0].front=p1[0]
     rad_data.fitparams[1].front=p2[0]
     rad_data.fitparams[2].front=p3[0]
     rad_data.fitsigma[0].front=s1[0]
     rad_data.fitsigma[1].front=s2[0]
     rad_data.fitsigma[2].front=s3[0]
  endif
  if keyword_set(peak) then begin
     rad_data.fitparams[0].peak=p1[0]
     rad_data.fitparams[1].peak=p2[0]
     rad_data.fitparams[2].peak=p3[0]
     rad_data.fitsigma[0].peak=s1[0]
     rad_data.fitsigma[1].peak=s2[0]
     rad_data.fitsigma[2].peak=s3[0]
  endif
  if keyword_set(back) then begin
     rad_data.fitparams[0].back=p1[0]
     rad_data.fitparams[1].back=p2[0]
     rad_data.fitparams[2].back=p3[0]
     rad_data.fitsigma[0].back=s1[0]
     rad_data.fitsigma[1].back=s2[0]
     rad_data.fitsigma[2].back=s3[0]
  endif
  
  ;DO SAVITZKY-GOLAY FITS!
  nt=n_elements(dist)
  if nt lt 10 then sgfpix=2 else sgfpix=4
  ;SPEED
  order=1
 
  sgfil_v = SAVGOL(sgfpix, sgfpix, order, 4)*(FACTORIAL(order)/(dtsec^order))
  if n_elements(sgfil_v) gt nt then begin
     print,'Unable to perform Savitzky-Golay fits - too few data points.'
     return
  endif
  speed=CONVOL(dist*RSUN, sgfil_v, /EDGE_TRUNCATE)
  speed=speed[0:nt-1]
  
  ;ACCELERATION
  order=2
  sgfil_a = SAVGOL(sgfpix, sgfpix, order, 4)*(FACTORIAL(order)/(dtsec^order))
  accel=CONVOL(dist*RSUN*1000., sgfil_a, /EDGE_TRUNCATE)
  accel=accel[0:nt-1]
  
  if keyword_set(front) then begin
     rad_data.savgolfits.front[stfit:etfit].speed=speed
     rad_data.savgolfits.front[stfit:etfit].accel=accel
  endif 
  if keyword_set(peak) then begin
     rad_data.savgolfits.peak[stfit:etfit].speed=speed
     rad_data.savgolfits.peak[stfit:etfit].accel=accel
  endif
  if keyword_set(back) then begin
     rad_data.savgolfits.back[stfit:etfit].speed=speed
     rad_data.savgolfits.back[stfit:etfit].accel=accel
  endif 
end
;-============================================================================
;------------------------------
