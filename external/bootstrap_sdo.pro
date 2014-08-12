pro bootstrap_sdo, distance, time, error=error, fit_line, p1, p2, p3, s1, s2, s3, _extra=_extra,linear=linear
;+
; NAME:
;       bootstrap_sdo
;
; PURPOSE:
;       Perform boostrapping analysis on an array of distance-time measurements  
;
; EXPLANATION:
;	Given a model and an array of distance and time values, uses a
;	residual-resampling bootstrapping technique to return the
;	optimal fit values.
;
; CALLING SEQUENCE:
;       bootstrap_sdo, distance, time, error=error, p1, p2, p3, s1,
;       s2, s3, fit_line
;
; INPUTS:
;	distance = the array of distance values (y-axis)
;       time = the array of time values (x-axis)
;       error = the array of distance-associated error values
;               
; OUTPUTS:
;	p1,p2,p3 = mean fit values
;       s1,s2,s3 = standard deviation associated with mean fit
;                  values
;       fit_line = the best-fit line to the given data-set
;
;
; AUTHOR:
;	David Long 10-July-2010 
;        - Stand-alone code taken from original all-inclusive
;          bootstrapping routines
;-
if not keyword_set(error) then error=fltarr(n_elements(time))+1.0e-33

; Bootstrap noisy data

  a_dist = size(distance, /n_elements)
  
  arr_size = 10000.0d
  
  res = dblarr(3, arr_size)
  
; Original fit to data
  
  x = dindgen(max(time))
  
  h_error = error

  h_error = replicate(1., a_dist)
  
  ;original model when Dave sent me this code (Kamen)
  ;fit_model = 'p[1] * (x - p[0]) + (1./2.) * p[2] * (x - p[0])^2.'

  fit_model = 'p[0] + p[1] * (x) + (1./2.) * p[2] * (x)^2.'
  if keyword_set(linear) then fit_model = 'p[0] + p[1] * (x)'
  
  fit = mpfitexpr(fit_model, time, distance, h_error, [0., 0.2, 0.00005], perror=perror, $
                  bestnorm = bestnorm, /quiet, _extra=_extra)
  
  ;original model when Dave sent me this code (Kamen)
  ;yfit = fit[1]*(time-fit[0]) + (1./2.)*fit[2]*(time-fit[0])^2.
  
  yfit = fit[0] + fit[1]*(time) + (1./2.)*fit[2]*(time)^2.
  
  if keyword_set(linear) then yfit = fit[0] + fit[1]*(time)
  
  x_r = time
  
; Loop over n! iterations to bootstrap
  
  for i = 0, arr_size-1 do begin
     
; Calculate residuals
     
     e = distance - yfit

; Generate number of data points random numbers between 0 and 100 from uniform distribution 
     
     ran = rand_ind(a_dist)

; Create array of 1 and -1 to multiply ran array by to fill gaps in resulting array
     
     unit_array = randomn(seed, a_dist, /normal)
     unit_arr = unit_array/abs(unit_array)
     
; Randomly reassign the residuals
     
     er = e[ran] * unit_arr
     
; Make new data with random residuals
     
     y_r = distance + er
     
; New fit and store the results
     
     fit = mpfitexpr(fit_model, x_r, y_r, h_error, [0., 0.2, 0.00005], perror=perror, $
                     bestnorm = bestnorm, /quiet)
     res[*,i] = fit

  endfor
  
; Calculate the moments of the results arrays for m and c
  
  p1 = moment(res[0,*], sdev=s1)
  p2 = moment(res[1,*], sdev=s2)
  p3 = moment(res[2,*], sdev=s3)
  
; Calculate fitted line for r(t) vs t plot.
  
  ;original model when Dave sent me this code (Kamen)
  ;fit_line = p2[0]*(x - p1[0]) + (1./2.)*p3[0]*(x - p1[0])^2.
  
  fit_line = p1[0] + p2[0]*(x) + (1./2.)*p3[0]*(x)^2.
  if keyword_set(linear) then fit_line = p1[0] + p2[0]*(x)
  
end
