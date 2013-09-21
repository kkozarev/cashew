; Routine to find local maxima and use to detect peaks in intensity
; profiles.
;
;+ 
; REQUIRED INPUTS:
; X -> Abscissa of data. Generally taken as time/distance.
; Y -> Ordinate of data. Intensity array at given distance/time.
; 
; OPTIONAL INPUTS:
; MAX_ITER -> Maximum number of iterations to be performed. If not
;             provided, max_iter taken as 2.
; INDICES -> Returns an array containing the indices of the peak limits for each
;            peak. 
; FIT_LINE -> Returns an array containing the line fitted to each
;             individual peak.
; ORDERED -> If set then PEAKS, INDICES and FIT_LINE results are
;            returned in order of increasing centroid distance from
;            source point.
;
; NOTES:
;    Code designed to detect peaks in intensity profiles to ensure
;    that individual flare and wave peaks are treated separately. Code
;    originally tested using simulated data, with testing then
;    beginning on real data. Code may not be applicable to non-CBF
;    intensity profiles; more work required.
;
; HISTORY:
;    Written by David Long 2011-Feb-15
;
;    Revision 1.1 David Long 2011-Feb-28
;    - Fixed bug producing errors when faced with only one peak in
;      int. prof. Additional rows up to max_iter are now filled with
;      1s.
;
;-

function peak_finder, x, y, indices = indices, max_iter = max_iter, fit_line = fit_line, $
                      ordered = ordered, errs = errs, confidence_level = confidence_level

; Check if max_iter contains a valid number. If not, define max_iter.

  chk_iter = n_elements(max_iter)
  if not keyword_set(confidence_level) then confidence_level=0.3

  if total(chk_iter) eq 0 then max_iter = 2 else max_iter = max_iter

  peaks = dblarr(3,1)
  errs = dblarr(3,1)
  indices = dblarr(2,1)
  fit_line = dblarr(max(x),1)

; Set maximum intensity at 100 then only examine intensity values
; between 0 and 100.
  if (max(y) ge 10000.) then y[where(y ge 10000)] = 10000.
  pbd_int_0 = ((y GE 0) AND (y LE 10000))*y
  help,reform(pbd_int_0)
; Find moment of intensity data. This is used to define lower bound on
; peak value.

  moment = moment(pbd_int_0)
  intensity = pbd_int_0

  count = 0
  while ((max(intensity) gt 1.5*moment[0]) and (count lt max_iter)) do begin
; Find maximum value of intensity profile

     m = max(intensity, index)
     print,m,index
     i_upr = index
     i_lwr = index
     n = n_elements(intensity - index)
 
; Moving from max to left and right, while value is greater than
; confidence_level*max and point index is within array then continue.
    
     while (intensity[i_upr+1] ge confidence_level*m) and (i_upr lt n) do i_upr += 1
     while (intensity[i_lwr-1] ge confidence_level*m) and (i_lwr ne 0) do i_lwr -= 1

     ind = [i_lwr, i_upr]

; Weight peak values to ensure accurate fitting
     
     weights = replicate(1., n_elements(intensity))
     
     weights[i_lwr:i_upr] = 100.
 
; Peak must be greater than 0 and less than peak of data+5 to allow
; some wiggle room, centroid position must be within peak
; limits, sigma must be less than width of peak limits.
    
     pi = replicate({fixed:0, limited:[0,0], limits:[0.D,0.D]},3)
     pi(0).limited(0) = 1
     pi(0).limits(0) = 0
     pi(0).limited(1) = 1
     pi(0).limits(1) = max(intensity)+5.
     pi(1).limited(0) = 1	
     pi(1).limits(0) = x[i_lwr-1]
     pi(1).limited(1) = 1	
     pi(1).limits(1) = x[i_upr+1]
     pi(2).limited(0) = 1
     pi(2).limits(0) = 0.
     pi(2).limited(1) = 1
     pi(2).limits(1) = x[i_upr] - x[i_lwr]

; Fit Gaussian curve to data
     
     fit = mpfitpeak(x, y, a, weights = weights, estimates = [max(intensity), x[index], (x[i_upr]-x[i_lwr])/2.], $
                       nterms = 3, /positive, parinfo = pi, perror = perror)

; Create resulting fit line

     x_vals = dindgen(max(x))

     f_line = gauss1(x_vals, shift(a, -1), /peak)

; Set peak values equal to zero and repeat.

     dat = intensity
     
     dat[i_lwr:i_upr] = dat[i_lwr:i_upr] - dat[i_lwr:i_upr]
     
     intensity = dat

; Add fit values, indices and fit line to respective arrays
     
; Error catching to ensure that a valid number is returned and routine
; does not crash.

     if (mean(finite(f_line)) ne 1) then f_line = replicate(1., max(x))
     if (mean(finite(fit)) ne 1) then perror = replicate(10., 3)

; Add fit values to array

     if (count eq 0) then peaks[*,count] = a else peaks = [[peaks], [a]]
     if (count eq 0) then errs[*,count] = perror else errs = [[errs], [perror]]
     if (count eq 0) then indices[*,count] = ind else indices = [[indices], [ind]]
     if (count eq 0) then fit_line[*,count] = f_line else fit_line = [[fit_line], [f_line]]

; Repeat.

     count += 1

  endwhile

; If /ordered set then order arrays of peaks, indices and fit line in
; terms of increasing distance from source point.

  if keyword_set(ordered) then begin
     s_ind = reverse(sort(peaks[1,*]))
     for i = 0, 2 do peaks[i,*] = peaks[i,s_ind]
     for i = 0, 2 do errs[i,*] = errs[i,s_ind]
     for i = 0, 1 do indices[i,*] = indices[i,s_ind]
     for i = 0, 1 do fit_line[i,*] = fit_line[i,s_ind]
  endif     

  sz_peaks = size(peaks)
  sz_indices = size(indices)
  sz_fit = size(fit_line)

  if (n_elements(sz_peaks) eq 4) then begin
     count = 1
     while (count lt max_iter) do begin
        peaks = [[peaks],[replicate(1., 3.)]]
        errs = [[errs],[replicate(1., 3.)]]
        indices = [[indices],[replicate(1., 2)]]
        fit_line = [[fit_line],[replicate(1., max(x))]]
        count+=1
     endwhile
  endif else begin
     while (sz_peaks[2] lt max_iter) do begin
        peaks = [[peaks],[replicate(1., 3.)]]
        errs = [[errs],[replicate(1., 3.)]]
        indices = [[indices],[replicate(1., 2.)]]
        fit_line = [[fit_line],[replicate(1., max(x))]]
        sz_peaks = size(peaks)
     endwhile
  endelse

  return, peaks

end
