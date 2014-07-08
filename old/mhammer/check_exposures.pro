pro check_exposures, good_exposures_bool, good_exposures, DATA_FILE = data_file, SUBDATA = subdata, SUBINDEX = subindex, DELETE = delete, W_DELETE = w_delete
; Parameters:
; good_exposures: The 'return array' containing booleans indicating
; which exposures are good and which are useless
; data_file: Data to be restored including subdata and subindex
; subdata: The subdata from the restored file
; subindex: The subindex from the restored file
;
; Note: Your input should be one of the following:
; (1) DATA_FILE
; (2) SUBDATA and SUBINDEX
; If you put neither or both, it makes no sense. Don't do it.
; Do (1) if you haven't restored the data yet.
; Do (2) if you have already restored the data.
;
; delete: If delete is selected, bad_exposures will be deleted from
; only the wavelength of the data_file
; w_delete: If w_delete is selected, bad_exposures will be deleted
; from all of the wavelength, not just that of the data_file 
;
; What This Program Does:
; This program checks the exposure times of the frames in an image
; sequence and identifies which frames had an unusually short exposure
; time due to a crazy flare going off or some other reason.
; This program may be used to delete those frames from the subdata and
; subindex, which would require more careful handling of the times of
; each of the frames since they would no longer be at equally spaced
; time intervals. Another possibility is that the frames would be at
; a constant time interval if the bad exposures were literally every
; other frame for the entire duration of the event. In this case, the
; constant interval would be 24 seconds instead of 12 seconds.

wave = ['171','193','211','335','094','131','304']
wave_exposure_times = [2.0,2.0,2.9,2.9,2.9,2.9,2.9]
; Expected Exposure Times for Each Wavelength
; Ideally, the program should work even if these are wrong.

if keyword_set(DATA_FILE) then begin
   restore, data_file ; re-establish subdata and subindex
endif
; else subdata and subindex should be set, so you don't have to do anything

; To find the names of tags in a struct, call 'tag_names(struct)'
wave_str = subindex[0].wavelnth ; using the struct tag
this_wave = strtrim(string(wave_str),1)
if strlen(this_wave) eq 2 then this_wave = '0' + this_wave ; For 094

wave_indices = where(wave eq this_wave) ; This should always have 1 element
wave_index = wave_indices[0]

nsteps = n_elements(subindex)

good_exposures_bool = intarr(nsteps) + 1 ; Booleans of good_exposures

std_dev = stddev(subindex[*].exptime)
if std_dev ge 0.1 then begin ; If there are bad exposures
   exposure_time_array = fltarr(nsteps) + wave_exposure_times[wave_index]
   bad_exposure_array = abs(subindex[*].exptime - exposure_time_array)

   cutoff = average(bad_exposure_array)
   ;print, cutoff
   
   bad_exposures = where(bad_exposure_array gt cutoff and $
                         bad_exposure_array gt 0.5, count)
   ; Switch 0.5 as a cutoff to subtracting the minimum?
   good_exposures = where(bad_exposure_array le cutoff or $
                          bad_exposure_array le 0.5, count2)

   print, bad_exposures ; Array of bad exposure frame numbers

   if count ne 0 then $
      good_exposures_bool[bad_exposures] = 0 ; Eliminate bad_exposures
   print, good_exposures_bool

   if keyword_set(DELETE) then begin
      keep_good_data = good_exposures
      ; This should be the remaining indices not in bad_exposures
      if count ne 0 then begin
         subdata = subdata[*,*,keep_good_data]
         subindex = subindex[keep_good_data]
      endif
   endif
   if keyword_set(W_DELETE) then begin
      keep_good_data = good_exposures
      ; This should be the remaining indices not in bad_exposures
      if count ne 0 then begin
         subdata = subdata[keep_good_data]
         subindex = subindex[keep_good_data]
      endif
      ; Warning: This keyword will delete a lot of data.
      print, 'Not implemented yet...'
   endif 
endif else begin
   good_exposures = indgen(n_elements(subindex))
endelse
print, good_exposures

end ; EOF
