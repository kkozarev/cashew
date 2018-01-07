function fix_bad_pixels, inarray
;This function will set the pixels that equal infinity or nan to the
;average value for the array.
  result=inarray
  bad=where(finite(result,/infinity) or finite(result,/nan),complement=good)
  ;print,n_elements(good)
  if bad[0] ne -1 and good[0] ne -1 then begin
     mean=mean(result[good])
     result[bad]=mean
  endif
  return,result
end
