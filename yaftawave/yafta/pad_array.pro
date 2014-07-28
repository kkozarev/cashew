function pad_array, arr, dim, all=all, before=before, $
                    after=after, both=both, silent=silent

if (n_params() eq 0) then begin
    print,'%================================================================'
    print,'% '
    print,'% PAD_ARRAY.PRO:' 
    print,'%'
    print,'% PURPOSE: Adds blank rows & columns to input array.'
    print,'% '
    print,'% USAGE: array_out = pad_array(array_in)'
    print,'% '
    print,'% KEYWORDS:'
    print,'% '
    print,'%    ALL: (default) Pads all sides of input array'
    print,'% '
    print,'%    DIM: If set to 0, pads columns; if 1, pads rows.'
    print,'% '
    print,'%    BEFORE/AFTER/BOTH: Specifies end(s) on which to append blanks.'
    print,'%                       ONE of these three *MUST* be set if DIM'
    print,'%                       is present, else pad_array returns 0.'
    print,'% '
    print,'% BLAME: Brian Welsch, 14 April 2004'
    print,'% '
    print,'%================================================================'
    return,0
endif 

arr_dim = size(arr,/dim)
nx = arr_dim(0)
ny = arr_dim(1)

if (keyword_set(both)) then begin
    after =1 
    before = 1
endif
if (n_elements(dim) eq 0) then dim = -1 

if (not(keyword_set(before)) and not(keyword_set(after)) and $
    (dim ne -1)) then begin
    print,'% PAD_ARRAY.PRO: No side specified for padding. Returning.'    
    return,0.
endif

if (not(keyword_set(all)) and (dim eq -1)) then begin
    if not(keyword_set(silent)) then $
      print,'% PAD_ARRAY.PRO: Padded all sides.'
    all = 1.
endif

if (keyword_set(all) or ((dim eq 1) and keyword_set(before))) then $
    arr = [[fltarr(nx)],[arr]]  ; adds a row to top
if (keyword_set(all) or ((dim eq 1) and keyword_set( after))) then $
    arr = [[arr],[fltarr(nx)]]  ; adds a row to bottom

arr_dim = size(arr,/dim)
ny = arr_dim(1)

if (keyword_set(all) or ((dim eq 0) and keyword_set(before))) then $
    arr = [transpose(fltarr(ny)),arr] ; adds col before
if (keyword_set(all) or ((dim eq 0) and keyword_set( after))) then $
    arr = [arr,transpose(fltarr(ny))] ; adds col after

return, arr

end



