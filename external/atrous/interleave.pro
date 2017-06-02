function interleave, array, range, inverse = inverse
;+
; NAME:
;   INTERLEAVE
; PURPOSE:
;   For an 2N element array, the program interleaves the last N
;   elements with the first N elements.
;
; CALLING SEQUENCE:
;   output = INTERLEAVE(X, [, range, inverse = inverse])
;
; INPUTS:
;  X -- A vector to be operated upon
;  RANGE -- Perform INTERLEAVE on the first RANGE elements of the array.
; KEYWORD PARAMETERS:
;  INVERSE -- Instead of interleaving, separate every other element
;             and move to the back.
;
; NOTES:
;  For images, the interleaving is performed only on the columns of
;  the matrix (the second dimension).
; 
; OUTPUTS:
;  OUTPUT -- the interleaved array.
;
; MODIFICATION HISTORY:
;
;       Wed Mar 26 13:36:46 2003, Erik Rosolowsky <eros@cosmic>
;		Written for use with wavelets.
;
;-


  if n_elements(range) eq 0 then begin
    if size(array, /n_dim) eq 2 then begin
      sz = size(array)
      range = sz[1]
    endif else range = n_elements(array) 
  endif
  
  if size(array, /n_dim) eq 2 then begin
    sz = size(array)
    output = array
    half = range/2
    front = lindgen(half)
    if keyword_set(inverse) then begin
      output[*, 0:half-1] = array[*, 2*front]
      output[*, half:2*half-1] = array[*, 2*front+1]
    endif else begin
      back = (lindgen(half)+half)
      output[*, 2*front] = array[*, front]
      output[*, 2*(back-half)+1] = array[*, back]
    endelse
  endif else begin
    output = array
    half = range/2
    front = lindgen(half)
    if keyword_set(inverse) then begin
      output[0:half-1] = array[2*front]
      output[half:2*half-1] = array[2*front+1]
    endif else begin
      back = lindgen(half)+half
      output[2*front] = array[front]
      output[2*(back-half)+1] = array[back]
    endelse
  endelse
  return, output
end

