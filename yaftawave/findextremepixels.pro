pro findextremepixels, inmask, label, excols
;this is a program that searches the rows and columns of the mask to
;find the extremely located pixels on the x-axis.

;NB! This version only works for the case that the feature is 
;propagating to the left(east)!

  sz=size(inmask)
  if sz[0] ne 2 then begin
     print,'Mask must be an m by n array. Returning...'
     return
  endif
  nrows=n_elements(inmask[0,*])
  ncols=n_elements(inmask[*,0])
  excols=fltarr(2,nrows)

  for row=0,nrows-1 do begin
     for col=0,ncols-1 do begin
        if inmask[col,row] eq label then break
     endfor
     excols[1,row]=row
     if col eq ncols then excols[0,row]=-1 else excols[0,row]=col
    ; print,excols[*,row]

  endfor
  
  ind1=where(excols[0,*] ne -1)
  excols=excols[*,ind1]

end
