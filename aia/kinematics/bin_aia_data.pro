pro bin_aia_data, time, yarray, data, yrng, binData=binData


binData = data

for t=0, n_elements(data[*,0])-1 do begin
   for r=0, n_elements(data[0,*])-1 do begin
      bindata[t,r] = 0.0
      if data[t, r] lt 0.0 then begin
         binData[t, r] = -10.0
      endif else if data[t, r] gt 50.0 && data[t,r] lt 75.0 then begin
         binData[t,r] = 25.0
      endif else if data[t,r] gt 75.0 then begin
         binData[t,r] = 150.0
      endif
   endfor
endfor 

cgImage, binData

end
