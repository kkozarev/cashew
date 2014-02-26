pro test_swap

swap = obj_new( 'swap' ) 
swap -> set, filter = 'lv1', prep = 0, local = 1 
;flist = swap -> list( timerange = ['13-June-2010 05:30:00', '13-June-2010 06:20:00'] )
flist = swap -> list( timerange = ['12-June-2010 00:50:00', '12-June-2010 01:10:00'] )
swap -> copy, filelist=flist 
swap -> read, filelist = flist 
data= swap->getdata(filelist=flist) 

;for i=1,20 do begin
;tv, data[512:*,70:581,i]-data[512:*,70:581,i-1] 
;wait,0.8
;endfor

;stop

wdef,0,1024,1024
for i=1,n_elements(data[0,0,*])-1 do begin
;tvscl,sqrt(data[*,*,i])
tvscl,data[*,*,i-1]-data[*,*,i]
wait,1.0
endfor

stop

end
