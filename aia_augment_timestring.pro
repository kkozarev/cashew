function aia_augment_timestring,oldtime,nsec
;a simple procedure that takes a date/time string, like '2011-05-11
;02:26:00' and adds a certain number of SECONDS to the time,
;taking care to augment it properly. It is only accurate to within a
;day change...
newtime=''
nsec=fix(nsec)

;Record the times in string arrays
st=strsplit(oldtime,' /:,.-T',/extract)
if n_elements(st) eq 4 then st=[st,'00']
if n_elements(st) eq 5 then st=[st,'00']


;The seconds
dsec=nsec mod 60
newsec=st[5]+dsec
while newsec ge 60 do newsec-=60
newsec=strtrim(string(newsec),2)
if newsec lt 10 then newsec='0'+newsec


;The minutes
dmin=nsec/60
newmin=st[4]+dmin
while newmin ge 60 do newmin-=60
newmin=strtrim(string(newmin),2)
if newmin lt 10 then newmin='0'+newmin


;The hours
dhr=(st[4]+dmin)/60
newhr=st[3]+dhr
while newhr ge 24 do newhr-=24
newhr=strtrim(string(newhr),2)
if newhr lt 10 then newhr='0'+newhr

;The days
dday=(st[3]+dhr)/24
newday=st[2]+dday
newday=strtrim(string(newday),2)
if newday lt 10 then newday='0'+newday


newtime=st[0]+'-'+st[1]+'-'+newday+' '+newhr+':'+newmin+':'+newsec

return, newtime
end
