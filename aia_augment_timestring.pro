pro test_aia_augment_timestring
  sts=['2011/05/11 01:10:00']
  print,'START T: '+sts
  
;A series of tests
  tim=[10,60,600,1000,-10,-60,-600,-1000]
  for i=0,n_elements(tim)-1 do begin
     print,'ADD '+strtrim(string(tim[i]),2)+' s: '+aia_augment_timestring(sts,tim[i])
  endfor

  print,'--------------------------'
  sts=['2011/05/03 23:50:50']
  print,'START T: '+sts
  
;A series of tests
  tim=[10,60,600,1000,-10,-60,-600,-1000]
  for i=0,n_elements(tim)-1 do begin
     print,'ADD '+strtrim(string(tim[i]),2)+' s: '+aia_augment_timestring(sts,tim[i])
  endfor

end



function aia_augment_timestring,oldtime,nsec
;PURPOSE:
;a simple procedure that takes a date/time string, like '2011-05-11
;02:26:00' and adds/subtracts a certain number of SECONDS to the time,
;taking care to augment it properly. It is only accurate to within a
;day change...
;
;CATEGORY:
; AIA/General
;
;INPUTS:
; oldtime - the original time string
;           nsec - number of seconds to add to the old time string. If
;                  nsec < 0, the routine subtracts seconds instead.
;KEYWORDS:
; 
;
;OUTPUTS:
; function returns newtime - the augmented time string
; 
;DEPENDENCIES:
;
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 2012
;10/01/2013, KAK - Generalized the routine to also be able to subtract seconds

newtime=''
nsec=fix(nsec)

;Record the times in string arrays
st=strsplit(oldtime,' /:,.-T',/extract)
if n_elements(st) eq 4 then st=[st,'00']
if n_elements(st) eq 5 then st=[st,'00']

;convert time to seconds
t0=fix(st[3])*3600.+fix(st[4])*60.+fix(st[5])

;Add the number of seconds
t2=t0+nsec

;Convert back to a string
newhr=fix(t2/3600)
subd=0.0
while newhr ge 24 do begin
   newhr=strtrim(string(newhr-24.),2)
   subd++
endwhile
newhr=strtrim(string(fix(newhr)),2)
if newhr lt 10 then newhr='0'+newhr

tmp=t2 mod 3600.
newmin=strtrim(string(fix(tmp/60)),2)
if newmin lt 10 then newmin='0'+newmin
newsec=strtrim(string(fix(tmp mod 60)),2)
if newsec lt 10 then newsec='0'+newsec

;The days
   newday=strtrim(string(fix(st[2]+subd)),2)
   if newday lt 10 then newday='0'+newday
   
;stop
newtime=st[0]+'-'+st[1]+'-'+newday+' '+newhr+':'+newmin+':'+newsec

return, newtime
end
