function get_time,date_obs
;PURPOSE:
;This function takes an array of timestamps (format
;2011-05-11T02:10:19.84), and creates a structure with all necessary
;time formats in it.
;
;CATEGORY:
; AIA/General
;
;INPUTS:
;       date_obs - one or more timestamp strings
;
;KEYWORDS:
;
;
;OUTPUTS:
;       Returns a function containing the time in string, JD, and
;       relative seconds formats
;
;DEPENDENCIES:
;
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 05/13/2014
;
nt=n_elements(date_obs)
times=replicate({date_obs:'',JD:0.D,relsec:0.D},nt)
times.date_obs=date_obs

tmp=anytim2jd(date_obs)
for t=0,nt-1 do times[t].jd=tmp[t].int+tmp[t].frac

relsec=(times.jd-times[0].jd)*86400.
times.relsec=relsec

return,times


end
