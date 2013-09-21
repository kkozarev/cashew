pro runcd
;This procedure performs multiple runs of the changedet routine
;to determine change in AIA data for different combinations
;of temporal and spatial parameters (see changedet.pro)
restore,'regionData_0613_211.sav'

nt=n_elements(totaldata[*,0,0])

;Normalize the exposure to 1 sec.
for t=0,nt-1 do totaldata[t,*,*]/=indices[t].exptime


r=totaldata[20:90,*,*]
t=n_elements(r[*,0,0])
roi=fltarr(1024,1024,t)
for i=0,t-1 do roi[*,*,i]=r[i,*,*]

ntrials=9
st={ht:0,hx:0}
trials=replicate(st,ntrials)

;Define the different trials to perform
trials[0]={ht:4,hx:4}
trials[1]={ht:6,hx:4}
trials[2]={ht:8,hx:4}
trials[3]={ht:4,hx:6}
trials[4]={ht:6,hx:6}
trials[5]={ht:8,hx:6}
trials[6]={ht:4,hx:8}
trials[7]={ht:6,hx:8}
trials[8]={ht:8,hx:8}


for tt=1,ntrials-1 do begin
	ht=strtrim(string(trials[tt].ht),2)
	hx=strtrim(string(trials[tt].hx),2)
;	print,'changedet_0613_211_ht'+ht+'_hx'+hx+'.sav
	;stop
	vmaps=changedet(roi,trials[tt].ht,trials[tt].hx)
	save,vmaps,filename='changedet_0613_211_ht'+ht+'_hx'+hx+'.sav'
endfor


end
