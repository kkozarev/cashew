pro test_plot_pfss_shock_energy_histogram
  evnum='113'
  path='/Volumes/Backscratch/Users/kkozarev/AIA/events/'
  shockjumps=1.1
  plot_pfss_shock_energy_histogram,shockjumps=shockjumps,mine=mine,maxe=maxe,path=path
end


pro pfss_shock_plot_energy_histogram,shockjumps=shockjumps,mine=mine,maxe=maxe,path=path
;PURPOSE:
;Create and plot the histogram of the maximum energy distribution.
;
;CATEGORY:
; PFSS_Shock
;
;INPUTS:
;
;KEYWORDS:
; 
;
;OUTPUTS:
;
; 
;DEPENDENCIES:
;
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 2011

;  +================================================
  ;Create the proton momentum grid
  ;Use a scheme in which energy bins are created exponentially. It covers the energy
  ;range better. In that scheme, E[i]=E0*((E1/E0)^(i/Nergs))
  if not keyword_set(mine) then mine=0.01 ;MeV
  if not keyword_set(maxe) then maxe=1000.0 ;MeV
  if not keyword_set(shockjumps) then shockjumps=4.0
  if not keyword_set(path) then path='./'
  datapath=path+evnum+'/'
  njumps=n_elements(shockjumps)
  mp=1.67e-27                               ;proton mass in kg
  MeVinJ=1.6e-19                             ;conversion between MeV and Joules
  nergs=100
  gride=dblarr(nergs) ;The energy grid in MeV
  for i=0,nergs-1 do gride[i]=mine*(maxe/mine)^(i/((nergs-1)*1.0))
  pgrid = sqrt(mp *2) * sqrt(gride*MeVinJ) ;momentum in kg m/s
  logpgrid=alog10(pgrid)
  binsiz=logpgrid[1]-logpgrid[0]
  histgrid=dblarr(njumps,nergs)
  protmaxe=dblarr(njumps)
  
  for jump=0,njumps-1 do begin
     shockjump=shockjumps[jump]
     
     fname='aia_shock_pfss_r'+$
           strtrim(string(shockjump,format='(f4.1)'),2)+'.sav'
     restore,datapath+fname
     logpfinal=alog10(finalMomentum)
     protmaxe[jump]=max(finalMomentum)^2/(2*mp)/MeVinJ
     hist=histogram(logpfinal,binsize=binsiz,min=logpgrid[0],nbins=nergs)
     histgrid[jump,*]=hist
  endfor
  
  ;Create the initial histogram for comparison
  inithist=fltarr(nergs)
  inithist[where(gride eq mine)]=n_elements(finalMomentum)
  
  
  xrng=[mine,maxe]
  yrng=[min(histgrid[where(histgrid gt 0.0)])/2.,max(inithist)*1.5]
  wdef,0,1200,800
  loadct,0,/silent
  !P.position=[0.14,0.14,0.9,0.9]
  !p.font=-1
  ;yrng=[1,500]
  PLOT,gride,histgrid[0,*],$
       PSYM = 10, $ 
       TITLE = '!6Maximum energies of shock-accelerated protons', $
       XTITLE = '!6Proton energy [MeV]', $
       YTITLE = '!6# protons', $
       xrange=xrng,$
       yrange=yrng,$
       xstyle=1,ystyle=1,color=0,background=255,$
       xthick=3,ythick=3,thick=3,charsize=3.2,$
       charthick=2,/ylog,/xlog,/nodata
  
  loadct,39,/silent
  tvlct,rr,gg,bb,/get
  for jump=0,njumps-1 do begin
     col=50+204.0*(jump*1.0/(njumps-1.0))
     ;Plot the histogram
     oplot,gride,histgrid[jump,*],thick=2,color=col,psym=10
     ;Plot a line denoting the maximum energy
     plots,[protmaxe[jump],protmaxe[jump]],[yrng[0],yrng[1]],$
           thick=4,linestyle=2,color=col
     ;Legend
     xyouts,!p.position[2]-0.23,!p.position[3]-0.1-jump*0.05,$
            'r = '+strtrim(string(shockjumps[jump],format='(f5.2)'),2),$
            color=col,/normal,charsize=3.4
  endfor
  
  ;Plot the histogram with the initial energies
  oplot,gride,inithist,psym=10,color=0,thick=3.0
  
  image=tvrd(true=1)
  write_png,datapath+'dsa_energy_histogram_'+evnum+'.png',image,rr,gg,bb
;-==============================================================================
  
end
