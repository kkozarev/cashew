

pro aia_aschdem_calculate_em,event,mapfile,em_tot,radcoords=radcoords,temperature=temperature,density=density,chi_map=chi_map,sig_map=sig_map,demcolumn=demcolumn
;This procedure calculates the total EM.
;It can also return average temperature, average density, the map of Chi-squared and the errors.
  ;teem_table=event.aschdempath+'aschdem_'+event.date+'_'+event.label+'_teem_table.sav'
  ;restore,teem_table
  
  restore,mapfile

  dim	=size(em_map)
  nx	=dim[1]
  ny	=dim[2]
  
  ;em_tot = fltarr(nx,ny)
  ;temt = fltarr(nx,ny)
  
 ; for j=0,ny-1 do begin
 ;    for i=0,nx-1 do begin
 ;       em  = 10.^em_map[i,j]    ;log(EM_max)-->EM_max
 ;       te  = te_map[i,j]        ;log(te)
 ;       sig = sig_map[i,j]
 ;       dem = em*exp(-(telog-te)^2/(2.*sig^2))
 ;       em_tot[i,j] = int_tabulated(10^telog,dem,/double)
 ;       temt[i,j] = int_tabulated(10^telog,dem*10^telog,/double)
 ;    endfor
 ; endfor
 ; emlog	=alog10(em_tot) 
 ; temtlog	=alog10(temt) 
  
  if not keyword_set(demcolumn) then demcol = 9.0e9 ;[cm] 90 Mm height, used in Vanninathan et al. 2015
  
  temt = 10^temtlog
  em_tot = 10^emlog
  if keyword_set(temperature) or keyword_set(density) then temperature = (temt+1.e-30)/(em_tot+1.e-30)
  if keyword_set(density) then begin
     if keyword_set(radcoords) and not keyword_set(demcolumn) then begin
                                ;This variable should be of shape
                                ;[2,NX,NY], and should contain the radial distance of each pixel
        rsun=6.957e8            ;Solar radius in meters
        kb=1.38e-23             ;Boltzmann constant
        mp=1.66e-27             ;Proton mass in kg
        grav=6.67e-11           ;Gravitational constant
        msun=1.99e30            ;Solar mass in kg
        demcol=sqrt((!PI * kb * temperature * (radcoords * rsun)^3) / (mp * grav * msun)) * 100. ;in cm
        demcol[where(radcoords lt 1.0)]=9.0e9 ;For on-disk positions, keep the value from Vanninathan et al.
     endif
     density = sqrt(em_tot/demcol)
  endif

end
