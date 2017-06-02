pro test_pfss_shock_aschdem_plasma_info
;A program to test pfss_shock_aschdem_plasma_info
  
 ;You can run for one event, like this.
  one=1
  if one eq 1 then begin
     labels=['110607_01','120526_01','130423_01','140708_01','130517_01','120915_01','110211_02','120424_01','110125_01','131212_01']
     labels=['151104_01']
     for ev=0,n_elements(labels)-1 do begin
        label=labels[ev]
        event=load_events_info(label=label)
        pfss_shock_aschdem_plasma_info,event,/lores,/force
        ;pfss_shock_aschdem_plasma_info,event,/hires
     endfor
  endif
  
  ;Or for all events
  all=0
  if all eq 1 then begin
     events=load_events_info()
     for ev=0,n_elements(events)-1 do begin
        event=events[ev]
        pfss_shock_aschdem_plasma_info,event
     endfor
  endif

end



pro pfss_shock_aschdem_plasma_info,event,hires=hires,lores=lores,force=force
;PURPOSE:
;A procedure to update the shock-crossing point structures with average model and density and temperature maps
;from the Aschwanden DEM model. It takes 5x5 pixels around the crossing point assuming the same density along the
;line of sight, then checks if the chi-squared is not too bad (<25). It gets rid of the 'bad' fit pixels, and averages
;the density and temperature in the ones that are left. This is passed on to the crossing point structures.
;
;CATEGORY:
; AIA/CSGS/ASCHDEM
;
;INPUTS:
; event - the event structure
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
;Written by Kamen Kozarev, 03/11/2016

  
  ;restore the crossing points
  savename=event.csgs.lores.map_savename
  if keyword_set(hires) then savename=event.csgs.hires.map_savename
  savepath=event.pfsspath
  if file_exist(savepath+savename) then begin
     restore,savepath+savename
     if not keyword_set(force) then begin
        print,''
        print,'Resulting file exists. Re-run with /force to force an update.'
        return
     endif
  endif else begin
     print,'Non-existent CSGS results file '+savepath+savename
     return
  endelse
  
  
  nmaxcrosses=max(allcrosses)
  ind=subindex[0]
  wcs = fitshead2wcs(ind)
  coords = wcs_get_coord(wcs)/ind.rsun_obs ;Cartesian coordinates of the pixels in solar radii.
  ;h = float(sqrt((coords[0,*,*] - 0.)^2. + (coords[1,*,*] - 0.)^2.))
  sz = size(reform(coords[0,*,*]))
  
  ;Get the density and temperature files (Aschdem total files)
  demtots = file_search(event.aschdempath+replace_string(event.aschdem.total_savename,'HHMMSS','??????'))
  if demtots[0] eq '' then begin
     print,'Non-existent AschDEM files of type '+event.aschdem.total_savename
  endif
  ;Get the dem totals files to know which times to compare.
  demtottimes=strarr(n_elements(demtots))
  for tt=0,n_elements(demtots)-1 do begin
     tmp=strsplit(file_basename(demtots[tt]),'_',/extract)
     demtottimes[tt]=strmid(event.date,0,4)+'-'+strmid(event.date,4,2)+'-'+strmid(event.date,6,2)+' '+$
                     strmid(tmp[4],0,2)+':'+strmid(tmp[4],2,2)+':'+strmid(tmp[4],4,2)+'.00'
  endfor
  demtimes=get_time(demtottimes)
  
  ;This is the array that holds the crossing points times
  cptimes=get_time(subindex.date_obs)
  
  ;Find the points on the AIA pixel grid that correspond to the crossing points. Since the crossing points
  ;have their coordinates in both arcseconds and in solar radii, the comparison can be in either unit.
  ;Let's say the x- and y- tolerance will be 3 arcseconds (~5x5 averaging pixels)
  tolerance=5./ind.rsun_obs
  cxtol=reform(coords[0,*,*]+tolerance)
  cytol=reform(coords[1,*,*]+tolerance)
  
  ;Find background density at the location
  aia_aschdem_calculate_em,event,demtots[0],emtot,temperature=basetemperature,density=basedensity,chi_map=chi_map
  
  for step=0,nsteps-1 do begin
     ;print,'basedensity[0]: '+string(basedensity[0])
     ncrosses=allcrosses[step]
      
     cptime=cptimes[step]
     tmp=min(abs(cptime.jd - demtimes.jd),demind)
     print,cptime.cashew_time + '  ' + demtimes[demind].cashew_time
     ;calculate the density and temperature maps for the appropriate time
     ;BY THE WAY, the density can be re-calculated using emtot and a better model for the
                                ;radial dependence of the column
                                ;depth! The formula is density =
                                ;sqrt(em_tot/demheight)
     
     
     aia_aschdem_calculate_em,event,demtots[demind],emtot,temperature=temperature,density=density,chi_map=chi_map
     
     for cross=0,ncrosses-1 do begin
        pt=crosspoints[step,cross]
        ptr=sqrt(pt.rpx^2+pt.rpy^2)
        ;Search on the x-axis and the y-axis simultaneously for the closest pixels
        ;within the tolerance.
        resx=where((pt.rpx gt reform(coords[0,*,*])) and (pt.rpx le cxtol)$
                   and (pt.rpy gt reform(coords[1,*,*])) and (pt.rpy le cytol))
        if resx[0] eq -1  then begin
           print,'point is outside of AIA FOV'
           continue
        endif
        ;print,pt.rpx,pt.rpy,n_elements(resx),min(density[resx]),max(density[resx]),mean(density[resx]),mean(chi_map[resx])
        
        ;Of the pixels that were found, only choose the ones with Chi-squared lower than 20.
        ;This means that the number of pixels, over which we average, will vary, but at least the quality of the
        ;model is much better.
        goodchi=where(chi_map[resx]^2 lt 25.)
        if goodchi[0] eq -1 or n_elements(goodchi) lt 3 then bestind=resx else bestind=resx[goodchi]
        ;if chi is huge, give up
        if mean(chi_map[bestind]) gt 15 then begin
           print,'chisq of '+strtrim(string(mean(chi_map[bestind])^2),2)
           continue
        endif
        ;print,pt.rpx,pt.rpy,n_elements(bestind),min(density[bestind]),max(density[bestind]),mean(density[bestind]),mean(chi_map[bestind])
     ;Assign the density and temperature to the crossing points
        crosspoints[step,cross].density=mean(density[resx])
        crosspoints[step,cross].temperature=mean(temperature[resx])
        ;print,'density[0]: '+string(density[0])+'  basedensity[0]: '+string(basedensity[0])
        crosspoints[step,cross].shockjump=mean(density[resx]/basedensity[resx])
     endfor
     ;print,'density[resx]: '
     ;print,density[resx]
     ;print,'basedensity[resx]: '
     ;print,basedensity[resx]
     ;stop
  endfor
  tmp=crosspoints.shockjump
  tmp[where(tmp le 1.0)]=1.00000001 
  crosspoints.shockjump=tmp
  
                                ;Finally, overwrite the csgs
  print, ''
  print, 'Saving file '+savename
  save,filename=savepath+savename,$
       ALLCROSSES,CARRLAT,CARRLON,CROSSPOINTS,DT,NSTEPS,NMAXCROSSES,$
       RADIUS,RADIUSFITLINES,ROTATIONANGLES,SC,SUBINDEX,SUNCENTER,$
       TIME,VERTEX_LIST,VERT_ROTMAT,VERT_TRANSMAT
  
end
