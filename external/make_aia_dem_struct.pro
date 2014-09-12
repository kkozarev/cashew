function make_aia_dem_struct, chiantifix=chiantifix, xrt_filters=xrt_filters, obs_date=obs_date, chn_filename=chn_filename, $
                              logt_min=logt_min, logt_max = logt_max, noblend=noblend, contfix=contfix, hybrid=hybrid,$
                              fix94=fix94, fix131=fix131, schmelz2012=schmelz2012

;; to massage the AIA responses into something that xrt_dem_iterative
;; recognizes
;By Kathy Reeves

  if not keyword_set(logt_min) then logt_min = 5.5
  if not keyword_set(logt_max) then logt_max = 8.0

 if keyword_set(xrt_filters) then begin

    wresps_all = make_xrt_wave_resp(contam_time=obs_date, chn_filename=chn_filename)

    if keyword_set(hybrid) then begin

       file = '/storage/kreeves/Data/emissivities/Takeda_CHIANTI/solspec_ch700_hybrid_chianti.genx'
       restgen, file=file, struct=hybrid

       modelname = 'Takeda.CHIANTI7.hybrid'
       wave = hybrid.lambda
       temp = 10^(hybrid.logte)
       abund_model='hybrid - Fludra&Schmelz 1999'
       ioneq_model='chianti.ioneq?'
       dens_model='???'
    
       dA = wave[1]-wave[0]
       spectrum = hybrid.solspec/dA
   

    endif else begin
       modelname = 'AIA default spectrum'

       if keyword_set(schmelz2012) then begin

          restgen, file="/storage/kreeves/IDL/aia/schmelz2012_fullemiss.genx", struct=aia
       endif else begin
          aia = aia_get_response(/emiss, /full)
       endelse

       wave = aia.total.wave
       temp = 10^(aia.total.logte)
       ;; fix aia response V2 continuum bug
       if keyword_set(contfix) then spectrum = (aia.total.emissivity + 9.0*aia.cont.emiss) else spectrum = aia.total.emissivity
       abund_model = aia.general.abundfile
       ioneq_model = aia.general.ioneq_name
       dens_model = aia.general.model_name +', p='+trim(aia.general.model_pe, 1)
       
    endelse

    emiss_model = make_xrt_emiss_model(modelname, wave, temp, spectrum, $
                                       abund_model, ioneq_model,dens_model,$
                                       data_files=data_files)
    tresps_all = make_xrt_temp_resp(wresps_all, emiss_model)
    
    diff_min = min(abs(logt_min-alog10(tresps_all[0].temp)),nmin)
    diff_max = min(abs(logt_max-alog10(tresps_all[0].temp)),nmax)

    length = nmax-nmin+1
    for i=0,n_elements(xrt_filters)-1 do begin
       filt_idx = where(strmatch(tresps_all.confg.name,xrt_filters[i]))
       print, filt_idx
       confg = create_struct("name",tresps_all[filt_idx].confg.name)
       xrt_tresp = create_struct("type",tresps_all[filt_idx].type,"temp",tresps_all[filt_idx].temp[nmin:nmax],"TEMP_UNITS","K",$
                                 "temp_resp",tresps_all[filt_idx].temp_resp[nmin:nmax],"temp_resp_units",$
                                 "DN cm^5 s^-1 pix^-1","length",length,"confg", confg)
       if i eq 0 then xrt_arr = xrt_tresp else $
       xrt_arr = concat_struct(xrt_arr, xrt_tresp)
    endfor

 endif
 


 if keyword_set(schmelz2012) then begin

     ;; this uses a hacked version of aia_get_response that uses a
    ;; chianti spectrum from Joan Schmelz, using coronal"hybrid"
    ;; abundances (i.e. Schmelz et al 2012).

    tresp = aia_get_response_schmelz2012(/temp, /dn, chiantifix=chiantifix, timedepend_date=obs_date,/evenorm,$
                                    noblend=noblend, fix94=fix94, fix131=fix131)
 endif else begin
    if keyword_set(hybrid) then begin
    
       ;; this uses a hacked version of aia_get_response that uses a
       ;; chianti spectrum from Aki Takeda, using coronal"hybrid"
       ;; abundances (i.e. fludra and schmelz, 1999).

       tresp = aia_get_response_hybrid(/temp, /dn, chiantifix=chiantifix, timedepend_date=obs_date,/evenorm,$
                                    noblend=noblend, fix94=fix94)

    endif else begin
       ;; this uses a bootleg fix to the AIA response functions that fix the
       ;; bug in V2 where the continuum is 10x too low
       ;; note - this is no longer necessary - bug has been fixed in
       ;;        V3 of the response functions.
       if keyword_set(contfix) then begin

          tresp = aia_get_response_contfix(/temp, /dn, chiantifix=chiantifix, timedepend_date=obs_date,/evenorm, noblend=noblend)
          ;; else use default (currently V4)
       endif else begin
          tresp = aia_get_response(/temp, /dn, chiantifix=chiantifix, timedepend_date=obs_date,/evenorm, noblend=noblend)
       endelse

    endelse
   
 endelse

 diff_min = min(abs(logt_min-tresp.a131.logte),nmin)
 diff_max = min(abs(logt_max-tresp.a131.logte),nmax)
 length = nmax-nmin+1
 
 temp = 10^[[tresp.a94.logte[nmin:nmax]],[tresp.a131.logte[nmin:nmax]],[tresp.a171.logte[nmin:nmax]],$
            [tresp.a193.logte[nmin:nmax]],[tresp.a211.logte[nmin:nmax]],[tresp.a335.logte[nmin:nmax]]]

 temp_resp = [[tresp.a94.tresp[nmin:nmax]],[tresp.a131.tresp[nmin:nmax]],[tresp.a171.tresp[nmin:nmax]],$
              [tresp.a193.tresp[nmin:nmax]],[tresp.a211.tresp[nmin:nmax]],[tresp.a335.tresp[nmin:nmax]]]

 names = ['94','131','171','193','211','335']


 for i=0,5 do begin

    confg = create_struct("name",names[i])
    dem_tresp = create_struct("type","AIA","temp",temp[*,i],"TEMP_UNITS","K","temp_resp",temp_resp[*,i],"temp_resp_units",$
                              "DN cm^5 s^-1 pix^-1","length",length,"confg", confg)

    if i eq 0 then dem_arr = dem_tresp else $
       dem_arr = concat_struct(dem_arr, dem_tresp)

 endfor

 for i=0,n_elements(xrt_filters)-1 do begin
    dem_arr =concat_struct(dem_arr, xrt_arr[i])
 endfor

 return, dem_arr

end
