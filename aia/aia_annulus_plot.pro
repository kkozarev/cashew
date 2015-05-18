;+============================================================================
pro test_aia_annulus_plot
;Procedure to run and test aia_annulus_analyze

;You can run for one event, like this.
  one=1
  if one eq 1 then begin
     wav='193'
     rmax=1.45
     event=load_events_info(label='110511_01')
     aia_annulus_plot,event,wav=wav,/base,/run,/raw
  endif
    
;Alternatively, run for all events
  all=0
  if all eq 1 then begin
     events=load_events_info()
     wavelengths=['193','211']
     rrange=[1.1,1.37]
;n_elements(events)-1
     for ev=0,n_elements(events)-1 do begin
        event=events[ev]
        for w=0,n_elements(wavelengths)-1 do begin
           wavelength=wavelengths[w]
           aia_annulus_plot,event,wav=wavelength,rrange=rrange,/constrain ;,/interactive
        endfor
     endfor
  endif

end
;-============================================================================


;+============================================================================
pro aia_annulus_plot_main,event,datapath=datapath,savepath=savepath,thrange=thrange,$
                          wav=wav,rmax=rmax,raw=raw,base=base,run=run,lines=lines
;PURPOSE:
;Procedure to plot the time-dependent base difference annuli.
;
;CATEGORY:
; AIA/Kinematics
;
;INPUTS:
;      EVENT - load an event information structure
;
;KEYWORDS:
;      DATAPATH:
;      SAVEPATH:
;      THRANGE:
;      INTERACTIVE:
;      WAVE:
;      CONSTRAIN - if set, constrain the wave to move with physical
;                  speeds (10-1500 km/s)
;
;OUTPUTS:
;
;DEPENDENCIES:
;
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 05/13/2014
;


;----------------------------
;Restore the file with the deprojected data
  date=event.date
  
  if not keyword_set(wav) then wave='193' else wave=wav
  if not keyword_set(savepath) then savepath=event.savepath+'annulusplot/' ;'./'
  if not keyword_set(datapath) then datapath=savepath ;'./'
  if keyword_set(rmax) then radrange=[1.0,rmax] else radrange=[1.0,1.45]

  fname=event.annplot.savename+wave+'.sav'
  restore, datapath+fname
  ;Create the time structure
  time=get_time(ind_arr.date_obs)
  nsteps=n_elements(subprojdata[*,0,0])
  ncols=n_elements(subprojdata[0,*,0])
  nrows=n_elements(subprojdata[0,0,*])
  projdata=0.0
  RSUN=ind_arr[0].rsun_ref/1000. ;Solar radius in km.
  KMPX=ind_arr[0].IMSCL_MP*ind_arr[0].RSUN_REF/(1000.0*ind_arr[0].RSUN_OBS)
  
;Pixel coordinates in arcseconds from the center of the sun. How do I convert them to km?
  y_arcsec_array=(res/ind_arr[0].cdelt1*findgen(nrows)+r_in)
  y_rsun_array=y_arcsec_array/ind_arr[0].rsun_obs
  
;Determine the radial range indices
  radind=[0,n_elements(y_rsun_array)-1]
  tmp=min(where(radrange[0] - y_rsun_array le 0.))
  if tmp gt -1 then radind[0]=tmp
  tmp=max(where(radrange[1] - y_rsun_array ge 0.))
  if tmp gt -1 then radind[1]=tmp
  

;The X-angular array (distance along the limb from the pole).
  x_deg_array=findgen(ncols)*ang_step+thrang[0]*180./!PI

;----------------------------------------------------
;1. Plot the annulus plots
  set_plot,'x'
  loadct,0,/silent
  
  !p.font=-1
  !P.position=[0.0,0.1,0.9,0.93]
  !P.background=255
  !P.color=0
  !P.charsize=1.6
  img_size=[1200,700]

  ;Create the base image for differencing.
  if keyword_set(base) then begin
     baseim=subprojdata[0,*,*]
     for ii=1,9 do baseim+=subprojdata[ii,*,*]
     baseim/=10.
     baseim=reform(baseim)
  endif



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MAIN TIME STEP LOOP!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
lwr_img=0

  for step=lwr_img,nsteps-1 do begin
     
     strind=strtrim(string(step),2)
     if strind lt 100 then strind='0'+strind
     if strind lt 10 then strind='0'+strind
     
; Setup Z-buffer for plotting
        if step eq lwr_img then begin
           set_plot, 'z'
           Device, Set_Resolution=[img_size[0], img_size[1]], set_pixel_depth=24, decomposed = 0
           ;!p.multi = 0
           !P.font=0
           chsize=2.2
           chthick=1.8          
           if keyword_set(run) or keyword_set(base) then begin
              loadct,0,/silent
              tvlct, r, g, b, /get
              col=0
              !P.color=0
              bckg=255
              !P.background=255
           endif else begin
              loadct,0,/silent
              tvlct, r, g, b, /get
              tvlct,reverse(r),reverse(g),reverse(b)
              col=0
              bckg=255
           endelse

        endif

;Set up the image to plot
     if keyword_set(base) then  $
     plotimg=reform(subprojdata[step,*,radind[0]:radind[1]] - $
                    baseim[*,radind[0]:radind[1]])
     if keyword_set(run) then begin
        if step eq 0 then continue else $
           plotimg=reform(subprojdata[step,*,radind[0]:radind[1]] - $
                          subprojdata[step-1,*,radind[0]:radind[1]])
     endif
     if keyword_set(raw) then  $
        plotimg=reform(subprojdata[step,*,radind[0]:radind[1]])

     
     if keyword_set(run) or keyword_set(base) then begin
        case wave of
           '94': int_range = [-5., 5]
           '304': int_range = [-5., 5]
           '335': int_range = [-5., 5]
           '131': int_range = [-5., 5]
           '171': int_range = [-5., 5]
           '193': int_range = [-40, 50]
           '211': int_range = [-40, 50]
           else: int_range = [min(plotimg), max(plotimg)]
        endcase
     endif else begin
        ;int_range = [1,35]
        int_range=[0.5,4.0]
     endelse
     ;Plot the sqrt of the image...
     if keyword_set(raw) then plotimg=alog10(plotimg);plotimg=sqrt(plotimg);
     
     ; Define image title      
     if keyword_set(run) then begin
        img_tit = '!5RDIFF AIA/'+wave+'A   '+event.date+' '+$
                  num2str(anytim(ind_arr[step].date_d$obs, /yohkoh, /time_only, /sec, /trun))
     endif else begin
        if keyword_set(base) then begin
           img_tit = '!5BDIFF AIA/'+wave+'A   '+event.date+' '+$
                     num2str(anytim(ind_arr[step].date_d$obs, /yohkoh, /time_only, /sec, /trun))
        endif else begin
           img_tit = '!5RAW AIA/'+wave+'A   '+event.date+' '+$
                     num2str(anytim(ind_arr[step].date_d$obs, /yohkoh, /time_only, /sec, /trun))
        endelse
     endelse

     plot_image, plotimg, xtitle = '!5Theta [degrees from solar north]', $
                 ytitle = '!5Radius [arcsec from Sun center]', $
                 title = img_tit, max = int_range[1], $
                 origin = [thrang[0]*180./!PI,r_in], charthick = chthick, charsize=chsize,$
                 scale = [ang_step, res/ind_arr[0].cdelt1], $
                 pos = [0.13, 0.14, 0.96, 0.92], min = int_range[0]
     
      htlimits=aia_rad_height_limits(degarray=x_deg_array)
      oplot,x_deg_array,htlimits,color=255,thick=2 ;,/data 
     
     if not keyword_set(centerlat) then begin
        if event.arlon lt 0.0 then arlat=270.0+event.arlat $
        else arlat=90.0-event.arlat
     endif else arlat=centerlat
     

if keyword_set(lines) then begin
;Find the radial outward limit/edge for which there's data
     yradlimit=aia_rad_height_limits(angle=arlat)
     yradlimind=min(where(y_arcsec_array ge yradlimit))
     oplot,[arlat,arlat],[ind_arr[0].rsun_obs,yradlimit],thick=2
     
;The index of the AR latitude
     arxcentind=min(where(x_deg_array ge arlat))
     limb=ind_arr[0].rsun_obs   ;Limb position in arcsec
     limbind=min(where(y_arcsec_array ge limb))
                                ;oplot,thrang*180./!PI,[limb,limb],linestyle=2,thick=2,color=255 ;Overplot the limb position
     lat_heights=(limb+[0.25,0.55,0.85]*(yradlimit-limb))/ind_arr[0].rsun_obs
     
     ;Find the indices of the lateral measurement heights
     nlatmeas=n_elements(lat_heights)
     htind=intarr(nlatmeas)
     goodlats=fltarr(nlatmeas,ncols)-10.
     for ii=0,nlatmeas-1 do begin
        htind[ii]=min(where(y_rsun_array ge lat_heights[ii]))
     ;Find where the lat_heights are larger than the radial limits 'htlimits'.
        tt=where(htlimits-lat_heights[ii]*ind_arr[0].rsun_obs ge 0.)
        if tt[0] ne -1 then goodlats[ii,tt]=y_arcsec_array[htind[ii]]
        plots,x_deg_array,goodlats[ii,*],psym=1,thick=1,symsize=0.1
     ;Overplot the lateral measurements location
     ;oplot,thrang*180./!PI,[y_arcsec_array[htind[ii]],y_arcsec_array[htind[ii]]],thick=2
     endfor
endif
; Save the image as a PNG 
        if keyword_set(run) then begin
              folder='arun/'
              prefix='annplot_'
              postfix='run'
        endif else begin
           if keyword_set(base) then begin
              folder='abase/'
              prefix='annplot_'
              postfix='base'
           endif else begin
              folder='araw/'
              prefix='annplot_'
              postfix='raw'
           endelse
        endelse

;Save the overview plot
     ;filename=savepath+'abase/'+wave+'/'+event.annplot.analyzed_savename+wave+'_base_'+strind+'.png'
     ;write_png,filename,tvrd(/true),ct_rr,ct_gg,ct_bb
        write_png, savepath + folder + passband +'/' + $
                   prefix + date + '_' + event.label+'_'+passband+'_'+postfix+'_'+strind+'.png', $
                   TVRD(/true),r,g,b     
        
endfor
  set_plot, 'x'
;----------------------------------------------------
     
end
;-============================================================================

  
;+============================================================================
pro aia_annulus_plot,event,run=run,base=base,raw=raw,_extra=_extra
  if not keyword_set(run) and not keyword_set(base) then raw=1
  if keyword_set(raw) then aia_annulus_plot_main, event, /raw, _extra=_extra
  if keyword_set(base) then aia_annulus_plot_main, event, /base, _extra=_extra
  if keyword_set(run) then aia_annulus_plot_main, event, /run, _extra=_extra
end 
;-============================================================================
