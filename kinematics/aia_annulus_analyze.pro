pro test_aia_annulus_analyze
;Procedure to run and test aia_annulus_analyze

;You can run for one event, like this.
  one=1
  if one eq 1 then begin
     wav='193'
     rrange=[1.11,1.34]
     event=load_events_info(label='test')
     aia_annulus_analyze,event,wave=wav;,rrange=rrange ;,/interactive
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
           aia_annulus_analyze,event,wave=wavelength,rrange=rrange ;,/interactive
        endfor
     endfor
  endif


end



pro aia_annulus_analyze,event,datapath=datapath,savepath=savepath,thrange=thrange,interactive=interactive,wave=wave,rrange=rrange
;PURPOSE:
;Procedure to analyze the speeds of radial and lateral expansion of a
;wave and/or a filament.
;Uses output from aia_annulus_plot.pro, a procedure deprojecting AIA
;data onto a rectangular grid, where the X-axis is latitude along the
;limb, and the Y-axis is radial distance.
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
;
;OUTPUTS:
;
; ;DEPENDENCIES:
;
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 08/07/2013
;   2013/11/19, Kamen Kozarev - Integrated the event structure,
;               updated and streamlined the procedure
;   2016/02/12, Kamen Kozarev - This is just a wrapper for the aia_annulus_analyze_radial
;                               and  aia_annulus_analyze_tangential procedures.
 
aia_annulus_analyze_radial,event,datapath=datapath,savepath=savepath,thrange=thrange,$
                               wave=wave,rrange=rrange,constrain=constrain, gradient=gradient, auto=auto

cashew_annulus_analyze_lateral,event,datapath=datapath,savepath=savepath,$
                                   thrange=thrange,interactive=interactive,wave=wave,rrange=rrange,$
                                   constrain=constrain, gradient=gradient, auto=auto


end
