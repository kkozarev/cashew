pro test_aia_annulus_plot_results_radial
  ;Test procedure
  labels=['110607_01']
  for ev=0,n_elements(labels)-1 do begin
     label=labels[ev]
     event=load_events_info(label=label)
     aia_annulus_plot_results_radial, event,/ps;, /force
  endfor
end



pro aia_annulus_plot_results_radial, event, ps=ps

  ;Plot the results of the kinematics fitting on a J-map.
  ;This version works on automatic measurements only, for now!
  ;Written by Kamen Kozarev, 08/27/2017
  !p.multi=0
  
  ;---------------Loads part of the event---------------------------------
  savepath=event.annuluspath
  save_fname=replace_string(event.annplot.analyzed.radial.savename,'WAV','193')
  save_fname=replace_string(save_fname,'SSSSS','_mxxxx')

  ;Find the analysis results file
  search_fname = event.annuluspath + 'annplot_'+strtrim(event.date)+'_'+strtrim(event.label)+'_193_analyzed_radial*_am*.sav'
  fnames=file_search(search_fname)
  restore, fnames[0]
  
  ;Defining some values for later
  sp=rad_data.timefitrange[0]
  ep=rad_data.timefitrange[1]
  yrng=rad_data.radfitrange
  time=rad_data.time
  xrng=rad_data.timefitrange
  yarray=rad_data.y_rsun_array
  dt2=(rad_data.time[1].jd-rad_data.time[0].jd)/2.
  ntimes=n_elements(rad_data.data[*,0])

  ;----------------------------
  ;Figure out the file names for the .sav files and the plot files.
  pa=get_polar_angle(event)
  strpa=strtrim(string(pa,format='(f6.2)'),2)
  analysis_run_id = '00'
  wav='193'
  
  save_fname=replace_string(event.annplot.analyzed.radial.savename,'SSSSS','_pa'+strpa+'_am'+analysis_run_id)
  save_fname=replace_string(save_fname,'WAV',wav)
  
  plot_fname=replace_string(event.annplot.analyzed.radial.plot_savename,'SSSSS','_pa'+strpa+'_am'+analysis_run_id)
  plot_fname=replace_string(plot_fname,'WAV',wav)
  ploteps=savepath+strjoin((strsplit(plot_fname,'.',/extract))[0:1],'.')+'.eps'
  plotpng=savepath+plot_fname
  ;----------------------------

  
  
  ;----------------------------
  ;If not present, fit the kinematics of the front, peak, and back of the wave!
  if mean(rad_data.savgolfits.back.speed) eq 0. or $
     mean(rad_data.savgolfits.front.speed) eq 0. or $
     mean(rad_data.savgolfits.peak.speed) eq 0. then begin
     print,'Fitting kinematics!'
     fit_wave_kinematics_radial,rad_data,ind_arr,/front
     fit_wave_kinematics_radial,rad_data,ind_arr,/peak
     fit_wave_kinematics_radial,rad_data,ind_arr,/back
     save,filename=savepath+save_fname,rad_data,ind_arr,annulus_info
  endif
  ;----------------------------

  
  
  ;----------------------------
  ;Do the actual plotting
  
  if keyword_set(ps) then begin
     set_plot,'ps'
     !p.position=[0.15,0.12,0.93,0.9]
     !p.background=255
     !p.color=0
     !p.font=0
     device,file=ploteps,/inches,xsize=11,ysize=9,$
            /encaps,/color,/helvetica
  endif else begin
     !p.background=255
     !p.color=0
     !p.font=-1
     wdef,0,1200,1000
  endelse
  if tag_exist(rad_data,'diffdata') then begin
     sm=smooth(rad_data.diffdata,[14,1],/edge_truncate)
     smoothdata=rad_data.diffdata-sm
  endif else begin
     diff=rad_data.origdata
     for ii=0,n_elements(diff[*,0])-1 do diff[ii,*] = rad_data.origdata[ii,*]-reform(rad_data.origdata[0,*])
     sm=smooth(diff,[14,1],/edge_truncate)
     smoothdata=diff-sm
  endelse
  ;smoothdata=rad_data.data-smooth(rad_data.data,[14,1],/edge_truncate)

  aia_plot_jmap_data,time.jd,yarray[yrng[0]:yrng[1]],smoothdata[*,yrng[0]:yrng[1]],charsize=2,$
                     min=-10,max=20,title='',xtitle=plotinfo.xtitle,ytitle=plotinfo.ytitle

  event=load_events_info(label=event.label)
  oplot,[time[xrng[0]].jd,time[xrng[0]].jd]+dt2,[yarray[yrng[0]],yarray[yrng[1]]],thick=4
  oplot,[time[xrng[0]].jd,time[xrng[0]].jd]+dt2,[yarray[yrng[0]],yarray[yrng[1]]],thick=4,color=255,linestyle=2
  oplot,[time[xrng[1]].jd,time[xrng[1]].jd]+dt2,[yarray[yrng[0]],yarray[yrng[1]]],thick=4
  oplot,[time[xrng[1]].jd,time[xrng[1]].jd]+dt2,[yarray[yrng[0]],yarray[yrng[1]]],thick=4,color=255,linestyle=2  
  
  if sp eq ep then begin
     print,''
     print,"Starting and ending times are the same - can't plot or save measurements."
  endif else begin
     loadct,39,/silent
     oplot,time[sp:ep].jd+dt2,rad_data.wave_frontedge[sp:ep].rad,psym=sym(3),symsize=3,color=50
     oplot,time[sp:ep].jd+dt2,rad_data.wave_frontedge[sp:ep].rad,psym=sym(8),symsize=3,color=0,thick=3
     oplot,time[sp:ep].jd+dt2,rad_data.wave_peak[sp:ep].rad,psym=sym(1),symsize=3,color=150
     oplot,time[sp:ep].jd+dt2,rad_data.wave_peak[sp:ep].rad,psym=sym(6),symsize=3,color=0,thick=3
     oplot,time[sp:ep].jd+dt2,rad_data.wave_backedge[sp:ep].rad,psym=sym(2),symsize=3,color=200
     oplot,time[sp:ep].jd+dt2,rad_data.wave_backedge[sp:ep].rad,psym=sym(7),symsize=3,color=0,thick=3
     loadct,0,/silent
     wait,2
  endelse
  ;----------------------------

  
  ;----------------------------
  ;Save the plot
  if keyword_set(ps) then begin
     device,/close
     ;here do conversions to PNG files.
     exec='convert -flatten '+ploteps+' '+plotpng
     spawn,exec
     set_plot,'x'
  endif else begin
     write_png, plotpng, tvrd(/true)
     wdelete, 1
  endelse
     print,''
     print,'Saved plot file '+plotpng
     print,''
  ;----------------------------
  wdel,0
end
