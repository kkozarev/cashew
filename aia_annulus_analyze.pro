pro test_aia_annulus_analyze
;Procedure to run and test aia_annulus_analyze

event=load_events_info(label='110511_01')
;event=load_events_info(label='110125_01')
wav='193'
aia_annulus_analyze,event,wave=wav;,/interactive

end



pro aia_annulus_analyze,event,datapath=datapath,savepath=savepath,thrange=thrange,interactive=interactive,wave=wave
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
; 
;DEPENDENCIES:
;
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 08/07/2013
;   2013/11/19, Kamen Kozarev - Integrated the event structure,
;               updated and streamlined the procedure

;Restore the file with the deprojected data
  tmp=strsplit(event.date,'/',/extract)
  date=tmp[0]+tmp[1]+tmp[2]
  if not keyword_set(wave) then wav='193' else wav=wave
  if not keyword_set(savepath) then savepath=event.savepath+'annulusplot/'
  if not keyword_set(datapath) then datapath=savepath
  fname='aia_deprojected_annulus_'+date+'_'+event.label+'_'+wav+'.sav'
  restore, datapath+fname
  
  
  nsteps=n_elements(projdata[*,0,0])
  ncols=n_elements(projdata[0,*,0])
  nrows=n_elements(projdata[0,0,*])
  
  if not keyword_set(interactive) then begin
     nsteps=n_elements(subprojdata[*,0,0])
     ncols=n_elements(subprojdata[0,*,0])
     nrows=n_elements(subprojdata[0,0,*])
  endif
  
;Pixel coordinates in arcseconds from the center of the sun. How do I convert them to km?
  y_arcsec_array=res/ind_arr[0].cdelt1*findgen(nrows)+r_in
  y_rsun_array=y_arcsec_array/ind_arr[0].rsun_obs
  
;The X-angular array (distance along the limb from the pole).
  if keyword_set(interactive) then x_deg_array=findgen(ncols)*ang_step $
  else x_deg_array=findgen(ncols)*ang_step+thrang[0]*180./!PI
  
  set_plot,'x'
  loadct,0,/silent
  tvlct,ct_rr,ct_gg,ct_bb,/get
  !p.font=-1
  !P.position[3]=0.8
  !p.position[1]=0.1
  !P.background=255
  !P.color=0
  !P.charsize=1.6
  wdef,0,1200,800
  if keyword_set(interactive) then begin
  
   img=reform(projdata[fix(nsteps/2),*,*]-projdata[fix(nsteps/2)-1,*,*])
     plot_image, img, xtitle = '!5Theta [degrees from solar north]', $
                 ytitle = '!5Radius [arcsec from Sun center]', $
                 charsize = 4, title = 'AIA deprojected image, '+data, max = 50, $
                 origin = [0,0], charthick = 1.2, $
                 scale = [ang_step, res/ind_arr[0].cdelt1], $
                 pos = [0.1, 0.1, 0.95, 0.95], min = -40
  endif else begin
     
     img=reform(subprojdata[fix(nsteps/3),*,*]-subprojdata[fix(nsteps/3)-1,*,*])
     plot_image, img, xtitle = '!5Theta [degrees from solar north]', $
                 ytitle = '!5Radius [arcsec from Sun center]', $
                 charsize = 4, title = 'AIA deprojected image', max = 50, $
                 origin = [thrang[0]*180./!PI,r_in], charthick = 1.2, $
                 scale = [ang_step, res/ind_arr[0].cdelt1], $
                 pos = [0.1, 0.1, 0.95, 0.95], min = -40, font_name='Hershey 5'
  endelse
  
  limb=ind_arr[0].rsun_obs ;Limb position in arcsec
  ;oplot,thrang*180./!PI,[limb,limb] ;Overplot the limb position
  
  if not keyword_set(thrange) then begin
     if keyword_set(interactive) then begin
        print,''
        print,'On the image, select the MIN(Theta) to include'
        cursor,x,y,/down,/data
        oplot,[x,x],[0,nrows-1],thick=2
        thrang[0]=x
        print,x
        
        print,''
        print,'On the image, select the MAX(Theta) to include'
        cursor,x,y,/down,/data
        oplot,[x,x],[0,nrows-1],thick=2
        thrang[1]=x
        print,x
     endif    
  endif else begin
     thrang=thrange
  endelse
  
  if not keyword_set(interactive) then begin
     if not keyword_set(centerlat) then begin
        if event.arlon lt 0.0 then arlat=270.0+event.arlat $
        else arlat=90.0-event.arlat
     endif else arlat=centerlat
     
;DEBUG!!!
;     arlat=arlat-3.
;DEBUG!!!

     oplot,[arlat,arlat],[min(y_arcsec_array),max(y_arcsec_array)],thick=2
  
     arxcentind=min(where(x_deg_array ge arlat))
     
     lat_heights=[1.02,1.07,1.12]
     htind=intarr(n_elements(lat_heights))
     for ii=0,2 do begin
        htind[ii]=min(where(y_rsun_array ge lat_heights[ii]))   
        oplot,thrang*180./!PI,[y_arcsec_array[htind[ii]],y_arcsec_array[htind[ii]]],thick=2
     endfor
    
  endif else begin
     print,''
     print,'On the image, select the latitude of the AR center:'
     cursor,x,y,/down,/data
     oplot,[x,x],[0,nrows-1],thick=2
     print,x
     arcenter=x
     
     uinput=0.0
     while uinput le 0.0 or uinput gt 5 do begin
        read,uinput,prompt='How many heights for lateral measurements (5 max)? '
        wait,0.1
     endwhile
     
     lat_heights=dblarr(uinput)
     htind=dblarr(uinput)
     
     for ii=0,uinput-1 do begin
        print,''
        print,'Select height for lateral measurement #'+strtrim(string(ii+1),2)
        cursor,x,y,/down,/data
        print,y
        oplot,[0,ncols-1],[y,y],thick=2
        lat_heights[ii]=y
        htind[ii]=min(where(yangular_array ge lat_heights[ii]))
     endfor
     
  endelse
;Save the overview plot
  write_png,savepath+'annplot_'+date+'_'+event.label+'_'+wav+'_overview_plot.png',tvrd(/true),ct_rr,ct_gg,ct_bb

  
  if keyword_set(interactive) then begin
     subfovind=where(new_theta[*,0] ge thrang[0]*!PI/180.0 and new_theta[*,0] le thrang[1]*!PI/180.0)
     plotimg=img[subfovind,*]
     
;  plot_image, plotimg, xtitle = '!5Theta [degrees from solar north]', $
;              ytitle = '!5Radius [arcsec from Limb]', $
;              charsize = 1.5, title = 'AIA deprojected image', max = 50, $
;              origin = [thrang[0]*180./!PI,r_in], charthick = 1.1, $
;              scale = [ang_step, res/ind_arr[0].cdelt1], $
;              pos = [0.1, 0.1, 0.95, 0.95], min = -40
     
     
;The index of the AR center in the SUB-FOV images
     arxcentind=min(where(new_theta[*,0] ge (arlat-thrang[0])*!PI/180.0))
;Get just the data to analyze
     subfovdata=projdata[*,subfovind,*]
     subncols=n_elements(subfovdata[0,*,0])
     andata=projdata[*,subfovind,htind]
  endif else begin
     subfovdata=subprojdata
     subncols=ncols
     andata=subprojdata[*,*,htind]
  endelse
  nlatmeas=n_elements(htind)

  ;Save the tangential data into a structure
  left_tan_data=reform(subfovdata[*,0:arxcentind-1,htind])
  right_tan_data=subfovdata[*,arxcentind:subncols-1,htind]
  tan_data={$
           left:{data:left_tan_data,bdiff:left_tan_data,winind:1,$
                 savename:'annplot_'+date+'_'+event.label+'_'+wav+'_tangential_left.png',$
                 plotinfo:replicate({p:!P, x:!X, y:!Y},nlatmeas)},$
           right:{data:right_tan_data,bdiff:right_tan_data,winind:2,$
                  savename:'annplot_'+date+'_'+event.label+'_'+wav+'_tangential_right.png',$
                  plotinfo:replicate({p:!P, x:!X, y:!Y},nlatmeas)},$
           scale:[24./60.,ang_step],origin:[0,0],$
           xtitle:'Minutes since start',ytitle:'Degrees from AR center',$
           winsize:[1200,800],multi:[0,nlatmeas,1]}    
  
  
  tmp=subfovdata[*,arxcentind-2:arxcentind+2,*]
  rad_data={data:total(tmp,2),scale:[24./60./60,res/ind_arr[0].cdelt1/ind_arr[0].rsun_obs],$
            origin:[0,y_rsun_array[0]],winsize:[600,800],multi:0,winind:3,$
            xtitle:'Hours since start',ytitle:'Rsun from Sun center',$
            savename:'annplot_'+date+'_'+event.label+'_'+wav+'_radial.png'}
  



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;MORE PLOTTING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
;;;;;;;;;;;;;;;;;;;;;;;;
;Plot the Radial Data!
;;;;;;;;;;;;;;;;;;;;;;;;
  !p.multi=rad_data.multi
  tmpdata=rad_data.data
  base=total(tmpdata[0:4,*,*],1)/5.0
  for tt=0,nsteps-1 do tmpdata[tt,*,*]=reform(tmpdata[tt,*,*]-base)
  
;Plot the Radial positions
  wdef,rad_data.winind,rad_data.winsize[0],rad_data.winsize[1]
                                ;DESPIKE THE IMAGE
  tmp=despike_gen(tmpdata)
  
  plot_image,tmp,scale=rad_data.scale,min=-40,max=50,charsize=1.6,charthick=1.2,origin=rad_data.origin, $
             title='Radial Positions !C Start at '+ind_arr[0].date_obs,font_name='Hershey 5',$
             xtitle=rad_data.xtitle,ytitle=rad_data.ytitle
  
  write_png,savepath+rad_data.savename,tvrd(/true),ct_rr,ct_gg,ct_bb  

  stop
  
  
  
  
  
;;;;;;;;;;;;;;;;;;;;;;;;
;Plot the Left Tan Data!
;;;;;;;;;;;;;;;;;;;;;;;;
  !p.multi=tan_data.multi
  tmpdata=tan_data.left.data
  base=total(tmpdata[0:4,*,*],1)/5.0
  for tt=0,nsteps-1 do tmpdata[tt,*,*]=reform(tmpdata[tt,*,*]-base)

;testdata[tt,*,*]=reform(testdata[tt,*,*]-base)
;    testdata[tt,*,*]=reform(testdata[tt,*,*]-testdata[0,*,*])
  
;Plot the Left Tangential positions
  wdef,tan_data.left.winind,tan_data.winsize[0],tan_data.winsize[1]
  for rr=0,nlatmeas-1 do begin
     tmp=reform(tmpdata[*,*,rr])
                                ;REVERSE THE IMAGE COLUMNS SO THE AR IS ALWAYS IN THE BOTTOM
     tmp=reverse(tmp,2)
                                ;DESPIKE THE IMAGE
     tmp=despike_gen(tmp)
     tmpdata[*,*,rr]=tmp
     height=strtrim(string(lat_heights[rr],format='(f5.3)'),2)
     
                                ;The middle height index
     midind=fix(median(indgen(nlatmeas)))
     if rr eq midind then $
        imgtit='Left Tangential Positions | Start at '+ind_arr[0].date_obs+'!C R = '+height+' R!Ds!N' $
     else imgtit='R = '+height+' R!Ds!N'
     
     plot_image,tmp,scale=tan_data.scale,min=-40,max=50,charsize=4,charthick=1.2,origin=tan_data.origin, $
                title=imgtit, font_name='Hershey 5',$
                xtitle=tan_data.xtitle,ytitle=tan_data.ytitle
     ;if rr eq 0 then leftplotinfo=replicate({p:!P, x:!X, y:!Y},nlatmeas)
     tan_data.left.plotinfo[rr]={p:!P, x:!X, y:!Y}
                                ;To restore the plot information and overplot on them, do
                                ;!p=tan_data.left.plotinfo[rr].p &
                                ;!x=tan_data.left.plotinfo[rr].x &
                                ;!y=tan_data.left.plotinfo[rr].y
  endfor
  tan_data.left.bdiff=tmpdata
  write_png,savepath+tan_data.left.savename,tvrd(/true),ct_rr,ct_gg,ct_bb
  
  
;;;;;;;;;;;;;;;;;;;;;;;;
;Plot the Right Tan Data!
;;;;;;;;;;;;;;;;;;;;;;;;
  !p.multi=tan_data.multi
  tmpdata=tan_data.right.data
  base=total(tmpdata[0:4,*,*],1)/5.0
  for tt=0,nsteps-1 do tmpdata[tt,*,*]=reform(tmpdata[tt,*,*]-base)
;testdata[tt,*,*]=reform(testdata[tt,*,*]-base)
;    testdata[tt,*,*]=reform(testdata[tt,*,*]-testdata[0,*,*])

;Plot the Right Tangential positions
  wdef,tan_data.right.winind,tan_data.winsize[0],tan_data.winsize[1]
  for rr=0,nlatmeas-1 do begin
     tmp=reform(tmpdata[*,*,rr])
     ;DESPIKE THE IMAGE
     tmp=despike_gen(tmp)
     height=strtrim(string(lat_heights[rr],format='(f5.3)'),2)
     
    ;The middle
     midind=fix(median(indgen(nlatmeas)))
     if rr eq midind then $
        imgtit='Right Tangential Positions | Start at '+ind_arr[0].date_obs+'!C R = '+height+' R!Ds!N' $
     else imgtit='R = '+height+' R!Ds!N'
     
     plot_image,tmp,scale=tan_data.scale,min=-40,max=50,charsize=4,charthick=1.2,origin=tan_data.origin, $
                title=imgtit,font_name='Hershey 5',$
                xtitle=tan_data.xtitle,ytitle=tan_data.ytitle
     ;if rr eq 0 then rightplotinfo=replicate({p:!P, x:!X, y:!Y},nlatmeas)
     tan_data.right.plotinfo[rr]={p:!P, x:!X, y:!Y}
     ;To restore the plot information and overplot on them, do
     ;!p=leftplotinfo[rr].p & !x=leftplotinfo[rr].x & !y=leftplotinfo[rr].y
  endfor
    tan_data.right.bdiff=tmpdata
    write_png,savepath+tan_data.right.savename,tvrd(/true),ct_rr,ct_gg,ct_bb


 end
