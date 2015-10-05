pro run_test_yaftawave_mask_analysis
  event=load_events_info(label='110511_01')
  path='/Users/kkozarev/corwav/events/'+event.label+'/yaftawave/'
  
;=======================================
;USER INPUT
  run=17
;=======================================
  
  
;Find the folder we're interested in and extract the version, 
;smoothing, and threshold information
  version=''
  folders=file_basename(file_search(path+'gprep_v*'))
  nf=n_elements(folders)
  for ff=0,nf-1 do begin
     tmp=strsplit(folders(ff),'_',/extract)
     res=strsplit(tmp[2],'run',/extract)
     if fix(res) eq run then begin
        version=tmp[1]
        thresh=tmp[3]
        smooth=tmp[4]
        strrun='run'+strtrim(string(run),2)
        break
     endif
  endfor
  if version eq '' then begin
     print,''
     print,'No run '+strtrim(string(run),2)+' exists, quitting...'
     print,''
  endif

  index_filename=path+"AIA_20110125_193_"+version+"_index.sav"
  datapath=path+'gprep_'+version+'_'+strrun+'_'+thresh+'_'+smooth+'/'
  yaftawave_filename='yaftawave_gprep_'+version+'_20110511_110511_01_193'
  ;Run the code
  test_yaftawave_mask_analysis,datapath,yaftawave_filename,index_filename
end




pro test_yaftawave_mask_analysis,path,filename,index_filename,event
;Play with the YAFTA/Wave results
set_plot,'x'
;1. Load the save file with the YAFTA/Wave-processed data
;restore,'/Users/kkozarev/corwav/events/110511_01/yaftawave/gprep_v2_run6_3000px_sm2/yaftawave_gprep_v2_20110511_110511_01_193.sav'
restore,path+filename+'.sav'

;2. Load the index information
  restore,index_filename
  ;Get the times right
  strtime=subindex[*].date_obs
  tm=anytim2jd(strtime)
  tm=1.D*tm.int+tm.frac
  RSUN=subindex[0].rsun_ref/1000. ;Solar radius in km.

;This mask contains the radii of all pixels in the particular FOV
radarray=replicate({x:0.0,y:0.0,r:0.0},1024,2048)
for xx=0,1023 do begin
   for yy=0,2047 do begin
      xpos=(4095-1024-subindex[0].X0_MP+xx) ;The pixel x-distance from Sun-center
      radarray[xx,yy].x=xpos
      ypos=(4095-2048-subindex[0].Y0_MP+yy) ;The pixel y-distance from Sun-center
      radarray[xx,yy].y=ypos
      radarray[xx,yy].r=sqrt(xpos^2+ypos^2)/(subindex[0].R_SUN)    
   endfor
endfor

nfeatures=max(features.label)
nt=n_elements(subindex)
;nt=max(features.step)
fpos=where(features.sign[0] eq 1)
fneg=where(features.sign[0] eq -1)
;featinfo=replicate({npix:0.,},nfeatures,nt)
cc=0.
pp=0.
nn=0.
aa=0.
pixthreshold=20000.

;goto,jump1

;Find the number of positive unique feature realizations
for ff=0,nfeatures-1 do begin
   ind=where(features.label eq ff+1)
   if ind[0] eq -1 then continue
   feat=features[ind]
   
;Keep only the unique steps
   uid=uniq(feat.step,sort(feat.step))
   ufeat=feat[uid]

;Figure out the positive and negative features, including the
;discriminating conditions
   if ufeat[0].sign eq -1 then nn++
   if ufeat[0].sign eq 1 and mean(ufeat.size) gt pixthreshold then begin   ;and n_elements(ufeat) gt 2 
      pp++
      
      res=strsplit(ufeat[0].mask_str,"\t \r",/extract)
;tmp[0,*] are the X-indices, tmp[1,*] are the Y-indices
      tmp=array_indices(radarray,res*1L)
;Find the Sun-radial distance of each pixel in the feature
;First, look up the pixel positions in the radarray
      tmp2=reform(radarray[tmp[0,*],tmp[1,*]].r)
;Then check which pixels are above the limb
      tmp3=where(tmp2 gt 1.)
      
;The pixels we want are now in tmp
      if tmp3[0] gt -1 then aa++

   endif
endfor


;===================================================
;For every feature, get the 2D pixel coordinates and
;their distance from Sun center in Rsun.
;loadct,0,/silent
;wdef,0,900,800
!p.position=[0.12,0.12,0.9,0.9]
!p.font=0
  dummy = LABEL_DATE(DATE_FORMAT=['%H:%I'])

  set_plot,'ps'
  fname=path+filename+'_pxsummary'
  device,file=fname+'.eps',/inches,xsize=10.0,ysize=8.0,$
         /encaps,/color,/helvetica

  loadct,0,/silent
  xrange=[tm[0],tm[nt-1]]
  plot,tm,dindgen(max(features.size)),/nodata,$
       background=255,color=0,ystyle=1,xstyle=1,$
       xrange=xrange,yrange=[pixthreshold,max(features.size)-1],$
       charsize=1.6,charthick=2.,/ylog,XTICKUNITS = ['Time'],XTICKFORMAT='LABEL_DATE',$
       xtitle="Time",ytitle="#Pixels",xminor=5,$
       title="YAFTA/Wave Feature Pixels" ;, Thresh="+strtrim(string(pixthreshold),2)+' px'
  loadct,13,/silent
;1. Reorganize the features information
  for ff=0,nfeatures-1 do begin
;First, check if the label index exists
     ind=where(features.label eq ff+1)
     if ind[0] eq -1 then continue
     
     feat=features[ind]
;Keep only the unique steps
     uid=uniq(feat.step,sort(feat.step))
     ufeat=feat[uid]
     
;Keep only the positive features
     if ufeat[0].sign eq -1 then continue
     
     if mean(ufeat.size) gt pixthreshold then begin
        
;Plot the pixel number vs. time step.
  ;      if n_elements(ufeat) ge 2 then begin
           if n_elements(ufeat) ge 2 then oplot,tm[ufeat.step-1],ufeat.size,color=30+225.*cc/(pp-1)
           plots,tm[ufeat.step-1],ufeat.size,color=30+225.*cc/(pp-1),psym=sym(1),symsize=1.5
           ;print,tm[ufeat[0].step],ufeat[0].label
                                ;Feature labels printed by the graph
                                ;xyouts,!p.position[2]+0.005,!p.position[3]-0.02*cc,ufeat[0].label,color=30+225.*cc/(pp-1),/norm
                                ;Feature labels printed at the first detection of every feature
           loadct,0,/silent
           xyouts,tm[ufeat[0].step-1]-1.3/1440.,ufeat[0].size,ufeat[0].label,color=0.,/data,charsize=1.6
           loadct,13,/silent
           xyouts,tm[ufeat[0].step-1]-1.2/1440.,ufeat[0].size,ufeat[0].label,color=30+225.*cc/(pp-1),/data,charsize=1.5
           cc++
  ;      endif
     endif
  endfor
  
  device,/close
  exec='convert -flatten '+fname+'.eps '+fname+'.png ; rm -rf '+fname+'.eps '
  spawn,exec
  set_plot,'x'
;===================================================
  
  
;Find all the pixels above the solar limb, and their radial distance
;from the Sun center
  cc=0
  mtp={avg:-1.,max:-1.,startstep:0,endstep:0}
  all_abovepx=replicate(mtp,pp,nt)
  featlabel=strarr(pp)
  featstartendsteps=intarr(pp,2)
;1. Reorganize the features information
  define_check = 0
  for ff=0,nfeatures-1 do begin
;First, check if the label index exists
     ind=where(features.label eq ff+1)
     if ind[0] eq -1 then continue
     
     feat=features[ind]
;Keep only the unique steps
     uid=uniq(feat.step,sort(feat.step))
     ufeat=feat[uid]
    
;Keep only the positive features
     if ufeat[0].sign eq -1 then continue     
     
     if mean(ufeat.size) gt pixthreshold then begin        ;and n_elements(ufeat) gt 2 
;Get the pixel 2D coordinates, relate them to the radial distance from
;Sun-center.
;This has to be done for every member of the ufeat array.
        numfeatsteps=n_elements(ufeat)
        print,string(ufeat[0].label) + ' -> ' + string(ufeat[numfeatsteps-1].trm)
        
        for nfs=0,numfeatsteps-1 do begin           
           res=strsplit(ufeat[nfs].mask_str,"\t \r",/extract)
;tmp[0,*] are the X-indices, tmp[1,*] are the Y-indices
           tmp=array_indices(radarray,res*1L)
;Find the Sun-radial distance of each pixel in the feature
;First, look up the pixel positions in the radarray
           tmp2=reform(radarray[tmp[0,*],tmp[1,*]].r)
;Then check which pixels are above the limb (i.e., R > 1.)
           tmp3=where(tmp2 gt 1.)
           
;The pixels we want are now in tmp[*,tmp3]
           if tmp3[0] gt -1 then begin
              tmp4=tmp[*,tmp3]
;Continue this, extracting the information for the feature and
;determining whether this is the right feature to fit a circle to.
             
;Try ordering the pixels by R, and fitting a circle to the farthest 50
;pixels. See what the goodness of fit is for every feature.
              above_limb_px=reform(radarray[tmp4[0,*],tmp4[1,*]].r)
              avgabovepx=average(above_limb_px)
              maxabovepx=max(above_limb_px)
              all_abovepx[cc,nfs].avg=avgabovepx
              all_abovepx[cc,nfs].max=maxabovepx
              featlabel[cc]=ufeat[nfs].label
              featstartendsteps[cc,0]=ufeat[0].step-1
              featstartendsteps[cc,1]=ufeat[numfeatsteps-1].step-1
             ; print,ff,n,avgabovepx,maxabovepx,255.*cc/(pp-1)
;Get all the X- and Y- pixel distances from the center of the Sun.
            ; alx=reform(radarray[tmp4[0,*],tmp4[1,*]].x)
            ; aly=reform(radarray[tmp4[0,*],tmp4[1,*]].y)
             
            ;indices of the pixels ordered by radial distance
              un=uniq(above_limb_px,sort(above_limb_px))
            ;Fit a circle to the feature with the fastball procedure
            ;from the test_fastball.pro file.
              pt={x:0.,y:0.}
              npts=n_elements(tmp4[0,*])
              pts=replicate(pt,npts)
              for i=0.,n_elements(pts)-1 do begin
                 pts[i].x=tmp4[0,i]
                 pts[i].y=tmp4[1,i]
              endfor
              ball=fastball(pts)
              print,nfs
              plot_scale_factor=4
              if define_check eq 0 then begin
                 wdef,0,n_elements(all_masks[*,0,0])/plot_scale_factor,n_elements(all_masks[0,*,0]/plot_scale_factor)
                 feature_circle_fits=replicate(ball,nfeatures,n_elements(tm))
                 define_check = 1
              endif
              feature_circle_fits[ff,nfs]=ball
              circle=circle(ball.center.x/plot_scale_factor, ball.center.y/plot_scale_factor, ball.radius/plot_scale_factor)
              plots,circle,/device,color=120
           endif else begin
              continue
           endelse
        endfor  
        cc++
     endif
        if define_check eq 1 then $
        wdef,0,n_elements(all_masks[*,0,0])/plot_scale_factor,n_elements(all_masks[0,*,0])/plot_scale_factor
  endfor
  save,filename="abovepx.sav",nfeatures,nt,tm,features,all_abovepx,feature_circle_fits,pixthreshold,pp,featlabel,featstartendsteps
jump1:

restore,'abovepx.sav'

  
  !p.font=0
  
  dummy = LABEL_DATE(DATE_FORMAT=['%H:%I'])
  ;loadct,0,/silent
  !P.multi=[0,2,1]
  ;wdef,0,1200,800
  !p.font=0
  set_plot,'ps'
  fname=path+filename+'_radstats'
  device,file=fname+'.eps',/inches,xsize=10.0,ysize=8.0,$
         /encaps,/color,/helvetica
  
  loadct,0,/silent
  
  cc=0
  ;Plot the mean positions of the pixel coordinates above the limb.
  !p.position=[0.12,0.12,0.52,0.93]
  plot,dindgen(max(features.size)),tm,/nodata,$
       background=255,color=0,ystyle=1,xstyle=1,$
       yrange=[tm[0],tm[nt-1]],xrange=[1.0,max(all_abovepx.avg)],$
       charsize=1.5,charthick=1.5,YTICKUNITS = ['Time'],YTICKFORMAT='LABEL_DATE',$
       ytitle="Time",yticks=3,xtitle="Mean Position Above Limb",yminor=5,$
       title="Feature Mean Position" ; Thresh="+strtrim(string(pixthreshold),2)+' px'
  loadct,13,/silent
  
  for ff=0,pp-1 do begin
     sstep=featstartendsteps[ff,0]
     estep=featstartendsteps[ff,1]
     in=where(all_abovepx[ff,*].avg gt -1.)
     
     if in[0] gt -1 then begin ; and n_elements(in) gt 2
        ;print,ff,255.*cc/(pp-1)
        ;if n_elements(in) gt 4 then avgpix=smooth(all_abovepx[ff,in].avg,4) $
        ;   else 
        avgpix=all_abovepx[ff,in].avg
        if  n_elements(in) ge 2 then oplot,avgpix,tm[sstep:estep],color=30+225.*cc/(pp-1),thick=2
        plots,avgpix,tm[sstep:estep],color=30+225.*cc/(pp-1),psym=sym(1),symsize=1.2
       ; xyouts,!P.position[2]+0.02,0.9-cc*0.02,featlabel[ff],/norm,color=30+225.*cc/(pp-1)
        cc++
     endif
     
  endfor

  !p.font=0
  ;Plot the maximum pixel positions above the limb.
  !p.position=[0.53,0.12,0.93,0.93]
  cc=0
  plot,dindgen(max(features.size)),tm,/nodata,$
       background=255,color=0,ystyle=1,xstyle=1,$
       yrange=[tm[0],tm[nt-1]],xrange=[1.0,max(all_abovepx.max)],$
       charsize=1.5,charthick=1.5,YTICKUNITS = ['Time'],ytickformat='(A1)',$
       ytitle="",yticks=3,xtitle="Maximum Position Above Limb",yminor=5,$
       title="Feature Max Position";, Thresh="+strtrim(string(pixthreshold),2)+' px'
  loadct,13,/silent
  
  for ff=0,pp-1 do begin
     sstep=featstartendsteps[ff,0]
     estep=featstartendsteps[ff,1] 
     in=where(all_abovepx[ff,*].avg gt -1.)
     
     if in[0] gt -1 then begin ;and n_elements(in) gt 2 
       ; print,ff,255.*cc/(pp-1)
       ; if n_elements(in) gt 4 then avgpix=smooth(reform(all_abovepx[ff,in].max),4) $
       ;    else 
        avgpix=reform(all_abovepx[ff,in].max)
        if  n_elements(in) ge 2 then oplot,avgpix,tm[sstep:estep],color=30+225.*cc/(pp-1),thick=1.5
        plots,avgpix,tm[sstep:estep],color=30+225.*cc/(pp-1),psym=sym(1),symsize=1.2
        xyouts,!P.position[2]+0.02,0.92-cc*0.03,strtrim(string(featlabel[ff]),2),/norm,color=30+225.*cc/(pp-1),charsize=1.5

        cc++
     endif
    ; stop
  endfor

  device,/close
  exec='convert -flatten '+fname+'.eps '+fname+'.png ; rm -rf '+fname+'.eps '
  spawn,exec
  set_plot,'x'
  
  
 loadct,0,/silent
 !P.multi=0
 !p.position=[0.12,0.12,0.93,0.93]
 
;Plot the derivative
 wdef,1,600,800
 cc=0
  plot,dindgen(max(features.size)),tm,/nodata,$
       background=255,color=0,ystyle=1,xstyle=1,$
       yrange=[tm[0],tm[nt-1]],xrange=[-1,1],$
       charsize=2,charthick=2.,YTICKUNITS = ['Time'],ytickformat='(A1)',$
       ytitle="",yticks=3,xtitle="Derivative of Max Position Above Limb",yminor=5,$
       title="YAFTA/Wave Feature D(Max Position Above Limb), Thresh="+strtrim(string(pixthreshold),2)+' px'
  loadct,13,/silent
  
  for ff=0,pp-1 do begin
     sstep=featstartendsteps[ff,0]
     estep=featstartendsteps[ff,1] 
     in=where(all_abovepx[ff,*].avg gt -1.)
     stop
     if in[0] gt -1 and n_elements(in) gt 2 then begin
        stop
        ;print,ff,255.*cc/(pp-1)
        avgpix=deriv((tm[sstep:estep]-tm[sstep])*1440,reform(all_abovepx[ff,in].max))
        oplot,avgpix,tm[sstep:estep],color=30+225.*cc/(pp-1),thick=1.5
        plots,avgpix,tm[sstep:estep],color=30+225.*cc/(pp-1),psym=sym(1)
        xyouts,!P.position[2]+0.02,0.9-cc*0.02,featlabel[ff],/norm,color=30+225.*cc/(pp-1),charsize=2
     endif
     cc++
  endfor

  
end
