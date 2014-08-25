pro test_opflow3d,saved=saved
; Test the opflow3d program that calculates the velocity fields
  event=load_events_info(label='110511_01')
  savefile=event.yaftawavepath+'opflow3d_result'+event.date+'_'+event.label+'_'+wav+'.sav'
  wav='193'
  if not keyword_set(saved) then begin
     restore,event.savepath+'normalized_AIA_'+event.date+'_'+event.label+'_'+wav+'_subdata.sav'
     
     scale=2.
     stride=2
     nx=n_elements(subdata[*,0,0])
     ny=n_elements(subdata[0,*,0])
     nt=n_elements(subdata[0,0,*])
     nvt=fix((nt-1)/stride)
     
     baseim=rebin(reform(subdata[*,*,0]),nx/scale,ny/scale)
     for i=1,4 do baseim+=rebin(reform(subdata[*,*,i]),nx/scale,ny/scale)
     baseim/=5
     
     diffim=dblarr(nx/scale,ny/scale,nt)
     velfields=dblarr(nx/scale,ny/scale,nvt,2)
     
     for i=0,nt-1 do diffim[*,*,i]=rebin(reform(subdata[*,*,i]),nx/scale,ny/scale)-baseim
     c=0
     for i=0,nvt*stride-1,stride do begin
        result=opflow3d(reform(diffim[*,*,i:i+stride]),12,12,vx,vy,mxx,myy,mxy,direct=1)
        velfields[*,*,c,*]=result
        c++
     endfor
     save,filename=savefile,velfields,diffim,stride,scale,nx,ny,nt,nvt
  endif else begin
     restore,savefile
  endelse
  
  
  wdef,0,nx/scale,ny/scale
;Plot the results with arrows
  for i=0,nvt-1 do begin
     ;loadct,0,/silent
     ;tv,bytscl(diffim[*,*,i*stride],-50,50)
     ;loadct,3,/silent
     vel,reform(velfields[*,*,i,0]),reform(velfields[*,*,i,1]),nvecs=1000,length=1.5
     wait,0.2
  endfor
end
