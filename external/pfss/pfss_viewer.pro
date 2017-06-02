;+
;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;
;  pfss_viewer - an IDL-based widget (GUI) application for viewing PFSS data
;
;  notes:  -Main strategy is to store the restored info from the IDL save 
;           file as well as the fieldline info in a common block so that it 
;           is globally available, and to store the state of the widget in a 
;           state structure stashed in the uvalue of the top level base.
;          -Complains if multiple file browser widgets are opened.  Should
;           have a check to see if one is already open (if so, then pop) 
;           before opening a second one.
;          -May want to use slat instead of lat for equal-area purposes
;          -Currently uses direct graphics which might be slow since all
;           rendering is done via software, might want to check out object
;           graphics in the future, which supposedly uses hardware as well as
;           software.  See the surf_track.pro procedure for an example.
;           One drawback to object graphics might be that the IDLgrWindow
;           object has a max size limit that differs from system to system.
;          -Remote use restricted to IDL version 5.4 or greater.
;
;  to do:  -data products could include "slicer" image, separation map, mask
;          -include option to hide closed field lines and blink
;          -change background image, accommodate cropping, overlay
;          -X/Y field line starting points
;          -filename widget
;          -VRML button
;          -message for SOHO vacation period
;
;  M.DeRosa     - 10 Feb 2003 - created
;                 21 Mar 2003 - moved field line bounding box to preview
;                               widget, and absorbed date widget here
;                 27 Mar 2003 - fixed bug with file listing (ls -1 vs ls)
;  S.L.Freeland - 28 Jan 2004 - permit remote SSW and make OS independent
;  M.DeRosa     -  3 Mar 2004 - put common block in its own include file
;                 23 Jan 2006 - now uses fixed-width fonts for all text boxes
;                 23 Aug 2006 - now exits cleanly if catalog is missing
;                 23 Aug 2006 - in the rendering widget, when the "display
;                               accurate line crossings button" is set to
;                               "yes", pfss_draw_field3 and object graphics
;                               are now used (instead of pfss_draw_field2
;                               which uses direct graphics and is much slower)
;                 15 Nov 2006 - fixed a bug in the "Save to TIFF" option
;                 29 Dec 2012 - made routine version 2 compliant
;
;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;-

; common block (put here to make data available on the command line)
@pfss_data_block

pro pfss_viewer_event,event

common pfss_date_block,pyr,pmo,pdy,ptm
@pfss_data_block

;  what happened?
widget_control,event.id,get_uval=uval
case uval of

  'EXIT': widget_control,event.top,/destroy

  'LOAD': begin

    ;  get state
    widget_control,event.top,get_uval=state,/no_copy,/hourglass

    ;  if user has not selected a date/time, the generate error and return
    if total(state.pind lt 0) gt 0 then begin
      result=dialog_message('Please choose a date/time.',$
        /err,dialog_parent=event.top)
      widget_control,event.top,set_uval=state,/no_copy
      return
    endif

    ;  determine filename, either remote or local (Sam's stuff)
    state.pdate=pyr(state.pind(0))+'-'+pmo(state.pind(1))+'-'+ $
      pdy(state.pind(2))
    state.ptime=ptm(state.pind(3))
    sswtime=anytim(arr2str([state.pdate,state.ptime],' '),/ecs)
    state.fname=pfss_time2file(sswtime,/ssw_cat)  ;  first try local
    if (not file_exist(state.fname)) $
        or (get_logenv('pfss_force_remote') ne '') then begin
      pfssurl=pfss_time2file(sswtime,/ssw_cat,/url)   ; url synonym
      break_url,pfssurl,IP,urlpath,urlfile
      tempdir=get_temp_dir()  ;  TODO - > $SSWDB/packages/pfss/?
      state.fname=concat_dir(tempdir,urlfile)  ;  local name post sock_copy
      if not file_exist(state.fname) then begin  
        result=dialog_message([ 'Cannot find local copy of '+$
          ssw_strsplit(state.fname,'/',/tail,/last),$
          'Now trying remote access via http','','>>> '+pfssurl],/info,$
          dialog_parent=event.top)
        sock_copy,pfssurl,out_dir=tempdir,use_network=1,/prog
        if not file_exist(state.fname) then begin 
          result=dialog_message('Hmmm, '+state.fname+' does not exist!',/err, $
            dialog_parent=event.top)
          widget_control,event.top,set_uval=state,/no_copy
          return
        endif else box_message,'http access successful'
      endif else box_message,'Using previous http tranfer already local...'
    endif

    ;  restore file
    print,'  pfss_viewer:  restoring '+state.fname
    widget_control,state.wloadlabel,set_val='Loading data...'
    widget_control,state.wdatelabel,set_val=state.pdate+'  '+state.ptime
    pfss_restore,state.fname
    print,'  pfss_viewer:  '+ state.fname+' restored.'
    widget_control,state.wloadlabel,set_val='Currently loaded:'

    ;  make sure l0 is between 0 and 360 degrees
    l0=(l0+360) mod 360

    ;  save state
    wprev=state.wprevbase  ;  for launching preview widget below
    widget_control,event.top,set_uval=state,/no_copy

    ;  launch preview widget, or just redraw if already opened
    if widget_info(wprev,/valid_id) then begin

      ;  get state
      widget_control,wprev,get_uval=pstate,/no_copy
      redraw=pstate.wredraw

      ;  enter new l0, b0 into widget
      widget_control,pstate.wmapbcent,set_val=string(b0,f='(f7.3)')
      widget_control,pstate.wmaplcent,set_val=string(l0,f='(f7.3)')

      ;  save state
      widget_control,wprev,set_uval=pstate,/no_copy

      ;  and draw
      widget_control,redraw,set_uval='DRAW_PREVIEW', $
        send_event={ID:redraw,TOP:wprev,HANDLER:wprev}

    endif else wprev=widget_pfss_preview(event.top)

    end

  'YR': begin

    ;  get state
    widget_control,event.top,get_uval=state,/no_copy

    ;  get year index
    state.pind(0)=widget_info(state.wyr,/list_select)

    ;  display valid months
    wh=where(strmid(state.flist,state.ppts(0),4) eq pyr(state.pind(0)))
    if state.ppts(1) lt 0 then $
      state.ppts(1)=strpos(state.ftemp,'?',state.ppts(0)+5)
    pmo=strmid(state.flist(wh),state.ppts(1),2)
    pmo=pmo(uniq(pmo))
    widget_control,state.wmo,set_val=pmo

    ;  zero out days, times
    widget_control,state.wdy,set_val=''
    widget_control,state.wtm,set_val=''

    ;  save state
    widget_control,event.top,set_uval=state,/no_copy

    end

  'MO': begin

    ;  get state
    widget_control,event.top,get_uval=state,/no_copy

    ;  if year has not been selected then return
    if state.pind(0) lt 0 then begin
      widget_control,state.wmo,set_list_select=-1
      widget_control,event.top,set_uval=state,/no_copy
      return
    endif

    ;  get month index
    state.pind(1)=widget_info(state.wmo,/list_select)

    ;  display valid days
    wh=where($
      (strmid(state.flist,state.ppts(0),4) eq pyr(state.pind(0))) and $
      (strmid(state.flist,state.ppts(1),2) eq pmo(state.pind(1))))
    if state.ppts(2) lt 0 then $
      state.ppts(2)=strpos(state.ftemp,'?',state.ppts(1)+3)
    pdy=strmid(state.flist(wh),state.ppts(2),2)
    pdy=pdy(uniq(pdy))
    widget_control,state.wdy,set_val=pdy

    ;  zero out times
    ptm=''  &  state.pind(3)=-1
    widget_control,state.wtm,set_val=ptm
  
    ;  save state
    widget_control,event.top,set_uval=state,/no_copy

    end

  'DY': begin

    ;  get state
    widget_control,event.top,get_uval=state,/no_copy

    ;  if month has not been selected then return
    if state.pind(1) lt 0 then begin
      widget_control,state.wdy,set_list_select=-1
      widget_control,event.top,set_uval=state,/no_copy
      return
    endif

    ;  get day index
    state.pind(2)=widget_info(state.wdy,/list_select)

    ;  display valid times
    wh=where($
      (strmid(state.flist,state.ppts(0),4) eq pyr(state.pind(0))) and $
      (strmid(state.flist,state.ppts(1),2) eq pmo(state.pind(1))) and $
      (strmid(state.flist,state.ppts(2),2) eq pdy(state.pind(2))))
    if state.ppts(3) lt 0 then $
      state.ppts(3)=strpos(state.ftemp,'?',state.ppts(2)+2)
    ptm=strmid(state.flist(wh),state.ppts(3),8)
    ptm=ptm(uniq(ptm))
    widget_control,state.wtm,set_val=ptm
 
    ;  save state
    widget_control,event.top,set_uval=state,/no_copy

    end

  'TM': begin

    ;  get state
    widget_control,event.top,get_uval=state,/no_copy

    ;  if day has not been selected then return
    if state.pind(2) lt 0 then begin
      widget_control,state.wtm,set_list_select=-1
      widget_control,event.top,set_uval=state,/no_copy
      return
    endif

    ;  get time index
    state.pind(3)=widget_info(state.wtm,/list_select)
     
    ;  save state
    widget_control,event.top,set_uval=state,/no_copy

    end

  else:  stop  ;  SHOULDN'T BE ABLE TO GET HERE
endcase

end

;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

pro pfss_viewer

common pfss_date_block,pyr,pmo,pdy,ptm

;  setup
ftemp='Bfield_??????????T????????.sav'
ind=[-1,-1,-1,-1]
pts=[-1,-1,-1,-1]

;  get file list
pfssf=pfss_time2file('1-jan-2001',cpfsslinks=cpfsslinks,/ssw_cat)
if n_elements(cpfsslinks) eq 0 then return else flist=temporary(cpfsslinks)

;  screen out non-data files
pts=[strpos(ftemp,'?',0),-1,-1,-1]  ;  other elements for mo,dy,tm fields
wh=where(strmid(flist,0,pts(0)) eq strmid(ftemp,0,pts(0)),nwh)
if nwh eq 0 then begin
  print,'  pfss_viewer:  no valid data files found'
  return
endif else flist=flist(wh)

;  get all years
pyr=strmid(flist,pts(0),4)
pyr=pyr(uniq(pyr))

;  construct master widget
master=widget_base(/col,title='PFSS date chooser',/tlb_frame_attr)

;  construct file browsing tool
fbrowse=widget_base(master,/col,frame=5,/align_center)
filelabel=widget_label(fbrowse,value='Choose date/time:')

;  year
yrwid=widget_base(fbrowse,/row,/align_center,/frame)
yrlabel=widget_label(yrwid,value=' year')
yrlist=widget_list(yrwid,value=pyr,xsiz=5,ysiz=5<n_elements(pyr),uval='YR',$
  font='Courier')
widget_control,yrlist,set_list_top=(n_elements(pyr)-5)>0

;  month
mowid=widget_base(fbrowse,/row,/align_center,/frame)
molabel=widget_label(mowid,value='month')
molist=widget_list(mowid,value='',xsiz=4,ysiz=5,uval='MO',font='Courier')

;  day
dywid=widget_base(fbrowse,/row,/align_center,/frame)
dylabel=widget_label(dywid,value='  day')
dylist=widget_list(dywid,value='',xsiz=4,ysiz=5,uval='DY',font='Courier')

;  time
tmwid=widget_base(fbrowse,/row,/align_center,/frame)
tmlabel=widget_label(tmwid,value='time')
tmlist=widget_list(tmwid,value='',xsiz=8,ysiz=5,uval='TM',font='Courier')

;  load button
butwid=widget_base(fbrowse,/row,/align_center)
loadbut=widget_button(butwid,value='   Load   ',uval='LOAD')

;  info area
loadlabel=widget_base(master,/col,frame=5,/align_center)
loadtitle=widget_label(loadlabel,val=' Currently loaded: ')
datelabel=widget_label(loadlabel,val='    (NONE LOADED)    ')

;  exit buttons
butwid=widget_base(master,/col,/align_center)
exbut=widget_button(butwid,val='   Exit   ',uval='EXIT',/align_center)

;  save the state (w=widget,f=file,p=parameter,win=window)
state={wloadlabel:loadtitle, wdatelabel:datelabel, wbutton:loadbut, $
       wyr:yrlist, wmo:molist, wdy:dylist, wtm:tmlist, $
       wdatebase:-1l, wprevbase:-1l, wfieldbase:-1l, $
       wpreview:-1l, winpreview:-1l, wfield:-1l, winfield:-1l, $
       ftemp:ftemp, flist:flist, fname:'', $
       pdate:'', ptime:'', pind:[-1,-1,-1,-1], ppts:pts, ptest:['']}
widget_control,master,set_uval=state,/no_copy

;  realize
widget_control,master,/realize
xmanager,'pfss_viewer',master,/no_block

end
