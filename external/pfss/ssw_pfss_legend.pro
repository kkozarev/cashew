function ssw_pfss_legend, pfss_data , pfss_index=pfss_index, _extra=_extra, $
   background=background, debug=debug, current_window=current_window, $
   time=time, ecliptic=ecliptic
;
;+
;   Name: ssw_pfss_legend
;
;   Purpose: annotate an SSW generated PFSS plot
;
;   Input Parameters:
;      pfss_data - pfss data array -or- graphics file (local or url)
;
;   Keyword Parameters:
;      Zbuffer - (switch) - if set, assume PFSS plot in existing Zbuffer
;      
debug=keyword_set(debug)
zbuffer=keyword_set(zbuffer)
if zbuffer and strupcase(!d.name) ne 'Z' then begin 
   box_message,'/ZBUFFER requested but current devices is non-Z, returning...
   return,-1
endif
curwin=keyword_set(current_window)

   
case 1 of 
   data_chk(pfss_data,/nimage) eq 1: pdata=pfss_data
   curwin: if n_params() eq 0 then pdata=tvrd() else pdata=pfss_data ; current X or Win
   data_chk(pfss_data,/string): begin 
       if file_exist(pfss_data) then pdata=files2data(pfss_data) else begin 
         if strpos(pfss_data,'http:') ne -1 then begin 
            break_url,pfss_data,host,path,file
            tdir=get_temp_dir()
            sock_copy,pfss_data,out_dir=tdir,use_network=1
            lname=concat_dir(tdir,file)
            if not file_exist(lname) then begin
               box_message,'Problem with http access, returning...'
               return,-1
            endif
            pdata=files2data(lname)
         endif else begin 
            box_message,'Expect PFSS array or filename or url...'
            return,-1
         endelse
      endelse
   endcase
   keyword_set(zbuffer): begin 
   endcase
   else: begin 
      box_message,'Cannot figure out form/type of your PFSS data??'
      return,-1
   endcase
endcase

pdata=pdata<240              ; kludge??
pcolors=[4,5,12,7,240,11]
llab=['NOAA AR#','Closed Field','Open Field Pos.(+)','Open Field Neg(-)',$
      'Coronal Hole Pos.(+)','Coronal Hole Neg(-)']
maxc=n_elements(pcolors)
sslabs=intarr(maxc)
for i=0,maxc-1 do begin 
   sslabs(i) = total(pdata eq pcolors(i)) gt 0
;   sslabs(i) = sslabs(i) and $
;      (pcolors(i) ne 11 or (total(pdata eq 240)) gt 0)
endfor
ssl=where(sslabs,lcnt) ; subset included in This PFSS
needz=(1-zbuffer) and (1-curwin)
if needz then   begin
   ptemp=!d.name
   tvlct,r,g,b,/get 
   wdef,im=pdata,/zbuffer
   linecolors
endif
tv,pdata
if not keyword_set(background) then background=150
legend,'!3'+llab(ssl),charsize=data_chk(pdata,/nx)/512.>.50, $
   textcol=pcolors(ssl),back=background, _extra=_extra, $
   pos=[.03,.98],/norm , spacing=data_chk(pdata,/nx)/256.

retval=tvrd()
if needz then begin 
   set_plot,ptemp
   tvlct,r,g,b
endif
if debug then stop
return,retval
end 

