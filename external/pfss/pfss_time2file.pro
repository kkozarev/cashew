function pfss_time2file, time0, time1, urls=urls, debug=debug, $
   before=before, after=after, count=count, refresh=refresh, $
   ssw_catalog=ssw_catalog, generate=generate, loud=loud, $
   cpfsslinks=cpfsslinks , csswlinks=csswlinks, $
   surffield=surffield, version=version, hdf5=hdf5, h5=h5
;+
;   Name: pfss_time2file
;
;   Purpose: any ssw time -> PFSS save file (MDI extrap); NFS or URL
;
;   Input Parameters:
;      time0 - time desired, any SSW format (string, index record..)
;      time1 - optional stop time for range
;
;   Output:
;      function returns files or urls matching users input time/range
;
;   Keyword Parameters:
;      URLS - if set, return URLs 
;      BEFORE - if set, match closest BEFORE (default is closest)
;      AFTER - if set, match closest AFTER   (default is closest)
;      COUNT (output) - number of files returned (zero implies error)
;      SSW_CATALOG - if set, use $SSWDB/pfss/catalog 
;      GENERATE - if set (and permission and local), generate SSWDB cat
;      VERSION - version number ; default=2 (.h5)
;      h5,hdf5 - switch (synonums) - equivilent to VERSION=2
;
;   Calling Sequence:
;      mdisav=pfss_time2file(time [,/BEFORE] [,/AFTER] )   
;      mdisavs=pfss_time2file(startT, endT, [,/BEFORE] [,/AFTER] )
;
;   5-December-2003 - S.L.Freeland - ssw/pfss integration helper
;  13-April-2007    - S.L.Freeland - fixed COUNT output calculation
;  10-Sep-2007 - S.L.F. - add SURFFIELD keyword & function
;   7-Feb-2007 - S.L.F. - tweak local nfs parent path per Marc 
;   8-Mar-2010 - S.L.F. - avoid common block hiccups due to 
;                         multiple field-type toggles
;   4-Oct-2012 - S.L.F - version 2 support = HDF5
;   1-Jan-2013 - M.DeRosa - version 2 is now the default as of 1/1/2013, see
;                           http://www.lmsal.com/forecast/surfflux-model-v2

;-

common pfss_time2file_blk, times, links, sswlinks
common pfss_time2file_blk2, lastbfieldcat
common pfss_time2file_blk3, lastftype, lastversion

; s.l.freeland 5-oct-2012 - add version 2 (HDF5) support

hdf5=keyword_set(hdf5) or keyword_set(h5)
case 1 of
   n_elements(version) gt 0: ver=round(version) > 1 < 2
   hdf5: ver=2
   ; else: ver=1 ; default
   else: ver=2 ; default as of 1/1/2013
endcase

surffield=keyword_set(surffield)
if n_elements(lastftype) eq 0 then lastftype=''
if n_elements(lastversion) eq 0 then lastversion=-1

ftype=(['Bfield','surffield'])(surffield)
if lastftype ne ftype or lastversion ne ver then begin ; refresh all global/common variables
   delvarx, times, links, sswlinks, lastbfieldcat
endif
lastftype=ftype ; 
lastversion=ver

run=(['48','76'])(ver eq 2)
ext=(['.sav','.h5'])(ver eq 2)
extsub=(['','/hfd5/'])(ver eq 2)

if n_elements(lastbfieldcat) eq 0 then lastbfieldcat=''  ; init common
debug=keyword_set(debug)
urls=keyword_set(urls)

count=0
retval=''
refresh=keyword_set(refresh)
before=keyword_set(before)
after=keyword_set(after)
loud=keyword_set(loud)

authorize=str2arr('freeland,derosa,hurlburt')
generate=keyword_set(generate) and is_member(get_user(),authorize)

ssw_catalog=keyword_set(ssw_catalog) ; or generate

pfss_path=get_logenv('PFSS_PATH')
pfss_http=get_logenv('PFSS_HTTP')
surffield=keyword_set(surffield)

fpat=([ftype,'kitrun0'+run])(surffield)

date_sub=ftype +'-bydate' + (['','/hdf5'])(ver eq 2)

if pfss_path eq '' then $
   pfss_path='/archive/pfss/kitrun' + run + '/' + date_sub 

if pfss_http eq '' then $
   pfss_http='http://www.lmsal.com/solarsoft' + (['','/archive/ssw'])(ver eq 2)  + '/pfss_links'


pfsscatdir=$
   concat_dir(concat_dir(concat_dir('$SSWDB','packages'),'pfss'),'genxcat')

vstring=(['','_v2'])(ver eq 2) 
bfieldcatname=strlowcase(ftype)+ vstring +'.geny'
;if file_exist(lastbfieldcat) then bfieldcat =lastbfieldcat else $
bfieldcat=concat_dir(pfsscatdir,bfieldcatname)  ; default = $SSWDB version 
bfieldurl='http://www.lmsal.com/solarsoft/pfss_genxcat/'+bfieldcatname ; url alternate
sswmaster=str_replace(bfieldurl,'http://www.lmsal.com/solarsoft',$
                '/archive/ssw')

if ssw_catalog then begin 
   if 1-file_exist(bfieldcat) then begin 
      box_message,'No local catalog in SSWDB, trying remote http access..'
      outdir=get_temp_dir()
      bfieldcat=concat_dir(outdir,bfieldcatname) ;
      ssw_file_delete,bfieldcat
      sock_copy,bfieldurl,out_dir=outdir,use_network=1,/prog     ; WWW -> local via http socket
      if not file_exist(bfieldcat) then begin 
         box_message,'Problem with remote http access; no ssw pfss catalog available...
         return,''
      endif else lastbfieldcat=bfieldcat           ; only do the transfer once
   endif 
   if get_logenv('pfss_checkcat') ne '' then box_message,'Using ssw cat>> '+bfieldcat(0)
   restgenx, file=bfieldcat, times, links, sswlinks    ; use ssw catalog
   csswlinks=csswlinks 
   cpfsslinks=links
endif

refresh=file_exist(pfss_path) and $
   n_elements(links) eq 0 or keyword_set(refresh) or generate

if refresh then begin 
   box_message,'Initializing file list'
   files=findfile(pfss_path)  ; + (['','/hdf5'])(ver eq 2)))
   ss=where(strpos(files,fpat) ne -1 and $
            strpos(files,ext)   ne -1, fcnt)
   if fcnt eq 0 then begin 
      box_message,'Problem with PFSS listing, bailing out..
      stop
      return,''
   endif
   links=files(ss)
   times=strextract(links,fpat+'_',ext) ; CCSDS filename
   sswlinks=$                               ; ssw-rational (OS-transportable) 
      ;temporary(strcompress,/remove, $
      temporary(str_replace( $
      temporary(str_replace( $
      temporary(str_replace(links,'-',' ')),':',' ')),'T','_'))	
   sswlinks=strcompress(temporary(sswlinks),/remove)
   times=anytim(temporary(times),/TAI)             ; ->TAI
   if generate then begin
;  box_message,'Updating pfss catalog'
      savegenx, times, links, sswlinks, file=bfieldcat  , /overwrite
if file_exist(sswmaster) then $
   savegenx,times, links, sswlinks, file=sswmaster,/overwrite
      box_message,'Making ssw links...
      linkdir='/archive/ssw/pfss_links' + $
         (['','_sfield'])(surffield) + vstring
      sswfull=concat_dir(linkdir,sswlinks)
      if strpos(pfss_path(0),date_sub) eq -1 then $
         pfssfull=concat_dir(concat_dir(pfss_path,date_sub),links) else $
         pfssfull=concat_dir(pfss_path,links)
      for i=0,n_elements(links)-1 do begin 
        if not file_exist(sswfull(i)) then begin 
           spawn,['rm','-f',sswfull(i)],/noshell 
           spawn,['ln','-s',pfssfull(i),sswfull(i)],/noshell ; shouldn't need but  
        endif
      endfor
   endif
endif

case n_params() of 
   1: begin
         t0=anytim(time0,/TAI)
         t1=t0         
   endcase
   2: begin
         t0=anytim(time0,/TAI)
         t1=anytim(time1,/TAI)
   endcase
   else: begin
      t0=times(0)
      t1=last_nelem(times)
   endcase
endcase

if n_params() eq 1 then begin 
   dtimes=times-t0
   case 1 of 
      keyword_set(before): begin 
        box_message,'Closest Bfield BEFORE input time'
        befbool=times le t0
        ss=max(where(dtimes*befbool))
      endcase
      keyword_set(after): begin
        box_message,'Closest Bfield AFTER input time'
        aftbool=times ge t0
        ss=min(where(dtimes*aftbool))
      endcase
      else: begin 
         abdt=abs(dtimes)
         ss=(where(abdt eq min(abdt)))(0)
      endcase
   endcase 
endif else begin 
   ss=where(times ge t0 and times le t1,count)
endelse

if ss(0) ne -1 then begin 
   case 1 of
      urls: retval=pfss_http + (['','_sfield'])(ftype eq 'surffield') + vstring + '/' + sswlinks(ss) 
      else: retval=concat_dir(pfss_path,links(ss))
   endcase
   count=n_elements(retval)
endif else retval=''
return, retval
end
  


