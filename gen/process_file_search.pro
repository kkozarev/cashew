pro process_file_search, event
;PURPOSE
;This procedure will search processed data directories for a given
;event and produce output describing which data products exist for
;that event
;
;CATAGORY:
;General
;
;INPUTS:
;   event - an event structure, returned from load_events(info)
;
;KEYWORDS:
;
;
;OUTPUTS:
;   Command line information describing existing data products
;
;DEPENDENCIES:
;
;
;MODIFICATION HISTORY:
;Written by Alex Kendrick, 06/2014

  folders=['radio','annulusplot','kinematics','pfss','swap','ionization','particles','png','movies','dem','yaftawave','euvi']
  subfolders={radio:['NRH','IPS','Callisto'],annulusplot:['araw','abase','arun'],png:['raw','base','run'],dem:['aschwanden','weber']}
  
  
  if size(event,/type) ne 8 then return
  
  path=event.savepath
    
  if not dir_exist(path) then begin
     print, "Event folders not found, returning..."
     return
  endif

for f=0, n_elements(folders)-1 do begin
   folder=folders[f]

   print, folder

   if not dir_exist(path+folder) then begin
      print, "Processed data folder: "+folder+"does not exist"
   endif

   if folder eq 'kinematics' then begin
      print, path+folder
      files = file_search(path+folder+'*.png')
      stop
      if file_search(path+folder+'*.png') ne '' then begin
         files=file_search(path+folder+'*.png')
         numFiles=n_elements(files)
         print, "In the kinematics subfolder: ", numFiles, " have been located"
      endif
   endif
endfor

end 
         
