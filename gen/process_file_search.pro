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

   nFiles = n_elements(file_search('*'))

   if folder eq 'kinematics' then begin

      print, "================================================================"
      
      print, "In the kinematics subfolder:"

      if nFiles  ne 0 then begin
         files=file_search(path+folder+'/*.png')
         numFiles=n_elements(files)

         numStr = string(numFiles)
         print, strcompress(numStr)+" .png files have been located"
         
         files193=file_search(path+folder+'/*_193*')
         overviewFiles193=file_search(path+folder+'/*_193_overview*')
         radialFiles193=file_search(path+folder+'/*_193_radial*')
         tangentialFiles193=file_search(path+folder+'/*_193_tangential*')

         print, "-------------------------------------------------------------"
         print, "The following 193A files have been located:"
         print, strcompress(n_elements(overviewFiles193))," overview .png files"
         print, strcompress(n_elements(radialFiles193))," radial .png files"
         print, strcompress(n_elements(tangentialFiles193))," tangential .png files"
         print, "-------------------------------------------------------------"

         files211=file_search(path+folder+'/*_211*')
         overviewFiles211=file_search(path+folder+'/*_211_overview*')
         radialFiles211=file_search(path+folder+'/*_211_radial*')
         tangentialFiles211=file_search(path+folder+'/*_211_tangential*')

         print, "The following 211A files have been located:"
         print, strcompress(n_elements(overviewFiles211))," overview .png files"
         print, strcompress(n_elements(radialFiles211))," radial .png files"
         print, strcompress(n_elements(tangentialFiles211))," tangential .png files"
         
      endif else begin
         print, "No kinematics files found"

      endelse 

      print, "================================================================"

   endif

   if folder eq 'annulusplot' then begin
      print, "================================================================"

      print, "In the annulusplot subfolder:"

      if nFiles  ne 0 then begin
         files=file_search(path+folder+'/*.png')
         numFiles=n_elements(files)

         numStr = string(numFiles)
         print, strcompress(numStr)+" files have been located"

         print, "-------------------------------------------------------------"
         print, "The following save files have been located"
         
         saveFiles=file_search(path+folder+'/*.sav')
         print, saveFiles
         
         files193=file_search(path+folder+'/*_193*')
         overviewFiles193=file_search(path+folder+'/*_193_overview*')
         radialFiles193=file_search(path+folder+'/*_193_radial*')
         tangentialFiles193=file_search(path+folder+'/*_193_tangential*')

         print, "-------------------------------------------------------------"
         print, "The following 193A files have been located:"
         print, strcompress(n_elements(overviewFiles193))," overview .png files"
         print, strcompress(n_elements(radialFiles193))," radial .png files"
         print, strcompress(n_elements(tangentialFiles193))," tangential .png files"
         print, "-------------------------------------------------------------"

         files211=file_search(path+folder+'/*_211*')
         overviewFiles211=file_search(path+folder+'/*_211_overview*')
         radialFiles211=file_search(path+folder+'/*_211_radial*')
         tangentialFiles211=file_search(path+folder+'/*_211_tangential*')

         print, "The following 211A files have been located:"
         print, strcompress(n_elements(overviewFiles211))," overview .png files"
         print, strcompress(n_elements(radialFiles211))," radial .png files"
         print, strcompress(n_elements(tangentialFiles211))," tangential .png files"

         print, "-------------------------------------------------------------"
         
         for i=0, n_elements(subfolders.annulusplot)-1 do begin
            subfolderFiles193=n_elements(file_search(path+folder+'/'+subfolders.annulusplot[i]+'/193/*.png'))
            subfolderFiles211=n_elements(file_search(path+folder+'/'+subfolders.annulusplot[i]+'/211/*.png'))
            if subfolderFiles193 ne 0 then begin
               print, "In the " + subfolders.annulusplot[i] + " directory for 193A:"
               print, strcompress(subfolderFiles193)," .png files exist"
            endif
            
            if subfolderFiles211 ne 0 then begin
               print, "In the " + subfolders.annulusplot[i] + " directory for 211A:"
               print, strcompress(subfolderFiles211)," .png files exist"
            endif
         endfor

            
         endif else begin
         print, "No annulusplot files found"

      endelse 

      print, "================================================================"

   endif

   if folder eq 'radio' then begin
      print, "================================================================"

      print, "In the radio subfolder:"

      if nFiles  ne 0 then begin
         for i=0, n_elements(subfolders.radio)-1 do begin
            subfolderFilesRadio=n_elements(file_search(path+folder+'/'+subfolders.radio[i], '*'))
            test=file_search(path+folder+'/'+subfolders.radio[i],'*')
            if subfolderFilesRadio ne 0 then begin
               print, "In the " + subfolders.radio[i] + " directory"
               print, strcompress(subfolderFilesRadio)," files exist"
            endif
         endfor
         
      endif else begin
         print, "No annulusplot files found"
         
      endelse 
      
      print, "================================================================"
      
   endif
   
   
endfor

end 
         
