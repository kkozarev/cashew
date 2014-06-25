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
         if files193[0] ne '' then begin
            print, "The following 193A files have been located:"
            print, strcompress(n_elements(overviewFiles193))," overview .png files"
            print, strcompress(n_elements(radialFiles193))," radial .png files"
            print, strcompress(n_elements(tangentialFiles193))," tangential .png files"
         endif else begin
            print, "No 193A files found."
         endelse 
         print, "-------------------------------------------------------------"

         files211=file_search(path+folder+'/*_211*')
         overviewFiles211=file_search(path+folder+'/*_211_overview*')
         radialFiles211=file_search(path+folder+'/*_211_radial*')
         tangentialFiles211=file_search(path+folder+'/*_211_tangential*')

         if files211[0] ne '' then begin
            print, "The following 211A files have been located:"
            print, strcompress(n_elements(overviewFiles211))," overview .png files"
            print, strcompress(n_elements(radialFiles211))," radial .png files"
            print, strcompress(n_elements(tangentialFiles211))," tangential .png files"
         endif else begin
            print, "No 211A files found."
         endelse 
         
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
            subfolderFilesRadio=file_search(path+folder+'/'+subfolders.radio[i], '*')
            if subfolderFilesRadio ne '' then begin
               print, "In the " + subfolders.radio[i] + " directory"
               print, strcompress(n_elements(subfolderFilesRadio))," files exist"
            endif else begin
               print, "No files found in "+subfolders.radio[i]
            endelse
         endfor
         
      endif else begin
         print, "No radio files found"
         
      endelse 
      
      print, "================================================================"
      
   endif
   
   if folder eq 'pfss' then begin
      print, "================================================================"

      print, "In the PFSS subfolder:"

      if nFiles ne 0 then begin
         
         files=file_search(path+folder+'/*.png')
         numFiles=n_elements(files)

         numStr = string(numFiles)
         print, strcompress(numStr)+" .png files have been located"

         print, "-------------------------------------------------------------"
         print, "The following save files have been located"
         
         saveFiles=file_search(path+folder+'/*.sav')
         print, saveFiles

         print, "-------------------------------------------------------------"
         shockFiles=file_search(path+folder+'/*_pfss_shock_2*')
         
         if shockFiles[0] ne '' then begin
            print, strcompress(n_elements(shockFiles))," PFSS shock files exist"
         endif else begin
            print, "No PFSS shock files exist"
         endelse 

         angularShockFiles=file_search(path+folder+'/*_pfss_shock_angular*')
         
         if angularShockFiles[0] ne '' then begin
            print, strcompress(n_elements(angularShockFiles))," angular influence PFSS shock files exist"
         endif else begin
            print, "No angular influence PFSS shock files exist"
         endelse 

         topShockFiles=file_search(path+folder+'/*_topview_*')
         
         if topShockFiles[0] ne '' then begin
            print, strcompress(n_elements(topShockFiles))," topview angular influence PFSS shock files exist"
         endif else begin
            print, "No topview angular influence PFSS shock files exist"
         endelse

         thetaFiles=file_search(path+folder+'/thetabn_2*')
         thetaOplotFiles=file_search(path+folder+'/*_oplot.png')
         
         if thetaFiles[0] ne '' then begin
            print, strcompress(n_elements(shockFiles))," thetabn files exist"

         endif else begin
            print, "No thetabn files exist"
         endelse 

         if thetaOplotFiles[0] ne '' then begin
            print, strcompress(n_elements(thetaOplotFiles)), " oplot thetabn files exist"
         endif else begin
             print, "No oplot thetabn files exist"
          endelse

         thetaStats=file_search(path+folder+'/thetabn_stats*')
         if thetaStats[0] ne '' then begin
            print, strcompress(n_elements(thetaStats)), " thetabn stats files exist"
         endif else begin
            print, "No thetabn stats files found"
         endelse

      endif else begin
         print, "No PFSS files found"
         
      endelse 
      
      print, "================================================================"
      
   endif

   
endfor

end 
         
