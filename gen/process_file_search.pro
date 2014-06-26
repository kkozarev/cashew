pro process_file_search, event, All=all
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

   topFiles = file_search(path+folder,'*')

   if folder eq 'kinematics' then begin

      print, "================================================================"
      
      print, "In the kinematics subfolder:"

      if topFiles[0] ne '' then begin
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

      if topFiles[0] ne '' then begin
         files=file_search(path+folder+'/*.png')
         numFiles=n_elements(files)

         numStr = string(numFiles)
         print, strcompress(numStr)+" files have been located"

         print, "-------------------------------------------------------------"
         saveFiles=file_search(path+folder+'/*.sav')

         if saveFiles[0] ne '' then begin
            print, "The following save files have been located"
            print, saveFiles
         endif else begin
            print, "No save files found"
         endelse            
         
         files193=file_search(path+folder+'/*_193*')
         
         print, "-------------------------------------------------------------"
         
         if files193[0] ne '' then begin
            
            overviewFiles193=file_search(path+folder+'/*_193_overview*')
            radialFiles193=file_search(path+folder+'/*_193_radial*')
            tangentialFiles193=file_search(path+folder+'/*_193_tangential*')

            print, "The following 193A files have been located:"

            if overviewFiles193[0] ne '' then begin
               print, strcompress(n_elements(overviewFiles193))," overview .png files"
            endif else begin
               print, "No overview files found"
            endelse

            if radialFiles193[0] ne '' then begin
               print, strcompress(n_elements(radialFiles193))," radial .png files"
            endif else begin
               print, "No radial files found"
            endelse

            if overviewFiles193[0] ne '' then begin
               print, strcompress(n_elements(tangentialFiles193))," tangential .png files"
            endif else begin
               print, "No tangential files found"
            endelse 

         endif else begin
            print, "No 193A files found"
         endelse

         print, "-------------------------------------------------------------"

         files211=file_search(path+folder+'/*_211*')

         if files211[0] ne '' then begin

            overviewFiles211=file_search(path+folder+'/*_211_overview*')
            radialFiles211=file_search(path+folder+'/*_211_radial*')
            tangentialFiles211=file_search(path+folder+'/*_211_tangential*')
            
            print, "The following 211A files have been located:"
            
            if overviewFiles211[0] ne '' then begin
               print, strcompress(n_elements(overviewFiles211))," overview .png files"
            endif else begin
               print, "No overview files found"
            endelse

            if radialFiles211[0] ne '' then begin
               print, strcompress(n_elements(radialFiles211))," radial .png files"
            endif else begin
               print, "No radial files found"
            endelse

            if overviewFiles211[0] ne '' then begin
               print, strcompress(n_elements(tangentialFiles211))," tangential .png files"
            endif else begin
               print, "No tangential files found"
            endelse 

         endif else begin
            print, "No 211A files found"
         endelse

         print, "-------------------------------------------------------------"
         
         for i=0, n_elements(subfolders.annulusplot)-1 do begin
            subfolderFiles193=file_search(path+folder+'/'+subfolders.annulusplot[i]+'/193/*.png')
            subfolderFiles211=file_search(path+folder+'/'+subfolders.annulusplot[i]+'/211/*.png')
            if subfolderFiles193[0] ne '' then begin
               print, "In the " + subfolders.annulusplot[i] + " directory for 193A:"
               print, strcompress(n_elements(subfolderFiles193))," .png files exist"
            endif else begin
               print, "In the " + subfolders.annulusplot[i] + " directory for 193A no files found"
            endelse
            
            if subfolderFiles211[0] ne '' then begin
               print, "In the " + subfolders.annulusplot[i] + " directory for 211A:"
               print, strcompress(n_elements(subfolderFiles211))," .png files exist"
            endif else begin
               print, "In the " + subfolders.annulusplot[i] + " directory for 211A no files found"
            endelse
         endfor

            
         endif else begin
         print, "No annulusplot files found"

      endelse 

      print, "================================================================"

   endif

   if folder eq 'radio' then begin
      print, "================================================================"

      print, "In the radio subfolder:"

      if topFiles[0] ne '' then begin
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

      if topFiles[0] ne '' then begin
         
         files=file_search(path+folder+'/*.png')
         numFiles=n_elements(files)

         numStr = string(numFiles)
         print, strcompress(numStr)+" .png files have been located"

         print, "-------------------------------------------------------------"

         saveFiles=file_search(path+folder+'/*.sav')
         
         if saveFiles[0] ne '' then begin
            print, "The following save files have been located"
            print, saveFiles
         endif else begin
            print, "No save files found"
         endelse

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

   if folder eq 'swap' then begin
      print, "================================================================"

      print, "In the swap subfolder:"
      swapFiles=file_search(path+folder, '*') 

      if swapFiles ne '' then begin
         print, strcompress(n_elements(swapFiles)), " swap files found"
      endif else begin
         print, "No files in swap found"
      endelse
      print, "================================================================"
   endif

   if folder eq 'ionization' then begin
      print, "================================================================"

      print, "In the ionization subfolder:"
      ionfiles=file_search(path+folder, '*') 
      
      if ionFiles ne '' then begin
         print, strcompress(n_elements(ionFiles)), " ionization files found"
      endif else begin
         print, "No files in ionization found"
      endelse
      print, "================================================================"
   endif

   if folder eq 'particles' then begin
      print, "================================================================"

      print, "In the particles subfolder:"
      partFiles=file_search(path+folder, '*') 
      
      if partFiles ne '' then begin
         print, strcompress(n_elements(partFiles)), " particles files found"
      endif else begin
         print, "No files in particles found"
      endelse
      print, "================================================================"
   endif


   if folder eq 'png' then begin
      print, "================================================================"
      
      print, "In the png subfolder:"
            
      for i=0, n_elements(subfolders.png)-1 do begin
         subfolderFiles193=file_search(path+folder+'/'+subfolders.png[i]+'/193/*.png')
         subfolderFiles211=file_search(path+folder+'/'+subfolders.png[i]+'/211/*.png')
         if subfolderFiles193[0] ne '' then begin
            print, "In the " + subfolders.png[i] + " directory for 193A:"
            print, strcompress(n_elements(subfolderFiles193))," .png files exist"
         endif else begin
            print, "In the " + subfolders.png[i] + " directory for 193A, no files found"
         endelse
         if subfolderFiles211[0] ne '' then begin
            print, "In the " + subfolders.png[i] + " directory for 211A:"
            print, strcompress(n_elements(subfolderFiles211))," .png files exist"
         endif else begin
            print, "In the " + subfolders.png[i] + " directory for 211A, no files found"
         endelse
      endfor
      print, "================================================================"
   endif

   if folder eq 'movies' then begin
      movies=['abase','araw','arun','aschdem','base','raw','run']
      
      movieFiles=file_search(path+folder+'/*.mp4')
      if movieFiles[0] ne '' then begin
         print, "The following movie files have been generated:"
         print, movieFiles
      endif else begin
         print, "No movie files found"
      endelse
      print, "================================================================"
   endif
   
   if folder eq 'dem' then begin
      
      for i=0, n_elements(subfolders.dem)-1 do begin
         subfolderFiles=file_search(path+folder+'/'+subfolders.dem[i], '*')

         if subfolderFiles[0] ne '' then begin
            print, "In the " + subfolders.dem[i] + " directory for DEM:"
            print, strcompress(n_elements(subfolderFiles))," .png files exist"
         endif else begin
            print, "In the " + subfolders.dem[i] + " directory, no files found"
         endelse
      endfor
      print, "================================================================"
   endif

   if folder eq 'yaftawave' then begin
      print, "================================================================"

      print, "In the yaftawave subfolder:"
      yaftaFiles=file_search(path+folder, '*') 
      
      if yaftaFiles ne '' then begin
         print, strcompress(n_elements(yaftaFiles)), " yaftawave files found"
      endif else begin
         print, "No files in yaftawave found"
      endelse
      print, "================================================================"
   endif

   if folder eq 'euvi' then begin
      print, "================================================================"

      print, "In the euvi subfolder:"
      euviFiles=file_search(path+folder, '*') 
      
      if euviFiles ne '' then begin
         print, strcompress(n_elements(euviFiles)), " euvi files found"
      endif else begin
         print, "No files in euvi found"
      endelse
      print, "================================================================"
   endif

endfor

end 
         
