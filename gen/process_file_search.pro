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

   if keyword_set(all) then print, folder

   if not dir_exist(path+folder) then begin
      print, "Processed data folder: "+folder+"does not exist"
   endif

   topFiles = file_search(path+folder,'*')

   if folder eq 'kinematics' then begin

      if keyword_set(all) then print, "================================================================"
      
      if keyword_set(all) then print, "In the kinematics subfolder:"

      kFiles193 = 0
      kFiles211 = 0
      if topFiles[0] ne '' then begin
         files=file_search(path+folder+'/*.png')
         numFiles=n_elements(files)

         numStr = string(numFiles)
         if keyword_set(all) then print, strcompress(numStr)+" .png files have been located"
         
         files193=file_search(path+folder+'/*_193*')
         overviewFiles193=file_search(path+folder+'/*_193_overview*')
         radialFiles193=file_search(path+folder+'/*_193_radial*')
         tangentialFiles193=file_search(path+folder+'/*_193_tangential*')

         if keyword_set(all) then print, "-------------------------------------------------------------"
         if files193[0] ne '' then begin
            if keyword_set(all) then print, "The following 193A files have been located:"
            if keyword_set(all) then print, strcompress(n_elements(overviewFiles193))," overview .png files"
            if keyword_set(all) then print, strcompress(n_elements(radialFiles193))," radial .png files"
            if keyword_set(all) then print, strcompress(n_elements(tangentialFiles193))," tangential .png files"
            kFiles193 = 1 
         endif else begin
            if keyword_set(all) then print, "No 193A files found."
         endelse 
         if keyword_set(all) then print, "-------------------------------------------------------------"

         files211=file_search(path+folder+'/*_211*')
         overviewFiles211=file_search(path+folder+'/*_211_overview*')
         radialFiles211=file_search(path+folder+'/*_211_radial*')
         tangentialFiles211=file_search(path+folder+'/*_211_tangential*')

         if files211[0] ne '' then begin
            if keyword_set(all) then print, "The following 211A files have been located:"
            if keyword_set(all) then print, strcompress(n_elements(overviewFiles211))," overview .png files"
            if keyword_set(all) then print, strcompress(n_elements(radialFiles211))," radial .png files"
            if keyword_set(all) then print, strcompress(n_elements(tangentialFiles211))," tangential .png files"
            kFiles211 = 1
         endif else begin
            if keyword_set(all) then print, "No 211A files found."
         endelse 
         
      endif else begin
         if keyword_set(all) then print, "No kinematics files found"

      endelse 

      if keyword_set(all) then print, "================================================================"

   endif

   if folder eq 'annulusplot' then begin
      if keyword_set(all) then print, "================================================================"

      if keyword_set(all) then print, "In the annulusplot subfolder:"
      
      annulSave = 0
      annul193 = 0
      annul211 = 0

      if topFiles[0] ne '' then begin
         files=file_search(path+folder+'/*.png')
         numFiles=n_elements(files)

         numStr = string(numFiles)
         if keyword_set(all) then print, strcompress(numStr)+" files have been located"

         if keyword_set(all) then print, "-------------------------------------------------------------"
         saveFiles=file_search(path+folder+'/*.sav')

         if saveFiles[0] ne '' then begin
            if keyword_set(all) then print, "The following save files have been located"
            if keyword_set(all) then print, saveFiles
            annulSave = 1
         endif else begin
            if keyword_set(all) then print, "No save files found"
            annulSave = 0
         endelse            
         
         files193=file_search(path+folder+'/*_193*')
         
         if keyword_set(all) then print, "-------------------------------------------------------------"
         
         if files193[0] ne '' then begin

            annul193 = 1
            
            overviewFiles193=file_search(path+folder+'/*_193_overview*')
            radialFiles193=file_search(path+folder+'/*_193_radial*')
            tangentialFiles193=file_search(path+folder+'/*_193_tangential*')

            if keyword_set(all) then print, "The following 193A files have been located:"

            if overviewFiles193[0] ne '' then begin
               if keyword_set(all) then print, strcompress(n_elements(overviewFiles193))," overview .png files"
               annulOverview193 = 1
            endif else begin
               annulOverview193 = 0
               if keyword_set(all) then print, "No overview files found"
            endelse

            if radialFiles193[0] ne '' then begin
               if keyword_set(all) then print, strcompress(n_elements(radialFiles193))," radial .png files"
               annulRadial193 = 1
            endif else begin
               annulRadial193 = 0
               if keyword_set(all) then print, "No radial files found"
            endelse

            if tangentialFiles193[0] ne '' then begin
               if keyword_set(all) then print, strcompress(n_elements(tangentialFiles193))," tangential .png files"
               annulTang193 = 1
            endif else begin
               annulTang193 = 0
               if keyword_set(all) then print, "No tangential files found"
            endelse 

         endif else begin
            if keyword_set(all) then print, "No 193A files found"

         endelse

         if keyword_set(all) then print, "-------------------------------------------------------------"

         files211=file_search(path+folder+'/*_211*')

         if files211[0] ne '' then begin

            annul211 = 1

            overviewFiles211=file_search(path+folder+'/*_211_overview*')
            radialFiles211=file_search(path+folder+'/*_211_radial*')
            tangentialFiles211=file_search(path+folder+'/*_211_tangential*')
            
            if keyword_set(all) then print, "The following 211A files have been located:"
            
            if overviewFiles211[0] ne '' then begin
               if keyword_set(all) then print, strcompress(n_elements(overviewFiles211))," overview .png files"
               annulOverview211 = 1
            endif else begin
               annulOverview211 = 0
               if keyword_set(all) then print, "No overview files found"
            endelse

            if radialFiles211[0] ne '' then begin
               if keyword_set(all) then print, strcompress(n_elements(radialFiles211))," radial .png files"
               annulRadial211 = 1
            endif else begin
               annulRadial211 = 0
               if keyword_set(all) then print, "No radial files found"
            endelse

            if tangentialFiles211[0] ne '' then begin
               if keyword_set(all) then print, strcompress(n_elements(tangentialFiles211))," tangential .png files"
               annulTang211 = 1
            endif else begin
               annulTang211 = 0
               if keyword_set(all) then print, "No tangential files found"
            endelse 

         endif else begin
            if keyword_set(all) then print, "No 211A files found"
         endelse

         if keyword_set(all) then print, "-------------------------------------------------------------"
         
         for i=0, n_elements(subfolders.annulusplot)-1 do begin
            subfolderFiles193=file_search(path+folder+'/'+subfolders.annulusplot[i]+'/193/*.png')
            subfolderFiles211=file_search(path+folder+'/'+subfolders.annulusplot[i]+'/211/*.png')
            if subfolderFiles193[0] ne '' then begin
               if keyword_set(all) then print, "In the " + subfolders.annulusplot[i] + " directory for 193A:"
               if keyword_set(all) then print, strcompress(n_elements(subfolderFiles193))," .png files exist"
            endif else begin
               if keyword_set(all) then print, "In the " + subfolders.annulusplot[i] + " directory for 193A no files found"
            endelse
            
            if subfolderFiles211[0] ne '' then begin
               if keyword_set(all) then print, "In the " + subfolders.annulusplot[i] + " directory for 211A:"
               if keyword_set(all) then print, strcompress(n_elements(subfolderFiles211))," .png files exist"
            endif else begin
               if keyword_set(all) then print, "In the " + subfolders.annulusplot[i] + " directory for 211A no files found"
            endelse
         endfor

            
         endif else begin
         if keyword_set(all) then print, "No annulusplot files found"

      endelse 

      if keyword_set(all) then print, "================================================================"

   endif

   if folder eq 'radio' then begin
      if keyword_set(all) then print, "================================================================"

      if keyword_set(all) then print, "In the radio subfolder:"
      
      radioFiles = 0

      if topFiles[0] ne '' then begin
         radioFiles = 1
         for i=0, n_elements(subfolders.radio)-1 do begin
            subfolderFilesRadio=file_search(path+folder+'/'+subfolders.radio[i], '*')
            if subfolderFilesRadio[0] ne '' then begin
               if keyword_set(all) then print, "In the " + subfolders.radio[i] + " directory"
               if keyword_set(all) then print, strcompress(n_elements(subfolderFilesRadio))," files exist"

               if subfolders.radio[i] eq 'NRH' then radioNRH = 1
               if subfolders.radio[i] eq 'IPS' then radioIPS = 1
               if subfolders.radio[i] eq 'Callisto' then radioCallisto = 1
            endif else begin
               if keyword_set(all) then print, "No files found in "+subfolders.radio[i]

               if subfolders.radio[i] eq 'NRH' then radioNRH = 0
               if subfolders.radio[i] eq 'IPS' then radioIPS = 0
               if subfolders.radio[i] eq 'Callisto' then radioCallisto = 0
            endelse
         endfor
         
      endif else begin
         radioFiles = 0
         if keyword_set(all) then print, "No radio files found"
         
      endelse 
      
      if keyword_set(all) then print, "================================================================"
      
   endif
   
   if folder eq 'pfss' then begin
      if keyword_set(all) then print, "================================================================"

      if keyword_set(all) then print, "In the PFSS subfolder:"

      pfssSave = 0
      pfssFiles = 0

      if topFiles[0] ne '' then begin
         
         pfssFiles=1
         files=file_search(path+folder+'/*.png')
         numFiles=n_elements(files)

         numStr = string(numFiles)
         if keyword_set(all) then print, strcompress(numStr)+" .png files have been located"

         if keyword_set(all) then print, "-------------------------------------------------------------"

         saveFiles=file_search(path+folder+'/*.sav')
         
         if saveFiles[0] ne '' then begin
            pfssSave = 1
            if keyword_set(all) then print, "The following save files have been located"
            if keyword_set(all) then print, saveFiles
         endif else begin
            if keyword_set(all) then print, "No save files found"
         endelse

         if keyword_set(all) then print, "-------------------------------------------------------------"
         shockFiles=file_search(path+folder+'/*_pfss_shock_2*')
         
         if shockFiles[0] ne '' then begin
            pfssShock = 1
            if keyword_set(all) then print, strcompress(n_elements(shockFiles))," PFSS shock files exist"
         endif else begin
            pfssShock = 0
            if keyword_set(all) then print, "No PFSS shock files exist"
         endelse 

         angularShockFiles=file_search(path+folder+'/*_pfss_shock_angular*')
         
         if angularShockFiles[0] ne '' then begin
            pfssAngularShock = 1
            if keyword_set(all) then print, strcompress(n_elements(angularShockFiles))," angular influence PFSS shock files exist"
         endif else begin
            pfssAngularShock = 0
            if keyword_set(all) then print, "No angular influence PFSS shock files exist"
         endelse 

         topShockFiles=file_search(path+folder+'/*_topview_*')
         
         if topShockFiles[0] ne '' then begin
            pfssTopShock = 1
            if keyword_set(all) then print, strcompress(n_elements(topShockFiles))," topview angular influence PFSS shock files exist"
         endif else begin
            pfssTopShock = 0
            if keyword_set(all) then print, "No topview angular influence PFSS shock files exist"
         endelse

         thetaFiles=file_search(path+folder+'/thetabn_2*')
         thetaOplotFiles=file_search(path+folder+'/*_oplot.png')
         
         if thetaFiles[0] ne '' then begin
            if keyword_set(all) then print, strcompress(n_elements(shockFiles))," thetabn files exist"
            thetaBn = 1
         endif else begin
            thetaBn = 0
            if keyword_set(all) then print, "No thetabn files exist"
         endelse 

         if thetaOplotFiles[0] ne '' then begin
            thetaOplot = 1
            if keyword_set(all) then print, strcompress(n_elements(thetaOplotFiles)), " oplot thetabn files exist"
         endif else begin
            thetaOplot = 0
            if keyword_set(all) then print, "No oplot thetabn files exist"
         endelse

         thetaStats=file_search(path+folder+'/thetabn_stats*')
         if thetaStats[0] ne '' then begin
            thetaStats = 1
            if keyword_set(all) then print, strcompress(n_elements(thetaStats)), " thetabn stats files exist"
         endif else begin
            thetaStats = 0
            if keyword_set(all) then print, "No thetabn stats files found"
         endelse

      endif else begin
         if keyword_set(all) then print, "No PFSS files found"
         
      endelse 
      
      if keyword_set(all) then print, "================================================================"
      
   endif

   if folder eq 'swap' then begin
      if keyword_set(all) then print, "================================================================"

      if keyword_set(all) then print, "In the swap subfolder:"
      swapFiles=file_search(path+folder, '*') 

      if swapFiles ne '' then begin
         if keyword_set(all) then print, strcompress(n_elements(swapFiles)), " swap files found"
      endif else begin
         if keyword_set(all) then print, "No files in swap found"
      endelse
      if keyword_set(all) then print, "================================================================"
   endif

   if folder eq 'ionization' then begin
      if keyword_set(all) then print, "================================================================"

      if keyword_set(all) then print, "In the ionization subfolder:"
      ionfiles=file_search(path+folder, '*') 
      
      if ionFiles ne '' then begin
         if keyword_set(all) then print, strcompress(n_elements(ionFiles)), " ionization files found"
      endif else begin
         if keyword_set(all) then print, "No files in ionization found"
      endelse
      if keyword_set(all) then print, "================================================================"
   endif

   if folder eq 'particles' then begin
      if keyword_set(all) then print, "================================================================"

      if keyword_set(all) then print, "In the particles subfolder:"
      partFiles=file_search(path+folder, '*') 
      
      if partFiles ne '' then begin
         if keyword_set(all) then print, strcompress(n_elements(partFiles)), " particles files found"
      endif else begin
         if keyword_set(all) then print, "No files in particles found"
      endelse
      if keyword_set(all) then print, "================================================================"
   endif


   if folder eq 'png' then begin
      if keyword_set(all) then print, "================================================================"
      
      if keyword_set(all) then print, "In the png subfolder:"
            
      for i=0, n_elements(subfolders.png)-1 do begin
         subfolderFiles193=file_search(path+folder+'/'+subfolders.png[i]+'/193/*.png')
         subfolderFiles211=file_search(path+folder+'/'+subfolders.png[i]+'/211/*.png')

         if subfolders.png[i] eq 'raw' then pngRaw193 = 0
         if subfolders.png[i] eq 'base' then pngBase193 = 0
         if subfolders.png[i] eq 'run' then pngRun193 = 0

         if subfolderFiles193[0] ne '' then begin
            if keyword_set(all) then print, "In the " + subfolders.png[i] + " directory for 193A:"
            if keyword_set(all) then print, strcompress(n_elements(subfolderFiles193))," .png files exist"

            if subfolders.png[i] eq 'raw' then pngRaw193 = 1
            if subfolders.png[i] eq 'base' then pngBase193 = 1
            if subfolders.png[i] eq 'run' then pngRun193 = 1

         endif else begin
            if keyword_set(all) then print, "In the " + subfolders.png[i] + " directory for 193A, no files found"

         endelse

         if subfolders.png[i] eq 'raw' then pngRaw211 = 0
         if subfolders.png[i] eq 'base' then pngBase211 = 0
         if subfolders.png[i] eq 'run' then pngRun211 = 0

         if subfolderFiles211[0] ne '' then begin
            if keyword_set(all) then print, "In the " + subfolders.png[i] + " directory for 211A:"
            if keyword_set(all) then print, strcompress(n_elements(subfolderFiles211))," .png files exist"

            if subfolders.png[i] eq 'raw' then pngRaw211 = 1
            if subfolders.png[i] eq 'base' then pngBase211 = 1
            if subfolders.png[i] eq 'run' then pngRun211 = 1

         endif else begin
            if keyword_set(all) then print, "In the " + subfolders.png[i] + " directory for 211A, no files found"

         endelse
      endfor
      if keyword_set(all) then print, "================================================================"
   endif

   if folder eq 'movies' then begin
      movies=['abase','araw','arun','aschdem','base','raw','run']
      
      movieFiles=file_search(path+folder+'/*.mp4')
      if movieFiles[0] ne '' then begin
         movieFile = 1
         if keyword_set(all) then print, "The following movie files have been generated:"
         if keyword_set(all) then print, movieFiles
      endif else begin
         movieFile = 0
         if keyword_set(all) then print, "No movie files found"
      endelse
      if keyword_set(all) then print, "================================================================"
   endif
   
   if folder eq 'dem' then begin
      
      for i=0, n_elements(subfolders.dem)-1 do begin
         subfolderFiles=file_search(path+folder+'/'+subfolders.dem[i], '*')
         
         if subfolders.dem[i] eq 'aschwanden' then demAsch = 0
         if subfolders.dem[i] eq 'weber' then demWeber = 0
            
         if subfolderFiles[0] ne '' then begin
            if keyword_set(all) then print, "In the " + subfolders.dem[i] + " directory for DEM:"
            if keyword_set(all) then print, strcompress(n_elements(subfolderFiles))," .png files exist"

            if subfolders.dem[i] eq 'aschwanden' then demAsch = 1
            if subfolders.dem[i] eq 'weber' then demWeber = 1
         endif else begin
            if keyword_set(all) then print, "In the " + subfolders.dem[i] + " directory, no files found"
         endelse
      endfor
      if keyword_set(all) then print, "================================================================"
   endif

   if folder eq 'yaftawave' then begin
      if keyword_set(all) then print, "================================================================"

      if keyword_set(all) then print, "In the yaftawave subfolder:"
      yaftaFiles=file_search(path+folder, '*') 
      
      if yaftaFiles ne '' then begin
         if keyword_set(all) then print, strcompress(n_elements(yaftaFiles)), " yaftawave files found"
      endif else begin
         if keyword_set(all) then print, "No files in yaftawave found"
      endelse
      if keyword_set(all) then print, "================================================================"
   endif

   if folder eq 'euvi' then begin
      if keyword_set(all) then print, "================================================================"

      if keyword_set(all) then print, "In the euvi subfolder:"
      euviFiles=file_search(path+folder, '*') 
      
      if euviFiles ne '' then begin
         if keyword_set(all) then print, strcompress(n_elements(euviFiles)), " euvi files found"
      endif else begin
         if keyword_set(all) then print, "No files in euvi found"
      endelse
      if keyword_set(all) then print, "================================================================"
   endif

endfor

print, "Generating Condensed Data Report"
print, "For a detailed report run the program with the /all keyword"

print, "================================================================"

str = '................................'
str2 = '.....................'

if kFiles193 eq 1 then value='Yes' else value='No'
print, "Kinematics Files 193"+str+value

if kFiles211 eq 1 then value='Yes' else value='No'
print, "Kinematics Files 211"+str+value

print, "-------------------------------------------------------------"

if annulSave eq 1 then value='Yes' else value='No'
print, "Annulus Save Files" +str+value

if annul193 eq 1 then begin
   print, "Annulus 193 Files:"
   if annulOverview193 eq 1 then value='Yes' else value='No'
   print, string(9b)+"Annulus Overview Files"+str2+value

   if annulRadial193 eq 1 then value='Yes' else value='No'
   print, string(9b)+"Annulus Radial Files"+str2+value

   if annulTang193 eq 1 then value='Yes' else value='No'
   print, string(9b)+"Annulus Tangential Files"+str2+value
endif else begin
      print, "Annulus 193 Files"+str+"No"
endelse

if annul211 eq 1 then begin
   print, "Annulus 211 Files:"
   if annulOverview211 eq 1 then value='Yes' else value='No'
   print, string(9b)+"Annulus Overview Files"+str2+value

   if annulRadial211 eq 1 then value='Yes' else value='No'
   print, string(9b)+"Annulus Radial Files"+str2+value

   if annulTang211 eq 1 then value='Yes' else value='No'
   print, string(9b)+"Annulus Tangential Files"+str2+value
endif else begin
      print, "Annulus 211 Files"+str+"No"
endelse

print, "-------------------------------------------------------------"
if radioFiles eq 1 then begin
   print, "Radio Spectra Files:"

   if radioNRH eq 1 then value='Yes' else value='No'
   print, string(9b)+"NRH Radio Files"+str2+value
   
   if radioIPS eq 1 then value='Yes' else value='No'
   print, string(9b)+"IPS Radio Files"+str2+value
   
   if radioCallisto eq 1 then value='Yes' else value='No'
   print, string(9b)+"Callisto Radio Files"+str2+value

endif else begin
   print, "Radio Spectra Files"+str+"No"
endelse


print, "-------------------------------------------------------------"
if pfssSave eq 1 then value='Yes' else value='No'
print, "PFSS Save Files"+str+value

if pfssFiles eq 1 then begin
   print, "PFSS Files:"
   if pfssShock eq 1 then value='Yes' else value='No'
   print, string(9b)+"PFSS Shock Files"+str2+value
   
   if pfssAngularShock eq 1 then value='Yes' else value='No'
   print, string(9b)+"PFSS Angular Influence Shock Files"+str2+value
   
   if pfssTopShock eq 1 then value='Yes' else value='No'
   print, string(9b)+"PFSS Topview Angular Influence Shock Files"+str2+value
   
   if thetaBn eq 1 then value='Yes' else value='No'
   print, string(9b)+"Theta Bn Plots"+str2+value
   
   if thetaOplot eq 1 then value='Yes' else value='No'
   print, string(9b)+"Theta Bn Oplots"+str2+value
   
   if thetaStats eq 1 then value='Yes' else value='No'
   print, string(9b)+"Theta Bn Statistics"+str2+value

endif else begin
   print, "PFSS Files"+str+"No"
endelse
   
print, "-------------------------------------------------------------"
print, "PNG 193 Images"

if pngRaw193 eq 1 then value='Yes' else value='No'
print, string(9b)+"Raw PNG Images"+str2+value

if pngBase193 eq 1 then value='Yes' else value='No'
print, string(9b)+"Base Difference PNG Images"+str2+value

if pngRun193 eq 1 then value='Yes' else value='No'
print, string(9b)+"Running Difference PNG Images"+str2+value

print, "PNG 211 Images"

if pngRaw211 eq 1 then value='Yes' else value='No'
print, string(9b)+"Raw PNG Images"+str2+value

if pngBase211 eq 1 then value='Yes' else value='No'
print, string(9b)+"Base Difference PNG Images"+str2+value

if pngRun211 eq 1 then value='Yes' else value='No'
print, string(9b)+"Running Difference PNG Images"+str2+value

print, "-------------------------------------------------------------"

if movieFile eq 1 then value='Yes' else value='No'
print, "Movie Files"+str+value

print, "-------------------------------------------------------------"
print, "DEM Model"

if demAsch eq 1 then value='Yes' else value='No'
print, string(9b)+"Aschwanden DEM Model"+str2+value

if demWeber eq 1 then value='Yes' else value='No'
print, string(9b)+"Weber DEM Model"+str2+value

end 
         
