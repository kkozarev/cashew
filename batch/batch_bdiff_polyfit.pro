pro batch_bdiff_polyfit
;get the velocities for the 0612 and 0613 events
;UPDATE 02/07/2011 Kamen Kozarev - this version calls
;                                  aia_bdiff_polyfit, which repeats
;                                  the calculations but also does
;                                  third-order fitting to the measurements.
path='/home/kkozarev/Desktop/AIA/limbCMEs/'


;First the 0612 event
restore,path+'06122010/results/proflocs_0612.sav'

;171 A
;The wave is barely visible in base difference images in 171 A. I was
;not able to measure positions and therefore speeds for this channel.


;193 A
;restore,path+'06122010/results/193/regionData_0612_193.sav'
;restore,path+'06122010/results/193/bdiffim/vel_pos_extract_params_2010-06-12_193.sav'
;aia_bdiff_polyfit,indices,[8,34],proflocs,peakpos,10,$
;                           outpath=path+'06122010/results/193/bdiffim/'

;211 A
restore,path+'06122010/results/211/regionData_0612_211.sav'
restore,path+'06122010/results/211/bdiffim/vel_pos_extract_params_2010-06-12_211.sav'
aia_bdiff_polyfit,indices,[8,35],proflocs,peakpos,10,$
                           outpath=path+'06122010/results/211/bdiffim/'


;335 A
;The wave is barely visible in base difference images in 335 A. I was
;not able to deduct positions and therefore speeds for this channel.


;Then, do the 0613 event
restore,path+'06132010/results/proflocs_0613.sav'


;171 A
;The wave is not visible in base difference images in 171 A. I was
;not able to deduct positions and therefore speeds for this channel.

;TEST
;restore,path+'06132010/results/193/regionData_0613_193.sav'
;aia_bdiff_image_velextract,totaldata,indices,[52,55],proflocs,3
;ENDTEST


;193 A
restore,path+'06132010/results/193/bdiffim/vel_pos_extract_params_2010-06-13_193.sav'
restore,path+'06132010/results/193/regionData_0613_193.sav'
aia_bdiff_polyfit,indices,[37,54],proflocs,peakpos,10,$
                           outpath=path+'06132010/results/193/bdiffim/'

;stop

;211 A
restore,path+'06132010/results/211/bdiffim/vel_pos_extract_params_2010-06-13_211.sav'
restore,path+'06132010/results/211/regionData_0613_211.sav'
aia_bdiff_polyfit,indices,[37,54],proflocs,peakpos,10,$
                           outpath=path+'06132010/results/211/bdiffim/'
;stop

;335 A
;The wave is barely visible in base difference images in 335 A. I was
;not able to deduct positions and therefore speeds for this channel.

end
