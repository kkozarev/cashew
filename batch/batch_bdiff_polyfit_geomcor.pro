pro batch_bdiff_polyfit_geomcor
;Load the positions of the 0612 and 0613 waves and correct for
;geometric effects
path='/home/kkozarev/Desktop/AIA/limbCMEs/'


;First the 0612 event
restore,path+'06122010/results/proflocs_0612.sav'
arlon=43 ; According to NGDC flare list

;171 A
;The wave is barely visible in base difference images in 171 A. I was
;not able to measure positions and therefore speeds for this channel.


;193 A
restore,path+'06122010/results/193/regionData_0612_193.sav'
restore,path+'06122010/results/193/bdiffim/vel_pos_extract_params_2010-06-12_193.sav'
aia_bdiff_polyfit_geomcor,indices,[8,34],arlon,proflocs,peakpos,nms,$
                           outpath=path+'06122010/results/193/bdiffim/gcor/'

;stop

;211 A
restore,path+'06122010/results/211/regionData_0612_211.sav'
restore,path+'06122010/results/211/bdiffim/vel_pos_extract_params_2010-06-12_211.sav'
aia_bdiff_polyfit_geomcor,indices,[8,34],arlon,proflocs,peakpos,nms,$
                           outpath=path+'06122010/results/211/bdiffim/gcor/'



;335 A
;The wave is barely visible in base difference images in 335 A. I was
;not able to deduct positions and therefore speeds for this channel.



;Then, do the 0613 event
restore,path+'06132010/results/proflocs_0613.sav'
arlon=84 ;according to NGDC flare list

;171 A
;The wave is not visible in base difference images in 171 A. I was
;not able to deduct positions and therefore speeds for this channel.


;193 A
restore,path+'06132010/results/193/regionData_0613_193.sav'
restore,path+'06132010/results/193/bdiffim/vel_pos_extract_params_2010-06-13_193.sav'
aia_bdiff_polyfit_geomcor,indices,[37,54],arlon,proflocs,peakpos,nms,$
                           outpath=path+'06132010/results/193/bdiffim/gcor/'


;211 A
restore,path+'06132010/results/211/regionData_0613_211.sav'
restore,path+'06132010/results/211/bdiffim/vel_pos_extract_params_2010-06-13_211.sav'
aia_bdiff_polyfit_geomcor,indices,[37,54],arlon,proflocs,peakpos,nms,$
                           outpath=path+'06132010/results/211/bdiffim/gcor/'

;335 A
;The wave is barely visible in base difference images in 335 A. I was
;not able to deduct positions and therefore speeds for this channel.

end
