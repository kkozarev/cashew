pro batchgetdiffprofiles
;get radial profiles for the 0612 and 0613 events

path='/home/kkozarev/Desktop/AIA/limbCMEs/'
 

;First the 0612 event
restore,path+'06122010/results/proflocs_0612.sav'

;193 A
restore,path+'06122010/results/193/regionData_0612_193.sav'
aiagetdiffprofiles,totaldata,indices,proflocs,outProfile,avgProfile,basediffProfile,imProfile,outpath=path+'06122010/results/193/'
stop

;211 A
restore,path+'06122010/results/211/regionData_0612_211.sav'
aiagetdiffprofiles,totaldata,indices,proflocs,outProfile,avgProfile,basediffProfile,imProfile,outpath=path+'06122010/results/211/'

stop


;Then the 0613 event
restore,path+'06132010/results/proflocs_0613.sav'

;193 A
restore,path+'06132010/results/193/regionData_0613_193.sav'
aiagetdiffprofiles,totaldata,indices,proflocs,outProfile,avgProfile,basediffProfile,imProfile,outpath=path+'06132010/results/193/'
stop

;211 A
restore,path+'06132010/results/211/regionData_0613_211.sav'
aiagetdiffprofiles,totaldata,indices,proflocs,outProfile,avgProfile,basediffProfile,imProfile,outpath=path+'06132010/results/211/'

stop



end
