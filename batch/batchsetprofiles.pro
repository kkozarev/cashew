pro batchsetprofiles
;set profiles for the AIA events.

path='/home/kkozarev/Desktop/AIA/limbCMEs/'
 

;First the 0612 event
restore,path+'06122010/results/193/regionData_0612_193.sav'
aiasetprofiles,totaldata[30,*,*],indices[30],proflocs,outpath=path+'06122010/results/',angres=1,latrange=20

stop

;Then the 0613 event
restore,path+'06132010/results/193/regionData_0613_193.sav'
aiasetprofiles,totaldata[40,*,*],indices[40],proflocs,outpath=path+'06132010/results/',angres=1,latrange=20
stop
end

