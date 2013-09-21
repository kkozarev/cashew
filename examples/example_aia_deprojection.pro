pro example_aia_deprojection
;fetch AIA data


;aia_load_data,'2010/06/13 05:35:00','2010/06/13 05:45:00','211',index,data,/cfa,/noprep
;save,index,data,filename='/Volumes/PLUME/AIA_data/normalized_noprep_0613_A211.sav'

;aia_project_annulus,index,data,500,projdata
;save, index, projdata,filename='/Volumes/PLUME/AIA_data/projected_offlimb_0613_A211.sav'
index=0
data=0
projdata=0

;aia_load_data,'2010/06/13 05:35:00','2010/06/13 05:45:00','193',index,data,/cfa,/noprep
;save,index,data,filename='/Volumes/PLUME/AIA_data/normalized_noprep_0613_A193.sav'

;aia_project_annulus,index,data,500,projdata
;save, index, projdata,filename='/Volumes/PLUME/AIA_data/projected_offlimb_0613_A193.sav'


index=0
data=0
projdata=0


;aia_load_data,'2010/06/12 00:55:00','2010/06/12 01:05:00','211',index,data,/cfa,/noprep
;save,index,data,filename='/Volumes/PLUME/AIA_data/normalized_noprep_0612_A211.sav'

;aia_project_annulus,index,data,500,projdata
;save, index, projdata,filename='/Volumes/PLUME/AIA_data/projected_offlimb_0612_A211.sav'
index=0
data=0
projdata=0

;aia_load_data,'2010/06/12 00:55:00','2010/06/12 01:05:00','193',index,data,/cfa,/noprep
;save,index,data,filename='/Volumes/PLUME/AIA_data/normalized_noprep_0612_A193.sav'

;aia_project_annulus,index,data,500,projdata
;save, index, projdata,filename='/Volumes/PLUME/AIA_data/projected_offlimb_0612_A193.sav'

;---------------------------------------------------------------------------------------


;aia_load_data,'2010/08/18 04:45:00','2010/08/18 05:15:00','211',index,data,/cfa,/noprep
;save,index,data,filename='/Volumes/PLUME/AIA_data/normalized_noprep_0818_A211.sav'

;aia_project_annulus,index,data,500,projdata
;save, index, projdata,filename='/Volumes/PLUME/AIA_data/projected_offlimb_0818_A211.sav'
;index=0
;data=0
;projdata=0

;aia_load_data,'2010/08/18 04:45:00','2010/08/18 05:15:00','193',index,data,/cfa,/noprep
;save,index,data,filename='/Volumes/PLUME/AIA_data/normalized_noprep_0818_A193.sav'

;aia_project_annulus,index,data,500,projdata
;save, index, projdata,filename='/Volumes/PLUME/AIA_data/projected_offlimb_0818_A193.sav'


aia_load_data,'2010/08/18 05:25:00','2010/08/18 05:35:00','211',index,data,/norm
save,index,data,filename='/Volumes/PLUME/AIA_data/normalized_noprep_0818_3_A211.sav'

;convert it to a deprojected array
aia_project_annulus_2,index,data,500,projdata,resolution=10,latrange=80,latzero=20,/norm,/plot,/loud

save, index,projdata,filename='/Volumes/PLUME/AIA_data/projected_offlimb_0818_3_A211.sav'

index=0
data=0
projdata=0

aia_load_data,'2010/08/18 05:25:00','2010/08/18 05:35:00','193',index,data,/norm
save,index,data,filename='/Volumes/PLUME/AIA_data/normalized_noprep_0818_3_A193.sav'

;convert it to a deprojected array
aia_project_annulus_2,index,data,500,projdata,resolution=10,latrange=80,latzero=20,/norm,/plot,/loud
save, index, projdata,filename='/Volumes/PLUME/AIA_data/projected_offlimb_0818_3_A193.sav'

end
