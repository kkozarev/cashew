pro batch_make_annulus

;------------------------0612/A211-----------------------
;get the data
;restore,'/Volumes/PLUME/AIA_data/normalized_noprep_0612_A211.sav'

;convert it to a deprojected array
;aia_project_annulus_2,index,data,600,projdata,latzero=5,latrange=80,resolution=10,/norm,/plot,/loud

;save the result
;save,filename='/Volumes/PLUME/AIA_data/projected_offlimb_0612_A211.sav',index,projdata

;Other stuff you can do with the data...
;pr=projdata
;pr[where(pr gt 3)]=3.0 ;just to see stuff better
;avg=pr[0,*,*]
;for i=1,5 do avg+=pr[i,*,*]
;avg=/6
;for i=0,51 do begin & tvscl,pr[i,*,*]-avg & wait,0.2 & endfor


;------------------------0612/A193-----------------------
;get the data
restore,'/Volumes/PLUME/AIA_data/normalized_noprep_0612_A193.sav'

;convert it to a deprojected array
aia_project_annulus_2,index,data,600,projdata,latzero=5,latrange=80,resolution=10,/norm,/plot,/loud

;save the result
save,filename='/Volumes/PLUME/AIA_data/projected_offlimb_0612_A193.sav',index,projdata




;------------------------0613/A211-----------------------
;get the data
restore,'/Volumes/PLUME/AIA_data/normalized_noprep_0613_A211.sav'

;convert it to a deprojected array
aia_project_annulus_2,index,data,600,projdata,resolution=10,/norm,/plot,/loud

;save the result
save,filename='/Volumes/PLUME/AIA_data/projected_offlimb_0613_A211.sav',index,projdata



;------------------------0613/A193-----------------------
;get the data
restore,'/Volumes/PLUME/AIA_data/normalized_noprep_0613_A193.sav'

;convert it to a deprojected array
aia_project_annulus_2,index,data,600,projdata,resolution=10,/norm,/plot,/loud

;save the result
save,filename='/Volumes/PLUME/AIA_data/projected_offlimb_0613_A193.sav',index,projdata


;------------------------0818/A211-----------------------
;get the data
restore,'/Volumes/PLUME/AIA_data/normalized_noprep_0818_A211.sav'

;convert it to a deprojected array
aia_project_annulus_2,index,data,500,projdata,resolution=10,latrange=80,latzero=20,/norm,/plot,/loud

;save the result
save,filename='/Volumes/PLUME/AIA_data/projected_offlimb_0818_A211.sav',index,projdata



;------------------------0818/A193-----------------------
;get the data
restore,'/Volumes/PLUME/AIA_data/normalized_noprep_0818_A193.sav'

;convert it to a deprojected array
aia_project_annulus_2,index,data,500,projdata,resolution=10,latrange=80,latzero=20,/norm,/plot,/loud

;save the result
save,filename='/Volumes/PLUME/AIA_data/projected_offlimb_0818_A193.sav',index,projdata


end
