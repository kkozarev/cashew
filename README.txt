This folder contains IDL procedures written by Kamen Kozarev and others, in the period 08/01/2010-06/30/2012
The IDL procedures are mostly for analyzing SDO/AIA and radio data.

FOLDERS:
	batch - put batch executable procedures here.
	dem - put procedures related to DEM analysis here.
	examples - put sample implementations of the IDL procedures here.
	studies - put the specific implementations of the procedures for specific events here.
	temp - put temporary procedures, such as testing, here.


PROCEDURES:

PRO FOLDER

pro aia_bdiff_circle_velextract,data,indices,framerange,proflocs,nms,checkbad=checkbad,titprefix=titprefix,outpath=outpath;a quick program to extract the velocity/acceleration of the CME/shock from a set of images. The point selection on the images is;done by hand.

pro aia_bdiff_image_geom_correct,data,indices,framerange,eventfile,arlon,proflocs,titprefix=titprefix,outpath=outpath;This program just corrects the positions determined with the aia_bdiff_image_velextract.pro routine for the fact that the wave;might not be on the limb.

pro aia_bdiff_image_velextract,data,indices,framerange,proflocs,nms,checkbad=checkbad,titprefix=titprefix,outpath=outpath;a quick program to extract the velocity/acceleration of the CME/shock from a set of images. The point selection on the images is;done by hand.

pro aia_bdiff_polyfit_geomcor,indices,framerange,arlon,proflocs,peakpos,nms,checkbad=checkbad,titprefix=titprefix,outpath=outpath;Using the hand measurements of the wave positions, perform second-and third-order polynomial fitting to obtain velocities and;accelerations. This version works on geometrically-corrected data.

pro aia_bdiff_polyfit,indices,framerange,proflocs,peakpos,nms,checkbad=checkbad,titprefix=titprefix,outpath=outpath;Using the hand measurements of the wave positions, perform second-and third-order polynomial fitting to obtain velocities and;accelerations.

pro aia_dem_prep_new,noselect=noselect; This program will prepare files for running DEM on them, and save them in a .sav file as a structure.This file is for 0613 event.

pro aia_dem_prep; This program will prepare files for running DEM on them, and save them in a .sav file as a structure. This file is for 0613 event.
;Original version

pro aia_image_velextract,data,indices,framerange,proflocs,nms,checkbad=checkbad,titprefix=titprefix,outpath=outpath;a quick program to extract the velocity/acceleration of the CME/shock from a set of images. The point selection on the images is;done by hand.

pro aia_inspect_data,index,data,subdata=subdata,subcor=subcor,automatic=automatic,bdiff=bdiff,bratio=bratio;This procedure converts the full 4k AIA data to 1k cube and shows a movie to the user, allowing an inspection of the data. ;Optionally, it can allow the user to select a subregion, and return it.

pro aia_inspect_demsoln;Inspect the results from the DEM analysis done by Mark Weber for the 06/13/2010 coronal shock/wave event

pro aia_inspect_map,map,submap=submap,automatic=automatic;This procedure converts the full 4k AIA mapcube to 1k mapcube and shows a movie to the user, allowing an inspection of the data. ;Optionally, it can allow the user to select a subregion, and return it.

pro aia_load_data,starttime,endtime,wave,index,data,savefile=savefile,nodata=nodata,map=map,norm=norm,noprep=noprep;This procedure reads in a sequence of AIA fits images from the CfA;archive and returns/saves a prepped data cube and index

pro aia_load_event,st,et,wave,index,data,savefile=savefile,map=map,subdata=subdata,submap=submap;This procedure will load full AIA data for a wavelength and time interval,then allow the user to select a subregion, 
;and create sub-arrays and submaps from that selection. When this is done the data can be easily manipulated.

pro aia_make_diff_maps,wav,date,maps,diffmaps,outpath=outpath;A procedure to create base difference maps (map tutorial available at http://www.sipwork.org/?p=42 ), and then;make difference maps. Save the difference and original maps for future use. Also make plots and save them for making a movie.

pro aia_make_diff_rad,wav,date,radData,radIndices,diffradData,outpath=outpath;A procedure to create base difference images from AIA images processed with a radial intensity filter.;Also make plots and save them for making a movie.

pro aia_map_contour_oplot;a quick program that overplots contours of 211 and 193 AIA images

function aia_oplot_radials,index,image,latrange=latrange,angres=angres,flarepos=flarepos,offset=offset,profrad=profrad,tv=tv,color=color,radcirc=radcirc;This program makes a radial grid of profiles going from the sun center out to some distance above the limb, centered in latitude on a;point the user supplies.

pro aia_overlay_pfss;this procedure will create overlayed aia/pfss maps.

pro aia_project_annulus, index, data, ring_width, projdata;This program transforms the r-theta annulus above the solar limb to an x-y rectangular projection.

pro aia_project_annulus_2, index, data, ring_width, projdata, norm=norm, plot=plot,loud=loud,latrange=latrange,latzero=latzero, resolution=resolution;This program transforms the r-theta annulus above the solar limb to;an x-y rectangular projection.

pro aia_rdiff_image_velextract,data,indices,framerange,proflocs,nms,checkbad=checkbad,titprefix=titprefix,outpath=outpath;a quick program to extract the velocity/acceleration of the CME/shock from a set of images. The point selection on the images is;done by hand.

pro aiagetdiffprofiles,data,indices,proflocs,outProfile,avgProfile,basediffProfile,imProfile,outpath=outpath;A procedure for obtaining the radial profiles from specific wavelengths of AIA data.

pro aiamancirclefit,circlepos,p,np=np,error=error;This procedure uses the mpfitellipse procedure to make a circle fit to a bunch of points selected by the user.

pro aiasetprofiles,data,index,proflocs,outpath=outpath,angres=angres,latrange=latrange;A procedure for selecting the radial profiles from AIA data.

pro arrowoverlay,vmaps;A procedure, which overlays arrows on the results from the change detection algorithm, showing the spatial velocity of the change.



FUNCTIONALITIES:

- Load AIA images and save as index+data and/or map structures in a .sav file - see aia_load_event.pro and aia_load_data.pro
- Inspect the AIA images, either in the form of index+data or maps. You can also select subregions and save them - see aia_inspect_data.pro and aia_inspect_map.pro
- Deproject the images on a flat limb - see aia_project_annulus_2.pro, batch_make_annulus.pro
- Perform DEM analysis - see procedures in the dem/ folder
- Overlay PFSS maps on the data - see aia_overlay_pfss.pro
- Measure the velocities of limb-waves - see aia_bdiff_image_velexctract.pro (IMPROVE to correct for geometric projection!), aia_bdiff_polyfit_geomcor.pro
- Automatically detect the waves - see changedet.pro, runcd.pro. This is still under development...



