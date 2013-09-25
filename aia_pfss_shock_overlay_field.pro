pro test_aia_pfss_shock_overlay_field
  aia_pfss_shock_overlay_field
end



pro aia_pfss_shock_overlay_field
;PURPOSE:
; This procedure will create overlayed aia/pfss maps.
;THIS PROCEDURE NEEDS TO BE GENERALIZED!
;
;CATEGORY:
; AIA/PFSS_Shock
;
;INPUTS:
;
;KEYWORDS:
; 
;
;OUTPUTS:
;
; 
;DEPENDENCIES:
; diff_map, sub_map, plot_map
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 2011
;

dsun_aia=1574.39*2 ; in px, from index[0].R_SUN
dsun_pfss=192.0 ; in px, from detailed notes in PFSS_VIEWER
rsunarc_aia=945.014 ;solar radius in arcseconds
width=1.6
;=====================================================
;0. Load/create AIA maps, load/create PFSS maps

;-----------------------------------------------

;0.1 The AIA maps


;-----------------------------------------------

;0.2 The PFSS maps
;
; You need to start PFSS by running pfss_viewer. Then, select the
;date/time, then the region on the Sun you're interested
;in. Then, increase the number of lines to something like 300. Then
;follow the steps below for the automated part.
;
;
;SSW Maps:
;      To generate a Dominic Zarro-style map for use within SSW,
;      issue the following commands:
;        IDL> pfss_draw_field,outim=outim,bcent=bcent,lcent=lcent,width=width,mag=mag
;        IDL> nax=size(outim,/dim)
;        IDL> map={data:outim,xc:0.0,yc:0.0,dx:2./nax(0),dy:2./nax(1),time:now,xunits:'normalized',yunits:'normalized',pfss_cent_l0:lcent,pfss_cent_b0:bcent,id:'PFSS field line map'}
;
;      Before calling pfss_draw_field, the following keywords need
;      to be defined.  They correspond exactly to the fields in
;      the "PFSS field line renderer" widget:
;        bcent = map projection latitude in degrees
;        lcent = map projection Carrington longitude in degrees
;        width = width of map in units of solar diameters
;        mag   = magnification factor over nascent magnification
;                (currently 192x192 pixels for full sun)
;
;      Note that the above map is in normalized coordinates, that
;      is, disk center is at (0,0) and the limb has a radius of 1.
;      The image can be scaled to any measurement unit (such as
;      arcseconds) by multiplying dx and dy by the appropriate 
;      conversion factor (e.g. rsun*width, where rsun is measured
;      in arcsec).

;For 06/12/2010
;lcent=55.598
;bcent=0.635
;width=1.6
;mag=rsunarc_aia*2.0/dsun_pfss
;pfss_draw_field,outim=outim0612,bcent=bcent,lcent=lcent,width=width,mag=mag
;nax=size(outim0612,/dim)
;map0612={data:outim0612,xc:0.0,yc:0.0,dx:2./nax(0),dy:2./nax(1),time:now,xunits:'normalized',yunits:'normalized',pfss_cent_l0:lcent,pfss_cent_b0:bcent,id:'PFSSfield line map',roll_angle:0.0,roll_center:[0.0,0.0]}

;save,filename='pfss_map_0612.sav',outim0612,map0612

;/home/kkozarev/Desktop/AIA/limbCMEs/06132010/DEM/new/

;For 06/13/2010
;bcent=0.785
;lcent=39.053
;width=1.6
;mag=rsunarc_aia*2.0/dsun_pfss
;pfss_draw_field,outim=outim0613,bcent=bcent,lcent=lcent,width=width,mag=mag
;nax=size(outim0613,/dim)
;map0613={data:outim0613,xc:0.0,yc:0.0,dx:2./nax(0),dy:2./nax(1),time:now,xunits:'normalized',yunits:'normalized',pfss_cent_l0:lcent,pfss_cent_b0:bcent,id:'PFSS field line map',roll_angle:0.0,roll_center:[0.0,0.0]}
;save,filename='pfss_map_0613.sav',outim0613,map0613


;For 08/18/2010
;bcent=6.780
;lcent=245.841
;width=1.6
;mag=16;rsunarc_aia*2.0/dsun_pfss
;pfss_draw_field,outim=outim0818,bcent=bcent,lcent=lcent,width=width,mag=mag
;nax=size(outim0818,/dim)
;map0818={data:outim0818,xc:0.0,yc:0.0,dx:2./nax(0),dy:2./nax(1),time:now,xunits:'normalized',yunits:'normalized',pfss_cent_l0:lcent,pfss_cent_b0:bcent,id:'PFSS field line map',roll_angle:0.0,roll_center:[0.0,0.0]}
;save,filename='pfss_map_0818.sav',outim0818,map0818


;============================================================
;1. Create the overlaid maps.
;http://www.sipwork.org/?p=42#s4
;Maps are graphically overlayed using the keyword /over in plot_map . In the following example, an SXT map is contoured over an EIT map.

;IDL>plot_map,eit_map,fov=sxt_map,/log  ;-- first plot EIT
;IDL>plot_map,sxt_map,/log,/over,/rotate  ;-- then contour over SXT

;The fov keyword is a map structure from which plot_map infers xrange and yrange values. In the above example, only the sub-region of eit_map that overlaps with that of sxt_map field-of-view is displayed. This keyword is optional. Different sub-regions can be displayed also via the xrange and yrange keywords.

;The /rotate keyword corrects for solar rotation of the overlayed image relative to the first. This correction is important if the time difference between images is more than several hours. The overlayed image can be rotated to a specified time via the keyword time.

;The following example shows an overlay of MDI magnetogram contours on an SXT observation of flare loops. The example illustrates the use of additional keyword options in plot_map.

;IDL>set_line_color
;IDL>levels=100+findgen(11)*100.
;IDL>plot_map,sxt_map,/log,bottom=11
;IDL>plot_map,mdi_map,/over,/rotate,/positive,lcolor=5,levels=levels
;IDL>plot_map,mdi_map,/over,/rotate,/negative,lcolor=2,levels=-levels  

;Each step is explained below:
;set_line_color allocates a simple color table between 0 and 10, in which color=2 corresponds to yellow, and color=5 corresponds to blue. When using set_line_color, it is necessary that plot_map be called with the keyword: bottom=11 to ensure that the image data are scaled to the correct number of available colors above the 11 colors reserved by set_line_color.
;findgen is used to define an array of contour levels: 100,200,...,1000.
;the SXT map specified in sxt_map is plotted.
;positive MDI magnetogram values in the map mdi_map are overplotted as blue contours.
;negative MDI magnetogram values are overplotted as yellow contours.

;The final example shows an overlay of a CDS Ne VI map on an EIT 171 map, for a limb active region.

;IDL>plot_map,eit_map,/log
;IDL>plot_map,cds_map,/over,cthick=2

;The keyword cthick=2 specifies a double thickness contour for the overlay image.



;aia_load_data,'2010/06/12 00:56:00','2010/06/12 01:03:00','211',index,data,/norm
;index2map,index,data,aiamap
;save,filename='aiamap0612.sav',aiamap
;restore,'aiamap0612.sav'
;nmaps=n_elements(aiamap)


;restore,'pfss_map_0612.sav'
;rescale the pfss map units to arcseconds
;map0612.dx*=rsunarc_aia*width
;map0612.dy*=rsunarc_aia*width

;just plot the two maps overlaid
;plot_map,aiamap[0],fov=map0612,/log,/limb
;plot_map,map0612,/over,/rotate

;select an AIA submap, manually.
;xr=[472.8,917.4]
;yr=[265.2,709.8]
;sub_map,aiamap[0],saiamap,/log,/plot ;xrange=xr,yrange=yr


;do the same with the pfss map using the aia submap for reference.
;sub_map,map0612,smap0612,ref_map=saiamap

;plot the submaps and you're done!
;plot_map,saiamap,fov=smap0612,/log,/limb
;plot_map,smap0612,/over,/rotate

;loadct,9,/silent
;tvlct,rr,gg,bb,/get
;wdef,0,800,1000
;make pfss submap
;sub_map,map0612,smap0612,xrange=[472.8,1217.4],yrange=[-265.2,809.8]

;for i=1,nmaps-1 do begin
;make aia difference map
;daiamap=diff_map(aiamap[i],aiamap[0])
;make aia submap
;sub_map,daiamap,sdaiamap,xrange=[472.8,1217.4],yrange=[-265.2,809.8]

;loadct,9,/silent
;plot_map,sdaiamap,fov=smap0612,/limb,dmin=-50,dmax=10
;loadct,6,/silent
;plot_map,smap0612,/over,/rotate,cthick=2
;write_png,'aia_pfss_0612_'+strtrim(string(i+1000),2)+'.png',tvrd(),rr,gg,bb

;endfor





;aia_load_data,'2010/06/13 05:36:00','2010/06/13 05:42:00','211',index,data,/norm
;index2map,index,data,aiamap
;save,filename='aiamap0613.sav',aiamap
restore,'aiamap0613.sav'
nmaps=n_elements(aiamap)

restore,'pfss_map_0613.sav'
;rescale the pfss map units to arcseconds
map0613.dx*=rsunarc_aia*width
map0613.dy*=rsunarc_aia*width

;just plot the two maps overlaid
;plot_map,aiamap[0],fov=map0613,/log,/limb
;plot_map,map0613,/over,/rotate

;select an AIA submap, manually.
;xr=[658.800,1221.00]
;yr=[-829.200,-34.2000]
;sub_map,aiamap[0],saiamap,/log,/plot ;xrange=xr,yrange=yr


;do the same with the pfss map using the aia submap for reference.
;sub_map,map0613,smap0613,ref_map=saiamap

;plot the submaps and you're done!
;plot_map,saiamap,fov=smap0613,/log,/limb
;plot_map,smap0613,/over,/rotate

loadct,9,/silent
tvlct,rr,gg,bb,/get
wdef,0,800,1000
;make pfss submap
sub_map,map0613,smap0613,xrange=[658.800,1221.00],yrange=[-829.200,-34.2000]

for i=1,nmaps-1 do begin
;make aia difference map
daiamap=diff_map(aiamap[i],aiamap[0])
;make aia submap
sub_map,daiamap,sdaiamap,xrange=[658.800,1221.00],yrange=[-829.200,-34.2000]

loadct,9,/silent
plot_map,sdaiamap,fov=smap0613,/limb,dmin=-50,dmax=10
loadct,6,/silent
plot_map,smap0613,/over,/rotate,cthick=2

write_png,'aia_pfss_0613_'+strtrim(string(i+1000),2)+'.png',tvrd(),rr,gg,bb

endfor







;aia_load_data,'2010/08/18 05:05:00','2010/08/18 05:06:00','211',index,data,/norm
;avg=data[*,*,0]/index[0].exptime
;for i=1,n_elements(index)-1 do avg+=data[*,*,i]/index[i].exptime
;avg/=n_elements(index)
;index2map,index[0],avg,avgmap
;aia_load_data,'2010/08/18 05:25:00','2010/08/18 05:35:00','211',index,data,/norm
;index2map,index,data,aiamap
;save,filename='aiamap0818.sav',aiamap,avgmap

restore,'aiamap0818.sav'
nmaps=n_elements(aiamap)

restore,'pfss_map_0818.sav'

width=1.6
 ;solar radius in arcseconds
width=1.6
;=====================================================
;0. Load/create AIA maps, load/create PFSS maps

;-----------------------------------------------

;0.1 The AIA maps


;-----------------------------------------------

;0.2 The PFSS maps
;SSW Maps:
;      To generate a Dominic Zarro-style map for use within SSW,
;      issue the following commands:
;        IDL> pfss_draw_field,outim=outim,bcent=bcent,lcent=lcent,width=width,mag=mag
;        IDL> nax=size(outim,/dim)
;        IDL> map={data:outim,xc:0.0,yc:0.0,dx:2./nax(0),dy:2./nax(1),time:now,xunits:'normalized',yunits:'normalized',pfss_cent_l0:lcent,pfss_cent_b0:bcent,id:'PFSS field line map'}
;
;      Before calling pfss_draw_field, the following keywords need
;      to be defined.  They correspond exactly to the fields in
;      the "PFSS field line renderer" widget:
;        bcent = map projection latitude in degrees
;        lcent = map projection Carrington longitude in degrees
;        width = width of map in units of solar diameters
;        mag   = magnification factor over nascent magnification
;                (currently 192x192 pixels for full sun)
;
;      Note that the above map is in normalized coordinates, that
;      is, disk center is at (0,0) and the limb has a radius of 1.
;      The image can be scaled to any measurement unit (such as
;      arcseconds) by multiplying dx and dy by the appropriate 
;      conversion factor (e.g. rsun*width, where rsun is measured
;      in arcsec).

;For 06/12/2010
;lcent=55.598
;bcent=0.635
;width=1.6
;mag=rsunarc_aia*2.0/dsun_pfss
;pfss_draw_field,outim=outim0612,bcent=bcent,lcent=lcent,width=width,mag=mag
;nax=size(outim0612,/dim)
;map0612={data:outim0612,xc:0.0,yc:0.0,dx:2./nax(0),dy:2./nax(1),time:now,xunits:'normalized',yunits:'normalized',pfss_cent_l0:lcent,pfss_cent_b0:bcent,id:'PFSSfield line map',roll_angle:0.0,roll_center:[0.0,0.0]}

;save,filename='pfss_map_0612.sav',outim0612,map0612

;/home/kkozarev/Desktop/AIA/limbCMEs/06132010/DEM/new/

;For 06/13/2010
;bcent=0.785
;lcent=39.053
;width=1.6
;mag=rsunarc_aia*2.0/dsun_pfss
;pfss_draw_field,outim=outim0613,bcent=bcent,lcent=lcent,width=width,mag=mag
;nax=size(outim0613,/dim)
;map0613={data:outim0613,xc:0.0,yc:0.0,dx:2./nax(0),dy:2./nax(1),time:now,xunits:'normalized',yunits:'normalized',pfss_cent_l0:lcent,pfss_cent_b0:bcent,id:'PFSS field line map',roll_angle:0.0,roll_center:[0.0,0.0]}
;save,filename='pfss_map_0613.sav',outim0613,map0613


;For 08/18/2010
;bcent=6.780
;lcent=245.841
;width=1.6
;mag=16;rsunarc_aia*2.0/dsun_pfss
;pfss_draw_field,outim=outim0818,bcent=bcent,lcent=lcent,width=width,mag=mag
;nax=size(outim0818,/dim)
;map0818={data:outim0818,xc:0.0,yc:0.0,dx:2./nax(0),dy:2./nax(1),time:now,xunits:'normalized',yunits:'normalized',pfss_cent_l0:lcent,pfss_cent_b0:bcent,id:'PFSS field line map',roll_angle:0.0,roll_center:[0.0,0.0]}
;save,filename='pfss_map_0818.sav',outim0818,map0818


;============================================================
;1. Create the overlaid maps.
;http://www.sipwork.org/?p=42#s4
;Maps are graphically overlayed using the keyword /over in plot_map . In the following example, an SXT map is contoured over an EIT map.

;IDL>plot_map,eit_map,fov=sxt_map,/log  ;-- first plot EIT
;IDL>plot_map,sxt_map,/log,/over,/rotate  ;-- then contour over SXT

;The fov keyword is a map structure from which plot_map infers xrange and yrange values. In the above example, only the sub-region of eit_map that overlaps with that of sxt_map field-of-view is displayed. This keyword is optional. Different sub-regions can be displayed also via the xrange and yrange keywords.

;The /rotate keyword corrects for solar rotation of the overlayed image relative to the first. This correction is important if the time difference between images is more than several hours. The overlayed image can be rotated to a specified time via the keyword time.

;The following example shows an overlay of MDI magnetogram contours on an SXT observation of flare loops. The example illustrates the use of additional keyword options in plot_map.

;IDL>set_line_color
;IDL>levels=100+findgen(11)*100.
;IDL>plot_map,sxt_map,/log,bottom=11
;IDL>plot_map,mdi_map,/over,/rotate,/positive,lcolor=5,levels=levels
;IDL>plot_map,mdi_map,/over,/rotate,/negative,lcolor=2,levels=-levels  

;Each step is explained below:
;set_line_color allocates a simple color table between 0 and 10, in which color=2 corresponds to yellow, and color=5 corresponds to blue. When using set_line_color, it is necessary that plot_map be called with the keyword: bottom=11 to ensure that the image data are scaled to the correct number of available colors above the 11 colors reserved by set_line_color.
;findgen is used to define an array of contour levels: 100,200,...,1000.
;the SXT map specified in sxt_map is plotted.
;positive MDI magnetogram values in the map mdi_map are overplotted as blue contours.
;negative MDI magnetogram values are overplotted as yellow contours.

;The final example shows an overlay of a CDS Ne VI map on an EIT 171 map, for a limb active region.

;IDL>plot_map,eit_map,/log
;IDL>plot_map,cds_map,/over,cthick=2

;The keyword cthick=2 specifies a double thickness contour for the overlay image.



;aia_load_data,'2010/06/12 00:56:00','2010/06/12 01:03:00','211',index,data,/norm
;index2map,index,data,aiamap
;save,filename='aiamap0612.sav',aiamap
;restore,'aiamap0612.sav'
;nmaps=n_elements(aiamap)


;restore,'pfss_map_0612.sav'
;rescale the pfss map units to arcseconds
;map0612.dx*=rsunarc_aia*width
;map0612.dy*=rsunarc_aia*width

;just plot the two maps overlaid
;plot_map,aiamap[0],fov=map0612,/log,/limb
;plot_map,map0612,/over,/rotate

;select an AIA submap, manually.
;xr=[472.8,917.4]
;yr=[265.2,709.8]
;sub_map,aiamap[0],saiamap,/log,/plot ;xrange=xr,yrange=yr


;do the same with the pfss map using the aia submap for reference.
;sub_map,map0612,smap0612,ref_map=saiamap

;plot the submaps and you're done!
;plot_map,saiamap,fov=smap0612,/log,/limb
;plot_map,smap0612,/over,/rotate

;loadct,9,/silent
;tvlct,rr,gg,bb,/get
;wdef,0,800,1000
;make pfss submap
;sub_map,map0612,smap0612,xrange=[472.8,1217.4],yrange=[-265.2,809.8]

;for i=1,nmaps-1 do begin
;make aia difference map
;daiamap=diff_map(aiamap[i],aiamap[0])
;make aia submap
;sub_map,daiamap,sdaiamap,xrange=[472.8,1217.4],yrange=[-265.2,809.8]

;loadct,9,/silent
;plot_map,sdaiamap,fov=smap0612,/limb,dmin=-50,dmax=10
;loadct,6,/silent
;plot_map,smap0612,/over,/rotate,cthick=2
;write_png,'aia_pfss_0612_'+strtrim(string(i+1000),2)+'.png',tvrd(),rr,gg,bb

;endfor





;aia_load_data,'2010/06/13 05:36:00','2010/06/13 05:42:00','211',index,data,/norm
;index2map,index,data,aiamap
;save,filename='aiamap0613.sav',aiamap
restore,'aiamap0613.sav'
nmaps=n_elements(aiamap)

restore,'pfss_map_0613.sav'
;rescale the pfss map units to arcseconds
map0613.dx*=rsunarc_aia*width
map0613.dy*=rsunarc_aia*width

;just plot the two maps overlaid
;plot_map,aiamap[0],fov=map0613,/log,/limb
;plot_map,map0613,/over,/rotate

;select an AIA submap, manually.
;xr=[658.800,1221.00]
;yr=[-829.200,-34.2000]
;sub_map,aiamap[0],saiamap,/log,/plot ;xrange=xr,yrange=yr


;do the same with the pfss map using the aia submap for reference.
;sub_map,map0613,smap0613,ref_map=saiamap

;plot the submaps and you're done!
;plot_map,saiamap,fov=smap0613,/log,/limb
;plot_map,smap0613,/over,/rotate

loadct,9,/silent
tvlct,rr,gg,bb,/get
wdef,0,800,1000
;make pfss submap
sub_map,map0613,smap0613,xrange=[658.800,1221.00],yrange=[-829.200,-34.2000]

for i=1,nmaps-1 do begin
;make aia difference map
daiamap=diff_map(aiamap[i],aiamap[0])
;make aia submap
sub_map,daiamap,sdaiamap,xrange=[658.800,1221.00],yrange=[-829.200,-34.2000]

loadct,9,/silent
plot_map,sdaiamap,fov=smap0613,/limb,dmin=-50,dmax=10
loadct,6,/silent
plot_map,smap0613,/over,/rotate,cthick=2

write_png,'aia_pfss_0613_'+strtrim(string(i+1000),2)+'.png',tvrd(),rr,gg,bb

endfor







;aia_load_data,'2010/08/18 05:05:00','2010/08/18 05:35:00','211',index,data,/norm
;avg=data[*,*,0]/index[0].exptime
;for i=1,n_elements(index)-1 do avg+=data[*,*,i]/index[i].exptime
;avg/=n_elements(index)
;index2map,index[0],avg,avgmap
;aia_load_data,'2010/08/18 05:25:00','2010/08/18 05:35:00','211',index,data,/norm
;index2map,index,data,aiamap
;save,filename='aiamap0818.sav',aiamap,avgmap

restore,'aiamap0818.sav'
nmaps=n_elements(aiamap)

restore,'pfss_map_0818.sav'

width=1.6
rsunarc_aia=945.014
;rescale the pfss map units to arcseconds
map0818.dx*=rsunarc_aia*width
map0818.dy*=rsunarc_aia*width

;just plot the two maps overlaid
;plot_map,aiamap[0],fov=map0618,/log,/limb
;plot_map,map0618,/over,/rotate

;select an AIA submap, manually.
;xrange=[658.800,1221.00]
;yrange=[-829.200,-34.2000]
;sub_map,aiamap[0],saiamap,/log,/plot ;xrange=xrange,yrange=yrange


;do the same with the pfss map using the aia submap for reference.
;sub_map,map0818,smap0818,ref_map=saiamap

;plot the submaps and you're done!
;plot_map,saiamap,fov=smap0818,/log,/limb
;plot_map,smap0818,/over,/rotate

loadct,9,/silent
tvlct,rr,gg,bb,/get
wdef,0,800,1000
;make pfss submap
sub_map,map0818,smap0818,xrange=[600.0,1221.00],yrange=[-200.0,800.00]

for i=1,nmaps-1 do begin
   print,o
;make aia difference map
   daiamap=diff_map(aiamap[i],aiamap[0])
;make aia submap
   sub_map,daiamap,sdaiamap,xrange=[600.0,1221.00],yrange=[-200.0,800.00]
   loadct,9,/silent
   plot_map,sdaiamap,fov=smap0818,/limb,dmin=-50,dmax=10
   loadct,6,/silent
   plot_map,smap0818,/over,/rotate,cthick=2
   write_png,'aia_pfss_0818_'+strtrim(string(i+1000),2)+'.png',tvrd(),rr,gg,bb
endfor
;================



end
