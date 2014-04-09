function aia_update_subdata_index,index,subroi_start,npx,coords
;PURPOSE:
;This procedure will update several AIA index tags,
;which need to be changed when choosing a subregion
;of data.
;
;CATEGORY:
; AIA/General
;
;INPUTS:
;      index - the index structure to be updated
;      subroi_start - the [x,y] pixel coords of lower left corner of the subroi.
;      npx - an array of [NX,NY] pixels of the subroi
;KEYWORDS:
; 
;
;OUTPUTS:
;
; 
;DEPENDENCIES:
;aia_get_arcoords, rep_tag_value (sswidl)
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 07/06/2011
;11/14/2013, Kamen Kozarev - changed npx to be 

  newind=index
  if n_elements(subroi_start) lt 2 then begin
     print,'subroi_start=[x,y]. Aborting...'
     return,0
  endif
  
;1. Update the image dimensions
  newind=rep_tag_value(newind,npx[0]*1L,'NAXIS1')
  newind=rep_tag_value(newind,npx[1]*1L,'NAXIS2')
  
  
;2. Update the pixel center of the sun
  x0=index[0].X0_MP
  y0=index[0].Y0_MP
  
  nx0=x0-subroi_start[0]
  ncrpx1=nx0+1
  ny0=y0-subroi_start[1]
  ncrpx2=ny0+1
  newind=rep_tag_value(newind,nx0,'X0_MP')
  newind=rep_tag_value(newind,ncrpx1,'CRPIX1')
  newind=rep_tag_value(newind,ny0,'Y0_MP')
  newind=rep_tag_value(newind,ncrpx2,'CRPIX2')
  
;3. Add a tag holding the starting pixels of the ROI
  newind=add_tag(newind,subroi_start[0],'SUBROI_X0')
  newind=add_tag(newind,subroi_start[1],'SUBROI_Y0')

;4. Update the history tag
  t=systime()
  newind=rep_tag_value(newind,[newind[0].history,[' subdata extracted at: '+t]],'HISTORY')
  
  
  arcoords=aia_get_arcoords(index[0],coords,subroi_start,arlonlat=arlonlat)
;5. Add tags for the AR position, in pixels.
  newind=add_tag(newind,arcoords[0],'ARX0')
  newind=add_tag(newind,arcoords[1],'ARY0')
  
;6. Add tags for the AR coordinates, in degrees.
  newind=add_tag(newind,arlonlat[0],'ARLON')
  newind=add_tag(newind,arlonlat[1],'ARLAT')
  
  return,newind
end
