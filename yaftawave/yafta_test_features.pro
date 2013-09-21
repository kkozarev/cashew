pro yafta_test_features
; A test program to get a feeling for what can be done with the YAFTA
;  features structures...

;Load some yafta results.
restore,'/home/kkozarev/algoTests/yafta/yafta_test_results.sav'
;Variables are: all_masks,data,index,features
nsteps=n_elements(index)

;The number of features that were detected in total.
nfeat=max(features.label)

;If you want to find out how many features were detected by the
;algorithm at every time step, use histogram.
featdist=histogram(features.step)
maxfeat=max(featdist)

stop

;If you want to get the labels of the features detected at individual
;time steps.Negative values are just place holders
featinfo=fltarr(maxfeat,nsteps)-1
cc=0
for j=0,nsteps-1 do begin
   for i=0,featdist[j]-1 do begin
      featinfo[i,j]=features[cc].label
      cc++
   endfor
endfor


;2. Obtaining information from individual features.

;Let's select a feature and examine it.
;n8 is the number of frames, in which this feature was detected.
ind8=where(features.label eq 8,n8)
feat8=features[ind8]

;The first step, at which this feature appeared.
;Note: This is the index+1.
initstep=abs(feat8[0].src)

;If the feature did not persist until the end of the period,
;find the last step. Note: This is the index+1.
laststep=where(feat8.trm le 0)+initstep-1

end

;============================================================================

pro yaftawave_remove_feature,inmasks,infeatures,label,outmasks,outfeatures
;This procedure will serve to remove a certain feature from the list
;of features.
;Kamen Kozarev - 09/2011

nframes=n_elements(inmasks[0,0,*])
outmasks=inmasks
;The mask is an array with the same dimensions as the image, where
;each pixel identified as part of the feature has as a value its label index.

;1. Remove the feature from the mask array.
for i=0,nframes-1 do begin
   mask=inmasks[*,*,i]
   ind=where(mask eq label,nind)
   if nind gt 0 then mask[ind]=0.0
   outmasks[*,*,i]=mask
endfor

;2. Remove the feature from the mask 
nolabel=where(infeatures.label ne label,nlb)
if nlb eq 0 then begin
   print,'Label does not exist! Try again...'
   return
endif
outfeatures=infeatures[nolabel]
end

;============================================================================

pro yaftawave_remove_features,inmasks,infeatures,labels,outmasks,outfeatures
;A wrapper procedure to remove multiple features.
;Kamen Kozarev, 09/2011
inm=inmasks
inf=infeatures

  for i=0,n_elements(labels)-1 do begin
     label=labels[i]
     yaftawave_remove_feature,inm,inf,label,tmpmasks,tmpfeatures
     inf=tmpfeatures
     inm=tmpmasks
  endfor
  outfeatures=tmpfeatures
  outmasks=tmpmasks

end



;============================================================================

pro yaftawave_extract_features,inmasks,infeatures,labels,outmasks,outfeatures
;This procedure will serve to extract one or more features from the list
;of features.
;Kamen Kozarev - 09/2011

  nx=n_elements(inmasks[*,0,0])
  ny=n_elements(inmasks[0,*,0])
  nframes=n_elements(inmasks[0,0,*])
  nlabels=n_elements(labels)
  outmasks=fltarr(nx,ny,nframes)
  
;The mask is an array with the same dimensions as the image, where
;each pixel identified as part of the feature has as a value its label index.

;1. Extract the features from the mask array.
  for nf=0,nframes-1 do begin
     tmpmask=fltarr(nx,ny)
     mask=inmasks[*,*,nf]
     
     for nl=0,nlabels-1 do begin
        label=labels[nl]
        ind=where(mask eq label,nind)
        if nind ne 0 then tmpmask[ind]=label
     endfor
     outmasks[*,*,nf]=tmpmask
  endfor

;2. Extract the features from the features array
  for nl=0,nlabels-1 do begin
     label=labels[nl]
     lab=where(label eq infeatures.label,nlb)
     if nlb eq 0 then begin
        print,'Label does not exist! Try again...'
        return
     endif
     if nl eq 0 then labind=lab else labind=[labind,lab]
  endfor
  outind=labind[sort(labind)]
  outfeatures=infeatures[outind]

end


;============================================================================

function yaftawave_get_pixel_coordinates, inmasks, label
;This function will extract all x,y coordinate pairs from a YAFTA
;mask of a single frame, for a particular feature.
;It returns an [2,N] array, where N is the number of pixels within the feature.
;Kamen Kozarev - 09/2011

  nx=n_elements(inmasks[*,0,0])
  ny=n_elements(inmasks[0,*,0])
  nframes=n_elements(inmasks[0,0,*])
  
  ;Define a structure array to hold the coordinates
  coord={label:0.0,xc:fltarr(100000),yc:fltarr(100000),nc:-1.0}
  coords=replicate(coord,nframes)

;Loop over all frames, checking to see if the feature exists before
;calculating the coordinates. If no feature pixels, nc is set to -1.0.
  for nf=0,nframes-1 do begin
     index=where(inmasks[*,*,nf] eq label,nind)
     if nind gt 0 then begin
        xc = index MOD nx  ;column
        yc = index/nx   ;row
        coords[nf].nc=n_elements(xc)
        coords[nf].xc=xc
        coords[nf].xc=coords[nf].xc[0:coords[nf].nc]
        coords[nf].yc=yc
        coords[nf].label=label
     endif

  endfor

  return,coords
  
end


function yaftawave_return_farthest_pixels, inmasks, label, ARcoords, numpoints=numpoints
;This function will return the coordinates of the NUMPOINTS farthest
;pixels from a given ARCOORDS position for a particular feature with a
;given LABEL. NUMPOINTS is the number of points to be returned, with a
;default value of 10.

  if not keyword_set(numpoints) then numpoints=10
  nx=n_elements(inmasks[*,0,0])
  ny=n_elements(inmasks[0,*,0])
  nframes=n_elements(inmasks[0,0,*])
  farpix=fltarr(2,numpoints,nframes)

;1. Get the pixel coordinates of the feature
  allpix=yaftawave_get_pixel_coordinates(inmasks,label)
  
  
;2. Compute the distance metric and select the farthest pixels for
;each frame
  for nf=0,nframes-1 do begin
     nc=allpix[nf].nc
     if nc gt 0 then begin
        ;for pp=0,nc-1 do begin
        rdist=sqrt((allpix[nf].xc[0:nc-1]-ARcoords[0])^2+$
                   (allpix[nf].yc[0:nc-1]-ARcoords[1])^2)
        farindex=reverse(sort(rdist))
        farindex=farindex[0:numpoints-1]
        farpix[0,*,nf]=allpix[nf].xc[farindex]
        farpix[1,*,nf]=allpix[nf].yc[farindex]
     endif
  endfor
  
  return,farpix
end


function yaftawave_return_extreme_pixels, inmasks, label, ARcoords, numpoints=numpoints
;This function will return the coordinates of NUMPOINTS extreme pixels
;for a particular feature with a given LABEL. NUMPOINTS is the number
;of points to be returned, with a default value of 10.
;The function will examine the position of the farthest pixels from
;the ARcoords in each of two dimensions. For example, if it determines that the
;feature is moving south and east from the AR, it will give
;NUMPOINTS/2 pixels in each of those directions, for fitting.

  if not keyword_set(numpoints) then numpoints=10
  nx=n_elements(inmasks[*,0,0])
  ny=n_elements(inmasks[0,*,0])
  nframes=n_elements(inmasks[0,0,*])
  farpix=fltarr(2,numpoints,nframes)

;1. Get the pixel coordinates of the feature
  allpix=yaftawave_get_pixel_coordinates(inmasks,label)
  
  
;2. Compute the distance metric for each frame - the farthest pixels
;in both dimensions, separately.
  
  for nf=0,nframes-1 do begin
     nc=allpix[nf].nc
     if nc gt 0 then begin
        rdist=sqrt((allpix[nf].xc[0:nc-1]-ARcoords[0])^2+$
                   (allpix[nf].yc[0:nc-1]-ARcoords[1])^2)
        farindex=reverse(sort(rdist))
        farindex=farindex[0:numpoints-1]
        farpix[0,*,nf]=allpix[nf].xc[farindex]
        farpix[1,*,nf]=allpix[nf].yc[farindex]

;Check the farthest pixels - where they are, relative to the AR:
        xsign=-1.0
        if mean(farpix[0,*,nf]-ARcoords[0]) gt 0.0 then xsign=1.0
        ysign=-1.0
        if mean(farpix[1,*,nf]-ARcoords[1]) gt 0.0 then ysign=1.0
           
;DEBUG!
        ;In the X-direction, find the farthest
        ;pixels, which have y-coordinates on
        ;the appropriate side of the extreme
        ;pixels' locations        
        if ysign lt 0 then yind=where(farpix[1,*,nf] lt ARcoords[1]) else $
           yind=where(farpix[1,*,nf] ge ARcoords[1])
        
        exydist=sqrt((allpix[nf].xc[yind]-ARcoords[0])^2+$
                     (allpix[nf].yc[yind]-ARcoords[1])^2)
        exyindex=reverse(sort(exydist))
        exyindex=farindex[0:numpoints/2-1]


        ;In the Y-direction, find the farthest
        ;pixels, which have x-coordinates on
        ;the appropriate side of the extreme
        ;pixels' locations        
        if xsign lt 0 then xind=where(farpix[0,*,nf] lt ARcoords[0]) else $
           xind=where(farpix[0,*,nf] ge ARcoords[0])
        
        exxdist=sqrt((allpix[nf].xc[xind]-ARcoords[0])^2+$
                     (allpix[nf].yc[xind]-ARcoords[1])^2)
        exxindex=reverse(sort(exxdist))
        exxindex=farindex[0:numpoints/2-1]
;END DEBUG!

     endif
  endfor

  return,farpix
end

pro tvmask, inmask, label
;A simple procedure to overplot the positions of a detected feature
;on an image.
sz=size(inmask)
if sz[0] ne 2 then begin
print,'Mask must be an m by n array. Returning...'
return
endif


res=yaftawave_get_pixel_coordinates(inmask,label)
xc=res.xc[0:res.nc-1]
yc=res.yc[0:res.nc-1]
plots,xc,yc,psym=3,/device
end
