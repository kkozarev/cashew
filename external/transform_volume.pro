;+
; NAME:
;       TRANSFORM_VOLUME
;
; PURPOSE:
;
;       The purpose of this program is to transform (e.g., rotate,
;       scale, and translate) a 3D array or volume.
;
;
; AUTHOR:
;
;       Martin Downing,
;       Clinical Research Physicist,
;       Grampian Orthopaedic RSA Research Centre,
;       Woodend Hospital, Aberdeen, AB15 6LS.
;       Pnone: 01224 556055 / 07903901612
;       Fa: 01224 556662
;       E-mail: m.downing@abdn.ac.uk
;
; CATEGORY:
;
;      Mathematics, graphics.
;
; CALLING SEQUENCE:
;
;      result = TRANSFORM_VOLUME( volume )
;
; INPUTS:
;
;       volume:    The 3D array or volume to be transformed, expected
;       to have dimensions [3,N], where N is the number of points.
;
; OPTIONAL KEYWORDS:
;
;      BUFFER_SIZE: To reduce memory overhead the routine processes the job in chunks, the number
;         of elements of which can be set using the BUFFER_SIZE keyword, set this keyword to
;         0 to force the whole array to be processed at one time. The default value is 128.
;
;      MISSING: The value to return for transformed values outside the bounds of
;         the volume. (Passed to the INTERPOLATE function.) Default is 0.
;
;      T3DMAT: The homogeneous transforamtion matrix. If this keyword is not present,
;         the following keywords can be used to create a homogeneous transformation matrix:
;
;         ROTATION - The rotation vector [rx,ry,rz]. The order of rotation is ZYX.
;         TRANSLATE - The translation vector [tx,ty,tz].
;         SCALE - The scale vector [sx,sy,sz].
;         CENTRE_ROTATION - The centre of rotation [cx,cy,cz].
;
; OUTPUTS:
;
;       result:    The transformed array or volume.
;
; COMMON BLOCKS:
;
;       None.
;
; DEPENDENCIES:
;
;       The program uses the library INTERPLOLATE routine, which currently (IDL 5.4)
;       uses linear interpolation. Note that the operation is performed in chunks,
;       each of which is independant of the result of the others, so the operation
;       could easiliy be parallelised.
;
; MODIFICATION HISTORY:
;
;       Written by: Martin Downing, 16 September 2001.
;       Added MISSING keyword. Removed INPLACE keyword. 25 Nov 2001. MD
;-
FUNCTION Transform_Volume, volume, Rotation=rotation, $
    Scale=scale, Translate=translate, Centre_Rotation=centre_rotation, $
    T3Dmat=t3dmat,yzexch=yzexch
  if !P.t3d eq 0 then !P.t3d=1

;If only a point is provided, create a volume around it.
  sz=size(volume)
  if sz[0] eq 1 then begin
     vol=fltarr(3,5)
     vol[*,0]=volume
     volume=vol
  endif

   ; Create a transform matrix, if one is not provided.
IF N_Elements(t3dmat) EQ 0 THEN begin

   IF N_Elements(rotation) EQ 0 THEN rotation =[0,0,0]
   IF N_Elements(centre_rotation) EQ 0  THEN centre_rotation=[0,0,0]
   IF N_Elements(translate) EQ 0 THEN translate =[0,0,0]
   IF N_Elements(scale) EQ 0 THEN scale =[1,1,1]
   
   T3D, /Reset
   if keyword_set(yzexch) then T3D, /yzexch
   T3D, Translate = -centre_rotation
   T3D, Rotate=rotation
   T3D, Translate= centre_rotation + translate, Scale=scale
   t3dmat = !P.T
   vol_t=vert_t3d(volume)
ENDIF else vol_t=vert_t3d(volume,matrix=t3dmat)

!P.t3d=0
; Return the transformed volume.
if sz[0] eq 1 then vol_t=vol_t[*,0]
RETURN, vol_t
END
