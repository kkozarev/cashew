FUNCTION GlidBackSub, imageP, wLenP, direction, $
	WEIGHTED = weighted, BACKGROUND = backgr

;+
; PURPOSE:
;	Subtracts a gliding background from an image.
; CALLING SEQUENCE:
;	result = GlidBackSub( image, wLen [ , direction ]  )
; INPUT PARAMETERS:
;	image: a 2D array
;	wLen: length of the window defining the gliding 
;		background
; 	direction: 'X' or 'Y'
; RESULT:
;	The image without background
; KEYWORDS:
;	/WEIGHTED: if present, the pixels on the center of the
;		gliding window have more weights in the
;		gliding background definition than the pixel on 
;		the border. Otherwise, they have all the same
;		weight.
; 	BACKGROUND: for getting the background array.
; PROCEDURE:
;	1. Computation of the coefficients (weights)
;	2. For each profile, selection of the gliding window,
;		and computation of the (weighted) average.
;	3. Subtarction of this average from the original image.
;
; MODIFICATION HISTORY:
;	Created in November 1991 by A.Csillaghy,
;		Institute of Astronomy, ETH Zurich.
;-


  IF N_Elements(direction) EQ 0 THEN direction = 'X'

  IF N_Params() NE 2 AND N_Params() NE 3 THEN BEGIN
    Message, 'Usage: result = GlidBackSub( image, wLen [ , direction ]  )',$
	/INFO, /CONT
    RETURN, 0
  ENDIF

  IF (direction EQ 'X') THEN image = imageP $
  ELSE image = Transpose(imageP)

  nx = N_Elements( image(*,0) )
  ny = N_Elements( image(0,*) )

  wLenHalf = Fix(wLenP)/2
  wLen = wLenHalf*2 + 1
 
  backgr = FltArr( nx, ny )

  i = 0L

  IF Keyword_Set( WEIGHTED ) THEN BEGIN
    coeffs = wLenHalf - FIndGen( wLenHalf )
    sum  = Total( coeffs )
    WHILE i LT wLenHalf DO BEGIN
       backgr(i,*) = Total( coeffs * image(0:wLenHalf+i-1, *) ) / sum
       backgr(nx-i-1,*) = Total( Reverse( coeffs ) * image( nx-i-1:nx-1, * ) ) / sum
       i = i+ 1
       coeffs = [ wLenHalf-i, coeffs ]
       sum = sum + coeffs(i)
    ENDWHILE
    WHILE i LT nx-wLenHalf DO BEGIN
       backgr(i,*) = Total( coeffs*image(i-wLenHalf:i+wLenHalf,*) ) / sum
       i = i+1
    ENDWHILE

  ENDIF ELSE BEGIN
      WHILE i LT wLenHalf+1 DO BEGIN
        backgr( i, * ) = Avg( image( 0 : (i + wLenHalf) < (nx-1), * ), 0 )
        i = i+1
      ENDWHILE
      i = wLenHalf + 1L
      WHILE i LT nx - wLenHalf DO BEGIN
        backgr(i, *) = backgr( i-1, * ) + (image( i+wLenHalf, * ) - $
	image( i-1-wLenHalf, * ))/wLen
        i = i+1
      ENDWHILE
      WHILE i LE nx-1 DO BEGIN
        backgr( i, * ) = Avg( image( i: nx-1, * ), 0 ) 
        i = i+1
      ENDWHILE
  ENDELSE
  IF direction EQ 'X' THEN  RETURN,(image - backgr ) $
  ELSE BEGIN
    IF Keyword_Set( BACKGROUND ) THEN $
	backgr = Transpose(backgr)
    RETURN,Transpose( image - backgr )
  ENDELSE

END
