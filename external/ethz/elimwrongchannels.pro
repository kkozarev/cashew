PRO ElimWrongChannels, image, x, y, PS = ps,  $
	AVGLIMIT  = avgLimit, SHOW = show
  
;+
; NAME:
;	Eliminate wrong channels 
; PURPOSE:
;	Find out (1) which channel has an unrealistic average relative to the
;	general average and (2) which one has an unrealistic sigma
;	value and then take them out of the image.
; CALLING SEQUENCE:
; 	ElimWrongChannels, image, xAxis, yAxis
; INPUTS:
;	image: 2D array of values
;	xAxis, yAxis: the corresponding 1D array with axis values
; OUTPUTS:
;	image: the cleaned image
;	yAxis: the axis without the channel values eliminated.
; MODIFICATION HISTORY:
;	Created in April 1994, A.Csillaghy, ETHZ
;-


  image = Float( image )
  ny = N_Elements( image(0,*))
  nx = N_Elements( image(*,0))

  IF N_Elements(avgLimit) EQ 0 THEN avgLimit= 10
  IF N_Elements( x ) EQ 0 THEN x = lindgen( nx )
  IF N_Elements( y ) EQ 0 THEN y = lindgen( ny )

  
  ps = Keyword_Set( ps) 

  IF (nx LE 1) OR (ny LE 1) THEN RETURN

  Print, 'Eliminating high-sigma channels ... 
  background = GlidBackSub( image, 3 )

  standardDev = DblArr( ny )
  FOR i=0, ny-1 DO standardDev(i) = StDev( image( *, i) )
  standardDev = BytScl( standardDev )
  meanSigma = Avg( standardDev )
  list = Where( standardDev LT 5*meanSigma )
  Print, N_Elements( standardDev ) - N_Elements( list ), ' channels eliminated'
  IF Total( list ) NE -1 THEN BEGIN
    image = image(*, list )
    y = y( list )
  ENDIF

  Print, 'Eliminating sharp jumps between channels ... '
  yprofile = Avg( Float( Roberts(  Float( image ) ) ), 0 ) 
  yProfile = yProfile - Avg( yProfile )
  meanSigma = StDev( yProfile )

  list = Where( ABS(yProfile) LT 2*meanSigma )
  Print, N_Elements( yProfile ) - N_Elements( list ), ' channels eliminated'
  IF Total( list ) NE -1 THEN BEGIN
    image = image(*, list )
    y = y( list )
  ENDIF

  

  IF ps THEN BEGIN
    Erase
    ny = N_Elements( image(0,*))
    Device, XOFFSET=1, YOFFSET =1,  XSIZE=19, YSIZE = 26
    !p.multi = [ny,  10, ny/10 ]
    FOR i=0,ny-1 DO Plot, image(*,i), xma=0.01, yma=0.01, xs=1, $
	YCHAR = 0.00001, XCHAR = 0.00001, $
	/yno, COLOR = (Total( Where( list EQ i ) ) NE -1 )+ 1
  ENDIF

  IF Total( list ) EQ - 1 THEN BEGIN
    Message, 'Sorry, all channels are bad ...', /INFO, /CONT
    image = 0 & y = 0
    RETURN
  ENDIF
;stop
;  image = image(*, list )
;  y = y( list )


END