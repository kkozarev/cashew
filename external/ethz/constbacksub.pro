FUNCTION ConstBackSub, image, min, max, direction, $
	BACKGROUND = background, $
	AUTOMATIC = automatic, BKGARR = bkgArr

;+ 
; PURPOSE: 
;	Computes the mean background out ouf a defined
;	set of neighbouring profiles and subtracts it.
; CALLING SEQUENCE:
;	result = ConstBackSub( image, min, max [, direction ] )
; INPUTS:
;	image: 2D array
;	min, max: pixel number of beginning and end of the
;		background region, respectively
;	direction: 'X' or 'Y'
; KEYWORDS:
;	BACKGROUND: to get the background array subtracted
;	AUTOMATIC: the background array to subtract is
;		searched automatically by taking some vectors
;		in the image with lowest mean and lowest
;		variance as background vectors. If a number is
;		passed it is taken as percet of channels to 
;		consider else the default is 5 %.
;	BKGARR: the background field computed by the AUTOMATIC proc.
; PROCEDURE:
;	Between min and max, the values are averaged in
;	X or Y, and the resulting vector is subtracted from the
;	original image.
; SIDE EFFECT:
;	The image is returned as FLOAT type.
; MODIFICATION HISTORY:
;	Created in August 1991 by A.Csillaghy,
;		Inst. of Astronomy, ETH Zurich
;	AUTOMATIC in july 93, A.Cs
;	BKGARR in Jan 94, A.Cs.
;       Performance optimization, June 98, P.Messmer
;-
  print, "Running Constant Backsub algorithm Version 2.1"
;  t = systime(1)
  IF N_Elements(direction) EQ 0 THEN direction = 'X'
  IF direction EQ 'X' THEN im = float(image) $
  ELSE im = float(temporary(Transpose(image)))

  nx = N_Elements( im(*,0))
  ny = N_Elements( im(0,*))

  IF Keyword_Set( AUTOMATIC ) THEN BEGIN

;-----
; test suite: demonstrates that original const-back-sub is 
;             not nice:
;
;   help, avg(im, 1)
;   help, Sigma( im,1,1)
; 
;   these vectors are added (!) (after normalization)
;-----

; First compute average over time  for every frequency channel
; Therefor the average is computed along dimension 0

    average = Avg( im, 0 )

; subtract this average value from every channel

    for i = 0L, ny-1 do im(*,i) = im(*,i) - average(i)

; now compute the standard-deviation for every timestep
; we transpose the matrix to have quicker  array operations

;    im = transpose(im)
;    sig = fltarr(nx)
;    for i = 0L, nx-1 do sig(i) = stdev(im(*,i))
;    im = transpose(im)

;    sig = fltarr(ny)
;    for i = 0L, ny-1 do sig(i) = stdev(im(i,*))
    sig = fltarr(nx)
    for i = 0L, nx-1 do sig(i) = stdev(im(i,*))

; build the list of background candidates: take those 
; time steps with lowest standard deviation

    list = sort(sig)

; keep only a certain amount of the possible background
; candidates

    IF automatic EQ 1 THEN automatic = 0.05
    nPart = (N_Elements( list )*automatic) < ( nx-1) > 0
    list = list( 0: nPart )
  ENDIF ELSE BEGIN
    min = min > 0 < (nx-1) 
    max = max > 0 < (nx-1)
    list = LIndGen( max-min ) + min
  ENDELSE

  bkgArr = im( list, * )
  background = Avg( bkgArr, 0 ) 
  FOR j  = 0L, ny-1 DO im(*,j) = im(*,j) - background(j)
  IF direction EQ 'X' THEN RETURN, im $
  ELSE RETURN, Transpose( im )
END
