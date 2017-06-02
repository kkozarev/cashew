;---------------------------------------------------------------------------
; Document name: radio_spectro_fits_read.pro
; Created by:    Andre Csillaghy, October 1992
;
; Time-stamp: <Sat Apr 17 2004 18:14:13 csillag apollo>
;---------------------------------------------------------------------------
;+
; NAME:
;       Radio Spectrogram FITS File reader
;
; PURPOSE:
;       Reads (solar) radio spectrograms from FITS files,
;       including non-regular axes. Tested for ETHZ data as well as
;       for Potsdam data
;
; CALLING SEQUENCE:
;       Radio_Spectro_FITS_Read, filename, spectrogram [, time_axis,
;       frequency_axis ]
;
; INPUT:
;       filename: string containing the name of the file with extension
; OUTPUTS:
;       image: 2D array containing the spectrogram.
;       frequency_axis: 1D array containing the frequency axis, in MHz
;       time_axis: 1D array containing the time in seconds from 1 Jan
;                  1979 (can be converted in any other time formats
;                  with Anytim). Can be set relative with the
;                  RELATIVE_TIME keyword. Uses DATE_OBS to set the
;                  correct time.
; KEYWORDS:
;       AXIS_HEADER: returns a structure containing the axis binary
;                    table header information
;       MAIN_HEADER: returns a structure containing the main header
;                    information 
;       NO_FSCALE: if set, the primary data are returned in bytes, i.e. the
;                 scaling is not applied (note: the axes are always scaled)
;       STRUCTURE: if set, the reader returns a spectrogram structure: 
;                  {spectrogram: fltarr(nx,ny), x:fltarr(nx), y:fltarr(ny)}
;       VERBOSE: tells what it's doing while proceeding
;
; MODIFICATION HISTORY
;       2004-04-15:  protection against date_d$end missing from file
;       2003-10-29:  added scaling of the result + no_fscale keyword
;       2003-04:     stuff with loc_file added for idl < 5.6 on 5.6 we
;                    will use file_search acs
;       2003-02:     csillag@ssl.berkeley.edu Now reading the binary table
;                    containing the axes with mrdfits instead of fxbread,
;                    which does work right when the files are compressed
;       2000-03:      Light version for Unix and PC in Jan/March 2000, 
;                    csillag@ssl.berkeley.edu
;       1999-11:     Changed name from ragfitsread to radio_spectro_fits_read, 
;                     adapted for ssw Nov 1999, csillag@ssl.berkeley.edu
;       1998-12:     Extended to read "transposed" fits data and extensions with
;                    only time or frequency axis; Dec. 98, P.Hackenberg,
;                    AIP Potsdam
;       1998-12:     Adaptation for DATE-OBS keyword with time; Dec. 98, P.Hackenberg
;       1998-03:     Adaptation for IDL5/SSW/Ragview in March 98 -- ACs
;       1995-11      Read also compressed file in November 95, ACs
;       1995-08      RELATIVETIME in august 1995, ACs
;       1993-10      DATAMIN, DATAMAX; When only filename provided, the header
;                    is read but not the array. Oct. 93, A.Cs
;       1993-06      SILENT in june, 93, A.Cs
;       1993-05      For IDL Sun in May 1993, 
;                    A.Cs. (originally it was a pv-wave program)
;       1992-10:     Created: A. Csillaghy, ETHZ, October 1992
;-

PRO Radio_Spectro_FITS_Read, filename, spectrogram, time_axis, frequency_axis, $
                             MAIN_HEADER=main_header, $
                             AXIS_HEADER=axis_header, $
                             VERBOSE=verbose, $
                             STRUCTURE=structure, $
                             _EXTRA=_extra, $
                             no_fscale = no_fscale

if keyword_set( verbose ) then silent = 0 else begin silent = 1 & verbose=0 & endelse 		
;PSH, 2003/12/01: added verbose=0 ...

if keyword_set( no_fscale ) then fscale = 0 else fscale = 1

IF N_Params() EQ 0 THEN BEGIN
    Message, 'Usage: radio_spectro_fits_read, filename, image, xAxis, yAxis ]',/INFORMATIONAL 
    RETURN
ENDIF

; o = fitsread( filename = filename )
; --- this disappears
f = FindFile( filename, count=count )
IF count EQ 0 THEN BEGIN
; try again...
    f = loc_file( filename, count=count )
    IF count EQ 0 THEN BEGIN 
        Message, filename + " not found", /CONTINUE
        RETURN
    ENDIF 
ENDIF

;currently reading only 1 file
filename =  filename[0]
; dont forget to scale result, acs 2003-10-29
spectrogram =  MrdFITS( filename, 0, main_header, SILENT=silent, FSCALE=fscale )
main_header =  FitsHead2struct( main_header )
;till here
; instead:
; spectrogram = o->getdata()
; main_header = o->getdata( /struct )

IF verbose THEN BEGIN
    Print, 'Header of FITS file: '
    HELP, main_header, /str
ENDIF

; As discussed in
; ftp://fits.cv.nrao.edu/fits/data/samples/year-2000/year2000.txt
; there is a new DATExxx syntax (CCYY-MM-DDThh:mm:ss) which allows
; to include the time into the date. Thus the keywords TIME-OBS and
; TIME-END are no longer necessary, if the time is already specified
; in DATE-OBS and DATE-END.
; Peter Hackenberg 23.12.1998
;
; If time-obs is present, we store its contents into date-obs so that
; we have a standard date-obs and a redundant time-obs. 

IF ChkTag( main_header, 'TIME_D$OBS') THEN BEGIN 
; acs 2004-04-17
; ragview does not write correctly the fits files. we are trying to
; reconstruct the date from the title infromation
  if valid_time( main_header.date_d$obs ) then begin 
    main_header.date_d$obs =  main_header.date_d$obs + 'T' + main_header.time_d$obs
  endif else begin
    message,  'warning: the date-obs parameter of the fits file is invalid',  /info,  /cont
    message,  'trying to fix the problem .... ',  /info,  /cont
    candidate_date =  strmid( main_header.content, 0, 10 ) 
    if valid_time( candidate_date ) then begin 
      message,  '... yes! the date-obs could be read from the title',  /info,  /cont
      main_header.date_d$obs =  candidate_date +  'T' + main_header.time_d$obs
    endif
  endelse
ENDIF

IF ChkTag( main_header, 'TIME_D$END') THEN BEGIN 
; seems this one is missing sometimes, acs 2004-04-15
  if chktag(  main_header, 'DATE_D$END' ) then begin
    main_header.date_d$end =  main_header.date_d$end + 'T' + main_header.time_d$end
  endif
ENDIF

; Usually radio spectrograms have a frequency and a time axis.
; In the fits definitions is no compulsory rule specifying
; which of these axes goes first and which is the second one.
; The RagView program uses time as x axis and frequency as y axis.
; We try to figure out from CTYPE1 and CTYPE2, whether the fits data 
; are already in right order or not. If not, we "transpose" the data.
; Peter Hackenberg 23.12.1998

axes = MrdFits( filename, 1, axis_header, /FSCALE, silent = silent )
;FxBOpen, unit, filename, 'Axes', axis_header, ERR=err

IF Size( axes, /TYPE ) NE 2 THEN BEGIN 
    axis_header = FITSHead2Struct( axis_header )
    
    axis_1 = axes.time
    
    IF ChkTag( axes, 'FREQUENCY' ) THEN BEGIN 
        axis_2 = axes.frequency
    ENDIF ELSE BEGIN 
        axis_2 = Findgen( main_header.naxis2 ) * main_header.cdelt2 + main_header.crval2
    ENDELSE 

ENDIF ELSE BEGIN 

    axis_1 = Findgen( main_header.naxis1) * main_header.cdelt1 +  main_header.crval1
    axis_2 = Findgen( main_header.naxis2 ) * main_header.cdelt2 + main_header.crval2

ENDELSE

IF Grep( 'time', Strlowcase( main_header.ctype1 ) ) NE '' THEN BEGIN 
    time_axis =  axis_1
    frequency_axis =  axis_2
ENDIF ELSE BEGIN 
    frequency_axis =  axis_1
    time_axis = axis_2
    spectrogram =  Transpose( temporary( spectrogram ) )
ENDELSE 

if valid_time( main_header.date_d$obs ) then begin 
  message
  time_axis =  time_axis + AnyTim( main_header.date_d$obs, /SEC )
endif

IF Keyword_Set( STRUCTURE ) THEN BEGIN 
    struct = {spectrogram: spectrogram, x: time_axis, y: frequency_axis }
    spectrogram = struct
ENDIF

END
