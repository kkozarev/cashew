pro test_plot_srs_kak
fname='./LM110511.SRS'
plot_srs_kak,fname,/repair
end


pro plot_srs_kak,inputFileName,repair=repair
;
;--------------------------------------------------------------------------------;+
; NAME:
;     plot_srs    
; PURPOSE:
;     Routine to plot daily RSTN/SRS spectrogram
; WRITTEN BY
;     William Denig (NOAA/NGDC/STP)
; CALLING SEQUENCE:
;     plot_srs
; INPUTS:
;     daily srs data files available from NGDC
;
;HISTORY
;     06 Jan 2014 - Derived from code kindly provided by Samuel D. Tun (SDT)
;     04 Jun 2014 - Rewrite for the older versions of IDL (KAK)
;KNOW ISSUES - Work-off list
;  - X-axis labels are sometimes clobbered; see lm120708.srs (06 Jan 2014)
;  - Filling gap assumes 3-s sample time - not always true; see ho010505.srs (06 Jan 2014)    
;
;********************************************************************************************
;
;Get filename and open file
;
openFile$:
If strLen(inputFileName) EQ 0 Then goTo, endJob$
Print,' SRS File: ',inputFileName
tmp=strmid(file_basename(inputFileName),0,2)
case tmp of
   'PA': iStationID='PHFF'
   'LM': iStationID='APLM'
   'HO': iStationID='KHMN'
   'K7': iStationID='K70L'
   'SV': iStationID='LISS'
   else: iStationID='XXXX'
endcase
if strLen(iStationID) EQ 0 Then goTo, endjob$
iStationID = strUpCase(iStationID)

Openr,u_dpd,inputFileName,/get_lun
inputArray=read_binary(inputFileName,/swap_endian)
Close,u_dpd
Free_LUN,u_dpd
goTo,processFile$
;
;Error handler (open)
;
openFileError$:
On_IOerror,null
Free_LUN,u_dpd
Print,' ***** File Error - Try again *****'
goTo,openFile$
;
processFile$:
if strLen(iStationID) NE 4 OR                              $
  (iStationID NE 'KHMN' AND                                $
   iStationID NE 'APLM' AND                                $
   iStationID NE 'PHFF' AND                                $
   iStationID NE 'K7OL' AND                                $
   iStationID NE 'LISS') Then Begin
   Print,' **** Station ID error ****'
   goTo, endjob$
 endIf
;  
nElements     = n_Elements(inputArray)                     ;Number of elements in the binary data
nTimeElements = nElements/826                              ;Number of time steps in file
inputArray    = Reform(inputArray,826,nTimeElements)       ;Reform into a frequency x time array
inputArray    = Rotate(inputArray,4)                       ;Rotate & Transpose into a time x frequency array
;
;Verify contiguous dataset
;
iOption$:
iYrArray = fix(Reform(inputArray[*,0]))
iMnArray = fix(Reform(inputArray[*,1]))
iDyArray = fix(Reform(inputArray[*,2]))
iHhArray = fix(Reform(inputArray[*,3]))
iMmArray = fix(Reform(inputArray[*,4]))
iSsArray = fix(Reform(inputArray[*,5]))
contiguousTime = 86400*Long(iDyArray-1) + 3600*Long(iHhArray) + 60*Long(iMmArray) + Long(iSsArray)
for i = 1,nTimeElements-1 do Begin                         ;Look for datagaps GT 15 seconds
  if contiguousTime[i] GT contiguousTime[i-1]+15 then Begin
    iAnswer = ''
    print,''
    print,'TimeStep: ',i-1,' =',contiguousTime[i-1],'    TimeStep: ',i,' =',contiguousTime[i] 
   ; read,prompt='*****File is non-contiguous. Repair? (Y/N): ',iAnswer
    if keyword_set(repair) then iAnswer='Y' else iAnswer='N'
    if strLen(iAnswer) EQ 0 Then goTo,endJob$
    iAnswer = strMid(iAnswer,0,1)
    if iAnswer EQ 'N' OR  iAnswer EQ 'n' Then goTo,continueJob$
    if iAnswer NE 'Y' AND iAnswer NE 'y' Then Begin
      print,'*****Unrecognized response - Try again'
      goTo,iOption$
    endIf
;    
    dummyArray    = bytArr(826)                            ;Establish dummy array for filling data gaps
    for j=0,23 do dummyarray[j] = 0 & for j=24,825 do dummyarray[j] = 0
    iTimeDiff = contiguousTime[i] - contiguousTime[i-1]
    iStepDiff = Fix(iTimeDiff/3)                           ;Steps to fill the gap
    fillArray = bytArr(iStepDiff,826)                      ;Establish fillArray
    iLow = i-1 & iHigh = i + iStepDiff                     ;Bracket iLow and iHigh
    fillArray = bytArr(iStepDiff,826)
    for j = 0,iStepDiff-1 do fillArray[j,*] = dummyArray
    tempArray = inputArray[0:iLow,*]
    tempArray = [tempArray,fillArray]
    tempArray = [tempArray,inputArray[iLow+1:nTimeElements-1,*]]
    inputArray = tempArray
    nTimeElements = nTimeElements + iStepDiff
;
;Last valid record
;
    iLow = i - 1                                               ;Index to last valid record
    yrLow  = inputArray[ iLow,0] & if  yrLow LE 50 Then  yrLow= yrLow+2000 Else  yrLow= yrLow+1900
    mnLow  = inputArray[ iLow,1]
    dyLow  = inputArray[ iLow,2]
    hhLow  = inputArray[ iLow,3]
    mmLow  = inputArray[ iLow,4]
    ssLow  = inputArray[ iLow,5]
    julDaySecLow  = uLong64(86400.*julDay( mnLow, dyLow, yrLow, hhLow, mmLow, ssLow))
;    print,'Last valid record',julDaySecLow, mnLow, dyLow, yrLow, hhLow, mmLow, ssLow
;
;Build fill values    
    for j = 1,iStepDiff do Begin
      julDaySec = julDaySecLow + j*3
      julDayDay = Double(julDaySec) / Double(86400)
      calDat,julDayDay,mnStep,dyStep,yrStep,hhStep,mmStep,ssStep
      ssStep = fix(ssStep+0.5)
      if yrStep LT 2000 Then yrStep=yrStep-1900 Else yrStep=yrStep-2000
      inputArray[iLow+j,0] = Byte(yrStep)
      inputArray[iLow+j,1] = Byte(mnStep)
      inputArray[iLow+j,2] = Byte(dyStep)
      inputArray[iLow+j,3] = Byte(hhStep)
      inputArray[iLow+j,4] = Byte(mmStep)
      inputArray[iLow+j,5] = Byte(ssStep)
;      print,'Calculating:',julDaySec,mnStep,dyStep,yrStep,hhStep,mmStep,ssStep
    endFor
;
;;Next valid record
;
    iHigh = i + iStepDiff                                  ;Index to next valid record
    yrHigh = inputArray[iHigh,0] & if yrHigh LE 50 Then yrHigh=yrHigh+2000 Else yrHigh=yrHigh+1900
    mnHigh = inputArray[iHigh,1]
    dyHigh = inputArray[iHigh,2]
    hhHigh = inputArray[iHigh,3]
    mmHigh = inputArray[iHigh,4]
    ssHigh = inputArray[iHigh,5]
    julDaySecHigh = uLong64(86400.*julDay(mnHigh,dyHigh,yrHigh,hhHigh,mmHigh,ssHigh))
;    print,'Next valid record:',julDaySecHigh,mnHigh,dyHigh,yrHigh,hhHigh,mmHigh,ssHigh 
    goTo,iOption$   
  endIf
endFor
;
continueJob$:
iYr = strTrim(iYrArray,2)
iMn = strTrim(iMnArray,2)
iDy = strTrim(iDyArray,2)
iHh = strTrim(iHhArray,2)
iMm = strTrim(iMmArray,2)
iSs = strTrim(iMmArray,2)
;
iLimit = ['','']
iIndex = [0,nTimeElements-1]
For i = 0,1 do Begin
  iTimeStyle = '00/00/00 00:00:00'
  iKntr  = iIndex[i]
  iYrNow = iYr[iKntr]
  iDyNow = iDy[iKntr]
  iMnNow = iMn[iKntr]
  iHhNow = iHh[iKntr]
  iMmNow = iMm[iKntr]
  iSsNow = iSs[iKntr]
;
  if strLen(iMn[iKntr]) EQ 2 Then strPut,iTimeStyle,iMnNow, 0 Else strPut,iTimeStyle,iMnNow, 1
  if strLen(iDy[iKntr]) EQ 2 Then strPut,iTimeStyle,iDyNow, 3 Else strPut,iTimeStyle,iDyNow, 4
  if strLen(iYr[iKntr]) EQ 2 Then strPut,iTimeStyle,iYrNow, 6 Else strPut,iTimeStyle,iYrNow, 7  
  if strLen(iHh[iKntr]) EQ 2 Then strPut,iTimeStyle,iHhNow, 9 Else strPut,iTimeStyle,iHhNow,10
  if strLen(iMm[iKntr]) EQ 2 Then strPut,iTimeStyle,iMmNow,12 Else strPut,iTimeStyle,iMnNow,13
  if strLen(iSs[iKntr]) EQ 2 Then strPut,iTimeStyle,iSsNow,15 Else strPut,iTimeStyle,iSsNow,16 
  iLimit[i] = iTimeStyle
  if i EQ 1 Then print,'Strt Time: ',iLimit[i] Else print,'Stop Time: ',iLimit[i]
endFor
;
tSecondOfDay=iHhArray*3600.+ iMmArray*60.+ Float(iSsArray)
tHourOfDay = tSecondOfDay/3600.
;
;Now make the frequency arrays, as in the SRS documentation
;
fA = 25. + ( 50.*lindgen(401))/400.
fB = 75. + (105.*lindgen(401))/400.
f_MHz=[fA,fB]
;
spectralArray = inputArray[*,24:*]                         ;Clip out the spectral part
x = uindGen(nTimeElements)
y = uindGen(802)
r = nTimeElements/802
;
;Build plot title
;
plotTitle = 'Station: xxxx | Start: 00/00/00 00:00:00 | End: 00/00/00 00:00:00'
strPut,plotTitle,iStationID,9
timeChr = iLimit[0] & strPut,plotTitle,timeChr,23
timeChr = iLimit[1] & strPut,plotTitle,timeChr,48
;
;Build x and y labels
;
xTickChr = ['','','','','','']
for i = 0,5 do Begin
  xTickStyle = '00:00:00'
  iKntr = i*fix(nTimeElements/5.)
  if i EQ 5 then iKntr = iKntr - 1                         ;Get the last time record
  iHhNow = iHh[iKntr]
  iMmNow = iMm[iKntr]
  iSsNow = iSs[iKntr]
;
  if strLen(iHh[iKntr]) EQ 2 Then strPut,xTickStyle,iHhNow, 0 Else strPut,xTickStyle,iHhNow, 1
  if strLen(iMm[iKntr]) EQ 2 Then strPut,xTickStyle,iMmNow, 3 Else strPut,xTickStyle,iMnNow, 4
  if strLen(iSs[iKntr]) EQ 2 Then strPut,xTickStyle,iSsNow, 6 Else strPut,xTickStyle,iSsNow, 7
;
  xTickChr[i] = xTickStyle
endfor
yTickChr = ['25','35','45','55','65','75','96','117','138','159','180']
;
;stop
wdef,0,900,600
loadct,39,/silent
!p.position=[0.05,0.12,0.87,0.90]
!p.background=255
!p.color=0
plot_image, spectralArray,$
            title=plotTitle,charsize=2,$
            xtitle='Universal Time (UT)',$
            ytitle='Frequency (MHz)',xstyle=1,ystyle=1,$
            xTickName=xTickChr,xMinor=0,xticks=n_elements(xTickChr)-1,$
            yTickName=yTickChr,yMinor=0,yticks=n_elements(yTickChr)-1,$
            scale=[1,r],origin=!p.position[0:1]

fcolorbar, Divisions=4, $
            Color=0,VERTICAL=1,RIGHT=1,$
            TITLE='Digital Value',$
            CHARSIZE=2, format='(i)',$
            Position=[0.88,0.10,0.9,0.90]
stop
;b1 = IMAGE(spectralArray,x,y,                              $
;  rgb_Table=39,                                            $
;  Position=[0.05,0.10,0.85,0.90],                          $
;  axis_style=2,                                            $
;  title = plotTitle,font_Size=10,                          $
;  xTickName=xTickChr,xMinor=0,xTickFont_Size=8,            $
;  yTickName=yTickChr,yMinor=0,yTickFont_Size=8,            $
;  xtitle='Universal Time (UT)',                            $
;  ytitle='Frequency (MHz)')
;b1.aspect_Ratio = r
;
;c = COLORBAR(TARGET=b1, ORIENTATION=1,                     $
;   POSITION=[0.90,0.10,0.93,0.90],                         $
;   TITLE='Digital Value')
;; Change some properties
;c.TEXTPOS = 0
;c.TICKDIR = 1
;c.BORDER_ON = 1
;c.COLOR = 'Black'
;c.FONT_STYLE = 'Normal'
;c.FONT_SIZE = 8

endJob$:

end
