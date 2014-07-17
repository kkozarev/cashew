pro test_rstn_plot_spectrum_date
date='20110606'
datapath='/data/george/corwav/RSTN_data/'

dlpath=datapath;+'radio/'
rstn_plot_spectrum_date,date,dlpath=dlpath
end


pro rstn_plot_spectrum_date,date,repair=repair,full=full,frange=frange,trange=trange,datarange=datarange,dlpath=dlpath
;
;--------------------------------------------------------------------------------;+
; NAME:
;     rstn_plot_spectrum    
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
;Search for RSTN data for this event
path='./'
if keyword_set(dlpath) then path=dlpath
files=file_basename(file_search(path+'*'+strmid(date,2,6)+'.srs',/fold_case))

if files[0] eq '' then begin
   print,'No files found. Quitting...'
   return
endif
inputFileName=path+files[0]
If strLen(inputFileName) EQ 0 Then goTo, endJob$
Print,' SRS File: ',inputFileName
tmp=strupcase(strmid(file_basename(inputFileName),0,2))
case tmp of
   'PA': iStationID='PHFF'
   'KP': iStationID='PHFF'
   'LM': iStationID='APLM'
   'HO': iStationID='KHMN'
   'K7': iStationID='K70L'
   'SV': iStationID='LISS'
   else: iStationID='XXXX'
endcase
if strLen(iStationID) EQ 0 Then goTo, endjob$
iStationID = strUpCase(iStationID)

Openr,u_dpd,inputFileName,/get_lun,/swap_endian
;little_endian = (BYTE(1, 0, 1))[0]
inputArray=read_binary(inputFileName)
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
   iStationID NE 'K70L' AND                                $
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

;Generate a string array to hold all times
tmp=strtrim(string(iYrArray,format='(i2)'),2)
ind=where(tmp lt 10)
if ind[0] ne -1 then tmp[ind]='0'+tmp[ind]
stringTime='20'+tmp
tmp=strtrim(string(iMnArray,format='(i2)'),2)
ind=where(tmp lt 10)
if ind[0] ne -1 then tmp[ind]='0'+tmp[ind]
stringTime+='/'+tmp
tmp=strtrim(string(iDyArray,format='(i2)'),2)
ind=where(tmp lt 10)
if ind[0] ne -1 then tmp[ind]='0'+tmp[ind]
stringTime+='/'+tmp
tmp=strtrim(string(iHhArray,format='(i2)'),2)
ind=where(tmp lt 10)
if ind[0] ne -1 then tmp[ind]='0'+tmp[ind]
stringTime+=' '+tmp
tmp=strtrim(string(iMmArray,format='(i2)'),2)
ind=where(tmp lt 10)
if ind[0] ne -1 then tmp[ind]='0'+tmp[ind]
stringTime+=':'+tmp
tmp=strtrim(string(iSsArray,format='(i2)'),2)
ind=where(tmp lt 10)
if ind[0] ne -1 then tmp[ind]='0'+tmp[ind]
stringTime+=':'+tmp

;Find the indices of the starting and ending times consistent with the
;event times
sectimes=anytim(stringTime)
startTime=stringTime[0]
startTimeError=0
if keyword_set(trange) then startTime=trange[0]
StartTimeIndex=min(where(anytim(startTime)-sectimes le 0.0))
if StartTimeIndex eq -1 then begin
   print,'Event starting time outside data range. Reverting to data starting time...'
   startTimeError=1
   StartTimeIndex=0
endif

endTime=stringTime[nTimeElements-1]
endTimeError=0
if keyword_set(trange) then endTime=trange[1]
EndTimeIndex=max(where(anytim(endTime)-sectimes ge 0.0))
if EndTimeIndex eq -1 then begin
   print,'Event ending time outside data range. Reverting to data ending time...'
   endTimeError=1
   EndTimeIndex=nTimeElements-1
endif

if startTimeError eq 1 and endTimeError eq 1 then begin
   print,''
   print,'Time-range error.'
   print,'User-supplied time range: '+startTime + ' - ' + endTime
   print,'Data time range: '+ stringTime[0] + ' - ' + stringTime[nTimeElements-1]
   print,''
   return
endif
   
stringTime=stringTime[StartTimeIndex:EndTimeIndex]

for i = StartTimeIndex+1,EndTimeIndex do Begin                         ;Look for datagaps GT 15 seconds
  if contiguousTime[i] GT contiguousTime[i-1]+15 then Begin
    iAnswer = ''
    print,''
    print,'TimeStep: ',i-1,' =',contiguousTime[i-1],'    TimeStep: ',i,' =',contiguousTime[i] 
   ; read,prompt='*****File is non-contiguous. Repair? (Y/N): ',iAnswer
   ; if keyword_set(repair) then iAnswer='Y' else iAnswer='N'
    iAnswer='Y'
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
iIndex = [StartTimeIndex,EndTimeIndex]
if keyword_set(full) then iIndex = [0,nTimeElements-1]
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
nFreqElements=n_elements(f_MHz)

StartFreqIndex = 0
EndFreqIndex = n_elements(f_MHz)-1

freqIndexRange=[StartFreqIndex,EndFreqIndex]
if keyword_set(frange) then begin
   StartFreqIndex=min(where(frange[0]-f_MHz le 0.0))
   if StartFreqIndex eq -1 then StartFreqIndex = 0
   EndFreqIndex=max(where(frange[1]-f_MHz ge 0.0))
   if EndFreqIndex eq -1 then EndFreqIndex = n_elements(f_MHz)-1
   freqIndexRange=[StartFreqIndex,EndFreqIndex]
endif
if keyword_set(full) then freqIndexRange=[0,n_elements(f_MHz)-1]

;
spectralArray = inputArray[*,24:*]                         ;Clip out the spectral part
if keyword_set(full) then begin
   nTimeElements_=nTimeElements
   nFreqElements_=nFreqElements
endif else begin
   nTimeElements_=EndTimeIndex-StartTimeIndex+1
   nFreqElements_=EndFreqIndex-StartFreqIndex+1
endelse
x = uindGen(nTimeElements_)
y = uindGen(nFreqElements_)
r = 1.*nTimeElements_/nFreqElements_

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
  iKntr = i*fix(nTimeElements_/5.)
  
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
  xTickChr[i] = strmid(stringTime[iKntr],11,8)
endfor

yTickChr = ['25','35','45','55','65','75','96','117','138','159','180']
for i = 0,10 do Begin
   iKntr = freqIndexRange[0]+i*fix((freqIndexRange[1]-freqIndexRange[0])/10.)
   yTickChr[i]=string(f_MHz[iKntr],format='(i)')
endfor
;
;stop
wdef,0,1200,800
loadct,39,/silent
!p.position=[0.01,0.12,0.9,0.90]
!p.background=255
!p.color=0
tvlct,rr,gg,bb,/get
drange=[0,255]
if keyword_set(datarange) then begin
   drange[0]=datarange[0]
   if (drange[0] lt 0) or (drange[0] gt datarange[1]) then drange[0]=0
   drange[1]=datarange[1]
   if (drange[1] gt 255) or (drange[1] lt 0)  then drange[1]=255
endif

plot_image, spectralArray[iIndex[0]:iIndex[1],freqIndexRange[0]:freqIndexRange[1]],$
            title=plotTitle,charsize=2,charthick=2,$
            xtitle='Universal Time (UT)',$
            ytitle='Frequency (MHz)',xstyle=1,ystyle=1,$
            xTickName=xTickChr,xMinor=0,xticks=n_elements(xTickChr)-1,$
            yTickName=yTickChr,yMinor=0,yticks=n_elements(yTickChr)-1,$
            scale=[1,r],origin=!p.position[0:1],$
            min=drange[0],max=drange[1]

fcolorbar, Divisions=4, $
           Color=0,VERTICAL=1,RIGHT=1,$
           TITLE='Digital Value',$
           CHARSIZE=2, format='(i)',$
           Position=[0.89,0.10,0.91,0.90],$
           min=drange[0],max=drange[1]

fname=path+'rstn_spectrum_'+date+'.png'
write_png,fname,tvrd(/true),rr,gg,bb


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
