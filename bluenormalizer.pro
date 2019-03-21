;;------------------------------------------------------------------------------
;;
;;       Preferences
;;

;;+
;;Name:
;;      preferences
;;Purpose:
;;      Define system variables
;;Calling sequence:
;;      preferences
;;Positional parameters:
;;      None
;;Keyword parameters:
;;      None
;;Output:
;;      None
;;Author and history:
;;      Daniel Hatcher, 2018
;;Notes:
;;      For flat list, expect one date formatted YYYY-MM-DD and
;;      one 5-digit frame number per line
;;-

PRO preferences

COMPILE_OPT IDL2
ON_ERROR, 3

;;Directory containing flat list
flatListDir = '/home/central/hatch1dw/Programs/S3_Analysis/'

;;Flat list filename
flatListFile = 'flat_list_all.txt'

;;Directory containing flats
flatDir = '/storage/hatch1dw/Scattered-light-tests/120-pix-smooth/'

;;Define read-only system variables
DEFSYSV, '!FLATLIST', flatListDir + flatListFile, 1
DEFSYSV, '!FLATDIR', flatDir, 1

END

;;------------------------------------------------------------------------------
;;
;;      Locate flat
;;

;;+
;;Name:
;;      locateflat
;;Purpose:
;;      Locate flat frame number using date and flat list in preferences
;;Calling sequence:
;;      Result = locateflat(inputDate)
;;Positional parameters:
;;      inputDate   :   string formatted YYYY-MM-DD
;;Keyword parameters:
;;      None
;;Output:
;;      flat    :   string frame number of length 5
;;Author and history:
;;      Daniel Hatcher, 2018
;;Notes:
;;      Expect one date formatted YYYY-MM-DD and one 5-digit
;;      frame number per line
;;-

FUNCTION locateflat, inputDate

COMPILE_OPT IDL2
ON_ERROR, 3

;;Is input date-like?
IF NOT STREGEX(inputDate,'[0-9]{4}-[0-9]{2}-[0-9]{2}',/BOOLEAN) THEN BEGIN
    MESSAGE, 'Expected date format YYYY-MM-DD!'
ENDIF

;;Open flat list
OPENR, logicalUnitNumber, !FLATLIST, /GET_LUN
numLines = FILE_LINES(!FLATLIST)

;;Initializations
line = ''
locateError = 1
i = 0

WHILE locateError EQ 1 AND i LT numLines DO BEGIN
    ;;Read lines form flat list
    READF, logicalUnitNumber, line
    ;;Extract date
    flatDate = STREGEX(line,'[2][0][0-2][0-9]-[0-1][0-9]-[0-3][0-9]',/EXTRACT)
    IF STRCMP(flatDate,inputDate) EQ 1 THEN BEGIN
        ;;If date matches, extract frame number
        flatFrame = STREGEX(line,'[0-9]{5}',/EXTRACT)
        locateError = 0
    ENDIF
    ;;Iterate
    i++
ENDWHILE

IF locateError THEN BEGIN
    MESSAGE, 'Unable to match date '+inputDate+'!'
ENDIF

CLOSE, logicalUnitNumber
FREE_LUN, logicalUnitNumber

RETURN, flatFrame

END

;;------------------------------------------------------------------------------
;;
;;      Filter
;;

;;+
;;Name:
;;      filter
;;Purpose:
;;      1D boxcar smoothing with adaptive-width edge truncation
;;Calling sequence:
;;      Result = filter(inputArray,inputWidth,TYPE=string)
;;Positional parameters:
;;      inputArray   :   values to be smoothed, 1D
;;      inputWidth   :   width of boxcar
;;Keyword parameters:
;;      TYPE    :   type of averaging - either 'mean' or 'median', required
;;Output:
;;      output  :   1D smoothed array of same length as input
;;Author and history:
;;      Daniel Hatcher, 2018
;;-

FUNCTION filter, inputArray, inputWidth, TYPE=type

COMPILE_OPT IDL2
ON_ERROR, 3

;;Check input
arraySize = SIZE(inputArray)
IF arraySize[0] GT 1 THEN BEGIN
    MESSAGE, "Array must be 1D!"
ENDIF
IF NOT KEYWORD_SET(type) THEN BEGIN
    MESSAGE, 'Filter type not provided!'
ENDIF

num = arraySize[1]
output = FLTARR(num)

;;First and last elements will not be filtered, so just output them
output[0] = inputArray[0]
output[-1] = inputArray[-1]

FOR i = 1, num-2 DO BEGIN
    ;;Edge truncate left
    IF i LT inputWidth/2 THEN BEGIN
        filterWindow = LINDGEN(2*i)
    ;;Edge truncate right
    ENDIF ELSE IF i+inputWidth/2 GT num THEN BEGIN
        diff = num-i
        filterWindow = LINDGEN(2*diff,START=i-diff)
    ;;No truncation
    ENDIF ELSE BEGIN
        filterWindow = LINDGEN(inputWidth,START=i-inputWidth/2)
    ENDELSE
    type = STRLOWCASE(type)
    CASE type OF
        'median': output[i] = MEDIAN(inputArray[filterWindow])
        'mean': output[i] = MEAN(inputArray[filterWindow])
        ELSE: MESSAGE, 'Filter type '+type+' is not allowed!'
    ENDCASE
ENDFOR

RETURN, output

END

;;------------------------------------------------------------------------------
;;
;;      Read flat
;;

;;+
;;Name:
;;      readflat
;;Purpose:
;;      Read, smooth, and normalize flat
;;Calling sequence:
;;      Result = readflat(inputFrame)
;;Positional parameters:
;;      inputFrame  :   String of length 5
;;Keyword parameters:
;;      None
;;Output:
;;      output  :   structure with tags:
;;          flux        :   512xc raw flat data
;;          smoothflux  :   512xc smoothed flux
;;          meanNormFlux    :   512xc mean normalized flux
;;          meanNormSmoothFlux  :   512xc mean normalized, smoothed flux
;;              (c = 16 for new CCD, 20 for old CCD)
;;          width   :   width of smoothing window
;;Author and history:
;;      Daniel Hatcher, 2018
;;-

FUNCTION readflat, inputFrame

COMPILE_OPT IDL2
ON_ERROR, 3

;;Check input
IF NOT STREGEX(inputFrame,'[0-9]{5}',/BOOLEAN) THEN BEGIN
    MESSAGE, "Input must be string of 5 numerals!"
ENDIF

;;Specify path
extension = '.RAW.spec'
path = !FLATDIR + inputFrame + extension

;;Check flat file attributes
checkFile = FILE_INFO(path)

;;512 pixels, 32 bits (4 bytes), 16 orders
newSize = 512*4*16
;;512 pixels, 32 bits (4 bytes), 20 orders
oldSize = 512*4*20

;;Determine CCD era
;;Frame 22684 is first of new CCD frames
IF LONG(inputFrame) GE 22684 AND checkFile.size EQ newSize THEN BEGIN
    ;;New CCD
    numOrders = 16
    flatData = FLTARR(512,numOrders)
ENDIF ELSE IF LONG(inputFrame) LT 22864 AND checkFile.size EQ oldSize THEN BEGIN
    ;;Old CCD
    numOrders = 20
    flatData = FLTARR(512,numOrders)
ENDIF ELSE BEGIN
    MESSAGE, "Flat frame attributes are not consistent!"
ENDELSE

;;Read flat flux data
OPENR, logicalUnitNumber, path, /GET_LUN
READU, logicalUnitNumber, flatData
CLOSE, logicalUnitNumber
FREE_LUN, logicalUnitNumber

;;Initialize and allocate
smoothWidth = 10
smoothFlux = FLTARR(512,numOrders)
meanNormFlux = FLTARR(512,numOrders)
meanNormSmoothFlux = FLTARR(512,numOrders)

;;Normalization and smoothing
FOR i = 0, numOrders-1 DO BEGIN
    smoothFlux[*,i] = filter(flatData[*,i],smoothWidth,type='median')
    meanValue = MEAN(flatData[*,i])
    meanNormFlux[*,i] = flatData[*,i] / meanValue
    meanSmoothValue = MEAN(smoothFlux[*,i])
    meanNormSmoothFlux[*,i] = smoothFlux[*,i] / meanSmoothValue
ENDFOR

;;Output
output = {readflatOutput, $
    flux    :   flatData, $
    smoothflux  :   smoothFlux, $
    meanNormFlux    :   meanNormFlux, $
    meanNormSmoothFlux  :   meanNormSmoothFlux, $
    width   :   smoothWidth}

RETURN, output

END

;;------------------------------------------------------------------------------
;;
;;      Peak finder
;;

;;+
;;Name:
;;      zscorepeaks
;;Purpose:
;;      Robust thresholding algorithm
;;Calling sequence:
;;      Result = zscorepeaks(inputData,LAG=integer,T=float,INF=float
;;      [,SCREEN=binary])
;;Positional parameters:
;;      inputData   :   1D array with unknown peak positions
;;Keyword parameters:
;;      LAG     :   Size of moving average window
;;      T       :   Threshold z-score for signal
;;      INF     :   Influence of signal on mean and standard deviation
;;                  If 0, excluded from moving average
;;                  If 1, included
;;      SCREEN  :   Screen output flag, 1 is on
;;Output:
;;      output  :   structure with tags:
;;          signals :   array of signals, same length as input
;;          upper   :   array of threshold upper limit, same length as input
;;          lower   :   array of threshold lower limit, same length as input
;;          avg     :   array of moving average, same length as input
;;Author and history:
;;      Jean-Paul van Brakel, 2014  :   Algorithm construction
;;      https://stackoverflow.com/questions/22583391/
;;      peak-signal-detection-in-realtime-timeseries-data
;;      Daniel Hatcher, 2018    :   IDL implementation
;;-

FUNCTION zscorepeaks,inputData,LAG=inputLag,T=inputT,INF=inputInf, $
    SCREEN=inputScreen

COMPILE_OPT IDL2
ON_ERROR, 3

;;Check input
dataSize = SIZE(inputData)
IF dataSize[0] GT 1 THEN BEGIN
    MESSAGE, "Input must be 1D!"
ENDIF

num = dataSize[1]

IF inputLag LT num THEN BEGIN
    ;;Allocations
    signals = LONARR(num)
    avgFilter = FLTARR(num)
    stdFilter = FLTARR(num+1)

    ;;Not dealing with realtime data, so filtered array does not need to be
    ;;allocated dynamically.
    filtered = FLTARR(num)

    ;;Initializations
    filtered[0:inputLag] = inputData[0:inputLag]
    avgFilter[inputLag] = MEAN(inputData[0:inputLag])
    stdFilter[inputLag] = STDDEV(inputData[0:inputLag])

    ;;Exclude initial lagged values from signal array
    signals[0:inputLag-1] = -2

    ;;Signal calculation
    FOR i = inputLag+1, num-1 DO BEGIN
        diff = ABS(inputData[i]-avgFilter[i-1])
        IF diff GT inputT*stdFilter[i-1] THEN BEGIN
            IF inputData[i] GT avgFilter[i-1] THEN BEGIN
                ;;Positive signal
                signals[i] = 1
            ENDIF ELSE BEGIN
                ;;Negative signal
                signals[i] = -1
            ENDELSE
            ;;Change influence of signal
            filtered[i] = inputInf*inputData[i] + (1-inputInf)*filtered[i-1]
        ENDIF ELSE BEGIN
            ;;No signal
            signals[i] = 0
            filtered[i] = inputData[i]
        ENDELSE
        ;;Adjust filters
        avgFilter[i] = MEAN(filtered[i-inputLag:i])
        stdFilter[i] = STDDEV(filtered[i-inputLag:i])
    ENDFOR

    ;;Upper and lower thresholds
    upper = avgFilter+InputT*stdFilter
    lower = avgFilter-InputT*stdFilter

    ;;Use NaNs for cleaner plotting of lagged values
    upper[0:inputLag] = !VALUES.F_NAN
    lower[0:inputLag] = !VALUES.F_NAN

ENDIF ELSE BEGIN
    ;;If input is smaller than lag, find outliers without moving
    signals = LONARR(num)
    avgFilter = MEAN(inputData)
    stdFilter = STDDEV(inputData)
    upper = FLTARR(num)
    lower = FLTARR(num)

    ;;Positive outliers
    signals[WHERE(inputData GT avgFilter+stdFilter, /NULL)] = 1

    ;;Negative outliers
    signals[WHERE(inputData LT avgFilter-stdFilter, /NULL)] = -1

    upper[*] = avgFilter+InputT*stdFilter
    lower[*] = avgFilter-InputT*stdFilter
ENDELSE

;;Plotting
IF KEYWORD_SET(inputScreen) THEN BEGIN
    IF inputScreen EQ 1 THEN BEGIN
        !P.MULTI = [0,2,1,0,0]
        PLOT, inputData, title = 'Data', PSYM=3
        OPLOT, avgFilter
        OPLOT, upper
        OPLOT, lower
        PLOT, signals, title = 'Signals', yrange=[-1.5,1.5]
    ENDIF
ENDIF

output = {$
    signals :   signals, $
    upper  :   upper, $
    lower  :   lower, $
    avg :   avgFilter}

RETURN, output

END

;;------------------------------------------------------------------------------
;;
;;      Centroid finder
;;

;;+
;;Name:
;;      centroid
;;Purpose:
;;      Find centroid of feature.
;;      Also determines amount of continuum to include for later comparisons.
;;Calling sequence:
;;      Result = centroid(inputFlux)
;;Positional parameters:
;;      inputFlux   :   1D array of floats
;;Keyword parameters:
;;      None
;;Output:
;;      output  :   structure with tags:
;;          centroid    :   estimated center of Ha feature (float)
;;          left        :   pixels left of centroid (integer array)
;;          right       :   pixels right of centroid (integer array)
;;Author and history:
;;      Daniel Hatcher, 2019
;;-

FUNCTION centroid, inputFlux

COMPILE_OPT IDL2
ON_ERROR, 3

;;Check input
inputSize = SIZE(inputFlux)
IF inputSize[0] GT 1 THEN BEGIN
    MESSAGE, "Input must be 1D!"
ENDIF

;;Median smoothing
sFlux = filter(inputFlux,5,TYPE='median')

;;Find extreme values
minFlux = MIN(sFlux,minInd)
maxFlux = MAX(sFlux,maxInd)

;;How extreme?
minDist = ABS(1.0 - minFlux)
maxDist = ABS(1.0 - maxFlux)

;;Choose most extreme
IF maxDist GT minDist THEN BEGIN
    extreme = maxInd
ENDIF ELSE IF minDist GT maxDist THEN BEGIN
    extreme = minInd
ENDIF

;;Initialize stopping points and specify limit
limit = 0.45*ABS(sFlux[extreme]-1.0)
leftStop = 0
rightStop = N_ELEMENTS(sFlux)-1

;;Find first points beyond limit
WHILE ABS(sFlux[leftStop]-1.0) LE limit AND leftStop LT extreme DO BEGIN
    leftStop++
ENDWHILE
WHILE ABS(sFlux[rightStop]-1.0) LE limit AND rightStop GT extreme DO BEGIN
    rightStop--
ENDWHILE

;;Wing pixel locations
left = LINDGEN(leftStop)
right = LINDGEN(N_ELEMENTS(inputFlux)-rightStop,START=rightStop)

centroid = (rightStop - leftStop)/2 + leftStop

output = {$
    centroid    :   centroid, $
    left    :   left, $
    right   :   right}

RETURN, output

END

;;------------------------------------------------------------------------------
;;
;;      Continuum normalization
;;

;;+
;;Name:
;;      contnorm
;;Purpose:
;;      Continuum normalization 
;;Calling sequence:
;;      Result = contnorm(wave,flux,index[,FRAME=string][,WIDTH=integer]
;;      [,SCREEN=binary][,PASS1WIDTH=integer])
;;Positional parameters:
;;      wave    :   512x1 array of wavelengths
;;      flux    :   512x1 array of flat-divided flux values
;;      index   :   index of wave array nearest to lab wavelength
;;                  of region of interest
;;Keyword parameters:
;;      FRAME   :   string frame number (for plotting)
;;      WIDTH   :   Feature width, dynamic if not provided
;;      SCREEN  :   Screen output flag, 1 is on
;;      PASS1WIDTH  :   Width of first median boxcar (optional)
;;Output:
;;      output  :   structure with tags:
;;          continuum   :   512x1 estimate of the continuum
;;          spectrum    :   512x1 input flux divided by continuum
;;          width       :   integer width of the feature patch
;;          patch       :   array of patched pixels
;;          notpatch    :   array of pixels not part of the patch
;;Author and history:
;;      C.A.L. Bailer-Jones et al., 1998    :  Filtering algorithm
;;      Daniel Hatcher, 2018    :   IDL implementation
;;-

FUNCTION contnorm, wave, flux, index, FRAME=frame, WIDTH=inputWidth, $
    SCREEN=inputScreen, PASS1WIDTH=inputPass1

COMPILE_OPT IDL2
ON_ERROR, 3

;;Check inputs
waveSize = SIZE(wave)
fluxSize = SIZE(flux)

IF waveSize[0] NE 1 OR fluxSize[0] NE 1 THEN BEGIN
    MESSAGE, "Inputs must be 1D!"
ENDIF
IF waveSize[1] NE fluxSize[1] THEN BEGIN
    MESSAGE, "Input lengths must match!"
ENDIF

;;If width not specified, search
IF NOT KEYWORD_SET(inputWidth) THEN BEGIN
    ;;Search for left and right shoulder of feature
    ;;Maximum search distance beyond lab frame index
    searchWidth = 70

    ;;Buffer around core not to be searched
    coreBuffer = 10

    ;;Check if maximum search width exceeded
    IF (index-searchWidth) LT 0 OR (index+searchWidth) GT fluxSize[1] THEN BEGIN
        PRINT, "Shoulder search width is too large!"
        PRINT, "Using maximum search width!"
        searchWidth = MIN([index,fluxSize[1]-index])  
    ENDIF

    ;;Create search indices
    leftSearch = LINDGEN(searchWidth-coreBuffer,START=index-searchWidth)
    ;;Search starting from right end of spectrum (reversed)
    rightSearch = REVERSE(LINDGEN(searchWidth-coreBuffer, $
        START=index+coreBuffer))

    ;;Search blue (left) side
    leftResult = zscorepeaks(flux[leftSearch],LAG=5,T=1.5,INF=0.1)

    ;;Search red (right) side
    rightResult = zscorepeaks(flux[rightSearch],LAG=5,T=1.5,INF=0.1)

    ;;Find first position of last signal (blue side)
    FOR i = N_ELEMENTS(leftSearch)-1,0,-1 DO BEGIN
        lastValue = leftResult.signals[-1]
        IF leftResult.signals[i] NE lastValue THEN BEGIN
            leftShoulder = leftSearch[i]
            BREAK
        ENDIF
    ENDFOR

   ;;Find first position of last signal (red side)
    FOR i = N_ELEMENTS(rightSearch)-1,0,-1 DO BEGIN
        lastValue = rightResult.signals[-1]
        IF rightResult.signals[i] NE lastValue THEN BEGIN
            rightShoulder = rightSearch[i]
            BREAK
        ENDIF
    ENDFOR

    ;;Choose largest width
    width = MAX([(rightShoulder-index),(index-leftShoulder)])
ENDIF ELSE BEGIN
    ;;If specified, use input width
    width = inputWidth
ENDELSE

;;If width is very large, warn user
limit = 10
IF N_ELEMENTS(flux)-width LT limit THEN BEGIN
    limString = STRTRIM(STRING(limit),2)
    PRINT, "Less than "+limString+" pixels on red end of spectrum!"
ENDIF

;;Remove feature
totalPixels = N_ELEMENTS(flux)
leftPixels = LINDGEN(index-width)
rightPixels = LINDGEN(totalPixels-(index+width),START=index+width)
patchedPixels = LINDGEN((width*2)+1,START=index-width)

;;If 1st pass width provided
IF KEYWORD_SET(inputPass1) THEN BEGIN
    medianWidth = inputPass1
;;Default 1st pass width
ENDIF ELSE BEGIN
    medianWidth = 30
ENDELSE

;;Median filter left and right separately
medianLeft = filter(flux[leftPixels],medianWidth,TYPE='median')
medianRight = filter(flux[rightPixels],medianWidth,TYPE='median')

;;Reposition filtered arrays
separated = FLTARR(totalPixels)
separated[leftPixels] = medianLeft
separated[rightPixels] = medianRight

;;Pixels on left and right of red region
joinedPixels = [leftPixels,rightPixels]

;;Estimate of continuum flux on left and right of red region
medianFlux = [medianLeft,medianRight]

;;Allocate space for patch estimate
patchEstimate = FLTARR(N_ELEMENTS(patchedPixels))

;;Index of end points
nLeft = N_ELEMENTS(leftPixels)

;;Number of points to include on either side
numEndPoints = 10
endPoints = [LINDGEN(numEndPoints,START=nleft-1,INCREMENT=-1), $
    LINDGEN(numEndPoints,START=nLeft)]

;;Linear regression across red region
slope = REGRESS(joinedPixels[endPoints],medianFlux[endPoints],CONST=const)
patchEstimate = const + patchedPixels*slope[0]
separated[patchedPixels] = patchEstimate

;;Boxcar filter to smooth boundaries between regions
boxWidth = 25
boxFlux = filter(separated,boxWidth,TYPE='mean')

;;Final estimate of continuum
continuum = boxFlux

;;Division by final continuum estimate
spectrum = flux/continuum

;;Output to screen?
IF KEYWORD_SET(inputScreen) THEN BEGIN
    IF NOT KEYWORD_SET(frame) THEN BEGIN
        PRINT, 'Frame number not provided in call to contnorm().'
        frame = ''
    ENDIF
    IF inputScreen EQ 1 THEN BEGIN
        !P.MULTI = [0,1,2,0,0]

        ;;Colors
        DEVICE, DECOMPOSED=0
        LOADCT, 39, /SILENT

        titleText = frame+" Width: "+STRTRIM(STRING(width),2)
        PLOT, wave,flux,PSYM=3,title=titleText,xtitle='Wavelength (nm)', $
            ytitle='Flattened Flux',yrange=[MIN(continuum),MAX(continuum)]
        OPLOT, wave[leftPixels],continuum[leftPixels]
        OPLOT, wave[rightPixels],continuum[rightPixels]
        OPLOT, wave[patchedPixels],continuum[patchedPixels],LINESTYLE=2
        OPLOT, [wave[index-width],wave[index-width]],[MIN(flux),MAX(flux)]
        OPLOT, [wave[index+width],wave[index+width]],[MIN(flux),MAX(flux)]
        PLOT, wave,spectrum,xtitle='Wavelength (nm)', $
            ytitle='Normalized Flux',yrange=[0.9,1.1]
        OPLOT, wave[patchedPixels],spectrum[patchedPixels],COLOR=250
        OPLOT, [wave[0],wave[-1]],[0.99,0.99],LINESTYLE=1
        OPLOT, [wave[0],wave[-1]],[1.01,1.01],LINESTYLE=1
        OPLOT, [wave[0],wave[-1]],[0.98,0.98],LINESTYLE=2
        OPLOT, [wave[0],wave[-1]],[1.02,1.02],LINESTYLE=2
        OPLOT, [wave[0],wave[-1]],[0.97,0.97]
        OPLOT, [wave[0],wave[-1]],[1.03,1.03]
    ENDIF
ENDIF

;;Output structure
output = {$
    continuum   :   continuum, $
    spectrum    :   spectrum, $
    width   :   width, $
    patch   :   patchedPixels, $
    notpatch:   joinedPixels}

RETURN, output

END

;;------------------------------------------------------------------------------
;;
;;      Wing comparison
;;

;;+
;;Name:
;;      wingcompare
;;Purpose:
;;      Compare wings of the feature assuming a Gaussian wing profile
;;Calling sequence:
;;      wingcompare, flux,centroid,left,right,frame[,SCREEN=bianry]
;;Positional parameters:
;;      flux    :   continuum normalized flux
;;      centroid:   center as determined by centroid function (float)
;;      left    :   array of pixels left of wing
;;      right   :   array of pixels right of wing
;;      frame   :   string frame number (for plotting)
;;Keyword parameters:
;;      SCREEN  :   Screen output flag, 1 is on
;;Output:
;;      Postscript file
;;Author and history:
;;      Daniel Hatcher, 2018
;;-

PRO wingcompare, flux, centroid, left, right, frame, SCREEN=inputScreen

COMPILE_OPT IDL2
ON_ERROR, 3

leftDist = ABS(left-centroid)
rightDist = ABS(right-centroid)

;;Fit left and right gaussians
nterms = 4
fitL = GAUSSFIT(leftDist,flux[left],coeffL,NTERMS=nterms)
fitR = GAUSSFIT(rightDist,flux[right],coeffR,NTERMS=nterms)

;;Resample
rsLeft = FINDGEN(2*N_ELEMENTS(left),START=MIN([leftDist,rightDist]),$
    INCREMENT=0.5)
rsRight = rsLeft

;;Calculate fit at resampled points
rsLeftFit = FLTARR(N_ELEMENTS(rsLeft))
rsRightFit = FLTARR(N_ELEMENTS(rsRight))
FOR i = 0, N_ELEMENTS(rsLeft)-1 DO BEGIN
    zL = (rsLeft[i] - coeffL[1]) / coeffL[2]
    rsLeftFit[i] = coeffL[0]*exp((-zL^2)/2)+coeffL[3]
    zR = (rsRight[i] - coeffR[1]) / coeffR[2]
    rsRightFit[i] = coeffR[0]*exp((-zR^2)/2)+coeffR[3]
ENDFOR

;;Output to screen?
IF KEYWORD_SET(inputScreen) THEN BEGIN
    IF inputScreen EQ 1 THEN BEGIN
        ;;Colors
        DEVICE, DECOMPOSED=0
        LOADCT, 39, /SILENT

        !P.MULTI = [0,1,4,0,0]

        PLOT, left,flux[left],PSYM=3,title=frame,$
            yrange=[MIN(flux[[left,right]]),MAX(flux[[left,right]])], $
            xrange=[MIN(left),MAX(right)], $
            xtitle='Index',ytitle='Normalized flux'
        OPLOT, right,flux[right],PSYM=3, COLOR=250
        OPLOT, [centroid,centroid],[MIN(flux),MAX(flux)]

        PLOT, leftDist, flux[left], PSYM=3, $
            yrange=[MIN(flux[[left,right]]),MAX(flux[[left,right]])],$
            xtitle = 'Distance from centroid',ytitle='Normalized flux'
        OPLOT, rightDist, flux[right], PSYM=3, COLOR=250
        OPLOT, rsLeft, rsLeftFit
        OPLOT, rsRight, rsRightFit, COLOR = 250

        measure = (rsRightFit/rsLeftFit) - (coeffR[3]/coeffL[3])
        tot = TOTAL(ABS(measure))
        PLOT, rsLeft, measure, $
            title='Right Fit / Left Fit - Right Constant / Left Constant', $
                xtitle='Distance from centroid'
        OPLOT, [MIN(rsLeft),MAX(rsLeft)],[0.0,0.0],LINESTYLE=2

        currentTot = FLTARR(N_ELEMENTS(rsLeft))
        FOR i = 0, N_ELEMENTS(rsLeft)-1 DO BEGIN
            currentTot[i] = TOTAL(ABS(measure[0:i]))
        ENDFOR
        PLOT, rsLeft, currentTot, title='Cumulative sum', $
            xtitle='Distance from centroid'
        OPLOT, [MIN(rsLeft),MAX(rsLeft)], [tot,tot], LINESTYLE=2
        XYOUTS, 50,0.5*tot,STRTRIM(STRING(tot),2)
    ENDIF
ENDIF

END

;;------------------------------------------------------------------------------
;;
;;      Main
;;

;;+
;;Name:
;;      bluenormalizer
;;Purpose:
;;      Continuum normalization of SSS order 0 (H8 and He)
;;Calling sequence:
;;      Result = bluenormalizer(inputFlux,inputWave,inputDate,inputFrame,
;;      inputName[,WIDTH=integer or array][,NORMSCREEN=binary]
;;      [,DIAGSCREEN=binary][,OUTFILE=string][,FLATDIV=binary]
;;      [,FLATFRAME=string][,TEXTPATH=string])
;;Positional parameters:
;;      inputFlux   :   512x16 array of floats
;;      inputWave   :   512x16 array of floats
;;      inputDate   :   Date fromatted as YYYY-MM-DD
;;      inputFrame  :   string frame number of length 5 (for plotting)
;;      inputName   :   string object name (HD number)
;;Optional parameters (keywords):
;;      WIDTH       :   width of feature (integer or 2-element array)
;;      NORMSCREEN  :   binary output flag for normalization plots, 1 is on
;;      DIAGSCREEN  :   binary output flag for diagnostic plots, 1 is on
;;      OUTFILE     :   path to output file (PostScript)
;;      FLATDIV     :   binary flat division flag, 1 is on
;;      FLATFRAME   :   string frame number of length 5
;;      TEXTPATH    :   path to output text file
;;Output:
;;      output      :   continuum normalized spectrum (512 float array)
;;      Postscript file of normalization and diagnostic plots
;;      ASCII file of wavelengths and normalized flux values
;;Author and history:
;;      Daniel Hatcher, 2018
;;-

FUNCTION bluenormalizer,inputFlux,inputWave,inputDate,inputFrame,inputName,$
    WIDTH=inputWidth,NORMSCREEN=normScreen,DIAGSCREEN=diagScreen,$
    OUTFILE=outFile,FLATDIV=inputFlatDiv,FLATFRAME=inputFlatFrame,$
    TEXTPATH=textPath

COMPILE_OPT IDL2
ON_ERROR, 3

IF NOT STRCMP(!VERSION.RELEASE,'8.7.0') THEN BEGIN
    PRINT, 'Warning: ' + STRING(10B) + 'Written with IDL version 8.7.0' 
ENDIF

;;Define system variables
preferences

;;Zero indexed order
order = 0

;;Select order
flux = inputFlux[*,order]
wave = inputWave[*,order]

;;If no flat division requested, assume division was done in 2D
IF NOT KEYWORD_SET(inputFlatDiv) THEN BEGIN
    flatDiv = flux
ENDIF ELSE IF inputFlatDiv EQ 1 THEN BEGIN
    ;;If flat division requested, but no frame provided, locate flat from list
    IF NOT KEYWORD_SET(inputFlatFrame) THEN BEGIN
        flat = readflat(locateflat(inputDate))
    ENDIF ELSE BEGIN
        flat = readflat(inputFlatFrame)
    ENDELSE
    ;;Flat division
    flatDiv = flux / flat.meanNormFlux[*,order]
ENDIF

;;To deal with each feature separately, break spectrum in two
leftBreak = 180
rightBreak = 300

;;Include some middle region to avoid edge truncation at break points
pass1Width = 30
leftPixelsPlus = LINDGEN(leftBreak+pass1Width)
rightPixelsPlus = LINDGEN(512-(rightBreak-pass1Width),$
    START=rightBreak-pass1Width)
leftFlux = flatDiv[leftPixelsPlus]
rightFlux = flatDiv[rightPixelsPlus]
leftWave = wave[leftPixelsPlus]
rightWave = wave[rightPixelsPlus]

;;H8 lab frame wavelength
H8 = 388.81

;;Hepsilon lab frame wavelength
He = 396.91

;;Find index closest to wavelengths
distanceH8 = ABS(leftWave-H8)
minDistanceH8 = MIN(distanceH8,indexH8)
distanceHe = ABS(rightWave-He)
minDistanceHe = MIN(distanceHe,indexHe) 

;;Create output file if provided and turn on screens
IF KEYWORD_SET(outFile) THEN BEGIN
    SET_PLOT, 'ps'
    DEVICE, /COLOR, BITS_PER_PIXEL=8
    DEVICE, XSIZE=7, YSIZE=10, XOFFSET=0.5, YOFFSET=0.5, /INCHES
    DEVICE, FILENAME = outFile 
    normScreen = 1
    diagScreen = 1
ENDIF

;;Set keyword default values if not provided
IF NOT KEYWORD_SET(normScreen) THEN BEGIN
    normScreen = 0
ENDIF

;;Continuum normalizaton with provided width(s)
IF KEYWORD_SET(inputWidth) THEN BEGIN
    numWidths = N_ELEMENTS(inputWidth)
    ;;One width?
    IF numWidths EQ 1 THEN BEGIN
        normalizedLeft = contnorm(leftWave,leftFlux,indexH8,FRAME=inputFrame,$
            WIDTH=inputWidth,SCREEN=normScreen,PASS1WIDTH=pass1Width)
        normalizedRight = contnorm(rightWave,rightFlux,indexHe,$
            FRAME=inputFrame,WIDTH=inputWidth,SCREEN=normScreen,$
            PASS1WIDTH=pass1Width)
    ;;Two widths?
    ENDIF ELSE IF numWidths EQ 2 THEN BEGIN
        ;;Array element 0 is H8 width
        normalizedLeft = contnorm(leftWave,leftFlux,indexH8,FRAME=inputFrame,$
            WIDTH=inputWidth[0],SCREEN=normScreen,PASS1WIDTH=pass1Width)
        ;;Array element 1 is He width
        normalizedRight = contnorm(rightWave,rightFlux,indexHe,$
            FRAME=inputFrame,WIDTH=inputWidth[1],SCREEN=normScreen,$
            PASS1WIDTH=pass1Width)
    ENDIF ELSE BEGIN
        MESSAGE, "Width must be intger or 2-element array of integers!"
    ENDELSE
;;Continuum normalization with dynamic width
ENDIF ELSE BEGIN
    normalizedLeft = contnorm(leftWave,leftFlux,indexH8,FRAME=inputFrame,$
        SCREEN=normScreen,PASS1WIDTH=pass1Width)
    normalizedRight = contnorm(rightWave,rightFlux,indexHe,FRAME=inputFrame,$
        SCREEN=normScreen,PASS1WIDTH=pass1Width)
ENDELSE

;;Define regions used to subset contnorm output
leftPixels = LINDGEN(leftBreak)
middlePixels = LINDGEN(rightBreak-leftBreak,START=leftBreak)
rightPixels = LINDGEN(512-rightBreak,START=pass1Width)

;;If diagnostic plots requested
IF KEYWORD_SET(diagScreen) THEN BEGIN
    cLeft = centroid(normalizedLeft.spectrum[leftPixels])
    cRight = centroid(normalizedRight.spectrum[rightPixels])
    wingcompare,normalizedLeft.spectrum[leftPixels],cLeft.centroid,cLeft.left,$
        cLeft.right,inputFrame+' H8',SCREEN=1
    wingcompare,normalizedRight.spectrum[rightPixels],cRight.centroid,$
        cRight.left,cRight.right,inputFrame+' He',SCREEN=1
ENDIF

;;Close output file
IF KEYWORD_SET(outFile) THEN BEGIN
    DEVICE, /CLOSE_FILE
ENDIF

;;Interpolate across middle region
middleSlope = REGRESS([leftPixelsPlus[leftBreak],rightPixelsPlus[pass1Width]],$
    [normalizedLeft.continuum[-pass1Width],$
    normalizedRight.continuum[pass1Width]],CONST=const)
middleCont = middlePixels*middleSlope[0] + const
middleSpectrum = flatDiv[middlePixels]/middleCont

output = [normalizedLeft.spectrum[leftPixels],middleSpectrum,$
    normalizedRight.spectrum[rightPixels]]

;;Was a text file path given?
IF NOT KEYWORD_SET(textPath) THEN BEGIN
    ;;Does default output directory exist?
    SPAWN, "pwd", pwd
    defaultDir = pwd + "/" + "BLUENORMALIZER_OUTPUT/"
    IF NOT FILE_TEST(defaultDir,/DIRECTORY) THEN BEGIN
        SPAWN, "mkdir " + defaultDir
    ENDIF
    ;;Default text file path
    textPath = defaultDir+"normalized"+inputFrame.Compress()+".txt"
    ;;Print text file location
    PRINT, "Text files in: " + defaultDir
ENDIF ELSE BEGIN
    textPath = textPath+"normalized"+inputFrame.Compress()+".txt"
ENDELSE

;;Specify text file header
header = $
    "% " + inputName + STRING(10B) + $
    ;;6 spaces between 'UT' and '='
    "% UT      = " + inputDate + STRING(10B)  + $
    "%" + STRING(10B) + $
    "%"

;;Open text file
OPENW, logicalUnitNumber, textPath, /GET_LUN

;;Write to text file
PRINTF, logicalUnitNumber, header
FOR i = 0,N_ELEMENTS(wave)-1 DO BEGIN
    PRINTF, logicalUnitNumber, wave[i], output[i],FORMAT="(F11.6,F11.6)"
ENDFOR

;;Close text file
CLOSE, logicalUnitNumber
FREE_LUN, logicalUnitNumber

RETURN, output

END
