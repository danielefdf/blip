/* Rexx */

/* debug tools
    trace(?l)
    trace(?i)
    trace(?a)
    signal on novalue
    signal on error
    signal on syntax
    signal on halt
*/

main:
    call housekeeping
    command = ''
    mainIterCounter = 0
    do while (SYS_TRUE)
        mainIterCounter = mainIterCounter + 1
        call checkMainIterCounter
        call checkProfileData
        select
        when (command = 'e') then call editConsole
        when (command = 'c') then call viewCopy
        when (command = 'd') then call viewData
        when (command = 'x') then call exitBlip
        otherwise
            nop
        end
        say
        say 'blip> enter command:'
        say '    e: edit console'
        say '    c: view copy'
        say '    d: view data...'
        say '    x: exit blip'
        parse pull command
    end
    exit 1

/*
 *
 * housekeeping
 *
 */

housekeeping:
    call checkOs
    call setConstants
    call initSelectFeatures
    call checkWorkDirectories
    return

checkOs:
    parse source os .
    if (os <> 'WIN64' ,
            & os <> 'TSO')
    then do
        say 'os ['os'] not managed  :-('
        call exitError
    end
    return

setConstants:
    TRUE  = 'TRUE'
    FALSE = 'FALSE'
    SYS_TRUE  = 1
    SYS_FALSE = 0
    /* user profile data */
    select
    when (os = 'WIN64')
    then do
        BLIP_DIRECTORY = directory()
        PRO_FILE = BLIP_DIRECTORY'\profile.txt'
    end
    when (os = 'TSO')
    then do
        BLIP_DIRECTORY = userid()'.BLIP'
        PRO_FILE = BLIP_DIRECTORY'.PROFILE'
        PRO_FILE_Q = "'" || BLIP_DIRECTORY'.PROFILE' || "'"
    end
    end
    /* data file */
    EMPTY_DATA_RECORD = '00'x
    /* comp-3 */
    DATA_CHUNK_LENGTH = 50
    /* from/to cols */
    RESTART_LOG_LEVEL = 99
    /* select records */
    FIELD_LABEL_PAD = 45
    /* overflow indicator */
    OVERFLOW_STRING = '...'
    /* select columns */
    ALL_FIELDS_LABEL = 'ALL FIELDS'
    /* ebcdic > ascii */
    call loadEbcdicToAscii
    /* loop safety counters */
    MAX_MAIN_ITER_COUNTER = 10
    MAX_PRO_FILE_ITER_COUNTER = 10
    /* output record formatting */
    OUT_REC_INITIAL_SPACE = ' '
    EMPTY_OUT_REC = ''
    return

initSelectFeatures:
    call initSelRecsFeatures
    call initSelColsFeatures
    selRecsFileWritten = FALSE
    selColsFileWritten = FALSE
    call setAllSelColsVisible
    return

checkWorkDirectories:
    select
    when (os = 'WIN64') then call checkWorkDirectoriesWIN
    when (os = 'TSO')   then call checkWorkDirectoriesTSO
    end
    return

checkWorkDirectoriesWIN:
    selRecsDirectory = BLIP_DIRECTORY'\selRecs'
    selColsDirectory = BLIP_DIRECTORY'\selCols'
    return

checkWorkDirectoriesTSO:
    selRecsDirectory = BLIP_DIRECTORY'.SELRECS'
    selColsDirectory = BLIP_DIRECTORY'.SELCOLS'
    return

/*
 *
 * checkMainIterCounter
 *
 */

checkMainIterCounter:
    if (mainIterCounter > MAX_MAIN_ITER_COUNTER)
    then do
        say 'Max number of usages per session reached.'
        say 'Please restart.'
        call exitBlip
    end
    return

/*
 *
 * checkProfileData
 *
 */

checkProfileData:
    call checkProfile
    proFileIterCounter = 0
    do while (proFileStatus = 'ko')
        proFileIterCounter = proFileIterCounter + 1
        call checkProFileIterCounter
        call startPRO_FILE
        call checkProfile
    end
    return

checkProfile:
    call closeProFile
    call openReadProFile
    if (openReadProFileStatus <> 'ok')
    then do
        call initProfile
        call openReadProFile
    end
    call getProfileOptions
    call getCOPY_NAME
    call setFiles
    call checkCurrentFiles
    call setProFileStatus
    return

initProfile:
    DATA_FILE         = '?'
    MAX_LRECL         = 500
    DATA_ENCODING     = '?'
    DATA_DSORG        = '?'
    EOL_TYPE          = '?'
    COPY_FILE         = '?'
    MAX_DATA_RECORDS  = 99
    MAX_ALPHA_LENGTH  = 99
    RESTART_LOG_LEVEL = 99
    call openWriteProFile
    proRecord = '*'
    call writeProRecord
    proRecord = '* file data'
    call writeProRecord
    proRecord = '*     dataset'
    call writeProRecord
    proRecord = '          'DATA_FILE
    call writeProRecord
    proRecord = '*     max LRECL               -- TSO OS only'
    call writeProRecord
    proRecord = '          'MAX_LRECL
    call writeProRecord
    proRecord = '*     encoding: ebcdic/ascii  -- WIN OS only'
    call writeProRecord
    proRecord = '          'DATA_ENCODING
    call writeProRecord
    proRecord = '*     organisation: lseq/seq  -- ascii enc. only'
    call writeProRecord
    proRecord = '          'DATA_DSORG
    call writeProRecord
    proRecord = '*     eol: CR/LF/CRLF         -- lseq org. only'
    call writeProRecord
    proRecord = '          'EOL_TYPE
    call writeProRecord
    proRecord = '*'
    call writeProRecord
    proRecord = '* file copy'
    call writeProRecord
    proRecord = '*     dataset'
    call writeProRecord
    proRecord = '          'COPY_FILE
    call writeProRecord
    proRecord = '*'
    call writeProRecord
    proRecord = '* options'
    call writeProRecord
    proRecord = '*     max number of records'
    call writeProRecord
    proRecord = '          'MAX_DATA_RECORDS
    call writeProRecord
    proRecord = '*     max length for alphanumeric fields'
    call writeProRecord
    proRecord = '          'MAX_ALPHA_LENGTH
    call writeProRecord
    proRecord = '*     restart record cursor for level'
    call writeProRecord
    proRecord = '          'RESTART_LOG_LEVEL
    call writeProRecord
    proRecord = '*'
    call writeProRecord
    proRecord = '*'
    call writeProRecord
    proRecord = '*'
    call writeProRecord
    call closeProFile
    return

getProfileOptions:
    proFileParmCursor = 0
    do proFileRecsCursor = 1 to proFileRecsCounter
        call readProRecord
        if (substr(proRecord, 1, 1) <> '*')
        then do
            proFileParmCursor = proFileParmCursor + 1
            select
            when (proFileParmCursor = 1)
            then DATA_FILE          = strip(proRecord)
            when (proFileParmCursor = 2)
            then MAX_LRECL          = strip(proRecord)
            when (proFileParmCursor = 3)
            then DATA_ENCODING      = strip(proRecord)
            when (proFileParmCursor = 4)
            then DATA_DSORG         = strip(proRecord)
            when (proFileParmCursor = 5)
            then EOL_TYPE           = strip(proRecord)
            when (proFileParmCursor = 6)
            then COPY_FILE          = strip(proRecord)
            when (proFileParmCursor = 7)
            then MAX_DATA_RECORDS   = strip(proRecord)
            when (proFileParmCursor = 8)
            then MAX_ALPHA_LENGTH   = strip(proRecord)
            when (proFileParmCursor = 9)
            then RESTART_LOG_LEVEL  = strip(proRecord)
            end
        end
    end
    if (os = 'TSO')
    then do
        DATA_DSORG = 'lseq'
        say 'DATA_DSORG automatically set to LSEQ in TSO environment'
    end
    return

getCOPY_NAME:
    select
    when (os = 'WIN64') then call getCOPY_NAME_WIN
    when (os = 'TSO')   then call getCOPY_NAME_TSO
    end
    return

getCOPY_NAME_WIN:
    copyFileCut = COPY_FILE
    do dirElemsCounter = 1 by 1 ,
            while (index(copyFileCut, '\') > 0)
        parse var copyFileCut dirElems.dirElemsCounter '\' ,
                copyFileCut
    end
    COPY_NAME = copyFileCut
    return

getCOPY_NAME_TSO:
    select
    when (index(COPY_FILE, '(') > 0)
    then do
        parse var COPY_FILE . '(' COPY_NAME ')'
    end
    when (index(COPY_FILE, '.') > 0)
    then do
        copyFileCut = COPY_FILE
        do dirElemsCounter = 1 by 1 ,
                while (index(copyFileCut, '.') > 0)
            parse var copyFileCut dirElems.dirElemsCounter '.' ,
                    copyFileCut
        end
        COPY_NAME = copyFileCut
    end
    otherwise
        COPY_NAME = COPY_FILE
    end
    return

setFiles:
    select
    when (os = 'WIN64') then call setFilesWIN
    when (os = 'TSO')   then call setFilesTSO
    end
    return

setFilesWIN:
    selRecsFile = selRecsDirectory'\'COPY_NAME
    selColsFile = selColsDirectory'\'COPY_NAME
    outCopyFile = BLIP_DIRECTORY'\outCopyFile.txt'
    outDataFile = BLIP_DIRECTORY'\outDataFile.txt'
    return

setFilesTSO:
    selRecsFile = "'" || selRecsDirectory'.'COPY_NAME || "'"
    selColsFile = "'" || selColsDirectory'.'COPY_NAME || "'"
    outCopyFile = "'" || BLIP_DIRECTORY'.OUTCOPYF'    || "'"
    outDataFile = "'" || BLIP_DIRECTORY'.OUTDATAF'    || "'"
    return

checkCurrentFiles:
    if (DATA_FILE <> '?' & DATA_FILE <> '')
    then do
        call openReadDataFile
    end
    if (COPY_FILE <> '?' & COPY_FILE <> '')
    then do
        call openReadCopyFile
    end
    call openWriteOutDataFile
    call openWriteOutCopyFile
    if (COPY_FILE <> '?' & COPY_FILE <> '')
    then do
        call openReadSelRecsFile
        call openReadSelColsFile
    end
    return

setProFileStatus:
    proFileStatus = 'ok'
    select
    when (DATA_FILE = '?' | DATA_FILE = '')
    then do
        say
        say 'blip> checkProfile> data file: missing'
        proFileStatus = 'ko'
    end
    when (datatype(MAX_LRECL) <> 'NUM')
    then do
        say
        say 'blip> checkProfile> max LRECL: wrong value'
        proFileStatus = 'ko'
    end
    when (DATA_ENCODING <> 'ascii' & DATA_ENCODING <> 'ebcdic')
    then do
        say
        say 'blip> checkProfile> data file encoding: wrong value'
        say 'blip> checkProfile> DATA_ENCODING='DATA_ENCODING
        proFileStatus = 'ko'
    end
    when (DATA_DSORG <> 'seq' & DATA_DSORG <> 'lseq')
    then do
        say
        say 'blip> checkProfile> data file organization: wrong value'
        proFileStatus = 'ko'
    end
    when (EOL_TYPE <> 'CR' & EOL_TYPE <> 'LF' & EOL_TYPE <> 'CRLF')
    then do
        say
        say 'blip> checkProfile> eol: wrong value'
        proFileStatus = 'ko'
    end
    when (COPY_FILE = '?' | COPY_FILE = '')
    then do
        say
        say 'blip> checkProfile> copy file: missing'
        proFileStatus = 'ko'
    end
    when (datatype(MAX_DATA_RECORDS) <> 'NUM')
    then do
        say
        say 'blip> checkProfile> max records: wrong value'
        proFileStatus = 'ko'
    end
    when (datatype(MAX_ALPHA_LENGTH) <> 'NUM')
    then do
        say
        say 'blip> checkProfile> max length for alpha: wrong value'
        proFileStatus = 'ko'
    end
    when (datatype(RESTART_LOG_LEVEL) <> 'NUM' ,
            | ((RESTART_LOG_LEVEL <= 0 ,
                | RESTART_LOG_LEVEL >= 50)) ,
                & RESTART_LOG_LEVEL <> 99)
    then do
        say
        say 'blip> checkProfile> restart from level: wrong value'
        proFileStatus = 'ko'
    end
    otherwise
        nop
    end
    return

checkProFileIterCounter:
    if (proFileIterCounter > MAX_PRO_FILE_ITER_COUNTER)
    then do
        say 'Max number of profile edits per session reached.'
        say 'Please restart.'
        call exitBlip
    end
    return

/*
 *
 * editConsole
 *
 */

editConsole:
    call startPRO_FILE
    return

/*
 *
 * viewCopy
 *
 */

viewCopy:
    call getFieldData
    call showCopy
    return

/*
 * getFieldData
 */

getFieldData:
    call setNormsList
    call setMonosList
    call setFieldsList
    call setOccursFields
    call setEbcdicFromToCols
    /*dg
    call displayFieldsList
    */
    return

/*
 * setNormsList
 */

setNormsList:
    norms. = ''
    normsCounter = 0
    literalQuote = ''
    do copyRecsIndex = 1 to copyRecsCounter
        call readCopyRecord
        copyRec7thChar = substr(copyRecord, 7, 1)
        copyRecBody    = substr(copyRecord, 8, 65)
        select
        when (copyRec7thChar = '*' ,
                | copyRec7thChar = '/')
        then do
            copyRec7thChar88 = 'commented'
        end
        when (copyRec7thChar = '-' ,
                | copyRec7thChar = '')
        then do
            copyRec7thChar88 = 'instructns'
        end
        otherwise
            say 'setNormsList'
            say 'otherwise'
            say 'copyRec7thChar ['copyRec7thChar']'
            call exitError
        end
        if (copyRecBody <> '' ,
                & copyRec7thChar88 = 'instructns' )
        then do
            call clearLiterals
            call addNormsTabRec
        end
    end
    return

clearLiterals:
    do copyCharsIndex = 1 to length(copyRecBody)
        copyChar = substr(copyRecBody, copyCharsIndex, 1)
        if (literalQuote = '')
        then do
            if (copyChar = '"' ,
                    | copyChar = "'")
            then do
                literalQuote = copyChar
            end
        end
        else do
            if (copyChar = literalQuote)
            then do
                literalQuote = ''
            end
            else do
                copyRecBody = overlay('?', copyRecBody, copyCharsIndex)
            end
        end
    end
    return

addNormsTabRec:
    normsCounter = normsCounter + 1
    norms.normsCounter = copyRecBody' '
    return

/*
 * setMonosList
 */

setMonosList:
    monos. = ''
    monosCounter = 0
    mono = ''
    normsIndex = 1
    do normsIndex = 1 to normsCounter
        norm = norms.normsIndex
        dotsCounter = index(norm, '.')
        call normalizeNorm
        mono = mono || norm' '
        if (dotsCounter > 0)
        then do
            call addMonoRec
            mono = ''
        end
    end
    return

normalizeNorm:
    norm = replaceText(norm, '.', ' ')
    norm = translate(norm, 'abcdefghijklmnopqrstuvwxyz', ,
                           'ABCDEFGHIJKLMNOPQRSTUVWXYZ')
    call clearOptionals
    call replaceSynonyms
    return

clearOptionals:
    norm = replaceText(norm, ' usage ', ' ')
    norm = replaceText(norm, ' is ',    ' ')
    return

replaceSynonyms:
    norm = replaceText(norm, ' picture ',         ' pic ')
    norm = replaceText(norm, ' binary ',          ' comp ')
    norm = replaceText(norm, ' computational ',   ' comp ')
    norm = replaceText(norm, ' packed-decimal ',  ' comp-3 ')
    norm = replaceText(norm, ' computational-3 ', ' comp-3 ')
    return

addMonoRec:
    monosCounter = monosCounter + 1
    monos.monosCounter = mono
    return

/*
 * setFieldsList
 */

setFieldsList:
    call initFields
    fieldsCounter = 0
    fieldsMaxLabelSubs = 0
    previousLogLevel = 0
    log2DisplayLevels. = ''
    fieldDisplayLevel = 0
    redefsDegreesFound = 0
    currentRedefsDegree = 0
    redefsDegreeLogLevels. = ''
    do monosIndex = 1 to monosCounter
        bodyRec = monos.monosIndex
        call getBodyWords
        call addDefsTabRec
    end
    return

getBodyWords:
    bodyWords.= ''
    bodyWordsCounter = 0
    bodyWordsIndex = 0
    bodyRecCut = bodyRec
    do bodyWordsIndex = 1 by 1 ,
            while bodyRecCut <> ''
        parse var bodyRecCut bodyWords.bodyWordsIndex ' ' bodyRecCut
        bodyWordLengths.bodyWordsIndex = ,
                length(bodyWords.bodyWordsIndex)
    end
    bodyWordsCounter = bodyWordsIndex - 1
    return

addDefsTabRec:
    call initializeField
    bodyWordsIndex = 1
    call getFieldLevel
    if (fieldLevel = 88)
    then do
        return
    end
    call getFieldLogLevel
    call getFieldDisplayLevel
    call getFieldLabel
    call getFieldLabelLen
    call getFieldLabelSubs
    call getNextBodyWord
    picWordSw = 'notfound'
    do while (bodyWordsIndex <= bodyWordsCounter)
        if (bodyWord = 'pic')
        then do
            picWordSw = 'found'
        end
        select
        when (bodyWord = 'pic')       then call setElemFieldfeatures
        when (bodyWord = 'occurs')    then call getFieldOccursNum
        when (bodyWord = 'redefines') then call setFieldRedefsFeatures
        otherwise
            nop
        end
        call getNextBodyWord
    end
    if (picWordSw = 'notfound')
    then do
        call setGroupFieldfeatures
    end
    call setFieldRedefsDegree
    fieldsCounter = fieldsCounter + 1
    call storeField fieldsCounter
    return

initializeField:
    fieldLevel            = 0
    fieldLogLevel         = 0
/*no
    fieldDisplayLevel     = 0
*/
    fieldLabel            = ''
    fieldLabelLen         = 0
    fieldLabelSubs.       = ''
    fieldLabelSubLengths. = 0
    fieldLabelSubsCounter = 0
    fieldPicString        = ''
    fieldPicChars.        = ''
    fieldPicReps.         = 0
    fieldPicCharsCounter  = 0
    fieldPicType          = ''
    /* alphanum */
    /* integer  */
    /* decimal  */
    /* group    */
    fieldPicIntsNum       = 0
    fieldPicDecsNum       = 0
    fieldPicSign          = ''
    /* signed   */
    /* unsigned */
    fieldPicCompType      = ''
    /* zoned */
    /* comp  */
    /* comp3 */
    fieldRedefdLabel      = ''
    fieldRedefsDegree     = 0
    fieldOccursNum        = 0
    fieldOccursDisplay    = 0
    fieldValueLength      = 0
    fieldColumnLength     = 0
    fieldTabColOverSw     = ''
    /* TRUE  */
    /* FALSE */
    fieldEbcdicLength     = 0
    fieldEbcdicFromCol    = 0
    fieldEbcdicToCol      = 0
    return

getFieldLevel:
    call getNextBodyWord
    if (datatype(bodyWord) <> 'NUM')
    then do
        say 'getFieldLevel'
        say 'datatype(bodyWord) <> "NUM"'
        say 'datatype(bodyWord) ['datatype(bodyWord)']'
        say 'bodyWord ['bodyWord']'
        say 'bodyRec ['bodyRec']'
        call exitError
    end
    fieldLevel = substr(bodyWord, 1, bodyWordLength)
    fieldLevel = copies('0', 2-length(fieldLevel)) || fieldLevel
    if (fieldLevel = 01 ,
            | fieldLevel = 77 ,
            | fieldLevel = 88 ,
            | (fieldLevel > 0 & fieldLevel <= 50))
    then do
        nop
    end
    /*when (fieldLevel = 66)*/
    else do
        say 'getFieldLevel'
        say 'else'
        say 'fieldLevel ['fieldLevel']'
        say 'bodyRec ['bodyRec']'
        call exitError
    end
    return

getNextBodyWord:
    bodyWord = ''
    do bodyWordsIndex = bodyWordsIndex to bodyWordsCounter ,
            while (bodyWord = '')
        bodyWord       = bodyWords.bodyWordsIndex
        bodyWordLength = bodyWordLengths.bodyWordsIndex
    end
    return

getFieldLogLevel:
    if (fieldLevel = 77)
    then do
        fieldLogLevel = 01
    end
    else do
        fieldLogLevel = fieldLevel
    end
    return

getFieldDisplayLevel:
    select
    when (fieldLogLevel = 1)
    then do
        fieldDisplayLevel = 1
        log2DisplayLevels.fieldLogLevel = fieldDisplayLevel
    end
    when (fieldLogLevel = previousLogLevel)
    then do
        fieldDisplayLevel = log2DisplayLevels.fieldLogLevel
    end
    when (fieldLogLevel < previousLogLevel)
    then do
        logLevelSearch = fieldLogLevel - 1
        do while (logLevelSearch >= 1)
            logLevelSearch = copies('0', 2-length(logLevelSearch)) ,
                    || logLevelSearch
            if (log2DisplayLevels.logLevelSearch <> '')
            then do
                leave
            end
            else do
                logLevelSearch = logLevelSearch - 1
            end
        end
        logLevelSearch = copies('0', 2-length(logLevelSearch)) ,
                || logLevelSearch
        fieldDisplayLevel = log2DisplayLevels.logLevelSearch + 1
        log2DisplayLevels.fieldLogLevel = fieldDisplayLevel
    end
    when (fieldLogLevel > previousLogLevel)
    then do
        fieldDisplayLevel = fieldDisplayLevel + 1
        log2DisplayLevels.fieldLogLevel = fieldDisplayLevel
    end
    end
    previousLogLevel = fieldLogLevel
    return

getFieldLabel:
    call getNextBodyWord
    fieldLabel = bodyWord
    return

getFieldLabelLen:
    fieldLabelLen = bodyWordLength
    return

getFieldLabelSubs:
    fieldLabelSubs = ''
    fieldLabelSubLengths = 0
    fieldLabelSubsCounter = 0
    fieldLabelCut = fieldLabel
    do fieldLabelSubsIndex = 1 by 1 ,
            while (fieldLabelCut <> '')
        parse var fieldLabelCut ,
                fieldLabelSubs.fieldLabelSubsIndex '-' fieldLabelCut
        fieldLabelSubLengths.fieldLabelSubsIndex = ,
                length(fieldLabelSubs.fieldLabelSubsIndex)
    end
    fieldLabelSubsCounter = fieldLabelSubsIndex - 1
    if (fieldsMaxLabelSubs < fieldLabelSubsCounter)
    then do
        fieldsMaxLabelSubs = fieldLabelSubsCounter
    end
    return

setElemFieldfeatures:
    call getPicString
    call getFieldPicElems
    call getFieldPicType
    call getFieldPicIntsNum
    if (fieldPicType = 'integer' ,
            | fieldPicType = 'decimal')
    then do
        if (fieldPicType = 'decimal')
        then do
            call getFieldPicDecsNum
        end
        call getFieldPicSign
        call getFieldPicCompType
    end
    if (fieldPicType <> 'group')
    then do
        call getFieldValueLength
        call getFieldColumnLength
        call getFieldEbcdicLength
    end
    return

getPicString:
    call getNextBodyWord
    fieldPicString = bodyWord
    return

getFieldPicElems:
    fieldPicCharsCounter = 0
    fieldPicChars.       = ''
    fieldPicReps.        = 0
    fieldPicChar    = '?'
    fieldPicCharNew = '?'
    fieldPicRep     = 0
    do picStringCursor = 1 to length(fieldPicString) ,
            while (fieldPicCharNew <> '')
        select
        when (fieldPicCharNew = '?')
        then do
            nop
        end
        when (fieldPicCharNew = 'x' ,
                | fieldPicCharNew = '9' ,
                | fieldPicCharNew = 's' ,
                | fieldPicCharNew = 'v')
        then do
            select
            when (fieldPicChar = '?')
            then do
                fieldPicChar = fieldPicCharNew
                fieldPicRep = 1
            end
            when (fieldPicChar = 'x' ,
                    | fieldPicChar = '9' ,
                    | fieldPicChar = 's' ,
                    | fieldPicChar = 'v')
            then do
                if (fieldPicChar = fieldPicCharNew)
                then do
                    fieldPicRep = fieldPicRep + 1
                end
                else do
                    call addFieldPicElem
                    fieldPicChar = fieldPicCharNew
                    fieldPicRep = 1
                end
            end
            otherwise
            end
        end
        when (fieldPicCharNew = '(')
        then do
            call addPicRepsByFactor
        end
        otherwise
            say 'getFieldPicElems'
            say 'otherwise'
            say 'fieldPicCharNew ['fieldPicCharNew']'
            say 'bodyRec ['bodyRec']'
            call exitError
        end
        fieldPicCharNew = substr(fieldPicString, picStringCursor, 1)
    end
    select
    when (fieldPicChar = '?')
    then do
        fieldPicChar = fieldPicCharNew
        fieldPicRep = 1
    end
    when (fieldPicChar = 'x' ,
            | fieldPicChar = '9' ,
            | fieldPicChar = 's' ,
            | fieldPicChar = 'v')
    then do
        if (fieldPicChar = fieldPicCharNew)
        then do
            fieldPicRep = fieldPicRep + 1
        end
        else do
            call addFieldPicElem
            fieldPicChar = fieldPicCharNew
            fieldPicRep = 1
        end
    end
    otherwise
        say 'getFieldPicElems'
        say 'otherwise'
        say 'fieldPicChar ['fieldPicChar']'
        say 'bodyRec ['bodyRec']'
        call exitError
    end
    if (fieldPicChar <> '')
    then do
        call addFieldPicElem
    end
    return

addFieldPicElem:
    fieldPicCharsCounter = fieldPicCharsCounter + 1
    fieldPicChars.fieldPicCharsCounter = fieldPicChar
    fieldPicReps.fieldPicCharsCounter  = fieldPicRep
    return

addPicRepsByFactor:
    bracketSw88 = 'opened'
    picRepsFigures = ''
    do picStringCursor = picStringCursor to length(fieldPicString) ,
            while (bracketSw88 = 'opened')
        fieldPicChar2 = substr(fieldPicString, picStringCursor, 1)
        if (fieldPicChar2 = ')')
        then do
            bracketSw88 = 'closed'
        end
        else do
            picRepsFigures = picRepsFigures || fieldPicChar2
        end
    end
    if (bracketSw88 = 'opened')
    then do
        say 'addPicRepsByFactor'
        say 'bracketSw88 = "opened"'
        say 'bodyRec ['bodyRec']'
        call exitError
    end
    fieldPicRep = fieldPicRep + picRepsFigures - 1
    return

getFieldPicType:
    fieldPicType = 'integer'
    do fieldPicCharsIndex = 1 to fieldPicCharsCounter ,
            while (fieldPicType = 'integer')
        fieldPicChar = fieldPicChars.fieldPicCharsIndex
        select
        when (fieldPicChar = 'x') then fieldPicType = 'alphanum'
        when (fieldPicChar = '9') then fieldPicType = 'integer'
        when (fieldPicChar = 's') then nop
        when (fieldPicChar = 'v') then fieldPicType = 'decimal'
        otherwise
            say 'getFieldPicType'
            say 'otherwise'
            say 'fieldPicChar ['fieldPicChar']'
            say 'bodyRec ['bodyRec']'
            call exitError
        end
    end
    return

getFieldPicIntsNum:
    do fieldPicCharsIndex = 1 to fieldPicCharsCounter ,
            while (fieldPicChars.fieldPicCharsIndex <> 'v')
        fieldPicChar = fieldPicChars.fieldPicCharsIndex
        fieldPicRep  = fieldPicReps.fieldPicCharsIndex
        select
        when (fieldPicChar = 's')
        then do
            nop
        end
        when (fieldPicChar = '9' ,
                | fieldPicChar = 'x')
        then do
            fieldPicIntsNum = fieldPicRep
        end
     /* when 'v' */
        otherwise
            say 'getFieldPicIntsNum'
            say 'otherwise'
            say 'fieldPicChar ['fieldPicChar']'
            say 'bodyRec ['bodyRec']'
            call exitError
        end
    end
    return

getFieldPicDecsNum:
    do fieldPicCharsIndex = fieldPicCharsIndex + 1 ,
            to fieldPicCharsCounter
        fieldPicChar = fieldPicChars.fieldPicCharsIndex
        fieldPicRep  = fieldPicReps.fieldPicCharsIndex
        select
        when (fieldPicChar = '9')
        then do
            fieldPicDecsNum = fieldPicRep
        end
     /* when 's' */
     /* when 'v' */
        otherwise
            say 'getFieldPicDecsNum'
            say 'otherwise'
            say 'fieldPicChar ['fieldPicChar']'
            say 'bodyRec ['bodyRec']'
            call exitError
        end
    end
    return

getFieldPicSign:
    fieldPicSign = 'unsigned'
    do fieldPicCharsIndex = 1 to fieldPicCharsCounter ,
            while (fieldPicSign = 'unsigned')
        fieldPicChar = fieldPicChars.fieldPicCharsIndex
        fieldPicRep  = fieldPicReps.fieldPicCharsIndex
        select
        when (fieldPicChar = 's')
        then do
            fieldPicSign = 'signed'
        end
        when (fieldPicChar = 'x' ,
                | fieldPicChar = '9' ,
                | fieldPicChar = 'v')
        then do
            nop
        end
        otherwise
            say 'getFieldPicSign'
            say 'otherwise'
            say 'fieldPicChar ['fieldPicChar']'
            say 'bodyRec ['bodyRec']'
            call exitError
        end
    end
    return

getFieldPicCompType:
    fieldPicCompType = 'zoned'
    if (index(bodyRec, ' comp ') > 0)
    then do
        fieldPicCompType = 'comp'
    end
    if (index(bodyRec, ' comp-3 ') > 0)
    then do
        fieldPicCompType = 'comp-3'
    end
    return

getFieldValueLength:
    select
    when (fieldPicType = 'alphanum' ,
            | fieldPicType = 'integer')
    then do
        fieldValueLength = fieldPicIntsNum
    end
    when (fieldPicType = 'decimal')
    then do
        fieldValueLength = fieldPicIntsNum + fieldPicDecsNum ,
                + 1  /* comma */
    end
    end
    if (fieldPicSign = 'signed')
    then do
        fieldValueLength = fieldValueLength + 1
    end
    return

getFieldColumnLength:
    fieldColumnLength = fieldValueLength
    if (fieldPicType = 'alphanum')
    then do
        if (fieldColumnLength > MAX_ALPHA_LENGTH)
        then do
            fieldColumnLength = MAX_ALPHA_LENGTH
            fieldTabColOverSw = TRUE
            fieldColumnLength = fieldColumnLength ,
                    + length(OVERFLOW_STRING)
        end
    end
    do fieldLabelSubsIndex = 1 to fieldLabelSubsCounter
        if (fieldLabelSubLengths.fieldLabelSubsIndex ,
                > fieldColumnLength)
        then do
            fieldColumnLength ,
                    = fieldLabelSubLengths.fieldLabelSubsIndex
        end
    end
    return

getFieldEbcdicLength:
    select
    when (fieldPicType = 'alphanum')
    then do
        fieldEbcdicLength = fieldPicIntsNum
    end
    when (fieldPicType = 'integer')
    then do
        fieldEbcdicLength = fieldPicIntsNum
        call checkFieldEbcdicCompLength
    end
    when (fieldPicType = 'decimal')
    then do
        fieldEbcdicLength = fieldPicIntsNum + fieldPicDecsNum
        call checkFieldEbcdicCompLength
    end
    end
    return

checkFieldEbcdicCompLength:
    /* comp managing */
    select
    when (fieldPicCompType = 'zoned')
    then do
        nop
    end
    when (fieldPicCompType = 'comp-3')
    then do
        fieldEbcdicLength = trunc(fieldEbcdicLength/2 + 1)
    end
    when (fieldPicCompType = 'comp')
    then do
        select
        when (fieldEbcdicLength >= 1  & fieldEbcdicLength <= 4)
        then do
             fieldEbcdicLength = 2
        end
        when (fieldEbcdicLength >= 5  & fieldEbcdicLength <= 9)
        then do
             fieldEbcdicLength = 4
        end
        when (fieldEbcdicLength >= 10 & fieldEbcdicLength <= 18)
        then do
             fieldEbcdicLength = 8
        end
        end
    end
    end
    return

setFieldRedefsFeatures:
    call getFieldRedefdLabel
    return

getFieldRedefdLabel:
    call getNextBodyWord
    fieldRedefdLabel = bodyWord
    return

getFieldOccursNum:
    call getNextBodyWord
    if (datatype(bodyWord) <> 'NUM')
    then do
        say 'getFieldOccursNum'
        say 'datatype(bodyWord) <> "NUM"'
        say 'bodyRec ['bodyRec']'
        call exitError
    end
    fieldOccursNum = bodyWord
    return

/*
 * setOccursFields
 */

setOccursFields:
    call setOccursFields1
    do while (occursSwitch = 'found')
        call reproFields2ToFields
        call setOccursFields1
    end
    call reproFields2ToFields
    return

setOccursFields1:
    call initOccursFields
    occursFieldsCounter = 0
    occursSwitch = 'notfound'
    do fieldsIndex = 1 to fieldsCounter
        call retrieveField fieldsIndex
        if (fieldOccursNum = 0)
        then do
            occursFieldsCounter = occursFieldsCounter + 1
            call storeOccursField occursFieldsCounter
        end
        else do
            occursSwitch = 'found'
            occursFieldIndex     = fieldsIndex
            occursFieldLabel     = fieldLabel
            occursFieldLogLevel  = fieldLevel
            occursFieldOccursNum = fieldOccursNum
            do occursFieldOccursIndex = 1 to occursFieldOccursNum
                call reproOccursFields
                fieldsIndex = fieldsIndex - 1
            end
        end
    end
    return

reproOccursFields:
    occursListSwitch = 'start'
    do fieldsIndex = occursFieldIndex to fieldsCounter ,
            while (occursListSwitch = 'start')
        call retrieveField fieldsIndex
        if (fieldsIndex = occursFieldIndex)
        then do
            if (occursFieldOccursIndex = 1)
            then do
                fieldOccursDisplay = fieldOccursNum
            end
            fieldOccursNum = 0
            call reproOccursField
        end
        else do
            if (fieldLogLevel <= occursFieldLogLevel)
            then do
                occursListSwitch = 'end'
            end
            else do
                call reproOccursField
            end
        end
    end
    if (fieldsIndex < fieldsCounter)
    then do
        fieldsIndex = fieldsIndex - 1
    end
    return

reproOccursField:
    fieldLabel = fieldLabel'_'occursFieldOccursIndex
    occursFieldsCounter = occursFieldsCounter + 1
    call storeOccursField occursFieldsCounter
    return

reproFields2ToFields:
    call initFields
    fieldsCounter = 0
    do occursFieldsIndex = 1 to occursFieldsCounter
        call retrieveOccursField occursFieldsIndex
        fieldsCounter = fieldsCounter + 1
        call storeField fieldsCounter
    end
    return

setGroupFieldfeatures:
    fieldPicType = 'group'
    return

setFieldRedefsDegree:
    if (currentRedefsDegree > 0 ,
            & fieldLogLevel <= ,
                    redefsDegreeLogLevels.currentRedefsDegree)
    then do
        call matchRedefsDegree
    end
    if (fieldRedefdLabel = '')
    then do
        fieldRedefsDegree = currentRedefsDegree
    end
    else do
        fieldRedefsDegree = currentRedefsDegree + 1
        call setRedefsDegreesFound
        select
        when (fieldPicType = 'alphanum' ,
                | fieldPicType = 'integer' ,
                | fieldPicType = 'decimal')
        then do
            nop
        end
        when (fieldPicType = 'group')
        then do
            currentRedefsDegree = currentRedefsDegree + 1
            redefsDegreeLogLevels.currentRedefsDegree = fieldLogLevel
        end
        end
    end
    return

matchRedefsDegree:
    do while (currentRedefsDegree > 0)
        if (fieldLogLevel <= redefsDegreeLogLevels.currentRedefsDegree)
        then do
            redefsDegreeLogLevels.currentRedefsDegree = ''
            currentRedefsDegree = currentRedefsDegree - 1
        end
        else do
            leave
        end
    end
    return

setRedefsDegreesFound:
    if (fieldRedefsDegree > redefsDegreesFound)
    then do
        redefsDegreesFound = fieldRedefsDegree
    end
    return

/*
 * setEbcdicFromToCols
 */

setEbcdicFromToCols:
    columnsCursor = 1
    do redefsDegreesIndex = 0 to redefsDegreesFound
        call setEbcdicFromToColsDegree
    end
    return

setEbcdicFromToColsDegree:
    do fieldsIndex = 1 to fieldsCounter
        call retrieveField fieldsIndex
        if (fieldRedefsDegree = redefsDegreesIndex)
        then do
            call checkEbcdicFromCol
            call setEbcdicFromToColsField
            call storeField fieldsIndex
        end
    end
    return

checkEbcdicFromCol:
    if (RESTART_LOG_LEVEL <> 99 ,
            & fieldLogLevel = RESTART_LOG_LEVEL)
    then do
        columnsCursor = 1
    end
    else do
        if (fieldRedefdLabel <> '')
        then do
            call searchRedefdEbcdicFromCol
            columnsCursor = redefdEbcdicFromCol
        end
    end
    return

searchRedefdEbcdicFromCol:
    redefdFound = FALSE
    redefdIndex = fieldsIndex - 1
    do redefdIndex = redefdIndex by -1 ,
            while (redefdFound = FALSE)
        call retrieveRedefdField redefdIndex
        if (redefdLogLevel = fieldLogLevel ,
                & redefdRedefdLabel = '')
        then do
            redefdFound = TRUE
            fieldEbcdicFromCol = redefdEbcdicFromCol
        end
    end
    return

retrieveRedefdField:
        parse arg ref
    redefdLogLevel      = _field._fieldLogLevel.ref
    redefdRedefdLabel   = _field._fieldRedefdLabel.ref
    redefdEbcdicFromCol = _field._fieldEbcdicFromCol.ref
    return

setEbcdicFromToColsField:
    fieldEbcdicFromCol = columnsCursor
    select
    when (fieldPicType = 'alphanum' ,
            | fieldPicType = 'integer' ,
            | fieldPicType = 'decimal')
    then do
        fieldEbcdicToCol = columnsCursor + fieldEbcdicLength - 1
        columnsCursor = fieldEbcdicToCol + 1
    end
    when (fieldPicType = 'group')
    then do
        call setEbcdicFromToColsGroup
    end
    end
    return

setEbcdicFromToColsGroup:
    subfieldLogLevel = 99
    fieldEbcdicToCol = columnsCursor
    do subfieldsIndex = fieldsIndex + 1 to fieldsCounter ,
            while (subfieldLogLevel > fieldLogLevel)
        call retrieveSubfield subfieldsIndex
        if (subfieldLogLevel > fieldLogLevel ,
                & subfieldRedefsDegrees = redefsDegreesIndex)
        then do
            call checkRestartCursor
            call addSubfieldLengths
        end
    end
    fieldEbcdicToCol = fieldEbcdicToCol - 1
    fieldEbcdicLength = fieldEbcdicToCol - fieldEbcdicFromCol + 1
    return

retrieveSubfield:
        parse arg ref
    subfieldLogLevel      = _field._fieldLogLevel.ref
    subfieldPicType       = _field._fieldPicType.ref
    subfieldRedefsDegrees = _field._fieldRedefsDegree.ref
    subfieldEbcdicLength  = _field._fieldEbcdicLength.ref
    return

checkRestartCursor:
    if (RESTART_LOG_LEVEL <> 99 ,
            & subfieldLogLevel = RESTART_LOG_LEVEL)
    then do
         columnsCursor = 1
    end
    return

addSubfieldLengths:
    select
    when (subfieldPicType = 'alphanum' ,
            | subfieldPicType = 'integer' ,
            | subfieldPicType = 'decimal')
    then do
        fieldEbcdicToCol = fieldEbcdicToCol + subfieldEbcdicLength
    end
    when (subfieldPicType = 'group')
    then do
        nop
    end
    end
    return

initFields:
    _field._fieldLevel.            = 0
    _field._fieldLogLevel.         = 0
    _field._fieldDisplayLevel.     = 0
    _field._fieldLabel.            = ''
    _field._fieldLabelLen.         = 0
    _field._fieldLabelSubsCounter. = 0
    _field._fieldLabelSubs.        = ''
    _field._fieldLabelSubLengths.  = 0
    _field._fieldPicString.        = ''
    _field._fieldPicCharsCounter.  = 0
    _field._fieldPicChars.         = ''
    _field._fieldPicReps.          = 0
    _field._fieldPicType.          = ''
    _field._fieldPicIntsNum.       = 0
    _field._fieldPicDecsNum.       = 0
    _field._fieldPicSign.          = ''
    _field._fieldPicCompType.      = ''
    _field._fieldRedefdLabel.      = ''
    _field._fieldRedefsDegree.     = 0
    _field._fieldOccursNum.        = 0
    _field._fieldOccursDisplay.    = 0
    _field._fieldTabColLen.        = 0
    _field._fieldTabColFrom.       = 0
    _field._fieldTabColOverSw.     = ''
    _field._fieldEbcdicLength.     = 0
    _field._fieldEbcdicFromCol.    = 0
    _field._fieldEbcdicToCol.      = 0
    return

storeField:
        parse arg ref
    _field._fieldLevel.ref            = fieldLevel
    _field._fieldLogLevel.ref         = fieldLogLevel
    _field._fieldDisplayLevel.ref     = fieldDisplayLevel
    _field._fieldLabel.ref            = fieldLabel
    _field._fieldLabelLen.ref         = fieldLabelLen
    _field._fieldLabelSubsCounter.ref = fieldLabelSubsCounter
    _field._fieldLabelSubs.ref.       = ''
    _field._fieldLabelSubLengths.ref. = 0
    do _fieldIndex = 1 ,
            to _field._fieldLabelSubsCounter.ref
        _field._fieldLabelSubs.ref._fieldIndex       = ,
                fieldLabelSubs._fieldIndex
        _field._fieldLabelSubLengths.ref._fieldIndex = ,
                fieldLabelSubLengths._fieldIndex
    end
    _field._fieldPicString.ref        = fieldPicString
    _field._fieldPicCharsCounter.ref  = fieldPicCharsCounter
    _field._fieldPicChars.ref.        = ''
    _field._fieldPicReps.ref.         = 0
    do _fieldIndex = 1 ,
            to _field._fieldPicCharsCounter.ref
        _field._fieldPicChars.ref._fieldIndex = ,
                fieldPicChars._fieldIndex
        _field._fieldPicReps.ref._fieldIndex  = ,
                fieldPicReps._fieldIndex
    end
    _field._fieldPicType.ref          = fieldPicType
    _field._fieldPicIntsNum.ref       = fieldPicIntsNum
    _field._fieldPicDecsNum.ref       = fieldPicDecsNum
    _field._fieldPicSign.ref          = fieldPicSign
    _field._fieldPicCompType.ref      = fieldPicCompType
    _field._fieldRedefdLabel.ref      = fieldRedefdLabel
    _field._fieldRedefsDegree.ref     = fieldRedefsDegree
    _field._fieldOccursNum.ref        = fieldOccursNum
    _field._fieldOccursDisplay.ref    = fieldOccursDisplay
    _field._fieldValueLength.ref      = fieldValueLength
    _field._fieldColumnLength.ref     = fieldColumnLength
    _field._fieldTabColOverSw.ref     = fieldTabColOverSw
    _field._fieldEbcdicLength.ref     = fieldEbcdicLength
    _field._fieldEbcdicFromCol.ref    = fieldEbcdicFromCol
    _field._fieldEbcdicToCol.ref      = fieldEbcdicToCol
    return

retrieveField:
        parse arg ref
    fieldLevel            = _field._fieldLevel.ref
    fieldLogLevel         = _field._fieldLogLevel.ref
    fieldDisplayLevel     = _field._fieldDisplayLevel.ref
    fieldLabel            = _field._fieldLabel.ref
    fieldLabelLen         = _field._fieldLabelLen.ref
    fieldLabelSubsCounter = _field._fieldLabelSubsCounter.ref
    fieldLabelSubs.       = ''
    fieldLabelSubLengths. = 0
    do _fieldIndex = 1 ,
            to fieldLabelSubsCounter
        fieldLabelSubs._fieldIndex       = ,
                _field._fieldLabelSubs.ref._fieldIndex
        fieldLabelSubLengths._fieldIndex = ,
                _field._fieldLabelSubLengths.ref._fieldIndex
    end
    fieldPicString        = _field._fieldPicString.ref
    fieldPicCharsCounter  = _field._fieldPicCharsCounter.ref
    fieldPicChars.        = ''
    fieldPicReps.         = 0
    do _fieldIndex = 1 ,
            to fieldPicCharsCounter
        fieldPicChars._fieldIndex = ,
                _field._fieldPicChars.ref._fieldIndex
        fieldPicReps._fieldIndex  = ,
                _field._fieldPicReps.ref._fieldIndex
    end
    fieldPicType          = _field._fieldPicType.ref
    fieldPicIntsNum       = _field._fieldPicIntsNum.ref
    fieldPicDecsNum       = _field._fieldPicDecsNum.ref
    fieldPicSign          = _field._fieldPicSign.ref
    fieldPicCompType      = _field._fieldPicCompType.ref
    fieldRedefdLabel      = _field._fieldRedefdLabel.ref
    fieldRedefsDegree     = _field._fieldRedefsDegree.ref
    fieldOccursNum        = _field._fieldOccursNum.ref
    fieldOccursDisplay    = _field._fieldOccursDisplay.ref
    fieldValueLength      = _field._fieldValueLength.ref
    fieldColumnLength     = _field._fieldColumnLength.ref
    fieldTabColOverSw     = _field._fieldTabColOverSw.ref
    fieldEbcdicLength     = _field._fieldEbcdicLength.ref
    fieldEbcdicFromCol    = _field._fieldEbcdicFromCol.ref
    fieldEbcdicToCol      = _field._fieldEbcdicToCol.ref
    return

initOccursFields:
    _oField._fieldLevel.            = 0
    _oField._fieldLogLevel.         = 0
    _oField._fieldDisplayLevel.     = 0
    _oField._fieldLabel.            = ''
    _oField._fieldLabelLen.         = 0
    _oField._fieldLabelSubsCounter. = 0
    _oField._fieldLabelSubs.        = ''
    _oField._fieldLabelSubLengths.  = 0
    _oField._fieldPicString.        = ''
    _oField._fieldPicCharsCounter.  = 0
    _oField._fieldPicChars.         = ''
    _oField._fieldPicReps.          = 0
    _oField._fieldPicType.          = ''
    _oField._fieldPicIntsNum.       = 0
    _oField._fieldPicDecsNum.       = 0
    _oField._fieldPicSign.          = ''
    _oField._fieldPicCompType.      = ''
    _oField._fieldRedefdLabel.      = ''
    _oField._fieldOccursNum.        = 0
    _oField._fieldOccursDisplay.    = 0
    _oField._fieldValueLength.      = 0
    _oField._fieldColumnLength.     = 0
    _oField._fieldColumnFrom.       = 0
    _oField._fieldTabColOverSw.     = ''
    return

storeOccursField:
        parse arg ref
    _oField._fieldLevel.ref            = fieldLevel
    _oField._fieldLogLevel.ref         = fieldLogLevel
    _oField._fieldDisplayLevel.ref     = fieldDisplayLevel
    _oField._fieldLabel.ref            = fieldLabel
    _oField._fieldLabelLen.ref         = fieldLabelLen
    _oField._fieldLabelSubsCounter.ref = fieldLabelSubsCounter
    _oField._fieldLabelSubs.ref.       = ''
    _oField._fieldLabelSubLengths.ref. = 0
    do _fieldIndex = 1 ,
            to _oField._fieldLabelSubsCounter.ref
        _oField._fieldLabelSubs.ref._fieldIndex       = ,
                fieldLabelSubs._fieldIndex
        _oField._fieldLabelSubLengths.ref._fieldIndex = ,
                fieldLabelSubLengths._fieldIndex
    end
    _oField._fieldPicString.ref        = fieldPicString
    _oField._fieldPicCharsCounter.ref  = fieldPicCharsCounter
    _oField._fieldPicChars.ref.        = ''
    _oField._fieldPicReps.ref.         = 0
    do _fieldIndex = 1 ,
            to _oField._fieldPicCharsCounter.ref
        _oField._fieldPicChars.ref._fieldIndex = ,
                fieldPicChars._fieldIndex
        _oField._fieldPicReps.ref._fieldIndex  = ,
                fieldPicReps._fieldIndex
    end
    _oField._fieldPicType.ref          = fieldPicType
    _oField._fieldPicIntsNum.ref       = fieldPicIntsNum
    _oField._fieldPicDecsNum.ref       = fieldPicDecsNum
    _oField._fieldPicSign.ref          = fieldPicSign
    _oField._fieldPicCompType.ref      = fieldPicCompType
    _oField._fieldRedefdLabel.ref      = fieldRedefdLabel
    _oField._fieldRedefsDegree.ref     = fieldRedefsDegree
    _oField._fieldOccursNum.ref        = fieldOccursNum
    _oField._fieldOccursDisplay.ref    = fieldOccursDisplay
    _oField._fieldValueLength.ref      = fieldValueLength
    _oField._fieldColumnLength.ref     = fieldColumnLength
    _oField._fieldTabColOverSw.ref     = fieldTabColOverSw
    _oField._fieldEbcdicLength.ref     = fieldEbcdicLength
    _oField._fieldEbcdicFromCol.ref    = fieldEbcdicFromCol
    _oField._fieldEbcdicToCol.ref      = fieldEbcdicToCol
    return

retrieveOccursField:
        parse arg ref
    fieldLevel            = _oField._fieldLevel.ref
    fieldLogLevel         = _oField._fieldLogLevel.ref
    fieldDisplayLevel     = _oField._fieldDisplayLevel.ref
    fieldLabel            = _oField._fieldLabel.ref
    fieldLabelLen         = _oField._fieldLabelLen.ref
    fieldLabelSubsCounter = _oField._fieldLabelSubsCounter.ref
    fieldLabelSubs.       = ''
    fieldLabelSubLengths. = 0
    do _fieldIndex = 1 ,
            to fieldLabelSubsCounter
        fieldLabelSubs._fieldIndex       = ,
                _oField._fieldLabelSubs.ref._fieldIndex
        fieldLabelSubLengths._fieldIndex = ,
                _oField._fieldLabelSubLengths.ref._fieldIndex
    end
    fieldPicString        = _oField._fieldPicString.ref
    fieldPicCharsCounter  = _oField._fieldPicCharsCounter.ref
    fieldPicChars.        = ''
    fieldPicReps.         = 0
    do _fieldIndex = 1 ,
            to fieldPicCharsCounter
        fieldPicChars._fieldIndex = ,
                _oField._fieldPicChars.ref._fieldIndex
        fieldPicReps._fieldIndex  = ,
                _oField._fieldPicReps.ref._fieldIndex
    end
    fieldPicType          = _oField._fieldPicType.ref
    fieldPicIntsNum       = _oField._fieldPicIntsNum.ref
    fieldPicDecsNum       = _oField._fieldPicDecsNum.ref
    fieldPicSign          = _oField._fieldPicSign.ref
    fieldPicCompType      = _oField._fieldPicCompType.ref
    fieldRedefdLabel      = _oField._fieldRedefdLabel.ref
    fieldRedefsDegree     = _oField._fieldRedefsDegree.ref
    fieldOccursNum        = _oField._fieldOccursNum.ref
    fieldOccursDisplay    = _oField._fieldOccursDisplay.ref
    fieldValueLength      = _oField._fieldValueLength.ref
    fieldColumnLength     = _oField._fieldColumnLength.ref
    fieldTabColOverSw     = _oField._fieldTabColOverSw.ref
    fieldEbcdicLength     = _oField._fieldEbcdicLength.ref
    fieldEbcdicFromCol    = _oField._fieldEbcdicFromCol.ref
    fieldEbcdicToCol      = _oField._fieldEbcdicToCol.ref
    return

displayFieldsList:
    say '-------------------------------------------------------------'
    say 'fieldsList'
    do fieldsIndex = 1 to fieldsCounter
        call retrieveField fieldsIndex
        call displayField
    end
    return

displayField:
    say '/index='fieldsIndex ,
           || '=====================================================' ,
           || '====================================================='
    say '-level['fieldLevel']' ,
           || 'log*['fieldLogLevel']' ,
           || 'display*['fieldDisplayLevel']'
    say '-label['fieldLabel']' ,
           || '*len='fieldLabelLen']'
    say '-labelSubs:'
    do fieldLabelSubsIndex = 1 to fieldLabelSubsCounter
        fieldLabelSub       = fieldLabelSubs.fieldLabelSubsIndex
        fieldLabelSubLength = fieldLabelSubLengths.fieldLabelSubsIndex
        if (fieldLabelSub <> '' ,
                | fieldLabelSubLength <> 0)
        then do
            say '    #'fieldLabelSubsIndex': ' ,
                    || 'fieldLabelSub['fieldLabelSub']' ,
                    || 'l['fieldLabelSubLength']'
        end
    end
    say '-labelSubsCounter['fieldLabelSubsCounter']'
    say '-fieldPicString['fieldPicString']'
    say '-picType['fieldPicType']'
    if (fieldPicType <> 'group')
    then do
        say '-fieldPicChar:'
        do fieldPicCharsIndex = 1 to fieldPicCharsCounter
            fieldPicChar = fieldPicChars.fieldPicCharsIndex
            fieldPicRep  = fieldPicReps.fieldPicCharsIndex
            if (fieldPicChar <> '' ,
                    | fieldPicRep <> 0)
            then do
                say '    #'fieldPicCharsIndex': ' ,
                        || 'fieldPicChar['fieldPicChar']' ,
                        || 'r['fieldPicRep']'
            end
        end
        say '-picElemsCounter['fieldPicCharsCounter']'
        say '-pic:' ,
                || '*intsNum['fieldPicIntsNum']' ,
                || '*decsNum['fieldPicDecsNum']' ,
                || '*sign['fieldPicSign']' ,
                || '*compType['fieldPicCompType']'
    end
    say '-redef:' ,
            || '*dLabel['fieldRedefdLabel']' ,
            || '*sDegree['fieldRedefsDegree']'
    say '-occursNum['fieldOccursNum']' ,
            || '*display['fieldOccursDisplay']'
    say '-tabCol:' ,
            || '*valueLength['fieldValueLength']' ,
            || '*columnLength['fieldColumnLength']' ,
            || '*overSw['fieldTabColOverSw']'
    say '-ebcdic:' ,
            || '*length['fieldEbcdicLength']' ,
            || '*fromCol['fieldEbcdicFromCol']' ,
            || '*toCol['fieldEbcdicToCol']'
    return

/*
 * showCopy
 */

showCopy:
    call setOutCopyHeader
    call writeOutCopyRecord
    call setOutCopySeparator
    call writeOutCopyRecord
    do fieldsIndex = 1 to fieldsCounter
        call retrieveField fieldsIndex
        call setOutCopyRecord
        call writeOutCopyRecord
    end
    call startOutCopyFile
    return

setOutCopyHeader:
    outCopyRecord = '' ,
            || ';'right( 'level',     5) ,
            || ';'left(  'label',     36) ,
            || ';'left(  'picture',   20) ,
            || ';'right( 'occurs',    7) ,
            || ';'left(  'redefines', 36) ,
            || ';'right( 'int',       5) ,
            || ';'right( 'dec',       5) ,
            || ';'right( 'from',      5) ,
            || ';'right( 'to',        5) ,
            || ';'right( 'len',       5) ,
            || ';'left(  'structure', 100) ,
            || ';'
    return

setOutCopySeparator:
    outCopyRecord = '' ,
            || ';'right( ' ', 5) ,
            || ';'left(  ' ', 36) ,
            || ';'left(  ' ', 20) ,
            || ';'right( ' ', 7) ,
            || ';'left(  ' ', 36) ,
            || ';'right( ' ', 5) ,
            || ';'right( ' ', 5) ,
            || ';'right( ' ', 5) ,
            || ';'right( ' ', 5) ,
            || ';'right( ' ', 5) ,
            || ';'left(  ' ', 100) ,
            || ';'
    return

setOutCopyRecord:
    call setPicString
    call setOccursString
    call setStructureString
    outCopyRecord = '' ,
            || ';'format( fieldLevel,         5) ,
            || ';'left(   fieldLabel,         36) ,
            || ';'left(   picString,          20) ,
            || ';'right(  occursString,       7) ,
            || ';'left(   fieldRedefdLabel,   36) ,
            || ';'format( fieldPicIntsNum,    5) ,
            || ';'format( fieldPicDecsNum,    5) ,
            || ';'format( fieldEbcdicFromCol, 5) ,
            || ';'format( fieldEbcdicToCol,   5) ,
            || ';'format( fieldEbcdicLength,  5) ,
            || ';'left(   structureString,    100) ,
            || ';'
    return

setPicString:
    picString = ''
    if (fieldPicType = 'group')
    then do
        picString = 'GROUP'
    end
    else do
        do fieldPicCharsIndex = 1 to fieldPicCharsCounter
            picChar = fieldPicChars.fieldPicCharsIndex
            picRep  = fieldPicReps.fieldPicCharsIndex
            picString = picString || picChar
            if (picRep > 1)
            then do
                picString = picString'('picRep')'
            end
        end
        if (fieldPicCompType <> 'zoned')
        then do
            picString = picString' 'fieldPicCompType
        end
    end
    picString = strip(picString)
    return

setOccursString:
    if (fieldOccursDisplay = 0)
    then do
        occursString = ''
    end
    else do
        occursString = strip(format(fieldOccursDisplay, 5))
        occursString = '('occursString')'
    end
    return

setStructureString:
    structureString = ' 'copies('| ', fieldDisplayLevel - 1) ,
            || '|'fieldLevel
    return

/*
 *
 * viewData
 *
 */

viewData:
    call getFieldData
    call showData
    return

setAllSelColsVisible:
    selColsConds. = '+'
    return

/*
 *
 * showData
 *
 */

showData:
    command = ''
    do while (SYS_TRUE)
        select
        when (command = 'r') then call setSelRecs
        when (command = 'c') then call setSelCols
        when (command = 'v') then call showData1
        when (command = 'm') then return
        when (command = 'x') then call exitBlip
        otherwise
            nop
        end
        say
        say 'blip> viewData> enter command:'
        say '    r: select records...'
        say '    c: select columns...'
        say '    v: view data'
        say '    m: return to parent menu'
        say '    x: exit blip'
        parse pull command
    end
    return

/*
 * setSelRecs
 */

setSelRecs:
    if (selRecsFileWritten = FALSE ,
            & selRecsRecsCounter = 0)
    then do
        call refreshSelRecsFile
    end
    selRecsFileWritten = TRUE
    call startSelRecsFile
    allSelRecsParmsOk = FALSE
    command = ''
    do while (allSelRecsParmsOk = FALSE)
        select
        when (command = 'r') then call refreshAndStartSelRecsFile
        when (command = 'h') then call showSelRecsHelp
        when (command = 'v') then call checkSelRecsAndShowData
        when (command = 'm') then return
        when (command = 'x') then call exitBlip
        otherwise
            nop
        end
        if (allSelRecsParmsOk = FALSE)
        then do
            say
            say 'blip> viewData> selRecs> enter command:'
            say '    r: refresh file'
            say '    h: help about parameters'
            say '    v: view data'
            say '    m: return to parent menu'
            say '    x: exit blip'
            parse pull command
        end
    end
    return

refreshSelRecsFile:
    call initSelRecsFeatures
    call closeSelRecsFile
    call openWriteSelRecsFile
    call setSelRecsHeader
    call writeSelRecsRecord
    call setSelRecsAllFields
    call writeSelRecsRecord
    do fieldsIndex = 1 to fieldsCounter
        call retrieveField fieldsIndex
        call setSelRecsRecord
        call writeSelRecsRecord
    end
    return

initSelRecsFeatures:
    selRecsCondLabels.    = ''
    selRecsCondOperators. = ''
    selRecsCondValues.    = ''
    selRecsCondsIndex   = 0
    selRecsCondsCounter = 0
    return

refreshAndStartSelRecsFile:
    call refreshSelRecsFile
    call startSelRecsFile
    return

setSelRecsHeader:
    selRecsRecord = ' field -------------------------------------' ,
            || ' query'
    return

setSelRecsAllFields:
    selRecsRecord = ' ALL FIELDS                                 ' ,
            || ' [][]'
    return

setSelRecsRecord:
    logLevelIndent = copies(' ', fieldDisplayLevel)
    if (fieldPicType = 'group')
    then do
        fieldLabel = fieldLabel'.'
    end
    fieldLabelPad = FIELD_LABEL_PAD - fieldDisplayLevel
    selRecsRecord = logLevelIndent ,
            || left(fieldLabel, fieldLabelPad) ,
            || '[][]'
    return

checkSelRecsAndShowData:
    call initSelRecsFeatures
    allSelRecsParmsOk = TRUE
    call checkSelRecsFile
    if (allSelRecsParmsOk = TRUE)
    then do
        call showData1
    end
    return

checkSelRecsFile:
    if (allSelRecsParmsOk = TRUE) then call checkAllSelRecsFields
    if (allSelRecsParmsOk = TRUE) then call checkAllSelRecsParms
    return

checkAllSelRecsFields:
    call closeSelRecsFile
    call openReadSelRecsFile
    call readSelRecsRecord /* record 1: header */
    call readSelRecsRecord /* record 2: all fields selection */
    do selectRecsIndex = 3 to selRecsRecsCounter
        call readSelRecsRecord
        selRecsFieldLabel = substr(selectRecord, 1, FIELD_LABEL_PAD)
        selRecsFieldLabel = strip(selRecsFieldLabel)
        selRecsFieldLabel = replaceText(selRecsFieldLabel, '.', '')
        call checkSelRecsFieldLabel
    end
    return

checkSelRecsFieldLabel:
    condFieldFound = FALSE
    do fieldsIndex = 1 to fieldsCounter ,
            while (condFieldFound = FALSE)
        call retrieveField fieldsIndex
        if (fieldLabel = selRecsFieldLabel)
        then do
            condFieldFound = TRUE
        end
    end
    if (condFieldFound = FALSE)
    then do
        allSelRecsParmsOk = FALSE
        say
        say 'blip> viewData> selRecs> field [' ,
                || selRecsFieldLabel'] not found'
    end
    return

checkAllSelRecsParms:
    call closeSelRecsFile
    call openReadSelRecsFile
    call readSelRecsRecord /* record 1: header */
                           /* record 2: all fields selection */
    do selectRecsIndex = 2 to selRecsRecsCounter
        call readSelRecsRecord
        selRecsFieldLabel = substr(selectRecord, 1, FIELD_LABEL_PAD)
        selRecsFieldLabel = strip(selRecsFieldLabel)
        selRecsFieldLabel = replaceText(selRecsFieldLabel, '.', '')
        call readSelectParms
        if (selRecsOperator <> '' ,
                & selRecsValue <> '')
        then do
            call checkSelRecsParms
        end
    end
    return

readSelectParms:
    selectParmsString = strip(substr(selectRecord, FIELD_LABEL_PAD + 1))
    parse var selectParmsString '['selRecsOperator']' ,
            . '['selRecsValue']'
    return

checkSelRecsParms:
    selRecsParmsOk = TRUE
    if (selRecsParmsOk = TRUE) then call checkSelRecsOperator
    if (selRecsParmsOk = TRUE) then call checkSelRecsValue
    if (selRecsParmsOk = TRUE) then call setSelRecsCondition
    if (selRecsParmsOk = FALSE)
    then do
        allSelRecsParmsOk = FALSE
    end
    return

checkSelRecsOperator:
    select
    when (os = 'WIN64') then call checkSelRecsOperatorWIN
    when (os = 'TSO')   then call checkSelRecsOperatorTSO
    end
    return

checkSelRecsOperatorWIN:
    if (      selRecsOperator <> '='   ,
            & selRecsOperator <> '=='  ,
            & selRecsOperator <> '\='  ,
            & selRecsOperator <> '\==' ,
            & selRecsOperator <> '<='  ,
            & selRecsOperator <> '<'   ,
            & selRecsOperator <> '<<'  ,
            & selRecsOperator <> '>='  ,
            & selRecsOperator <> '>'   ,
            & selRecsOperator <> '>>'    )
    then do
        selRecsParmsOk = FALSE
        say
        say 'blip> viewData> selRecs> comparison operator ' ,
                || '['selRecsOperator'] not allowed'
    end
    return

checkSelRecsOperatorTSO:
    if (      selRecsOperator <> '='   ,
            & selRecsOperator <> '=='  ,
            & selRecsOperator <> '/='  ,
            & selRecsOperator <> '/==' ,
            & selRecsOperator <> '<='  ,
            & selRecsOperator <> '<'   ,
            & selRecsOperator <> '<<'  ,
            & selRecsOperator <> '>='  ,
            & selRecsOperator <> '>'   ,
            & selRecsOperator <> '>>'    )
    then do
        selRecsParmsOk = FALSE
        say
        say 'blip> viewData> selRecs> comparison operator ' ,
                || '['selRecsOperator'] not allowed'
    end
    return

checkSelRecsValue:
    /* cond value not checked: wrong values must be allowed */
    return

setSelRecsCondition:
    selRecsCondsCounter = selRecsCondsCounter + 1
    selRecsCondLabels.selRecsCondsCounter    = selRecsFieldLabel
    selRecsCondOperators.selRecsCondsCounter = selRecsOperator
    selRecsCondValues.selRecsCondsCounter    = selRecsValue
    return

/*
 * setSelCols
 */

setSelCols:
    if (selColsFileWritten = FALSE ,
            & selColsRecsCounter = 0)
    then do
        call refreshSelColsFile
    end
    selColsFileWritten = TRUE
    call startSelColsFile
    allSelColsParmsOk = FALSE
    command = ''
    do while (allSelColsParmsOk = FALSE)
        select
        when (command = 'r') then call refreshAndStartSelColsFile
        when (command = 'h') then call showSelColsHelp
        when (command = 'v') then call checkSelColsAndShowData
        when (command = 'm') then return
        when (command = 'x') then call exitBlip
        otherwise
            nop
        end
        if (allSelColsParmsOk = FALSE)
        then do
            say
            say 'blip> viewData> selCols> enter command:'
            say '    r: refresh file'
            say '    h: help about parameters'
            say '    v: view data'
            say '    m: return to parent menu'
            say '    x: exit blip'
            parse pull command
        end
    end
    return

refreshSelColsFile:
    call initSelColsFeatures
    call closeSelColsFile
    call openWriteSelColsFile
    selColsRecord = '*'
    call writeSelColsRecord
    selColsRecord = '* examples:'
    call writeSelColsRecord
    selColsRecord = '*'
    call writeSelColsRecord
    selColsRecord = '* when [rec-type][=][01]'
    call writeSelColsRecord
    selColsRecord = '* and  [rec-subtype][=][01]'
    call writeSelColsRecord
    selColsRecord = '* show [rec-header]'
    call writeSelColsRecord
    selColsRecord = '* and  [rec-type]'
    call writeSelColsRecord
    selColsRecord = '* and  [rec-body-01-01]'
    call writeSelColsRecord
    selColsRecord = '*'
    call writeSelColsRecord
    selColsRecord = '* default'
    call writeSelColsRecord
    selColsRecord = '* show [*]'
    call writeSelColsRecord
    selColsRecord = '*'
    call writeSelColsRecord
    selColsRecord = ''
    call writeSelColsRecord
    selColsRecord = ' default'
    call writeSelColsRecord
    selColsRecord = ' show [*]'
    call writeSelColsRecord
    selColsRecord = ''
    call writeSelColsRecord
    return

initSelColsFeatures:
    selColsLabels.    = ''
    selColsOperators. = ''
    selColsValues.    = ''
    selColsColumns.   = ''
    selColsCondsCounters.   = 0
    selColsColumnsCounters. = 0
    selColsIndex   = 0
    selColsCounter = 0
    return

refreshAndStartSelColsFile:
    call refreshSelColsFile
    call startSelColsFile
    return

showSelColsHelp:
    select
    when (os = 'WIN64') then call showSelColsHelpWIN
    when (os = 'TSO')   then call showSelColsHelpTSO
    end
    return

showSelColsHelpWIN:
    say
    say 'blip> viewData> selCols> help> cond format:'
    say '       when [copy-recType][=][01] show [copy-record01]'
    say
    say 'blip> viewData> selCols> help> comparison operators:'
    say '       =   : equal'
    say '       ==  : exactly equal     - example: "A "=="A"  => FALSE'
    say '       \=  : not equal'
    say '       \== : exactly not equal - example: "A "\=="A" => TRUE'
    say '       <=  : less or equal'
    say '       <   : less'
    say '       <<  : exactly less      - example: "A"<<"A "  => TRUE'
    say '       >=  : greater or equal'
    say '       >   : greater'
    say '       >>  : exactly greater   - example: "A ">>"A"  => TRUE'
    return

showSelColsHelpTSO:
    say
    say 'blip> viewData> selCols> help> cond format:'
    say '       when [copy-recType][=][01] show [copy-record01]'
    say
    say 'blip> viewData> selCols> help> comparison operators:'
    say '       =   : equal'
    say '       ==  : exactly equal     - example: "A "=="A"  => FALSE'
    say '       /=  : not equal'
    say '       /== : exactly not equal - example: "A "/=="A" => TRUE'
    say '       <=  : less or equal'
    say '       <   : less'
    say '       <<  : exactly less      - example: "A"<<"A "  => TRUE'
    say '       >=  : greater or equal'
    say '       >   : greater'
    say '       >>  : exactly greater   - example: "A ">>"A"  => TRUE'
    return

checkSelColsAndShowData:
    call initSelColsFeatures
    allSelColsParmsOk = TRUE
    call checkSelColsFile
    if (allSelColsParmsOk = TRUE)
    then do
        call showData1
    end
    return

checkSelColsFile:
    allSelColsParmsOk = TRUE
    if (allSelColsParmsOk = TRUE) then call getAllSelColsStrings
    if (allSelColsParmsOk = TRUE) then call checkAllSelColsFields
    if (allSelColsParmsOk = TRUE) then call checkAllSelColsParms
    if (allSelColsParmsOk = TRUE) then call setSelColsConds
    return

getAllSelColsStrings:
    selColsStrings.       = ''
    selColsStringsIndex   = 0
    selColsStringsCounter = 0
    call closeSelColsFile
    call openReadSelColsFile
    do selColsRecsIndex = 1 to selColsRecsCounter
        call readSelColsRecord
        parse var selColsRecord firstWord .
        if (firstWord <> '*' ,
                & firstWord <> '')
        then do
            if (firstWord = 'when' ,
                    | firstWord = 'default')
            then do
                selColsStringsCounter = selColsStringsCounter + 1
            end
            selColsStrings.selColsStringsCounter = ,
                  selColsStrings.selColsStringsCounter' 'selColsRecord
        end
    end
    return

checkAllSelColsFields:
    do selColsStringsIndex = 1 to selColsStringsCounter
        /* space added in order to manage "default show" */
        selColsString = ' 'selColsStrings.selColsStringsIndex
        if (selColsString <> '')
        then do
            call checkSelColsString
        end
    end
    return

checkSelColsString:
    selColsPhase = ''
    selColsCounter = selColsCounter + 1
    selColsCondsCounter   = 0
    selColsColumnsCounter = 0
    selColsStringCut = selColsString
    do while (length(selColsStringCut) > 0 ,
            & allSelColsParmsOk = TRUE ,
            & nextWord <> '')
        call checkSelColsWords
    end
    selColsCondsCounters.selColsCounter   = selColsCondsCounter
    selColsColumnsCounters.selColsCounter = selColsColumnsCounter
    return

checkSelColsWords:
    parse var selColsStringCut nextWord '[' .
    nextWord = space(nextWord, 1)
    select
    when (nextWord = 'when' ,
            | (nextWord = 'and' & selColsPhase = 'when'))
    then do
        selColsPhase = 'when'
        parse var selColsStringCut . '[' selColsLabel    ']' ,
                                   . '[' selColsOperator ']' ,
                                   . '[' selColsValue    ']' ,
                                   selColsStringCut
        selColsColumns.selColsCounter.ALL_FIELDS_LABEL = '-'
        selColsCondsCounter = selColsCondsCounter + 1
        selColsLabels.selColsCounter.selColsCondsCounter ,
                = selColsLabel
        selColsOperators.selColsCounter.selColsCondsCounter ,
                = selColsOperator
        selColsValues.selColsCounter.selColsCondsCounter ,
                = selColsValue
    end
    when (nextWord = 'show' ,
            | (nextWord = 'and' & selColsPhase = 'show'))
    then do
        selColsPhase = 'show'
        parse var selColsStringCut . '[' selColsColumn ']' ,
                                   selColsStringCut
        selColsColumns.selColsCounter.ALL_FIELDS_LABEL = '-'
        selColsColumnsCounter = selColsColumnsCounter + 1
        selColsColumns.selColsCounter.selColsColumnsCounter ,
                = selColsColumn
    end
    when (nextWord = 'default show')
    then do
        selColsPhase = 'default'
        parse var selColsStringCut 'default' ,
                                selColsStringCut
    end
    when (nextWord = 'show' ,
            | (nextWord = 'and' & selColsPhase = 'default'))
    then do
        selColsPhase = 'show'
        parse var selColsStringCut . '[' selColsColumn ']' ,
                                   selColsStringCut
        if (selColsColumn = '*')
        then do
            selColsColumns.0.ALL_FIELDS_LABEL = '+'
        end
        else do
            selColsColumns.0.ALL_FIELDS_LABEL = '-'
            selColsColumnsCounter = selColsColumnsCounter + 1
            selColsColumns.0.selColsColumnsCounter = selColsColumn
        end
    end
    when (nextWord = '')
    then do
        nop
    end
    otherwise
        allSelColsParmsOk = FALSE
        say
        say 'blip> viewData> selCols> word ['nextWord'] not allowed'
    end
    return

checkAllSelColsParms:
    do selColsIndex = 1 to selColsCounter ,
            while (allSelColsParmsOk = TRUE)
        do scCondsIndex = 1 to selColsCondsCounters.selColsIndex
            selColsLabel    = selColsLabels.selColsIndex.scCondsIndex
            selColsOperator = ,
                    selColsOperators.selColsIndex.scCondsIndex
            selColsValue    = selColsValues.selColsIndex.scCondsIndex
            call checkSelColsLabel    selColsLabel
            call checkSelColsOperator selColsOperator
        end
        do scColumnsIndex = 1 to selColsColumnsCounters.selColsIndex
            selColsColumn = selColsColumns.selColsIndex.scColumnsIndex
            call checkSelColsLabel selColsColumn
        end
    end
    return

checkSelColsLabel:
        parse arg label
    fieldFound = FALSE
    if (label = '*')
    then do
        fieldFound = TRUE
    end
    do fieldsIndex = 1 to fieldsCounter ,
            while (fieldFound = FALSE)
        call retrieveField fieldsIndex
        if (fieldLabel = label)
        then do
            fieldFound = TRUE
        end
    end
    if (fieldFound = FALSE)
    then do
        allSelColsParmsOk = FALSE
        say
        say 'blip> viewData> selCols> field ['label']' ,
                || ' not found'
    end
    return

checkSelColsOperator:
        parse arg operator
    select
    when (os = 'WIN64') then call checkSelColsOperatorWIN operator
    when (os = 'TSO')   then call checkSelColsOperatorTSO operator
    end
    return

checkSelColsOperatorWIN:
        parse arg operator
    if (      operator <> '='   ,
            & operator <> '=='  ,
            & operator <> '\='  ,
            & operator <> '\==' ,
            & operator <> '<='  ,
            & operator <> '<'   ,
            & operator <> '<<'  ,
            & operator <> '>='  ,
            & operator <> '>'   ,
            & operator <> '>>'    )
    then do
        allSelColsParmsOk = FALSE
        say
        say 'blip> viewData> selCols> comparison operator [' ,
                || operator'] not allowed'
    end
    return

checkSelColsOperatorTSO:
        parse arg operator
    if (      operator <> '='   ,
            & operator <> '=='  ,
            & operator <> '/='  ,
            & operator <> '/==' ,
            & operator <> '<='  ,
            & operator <> '<'   ,
            & operator <> '<<'  ,
            & operator <> '>='  ,
            & operator <> '>'   ,
            & operator <> '>>'    )
    then do
        allSelColsParmsOk = FALSE
        say
        say 'blip> viewData> selCols> comparison operator [' ,
                || operator'] not allowed'
    end
    return

setSelColsConds:
    do selColsIndex = 1 to selColsCounter
        do selColsFieldsIndex = 1 to fieldsCounter
            call retrieveField selColsFieldsIndex
            selColsConds.selColsIndex.fieldLabel = ''
        end
        selColsConds.selColsIndex.ALL_FIELDS_LABEL = ,
                selColsColumns.selColsIndex.ALL_FIELDS_LABEL
        do scColumnsIndex = 1 to selColsColumnsCounters.selColsIndex
            selColsColumn = selColsColumns.selColsIndex.scColumnsIndex
            selColsConds.selColsIndex.selColsColumn = '+'
        end
        call expandAllSelColsParms
    end
    return

expandAllSelColsParms:
    do dLevelsIndex = 49 to 1 by -1
        selColsFieldsIndex = 1
        do while (selColsFieldsIndex <= fieldsCounter)
            call retrieveField selColsFieldsIndex
            if (fieldDisplayLevel = dLevelsIndex ,
                    & fieldPicType = 'group' ,
                    & (selColsConds.selColsIndex.fieldLabel = '+' ,
                         | selColsConds.selColsIndex.fieldLabel = '-'))
            then do
                call checkSelColsGroupParms
            end
            else do
                selColsFieldsIndex = selColsFieldsIndex + 1
            end
        end
    end
    call checkSelColsAllFieldsParms
    return

checkSelColsGroupParms:
    groupDisplayLevel = fieldDisplayLevel
    groupSelColsCond  = selColsConds.selColsIndex.fieldLabel
    selColsFieldsIndex = selColsFieldsIndex + 1
    insideGroup = TRUE
    do while (selColsFieldsIndex <= fieldsCounter ,
                & insideGroup = TRUE)
        call retrieveField selColsFieldsIndex
        if (fieldDisplayLevel > groupDisplayLevel)
        then do
            if (selColsConds.selColsIndex.fieldLabel = '')
            then do
                selColsConds.selColsIndex.fieldLabel = groupSelColsCond
            end
        end
        else do
            insideGroup = FALSE
        end
        selColsFieldsIndex = selColsFieldsIndex + 1
    end
    return

checkSelColsAllFieldsParms:
    allFieldsSelColsCond = selColsConds.selColsIndex.ALL_FIELDS_LABEL
    do selColsFieldsIndex = 1 to fieldsCounter
        call retrieveField selColsFieldsIndex
        if (selColsConds.selColsIndex.fieldLabel = '')
        then do
            selColsConds.selColsIndex.fieldLabel = allFieldsSelColsCond
        end
    end
    return

/*
 * showData1
 */

showData1:
    call openWriteOutDataFile
    call resetDataFile
    call selectDataRecords
    call startOutDataFile
    return

resetDataFile:
    call closeDataFile
    call openReadDataFile
    dataCursor = 1
    call setDataFileAtCursor
    return

setDataFileAtCursor:
    if (DATA_DSORG = 'seq')
    then do
        charinReset = charin(DATA_FILE, dataCursor, 0)
    end
    return

selectDataRecords:
    lastOccurredSCIndex = -1
    dataFileRecsCursor = 1
    do while ( (DATA_DSORG = 'seq' ,
                | dataFileRecsCursor < dataFileRecsCounter) ,
            & dataFileRecsCursor < MAX_DATA_RECORDS ,
            & checkDataExists() = TRUE)
        if (DATA_DSORG = 'lseq')
        then do
            call readDataFileRecord
        end
        call selectOutputRecord
        dataFileRecsCursor = dataFileRecsCursor + 1
    end
    return

checkDataExists:
    if (DATA_DSORG = 'seq')
    then return checkDataExistsCUR()
    return TRUE

checkDataExistsCUR:
    if (chars(DATA_FILE) > 0)
    then return TRUE
    return FALSE

selectOutputRecord:
    formatDataValues = FALSE
    call checkAllSelRecsConds
    if (allSelRecsCondsOccur = TRUE)
    then do
        formatDataValues = TRUE
        if (occurredSCIndex <> lastOccurredSCIndex)
        then do
            call setOutTableHeader
            lastOccurredSCIndex = occurredSCIndex
        end
        call setOutputRecord
        call writeOutputRecord
    end
    return

checkAllSelRecsConds:
    call saveStartDataCursor
    fieldsDataCursors. = ''
    selRecsCondsOccs. = FALSE
    occurredSCIndex = 0
    call getAllFieldsValue
    do fieldsIndex = 1 to fieldsCounter
        call retrieveField fieldsIndex
        call getValue
        do selRecsCondsIndex = 1 to selRecsCondsCounter
            call checkSelRecsConds
        end
        do selColsIndex = 1 to selColsCounter
            call checkSelColsConds
        end
    end
    call resetStartDataCursor
    call setAllSelRecsCondsSwitch
    return

saveStartDataCursor:
    startDataCursor = dataCursor
    return

checkSelRecsConds:
    selRecsFieldLabel = selRecsCondLabels.selRecsCondsIndex
    select
    when (selRecsFieldLabel = 'ALL FIELDS')
    then do
        selRecsOperator = selRecsCondOperators.selRecsCondsIndex
        selRecsValue    = selRecsCondValues.selRecsCondsIndex
        interpret 'condRc="'allFieldsValue'"' ,
                || selRecsOperator'"'selRecsValue'"'
        if (condRc = SYS_TRUE)
        then do
            selRecsCondsOccs.selRecsCondsIndex = TRUE
        end
    end
    when (selRecsFieldLabel = fieldLabel)
    then do
        selRecsOperator = selRecsCondOperators.selRecsCondsIndex
        selRecsValue    = selRecsCondValues.selRecsCondsIndex
        interpret 'condRc="'dataValue'"' ,
                || selRecsOperator'"'selRecsValue'"'
        if (condRc = SYS_TRUE)
        then do
            selRecsCondsOccs.selRecsCondsIndex = TRUE
        end
    end
    otherwise
        nop
    end
    return

checkSelColsConds:
    do scCondsIndex = 1 to selColsCondsCounters.selColsIndex
        selColsLabel    = selColsLabels.selColsIndex.scCondsIndex
        selColsOperator = selColsOperators.selColsIndex.scCondsIndex
        selColsValue    = selColsValues.selColsIndex.scCondsIndex
        if (selColsLabel = fieldLabel)
        then do
            interpret 'selColsCondRc="'dataValue'"' ,
                    || selColsOperator'"'selColsValue'"'
            if (selColsCondRc = SYS_TRUE)
            then do
                occurredSCIndex = selColsIndex
            end
        end
    end
    return

resetStartDataCursor:
    dataCursor = startDataCursor
    call setDataFileAtCursor
    return

setAllSelRecsCondsSwitch:
    allSelRecsCondsOccur = TRUE
    do selRecsCondsIndex = 1 to selRecsCondsCounter
        if (selRecsCondsOccs.selRecsCondsIndex = FALSE)
        then do
            allSelRecsCondsOccur = FALSE
        end
    end
    return

setOutTableHeader:
    outputRecord = EMPTY_OUT_REC
    call writeOutputRecord
    do fieldLabelSubsIndex = 1 to fieldsMaxLabelSubs
        call setOutTableHeaderRecord
        call writeOutputRecord
    end
    outputRecord = EMPTY_OUT_REC
    call writeOutputRecord
    return

setOutTableHeaderRecord:
    outputRecord = OUT_REC_INITIAL_SPACE
    do fieldsIndex = 1 to fieldsCounter
        call retrieveField fieldsIndex
        if (fieldPicType <> 'group' ,
                & selColsConds.occurredSCIndex.fieldLabel = '+')
        then do
            call showOutTableHeaderField
        end
    end
    return

showOutTableHeaderField:
    select
    when (fieldPicType = 'alphanum')
    then do
        column = left(fieldLabelSubs.fieldLabelSubsIndex, ,
                fieldColumnLength)
    end
    when (fieldPicType = 'integer' ,
            | fieldPicType = 'decimal')
    then do
        column = right(fieldLabelSubs.fieldLabelSubsIndex, ,
                fieldColumnLength)
    end
    end
    outputRecord = outputRecord || column' ; '
    return

setOutputRecord:
    outputRecord = OUT_REC_INITIAL_SPACE
    do fieldsIndex = 1 to fieldsCounter
        call retrieveField fieldsIndex
        call getValue
        if (fieldPicType <> 'group' ,
                & selColsConds.occurredSCIndex.fieldLabel = '+')
        then do
            call setOutTableDataField
        end
    end
    return

setOutTableDataField:
    valueString = dataValue
    select
    when (fieldPicType = 'alphanum')
    then do
        column = left(valueString, fieldColumnLength)
    end
    when (fieldPicType = 'integer' ,
            | fieldPicType = 'decimal')
    then do
        column = right(valueString, fieldColumnLength)
    end
    end
    outputRecord = outputRecord || column' ; '
    return

/*
 * get*Value*
 */

getAllFieldsValue:
    dataCursor = 1 + (dataFileRecsCursor - 1) * 600
    /* treated as group */
    if (DATA_DSORG = 'seq')
    then call getAllFieldsValueCUR
    else call getAllFieldsValueCOL
    return

getAllFieldsValueCUR:
    allFieldsValue = charin(DATA_FILE, dataCursor, MAX_ALPHA_LENGTH)
    call setDataFileAtCursor
    if (DATA_ENCODING = 'ebcdic')
    then do
        allFieldsValue = ebcdic2AsciiAlpha(allFieldsValue)
    end
    return

getAllFieldsValueCOL:
    allFieldsValue = substr(dataFileRecord, fieldEbcdicFromCol, ,
            MAX_ALPHA_LENGTH)
    return

getValue:
    dataCursor = fieldEbcdicFromCol + (dataFileRecsCursor - 1) * 600
    numeric digits 19  /* per evitare notazione esponenziale in output */
    dataValue = ''
    select
    when (fieldPicType = 'group')    then nop
    when (fieldPicType = 'alphanum') then call getAlphanumValue
    when (fieldPicType = 'integer' ,
        | fieldPicType = 'decimal')  then call getNumericValue
    end
    numeric digits 9  /* ripristino default */
    return

getAlphanumValue:
    if (DATA_DSORG = 'seq')
    then call getAlphanumValueCUR
    else call getAlphanumValueCOL
    if (formatDataValues = TRUE)
    then do
        call formatAlphanumValue
    end
    return

getAlphanumValueCUR:
    dataValue = charin(DATA_FILE, dataCursor, fieldPicIntsNum)
    if (DATA_ENCODING = 'ebcdic')
    then do
        dataValue = ebcdic2AsciiAlpha(dataValue)
    end
    return

getAlphanumValueCOL:
    dataValue = substr(dataFileRecord, fieldEbcdicFromCol, ,
            fieldEbcdicLength)
    return

getNumericValue:
    select
    when (fieldPicCompType = 'zoned')  then call getZonedValue
    when (fieldPicCompType = 'comp-3') then call getComp3Value
    when (fieldPicCompType = 'comp')   then call getCompValue
    end
    select
    when (formatDataValues = FALSE)
    then do
        nop
    end
    when (checkValidNumValue() = TRUE)
    then do
        call formatValidNumValue
    end
    otherwise
        call formatNotValidNumValue
    end
    return

getZonedValue:
    if (DATA_DSORG = 'seq')
    then call getZonedValueCUR
    else call getZonedValueCOL
    return

getZonedValueCUR:
    dataValue = charin(DATA_FILE, dataCursor, fieldEbcdicLength)
    select
    when (DATA_ENCODING = 'ebcdic') then call getZonedValueEbcdic
    when (DATA_ENCODING = 'ascii')  then call getZonedValueAscii
    end
    return

getZonedValueEbcdic:
    dataValue = ebcdic2AsciiNum(dataValue)
    return

getZonedValueAscii:
    say 'getZonedValueAscii'
    say 'fieldLabel ['fieldLabel']'
    say 'fieldPicCompType ['fieldPicCompType']'
    say 'field format not supported with ASCII encoding'
    call exitError
    return

getZonedValueCOL:
    dataValue = substr(dataFileRecord, fieldEbcdicFromCol, ,
            fieldEbcdicLength)
    return

getComp3Value:
    if (DATA_DSORG = 'seq')
    then call getComp3ValueCUR
    else call getComp3ValueCOL
    dataValue = c2x(dataValue)
    rrr = (fieldPicIntsNum + fieldPicDecsNum) // 2
    if (rrr = 0)
    then do
        dataValue = substr(dataValue, 2)
    end
    return

getComp3ValueCUR:
    select
    when (DATA_ENCODING = 'ebcdic') then call getComp3ValueEbcdic
    when (DATA_ENCODING = 'ascii')  then call getComp3ValueAscii
    end
    return

getComp3ValueEbcdic:
    dataValue = charin(DATA_FILE, dataCursor, fieldEbcdicLength)
    return

getComp3ValueAscii:
    say 'getComp3ValueAscii'
    say 'fieldLabel ['fieldLabel']'
    say 'fieldPicCompType ['fieldPicCompType']'
    say 'field format not supported with ASCII encoding'
    call exitError
    return

getComp3ValueCOL:
    dataValue = substr(dataFileRecord, fieldEbcdicFromCol, ,
            fieldEbcdicLength)
    return

getCompValue:
    if (DATA_DSORG = 'seq')
    then call getCompValueCUR
    else call getCompValueCOL
    hexValue = c2x(dataValue)

/* correzione empirica:
                          1110001001000000        E240    -7616
       -> 00000000000000011110001001000000    0001E240    123456
                          0001110111000000        1DC0    7616
       -> 11111111111111100001110111000000    FFFE1DC0    -123456
*/

    if (fieldPicDecsNum > 0 ,
            & fieldEbcdicLength <= 2 ,
            & hexValue <> 0)
    then do
        if (left(binValue, 1) = '0')
        then do
            hexValue = '0001'hexValue
        end
        else do
            hexValue = 'FFFE'hexValue
        end
    end

    dataValue = x2d(hexValue, length(hexValue))
    if (dataValue = 0)
    then do
        dataSign = ''
    end
    else do
        binValue = x2b(hexValue)
        if (left(binValue, 1) = '0')  /* positive */
        then do
            dataValue = x2d(hexValue)
            if (fieldPicSign = 'signed')
            then dataSign = 'C'
            else dataSign = 'F'
        end
        else do
            flipValue = binFlip(binValue)
            hexValue  = b2x(flipValue)
            dataValue = x2d(hexValue) + 1
            dataSign = 'D'
        end
    end
    dataValueLDiff = fieldPicIntsNum + fieldPicDecsNum - length(dataValue)
    select  /* correzione empirica */
    when (dataValueLDiff = 0)
    then do
        nop
    end
    when (dataValueLDiff > 0)
    then do
        dataValue = copies('0', dataValueLDiff) || dataValue
    end
    when (dataValueLDiff < 0)
    then do
        dataValue = right(dataValue, fieldPicIntsNum + fieldPicDecsNum)
    end
    end
    dataValue = dataValue || dataSign
    return

getCompValueCUR:
    select
    when (DATA_ENCODING = 'ebcdic') then call getCompValueEbcdic
    when (DATA_ENCODING = 'ascii')  then call getCompValueAscii
    end
    return

getCompValueEbcdic:
    dataValue = charin(DATA_FILE, dataCursor, fieldEbcdicLength)
    return

getCompValueAscii:
    say 'getCompValueAscii'
    say 'fieldLabel ['fieldLabel']'
    say 'fieldPicCompType ['fieldPicCompType']'
    say 'field format not supported with ASCII encoding'
    call exitError
    return

getCompValueCOL:
    dataValue = substr(dataFileRecord, fieldEbcdicFromCol, ,
            fieldEbcdicLength)
    return

checkValidNumValue:
    if (length(dataValue) = 0)
    then return FALSE
    if (isANumber(dataValue) = TRUE)
    then return TRUE
    numbersChar = substr(dataValue, 1, length(dataValue) - 1)
    signChar    = substr(dataValue, length(dataValue), 1)
    if (isANumber(numbersChar) = TRUE ,
            & (signChar = 'C' | signChar = 'D' | signChar = 'F') )
    then do
        /* i dati COMP possono contenere piu' cifre di quelle che nominalmente
           spetterebbero al campo secondo la PIC */
        if (length(numbersChar) > fieldPicIntsNum + fieldPicDecsNum)
        then return FALSE
        else return TRUE
    end
    return FALSE

/*
 * format*
 */

formatAlphanumValue:
    if (fieldTabColOverSw = TRUE)
    then do
        dataValue = substr(dataValue, 1, MAX_ALPHA_LENGTH) ,
                || OVERFLOW_STRING
    end
    else do
        dataValue = substr(dataValue, 1, fieldPicIntsNum)
    end
    return

formatValidNumValue:
    call formatSign
    call formatNumber
    return

formatSign:
    dataValueChar = right(dataValue, 1)
    valueSign = '?'
    select
    when (dataValueChar = 'F') then valueSign = ''
    when (dataValueChar = 'C') then valueSign = '+'
    when (dataValueChar = 'D') then valueSign = '-'
    otherwise
        nop
    end
    if (valueSign <> '?')
    then do
        dataValue = valueSign ,
                || substr(dataValue, 1, length(dataValue) - 1)
    end
    return

formatNumber:
    if (left(dataValue, 1) = '+' ,
            | left(dataValue, 1) = '-')
    then do
        signChar    = substr(dataValue, 1, 1)
        numbersChar = substr(dataValue, 2, length(dataValue) - 1)
    end
    else do
        signChar    = ''
        numbersChar = dataValue
    end
    if (fieldPicDecsNum = 0)
    then do
        intNumbers = substr(numbersChar, 1, fieldPicIntsNum)
        dataValue = signChar || intNumbers
    end
    else do
        intNumbers = substr(numbersChar, 1, fieldPicIntsNum)
        decNumbers = substr(numbersChar, fieldPicIntsNum + 1)
        dataValue = signChar || intNumbers || '.' || decNumbers
    end
    dataValue = replaceText(dataValue, '+0', ' +')
    dataValue = replaceText(dataValue, '-0', ' -')
    if (left(dataValue, 2) == '0.')
    then do
        dataValue = ' .'substr(dataValue, 3)
    end
    if (left(dataValue, 2) == '00')
    then do
        dataValue = ' 0'substr(dataValue, 3)
    end
    dataValue = replaceText(dataValue, ' 0', ' ')
    if (dataValue = '' ,
            | dataValue = '+' ,
            | dataValue = '-')
    then do
        dataValue = overlay('0', dataValue, length(dataValue))
    end
    return

formatNotValidNumValue:
    if (length(dataValue) > fieldValueLength)
    then do
        say 'formatNotValidNumValue'
        say 'length(dataValue) > fieldValueLength'
        say 'length(dataValue) ['length(dataValue)']'
        say 'fieldValueLength ['fieldValueLength']'
        dataValue = substr(dataValue, 1, fieldValueLength)
        dataValue = substr(dataValue, 1, length(dataValue) - 1) || '?'
    end
    else do
        dataValue = dataValue ,
                || copies(' ', fieldValueLength - length(dataValue))
    end
    return

/*
 * showSelRecsHelp
 */

showSelRecsHelp:
    say
    say 'blip> viewData> selRecs> help> cond format:'
    say '       [operator][value]       - example: [>=][3.01]'
    say
    say 'blip> viewData> selRecs> help> comparison operators:'
    say '       =   : equal'
    say '       ==  : exactly equal     - example: "A "=="A"  => FALSE'
        select
        when (os = 'WIN64')
        then do
    say '       \=  : not equal'
    say '       \== : exactly not equal - example: "A "\=="A" => TRUE'
        end
        when (os = 'TSO')
        then do
    say '       /=  : not equal'
    say '       /== : exactly not equal - example: "A "/=="A" => TRUE'
        end
        end
    say '       <=  : less or equal'
    say '       <   : less'
    say '       <<  : exactly less      - example: "A"<<"A "  => TRUE'
    say '       >=  : greater or equal'
    say '       >   : greater'
    say '       >>  : exactly greater   - example: "A ">>"A"  => TRUE'
    return

/*
 *
 * utilities
 *
 */

loadEbcdicToAscii:
    i = 0
    /* lowercase alphabetic letters */
    i = i+1;  ebcdic.i = '81';  ascii.i = '61'  /* a */
    i = i+1;  ebcdic.i = '82';  ascii.i = '62'  /* b */
    i = i+1;  ebcdic.i = '83';  ascii.i = '63'  /* c */
    i = i+1;  ebcdic.i = '84';  ascii.i = '64'  /* d */
    i = i+1;  ebcdic.i = '85';  ascii.i = '65'  /* e */
    i = i+1;  ebcdic.i = '86';  ascii.i = '66'  /* f */
    i = i+1;  ebcdic.i = '87';  ascii.i = '67'  /* g */
    i = i+1;  ebcdic.i = '88';  ascii.i = '68'  /* h */
    i = i+1;  ebcdic.i = '89';  ascii.i = '69'  /* i */
    i = i+1;  ebcdic.i = '91';  ascii.i = '6A'  /* j */
    i = i+1;  ebcdic.i = '92';  ascii.i = '6B'  /* k */
    i = i+1;  ebcdic.i = '93';  ascii.i = '6C'  /* l */
    i = i+1;  ebcdic.i = '94';  ascii.i = '6D'  /* m */
    i = i+1;  ebcdic.i = '95';  ascii.i = '6E'  /* n */
    i = i+1;  ebcdic.i = '96';  ascii.i = '6F'  /* o */
    i = i+1;  ebcdic.i = '97';  ascii.i = '70'  /* p */
    i = i+1;  ebcdic.i = '98';  ascii.i = '71'  /* q */
    i = i+1;  ebcdic.i = '99';  ascii.i = '72'  /* r */
    i = i+1;  ebcdic.i = 'A2';  ascii.i = '73'  /* s */
    i = i+1;  ebcdic.i = 'A3';  ascii.i = '74'  /* t */
    i = i+1;  ebcdic.i = 'A4';  ascii.i = '75'  /* u */
    i = i+1;  ebcdic.i = 'A5';  ascii.i = '76'  /* v */
    i = i+1;  ebcdic.i = 'A6';  ascii.i = '77'  /* w */
    i = i+1;  ebcdic.i = 'A7';  ascii.i = '78'  /* x */
    i = i+1;  ebcdic.i = 'A8';  ascii.i = '79'  /* y */
    i = i+1;  ebcdic.i = 'A9';  ascii.i = '7A'  /* z */
    /* uppercase alphabetic letters / positive numbers */
    i = i+1;  ebcdic.i = 'C1';  ascii.i = '41'  /* A */
    i = i+1;  ebcdic.i = 'C2';  ascii.i = '42'  /* B */
    i = i+1;  ebcdic.i = 'C3';  ascii.i = '43'  /* C */
    i = i+1;  ebcdic.i = 'C4';  ascii.i = '44'  /* D */
    i = i+1;  ebcdic.i = 'C5';  ascii.i = '45'  /* E */
    i = i+1;  ebcdic.i = 'C6';  ascii.i = '46'  /* F */
    i = i+1;  ebcdic.i = 'C7';  ascii.i = '47'  /* G */
    i = i+1;  ebcdic.i = 'C8';  ascii.i = '48'  /* H */
    i = i+1;  ebcdic.i = 'C9';  ascii.i = '49'  /* I */
    /* uppercase alphabetic letters / negative numbers */
    i = i+1;  ebcdic.i = 'D1';  ascii.i = '4A'  /* J */
    i = i+1;  ebcdic.i = 'D2';  ascii.i = '4B'  /* K */
    i = i+1;  ebcdic.i = 'D3';  ascii.i = '4C'  /* L */
    i = i+1;  ebcdic.i = 'D4';  ascii.i = '4D'  /* M */
    i = i+1;  ebcdic.i = 'D5';  ascii.i = '4E'  /* N */
    i = i+1;  ebcdic.i = 'D6';  ascii.i = '4F'  /* O */
    i = i+1;  ebcdic.i = 'D7';  ascii.i = '50'  /* P */
    i = i+1;  ebcdic.i = 'D8';  ascii.i = '51'  /* Q */
    i = i+1;  ebcdic.i = 'D9';  ascii.i = '52'  /* R */
    /* uppercase alphabetic letters */
    i = i+1;  ebcdic.i = 'E2';  ascii.i = '53'  /* S */
    i = i+1;  ebcdic.i = 'E3';  ascii.i = '54'  /* T */
    i = i+1;  ebcdic.i = 'E4';  ascii.i = '55'  /* U */
    i = i+1;  ebcdic.i = 'E5';  ascii.i = '56'  /* V */
    i = i+1;  ebcdic.i = 'E6';  ascii.i = '57'  /* W */
    i = i+1;  ebcdic.i = 'E7';  ascii.i = '58'  /* X */
    i = i+1;  ebcdic.i = 'E8';  ascii.i = '59'  /* Y */
    i = i+1;  ebcdic.i = 'E9';  ascii.i = '5A'  /* Z */
    /* unsigned numbers */
    i = i+1;  ebcdic.i = 'F0';  ascii.i = '30'  /* 0 */
    i = i+1;  ebcdic.i = 'F1';  ascii.i = '31'  /* 1 */
    i = i+1;  ebcdic.i = 'F2';  ascii.i = '32'  /* 2 */
    i = i+1;  ebcdic.i = 'F3';  ascii.i = '33'  /* 3 */
    i = i+1;  ebcdic.i = 'F4';  ascii.i = '34'  /* 4 */
    i = i+1;  ebcdic.i = 'F5';  ascii.i = '35'  /* 5 */
    i = i+1;  ebcdic.i = 'F6';  ascii.i = '36'  /* 6 */
    i = i+1;  ebcdic.i = 'F7';  ascii.i = '37'  /* 7 */
    i = i+1;  ebcdic.i = 'F8';  ascii.i = '38'  /* 8 */
    i = i+1;  ebcdic.i = 'F9';  ascii.i = '39'  /* 9 */
    /* symbols */
    i = i+1;  ebcdic.i = '50';  ascii.i = '26'  /* & */
    i = i+1;  ebcdic.i = '7D';  ascii.i = '27'  /* ' */
    i = i+1;  ebcdic.i = '40';  ascii.i = '20'  /*   */
    i = i+1;  ebcdic.i = '7C';  ascii.i = '40'  /* @ */
    i = i+1;  ebcdic.i = '60';  ascii.i = '2D'  /* - */
    i = i+1;  ebcdic.i = '6B';  ascii.i = '2C'  /* â€? */
    i = i+1;  ebcdic.i = '5E';  ascii.i = '3B'  /* ; */
    i = i+1;  ebcdic.i = '6D';  ascii.i = '5F'  /* _ */
    i = i+1;  ebcdic.i = '6E';  ascii.i = '3E'  /* > */
    i = i+1;  ebcdic.i = '6F';  ascii.i = '3F'  /* ? */
    i = i+1;  ebcdic.i = '4B';  ascii.i = '2E'  /* . */
    i = i+1;  ebcdic.i = '4C';  ascii.i = '3C'  /* < */
    i = i+1;  ebcdic.i = '7E';  ascii.i = '3D'  /* = */
    i = i+1;  ebcdic.i = 'A1';  ascii.i = '7E'  /* â?? */
    i = i+1;  ebcdic.i = '4D';  ascii.i = '28'  /* ( */
    i = i+1;  ebcdic.i = '4E';  ascii.i = '2B'  /* + */
    i = i+1;  ebcdic.i = '4F';  ascii.i = '21'  /* | */
    i = i+1;  ebcdic.i = '5B';  ascii.i = '24'  /* $ */
    i = i+1;  ebcdic.i = '5C';  ascii.i = '2A'  /* * */
    i = i+1;  ebcdic.i = 'E0';  ascii.i = '5C'  /* \ */
    i = i+1;  ebcdic.i = '5D';  ascii.i = '29'  /* ) */
    i = i+1;  ebcdic.i = '7A';  ascii.i = '3A'  /* : */
    i = i+1;  ebcdic.i = '7B';  ascii.i = '23'  /* # */
    i = i+1;  ebcdic.i = '7F';  ascii.i = '22'  /* " */
    /* otherwise => default
    i = i+1;  ebcdic.i = '00'                   // nul
    i = i+1;  ebcdic.i = 'C0';  ascii.i = '7B'  // positive zero
    i = i+1;  ebcdic.i = 'D0';  ascii.i = '7D'  // negative zero
    */
    /* load */
    toAscii. = '23'  /* default: # */
    do j = 1 to i
        ebcdicHex = ebcdic.j
        asciiHex = ascii.j
        toAscii.ebcdicHex = ascii.j
    end
    return

ebcdic2AsciiNum:
        arg ebcdicString
    ebcdicHex = c2x(ebcdicString)
    if (length(ebcdicHex) > 1)
    then ebcSignChar = substr(ebcdicHex, length(ebcdicHex) - 1, 1)
    else ebcSignChar = ''
    asciiHex = replaceText(ebcdicHex, 'F', '')
    select
    when (ebcSignChar = 'C')
    then do
        asciiHex = replaceText(asciiHex, 'C', '')
        asciiHex = asciiHex'C'
    end
    when (ebcSignChar = 'D')
    then do
        asciiHex = replaceText(asciiHex, 'D', '')
        asciiHex = asciiHex'D'
    end
    otherwise
        nop
    end
    asciiString = asciiHex
    return asciiString

ebcdic2AsciiAlpha:
        arg ebcdicString
    ebcdicHex = c2x(ebcdicString)
    asciiHex = ''
    do i = 1 by 2 to length(ebcdicHex)
        ebcdicByte = substr(ebcdicHex, i, 2)
        asciiHex = asciiHex || toAscii.ebcdicByte
    end
    asciiString = '#'
    if (datatype(asciiHex, 'X') = SYS_TRUE)
    then do
        asciiString = x2c(asciiHex)
    end
    else do
        say 'ebcdicHex 'ebcdicHex
        say 'asciiHex 'asciiHex
    end
    return asciiString

replaceText: procedure
    parse arg oldString, ,
              oldText, ,
              newText
    newString = oldString
    oldTextPointer = index(newString, oldText)
    replacingsCounter = 0
    do while (oldTextPointer > 0 & replacingsCounter <= 100)
        RTSubstrTo = oldTextPointer - 1
        RTSubstrFrom = oldTextPointer + length( oldText )
        newString = substr(newString, 1, RTSubstrTo) ,
                || newText ,
                || substr(newString, RTSubstrFrom)
        oldTextPointer = index(newString, oldText)
        replacingsCounter = replacingsCounter + 1
    end
    if (replacingsCounter > 100)
    then do
        say 'replaceText'
        say 'replacingsCounter > 100'
        say 'replacingsCounter ['replacingsCounter']'
        say 'newString ['newString']'
        call exitError
    end
    return newString

isANumber: procedure
        parse arg data
    if (dataType(data) = 'NUM' ,
            & index(data, ' ') = 0 ,
            & index(data, 'E') = 0)
    then return TRUE
    return FALSE

binFlip: procedure
        parse arg binString
    binString = replaceText(binString, '0', '?')
    binString = replaceText(binString, '1', '0')
    binString = replaceText(binString, '?', '1')
    return binString

checkRc: procedure
        parse arg procLabel,
                  returnCode,
                  okCode
    if (returnCode <> okCode)
    then do
        say
        say
        say
        say procLabel':'
        say 'returnCode ['returnCode']'
        call exitError
    end
    return

/*
 *
 * proFile
 *
 */

openReadProFile:
    select
    when (os = 'WIN64') then call openReadProFileWIN
    when (os = 'TSO')   then call openReadProFileTSO
    end
    if (openReadProFileStatus = 'ok')
    then do
        call setProFileRecsCounter
    end
    return

openReadProFileWIN:
    proFileRc = stream(PRO_FILE, 'C', 'OPEN READ')
    if (proFileRc = 'READY:')
    then do
        openReadProFileStatus = 'ok'
        call initProFileReadCursor
    end
    return

initProFileReadCursor:
    proFileRc = linein(PRO_FILE, 1, 0)
    call checkRc 'initProFileReadCursor' ,
            proFileRc ''
    return

openReadProFileTSO:
    if (sysdsn(PRO_FILE_Q) = 'OK')
    then do
        call allocateProFile
        openReadProFileStatus = 'ok'
    end
    else do
        call allocateNewProFile
        openReadProFileStatus = 'ko'
    end
    return

allocateProFile:
    call closeProFile
    address tso
        'allocate ddname(proDD) dataset('PRO_FILE_Q') shr'
    address
    call checkRc 'allocateProFile' ,
            rc 0
    return

allocateNewProFile:
    call closeProFile
    address tso
        'allocate ddname(proDD) dataset('PRO_FILE_Q') new' ,
                'space(50 1) tracks release' ,
                'lrecl(80) block(1600) recfm(f b) dsorg(ps)'
    address
    call checkRc 'allocateNewProFile' ,
            rc 0
    return

setProFileRecsCounter:
    select
    when (os = 'WIN64') then call setProFileRecsCounterWIN
    when (os = 'TSO')   then call setProFileRecsCounterTSO
    end
    return

setProFileRecsCounterWIN:
    proFileRecsCounter = lines(PRO_FILE, 'C')
    return

setProFileRecsCounterTSO:
    proFileStem. = ''
    address tso
        'execio * diskr proDD (stem proFileStem.'
        'execio 0 diskr proDD (finis) '
    address
    call checkRc 'setProFileRecsCounterTSO' ,
            rc 0
    proFileRecsCounter = proFileStem.0
    proFileStem. = ''
    return

readProRecord:
    select
    when (os = 'WIN64') then call readProRecordWIN
    when (os = 'TSO')   then call readProRecordTSO
    end
    return

readProRecordWIN:
    proRecord = linein(PRO_FILE)
    return

readProRecordTSO:
    proFileStem. = ''
    address tso
        'execio 1 diskr proDD (stem proFileStem.'
    address
    call checkRc 'readProRecordTSO' ,
            rc 0
    proRecord = proFileStem.1
    return

openWriteProFile:
    select
    when (os = 'WIN64') then call openWriteProFileWIN
    when (os = 'TSO')   then nop
    end
    return

openWriteProFileWIN:
    proFileRc = stream(PRO_FILE, 'C', 'OPEN WRITE')
    call checkRc 'openWriteProFileWIN' ,
            proFileRc 'READY:'
    call initProFileWriteCursor
    return

initProFileWriteCursor:
    proFileRc = lineout(PRO_FILE, , 1)
    call checkRc 'initProFileWriteCursor' ,
            proFileRc 0
    proFileRc = 'ok'
    return

writeProRecord:
    select
    when (os = 'WIN64') then call writeProRecordWIN
    when (os = 'TSO')   then call writeProRecordTSO
    end
    return

writeProRecordWIN:
    proFileRc = lineout(PRO_FILE, proRecord)
    call checkRc 'writeProRecordWIN' ,
            proFileRc 0
    return

writeProRecordTSO:
    proFileStem. = ''
    proFileStem.1 = proRecord
    proFileStem.0 = 1
    address tso
        'execio 1 diskw proDD (stem proFileStem.'
    address
    call checkRc 'writeProRecordTSO' ,
            rc 0
    return

startPRO_FILE:
    call closeProFile
    select
    when (os = 'WIN64') then call startPRO_FILE_WIN
    when (os = 'TSO')   then call startPRO_FILE_TSO
    end
    return

startPRO_FILE_WIN:
    address system
        'start "" "'PRO_FILE'"'
    address
    call checkRc 'startPRO_FILE_WIN' ,
            rc 0
    return

startPRO_FILE_TSO:
    address ispexec
        'edit dataset ('PRO_FILE_Q')'
    address
    if (rc = 4)
    then rc = 0
    call checkRc 'startPRO_FILE_TSO' ,
            rc 0
    return

closeProFile:
    select
    when (os = 'WIN64') then call closeProFileWIN
    when (os = 'TSO')   then call closeProFileTSO
    end
    return

closeProFileWIN:
    proFileRc = stream(PRO_FILE, 'C', 'CLOSE')
    return

closeProFileTSO:
    msgStatus = msg(off)
    address tso
        'execio 0 diskr proDD (finis) '
        'free ddname(proDD)'
    address
    address tso
        'free ddname(proDD)'
    address
    msgStatus = msg(on)
    if (rc = 12)
    then rc = 0
    call checkRc 'closeProFileTSO' ,
            rc 0
    return

/*
 *
 * dataFile
 *
 */

openReadDataFile:
    select
    when (os = 'WIN64') then call openReadDataFileWIN
    when (os = 'TSO')   then call openReadDataFileTSO
    end
    if (openReadDataFileStatus = 'ok')
    then do
        call setDataFileRecsCounter
    end
    return

openReadDataFileWIN:
    dataFileRc = stream(DATA_FILE, 'C', 'OPEN READ')
    call checkRc 'openReadDataFileWIN' ,
            dataFileRc 'READY:'
    openReadDataFileStatus = 'ok'
    call initDataFileReadCursor
    return

initDataFileReadCursor:
    dataFileRc = linein(DATA_FILE, 1, 0)
    call checkRc 'initDataFileReadCursor' ,
            dataFileRc ''
    return

openReadDataFileTSO:
    DATA_FILE_Q = "'" || DATA_FILE || "'"
    call allocateDataFile
    openReadDataFileStatus = 'ok'
    return

allocateDataFile:
    call closeDataFile
    address tso
        'allocate ddname(dataDD) dataset('DATA_FILE_Q') shr'
    address
    call checkRc 'allocateDataFile' ,
            rc 0
    return

setDataFileRecsCounter:
    select
    when (os = 'WIN64') then call setDataFileRecsCounterWIN
    when (os = 'TSO')   then call setDataFileRecsCounterTSO
    end
    return

setDataFileRecsCounterWIN:
    dataFileRecsCounter = lines(DATA_FILE, 'C')
    return

setDataFileRecsCounterTSO:
    dataFileStem. = ''
    address tso
        'execio * diskr proDD (stem dataFileStem.'
        'execio 0 diskr proDD (finis) '
    address
    call checkRc 'setDataFileRecsCounterTSO' ,
            rc 0
    dataFileRecsCounter = dataFileStem.0
    dataFileStem. = ''
    return

readDataFileRecord:
    select
    when (os = 'WIN64') then call readDataFileRecordWIN
    when (os = 'TSO')   then call readDataFileRecordTSO
    end
    return

readDataFileRecordWIN:
    dataFileRecord = linein(DATA_FILE)
    return

readDataFileRecordTSO:
    dataFileStem. = ''
    address tso
        'execio 1 diskr dataDD (stem dataFileStem.'
    address
    if (rc = 2)
    then rc = 0
    call checkRc 'readDataFileRecordTSO' ,
            rc 0
    dataFileRecord = dataFileStem.1
    return

closeDataFile:
    select
    when (os = 'WIN64') then call closeDataFileWIN
    when (os = 'TSO')   then call closeDataFileTSO
    end
    return

closeDataFileWIN:
    dataFileRc = stream(DATA_FILE, 'C', 'CLOSE')
    return

closeDataFileTSO:
    msgStatus = msg(off)
    address tso
        'execio 0 diskr dataDD (finis) '
        'free ddname(dataDD)'
    address
    msgStatus = msg(on)
    if (rc = 12)
    then rc = 0
    call checkRc 'closeDataFileTSO' ,
            rc 0
    return

/*
 *
 * copyFile
 *
 */

openReadCopyFile:
    select
    when (os = 'WIN64') then call openReadCopyFileWIN
    when (os = 'TSO')   then call openReadCopyFileTSO
    end
    if (openReadCopyFileStatus = 'ok')
    then do
        call setCopyRecsCounter
    end
    return

openReadCopyFileWIN:
    copyFileRc = stream(COPY_FILE, 'C', 'OPEN READ')
    call checkRc 'openReadCopyFileWIN' ,
            copyFileRc 'READY:'
    openReadCopyFileStatus = 'ok'
    call initCopyFileReadCursor
    return

initCopyFileReadCursor:
    copyFileRc = linein(COPY_FILE, 1, 0)
    call checkRc 'initCopyFileReadCursor' ,
            copyFileRc ''
    return

openReadCopyFileTSO:
    COPY_FILE_Q = "'"COPY_FILE"'"
    call allocateCopyFile
    openReadCopyFileStatus = 'ok'
    return

allocateCopyFile:
    call closeCopyFile
    address tso
        'allocate ddname(copyDD) dataset('COPY_FILE_Q') shr'
    address
    call checkRc 'allocateCopyFile' ,
            rc 0
    return

setCopyRecsCounter:
    select
    when (os = 'WIN64') then call setCopyRecsCounterWIN
    when (os = 'TSO')   then call setCopyRecsCounterTSO
    end
    return

setCopyRecsCounterWIN:
    copyRecsCounter = lines(COPY_FILE, 'C')
    return

setCopyRecsCounterTSO:
    copyFileStem. = ''
    address tso
        'execio * diskr copyDD (stem copyFileStem.'
        'execio 0 diskr copyDD (finis) '
    address
    call checkRc 'setCopyRecsCounterTSO' ,
            rc 0
    copyRecsCounter = copyFileStem.0
    copyFileStem. = ''
    return

readCopyRecord:
    select
    when (os = 'WIN64') then call readCopyRecordWIN
    when (os = 'TSO')   then call readCopyRecordTSO
    end
    return

readCopyRecordWIN:
    copyRecord = linein(COPY_FILE)
    return

readCopyRecordTSO:
    copyFileStem. = ''
    address tso
        'execio 1 diskr copyDD (stem copyFileStem.'
    address
    call checkRc 'readCopyRecordTSO' ,
            rc 0
    copyRecord = copyFileStem.1
    return

closeCopyFile:
    select
    when (os = 'WIN64') then call closeCopyFileWIN
    when (os = 'TSO')   then call closeCopyFileTSO
    end
    return

closeCopyFileWIN:
    copyFileRc = stream(copyFile, 'C', 'CLOSE')
    return

closeCopyFileTSO:
    msgStatus = msg(off)
    address tso
        'execio 0 diskr copyDD (finis) '
        'free ddname(copyDD)'
    address
    msgStatus = msg(on)
    if (rc = 12)
    then rc = 0
    call checkRc 'closeCopyFileTSO' ,
            rc 0
    return

/*
 *
 * selRecsFile
 *
 */

openReadSelRecsFile:
    select
    when (os = 'WIN64') then call openReadSelRecsFileWIN
    when (os = 'TSO')   then call openReadSelRecsFileTSO
    end
    call setSelRecsRecsCounter
    return

openReadSelRecsFileWIN:
    selRecsFileRc = stream(selRecsFile, 'C', 'OPEN READ')
    openReadSelRecsFileStatus = 'ok'
    call initSelRecsFileReadCursor
    return

initSelRecsFileReadCursor:
    selRecsFileRc = linein(selRecsFile, 1, 0)
    call checkRc 'openReadSelRecsFileWIN' ,
            selRecsFileRc ''
    return

openReadSelRecsFileTSO:
    if (sysdsn(selRecsFile) = 'OK')
    then do
        call allocateSelRecsFile
        openReadSelRecsFileStatus = 'ok'
    end
    else do
        call allocateNewSelRecsFile
        openReadSelRecsFileStatus = 'ko'
    end
    return

allocateSelRecsFile:
    call closeSelRecsFile
    address tso
        'allocate ddname(selRcsDD) dataset('selRecsFile') shr'
    address
    call checkRc 'allocateSelRecsFile' ,
            rc 0
    return

allocateNewSelRecsFile:
    call closeSelRecsFile
    address tso
        'allocate ddname(selRcsDD) dataset('selRecsFile') new' ,
                'space(50 1) tracks release' ,
                'lrecl(80) block(1600) recfm(f b) dsorg(ps)'
    address
    call checkRc 'allocateNewSelRecsFile' ,
            rc 0
    return

setSelRecsRecsCounter:
    selRecsRecsCounter = 0
    if (openReadSelRecsFileStatus = 'ok')
    then do
        select
        when (os = 'WIN64') then call setSelRecsRecsCounterWIN
        when (os = 'TSO')   then call setSelRecsRecsCounterTSO
        end
    end
    return

setSelRecsRecsCounterWIN:
    selRecsRecsCounter = lines(selRecsFile, 'C')
    return

setSelRecsRecsCounterTSO:
    selRecsFileStem. = ''
    address tso
        'execio * diskr selRcsDD (stem selRecsFileStem.'
        'execio 0 diskr selRcsDD (finis) '
    address
    call checkRc 'setSelRecsRecsCounterTSO' ,
            rc 0
    selRecsRecsCounter = selRecsFileStem.0
    selRecsFileStem. = ''
    return

openWriteSelRecsFile:
    select
    when (os = 'WIN64') then call openWriteSelRecsFileWIN
    when (os = 'TSO')   then call openWriteSelRecsFileTSO
    end
    return

openWriteSelRecsFileWIN:
    selRecsFileRc = stream(selRecsFile, 'C', 'OPEN WRITE')
    call checkRc 'openWriteSelRecsFileWIN' ,
            selRecsFileRc 'READY:'
    call initSelRecsFileWriteCursor
    return

initSelRecsFileWriteCursor:
    selRecsFileRc = lineout(selRecsFile, , 1)
    call checkRc 'initSelRecsFileWriteCursor' ,
            rc 0
    return

openWriteSelRecsFileTSO:
    call allocateSelRecsFile
    return

readSelRecsRecord:
    select
    when (os = 'WIN64') then call readSelRecsRecordWIN
    when (os = 'TSO')   then call readSelRecsRecordTSO
    end
    return

readSelRecsRecordWIN:
    selectRecord = linein(selRecsFile)
    return

readSelRecsRecordTSO:
    selRecsFileStem. = ''
    address tso
        'execio 1 diskr selRcsDD (stem selRecsFileStem.'
    address
    call checkRc 'readSelRecsRecordTSO' ,
            rc 0
    selectRecord = selRecsFileStem.1
    return

writeSelRecsRecord:
    select
    when (os = 'WIN64') then call writeSelRecsRecordWIN
    when (os = 'TSO')   then call writeSelRecsRecordTSO
    end
    return

writeSelRecsRecordWIN:
    selRecsFileRc = lineout(selRecsFile, selRecsRecord)
    call checkRc 'writeSelRecsRecord' ,
            selRecsFileRc 0
    return

writeSelRecsRecordTSO:
    selRecsFileStem. = ''
    selRecsFileStem.1 = selRecsRecord
    selRecsFileStem.0 = 1
    address tso
        'execio 1 diskw selRcsDD (stem selRecsFileStem.'
    address
    call checkRc 'writeSelRecsRecordTSO' ,
            rc 0
    return

startSelRecsFile:
    call closeSelRecsFile
    select
    when (os = 'WIN64') then call startSelRecsFileWIN
    when (os = 'TSO')   then call startSelRecsFileTSO
    end
    return

startSelRecsFileWIN:
    address system
        'start "" "'selRecsFile'"'
    address
    call checkRc 'startSelRecsFileWIN' ,
            rc 0
    return

startSelRecsFileTSO:
    address ispexec
        'edit dataset ('selRecsFile')'
    address
    if (rc = 4)
    then rc = 0
    call checkRc 'startSelRecsFileTSO' ,
            rc 0
    return

closeSelRecsFile:
    select
    when (os = 'WIN64') then call closeSelRecsFileWIN
    when (os = 'TSO')   then call closeSelRecsFileTSO
    end
    return

closeSelRecsFileWIN:
    selRecsFileRc = stream(selRecsFile, 'C', 'CLOSE')
    return

closeSelRecsFileTSO:
    msgStatus = msg(off)
    address tso
        'execio 0 diskr selRcsDD (finis) '
        'free ddname(selRcsDD)'
    address
    msgStatus = msg(on)
    if (rc = 12)
    then rc = 0
    call checkRc 'closeSelRecsFileTSO' ,
            rc 0
    return

/*
 *
 * selColsFile
 *
 */

openReadSelColsFile:
    select
    when (os = 'WIN64') then call openReadSelColsFileWIN
    when (os = 'TSO')   then call openReadSelColsFileTSO
    end
    call setSelColsRecsCounter
    return

openReadSelColsFileWIN:
    selColsFileRc = stream(selColsFile, 'C', 'OPEN READ')
    openReadSelColsFileStatus = 'ok'
    call initSelColsFileReadCursor
    return

initSelColsFileReadCursor:
    selColsFileRc = linein(selColsFile, 1, 0)
    call checkRc 'initSelColsFileReadCursor' ,
            selColsFileRc ''
    return

openReadSelColsFileTSO:
    if (sysdsn(selColsFile) = 'OK')
    then do
        call allocateSelColsFile
        openReadSelColsFileStatus = 'ok'
    end
    else do
        call allocateNewSelColsFile
        openReadSelColsFileStatus = 'ko'
    end
    return

allocateSelColsFile:
    call closeSelColsFile
    address tso
        'allocate ddname(selClsDD) dataset('selColsFile') shr'
    address
    call checkRc 'allocateSelColsFile' ,
            rc 0
    return

allocateNewSelColsFile:
    call closeSelColsFile
    address tso
        'allocate ddname(selClsDD) dataset('selColsFile') new' ,
                'space(50 1) tracks release' ,
                'lrecl(80) block(1600) recfm(f b) dsorg(ps)'
    address
    call checkRc 'allocateNewSelColsFile' ,
            rc 0
    return

setSelColsRecsCounter:
    selColsRecsCounter = 0
    if (openReadSelColsFileStatus = 'ok')
    then do
        select
        when (os = 'WIN64') then call setSelColsRecsCounterWIN
        when (os = 'TSO')   then call setSelColsRecsCounterTSO
        end
    end
    return

setSelColsRecsCounterWIN:
    selColsRecsCounter = lines(selColsFile, 'C')
    return

setSelColsRecsCounterTSO:
    selColsStem. = ''
    address tso
        'execio * diskr selClsDD (stem selColsStem.'
        'execio 0 diskr selClsDD (finis) '
    address
    call checkRc 'setSelColsRecsCounterTSO' ,
            rc 0
    selColsRecsCounter = selColsStem.0
    selColsStem. = ''
    return

openWriteSelColsFile:
    select
    when (os = 'WIN64') then call openWriteSelColsFileWIN
    when (os = 'TSO')   then call openWriteSelColsFileTSO
    end
    return

openWriteSelColsFileWIN:
    selColsFileRc = stream(selColsFile, 'C', 'OPEN WRITE')
    call checkRc 'openWriteSelColsFileWIN' ,
            selColsFileRc 'READY:'
    call initSelColsFileWriteCursor
    return

initSelColsFileWriteCursor:
    selColsFileRc = lineout(selColsFile, , 1)
    call checkRc 'initSelColsFileWriteCursor' ,
            selColsFileRc 0
    return

openWriteSelColsFileTSO:
    call allocateSelColsFile
    return

readSelColsRecord:
    select
    when (os = 'WIN64') then call readSelColsRecordWIN
    when (os = 'TSO')   then call readSelColsRecordTSO
    end
    return

readSelColsRecordWIN:
    selColsRecord = linein(selColsFile)
    return

readSelColsRecordTSO:
    selColsFileStem. = ''
    address tso
        'execio 1 diskr selClsDD (stem selColsFileStem.'
    address
    call checkRc 'readSelColsRecordTSO' ,
            rc 0
    selColsRecord = selColsFileStem.1
    return

writeSelColsRecord:
    select
    when (os = 'WIN64') then call writeSelColsRecordWIN
    when (os = 'TSO')   then call writeSelColsRecordTSO
    end
    return

writeSelColsRecordWIN:
    selColsFileRc = lineout(selColsFile, selColsRecord)
    call checkRc 'writeSelColsRecordWIN' ,
            selColsFileRc 0
    return

writeSelColsRecordTSO:
    selColsFileStem. = ''
    selColsFileStem.1 = selColsRecord
    selColsFileStem.0 = 1
    address tso
        'execio 1 diskw selClsDD (stem selColsFileStem.'
    address
    call checkRc 'writeSelColsRecordTSO' ,
            rc 0
    return

startSelColsFile:
    call closeSelColsFile
    select
    when (os = 'WIN64') then call startSelColsFileWIN
    when (os = 'TSO')   then call startSelColsFileTSO
    end
    return

startSelColsFileWIN:
    address system
        'start "" "'selColsFile'"'
    address
    call checkRc 'startSelColsFileWIN' ,
            rc 0
    return

startSelColsFileTSO:
    address ispexec
        'edit dataset ('selColsFile')'
    address
    if (rc = 4)
    then rc = 0
    call checkRc 'startSelColsFileTSO' ,
            rc 0
    return

closeSelColsFile:
    select
    when (os = 'WIN64') then call closeSelColsFileWIN
    when (os = 'TSO')   then call closeSelColsFileTSO
    end
    return

closeSelColsFileWIN:
    selColsFileRc = stream(selColsFile, 'C', 'CLOSE')
    return

closeSelColsFileTSO:
    msgStatus = msg(off)
    address tso
        'execio 0 diskr selClsDD (finis) '
        'free ddname(selClsDD)'
    address
    msgStatus = msg(on)
    if (rc = 12)
    then rc = 0
    call checkRc 'closeSelColsFileTSO' ,
            rc 0
    return

/*
 *
 * outCopyFile
 *
 */

openWriteOutCopyFile:
    select
    when (os = 'WIN64') then call openWriteOutCopyFileWIN
    when (os = 'TSO')   then call openWriteOutCopyFileTSO
    end
    return

openWriteOutCopyFileWIN:
    outCopyFileRc = stream(outCopyFile, 'C', 'OPEN WRITE')
    call checkRc 'openWriteOutCopyFileWIN' ,
            outCopyFileRc 'READY:'
    call initOutCopyFileWriteCursor
    return

initOutCopyFileWriteCursor:
    outCopyFileRc = lineout(outCopyFile, , 1)
    call checkRc 'initOutCopyFileWriteCursor' ,
            outCopyFileRc 0
    return

openWriteOutCopyFileTSO:
    if (sysdsn(outCopyFile) = 'OK')
    then do
        call allocateOutCopyFile
    end
    else do
        call allocateNewOutCopyFile
    end
    return

allocateOutCopyFile:
    call closeOutCopyFile
    address tso
        'allocate ddname(outCpyDD) dataset('outCopyFile') shr'
    address
    call checkRc 'allocateOutCopyFile' ,
            rc 0
    return

allocateNewOutCopyFile:
    call closeOutCopyFile
    address tso
        'allocate ddname(outCpyDD) dataset('outCopyFile') new' ,
                'space(50 1) tracks release' ,
                'lrecl('MAX_LRECL') block(1600) recfm(f b) dsorg(ps)'
    address
    call checkRc 'allocateNewOutCopyFile' ,
            rc 0
    return

writeOutCopyRecord:
    select
    when (os = 'WIN64') then call writeOutCopyRecordWIN
    when (os = 'TSO')   then call writeOutCopyRecordTSO
    end
    return

writeOutCopyRecordWIN:
    outCopyFileRc = lineout(outCopyFile, outCopyRecord)
    call checkRc 'writeOutCopyRecordWIN' ,
            outCopyFileRc 0
    return

writeOutCopyRecordTSO:
    outCopyFileStem. = ''
    outCopyFileStem.1 = outCopyRecord
    outCopyFileStem.0 = 1
    address tso
        'execio 1 diskw outCpyDD (stem outCopyFileStem.'
    address
    call checkRc 'writeOutCopyRecordTSO' ,
            rc 0
    return

closeOutCopyFile:
    select
    when (os = 'WIN64') then call closeOutCopyFileWIN
    when (os = 'TSO')   then call closeOutCopyFileTSO
    end
    return

closeOutCopyFileWIN:
    outCopyFileRc = stream(outCopyFile, 'C', 'CLOSE')
    return

closeOutCopyFileTSO:
    msgStatus = msg(off)
    address tso
        'execio 0 diskw outCpyDD (finis'
        'free ddname(outCpyDD)'
    address
    msgStatus = msg(on)
    if (rc = 12)
    then rc = 0
    call checkRc 'closeOutCopyFileTSO' ,
            rc 0
    return

startOutCopyFile:
    call closeOutCopyFile
    select
    when (os = 'WIN64') then call startOutCopyFileWIN
    when (os = 'TSO')   then call startOutCopyFileTSO
    end
    return

startOutCopyFileWIN:
    address system
        'start "" "'outCopyFile'"'
    address
    call checkRc 'startOutCopyFileWIN' ,
            rc 0
    return

startOutCopyFileTSO:
    address ispexec
        'edit dataset ('outCopyFile')'
    address
    if (rc = 4)
    then rc = 0
    call checkRc 'startOutCopyFileTSO' ,
            rc 0
    return

/*
 *
 * outDataFile
 *
 */

openWriteOutDataFile:
    select
    when (os = 'WIN64') then call openWriteOutDataFileWIN
    when (os = 'TSO')   then call openWriteOutDataFileTSO
    end
    return

openWriteOutDataFileWIN:
    outDataFileRc = stream(outDataFile, 'C', 'OPEN WRITE')
    call checkRc 'openWriteOutDataFileWIN' ,
            outDataFileRc 'READY:'
    call initOutDataFileWriteCursor
    return

initOutDataFileWriteCursor:
    outDataFileRc = lineout(outDataFile, , 1)
    call checkRc 'initOutDataFileWriteCursor' ,
            outDataFileRc 0
    return

openWriteOutDataFileTSO:
    if (sysdsn(outDataFile) = 'OK')
    then do
        call allocateOutDataFile
    end
    else do
        call allocateNewOutDataFile
    end
    return

allocateOutDataFile:
    call closeOutDataFile
    address tso
        'allocate ddname(outDtaDD) dataset('outDataFile') shr'
    address
    call checkRc 'allocateOutDataFile' ,
            rc 0
    return

allocateNewOutDataFile:
    call closeOutDataFile
    address tso
        'allocate ddname(outDtaDD) dataset('outDataFile') new' ,
                'space(50 1) tracks release' ,
                'lrecl(400) block(1600) recfm(f b) dsorg(ps)'
    address
    call checkRc 'allocateNewOutDataFile' ,
            rc 0
    return

writeOutputRecord:
    select
    when (os = 'WIN64') then call writeOutputRecordWIN
    when (os = 'TSO')   then call writeOutputRecordTSO
    end
    return

writeOutputRecordWIN:
    outDataFileRc = lineout(outDataFile, outputRecord)
    call checkRc 'writeOutputRecordWIN' ,
            outDataFileRc 0
    return

writeOutputRecordTSO:
    outputFileStem. = ''
    outputFileStem.1 = outputRecord
    outputFileStem.0 = 1
    address tso
        'execio 1 diskw outDtaDD (stem outputFileStem.'
    address
    call checkRc 'writeOutputRecordTSO' ,
            rc 0
    return

closeOutDataFile:
    select
    when (os = 'WIN64') then call closeOutDataFileWIN
    when (os = 'TSO')   then call closeOutDataFileTSO
    end
    return

closeOutDataFileWIN:
    outDataFileRc = stream(outDataFile, 'C', 'CLOSE')
    return

closeOutDataFileTSO:
    msgStatus = msg(off)
    address tso
        'execio 0 diskw outDtaDD (finis'
        'free ddname(outDtaDD)'
    address
    msgStatus = msg(on)
    if (rc = 12)
    then rc = 0
    call checkRc 'closeOutDataFileTSO' ,
            rc 0
    return

startOutDataFile:
    call closeOutDataFile
    select
    when (os = 'WIN64') then call startOutDataFileWIN
    when (os = 'TSO')   then call startOutDataFileTSO
    end
    return

startOutDataFileWIN:
    address system
        'start "" "'outDataFile'"'
    address
    call checkRc 'startOutDataFileWIN' ,
            rc 0
    return

startOutDataFileTSO:
    address ispexec
        'edit dataset ('outDataFile')'
    address
    if (rc = 4)
    then rc = 0
    call checkRc 'startOutDataFileTSO' ,
            rc 0
    return

/*
 *
 * exit / error
 *
 */

exitBlip:
    exit 0

exitError:
    exit 99
