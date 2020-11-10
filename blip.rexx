/* Rexx */

/* DONE
 - I/O da file anche per opzioni e file
 - gestire commenti
*/

/* DOING
*/

/* TODO
 - da portare e testare su MF
 - da gestire in formato ascii se/quando ci sarà occasione
    doppia gestione segno per signed - parametro SIGNED_ZONED_FORMAT
    formattazione comp
    lettura dati con redefines su ascii
    regole
*/

/* debug tools
    trace(?l)
    trace(?i)
    trace(?a)
    signal on novalue
    signal on error
    signal on syntax
    signal on halt
    SIMULATE_TSO_FROM_WIN64 = TRUE
*/

main:
    call checkOs
    call setConstants
    call initSelectFeatures
    select
    when (os = 'WIN64') then nop
    when (os = 'TSO')   then call checkWorkLibrariesTSO
    end
    command = ''
    do while (SYS_TRUE)
        call checkProfile
        do while (proFileStatus = 'ko')
            call startPRO_FILE
            call checkProfile
        end
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
    return

initSelectFeatures:
    call initSelRecsFeatures
    call initSelColsFeatures
    selRecsFileWritten = FALSE
    selColsFileWritten = FALSE
    call setAllSelColsVisible
    return

checkWorkLibrariesTSO:
    if (sysdsn(selRecsFile) <> 'OK')
    then do
        call allocateSelRecsDirectory
    end
    if (sysdsn(selColsFile) <> 'OK')
    then do
        call allocateSelColsDirectory
    end
    return

editConsole:
    call startPRO_FILE
    return

checkProfile:
    call closeProFile
    call openReadProFile
    if (openReadProFileStatus <> 'ok')
    then do
        call initProfile
    end
    call getProfileOptions
    call getCOPY_NAME
    call setFiles
    call checkCurrentFiles
    call setProFileStatus
    return

initProfile:
    DATA_FILE         = '?'
    DATA_ENCODING     = '?'
    DATA_DSORG        = '?'
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
    proRecord = '*     encoding: ebcdic/ascii'
    call writeProRecord
    proRecord = '          'DATA_ENCODING
    call writeProRecord
    proRecord = '*     organisation: lseq/seq'
    call writeProRecord
    proRecord = '          'DATA_DSORG
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
    return

getProfileOptions:
    proFileParmCursor = 0
    do proFileRecordsCursor = 1 to proFileRecordsCounter
        call readProRecord
        if (substr(proRecord, 1, 1) <> '*')
        then do
            proFileParmCursor = proFileParmCursor + 1
            select
            when (proFileParmCursor = 1)    
            then DATA_FILE         = strip(proRecord)
            when (proFileParmCursor = 2)    
            then DATA_ENCODING     = strip(proRecord)
            when (proFileParmCursor = 3)
            then DATA_DSORG        = strip(proRecord)
            when (proFileParmCursor = 4)
            then COPY_FILE         = strip(proRecord)
            when (proFileParmCursor = 5)
            then MAX_DATA_RECORDS  = strip(proRecord)
            when (proFileParmCursor = 6)
            then MAX_ALPHA_LENGTH  = strip(proRecord)
            when (proFileParmCursor = 7)
            then RESTART_LOG_LEVEL = strip(proRecord)
            end
        end
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
    selRecsDirectory = BLIP_DIRECTORY'\selRecs'
    selColsDirectory = BLIP_DIRECTORY'\selCols'
    selRecsFile = selRecsDirectory'\'COPY_NAME'.txt'
    selColsFile = selColsDirectory'\'COPY_NAME'.txt'
    outCopyFile = BLIP_DIRECTORY'\outCopyFile.txt'
    outDataFile = BLIP_DIRECTORY'\outDataFile.txt'
    return

setFilesTSO:
    selRecsDirectory = BLIP_DIRECTORY'.SELRECS'
    selColsDirectory = BLIP_DIRECTORY'.SELCOLS'
    selRecsFile = selRecsDirectory'('COPY_NAME')'
    selColsFile = selColsDirectory'('COPY_NAME')'
    outCopyFile = BLIP_DIRECTORY'.OUTCOPYF'
    outDataFile = BLIP_DIRECTORY'.OUTDATAF'
    return

viewCopy:
    call setNormsList
    call setMonosList
    call setFieldsList
    call setOccursFields
    call setEbcdicFromToCols
    /*dg
    call displayFieldsList
    */
    call showCopy
    return

viewData:
    call setNormsList
    call setMonosList
    call setFieldsList
    call setOccursFields
    call setEbcdicFromToCols
    /*dg
    call displayFieldsList
    */
    call showData
    return

checkCurrentFiles:
    call openReadDataFile
    call openReadCopyFile
    call openWriteOutDataFile
    call openWriteOutCopyFile
    call openReadSelRecsFile
    call openReadSelColsFile
    return

setProFileStatus:
    proFileStatus = 'ok'
    select
    when (DATA_FILE = '')
    then do
        say
        say 'blip> checkProfile> data file: empty'
        proFileStatus = 'ko'
    end
    when (DATA_ENCODING <> 'ascii' & DATA_ENCODING <> 'ebcdic')
    then do
        say
        say 'blip> checkProfile> data file encoding: wrong value'
        proFileStatus = 'ko'
    end
    when (DATA_DSORG <> 'seq' & DATA_DSORG <> 'lseq')
    then do
        say
        say 'blip> checkProfile> data file organization: wrong value'
        proFileStatus = 'ko'
    end
    when (COPY_FILE = '')
    then do
        say
        say 'blip> checkProfile> copy file: empty'
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

setAllSelColsVisible:
    selColsConds. = '+'
    return

setNormsList:
    call initCopyFileReadCursor
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
    parse lower var norm norm
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

setFieldsList:
    call initFields
    fieldsCounter = 0
    fillerLabelsCounter = 0
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
    if (fieldLabel = 'filler')
    then do
        fillerLabelsCounter = fillerLabelsCounter + 1
        fieldLabel = fieldLabel'-('fillerLabelsCounter')'
    end
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

showCopy:
    call initOutCopyFileCursor
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

setSelRecs:
    if (selRecsFileWritten = FALSE ,
            & selRecsRecordsCounter = 0)
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
    do selectRecsIndex = 3 to selRecsRecordsCounter
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
                || selRecsFieldLabel'] not found!'
    end
    return

checkAllSelRecsParms:
    call closeSelRecsFile
    call openReadSelRecsFile
    call readSelRecsRecord /* record 1: header */
                           /* record 2: all fields selection */
    do selectRecsIndex = 2 to selRecsRecordsCounter
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
                || '['selRecsOperator'] not allowed!'
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
                || '['selRecsOperator'] not allowed!'
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

setSelCols:
    if (selColsFileWritten = FALSE ,
            & selColsRecordsCounter = 0)
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
    do selColsRecsIndex = 1 to selColsRecordsCounter
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
            & allSelColsParmsOk = TRUE)
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
                || ' not found!'
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
                || operator'] not allowed!'
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
                || operator'] not allowed!'
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

showData1:
    call initOutDataFileWriteCursor
    call resetDataFile
    call selectDataRecords
    call startOutDataFile
    return

resetDataFile:
    call closeDataFile
    call openReadDataFile
    dataCursor = 1
    call resetDataCursor
    return

resetDataCursor:
    charinReset = charin(DATA_FILE, dataCursor, 0)
    return

selectDataRecords:
    if (os = 'WIN64' ,
            & SIMULATE_TSO_FROM_WIN64 = TRUE)
    then do
        os = 'TSO'
        dataFileRecordsCounter = lines(DATA_FILE, 'C')
    end
    select
    when (os = 'WIN64') then call selectDataRecordsWIN
    when (os = 'TSO')   then call selectDataRecordsTSO
    end
    if (os = 'TSO' ,
            & SIMULATE_TSO_FROM_WIN64 = TRUE)
    then do
        os = 'WIN64'
    end
    return

selectDataRecordsWIN:
    formatDataValues = FALSE
    outputRecordsCounter = 1
    lastOccurredSCIndex = -1
    do while (chars(DATA_FILE) > 0 ,
            & outputRecordsCounter <= MAX_DATA_RECORDS - 1)
        call selectOutputRecordWIN
        if (DATA_DSORG = 'lseq')
        then do
            dataCursor = dataCursor + 1
        end
        outputRecordsCounter = outputRecordsCounter + 1
    end
    return

selectOutputRecordWIN:
    outputRecord = ' '
    dataCheck = charin(DATA_FILE, dataCursor, 1)
    call resetDataCursor
    if (dataCheck <> EMPTY_DATA_RECORD)
    then do
        call selectOutputRecordWIN1
    end
    return

selectOutputRecordWIN1:
    startDataCursor = dataCursor
    call checkAllSelRecsCondsWIN
    if (allSelRecsCondsOccur = TRUE)
    then do
        dataCursor = startDataCursor
        call resetDataCursor
        formatDataValues = TRUE
        if (occurredSCIndex <> lastOccurredSCIndex)
        then do
            call setOutTableHeader
            lastOccurredSCIndex = occurredSCIndex
        end
        call setOutputRecordWIN
        call writeOutputRecord
        formatDataValues = FALSE
    end
    return

checkAllSelRecsCondsWIN:
    fieldsDataCursors. = ''
    selRecsCondsOccs. = FALSE
    occurredSCIndex = 0
    call getAllFieldsValueWIN
    do fieldsIndex = 1 to fieldsCounter ,
            while (chars(DATA_FILE) > 0)
        call retrieveField fieldsIndex
        call checkRedefCursor
        dataValue = ''
        select
        when (fieldPicType = 'group')    then call getGroupValueWIN
        when (fieldPicType = 'alphanum') then call getAlphanumValueWIN
        when (fieldPicType = 'integer' ,
            | fieldPicType = 'decimal')  then call getNumericValueWIN
        end
        do selRecsCondsIndex = 1 to selRecsCondsCounter
            call checkSelRecsConds
        end
        do selColsIndex = 1 to selColsCounter
            call checkSelColsConds
        end
    end
    call setAllSelRecsCondsSwitch
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

getAllFieldsValueWIN:
    /* treated as group */
    allFieldsValue = charin(DATA_FILE, dataCursor, MAX_ALPHA_LENGTH)
    call resetDataCursor
    if (DATA_ENCODING = 'ebcdic')
    then do
        allFieldsValue = ebcdic2AsciiAlpha(allFieldsValue)
    end
    return

checkRedefCursor:
    if (fieldRedefdLabel <> '')
    then do
        dataCursor = fieldsDataCursors.fieldLogLevel.fieldRedefdLabel
        call resetDataCursor
    end
    fieldsDataCursors.fieldLogLevel.fieldLabel = dataCursor
    return

getGroupValueWIN:
    /* treated as alphanum, limited by MAX_ALPHA_LENGTH */
    dataValue = charin(DATA_FILE, dataCursor, MAX_ALPHA_LENGTH)
    call resetDataCursor
    if (DATA_ENCODING = 'ebcdic')
    then do
        dataValue = ebcdic2AsciiAlpha(dataValue)
    end
    return

getAlphanumValueWIN:
    dataValue = charin(DATA_FILE, dataCursor, fieldPicIntsNum)
    if (DATA_ENCODING = 'ebcdic')
    then do
        dataValue = ebcdic2AsciiAlpha(dataValue)
    end
    dataCursor = dataCursor + fieldPicIntsNum
    if (formatDataValues = TRUE)
    then do
        call formatAlphanumValue
    end
    return

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

getNumericValueWIN:
    select
    when (fieldPicCompType = 'zoned')   then call getZonedValue
    when (fieldPicCompType = 'comp-3')  then call getComp3Value
    when (fieldPicCompType = 'comp')    then call getCompValue
    end
    if (formatDataValues = TRUE)
    then do
        select
        when (fieldPicCompType = 'zoned')   then call formatZonedValue
        when (fieldPicCompType = 'comp-3')  then call formatComp3Value
        when (fieldPicCompType = 'comp')    then call formatCompValue
        end
    end
    return

getZonedValue:
    dataValue = charin(DATA_FILE, dataCursor, fieldEbcdicLength)
    select
    when (DATA_ENCODING = 'ascii')
    then do
        if (fieldPicSign = 'signed')
        then do
            call checkSignedZonedFormat
        end
    end
    when (DATA_ENCODING = 'ebcdic')
    then do
        dataValue = ebcdic2AsciiNum(dataValue)
    end
    end
    dataCursor = dataCursor + fieldEbcdicLength
    return

checkSignedZonedFormat:
    /* ??? */
    /* zonedNote
        leggere esadecimale
        esaminare contenuto e verificare gestione segno
        riportare contenuto in formato 12345C/D/F per formattazione */
    select
    when (SIGNED_ZONED_FORMAT = 'hex')
    then do
        /* preserva il codice esadecimale,
            quindi i char sono errati */
    end
    when (SIGNED_ZONED_FORMAT = 'char')
    then do
        /* preserva i chars,
            quindi i codici esadecimali sono errati */
    end
    otherwise
        /* altre eventuali gestioni */
    end
    say 'warning: presente campo numerico zoned - completare gestione'
    return

getComp3Value:
    /* comp3Note */
    select
    when (DATA_ENCODING = 'ebcdic') then call getComp3ValueEbcdic
    when (DATA_ENCODING = 'ascii')  then call getComp3ValueAscii
    end
    return

getComp3ValueEbcdic:
    dataValue = charin(DATA_FILE, dataCursor, fieldEbcdicLength)
    dataValue = c2x(dataValue)
    dataCursor = dataCursor + fieldEbcdicLength
    return

getComp3ValueAscii:
    hexDataChunk = charin(DATA_FILE, dataCursor, DATA_CHUNK_LENGTH)
    dataChunk = c2x(hexDataChunk)
    endOfFieldFound = FALSE
    do dataChunkCursor = 1 to length(dataChunk) ,
            while (endOfFieldFound = FALSE)
        dataChar = substr(dataChunk, dataChunkCursor, 1)
        dataValue = dataValue || dataChar
        if (dataChar = 'F' | dataChar = 'C' | dataChar = 'D')
        then do
            endOfFieldFound = TRUE
        end
    end
    hexDataValue = x2c(dataValue)
    dataCursor = dataCursor + length(hexDataValue)
    return

getCompValue:
    /* compNote */
    select
    when (DATA_ENCODING = 'ebcdic') then call getCompValueEbcdic
    when (DATA_ENCODING = 'ascii')  then call getCompValueAscii
    end
    return

getCompValueEbcdic:
    dataValue = charin(DATA_FILE, dataCursor, fieldEbcdicLength)
    hexValue = c2x(dataValue)
    binValue = x2b(hexValue)
    if (left(binValue, 1) = '0')  /* positive */
    then do
        dataValue = x2d(hexValue)
        dataValue = dataValue'C'  /* set normal sign to format */
    end
    else do
        posiValue = bitxor(dataValue, 'FFFFFFFFFFFFFFFF'x)
        hexValue  = b2x(posiValue)
        dataValue = x2d(hexValue)
        dataValue = dataValue - 1
        dataValue = dataValue'D'  /* set normal sign to format */
    end
    dataCursor = dataCursor + fieldEbcdicLength
    return

getCompValueAscii:
    say 'getCompValueAscii'
    say 'fieldLabel ['fieldLabel']'
    say 'fieldPicCompType ['fieldPicCompType']'
    say 'to be defined with a real case'
    call exitError
    return

formatZonedValue:
    if (fieldPicType = 'decimal')
    then do
        call formatZonedComma
    end
    if (fieldPicSign = 'signed')
    then do
        call formatSign
    end
    call formatNumber
    return

formatZonedComma:
    dataValue = substr(dataValue, 1, fieldPicIntsNum) ,
            || '.'substr(dataValue, fieldPicIntsNum + 1)
    return

formatSign:
    dataValueChar = right(dataValue, 1)
    valueSign = ''
    select
    when (dataValueChar = 'F') then valueSign = ' '
    when (dataValueChar = 'C') then valueSign = '+'
    when (dataValueChar = 'D') then valueSign = '-'
    otherwise
        say 'formatSign'
        say 'field: 'fieldLabel' pic 'fieldPicString' 'fieldPicCompType
        say 'dataValue ['dataValue'] - dataValueChar ['dataValueChar']'
        call exitError
    end
    dataValue = valueSign ,
            || substr(dataValue, 1, length(dataValue) - 1)
    return

formatNumber:
    numValue = strip(format(dataValue, 16))
    if (datatype(dataValue) = 'NUM' ,
            & length(numValue) < fieldPicIntsNum + fieldPicDecsNum)
    then do
        dataValue = format(dataValue, fieldPicIntsNum, ,
                fieldPicDecsNum, 0)
    end
    return

formatComp3Value:
    if (fieldPicType = 'decimal')
    then do
        call formatComp3Comma
    end
    call formatSign
    call formatNumber
    return

formatComp3Comma:
    select
    when (DATA_ENCODING = 'ebcdic') then call formatComp3CommaEbcdic
    when (DATA_ENCODING = 'ascii')  then call formatComp3CommaAscii
    end
    return

formatComp3CommaEbcdic:
    dataValue = substr(dataValue, 1, fieldPicIntsNum) ,
            || '.'substr(dataValue, fieldPicIntsNum + 1)
    return

formatComp3CommaAscii:
    significantFound = FALSE
    decimalZeros = 0
    dataValueDecimalChars = 0
    do dataValueCharsCursor = length(dataValue) - 1 to 1 by -1 ,
            while (dataValueDecimalChars < fieldPicDecsNum)
        dataValueChar = substr(dataValue, dataValueCharsCursor, 1)
        if (dataValueChar = '0' ,
                & significantFound = FALSE)
        then do
            decimalZeros = decimalZeros + 1
            if (decimalZeros = 2)
            then do
                dataValueDecimalChars = dataValueDecimalChars + 1
                decimalZeros = 0
            end
        end
        else do
            dataValueDecimalChars = dataValueDecimalChars + 1
            significantFound = TRUE
        end
    end
    commaIndex = dataValueCharsCursor
    dataValue = substr(dataValue, 1, commaIndex) ,
            || '.'substr(dataValue, commaIndex + 1)
    return

formatCompValue:
    if (fieldPicType = 'decimal')
    then do
        call formatCompComma
    end
    call formatSign
    call formatNumber
    return

formatCompComma:
    select
    when (DATA_ENCODING = 'ebcdic') then call formatComp3CommaEbcdic
    when (DATA_ENCODING = 'ascii')  then call formatCompCommaAscii
    end
    return

formatCompCommaAscii:
    /* ??? verificare posizione virgola */
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

setOutTableHeader:
    outputRecord = ''
    call writeOutputRecord
    do fieldLabelSubsIndex = 1 to fieldsMaxLabelSubs ,
            + 1  /* +1 is a trick to insert an empty line */
        call setOutTableHeaderRecord
    end
    return

setOutTableHeaderRecord:
    outputRecord = ' '
    do fieldsIndex = 1 to fieldsCounter
        call retrieveField fieldsIndex
        if (fieldPicType <> 'group' ,
                & selColsConds.occurredSCIndex.fieldLabel = '+')
        then do
            call showOutTableHeaderField
        end
    end
    call writeOutputRecord
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

setOutputRecordWIN:
    outputRecord = ' '
    do fieldsIndex = 1 to fieldsCounter ,
            while (chars(DATA_FILE) > 0)
        call retrieveField fieldsIndex
        call checkRedefCursor
        dataValue = ''
        select
        when (fieldPicType = 'group')    then nop
        when (fieldPicType = 'alphanum') then call getAlphanumValueWIN
        when (fieldPicType = 'integer' ,
            | fieldPicType = 'decimal')  then call getNumericValueWIN
        end
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

selectDataRecordsTSO:
    formatDataValues = FALSE
    outputRecordsCounter = 1
    lastOccurredSCIndex = -1
    do while (dataFileRecordsCursor < dataFileRecordsCounter ,
            & outputRecordsCounter <= MAX_DATA_RECORDS - 1)
        call readDataFileRecord
        outputRecord = ' '
        call selectOutputRecordTSO
        outputRecordsCounter = outputRecordsCounter + 1
    end
    return

selectOutputRecordTSO:
    call checkAllSelRecsCondsTSO
    if (allSelRecsCondsOccur = TRUE)
    then do
        formatDataValues = TRUE
        if (occurredSCIndex <> lastOccurredSCIndex)
        then do
            call setOutTableHeader
            lastOccurredSCIndex = occurredSCIndex
        end
        call setOutputRecordTSO
        call writeOutputRecord
        formatDataValues = FALSE
    end
    return

checkAllSelRecsCondsTSO:
    selRecsCondsOccs. = FALSE
    occurredSCIndex = 0
    call getAllFieldsValueTSO
    do fieldsIndex = 1 to fieldsCounter
        call retrieveField fieldsIndex
        dataValue = ''
        select
        when (fieldPicType = 'group')    then call getGroupValueTSO
        when (fieldPicType = 'alphanum') then call getAlphanumValueTSO
        when (fieldPicType = 'integer' ,
            | fieldPicType = 'decimal')  then call getNumericValueTSO
        end
        do selRecsCondsIndex = 1 to selRecsCondsCounter
            call checkSelRecsConds
        end
        do selColsIndex = 1 to selColsCounter
            call checkSelColsConds
        end
    end
    call setAllSelRecsCondsSwitch
    return

getAllFieldsValueTSO:
    /* treated as group */
    dataValue = substr(dataFileRecord, fieldEbcdicFromCol, ,
            MAX_ALPHA_LENGTH)
    return

getGroupValueTSO:
    /* treated as alphanum, limited by MAX_ALPHA_LENGTH */
    dataValue = substr(dataFileRecord, fieldEbcdicFromCol, ,
            MAX_ALPHA_LENGTH)
    return

getAlphanumValueTSO:
    dataValue = substr(dataFileRecord, fieldEbcdicFromCol, ,
            fieldEbcdicLength)
    if (formatDataValues = TRUE)
    then do
        call formatAlphanumValue
    end
    return

getNumericValueTSO:
    dataValue = substr(dataFileRecord, fieldEbcdicFromCol, ,
            fieldEbcdicLength)
    select
    when (fieldPicCompType = 'zoned')   then nop
    when (fieldPicCompType = 'comp-3')  then call getComp3ValueTSO
    when (fieldPicCompType = 'comp')    then call getCompValueTSO
    end
    if (formatDataValues = TRUE)
    then do
        select
        when (fieldPicCompType = 'zoned')   then call formatZonedValue
        when (fieldPicCompType = 'comp-3')  then call formatComp3Value
        when (fieldPicCompType = 'comp')    then call formatCompValue
        end
    end
    return

getComp3ValueTSO:
    dataValue = c2x(dataValue)
    dataCursor = dataCursor + fieldEbcdicLength
    return

getCompValueTSO:
    hexValue = c2x(dataValue)
    binValue = x2b(hexValue)
    if (left(binValue, 1) = '0')  /* positive */
    then do
        dataValue = x2d(hexValue)
        dataValue = dataValue'C'  /* set normal sign to format */
    end
    else do
        posiValue = bitxor(dataValue, 'FFFFFFFFFFFFFFFF'x)
        hexValue  = b2x(posiValue)
        dataValue = x2d(hexValue)
        dataValue = dataValue - 1
        dataValue = dataValue'D'  /* set normal sign to format */
    end
    dataCursor = dataCursor + fieldEbcdicLength
    return

setOutputRecordTSO:
    outputRecord = ' '
    do fieldsIndex = 1 to fieldsCounter
        call retrieveField fieldsIndex
        dataValue = ''
        select
        when (fieldPicType = 'group')    then nop
        when (fieldPicType = 'alphanum') then call getAlphanumValueTSO
        when (fieldPicType = 'integer' ,
            | fieldPicType = 'decimal')  then call getNumericValueTSO
        end
        if (fieldPicType <> 'group' ,
                & selColsConds.occurredSCIndex.fieldLabel = '+')
        then do
            call setOutTableDataField
        end
    end
    return

showSelRecsHelp:
    select
    when (os = 'WIN64') then call showSelRecsHelpWIN
    when (os = 'TSO')   then call showSelRecsHelpTSO
    end
    return

showSelRecsHelpWIN:
    say
    say 'blip> viewData> selRecs> help> cond format:'
    say '       [operator][value]       - example: [>=][3.01]'
    say
    say 'blip> viewData> selRecs> help> comparison operators:'
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

showSelRecsHelpTSO:
    say
    say 'blip> viewData> selRecs> help> cond format:'
    say '       [operator][value]       - example: [>=][3.01]'
    say
    say 'blip> viewData> selRecs> help> comparison operators:'
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

/* utilities */

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
    i = i+1;  ebcdic.i = '6B';  ascii.i = '2C'  /* ‘ */
    i = i+1;  ebcdic.i = '5E';  ascii.i = '3B'  /* ; */
    i = i+1;  ebcdic.i = '6D';  ascii.i = '5F'  /* _ */
    i = i+1;  ebcdic.i = '6E';  ascii.i = '3E'  /* > */
    i = i+1;  ebcdic.i = '6F';  ascii.i = '3F'  /* ? */
    i = i+1;  ebcdic.i = '4B';  ascii.i = '2E'  /* . */
    i = i+1;  ebcdic.i = '4C';  ascii.i = '3C'  /* < */
    i = i+1;  ebcdic.i = '7E';  ascii.i = '3D'  /* = */
    i = i+1;  ebcdic.i = 'A1';  ascii.i = '7E'  /* ∞ */
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
    asciiHex = replaceText(ebcdicHex, 'F', '')
    asciiString = 0
    if (datatype(asciiHex, 'X') = SYS_TRUE)
    then do
        asciiString = asciiHex
    end
    else do
        say 'ebcdic2AsciiNum'
        say 'datatype(asciiHex, "X") = FALSE'
        say 'asciiHex ['asciiHex']'
        call exitError
    end
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
    oldTexPointer = index(newString, oldText)
    replacingsCounter = 0
    do while (oldTexPointer > 0 & replacingsCounter <= 100)
        RTSubstrTo = oldTexPointer - 1
        RTSubstrFrom = oldTexPointer + length( oldText )
        newString = substr(newString, 1, RTSubstrTo) ,
                || newText ,
                || substr(newString, RTSubstrFrom)
        oldTexPointer = index(newString, oldText)
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
    
/* file managing */

allocateSelRecsDirectory:
    selRecsDirectoryQ = "'"selRecsDirectory"'"
    address tso
        'allocate dataset('selRecsDirectoryQ') new' ,
                'space(10 1) cylinders release' ,
                'lrecl(80) block(1600) recfm(f b) dsorg(po)'
    address
    call checkRc 'allocateSelRecsDirectory' ,
            rc 0
    return

allocateSelColsDirectory:
    selColsDirectoryQ = "'"selColsDirectory"'"
    address tso
        'allocate dataset('selColsDirectoryQ') new' ,
                'space(10 1) cylinders release' ,
                'lrecl(80) block(1600) recfm(f b) dsorg(po)'
    address
    call checkRc 'allocateSelColsDirectory' ,
            rc 0
    return

openReadProFile:
    openReadProFileStatus = ''
    select
    when (os = 'WIN64') then call openReadProFileWIN
    when (os = 'TSO')   then call openReadProFileTSO
    end
    return

openReadProFileWIN:
    proFileRc = stream(PRO_FILE, 'C', 'OPEN READ')
    call checkRc 'openReadProFileWIN' ,
            proFileRc 'READY:'
    select
    when (proFileRc = 'READY:')
    then do
        openReadProFileStatus = 'ok'
        call initProFileReadCursor
        proFileRecordsCounter = lines(PRO_FILE, 'C')
    end
    otherwise
        say
        say 'open-read profile KO'
        say 'proFileRc ['proFileRc']'
    end
    return

initProFileReadCursor:
    proFileRc = linein(PRO_FILE, 1, 0)
    call checkRc 'initProFileReadCursor' ,
            proFileRc ''
    return

openReadProFileTSO:
    if (sysdsn(PRO_FILE) = 'OK')
    then do
        call allocateProFile
    end
    else do
        call allocateNewProFile
    end
    openReadProFileStatus = 'ok'
    return

allocateProFile:
    address tso
            'free ddname(proDD)'
        'allocate ddname(proDD) dataset('PRO_FILE') shr'
    address
    call checkRc 'allocateProFile' ,
            rc 0
    return

allocateNewProFile:
    address tso
            'free ddname(proDD)'
        'allocate ddname(proDD) dataset('PRO_FILE') new' ,
                'space(1 1) tracks release' ,
                'lrecl(80) block(1600) recfm(f b) dsorg(ps)'
    address
    call checkRc 'allocateNewProFile' ,
            rc 0
    return

openWriteProFile:
    select
    when (os = 'WIN64') then call openWriteProFileWIN
    when (os = 'TSO')   then call openWriteProFileTSO
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

openWriteProFileTSO:
    call allocateProFile
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
        'execio 1 diskw proDD (proFileStem.'
    address
    call checkRc 'writeProRecordTSO' ,
            rc 0
    return

startPRO_FILE:
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
        'edit dataset ('PRO_FILE')'
    address
    call checkRc 'startPRO_FILE_TSO' ,
            rc 0
    return

closeProFile:
    select
    when (os = 'WIN64') then call closeProFileWIN
    when (os = 'TSO')   then nop
    end
    return

closeProFileWIN:
    proFileRc = stream(PRO_FILE, 'C', 'CLOSE')
    return

openReadDataFile:
    select
    when (os = 'WIN64') then call openReadDataFileWIN
    when (os = 'TSO')   then call openReadDataFileTSO
    end
    return

openReadDataFileWIN:
    dataFileRc = stream(DATA_FILE, 'C', 'OPEN READ')
    call checkRc 'openReadDataFileWIN' ,
            dataFileRc 'READY:' '???'
    if (dataFileRc = 'READY:')
    then do
        openReadDataFileStatus = 'ok'
        dataFileRecordsCursor = 1
        call initDataFileReadCursor
    end
    else do
        say
        say 'open-read data file KO'
        say 'dataFileRc ['dataFileRc']'
    end
    return

initDataFileReadCursor:
    dataFileRc = linein(DATA_FILE, dataFileRecordsCursor, 0)
    call checkRc 'initDataFileReadCursor' ,
            dataFileRc ''
    return

openReadDataFileTSO:
    call allocateDataFile
    openReadDataFileStatus = 'ok'
    return

allocateDataFile:
    address tso
            'free ddname(dataDD)'
        'allocate ddname(dataDD) dataset('DATA_FILE') shr'
    address
    call checkRc 'allocateDataFile' ,
            dataFileRc 0
    return

readDataFileRecord:
    select
    when (os = 'WIN64') then call readDataFileRecordWIN
    when (os = 'TSO')   then call readDataFileRecordTSO
    end
    return

readDataFileRecordWIN:
    dataFileRecord = linein(DATA_FILE)
    dataFileRecordsCursor = dataFileRecordsCursor + 1
    return

readDataFileRecordTSO:
    dataFileStem. = ''
    address tso
        'execio 1 diskr dataDD (stem dataFileStem.'
    address
    call checkRc 'readDataFileRecordTSO' ,
            rc 0
    dataFileRecord = dataFileStem.1
    return

closeDataFile:
    select
    when (os = 'WIN64') then call closeDataFileWIN
    when (os = 'TSO')   then nop
    end
    return

closeDataFileWIN:
    dataFileRc = stream(DATA_FILE, 'C', 'CLOSE')
    return

openReadCopyFile:
    select
    when (os = 'WIN64') then call openReadCopyFileWIN
    when (os = 'TSO')   then call openReadCopyFileTSO
    end
    return

openReadCopyFileWIN:
    openReadCopyFileStatus = ''
    copyFileRc = stream(COPY_FILE, 'C', 'OPEN READ')
    call checkRc 'openReadCopyFileWIN' ,
            copyFileRc 'READY:' '???'
    if (copyFileRc = 'READY:')
    then do
        openReadCopyFileStatus = 'ok'
        call initCopyFileReadCursor
        copyRecsCounter = lines(COPY_FILE, 'C')
    end
    else do
        say
        say 'open-read copy file KO'
        say 'copyFileRc ['copyFileRc']'
    end
    return

initCopyFileReadCursor:
    copyFileRc = linein(COPY_FILE, 1, 0)
    call checkRc 'initCopyFileReadCursor' ,
            copyFileRc ''
    return

openReadCopyFileTSO:
    call allocateCopyFile
    openReadCopyFileStatus = 'ok'
    return

allocateCopyFile:
    address tso
            'free ddname(copyDD)'
        'allocate ddname(copyDD) dataset('COPY_FILE') shr'
    address
    call checkRc 'allocateCopyFile' ,
            rc 0
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
    copyFileRecord = copyFileStem.1
    return

openReadSelRecsFile:
    select
    when (os = 'WIN64') then call openReadSelRecsFileWIN
    when (os = 'TSO')   then call openReadSelRecsFileTSO
    end
    return

openReadSelRecsFileWIN:
    selRecsFileRc = stream(selRecsFile, 'C', 'OPEN READ')
    call checkRc 'openReadSelRecsFileWIN' ,
            selRecsFileRc 'READY:' '???'
    select
    when (selRecsFileRc = 'READY:')
    then do
        call initSelRecsFileReadCursor
        selRecsRecordsCounter = lines(selRecsFile, 'C')
    end
    otherwise
        say
        say 'open-read select file KO'
        say 'selRecsFileRc ['selRecsFileRc']'
    end
    return

initSelRecsFileReadCursor:
    selRecsFileRc = linein(selRecsFileRc, 1, 0)
    call checkRc 'openReadSelRecsFileWIN' ,
            selRecsFileRc ''
    return

openReadSelRecsFileTSO:
    call allocateSelRecsFile
    return

allocateSelRecsFile:
    address tso
            'free ddname(selRcsDD)'
        'allocate ddname(selRcsDD) dataset('selRecsFile') shr'
    address
    call checkRc 'allocateSelRecsFile' ,
            rc 0
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
    if (selRecsFileRc = 'READY:')
    then do
        call initSelRecsFileWriteCursor
    end
    else do
        say
        say 'open-write selRecsFile KO'
        say 'selRecsFileRc ['selRecsFileRc']'
    end
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
        'execio 1 diskr selRecsDD (stem selRecsFileStem.'
    address
    call checkRc 'readSelRecsRecordTSO' ,
            rc 0
    selRecsFileRecord = selRecsFileStem.1
    return

writeSelRecsRecord:
    selRecsFileRc = lineout(selRecsFile, selRecsRecord)
    call checkRc 'writeSelRecsRecord' ,
            selRecsFileRc 0
    return

startSelRecsFile:
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
    call checkRc 'startSelRecsFileTSO' ,
            rc 0
    return

closeSelRecsFile:
    select
    when (os = 'WIN64') then call closeSelRecsFileWIN
    when (os = 'TSO')   then nop
    end
    return

closeSelRecsFileWIN:
    selRecsFileRc = stream(selRecsFile, 'C', 'CLOSE')
    return

openReadSelColsFile:
    select
    when (os = 'WIN64') then call openReadSelColsFileWIN
    when (os = 'TSO')   then call openReadSelColsFileTSO
    end
    return

openReadSelColsFileWIN:
    selColsFileRc = stream(selColsFile, 'C', 'OPEN READ')
    call checkRc 'openReadSelColsFileWIN' ,
            selColsFileRc 'READY:'
    select
    when (selColsFileRc = 'READY:')
    then do
        call initSelColsFileReadCursor
        selColsRecordsCounter = lines(selColsFile, 'C')
    end
    otherwise
        say
        say 'open-read selCols file KO'
        say 'selColsFileRc ['selColsFileRc']'
        /* no error
        call exitError
        */
    end
    return

initSelColsFileReadCursor:
    selColsFileRc = linein(selColsFileRc, 1, 0)
    call checkRc 'initSelColsFileReadCursor' ,
            selColsFileRc ''
    return

openReadSelColsFileTSO:
    call allocateSelColsFile
    return

allocateSelColsFile:
    address tso
            'free ddname(selClsDD)'
        'allocate ddname(selClsDD) dataset('selColsFile') shr'
    address
    call checkRc 'allocateSelColsFile' ,
            rc 0
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
            selColsFileRc 'READY:' '???'
    if (selColsFileRc = 'READY:')
    then do
        call initSelColsFileWriteCursor
    end
    else do
        say
        say 'open-write selCols file KO'
        say 'selColsFileRc ['selColsFileRc']'
    end
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
        'execio 1 diskr selColsDD (stem selColsFileStem.'
    address
    call checkRc 'readSelColsRecordTSO' ,
            rc 0
    selColsFileRecord = selColsFileStem.1
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
        'execio 1 diskw selClsDD (selColsFileStem.'
    address
    call checkRc 'writeSelColsRecordTSO' ,
            rc 0
    return

startSelColsFile:
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
    call checkRc 'startSelColsFileTSO' ,
            rc 0
    return

closeSelColsFile:
    select
    when (os = 'WIN64') then call closeSelColsFileWIN
    when (os = 'TSO')   then nop
    end
    return

closeSelColsFileWIN:
    selColsFileRc = stream(selColsFile, 'C', 'CLOSE')
    return

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
    if (outCopyFileRc = 'READY:')
    then do
        call initOutCopyFileCursor
    end
    else do
        say
        say 'open-write outCopyFile KO'
        say 'outCopyFileRc ['outCopyFileRc']'
    end
    return

initOutCopyFileCursor:
    outCopyFileRc = lineout(outCopyFile, , 1)
    call checkRc 'initOutCopyFileCursor' ,
            outCopyFileRc 0
    return

openWriteOutCopyFileTSO:
    call allocateOutCopyFile
    return

allocateOutCopyFile:
    address tso
            'free ddname(outCpyDD)'
        'allocate ddname(outCpyDD) dataset('outCopyFile') shr'
    address
    call checkRc 'allocateOutCopyFile' ,
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
        'execio 1 diskw outCpyDD (outCopyFileStem.'
    address
    call checkRc 'writeOutCopyRecordTSO' ,
            rc 0
    return

startOutCopyFile:
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
    call checkRc 'startOutCopyFileTSO' ,
            rc 0
    return

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
    if (outDataFileRc = 'READY:')
    then do
        call initOutDataFileWriteCursor
    end
    else do
        say
        say 'open-write output file KO'
        say 'outDataFileRc ['outDataFileRc']'
    end
    return

initOutDataFileWriteCursor:
    outDataFileRc = lineout(outDataFile, , 1)
    call checkRc 'initOutDataFileWriteCursor' ,
            outDataFileRc 0
    return

openWriteOutDataFileTSO:
    call allocateOutDataFile
    return

allocateOutDataFile:
    address tso
            'free ddname(outDtaDD)'
        'allocate ddname(outDtaDD) dataset('outDataFile') shr'
    address
    call checkRc 'allocateOutDataFile' ,
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
        'execio 1 diskw outputDD (outputFileStem.'
    address
    call checkRc 'writeOutputRecordTSO' ,
            rc 0
    return

startOutDataFile:
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
    call checkRc 'startOutDataFileTSO' ,
            rc 0
    return

/* exit */

exitBlip:
    exit 0

/* error managing */

checkRc: procedure
        parse arg procLabel, returnCode, okCode
    interpret 'returnCodeValue = 'returnCode
    if (returnCodeValue <> okCode)
    then do
        say
        say
        say
        say procLabel':'
        say returnCode' ['returnCodeValue']'
        call exitError
    end
    return

exitError:
    exit 99

/* zonedNote:
    picture           value  display mem.content   mem.content
    pic 9(5)          00123+ 00123   F0F0F1F2F3    3030313233 *
    pic s9(5)         00123+ 0012C   F0F0F1F2C3    3030313233 *
    pic s9(5)         00123- 0012L   F0F0F1F2D3    3030313273 *
    * with Micro-focus, but different representations are possible:
    pic 9(5)          00123+ 00123   F0F0F1F2F3    3030313233
    pic s9(5)         00123+ 0012C   F0F0F1F2C3    30303132@@
    pic s9(5)         00123- 0012L   F0F0F1F2D3    30303132@@
    this must be checked
*/

/* comp3Note:
    picture           value  display mem.content   mem.content
    pic s9(5) comp-3  00123+ ?       00123C        00123C
    pic s9(5) comp-3  00123- ?       00123D        00123D
    * different sizes, same integers, different decimals
        ---------- number  ASCII data -------  length  notes ---------
        -456789012,123456  {    Eg‰ ?!#Em}         13  !=x'11' m=x'6D'
        -456789012,12345   {    Eg‰ ?!#E §}        14  !=x'11' §=x'0D'
        -456789012,1234    {    Eg‰ ?!#@ §}        14  !=x'11' §=x'0D'
        -456789012,123     {    Eg‰ ?!#   §}       15  !=x'11' §=x'0D'
        -456789012,12      {    Eg‰ ?!    §}       15  !=x'11' §=x'0D'
        -456789012,1       {    Eg‰ ?!     §}      16  !=x'11' §=x'0D'
        -456789012,        {    Eg‰ ?      §}      16  !=x'10' §=x'0D'
    * different sizes, different integers, different decimals
        ---------- number  ASCII data -------  length  notes ---------
        +456789012,        {    Eg‰ ?      ?}      16   =x'20' ?=x'0C'
        +456789012,1       {    Eg‰ ?!     ?}      16  !=x'21' ?=x'0C'
        +456789012,12      {    Eg‰ ?!    ?}       15  !=x'21' ?=x'0C'
        +456789012,123     {    Eg‰ ?!#   ?}       15  !=x'21' ?=x'0C'
         +56789012,1234    {     ?g‰ ?!#@ ?}       15  !=x'21' ?=x'0C'
          +6789012,12345   {      g‰ ?!#E ?}       15  !=x'21' ?=x'0C'
           +789012,123456  {       ?‰ ?!#El}       15  !=x'21' l=x'6C'
*/

/* compNote:
    picture           value  display mem.content   mem.content
    pic s9(5) comp    00123+ ?       0000007B      0000007B
    pic s9(5) comp    00123- ?       FFFFFF85      FFFFFF85
*/






























