unit jcfbaseConsts;

{$mode objfpc}{$H+}

interface

resourcestring
  //Settings caption and error messages
  lisTheSettingsFileDoesNotExist = 'The settings file "%s" does not exist.%s'+
    'The formatter will work better if it is configured to use a valid settings file';
  lisErrorWritingSettingsFileReadOnly = 'Error writing settings file: %s is read only';
  lisErrorWritingSettingsException = 'Error writing settings file %s:%s%s';
  lisNoSettingsFound = 'No settings found';

const
  //Parser messages
  lisMsg1File = '1 file';
  lisMsgAbortedAfter = 'Aborted after ';
  lisMsgAbortedDueToError = 'Aborted due to error';
  lisMsgAssignmentToFuncionNameIsDeprecated = 'Assignment to the function name "%s" is deprecated, Use assignment to "Result"';
  lisMsgBackup = 'Backup';
  lisMsgBadBackupMode = 'TCodeFormatSettings.Output: bad backup mode ';
  lisMsgBadFileRecurseType = 'TConverter.Convert: Bad file recurse type';
  lisMsgCannotConvertStringToBoolean = 'Cannot convert string [%s] to boolean';
  lisMsgCaseStatmentHasNoElseCase = 'Case statement has no else case';
  lisMsgCouldNorRenameSourceFile = 'TFileConverter.ProcessFile: could not rename source file %s to %s';
  lisMsgCouldNotReadBooleanSetting = 'Could not read boolean setting';
  lisMsgCouldNotReadFloatSetting = 'Could not read float setting';
  lisMsgCouldNotReadIntegerSetting = 'Could not read integer setting';
  lisMsgDeleteBackNotAllowedInStackMode = 'TSourceTokenList: Delete back not allowed in Stack mode';
  lisMsgDestroyShoultNotBeCalled = 'Destroy should not normally be called. You may want to use FreeAndNil(MyObj), or MyObj.Free, or MyForm.Release';
  lisMsgDirectoryDoesNotExist = 'The directory %s does not exist';
  lisMsgDuplicateDefaultWord = 'TSetAnyWordCaps.Read: duplicate default word: %s';
  lisMsgEmptyBeginEndBlock = 'Empty begin..end block';
  lisMsgEmptyExceptEndBlock = 'Empty except..end block';
  lisMsgEmptyFinallyEndBlock = 'Empty finally..end block';
  lisMsgEmptyTryBlock = 'Empty try block';
  lisMsgExceptionParsing = 'Exception parsing "%s": %s';
  lisMsgExceptionTokenising = 'Exception tokenising "%s": %s';
  lisMsgExistsAlreadyRemoveIt = ' exists already. Remove it?';
  lisMsgExpressionHasTrailingTokens = 'Expression has trailing tokens';
  lisMsgFailedToDelete = 'TFileConverter.ProcessFile: Failed to delete ';
  lisMsgFile = ' file ';
  lisMsgFiles = '%d files';
  lisMsgFinishedProcessing = 'Finished processing ';
  lisMsgFormattingFile = 'Formatting file %s';
  lisMsgImbalancedBracketStarComment = 'Imbalanced bracket star comment!';
  lisMsgImbalancedCurlyBracketComment = 'Imbalanced curly bracket comment!';
  lisMsgIn = ' in ';
  lisMsgIncludeFile = 'Include file: %s  %s';
  lisMsgInsertBackNotAllowedInStackMode = 'TSourceTokenList: Insert back not allowed in Stack mode';
  lisMsgJCFInternalErrorUnexpectedErrorInSourceCode = 'JCF Internal Error. Unexpected error in source code';
  lisMsgJCFInternalErrorUnexpectedNilPointer = 'JCF Internal Error. Unexpected NIL pointer';
  lisMsgName = 'name: ';
  lisMsgNear = ' near ';
  lisMsgNoLogFileFoundAt = 'No log file found at %s';
  lisMsgNoOutputBackupFileSpecified = 'No output/backup file specified';
  lisMsgNoSourceToParse = 'No source to parse';
  lisMsgNothingDone = 'Nothing done';
  lisMsgNotValidFloatingPointString = 'Str2Float: %s is not a valid floating point string';
  lisMsgOutput = 'Output';
  lisMsgParameterIsNotUsed = 'Parameter %s is not used';
  lisMsgPreprocessorExpressionCouldNotBeParsed = 'Preprocessor expression could not be parsed';
  lisMsgPreprocessorTermCouldNotBeParsed = 'Preprocessor term could not be parsed';
  lisMsgProcessingDirectory = 'Processing directory %s';
  lisMsgReal48TypeUsed = 'Real48 type used. This type is obsolete and is seldom useful';
  lisMsgRealTypeUsed = 'Real type used. This type is obsolete and is seldom useful';
  lisMsgSelectAFile = 'Select a file';
  lisMsgTheFileDoesNotExist = 'The file "%s" does not exist';
  lisMsgTheFileIsEmpty = 'The file "%s" is empty';
  lisMsgTooManyChildNodes = 'Too many child nodes %d';
  lisMsgUnableToRecoverImbalancedBracketStarComment = 'Unable to recover from imbalanced bracket star comment.';
  lisMsgUnableToRecoverImbalancedCurlyComment = 'Unable to recover from imbalanced curly comment.';
  lisMsgUnhandledErrorInSourceCode = 'Unhandled error in source code!';
  lisMsgUnknownFileContentType = 'Unknown file content type: %d';
  lisMsgUnterminatedString = 'Unterminated string: %s' ;
  lisMsgValue = 'value: ';

  lisExpectedCompilerDirective = 'Expected compiler directive ';
  lisMsgExpectedArraySetFileOrRecordType = 'Expected array, set, file or record type';
  lisMsgExpectedAsmIdentifier = 'Expected asm identifier';
  lisMsgExpectedAsmOpcode = 'Expected asm opcode';
  lisMsgExpectedAsmParam = 'Expected asm param';
  lisMsgExpectedClassProcedureOrClassFunction = 'Expected class procedure or class function';
  lisMsgExpectedConstTypeVarProcedureOrFunction = 'Expected const, type, var, procedure or function';
  lisMsgExpectedEqualsOrColon = 'Expected equals or colon';
  lisMsgExpectedExceptOrFinally = 'Expected except or finally';
  lisMsgExpectedExportDirective = 'Expected export directive';
  lisMsgExpectedIdentifier = 'Expected identifier';
  lisMsgExpectedInitializationBeginOrEnd = 'Expected initialization, begin or end';
  lisMsgExpectedLabelConstTypeVarProcedureOrFunction = 'Expected label, const, type, var, procedure or function';
  lisMsgExpectedObjectClassOrInterface = 'Expected object, class or interface';
  lisMsgExpectedProcedureOrFunction = 'Expected procedure or function';
  lisMsgExpectedProcedureOrFunctionType = 'Expected procedure or function type';
  lisMsgExpectedProgramPackageLibraryUnit = 'Expected program, package, library, unit, ''.inc'' got "%s" ';
  lisMsgExpectedPropertySpecifier = 'Expected property specifier';
  lisMsgExpectedSimpleStatement = 'Expected simple statement';
  lisMsgExpectedStructuredStatement = 'Expected structured statement';

  lisMsgUnexpectedEOF = 'Unexpected EOF.';
  lisMsgUnexpectedPreprocessorSymbol = 'Unexpected preprocessor symbol';
  lisMsgUnexpectedToken = 'Unexpected token';
  lisMsgUnexpectedTokenExpected = 'Unexpected token "%s", expected %s';
  lisMsgUnexpectedTokenExpectedSingleChar = 'Unexpected token, expected single char after ^';
  lisMsgUnexpectedTokenInAddOp = 'Unexpected token in add op';
  lisMsgUnexpectedTokenInFactor = 'Unexpected token in factor';
  lisMsgUnexpectedTokenInMulOp = 'Unexpected token in mul op';
  lisMsgUnexpectedTokenInRecogniseAnonymousMethod = 'Unexpected token in RecogniseAnonymousMethod';
  lisMsgUnexpectedTokenInRelOp = 'Unexpected token in rel op';

implementation

end.

