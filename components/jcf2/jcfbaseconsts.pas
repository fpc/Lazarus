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
  lisMsgAbortedAfter = 'Aborted after %d file(s)';
  lisMsgAbortedDueToError = 'Aborted due to error';
  lisMsgAssignmentToFuncionNameIsDeprecated = 'Assignment to the function name "%s" is deprecated, Use assignment to "Result"';
  lisMsgAtLineCol = '%s at line %d col %d';
  lisMsgBackup = 'Backup';
  lisMsgBadBackupMode = 'TCodeFormatSettings.Output: bad backup mode ';
  lisMsgBadFileRecurseType = 'TConverter.Convert: Bad file recurse type';
  lisMsgCannotConvertStringToBoolean = 'Cannot convert string [%s] to boolean';
  lisMsgCaseStatmentHasNoElseCase = 'Case statement has no else case';
  lisMsgChangesWhereMade = '%s: %d changes were made';
  lisMsgCommentHasPrefixButCannotBeParsed = 'Comment "%s" has prefix but cannot be parsed';
  lisMsgCouldNorRenameSourceFile = 'TFileConverter.ProcessFile: could not rename source file %s to %s';
  lisMsgCouldNotReadBooleanSetting = 'Could not read boolean setting';
  lisMsgCouldNotReadFloatSetting = 'Could not read float setting';
  lisMsgCouldNotReadIntegerSetting = 'Could not read integer setting';
  lisMsgDestroyShoultNotBeCalled = 'Destroy should not normally be called. You may want to use FreeAndNil(MyObj), or MyObj.Free, or MyForm.Release';
  lisMsgDirectoryDoesNotExist = 'The directory %s does not exist';
  lisMsgDuplicateDefaultWord = 'TSetAnyWordCaps.Read: duplicate default word: %s';
  lisMsgEmptyBeginEndBlock = 'Empty begin..end block';
  lisMsgEmptyExceptEndBlock = 'Empty except..end block';
  lisMsgEmptyFinallyEndBlock = 'Empty finally..end block';
  lisMsgEmptyTryBlock = 'Empty try block';
  lisMsgExpectionClassMsg = 'Exception %s  %s';
  lisMsgExceptionParsing = 'Exception parsing "%s": %s';
  lisMsgExceptionTokenising = 'Exception tokenising "%s": %s';
  lisMsgExistsAlreadyRemoveIt = '%s %s %s exists already. Remove it?';
  lisMsgExpressionHasTrailingTokens = 'Expression has trailing tokens';
  lisMsgFailedToDelete = 'TFileConverter.ProcessFile: Failed to delete %s %s %s';
  lisMsgFile = 'file';
  lisMsgFiles = '%d file(s)';
  lisMsgFinishedProcessing = 'Finished processing %d file(s)';
  lisMsgFormattingFile = 'Formatting file %s';
  lisMsgIdentifierCaps = 'Identifier caps';
  lisMsgImbalancedBracketStarComment = 'Imbalanced bracket star comment!';
  lisMsgImbalancedCurlyBracketComment = 'Imbalanced curly bracket comment!';
  lisMsgIn = '%s in %s';
  lisMsgIncludeFile = 'Include file: %s  %s';
  lisMsgInCommentSettingIsNotKnown = 'In comment "%s" , setting "%s" is not known';
  lisMsgInCommentStateCannotBeParsedToOnOff = 'In comment  "%s" , state "%s" cannot be parsed to either on or off';
  lisMsgName = 'name: ';
  lisMsgNear = '%s near %s';
  lisMsgNear2 = 'Near %s';
  lisMsgNoLogFileFoundAt = 'No log file found at %s';
  lisMsgNonIdentifierCaps = 'Non-identifier caps';
  lisMsgNoOutputBackupFileSpecified = 'No output/backup file specified';
  lisMsgNoSourceToParse = 'No source to parse';
  lisMsgNothingDone = 'Nothing done';
  lisMsgNotValidFloatingPointString = 'Str2Float: %s is not a valid floating point string';
  lisMsgOneChangeWasMade = '%s: One change was made: %s';
  lisMsgOnLine = 'on line %d';
  lisMsgOutput = 'Output';
  lisMsgParameterIsNotUsed = 'Parameter %s is not used';
  lisMsgPosition = '%s position %d';
  lisMsgPreprocessorExpressionCouldNotBeParsed = 'Preprocessor expression could not be parsed';
  lisMsgPreprocessorTermCouldNotBeParsed = 'Preprocessor term could not be parsed';
  lisMsgProcessingDirectory = 'Processing directory %s';
  lisMsgReal48TypeUsed = 'Real48 type used. This type is obsolete and is seldom useful';
  lisMsgRealTypeUsed = 'Real type used. This type is obsolete and is seldom useful';
  lisMsgReplaceChangesWereMade = 'Replace: %d changes were made';
  lisMsgSelectAFile = 'Select a file';
  lisMsgSpacesToEnd = 'Spaces to end is %d on %s at %s line is ';
  lisMsgSpecificWordCaps = 'Specific word caps';
  lisMsgTheFileDoesNotExist = 'The file "%s" does not exist';
  lisMsgTheFileIsEmpty = 'The file "%s" is empty';
  lisMsgTo = '%s to %s';
  lisMsgTooManyChildNodes = 'Too many child nodes %d';
  lisMsgUnableToRecoverImbalancedBracketStarComment = 'Unable to recover from imbalanced bracket star comment.';
  lisMsgUnableToRecoverImbalancedCurlyComment = 'Unable to recover from imbalanced curly comment.';
  lisMsgUnhandledErrorInSourceCode = 'Unhandled error in source code!';
  lisMsgUnitNameCaps = 'Unit name caps';
  lisMsgUnknownFileContentType = 'Unknown file content type: %d';
  lisMsgUnterminatedString = 'Unterminated string: %s' ;
  lisMsgUsesClauseFindReplace = 'Uses clause find/replace: %d changes were made';
  lisMsgUsesClauseInsertion = 'Uses clause insertion: %d insertions were made';
  lisMsgUsesClauseRemoval = 'Uses clause removal: %d removals were made';
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
  lisMsgUnexpectedTokenInRecogniseAnonymousMethod = 'Unexpected token recognising anonymous method';
  lisMsgUnexpectedTokenInRelOp = 'Unexpected token in rel op';

  lisMsgStatsBasicNumbers = 'Basic numbers and averages: ';
  lisMsgStatsLinesLong = 'Unit is %d lines long';
  lisMsgStatsUnitHasTokens = 'Unit has %d tokens in %d characters: ';
  lisMsgStatsCharsPerToken = '%s chars per token';
  lisMsgStatsCharsPerLine = '%s chars per line ';
  lisMsgStatsTokensPerLine = '%s tokens per line ';
  lisMsgStatsCommentsIn = '%d comments in %d characters ';
  lisMsgStatsCharsPerComment = '%s chars per comment';
  lisMsgStatsCharsAreComments = '%s of chars are comments';
  lisMsgStatsSpacingAndReturnTokens = '%d spacing and return tokens in %d characters';
  lisMsgStatsCharsAreSpacing = '%s of chars are spacing';
  lisMsgStatsSolidTokens = '%d solid tokens in %d characters ';
  lisMsgStatsCharsAreSolid = '%s of chars are solid';
  lisMsgStatsTokensAreSolid = '%s of tokens are solid';
  lisMsgStatsConstants = '%d constants';
  lisMsgStatsTypes = '%d types';
  lisMstStatsClasses = '%d classes';
  lisMsgStatsInterfaces = '%d interfaces';
  lisMsgStatsProcedures = '%d procedures';
  lisMsgStatsGlobalVarsInInterface = '%d global vars in interface';
  lisMsgStatsGlobalVarsInRestOfUnit = '%d global vars in rest of unit';
  lisMsgStatsProceduresInInterface = '%d procedures in interface';

const  // internal errors don't localize.
  lisMsgDeleteBackNotAllowedInStackMode = 'TSourceTokenList: Delete back not allowed in Stack mode';
  lisMsgJCFInternalErrorUnexpectedErrorInSourceCode = 'JCF Internal Error. Unexpected error in source code';
  lisMsgJCFInternalErrorUnexpectedNilPointer = 'JCF Internal Error. Unexpected NIL pointer';
  lisMsgInsertBackNotAllowedInStackMode = 'TSourceTokenList: Insert back not allowed in Stack mode';

implementation

end.

