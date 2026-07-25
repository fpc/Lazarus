unit IdeHelpUtilStrings;

{$mode objfpc}{$H+}

interface

resourcestring
  lisProjAddPackageNotFound = 'Package not found';
  lisLDTheUnitIsNotOwnedBeAnyPackageOrProjectPleaseAddThe = 'The unit %s is '
    +'not owned be any package or project.%sPlease add the unit to a package '
    +'or project.%sUnable to create the fpdoc file.';
  lisLDNoValidFPDocPath = 'No valid FPDoc path';
  lisTheUnitIsPartOfTheFPCSourcesButTheCorrespondingFpd = 'The unit %s is part'
    +' of the FPC sources but the corresponding fpdoc xml file was not found.'
    +'%sEither you have not yet added the fpcdocs directory to the search path or the '
    +'unit is not yet documented.%sThe fpdoc files for the FPC sources can be'
    +' downloaded from: %s%sPlease add the directory in the '
    +'fpdoc editor options.%sIn order to create a new file the directory must '
    +'be writable.';
  lisLDDoesNotHaveAnyValidFPDocPathUnableToCreateTheFpdo = '%s does not have '
    +'any valid FPDoc path.%sUnable to create the fpdoc file for %s';
  lisErrorReadingXML = 'Error reading XML';
  lisErrorReadingXmlFile = 'Error reading xml file "%s"%s%s';

  rsSelectAnInheritedEntry = 'Select an inherited entry';
  lisChooseAFPDocLink = 'Choose a FPDoc link';
  lisLinkTarget = 'Link target';
  lisExamplesIdentifierTMyEnumEnumUnitnameIdentifierPac = 'Examples:'
    +'%sIdentifier'
    +'%sTMyEnum.Enum'
    +'%sUnitname.Identifier'
    +'%s#PackageName.UnitName.Identifier';
  lisTitleLeaveEmptyForDefault = 'Title (leave empty for default)';
  lisPackageUnit = 'package unit';
  lisPackage2 = 'package %s';
  lisIdentifier = 'identifier';
  lisProjectUnit = 'project unit';

  // Edit context help dialog (IDEContextHelpEdit.pas)
  lisName = 'Name';
  dlgCCOTest = 'Test';
  lisPath = 'Path';
  lisHelpEntries = 'Help entries';
  lisCEIsARootControl = 'Is a root control';
  lisHasHelp = 'Has Help';
  lisCreateHelpNode = 'Create Help node';
  lisDlgOpen = 'Open ...';
  lisEditContextHelp = 'Edit context help';
  lisNoNodeSelected = 'no node selected';
  lisNoIDEWindowSelected = 'No IDE window selected';

  lisErrorWritingFile = 'Error writing file "%s"';

  lisLink = 'Link:';
  lisShort = 'Short:';
  lisInsertUrlTag = 'Insert url tag';
  lisInsertPrintshortTag2 = 'Insert printshort tag';
  lisCodeHelpMainFormCaption = 'FPDoc Editor';
  lisCodeHelpInherited = 'Inherited';
  lisCodeHelpShortTag = 'Short';
  lisCodeHelpDescrTag = 'Description';
  lisCodeHelpErrorsTag = 'Errors';
  lisCodeHelpSeeAlsoTag = 'See also';
  lisCodeHelpConfirmreplace = 'Confirm replace';
  lisCodeHelpHintBoldFormat = 'Insert bold formatting tag';
  lisCodeHelpHintItalicFormat = 'Insert italic formatting tag';
  lisCodeHelpHintUnderlineFormat = 'Insert underline formatting tag';
  lisCodeHelpHintInsertCodeTag = 'Insert code formatting tag';
  lisCodeHelpHintRemarkTag = 'Insert remark formatting tag';
  lisCodeHelpHintVarTag = 'Insert var formatting tag';
  lisCodeHelpCreateButton = 'Create help item';
  lisOpenXML = 'Open XML';
  lisCodeHelpInsertALink = 'Insert a link ...';
  lisCodeHelpInsertParagraphFormattingTag = 'Insert paragraph formatting tag';
  lisCodeHelpExampleTag = 'Example';
  lisCodeHelpBrowseExampleButton = 'Browse';
  lisLDMoveEntriesToInherited = 'Move entries to inherited';
  lisLDCopyFromInherited = 'Copy from inherited';
  lisLDAddLinkToInherited = 'Add link to inherited';
  lisCodeHelpNoTagCaption = '<NONE>';
  lisCodeHelpnoinheriteddescriptionfound = '(no inherited description found)';
  lisCodeHelpShortdescriptionOf = 'Short description of';
  lisFPDocErrorWriting = 'Error writing "%s"%s%s';
  lisFPDocFPDocSyntaxError = 'FPDoc syntax error';
  lisFPDocThereIsASyntaxErrorInTheFpdocElement = 'There is a syntax error in '
    +'the fpdoc element "%s":%s%s';
  lisChooseAnExampleFile = 'Choose an example file';
  lisAlreadyContainsTheHe = '%s already contains the help:'+LineEnding+'%s';
  lisInvalidDeclaration = 'Invalid Declaration';
  lisPleasePlaceTheEditorCaretOnAnIdentifierIfThisIsANe = 'Please place the editor caret on an '
    +'identifier. If this is a new unit, please save the file first.';

  lisSave = 'Save';
  lisReplace = 'Replace';
  lisConfirmReplace = 'Confirm Replace';
  dlgFilterAll = 'All files';
  dlgFilterPascalFile = 'Pascal file';

implementation

end.

