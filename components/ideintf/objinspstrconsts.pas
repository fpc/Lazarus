{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit ObjInspStrConsts;

{$mode objfpc}{$H+}

interface

resourcestring

  // Object Inspector
  oisObjectInspector = 'Object Inspector';
  oisError = 'Error';
  oisMixed = '(Mixed)';
  oisItemsSelected = '%u items selected';
  //oiscAdd = '&Add';
  oiscDelete = 'Delete?';
  oisProperties = 'Properties';
  oisBtnProperties = '&Properties';
  oisEvents = 'Events';
  oisFavorites = 'Favorites';
  oisRestricted = 'Restricted';
  oisWidgetSetRestrictions = 'General widget set restrictions: ';
  oisComponentRestrictions = 'Component restrictions: ';

  //Object Inspector Popup Menu
  oisZOrder = 'Z-order';
  oisOrderMoveToFront = 'Move to Front';
  oisOrderMoveToBack = 'Move to Back';
  oisOrderForwardOne = 'Forward One';
  oisOrderBackOne = 'Back One';
  oisSetToDefault = 'Set to default: %s';
  oisRevertToInherited = 'Revert to inherited';
  oisSetToDefaultHint = 'Set property value to Default';
  oisSetMaxConstraints = 'Set MaxHeight=%d, MaxWidth=%d';
  oisSetMinConstraints = 'Set MinHeight=%d, MinWidth=%d';
  oisSetMaxConstraintsHint = 'Use current size as Max Constraints';
  oisSetMinConstraintsHint = 'Use current size as Min Constraints';
  oisAddToFavorites = 'Add to Favorites';
  oisViewRestrictedProperties = 'View restricted properties';
  oisRemoveFromFavorites = 'Remove from Favorites';
  oisUndo = 'Undo';
  oisFinddeclaration = 'Jump to declaration';
  oisJumpToDeclarationOf = 'Jump to declaration of %s';
  oisCutComponents = 'Cu&t';
  oisCopyComponents = '&Copy';
  oisPasteComponents = '&Paste';
  oisDeleteComponents = '&Delete';
  oisDelete = 'Delete';
  rscdMoveUp = 'Move up';
  rscdMoveDown = 'Move down';
  rscdOK = 'OK';
  oisShowComponentTree = 'Show Component Tree';
  oisShowHints = 'Show Hints';
  oisShowInfoBox = 'Show Information Box';
  oisShowStatusBar = 'Show Status Bar';
  oisOptions = 'Options';

  // typeinfo, PropEdits
  oisInvalid = '(Invalid)';
  oisUnknown = 'Unknown';
  oisObject = 'Object';
  oisClass = 'Class';
  oisWord = 'Word';
  oisString = 'String';
  oisFloat = 'Float';
  oisSet = 'Set';
  oisMethod = 'Method';
  oisVariant = 'Variant';
  oisArray = 'Array';
  oisRecord = 'Record';
  oisInterface = 'Interface';
  oisValue = 'Value:';
  oisInteger = 'Integer';
  oisInt64 = 'Int64';
  oisBoolean = 'Boolean';
  oisEnumeration = 'Enumeration';
  oisChar = 'Char';

  // Editors
  oisDeleteSelectedFieldS = 'Delete selected field(s)';
  oisNew = '&New';
  oisCreateNewFieldAndAddItAtCurrentPosition = 'Create new field and add it '
    +'at current position';
  oisMoveUp = 'Move &Up';
  oisMoveDown = 'Move &Down';
  oisSelectAll = '&Select all';
  oisUnselectAll = '&Unselect all';
  oisConfirmDelete = 'Confirm delete';
  oisDeleteItem = 'Delete item "%s"?';

  // TreeView Items Editor
  sccsTrEdtCaption         = 'TreeView Items Editor';
  sccsTrEdt                = 'Edit Items ...';
  sccsTrEdtGrpLCaption     = 'Items';
  sccsTrEdtGrpRCaption     = 'Item Properties';
  sccsTrEdtNewItem         = 'New Item';
  sccsTrEdtNewSubItem      = 'New SubItem';
  sccsTrEdtDelete          = 'Delete';
  sccsTrEdtApply           = 'Apply';
  sccsTrEdtLoad            = 'Load';
  sccsTrEdtSave            = 'Save';
  sccsTrEdtLabelText       = 'Text:';
  sccsTrEdtLabelImageIndex = 'Image Index:';
  sccsTrEdtLabelSelIndex   = 'Selected Index:';
  sccsTrEdtLabelStateIndex = 'State Index:';
  sccsTrEdtItem            = 'Item';
  sccsTrEdtOpenDialog      = 'Open';
  sccsTrEdtSaveDialog      = 'Save';

  // ListView Items Editor
  sccsLvEdtCaption         = 'ListView Items Editor';
  sccsLvEdt                = 'Edit Items ...';
  sccsLvColEdt             = 'Edit Columns ...';
  sccsLvEdtGrpLCaption     = 'Items';
  sccsLvEdtGrpRCaption     = 'Item Properties';
  sccsLvEdtNewItem         = 'New Item';
  sccsLvEdtNewSubItem      = 'New SubItem';
  sccsLvEdtApply           = 'Apply';
  sccsLvEdtDelete          = 'Delete';
  sccsLvEdtLabelCaption    = 'Caption:';
  sccsLvEdtLabelImageIndex = 'Image Index:';
  sccsLvEdtLabelStateIndex = 'State Index:';
  sccsLvEdtItem            = 'Item';

  // Image editor strings
  oisImageListComponentEditor = 'I&mageList Editor ...';
  sccsILEdtCaption     = 'ImageList Editor';
  sccsILEdtGrpLCaption = 'Images';
  sccsILEdtGrpRCaption = 'Selected Image';
  sccsILEdtAdd         = '&Add ...';
  sccsILEdtAddMoreResolutions = 'Add more resolutions ...';
  sccsILEdtReplace     = '&Replace ...';
  sccsILEdtReplaceAllResolutions = 'Replace all resolutions ...';
  sccsILEdtDelete      = '&Delete';
  sccsILEdtApply       = '&Apply';
  sccsILEdtClear       = '&Clear';
  sccsILEdtMoveUp      = 'Move &Up';
  sccsILEdtMoveDown    = 'Move D&own';
  sccsILEdtSave        = '&Save ...';
  sccsILEdtSaveAll     = 'Save All ...';
  sccsILEdtAddNewResolution = 'New resolution ...';
  sccsILEdtDeleteResolution = 'Delete resolution ...';
  sccsILEdtDeleteResolutionConfirmation = 'Select the resolution to delete.';
  sccsILEdtCannotDeleteResolution = 'Cannot delete default resolution.';
  sccsILEdtImageWidthOfNewResolution = 'Image width of the new resolution:';
  sccsILEdtransparentColor = 'Transparent Color:';
  sccsILEdtAdjustment  = 'Adjustment';
  sccsILEdtNone        = 'None';
  liisIf               = 'If';
  liisIfDef            = 'IfDef';
  liisIfNDef           = 'IfNDef';
  liisElseIf           = 'ElseIf';
  liisElse             = 'Else';
  liisAddValue         = 'Add value';
  liisSetValue         = 'Set value';
  sccsILEdtStretch     = 'Stretch';
  sccsILEdtCrop        = 'Crop';
  sccsILEdtCenter      = 'Center';
  rscdRight            = 'Right';
  rscdVisible          = 'Visible';
  rscdAutoSize         = 'Auto Size';
  sccsILEdtOpenDialog  = 'Add Images';
  sccsILEdtOpenDialogN = 'New Image';
  sccsILEdtSaveDialog  = 'Save Image';
  
  // StringGrid Editor
  sccsSGEdt           = 'Edit StringGrid ...';
  sccsSGEdtCaption    = 'StringGrid Editor';
  sccsSGEdtGrp        = 'String Grid';
  sccsSGEdtApply      = 'Apply';
  sccsSGEdtClean      = 'Clean';
  sccsSGEdtLoad       = 'Load ...';
  sccsSGEdtSave       = 'Save ...';
  sccsSGEdtOpenDialog = 'Open';
  sccsSGEdtSaveDialog = 'Save';
  sccsSGEdtMoveRowsCols = 'Move Rows/Cols';

  // HeaderControl Editor
  sccsHCEditSections  = 'Sections Editor ...';

  // StatusBar Editor
  sccsSBEditPanels    = 'Panels Editor ...';

  // component editors
  nbcesAddPage  = 'Add Page';
  nbcesInsertPage = 'Insert Page';
  nbcesDeletePage = 'Delete Page';
  nbcesMovePageLeft = 'Move Page Left';
  nbcesMovePageRight = 'Move Page Right';
  nbcesShowPage = 'Show Page';
  oisCreateDefaultEvent = 'Create default event';
  tccesAddTab  = 'Add tab';
  tccesInsertTab = 'Insert tab';
  tccesDeleteTab = 'Delete tab';
  tccesMoveTabLeft = 'Move tab left';
  tccesMoveTabRight = 'Move tab right';
  tbceNewButton = 'New Button';
  tbceNewCheckbutton = 'New CheckButton';
  tbceNewSeparator = 'New Separator';
  tbceNewDivider = 'New Divider';

  //checklistbox editor
  clbCheckListBoxEditor = 'CheckListBox Editor';
  clbUp = 'Up';
  clbDown = 'Down';
  clbModify = 'Modify the Item';
  clbAdd = 'Add new Item';
  clbDeleteHint = 'Delete the Item';
  clbDeleteQuest = 'Delete the Item %d "%s"?';

  //checkgroup editor
  cgCheckGroupEditor = 'CheckGroup Editor';
  cgDisable = 'Popup to disable/enable items';
  cgColumns = 'Columns:';
  cgCheckDuplicate = 'On Add, Check for Duplicate in Items';
  cgCheckDuplicateMsg = 'The "%s" Item is already listed. Add it anyway?';

  // flowpanel editor
  fpFlowPanelEditor = 'FlowPanel Editor';

  // Collection Editor
  oiColEditAdd = 'Add';
  oiColEditDelete = 'Delete';
  oiColEditUp = 'Up';
  oiColEditDown = 'Down';
  oiColEditEditing = 'Editing';

  // Actions Editor
  cActionListEditorUnknownCategory = '(Unknown)';
  cActionListEditorAllCategory = '(All)';
  cActionListEditorEditCategory = 'Edit';
  cActionListEditorSearchCategory = 'Search';
  cActionListEditorHelpCategory = 'Help';
  oisCategory = 'Category';
  oisAction = 'Action';

  // Mask Editor
  sccsMaskEditor = 'Edit Mask Editor ...';
  oisMasks = 'Masks ...';
  oisSaveLiteralCharacters = 'Save Literal Characters';
  oisInputMask = 'Input Mask:';
  oisSampleMasks = 'Sample Masks:';
  oisCharactersForBlanks = 'Characters for Blanks';
  oisTestInput = 'Test Input';
  oisOpenMaskFile = 'Open masks file (*.dem)';
  cActionListEditorDialogCategory = 'Dialog';
  cActionListEditorFileCategory = 'File';
  cActionListEditorDatabaseCategory = 'Database';
  
  oisActionListComponentEditor = 'Action&List Editor ...';
  oisActionListEditor = 'ActionList Editor';
  oisErrorDeletingAction = 'Error deleting action';
  oisErrorWhileDeletingAction = 'Error while deleting action:%s%s';
  cActionListEditorNewAction = 'New Action';
  cActionListEditorNewStdAction = 'New Standard Action';
  cActionListEditorMoveDownAction = 'Move Down';
  ilesAdd = 'Add';
  cActionListEditorMoveUpAction = 'Move Up';
  cActionListEditorDeleteActionHint = 'Delete Action';
  cActionListEditorDeleteAction = 'Delete';
  cActionListEditorPanelDescrriptions = 'Panel Descriptions';
  cActionListEditorPanelToolBar = 'Toolbar';

  oiStdActEditCutHeadLine = 'Cu&t';
  oiStdActEditCopyHeadLine = '&Copy';
  oiStdActEditPasteHeadLine = '&Paste';
  oiStdActEditSelectAllHeadLine = 'Select &All';
  oiStdActEditUndoHeadLine = '&Undo';
  oiStdActEditDeleteHeadLine = '&Delete';
  oiStdActSearchFindHeadLine = '&Find ...';
  oiStdActSearchFindFirstHeadLine = 'F&ind First';
  oiStdActSearchFindNextHeadLine = 'Find &Next';
  oiStdActSearchReplaceHeadLine = '&Replace';
  oiStdActHelpContentsHeadLine = '&Contents';
  oiStdActHelpTopicSearchHeadLine = '&Topic Search';
  oiStdActHelpHelpHelpHeadLine = '&Help on Help';
  oiStdActFileOpenHeadLine = '&Open ...';
  oiStdActFileOpenWithHeadLine = 'Open with ...';
  oiStdActFileSaveAsHeadLine = 'Save &As ...';
  oiStdActFileExitHeadLine = 'E&xit';
  oiStdActColorSelect1HeadLine = 'Select &Color ...';
  oiStdActFontEditHeadLine = 'Select &Font ...';

  oiStdActDataSetFirstHeadLine = '&First';
  oiStdActDataSetPriorHeadLine = '&Prior';
  oiStdActDataSetNextHeadLine = '&Next';
  oiStdActDataSetLastHeadLine = '&Last';
  oiStdActDataSetInsertHeadLine = '&Insert';
  oiStdActDataSetDeleteHeadLine = '&Delete';
  oiStdActDataSetEditHeadLine = '&Edit';
  oiStdActDataSetPostHeadLine = 'P&ost';
  oiStdActDataSetCancelHeadLine = '&Cancel';
  oiStdActDataSetRefreshHeadLine = '&Refresh';

  oiStdActEditCutShortCut = 'Ctrl+X';
  oiStdActEditCopyShortCut = 'Ctrl+C';
  oiStdActEditPasteShortCut = 'Ctrl+V';
  oiStdActEditSelectAllShortCut = 'Ctrl+A';
  oiStdActEditUndoShortCut = 'Ctrl+Z';
  oiStdActEditDeleteShortCut = 'Del';
  oiStdActSearchFindShortCut = 'Ctrl+F';
  oiStdActSearchFindNextShortCut = 'F3';
  oiStdActFileOpenShortCut = 'Ctrl+O';

  oiStdActEditCutShortHint = 'Cut';
  oiStdActEditCopyShortHint = 'Copy';
  oiStdActEditPasteShortHint = 'Paste';
  oiStdActEditSelectAllShortHint = 'Select All';
  oiStdActEditUndoShortHint = 'Undo';
  oiStdActEditDeleteShortHint = 'Delete';
  oiStdActSearchFindHint = 'Find';
  oiStdActSearchFindFirstHint = 'Find first';
  oiStdActSearchFindNextHint = 'Find next';
  oiStdActSearchReplaceHint = 'Replace';
  oiStdActHelpContentsHint = 'Help Contents';
  oiStdActHelpTopicSearchHint = 'Topic Search';
  oiStdActHelpHelpHelpHint = 'Help on help';
  oiStdActFileOpenHint = 'Open';
  oiStdActFileOpenWithHint = 'Open with';
  oiStdActFileSaveAsHint = 'Save As';
  oiStdActFileExitHint = 'Exit';
  oiStdActColorSelectHint = 'Color Select';
  oiStdActFontEditHint = 'Font Select';
  oiStdActDataSetFirstHint = 'First';
  oiStdActDataSetPriorHint = 'Prior';
  oiStdActDataSetNextHint = 'Next';
  oiStdActDataSetLastHint = 'Last';
  oiStdActDataSetInsertHint = 'Insert';
  oiStdActDataSetDeleteHint = 'Delete';
  oiStdActDataSetEditHint = 'Edit';
  oiStdActDataSetPostHint = 'Post';
  oiStdActDataSetCancel1Hint = 'Cancel';
  //oisComponents = 'Components';
  oisBtnComponents = 'Co&mponents';
  oiStdActDataSetRefreshHint = 'Refresh';
  oisSelectedProperties = '&Selected Properties';
  
  oisStdActionListEditor = 'Standard Action Classes';
  oisStdActionListEditorClass = 'Available Action Classes:';

  // TFileNamePropertyEditor
  oisSelectAFile = 'Select a file';
  oisPropertiesOf = 'Properties of %s';
  oisAllFiles = 'All files';
  
  // TCommonDialogComponentEditor
  oisTestDialog = 'Test dialog ...';

  // property editors
  oisSort = 'Sort';
  oisDLinesDChars = '%d lines, %d chars';
  ois1LineDChars = '1 line, %d chars';
  oisStringsEditorDialog = 'Strings Editor Dialog';
  ois0Lines0Chars = '0 lines, 0 chars';
  oisInvalidPropertyValue = 'Invalid property value';
  oisNone = '(none)';
  oisPressAKey = 'Press a key ...';
  oisPressAKeyEGCtrlP = 'You can press e.g. Ctrl+P ...';
  oisSelectShortCut = 'Select short cut';
  srGrabKey = 'Grab key';
  oisComponentNameIsNotAValidIdentifier = 'Component name "%s" is not a valid identifier';
  oisLoadImageDialog = 'Load Image Dialog';
  oisOK = '&OK';
  oisCancel = 'Cancel';
  oisHelp = '&Help';
  oisPEPicture = 'Picture';
  oisLoadPicture = 'Load picture';
  oisSavePicture = 'Save picture';
  oisClearPicture = 'Clear picture';
  oisLoad = '&Load';
  oisSave = '&Save';
  oisClear = 'C&lear';
  oisPEOpenImageFile = 'Open image file';
  oisPESaveImageAs = 'Save image as';
  oisErrorLoadingImage = 'Error loading image';
  oisErrorLoadingImage2 = 'Error loading image "%s":%s%s';
  oisOk2 = 'Ok';
  oisCreateANewPascalUnit = 'Create a new pascal unit.';
  rscdColumnEditor = 'Column Editor';
  rscdCaption = 'Caption';
  rscdInvalidNumericValue = 'Invalid numeric Value';
  rscdWidth = 'Width';
  rscdAlignment = 'Alignment';
  rscdLeft = 'Left';

  // image list editor
  s_SuggestSplitImage = 'Do you want to split the image?';
  s_Confirm_Clear = 'Are you sure to clear image list?';
  s_AddAsSingle = 'Add as single';
  s_SplitImage = 'Split image';
  
  // Fields Editor
  fesFeTitle = 'Edit Fields ...';
  oisAddFields = '&Add fields';
  oisAddFieldsFromFieldDefs = 'Add fields from FieldDefs';
  fesNoFields = 'It was not possible to get the dataset field''s list';
  fesCheckDset = 'Check dataset settings';
  fesFlTitle = 'FieldDefs';
  fesNoFieldsNote = 'Field''s list is not available, can''t check for duplicates';
  oisIncompatibleIdentifier = 'Incompatible Identifier';
  oisIsNotAValidMethodName = '"%s" is not a valid method name.';
  oisTheIdentifierIsNotAMethodPressCancelToUndoPressIgn = 'The identifier "%s" is not a method.'
    +'%sPress Cancel to undo,'
    +'%spress Ignore to force it.';
  oisIncompatibleMethod = 'Incompatible Method';
  oisTheMethodIsNotPublishedPressCancelToUndoPressIgnor = 'The method "%s" is not published.'
    +'%sPress Cancel to undo,'
    +'%spress Ignore to force it.';
  oisTheMethodIsIncompatibleToThisEventPressCancelToUnd = 'The method "%s" '
    +'is incompatible to this event (%s).'
    +'%sPress Cancel to undo,'
    +'%spress Ignore to force it.';
  peFilterEditor = 'Filter editor';
  peFilterName = 'Filter name';
  peFilter = 'Filter';
  pirsUnit = 'Pascal unit';

  fesFormCaption = 'New field';
  fesFieldType   = 'Field Type';
  fesData        = '&Data';
  fesCalculated  = '&Calculated';
  fesLookup      = '&Lookup';
  fesFieldProps  = 'Field properties';
  fesName        = '&Name:';
  fesType        = '&Type:';
  fesSize        = '&Size:';
  fesLookupDef   = 'Lookup definition';
  fesKeyfield    = '&Key fields:';
  fesDataset     = '&Dataset:';
  fesLookupKeys  = 'L&ookup keys:';
  fesResultField = '&Result Fields:';
  fesOkBtn       = 'OK';
  fesCancelBtn   = 'Cancel';
  fesFieldCanTBeC = 'Field %s cannot be created!';
  fesPersistentCompName = 'Co&mponent Name:';

  oisMoveUpHint = 'Move field up';
  oisMoveDownHint = 'Move field down';
  oisSelectAllHint = 'Select All Fields';
  oisUnselectAllHint = 'Unselect All';
  oisUnableToChangeParentOfControlToNewParent = 'Unable to change parent of '
    +'control "%s" to new parent "%s".%s%s';
  oisAddCollectionItem = '&Add Item';
  oisChangeClass = 'Change Class ...';
  oisChangeParent = 'Change Parent';
  lisUnableToFindParserForTool = 'Unable to find parser for tool "%s"';
  oisShowAllOutputLines = 'Show all output lines';
  lisUnableToFindParserWithName = 'Unable to find parser with name "%s"';

  // TChangeParentDlg
  oisShowClasses = 'Show classes';
  oisSelectedControl = 'Selected control';
  oisSelectedControls = 'Selected controls';
  oisCurrentParent = 'Current parent';
  oisCurrentParents = 'Current parents';

  // Dbgrid Columns editor
  dceAddFields = 'Add Fields';
  dceDeleteAll  = 'Delete All';
  dceFetchLabels = 'Fetch Labels';
  dceWillReplaceContinue = 'This will replace all captions from dataset. Continue?';
  dceColumnEditor = 'DBGrid Columns Editor';
  dceOkToDelete = 'This will delete all columns. Continue?';

  // IDE Text Converter
  itcsSearchAndReplace = 'Search and replace';

  // DBPropedits
  dpeUnableToRetrieveFieldsDefinitions = 'Unable to retrieve fields definition from dataset.';

  // PagesPropEditDlg
  oisPagesEditorDialog = 'Pages Editor';
  oisPages = 'Pages';
  oisAdd = 'Add';
  oisAddPage = 'Add Page';
  oisInsertPageName = 'Insert Page Name';
  oisRename = 'Rename';
  oisRenamePage = 'Rename Page';
  oisDeletePageQuestion = 'Do you want to delete the page?';

implementation

end.

