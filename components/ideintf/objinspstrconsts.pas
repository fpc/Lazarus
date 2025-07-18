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
  oisItems = 'items: %s';
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
  oisShowPropertyFilter = 'Show Property Filter';
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
  sccsTrEdtNewItem         = 'New Item';
  sccsTrEdtNewSubItem      = 'New SubItem';
  sccsTrEdtDelete          = 'Delete';
  sccsTrEdtRename          = 'Rename';
  sccsTrEdtApply           = 'Apply';
  sccsTrEdtOpen            = 'Open file';
  sccsTrEdtSave            = 'Save to file';
  sccsTrEdtLabelImageIndex = 'Image Index%s:';
  sccsTrEdtLabelSelIndex   = 'Selected Index%s:';
  sccsTrEdtLabelStateIndex = 'State Index%s:';
  sccsTrEdtItem            = 'Item';
  sccsTrEdtOpenDialog      = 'Open';
  sccsTrEdtSaveDialog      = 'Save';
  sccsTrEdtYes             = '&Yes';
  sccsTrEdtNo              = '&No';
  sccsTrEdtCancel          = '&Cancel';
  sccsTrEdtInvalidIndex         = '"%s" has only %d images';
  sccsTrEdtImageListNotAssigned = '"%s" not assigned';
  sccsTrEdtConfirmationImagesLoss  = 'Information about node images can only be stored in XML file format. Continue anyway?';
  sccsTrEdtConfirmationTreeReplace = 'The current tree will be overwritten, continue?';
  sccsTrEdtOpenSaveDlgFilterXMLFiles = 'XML files (Node captions and properties)';
  sccsTrEdtOpenSaveDlgFilterTextFiles = 'Text files (Node captions only)';

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
  sccsLvEdtMsgOwnerData    = 'List items cannot be edited at design-time when OwnerData is enabled. Use Items.Count and OnData at run-time instead.';

  // Image editor strings
  oisImageListComponentEditor = 'I&mageList Editor ...';
  sccsILEdtCaption     = 'ImageList Editor';
  sccsILEdtGrpLCaption = 'Images';
  sccsILEdtGrpRCaption = 'Selected Image';
  sccsILEdtAdd         = 'Add';
  sccsILEdtAddImg      = 'Add image ...';
  sccsILEdtAddSingleResolution = 'Add image(s) with single resolution...';
  sccsILEdtAddMoreResolutions  = 'Add image with multiple resolutions ...';
  sccsILEdtAddSliced   = 'Add sliced ...';
  sccsILEdtReplace     = 'Replace';
  sccsILEdtReplaceImg  = 'Replace image ...';
  sccsILEdtReplaceSingleResolution = 'Replace single resolution of an image ...';
  sccsILEdtReplaceAllResolutions   = 'Replace all resolutions of an image ...';
  sccsILEdtPasteFromClipboard      = 'Paste image from clipboard';
  sccsILEdtRemove      = 'Remove';
  sccsILEdtDelete      = '&Delete image';
  sccsILEdtApply       = '&Apply';
  sccsILEdtClear       = '&Clear list';
  sccsILEdtMoveUp      = 'Move &Up';
  sccsILEdtMoveDown    = 'Move D&own';
  sccsILEdtSaveBtn     = 'Save';
  sccsILEdtSave        = '&Save image ...';
  sccsILEdtSaveAll     = 'Save all ...';
  sccsILEdtResolutionsBtn   = 'Resolutions';
  sccsILEdtAddNewResolution = 'Register new resolution (image size) ...';
  sccsILEdtDeleteResolution = 'Delete resolution (image size) ...';
  sccsILEdtDeleteResolutionConfirmation = 'Select the resolution (image size) to delete.';
  sccsILEdtCannotDeleteResolution = 'Cannot delete default resolution.';
  sccsILEdtImageWidthOfNewResolution = 'Image width of the new resolution:';
  sccsILEdtNoValidImageWidth = '"%s" is not a valid image width.';
  sccsILEdtransparentColor = 'Transparent Color:';
  sccsILEdtAdjustment  = 'Adjustment';
  sccsILEdtNone        = 'None';
  sccsILEdtAddSlicedIconError = 'Adding sliced images is not supported.';
  sccsILEdtCannotSlice = 'Source image size must be an integer multiple of the ImageList''s Width and Height.';
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
  sccsSGEdtMoveRowsCols = 'Move rows/columns';
  sccsSGEdtDelRow     = 'Delete row';
  sccsSGEdtDelCol     = 'Delete column';
  sccsSGEdtInsRow     = 'Insert row';
  sccsSGEdtInsCol     = 'Insert column';
  sccsSGEdtDelRowNo   = 'Delete row #%d?';
  sccsSGEdtDelColNo   = 'Delete column #%d?';
  sccsSGEdtTitle      = 'Title';
  sccsSGEdtEditColTitle = 'Edit column title';
  sccsSGEdtEditFixedColTitle = 'Edit fixed column title';
  sccsSGEdtEditRowHeader = 'Edit row header';
  sccsSGEdtColRowInfo = '(%d columns, %d rows)';

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
  oisUnload = 'Unload';
  oisUnloadHint = 'Unloads the current sample file';
  oisInputMaskEditor = 'Input Mask Editor';
  oisSaveLiteralCharacters = 'Save Literal Characters';
  oisInputMask = 'Input Mask:';
  oisSampleMasks = 'Sample Masks:';
  oisCharactersForBlanks = 'Characters for Blanks';
  oisTestInput = 'Test Input';
  oisEnableSetsHint = 'Only affects the EnableSets property of the Test Input';
  oisMaskSampleFilter = 'Input mask sample files (*.lem;*.dem)|*.lem;*.dem|All files|*';
  oisSelectInputMaskSample = 'Select input mask sample file';
  oisErrorReadingSampleFile = 'Error reading file %s';

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
  
  // TCommonDialogComponentEditor
  oisTestDialog = 'Test dialog ...';

  // property editors
  oisSort = 'Sort';
  oisDLinesDChars = '%d lines, %d chars';
  ois1LineDChars = '1 line, %d chars';
  oisStringsEditorDialog = 'Strings Editor Dialog';
  ois0Lines0Chars = '0 lines, 0 chars';
  oisOnlyOneFile = 'Only one file can be opened.';
  oisInvalidPropertyValue = 'Invalid property value';
  oisInfinityNotSupported = 'Setting a floating point property to positive or negative Infinity at design time is not supported';
  oisNaNNotSupported = 'Setting a floating point property to NaN at design time is not supported';
  oisValueOutOfRange = 'Property value out of range.';
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
  fesNoFields = 'It was not possible to get the dataset fields list.';
  fesCheckDset = 'Check dataset settings.';
  fesErrorMessage = 'Error message:' + LineEnding + '%s';
  fesFlTitle = 'FieldDefs';
  fesNoFieldsNote = 'Fields list is not available, can''t check for duplicates.';
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

  // TBufDatasetDesignEditor
  lrsDatasetActive = 'Dataset is already active, close it first.';
  lrsCreateDataset = 'Create dataset';
  lrsLoadFromFile = 'Load data from file';
  lrsSaveToFile = 'Save data to file';
  lrsSelectDataFileName = 'Select a file with data to load into dataset';
  lrsProvideDataFileName = 'Select a data file to save data to';
  lrsBufDSFilters = 'XML data files|*.xml;Binary data files|*.dat';
  lrsCopyFromDataset = 'Copy data from other dataset';
  lrsNoDatasetsAvailableForCopy = 'No dataset available to copy data from.';

  // File types
  oisAllFiles = 'All files';

implementation

end.

