{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit ObjInspStrConsts;

{$mode objfpc}{$H+}

interface

resourcestring

  // Object Inspector
  oisObjectInspector = 'Object Inspector';
  oisAll = 'All';
  oisError = 'Error';
  oisItemsSelected = '%u items selected';
  
  oiscAdd = '&Add';
  oiscDelete = '&Delete';
  oisConfirmDelete = 'Confirm delete';
  oisDeleteItem = 'Delete item %s%s%s?';
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

  oisProperties='Properties';
  oisEvents='Events';
  oisFavorites = 'Favorites';

  //Object Inspector Popup Menu
  oisSetToDefault = 'Set to default: %s';
  oisSetToDefaultValue = 'Set to default value';
  oisAddToFavorites = 'Add to Favorites';
  oisRemoveFromFavorites = 'Remove from Favorites';
  oisUndo = 'Undo';
  oisFinddeclaration = 'Jump to declaration';
  oisCut = 'Cut';
  oisCopy = 'Copy';
  oisPaste = 'Paste';
  oisDelete = 'Delete';
  rscdMoveUp = 'Move up';
  rscdMoveDown = 'Move down';
  rscdOK = 'OK';
  oisShowHints = 'Show Hints';
  oisShowComponentTree = 'Show Component Tree';
  oisOptions = 'Options';

  
  // typeinfo
  oisValue = 'Value:';
  oisInteger = 'Integer';
  oisInt64 = 'Int64';
  oisBoolean = 'Boolean';
  oisEnumeration = 'Enumeration';
  oisChar = 'Char';
  
  // TreeView Items Editor
  sccsTrEdtCaption         = 'TreeView Items Editor';
  sccsTrEdt                = 'Edit TreeView Items...';
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
  sccsLvEdt                = 'Edit ListView Items...';
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
  sccsILEdtCaption     = 'ImageList Editor';
  sccsILEdtGrpLCaption = 'Images';
  sccsILEdtGrpRCaption = 'Selected Image';
  sccsILEdtAdd         = 'Add...';
  sccsILEdtDelete      = 'Delete';
  sccsILEdtApply       = 'Apply';
  sccsILEdtClear       = 'Clear';
  sccsILEdtMoveUp      = 'Move Up';
  sccsILEdtMoveDown    = 'Move Down';
  sccsILEdtSave        = 'Save...';
  sccsILEdtransparentColor = 'Transparent Color:';
  sccsILEdtAdjustment  = 'Adjustment';
  sccsILEdtNone        = 'None';
  sccsILEdtStretch     = 'Stretch';
  sccsILEdtCrop        = 'Crop';
  sccsILEdtCenter      = 'Center';
  rscdRight = 'Right';
  rscdVisible = 'Visible';
  rscdAutoSize = 'Auto Size';
  sccsILEdtOpenDialog  = 'Add Images';
  sccsILEdtSaveDialog  = 'Save Image';
  
  // StringGrid Editor
  sccsSGEdt           = 'Edit StringGrid...';
  sccsSGEdtCaption    = 'StringGrid Editor';
  sccsSGEdtGrp        = 'String Grid';
  sccsSGEdtApply      = 'Apply';
  sccsSGEdtClean      = 'Clean';
  sccsSGEdtLoad       = 'Load...';
  sccsSGEdtSave       = 'Save...';
  sccsSGEdtOpenDialog = 'Open';
  sccsSGEdtSaveDialog = 'Save';
  sccsSGEdtMoveRowsCols = 'Move Rows/Cols';

  // component editors
  nbcesAddPage  = 'Add page';
  nbcesInsertPage = 'Insert page';
  nbcesDeletePage = 'Delete page';
  nbcesMovePageLeft = 'Move page left';
  nbcesMovePageRight = 'Move page right';
  nbcesShowPage = 'Show page ...';
  oisCreateDefaultEvent = 'Create default event';
  tccesAddTab  = 'Add tab';
  tccesInsertTab = 'Insert tab';
  tccesDeleteTab = 'Delete tab';
  tccesMoveTabLeft = 'Move tab left';
  tccesMoveTabRight = 'Move tab right';

  //checklistbox editor
  clbCheckListBoxEditor = 'CheckListBox Editor';
  clbUp = '&Up';
  clbDown = 'Do&wn';
  clbModify = 'Modify the Item';
  clbAdd = 'Add new Item';
  clbDelete = 'Delete the Item %d "%s"?';

  //checkgroup editor
  clbCheckGroupEditor = 'CheckGroup Editor';
  clbDisable = 'Popup to disable/enable items';
  
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
  cActionListEditorHelpCategory = 'Help';
  oisCategory = 'Category';
  oisAction = 'Action';
  oisMasks = 'Masks...';
  oisSaveLiteralCharacters = 'Save Literal Characters';
  oisInputMask = 'Input Mask:';
  oisSampleMasks = 'Sample Masks:';
  oisCharactersForBlanks = 'Characters for Blanks';
  oisTestInput = 'Test Input';
  oisOpenMaskFile = 'Open masks file (*.dem)';
  cActionListEditorDialogCategory = 'Dialog';
  cActionListEditorFileCategory = 'File';
  cActionListEditorDatabaseCategory = 'Database';
  
  oisEditActionList = 'Edit action list...';
  oisActionListEditor = 'Action List Editor';
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
  oiStdActHelpContentsHeadLine = '&Contents';
  oiStdActHelpTopicSearchHeadLine = '&Topic Search';
  oiStdActHelpHelpHelpHeadLine = '&Help on Help';
  oiStdActFileOpenHeadLine = '&Open...';
  oiStdActFileSaveAsHeadLine = 'Save &As...';
  oiStdActFileExitHeadLine = 'E&xit';
  oiStdActColorSelect1HeadLine = 'Select &Color...';
  oiStdActFontEditHeadLine = 'Select &Font...';

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
  oiStdActFileOpenShortCut = 'Ctrl+O';
  oiStdActEditCutShortHint = 'Cut';
  oiStdActEditCopyShortHint = 'Copy';
  oiStdActEditPasteShortHint = 'Paste';
  oiStdActEditSelectAllShortHint = 'Select All';
  oiStdActEditUndoShortHint = 'Undo';
  oiStdActEditDeleteShortHint = 'Delete';

  oiStdActHelpContentsHint = 'Help Contents';
  oiStdActHelpTopicSearchHint = 'Topic Search';
  oiStdActHelpHelpHelpHint = 'Help on help';
  oiStdActFileOpenHint = 'Open';
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
  oisComponents = 'Components';
  oiStdActDataSetRefreshHint = 'Refresh';
  
  oisStdActionListEditor = 'Standard Action Classes';
  oisStdActionListEditorClass = 'Available Action Classes:';

  // TFileNamePropertyEditor
  oisSelectAFile = 'Select a file';
  oisPropertiesOf = 'Properties of %s';
  oisAllFiles = 'All files';
  
  // TCommonDialogComponentEditor
  oisTestDialog = 'Test dialog...';

  // property editors
  oisSort = 'Sort';
  oisDLinesDChars = '%d lines, %d chars';
  ois1LineDChars = '1 line, %d chars';
  oisStringsEditorDialog = 'Strings Editor Dialog';
  ois0Lines0Chars = '0 lines, 0 chars';
  oisInvalidPropertyValue = 'Invalid property value';
  oisNone = '(none)';
  oisComponentNameIsNotAValidIdentifier = 'Component name %s%s%s is not a '
    +'valid identifier';
  oisLoadImageDialog = 'Load Image Dialog';
  oisOK = '&OK';
  oisPEPicture = 'Picture';
  oisCancel = '&Cancel';
  oisLoadPicture = 'Load picture';
  oisSavePicture = 'Save picture';
  oisClearPicture = 'Clear picture';
  oisLoad = '&Load';
  oisSave = '&Save';
  oisClear = 'C&lear';
  oisPEOpenImageFile = 'Open image file';
  oisPESaveImageAs = 'Save image as';
  oisErrorLoadingImage = 'Error loading image';
  oisErrorLoadingImage2 = 'Error loading image %s%s%s:%s%s';
  oisOk2 = 'Ok';
  oisCreateANewPascalUnit = 'Create a new pascal unit.';
  rscdColumnEditor = 'Column Editor';
  rscdCaption = 'Caption';
  rscdInvalidNumericValue = 'Invalid numeric Value';
  rscdWidth = 'Width';
  rscdAlignment = 'Alignment';
  rscdLeft = 'Left';

implementation

end.

