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
  oisCut = 'Cut';
  oisCopy = 'Copy';
  oisPaste = 'Paste';
  oisDelete = 'Delete';
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
  

  // ListView items editor
  sccsLvEdtCaption        = 'ListView editor';
  sccsLvEdtGrpLCaption    = ' Items ';
  sccsLvEdtGrpRCaption    = ' Item property ';
  sccsLvEdtlabCaption     = 'Label';
  sccsLvEdtImgIndexCaption= 'Image index';
  sccsLvEdtBtnAdd         = 'New';
  sccsLvEdtBtnDel         = 'Delete';
  oisCategory = 'Category';
  oisAction = 'Action';
  sccsLvEdtBtnAddSub      = 'Sub item';

  // Image editor strings
  sccsILEdtCaption = 'Image list editor';
  sccsILCmbImgSel  = ' Selected image ';
  sccsILCmbImgList = ' Images ';
  sccsILBtnAdd     = 'Add ...';
  sccsILBtnClear   = 'Clear';
  sccsILConfirme   = 'Confirme clear all images ?';

  // component editors
  cesStringGridEditor = 'StringGrid Editor ...';
  cesStringGridEditor2 = 'StringGrid Editor';
  nbcesAddPage = 'Add page';
  nbcesInsertPage = 'Insert page';
  nbcesDeletePage = 'Delete page';
  nbcesMovePageLeft = 'Move page left';
  nbcesMovePageRight = 'Move page right';
  nbcesShowPage = 'Show page ...';
  oisCreateDefaultEvent = 'Create default event';

  //checklistbox editor
  clbCheckListBoxEditor = 'CheckListBox Editor';
  clbUp = '&Up';
  clbDown = 'Do&wn';
  clbModify = 'Modify the Item';
  clbAdd = 'Add new Item';
  clbDelete = 'Delete the Item %d "%s"?';

  //checkgroup editor
  clbCheckGroupEditor = 'CheckGroup Editor';

  // Actions Editor
  cActionListEditorUnknownCategory = '(Unknown)';
  cActionListEditorAllCategory = '(All)';
  cActionListEditorEditCategory = 'Edit';
  cActionListEditorHelpCategory = 'Help';
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
  oisHelpTheHelpDatabaseWasUnableToFindFile = 'The help database %s%s%s was '
    +'unable to find file %s%s%s.';
  oisHelpTheMacroSInBrowserParamsWillBeReplacedByTheURL = 'The macro %s in '
    +'BrowserParams will be replaced by the URL.';
  oisHelpNoHTMLBrowserFoundPleaseDefineOneInHelpConfigureHe = 'No HTML '
    +'Browser found.%sPlease define one in Help -> Configure Help -> Viewers';
  oisHelpBrowserNotFound = 'Browser %s%s%s not found.';
  oisHelpBrowserNotExecutable = 'Browser %s%s%s not executable.';
  oisHelpErrorWhileExecuting = 'Error while executing %s%s%s:%s%s';
  oisHelpHelpNodeHasNoHelpDatabase = 'Help node %s%s%s has no Help Database';
  oisHelpHelpDatabaseDidNotFoundAViewerForAHelpPageOfType = 'Help Database %s%'
    +'s%s did not found a viewer for a help page of type %s';
  oisHelpAlreadyRegistered = '%s: Already registered';
  oisHelpNotRegistered = '%s: Not registered';
  oisHelpHelpDatabaseNotFound = 'Help Database %s%s%s not found';
  oisHelpHelpKeywordNotFoundInDatabase = 'Help keyword %s%s%s not found in '
    +'Database %s%s%s.';
  oisHelpHelpKeywordNotFound = 'Help keyword %s%s%s not found.';
  oisHelpHelpContextNotFoundInDatabase = 'Help context %s not found in '
    +'Database %s%s%s.';
  oisHelpHelpContextNotFound = 'Help context %s not found.';
  oisHelpNoHelpFoundForSource = 'No help found for line %d, column %d of %s.';
  oisLoadImageDialog = 'Load Image Dialog';
  oisOK = '&OK';
  oisCancel = '&Cancel';
  oisLoadPicture = 'Load picture';
  oisSavePicture = 'Save picture';
  oisClearPicture = 'Clear picture';
  oisLoad = '&Load';
  oisSave = '&Save';
  oisClear = 'C&lear';
  oisErrorLoadingImage = 'Error loading image';
  oisErrorLoadingImage2 = 'Error loading image %s%s%s:%s%s';
  oisOk2 = 'Ok';

implementation

end.

