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
  oisSetToDefault = 'Set to default: %s';
  oisSetToDefaultValue = 'Set to default value';
  
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

  // Actions Editor
  cActionListEditorUnknownCategory='(Unknown)';
  oisEditActionList = 'Edit action list...';
  oisActionListEditor = 'Action List Editor';
  oisAdd = 'Add';
  cActionListEditorAllCategory='(All)';

  // TFileNamePropertyEditor
  oisSelectAFile = 'Select a file';
  oisAllFiles = 'All files (*.*)|*.*';

  // property editors
  oisSort = 'Sort';
  oisStringsEditorDialog = 'Strings Editor Dialog';
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
  oisLoadImageDialog = 'Load Image Dialog';
  oisOK = '&OK';
  oisCancel = '&Cancel';
  oisLoad = '&Load';
  oisSave = '&Save';
  oisCLear = 'C&lear';
  oisErrorLoadingImage = 'Error loading image';
  oisErrorLoadingImage2 = 'Error loading image %s%s%s:%s%s';

implementation

end.

