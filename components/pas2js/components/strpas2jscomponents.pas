unit strpas2jscomponents;

{$mode objfpc}{$H+}

interface

Resourcestring
  rsActionListComponentEditor = 'HTM&L Element Actionlist Editor...';
  rsActionListCreateMissing = 'Create &actions for HTML tags...';
  rsActionListRemoveNoID = '&Remove actions without corresponding tag';
  rsErrNoHTMLFileNameForComponent = 'No HTML filename found for component %s';
  rsAllTagsHaveAction = 'All HTML tags with IDs already have a corresponding Action component.';
  rsHTMLActionsCreated = '%d HTML Element Action components were created';
  rsHTMLFragment = 'Pas2JS HTML Fragment Module';
  rsHTMLFragmentDescr = 'A Pas2JS datamodule to load and show a HTML fragment in the browser.';
  rsHTMLFIleFilters = 'HTML Files |*.html;*.htm|All files|*.*';
  rsPas2JSRest = 'Pas2JS REST';
  rsMenuRestShowData = 'Show data';
  rsMenuRestCreateFieldDefs = 'Create field defs';
  rsMenuRestCreateParamDefs = 'Create parameters';
  rsMenuGenHTMLForm = 'Create HTML form for data';
  rsNoMetaDataResource = 'No metadata resource present';
  rsNoResource = 'No resource present';
  rsNoFieldsAvailable = 'No fields available';
  rsHTMLCopiedToClipBoard = 'Data entry HTML copied to clipboard';
  rsNoMetaDataResourceCannotCreateFieldDefs = 'No metadata resource present, cannot get fielddefs';
  rsNoMetaDataResourceCannotCreateParams = 'No metadata resource present, cannot get parameters';
  rsNoResourceCannotCreateFieldDefs = 'No resource present, cannot get fielddefs';
  rsNoResourceCannotCreateParams = 'No resource present, cannot get params';
  rsNoResourceCannotShowData = 'No resource present, cannot show data';
  rsServerRequestFailedCannotCreateFieldDefs = 'Server request failed, cannot update fielddefs';
  rsServerRequestFailedCannotCreateParams = 'Server request failed, cannot update parameters';
  rsCreateFieldDefsCount = 'Added %d fielddefs';
  rsCreateFieldDefsNoNew = 'Fielddefs are up-to-date, no new fielddefs were added';
  rsCreateParamsCount = 'Added %d parameters';
  rsCreateParamsNoNew = 'Parameters are up-to-date, no new parameters were added';
  rsCaution = 'Please confirm';
  rsMayDeleteActionsInCode = 'Removing actions with missing tag may remove actions that reference a tag created in code.';
  rsRiskOK = 'I understand the risk, delete the actions.';
  rsCancel = 'Cancel.';

  rsEditingHTMLProp = 'Editing HTML property: %s';
  rsEditTemplate = 'Edit Template';

  rsCreateMissingColumns = 'Create missing columns';
  rsColumnsForAllFields = 'There is already a column for all fields.';

  rsStandardHTMLAction = 'Standard HTML Element action.';
  rsDBEditHTMLAction = 'Standard Data-Aware HTML Element action.';
  rsDBHTMLAction = 'Standard Data-Aware HTML read-only Element action.';
  rsDBButtonHTMLAction = 'Data-aware HTML button action.';

  rsActionListEditorNewAction = 'New Element Action';
  rsActionListEditorNewStdAction = 'New Standard Element Action';
  rsActionListEditorMoveDownAction = 'Move Down';
  rsActionListEditorMoveUpAction = 'Move Up';
  rsActionListEditorDeleteActionHint = 'Delete Element Action';
  rsActionListEditorDeleteAction = 'Delete';
  rsActionListEditorPanelDescrriptions = 'Show Description Panel';
  rsActionListEditorPanelToolBar = 'Show Toolbar';
  rsActionListEditor = 'HTML Element Action list editor';
  rsElementAction = 'Element action';
  rsErrorDeletingAction = 'Error when deleting element action';
  rsErrorWhileDeletingAction = 'An error occurred when deleting element action:%s%s';
  rsAddHTMLElementActions = 'Add HTML Element actions';
  rsUseDBAwareActions = 'Use Data-Aware actions';
  rsCreateServiceClient = 'Create Service client component';
  rsInvalidAPIReturned = 'The service URL "%s" returned an invalid API: %s';

  rsNoControl = '<none>';

implementation

end.

