unit msgjsonviewer; 

{$mode objfpc}

interface

uses
  Classes, SysUtils;

Resourcestring

  SCaption = 'JSON Viewer';
  SEmpty   = 'Empty document';
  SArray   = 'Array (%d elements)';
  SObject  = 'Object (%d members)';
  SNull    = 'null';

  SNewMember        = 'New object member';
  SNewMemberName    = 'Enter a name for the new member (type: %s)';
  SElement          = 'Element nr. %d';
  SDocumentModified = 'JSON document modified';
  SDocumentModifiedAction = 'The JSON data was changed but not saved.'+slinebreak+
                             'What do want to do ?';
  SDuplicateMemberName = 'Duplicate member name "%s".';
  SErrinvalidValue = 'Invalid value or wrong type: "%s"';
  SErrNoValidJSONClipBoard = 'The clipboard does not contain valid JSON data';
  SErrCreatingConfigDir = 'Could not create the configuration files directory "s"';
  SNoMoreMatches = 'No nodes match the criterium';

  SDiscard = 'Discard changes';
  SSaveData = 'Save the changes';
  SCancelClose = 'Do not close the window';
  SCancelPaste = 'Do not paste the new data';

implementation

end.

