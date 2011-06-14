unit ChmLcl;

{$mode objfpc}{$H+}

interface

uses
  LazHelpIntf, HelpFPDoc;

const
  sLclUnits = 'LCL - Lazarus component library';

procedure RegisterLclHelpDatabase;

var
  LCLHelpDatabase: TFPDocHTMLHelpDatabase = nil;

implementation

procedure RegisterLclHelpDatabase;
var
  FPDocNode: THelpNode;
  BaseURL: THelpBaseURLObject;
  DirItem: THelpDBISourceDirectory;
begin
  if Assigned(LCLHelpDatabase) then Exit;
  LCLHelpDatabase := TFPDocHTMLHelpDatabase(
    HelpDatabases.CreateHelpDatabase(sLclUnits, TFPDocHTMLHelpDatabase, True));

  BaseURL := THelpBaseURLObject.Create;
  BaseURL.BaseURL := 'lcl.chm://';
  LCLHelpDatabase.BasePathObject := BaseURL;

  // FPDoc nodes for units in the LCL
  FPDocNode := THelpNode.CreateURL(LCLHelpDatabase,
                 'LCL - Lazarus Component Library Units',
                 'file://index.html');
  LCLHelpDatabase.TOCNode := THelpNode.Create(LCLHelpDatabase, FPDocNode);
  DirItem := THelpDBISourceDirectory.Create(FPDocNode, '$(LazarusDir)/lcl',
                            '*.pp;*.pas;*.inc', True);// and once as normal page
  LCLHelpDatabase.RegisterItem(DirItem);
end;

end.

