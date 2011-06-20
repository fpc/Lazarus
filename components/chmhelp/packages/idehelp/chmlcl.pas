unit ChmLcl;

{$mode objfpc}{$H+}

interface

uses
  Classes, LazHelpIntf, HelpFPDoc;

const
  sLclUnits = 'LCL - Lazarus component library';

type

  { TLclChmHelpDatabase }

  TLclChmHelpDatabase = class(TFPDocHTMLHelpDatabase)
  private
    FBaseURL: THelpBaseURLObject;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

procedure RegisterLclHelpDatabase;

var
  LCLHelpDatabase: TLclChmHelpDatabase = nil;

implementation

procedure RegisterLclHelpDatabase;
var
  FPDocNode: THelpNode;
  DirItem: THelpDBISourceDirectory;
begin
  if Assigned(LCLHelpDatabase) then Exit;
  LCLHelpDatabase := TLclChmHelpDatabase(
    HelpDatabases.CreateHelpDatabase(sLclUnits, TLclChmHelpDatabase, True));

  // FPDoc nodes for units in the LCL
  FPDocNode := THelpNode.CreateURL(LCLHelpDatabase,
                 'LCL - Lazarus Component Library Units',
                 'file://index.html');
  LCLHelpDatabase.TOCNode := THelpNode.Create(LCLHelpDatabase, FPDocNode);
  DirItem := THelpDBISourceDirectory.Create(FPDocNode, '$(LazarusDir)/lcl',
                            '*.pp;*.pas;*.inc', True);// and once as normal page
  LCLHelpDatabase.RegisterItem(DirItem);
end;

{ TLclChmHelpDatabase }

constructor TLclChmHelpDatabase.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FBaseURL := THelpBaseURLObject.Create;
  FBaseURL.BaseURL := 'lcl.chm://';
  BasePathObject := FBaseURL;
end;

destructor TLclChmHelpDatabase.Destroy;
begin
  FBaseURL.Free;
  inherited Destroy;
end;

end.

