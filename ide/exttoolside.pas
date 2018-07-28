unit ExtToolsIDE;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  Forms,
  // LazUtils
  LazLogger,
  // IDEIntf
  IDEExternToolIntf, IDEMsgIntf, PackageIntf, LazIDEIntf,
  // IDE
  ExtTools;

type
  { TExternalToolIDE }

  TExternalToolIDE = class(TExternalTool)
  private
    procedure SyncAutoFree({%H-}aData: PtrInt); // (main thread)
  protected
    procedure CreateView; override;
    procedure QueueAsyncAutoFree; override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { TExternalToolsIDE }

  TExternalToolsIDE = class(TExternalTools)
  private
    function RunExtToolHandler(ToolOptions: TIDEExternalToolOptions): boolean;
  protected
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    function GetIDEObject(ToolData: TIDEExternalToolData): TObject; override;
    procedure HandleMesages; override;
  end;


implementation

{ TExternalToolIDE }

constructor TExternalToolIDE.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
end;

destructor TExternalToolIDE.Destroy;
begin
  inherited Destroy;
end;

procedure TExternalToolIDE.CreateView;
// this tool generates parsed output => auto create view
var
  View: TExtToolView;
begin
  if ViewCount>0 then exit;
  if (ViewCount=0) and (ParserCount>0) and (IDEMessagesWindow<>nil) then
  begin
    View := IDEMessagesWindow.CreateView(Title);
    if View<>nil then
      AddView(View);
  end;
end;

procedure TExternalToolIDE.SyncAutoFree(aData: PtrInt);
begin
  AutoFree;
end;

procedure TExternalToolIDE.QueueAsyncAutoFree;
begin
  Application.QueueAsyncCall(@SyncAutoFree,0);
end;

{ TExternalToolsIDE }

constructor TExternalToolsIDE.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FToolClass := TExternalToolIDE;
  RunExternalTool := @RunExtToolHandler;
end;

destructor TExternalToolsIDE.Destroy;
begin
  inherited Destroy;
end;

function TExternalToolsIDE.GetIDEObject(ToolData: TIDEExternalToolData): TObject;
begin
  Result:=nil;
  if ToolData=nil then exit;
  if ToolData.Kind=IDEToolCompileProject then begin
    Result:=LazarusIDE.ActiveProject;
  end else if ToolData.Kind=IDEToolCompilePackage then begin
    Result:=PackageEditingInterface.FindPackageWithName(ToolData.ModuleName);
  end else if ToolData.Kind=IDEToolCompileIDE then begin
    Result:=LazarusIDE;
  end;
end;

procedure TExternalToolsIDE.HandleMesages;
begin
  Application.ProcessMessages;
end;

function TExternalToolsIDE.RunExtToolHandler(ToolOptions: TIDEExternalToolOptions): boolean;
begin
  {$IFDEF VerboseExtToolThread}
  debugln(['TExternalToolsIDE.RunExtToolHandler ',ToolOptions.Title,' exe="',ToolOptions.Executable,'" params="',ToolOptions.CmdLineParams,'"']);
  {$ENDIF}
  //if ToolOptions.Parsers.Count=0 then
  Result := RunToolAndDetach(ToolOptions)
end;

end.

