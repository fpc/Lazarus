unit RegIDEFileBrowser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  LazLoggerBase,
  Controls, Forms,
  LazIDEIntf, MenuIntf, IDECommands, IDEWindowIntf, BaseIDEIntf,
  frmFileBrowser, frmConfigFileBrowser;

type

  { TFileBrowserController }

  TFileBrowserController = class(TComponent)
  private
    FStartDir: TStartDir;
    FCustomDir: string;
    FWindow: TFileBrowserForm; // same as FileBrowserForm, which one is redundant?
    FNeedSave: Boolean;
    FSplitterPos: integer;
    procedure DoSelectDir(Sender: TObject);
    procedure ReadConfig; virtual;
    procedure SetSplitterPos(AValue: integer);
    procedure WriteConfig; virtual;
    procedure OnFormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
  protected
    procedure CreateWindow(AForm: TFileBrowserForm); virtual;
    { Called by file browser window }
    procedure DoOpenFile(Sender: TObject; const AFileName: string); virtual;
    { Called by file browser window }
    procedure DoConfig(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ShowConfig: Boolean;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property StartDir: TStartDir read FStartDir write FStartDir;
    property CustomDir: string read FCustomDir write FCustomDir;
    property SplitterPos: integer read FSplitterPos write SetSplitterPos;
  end;

var
  FileBrowserCreator: TIDEWindowCreator; // set by Register procedure

procedure ShowFileBrowser(Sender: TObject);
procedure Register;


implementation

const
  SConfigFile         = 'idebrowserwin.xml';
  KeyStartDir         = 'StartDir';
  KeyCustomDir        = 'CustomDir';
  KeySplitterPos      = 'SplitterPos';

resourcestring
  SFileBrowserIDEMenuCaption = 'File Browser';


{ TFileBrowserController }

procedure TFileBrowserController.ReadConfig;
begin
  with GetIDEConfigStorage(SConfigFile, True) do
    try
      FStartDir  := TStartDir(GetValue(KeyStartDir, Ord(sdProjectDir)));
      FCustomDir := GetValue(KeyCustomDir, '');
      FSplitterPos:=GetValue(KeySplitterPos, 150);
    finally
      Free;
    end;
end;

procedure TFileBrowserController.SetSplitterPos(AValue: integer);
begin
  if FSplitterPos=AValue then Exit;
  FSplitterPos:=AValue;
  FNeedSave:=true;
end;

procedure TFileBrowserController.WriteConfig;
begin
  with GetIDEConfigStorage(SConfigFile, True) do
    try
      SetDeleteValue(KeyStartDir, Ord(FStartDir), ord('C'));
      SetDeleteValue(KeyCustomDir, CustomDir, '');
      SetDeleteValue(KeySplitterPos, FSplitterPos, 150);
      FNeedSave := False;
    finally
      Free;
    end;
end;

procedure TFileBrowserController.CreateWindow(AForm: TFileBrowserForm);
var
  D: string;
begin
  FWindow := AForm;
  FWindow.Caption:=SFileBrowserIDEMenuCaption;
  FWindow.FreeNotification(Self);
  FWindow.OnOpenFile := @DoOpenFile;
  FWindow.OnConfigure := @DoConfig;
  FWindow.OnSelectDir := @DoSelectDir;
  FWindow.AddHandlerClose(@OnFormClose);
  FWindow.TV.Height:=FSplitterPos;
  D := FCustomDir;
  if (FStartDir = sdProjectDir) and Assigned(LazarusIDE.ActiveProject) then
    D := ExtractFilePath(LazarusIDE.ActiveProject.ProjectInfoFile);
  FWindow.Directory := D;
end;

procedure TFileBrowserController.DoOpenFile(Sender: TObject; const AFileName: string);
var
  Flags: TOpenFlags;
begin
  // Set up as desired. Maybe create config settings;
  Flags := [ofOnlyIfExists, ofAddToRecent, ofUseCache];
  LazarusIDE.DoOpenEditorFile(AFileName, 0, 0, Flags);
end;

procedure TFileBrowserController.OnFormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  SplitterPos:=FWindow.Splitter1.Top;
end;

procedure TFileBrowserController.DoSelectDir(Sender: TObject);
begin
  if FStartDir = sdLastOpened then
  begin
    FCustomDir := FWindow.Directory;
    FNeedSave  := True;
  end;
end;

procedure TFileBrowserController.DoConfig(Sender: TObject);
begin
  // Maybe later some reconfiguration of FWindow is needed after ShowConfig ?
  if ShowConfig then
    WriteConfig;
end;

constructor TFileBrowserController.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ReadConfig;
end;

destructor TFileBrowserController.Destroy;
begin
  if FNeedSave then
    WriteConfig;
  if Assigned(FWindow) then
    FreeAndNil(FWindow);
  inherited;
end;

function TFileBrowserController.ShowConfig: Boolean;
var
  F: TFileBrowserConfigForm;
begin
  F := TFileBrowserConfigForm.Create(Self);
  try
    F.CustomDir := Self.FCustomDir;
    F.StartDir := Self.StartDir;
    Result := F.ShowModal = mrOk;
    if Result then
    begin
      FCustomDir := F.CustomDir;
      FStartDir  := F.StartDir;
    end;
  finally
    F.Free;
  end;
end;

procedure TFileBrowserController.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (AComponent = FWindow) and (opRemove = Operation) then
    FWindow := nil;
end;

procedure ShowFileBrowser(Sender: TObject);
begin
  IDEWindowCreators.ShowForm(FileBrowserCreator.FormName,true);
end;

procedure CreateFileBrowser(Sender: TObject; aFormName: string;
  var AForm: TCustomForm; DoDisableAutoSizing: boolean);
var
  C: TFileBrowserController;
begin
  // sanity check to avoid clashing with another package that has registered a window with the same name
  if CompareText(aFormName,'FileBrowser')<>0 then begin
    DebugLn(['ERROR: CreateFileBrowser: there is already a form with this name']);
    exit;
  end;
  C := LazarusIDE.OwningComponent.FindComponent('IDEFileBrowserController') as TFileBrowserController;
  if (C = nil) then
    C := TFileBrowserController.Create(LazarusIDE.OwningComponent);
  IDEWindowCreators.CreateForm(AForm,TFileBrowserForm,true,C);
  AForm.Name:=aFormName;
  FileBrowserForm:=AForm as TFileBrowserForm;
  C.CreateWindow(FileBrowserForm);
  if not DoDisableAutoSizing then
    AForm.EnableAutoSizing;
end;

procedure Register;
var
  CmdCatViewMenu: TIDECommandCategory;
  ViewFileBrowserCommand: TIDECommand;
begin
  // search shortcut category
  CmdCatViewMenu:=IDECommandList.FindCategoryByName(CommandCategoryViewName);
  // register shortcut
  ViewFileBrowserCommand:=RegisterIDECommand(CmdCatViewMenu,
    'ViewFileBrowser',SFileBrowserIDEMenuCaption,
    CleanIDEShortCut,nil,@ShowFileBrowser);
  // register menu item in View menu
  RegisterIDEMenuCommand(itmViewMainWindows,
    ViewFileBrowserCommand.Name,
    SFileBrowserIDEMenuCaption, nil, nil, ViewFileBrowserCommand);

  // register dockable Window
  FileBrowserCreator:=IDEWindowCreators.Add(
    'FileBrowser',
    @CreateFileBrowser,nil,
    '200','100','400','400'  // default place at left=200, top=100, right=400, bottom=400
     // you can also define percentage values of screen or relative positions, see wiki
    );
end;

end.

