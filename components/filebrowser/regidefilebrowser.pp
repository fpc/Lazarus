unit regidefilebrowser;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  frmfilebrowser,
  frmconfigfilebrowser;

type
  TFileBrowserController = class(TComponent)
  private
    FStartDir: TStartDir;
    FCustomDir: string;
    FWindow: TFileBrowserForm;
    FNeedSave: Boolean;
    procedure DoLoadLayout(Sender: TObject);
    procedure DoSelectDir(Sender: TObject);
    procedure DoSaveLayout(Sender: TObject);
    procedure ReadConfig; virtual;
    procedure WriteConfig; virtual;
  protected
    procedure CreateWindow; virtual;
    { Called by file browser window }
    procedure DoOpenFile(Sender: TObject; const AFileName: string); virtual;
    { Called by file browser window }
    procedure DoConfig(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ShowWindow;
    function ShowConfig: Boolean;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property StartDir: TStartDir read FStartDir write FStartDir;
    property CustomDir: string read FCustomDir write FCustomDir;
  end;

procedure Register;


implementation

uses
  Controls,
  Forms,
  lazideintf,
  menuintf,
  baseideintf,
  idewindowintf;

const
  SConfigFile         = 'idebrowserwin.xml';
  KeyStartDir         = 'StartDir';
  KeyCustomDir        = 'CustomDir';
  KeySplitterPos      = 'SplitterPos';

resourcestring
  SFileBrowserIDEMenu = 'IDEFileBrowser';
  SFileBrowserIDEMenuCaption = 'File Browser window';


{ TFileBrowserController }

procedure TFileBrowserController.ReadConfig;
begin
  with GetIDEConfigStorage(SConfigFile, True) do
    try
      FStartDir  := TStartDir(GetValue(KeyStartDir, Ord(sdProjectDir)));
      FCustomDir := GetValue(KeyCustomDir, '');
    finally
      Free;
    end;
end;

procedure TFileBrowserController.WriteConfig;
begin
  with GetIDEConfigStorage(SConfigFile, True) do
    try
      SetValue(KeyStartDir, Ord(FstartDir));
      SetValue(KeyCustomDir, CustomDir);
      FNeedSave := False;
    finally
      Free;
    end;
end;

procedure TFileBrowserController.CreateWindow;
var
  D: string;
begin
  FWindow := TFileBrowserForm.Create(Self);
  FWindow.FreeNotification(Self);
  FWindow.OnOpenFile := @DoOpenFile;
  FWindow.OnConfigure := @DoConfig;
  FWindow.OnSelectDir := @DoSelectDir;
  FWindow.OnSaveLayout := @DoSaveLayout;
  FWindow.OnLoadLayout := @DoLoadLayout;
  IDEDialogLayoutList.ApplyLayout(FWindow);
  D       := FCustomDir;
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

procedure TFileBrowserController.DoLoadLayout(Sender: TObject);
begin
  with GetIDEConfigStorage(SConfigFile, True) do
  begin
    try
      FWindow.Top := GetValue('Position/Top', FWindow.Top);
      FWindow.Left := GetValue('Position/Left', FWindow.Left);
      FWindow.TV.Height := GetValue(KeySplitterPos, FWindow.TV.Height);
    finally
      Free;
    end;
  end;
end;

procedure TFileBrowserController.DoSelectDir(Sender: TObject);
begin
  if FStartDir = sdLastOpened then
  begin
    FCustomDir := FWindow.Directory;
    FNeedSave  := True;
  end;
end;

procedure TFileBrowserController.DoSaveLayout(Sender: TObject);
begin
  IDEDialogLayoutList.SaveLayout(FWindow);
  with GetIDEConfigStorage(SConfigFile, True) do
    try
      SetValue(KeySplitterPos, FWindow.TV.Height);
      SetValue('Position/Top', FWindow.Top);
      SetValue('Position/Left', FWindow.Left);
      FNeedSave := False;
    finally
      Free;
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

procedure TFileBrowserController.ShowWindow;
begin
  if (FWindow = nil) then
  begin
    CreateWindow;
    FWindow.Show;
  end
  else
    FWindow.BringToFront;
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
var
  C: TFileBrowserController;
begin
  C := Application.FindComponent('IDEFileBrowserController') as TFileBrowserController;
  if (C = nil) then
    C := TFileBrowserController.Create(Application);
  C.ShowWindow;
end;

procedure Register;
begin
  RegisterIDEMenuCommand(itmViewMainWindows, SFileBrowserIDEMEnu,
    SFileBrowserIDEMenuCaption, nil, @ShowFileBrowser, nil, '');
end;

end.

