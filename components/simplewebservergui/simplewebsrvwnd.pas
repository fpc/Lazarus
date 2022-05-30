{
  Author: Mattias Gaertner
}
unit SimpleWebSrvWnd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process,
  // lazutils
  LazFileUtils, LazLogger, LazUTF8, FileUtil,
  // lcl
  Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ComCtrls, Menus,
  Clipbrd,
  // SynEdit
  SynEdit,
  // IDEIntf
  IDECommands, MenuIntf, IDEWindowIntf, LazIDEIntf,
  IDEOptEditorIntf, IDEDialogs, IDEOptionsIntf, SimpleWebSrvOptionsFrame,
  SimpleWebSrvController, SimpleWebSrvAdd, SimpleWebSrvStrConsts;

const
  SimpleWebSrvWindowName = 'SimpleWebServerWindow';
type

  { TSimpleWebServerWindow }

  TSimpleWebServerWindow = class(TForm)
    AddButton: TButton;
    ConfigureButton: TButton;
    DeleteButton: TButton;
    DeleteSelectedButton: TButton;
    GetLocationsButton: TButton;
    ILProcesses: TImageList;
    ListeningEdit: TEdit;
    ListeningLabel: TLabel;
    LocationsListView: TListView;
    LocationsPopupMenu: TPopupMenu;
    LogSplitter: TSplitter;
    CopyLocationMenuItem: TMenuItem;
    CopyDirectoryPathMenuItem: TMenuItem;
    CopyOriginMenuItem: TMenuItem;
    MsgSplitter: TSplitter;
    OptionsPanel: TPanel;
    ServerLogGroupBox: TGroupBox;
    ServerLogSplitter: TSplitter;
    ServerLogSynEdit: TSynEdit;
    StartStopServerButton: TButton;
    procedure AddButtonClick(Sender: TObject);
    procedure ConfigureButtonClick(Sender: TObject);
    procedure CopyDirectoryPathMenuItemClick(Sender: TObject);
    procedure CopyLocationMenuItemClick(Sender: TObject);
    procedure CopyOriginMenuItemClick(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LocationsListViewItemChecked(Sender: TObject; Item: TListItem);
    procedure LocationsListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure LocationsPopupMenuPopup(Sender: TObject);
    procedure StartStopServerButtonClick(Sender: TObject);
  private
    function GetController: TSimpleWebServerController;
    procedure OnLocationsChanged(Sender: TObject);
    procedure OnServerLog(Sender: TObject; OutLines: TStrings);
    procedure OnStateChanged(Sender: TObject; Instance: TSWSInstance);
    procedure UpdateLocationsView;
    procedure UpdateButtons;
    function RowToControllerObj(Row: integer): TObject;
    function CaptionToControllerObj(s: string): TObject;
    procedure DeleteLocation(aCaption: string);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property Controller: TSimpleWebServerController read GetController;
  end;

var
  SimpleWebServerWindow: TSimpleWebServerWindow;
  SimpleWebServerWindowCreator: TIDEWindowCreator; // set by Register
  ViewSimpleWebServerWindowCommand: TIDECommand; // set by Register

procedure ShowSimpleWebServerWindow(Sender: TObject);
procedure CreateSimpleWebServerWindow(Sender: TObject; aFormName: string;
                          var AForm: TCustomForm; DoDisableAutoSizing: boolean);

procedure Register;

implementation

const
  ServerPrefix = 'Port:';

{$R *.lfm}

procedure Register;
var
  CmdCatView: TIDECommandCategory;
begin
  SimpleWebServerController:=TSimpleWebServerController.Create(nil);
  SimpleWebServerController.Options.LoadSafe;

  CmdCatView:=IDECommandList.FindCategoryByName(CommandCategoryViewName);
  if CmdCatView=nil then
    raise Exception.Create('simplewebsrv: command category '+CommandCategoryViewName+' not found');

  // Windows - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  SimpleWebServerWindowCreator:=IDEWindowCreators.Add(SimpleWebSrvWindowName,
       @CreateSimpleWebServerWindow,nil,
       '20%','20%','+50%','+20%');

  // Windows - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ViewSimpleWebServerWindowCommand:=RegisterIDECommand(CmdCatView, 'SimpleWebServer',
    rsSWSTitle, CleanIDEShortCut, CleanIDEShortCut, nil, @ShowSimpleWebServerWindow);
  RegisterIDEMenuCommand(itmViewMainWindows, 'ViewSimpleWebServer',
    rsSWSTitle, nil, nil, ViewSimpleWebServerWindowCommand);

  // Options Frame - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  SimpleWebServerOptionID:=RegisterIDEOptionsEditor(GroupEnvironment,
      TSimpleWebSrvOptsFrame,SimpleWebServerOptionID)^.Index;
  SimpleWebServerController.Options.Apply;
  SimpleWebServerController.HookMacros;
end;

procedure ShowSimpleWebServerWindow(Sender: TObject);
begin
  if SimpleWebServerWindow=nil then
    IDEWindowCreators.ShowForm(SimpleWebServerWindowCreator.FormName,true)
  else
    IDEWindowCreators.ShowForm(SimpleWebServerWindow,true);
end;

procedure CreateSimpleWebServerWindow(Sender: TObject; aFormName: string;
  var AForm: TCustomForm; DoDisableAutoSizing: boolean);
begin
  if not SameText(aFormName,SimpleWebSrvWindowName) then begin
    debugln(['ERROR: CreateSimpleWebServerWindow: there is already a form with this name']);
    exit;
  end;
  IDEWindowCreators.CreateForm(AForm,TSimpleWebServerWindow,DoDisableAutoSizing,
    LazarusIDE.OwningComponent);
  AForm.Name:=aFormName;
end;

{ TSimpleWebServerWindow }

procedure TSimpleWebServerWindow.StartStopServerButtonClick(Sender: TObject);
begin
  case Controller.MainSrvState of
    swssNone: Controller.StartMainServer(true);
    swssStopping: ;
    swssStarting: ;
    swssRunning: Controller.StopMainServer(true);
  end;
end;

function TSimpleWebServerWindow.GetController: TSimpleWebServerController;
begin
  Result:=SimpleWebServerController;
end;

procedure TSimpleWebServerWindow.OnLocationsChanged(Sender: TObject);
begin
  UpdateLocationsView;
end;

procedure TSimpleWebServerWindow.OnServerLog(Sender: TObject; OutLines: TStrings
  );
var
  CaretWasAtEnd: Boolean;
begin
  ServerLogSynEdit.BeginUpdate(false);
  try
    CaretWasAtEnd:=ServerLogSynEdit.CaretY>=ServerLogSynEdit.Lines.Count;
    ServerLogSynEdit.Lines.AddStrings(OutLines);
    if CaretWasAtEnd then
      // auto scroll
      ServerLogSynEdit.CaretXY:=Point(1,ServerLogSynEdit.Lines.Count);
  finally
    ServerLogSynEdit.EndUpdate;
  end;
end;

procedure TSimpleWebServerWindow.OnStateChanged(Sender: TObject;
  Instance: TSWSInstance);
begin
  if Instance=Controller.MainSrvInstance then
    UpdateButtons;
end;

procedure TSimpleWebServerWindow.UpdateLocationsView;
var
  i, Row: Integer;
  Loc: TSWSLocation;
  Item: TListItem;
  Server: TSWSInstance;
begin
  LocationsListView.BeginUpdate;
  try
    Row:=0;
    for i:=0 to Controller.LocationCount-1 do
    begin
      Loc:=Controller.Locations[i];
      if LocationsListView.Items.Count>Row then
      begin
        Item:=LocationsListView.Items[Row];
        Item.SubItems[0]:=Loc.Path;
        Item.SubItems[1]:=Loc.Origin;
      end
      else begin
        Item:=LocationsListView.Items.Add;
        Item.SubItems.Add(Loc.Path);
        Item.SubItems.Add(Loc.Origin);
      end;
      Item.Caption:=Loc.Location;
      Item.Checked:=Loc.Enable;
      inc(Row);
    end;

    for i:=0 to Controller.ServerCount-1 do
    begin
      Server:=Controller.Servers[i];
      if Server=Controller.MainSrvInstance then continue;
      if LocationsListView.Items.Count>Row then
      begin
        Item:=LocationsListView.Items[Row];
        Item.SubItems[0]:=Server.PathUsed;
        Item.SubItems[1]:=Server.Origin;
      end
      else begin
        Item:=LocationsListView.Items.Add;
        Item.SubItems.Add(Server.Path);
        Item.SubItems.Add(Server.Origin);
      end;
      Item.Caption:=ServerPrefix+IntToStr(Server.Port);
      Item.Checked:=Server.State in [swssStarting,swssRunning];
      inc(Row);
    end;

    while LocationsListView.Items.Count>Row do
      LocationsListView.Items.Delete(LocationsListView.Items.Count-1);
  finally
    LocationsListView.EndUpdate;
  end;

  DeleteButton.Enabled:=LocationsListView.Selected<>nil;
end;

procedure TSimpleWebServerWindow.UpdateButtons;
begin
  case Controller.MainSrvState of
    swssNone: StartStopServerButton.Caption:=rsSWStartServer;
    swssStarting: ;
    swssRunning: StartStopServerButton.Caption:=rsSWStopServer;
    swssStopping: ;
  end;
  StartStopServerButton.Enabled:=Controller.MainSrvState in [swssNone,swssRunning];
end;

function TSimpleWebServerWindow.RowToControllerObj(Row: integer): TObject;
begin
  Result:=nil;
  if (Row<0) or (Row>=LocationsListView.Items.Count) then
    exit;
  Result:=CaptionToControllerObj(LocationsListView.Items[Row].Caption);
end;

function TSimpleWebServerWindow.CaptionToControllerObj(s: string): TObject;
var
  aPort: LongInt;
begin
  if LeftStr(s,length(ServerPrefix))=ServerPrefix then
  begin
    aPort:=StrToIntDef(copy(s,length(ServerPrefix)+1,5),-1);
    if (aPort>0) and (aPort<=65535) then
    begin
      Result:=Controller.FindServerWithPort(aPort);
      if Result<>nil then exit;
    end;
  end;

  Result:=Controller.FindLocation(s);
end;

procedure TSimpleWebServerWindow.DeleteLocation(aCaption: string);
var
  Obj: TObject;
  Server: TSWSInstance;
  r: Integer;
begin
  Obj:=CaptionToControllerObj(aCaption);
  if Obj is TSWSLocation then
  begin
    r:=IDEMessageDialog(rsSWDelete,
      Format(rsSWDeleteLocation, [aCaption]), mtConfirmation, [mbYes, mbNo]);
    if r=mrYes then
      Controller.DeleteLocation(aCaption);
  end else if Obj is TSWSInstance then
  begin
    Server:=TSWSInstance(Obj);
    r:=IDEMessageDialog(rsSWDelete,
      Format(rsSWDeleteServerAt, [aCaption]), mtConfirmation, [mbYes, mbNo]);
    if r=mrYes then
      Controller.StopServer(Server,true);
  end;
end;

constructor TSimpleWebServerWindow.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  SimpleWebServerWindow:=Self;
end;

destructor TSimpleWebServerWindow.Destroy;
begin
  inherited Destroy;
  SimpleWebServerWindow:=nil;
end;

procedure TSimpleWebServerWindow.FormCreate(Sender: TObject);
begin
  ServerLogSynEdit.Font.Name:=SynDefaultFontName;
  ServerLogSynEdit.Font.Height:=SynDefaultFontHeight;

  Controller.AddHandlerLocationsChanged(@OnLocationsChanged);
  Controller.AddHandlerServerLog(@OnServerLog);
  Controller.AddHandlerStateChanged(@OnStateChanged);

  // fetch existing log
  OnServerLog(Self,Controller.LogLines);

  UpdateButtons;
  UpdateLocationsView;

  Caption:=rsSWSTitle;
  ConfigureButton.Caption:=rsSWConfigure;
  AddButton.Caption:=rsSWAdd;
  DeleteButton.Caption:=rsSWDelete2;

  LocationsListView.Columns[0].Caption:=rsSWLocation;
  LocationsListView.Columns[1].Caption:=rsSWWorkingDirectory;
  LocationsListView.Columns[2].Caption:=rsSWOrigin;
  LocationsListView.Columns[3].Caption:=rsSWNote;

  CopyLocationMenuItem.Caption:=rsSWCopyLocation;
  CopyDirectoryPathMenuItem.Caption:=rsSWCopyWorkingDirectoryPath;
  CopyOriginMenuItem.Caption:=rsSWCopyOrigin;
end;

procedure TSimpleWebServerWindow.ConfigureButtonClick(Sender: TObject);
begin
  LazarusIDE.DoOpenIDEOptions(TSimpleWebSrvOptsFrame);
end;

procedure TSimpleWebServerWindow.AddButtonClick(Sender: TObject);
begin
  ShowAddSWSLocationDialog(Controller);
end;

procedure TSimpleWebServerWindow.CopyDirectoryPathMenuItemClick(Sender: TObject
  );
var
  Item: TListItem;
begin
  Item:=LocationsListView.Selected;
  if Item=nil then exit;
  Clipboard.AsText:=Item.SubItems[0];
end;

procedure TSimpleWebServerWindow.CopyLocationMenuItemClick(Sender: TObject);
var
  Item: TListItem;
begin
  Item:=LocationsListView.Selected;
  if Item=nil then exit;
  Clipboard.AsText:=Item.Caption;
end;

procedure TSimpleWebServerWindow.CopyOriginMenuItemClick(Sender: TObject);
var
  Item: TListItem;
begin
  Item:=LocationsListView.Selected;
  if Item=nil then exit;
  Clipboard.AsText:=Item.SubItems[1];
end;

procedure TSimpleWebServerWindow.DeleteButtonClick(Sender: TObject);
var
  LI: TListItem;
begin
  LI:=LocationsListView.Selected;
  if LI=nil then exit;
  DeleteLocation(LI.Caption);
end;

procedure TSimpleWebServerWindow.FormDestroy(Sender: TObject);
begin
  Controller.RemoveAllHandlersOfObject(Self);
end;

procedure TSimpleWebServerWindow.LocationsListViewItemChecked(Sender: TObject;
  Item: TListItem);
var
  Loc: TSWSLocation;
  Obj: TObject;
begin
  Obj:=CaptionToControllerObj(Item.Caption);
  if Obj is TSWSLocation then
  begin
    Loc:=TSWSLocation(Obj);
    Controller.EnableLocation(Loc.Location,Item.Checked);
  end else if Obj is TSWSInstance then
  begin
    Item.Checked:=true;
  end;
end;

procedure TSimpleWebServerWindow.LocationsListViewSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  DeleteButton.Enabled:=LocationsListView.Selected<>nil;
  if Item=nil then ;
  if Selected then ;
end;

procedure TSimpleWebServerWindow.LocationsPopupMenuPopup(Sender: TObject);
var
  LI: TListItem;
begin
  LI:=LocationsListView.Selected;
  CopyLocationMenuItem.Enabled:=LI<>nil;
  CopyDirectoryPathMenuItem.Enabled:=LI<>nil;
  CopyOriginMenuItem.Enabled:=LI<>nil;
end;

end.

