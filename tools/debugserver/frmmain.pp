{ Debug server main form

  Copyright (C) 2009 Michael Van Canneyt (michael@freepascal.org)

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
  Boston, MA 02110-1335, USA.
}
unit frmmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, simpleipc, dbugmsg,
  Forms, Controls, Dialogs, Menus, ActnList, ComCtrls, ExtCtrls, Clipbrd;

type

  { TMainForm }

  TMainForm = class(TForm)
    AClear: TAction;
    ACopyLines: TAction;
    AShow: TAction;
    AResume: TAction;
    ASelectAll: TAction;
    ASave: TAction;
    AHide: TAction;
    AOptions: TAction;
    APause: TAction;
    AQuit: TAction;
    ALMain: TActionList;
    ITMessages: TIdleTimer;
    ILMain: TImageList;
    ILMessages: TImageList;
    LVMessages: TListView;
    MEdit: TMenuItem;
    MenuItem1: TMenuItem;
    MISelectAll: TMenuItem;
    PMIQuit: TMenuItem;
    PMIShow: TMenuItem;
    PMIClear: TMenuItem;
    MenuItem5: TMenuItem;
    PMIPause: TMenuItem;
    MIToolbar: TMenuItem;
    MIAlwaysOntop: TMenuItem;
    MView: TMenuItem;
    MISave: TMenuItem;
    MICopy: TMenuItem;
    MIClear: TMenuItem;
    MIHide: TMenuItem;
    MIQuit: TMenuItem;
    MIOptions: TMenuItem;
    MIPause: TMenuItem;
    MFile: TMenuItem;
    MMDebugServer: TMainMenu;
    PMTray: TPopupMenu;
    SDMessages: TSaveDialog;
    TBMain: TToolBar;
    TBPause: TToolButton;
    TBCopyMessages: TToolButton;
    TBQuit: TToolButton;
    TBSave: TToolButton;
    TBHideMessagesWindow: TToolButton;
    TBClearMessages: TToolButton;
    ToolButton3: TToolButton;
    TIDebug: TTrayIcon;
    TBSaveSelected: TToolButton;
    procedure AClearExecute(Sender: TObject);
    procedure ACopyLinesExecute(Sender: TObject);
    procedure ACopyLinesUpdate(Sender: TObject);
    procedure AHideExecute(Sender: TObject);
    procedure AOptionsExecute(Sender: TObject);
    procedure APauseExecute(Sender: TObject);
    procedure AQuitExecute(Sender: TObject);
    procedure AResumeExecute(Sender: TObject);
    procedure ASaveExecute(Sender: TObject);
    procedure ASelectAllExecute(Sender: TObject);
    procedure AShowExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ITMessagesTimer(Sender: TObject);
    procedure LVMessagesDblClick(Sender: TObject);
    procedure MIAlwaysOntopClick(Sender: TObject);
    procedure MIToolbarClick(Sender: TObject);
    procedure MViewClick(Sender: TObject);
  private
    FPaused : Boolean;
    FSrv : TSimpleIPCServer;
    FShowOnStartUp,
    FShowOnmessage,
    FKeepVisible : Boolean;
    FCleanLog : Boolean;
    FAtBottom : Boolean;
    FQuitting : Boolean;
    FDiscarded : Int64;
    procedure CheckDebugMessages;
    procedure CheckMessages(Sender: TObject; Var Done : Boolean);
    procedure ClearMessages;
    procedure CopySelectedToClipBoard;
    procedure GetMessagesAsText(L: TStrings; SelectedOnly: Boolean);
    function GetShowToolbar: Boolean;
    function GetStayOnTop: Boolean;
    procedure LoadSettings;
    procedure ReadDebugMessage;
    procedure ResumeMessages;
    procedure SaveMessagesToFile(SelectedOnly: Boolean);
    procedure SaveSettings;
    procedure SelectAllMessages;
    procedure SetPauseAction(AAction: TAction);
    procedure SetShowToolBar(const AValue: Boolean);
    procedure SetStayOnTop(const AValue: Boolean);
    procedure ShowCurrentMessage;
    procedure ShowDebugmessage(const Msg: TDebugmessage);
    procedure ShowMessageWindow;
    procedure ShowOptions;
    procedure StartServer;
    procedure StopServer;
  public
    Property StayOnTop : Boolean Read GetStayOnTop Write SetStayOnTop;
    Property ShowToolbar : Boolean Read GetShowToolbar Write SetShowToolBar;
  end; 

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses frmoptions;

{ TMainForm }

{ ---------------------------------------------------------------------
  Event handlers
  ---------------------------------------------------------------------}
procedure TMainForm.AShowExecute(Sender: TObject);
begin
  ShowMessageWindow;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  If FQuitting Then
    CloseAction:=caFree
  else
    CloseAction:=caHide
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  LoadSettings;
  If Not FShowOnStartup Then
    Hide;
  StartServer;
end;


procedure TMainForm.AQuitExecute(Sender: TObject);
begin
  FQuitting:=True;
  Close;
end;

procedure TMainForm.AResumeExecute(Sender: TObject);
begin
  ResumeMessages;
  SetPauseAction(APause);
end;

procedure TMainForm.ASaveExecute(Sender: TObject);

begin
  SaveMessagesToFile(False);
end;


procedure TMainForm.ASelectAllExecute(Sender: TObject);
begin
  SelectAllMessages;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  StopServer;
end;

procedure TMainForm.ITMessagesTimer(Sender: TObject);
begin
  CheckDebugMessages;
end;

procedure TMainForm.LVMessagesDblClick(Sender: TObject);
begin
  ShowCurrentMessage;
end;

procedure TMainForm.MIAlwaysOntopClick(Sender: TObject);
begin
  StayOnTop:=(Sender as TMenuItem).Checked;
end;

procedure TMainForm.MIToolbarClick(Sender: TObject);
begin
  ShowToolBar:=(Sender as TMenuItem).Checked;
end;

procedure TMainForm.MViewClick(Sender: TObject);
begin
  MIAlwaysOnTop.Checked:=STayOnTop;
  MIToolbar.Checked:=ShowToolbar;
end;

procedure TMainForm.CheckMessages(Sender: TObject; Var Done : Boolean);

begin
  CheckDebugMessages;
end;

procedure TMainForm.AClearExecute(Sender: TObject);
begin
  ClearMessages;
end;

procedure TMainForm.ACopyLinesExecute(Sender: TObject);
begin
  CopySelectedToClipBoard;
end;
procedure TMainForm.ACopyLinesUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=(LVMessages.SelCount>0);
end;

procedure TMainForm.AHideExecute(Sender: TObject);
begin
  Hide;
end;

procedure TMainForm.AOptionsExecute(Sender: TObject);
begin
  ShowOptions;
end;

procedure TMainForm.APauseExecute(Sender: TObject);

begin
  FPaused:=True;
  SetPauseAction(AResume);
end;

{ ---------------------------------------------------------------------
  Methods that do the actual work
  ---------------------------------------------------------------------}

procedure TMainForm.StartServer;

begin
  FSrv:=TSimpleIPCServer.Create(Nil);
  FSrv.ServerID:=DebugServerID;
  FSrv.Global:=True;
  FSrv.Active:=True;
  FSrv.StartServer;
  Application.OnIdle:=@CheckMessages;
  ITMessages.Enabled:=True;
end;

procedure TMainForm.StopServer;

begin
  Application.OnIdle:=Nil;
  ITMessages.Enabled:=False;
  FreeAndNil(FSrv);
end;

procedure TMainForm.SaveMessagesToFile(SelectedOnly : Boolean);

Var
  L : TStrings;
  FN : String;

begin
  With SDMessages do
    If Execute then
      FN:=FileName
    else
      Exit;
  L:=TstringList.Create;
  try
    Self.GetMessagesAsText(L,SelectedOnly);
    L.SaveToFile(FN);
  finally
    L.Free;
  end;
end;

procedure TMainForm.SetPauseAction(AAction : TAction);
begin
  MIPause.Action:=AAction;
  PMIPause.Action:=AAction;
  TBPause.Action:=AAction;
end;

procedure TMainForm.SelectAllMessages;

Var
  I : Integer;

begin
  LVmessages.Items.BeginUpdate;
  try
    For I:=0 to LVmessages.Items.Count-1 do
      LVmessages.Items[I].Selected:=True;
  finally
    LVmessages.Items.EndUpdate;
  end;
end;

procedure TMainForm.ShowCurrentMessage;

begin
  If LVMessages.Selected<>Nil then
    ShowMessage(LVMessages.Selected.SubItems[1]);
end;

function TMainForm.GetShowToolbar: Boolean;
begin
  Result:=TBMain.Visible;
end;

function TMainForm.GetStayOnTop: Boolean;
begin
  Result:=FormStyle=fsStayOnTop;
end;

procedure TMainForm.CheckDebugMessages;

begin
  While FSrv.PeekMessage(1,True) do
    ReadDebugMessage;
end;

procedure TMainForm.ReadDebugMessage;

Var
  Msg : TDebugMessage;

begin
  FSrv.MsgData.Seek(0,soFrombeginning);
  ReadDebugMessageFromStream(FSrv.MsgData,MSg);
  If not FPaused then
    ShowDebugMessage(Msg)
  else
    Inc(FDiscarded);
end;

procedure TMainForm.ShowDebugmessage(Const Msg : TDebugmessage);

Var
  LI : TListItem;

begin
  if (Msg.MsgType = lctIdentify) and FCleanLog then
    ClearMessages;

  LVmessages.Items.BeginUpdate;
  try
    if FAtBottom then
      LI:=LVmessages.Items.Add
    else
      LI:=LVmessages.Items.Insert(0);
    If (Msg.MsgType=lctStop) then
      LI.ImageIndex:=4
    else
      LI.ImageIndex:=Msg.MsgType;
    LI.Caption:=DebugMessageName(Msg.MsgType);
    LI.Subitems.Add(TimeToStr(Msg.MsgTimeStamp));
    LI.SubItems.Add(Msg.Msg);
  finally
    LVmessages.Items.EndUpdate;
    if FKeepVisible then
      LI.MakeVisible(False);
  end;
  If FShowOnMessage then
    ShowMessageWindow;
end;

procedure TMainForm.ShowMessageWindow;

begin
  If Not Visible then
    Show;
  If (WindowState=wsMinimized) then
    WindowState:=wsNormal;
end;

procedure TMainForm.ClearMessages;

begin
  LVMessages.Items.Clear;
end;


procedure TMainForm.CopySelectedToClipBoard;

Var
  L : TStringList;

begin
  L:=TStringList.Create;
  try
    GetMessagesAsText(L,True);
    ClipBoard.AsText:=L.Text;
  finally
    L.Free;
  end;
end;

procedure TMainForm.GetMessagesAsText(L : TStrings; SelectedOnly : Boolean);

Var
  I : Integer;
  S : String;
  LI : TListItem;

begin
  For I:=0 to LVMessages.Items.Count-1 do
    begin
    LI:=LVMessages.Items[i];
    If (Not SelectedOnly) or LI.Selected then
      begin
      S:=LI.Caption;
      S:=S+': ['+Li.SubItems[0]+'] ';
      S:=S+Li.SubItems[1];
      L.Add(S);
      end;
    end;
end;

procedure TMainForm.ShowOptions;

begin
  With TOptionsForm.Create(Self) do
    try
      ShowOnStartUp:=FShowOnStartUp;
      ShowOnMessage:=FShowOnmessage;
      NewMessageAtBottom:=FAtBottom;
      NewMessageVisible:=FKeepVisible;
      CleanLogOnNewProcess := FCleanLog;
      If (ShowModal=mrOk) then
        begin
        FShowOnStartUp:=ShowOnStartUp;
        FShowOnmessage:=ShowOnMessage;
        FAtBottom:=NewMessageAtBottom;
        FKeepVisible:=NewMessageVisible;
        SaveSettings;
        end;
    finally
       Free;
    end;
end;


procedure TMainForm.ResumeMessages;

Var
  Msg : TDebugmessage;

begin
  FPaused:=False;
  Msg.MsgTimeStamp:=Now;
  Msg.MsgType:=lctInformation;
  Msg.Msg:=Format('Discarded %d messages while paused.',[FDiscarded]);
  FDiscarded:=0;
  ShowDebugMessage(Msg);
end;

procedure TMainForm.SetShowToolBar(const AValue: Boolean);
begin
  TBMain.Visible:=AValue;
end;

procedure TMainForm.SetStayOnTop(const AValue: Boolean);
begin
  if AValue then
    FormStyle:=fsStayOnTop
  else
    FormStyle:=fsNormal;
end;

Const
  SSettings        = 'Settings';
  KeyShowOnStartup = 'ShowOnStartup';
  KeyShowOnMessage = 'ShowOnMessage';
  KeyAtBottom      = 'NewAtBottom';
  KeyNewVisible    = 'NewVisible';
  KeyCleanLog      = 'CleanLog';
  KeyStayOnTop     = 'StayOnTop';
  KeyToolBar       = 'ShowToolBar';

procedure TMainForm.LoadSettings;

Var
  Ini : TMemIniFile;

begin
  Ini:=TMeminiFile.Create(GetAppConfigFile(False));
  With Ini do
    try
      FShowOnStartUp:=ReadBool(SSettings,KeyShowOnStartup,True);
      FShowOnMessage:=ReadBool(SSettings,KeyShowOnMessage,True);
      FAtBottom:=ReadBool(SSettings,KeyAtBottom,False);
      FKeepVisible:=ReadBool(SSettings,KeyNewVisible,True);
      FCleanLog:=ReadBool(SSettings,KeyCleanLog,False);
      StayOnTop:=ReadBool(SSettings,KeyStayOnTop,False);
      ShowToolBar:=ReadBool(SSettings,KeyToolBar,True);
    finally
      Free;
    end;
end;

procedure TMainForm.SaveSettings;

Var
  Ini : TMemIniFile;

begin
  if not(DirectoryExists(GetAppConfigDir(False))) Then
    if not(CreateDir (GetAppConfigDir(False))) Then
      ShowMessage('Cannot create config dir');

  Ini:=TMeminiFile.Create(GetAppConfigFile(False));
  With Ini do
    try
      WriteBool(SSettings,KeyShowOnStartup,FShowOnStartUp);
      WriteBool(SSettings,KeyShowOnMessage,FShowOnMessage);
      WriteBool(SSettings,KeyAtBottom,FAtBottom);
      WriteBool(SSettings,KeyNewVisible,FKeepVisible);
      WriteBool(SSettings,KeyCleanLog,FCleanLog);
      WriteBool(SSettings,KeyStayOnTop,StayOnTop);
      WriteBool(SSettings,KeyToolBar,ShowToolBar);
    finally
      Free;
    end;
end;

Function MyGetAppName : String;

begin
  Result:='FPCDebugSrv';
end;

initialization
  OnGetApplicationName:=@MyGetAppName;
end.

