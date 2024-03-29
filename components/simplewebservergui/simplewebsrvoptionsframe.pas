{
  Author: Mattias Gaertner
}
unit SimpleWebSrvOptionsFrame;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, Dialogs, StdCtrls,
  FileUtil, LazFileUtils, IDEOptEditorIntf, IDEDialogs,
  FileProcs, IDEOptionsIntf, MacroIntf,
  SimpleWebSrvStrConsts, SimpleWebSrvOptions, SimpleWebSrvController;

type

  { TSimpleWebSrvOptsFrame }

  TSimpleWebSrvOptsFrame = class(TAbstractIDEOptionsEditor)
    BindAnyCheckBox: TCheckBox;
    BrowseBrowserButton: TButton;
    BrowserKindComboBox: TComboBox;
    BrowserCmdComboBox: TComboBox;
    BrowserLabel: TLabel;
    ServerOptionsMemo: TMemo;
    ServerOptsLabel: TLabel;
    ServerAddrLabel: TLabel;
    ServerAddrComboBox: TComboBox;
    PortComboBox: TComboBox;
    PortLabel: TLabel;
    ServerExeBrowseButton: TButton;
    ServerExeComboBox: TComboBox;
    ServerExeLabel: TLabel;
    procedure BrowseBrowserButtonClick(Sender: TObject);
    procedure BrowserCmdComboBoxChange(Sender: TObject);
    procedure BrowserKindComboBoxSelect(Sender: TObject);
    procedure ServerExeBrowseButtonClick(Sender: TObject);
  private
    FOldOptions: TSimpleWebServerOptions;
    procedure SetCombobox(aComboBox: TComboBox; const aValue: string; ListItems: TStrings);
    procedure SetComboBoxText(aComboBox: TComboBox; const aValue: string);
    procedure BrowserKindComboBoxChanged;
    procedure SetRecentList(Options: TSimpleWebServerOptions; List: TSWSRecentList;
      aComboBox: TComboBox; const aValue: string);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function Check: Boolean; override;
    function GetTitle: String; override;
    procedure ReadSettings({%H-}AOptions: TAbstractIDEOptions); override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
    procedure WriteSettings({%H-}AOptions: TAbstractIDEOptions); override;
    procedure RestoreSettings({%H-}AOptions: TAbstractIDEOptions); override;
    property OldOptions: TSimpleWebServerOptions read FOldOptions;
  end;

var
  SimpleWebSrvOptsFrame: TSimpleWebSrvOptsFrame;
  SimpleWebServerOptionID: integer = 1000; // IDE frame ID, set by Register

implementation

{$R *.lfm}

procedure SetComboBoxText(Box: TComboBox; NewText: string; CaseSensitive: boolean; aCapacity: integer = 30);

  function IsSame(const A,B: string): boolean;
  begin
    if CaseSensitive then
      Result:=A=B
    else
      Result:=SameText(A,B);
  end;

var
  i: Integer;
begin
  if IsSame(Box.Text,NewText) then exit;
  i:=0;
  while (i<Box.Items.Count) and not IsSame(Box.Items[i],NewText) do inc(i);
  if i=Box.Items.Count then
  begin
    Box.Items.Insert(0,NewText);
    i:=0;
    if Box.Items.Count>aCapacity then
      Box.Items.Delete(Box.Items.Count-1);
  end else begin
    // move to top
    Box.Items.Move(i,0);
    i:=0;
  end;

  Box.ItemIndex:=i;
  Box.Text:=NewText;
end;

{ TSimpleWebSrvOptsFrame }

procedure TSimpleWebSrvOptsFrame.ServerExeBrowseButtonClick(Sender: TObject);
var
  Dlg: TOpenDialog;
  ExpExe, ErrMsg: String;
begin
  Dlg:=TOpenDialog.Create(nil);
  try
    Dlg.Title:='Select simpleserver'+GetExeExt;
    Dlg.Options:=Dlg.Options+[ofFileMustExist];
    if not Dlg.Execute then exit;
    ExpExe:=ExpandFileNameUTF8(Dlg.FileName);
    ServerExeComboBox.Text:=ExpExe;
    ErrMsg:=CheckWebServerExeQuality(ExpExe,'',true);
    if ErrMsg<>'' then
      IDEMessageDialog('Invalid simpleserver'+GetExeExt,
        ErrMsg,mtError,[mbOk]);
  finally
    Dlg.Free;
  end;
end;

procedure TSimpleWebSrvOptsFrame.BrowserKindComboBoxSelect(Sender: TObject);
begin
  BrowserKindComboBoxChanged;
end;

procedure TSimpleWebSrvOptsFrame.BrowseBrowserButtonClick(Sender: TObject);
var
  Dlg: TOpenDialog;
  ExpExe, Cmd: String;
  p: Integer;
  c: Char;
begin
  Dlg:=TOpenDialog.Create(nil);
  try
    Dlg.Title:='Select browser executable';
    Dlg.Options:=Dlg.Options+[ofFileMustExist];
    if not Dlg.Execute then exit;
    ExpExe:=ExpandFileNameUTF8(Dlg.FileName);
    if Pos(' ',ExpExe)>0 then
      ExpExe:=StrToCmdLineParam(ExpExe);

    // replace first, keep params
    Cmd:=BrowserCmdComboBox.Text;
    if Cmd='' then
      Cmd:=ExpExe
    else if Cmd[1] in [' ',#9,#10,#13] then
      Cmd:=ExpExe+Cmd
    else begin
      p:=1;
      while p<=length(Cmd) do
      begin
        c:=Cmd[p];
        case c of
        ' ',#9,#10,#13: break;
        '"','''':
          begin
            inc(p);
            while (p<=length(Cmd)) and (Cmd[p]<>c) do inc(p);
          end;
        end;
        inc(p);
      end;
      Cmd:=ExpExe+copy(Cmd,p,length(Cmd));
    end;
    BrowserCmdComboBox.Text:=Cmd;
  finally
    Dlg.Free;
  end;
end;

procedure TSimpleWebSrvOptsFrame.BrowserCmdComboBoxChange(Sender: TObject);
begin

end;

procedure TSimpleWebSrvOptsFrame.SetCombobox(aComboBox: TComboBox;
  const aValue: string; ListItems: TStrings);
begin
  aComboBox.Items.Assign(ListItems);
  SetComboBoxText(aComboBox,aValue);
end;

procedure TSimpleWebSrvOptsFrame.SetComboBoxText(aComboBox: TComboBox;
  const aValue: string);
var
  i: Integer;
begin
  for i:=0 to aComboBox.Items.Count-1 do
    if aComboBox.Items[i]=aValue then
    begin
      aComboBox.ItemIndex:=i;
      exit;
    end;
  aComboBox.Items.Insert(0,aValue);
  aComboBox.ItemIndex:=0;
end;

procedure TSimpleWebSrvOptsFrame.BrowserKindComboBoxChanged;
var
  Kind: TSWSBrowserKind;
begin
  Kind:=StrToBrowserKind(BrowserKindComboBox.Text);
  BrowserCmdComboBox.Enabled:=Kind=swsbkCustom;
end;

procedure TSimpleWebSrvOptsFrame.SetRecentList(
  Options: TSimpleWebServerOptions; List: TSWSRecentList; aComboBox: TComboBox;
  const aValue: string);
var
  i: Integer;
  sl: TStringList;
begin
  sl:=TStringList.Create;
  try
    sl.Assign(aComboBox.Items);
    i:=sl.Count-1;
    while i>=0 do
    begin
      if sl[i]=aValue then
      begin
        if i=0 then exit;
        sl.Move(i,0);
        break;
      end;
      dec(i);
    end;
    if i<0 then
    begin
      sl.Insert(0,aValue);
      if sl.Count>30 then
        sl.Delete(sl.Count-1);
    end;

    aComboBox.Items.Assign(sl);
    aComboBox.ItemIndex:=0;
  finally
    Options.RecentLists[List]:=sl;
    sl.Free;
  end;
end;

constructor TSimpleWebSrvOptsFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FOldOptions:=TSimpleWebServerOptions.Create;
end;

destructor TSimpleWebSrvOptsFrame.Destroy;
begin
  FreeAndNil(FOldOptions);
  inherited Destroy;
end;

function TSimpleWebSrvOptsFrame.Check: Boolean;
var
  NewBrowserKind: TSWSBrowserKind;
  NewBrowserCmd, Cmd: TCaption;
  Options: TSimpleWebServerOptions;
  Params: TStringList;
  ExeFilename: String;
begin
  Result:=inherited Check;
  if not Result then exit;

  Options:=SimpleWebServerController.Options;
  NewBrowserKind:=StrToBrowserKind(BrowserKindComboBox.Text);
  NewBrowserCmd:=BrowserCmdComboBox.Text;
  SetRecentList(Options,swsrlBrowserCmd,BrowserCmdComboBox,NewBrowserCmd);

  if (NewBrowserKind<>Options.BrowserKind) or (NewBrowserCmd<>Options.BrowserCmd) then
  begin
    // browser changed -> check
    case NewBrowserKind of
      swsbkDefault: ;
      swsbkFirefox: ;
      swsbkChrome: ;
      swsbkOpera: ;
      swsbkVivaldi: ;
      swsbkCustom:
        begin
          Cmd:=SimpleWebServerController.SubstituteURLMacro(NewBrowserCmd,'Test.html');
          if not IDEMacros.SubstituteMacros(Cmd) then
          begin
            if IDEMessageDialog(rsSWError, rsSWInvalidMacroSee+sLineBreak +
              rsSWToolsOptionsSimpleWebServerBrowser,mtError,[mbIgnore,mbCancel])=mrIgnore then
              exit(true)
            else
              exit(false);
          end;
          Params:=TStringList.Create;
          try
            SplitCmdLineParams(Cmd,Params);
            if Params.Count=0 then begin
              if IDEMessageDialog(rsSWError, rsSWMissingBrowserFilename+
                sLineBreak +
                rsSWToolsOptionsSimpleWebServerBrowser,mtError,[mbIgnore,mbCancel])=mrIgnore then
                exit(true)
              else
                exit(false);
            end;
            ExeFilename:=Params[0];
            if not FilenameIsAbsolute(ExeFilename) then begin
              ExeFilename:=FindDefaultExecutablePath(ExeFilename);
            end;
            if not FileExists(ExeFilename) then begin
              if IDEMessageDialog(rsSWError, Format(rsSWBrowserFileNotFound, [
                Params[0]])+sLineBreak +
                rsSWToolsOptionsSimpleWebServerBrowser,mtError,[mbIgnore,mbCancel])=mrIgnore then
                exit(true)
              else
                exit(false);
            end;
          finally
            Params.Free;
          end;
        end;
    end;
  end;
end;

function TSimpleWebSrvOptsFrame.GetTitle: String;
begin
  Result:=rsSWSTitle;
end;

procedure TSimpleWebSrvOptsFrame.ReadSettings(AOptions: TAbstractIDEOptions);

  procedure AddDefault(sl: TStringList; aValue: string);
  begin
    if sl.IndexOf(aValue)<0 then
      sl.Add(aValue);
  end;

var
  sl: TStringList;
  Options: TSimpleWebServerOptions;
  bk: TSWSBrowserKind;
  i: Integer;
  s: String;
begin
  if not (AOptions is SupportedOptionsClass) then exit;

  Options:=SimpleWebServerController.Options;
  OldOptions.Assign(Options);

  sl:=TStringList.Create;
  try
    sl.Assign(Options.RecentLists[swsrlServerExe]);
    AddDefault(sl,Options.GetDefaultServerExe);
    SetCombobox(ServerExeComboBox,Options.ServerExe,sl);

    sl.Assign(Options.RecentLists[swsrlServerAddr]);
    AddDefault(sl,SWSDefaultServerAddr);
    SetCombobox(ServerAddrComboBox,Options.ServerAddr,sl);

    BindAnyCheckBox.Checked:=Options.BindAny;

    sl.Assign(Options.RecentLists[swsrlServerPort]);
    AddDefault(sl,IntToStr(SWSDefaultServerPort));
    SetCombobox(PortComboBox,IntToStr(Options.ServerPort),sl);

    ServerOptionsMemo.Lines:=Options.ServerOpts;

    sl.Clear;
    for bk in TSWSBrowserKind do
      sl.Add(SWSBrowserKindNames[bk]);
    SetCombobox(BrowserKindComboBox,SWSBrowserKindNames[Options.BrowserKind],sl);

    BrowserCmdComboBox.Enabled:=Options.BrowserKind=swsbkCustom;
    sl.Assign(Options.RecentLists[swsrlBrowserCmd]);
    for i:=sl.Count-1 downto 0 do
      if Trim(sl[i])='' then
        sl.Delete(i);
    s:=Options.BrowserCmd;
    if Trim(s)='' then
      s:=SWSDefaultBrowserCmd;
    AddDefault(sl,s);
    SetCombobox(BrowserCmdComboBox,s,sl);
  finally
    sl.Free;
  end;
end;

procedure TSimpleWebSrvOptsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  ServerExeLabel.Caption:=rsSWSPathOfCompileserver;
  ServerAddrLabel.Caption:=rsSWSAddress;

  BindAnyCheckBox.Caption:=rsSWSBindAny+' (0.0.0.0)';

  PortLabel.Caption:=rsSWSPortMacro;

  ServerOptsLabel.Caption:=rsSWServerExtraCommandLineOptionsOnePerLine;
  ServerOptsLabel.Hint:=rsSWAddExtraCommandLineOptionsForTheCommandWhichStarts;

  BrowserLabel.Caption:=rsSWBrowserToOpenHTMLPageMacroSWSBrowser;
  BrowserLabel.Hint:= rsSWUseThisBrowserWhenOpeningTheURLOrHTMLFileOfAWebBro;
  BrowseBrowserButton.Hint:=rsSWForCustomBrowser;
end;

class function TSimpleWebSrvOptsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result:=IDEEditorGroups.GetByIndex(GroupEnvironment)^.GroupClass;
end;

procedure TSimpleWebSrvOptsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  Options: TSimpleWebServerOptions;
var
  s: string;
  i: LongInt;
begin
  if not (AOptions is SupportedOptionsClass) then exit;

  Options:=SimpleWebServerController.Options;

  s:=Trim(ServerExeComboBox.Text);
  if s<>'' then
    Options.ServerExe:=s;
  SetRecentList(Options,swsrlServerExe,ServerExeComboBox,Options.ServerExe);

  s:=Trim(ServerAddrComboBox.Text);
  if s<>'' then
    Options.ServerAddr:=s;
  SetRecentList(Options,swsrlServerAddr,ServerAddrComboBox,Options.ServerAddr);

  Options.BindAny:=BindAnyCheckBox.Checked;

  s:=Trim(PortComboBox.Text);
  i:=StrToIntDef(s,0);
  if (i>0) and (i<=65535) then
    Options.ServerPort:=i;
  SetRecentList(Options,swsrlServerPort,PortComboBox,IntToStr(Options.ServerPort));

  Options.BrowserKind:=StrToBrowserKind(BrowserKindComboBox.Text);
  Options.BrowserCmd:=BrowserCmdComboBox.Text;
  SetRecentList(Options,swsrlBrowserCmd,BrowserCmdComboBox,Options.BrowserCmd);

  if Options.Modified then
  begin
    Options.SaveSafe;
    Options.Apply;
  end;
end;

procedure TSimpleWebSrvOptsFrame.RestoreSettings(AOptions: TAbstractIDEOptions);
begin
  inherited RestoreSettings(AOptions);
end;

end.

