unit SimpleWebSrvAdd;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  ComCtrls, ExtCtrls, StdCtrls, LazIDEIntf, IDEDialogs, MacroIntf, LazUTF8,
  LazFileUtils, FileUtil, LazLoggerBase,
  SimpleWebSrvStrConsts, SimpleWebSrvController, SimpleWebSrvOptions,
  SimpleWebSrvUtils;

type

  { TSimpleWebSrvAddDialog }

  TSimpleWebSrvAddDialog = class(TForm)
    LocationBrowserButton: TButton;
    LocationComboBox: TComboBox;
    LocationDirComboBox: TComboBox;
    SrvBrowserButton: TButton;
    SrvExeBrowseButton: TButton;
    SrvExeComboBox: TComboBox;
    LocationDirLabel: TLabel;
    SrvPortComboBox: TComboBox;
    SrvWorkDirComboBox: TComboBox;
    SrvWorkDirLabel: TLabel;
    SrvParamsComboBox: TComboBox;
    SrvParamsLabel: TLabel;
    LocationLabel: TLabel;
    SrvPortLabel: TLabel;
    LocationURLLabel: TLabel;
    SrvExeLabel: TLabel;
    LocButtonPanel: TButtonPanel;
    SrvButtonPanel: TButtonPanel;
    OptionsPageControl: TPageControl;
    LocationTabSheet: TTabSheet;
    ServerTabSheet: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure LocationBrowserButtonClick(Sender: TObject);
    procedure LocationComboBoxEditingDone(Sender: TObject);
  private
    FController: TSimpleWebServerController;
    procedure LocButtonPanelOKButtonClick(Sender: TObject);
    procedure SetController(const AValue: TSimpleWebServerController);
    procedure SrvButtonPanelOKButtonClick(Sender: TObject);
    procedure UpdateLocationURL;
    function CheckLocationName(var aLoc: string; Interactive: boolean): boolean;
    function CheckLocationPath(var aPath: string; Interactive: boolean): boolean;
    function CheckServerPort(var aPort: string; ResolvePort0, Interactive: boolean): boolean;
    function CheckServerExe(var aExe: string; Interactive: boolean): boolean;
    function GetDefaultLocationName: string;
    function GetDefaultLocationDir: string;
    function GetDefaultServerPort: string;
    function GetDefaultServerExe: string;
    function GetDefaultSrvParams: string;
  public
    property Controller: TSimpleWebServerController read FController write SetController;
  end;

function ShowAddSWSLocationDialog(Controller: TSimpleWebServerController): TModalResult;

implementation

function ShowAddSWSLocationDialog(Controller: TSimpleWebServerController
  ): TModalResult;
var
  Dlg: TSimpleWebSrvAddDialog;
begin
  Dlg:=TSimpleWebSrvAddDialog.Create(nil);
  try
    Dlg.Controller:=Controller;
    Result:=Dlg.ShowModal;
  finally
    Dlg.Free;
  end;
end;

{$R *.lfm}

{ TSimpleWebSrvAddDialog }

procedure TSimpleWebSrvAddDialog.FormCreate(Sender: TObject);
begin
  // localize
  Caption:=rsSWAddSimpleWebServerLocation;

  // add location page
  LocationLabel.Caption:=rsSWLocation;
  LocationComboBox.TextHint:='location';
  LocationComboBox.Hint:=rsSWAnArbitraryNameForTheURLSubfolder;

  LocationDirLabel.Caption:=rsSWLocalDirectory;
  LocationDirComboBox.Hint:=
    rsSWWorkingDirectoryOnDiskUsuallyWhereTheServerFetches;

  LocButtonPanel.OKButton.Caption:=rsSWAddLocation;

  // add server page
  SrvPortLabel.Caption:=rsSWSPort;
  SrvPortComboBox.Hint:=rsSWTCPPort102465535YouCanUseMacroPortForBelowParams;
  SrvExeLabel.Caption:=rsSWExecutable;
  SrvExeComboBox.Hint:='';
  SrvWorkDirLabel.Caption:=rsSWWorkingDirectory;
  SrvWorkDirComboBox.Hint:=LocationDirComboBox.Hint;
  SrvParamsLabel.Caption:=rsSWParametersPleaseUseMacroPort;
  SrvParamsComboBox.Hint:=rsSWCommandLineParameters;

  SrvButtonPanel.OKButton.Caption:=rsSWAddCustomServer;

  // hook
  LocButtonPanel.OKButton.OnClick:=@LocButtonPanelOKButtonClick;
  SrvButtonPanel.OKButton.OnClick:=@SrvButtonPanelOKButtonClick;

  OptionsPageControl.ActivePage:=LocationTabSheet;
end;

procedure TSimpleWebSrvAddDialog.LocationBrowserButtonClick(Sender: TObject);
var
  Dlg: TSelectDirectoryDialog;
  s: String;
begin
  Dlg:=TSelectDirectoryDialog.Create(nil);
  try
    Dlg.Title:=rsSWSelectDirectory;
    s:='$Project(OutputDir)';
    IDEMacros.SubstituteMacros(s);
    Dlg.InitialDir:=s;
    Dlg.Options:=Dlg.Options+[ofPathMustExist];
    if not Dlg.Execute then exit;
    LocationDirComboBox.Text:=Dlg.FileName;
  finally
    Dlg.Free;
  end;
end;

procedure TSimpleWebSrvAddDialog.LocationComboBoxEditingDone(Sender: TObject);
begin
  UpdateLocationURL;
end;

procedure TSimpleWebSrvAddDialog.SetController(
  const AValue: TSimpleWebServerController);
var
  sl: TStringListUTF8Fast;
  Opts: TSimpleWebServerOptions;

  procedure FillCombobox(Box: TComboBox; RecentList: TSWSRecentList;
    DefaultValue: string; UseDefault: boolean = false);
  begin
    sl.Assign(Opts.RecentLists[RecentList]);
    if (DefaultValue<>'') and (sl.IndexOf(DefaultValue)<0) then
      sl.Add(DefaultValue);
    Box.Items.Assign(sl);
    if Box.Items.Count>0 then
    begin
      if UseDefault then
        Box.Text:=DefaultValue
      else
        Box.ItemIndex:=0;
    end else
      Box.Text:='';
  end;

begin
  if FController=AValue then Exit;
  FController:=AValue;

  if FController<>nil then
  begin
    Opts:=Controller.Options;

    sl:=TStringListUTF8Fast.Create;
    try
      FillCombobox(LocationComboBox,swsrlUserLocation,GetDefaultLocationName,true);
      FillCombobox(LocationDirComboBox,swsrlUserPath,GetDefaultLocationDir);

      FillCombobox(SrvPortComboBox,swsrlServerPort,GetDefaultServerPort,true);
      FillCombobox(SrvExeComboBox,swsrlServerExe,GetDefaultServerExe);
      FillCombobox(SrvWorkDirComboBox,swsrlUserPath,GetDefaultLocationDir);
      FillCombobox(SrvParamsComboBox,swsrlUserParams,GetDefaultSrvParams);

      UpdateLocationURL;
    finally
      sl.Free;
    end;
  end;
end;

procedure TSimpleWebSrvAddDialog.SrvButtonPanelOKButtonClick(Sender: TObject);
var
  aPort, aExe, aWorkDir, Params: string;
  Port: word;
  ParamsList: TStringListUTF8Fast;
  Server: TSWSInstance;
  Opts: TSimpleWebServerOptions;
begin
  aPort:=SrvPortComboBox.Text;
  if not CheckServerPort(aPort,true,true) then exit;
  Port:=StrToInt(aPort);

  aExe:=SrvExeComboBox.Text;
  if not CheckServerExe(aExe,true) then exit;

  aWorkDir:=SrvWorkDirComboBox.Text;
  if not CheckLocationPath(aWorkDir,true) then exit;

  Params:=SrvParamsComboBox.Text;
  Params:=Controller.SubstitutePortMacro(Params,aPort);
  IDEMacros.SubstituteMacros(Params);

  ParamsList:=TStringListUTF8Fast.Create;
  SplitCmdLineParams(Params,ParamsList);
  Server:=Controller.AddServer(Port,aExe,ParamsList,aWorkDir,rsSWSUserOrigin,false,true);
  if Server=nil then exit;

  // store
  Opts:=Controller.Options;
  Opts.AddRecent(swsrlServerPort,SrvPortComboBox.Text);
  Opts.AddRecent(swsrlServerExe,SrvExeComboBox.Text);
  Opts.AddRecent(swsrlUserPath,SrvWorkDirComboBox.Text);
  Opts.AddRecent(swsrlUserParams,SrvParamsComboBox.Text);

  Opts.SaveSafe;

  ModalResult:=mrOk;
end;

procedure TSimpleWebSrvAddDialog.LocButtonPanelOKButtonClick(Sender: TObject);
var
  aLoc, aPath: string;
  Loc: TSWSLocation;
begin
  // check Location
  aLoc:=LocationComboBox.Text;
  if not CheckLocationName(aLoc,true) then exit;

  // check local directory
  aPath:=LocationDirComboBox.Text;
  if not CheckLocationPath(aPath,true) then exit;

  // add
  Loc:=Controller.AddLocation(aLoc,aPath,rsSWSUserOrigin,true);
  if Loc=nil then
  begin
    IDEMessageDialog(rsSWError, 'Unable to add location [20220129122529]',
      mtError, [mbOK]);
    exit;
  end;
  if Loc.ErrorDesc<>'' then
  begin
    IDEMessageDialog(rsSWError, rsSWUnableToAddLocation+sLineBreak+Loc.
      ErrorDesc,
      mtError, [mbOK]);
    exit;
  end;

  // store
  Controller.Options.AddRecent(swsrlUserLocation,LocationComboBox.Text);
  Controller.Options.AddRecent(swsrlUserPath,LocationDirComboBox.Text);
  Controller.Options.SaveSafe;

  ModalResult:=mrOk;
end;

procedure TSimpleWebSrvAddDialog.UpdateLocationURL;
var
  aLoc: TCaption;
begin
  if Controller=nil then
    LocationURLLabel.Caption:=rsSWMissingController
  else begin
    aLoc:=LocationComboBox.Text;
    CheckLocationName(aLoc,false);
    LocationURLLabel.Caption:='http://'+Controller.MainSrvAddr+':'+IntToStr(Controller.MainSrvPort)+'/'+aLoc+'/';
  end;
end;

function TSimpleWebSrvAddDialog.CheckLocationName(var aLoc: string;
  Interactive: boolean): boolean;
var
  i: Integer;
  s: String;
begin
  Result:=false;

  aLoc:=UTF8Trim(aLoc);
  if aLoc='' then
  begin
    if Interactive then
      IDEMessageDialog(rsSWError, rsSWMissingLocation, mtError, [mbOK]);
    exit;
  end;

  for i:=1 to length(aLoc) do
  begin
    if aLoc[i] in [#0..#31] then
      s:='#'+IntToStr(Ord(aLoc[i]))
    else if aLoc[i] in [':','/','\','&'] then
      s:=aLoc[i]
    else
      continue;
    if Interactive then
      IDEMessageDialog(rsSWError, 'Invalid char '+s+' in Location', mtError, [
        mbOK]);
    exit;
  end;

  if (aLoc=SWSDefaultAPIPath)
     or (Controller.FindLocation(aLoc)<>nil) then
  begin
    if Interactive then
      IDEMessageDialog(rsSWError, rsSWLocationAlreadyUsed, mtError, [mbOK]);
    exit;
  end;

  Result:=true;
end;

function TSimpleWebSrvAddDialog.CheckLocationPath(var aPath: string;
  Interactive: boolean): boolean;
begin
  Result:=false;
  aPath:=UTF8Trim(aPath);
  IDEMacros.SubstituteMacros(aPath);
  aPath:=ChompPathDelim(aPath);
  if aPath='' then
  begin
    if Interactive then
      IDEMessageDialog(rsSWError, rsSWMissingLocalDirectory, mtError, [mbOK]);
    exit;
  end;
  if not DirectoryExists(aPath) then
  begin
    if Interactive then
      IDEMessageDialog(rsSWError, rsSWLocalDirectoryNotFound+sLineBreak+
        aPath, mtError, [mbOK]);
    exit;
  end;
  Result:=true;
end;

function TSimpleWebSrvAddDialog.CheckServerPort(var aPort: string;
  ResolvePort0, Interactive: boolean): boolean;
var
  p: LongInt;
begin
  Result:=false;
  aPort:=UTF8Trim(aPort);
  IDEMacros.SubstituteMacros(aPort);
  if length(aPort)>5 then
  begin
    if Interactive then
      IDEMessageDialog(rsSWError, rsSWInvalidPort, mtError, [mbOK]);
    exit;
  end;
  p:=StrToIntDef(aPort,-1);
  if (p<0) or (p>65535) then
  begin
    if Interactive then
      IDEMessageDialog(rsSWError, rsSWInvalidPort, mtError, [mbOK]);
    exit;
  end;

  if (p=0) and ResolvePort0 then
    p:=Controller.FindFreePort(Interactive,false);

  aPort:=IntToStr(p);

  if (p>0) and (Controller.FindServerWithPort(p)<>nil) then
  begin
    if Interactive then
      IDEMessageDialog(rsSWError, rsSWPortAlreadyUsed, mtError, [mbOK]);
    exit;
  end;

  Result:=true;
end;

function TSimpleWebSrvAddDialog.CheckServerExe(var aExe: string;
  Interactive: boolean): boolean;
var
  BaseDir: String;
begin
  Result:=false;
  aExe:=UTF8Trim(aExe);
  IDEMacros.SubstituteMacros(aExe);
  if ExtractFilename(aExe)='' then
  begin
    if Interactive then
      IDEMessageDialog(rsSWError, rsSWMissingServerExecutable, mtError, [mbOK]);
    exit;
  end;

  BaseDir:='$(LazarusDir)';
  IDEMacros.SubstituteMacros(BaseDir);
  if ExtractFilePath(aExe)='' then
  begin
    aExe:=FindDefaultExecutablePath(aExe,BaseDir);
    if aExe='' then
    begin
      if Interactive then
        IDEMessageDialog(rsSWError, rsSWServerExecutableNotFoundInPATH2,
          mtError,
          [mbOK]);
      exit;
    end;
  end else begin
    aExe:=ExpandFileNameUTF8(aExe,BaseDir);
  end;

  if not FileExists(aExe) then
  begin
    if Interactive then
      IDEMessageDialog(rsSWError, rsSWServerExecutableNotFound, mtError, [mbOK]
        );
    exit;
  end;
  if not FileIsExecutable(aExe) then
  begin
    if Interactive then
      IDEMessageDialog(rsSWError, rsSWServerExeIsNotExecutable2, mtError, [mbOK]
        );
    exit;
  end;

  Result:=true;
end;

function TSimpleWebSrvAddDialog.GetDefaultLocationName: string;
var
  i: Integer;
  sl: TStrings;
  Prefix: String;
begin
  sl:=Controller.Options.RecentLists[swsrlUserLocation];
  if sl.Count>0 then
  begin
    Result:=sl[0];
    if CheckLocationName(Result,false) then exit;
    Prefix:=Result;
  end else
    Prefix:='loc';
  i:=1;
  repeat
    Result:=Prefix+IntToStr(i);
    if CheckLocationName(Result,false) then exit;
    inc(i);
  until false;
end;

function TSimpleWebSrvAddDialog.GetDefaultLocationDir: string;
begin
  Result:='$Project(OutputDir)';
  IDEMacros.SubstituteMacros(Result);
  if CheckLocationPath(Result,false) then exit;

  Result:='$(EdFile)';
  IDEMacros.SubstituteMacros(Result);
  Result:=ExtractFilePath(Result);
  if CheckLocationPath(Result,false) then exit;

  Result:='';
end;

function TSimpleWebSrvAddDialog.GetDefaultServerPort: string;
var
  aPort: word;
  i: Integer;
begin
  aPort:=Controller.MainSrvPort;
  for i:=1 to 65535 do
  begin
    aPort:=GetNextIPPort(aPort);
    if Controller.FindServerWithPort(aPort)=nil then
      exit(IntToStr(aPort));
  end;
  Result:=IntToStr(GetNextIPPort(Controller.MainSrvPort));
end;

function TSimpleWebSrvAddDialog.GetDefaultServerExe: string;
begin
  Result:=Controller.MainSrvExe;
end;

function TSimpleWebSrvAddDialog.GetDefaultSrvParams: string;
begin
  Result:='-s -n -I 127.0.0.1 --port=$(port)';
end;

end.

