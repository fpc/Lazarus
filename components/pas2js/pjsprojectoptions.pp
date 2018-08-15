unit pjsprojectoptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  StdCtrls, Spin,
  // LazUtils
  LazLoggerBase,
  // IdeIntf
  LazIDEIntf, ProjectIntf, CompOptsIntf, IDEOptionsIntf, IDEOptEditorIntf;

type

  { TPas2JSProjectOptionsFrame }

  TPas2JSProjectOptionsFrame = class(TAbstractIDEOptionsEditor)
    BResetRunCommand: TButton;
    BResetCompileCommand: TButton;
    CBRunOnReady: TCheckBox;
    CBServerURL: TComboBox;
    CBUseBrowserConsole: TCheckBox;
    CBUseHTTPServer: TCheckBox;
    CBWebProject: TCheckBox;
    CBHTMLFile: TComboBox;
    CBMaintainHTMLFile: TCheckBox;
    LCBProjectHTMLFile: TLabel;
    RBStartServerAt: TRadioButton;
    RBUseURL: TRadioButton;
    SEPort: TSpinEdit;
    procedure BResetCompileCommandClick(Sender: TObject);
    procedure BResetRunCommandClick(Sender: TObject);
    procedure CBMaintainHTMLFileChange(Sender: TObject);
    procedure CBUseHTTPServerChange(Sender: TObject);
    procedure CBWebProjectChange(Sender: TObject);
    procedure RBStartServerAtChange(Sender: TObject);
    procedure RBUseURLChange(Sender: TObject);
  private
    FDialog: TAbstractOptionsEditorDialog;
    procedure CheckAllControls(aEnabled: Boolean);
    procedure CheckHTMLOptions(aEnabled: Boolean);
    procedure CheckServerOptions(aEnabled: Boolean);
    function FillFilesCombo(PRJ: TLazProject): Integer;
    procedure ToggleCB(CB: TCheckBox; aEnabled: Boolean);

  public
    function GetTitle: string; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

Procedure SetDefaultWebRunParams(RunParams : TAbstractRunParamsOptionsMode);
Procedure SetDefaultNodeRunParams(RunParams : TAbstractRunParamsOptionsMode);
Procedure SetDefaultWebCompileOptions(CompOpts: TLazCompilerOptions);
Procedure SetDefaultNodeJSCompileOptions(CompOpts: TLazCompilerOptions);

implementation

uses pjsdsgnoptions, pjscontroller;

Procedure ResetRunParams(RunParams : TAbstractRunParamsOptionsMode);

begin
  RunParams.HostApplicationFilename:='';
  RunParams.CmdLineParams:='';
  RunParams.UseLaunchingApplication:=True;
end;

Procedure SetDefaultWebRunParams(RunParams : TAbstractRunParamsOptionsMode);

begin
  ResetRunParams(RunParams);
  RunParams.LaunchingApplicationPathPlusParams:='$(Pas2JSBrowser) $(Pas2JSProjectURL)';
end;

Procedure SetDefaultNodeRunParams(RunParams : TAbstractRunParamsOptionsMode);

begin
  ResetRunParams(RunParams);
  RunParams.LaunchingApplicationPathPlusParams:='$(Pas2JSNodeJS) "$MakeDir($(ProjPath))$NameOnly($(ProjFile)).js"';
end;

Procedure SetPasJSCompileOptions(CompOpts: TLazCompilerOptions;
  TargetOS, CustomOpts : String);

Var
  Compiler : String;

begin
  DebugLn(['SetPasJSCompileOptions START']);
  CompOpts.UnitOutputDirectory:='js';

  CompOpts.TargetFileExt:='.js';
  CompOpts.TargetOS:=TargetOS;

  CompOpts.AllowLabel:=false;
  CompOpts.UseAnsiStrings:=false;

  CompOpts.IOChecks:=false;
  CompOpts.StackChecks:=false;
  CompOpts.SmartLinkUnit:=false;

  CompOpts.GenerateDebugInfo:=false;
  CompOpts.DebugInfoType:=dsAuto;
  CompOpts.UseLineInfoUnit:=false;
  CompOpts.UseHeaptrc:=false;
  CompOpts.Win32GraphicApp:=false;

  CompOpts.WriteFPCLogo:=true;
  CompOpts.CustomOptions:=CustomOpts;

  Compiler:='$(pas2js)';
  CompOpts.CompilerPath:=Compiler;
  debugln(['Hint: (lazarus) [pjsprojectoptions.SetPasJSCompileOptions] Compiler=',CompOpts.CompilerPath,' TargetOS=',CompOpts.TargetOS,' Custom="',CompOpts.CustomOptions,'"']);
end;

Procedure SetDefaultWebCompileOptions(CompOpts: TLazCompilerOptions);

begin
  SetPasJSCompileOptions(CompOpts,'browser','-Jeutf-8 -Jirtl.js -Jc -Jminclude');
end;

Procedure SetDefaultNodeJSCompileOptions(CompOpts: TLazCompilerOptions);

begin
  SetPasJSCompileOptions(CompOpts,'nodejs','-Jeutf-8 -Jminclude');
end;

{$R *.lfm}

{ TPas2JSProjectOptionsFrame }

function TPas2JSProjectOptionsFrame.GetTitle: string;
begin
  Result:='Web Project (pas2js)'
end;

procedure TPas2JSProjectOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  // Do nothing
  FDialog:=ADialog;
end;

procedure TPas2JSProjectOptionsFrame.CBWebProjectChange(Sender: TObject);
begin
  CheckAllControls(CBWebProject.Checked);
end;

procedure TPas2JSProjectOptionsFrame.RBStartServerAtChange(Sender: TObject);
begin
  SEPort.Enabled:=RBStartServerAt.Enabled and RBStartServerAt.Checked;;
end;

procedure TPas2JSProjectOptionsFrame.RBUseURLChange(Sender: TObject);
begin
  CBServerURL.Enabled:=RBUseURL.Enabled and RBUseURL.Checked;
end;

procedure TPas2JSProjectOptionsFrame.CBMaintainHTMLFileChange(Sender: TObject);
begin
  CheckHTMLOptions(CBMaintainHTMLFile.Checked);
end;

procedure TPas2JSProjectOptionsFrame.BResetRunCommandClick(Sender: TObject);

Var
  Prj : TLazProject;

begin
  PRJ:=LazarusIDE.ActiveProject;
  SetDefaultWebRunParams(Prj.RunParameters.GetOrCreate('Default'));
end;

procedure TPas2JSProjectOptionsFrame.BResetCompileCommandClick(Sender: TObject);

Var
  Prj : TLazProject;

begin
  PRJ:=LazarusIDE.ActiveProject;
  SetDefaultWebCompileOptions(PRJ.LazBuildModes.BuildModes[0].LazCompilerOptions);
  SetDefaultWebCompileOptions(PRJ.LazCompilerOptions);
end;

procedure TPas2JSProjectOptionsFrame.CBUseHTTPServerChange(Sender: TObject);
begin
  CheckServerOptions(CBUseHTTPServer.Checked);
end;

procedure TPas2JSProjectOptionsFrame.ToggleCB(CB : TCheckBox; aEnabled: Boolean);

begin
  With CB do
    begin
    Enabled:=aEnabled;
    if not AEnabled then
      Checked:=False;
    end;
end;

procedure TPas2JSProjectOptionsFrame.CheckHTMLOptions(aEnabled : Boolean);

begin
  ToggleCB(CBUseBrowserConsole,aEnabled);
  ToggleCB(CBRunOnReady,aEnabled);
end;

procedure TPas2JSProjectOptionsFrame.CheckServerOptions(aEnabled : Boolean);

begin
  RBStartServerAt.Enabled:=aEnabled;
  RBUseURL.Enabled:=aEnabled;
  SEPort.Enabled:=aEnabled and RBStartServerAt.Checked;
  CBServerURL.Enabled:=aEnabled and RBUseURL.Checked;
end;

procedure TPas2JSProjectOptionsFrame.CheckAllControls(aEnabled : Boolean);

begin
  BResetRunCommand.enabled:=aEnabled;
  BResetCompileCommand.Enabled:=aEnabled;
  CBHTMLFile.Enabled:=aEnabled;
  ToggleCB(CBMaintainHTMLFile,aEnabled);
  CheckHTMLOptions(CBMaintainHTMLFile.Checked);
  ToggleCB(CBUseHTTPServer,aEnabled);
  CheckServerOptions(CBUseHTTPServer.Checked)
end;


// Fill combo with HTM(L) files.
// Return index of file that has IsProjectHTMLFile set.
Function TPas2JSProjectOptionsFrame.FillFilesCombo(PRJ : TLazProject) : Integer;

Var
  I : integer;
  HPF,PF : TLazProjectFile;
  Ext : String;
  L : TStringList;

begin
  Result:=-1;
  HPF:=Nil;
  L:=TstringList.Create;
  try
    For I:=0 to PRJ.FileCount-1 do
      begin
      PF:=PRJ.Files[i];
      Ext:=LowerCase(ExtractFileExt(PF.Filename));
      if Pos(Ext,'.html')>0 then
        begin
        L.AddObject(PF.FileName,PF);
        If PF.CustomData[PJSIsProjectHTMLFile]='1' then
          HPF:=PF;
        end;
      end;
      L.Sort;
      CBHTMLFile.Items:=L;;
  finally
    L.Free;
  end;
  if (HPF<>Nil) then
    Result:=CBHTMLFile.Items.IndexOfObject(HPF);
end;

procedure TPas2JSProjectOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);

Var
  Prj : TLazProject;
  HFN : String;
  HTMLIdx : Integer;
  Port : Word;
  URL : String;

begin
  PRJ:=LazarusIDE.ActiveProject;
  HTMLIdx:=FillFilesCombo(PRJ);
  CBWebProject.Checked:=PRJ.CustomData[PJSProjectWebBrowser]='1';
  if HTMLIdx=-1 then
    begin
    HFN:=PRJ.CustomData[PJSProjectHTMLFile];
    HTMLIdx:=CBHTMLFile.Items.IndexOf(HFN);
    end;
  CBHTMLFile.ItemIndex:=HTMLIdx;
  CBMaintainHTMLFile.Checked:=PRJ.CustomData[PJSProjectMaintainHTML]='1';
  CBUseBrowserConsole.Checked:=PRJ.CustomData[PJSProjectWebBrowser]='1';
  CBRunOnReady.Checked:=PRJ.CustomData[PJSProjectRunAtReady]='1';
  Port:=StrToIntDef(PRJ.CustomData[PJSProjectPort],0);
  URL:=PRJ.CustomData[PJSProjectURL];
  CBUseHTTPServer.Checked:=(Port>0) or (URL<>'');
  SEPort.Value:=Port;
  CBServerURL.AddHistoryItem(URL,10,True,False);
end;

procedure TPas2JSProjectOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);

Var
  Prj : TLazProject;

  Procedure DoBool(N : String; AValue : Boolean);

  begin
    if AValue then
      PRJ.CustomData[N]:='1';
  end;

begin
  PRJ:=LazarusIDE.ActiveProject;
  // Clear everything
  With PRJ.CustomData do
    begin
    Remove(PJSProjectWebBrowser);
    Remove(PJSProjectHTMLFile);
    Remove(PJSProjectMaintainHTML);
    Remove(PJSProjectWebBrowser);
    Remove(PJSProjectRunAtReady);
    Remove(PJSProjectPort);
    Remove(PJSProjectURL);
    end;
  // Set what is needed
  if CBWebProject.Checked then
    begin
    PRJ.CustomData[PJSProjectWebBrowser]:='1';
    With CBHTMLFile do
      if ItemIndex<>-1 then
        (Items.Objects[ItemIndex] as TLazProjectFile).CustomData[PJSIsProjectHTMLFile]:='1';
    DoBool(PJSProjectMaintainHTML,CBMaintainHTMLFile.Checked);
    DoBool(PJSProjectWebBrowser,CBUseBrowserConsole.Checked);
    DoBool(PJSProjectRunAtReady,CBRunOnReady.Checked);
    if CBUseHTTPServer.Checked then
      begin
      if RBStartServerAt.Checked and (SEPort.Value>0) then
        PRJ.CustomData[PJSProjectPort]:=IntToStr(SEPort.Value);
      if RBUseURL.Checked and (CBServerURL.Text<>'') then
        PRJ.CustomData[PJSProjectURL]:=CBServerURL.Text;
      end;
    end;
end;

class function TPas2JSProjectOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result:=TAbstractIDEProjectOptions;
end;

end.

