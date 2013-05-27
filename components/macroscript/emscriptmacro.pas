unit EMScriptMacro;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SrcEditorIntf, IDECommands, IDEMsgIntf, Controls, SynEdit,
  SynEditKeyCmds, EMScriptClasses, Laz2_XMLCfg, LazLoggerBase, uPSCompiler, uPSRuntime,
  uPSUtils, uPSC_std, uPSR_std, uPSDebugger;

type

  { TEMSEditorMacro }

  TEMSEditorMacro = class(TEditorMacro)
  private
    FMacroName: String;
    FSource: String;
    FState: TEditorMacroState;
    FHasError: Boolean;
    FErrorMsg: String;
    FKeyBinding: TEditorMacroKeyBinding;
  protected
    function  GetMacroName: String; override;
    procedure SetMacroName(AValue: string); override;
    function  GetState: TEditorMacroState; override;
    function  GetErrorMsg: String; override;
    function  GetKeyBinding: TEditorMacroKeyBinding; override;

    procedure DoRecordMacro({%H-}aEditor: TWinControl); override;
    procedure DoPlaybackMacro(aEditor: TWinControl); override;
    procedure DoStop; override;
    procedure DoPause; override;
    procedure DoResume; override;

    procedure Compile;
    procedure FixBeginEnd;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure AssignEventsFrom(AMacroRecorder: TEditorMacro); override;

    procedure Clear; override;

    function  GetAsSource: String; override;
    procedure SetFromSource(const AText: String); override;
    procedure WriteToXmlConf(AConf: TXMLConfig; const APath: String); override;
    procedure ReadFromXmlConf(AConf: TXMLConfig; const APath: String); override;

    function  IsEmpty: Boolean; override;
    function  IsInvalid: Boolean; override;
    function  IsRecording({%H-}AnEditor: TWinControl): Boolean; override;
  end;


  { TEMSTPSExec }

  TEMSTPSExec = class(TPSDebugExec)
  public
    SynEdit: TCustomSynEdit;
    procedure AddECFuncToExecEnum(const s: String);
  end;

  { TEMSPSPascalCompiler }

  TEMSPSPascalCompiler = class(TPSPascalCompiler)
  public
    procedure AddECFuncToCompEnum(const s: String);
  end;

implementation

var
  TheCompiler: TEMSPSPascalCompiler;
  TheExec: TEMSTPSExec;
  TheCLassImp: TPSRuntimeClassImporter;


function HandleEcCommandFoo({%H-}Caller: TPSExec; p: TPSExternalProcRec; {%H-}Global, Stack: TPSStack): Boolean;
var
  i: integer;
  pt: TPoint;
  e: TEMSTPSExec;
begin
  i := PtrUint(p.Ext2);
  e := TEMSTPSExec(p.Ext1);
  case i of
    ecGotoXY, ecSelGotoXY:
      begin
        pt.x := Stack.GetInt(-1);
        pt.y := Stack.GetInt(-2);
        e.SynEdit.CommandProcessor(i, '', @pt);
      end;
    ecChar:
      e.SynEdit.CommandProcessor(i, Stack.GetAnsiString(-1), nil);
    else
      e.SynEdit.CommandProcessor(i, '', nil);
  end;
  Result := True;
end;

function HandleGetCaller({%H-}Caller: TPSExec; p: TPSExternalProcRec; {%H-}Global, Stack: TPSStack): Boolean;
var
  e: TEMSTPSExec;
begin
  e := TEMSTPSExec(p.Ext1);
  Stack.SetClass(-1, e.SynEdit);
  Result := True;
end;

function CompilerOnUses(Sender: TPSPascalCompiler; const Name: TbtString): Boolean;
begin
  if Name = 'SYSTEM' then
  begin
    SIRegisterTObject(Sender);
    //SIRegister_Std(Sender);
    if Sender is TEMSPSPascalCompiler then begin
      GetEditorCommandValues(@TEMSPSPascalCompiler(Sender).AddECFuncToCompEnum);
      GetIDEEditorCommandValues(@TEMSPSPascalCompiler(Sender).AddECFuncToCompEnum);
    end;

    CompRegisterBasics(TheCompiler);
    CompRegisterTSynEdit(TheCompiler);
    Sender.AddFunction('function Caller: TSynEdit;');
    CompRegisterTClipboard(TheCompiler);

    Result := True;
  end else
    Result := False;
end;

procedure AddECFuncToExec;
begin
  GetEditorCommandValues(@TheExec.AddECFuncToExecEnum);
  GetIDEEditorCommandValues(@TheExec.AddECFuncToExecEnum);
  ExecRegisterBasics(TheExec);
  ExecRegisterTSynEdit(TheCLassImp);
  TheExec.RegisterFunctionName('CALLER', @HandleGetCaller, TheExec, nil);
  ExecRegisterTClipboard(TheCLassImp, TheExec);
end;


{ Create global objects }

procedure CreateCompiler;
begin
  TheCompiler := TEMSPSPascalCompiler.Create;
  TheCompiler.OnUses := @CompilerOnUses;
  TheCompiler.BooleanShortCircuit := True;
end;

procedure CreateExec;
begin
  TheExec := TEMSTPSExec.Create;
  TheCLassImp := TPSRuntimeClassImporter.Create;
  RIRegisterTObject(TheCLassImp);
  // ## RIRegister_Std(CL);

  AddECFuncToExec;

  RegisterClassLibraryRuntime(TheExec, TheCLassImp);
end;

{ TEMSPSPascalCompiler }

procedure TEMSPSPascalCompiler.AddECFuncToCompEnum(const s: String);
begin
  if (s = 'ecSynMacroPlay') or (s = 'ecSynMacroRecord') then exit;

  if (s = 'ecGotoXY') or (s = 'ecSelGotoXY') then
    AddFunction('procedure '+s+'(X, Y: Integer);')
  else
  if  (s = 'ecChar') then
    AddFunction('procedure '+s+'(s: string);')
    // ecString
  else
    AddFunction('procedure '+s+';');
end;

{ TEMSTPSExec }

procedure TEMSTPSExec.AddECFuncToExecEnum(const s: String);
var
  i: longint;
begin
  i := 0;
  if not IdentToEditorCommand(s, i) then exit;
  TheExec.RegisterFunctionName(UpperCase(s), @HandleEcCommandFoo, self, Pointer(PtrUInt(i)));
end;


{ TEMSEditorMacro }

function TEMSEditorMacro.GetMacroName: String;
begin
  Result := FMacroName;
end;

procedure TEMSEditorMacro.SetMacroName(AValue: string);
begin
  FMacroName := AValue;
  DoChanged;
end;

function TEMSEditorMacro.GetState: TEditorMacroState;
begin
  Result := FState;
end;

function TEMSEditorMacro.GetErrorMsg: String;
begin
  Result := FErrorMsg;
end;

function TEMSEditorMacro.GetKeyBinding: TEditorMacroKeyBinding;
begin
  if FKeyBinding = nil then
    FKeyBinding := GetDefaultKeyBinding;
  Result := FKeyBinding;
end;

procedure TEMSEditorMacro.DoRecordMacro(aEditor: TWinControl);
begin
  // Not supported
end;

procedure TEMSEditorMacro.DoPlaybackMacro(aEditor: TWinControl);
var
  s, s2: tbtString;
  ExObj: TObject;
  i, x, y: Cardinal;
begin
  if IsEmpty or IsInvalid then exit;

  FState := emPlaying;
  if Assigned(OnStateChange) then
    OnStateChange(Self);

  try
    Compile;
    if IsInvalid then exit;

    TheCompiler.GetOutput({%H-}s);
    if not TheExec.LoadData(s) then // Load the data from the Data string.
      exit;
    TheCompiler.GetDebugOutput({%H-}s2);
    TheExec.LoadDebugData(s2);

    TheExec.SynEdit := aEditor as TCustomSynEdit;
    try
      TheExec.RunScript;
    except
      on e: Exception do
        IDEMessagesWindow.AddMsg(Format('%s: %s', [e.ClassName, e.Message]), '', -1);
    end;
    if TheExec.ExceptionCode <> erNoError then begin
      ExObj := TheExec.ExceptionObject;
      if ExObj <> nil then
        s := ExObj.ClassName
      else
        s := '<nil>';
      s2 := '';
      i := 0;
      x := 0;
      y := 0;
      TheExec.TranslatePositionEx(TheExec.ExceptionProcNo, TheExec.ExceptionPos, i, x, y, s2);
      if IDEMessagesWindow <> nil then
        IDEMessagesWindow.AddMsg(Format('%s: "%s" at %d/%d', [s, TheExec.ExceptionString, x,y]), '', -1);
    end;

  finally
    FState := emStopped;
    if Assigned(OnStateChange) then
      OnStateChange(Self);
  end;
end;

procedure TEMSEditorMacro.DoStop;
begin

end;

procedure TEMSEditorMacro.DoPause;
begin
  // Not supported
end;

procedure TEMSEditorMacro.DoResume;
begin
  // Not supported
end;

procedure TEMSEditorMacro.Compile;
var
  i: Integer;
begin
  FHasError := False;
  FErrorMsg := '';
  TheCompiler.Clear;


  if not TheCompiler.Compile(FSource) then
  begin
    for i := 0 to TheCompiler.MsgCount -1 do
      FErrorMsg := FErrorMsg + TheCompiler.Msg[i].MessageToString + LineEnding;
    FHasError := True;
  end;
end;

procedure TEMSEditorMacro.FixBeginEnd;
begin
  if (not IsEmpty) and (not IsInvalid) then
  begin
    if (pos('begin', FSource) < 1) or (pos('end.', FSource) < 1) then
      FSource := 'begin' + LineEnding + FSource + 'end.'+LineEnding;
  end;
end;

constructor TEMSEditorMacro.Create(aOwner: TComponent);
begin
  FState := emStopped;
end;

destructor TEMSEditorMacro.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FKeyBinding);
end;

procedure TEMSEditorMacro.AssignEventsFrom(AMacroRecorder: TEditorMacro);
begin
  FHasError := False;
  if AMacroRecorder = nil then
    Clear
  else
    FSource := AMacroRecorder.GetAsSource;

  if not(AMacroRecorder is TEMSEditorMacro) then
    FixBeginEnd;
  Compile;
  DoChanged;
end;

procedure TEMSEditorMacro.Clear;
begin
  FSource := '';
  FHasError := False;
  DoChanged;
end;

function TEMSEditorMacro.GetAsSource: String;
begin
  Result := FSource;
end;

procedure TEMSEditorMacro.SetFromSource(const AText: String);
begin
  FSource := AText;
  Compile;
  DoChanged;
end;

procedure TEMSEditorMacro.WriteToXmlConf(AConf: TXMLConfig; const APath: String);
begin
  AConf.SetValue(APath + 'Name', MacroName);
  AConf.SetValue(APath + 'Code/Value', GetAsSource);

  if (KeyBinding <> nil) then
    KeyBinding.WriteToXmlConf(AConf, APath);
end;

procedure TEMSEditorMacro.ReadFromXmlConf(AConf: TXMLConfig; const APath: String);
var
  s: String;
begin
  s := AConf.GetValue(APath + 'Code/Value', '');
  FSource := s;
  FixBeginEnd;
  Compile;
  s := AConf.GetValue(APath + 'Name', '');
  if s <> '' then MacroName := s;

  if (not FHasError) and (IsEmpty) then begin
    FHasError := True;
    FErrorMsg := s;
  end;

  if (KeyBinding <> nil) then
    KeyBinding.ReadFromXmlConf(AConf, APath);

  DoChanged;
end;

function TEMSEditorMacro.IsEmpty: Boolean;
begin
  Result := FSource = '';
end;

function TEMSEditorMacro.IsInvalid: Boolean;
begin
  Result := FHasError;
end;

function TEMSEditorMacro.IsRecording(AnEditor: TWinControl): Boolean;
begin
  Result := False;
end;

initialization
  CreateCompiler;
  CreateExec;

finalization
  FreeAndNil(TheExec);
  FreeAndNil(TheCLassImp);
  FreeAndNil(TheCompiler);

end.

