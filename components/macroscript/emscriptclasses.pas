unit EMScriptClasses;
{
  Classes that can be accessed from Scripts
}

{$mode objfpc}{$H+}
{$WARN 4055 off : Conversion between ordinals and pointers is not portable} // PtrInt is ok
interface

{$IFDEF PasMacroNativeCalls}
{$IFDEF darwin}
  {$DEFINE NeedTPointFix }
{$ENDIF}
{$ENDIF}

uses
  Classes, SysUtils, SynEdit, SynEditTypes, SynEditKeyCmds, LazLoggerBase, IDECommands,
  Clipbrd, Dialogs, Controls, uPSCompiler, uPSRuntime, uPSUtils, uPSDebugger, uPSR_std,
  uPSC_std;

{$IFnDEF PasMacroNativeCalls}
const
  FunctionId_POINT =             0;

  FunctionId_MessageDlg =        50;
  FunctionId_MessageDlgPos =     51;
  FunctionId_MessageDlgPosHelp = 52;
  FunctionId_ShowMessage =       53;
  FunctionId_ShowMessagePos =    54;
  FunctionId_InputBox =          55;
  FunctionId_InputQuery =        56;

  PropReadId_CaretXY =          100;
  PropWriteId_CaretXY =         101;
  PropReadId_CaretX =           102;
  PropWriteId_CaretX =          103;
  PropReadId_CaretY =           104;
  PropWriteId_CaretY =          105;
  PropReadId_LogicalCaretXY =   106;
  PropWriteId_LogicalCaretXY =  107;
  PropReadId_LogicalCaretX =    108;
  PropWriteId_LogicalCaretX =   109;
  FunctionId_MoveCaretIgnoreEOL        = 110;
  FunctionId_MoveLogicalCaretIgnoreEOL = 111;

  PropReadId_BlockBegin =             120;
  PropWriteId_BlockBegin =            121;
  PropReadId_BlockEnd =               122;
  PropWriteId_BlockEnd =              123;
  PropReadId_SelAvail =               124;
  PropWriteId_SelAvail =              125;
  PropReadId_SelText =                126;
  PropWriteId_SelText =               127;
  PropReadId_SelectionMode =          128;
  PropWriteId_SelectionMode =         129;
  FunctionId_ClearSelection =         130;
  FunctionId_SelectAll =              131;
  FunctionId_SelectToBrace =          132;
  FunctionId_SelectWord =             133;
  FunctionId_SelectLine =             134;
  FunctionId_SelectParagraph =        135;
  FunctionId_SearchReplace =          140;
  FunctionId_SearchReplaceEx =        141;

  PropReadId_Lines =                  150;
  PropWriteId_Lines =                 151;
  PropReadId_LineAtCaret =            152;
  PropWriteId_LineAtCaret =           153;
  FunctionId_InsertTextAtCaret =      154;
  PropReadId_TextBetweenPoints =      155;
  PropWriteId_TextBetweenPoints =     156;
  //PropReadId_TextBetweenPointsEx =    15x;
  //PropWriteId_TextBetweenPointsEx =   15x;
  FunctionId_SetTextBetweenPoints =   157;

  FunctionId_CopyToClipboard =        160;
  FunctionId_CutToClipboard =         161;
  FunctionId_PasteFromClipboard =     162;
  PropReadId_CanPaste =               163;
  PropWriteId_CanPaste =              164;

  FunctionId_LogicalToPhysicalPos =   170;
  FunctionId_LogicalToPhysicalCol =   171;
  FunctionId_PhysicalToLogicalPos =   172;
  FunctionId_PhysicalToLogicalCol =   173;
  FunctionId_PhysicalLineLength =     174;

{$ENDIF}

type
  TEMScriptBadParamException = Exception;

  { TEMSTPSExec }

  TEMSTPSExec = class(TPSDebugExec)
  protected
    FCLassImp: TPSRuntimeClassImporter;
    FSynEdit: TCustomSynEdit;

    procedure AddFuncToExec; virtual;
    procedure AddECFuncToExecEnum(const s: String); // ec... commands
  public
    constructor Create;
    destructor Destroy; override;
    property SynEdit: TCustomSynEdit read FSynEdit write FSynEdit;
  end;

  { TEMSPSPascalCompiler }

  TEMSPSPascalCompiler = class(TPSPascalCompiler)
  private
    procedure AddECFuncToCompEnum(const s: String);
  public
    constructor Create;
  end;

{$IFnDEF PasMacroNativeCalls}
  PPoint = ^TPoint;

function GetSetFromStack(Stack: TPSStack; Idx: Integer): Cardinal;
function GetEnumFromStack(Stack: TPSStack; Idx: Integer): Cardinal;
function GetVarPointFromStack(Stack: TPSStack; Idx: Integer): PPoint;
function GetPointFromStack(Stack: TPSStack; Idx: Integer): TPoint;
{$ENDIF}


implementation

procedure CompRegisterBasics(AComp: TPSPascalCompiler); forward;
procedure ExecRegisterBasics(AExec: TEMSTPSExec); forward;

procedure CompRegisterTSynEdit(AComp: TPSPascalCompiler); forward;
procedure ExecRegisterTSynEdit(AExec: TEMSTPSExec); forward;

procedure CompRegisterTClipboard(AComp: TPSPascalCompiler); forward;
procedure ExecRegisterTClipboard(AExec: TEMSTPSExec); forward;

{$IFDEF NeedTPointFix}
type TPoint2 = record x,y,a,b,c: Longint; end;
{$ENDIF}

{ TEMSPSPascalCompiler }

function CompilerOnUses(Sender: TPSPascalCompiler; const Name: TbtString): Boolean;
var
  S: TEMSPSPascalCompiler;
begin
  if Name = 'SYSTEM' then
  begin
    SIRegisterTObject(Sender);
    //SIRegister_Std(Sender);

    if Sender is TEMSPSPascalCompiler then begin
      S := TEMSPSPascalCompiler(Sender);
      // ec... commands
      GetEditorCommandValues(@S.AddECFuncToCompEnum);
      GetIDEEditorCommandValues(@S.AddECFuncToCompEnum);

      CompRegisterBasics(S);
      CompRegisterTSynEdit(S);
      S.AddFunction('function Caller: TSynEdit;');
      CompRegisterTClipboard(S);
    end;

    Result := True;
  end else
    Result := False;
end;

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

constructor TEMSPSPascalCompiler.Create;
begin
  inherited Create;
  OnUses := @CompilerOnUses;
  BooleanShortCircuit := True;
end;

{ TEMSTPSExec }

function HandleGetCaller({%H-}Caller: TPSExec; p: TPSExternalProcRec; {%H-}Global, Stack: TPSStack): Boolean;
var
  e: TEMSTPSExec;
begin
  e := TEMSTPSExec(p.Ext1);
  Stack.SetClass(-1, e.SynEdit);
  Result := True;
end;

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

constructor TEMSTPSExec.Create;
begin
  inherited Create;
  FCLassImp := TPSRuntimeClassImporter.Create;
  RIRegisterTObject(FCLassImp);
  // ## RIRegister_Std(CL);
  AddFuncToExec;
  RegisterClassLibraryRuntime(Self, FCLassImp);
end;

destructor TEMSTPSExec.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FCLassImp);
end;

procedure TEMSTPSExec.AddECFuncToExecEnum(const s: String);
var
  i: longint;
begin
  i := 0;
  if not IdentToEditorCommand(s, i) then exit;
  RegisterFunctionName(UpperCase(s), @HandleEcCommandFoo, self, Pointer(PtrUInt(i)));
end;

procedure TEMSTPSExec.AddFuncToExec;
begin
  GetEditorCommandValues(@AddECFuncToExecEnum);
  GetIDEEditorCommandValues(@AddECFuncToExecEnum);
  ExecRegisterBasics(Self);
  ExecRegisterTSynEdit(Self);
  RegisterFunctionName('CALLER', @HandleGetCaller, Self, nil);
  ExecRegisterTClipboard(Self);
end;

{%region RegisterBasics}

Function EMS_MessageDlg(Msg: string; DlgType :TMsgDlgType; Buttons :TMsgDlgButtons; HelpCtx: Longint): Integer;
begin
  Result := MessageDlg(Msg, DlgType, Buttons, HelpCtx);
end;
Function EMS_MessageDlgPos(Msg: string; DlgType :TMsgDlgType; Buttons :TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer): Integer;
begin
  Result := MessageDlgPos(Msg, DlgType, Buttons, HelpCtx, X, Y);
end;
Function EMS_MessageDlgPosHelp(Msg: string; DlgType :TMsgDlgType; Buttons :TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer; HelpFileName: string): Integer;
begin
  Result := MessageDlgPosHelp(Msg, DlgType, Buttons, HelpCtx, X, Y, HelpFileName);
end;
Procedure EMS_ShowMessage(Msg: string);
begin
  ShowMessage(Msg);
end;
Procedure EMS_ShowMessagePos(Msg: string; X, Y :Integer);
begin
  ShowMessagePos(Msg, X, Y);
end;
Function EMS_InputBox(ACaption, APrompt, ADefault: string): string;
begin
  Result := InputBox(ACaption, APrompt, ADefault);
end;
Function EMS_InputQuery(ACaption, APrompt: string; var Value: string): Boolean;
begin
  Result := InputQuery(ACaption, APrompt, Value);
end;

function EMS_Point(AX, AY: Integer): {$IFDEF NeedTPointFix}TPoint2{$ELSE}TPoint{$ENDIF};
begin
  Result.X := AX;
  Result.Y := AY;
end;

const
  DeclMessageDlg        = 'Function MessageDlg(Msg: string; DlgType :TMsgDlgType; Buttons :TMsgDlgButtons; HelpCtx: Longint): Integer';
  DeclMessageDlgPos     = 'Function MessageDlgPos(Msg: string; DlgType :TMsgDlgType; Buttons :TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer): Integer';
  DeclMessageDlgPosHelp = 'Function MessageDlgPosHelp(Msg: string; DlgType :TMsgDlgType; Buttons :TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer; HelpFileName: string): Integer';
  DeclShowMessage       = 'Procedure ShowMessage(Msg: string)';
  DeclShowMessagePos    = 'Procedure ShowMessagePos(Msg: string; X, Y :Integer)';
  DeclInputBox          = 'Function InputBox(ACaption, APrompt, ADefault: string): string';
  DeclInputQuery        = 'Function InputQuery(ACaption, APrompt: string; var Value: string): Boolean';

  FuncMessageDlg:        function(Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer = @EMS_MessageDlg;
  FuncMessageDlgPos:     function(Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer): Integer = @EMS_MessageDlgPos;
  FuncMessageDlgPosHelp: function(Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer; HelpFileName: string): Integer = @EMS_MessageDlgPosHelp;
  FuncShowMessage:       procedure(Msg: string) = @EMS_ShowMessage;
  FuncShowMessagePos:    procedure(Msg: string; X, Y: Integer) = @EMS_ShowMessagePos;
  FuncInputBox:          function(ACaption, APrompt, ADefault: string): string = @EMS_InputBox;
  FuncInputQuery:        function(ACaption, APrompt: string; var Value : string): Boolean = @EMS_InputQuery;

  DeclPoint = 'function Point(AX, AY: Integer): TPoint;';
  FuncPoint: function(AX, AY: Integer): {$IFDEF NeedTPointFix}TPoint2{$ELSE}TPoint{$ENDIF} = @EMS_Point; // @Classes.Point;

procedure CompRegisterBasics(AComp: TPSPascalCompiler);
  procedure AddConst(const Name, FType: TbtString; I: Integer);
  begin
    AComp.AddConstantN(Name, FType).Value^.ts32 := I;
  end;

begin
  AComp.AddTypeS('TPoint', 'record x,y: Longint; end;');
  AComp.AddDelphiFunction(DeclPoint);

  AddConst('mrNone', 'Integer', mrNone);
  AddConst('mrOk', 'Integer', mrOK);
  AddConst('mrCancel', 'Integer', mrCancel);
  AddConst('mrAbort', 'Integer', mrAbort);
  AddConst('mrRetry', 'Integer', mrRetry);
  AddConst('mrIgnore', 'Integer', mrIgnore);
  AddConst('mrYes', 'Integer', mrYes);
  AddConst('mrNo', 'Integer', mrNo);
  AddConst('mrAll', 'Integer', mrAll);
  AddConst('mrNoToAll', 'Integer', mrNoToAll);
  AddConst('mrYesToAll', 'Integer', mrYesToAll);
  AComp.AddTypeS('TMsgDlgType', '( mtWarning, mtError, mtInformation, mtConfirmation, mtCustom )');
  AComp.AddTypeS('TMsgDlgBtn', '( mbYes, mbNo, mbOK, mbCancel, mbAbort, mbRetry, mbIgnore, mbAll, mbNoToAll, mbYesToAll, mbHelp )');
  AComp.AddTypeS('TMsgDlgButtons', 'set of TMsgDlgBtn');

  AComp.AddDelphiFunction(DeclMessageDlg);
  AComp.AddDelphiFunction(DeclMessageDlgPos);
  AComp.AddDelphiFunction(DeclMessageDlgPosHelp);
  AComp.AddDelphiFunction(DeclShowMessage);
  AComp.AddDelphiFunction(DeclShowMessagePos);
  AComp.AddDelphiFunction(DeclInputBox);
  AComp.AddDelphiFunction(DeclInputQuery);
end;

{$IFnDEF PasMacroNativeCalls}
function GetSetFromStack(Stack: TPSStack; Idx: Integer): Cardinal;
var
  val: PPSVariant;
  dat: Pointer;
begin
  if Idx < 0 then Idx := Idx + Stack.Count;
  val := Stack[Idx];
  if val^.FType.BaseType <> btSet then raise TEMScriptBadParamException.Create('Invalid set');
  dat := @PPSVariantData(val)^.Data;
  Result := tbtu32(dat^);
end;

function GetEnumFromStack(Stack: TPSStack; Idx: Integer): Cardinal;
var
  val: PPSVariant;
  dat: Pointer;
begin
  if Idx < 0 then Idx := Idx + Stack.Count;
  val := Stack[Idx];
  if val^.FType.BaseType <> btEnum then raise TEMScriptBadParamException.Create('Invalid set');
  dat := @PPSVariantData(val)^.Data;
  Result := tbtu32(dat^);
end;

function GetVarPointFromStack(Stack: TPSStack; Idx: Integer): PPoint;
var
  res: PPSVariant;
  data: Pointer;
  typerec: TPSTypeRec;
begin
  if Idx < 0 then Idx := Idx + Stack.Count;
  res := Stack[Idx];
  typerec := res^.FType;

  if typerec.BaseType = btPointer then begin
    typerec := PPSVariantPointer(res)^.DestType;
    Result := PPSVariantPointer(res)^.DataDest;
  end
  else
    Result := @(PPSVariantRecord(res)^.data);

  if typerec.BaseType <> btRecord then raise TEMScriptBadParamException.Create('Invalid result type for "point(x,y)"');
  if typerec.RealSize <> SizeOf({$IFDEF NeedTPointFix}TPoint2{$ELSE}TPoint{$ENDIF}) then raise TEMScriptBadParamException.Create('Invalid result size for "point(x,y)"');
  if Result = nil then raise TEMScriptBadParamException.Create('Invalid result data for "point(x,y)"');
end;

function GetPointFromStack(Stack: TPSStack; Idx: Integer): TPoint;
var
  res: PPSVariant;
  data: Pointer;
  typerec: TPSTypeRec;
begin
  if Idx < 0 then Idx := Idx + Stack.Count;
  res := Stack[Idx];
  typerec := res^.FType;

  if typerec.BaseType <> btRecord then raise TEMScriptBadParamException.Create('Invalid result type for "point(x,y)"');
  if typerec.RealSize <> SizeOf({$IFDEF NeedTPointFix}TPoint2{$ELSE}TPoint{$ENDIF}) then raise TEMScriptBadParamException.Create('Invalid result size for "point(x,y)"');

  data := @(PPSVariantRecord(res)^.data);
  if data = nil then raise TEMScriptBadParamException.Create('Invalid result data for "point(x,y)"');

  Result := PPoint(data)^;

end;

function ExecBasicHandler({%H-}Caller: TPSExec; p: TPSExternalProcRec;
  {%H-}Global, Stack: TPSStack): Boolean;
  procedure CheckMinParamCount(AMinCnt: Integer; const AName: String);
  begin
    if Stack.Count < AMinCnt then raise TEMScriptBadParamException.Create('Invalid param count for "'+AName+'"');
  end;
  function GetSynEditFromStack(AIndex: Integer; out ASyn: TSynEdit): boolean;
  var
    o: TObject;
  begin
    o := Stack.GetClass(AIndex);
    Result := (o <> nil) and (o is TSynEdit);
    ASyn := TSynEdit(o);
  end;
var
  res: PPSVariant;
  data: PPoint;
  temp: TPSVariantIFC;
  s: String;
  typerec: TPSTypeRec;
  Obj: TSynEdit;
begin
  Result := True;
  case Longint(p.Ext1) of
    FunctionId_POINT: begin // POINT()
        CheckMinParamCount(3, 'Point');
        data := GetVarPointFromStack(Stack, -1);
        TPoint(data^) := Point(Stack.GetInt(-2), Stack.GetInt(-3));
      end;

    FunctionId_MessageDlg: begin // MessageDlg(Msg: string; DlgType :TMsgDlgType; Buttons :TMsgDlgButtons; HelpCtx: Longint): Integer';
        CheckMinParamCount(5, 'MessageDlg');
        Stack.SetInt(-1,
          MessageDlg(Stack.GetAnsiString(-2), TMsgDlgType(Stack.GetUInt(-3)),
            TMsgDlgButtons(GetSetFromStack(Stack, -4)), Stack.GetInt(-5))
        );
      end;
    FunctionId_MessageDlgPos: begin // MessageDlgPos(Msg: string; DlgType :TMsgDlgType; Buttons :TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer): Integer
        CheckMinParamCount(7, 'MessageDlgPos');
        Stack.SetInt(-1,
          MessageDlgPos(Stack.GetAnsiString(-2), TMsgDlgType(Stack.GetUInt(-3)),
            TMsgDlgButtons(GetSetFromStack(Stack, -4)), Stack.GetInt(-5),
            Stack.GetInt(-6), Stack.GetInt(-7) )
        );
      end;
    FunctionId_MessageDlgPosHelp: begin // MessageDlgPosHelp(Msg: string; DlgType :TMsgDlgType; Buttons :TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer; HelpFileName: string): Integer
        CheckMinParamCount(8, 'MessageDlgPosHelp');
        Stack.SetInt(-1,
          MessageDlgPosHelp(Stack.GetAnsiString(-2), TMsgDlgType(Stack.GetUInt(-3)),
            TMsgDlgButtons(GetSetFromStack(Stack, -4)), Stack.GetInt(-5),
            Stack.GetInt(-6), Stack.GetInt(-7), Stack.GetAnsiString(-8))
        );
      end;
    FunctionId_ShowMessage: begin // ShowMessage(Msg: string)
        CheckMinParamCount(1, 'ShowMessage');
        ShowMessage(Stack.GetAnsiString(-1));
      end;
    FunctionId_ShowMessagePos: begin // ShowMessagePos(Msg: string; X, Y :Integer)
        CheckMinParamCount(3, 'ShowMessagePos');
        ShowMessagePos(Stack.GetAnsiString(-1), Stack.GetInt(-2), Stack.GetInt(-3));
      end;
    FunctionId_InputBox: begin // InputBox(ACaption, APrompt, ADefault: string): string
        CheckMinParamCount(4, 'InputBox');
        Stack.SetAnsiString(-1,
          InputBox(Stack.GetAnsiString(-2), Stack.GetAnsiString(-3), Stack.GetAnsiString(-4))
        );
      end;
    FunctionId_InputQuery: begin // InputQuery(ACaption, APrompt: string; var Value: string): Boolean
        CheckMinParamCount(4, 'InputQuery');
        temp := NewTPSVariantIFC(Stack[Stack.Count-4], True);
        if (temp.aType.BaseType <> btString) then raise TEMScriptBadParamException.Create('Invalid param type for "InputQuery"');
        if (temp.Dta = nil) then raise TEMScriptBadParamException.Create('Invalid param data for "InputQuery"');
        s := tbtstring(temp.Dta^);
        Stack.SetBool(-1,
          InputQuery(Stack.GetAnsiString(-2), Stack.GetAnsiString(-3), s)
        );
        tbtstring(temp.Dta^) := s;
      end;


    PropReadId_CaretXY: begin
        CheckMinParamCount(2, 'TSynEdit.CaretXY (r)');
        Result := GetSynEditFromStack(-2, Obj);
        if not Result then
          exit;
        GetVarPointFromStack(Stack, -1)^ := Obj.CaretXY;
      end;
    PropWriteId_CaretXY: begin
        CheckMinParamCount(2, 'TSynEdit.CaretXY (w)');
        Result := GetSynEditFromStack(-1, Obj);
        if not Result then
          exit;
        Obj.CaretXY := GetVarPointFromStack(Stack, -2)^;
      end;
    PropReadId_CaretX: begin
        CheckMinParamCount(2, 'TSynEdit.CaretX (r)');
        Result := GetSynEditFromStack(-2, Obj);
        if not Result then
          exit;
        Stack.SetInt(-1, Obj.CaretX);
      end;
    PropWriteId_CaretX: begin
        CheckMinParamCount(2, 'TSynEdit.CaretX (w)');
        Result := GetSynEditFromStack(-1, Obj);
        if not Result then
          exit;
        Obj.CaretX := Stack.GetInt(-2);
      end;
    PropReadId_CaretY: begin
        CheckMinParamCount(2, 'TSynEdit.CaretY (r)');
        Result := GetSynEditFromStack(-2, Obj);
        if not Result then
          exit;
        Stack.SetInt(-1, Obj.CaretY);
      end;
    PropWriteId_CaretY: begin
        CheckMinParamCount(2, 'TSynEdit.CaretY (w)');
        Result := GetSynEditFromStack(-1, Obj);
        if not Result then
          exit;
        Obj.CaretY := Stack.GetInt(-2);
      end;
    PropReadId_LogicalCaretXY: begin
        CheckMinParamCount(2, 'TSynEdit.LogicalCaretXY (r)');
        Result := GetSynEditFromStack(-2, Obj);
        if not Result then
          exit;
        GetVarPointFromStack(Stack, -1)^ := Obj.LogicalCaretXY;
      end;
    PropWriteId_LogicalCaretXY: begin
        CheckMinParamCount(2, 'TSynEdit.LogicalCaretXY (w)');
        Result := GetSynEditFromStack(-1, Obj);
        if not Result then
          exit;
        Obj.LogicalCaretXY := GetVarPointFromStack(Stack, -2)^;
      end;
    PropReadId_LogicalCaretX: begin
        CheckMinParamCount(2, 'TSynEdit.LogicalCaretX (r)');
        Result := GetSynEditFromStack(-2, Obj);
        if not Result then
          exit;
        Stack.SetInt(-1, Obj.LogicalCaretXY.X);
      end;
    PropWriteId_LogicalCaretX: begin
        CheckMinParamCount(2, 'TSynEdit.LogicalCaretX (w)');
        Result := GetSynEditFromStack(-1, Obj);
        if not Result then
          exit;
        Obj.LogicalCaretXY := Point(Stack.GetInt(-2), Obj.CaretY);
      end;
    FunctionId_MoveCaretIgnoreEOL: begin  // procedure MoveCaretIgnoreEOL(NewCaret: TPoint);
        CheckMinParamCount(2, 'TSynEdit.MoveCaretIgnoreEOL');
        Result := GetSynEditFromStack(-1, Obj);
        if not Result then
          exit;
        Obj.MoveCaretIgnoreEOL(GetVarPointFromStack(Stack, -2)^);
      end;
    FunctionId_MoveLogicalCaretIgnoreEOL: begin  // procedure MoveLogicalCaretIgnoreEOL(NewLogCaret: TPoint);
        CheckMinParamCount(2, 'TSynEdit.MoveLogicalCaretIgnoreEOL');
        Result := GetSynEditFromStack(-1, Obj);
        if not Result then
          exit;
        Obj.MoveLogicalCaretIgnoreEOL(GetVarPointFromStack(Stack, -2)^);
      end;


    PropReadId_BlockBegin: begin  //
        CheckMinParamCount(2, 'TSynEdit.BlockBegin (r)');
        Result := GetSynEditFromStack(-2, Obj);
        if not Result then
          exit;
        GetVarPointFromStack(Stack, -1)^ := Obj.BlockBegin;
      end;
    PropWriteId_BlockBegin: begin  //
        CheckMinParamCount(2, 'TSynEdit.BlockBegin (w)');
        Result := GetSynEditFromStack(-1, Obj);
        if not Result then
          exit;
        Obj.BlockBegin := GetVarPointFromStack(Stack, -2)^;
      end;
    PropReadId_BlockEnd: begin  //
        CheckMinParamCount(2, 'TSynEdit.BlockEnd (r)');
        Result := GetSynEditFromStack(-2, Obj);
        if not Result then
          exit;
        GetVarPointFromStack(Stack, -1)^ := Obj.BlockEnd;
      end;
    PropWriteId_BlockEnd: begin  //
        CheckMinParamCount(2, 'TSynEdit.BlockEnd (w)');
        Result := GetSynEditFromStack(-1, Obj);
        if not Result then
          exit;
        Obj.BlockEnd := GetVarPointFromStack(Stack, -2)^;
      end;
    PropReadId_SelAvail: begin  //
        CheckMinParamCount(2, 'TSynEdit.SelAvail (r)');
        Result := GetSynEditFromStack(-2, Obj);
        if not Result then
          exit;
        Stack.SetBool(-1, Obj.SelAvail);
      end;
    PropWriteId_SelAvail: begin  //
        assert(false, 'read only');
      end;
    PropReadId_SelText: begin  //
        CheckMinParamCount(2, 'TSynEdit.SelText (r)');
        Result := GetSynEditFromStack(-2, Obj);
        if not Result then
          exit;
        Stack.SetAnsiString(-1, Obj.SelText);
      end;
    PropWriteId_SelText: begin  //
        CheckMinParamCount(2, 'TSynEdit.SelText (w)');
        Result := GetSynEditFromStack(-1, Obj);
        if not Result then
          exit;
        Obj.SelText := Stack.GetAnsiString(-2);
      end;
    PropReadId_SelectionMode: begin  //
        CheckMinParamCount(2, 'TSynEdit.SelectionMode (r)');
        Result := GetSynEditFromStack(-2, Obj);
        if not Result then
          exit;
        Stack.SetUInt(-1, cardinal(Obj.SelectionMode));
      end;
    PropWriteId_SelectionMode: begin  //
        CheckMinParamCount(2, 'TSynEdit.SelectionMode (w)');
        Result := GetSynEditFromStack(-1, Obj);
        if not Result then
          exit;
        Obj.SelectionMode := TSynSelectionMode(Stack.GetUInt(-2));
      end;
    FunctionId_ClearSelection: begin  //
        CheckMinParamCount(1, 'TSynEdit.ClearSelection');
        Result := GetSynEditFromStack(-1, Obj);
        if not Result then
          exit;
        Obj.ClearSelection;
      end;
    FunctionId_SelectAll: begin  //
        CheckMinParamCount(1, 'TSynEdit.SelectAll');
        Result := GetSynEditFromStack(-1, Obj);
        if not Result then
          exit;
        Obj.SelectAll;
      end;
    FunctionId_SelectToBrace: begin  //
        CheckMinParamCount(1, 'TSynEdit.SelectToBrace');
        Result := GetSynEditFromStack(-1, Obj);
        if not Result then
          exit;
        Obj.SelectToBrace;
      end;
    FunctionId_SelectWord: begin  //
        CheckMinParamCount(1, 'TSynEdit.SelectWord');
        Result := GetSynEditFromStack(-1, Obj);
        if not Result then
          exit;
        Obj.SelectWord;
      end;
    FunctionId_SelectLine: begin  //
        CheckMinParamCount(2, 'TSynEdit.SelectLine');
        Result := GetSynEditFromStack(-1, Obj);
        if not Result then
          exit;
        Obj.SelectLine(Stack.GetBool(-2));
      end;
    FunctionId_SelectParagraph: begin  //
        CheckMinParamCount(1, 'TSynEdit.SelectParagraph');
        Result := GetSynEditFromStack(-1, Obj);
        if not Result then
          exit;
        Obj.SelectParagraph;
      end;

    FunctionId_SearchReplace: begin // function SearchReplace(ASearch, AReplace: string; AOptions: TSynSearchOptions): integer;
        CheckMinParamCount(5, 'TSynEdit.SearchReplace');
        Result := GetSynEditFromStack(-2, Obj);
        if not Result then
          exit;
        Stack.SetInt(-1,
          Obj.SearchReplace(Stack.GetAnsiString(-3), Stack.GetAnsiString(-4), TSynSearchOptions(GetSetFromStack(Stack, -5)))
        );
      end;
    FunctionId_SearchReplaceEx: begin  // function SearchReplaceEx(ASearch, AReplace: string; AOptions: TSynSearchOptions; AStart: TPoint): integer;
        CheckMinParamCount(6, 'TSynEdit.SearchReplaceEx');
        Result := GetSynEditFromStack(-2, Obj);
        if not Result then
          exit;
        Stack.SetInt(-1,
          Obj.SearchReplaceEx(Stack.GetAnsiString(-3), Stack.GetAnsiString(-4),
            TSynSearchOptions(GetSetFromStack(Stack, -5)), GetVarPointFromStack(Stack, -6)^)
        );
      end;


    PropReadId_Lines: begin  //
        CheckMinParamCount(3, 'TSynEdit.Lines (r)');
        Result := GetSynEditFromStack(-2, Obj);
        if not Result then
          exit;
        Stack.SetAnsiString(-1, Obj.Lines[Stack.GetInt(-3)]);
      end;
    PropWriteId_Lines: begin  //
        assert(false, 'read only');
      end;
    PropReadId_LineAtCaret: begin  //
        CheckMinParamCount(1, 'TSynEdit.LineAtCaret (r)');
        Result := GetSynEditFromStack(-2, Obj);
        if not Result then
          exit;
        Stack.SetAnsiString(-1, Obj.LineText);
      end;
    PropWriteId_LineAtCaret: begin  //
        assert(false, 'read only');
      end;

    FunctionId_InsertTextAtCaret: begin  //
        CheckMinParamCount(3, 'TSynEdit.InsertTextAtCaret');
        Result := GetSynEditFromStack(-1, Obj);
        if not Result then
          exit;
        Obj.InsertTextAtCaret(Stack.GetAnsiString(-2), TSynCaretAdjustMode(Stack.GetUInt(-3)));
      end;
    PropReadId_TextBetweenPoints: begin  //
        CheckMinParamCount(4, 'TSynEdit.TextBetweenPoints" (r)');
        Result := GetSynEditFromStack(-2, Obj);
        if not Result then
          exit;
        Stack.SetAnsiString(-1,
          Obj.TextBetweenPoints[GetVarPointFromStack(Stack, -3)^, GetVarPointFromStack(Stack, -4)^]
        );
      end;
    PropWriteId_TextBetweenPoints: begin  //
        CheckMinParamCount(4, 'TSynEdit.TextBetweenPoints" (w)');
        Result := GetSynEditFromStack(-1, Obj);
        if not Result then
          exit;
        Obj.TextBetweenPoints[GetVarPointFromStack(Stack, -2)^, GetVarPointFromStack(Stack, -3)^] := Stack.GetAnsiString(-4);
      end;
    FunctionId_SetTextBetweenPoints: begin  // procedure SetTextBetweenPoints(aStartPoint, aEndPoint: TPoint;
                                             //'AValue: String; aFlags: TSynEditTextFlags; ' + // = []
                                             //'aCaretMode: TSynCaretAdjustMode; ' + //  = scamIgnore
                                             //'aMarksMode: TSynMarksAdjustMode; ' + //  = smaMoveUp
                                             //'aSelectionMode: TSynSelectionMode);'); //  = smNormal
        CheckMinParamCount(8, 'TSynEdit.SetTextBetweenPoints');
        Result := GetSynEditFromStack(-1, Obj);
        if not Result then
          exit;
        Obj.SetTextBetweenPoints(GetVarPointFromStack(Stack, -2)^, GetVarPointFromStack(Stack, -3)^,
          Stack.GetAnsiString(-4), TSynEditTextFlags(Stack.GetUInt(-5)),
          TSynCaretAdjustMode(Stack.GetUInt(-6)), TSynMarksAdjustMode(Stack.GetUInt(-7)),
          TSynSelectionMode(Stack.GetUInt(-8))
        );
      end;


    FunctionId_CopyToClipboard: begin  //
        CheckMinParamCount(1, 'TSynEdit.CopyToClipboard');
        Result := GetSynEditFromStack(-1, Obj);
        if not Result then
          exit;
        Obj.CopyToClipboard;
      end;
    FunctionId_CutToClipboard: begin  //
        CheckMinParamCount(1, 'TSynEdit.CutToClipboard');
        Result := GetSynEditFromStack(-1, Obj);
        if not Result then
          exit;
        Obj.CutToClipboard;
      end;
    FunctionId_PasteFromClipboard: begin  //
        CheckMinParamCount(1, 'TSynEdit.PasteFromClipboard');
        Result := GetSynEditFromStack(-1, Obj);
        if not Result then
          exit;
        Obj.PasteFromClipboard;
      end;
    PropReadId_CanPaste: begin  //
        CheckMinParamCount(2, 'TSynEdit.CanPaste (r)');
        Result := GetSynEditFromStack(-2, Obj);
        if not Result then
          exit;
        Stack.SetBool(-1, Obj.CanPaste);
      end;
    PropWriteId_CanPaste: begin  //
        assert(false, 'read only');
      end;


    FunctionId_LogicalToPhysicalPos: begin  // function LogicalToPhysicalPos(p: TPoint): TPoint;
        CheckMinParamCount(3, 'TSynEdit.LogicalToPhysicalPos');
        Result := GetSynEditFromStack(-2, Obj);
        if not Result then
          exit;
        GetVarPointFromStack(Stack, -1)^ := Obj.LogicalToPhysicalPos(GetVarPointFromStack(Stack, -3)^);
      end;
    FunctionId_LogicalToPhysicalCol: begin  // function LogicalToPhysicalCol(Line: String; Index, LogicalPos : integer): integer;
        CheckMinParamCount(5, 'TSynEdit.LogicalToPhysicalCol');
        Result := GetSynEditFromStack(-2, Obj);
        if not Result then
          exit;
        Stack.SetInt(-1,
          Obj.LogicalToPhysicalCol(Stack.GetAnsiString(-3), Stack.GetInt(-4), Stack.GetInt(-5))
        );
      end;
    FunctionId_PhysicalToLogicalPos: begin  // function PhysicalToLogicalPos(p: TPoint): TPoint;
        CheckMinParamCount(3, 'TSynEdit.PhysicalToLogicalPos');
        Result := GetSynEditFromStack(-2, Obj);
        if not Result then
          exit;
        GetVarPointFromStack(Stack, -1)^ := Obj.PhysicalToLogicalPos(GetVarPointFromStack(Stack, -3)^);
      end;
    FunctionId_PhysicalToLogicalCol: begin  // function PhysicalToLogicalCol(Line: string; Index, PhysicalPos: integer): integer;
        CheckMinParamCount(5, 'TSynEdit.PhysicalToLogicalCol');
        Result := GetSynEditFromStack(-2, Obj);
        if not Result then
          exit;
        Stack.SetInt(-1,
          Obj.PhysicalToLogicalCol(Stack.GetAnsiString(-3), Stack.GetInt(-4), Stack.GetInt(-5))
        );
      end;
    FunctionId_PhysicalLineLength: begin  // function PhysicalLineLength(Line: String; Index: integer): integer;
        CheckMinParamCount(4, 'TSynEdit.PhysicalLineLength');
        Result := GetSynEditFromStack(-2, Obj);
        if not Result then
          exit;
        Stack.SetInt(-1,
          Obj.PhysicalLineLength(Stack.GetAnsiString(-3), Stack.GetInt(-4))
        );
      end;

    else
      Result := False;
  end;
end;
{$ENDIF}

procedure ExecRegisterBasics(AExec: TEMSTPSExec);
begin
  {$IFDEF PasMacroNativeCalls}
  AExec.RegisterDelphiFunction(FuncPoint, 'POINT', cdRegister);

  AExec.RegisterDelphiFunction(FuncMessageDlg, 'MessageDlg', cdRegister);
  AExec.RegisterDelphiFunction(FuncMessageDlgPos, 'MessageDlgPos', cdRegister);
  AExec.RegisterDelphiFunction(FuncMessageDlgPosHelp, 'MessageDlgPosHelp', cdRegister);
  AExec.RegisterDelphiFunction(FuncShowMessage, 'ShowMessage', cdRegister);
  AExec.RegisterDelphiFunction(FuncShowMessagePos, 'ShowMessagePos', cdRegister);
  AExec.RegisterDelphiFunction(FuncInputBox, 'InputBox', cdRegister);
  AExec.RegisterDelphiFunction(FuncInputQuery, 'InputQuery', cdRegister);
  {$ELSE}
  AExec.RegisterFunctionName('POINT',             @ExecBasicHandler, Pointer(FunctionId_POINT), nil);

  AExec.RegisterFunctionName('MessageDlg',        @ExecBasicHandler, Pointer(FunctionId_MessageDlg), nil);
  AExec.RegisterFunctionName('MessageDlgPos',     @ExecBasicHandler, Pointer(FunctionId_MessageDlgPos), nil);
  AExec.RegisterFunctionName('MessageDlgPosHelp', @ExecBasicHandler, Pointer(FunctionId_MessageDlgPosHelp), nil);
  AExec.RegisterFunctionName('ShowMessage',       @ExecBasicHandler, Pointer(FunctionId_ShowMessage), nil);
  AExec.RegisterFunctionName('ShowMessagePos',    @ExecBasicHandler, Pointer(FunctionId_ShowMessagePos), nil);
  AExec.RegisterFunctionName('InputBox',          @ExecBasicHandler, Pointer(FunctionId_InputBox), nil);
  AExec.RegisterFunctionName('InputQuery',        @ExecBasicHandler, Pointer(FunctionId_InputQuery), nil);
  {$ENDIF}
end;

{%endregion RegisterBasics}

{%region RegisterTSynEdit}

  {%region SynEdit class wrappers}

    // Caret
procedure TSynEdit_CaretXY_W(Self: TSynEdit; P: TPoint);        begin   Self.CaretXY := P;   end;
procedure TSynEdit_CaretXY_R(Self: TSynEdit; var P: TPoint);    begin   P := Self.CaretXY;   end;

procedure TSynEdit_CaretX_W(Self: TSynEdit; I: Integer);        begin   Self.CaretX := I;   end;
procedure TSynEdit_CaretX_R(Self: TSynEdit; var I: Integer);    begin   I := Self.CaretX;   end;

procedure TSynEdit_CaretY_W(Self: TSynEdit; I: Integer);        begin   Self.CaretY := I;   end;
procedure TSynEdit_CaretY_R(Self: TSynEdit; var I: Integer);    begin   I := Self.CaretY;   end;

procedure TSynEdit_LogCaretXY_W(Self: TSynEdit; P: TPoint);     begin   Self.LogicalCaretXY := P;   end;
procedure TSynEdit_LogCaretXY_R(Self: TSynEdit; var P: TPoint); begin   P := Self.LogicalCaretXY;   end;

procedure TSynEdit_LogCaretX_W(Self: TSynEdit; I: Integer);     begin   Self.LogicalCaretXY := Point(I, Self.CaretY);   end;
procedure TSynEdit_LogCaretX_R(Self: TSynEdit; var I: Integer); begin   I := Self.LogicalCaretXY.X;   end;

    // Selection
procedure TSynEdit_BlockBegin_W(Self: TSynEdit; P: TPoint);     begin   Self.BlockBegin := P;   end;
procedure TSynEdit_BlockBegin_R(Self: TSynEdit; var P: TPoint); begin   P := Self.BlockBegin;   end;

procedure TSynEdit_BlockEnd_W(Self: TSynEdit; P: TPoint);       begin   Self.BlockEnd := P;   end;
procedure TSynEdit_BlockEnd_R(Self: TSynEdit; var P: TPoint);   begin   P := Self.BlockEnd;   end;

procedure TSynEdit_SelAvail_R(Self: TSynEdit; var V: Boolean);  begin   V := Self.SelAvail;   end;

procedure TSynEdit_SelText_W(Self: TSynEdit; S: String);        begin   Self.SelText := S;   end;
procedure TSynEdit_SelText_R(Self: TSynEdit; var S: String);    begin   S := Self.SelText;   end;

procedure TSynEdit_SelMode_W(Self: TSynEdit; M: TSynSelectionMode);     begin   Self.SelectionMode := M;   end;
procedure TSynEdit_SelMode_R(Self: TSynEdit; var M: TSynSelectionMode); begin   M := Self.SelectionMode;   end;

    // Text
procedure TSynEdit_Lines_R(Self: TSynEdit; var S: string; I: Longint);  begin   S := Self.Lines[I];   end;
procedure TSynEdit_LineAtCaret_R(Self: TSynEdit; var S: string);        begin   S := Self.Lines[Self.CaretY-1];   end;

procedure TSynEdit_TextBetweenPoints_W(Self: TSynEdit; M: String; P1, P2: TPoint);
begin   Self.TextBetweenPoints[P1, P2] := M;   end;
procedure TSynEdit_TextBetweenPoints_R(Self: TSynEdit; var M: String; P1, P2: TPoint);
begin   M := Self.TextBetweenPoints[P1, P2];   end;
//procedure TSynEdit_TextBetweenPointsEx_W(Self: TSynEdit; var M: String; P1, P2: TPoint; C: TSynCaretAdjustMode);
//begin   Self.TextBetweenPointsEx[P1, P2, C] := M;   end;

    // Clipboard
procedure TSynEdit_CanPaste_R(Self: TSynEdit; var V: Boolean);  begin   V := Self.CanPaste;   end;

type

  { TEmsSynWrapper }

  TEmsSynWrapper = class(TSynEdit)
    // Methods will be called with an instace of TSynEdit
  public
    procedure EMS_MoveCaretIgnoreEOL(NewCaret: TPoint);
    procedure EMS_MoveLogicalCaretIgnoreEOL(NewLogCaret: TPoint);

    procedure EMS_ClearSelection;
    procedure EMS_SelectAll;
    procedure EMS_SelectToBrace;
    procedure EMS_SelectWord;
    procedure EMS_SelectLine(WithLeadSpaces: Boolean = True);
    procedure EMS_SelectParagraph;

    function EMS_SearchReplace(ASearch, AReplace: string;
      AOptions: TSynSearchOptions): integer;
    function EMS_SearchReplaceEx(ASearch, AReplace: string;
      AOptions: TSynSearchOptions; AStart: TPoint): integer;

    procedure EMS_InsertTextAtCaret(aText: String; aCaretMode : TSynCaretAdjustMode = scamEnd);
    procedure EMS_SetTextBetweenPoints(aStartPoint, aEndPoint: TPoint;
                                   AValue: String;
                                   aFlags: TSynEditTextFlags = [];
                                   aCaretMode: TSynCaretAdjustMode = scamIgnore;
                                   aMarksMode: TSynMarksAdjustMode = smaMoveUp;
                                   aSelectionMode: TSynSelectionMode = smNormal
                                  );

    procedure EMS_CopyToClipboard;
    procedure EMS_CutToClipboard;
    procedure EMS_PasteFromClipboard;

    function EMS_LogicalToPhysicalPos(p: TPoint): {$IFDEF NeedTPointFix}TPoint2{$ELSE}TPoint{$ENDIF};
    function EMS_LogicalToPhysicalCol(Line: String; Index, LogicalPos: integer): integer;
    function EMS_PhysicalToLogicalPos(p: TPoint): {$IFDEF NeedTPointFix}TPoint2{$ELSE}TPoint{$ENDIF};
    function EMS_PhysicalToLogicalCol(Line: string; Index, PhysicalPos: integer): integer;
    function EMS_PhysicalLineLength(Line: String; Index: integer): integer;
  end;

{ TEmsSynWrapper }

procedure TEmsSynWrapper.EMS_MoveCaretIgnoreEOL(NewCaret: TPoint);
begin
  MoveCaretIgnoreEOL(NewCaret);
end;
procedure TEmsSynWrapper.EMS_MoveLogicalCaretIgnoreEOL(NewLogCaret: TPoint);
begin
  MoveLogicalCaretIgnoreEOL(NewLogCaret);
end;

procedure TEmsSynWrapper.EMS_ClearSelection;  begin   ClearSelection;   end;
procedure TEmsSynWrapper.EMS_SelectAll;       begin   SelectAll;   end;
procedure TEmsSynWrapper.EMS_SelectToBrace;   begin   SelectToBrace;   end;
procedure TEmsSynWrapper.EMS_SelectWord;      begin   SelectWord;   end;
procedure TEmsSynWrapper.EMS_SelectLine(WithLeadSpaces: Boolean);
begin
  SelectLine(WithLeadSpaces);
end;
procedure TEmsSynWrapper.EMS_SelectParagraph; begin   SelectParagraph;   end;

function TEmsSynWrapper.EMS_SearchReplace(ASearch, AReplace: string;
  AOptions: TSynSearchOptions): integer;
begin
  Result := SearchReplace(ASearch, AReplace, AOptions);
end;
function TEmsSynWrapper.EMS_SearchReplaceEx(ASearch, AReplace: string;
  AOptions: TSynSearchOptions; AStart: TPoint): integer;
begin
  Result := SearchReplaceEx(ASearch, AReplace, AOptions, AStart);
end;

procedure TEmsSynWrapper.EMS_InsertTextAtCaret(aText: String; aCaretMode: TSynCaretAdjustMode);
begin
  InsertTextAtCaret(aText, aCaretMode);
end;

procedure TEmsSynWrapper.EMS_SetTextBetweenPoints(aStartPoint, aEndPoint: TPoint;
  AValue: String; aFlags: TSynEditTextFlags; aCaretMode: TSynCaretAdjustMode;
  aMarksMode: TSynMarksAdjustMode; aSelectionMode: TSynSelectionMode);
begin
  SetTextBetweenPoints(aStartPoint, aEndPoint, AValue, aFlags, aCaretMode, aMarksMode,
    aSelectionMode);
end;

procedure TEmsSynWrapper.EMS_CopyToClipboard;    begin   CopyToClipboard;   end;
procedure TEmsSynWrapper.EMS_CutToClipboard;     begin   CutToClipboard;   end;
procedure TEmsSynWrapper.EMS_PasteFromClipboard; begin   PasteFromClipboard;   end;

function TEmsSynWrapper.EMS_LogicalToPhysicalPos(p: TPoint): {$IFDEF NeedTPointFix}TPoint2{$ELSE}TPoint{$ENDIF};
{$IFDEF NeedTPointFix}var r: TPoint;{$ENDIF}
begin
  {$IFDEF NeedTPointFix}
  r := LogicalToPhysicalPos(p);
  Result.x := r.x;
  Result.y := r.y;
  {$ELSE}
  Result := LogicalToPhysicalPos(p);
  {$ENDIF}
end;
function TEmsSynWrapper.EMS_LogicalToPhysicalCol(Line: String; Index,
  LogicalPos: integer): integer;
begin
  Result := LogicalToPhysicalCol(Line, Index, LogicalPos);
end;
function TEmsSynWrapper.EMS_PhysicalToLogicalPos(p: TPoint): {$IFDEF NeedTPointFix}TPoint2{$ELSE}TPoint{$ENDIF};
{$IFDEF NeedTPointFix}var r: TPoint;{$ENDIF}
begin
  {$IFDEF NeedTPointFix}
  r:= PhysicalToLogicalPos(p);
  Result.x := r.x;
  Result.y := r.y;
  {$ELSE}
  Result := PhysicalToLogicalPos(p);
  {$ENDIF}
end;
function TEmsSynWrapper.EMS_PhysicalToLogicalCol(Line: string; Index,
  PhysicalPos: integer): integer;
begin
  Result := PhysicalToLogicalCol(Line, Index, PhysicalPos);
end;
function TEmsSynWrapper.EMS_PhysicalLineLength(Line: String; Index: integer): integer;
begin
  Result := PhysicalLineLength(Line, Index);
end;

  {%endregion}

procedure CompRegisterTSynEdit(AComp: TPSPascalCompiler);
begin
  AComp.AddTypeS('TSynSelectionMode', '(smNormal, smLine, smColumn, smCurrent)');
  AComp.AddTypeS('TSynSearchOption',
              '(ssoMatchCase, ssoWholeWord, ssoBackwards, ssoEntireScope, ' +
              'ssoSelectedOnly, ssoReplace, ssoReplaceAll, ssoPrompt, ' +
              'ssoSearchInReplacement, ssoRegExpr, ssoRegExprMultiLine, ssoFindContinue)'
  );
  AComp.AddTypeS('TSynSearchOptions', 'set of TSynSearchOption');
  AComp.AddTypeS('TSynCaretAdjustMode', '(scamIgnore, scamAdjust, scamForceAdjust, scamEnd, scamBegin)');
  AComp.AddTypeS('TSynEditTextFlag', '(setSelect);');
  AComp.AddTypeS('TSynEditTextFlags', 'set of TSynEditTextFlag;');
  AComp.AddTypeS('TSynMarksAdjustMode', '(smaMoveUp, smaKeep);');

  with AComp.AddClassN(nil, 'TSynEdit') do
  begin
    // Caret
    RegisterProperty('CaretXY', 'TPoint', iptRW);
    RegisterProperty('CaretX',  'Integer', iptRW);
    RegisterProperty('CaretY',  'Integer', iptRW);
    RegisterProperty('LogicalCaretXY', 'TPoint', iptRW);
    RegisterProperty('LogicalCaretX',  'Integer', iptRW);
    RegisterMethod('procedure MoveCaretIgnoreEOL(NewCaret: TPoint);');
    RegisterMethod('procedure MoveLogicalCaretIgnoreEOL(NewLogCaret: TPoint);');

    // Selection
    RegisterProperty('BlockBegin', 'TPoint', iptRW);
    RegisterProperty('BlockEnd',   'TPoint', iptRW);
    RegisterProperty('SelAvail',   'Boolean', iptR);
    RegisterProperty('SelText',    'string', iptRW);
    RegisterProperty('SelectionMode', 'TSynSelectionMode', iptRW);
    RegisterMethod('procedure ClearSelection;');
    RegisterMethod('procedure SelectAll;');
    RegisterMethod('procedure SelectToBrace;');
    RegisterMethod('procedure SelectWord;');
    RegisterMethod('procedure SelectLine(WithLeadSpaces: Boolean);');  //  = True
    RegisterMethod('procedure SelectParagraph;');

    // Search
    RegisterMethod('function SearchReplace(ASearch, AReplace: string; AOptions: TSynSearchOptions): integer;');
    RegisterMethod('function SearchReplaceEx(ASearch, AReplace: string; AOptions: TSynSearchOptions; AStart: TPoint): integer;');

    // Text
    RegisterProperty('Lines', 'String Integer', iptR);
    RegisterProperty('LineAtCaret', 'String', iptR); // LineText
    RegisterMethod('procedure InsertTextAtCaret(aText: String; aCaretMode : TSynCaretAdjustMode);'); //  = scamEnd
    RegisterProperty('TextBetweenPoints', 'String TPoint TPoint', iptRW);
    //RegisterProperty('TextBetweenPointsEx', 'String TPoint TPoint TSynCaretAdjustMode', iptW);
    RegisterMethod('procedure SetTextBetweenPoints(aStartPoint, aEndPoint: TPoint; ' +
                   'AValue: String; aFlags: TSynEditTextFlags; ' + // = []
                   'aCaretMode: TSynCaretAdjustMode; ' + //  = scamIgnore
                   'aMarksMode: TSynMarksAdjustMode; ' + //  = smaMoveUp
                   'aSelectionMode: TSynSelectionMode);'); //  = smNormal

    // Clipboard
    RegisterMethod('procedure CopyToClipboard;');
    RegisterMethod('procedure CutToClipboard;');
    RegisterMethod('procedure PasteFromClipboard;');
    RegisterProperty('CanPaste', 'Boolean', iptR);

    // Logical / Physical
    RegisterMethod('function LogicalToPhysicalPos(p: TPoint): TPoint;');
    RegisterMethod('function LogicalToPhysicalCol(Line: String; Index, LogicalPos : integer): integer;');
    RegisterMethod('function PhysicalToLogicalPos(p: TPoint): TPoint;');
    RegisterMethod('function PhysicalToLogicalCol(Line: string; Index, PhysicalPos: integer): integer;');
    RegisterMethod('function PhysicalLineLength(Line: String; Index: integer): integer;');

  end;
end;

procedure ExecRegisterTSynEdit(AExec: TEMSTPSExec);
begin
  with AExec.FCLassImp.Add(TSynEdit) do
  begin
    {$IFnDEF PasMacroNativeCalls}
    RegisterPropertyNameHelper('CARETXY', @ExecBasicHandler, Pointer(PropReadId_CaretXY), nil, Pointer(PropWriteId_CaretXY), nil);
    RegisterPropertyNameHelper('CARETX', @ExecBasicHandler, Pointer(PropReadId_CaretX), nil, Pointer(PropWriteId_CaretX), nil);
    RegisterPropertyNameHelper('CARETY', @ExecBasicHandler, Pointer(PropReadId_CaretY), nil, Pointer(PropWriteId_CaretY), nil);
    RegisterPropertyNameHelper('LOGICALCARETXY', @ExecBasicHandler, Pointer(PropReadId_LogicalCaretXY), nil, Pointer(PropWriteId_LogicalCaretXY), nil);
    RegisterPropertyNameHelper('LOGICALCARETX', @ExecBasicHandler, Pointer(PropReadId_LogicalCaretX), nil, Pointer(PropWriteId_LogicalCaretX), nil);
    RegisterMethodName('MOVECARETIGNOREEOL', @ExecBasicHandler, Pointer(FunctionId_MoveCaretIgnoreEOL), nil);
    RegisterMethodName('MOVELOGICALCARETIGNOREEOL', @ExecBasicHandler, Pointer(FunctionId_MoveLogicalCaretIgnoreEOL), nil);

    // Selection
    RegisterPropertyNameHelper('BLOCKBEGIN', @ExecBasicHandler, Pointer(PropReadId_BlockBegin), nil, Pointer(PropWriteId_BlockBegin), nil);
    RegisterPropertyNameHelper('BLOCKEND', @ExecBasicHandler, Pointer(PropReadId_BlockEnd), nil, Pointer(PropWriteId_BlockEnd), nil);
    RegisterPropertyNameHelper('SELAVAIL', @ExecBasicHandler, Pointer(PropReadId_SelAvail), nil, Pointer(PropWriteId_SelAvail), nil);
    RegisterPropertyNameHelper('SELTEXT', @ExecBasicHandler, Pointer(PropReadId_SelText), nil, Pointer(PropWriteId_SelText), nil);
    RegisterPropertyNameHelper('SELECTIONMODE', @ExecBasicHandler, Pointer(PropReadId_SelectionMode), nil, Pointer(PropWriteId_SelectionMode), nil);
    RegisterMethodName('CLEARSELECTION', @ExecBasicHandler, Pointer(FunctionId_ClearSelection), nil);
    RegisterMethodName('SELECTALL', @ExecBasicHandler, Pointer(FunctionId_SelectAll), nil);
    RegisterMethodName('SELECTTOBRACE', @ExecBasicHandler, Pointer(FunctionId_SelectToBrace), nil);
    RegisterMethodName('SELECTWORD', @ExecBasicHandler, Pointer(FunctionId_SelectWord), nil);
    RegisterMethodName('SELECTLINE', @ExecBasicHandler, Pointer(FunctionId_SelectLine), nil);
    RegisterMethodName('SELECTPARAGRAPH', @ExecBasicHandler, Pointer(FunctionId_SelectParagraph), nil);

//    // Search
    RegisterMethodName('SEARCHREPLACE', @ExecBasicHandler, Pointer(FunctionId_SearchReplace), nil);
    RegisterMethodName('SEARCHREPLACEEX', @ExecBasicHandler, Pointer(FunctionId_SearchReplaceEx), nil);

    RegisterPropertyNameHelper('LINES', @ExecBasicHandler, Pointer(PropReadId_Lines), nil, Pointer(PropWriteId_Lines), nil);
    RegisterPropertyNameHelper('LINEATCARET', @ExecBasicHandler, Pointer(PropReadId_LineAtCaret), nil, Pointer(PropWriteId_LineAtCaret), nil);
    RegisterMethodName('INSERTTEXTATCARET', @ExecBasicHandler, Pointer(FunctionId_InsertTextAtCaret), nil);

    RegisterPropertyNameHelper('TEXTBETWEENPOINTS', @ExecBasicHandler, Pointer(PropReadId_TextBetweenPoints), nil, Pointer(PropWriteId_TextBetweenPoints), nil);
    //RegisterPropertyNameHelper('TEXTBETWEENPOINTSEX', @ExecBasicHandler, Pointer(PropReadId_TextBetweenPointsEx), nil, Pointer(PropWriteId_TextBetweenPointsEx), nil);
    RegisterMethodName('SETTEXTBETWEENPOINTS', @ExecBasicHandler, Pointer(FunctionId_SetTextBetweenPoints), nil);

    // Clipboard
    RegisterMethodName('COPYTOCLIPBOARD', @ExecBasicHandler, Pointer(FunctionId_CopyToClipboard), nil);
    RegisterMethodName('CUTTOCLIPBOARD', @ExecBasicHandler, Pointer(FunctionId_CutToClipboard), nil);
    RegisterMethodName('PASTEFROMCLIPBOARD', @ExecBasicHandler, Pointer(FunctionId_PasteFromClipboard), nil);
    RegisterPropertyNameHelper('CANPASTE', @ExecBasicHandler, Pointer(PropReadId_CanPaste), nil, Pointer(PropWriteId_CanPaste), nil);

    // Logical / Physical
    RegisterMethodName('LOGICALTOPHYSICALPOS', @ExecBasicHandler, Pointer(FunctionId_LogicalToPhysicalPos), nil);
    RegisterMethodName('LOGICALTOPHYSICALCOL', @ExecBasicHandler, Pointer(FunctionId_LogicalToPhysicalCol), nil);
    RegisterMethodName('PHYSICALTOLOGICALPOS', @ExecBasicHandler, Pointer(FunctionId_PhysicalToLogicalPos), nil);
    RegisterMethodName('PHYSICALTOLOGICALCOL', @ExecBasicHandler, Pointer(FunctionId_PhysicalToLogicalCol), nil);
    RegisterMethodName('PHYSICALLINELENGTH', @ExecBasicHandler, Pointer(FunctionId_PhysicalLineLength), nil);

    {$ELSE}
    // Caret
    RegisterPropertyHelper(@TSynEdit_CaretXY_R, @TSynEdit_CaretXY_W, 'CARETXY');
    RegisterPropertyHelper(@TSynEdit_CaretX_R,  @TSynEdit_CaretX_W,  'CARETX');
    RegisterPropertyHelper(@TSynEdit_CaretY_R,  @TSynEdit_CaretY_W,  'CARETY');
    RegisterPropertyHelper(@TSynEdit_LogCaretXY_R, @TSynEdit_LogCaretXY_W, 'LOGICALCARETXY');
    RegisterPropertyHelper(@TSynEdit_LogCaretX_R,  @TSynEdit_LogCaretX_W,  'LOGICALCARETX');
    RegisterMethod(@TEmsSynWrapper.EMS_MoveCaretIgnoreEOL,        'MOVECARETIGNOREEOL');
    RegisterMethod(@TEmsSynWrapper.EMS_MoveLogicalCaretIgnoreEOL, 'MOVELOGICALCARETIGNOREEOL');

    // Selection
    RegisterPropertyHelper(@TSynEdit_BlockBegin_R, @TSynEdit_BlockBegin_W, 'BLOCKBEGIN');
    RegisterPropertyHelper(@TSynEdit_BlockEnd_R,   @TSynEdit_BlockEnd_W,   'BLOCKEND');
    RegisterPropertyHelper(@TSynEdit_SelAvail_R,   nil,                    'SELAVAIL');
    RegisterPropertyHelper(@TSynEdit_SelText_R,    @TSynEdit_SelText_W,    'SELTEXT');
    RegisterPropertyHelper(@TSynEdit_SelMode_R, @TSynEdit_SelMode_W, 'SELECTIONMODE');
    RegisterMethod(@TEmsSynWrapper.EMS_ClearSelection, 'CLEARSELECTION');
    RegisterMethod(@TEmsSynWrapper.EMS_SelectAll, 'SELECTALL');
    RegisterMethod(@TEmsSynWrapper.EMS_SelectToBrace, 'SELECTTOBRACE');
    RegisterMethod(@TEmsSynWrapper.EMS_SelectWord, 'SELECTWORD');
    RegisterMethod(@TEmsSynWrapper.EMS_SelectLine, 'SELECTLINE');
    RegisterMethod(@TEmsSynWrapper.EMS_SelectParagraph, 'SELECTPARAGRAPH');

    // Search
    RegisterMethod(@TEmsSynWrapper.EMS_SearchReplace, 'SEARCHREPLACE');
    RegisterMethod(@TEmsSynWrapper.EMS_SearchReplaceEx, 'SEARCHREPLACEEX');

    RegisterPropertyHelper(@TSynEdit_Lines_R, nil, 'LINES');
    RegisterPropertyHelper(@TSynEdit_LineAtCaret_R, nil, 'LINEATCARET');
    RegisterMethod(@TEmsSynWrapper.EMS_InsertTextAtCaret, 'INSERTTEXTATCARET');

    RegisterPropertyHelper(@TSynEdit_TextBetweenPoints_R, @TSynEdit_TextBetweenPoints_W, 'TEXTBETWEENPOINTS');
    //RegisterPropertyHelper(nil, @TSynEdit_TextBetweenPointsEx_W, 'TEXTBETWEENPOINTSEX');
    RegisterMethod(@TEmsSynWrapper.EMS_SetTextBetweenPoints, 'SETTEXTBETWEENPOINTS');

    // Clipboard
    RegisterMethod(@TEmsSynWrapper.EMS_CopyToClipboard, 'COPYTOCLIPBOARD');
    RegisterMethod(@TEmsSynWrapper.EMS_CutToClipboard, 'CUTTOCLIPBOARD');
    RegisterMethod(@TEmsSynWrapper.EMS_PasteFromClipboard, 'PASTEFROMCLIPBOARD');
    RegisterPropertyHelper(@TSynEdit_CanPaste_R, nil, 'CANPASTE');

    // Logical / Physical
    RegisterMethod(@TEmsSynWrapper.EMS_LogicalToPhysicalPos, 'LOGICALTOPHYSICALPOS');
    RegisterMethod(@TEmsSynWrapper.EMS_LogicalToPhysicalCol, 'LOGICALTOPHYSICALCOL');
    RegisterMethod(@TEmsSynWrapper.EMS_PhysicalToLogicalPos, 'PHYSICALTOLOGICALPOS');
    RegisterMethod(@TEmsSynWrapper.EMS_PhysicalToLogicalCol, 'PHYSICALTOLOGICALCOL');
    RegisterMethod(@TEmsSynWrapper.EMS_PhysicalLineLength, 'PHYSICALLINELENGTH');
    {$ENDIF}
  end;
end;

{%endregion RegisterTSynEdit}

{%region RegisterTClipboard}

function HandleGetClipboard({%H-}Caller: TPSExec; {%H-}p: TPSExternalProcRec; {%H-}Global, Stack: TPSStack): Boolean;
//var
//  e: TPSExec;
begin
  //e := TPSExec(p.Ext1);
  Stack.SetClass(-1, Clipboard);
  Result :=  True;
end;

procedure TClipboard_AsText_W({%H-}Self: TClipboard; S: String);
begin   Clipboard.AsText := S;   end;
procedure TClipboard_AsText_R({%H-}Self: TClipboard; var S: String);
begin   S := Clipboard.AsText;   end;

procedure CompRegisterTClipboard(AComp: TPSPascalCompiler);
begin
  with AComp.AddClassN(nil, 'TClipboard') do
  begin
    RegisterProperty('AsText', 'String', iptRW);
  end;

  AComp.AddFunction('function Clipboard: TClipboard;');
end;

procedure ExecRegisterTClipboard(AExec: TEMSTPSExec);
begin
  with AExec.FCLassImp.Add(TClipboard) do
  begin
    RegisterPropertyHelper(@TClipboard_AsText_R, @TClipboard_AsText_W, 'ASTEXT');
  end;

  AExec.RegisterFunctionName('CLIPBOARD', @HandleGetClipboard, AExec, nil);
end;

{%endregion RegisterTClipboard}

end.
