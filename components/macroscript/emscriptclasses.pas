unit EMScriptClasses;
{
  Classes that can be accessed from Scripts
}

{$mode objfpc}{$H+}
{$WARN 4055 off : Conversion between ordinals and pointers is not portable} // PtrInt is ok
interface

{$IFDEF darwin}
  {$DEFINE NeedTPointFix }
{$ENDIF}

uses
  Classes, SysUtils, SynEdit, SynEditTypes, SynEditKeyCmds, LazLoggerBase, IDECommands,
  Clipbrd, Dialogs, Controls, uPSCompiler, uPSRuntime, uPSUtils, uPSDebugger, uPSR_std,
  uPSC_std;

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

{$IFDEF PasMacroNoNativeCalls}
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

{$IFDEF PasMacroNoNativeCalls}
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
var
  res: PPSVariant;
  data: Pointer;
  temp: TPSVariantIFC;
  s: String;
  typerec: TPSTypeRec;
begin
  Result := True;
  case Longint(p.Ext1) of
    0: begin // POINT()
        if Stack.Count < 3 then raise TEMScriptBadParamException.Create('Invalid param count for "Point"');;
        data := GetVarPointFromStack(Stack, -1);
        TPoint(data^) := Point(Stack.GetInt(-2), Stack.GetInt(-3));
      end;
    50: begin // MessageDlg(Msg: string; DlgType :TMsgDlgType; Buttons :TMsgDlgButtons; HelpCtx: Longint): Integer';
        if Stack.Count < 5 then raise TEMScriptBadParamException.Create('Invalid param count for "MessageDlg"');;
        Stack.SetInt(-1,
          MessageDlg(Stack.GetAnsiString(-2), TMsgDlgType(Stack.GetUInt(-3)),
            TMsgDlgButtons(GetSetFromStack(Stack, -4)), Stack.GetInt(-5))
        );
      end;
    51: begin // MessageDlgPos(Msg: string; DlgType :TMsgDlgType; Buttons :TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer): Integer
        if Stack.Count < 7 then raise TEMScriptBadParamException.Create('Invalid param count for "MessageDlgPos"');;
        Stack.SetInt(-1,
          MessageDlgPos(Stack.GetAnsiString(-2), TMsgDlgType(Stack.GetUInt(-3)),
            TMsgDlgButtons(GetSetFromStack(Stack, -4)), Stack.GetInt(-5),
            Stack.GetInt(-6), Stack.GetInt(-7) )
        );
      end;
    52: begin // MessageDlgPosHelp(Msg: string; DlgType :TMsgDlgType; Buttons :TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer; HelpFileName: string): Integer
        if Stack.Count < 8 then raise TEMScriptBadParamException.Create('Invalid param count for "MessageDlgPosHelp"');;
        Stack.SetInt(-1,
          MessageDlgPosHelp(Stack.GetAnsiString(-2), TMsgDlgType(Stack.GetUInt(-3)),
            TMsgDlgButtons(GetSetFromStack(Stack, -4)), Stack.GetInt(-5),
            Stack.GetInt(-6), Stack.GetInt(-7), Stack.GetAnsiString(-8))
        );
      end;
    53: begin // ShowMessage(Msg: string)
        if Stack.Count < 1 then raise TEMScriptBadParamException.Create('Invalid param count for "ShowMessage"');;
        ShowMessage(Stack.GetAnsiString(-1));
      end;
    54: begin // ShowMessagePos(Msg: string; X, Y :Integer)
        if Stack.Count < 3 then raise TEMScriptBadParamException.Create('Invalid param count for "ShowMessagePos"');;
        ShowMessagePos(Stack.GetAnsiString(-1), Stack.GetInt(-2), Stack.GetInt(-3));
      end;
    55: begin // InputBox(ACaption, APrompt, ADefault: string): string
        if Stack.Count < 4 then raise TEMScriptBadParamException.Create('Invalid param count for "InputBox"');;
        Stack.SetAnsiString(-1,
          InputBox(Stack.GetAnsiString(-2), Stack.GetAnsiString(-3), Stack.GetAnsiString(-4))
        );
      end;
    56: begin // InputQuery(ACaption, APrompt: string; var Value: string): Boolean
        if Stack.Count < 4 then raise TEMScriptBadParamException.Create('Invalid param count for "InputQuery"');
        temp := NewTPSVariantIFC(Stack[Stack.Count-4], True);
        if (temp.aType.BaseType <> btString) then raise TEMScriptBadParamException.Create('Invalid param type for "InputQuery"');
        if (temp.Dta = nil) then raise TEMScriptBadParamException.Create('Invalid param data for "InputQuery"');
        s := tbtstring(temp.Dta^);
        Stack.SetBool(-1,
          InputQuery(Stack.GetAnsiString(-2), Stack.GetAnsiString(-3), s)
        );
        tbtstring(temp.Dta^) := s;
      end;
    else
      Result := False;
  end;
end;
{$ENDIF}

procedure ExecRegisterBasics(AExec: TEMSTPSExec);
begin
  {$IFnDEF PasMacroNoNativeCalls}
  AExec.RegisterDelphiFunction(FuncPoint, 'POINT', cdRegister);

  AExec.RegisterDelphiFunction(FuncMessageDlg, 'MessageDlg', cdRegister);
  AExec.RegisterDelphiFunction(FuncMessageDlgPos, 'MessageDlgPos', cdRegister);
  AExec.RegisterDelphiFunction(FuncMessageDlgPosHelp, 'MessageDlgPosHelp', cdRegister);
  AExec.RegisterDelphiFunction(FuncShowMessage, 'ShowMessage', cdRegister);
  AExec.RegisterDelphiFunction(FuncShowMessagePos, 'ShowMessagePos', cdRegister);
  AExec.RegisterDelphiFunction(FuncInputBox, 'InputBox', cdRegister);
  AExec.RegisterDelphiFunction(FuncInputQuery, 'InputQuery', cdRegister);
  {$ELSE}
  AExec.RegisterFunctionName('POINT',             @ExecBasicHandler, Pointer(0), nil);

  AExec.RegisterFunctionName('MessageDlg',        @ExecBasicHandler, Pointer(50), nil);
  AExec.RegisterFunctionName('MessageDlgPos',     @ExecBasicHandler, Pointer(51), nil);
  AExec.RegisterFunctionName('MessageDlgPosHelp', @ExecBasicHandler, Pointer(52), nil);
  AExec.RegisterFunctionName('ShowMessage',       @ExecBasicHandler, Pointer(53), nil);
  AExec.RegisterFunctionName('ShowMessagePos',    @ExecBasicHandler, Pointer(54), nil);
  AExec.RegisterFunctionName('InputBox',          @ExecBasicHandler, Pointer(55), nil);
  AExec.RegisterFunctionName('InputQuery',        @ExecBasicHandler, Pointer(56), nil);
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
  AComp.AddTypeS('TSynCaretAdjustMode', '(scamIgnore, scamAdjust, scamEnd, scamBegin)');
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
