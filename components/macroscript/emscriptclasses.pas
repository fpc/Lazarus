unit EMScriptClasses;
{
  Classes that can be accessed from Scripts
}

{$mode objfpc}{$H+}

interface

{$IFDEF darwin}
  {$DEFINE NeedTPointFix }
{$ENDIF}

uses
  Classes, SysUtils, SynEdit, SynEditTypes, Clipbrd, Dialogs, Controls, uPSCompiler,
  uPSRuntime, uPSUtils;

procedure CompRegisterBasics(AComp: TPSPascalCompiler);
procedure ExecRegisterBasics(AExec: TPSExec);

procedure CompRegisterTSynEdit(AComp: TPSPascalCompiler);
procedure ExecRegisterTSynEdit(cl: TPSRuntimeClassImporter);

procedure CompRegisterTClipboard(AComp: TPSPascalCompiler);
procedure ExecRegisterTClipboard(cl: TPSRuntimeClassImporter; AExec: TPSExec);

implementation

{$IFDEF NeedTPointFix}
type TPoint2 = record x,y,a,b,c: Longint; end;
{$ENDIF}

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

function test_ord_mt(AType: TMsgDlgType): Integer;
begin
  Result := ord(AType);
end;

function test_ord_mb(ABtn: TMsgDlgBtn): Integer;
begin
  Result := ord(ABtn);
end;

const
  DeclMessageDlg        = 'Function MessageDlg(Msg: string; DlgType :TMsgDlgType; Buttons :TMsgDlgButtons; HelpCtx: Longint): Integer';                                                                     //
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

  Decltest_ord_mt = 'function test_ord_mt(AType: TMsgDlgType): Integer;';
  Decltest_ord_mb = 'function test_ord_mb(ABtn: TMsgDlgBtn): Integer;';
  Functest_ord_mt: function(AType: TMsgDlgType): Integer = @test_ord_mt;
  Functest_ord_mb: function(ABtn: TMsgDlgBtn): Integer = @test_ord_mb;

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

  // for tests
  AComp.AddDelphiFunction(Decltest_ord_mb);
  AComp.AddDelphiFunction(Decltest_ord_mt);
end;

procedure ExecRegisterBasics(AExec: TPSExec);
begin
  AExec.RegisterDelphiFunction(FuncPoint, 'POINT', cdRegister);

 AExec.RegisterDelphiFunction(FuncMessageDlg, 'MessageDlg', cdRegister);
 AExec.RegisterDelphiFunction(FuncMessageDlgPos, 'MessageDlgPos', cdRegister);
 AExec.RegisterDelphiFunction(FuncMessageDlgPosHelp, 'MessageDlgPosHelp', cdRegister);
 AExec.RegisterDelphiFunction(FuncShowMessage, 'ShowMessage', cdRegister);
 AExec.RegisterDelphiFunction(FuncShowMessagePos, 'ShowMessagePos', cdRegister);
 AExec.RegisterDelphiFunction(FuncInputBox, 'InputBox', cdRegister);
 AExec.RegisterDelphiFunction(FuncInputQuery, 'InputQuery', cdRegister);

  // for tests
  AExec.RegisterDelphiFunction(Functest_ord_mb, 'test_ord_mb', cdRegister);
  AExec.RegisterDelphiFunction(Functest_ord_mt, 'test_ord_mt', cdRegister);
end;

{   SynEdit   }
{%region SynEdit}


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

procedure ExecRegisterTSynEdit(cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TSynEdit) do
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

(*   ClipBoard   *)

function HandleGetClipboard(Caller: TPSExec; p: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;
var
  e: TPSExec;
begin
  e := TPSExec(p.Ext1);
  Stack.SetClass(-1, Clipboard);
  Result :=  True;
end;

procedure TClipboard_AsText_W(Self: TClipboard; S: String);
begin   Clipboard.AsText := S;   end;
procedure TClipboard_AsText_R(Self: TClipboard; var S: String);
begin   S := Clipboard.AsText;   end;

procedure CompRegisterTClipboard(AComp: TPSPascalCompiler);
begin
  with AComp.AddClassN(nil, 'TClipboard') do
  begin
    RegisterProperty('AsText', 'String', iptRW);
  end;

  AComp.AddFunction('function Clipboard: TClipboard;');
end;

procedure ExecRegisterTClipboard(cl: TPSRuntimeClassImporter; AExec: TPSExec);
begin
  with Cl.Add(TClipboard) do
  begin
    RegisterPropertyHelper(@TClipboard_AsText_R, @TClipboard_AsText_W, 'ASTEXT');
  end;

  AExec.RegisterFunctionName('CLIPBOARD', @HandleGetClipboard, AExec, nil);
end;


end.

