unit EMScriptClasses;
{
  Classes that can be accessed from Scripts
}

{$mode objfpc}{$H+}

interface

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

{$IFDEF darwin}
type
  TPoint2 = record x,y,a,b,c: Longint; end;
{$ENDIF}

function Point(AX, AY: Integer): {$IFDEF darwin}TPoint2{$ELSE}TPoint{$ENDIF};
begin
  with Result do
  begin
    X := AX;
    Y := AY;
  end;
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
  DeclMessageDlg        = 'Function MessageDlg(const Msg: string; DlgType :TMsgDlgType; Buttons :TMsgDlgButtons; HelpCtx: Longint): Integer';
  DeclMessageDlgPos     = 'Function MessageDlgPos(const Msg: string; DlgType :TMsgDlgType; Buttons :TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer): Integer';
  DeclMessageDlgPosHelp = 'Function MessageDlgPosHelp(const Msg: string; DlgType :TMsgDlgType; Buttons :TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer; const HelpFileName: string): Integer';
  DeclShowMessage       = 'Procedure ShowMessage(const Msg: string)';
  DeclShowMessagePos    = 'Procedure ShowMessagePos(const Msg: string; X, Y :Integer)';
  DeclInputBox          = 'Function InputBox(const ACaption, APrompt, ADefault: string): string';
  DeclInputQuery        = 'Function InputQuery(const ACaption, APrompt: string; var Value: string): Boolean';

  FuncMessageDlg:        function(const Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer = @MessageDlg;
  FuncMessageDlgPos:     function(const Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer): Integer = @MessageDlgPos;
  FuncMessageDlgPosHelp: function(const Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer; const HelpFileName: string): Integer = @MessageDlgPosHelp;
  FuncShowMessage:       procedure(const Msg: string) = @ShowMessage;
  FuncShowMessagePos:    procedure(const Msg: string; X, Y: Integer) = @ShowMessagePos;
  FuncInputBox:          function(const ACaption, APrompt, ADefault: string): string = @InputBox;
  FuncInputQuery:        function(const ACaption, APrompt: string; var Value : string): Boolean = @InputQuery;

  DeclPoint = 'function Point(AX, AY: Integer): TPoint;';
  FuncPoint: function(AX, AY: Integer): {$IFDEF darwin}TPoint2{$ELSE}TPoint{$ENDIF} = @Point; // @Classes.Point;

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

    // Caret
procedure TSynEdit_CaretXY_W(Self: TSynEdit; const P: TPoint);
begin   Self.CaretXY := P;   end;
procedure TSynEdit_CaretXY_R(Self: TSynEdit; var P: TPoint);
begin   P := Self.CaretXY;   end;

procedure TSynEdit_CaretX_W(Self: TSynEdit; const I: Integer);
begin   Self.CaretX := I;   end;
procedure TSynEdit_CaretX_R(Self: TSynEdit; var I: Integer);
begin   I := Self.CaretX;   end;

procedure TSynEdit_CaretY_W(Self: TSynEdit; const I: Integer);
begin   Self.CaretY := I;   end;
procedure TSynEdit_CaretY_R(Self: TSynEdit; var I: Integer);
begin   I := Self.CaretY;   end;

procedure TSynEdit_LogicalCaretXY_W(Self: TSynEdit; const P: TPoint);
begin   Self.LogicalCaretXY := P;   end;
procedure TSynEdit_LogicalCaretXY_R(Self: TSynEdit; var P: TPoint);
begin   P := Self.LogicalCaretXY;   end;

procedure TSynEdit_LogicalCaretX_W(Self: TSynEdit; const I: Integer);
begin   Self.LogicalCaretXY := Classes.Point(I, Self.CaretY);   end;
procedure TSynEdit_LogicalCaretX_R(Self: TSynEdit; var I: Integer);
begin   I := Self.LogicalCaretXY.X;   end;


    // Selection
procedure TSynEdit_BlockBegin_W(Self: TSynEdit; const P: TPoint);
begin   Self.BlockBegin := P;   end;
procedure TSynEdit_BlockBegin_R(Self: TSynEdit; var P: TPoint);
begin   P := Self.BlockBegin;   end;

procedure TSynEdit_BlockEnd_W(Self: TSynEdit; const P: TPoint);
begin   Self.BlockEnd := P;   end;
procedure TSynEdit_BlockEnd_R(Self: TSynEdit; var P: TPoint);
begin   P := Self.BlockEnd;   end;

procedure TSynEdit_SelAvail_R(Self: TSynEdit; var V: Boolean);
begin   V := Self.SelAvail;   end;

procedure TSynEdit_SelText_W(Self: TSynEdit; const S: String);
begin   Self.SelText := S;   end;
procedure TSynEdit_SelText_R(Self: TSynEdit; var S: String);
begin   S := Self.SelText;   end;

procedure TSynEdit_SelectionMode_W(Self: TSynEdit; const M: TSynSelectionMode);
begin   Self.SelectionMode := M;   end;
procedure TSynEdit_SelectionMode_R(Self: TSynEdit; var M: TSynSelectionMode);
begin   M := Self.SelectionMode;   end;


    // Text
procedure TSynEdit_Lines_R(Self: TSynEdit; var S: string; I: Longint);
begin   S := Self.Lines[I];   end;

procedure TSynEdit_LineAtCaret_R(Self: TSynEdit; var S: string);
begin
  S := Self.Lines[Self.CaretY-1];
end;

procedure TSynEdit_TextBetweenPoints_W(Self: TSynEdit; const M: String; const P1, P2: TPoint);
begin   Self.TextBetweenPoints[P1, P2] := M;   end;
procedure TSynEdit_TextBetweenPoints_R(Self: TSynEdit; var M: String; const P1, P2: TPoint);
begin   M := Self.TextBetweenPoints[P1, P2];   end;

//procedure TSynEdit_TextBetweenPointsEx_W(Self: TSynEdit; var M: String; const P1, P2: TPoint; const C: TSynCaretAdjustMode);
//begin   Self.TextBetweenPointsEx[P1, P2, C] := M;   end;

    // Clipboard
procedure TSynEdit_CanPaste_R(Self: TSynEdit; var V: Boolean);
begin   V := Self.CanPaste;   end;


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
    RegisterMethod('procedure MoveCaretIgnoreEOL(const NewCaret: TPoint);');
    RegisterMethod('procedure MoveLogicalCaretIgnoreEOL(const NewLogCaret: TPoint);');

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
    RegisterMethod('function SearchReplace(const ASearch, AReplace: string; AOptions: TSynSearchOptions): integer;');
    RegisterMethod('function SearchReplaceEx(const ASearch, AReplace: string; AOptions: TSynSearchOptions; AStart: TPoint): integer;');

    // Text
    RegisterProperty('Lines', 'String Integer', iptR);
    RegisterProperty('LineAtCaret', 'String', iptR); // LineText
    RegisterMethod('procedure InsertTextAtCaret(aText: String; aCaretMode : TSynCaretAdjustMode);'); //  = scamEnd
    RegisterProperty('TextBetweenPoints', 'String TPoint TPoint', iptRW);
    //RegisterProperty('TextBetweenPointsEx', 'String TPoint TPoint TSynCaretAdjustMode', iptW);
    RegisterMethod('procedure SetTextBetweenPoints(aStartPoint, aEndPoint: TPoint; ' +
                   'const AValue: String; aFlags: TSynEditTextFlags; ' + // = []
                   'aCaretMode: TSynCaretAdjustMode; ' + //  = scamIgnore
                   'aMarksMode: TSynMarksAdjustMode; ' + //  = smaMoveUp
                   'aSelectionMode: TSynSelectionMode);'); //  = smNormal

    // Clipboard
    RegisterMethod('procedure CopyToClipboard;');
    RegisterMethod('procedure CutToClipboard;');
    RegisterMethod('procedure PasteFromClipboard;');
    RegisterProperty('CanPaste', 'Boolean', iptR);

    // Logical / Physical
    RegisterMethod('function LogicalToPhysicalPos(const p: TPoint): TPoint;');
    RegisterMethod('function LogicalToPhysicalCol(const Line: String; Index, LogicalPos : integer): integer;');
    RegisterMethod('function PhysicalToLogicalPos(const p: TPoint): TPoint;');
    RegisterMethod('function PhysicalToLogicalCol(const Line: string; Index, PhysicalPos: integer): integer;');
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
    RegisterPropertyHelper(@TSynEdit_LogicalCaretXY_R, @TSynEdit_LogicalCaretXY_W, 'LOGICALCARETXY');
    RegisterPropertyHelper(@TSynEdit_LogicalCaretX_R,  @TSynEdit_LogicalCaretX_W,  'LOGICALCARETX');
    RegisterMethod(@TSynEdit.MoveCaretIgnoreEOL,        'MOVECARETIGNOREEOL');
    RegisterMethod(@TSynEdit.MoveLogicalCaretIgnoreEOL, 'MOVELOGICALCARETIGNOREEOL');

    // Selection
    RegisterPropertyHelper(@TSynEdit_BlockBegin_R, @TSynEdit_BlockBegin_W, 'BLOCKBEGIN');
    RegisterPropertyHelper(@TSynEdit_BlockEnd_R,   @TSynEdit_BlockEnd_W,   'BLOCKEND');
    RegisterPropertyHelper(@TSynEdit_SelAvail_R,   nil,                    'SELAVAIL');
    RegisterPropertyHelper(@TSynEdit_SelText_R,    @TSynEdit_SelText_W,    'SELTEXT');
    RegisterPropertyHelper(@TSynEdit_SelectionMode_R, @TSynEdit_SelectionMode_W, 'SELECTIONMODE');
    RegisterMethod(@TSynEdit.ClearSelection, 'CLEARSELECTION');
    RegisterMethod(@TSynEdit.SelectAll, 'SELECTALL');
    RegisterMethod(@TSynEdit.SelectToBrace, 'SELECTTOBRACE');
    RegisterMethod(@TSynEdit.SelectWord, 'SELECTWORD');
    RegisterMethod(@TSynEdit.SelectLine, 'SELECTLINE');
    RegisterMethod(@TSynEdit.SelectParagraph, 'SELECTPARAGRAPH');

    // Search
    RegisterMethod(@TSynEdit.SearchReplace, 'SEARCHREPLACE');
    RegisterMethod(@TSynEdit.SearchReplaceEx, 'SEARCHREPLACEEX');

    RegisterPropertyHelper(@TSynEdit_Lines_R, nil, 'LINES');
    RegisterPropertyHelper(@TSynEdit_LineAtCaret_R, nil, 'LINEATCARET');
    RegisterMethod(@TSynEdit.InsertTextAtCaret, 'INSERTTEXTATCARET');

    RegisterPropertyHelper(@TSynEdit_TextBetweenPoints_R, @TSynEdit_TextBetweenPoints_W, 'TEXTBETWEENPOINTS');
    //RegisterPropertyHelper(nil, @TSynEdit_TextBetweenPointsEx_W, 'TEXTBETWEENPOINTSEX');
    RegisterMethod(@TSynEdit.SetTextBetweenPoints, 'SETTEXTBETWEENPOINTS');

    // Clipboard
    RegisterMethod(@TSynEdit.CopyToClipboard, 'COPYTOCLIPBOARD');
    RegisterMethod(@TSynEdit.CutToClipboard, 'CUTTOCLIPBOARD');
    RegisterMethod(@TSynEdit.PasteFromClipboard, 'PASTEFROMCLIPBOARD');
    RegisterPropertyHelper(@TSynEdit_CanPaste_R, nil, 'CANPASTE');

    // Logical / Physical
    RegisterMethod(@TSynEdit.LogicalToPhysicalPos, 'LOGICALTOPHYSICALPOS');
    RegisterMethod(@TSynEdit.LogicalToPhysicalCol, 'LOGICALTOPHYSICALCOL');
    RegisterMethod(@TSynEdit.PhysicalToLogicalPos, 'PHYSICALTOLOGICALPOS');
    RegisterMethod(@TSynEdit.PhysicalToLogicalCol, 'PHYSICALTOLOGICALCOL');
    RegisterMethod(@TSynEdit.PhysicalLineLength, 'PHYSICALLINELENGTH');
  end;
end;

(*   ClipBoard   *)

function HandleGetClipboard(Caller: TPSExec; p: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;
var
  e: TPSExec;
begin
  e := TPSExec(p.Ext1);
  Stack.SetClass(-1, Clipboard);
end;

procedure TClipboard_AsText_W(Self: TClipboard; const S: String);
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

