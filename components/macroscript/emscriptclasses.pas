unit EMScriptClasses;
{
  Classes that can be accessed from Scripts
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SynEdit, SynEditTypes, Clipbrd, uPSCompiler, uPSRuntime;

procedure CompRegisterTSynEdit(AComp: TPSPascalCompiler);
procedure ExecRegisterTSynEdit(cl: TPSRuntimeClassImporter);

procedure CompRegisterTClipboard(AComp: TPSPascalCompiler);
procedure ExecRegisterTClipboard(cl: TPSRuntimeClassImporter; AExec: TPSExec);

implementation

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
begin   Self.LogicalCaretXY := Point(I, Self.CaretY);   end;
procedure TSynEdit_LogicalCaretX_R(Self: TSynEdit; var I: Integer);
begin   I := Self.LogicalCaretXY.X;   end;


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


procedure TSynEdit_Lines_R(Self: TSynEdit; var S: string; I: Longint);
begin   S := Self.Lines[I];   end;

procedure TSynEdit_LineAtCaret_R(Self: TSynEdit; var S: string);
begin
  S := Self.Lines[Self.CaretY-1];
end;

procedure CompRegisterTSynEdit(AComp: TPSPascalCompiler);
begin
  AComp.AddTypeS('TPoint', 'record x,y: Longint; end;');
  AComp.AddTypeS('TSynSelectionMode', '(smNormal, smLine, smColumn, smCurrent)');
  AComp.AddTypeS('TSynSearchOption',
              '(ssoMatchCase, ssoWholeWord, ssoBackwards, ssoEntireScope, ' +
              'ssoSelectedOnly, ssoReplace, ssoReplaceAll, ssoPrompt, ' +
              'ssoSearchInReplacement, ssoRegExpr, ssoRegExprMultiLine, ssoFindContinue)'
  );
  AComp.AddTypeS('TSynSearchOptions', 'set of TSynSearchOption');
  AComp.AddTypeS('TSynCaretAdjustMode', '(scamIgnore, scamAdjust, scamEnd, scamBegin)');

  with AComp.AddClassN(nil, 'TSynEdit') do
  begin
    RegisterProperty('CaretXY', 'TPoint', iptRW);
    RegisterProperty('CaretX',  'Integer', iptRW);
    RegisterProperty('CaretY',  'Integer', iptRW);
    RegisterProperty('LogicalCaretXY', 'TPoint', iptRW);
    RegisterProperty('LogicalCaretX',  'Integer', iptRW);
    RegisterMethod('procedure MoveCaretIgnoreEOL(const NewCaret: TPoint);');
    RegisterMethod('procedure MoveLogicalCaretIgnoreEOL(const NewLogCaret: TPoint);');

    RegisterProperty('BlockBegin', 'TPoint', iptRW);
    RegisterProperty('BlockEnd',   'TPoint', iptRW);
    RegisterProperty('SelAvail',   'Boolean', iptR);
    RegisterProperty('SelText',    'string', iptRW);
    RegisterProperty('SelectionMode', 'TSynSelectionMode', iptRW);

    RegisterProperty('Lines', 'String Integer', iptR);
    RegisterProperty('LineAtCaret', 'String', iptR); // LineText

    RegisterMethod('function SearchReplace(const ASearch, AReplace: string; AOptions: TSynSearchOptions): integer;');
    RegisterMethod('function SearchReplaceEx(const ASearch, AReplace: string; AOptions: TSynSearchOptions; AStart: TPoint): integer;');

    RegisterMethod('procedure InsertTextAtCaret(aText: String; aCaretMode : TSynCaretAdjustMode);'); //  = scamEnd
  end;
end;

procedure ExecRegisterTSynEdit(cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TSynEdit) do
  begin

    RegisterPropertyHelper(@TSynEdit_CaretXY_R, @TSynEdit_CaretXY_W, 'CARETXY');
    RegisterPropertyHelper(@TSynEdit_CaretX_R,  @TSynEdit_CaretX_W,  'CARETX');
    RegisterPropertyHelper(@TSynEdit_CaretY_R,  @TSynEdit_CaretY_W,  'CARETY');
    RegisterPropertyHelper(@TSynEdit_LogicalCaretXY_R, @TSynEdit_LogicalCaretXY_W, 'LOGICALCARETXY');
    RegisterPropertyHelper(@TSynEdit_LogicalCaretX_R,  @TSynEdit_LogicalCaretX_W,  'LOGICALCARETX');
    RegisterMethod(@TSynEdit.MoveCaretIgnoreEOL,        'MOVECARETIGNOREEOL');
    RegisterMethod(@TSynEdit.MoveLogicalCaretIgnoreEOL, 'MOVELOGICALCARETIGNOREEOL');

    RegisterPropertyHelper(@TSynEdit_BlockBegin_R, @TSynEdit_BlockBegin_W, 'BLOCKBEGIN');
    RegisterPropertyHelper(@TSynEdit_BlockEnd_R,   @TSynEdit_BlockEnd_W,   'BLOCKEND');
    RegisterPropertyHelper(@TSynEdit_SelAvail_R,   nil,                    'SELAVAIL');
    RegisterPropertyHelper(@TSynEdit_SelText_R,    @TSynEdit_SelText_W,    'SELTEXT');
    RegisterPropertyHelper(@TSynEdit_SelectionMode_R, @TSynEdit_SelectionMode_W, 'SELECTIONMODE');

    RegisterPropertyHelper(@TSynEdit_Lines_R, nil, 'LINES');
    RegisterPropertyHelper(@TSynEdit_LineAtCaret_R, nil, 'LINEATCARET');

    RegisterMethod(@TSynEdit.SearchReplace, 'SEARCHREPLACE');
    RegisterMethod(@TSynEdit.SearchReplaceEx, 'SEARCHREPLACEEX');

    RegisterMethod(@TSynEdit.InsertTextAtCaret, 'INSERTTEXTATCARET');

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

