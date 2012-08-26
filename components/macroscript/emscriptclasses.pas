unit EMScriptClasses;
{
  Classes that can be accessed from Scripts
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SynEdit, SynEditTypes, uPSCompiler, uPSRuntime;

procedure CompRegisterTSynEdit(Cl: TPSPascalCompiler);
procedure ExecRegisterTSynEdit(cl: TPSRuntimeClassImporter);


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


procedure CompRegisterTSynEdit(Cl: TPSPascalCompiler);
begin
  cl.AddTypeS('TPoint', 'record x,y: Longint; end;');
  CL.AddTypeS('TSynSelectionMode', '(smNormal, smLine, smColumn, smCurrent)');
  CL.AddTypeS('TSynSearchOption',
              '(ssoMatchCase, ssoWholeWord, ssoBackwards, ssoEntireScope, ' +
              'ssoSelectedOnly, ssoReplace, ssoReplaceAll, ssoPrompt, ' +
              'ssoSearchInReplacement, ssoRegExpr, ssoRegExprMultiLine, ssoFindContinue)'
  );
  CL.AddTypeS('TSynSearchOptions', 'set of TSynSearchOption');


  with Cl.AddClassN(cl.FindClass('TOBJECT'), 'TSynEdit') do
  begin
    RegisterProperty('CaretXY', 'TPoint', iptRW);
    RegisterProperty('CaretX',  'Integer', iptRW);
    RegisterProperty('CaretY',  'Integer', iptRW);
    RegisterProperty('LogicalCaretXY', 'TPoint', iptRW);
    RegisterProperty('LogicalCaretX',  'Integer', iptRW);

    RegisterProperty('BlockBegin', 'TPoint', iptRW);
    RegisterProperty('BlockEnd',   'TPoint', iptRW);
    RegisterProperty('SelAvail',   'Boolean', iptR);
    RegisterProperty('SelText',    'string', iptRW);
    RegisterProperty('SelectionMode', 'TSynSelectionMode', iptRW);

    RegisterProperty('Lines', 'String Integer', iptR);
    RegisterProperty('LineAtCaret', 'String', iptR); // LineText

    RegisterMethod('function SearchReplace(const ASearch, AReplace: string; AOptions: TSynSearchOptions): integer;');
    RegisterMethod('function SearchReplaceEx(const ASearch, AReplace: string; AOptions: TSynSearchOptions; AStart: TPoint): integer;');

  end;
end;

procedure ExecRegisterTSynEdit(cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TSynEdit) do
  begin
    //RegisterConstructor(@TSynEdit.CREATE, 'CREATE');

    RegisterPropertyHelper(@TSynEdit_CaretXY_R, @TSynEdit_CaretXY_W, 'CARETXY');
    RegisterPropertyHelper(@TSynEdit_CaretX_R,  @TSynEdit_CaretX_W,  'CARETX');
    RegisterPropertyHelper(@TSynEdit_CaretY_R,  @TSynEdit_CaretY_W,  'CARETY');
    RegisterPropertyHelper(@TSynEdit_LogicalCaretXY_R, @TSynEdit_LogicalCaretXY_W, 'LOGICALCARETXY');
    RegisterPropertyHelper(@TSynEdit_LogicalCaretX_R,  @TSynEdit_LogicalCaretX_W,  'LOGICALCARETX');

    RegisterPropertyHelper(@TSynEdit_BlockBegin_R, @TSynEdit_BlockBegin_W, 'BLOCKBEGIN');
    RegisterPropertyHelper(@TSynEdit_BlockEnd_R,   @TSynEdit_BlockEnd_W,   'BLOCKEND');
    RegisterPropertyHelper(@TSynEdit_SelAvail_R,   nil,                    'SELAVAIL');
    RegisterPropertyHelper(@TSynEdit_SelText_R,    @TSynEdit_SelText_W,    'SELTEXT');
    RegisterPropertyHelper(@TSynEdit_SelectionMode_R, @TSynEdit_SelectionMode_W, 'SELECTIONMODE');

    RegisterPropertyHelper(@TSynEdit_Lines_R, nil, 'LINES');
    RegisterPropertyHelper(@TSynEdit_LineAtCaret_R, nil, 'LINEATCARET');

    RegisterMethod(@TSynEdit.SearchReplace, 'SEARCHREPLACE');
    RegisterMethod(@TSynEdit.SearchReplaceEx, 'SEARCHREPLACEEX');
  end;
end;


end.

