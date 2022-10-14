unit JcfUiTools;

{$mode ObjFPC}

interface

uses
  Classes, SysUtils, System.UITypes, ParseTreeNode;

procedure UpdateGUI(aCounter:integer=0;aUpdateInterval:integer=512);
function MessageDlgUI(const aMessage:string):TModalResult;
procedure ShowErrorMessageUI(const aMessage:string);
procedure SetWaitCursorUI;
procedure RestoreCursorUI;
procedure ShowParseTreeUI(const pcRoot: TParseTreeNode);

implementation

{$ifdef COMMAND_LINE}
uses
  crt;

procedure UpdateGUI(aCounter:integer=0;aUpdateInterval:integer=512);
begin
end;

function MessageDlgUI(const aMessage:string):TModalResult;
const
  MR:array[1..4] of TModalResult = (mrYes, mrNo, mrAbort, mrAll);
var
  C:char;
  I:integer;
begin
  WriteLn(aMessage);
  Write('Yes No Abort aLl <YNAL>? ');
  I:=0;
  repeat
    C:=ReadKey;
    C:=UpCase(C);
    I:=Pos(C,'YNAL');
  until I>0;
  WriteLn(C);
  result:=MR[I];
end;

procedure ShowErrorMessageUI(const aMessage:string);
begin
  writeln(aMessage);
end;


procedure SetWaitCursorUI;
begin
end;

procedure RestoreCursorUI;
begin
end;

procedure ShowParseTreeUI(const pcRoot: TParseTreeNode);
begin
end;


{$else}
uses
  Forms, Dialogs, Controls, fShowParseTree;

procedure UpdateGUI(aCounter:integer=0;aUpdateInterval:integer=512);
begin
  if (aCounter mod aUpdateInterval) = 0 then
     Application.ProcessMessages;
end;

function MessageDlgUI(const aMessage:string):TModalResult;
begin
  result := MessageDlg(aMessage, mtConfirmation, [mbYes, mbNo, mbAll, mbAbort], 0);
end;

procedure ShowErrorMessageUI(const aMessage:string);
begin
  MessageDlg(aMessage, mtError, [mbOK], 0);
end;

var
  OldCursor:TCursor;

procedure SetWaitCursorUI;
begin
 OldCursor := Screen.Cursor;
 Screen.Cursor := crHourGlass;
end;

procedure RestoreCursorUI;
begin
  Screen.Cursor := OldCursor;
end;

procedure ShowParseTreeUI(const pcRoot: TParseTreeNode);
begin
  fShowParseTree.ShowParseTree(pcRoot);
end;

{$endif}

end.

