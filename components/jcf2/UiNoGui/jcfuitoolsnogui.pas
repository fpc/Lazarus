unit JcfUiToolsNoGUI;

{$mode ObjFPC}

interface

uses
  Classes, SysUtils, System.UITypes, ParseTreeNode,JcfUiTools;

type

TJcfUINoGUI=class(TJcfUIBase)
public
  procedure UpdateGUI(aCounter:integer=0;aUpdateInterval:integer=512);override;
  function MessageDlgUI(const aMessage:string):TModalResult;override;
  procedure ShowErrorMessageUI(const aMessage:string);override;
  procedure SetWaitCursorUI;override;
  procedure RestoreCursorUI;override;
  procedure ShowParseTreeUI(const pcRoot: TParseTreeNode);override;
  function  OpenDocumentUI(const aPath:string):boolean;override;
end;

implementation

uses
  crt;

procedure TJcfUINoGUI.UpdateGUI(aCounter:integer=0;aUpdateInterval:integer=512);
begin
end;

function TJcfUINoGUI.MessageDlgUI(const aMessage:string):TModalResult;
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

procedure TJcfUINoGUI.ShowErrorMessageUI(const aMessage:string);
begin
  writeln(aMessage);
end;

procedure TJcfUINoGUI.SetWaitCursorUI;
begin
end;

procedure TJcfUINoGUI.RestoreCursorUI;
begin
end;

procedure TJcfUINoGUI.ShowParseTreeUI(const pcRoot: TParseTreeNode);
begin
end;

function TJcfUINoGUI.OpenDocumentUI(const aPath:string):boolean;
begin
  result:=false;
end;

end.

