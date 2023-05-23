unit JcfUiToolsGUI;

{$mode ObjFPC}

interface

uses
  Classes, SysUtils, System.UITypes, ParseTreeNode,JcfUiTools;

type

  TJcfUIGUI=class(TJcfUIBase)
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
  Forms, Dialogs, Controls, fShowParseTree,lclintf;

procedure TJcfUIGUI.UpdateGUI(aCounter:integer=0;aUpdateInterval:integer=512);
begin
  if (aCounter mod aUpdateInterval) = 0 then
     Application.ProcessMessages;
end;

function TJcfUIGUI.MessageDlgUI(const aMessage:string):TModalResult;
begin
  result := MessageDlg(aMessage, mtConfirmation, [mbYes, mbNo, mbAll, mbAbort], 0);
end;

procedure TJcfUIGUI.ShowErrorMessageUI(const aMessage:string);
begin
  MessageDlg(aMessage, mtError, [mbOK], 0);
end;

var
  OldCursor:TCursor;

procedure TJcfUIGUI.SetWaitCursorUI;
begin
 OldCursor := Screen.Cursor;
 Screen.Cursor := crHourGlass;
end;

procedure TJcfUIGUI.RestoreCursorUI;
begin
  Screen.Cursor := OldCursor;
end;

procedure TJcfUIGUI.ShowParseTreeUI(const pcRoot: TParseTreeNode);
begin
  fShowParseTree.ShowParseTree(pcRoot);
end;

function TJcfUIGUI.OpenDocumentUI(const aPath:string):boolean;
begin
  result:=OpenDocument(aPath);
end;

end.

