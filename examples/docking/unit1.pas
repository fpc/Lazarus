unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Menus, ExtCtrls, Types, LDockTree, HelpIntfs, LazHelpHTML;

type

  { TForm1 }

  TForm1 = class(TForm)
    HTMLBrowserHelpViewer1: THTMLBrowserHelpViewer;
    HTMLHelpDatabase1: THTMLHelpDatabase;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    procedure FormDockOver(Sender: TObject; Source: TDragDockObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
  private
    FIndex: Integer;
    function NextIndex: String;
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.MenuItem1Click(Sender: TObject);
var
  F: TForm;
  Idx: String;
begin
  F := TForm.Create(Application);
  with F do
  begin
    DragKind := dkDock;
    DragMode := dmAutomatic;
    Idx := NextIndex;
    Caption := 'New Form ' + Idx;
    Name := 'DockForm' + Idx;
    with TPanel.Create(F) do
    begin
      Parent := F;
      Align := alClient;
      Caption := F.Caption;
    end;
    Show;
  end;
end;

procedure TForm1.MenuItem2Click(Sender: TObject);
var
  FileName, err: String;
begin
  FileName := ExtractFilePath(ParamStr(0))+'1.html';
  TDockTree(DockManager).DumpLayout(FileName);
  // how to open it in a cross-platform way?
  err := '';
  if HelpIntfs.ShowHelp('file:///' + FileName, 'Dock Layout', 'text/html', err) <> shrSuccess then
    ShowMessage(err);
end;

procedure TForm1.FormDockOver(Sender: TObject; Source: TDragDockObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);

  function RectToScreen(R: TRect): TRect;
  begin
    Result.TopLeft := ClientToScreen(R.TopLeft);
    Result.BottomRight := ClientToScreen(R.BottomRight);
  end;

var
  ChopWidth, ChopHeight: Integer;
  Pt: TPoint;
  R: TRect;
begin
  Accept := True;
  ChopWidth := ClientWidth div 3;
  ChopHeight := ClientHeight div 3;
  Pt := Point(X, Y);
  R := Rect(0, 0, ChopWidth, ClientHeight);
  if PtInRect(R, Pt) then
  begin
    Source.DockRect := RectToScreen(R);
    Exit;
  end;
  R := Rect(0, 0, ClientWidth, ChopHeight);
  if PtInRect(R, Pt) then
  begin
    Source.DockRect := RectToScreen(R);
    Exit;
  end;
  R := Rect(ClientWidth - ChopWidth, 0, ClientWidth, ClientHeight);
  if PtInRect(R, Pt) then
  begin
    Source.DockRect := RectToScreen(R);
    Exit;
  end;
  R := Rect(0, ClientHeight - ChopHeight, ClientWidth, ClientHeight);
  if PtInRect(R, Pt) then
  begin
    Source.DockRect := RectToScreen(R);
    Exit;
  end;
  R := Rect(0, 0, ClientWidth, ClientHeight);
  Source.DockRect := RectToScreen(R);
end;

function TForm1.NextIndex: String;
begin
  Result := IntToStr(FIndex);
  inc(FIndex);
end;

initialization
  {$I unit1.lrs}

end.

