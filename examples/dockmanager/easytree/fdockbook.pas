unit fDockBook;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, ExtCtrls;

type
  TTabButton = class(TToolButton)
  protected
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(TheOwner: TComponent); override;
    Control: TControl;
  end;

  TEasyDockBook = class(TForm)
    pnlDock: TPanel;
    Tabs: TToolBar;
    procedure pnlDockDockDrop(Sender: TObject; Source: TDragDockObject;
      X, Y: Integer);
    procedure pnlDockUnDock(Sender: TObject; Client: TControl;
      NewTarget: TWinControl; var Allow: Boolean);
    procedure ToolButton1Click(Sender: TObject);
  private
    CurTab: TTabButton;
  protected
    function GetDefaultDockCaption: string; override;
  end;

var
  EasyDockBook: TEasyDockBook;

implementation

{ TEasyDockBook }

function TEasyDockBook.GetDefaultDockCaption: string;
var
  i: integer;
  pg: TToolButton;
begin
  Result := '';
  for i := 0 to Tabs.ButtonCount - 1 do begin
    pg := Tabs.Buttons[i];
    if Result = '' then
      Result := pg.Caption
    else
      Result := Result + ', ' + pg.Caption;
  end;
end;

procedure TEasyDockBook.pnlDockDockDrop(Sender: TObject; Source: TDragDockObject;
  X, Y: Integer);
var
  btn: TTabButton;
begin
  btn := TTabButton.Create(Tabs);
  btn.Control := Source.Control;
  btn.Control.Align := alClient;
  btn.Caption := GetDockCaption(btn.Control);
  //btn.Caption := ' ' + GetDockCaption(btn.Control) + ' ';
  btn.OnClick := @ToolButton1Click;
  btn.Down := True;
  btn.Click;
end;

procedure TEasyDockBook.pnlDockUnDock(Sender: TObject; Client: TControl;
  NewTarget: TWinControl; var Allow: Boolean);
var
  i: integer;
begin
  Allow := true;
  //assert(CurTab.Control = Client, 'diff client');
  i := CurTab.Index;
  Tabs.ButtonList.Delete(i);
  CurTab.Free; //seems to work
  //Tabs.removebutton
  CurTab := nil;
  if i >= Tabs.ButtonCount then
    dec(i);
  if Tabs.ButtonCount > 0 then begin
    CurTab := Tabs.Buttons[i] as TTabButton;
    CurTab.Down := True;
    CurTab.Click;
  end else if HostDockSite <> nil then begin
  //undock before closing
    ManualDock(nil);
    Close;
  end else
    close;
end;

procedure TEasyDockBook.ToolButton1Click(Sender: TObject);
var
  btn: TTabButton absolute Sender;
begin
  if CurTab <> nil then begin
    CurTab.Control.Visible := false;
  end;
  if btn.Control <> nil then
    btn.Control.Visible := True;
  CurTab := btn;
end;

{ TTabButton }

constructor TTabButton.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Parent := TWinControl(TheOwner);
  Grouped := True;
  AllowAllUp := False;
  Style := tbsCheck;
  AutoSize := True;
end;

procedure TTabButton.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if ssLeft in Shift then begin
    if Control <> nil then begin
      Control.BeginDrag(False); //delayed docking
    end;
  end;
end;

initialization
  {$I fdockbook.lrs}

end.

