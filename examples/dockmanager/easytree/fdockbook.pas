unit fDockBook;
(* Notebook for docking multiple controls into a tabbed control.
  By DoDi <DrDiettrich1@aol.com> 2009.

  A tab is created for every docked control.
  The currently visible tab remains down.
  A control can be undocked by dragging the associated tab.
    This makes the tabs act as grab regions, for undocking forms.
*)

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
    function GetControlTab(AControl: TControl): TTabButton;
  end;

var
  EasyDockBook: TEasyDockBook;

implementation

{ TEasyDockBook }

function TEasyDockBook.GetControlTab(AControl: TControl): TTabButton;
var
  i: integer;
  btn: TToolButton absolute Result;
begin
  for i := 0 to Tabs.ButtonCount - 1 do begin
    btn := Tabs.Buttons[i];
    if Result.Control = AControl then
      exit;
  end;
//not found - raise exception?
  Result := nil;
end;

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
  btn.OnClick := @ToolButton1Click;
  btn.Down := True;
  btn.Click;
end;

procedure TEasyDockBook.pnlDockUnDock(Sender: TObject; Client: TControl;
  NewTarget: TWinControl; var Allow: Boolean);
var
  i: integer;
  btn: TTabButton;
begin
(* Client undocked, remove associated tab.
   We'll have to find the tab, associated with the control.
*)
  Allow := true;
  //assert(CurTab.Control = Client, 'diff client');
  btn := GetControlTab(Client);
  //i := CurTab.Index;
  i := btn.Index;
  if btn = CurTab then begin
    CurTab := nil;
  end else begin
    Client.Visible := True; //make hidden page control visible
  end;
  Tabs.ButtonList.Delete(i);
  btn.Free; //seems to work
//special handle remove of current and last tab
  if Tabs.ButtonCount > 0 then begin
  //tab moved?
    if CurTab = nil then begin //current button removed
    //find next tab to show
      if i >= Tabs.ButtonCount then
        i := Pred(Tabs.ButtonCount);  //  dec(i);
    //activate new tab
      CurTab := Tabs.Buttons[i] as TTabButton;
      CurTab.Down := True;
      CurTab.Click;
    end;
  end else begin
  //last tab removed
    if HostDockSite <> nil then
      ManualDock(nil);  //undock before closing
    Close;
  end;
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
var
  i, last: integer;
begin
  inherited Create(TheOwner);
//these properties must be set before Parent
  Style := tbsCheck;
  AutoSize := True;
  Parent := TWinControl(TheOwner);
//these properties must be set after Parent
  Grouped := True;
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

