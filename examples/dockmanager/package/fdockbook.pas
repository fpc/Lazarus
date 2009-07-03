unit fDockBook;
(* Notebook for docking multiple controls into a tabbed control.
  By DoDi <DrDiettrich1@aol.com> 2009.

Example of a dock site without an docking manager.
Unmanaged docking requires:
- OnGetSiteInfo handler, at least indicating acceptance
- OnDockOver handler, indicating acceptance, optionally placing the DockRect
- OnDockDrop handler, when the control is not docked as an immediate client
  of the dock site (here: dedicated panel becomes the Parent).

In this (notebook) implementation:
  The form is a dock site that manages docked clients itself.
  Controls are docked into a dedicated panel, i.e. the panel becomes their Parent.
  A tab is created for every docked control, in a dedicated toolbar.
  The tab, associated with the currently visible page, is in down state.
  A control can be undocked by dragging the associated tab.
    This makes the tabs act as grab regions for undocking e.g. forms or other
    controls, which otherwise deny undocking from their client area.
  The entire notebook can be docked from the empty part of the toolbar.
    Again for use with widgetsets and controls that do not drag properly.

TWinControls are not really draggable, unless they have parts that do not
normally react on mouse buttons (borders...). E.g. a SynEdit has to be wrapped
into a form, before it can be dragged and docked by dragging the form.

Apply ToolButtonAutoSizeAlign.patch to improve the appearance and behaviour
of the toolbar buttons.
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

  TTabs = class(TToolBar)
  protected
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(TheOwner: TComponent); override;
  end;

  TEasyDockBook = class(TForm)
    pnlDock: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDockDrop(Sender: TObject; Source: TDragDockObject;
      X, Y: Integer);
    procedure FormDockOver(Sender: TObject; Source: TDragDockObject;
      X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure FormGetSiteInfo(Sender: TObject; DockClient: TControl;
      var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
    procedure FormUnDock(Sender: TObject; Client: TControl;
      NewTarget: TWinControl; var Allow: Boolean);
    procedure ToolButton1Click(Sender: TObject);
  private
    Tabs: TTabs;
    CurTab: TTabButton;
  protected
    function GetDefaultDockCaption: string; override;
    function GetControlTab(AControl: TControl): TTabButton;
  end;

//procedure Register;

implementation

uses
  LCLProc; //debug only

procedure Register;
begin
  //RegisterComponents('Common Controls', [TEasyDockBook]);
end;

{ TEasyDockBook }

procedure TEasyDockBook.FormCreate(Sender: TObject);
begin
  Tabs := TTabs.Create(self);
  Visible := True;
end;

procedure TEasyDockBook.FormDockDrop(Sender: TObject; Source: TDragDockObject;
  X, Y: Integer);
var
  btn: TTabButton;
begin
  //Source.DragTarget := pnlDock; ?
  Source.Control.Parent := pnlDock; //overwrite DoAddDockClient behaviour

  btn := TTabButton.Create(Tabs);
  btn.Control := Source.Control;
  btn.Control.Align := alClient;
  btn.Caption := GetDockCaption(btn.Control);
  btn.OnClick := @ToolButton1Click;
  btn.Down := True;
  btn.Visible := True;
  btn.Click;
end;

procedure TEasyDockBook.FormDockOver(Sender: TObject; Source: TDragDockObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
//unmanaged dock site requires an OnDockOver handler.
  Accept := True; //this is the default, can be omitted
//make DockRect reflect the docking area
  with Source do begin
    DockRect := pnlDock.ClientRect;
    DockRect.TopLeft := pnlDock.ClientToScreen(DockRect.TopLeft);
    inc(DockRect.Bottom, DockRect.Top);
    inc(DockRect.Right, DockRect.Left);
  end;
end;

procedure TEasyDockBook.FormGetSiteInfo(Sender: TObject; DockClient: TControl;
  var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
begin
//override TCustomForm behaviour!
  CanDock := True;
end;

procedure TEasyDockBook.FormUnDock(Sender: TObject; Client: TControl;
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
//these properties must be set before Parent
  Style := tbsCheck; //allow button to stay down
  AutoSize := True; //depending on the Caption text width
  Parent := TWinControl(TheOwner); //seems to be required
//these properties must be set after Parent
  Grouped := True;
end;

procedure TTabButton.MouseMove(Shift: TShiftState; X, Y: Integer);
//var AControl: TControl;
begin
(* Implement dragging of the associated page.
*)
  inherited MouseMove(Shift, X, Y);
  if (ssLeft in Shift) and not DragManager.IsDragging then begin
    if Control <> nil then begin
{ Problems with TWinControls - must be wrapped into forms :-(
      //DebugLn('---undock "', Control.GetDefaultDockCaption, '"');
      DebugLn('---undock "', Control.ClassName, '"');
      if False and (Control is TWinControl) then begin
        AControl := Control; //will change when undocked?
        AControl.ManualDock(nil);
        if AControl.HostDockSite <> nil then
          AControl.HostDockSite.BeginDrag(True);
      end else
}
//both immediate and delayed drag start seem to work
        Control.BeginDrag(False); //delayed docking
        //Control.BeginDrag(True); //immediate drag
    end;
  end;
end;

{ TTabs }

constructor TTabs.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Align := alTop;
  AutoSize := True;
  Color := clBtnFace;
  Flat := False;
  Height := 28; //?
  List := True;
  ParentColor := False;
  ParentFont := False; //which one?
  Font.Style := [fsBold];
  ShowCaptions := True;
  Wrapable := True;
  Visible := True;
  Parent := TWinControl(TheOwner);
end;

procedure TTabs.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
(* Implement dragging of the entire notebook.
  Parent is assumed to be the notebook form.
*)
  inherited MouseMove(Shift, X, Y);
  if ssLeft in Shift then
    Parent.BeginDrag(False); //delayed docking of the container form
end;

initialization
  {$I fdockbook.lrs}

end.

