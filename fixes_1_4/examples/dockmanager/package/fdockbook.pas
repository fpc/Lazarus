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
of the toolbar buttons. (new version will use TToggleBox)

Close docked forms on notebook destruction.
*)

(* Applications
Stand alone form (not recommended)
Parent=Nil, HostDockSite=Nil

Not-docked part of a form (Editor)
The form should never close itself.
Parent<>Nil, HostDockSite=Nil

Part of an dock tree
The form is automatically created and docked by the EasyTree.
It must notify the tree (HostDockSite) when it has 1 client left,
  for replacement by that client.
It also should notify the HostDockSite of any un/dock, to update the caption.
Parent=HostDockSite (<>Nil)

Suggested methods:
HostDockSite.ReplaceDockedControl (self by last client)
HostDockSite.UpdateDockCaption (provide composed dock caption)
*)

{$mode objfpc}{$H+}

{.$DEFINE undockFix}
{$DEFINE closeFix}
{.$DEFINE autoWrap}  //request FloatingDockSiteClass

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, EasyDockSite;

type
  TTabButton = class(TToolButton)
  protected
    function GetDefaultDockCaption: String; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    Control: TControl;
    constructor Create(TheOwner: TComponent); override;
  end;

  TTabs = class(TToolBar)
  protected
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(TheOwner: TComponent); override;
  end;

  { TEasyDockBook }

  TEasyDockBook = class(TCustomDockSite)
    pnlDock: TPanel;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
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
  protected
    Tabs: TTabs;
    CurTab: TTabButton;
  protected
    procedure DoAddDockClient(Client: TControl; const ARect: TRect); override;
    function  GetFloatingDockSiteClass: TWinControlClass; override;
    function  GetDefaultDockCaption: string; override;
    function  GetControlTab(AControl: TControl): TTabButton;
    procedure AfterUndock(tabidx: integer); virtual;
  public
  {$IFDEF undockFix}
    destructor Destroy; override;
  {$ENDIF}
  {$IFDEF closeFix}
    destructor Destroy; override;
  {$ENDIF}
    procedure Clear; virtual;
    procedure ShowForm(AForm: TControl); override;
    procedure ShowPage(ATab: TTabButton);
    procedure LoadFromStream(strm: TStream); override;
    procedure SaveToStream(strm: TStream); override;
  end;

//procedure Register;

implementation

uses
  fFloatingSite,
  LCLProc; //debug only

procedure Register;
begin
  //RegisterComponents('Common Controls', [TEasyDockBook]);
end;

type
  TControlAccess = class(TControl)
  end;

{ TEasyDockBook }

{$IFDEF undockFix}
destructor TEasyDockBook.Destroy;
var
  i: integer;
  ctl: TControl;
begin
(* Problem with undocking?

  The DockClients are not properly undocked when we (HostDockSite) are destroyed :-(

  This code prevents an error when the DockClients are docked later,
    by definitely undocking all clients.

  But then the bug will strike back when the notebook is destroyed at the
    end of the application.
  Fix: check ctl.ComponentState for csDestroying.
*)
  for i := DockClientCount - 1 downto 0 do begin
    ctl := DockClients[i];
    if not (csDestroying in ctl.ComponentState) then
      ctl.ManualDock(nil);
    DebugLn('Undocked %s P=%p H=%p', [ctl.Name,
      pointer(ctl.Parent), pointer(ctl.HostDockSite)]);
  end;
  inherited Destroy;
end;
{$ELSE}
  //LCL updated accordingly?
{$ENDIF}

{$IFDEF closeFix}
destructor TEasyDockBook.Destroy;
begin
(* Close docked forms, make all docked controls visible - or hidden?
*)
  Clear; //allow for override
  inherited Destroy;
end;
{$ELSE}
  //pure option
{$ENDIF}

procedure TEasyDockBook.Clear;
var
  i: integer;
  ctl: TControl;
  frm: TCustomForm absolute ctl;
begin
(* Remove all docked clients.
  By default the clients are made visible (debug feature)
*)
  for i := DockClientCount - 1 downto 0 do begin
    ctl := DockClients[i];
    if not (csDestroying in ctl.ComponentState) then begin
    {$IFDEF old}
      ctl.Visible := True; //make hidden notebook pages visible
      if ctl is TCustomForm then
        if frm.CloseQuery then
          frm.Close;
        //else ctl.Visible := True; //make hidden notebook pages visible
    {$ELSE}
      ctl.Visible := True; //show notebook page
    {$ENDIF}
    end;
  end;
end;

procedure TEasyDockBook.ShowForm(AForm: TControl);
var
  btn: TTabButton;
begin
//activate page of this control
  btn := GetControlTab(AForm);
  if btn = nil then
    inherited ShowForm(AForm)
  else
    ShowPage(btn);
end;

procedure TEasyDockBook.ShowPage(ATab: TTabButton);
begin
(* Show the (clicked) page
*)
  if (ATab = nil) or (ATab = CurTab) then
    exit; //nothing to change

  if CurTab <> nil then
    CurTab.Control.Visible := false;
  if ATab.Control <> nil then
    ATab.Control.Visible := True;
  CurTab := ATab;
end;


procedure TEasyDockBook.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
var
  i: integer;
  ctl: TControl;
  //wc: TWinControl absolute ctl;
  frm: TCustomForm absolute ctl;
begin
(* When an empty notebook is closed, it shall be freed.
  Otherwise the clients must be handled (close forms)
*)
  DebugLn(['TEasyDockBook.FormClose ',DbgSName(Self),' ',dbgs(Pointer(Self))]);
  CloseAction := caFree;
end;

procedure TEasyDockBook.FormCreate(Sender: TObject);
begin
  Tabs := TTabs.Create(self);
//also this?
  DragKind := dkDock;
  DragMode := dmAutomatic;
  Visible := True; //immediately?
end;

procedure TEasyDockBook.DoAddDockClient(Client: TControl; const ARect: TRect);
begin
(* Make the docked client visible in our dedicated panel.
  It seemed to be necessary to separate this from the OnDockDrop handler?
*)
  //if False then inherited DoAddDockClient(Client, ARect);
  Client.Parent := pnlDock;
end;

procedure TEasyDockBook.FormDockDrop(Sender: TObject; Source: TDragDockObject;
  X, Y: Integer);
var
  btn: TTabButton;
begin
  //Source.Control.Parent := pnlDock; //overwrite DoAddDockClient behaviour???

  btn := TTabButton.Create(Tabs);
  btn.Control := Source.Control;
  btn.Control.Align := alClient;
  btn.Control.DockOrientation := doPages;
  btn.Caption := GetDockCaption(btn.Control);
  btn.OnClick := @ToolButton1Click;
  btn.Down := True;
  btn.Visible := True;
  btn.Click;
end;

procedure TEasyDockBook.FormDockOver(Sender: TObject; Source: TDragDockObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  ARect: TRect;
begin
//unmanaged dock site requires an OnDockOver handler.
  Accept := True; //this is the default, can be omitted
//make DockRect reflect the docking area
  Source.DockRect := ScreenRect(pnlDock);
end;

procedure TEasyDockBook.FormGetSiteInfo(Sender: TObject; DockClient: TControl;
  var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
begin
//override TCustomForm behaviour!
  CanDock := True;
  InfluenceRect := ScreenRect(pnlDock); //exclude eventual elastic sites
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
  Application.ReleaseComponent(btn);
//special handle remove of current and last tab
  AfterUndock(i);
end;

procedure TEasyDockBook.AfterUndock(tabidx: integer);
begin
(* A client has undocked, we have various options:
  If 1 client remains, replace ourselves by this client (if docked)
    Opt: hide Tabs.
  If 0 clients remain, free ourselves (if docked or floating)

Finally update Parent (DockCaption if docked, if destroying self)

We can be either:
  docked - HDS<>nil, HDS<>floating site
  floating - HDS=Parent=floating site (which exactly? having no kids except us?)
  child - HDS=nil, Parent<>nil.

*)
{$IFDEF new}
  //if not StayDocked and (Tabs.ButtonCount = 1) then begin
  if False then begin
  //push up last client
    if HostDockSite <> nil then begin
      CurTab := Tabs.Buttons[0] as TTabButton;
     (* Problem: a floating HostDockSite may close itself, in between.
     *)
    {$IFDEF old}
      CurTab.Control.ReplaceDockedControl(self, HostDockSite, nil, alNone);
    {$ELSE}
      CurTab.Control.ManualDock(HostDockSite, self, alLeft);
    {$ENDIF}
      Release;
    end else begin
    end;
  end else
{$ELSE}
  //above code doesn't work :-(
  //retry: explicit replace by last client, undock(?) and release
{$ENDIF}
  if Tabs.ButtonCount > 0 then begin
  //tab moved?
    if CurTab = nil then begin //current button removed
    //find next tab to show
      if tabidx >= Tabs.ButtonCount then
        tabidx := Pred(Tabs.ButtonCount);  //  dec(i);
    //activate new tab
      CurTab := Tabs.Buttons[tabidx] as TTabButton;
      CurTab.Down := True;
      CurTab.Click;
    end;
    Caption := GetDefaultDockCaption;
  end else if not StayDocked then begin
  //last tab removed - close ONLY if we are docked or floating
  //and if we have no other controls (elastic sites!) - detect how?
    if (Parent = nil) and (DragKind = dkDock)
    //and (ControlCount <= 1)
    then begin
    //we are floating
      Release;
      exit;
    end else if (HostDockSite <> nil) then begin //may be cleared already???
      Visible := False;
      ManualDock(nil);  //undock before closing?
      Release;
      exit;
    end;
  end;
//update the host dock site and its DockManager
  if HostDockSite <> nil then begin
    if (HostDockSite is TFloatingSite) then
      TFloatingSite(HostDockSite).UpdateCaption(nil);
    if HostDockSite.DockManager <> nil then
      HostDockSite.Invalidate;
  end else if Parent <> nil then begin
    //notify - how?
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
  btn: TToolButton;
  pg: TTabButton absolute btn;
begin
(* Update button captions?
  If 1 control docked: get full caption?
*)
  Result := '';
  for i := 0 to Tabs.ButtonCount - 1 do begin
    btn := Tabs.Buttons[i];
    if Result <> '' then
      Result := Result + ', ' + pg.GetDefaultDockCaption
    else if tabs.ButtonCount = 1 then
      Result := pg.Control.Caption //full Caption of single client
    else
      Result := pg.GetDefaultDockCaption;
  end;
end;

function TEasyDockBook.GetFloatingDockSiteClass: TWinControlClass;
begin
(* Try: request a floating site, if no OnEndDock handler is installed
*)
  if assigned(OnEndDock) then
    Result := inherited GetFloatingDockSiteClass
  else
    Result:= TFloatingSite;
end;

procedure TEasyDockBook.LoadFromStream(strm: TStream);
var
  i, n: integer;
  ctl: TControl;
  cn: string;
begin
  if False then inherited LoadFromStream(strm);
  n := strm.ReadByte;
  for i := 1 to n do begin
    cn := strm.ReadAnsiString;
    ctl := DockLoader.ReloadControl(cn, self);
    if ctl <> nil then
      ctl.ManualDock(self); //orientation???
    //make visible?
  end;
end;

procedure TEasyDockBook.SaveToStream(strm: TStream);
var
  i, n: integer;
  ctl: TControl;
  s: string;
begin
  if False then inherited SaveToStream(strm);
  n := DockClientCount;
  strm.WriteByte(n);
  for i := 0 to n-1 do begin
    ctl := DockClients[i];
    s := DockLoader.SaveControl(ctl, self);
    strm.WriteAnsiString(s);
  end;
end;

procedure TEasyDockBook.ToolButton1Click(Sender: TObject);
var
  btn: TTabButton absolute Sender;
begin
  ShowPage(btn);
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

function TTabButton.GetDefaultDockCaption: String;
begin
(* Also update button caption, to reflect an eventually changed control caption.
*)
  if Control = nil then
    Result:=inherited GetDefaultDockCaption
  else begin
    Result := TControlAccess(Control).GetDefaultDockCaption;
    Caption := Result;
  end;
end;

procedure TTabButton.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
(* Implement dragging of the associated page.
*)
  inherited MouseMove(Shift, X, Y);
  if (ssLeft in Shift) and not DragManager.IsDragging then begin
    if Control <> nil then
      Control.BeginDrag(True); //immediate drag
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
  Try prevent undocking of NOT docked form.
*)
  inherited MouseMove(Shift, X, Y);
  if (ssLeft in Shift) and (Parent.HostDockSite <> nil) then
    Parent.BeginDrag(False); //delayed docking of the container form
end;

{$R *.lfm}

initialization
  RegisterClass(TEasyDockBook);
end.


