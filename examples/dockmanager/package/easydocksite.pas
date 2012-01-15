unit EasyDockSite;
(* A tree docking manager by DoDi <DrDiettrich1@aol.com>.

This project can be used instead of the LDockTree manager.

AppLoadStore is a global link to an application handler for saving and reloading
sites and docked controls.

CustomDockSites have their own handler for saving and loading site and clients.


To be added or ported:
  - field and method argument names

Possible extensions:
  - separate docking management and dock site layout
  + various dock headers
  - multiple splitters (on zones without controls)
  + persistence (requires application-wide management of dock sources!)
  - purpose of Restore button?
  - purpose of Close button? (currently: undock)

More issues, concerning the rest of the LCL (mainly unit Controls):

LCL TODO:
=========
  LCL does not handle existing (or added) controls in a dock site. This results
  in interference of docked and "resident" controls, handled by different
  layout/docking managers.
  Delphi moves all controls into the docked clients list, as soon as a TWinControl
  becomes a DockSite.

  LCL does not handle docking managers as interfaces.
  (this will most probably never come)

  LCL undocks a control when it just is clicked. (Delphi flaw)

  LCL controls don't notify the dock manager about visibility changes.

  LCL doesn't handle properly the possible start of a drag operation (WM_LBUTTONDOWN).
  In DragMode=dmAutomatic the drag manager should capture mouse input.
  When an LB_UP occurs while waiting for the threshold, a normal Click should
  occur (perform LB_DOWN and LB_UP).
  Otherwise dragging starts, and the control has to be reset into "no button down"
  state.

  The default floating site doesn't work properly.
  When multiple clients are docked, and one of them should become floating,
  the client is undocked BUT stays in the site.
*)

{$mode objfpc}{$H+}

{$DEFINE ctlType} //save <name>:<classname>=<caption>
{$DEFINE RootDock} //allow docking into the root zone?
{$DEFINE KeepSize}  //preserve relative sizes on dock?
{.$DEFINE freeImages} //free dockheader images on finalization? (can cause exception)
//{$DEFINE newSplitter} //exclude splitter from remaining zone
{.$DEFINE handle_existing} //dock controls existing in the dock site?
{.$DEFINE splitter_color} //use colored splitter, for debugging?
{.$DEFINE visibility} //handling of invisible clients deserves dock manager notification!
{.$DEFINE restore} //restore button?
  //looks useless: how to restore a hidden zone?
{$DEFINE singleTab} //allow to create notebooks with 1 tab (in the topzone)?
  //doesn't look nice, with both a header AND a button list

//depending on widgetset or patched LCL
{.$DEFINE NoDrop} //applied DoDiPatch1?
{.$DEFINE replace} //using ReplaceDockedControl?

interface

uses
  LCLIntf, //TRect
  LCLType, //HDC
  LMessages, //TLMessage
  Classes, //TStream
  Graphics, //TCanvas
  Forms,
  ExtCtrls, //splitter
  Controls,
  ComCtrls; //TPageControl

type
  TEasyTree = class; //forward declaration
  TEasyZone = class; //forward declaration
  TEasySplitter = TCustomSplitter;

  TEasyZonePart =
  (
    zpNowhere,        // not in any zone
    zpClient,         // on client control
    zpAll,            // total header rect
    zpCaption,        // header caption
    zpSizer,          // splitter/sizer
  {$IFDEF restore}
    zpRestoreButton,  // header restore button
  {$ENDIF}
    zpCloseButton     // header close button
  );

  TEasyHeaderStyle = (
    hsMinimal,  //Delphi style
    hsForm,     //form style
    hsNone      //no header (special notebook etc. style)
  );

  TEasyDockHeader = class
  public
  //state last drawn
    MouseZone: TEasyZone;
    MouseDown: boolean;
    MousePart: TEasyZonePart;
    PartRect: TRect;
  public
    constructor Create;
    function  GetRectOfPart(ARect: TRect; AOrientation: TDockOrientation; APart: TEasyZonePart; HasSplitter: boolean; AStyle: TEasyHeaderStyle): TRect; virtual;
    function  FindPart(AZone: TEasyZone; MousePos: TPoint; fButtonDown: boolean): TEasyZonePart;
    procedure Draw(AZone: TEasyZone; ACanvas: TCanvas; ACaption: string; const MousePos: TPoint);
  end;

  TEasyZone = class
  private
    FChildControl: TControl;
    FTree: TEasyTree;
    FFirstChild,
    FNextSibling, FPrevSibling, FParent: TEasyZone;
    FZoneOrientation, //true orientation
    FOrientation: TDockOrientation; //Delphi compatible (doNone when containing a control)
    procedure SetControl(Control: TControl);
    procedure SetOrientation(NewOrientation: TDockOrientation);
    function GetLeft: Integer;
    function GetTop: Integer;
    function GetTopOrLeft(fTop: boolean): Integer;
  private //very basic liniking
    procedure InsertAfter(LinkAfter, NewZone: TEasyZone);
      //do not handle orientation!
    procedure SetParent(zone: TEasyZone);
      //unlink
  protected
    BR: TPoint;
    procedure SetBounds(TLBR: TRect);

    function  GetHandle: HWND;
  {$IFDEF visibility}  //to be redesigned
    function  GetVisible: boolean;
    function  GetVisibleControl: TControl;
  {$ENDIF}
    function  GetPartRect(APart: TEasyZonePart): TRect;
    function  GetStyle: TEasyHeaderStyle;
  public
    constructor Create(ATree: TEasyTree);
    destructor Destroy; override;
    procedure Clear;
    function  DockSite: TWinControl;
    function  HasSizer: boolean;
    function  GetBounds: TRect;

    procedure AddSibling(NewZone: TEasyZone; where: TAlign);
    procedure ReplaceChild(OldChild, NewChild: TEasyZone);
    procedure ScaleTo(ptOld, ptNew, ptOuter: TPoint);
    procedure PositionControl;
  public //properties
    property ChildControl: TControl read FChildControl write SetControl;
    property FirstChild: TEasyZone read FFirstChild;
    property NextSibling: TEasyZone read FNextSibling;
    property PrevSibling: TEasyZone read FPrevSibling;
    property Parent: TEasyZone read FParent write SetParent;
    property Orientation: TDockOrientation read FOrientation write SetOrientation;
    property Style: TEasyHeaderStyle read GetStyle;
    //property Visible: boolean read GetVisible;

    property Bottom: integer read BR.Y;
    property Left: integer read GetLeft;
    property Right: integer read BR.X;
    property Top: integer read GetTop;
  end;

(* TEasyDockManager implements some of the abstract methods of TDockManager.
*)
  TEasyDockManager = class(TDockManager)
  protected
    FDockSite: TWinControl;
    FReplacingControl: TControl;
    FUpdateCount: integer;
    procedure Update; virtual;
    property  DockSite: TWinControl read FDockSite;
  public
    constructor Create(ADockSite: TWinControl); override;
    class function  DetectAlign(ZoneRect: TRect; MousePos: TPoint): TAlign;
    function GetDockEdge(ADockObject: TDragDockObject): boolean; override;
    procedure PositionDockRect(ADockObject: TDragDockObject); override;
    procedure PositionDockRect(Client, DropCtl: TControl; DropAlign: TAlign;
      var DockRect: TRect);  override;
    procedure SetReplacingControl(Control: TControl); override; //unused
    procedure BeginUpdate; override;
    procedure EndUpdate; override;
  end;

  { TEasyTree }

  TEasyTree = class(TEasyDockManager)
  private
  {$IFDEF replace}
    FReplaceZone: TEasyZone;
  {$ELSE}
  {$ENDIF}
    FTopZone: TEasyZone;
    FSiteRect: TRect; //to detect changed site extent
    procedure UpdateTree;
  protected
  //extended interface
    //procedure ControlVisibilityChanged(Control: TControl; Visible: Boolean);  override;
    function  ZoneFromPoint(SitePos: TPoint): TEasyZone;
    function  ReloadDockedControl(const AName: string): TControl; virtual;
    function  SaveDockedControl(Control: TControl): string; virtual;
  protected //added
    function  FindControlZone(zone: TEasyZone; Control: TControl): TEasyZone;
    procedure RemoveZone(Zone: TEasyZone);
    procedure MakeVisible(ctl: TControl);
  //Lazarus extension
  private
    FHeader: TEasyDockHeader;
    FHideSingleCaption: boolean;
    FStyle: TEasyHeaderStyle;
    FSplitter: TEasySplitter;
    FSizeZone: TEasyZone; //zone to be resized, also PrevSibling
    procedure SplitterMoved(Sender: TObject); //hide and reposition zone
    procedure SetSingleCaption(Value: boolean);
  public
    procedure MessageHandler(Sender: TControl; var Message: TLMessage); override;
    procedure GetControlBounds(Control: TControl; out CtlBounds: TRect);  override;
    procedure InsertControl(Control: TControl; InsertAt: TAlign;
      DropCtl: TControl);  override;
    procedure RemoveControl(Control: TControl);  override;
    procedure ResetBounds(Force: Boolean);  override; //site resized
    procedure LoadFromStream(Stream: TStream);  override;
    procedure SaveToStream(Stream: TStream);  override;
    procedure SetReplacingControl(Control: TControl); override;
  public
  {$IFDEF singleTab}
    SingleTab: boolean; //always create notebook for alCustom?
  {$ENDIF}
    constructor Create(ADockSite: TWinControl); override;
    destructor Destroy; override;
    procedure DumpToStream(Stream: TStream);
    procedure PaintSite(DC: HDC); override;
    procedure SetStyle(NewStyle: TEasyHeaderStyle);
    function  GetEffectiveStyle: TEasyHeaderStyle;
    procedure PositionDockRect(ADockObject: TDragDockObject); override;
    property HideSingleCaption: boolean read FHideSingleCaption write SetSingleCaption;
  end;

(* General DockSite, also Notebook dock site.

  All restorable application forms should inherit from this class,
  regardless of docking capabilities.

  Add Save/Reload of docked controls?
*)
  TCustomDockSite = class(TForm)
  public
    StayDocked: boolean; //here???
    class function ReloadSite(AName: string; AOwner: TComponent): TCustomDockSite;
    procedure ShowForm(AForm: TControl); virtual;
    function  SaveSite: string; virtual;
    procedure LoadFromStream(strm: TStream); virtual;
    procedure SaveToStream(strm: TStream); virtual;
  end;

  TDockSiteClass = class of TCustomDockSite;


//events triggered from load/save docked controls
  TOnReloadControl = function(const CtrlName: string; ASite: TWinControl): TControl of object;
  TOnSaveControl = function(ACtrl: TControl): string of object;

(* TDockMaster base class, contains functions for layout save/load controls.
  The default implementation is for use by both a DockManager and a (unmanaged) DockSite.
  Unless overridden, it handles in this sequence:
  1) TCustomDockSite (notebooks...)
  2) AppLoadStore
  3) OnSave/Reload
  4) Owner of DockMaster (Application)
  5) default (Delphi) compatible handling.

  Customization is provided by OnSave/Restore handlers.
  Further customization is feasable using the AppLoadStore variable (below).
  At the time of an call, ControlDescriptor holds the full control description.

  The DockLoader variable is initialized to a default TCustomDockMaster instance,
  owned by Application. It can be overridden by the application.
  uMakeSite.DockMaster represents the full interface.
*)
  RControlDescriptor = record
    Ctrl: TControl;
    Name, ClassName, Caption: string;
  end;

  TCustomDockMaster = class(TComponent)
  protected
    rc: RControlDescriptor;
    FOnSave: TOnSaveControl;
    FOnRestore: TOnReloadControl;
    procedure CtlToRec(ctl: TControl);
    procedure IdToRec(const ID: string);
    function  RecToId: string;
  public //become class functions?
    TryCreateControls: boolean;
    function  MakeDockable(AForm: TWinControl; fWrap: boolean = True; fVisible: boolean = False): TForm; virtual;
    function  SaveControl(Control: TControl; Site: TWinControl): string; virtual;
    function  ReloadControl(const AName: string; Site: TWinControl): TControl; virtual;
    property OnSave: TOnSaveControl read FOnSave write FOnSave;
    property OnRestore: TOnReloadControl read FOnRestore write FOnRestore;
    property ControlDescriptor: RControlDescriptor read rc;
  end;
var
  DockLoader: TCustomDockMaster;

(* Application loader - a single method for consistency.
  The method has several tasks, see eAppLoadStore.
  When the application handles the request, it returns True.
  If False is returned, default actions are used (load/store controls by name).

The DockManager only deals with docked clients, whereas
the DockLoader manages sites.

When a site is to be created, notebook sites can have a different docksite
control. Here Ctrl is the outer control, containing Site.
*)
//application loader link
type
  eAppLoadStore = (
    alsSaveControl,   //save docked Control as string
    alsReloadControl, //load docked Control from string
    alsSaveSite,  //save entire Site as string
    alsReloadSite //load entire Site from string
  );

  //TAppLoadStore = function(fLoad, fSite: boolean;
  TAppLoadStore = function(mode: eAppLoadStore;
    var Site: TWinControl; var Control: TControl; var AName: string): boolean of object;
var
  AppLoadStore: TAppLoadStore;

//handy function for DockRect initialization
function  ScreenRect(ACtrl: TControl): TRect;

//primarily for DockManager internal use
function  NoteBookCreate: TCustomDockSite;
procedure NoteBookAdd(ABook: TCustomDockSite; AItem: TControl); //to be removed
function  TryRename(AComp: TComponent; const NewName: string): boolean;
function  CreateUniqueComponentName(const AClassName: string; OwnerComponent: TComponent): string;
function  FindOwnedComponent(const AName: string; AOwner: TComponent): TComponent;

const
  CustomDockSiteID = 1;
//var  AppDockBookClass: TDockBookClass;

var //debug only, these are valid only until drop
  DropOn: TControl;
  DockObj: TDragDockObject;

implementation

uses
  SysUtils, Types,
  math,
  Themes, LResources,
  fDockBook,
  Dialogs,
  LCLproc; //DebugLn

type
  TWinControlAccess = class(TWinControl)
  end;

const
{$IFDEF restore}
  HeaderButtons = [zpCloseButton, zpRestoreButton];
{$ELSE}
  HeaderButtons = [zpCloseButton];
{$ENDIF}

function  NoteBookCreate: TCustomDockSite;
begin
(* Create default dockbook type.
  Dockable notebooks must not have an specific owner.
*)
{
  if assigned(AppDockBookClass) then
    Result := AppDockBookClass.Create(AOwner)
  else
}
  //Result := TEasyDockBook.Create(AOwner);
  Result := TEasyDockBook.Create(Application);
  DockLoader.MakeDockable(Result, False, False); //do not make visible here!
  { TODO : form style should become bsNone when docked - workaround here }
  //Result.BorderStyle := bsNone;
end;

procedure NoteBookAdd(ABook: TCustomDockSite; AItem: TControl);
begin
  AItem.ManualDock(ABook);
  AItem.Visible := True;
end;

function  TryRename(AComp: TComponent; const NewName: string): boolean;
begin
(* catch errors in renaming a component
*)
  try
    AComp.Name := NewName;
    Result := True;
  except
    Result := False;
  end;
end;

function  ScreenRect(ACtrl: TControl): TRect;
begin
  Result.TopLeft := ACtrl.ControlOrigin; //screen coords
  Result.Right := Result.Left + ACtrl.Width;
  Result.Bottom := Result.Top + ACtrl.Height;
end;

function  FindOwnedComponent(const AName: string; AOwner: TComponent): TComponent;
var
  i: integer;
begin
  if assigned(AOwner) then begin
    for i := 0 to AOwner.ComponentCount - 1 do begin
      Result := AOwner.Components[i];
      if CompareText(Result.Name, AName) = 0 then
        exit; //found
    end;
  end;
//not found
  Result := nil;
end;

//from CustomFormEditor.pp
function {TCustomFormEditor.}CreateUniqueComponentName(const AClassName: string;
  OwnerComponent: TComponent): string;
var
  inst: integer;
  basename: string;
begin
(* Add instance number until unique.
  <T><basename> ==> <basename><_><number>
  The classname can be reconstructed by prefixing 'T' (typically)
  and stripping trailing digits and (optionally) '_'.
*)
  Result:=AClassName;
//strip leading 'T'
  if (length(Result)>1) and (Result[1]='T') then
    Result:=RightStr(Result,length(Result)-1);
  if (OwnerComponent=nil) or (Result='') then
    exit;
//if trailing digit, append '_'
  if Result[length(Result)] in ['0'..'9'] then
    Result:=Result+'_';
  basename := Result;
  inst := 1;
  while true do begin
  //append instance number
    Result := basename + IntToStr(inst);
    if FindOwnedComponent(Result, OwnerComponent) = nil then
      break;
    inc(inst);
  end;
end;


//implement various headers
{$I zoneheader.inc}

{ TEasyTree }

constructor TEasyTree.Create(ADockSite: TWinControl);

  procedure DockExisting;
  var
    i: integer;
    ctl: TControl;
  begin
    for i := DockSite.ControlCount - 1 downto 0 do begin
      ctl := DockSite.Controls[i];
      //InsertControl(ctl, ctl.Align, nil); //this is what Delphi does
      ctl.ManualDock(FDockSite, nil, ctl.Align); //this is what should be done
    end;
  end;

begin
  inherited Create(ADockSite);
{$IFDEF singleTab}
//test: notebook with 1 tab in root zone
  SingleTab := True;
{$ENDIF}
//init top zone
  FSiteRect := DockSite.ClientRect;
  FTopZone := TEasyZone.Create(self);
  FTopZone.SetBounds(FSiteRect);
//init helpers
  FHeader := TEasyDockHeader.Create;

  FSplitter := TEasySplitter.Create(nil);
  FSplitter.Beveled := True;
  //FSplitter.BorderStyle := bsSingle; //border does NOT react!
  //FSplitter.BorderWidth := 1;
  FSplitter.Parent := ADockSite;
  FSplitter.OnMoved := @SplitterMoved;
  FSplitter.Align := alNone;
  FSplitter.ResizeStyle := rsLine;
{$IFDEF splitter_color}
  FSplitter.Color := clPurple; //test!!!
{$ENDIF}
{$IFDEF handle_existing}
//handle controls, already residing in the site
  DockExisting; //doesn't work in the current LCL :-(
{$ENDIF}
end;

destructor TEasyTree.Destroy;
begin
  FreeAndNil(FTopZone);
  FreeAndNil(FSplitter);
  FreeAndNil(FHeader);
  inherited;
end;

function TEasyTree.ZoneFromPoint(SitePos: TPoint): TEasyZone;
var
  zone: TEasyZone;
begin
(* Return zone from site client coordinates.
*)
  zone := FTopZone; //bad coordinates???
  while zone <> nil do begin
    if (SitePos.X > zone.Right) or (SitePos.Y > zone.Bottom) then
      zone := zone.NextSibling
    else if zone.HasSizer and PtInRect(zone.GetPartRect(zpSizer), SitePos) then
      break
    else if zone.FirstChild <> nil then
      zone := zone.FirstChild
    else begin
      break; //found it
    end;
  end;
  Result := zone;
end;

function TEasyTree.FindControlZone(zone: TEasyZone; Control: TControl): TEasyZone;
begin
//return the zone containing the given control
  Result := zone;
  if Result = nil then
    exit;
  if Result.ChildControl = Control then
    exit;
  zone := zone.FirstChild;
  Result := nil;
  while (zone <> nil) and (Result = nil) do begin
    Result := FindControlZone(zone, Control);
    zone := zone.NextSibling;
  end;
end;

procedure TEasyTree.GetControlBounds(Control: TControl;
  out CtlBounds: TRect);
var
  zone: TEasyZone;
begin
//zone in client TLBR
  zone := FindControlZone(FTopZone, Control);
  if zone = nil then
    CtlBounds := Rect(0,0,0,0)
  else begin
    CtlBounds := zone.GetPartRect(zpClient);
  end;
end;

function TEasyTree.GetEffectiveStyle: TEasyHeaderStyle;
begin
(* Handle suppression of single-client header.
  DockSite.DockClientCount is not reliable at the time a control is being un/docked.
  We could count our client controls, or take some more direct approach.
*)
  //if FHideSingleCaption and (DockSite.DockClientCount <= 1) then begin
  if FHideSingleCaption //and (ChildControlCount <= 1)
  and ((FTopZone.FFirstChild = nil)
      or ((FTopZone.FFirstChild.ChildControl <> nil)
        and (FTopZone.FFirstChild.FNextSibling = nil)))
  then begin
    Result := hsNone; //single client should have no header
    //DebugLn('client style: hsNone');
  end else begin
    Result := FStyle;
    //DebugLn('zones style: %d', [FStyle]);
  end;
end;

const
  OrthoOrientation: array[TDockOrientation] of TDockOrientation = (
    //doNoOrient, doHorizontal, doVertical, doPages
    doNoOrient, doVertical, doHorizontal, doPages
  );

procedure TEasyTree.InsertControl(Control: TControl; InsertAt: TAlign;
  DropCtl: TControl);
var
  DropZone, OldZone, NewZone, OldParent, NewParent: TEasyZone;
  OldOrientation, NewOrientation: TDockOrientation;
  r: TRect;
  NoteBook: TCustomDockSite;
(* special cases:
  1) first child in top zone - no orientation
  2) second child in top zone - determines orientation
In all other cases all zones have an orientation:
  3) insert isogonal
  4) insert orthogonal

Cases 2 and 3 can be merged, with an additional or redundant setting of the orientation.
*)

begin
{$IFDEF replace}
(* Problem: the control may be undocked prior to being replaced?
*)
  {$IFDEF old}
  if FReplacingControl <> nil then begin
    DropZone := FindControlZone(FTopZone, FReplacingControl);
    FReplacingControl := nil; //flag done
    if DropZone <> nil then begin
      DropZone.ChildControl := Control;
      //DropZone.update?
      exit;
    end;
  end;
  {$ELSE}
  if FReplaceZone <> nil then begin
    FReplaceZone.ChildControl := Control;
    FReplaceZone := nil;
    FReplacingControl := nil;
    Control.Align := alClient;
    ResetBounds(True);
    exit;
  end;
  {$ENDIF}
{$ELSE}
  if Control = FReplacingControl then begin
  (* hack for morphing DropCtl into notebook,
    or initial docking of undocked controls in the dock site.
    Everything is done by the caller, after ManualDock.
  *)
    FReplacingControl := nil;
    exit;
  end;
{$ENDIF}

//some checks
  if (Control = nil)
  //or (not Control.Visible)
  or (DropCtl = Control) then begin
  //bug? The dock site is changed when a control is dropped onto itself!?
    DebugLn('Redock-----');
    exit; //nothing changed
  end;

  if DropCtl = nil then begin
  //top level docking!
    //DropCtl := FDockSite; //the dock site
    DropZone := FTopZone; // FTopZone.FirstChild;
  end else begin
    DropZone := FindControlZone(FTopZone, DropCtl);
    if DropZone = nil then exit; //not here!?
  end;

  if Control.Name = '' then //name it - for header caption
    Control.Name := CreateUniqueComponentName(Control.ClassName, Control.Owner);

(* alCustom means: drop into notebook.
  Valid only when dropped onto an existing control, not into empty dock site.
  Create notebook, if required (put both controls into new notebook).

  Try: create notebook already for first dropped control (ifdef singleTab).

  Use custom handler? Per Site or per application?
*)
  if (InsertAt = alCustom) then begin
  //dock into book
    if (FTopZone.FirstChild <> nil) then begin
    //root zone is not empty
      if (DropCtl is TCustomDockSite) then begin
        NoteBook := TCustomDockSite(DropCtl);
      end else begin
      //create new book
        NoteBook := NoteBookCreate;
      {$IFDEF replace}
        NoteBook.ReplaceDockedControl(DropZone.ChildControl, NoteBook, nil, alCustom);
      {$ELSE}
        //NoteBook.ManualDock(nil, nil); //float it - purpose???
      //hack: manually dock the notebook
        FReplacingControl := NoteBook; //ignore insert (see above)
        NoteBook.ManualDock(FDockSite); //move into DockClients[]
        DropZone.ChildControl := NoteBook; //put into the zone
        NoteBook.DockOrientation := DropCtl.DockOrientation; //strange bug?
        r := DropZone.GetPartRect(zpClient);
        NoteBookAdd(NoteBook, DropCtl); //put the original control into the notebook
        NoteBook.BoundsRect := r;
      {$ENDIF}
        NoteBook.Show;
      end; //else use existing control
      DebugLn('DM:add to existing notebook ', DbgSName(NoteBook));
      NoteBookAdd(NoteBook, Control);
      FDockSite.Invalidate; //update notebook caption
      exit;
  {$IFDEF singleTab}
    end else if SingleTab and not (DropCtl is TEasyDockBook)  then begin
    //empty root zone, create new notebook
      NoteBook := NoteBookCreate;
      NoteBook.ManualDock(FDockSite, nil, alClient);
      NoteBookAdd(NoteBook, Control);
      NoteBook.Show;
      FDockSite.Invalidate; //update notebook caption
      exit;
  {$ELSE}
  {$ENDIF}
    end;  // else //continue docking of the notebook
    InsertAt := alNone; //force automatic orientation
  end;

  NewZone := TEasyZone.Create(self);
  NewZone.ChildControl := Control as TControl;
  Control.Align := alNone;

//special case: in root zone (empty dock site)

  if FTopZone.FirstChild = nil then begin
  //insert first, without orientation
    FTopZone.InsertAfter(nil, NewZone);
    NewZone.SetBounds(FDockSite.ClientRect);
  end else begin
  //more checks
    OldZone := FindControlZone(FTopZone, Control);
      //check after placing the control
    r := DropZone.GetBounds; //for later adjustment
  //get requested orientation, adjust align
    case InsertAt of
    alTop, alBottom: NewOrientation := doVertical;
    alLeft, alRight: NewOrientation := doHorizontal;
    else //unhandled or unspecific
    { TODO : DropCtl=nil - if docked into root zone }
      if (DropCtl = nil) or (DropCtl.DockOrientation = doNoOrient) then begin
        NewOrientation := DropZone.Orientation;
      end else
        NewOrientation := DropCtl.DockOrientation;
      if NewOrientation = doNoOrient then
        NewOrientation := doHorizontal; //assume something
    //fix alignment
      if NewOrientation = doVertical then
        InsertAt := alBottom
      else
        InsertAt := alRight;
    end;
    Control.DockOrientation := NewOrientation;
{
//check topzone now must have an orientation
    if FTopZone.Orientation = doNoOrient then begin
      FTopZone.Orientation := NewOrientation;
      FTopZone.FirstChild.ChildControl.DockOrientation := NewOrientation;
    end;
}
    (* Now Control.DockOrientation is the insert orientation,
      DropCtl.DockOrientation is the zone orientation,
      InsertAt is one of alLeft/Right/Top/Bottom
    *)

  //check orientation - control orientation cannot be doNone!
    OldParent := DropZone.Parent; //nil if docked into root zone!
    if OldParent = nil then begin
    //dock into rootzone - may have no orientation
      OldOrientation := FTopZone.Orientation; // OrthoOrientation[FTopZone.Orientation];
    end else
      OldOrientation := OldParent.Orientation;;
    if (OldOrientation = doNoOrient) then begin
    //second insert into root zone - fix zone and control orientation!
      //assert(OldParent = FTopZone, '???');
      OldOrientation := NewOrientation; // Control.DockOrientation; //easy
      FTopZone.Orientation := NewOrientation;
      FTopZone.FirstChild.ChildControl.DockOrientation := NewOrientation;
    end;
  //iso or orthogonal insert?
    if (OldOrientation <> NewOrientation) then begin
    //need intermediate zone
      NewParent := TEasyZone.Create(self);
      NewParent.Orientation := NewOrientation;
      NewParent.BR := r.BottomRight;
      if OldParent <> nil then
        OldParent.ReplaceChild(DropZone, NewParent) //unlink DropZone
      else begin
        //DropZone is FTopZone
        FTopZone := NewParent;
        { TODO : what more is required??? }
      end;
    //orthogonal orientation
      NewParent.InsertAfter(nil, DropZone);
    end;
  //set control orientation - !rootzone?
    if DropZone = FTopZone then begin
    //dropzone must have a parent
      DropZone := DropZone.FirstChild;
      case InsertAt of
      //alLeft, alTop:
      alBottom, alRight:
        while DropZone.NextSibling <> nil do
          DropZone := DropZone.NextSibling;
      end;
    end;
    DropZone.AddSibling(NewZone, InsertAt);
    //if FTopZone.Orientation = doNoOrient then FTopZone.Orientation := Control.DockOrientation;
  //clear eventually moved zone, when redocking within this site
    if OldZone <> nil then begin
      RemoveZone(OldZone); //must NOT modify moved control!
      OldZone.Free;
    end;
  end;
  Control.Visible := True;
  ResetBounds(True); //splitters may have to be inserted
end;

procedure TEasyTree.PositionDockRect(ADockObject: TDragDockObject);
var
  zone: TEasyZone;
  //ZoneExtent: TPoint;
  ADockRect: TRect;
begin
(* New DockManager interface, called instead of the old version.
  Determine exact target (zone) and DropAlign.

  For top-level docking: check mouse IN site!

Signal results:
  Prevent docking by setting DropOnControl=Control (prevent changes when dropped).
  DragTarget=nil means: become floating.

  Unfortunately there exists no way to signal invalid docking attempts :-(
*)
{ TODO -cdocking : why is this method not called for a docksite with 1 client?
  If exactly the client is moved over it?
-> by design of GetDockTarget in DockPerformer!
}
//debug only
  DockObj := ADockObject;

//determine the zone containing the DragTargetPos
  with ADockObject do begin
  //mouse position within dock site
    //DragTargetPos := DragTarget.ScreenToClient(DragPos);
  //find zone, handle empty site for elastic panels
    if (DockSite.DockClientCount = 0)
  {$IFDEF RootDock}
    or (not PtInRect(DockRect, DragPos))
  {$ENDIF}
    then
      zone := FTopZone
    else
      zone := ZoneFromPoint(DragTargetPos);

    if (zone = nil) or (Control = zone.ChildControl) then begin
      DropAlign := alNone; //prevent drop (below)
    end else begin
      ADockRect := zone.GetBounds; //include header
      DropOnControl := zone.ChildControl; //correct hit in header, not in control
      if DockSite.DockClientCount = 0 then
        DropAlign := alClient //always span empty site
      else
       DropAlign := DetectAlign(ADockRect, DragTargetPos);
    {$IFnDEF RootDock}
      if DropOnControl = nil then begin
      {$IFDEF singleTab}
        if SingleTab and (DropAlign = alCustom) then begin
          //notebook in top zone
        end else
      {$ENDIF}
          DropAlign := alClient; //first element in entire site
      end; //else //determine the alignment within the zone.
    {$ELSE}
    //allow for top-level docking
    {$ENDIF}
    //to screen coords
      ADockRect.TopLeft := FDockSite.ClientToScreen(ADockRect.TopLeft);
      ADockRect.BottomRight := FDockSite.ClientToScreen(ADockRect.BottomRight);
    end;
  //position DockRect
    if DropAlign = alNone then begin
    //force DockRect update by DockTrackNoTarget
      DropOnControl := Control; //prevent drop - signal drop onto self
    {$IFDEF NoDrop}
      NoDrop := True; //signal that nothing will happen on a drop
    {$ELSE}
    //all these are lousy features, do not allow to signal an invalid drop
      DragTarget := nil;
      //Control.DockTrackNoTarget
      //DragTarget := FDockSite; //prevent floating - doesn't work :-(
      //DockRect := Rect(MaxInt, MaxInt, 0, 0); //LTRB - very strange effect!
      //DockRect := Rect(MaxInt, 0, MaxInt, 0); //LTRB
    {$ENDIF}
    end else begin
      PositionDockRect(Control, DropOnControl, DropAlign, ADockRect);
      DockRect := ADockRect;
    end;
  end;
end;

procedure TEasyTree.MessageHandler(Sender: TControl; var Message: TLMessage);
//was: procedure TEasyTree.MouseMessage(var Message: TLMessage);
var
  r: TRect;
  Part: TEasyZonePart;
  Control: TControl;
  MouseMsg: TLMMouse absolute Message;
  Zone: TEasyZone;
  MousePos: TPoint;

  function FindZone(fButtonDown: boolean): TEasyZonePart;
  begin
    MousePos := SmallPointToPoint(MouseMsg.Pos);
    Zone := ZoneFromPoint(MousePos);
  //exact zone part
    if (Zone = nil) then begin
      Result := zpNowhere;
      Control := nil;
    end else begin
      Control := Zone.ChildControl;
      Result := FHeader.FindPart(zone, MousePos, fButtonDown);
    end;
    Part := Result;
  end;

  procedure ShowSplitter(z: TEasyZone);
  begin
  //move splitter into header
  //also set splitter range! (todo)
    FSizeZone := z; //now: the zone with the header (second sibling!)
    FSplitter.BoundsRect := FHeader.PartRect;
    if z.Parent.Orientation = doVertical then begin
      FSplitter.ResizeAnchor := akTop
    end else begin
      FSplitter.ResizeAnchor := akLeft;
    end;
    //FSplitter.??? - make topmost child?
    FSplitter.Show;
  end;

begin
  case Message.msg of
    LM_LBUTTONUP:
      case FindZone(True) of
    {$IFDEF restore}
      zpRestoreButton:
        Control.ManualDock(nil, nil, alNone);
    {$ENDIF}
      zpCloseButton:
        begin
        {$IFDEF visibility}
        //handling of invisible clients deserves dock manager notification!
          if Control is TCustomForm then
            TCustomForm(Control).Close
          else // not a form => doesnot have close => just hide
            Control.Visible := False;
          DockSite.Invalidate;
        {$ELSE}
        (* Definitely close a form, but other controls???
        *)
          if Control is TCustomForm then begin
            TCustomForm(Control).Close;
            Control.ManualDock(nil, nil, alNone); //do float
            TWinControlAccess(Control).DoEndDock(nil, Control.Left, Control.Top);
          end else begin
        { TODO -cLCL : OnEndDock not called by ManualDock? }
            Control.ManualDock(nil, nil, alNone); //do float
            TWinControlAccess(Control).DoEndDock(nil, Control.Left, Control.Top);
            Control.Visible := False;
          end;
        {$ENDIF}
        end;
      end;
    LM_LBUTTONDOWN:
      case FindZone(False) of
      zpCaption: // mouse down on not buttons => start drag
        begin //problem here - app hangs!
        (* perhaps the mousedown state flag gets into the way?
        *)
          DebugLn('start dragging from header: %s', [FDockSite.GetDockCaption(Control)]);
          //MousePos := Control.ClientToScreen(Point(1,1));
          //Mouse.CursorPos := MousePos; //todo: inside control?
          Control.BeginDrag(False);
          //Control.BeginDrag(True); //floats only - due to mouse outside control?
        end;
      end;
    LM_MOUSEMOVE:
      begin //what's Message.Keys???
        case FindZone(FHeader.MouseDown) of
        zpSizer:
          if Zone.HasSizer then begin
            ShowSplitter(zone);
          end;
        else
          FSplitter.Hide;
        end;

{ TODO : How to set the cursor, depending on the zone part? }
      case Part of
        zpCaption:
          //Screen.Cursor := crHandPoint;
          //FDockSite.SetTempCursor(crHandPoint);
          FDockSite.Cursor := crHandPoint; //seems to work
        //else  SetCursor(crNone); //seems not required?
        end;

      end;

{
    CM_MOUSELEAVE:
      CheckNeedRedraw(nil, Rect(0,0,0,0), ldhpAll);
    CM_TEXTCHANGED:
      begin
        if GetControlHeaderRect(Sender, ARect) then
        begin
          ARect := TDockHeader.GetRectOfPart(ARect, Sender.DockOrientation, ldhpCaption);
          InvalidateRect(DockSite.Handle, @ARect, False);
        end;
      end;
    CM_VISIBLECHANGED:
      begin
        if not (csDestroying in Sender.ComponentState) then
        begin
          AZone := RootZone.FindZone(Sender) as TLazDockZone;
          if AZone <> nil then
            BuildDockLayout(TLazDockZone(AZone.Parent));
        end;
      end;
}
  end;
end;

procedure TEasyTree.SplitterMoved(Sender: TObject);
var
  ptNew: TPoint;
begin
(* The unbound splitter has been moved.
  Reflect new sizes in FSizeZone and prev(!) sibling.
  ptOuter is the new parent extent, BR of the last sibling.
*)
  FSplitter.Hide;
  ptNew := FSizeZone.PrevSibling.BR;
  if FSizeZone.Parent.Orientation = doVertical then
    ptNew.y := FSplitter.Top //above splitter
  else
    ptNew.x := FSplitter.Left; //left of splitter
  FSizeZone.PrevSibling.ScaleTo(FSizeZone.PrevSibling.BR, ptNew, ptNew);
  FSizeZone.SetBounds(FSizeZone.GetBounds); //BR unchanged, only update the control
  FDockSite.Invalidate;
end;

procedure TEasyTree.PaintSite(DC: HDC);
var
  ACanvas: TCanvas;
  MousePos: TPoint;

  procedure PaintZone(z: TEasyZone);
  var
    ACaption: string;
  begin
    if z.ChildControl <> nil then begin
    //paint header
      ACaption := DockSite.GetDockCaption(z.ChildControl);
      FHeader.Draw(z, ACanvas, ACaption, MousePos);
    end else begin
    //paint children
      z := z.FirstChild;
      while z <> nil do begin
        PaintZone(z);
        z := z.NextSibling;
      end;
    end;
  end;

// paint bounds for each control and close button
begin
  if FTopZone.FirstChild = nil then
    exit; //no zones - nothing to paint
  ACanvas := TCanvas.Create;
  try
    ACanvas.Handle := DC;
    GetCursorPos(MousePos);
    MousePos := DockSite.ScreenToClient(MousePos);
    PaintZone(FTopZone);
  finally
    Acanvas.Free;
  end;
end;

procedure TEasyTree.RemoveControl(Control: TControl);
var
  zone: TEasyZone;
begin
//propagate changes into parent zones
  zone := FindControlZone(FTopZone, Control);
  if zone <> nil then begin
    RemoveZone(zone);
    ResetBounds(True);
  end;
end;

procedure TEasyTree.ResetBounds(Force: Boolean);
var
  rNew: TRect;
begin
//drop site resized - never called in Lazarus???
  if (csLoading in FDockSite.ComponentState) then
    exit; //not the right time to do anything
//how to determine old bounds?
  rNew := FDockSite.ClientRect;
//try catch bad calls (Win32)?????
  if (rNew.Right <= 0) or (rNew.Bottom <= 0) then
    exit;

  if not CompareMem(@rNew, @FSiteRect, sizeof(rNew)) then
    Force := True;  //something has changed
  if not Force then
    exit;
  if FTopZone.FirstChild = nil then begin
    FSiteRect := rNew;
    FTopZone.BR := rNew.BottomRight;
    exit; //zone is empty, all done
  end;
  FTopZone.ScaleTo(FSiteRect.BottomRight, rNew.BottomRight, rNew.BottomRight);
  FSiteRect := rNew;
  FSplitter.Hide;
  FDockSite.Invalidate; //force repaint of headers
end;


type
  RZone = packed record
    BottomRight: TPoint;
    Level: byte;
    Orientation: TDockOrientation;
    //NameLen: integer; //+chars, can be a complete notebook description
    //+ZoneName as AnsiString
  end;

var
  ZoneRec: RZone;
  ZoneName: string;

procedure TEasyTree.LoadFromStream(Stream: TStream);

  function GetRec: integer;
  //var NameLen: integer;
  begin
    Stream.Read(ZoneRec, SizeOf(ZoneRec));
    Result := ZoneRec.Level;
    if Result > 0 then begin
      ZoneName := Stream.ReadAnsiString;
      DockLoader.IdToRec(ZoneName);
    end;
  //debug
    if Result > 0 then
      DebugLn('reload %s @%d [%d,%d]', [ZoneName, Result, ZoneRec.BottomRight.x, ZoneRec.BottomRight.y])
    else
      DebugLn('reload done');
  end;

  procedure ClearSite;
  var
    i: integer;
    ctl: TControl;
  begin
  (* undock all currently docked clients, prior to restoring a layout
  *)
    for i := FDockSite.DockClientCount - 1 downto 0 do begin
      ctl := FDockSite.DockClients[i];
      ctl.Visible := True; //False?
      ctl.ManualDock(nil);  //restore undocked position and extent
    end;
  end;

  procedure MakeZones;
  var
    PrevZone, NewZone: TEasyZone;
    PrevLvl, NewLvl: byte;
    NewCtl: TControl;
    r: TRect;
  begin
    PrevZone := FTopZone;
    PrevLvl := 1;
    while GetRec > 0 do begin
      NewLvl := ZoneRec.Level;
      NewZone := TEasyZone.Create(self);
      NewZone.Orientation := ZoneRec.Orientation;
      NewZone.BR := ZoneRec.BottomRight;
      //if ZoneRec.NameLen > 0 then begin
      if ZoneName <> '' then begin
      //we can NOT expect that Reload... is overridden!?
        NewCtl := ReloadDockedControl(ZoneName);
      //do we need a control in any case?
        if NewCtl = nil then begin
        //debug: create some control
          NewCtl := TPanel.Create(DockSite);
          TryRename(NewCtl, DockLoader.ControlDescriptor.Name);   // ZoneName);
        end;
        if NewCtl <> nil then begin
          NewZone.ChildControl := NewCtl;
          SetReplacingControl(NewCtl); //prevent DockManager actions
          NewCtl.ManualDock(DockSite);
          //NewCtl.DisableAutoSizing; //forever?
          r := NewZone.GetPartRect(zpClient);
          NewCtl.BoundsRect := r;
          DebugLn('?NewCtl w=%d=%d h=%d=%d', [NewCtl.Width, ZoneRec.BottomRight.x,NewCtl.Height, ZoneRec.BottomRight.y]);
          NewCtl.Visible := True;
          //NewCtl.EnableAutoSizing;
          DebugLn('!NewCtl w=%d=%d h=%d=%d', [NewCtl.Width, ZoneRec.BottomRight.x,NewCtl.Height, ZoneRec.BottomRight.y]);
        end;
      {$IFDEF oldOrient}
      {$ELSE}
        NewZone.Orientation := doNoOrient;
        NewCtl.DockOrientation := ZoneRec.Orientation;
      {$ENDIF}
      end;
      while NewLvl < PrevLvl do begin
        PrevZone := PrevZone.Parent;
        dec(PrevLvl);
      end;
      if NewLvl = PrevLvl then //add sibling
        PrevZone.Parent.InsertAfter(PrevZone, NewZone)
      else begin //NewLvl > PrevLvl - make child
        PrevZone.InsertAfter(nil, NewZone);
        PrevLvl := NewLvl;
      end;
      PrevZone := NewZone;
    end;
  end;

begin
(* Problem: what if site doesn't match zone extent?
*)
//remove all docked controls
  ClearSite;
//read record
  if GetRec > 0 then begin
    FSiteRect.BottomRight := ZoneRec.BottomRight; //stored extent, not current one!
    FTopZone.BR := ZoneRec.BottomRight;
    FTopZone.Orientation := ZoneRec.Orientation;
    MakeZones;
  //now make the zone fit the current extent, and position all child controls
    //DebugLn('fit zone ', DbgS(FTopZone.BR), ' into site ', DbgS(FDockSite.ClientRect.BottomRight));
    ResetBounds(True);  //always position controls!
  //finish?
  //remove all leafs without a child control?
  end;
//assure everything is visible (recursive, until topmost parent)
  MakeVisible(FDockSite);
end;

function TEasyTree.ReloadDockedControl(const AName: string): TControl;
begin
(* Reload from
- saved site info (if CustomDockSite)
- AppLoadStore
- DockSite
*)
  Result := DockLoader.ReloadControl(AName, FDockSite);
end;

function TEasyTree.SaveDockedControl(Control: TControl): string;
begin
(* Create string descriptor for docked control.
  Override in sync with ReloadDockedControl!
*)
  Result := DockLoader.SaveControl(Control, FDockSite);
end;

procedure TEasyTree.SaveToStream(Stream: TStream);

  procedure DoSaveZone(Zone: TEasyZone; Level: byte);
  var
    child: TControl;
  begin
    while Zone <> nil do begin
    //fill ZoneRec
      ZoneRec.Level := Level;
      ZoneRec.Orientation := Zone.Orientation;
      ZoneRec.BottomRight := Zone.BR;
      child := Zone.ChildControl;
      if child = nil then
        ZoneName := ''
      else begin
        ZoneName := SaveDockedControl(child); //ctrl or entire site
      {$IFDEF oldOrient}
      {$ELSE}
        ZoneRec.Orientation := child.DockOrientation;
      {$ENDIF}
      end;
    //write descriptor
      Stream.Write(ZoneRec, sizeof(ZoneRec));
      Stream.WriteAnsiString(ZoneName);
    // recurse into first child
      if Zone.FirstChild <> nil then
        DoSaveZone(Zone.FirstChild, Level + 1); //all children of Level
    // recurse into next sibling
      Zone := Zone.NextSibling;
    end;
  end;

begin
// write all zones from tree
  DoSaveZone(FTopZone, 1);
//write end marker (dummy record of level 0)
  ZoneRec.Level := 0;
  Stream.Write(ZoneRec, sizeof(ZoneRec));
end;

procedure TEasyTree.DumpToStream(Stream: TStream);
var
  r: TRect;
  s: string;
const
  eol: string = #13#10; //to be replaced by system constant?
  OrientString: array[TDockOrientation] of char = ('N','H','V'
    {$IFDEF FPC} ,'P' {$ENDIF} );

  procedure WriteZone(zone: TEasyZone; level: integer);
  var
    ind: string;
    s: string;
    ctl: TControl;
  begin
    ind := StringOfChar(' ', level*2);
  //zone
    if zone.ChildControl <> nil then
      s := zone.ChildControl.ClassName + '.' + zone.ChildControl.Name
    else
      s := '''''';
    r := zone.GetBounds;
    s := Format('%s%s (%d,%d)-(%d,%d)%s', [ind, OrientString[zone.orientation],
      r.Top, r.Left, r.Bottom, r.Right, eol]);
    Stream.Write(s[1], length(s));
  //splitter
    if zone.HasSizer then begin
      r := zone.GetPartRect(zpSizer);
      s := Format('%sSplitter (%d,%d)-(%d,%d)%s', [ind,
        r.Top, r.Left, r.Bottom, r.Right, eol]);
      Stream.Write(s[1], length(s));
    end;
  //control
    ctl := zone.ChildControl;
    if ctl <> nil then begin
      r := ctl.BoundsRect;
      s := Format('%s%s %s.%s (%d,%d)-(%d,%d)%s', [ind, OrientString[ctl.DockOrientation],
        ctl.ClassName, ctl.Name,
        r.Top, r.Left, r.Bottom, r.Right, eol]);
      Stream.Write(s[1], length(s));
      if ctl.Visible then
        s := 'visible'
      else
        s := 'hidden';
      s := ind + s + eol;
      Stream.Write(s[1], length(s));
    end;
    zone := zone.FirstChild;
    while zone <> nil do begin
      WriteZone(zone, level+1);
      zone := zone.NextSibling;
    end;
    Stream.Write(eol[1], length(eol));
  end;

begin
//for now: dump tree
//splitter
  if FSplitter.Visible then begin
    r := FSplitter.BoundsRect;
    s := Format('Splitter (%d,%d)-(%d,%d)%s', [
      r.Top, r.Left, r.Bottom, r.Right, eol]);
    Stream.Write(s[1], length(s));
  end;
  WriteZone(FTopZone, 0);
end;

procedure TEasyTree.SetReplacingControl(Control: TControl);
begin
(* The Control may have been undocked, until the replace request is handled.
*)
  inherited SetReplacingControl(Control);
{$IFDEF replace}
  FReplaceZone := FindControlZone(FTopZone, Control);
{$ELSE}
{$ENDIF}
end;

procedure TEasyTree.SetSingleCaption(Value: boolean);
begin
(* Hide header if no more than one client is docked.
*)
  if FHideSingleCaption = Value then
    exit;
  FHideSingleCaption := Value;
  ResetBounds(True);
end;

procedure TEasyTree.SetStyle(NewStyle: TEasyHeaderStyle);
begin
  if NewStyle = FStyle then
    exit;
  FStyle := NewStyle;
  ResetBounds(True);
end;

procedure TEasyTree.UpdateTree;
begin
  //nothing to do?
  //FDockSite.Invalidate;
end;

procedure TEasyTree.RemoveZone(Zone: TEasyZone);

  procedure InlineKids(z: TEasyZone);
  var
    pp, p, ch: TEasyZone;
  begin
  (* move all children of z into z.parent.parent, in place of z.parent
  *)
    p := z.Parent;
    pp := p.Parent;
  //link first child
    ch := z.FirstChild;
    ch.FPrevSibling := p.FPrevSibling;
    if ch.PrevSibling <> nil then
      ch.PrevSibling.FNextSibling := ch
    else
      pp.FFirstChild := ch;
    ch.FParent := pp;
  //link last child
    while ch.NextSibling <> nil do begin
      ch := ch.NextSibling;
      ch.FParent := pp;
    end;
    ch.FNextSibling := p.NextSibling;
    if ch.NextSibling <> nil then
      ch.NextSibling.FPrevSibling := ch;
  //delete zones
    z.FFirstChild := nil;
    p.FParent := nil; //don't unlink!
    p.Free;
  end;

var
  p, ch: TEasyZone;
  affected: TEasyZone; //the zone whose children have changed
begin
//propagate changes into parents!
//parents only can have zones, no controls
  affected := nil;
  repeat
    p := zone.Parent;
    if p = nil then
      break; //exit; //reached top zone!
    zone.Free; //unlink
    zone := p;
    affected := p;
  until zone.FirstChild <> nil;
(* cases:
  zone without parent (top) - check orientation, exit
  zone with 0 children - excluded before
  zone with 1 child (ch),
    child is leaf -> looses orientation, move up
    child contains 1 child (cc) -> orientation fits, move child.child up
  zone with more children - exit
*)
  while (zone.Parent <> nil) do begin
    p := zone.Parent;
    ch := zone.FirstChild;
    if ch.NextSibling <> nil then begin
    //more than 1 child - check next level
    end else if ch.FirstChild = nil then begin
    //contains control, move up
      ch.ChildControl.DockOrientation := p.Orientation;
      p.ReplaceChild(zone, ch); //move control up
      zone.Free;
      affected := p;
    end else if ch.FirstChild.NextSibling = nil then begin
    //contains zone with 1 child: orientation fits, move up
      InlineKids(ch.FirstChild);
      affected := ch.Parent; //the new parent
      p := ch;  // affected;
    end;
    zone := p;
  end;
//update affected zone, to close the gap
  if affected <> nil then begin
    Zone := affected.FirstChild;
    while Zone <> nil do begin
      if Zone.NextSibling = nil then
        Zone.BR := affected.BR; //resize last zone
      zone.PositionControl;
      zone := Zone.NextSibling;
    end;
{
//root zone may loose orientation!
    if affected = FTopZone then begin
      if affected.FirstChild.NextSibling = nil then begin
        FTopZone.Orientation := doNoOrient;
        FTopZone.FirstChild.ChildControl.DockOrientation := doNoOrient; <--- non-leaf!?
      end;
    end;
    }
  end;
{$IFDEF RootDock}
  while FTopZone.FirstChild <> nil do begin
    Zone := FTopZone.FirstChild;
    if Zone.NextSibling <> nil then
      break; //has multiple kids - done
    if Zone.ChildControl <> nil then begin
    //single child, loose orientation
      FTopZone.Orientation := doNoOrient;
      Zone.ChildControl.DockOrientation := doNoOrient;
      break; //done
    end;
  //move zone up
    FTopZone.FFirstChild := nil;
    zone.Parent := nil;
    FTopZone.Free;
    FTopZone := zone;
  end;
{$ELSE}
{$ENDIF}
//check root empty
  if FTopZone.FirstChild = nil then
    FTopZone.Orientation := doNoOrient;

//update zone, here simply the whole dock site
  FSplitter.Hide;
  DockSite.Invalidate;
end;

procedure TEasyTree.MakeVisible(ctl: TControl);
begin
  while assigned(ctl) do begin
    ctl.Visible := True;
    ctl := ctl.Parent;
  end;
end;

{ TEasyZone }

constructor TEasyZone.Create(ATree: TEasyTree);
begin
  FTree := ATree;
end;

destructor TEasyZone.Destroy;
begin
  Clear;
  Parent := nil; //unlink
  inherited;
end;

procedure TEasyZone.Clear;
begin
//let parent care for updating its chain?
  while FirstChild <> nil do
    FirstChild.Free;
end;

function TEasyZone.DockSite: TWinControl;
begin
  Result := FTree.FDockSite;
end;


{$IFDEF visibility}
function TEasyZone.GetVisible: boolean;
begin
  Result := GetVisibleControl <> nil;
end;

function TEasyZone.GetVisibleControl: TControl;
var
  zone: TEasyZone;
begin
  Result := ChildControl;
  if (ChildControl <> nil) then begin
    if not ChildControl.Visible then
      Result := nil;
    exit;
  end;
//search children
  zone := FirstChild;
  while zone <> nil do begin
    Result := zone.GetVisibleControl;
    if Result <> nil then
      exit;
    zone := zone.NextSibling;
  end;
end;
{$ENDIF}

function TEasyZone.GetPartRect(APart: TEasyZonePart): TRect;
begin
(* Can zones have individual headers?
  For notebooks a caption text is not required!

  NewSplitter: non-leaf zones also can have an splitter!
  The higher level splitters have to be excluded from the zone rect.
    (done in GetBounds)
*)
  if (ChildControl <> nil) then
    Result := FTree.FHeader.GetRectOfPart(GetBounds, ChildControl.DockOrientation,
      APart, HasSizer, Style)
  else if (APart = zpSizer) and HasSizer then
    Result := FTree.FHeader.GetRectOfPart(GetBounds, Parent.Orientation,
      APart, HasSizer, Style)
  else
    Result := Rect(0,0,0,0);
end;

function TEasyZone.GetStyle: TEasyHeaderStyle;
begin
(* Get the effective header style.
  A single notebook client deserves no header at all.
  Other single clients can have no header (optional)
*)
  Result := FTree.GetEffectiveStyle;
end;

function TEasyZone.HasSizer: boolean;
begin
  Result := PrevSibling <> nil;
end;

//-------------- basic linking ----------------

procedure TEasyZone.SetParent(zone: TEasyZone);
begin
//for public use! unlink if parent changes.
  if zone <> Parent then begin
  //unlink, handle FirstChild
    if PrevSibling <> nil then
      PrevSibling.FNextSibling := NextSibling
    else if Parent <> nil then
      Parent.FFirstChild := NextSibling;
    if NextSibling <> nil then
      NextSibling.FPrevSibling := PrevSibling;
    FParent := zone;
    FNextSibling := nil;
    FPrevSibling := nil;
  end;
end;

procedure TEasyZone.InsertAfter(LinkAfter, NewZone: TEasyZone);
begin
//LinkAfter=nil for insert as first child.
  NewZone.Parent := self; //unlink if required
  if LinkAfter = nil then begin //as first child
    NewZone.FNextSibling := FirstChild;
    if FirstChild <> nil then
      FirstChild.FPrevSibling := NewZone;
    FFirstChild := NewZone;
  end else begin
    NewZone.FPrevSibling := LinkAfter;
    NewZone.FNextSibling := LinkAfter.NextSibling;
    if LinkAfter.NextSibling <> nil then
      LinkAfter.NextSibling.FPrevSibling := NewZone;
    LinkAfter.FNextSibling := NewZone;
  end;
  //NewZone.Orientation := LinkAfter.Orientation;
end;

procedure TEasyZone.PositionControl;
var
  r: TRect;
begin
//obsolete, part of SetBounds!
  if FChildControl = nil then
    exit;
  r := GetPartRect(zpClient);
  FChildControl.BoundsRect := r;
end;

procedure TEasyZone.AddSibling(NewZone: TEasyZone; where: TAlign);
var
  LinkAfter: TEasyZone;
  r, r2: TRect;
  NewOrientation: TDockOrientation;
begin
//orientation is NOT checked!
  r := GetBounds; //valid old values
  case where of
  alLeft, alTop:  LinkAfter := PrevSibling;
  alRight, alBottom: LinkAfter := self;
  else
    DebugLn('unhandled insertion');
    LinkAfter := nil; //must never happen!
  end;
  Parent.InsertAfter(LinkAfter, NewZone);
//resize?
  r2 := r;
{$IFDEF KeepSize}
{ TODO : update TControl to init the UndockWidth/Height properly }
  case where of
  alLeft:
    begin
      NewOrientation := doHorizontal;
      if (LinkAfter <> nil) and (LinkAfter.ChildControl <> nil) then
        r.Left := r.Left + (r.Right - r.Left) * NewZone.ChildControl.UndockWidth div (NewZone.ChildControl.UndockWidth + LinkAfter.ChildControl.UndockWidth)
      else
        r.Left := (r.Left+r.Right) div 2;
      r2.Right := r.Left;
    end;
  alRight:
    begin
      NewOrientation := doHorizontal;
      if (LinkAfter <> nil) and (LinkAfter.ChildControl <> nil) then
        r.Right := r.Left + (r.Right - r.Left) * LinkAfter.ChildControl.UndockWidth div (NewZone.ChildControl.UndockWidth + LinkAfter.ChildControl.UndockWidth)
      else
        r.Right := (r.Left+r.Right) div 2;
      r2.Left := r.Right;
    end;
  alTop:
    begin
      NewOrientation := doVertical;
      if (LinkAfter <> nil) and (LinkAfter.ChildControl <> nil) then
        r.Top := r.Top + (r.Bottom - r.Top) * NewZone.ChildControl.UndockHeight div (NewZone.ChildControl.UndockHeight + LinkAfter.ChildControl.UndockHeight)
      else
        r.Top := (r.Bottom+r.Top) div 2;
      r2.Bottom := r.Top;
    end;
  alBottom:
    begin
      NewOrientation := doVertical;
      if (LinkAfter <> nil) and (LinkAfter.ChildControl <> nil) then
        r.Bottom := r.Top + (r.Bottom - r.Top) * LinkAfter.ChildControl.UndockHeight div (NewZone.ChildControl.UndockHeight + LinkAfter.ChildControl.UndockHeight)
      else
        r.Bottom := (r.Bottom+r.Top) div 2;
      r2.Top := r.Bottom;
    end;
  else //keep compiler happy
    NewOrientation := doNoOrient;
  end;
{$ELSE}
  case where of
  alLeft:
    begin
      NewOrientation := doHorizontal;
      r.Left := (r.Left+r.Right) div 2;
      r2.Right := r.Left;
    end;
  alRight:
    begin
      NewOrientation := doHorizontal;
      r.Right := (r.Left+r.Right) div 2;
      r2.Left := r.Right;
    end;
  alTop:
    begin
      NewOrientation := doVertical;
      r.Top := (r.Bottom+r.Top) div 2;
      r2.Bottom := r.Top;
    end;
  alBottom:
    begin
      NewOrientation := doVertical;
      r.Bottom := (r.Bottom+r.Top) div 2;
      r2.Top := r.Bottom;
    end;
  else //keep compiler happy
    NewOrientation := doNoOrient;
  end;
{$ENDIF}
//parent orientation? (if in rootzone)
  //if parent.Orientation = doNoOrient then
  if Parent <> nil then
    Parent.Orientation := NewOrientation;
  if ChildControl <> nil then
    ChildControl.DockOrientation := NewOrientation;
  if NewZone.ChildControl <> nil then
    NewZone.ChildControl.DockOrientation := NewOrientation;
  SetBounds(r);
  NewZone.SetBounds(r2);
end;

procedure TEasyZone.ReplaceChild(OldChild, NewChild: TEasyZone);
begin
//usage: insert intermediate parent zone
  NewChild.Parent := nil; //unlink
  NewChild.Parent := Self;
  NewChild.FNextSibling := OldChild.NextSibling;
  NewChild.FPrevSibling := OldChild.PrevSibling;
  OldChild.Parent := nil; //unlink
  if NewChild.NextSibling <> nil then
    NewChild.NextSibling.FPrevSibling := NewChild;
//handle FirstChild
  if NewChild.PrevSibling <> nil then
    NewChild.PrevSibling.FNextSibling := NewChild
  else
    FFirstChild := NewChild;
//init size
  NewChild.Orientation := OldChild.Orientation; //propagate?
  NewChild.BR := OldChild.BR;
end;

//procedure TEasyZone.ScaleTo(const ptOld, ptNew, ptOuter: TPoint);
procedure TEasyZone.ScaleTo(ptOld, ptNew, ptOuter: TPoint);
var
  ch: TEasyZone;
begin
(* change all coordinates from old to new extent.
  Set exact values for last sibling.
  Must use floating point division, to keep rounding errors acceptable!
  ptOuter is the new BottomRight of the parent zone.
*)
(* "const" woes:
  aliased points (BR!) must not be passed as const (pointers!)
*)
  //DebugLn('scale (%d,%d) to (%d,%d)', [ptOld.y, ptOld.x, ptNew.y, ptNew.x]);

  //exact boundary in parent orientation
  if (Parent = nil) or (Parent.Orientation = doNoOrient) then
    br := ptOuter
  else if Parent.Orientation = doVertical then begin
    br.X := ptOuter.X;
    if NextSibling = nil then
      br.Y := ptOuter.Y
    else
      br.Y := round(br.Y * ptNew.Y / ptOld.Y);
  end else begin
    br.Y := ptOuter.Y;
    if NextSibling = nil then
      br.X := ptOuter.X
    else
      br.X := round(br.X * ptNew.X / ptOld.X);
  end;

  if ChildControl <> nil then begin
    PositionControl;
  end else begin
    ch := FirstChild;
    while ch <> nil do begin
      ch.ScaleTo(ptOld, ptNew, BR);
      ch := ch.NextSibling;
    end;
  end;
end;

procedure TEasyZone.SetControl(Control: TControl);
begin
// propagate orientation into control.
  FChildControl := Control;
  FOrientation := doNoOrient;
end;

procedure TEasyZone.SetOrientation(NewOrientation: TDockOrientation);
begin
  FZoneOrientation := NewOrientation; //independent from ChildControl
  if ChildControl = nil then
    FOrientation := NewOrientation
  else
    FOrientation := doNoOrient;
end;

//----------- prev sibling based coordinates ----------

function TEasyZone.GetTopOrLeft(fTop: boolean): Integer;
var
  zone, prev: TEasyZone;
begin
// In a parent zone of vertical orientation the zone.PrevSibling.Bottom is zone.Top
(* NewSplitter: exclude higher level splitters.
*)
  zone := self;
  if zone.Parent <> nil then begin
    if (fTop = (zone.Parent.Orientation = doVertical)) then begin
      prev := zone.PrevSibling;
      while prev <> nil do begin
      {$IFDEF visibility}
        if prev.Visible then
      {$ELSE}
        //zones are always visible
      {$ENDIF}
        begin
          if fTop then
            Result := prev.Bottom
          else
            Result := prev.Right;
          exit;
        end;
        prev := prev.PrevSibling;
      end;
    end;
  //no sibling, get from parent zone
    Result := Parent.GetTopOrLeft(fTop);
    if Parent.HasSizer and (fTop <> (Parent.Orientation = doVertical)) then
      inc(Result, dSizer); //exclude immediate parent's splitter (opposite orientation?)
  end else begin
    Result := 0;
  end;
end;

function TEasyZone.GetLeft: Integer;
begin
  Result := GetTopOrLeft(False);
end;

function TEasyZone.GetTop: Integer;
begin
  Result := GetTopOrLeft(True);
end;

function TEasyZone.GetBounds: TRect;
begin
//return defined extent
  Result.Top := Top;
  Result.Left := Left;
  Result.BottomRight := BR;
end;

function TEasyZone.GetHandle: HWND;
begin
  Result := FTree.FDockSite.Handle;
end;

procedure TEasyZone.SetBounds(TLBR: TRect);
var
  z: TEasyZone;
begin
(* Zone cannot be the root zone. If so, ignore?
  Recurse into child zones.
*)
  BR := TLBR.BottomRight;
  if ChildControl <> nil then begin //is control zone
    //FTree.AdjustDockRect(ChildControl, TLBR);
    TLBR := FTree.FHeader.GetRectOfPart(TLBR, ChildControl.DockOrientation, zpClient, HasSizer, Style);
    ChildControl.BoundsRect := TLBR;
  end else if FirstChild <> nil then begin
    z := FirstChild;
    while z <> nil do begin
    //resize - for splitter move only!
      if Orientation = doVertical then //left/right changed
        z.BR.x := TLBR.Right
      else
        z.BR.y := TLBR.Bottom;
      z.SetBounds(z.GetBounds);
      z := z.NextSibling;
    end;
  end; //else empty root zone?
end;

{ TEasyDockManager }

procedure TEasyDockManager.BeginUpdate;
begin
  //inherited BeginUpdate;
  inc(FUpdateCount);
end;

constructor TEasyDockManager.Create(ADockSite: TWinControl);
begin
(* Init DockSite and DragManager.
*)
  FDockSite := ADockSite;
  ADockSite.DockManager := self;
//reset inappropriate docking defaults - should be fixed in Controls/DragManager!
  DragManager.DragImmediate := False;
  inherited Create(ADockSite);
end;

class function TEasyDockManager.DetectAlign(ZoneRect: TRect;
  MousePos: TPoint): TAlign;
var
  w, h, zphi: integer;
  cx, cy: integer;
  dx, dy: integer;
  phi: double;
  izone: integer;
  dir: TAlign;
const
  k = 5; //matrix dimension
//mapping octants into aligns, assuming k=5
  cDir: array[-4..4] of TAlign = (
    alLeft, alLeft, alTop, alTop, alRight, alBottom, alBottom, alLeft, alLeft
  );
begin
(* Determine alignment from the position of the mouse within ZoneRect.
  ZoneRect in screen TLBR coordinates, MousePos in screen coordinates.
*)
//center and extent of dock zone
  cx := (ZoneRect.Right + ZoneRect.Left) div 2;
  cy := (ZoneRect.Top + ZoneRect.Bottom) div 2;
  w := ZoneRect.Right - ZoneRect.Left;
  h := ZoneRect.Bottom - ZoneRect.Top;
  if (w > 0) and (h > 0) then begin
  //mouse position within k*k rectangles (squares)
    dx := trunc((MousePos.x - cx) / w * k);
    dy := trunc((MousePos.y - cy) / h * k);
    izone := max(abs(dx), abs(dy)); //0..k
  //map into 0=innermost (custom), 1=inner, 2=outer
    if izone = 0 then begin
      //zone := zInnermost;
      dir := alCustom; //pages
    end else begin
      phi := arctan2(dy, dx); //zero at East (right), increasing clockwise
      zphi := trunc(radtodeg(phi)) div 45; //both EN and ES are zero
      dir := cDir[zphi];
    end;
  end else
    dir := alClient;
  Result := dir;
end;

procedure TEasyDockManager.EndUpdate;
begin
  //inherited EndUpdate;
  dec(FUpdateCount);
  if FUpdateCount = 0 then
    Update;
end;

function TEasyDockManager.GetDockEdge(ADockObject: TDragDockObject): boolean;
var
  CanDock: boolean; //dummy
  r: TRect;
  s: string;
begin
(* Determine dock target(?) and align.
  Called with current DockRect (should use InfluenceRect?)
  Usage: Only to prevent calls to the target control, in case of extended
    InfluenceRect.
*)
//test
  exit(false);

  if false then
    Result:=inherited GetDockEdge(ADockObject);
{$IFDEF old}
  //ADockObject.DockRect
  Result := DockSite.DockClientCount = 0; //do nothing if docked clients exist!
  if Result then begin
    ADockObject.DropAlign := DetectAlign(ADockObject.DockRect, ADockObject.DragPos);
  {$IFDEF VerboseDrag}
    WriteStr(s, ADockObject.DropAlign);
    DebugLn('dockedge x(%d-%d) y(%d-%d) %s', [
      ADockObject.DockRect.Left, ADockObject.DockRect.Right,
      ADockObject.DockRect.Top, ADockObject.DockRect.Bottom, s
      ]);
  {$ELSE}
  {$ENDIF}
  end else begin
  //init a valid DockRect?
    r := ADockObject.DockRect;
    TWinControlAccess(FDockSite).GetSiteInfo(nil, r, ADockObject.DragPos, CanDock);
    ADockObject.DockRect := r;
    Result := CanDock;
  end;
{$ELSE}
  DebugLn('DropOnControl ', DbgS(ADockObject.DropOnControl));
  ADockObject.DropAlign := DetectAlign(ADockObject.DockRect, ADockObject.DragPos);
  WriteStr(s, ADockObject.DropAlign);
  Result := True;
  DebugLn('dockedge x(%d-%d) y(%d-%d) %s', [
    ADockObject.DockRect.Left, ADockObject.DockRect.Right,
    ADockObject.DockRect.Top, ADockObject.DockRect.Bottom, s
    ]);
{$ENDIF}
end;

procedure TEasyDockManager.PositionDockRect(Client, DropCtl: TControl;
  DropAlign: TAlign; var DockRect: TRect);
var
  wh: integer;
begin
(* DockRect is initialized to the screen rect of the dock site by TControl,
  or to the zone rect by TEasyTree.

  DropCtl=Nil means: dock into the root zone.
*)
  //if False then inherited PositionDockRect(Client, DropCtl, DropAlign, DockRect);
{$IFDEF old}
  if (DropCtl = nil) {$IFDEF singleTab} and not SingleTab {$ENDIF}  then
  //if (DropCtl = nil) then
    exit; //entire dock site
{$ELSE}
{$ENDIF}

  case DropAlign of
  //alClient: as is
  alTop:    DockRect.Bottom := (DockRect.Top + DockRect.Bottom) div 2;
  alBottom: DockRect.Top := (DockRect.Top + DockRect.Bottom) div 2;
  alLeft:   DockRect.Right := (DockRect.Left + DockRect.Right) div 2;
  alRight:  DockRect.Left := (DockRect.Left + DockRect.Right) div 2;
  alCustom: //pages
    begin
      wh := (DockRect.Right - DockRect.Left) div 3;
      inc(DockRect.Left, wh);
      dec(DockRect.Right, wh);
      wh := (DockRect.Bottom - DockRect.Top) div 3;
      inc(DockRect.Top, wh);
      dec(DockRect.Bottom, wh);
    end;
  end;
end;

procedure TEasyDockManager.PositionDockRect(ADockObject: TDragDockObject);
var
  r: TRect;
begin
(* Something is wrong here?
ADockObject:
  DockRect is the site rect, to be replaced by the target control/zone rect.
*)

//test
  exit;

  if false then inherited PositionDockRect(ADockObject);
  with ADockObject do begin;
  //check target
    if DropOnControl <> nil then begin
    //init control rect
      r.TopLeft := DropOnControl.ClientOrigin;
      r.Bottom := r.Top + DropOnControl.Height;
      r.Right := r.Left + DropOnControl.Width;
    end else begin
      r := DockRect;
    end;
    DetectAlign(r, DragPos);
  end;
end;

procedure TEasyDockManager.SetReplacingControl(Control: TControl);
begin
//nop
  //inherited SetReplacingControl(Control);
  FReplacingControl := Control;
end;

procedure TEasyDockManager.Update;
begin
  //nop
end;

{ TCustomDockSite }

class function TCustomDockSite.ReloadSite(AName: string;
  AOwner: TComponent): TCustomDockSite;
var
  ss: TStringStream;
  tn: string;
  //tc: TClass;
  ct: TPersistentClass;
  dst: TDockSiteClass absolute ct;
begin
(* Restore from typename, then restore clients.
  Called when AName[1]=CustomDockSiteID.
*)
  Result := nil;
  ss := TStringStream.Create(AName);
  try
    if ss.ReadByte <> CustomDockSiteID then begin
      ShowMessage('bad stream format');
      exit; //bad format
    end;
    tn := ss.ReadAnsiString;
    ct := GetClass(tn);
    if ct = nil then begin
      ShowMessage('class not registered: ' + tn);
      exit;
    end;
    if ct.InheritsFrom(TCustomDockSite) then begin
      Result := dst.Create(AOwner);
      Result.LoadFromStream(ss);
    end;
  finally
    ss.Free;
  end;
end;

procedure TCustomDockSite.ShowForm(AForm: TControl);
begin
(* Notebooks activate this control
*)
  AForm.Visible := True;
end;

function TCustomDockSite.SaveSite: string;
var
  ss: TStringStream;
begin
(* Save typename and clients.
  Write to string:
  - CustomDockSiteID
  - ClassName
  - content (SaveToStream)
*)
  ss := TStringStream.Create('');
  try
    ss.WriteByte(CustomDockSiteID);
    ss.WriteAnsiString(ClassName);
    SaveToStream(ss);
    Result := ss.DataString;
  finally
    ss.Free;
  end;
end;

procedure TCustomDockSite.SaveToStream(strm: TStream);
var
  i, n: integer;
  ctl: TControl;
  s: string;
begin
(* Save all docked controls.
  Flag nested custom dock sites - how?
*)
  if assigned(DockManager) then begin
    DockManager.SaveToStream(strm);
    exit;
  end;
{$IFDEF new}
(* requires:
- flag unmanaged
- ctl.BoundsRect (within site!) -> translate for ManualDock
*)
  n := DockClientCount;
  strm.WriteByte(n);
  for i := 0 to n-1 do begin
    ctl := DockClients[i];
    s := DockLoader.SaveControl(ctl, self);
    strm.WriteAnsiString(s);
  end;
{$ELSE}
  //unmanaged docksites unhandled
{$ENDIF}
end;

procedure TCustomDockSite.LoadFromStream(strm: TStream);
var
  i, n: integer;
  ctl: TControl;
  cn: string;
begin
  if assigned(DockManager) then begin
    DockManager.LoadFromStream(strm);
    exit;
  end
{$IFDEF new}
//see SaveToStream!
  n := strm.ReadByte;
  for i := 1 to n do begin
    cn := strm.ReadAnsiString;
    ctl := DockLoader.ReloadControl(cn, self);
    if ctl <> nil then
      ctl.ManualDock(self); //orientation???
    //make visible?
  end;
{$ELSE}
  //unmanaged docksites unhandled
{$ENDIF}
end;

{ TCustomDockMaster }

function TCustomDockMaster.SaveControl(Control: TControl;
  Site: TWinControl): string;
begin
(* Create string descriptor for docked control.
*)
  Result := '';
  if Control is TCustomDockSite then
    Result := TCustomDockSite(Control).SaveSite
  else if Assigned(AppLoadStore)
  and AppLoadStore(alsSaveControl, Site, Control, Result) then begin
    //all done
  end else if Assigned(FOnSave) then
    Result := FOnSave(Control);
//last resort
  if Result = '' then begin
    CtlToRec(Control);
    Result := RecToId;
  end;
  //Result := Control.Name; //definitely child.Name
end;

procedure TCustomDockMaster.CtlToRec(ctl: TControl);
begin
//fill record with all names
  rc.Ctrl := ctl;
  rc.ClassName := ctl.ClassName;
  rc.Name := ctl.Name;
  rc.Caption := ctl.Caption; //general purpose
end;

procedure TCustomDockMaster.IdToRec(const ID: string);
var
  i: integer;
begin
  rc.Ctrl := nil; //flag: not yet found/created
//split <Name>':'<ClassName>'='<Caption>
  i := Pos(':', ID);
  if i > 0 then begin
    rc.Name := Copy(ID, 1, i-1);
    rc.ClassName := Copy(ID, i+1, Length(ID));
    i := Pos('=', rc.ClassName);
    if i > 0 then begin
      rc.Caption := Copy(rc.ClassName, i+1, Length(rc.Caption));
      SetLength(rc.ClassName, i-1);
    end else
      rc.Caption := rc.Name;
  end else begin
    rc.Caption := ID;
    rc.ClassName := 'T'+ID;
  //strip spaces from Caption
    rc.Name := StringReplace(rc.Caption, ' ', '', [rfReplaceAll]);
  end;
end;

function TCustomDockMaster.RecToId: string;
begin
  Result := rc.Name + ':' + rc.ClassName + '=' + rc.Caption
end;

function TCustomDockMaster.MakeDockable(AForm: TWinControl; fWrap: boolean;
  fVisible: boolean): TForm;
var
  wctl: TWinControlAccess absolute AForm;
begin
(* Result is the floating site, if created (fWrap).
  fWrap here is ignored.
To be overridden by final TDockMaster, for wrapping.
*)
//make it dockable
  wctl.DragKind := dkDock;
  wctl.DragMode := dmAutomatic;
  if fVisible then
    AForm.Show;
  Result := nil; //not wrapped
end;

function TCustomDockMaster.ReloadControl(const AName: string;
  Site: TWinControl): TControl;
var
  fc: TWinControlClass;
  fo: TComponent; //form owner
  cmp: TComponent absolute Result;
begin
(* Reload from:
- CustomDockSite (allow for nested layouts)
- AppLoadStore
- OnRestore
- our Owner
- create from descriptor (optionally)
- DockSite
*)
  Result := nil;
  IdToRec(AName); //make all information accessible to external subroutines
//first check for special CustomDockSite format
  if ord(AName[1]) = CustomDockSiteID then
    Result := TCustomDockSite.ReloadSite(AName, Site)
  else if assigned(AppLoadStore)
  and AppLoadStore(alsReloadControl, Site, Result, rc.Name) then begin
    //all done - string-argument questionable!
  end else if assigned(FOnRestore) then
    Result := FOnRestore(rc.Name, Site);
  if Result <> nil then
    exit;
//check existing control
  fo := Owner; //assume all freely dockable controls are owned by our owner (Application)
  cmp := FindOwnedComponent(rc.Name, fo);
  //if Result <> nil then
  if Result is TControl then
    exit;
  Result := nil; //exclude non-controls
//create from descriptor?
  if TryCreateControls then begin
    fc := TWinControlClass(GetClass(rc.ClassName));
    if not assigned(fc) then begin
      DebugLn(rc.ClassName, ' is not a registered class');
      //exit(nil); //bad form name
    end else begin
    //init from descriptor
      Result := fc.Create(fo);
      if Result.Name <> rc.Name then
        TryRename(Result, rc.Name);
      if Result.Caption <> rc.Caption then
        Result.Caption := rc.Caption; //really?
    end;
  end;
//last resort
  if Result = nil then
    TWinControlAccess(Site).ReloadDockedControl(rc.Name, Result);
end;

initialization
{$I easy_dock_images.lrs}
  //DefaultDockManagerClass := TEasyTree;
  CreateDockHeaderImages;
  if DockLoader = nil then
    DockLoader := TCustomDockMaster.Create(Application);
finalization
  DestroyDockHeaderImages;
  //FreeAndNil(LoadStore); - by owner!
end.


