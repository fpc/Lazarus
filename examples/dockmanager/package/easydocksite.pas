unit EasyDockSite;
(* A tree docking manager by DoDi <DrDiettrich1@aol.com>.

This project can be used instead of the LDockTree manager.

To be added or ported:
  - field and method argument names

Possible extensions:
  - separate docking management and dock site layout
  - various dock headers
  - multiple splitters (on zones without controls)
  - persistence (requires application wide management of dock sources!)
  - purpose of Restore button?

Known bugs:
  - Problem with dragging header, seems to interfere with dragmanager (capture)?

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
*)

{$H+}

//{$DEFINE newSplitter} //exclude splitter from remaining zone
{.$DEFINE handle_existing} //dock controls existing in the dock site?
{.$DEFINE splitter_color} //use colored splitter, for debugging?
{.$DEFINE visibility} //handling of invisible clients deserves dock manager notification!
{.$DEFINE restore} //restore button?
  //looks useless: how to restore a hidden zone?
{.$DEFINE singleTab} //allow to create notebooks with 1 tab (in the topzone)?
  //doesn't look nice, with both a header AND a button list

//depending on widgetset or patched LCL
{.$DEFINE NoDrop} //applied DoDiPatch1?

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
    hsForm      //form style
  );

  TEasyDockHeader = class
  public
    //HeaderSize: integer;
  //state last drawn
    MouseZone: TEasyZone;
    MouseDown: boolean;
    MousePart: TEasyZonePart;
    PartRect: TRect;
  public
    //Style: TEasyHeaderStyle;
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
    FOrientation: TDockOrientation;
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
  {$IFDEF old}
    function  GetHeaderSize: integer;
  {$ELSE}
  {$ENDIF}
    function  GetVisible: boolean;
    function  GetVisibleControl: TControl;
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
    property Visible: boolean read GetVisible;

    property Bottom: integer read BR.Y;
    property Left: integer read GetLeft;
    property Right: integer read BR.X;
    property Top: integer read GetTop;
  end;


  { TEasyTree }

  TEasyTree = class(TDockManager)
  private
    FDockSite: TWinControl;
    FReplacingControl: TControl;
    FTopZone: TEasyZone;
    FSiteRect: TRect; //to detect changed site extent
    FUpdateCount: integer;
    procedure UpdateTree;
  protected
    procedure BeginUpdate; override;
    procedure EndUpdate; override;
  //extended interface
    //procedure ControlVisibilityChanged(Control: TControl; Visible: Boolean);  override;
    function ZoneFromPoint(SitePos: TPoint): TEasyZone;
    procedure GetControlBounds(Control: TControl; out CtlBounds: TRect);  override;
    procedure InsertControl(Control: TControl; InsertAt: TAlign;
      DropCtl: TControl);  override;
    procedure PositionDockRect(Client, DropCtl: TControl; DropAlign: TAlign;
      var DockRect: TRect);  override;
    procedure RemoveControl(Control: TControl);  override;
    procedure ResetBounds(Force: Boolean);  override; //site resized
    procedure SetReplacingControl(Control: TControl); override; //unused
    procedure LoadFromStream(Stream: TStream);  override;
    procedure SaveToStream(Stream: TStream);  override;
  protected //added
    procedure PositionDockRect(ADockObject: TDragDockObject); override;
    property  DockSite: TWinControl read FDockSite;
  {$IFDEF old}
    function  DockHeaderSize: integer; virtual;
  {$ELSE}
  {$ENDIF}
    function  FindControlZone(zone: TEasyZone; Control: TControl): TEasyZone;
    procedure RemoveZone(Zone: TEasyZone);
  //Lazarus extension
  private
    FHeader: TEasyDockHeader;
    FStyle: TEasyHeaderStyle;
    FSplitter: TEasySplitter;
    FSizeZone: TEasyZone; //zone to be resized, also PrevSibling
    procedure SplitterMoved(Sender: TObject); //hide and reposition zone
  public
    procedure MessageHandler(Sender: TControl; var Message: TLMessage); override;
  public
  {$IFDEF singleTab}
    SingleTab: boolean; //always create notebook for alCustom?
  {$ENDIF}
    constructor Create(ADockSite: TWinControl);
    destructor Destroy; override;
  {$IFDEF old}
    procedure AdjustDockRect(Control: TControl; var ARect: TRect);
  {$ELSE}
  {$ENDIF}
    procedure PaintSite(DC: HDC); override;
    procedure SetStyle(NewStyle: TEasyHeaderStyle);
  end;

const
  AlignNames: array[TAlign] of string = (
    'alNone', 'alTop', 'alBottom', 'alLeft', 'alRight', 'alClient', 'alCustom'
  );
  PartNames: array[TEasyZonePart] of string =
  (
    'zpNowhere',        // not in any zone
    'zpClient',         // on client control
    'zpAll',            // total header rect
    'zpCaption',        // header caption
    'zpSizer',          // splitter/sizer
  {$IFDEF restore}
    'zpRestoreButton',  // header restore button
  {$ENDIF}
    'zpCloseButton'     // header close button
  );

var //debug only
  DropOn: TControl;
  DockObj: TDragDockObject;

implementation

uses
  SysUtils, Types,
  math,
  fDockBook,
  Themes, LResources,
  LCLproc; //debugging

const
{$IFDEF restore}
  HeaderButtons = [zpCloseButton, zpRestoreButton];
{$ELSE}
  HeaderButtons = [zpCloseButton];
{$ENDIF}

type
  TEasyBook = TEasyDockBook;

function  NoteBookCreate(AOwner: TWinControl): TEasyBook; inline;
begin
  Result := TEasyDockBook.Create(AOwner);
end;

procedure NoteBookAdd(ABook: TEasyBook; AItem: TControl); inline;
begin
  //AItem.ManualDock(ABook.pnlDock);
  AItem.ManualDock(ABook);
end;

//from CustomFormEditor.pp
function {TCustomFormEditor.}CreateUniqueComponentName(const AClassName: string;
  OwnerComponent: TComponent): string;
var
  i, j: integer;
begin
  Result:=AClassName;
  if (OwnerComponent=nil) or (Result='') then exit;
  i:=1;
  while true do begin
    j:=OwnerComponent.ComponentCount-1;
    Result:=AClassName;
    if (length(Result)>1) and (Result[1]='T') then
      Result:=RightStr(Result,length(Result)-1);
    if Result[length(Result)] in ['0'..'9'] then
      Result:=Result+'_';
    Result:=Result+IntToStr(i);
    while (j>=0)
    and (CompareText(Result,OwnerComponent.Components[j].Name)<>0) do
      dec(j);
    if j<0 then exit;
    inc(i);
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
  FDockSite := ADockSite;
//reset inappropriate docking defaults - should be fixed in Controls/DragManager!
  DragManager.DragImmediate := False;
//workaround: check for already assigned docking manager
  //FreeAndNil(DockSite.DockManager); - seems to be fixed
  DockSite.DockManager := self;
//init node class - impossible due to visibility restrictions!
  inherited Create; //(DockSite);
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

procedure TEasyTree.BeginUpdate;
begin
  inc(FUpdateCount);
end;

procedure TEasyTree.EndUpdate;
begin
  dec(FUpdateCount);
  if (FUpdateCount = 0) and (FTopZone.FirstChild <> nil) then begin
    //DebugLn('EndUpdate---');
    UpdateTree;
  end;
end;

function TEasyTree.ZoneFromPoint(SitePos: TPoint): TEasyZone;
var
  zone: TEasyZone;
begin
(* Return zone from site client coordinates.
*)
  zone := FTopZone;
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

{$IFDEF old}
function TEasyTree.DockHeaderSize: integer;
begin
  Result := FHeader.HeaderSize;
end;
{$ELSE}
{$ENDIF}

{$IFDEF old}
procedure TEasyTree.AdjustDockRect(Control: TControl; var ARect: TRect);
begin
//get the client area within the given zone rectangle
  ARect := FHeader.GetRectOfPart(ARect, Control.DockOrientation, zpClient, true);
end;
{$ELSE}
{$ENDIF}

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

procedure TEasyTree.InsertControl(Control: TControl; InsertAt: TAlign;
  DropCtl: TControl);
var
  DropZone, OldZone, NewZone, OldParent, NewParent: TEasyZone;
  r: TRect;
  NoteBook: TEasyBook;
(* special cases:
  1) first child in top zone - no orientation
  2) second child in top zone - determines orientation
In all other cases all zones have an orientation:
  3) insert isogonal
  4) insert orthogonal

Cases 2 and 3 can be merged, with an additional or redundant setting of the orientation.
*)

begin
  if Control = FReplacingControl then begin
  (* hack for morphing DropCtl into notebook,
    or initial docking of undocked controls in the dock site.
    Everything is done by the caller, after ManualDock.
  *)
    FReplacingControl := nil;
    exit;
  end;

//some checks
  if (Control = nil) or (not Control.Visible) or (DropCtl = Control) then begin
  //bug? The dock site is changed when a control is dropped onto itself!?
    DebugLn('Redock-----');
    exit; //nothing changed
  end;

  if Control.Name = '' then //name it - for header caption
    Control.Name := CreateUniqueComponentName(Control.ClassName, Control.Owner);

  if DropCtl = nil then begin
    DropCtl := FDockSite; //the dock site
    DropZone := FTopZone.FirstChild;
  end else begin
    DropZone := FindControlZone(FTopZone, DropCtl);
    if DropZone = nil then exit; //not here!?
  end;

(* alCustom means: drop into notebook.
  Valid only when dropped onto an existing control, not into empty dock site.
  Create notebook, if required (put both controls into new notebook).

  Try: create notebook already for first dropped control.
*)
  if (InsertAt = alCustom) then begin
  //dock into book
    if (FTopZone.FirstChild <> nil) then begin
    //root zone is not empty
      if (DropCtl is TEasyBook) then begin
        NoteBook := DropCtl as TEasyBook;
      end else begin
      //create new book
        NoteBook := NoteBookCreate(FDockSite);
        NoteBook.ManualDock(nil, nil);
      //hack: manually dock the notebook
        FReplacingControl := NoteBook; //ignore insert (see above)
        NoteBook.ManualDock(FDockSite); //move into DockClients[]
        DropZone.ChildControl := NoteBook; //put into the zone
        r := DropZone.GetPartRect(zpClient);
        NoteBookAdd(NoteBook, DropCtl); //put the original control into the notebook
        NoteBook.BoundsRect := r;
      end; //else use existing control
      NoteBookAdd(NoteBook, Control);
      FDockSite.Invalidate; //update notebook caption
      exit;
  {$IFDEF singleTab}
    end else if SingleTab and not (DropCtl is TEasyBook)  then begin
    //empty root zone, create new notebook
      NoteBook := NoteBookCreate(FDockSite);
      NoteBook.ManualDock(FDockSite, nil, alClient);
      NoteBookAdd(NoteBook, Control);
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
    FTopZone.InsertAfter(nil, NewZone);
    NewZone.SetBounds(FDockSite.ClientRect);
  end else begin
  //more checks
    OldZone := FindControlZone(FTopZone, Control);
      //check after placing the control
    r := DropZone.GetBounds; //for later adjustment
  //get requested orientation, adjust align
    case InsertAt of
    alTop, alBottom: Control.DockOrientation := doVertical;
    alLeft, alRight: Control.DockOrientation := doHorizontal;
    else //unhandled or unspecific
      if DropCtl.DockOrientation = doNoOrient then
        DropCtl.DockOrientation := doHorizontal; //assume
      Control.DockOrientation := DropCtl.DockOrientation;
    //fix alignment
      if Control.DockOrientation = doVertical then
        InsertAt := alBottom
      else
        InsertAt := alRight;
    end;
    (* Now Control.DockOrientation is the insert orientation,
      DropCtl.DockOrientation is the zone orientation,
      InsertAt is one of alLeft/Right/Top/Bottom
    *)

  //check orientation - control orientation cannot be doNone!
    OldParent := DropZone.Parent;
    (* One special case remains: top zone without orientation
    *)
    if (OldParent.Orientation = doNoOrient) then begin
      assert(OldParent = FTopZone, '???');
      FTopZone.Orientation := Control.DockOrientation; //easy
    end;
  //iso or orthogonal insert?
    if (OldParent.Orientation <> Control.DockOrientation) then begin
    //need intermediate zone
      NewParent := TEasyZone.Create(self);
      NewParent.Orientation := Control.DockOrientation;
      NewParent.BR := r.BottomRight;
      OldParent.ReplaceChild(DropZone, NewParent); //unlink DropZone
    //orthogonal orientation
      NewParent.InsertAfter(nil, DropZone);
    end;
  //set control orientation
    DropZone.AddSibling(NewZone, InsertAt);
  //clear eventually moved zone
    if OldZone <> nil then begin
      RemoveZone(OldZone); //must NOT modify moved control!
      OldZone.Free;
    end;
  end;
  ResetBounds(True); //splitters may have to be inserted
end;

procedure TEasyTree.PositionDockRect(ADockObject: TDragDockObject);
var
  i: integer;
  zone: TEasyZone;

  function DetectAlign(ZoneRect: TRect; MousePos: TPoint): TAlign;
  var
    w, h, zphi: integer;
    cx, cy: integer;
    dx, dy: integer;
    phi: double;
    izone: integer;
    //zone: eZone;
    dir: TAlign;
  const
    k = 5; //matrix dimension
  //mapping octants into aligns, assuming k=5
    cDir: array[-4..4] of TAlign = (
      alLeft, alLeft, alTop, alTop, alRight, alBottom, alBottom, alLeft, alLeft
    );
  begin
  //center and extent of dock zone
    cx := (ZoneRect.Right + ZoneRect.Left) div 2;
    cy := (ZoneRect.Top + ZoneRect.Bottom) div 2;
    w := ZoneRect.Right - ZoneRect.Left;
    h := ZoneRect.Bottom - ZoneRect.Top;
  //mouse position within k*k rectangles (squares)
    dx := trunc((MousePos.x - cx) / w * k);
    dy := trunc((MousePos.y - cy) / h * k);
    izone := max(abs(dx), abs(dy)); //0..k
  //map into 0=innermost (custom), 1=inner, 2=outer
    if izone = 0 then begin
      //zone := zInnermost;
      dir := alCustom; //pages
    end else begin
    { not yet: outer zones, meaning docking into parent zone
      if izone >= k-1 then
        zone := zOuter
      else //if izone > 0 then
        zone := zInner;
    }
      phi := arctan2(dy, dx);
      zphi := trunc(radtodeg(phi)) div 45;
      dir := cDir[zphi];
    end;
    Result := dir;
  end;

var
  ZoneExtent: TPoint;
  ADockRect: TRect;
begin
(* New DockManager interface, called instead of the old version.
  Determine exact target (zone) and DropAlign.
Signal results:
  Prevent docking by setting DropOnControl=Control (prevent changes when dropped).
  DragTarget=nil means: become floating.

  Unfortunately there exists no way to signal invalid docking attempts :-(
*)
{ TODO -cdocking : why is this method not called for a docksite with 1 client?
  If exactly the client is moved over it?
}
//debug only
  DockObj := ADockObject;

//determine the zone containing the DragTargetPos
  with ADockObject do begin
  //mouse position within dock site
    DragTargetPos := DragTarget.ScreenToClient(DragPos);
  //find zone
    zone := ZoneFromPoint(DragTargetPos);

    if (zone = nil) or (Control = zone.ChildControl) then begin
      DropAlign := alNone; //prevent drop (below)
    end else begin
      ADockRect := zone.GetBounds; //include header
      DropOnControl := zone.ChildControl;
      DropAlign := DetectAlign(ADockRect, DragTargetPos);
      if DropOnControl = nil then begin
      {$IFDEF singleTab}
        if SingleTab and (DropAlign = alCustom) then begin
          //notebook in top zone
        end else
      {$ENDIF}
          DropAlign := alClient; //first element in entire site
      end; //else //determine the alignment within the zone.
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

procedure TEasyTree.PositionDockRect(Client, DropCtl: TControl;
  DropAlign: TAlign; var DockRect: TRect);
var
  wh: integer;
begin
(* DockRect is initialized to the screen rect of the dock site by TControl,
  or to the zone rect by TEasyTree.
*)
//debug!
  DropOn := DropCtl;

  if (DropCtl = nil) {$IFDEF singleTab} and not SingleTab {$ENDIF}  then
    exit; //empty dock site

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

procedure TEasyTree.LoadFromStream(Stream: TStream);
begin
  //todo
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
          Control.ManualDock(nil, nil, alNone); //do float
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
          Control.BeginDrag(False); //doesn't work!?
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
  ACanvas.Handle := DC;
  GetCursorPos(MousePos);
  MousePos := DockSite.ScreenToClient(MousePos);
  PaintZone(FTopZone);
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
  if FTopZone.FirstChild = nil then
    exit; //zone is empty, nothing to do
//how to determine old bounds?
  rNew := FDockSite.ClientRect;
  if not CompareMem(@rNew, @FSiteRect, sizeof(rNew)) then
    Force := True;  //something has changed
  if not Force then
    exit;
  FTopZone.ScaleTo(FSiteRect.BottomRight, rNew.BottomRight, rNew.BottomRight);
  FSiteRect := rNew;
  FSplitter.Hide;
  FDockSite.Invalidate; //force repaint of headers
end;

procedure TEasyTree.SaveToStream(Stream: TStream);
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
  //FReplacingControl := Control;
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
  r: TRect;
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
  zone without parent (top) - exit
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
      ch.ChildControl.DockOrientation := zone.Parent.Orientation;
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
//update parent(?) zone, to close the gap
  //Zone := p.FirstChild;
  if affected <> nil then begin
    Zone := affected.FirstChild;
    while Zone <> nil do begin
      if Zone.NextSibling = nil then
        //Zone.BR := p.BR; //resize last zone
        Zone.BR := affected.BR; //resize last zone
    {$IFDEF old}
      if Zone.ChildControl <> nil then begin
        r := Zone.GetBounds;
        AdjustDockRect(Zone.ChildControl, r);
        Zone.ChildControl.BoundsRect := r;
      end;
    {$ELSE}
      //r := zone.GetPartRect(zpClient);
      zone.PositionControl;
    {$ENDIF}
      zone := Zone.NextSibling;
    end;
  end;
//update zone, here simply the whole dock site
  FSplitter.Hide;
  DockSite.Invalidate;
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

{$IFDEF old}
function TEasyZone.GetHeaderSize: integer;
begin
  Result := FTree.DockHeaderSize;
end;
{$ELSE}
{$ENDIF}

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
  if ChildControl is TEasyBook then
    Result := hsMinimal //or none at all?
  else
    Result := FTree.FStyle;
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
  else assert(False, 'unhandled insertion');
    LinkAfter := nil; //must never happen!
  end;
  Parent.InsertAfter(LinkAfter, NewZone);
//resize?
  r2 := r;
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
//parent orientation? (if in rootzone)
  //if parent.Orientation = doNoOrient then
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
  r: TRect;
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
  {$IFDEF old}
    r := GetBounds;
    FTree.AdjustDockRect(ChildControl, r);
    ChildControl.BoundsRect := r;
  {$ELSE}
    PositionControl;
  {$ENDIF}
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
        if prev.Visible then begin
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

initialization
{$I easy_dock_images.lrs}
  CreateDockHeaderImages;
finalization
  DestroyDockHeaderImages;
end.

