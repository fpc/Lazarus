unit EasyDockSite;
(* A tree docking manager by DoDi <DrDiettrich1@aol.com>.

This project can be used instead of the LDockTree manager.

To be added or ported:
  - field and method argument names
  - persistence

More issues, concerning the rest of the LCL (mainly unit Controls):

LCL TODO:
=========
  LCL does not handle existing (or added) controls in a dock site. This results
  in interference of docked and "resident" controls, handled by different
  layout/docking managers.
  Delphi moves all controls into the docked clients list, as soon as a TWinControl
  becomes a DockSite.

  LCL does not handle docking managers as interfaces.

  LCL undocks a control when it just is clicked.

  LCL controls don't notify the dock manager about visibility changes.

done? (unclear whether this is really fixed in the trunk)
=====
  LCL does not notify the docking manager of a resized dock site.
  Should call ResetBounds().
*)

{$H+}

{$DEFINE splitter_color} //use colored splitter, for debugging?
{.$DEFINE NoDrop} //patched dragobject?

interface

uses
  LCLIntf, //TRect
  LCLType, //HDC
  LMessages, //TLMessage
  Classes, //TStream
  Forms,
  ExtCtrls, //splitter
  Controls,
  ComCtrls, //TPageControl
  EasyDockHelpers;

type
  TEasyTree = class; //forward declaration

  { TEasyZone }

  TEasyZone = class(TCustomDockZone)
  private
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
  (* Beware: Lazarus tends to insert a private member FChildControl,
    which hides the inherited member of the same name!
  *)
    procedure InsertAfter(LinkAfter, NewZone: TEasyZone);
      //do not handle orientation!
    procedure SetParent(zone: TEasyZone);
      //unlink
  protected
    BR: TPoint;
    procedure SetBounds(TLBR: TRect);

    function  GetHandle: HWND; override;
    function  GetHeaderSize: integer; override;
    function  GetVisible: boolean;
    function  GetVisibleControl: TControl;
    function  GetPartRect(APart: TEasyZonePart): TRect;
  public
    constructor Create(ATree: TEasyTree);
    destructor Destroy; override;
    procedure Clear;
    function  DockSite: TWinControl;
    function  HasSizer: boolean; override;
    function  GetBounds: TRect; override;

    procedure AddSibling(NewZone: TEasyZone; where: TAlign);
    procedure ReplaceChild(OldChild, NewChild: TEasyZone);
    procedure ScaleTo(ptOld, ptNew, ptOuter: TPoint);
  public //properties
    property ChildControl: TControl read FChildControl write SetControl;
    property FirstChild: TEasyZone read FFirstChild;
    property NextSibling: TEasyZone read FNextSibling;
    property PrevSibling: TEasyZone read FPrevSibling;
    property Parent: TEasyZone read FParent write SetParent;
    property Orientation: TDockOrientation read FOrientation write SetOrientation;
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
    function ZoneFromPoint(SitePos: TPoint): TEasyZone;
    procedure GetControlBounds(Control: TControl; out CtlBounds: TRect);  override;
    procedure InsertControl(Control: TControl; InsertAt: TAlign;
      DropCtl: TControl);  override;
    procedure PositionDockRect(Client, DropCtl: TControl; DropAlign: TAlign;
      var DockRect: TRect);  override;
    procedure RemoveControl(Control: TControl);  override;
    procedure ResetBounds(Force: Boolean);  override; //site resized
    procedure SetReplacingControl(Control: TControl); override; //unused, Delphi compatible
    procedure LoadFromStream(Stream: TStream);  override;
    procedure SaveToStream(Stream: TStream);  override;
  protected //added
    procedure PositionDockRect(ADockObject: TDragDockObject); override;
    property  DockSite: TWinControl read FDockSite;
    function  DockHeaderSize: integer; virtual;
    function  FindControlZone(zone: TEasyZone; Control: TControl): TEasyZone;
    procedure RemoveZone(Zone: TEasyZone);
  //Lazarus extension
  private
    FHeader: TEasyDockHeader;
    FSplitter: TEasySplitter;
    FSizeZone: TEasyZone; //zone to be resized, along with NextSibling
    procedure SplitterMoved(Sender: TObject); //hide and reposition zone
  public
    procedure MouseMessage(var Message: TLMessage); override;
  public
    constructor Create(ADockSite: TWinControl);
    destructor Destroy; override;
    procedure AdjustDockRect(Control: TControl; var ARect: TRect);
    procedure PaintSite(DC: HDC); override;
  end;

(* Notebook for alCustom docking.
  Added behaviour: free self on undock of the last client/page.
  The behaviour of TPageControl sucks :-(
*)
  TEasyBook = class(TPageControl)
  protected
    procedure DoDock(NewDockSite: TWinControl; var ARect: TRect); override;
    procedure DoRemoveDockClient(Client: TControl); override;
    function  GetDefaultDockCaption: string; override;
  public
    constructor Create(TheOwner: TComponent); override;
  end;

const
  AlignNames: array[TAlign] of string = (
    'alNone', 'alTop', 'alBottom', 'alLeft', 'alRight', 'alClient', 'alCustom'
  );

var //debug only
  DropOn: TControl;
  DockObj: TDragDockObject;

implementation

uses
  SysUtils, Types, Graphics,
  math,
  LCLproc; //debugging

const
  ParentChildOrientation: array[TDockOrientation] of TDockOrientation = (
    doNoOrient, doVertical, doHorizontal, doPages
  );

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

{ TEasyTree }

constructor TEasyTree.Create(ADockSite: TWinControl);

  procedure DockExisting;
  var
    i: integer;
    ctl: TControl;
  begin
    for i := 0 to DockSite.ControlCount - 1 do begin
      ctl := DockSite.Controls[i];
      InsertControl(ctl, ctl.Align, nil);
    end;
  end;

begin
  FDockSite := ADockSite;
//reset inappropriate docking defaults - should be fixed in Controls/DragManager!
  DragManager.DragImmediate := False;
  DragManager.DragThreshold:=5;
//workaround: check for already assigned docking manager
  //FreeAndNil(DockSite.DockManager); - seems to be fixed
  DockSite.DockManager := self;
//init node class - impossible due to visibility restrictions!

  inherited Create; //(DockSite);
//init top zone
  FSiteRect := DockSite.ClientRect; //handle resize of the dock site
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
{$ELSE}
{$ENDIF}
//handle controls, already residing in the site
  //DockExisting; //doesn't work in the current LCL :-(
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
    DebugLn('EndUpdate---');
    UpdateTree;
    //BuildDockLayout(FTopZone);
  end;
end;

function TEasyTree.ZoneFromPoint(SitePos: TPoint): TEasyZone;
var
  zone: TEasyZone;
begin
(* Return zone a site client coordinates.
*)
  zone := FTopZone;
  while zone <> nil do begin
    if (SitePos.X > zone.Right) or (SitePos.Y > zone.Bottom) then
      zone := zone.NextSibling
    else if zone.FirstChild <> nil then
      zone := zone.FirstChild
    else begin
      break; //here?
    end;
  end;
  Result := zone;
end;

function TEasyTree.DockHeaderSize: integer;
begin
  Result := FHeader.HeaderSize;
end;

procedure TEasyTree.AdjustDockRect(Control: TControl; var ARect: TRect);
begin
//get the client area within the given zone rectangle
//raw extimate, lacking exact (not yet existing) zone information
  if Control.DockOrientation = doVertical then
    inc(ARect.Top, DockHeaderSize)
  else
    inc(ARect.Left, DockHeaderSize);
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
    CtlBounds := zone.GetBounds;
  end;
end;

procedure TEasyTree.InsertControl(Control: TControl; InsertAt: TAlign;
  DropCtl: TControl);
var
  DropZone, OldZone, NewZone, OldParent, NewParent: TEasyZone;
  r: TRect;
  NoteBook: TPageControl;
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
  //hack for morphing DropCtl into notebook
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
*)
  if (InsertAt = alCustom) and (FTopZone.FirstChild <> nil) then begin
  //dock into book
    if not (DropCtl is TPageControl) then begin
    //create new book
      NoteBook := TEasyBook.Create(FDockSite);
      NoteBook.Align := alNone;
      NoteBook.DragKind := dkDock;
      NoteBook.DragMode := dmAutomatic;
    //hack: manually dock the notebook
      FReplacingControl := NoteBook; //ignore insert (see above)
      NoteBook.ManualDock(FDockSite); //move into DockClients[]
      DropZone.ChildControl := NoteBook; //put into the zone
    { TODO -cdocking : make the notebook take the desired position }
      r := DropZone.GetPartRect(zpClient);
      DebugLn('NoteBook as (%d,%d)-(%d,%d)', [r.Top, r.Left, r.Bottom, r.Right]);
      NoteBook.BoundsRect := r;
      r := NoteBook.BoundsRect;
      DebugLn('NoteBook is (%d,%d)-(%d,%d)', [r.Top, r.Left, r.Bottom, r.Right]);

      DropCtl.ManualDock(NoteBook); //put the original control into the notebook
      DropCtl := NoteBook; //put further controls into the notebook
      ResetBounds(True); //for some reason only setting the size doesn't work
      NoteBook.Update;
    end;
    Control.ManualDock(TPageControl(DropCtl));
    FDockSite.Invalidate;
    exit;
  end;


  NewZone := TEasyZone.Create(self);
  NewZone.ChildControl := Control as TControl;
  Control.Align := alNone; //hack: prevent reposition by host

  if FTopZone.FirstChild = nil then begin
  //special case: in root zone (empty dock site)
    FTopZone.InsertAfter(nil, NewZone);
    NewZone.SetBounds(FDockSite.ClientRect);
  end else begin //normal dock, along with existing zones
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
      Control.DockOrientation := ParentChildOrientation[DropCtl.DockOrientation];
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

  //check orientation - control orientation cannot be doNoOrient!
    OldParent := DropZone.Parent;
    (* One special case remains: top zone without orientation
    *)
    if (OldParent.Orientation = doNoOrient) then begin
      assert(OldParent = FTopZone, '???');
      FTopZone.Orientation := ParentChildOrientation[Control.DockOrientation]; //easy
    end;
  //iso or orthogonal insert?
    if (OldParent.Orientation <> ParentChildOrientation[Control.DockOrientation]) then begin
    //need intermediate zone, due to different alignment
      NewParent := TEasyZone.Create(self);
      NewParent.Orientation := Control.DockOrientation;
      NewParent.BR := r.BottomRight;
      OldParent.ReplaceChild(DropZone, NewParent); //unlink DropZone
    //now: orthogonal orientation
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
  FDockSite.Invalidate;
end;

procedure TEasyTree.LoadFromStream(Stream: TStream);
begin
  //todo
end;

procedure TEasyTree.MouseMessage(var Message: TLMessage);
var
  r: TRect;
  Part: TEasyZonePart;
  Control: TControl;
  MouseMsg: TLMMouse absolute Message;
  Zone: TEasyZone;

  function FindZone(fButtonDown: boolean): TEasyZonePart;
  var
    MousePos: TPoint;
  begin
    MousePos := SmallPointToPoint(MouseMsg.Pos);
    Zone := ZoneFromPoint(MousePos);
  //exact zone part
    if (Zone = nil) or (Zone.ChildControl = nil) then begin
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
    if z.Parent.Orientation = doHorizontal then begin
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
          if Control is TCustomForm then
            TCustomForm(Control).Close
          else // not a form => doesnot have close => just hide
            Control.Visible := False;
          DockSite.Invalidate;
        end;
      end;
    LM_LBUTTONDOWN:
      case FindZone(False) of
      zpCaption: // mouse down on not buttons => start drag
        Control.BeginDrag(False);
      end;
    LM_MOUSEMOVE:
      begin //what's Message.Keys???
        case FindZone(FHeader.MouseDown) of
        zpSizer:
          if Zone.HasSizer then begin
            ShowSplitter(zone);
          end;
        else
          //DockSite.Cursor := crDefault;
          FSplitter.Hide;
        end;
      end;
{
    CM_MOUSELEAVE: //reset states?
      CheckNeedRedraw(nil, Rect(0,0,0,0), ldhpAll);
}
  end;
end;

procedure TEasyTree.SplitterMoved(Sender: TObject);
var
  ptNew, ptOuter: TPoint;
begin
(* The unbound splitter has been moved.
  Reflect new sizes in FSizeZone and next sibling.
  ptOuter is the new parent extent, BR of the last sibling.
*)
  FSplitter.Hide;
  ptNew := FSizeZone.BR;
  if FSizeZone.Parent.Orientation = doHorizontal then
    ptNew.y := FSplitter.Top + FSplitter.Height //above (and including) splitter
  else
    ptNew.x := FSplitter.Left + FSplitter.Width; //left of splitter
  ptOuter := FSizeZone.Parent.BR; //not affected
  FSizeZone.ScaleTo(FSizeZone.BR, ptNew, ptNew);  // ptOuter);
  FSizeZone.NextSibling.SetBounds(FSizeZone.NextSibling.GetBounds); //BR unchanged, only update the control
{ TODO -cdocking : Invalidate seems to miss a repaint of the docked controls, sometimes? }
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
      //ACaption := z.ChildControl.Name;
      //ACaption := z.ChildControl.ClassName;
      //ACaption := z.ChildControl.DefaultDockCaption;
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

procedure TEasyTree.PositionDockRect(ADockObject: TDragDockObject);
var
  i: integer;
  zone: TEasyZone;

  function DetectAlign(ZoneSize, MousePos: TPoint): TAlign;
  var
    w, h, zphi: integer;
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
  //center of dock zone
    w := ZoneSize.x div 2;
    h := ZoneSize.y div 2;
  //mouse position within k*k rectangles (squares)
    dx := trunc((MousePos.x - w) / w * k);
    dy := trunc((MousePos.y - h) / h * k);
    izone := max(abs(dx), abs(dy)); //0..k
  //map into 0=innermost (custom), 1=inner, 2=outer
    if izone = 0 then begin
      //zone := zInnermost;
      dir := alCustom; //alClient?
    end else begin
    {
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
begin
(* New DockManager interface, called instead of the old version.
  Determine exact target (zone) and DropAlign.
Signal results:
  Prevent docking by setting DropOnControl=Control (prevent changes when dropped).
  DragTarget=nil means: become floating.

  Unfortunately there exists no way to signal invalid docking attempts :-(
*)
//determine the zone containing the DragTargetPos
  DockObj := ADockObject;
  with ADockObject do begin
  //mouse position within dock site
    DragTargetPos := DragTarget.ScreenToClient(DragPos);
  //find zone
    zone := ZoneFromPoint(DragTargetPos);

    if (zone = nil) or (Control = zone.ChildControl) then begin
      DropAlign := alNone; //prevent drop (below)
    end else begin
      DropOnControl := zone.ChildControl;
      if DropOnControl = nil then begin
        DropAlign := alClient; //first element in entire site
      end else begin
      //determined the alignment within the zone.
        DockRect := zone.GetBounds; //include header
        DropAlign := DetectAlign(zone.BR, DragTargetPos);
      //to screen coords
        DockRect.TopLeft := FDockSite.ClientToScreen(DockRect.TopLeft);
        DockRect.BottomRight := FDockSite.ClientToScreen(DockRect.BottomRight);
      end;
    end;
  //position DockRect
    if DropAlign = alNone then begin
      DropOnControl := Control; //prevent drop - signal drop onto self
    {$IFDEF NoDrop}
      NoDrop := True;
    {$ELSE}
      //DragTarget := FDockSite; //prevent floating - doesn't work :-(
      //DockRect := Rect(MaxInt, MaxInt, 0, 0); //LTRB - very strange effect!
      //DockRect := Rect(MaxInt, 0, MaxInt, 0); //LTRB
    {$ENDIF}
    end else begin
      PositionDockRect(Control, DropOnControl, DropAlign, DockRect);
    end;
  end;
end;

procedure TEasyTree.PositionDockRect(Client, DropCtl: TControl;
  DropAlign: TAlign; var DockRect: TRect);
var
  wh: integer;
begin
(* DockRect is initialized to the screen rect of the dock site by TControl,
  or to the zone rect by TEasyTree!

  We assume call by TEasyTree...
*)
//debug!
  DropOn := DropCtl;

  if (DropCtl = nil) then begin
    //DebugLn('no DropCtl');
    exit; //empty dock site
  end;
{
  with DockRect do
  DebugLn('drop onto %s[%d,%d - %d,%d] %s', [
      DropCtl.Name, Top, Left, Bottom, Right, AlignNames[DropAlign]
      ]);
}
  case DropAlign of
  //alClient: as is
  alTop:    DockRect.Bottom := (DockRect.Top + DockRect.Bottom) div 2;
  alBottom: DockRect.Top := (DockRect.Top + DockRect.Bottom) div 2;
  alLeft:   DockRect.Right := (DockRect.Left + DockRect.Right) div 2;
  alRight:  DockRect.Left := (DockRect.Left + DockRect.Right) div 2;
  alCustom:
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
//how to determine old bounds? We use saved size in FSiteRect.
  rNew := FDockSite.ClientRect;
  if not Force and not CompareMem(@rNew, @FSiteRect, sizeof(rNew)) then
    Force := True;  //something has changed
  if not Force then
    exit;
  FTopZone.ScaleTo(FSiteRect.BottomRight, rNew.BottomRight, rNew.BottomRight);
  FSiteRect := rNew;
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
    s := Format('%s%s (%d,%d)-(%d,%d)%s', [ind, OrientString[zone.orientation], //zone.Limit,
      r.Top, r.Left, r.Bottom, r.Right, eol]);
    Stream.Write(s[1], length(s));
  //control
    ctl := zone.ChildControl;
    if ctl <> nil then begin
      r := ctl.BoundsRect;
      s := Format('%s%s %s.%s (%d,%d)-(%d,%d)%s', [ind, OrientString[zone.orientation],
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
(* This is a debug version of SaveToStream.
  It only produces a readable representation of the internal structure,
  not suited for restoring the layout with LoadFromStream.
*)
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

procedure TEasyTree.UpdateTree;
begin
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
      ch.ChildControl.DockOrientation := ParentChildOrientation[zone.Parent.Orientation];
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
  if affected <> nil then begin
    Zone := affected.FirstChild;
    while Zone <> nil do begin
      if Zone.NextSibling = nil then
        Zone.BR := affected.BR; //resize last zone
      if Zone.ChildControl <> nil then begin
        Zone.ChildControl.BoundsRect := Zone.GetPartRect(zpClient);
      end;
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

function TEasyZone.GetHeaderSize: integer;
begin
  Result := FTree.DockHeaderSize;
end;

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
  if ChildControl <> nil then
    Result := TEasyDockHeader.GetRectOfPart(GetBounds, ChildControl.DockOrientation,
      APart, HasSizer)
  else
    Result := Rect(0,0,0,0);
end;

function TEasyZone.HasSizer: boolean;
begin
(* New sizer at bottom/right of zone (was: part of header)
*)
  Result := NextSibling <> nil;
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
end;

procedure TEasyZone.AddSibling(NewZone: TEasyZone; where: TAlign);
var
  LinkAfter: TEasyZone;
  r, r2: TRect;
  NewOrientation: TDockOrientation; //of the child control
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
    Parent.Orientation := ParentChildOrientation[NewOrientation];
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
  else if Parent.Orientation <> doVertical then begin
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
    //r := GetBounds;
    //FTree.AdjustDockRect(ChildControl, r);
    ChildControl.BoundsRect := GetPartRect(zpClient);
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
// In a parent zone of vertical orientation the zone.PrevSibling.Bottom is zone.Left
  zone := self;
  while zone.Parent <> nil do begin
    if (fTop = (zone.Parent.Orientation = doHorizontal)) then begin
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
    zone := zone.Parent;
  end;
//reached top zone
  Result := 0;
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
    ChildControl.BoundsRect := GetPartRect(zpClient);
  end else if FirstChild <> nil then begin
    z := FirstChild;
    while z <> nil do begin
    //resize - for splitter move only!
      if Orientation = doHorizontal then //left/right changed
        z.BR.x := TLBR.Right
      else
        z.BR.y := TLBR.Bottom;
      z.SetBounds(z.GetBounds);
      z := z.NextSibling;
    end;
  end; //else empty root zone?
end;

{ TEasyBook }

constructor TEasyBook.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
//this does not help :-(
  DragKind := dkDock;
  DragMode := dmAutomatic;
end;

procedure TEasyBook.DoDock(NewDockSite: TWinControl; var ARect: TRect);
begin
  //test: do nothing
  //inherited DoDock(NewDockSite, ARect);
  //DebugLn('NoteBook as (%d,%d)-(%d,%d)', [ARect.Top, ARect.Left, ARect.Bottom, ARect.Right]);
  //BoundsRect := ARect;
  //DebugLn('NoteBook is (%d,%d)-(%d,%d)', [Top, Left, Height, Width]);
end;

procedure TEasyBook.DoRemoveDockClient(Client: TControl);
begin
(* Destroy notebook when it becomes empty.
  Notebook clients are organized in pages, not in dock clients.
  Hence we have to test for PageCount, instead of DockClientCount.
*)
  inherited;
  //DebugLn('TEasyBook.DoRemoveDockClient: remaining ' + IntToStr(PageCount));
  if PageCount = 0 then
    Application.ReleaseComponent(self);
end;

function TEasyBook.GetDefaultDockCaption: string;
var
  i: integer;
  pg: TTabSheet;
begin
  Result := '';
  for i := 0 to PageCount - 1 do begin
    pg := Pages[i];
    if Result = '' then
      Result := pg.Caption
    else
      Result := Result + ' ' + pg.Caption;
  end;
end;

end.

