unit uMakeSite;
(* Create elastic dock sites within a form, make forms dockable.

Owners:
The DockMaster can own all floating forms, for easy enumeration.
The DockMaster can own all dock grips, for easy detection of the dockable forms.
The owner of the dockable forms is responsible for creating or finding dockable forms?

Problems:

Forms are not (easily) dockable on all platforms,
  we add a grabber icon to each dockable form,
  and wrap them in a managed floating form.

Default floating sites are owned by Application,
  we have to create the floating sites in the form.OnEndDock event.

  Owning panels is dangerous, they are not destroyed with their parent form!
*)

{$mode objfpc}{$H+}

{$DEFINE ownSites}  //floating sites owned by TDockMaster?
{$DEFINE ownGrips}  //docking grips owned by TDockMaster?

interface

uses
  Classes, SysUtils, Controls, Forms, ExtCtrls, EasyDockSite,
  fFloatingSite;

type
  sDockSides = TAlignSet;

  TDockPanel = class(TPanel)
  protected
    AutoExpand: boolean;  //do autoshrink?
    Splitter: TSplitter;    //associated
    procedure pnlDockDrop(Sender: TObject; Source: TDragDockObject;
      X, Y: Integer);
    procedure pnlDockOver(Sender: TObject; Source: TDragDockObject;
      X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure pnlGetSiteInfo(Sender: TObject; DockClient: TControl;
      var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
    procedure pnlUnDock(Sender: TObject; Client: TControl;
      NewTarget: TWinControl; var Allow: Boolean);
  public
  end;

(* The owner of all docksites (if ownSites is defined),
  and of all dockable window grips (if ownGrips is defined)
*)
  TDockMaster = class(TComponent)
  protected //event handlers
    procedure DockHandleMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure FormEndDock(Sender, Target: TObject; X, Y: Integer);
    function  WrapDockable(Client: TControl): TFloatingSite;
  public
    Factory: TComponent; //generic owner
    procedure AddElasticSites(AForm: TCustomForm; Sides: sDockSides);
    function  CreateDockable(const AName: string; fMultiInst: boolean; fWrap: boolean = True): TForm;
    procedure DumpSites;
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
  end;

implementation

uses
  LCLIntf, LCLProc;
  //fMasterSite,

type
  TWinControlAccess = class(TWinControl)
  end;

const
  PanelNames: array[TAlign] of string = (
    '', '', //alNone, alTop,
    'pnlBottom', 'pnlLeft', 'pnlRight',
    '', ''  //alClient, alCustom
  );

{ TDockMaster }

procedure TDockMaster.AddElasticSites(AForm: TCustomForm; Sides: sDockSides);
var
  side: TAlign;
  pnl: TDockPanel;
  spl: TSplitter;
  dm: TEasyTree;
const
  AllowedSides: sDockSides = [alLeft, alRight, alBottom];
begin
  for side := low(side) to high(side) do begin
    if (side in AllowedSides) and (side in Sides) then begin
      //TWinControlAccess(AForm).ReloadDockedControl(PanelNames[side], pnl);
      TComponent(pnl) := AForm.FindComponent(PanelNames[side]);
      if pnl = nil then begin
      //create the components
      {$IFDEF ownSites}
        pnl := TDockPanel.Create(self); //owned by?
      {$ELSE}
        pnl := TDockPanel.Create(AForm); //owned by?
      {$ENDIF}
        pnl.Name := PanelNames[side];
        pnl.Parent := AForm;
        pnl.Align := side;
        pnl.BorderWidth := 1;
        //pnl.BorderStyle := bsSingle; // does not properly handle the size
        dm := TEasyTree.Create(pnl);
        dm.SetStyle(hsForm);
        pnl.DockSite := True;
        pnl.UseDockManager := True;
        pnl.Visible := True;
        spl := TSplitter.Create(AForm);
        spl.Parent := AForm;
        spl.Align := side;
        spl.BorderStyle := bsSingle;
      //size components
        pnl.Splitter := spl;
        if side in [alLeft,alRight] then
          pnl.Width := 0
        else
          pnl.Height := 0;
      //handlers required for elastic sites
        pnl.OnDockDrop := @pnl.pnlDockDrop;
        pnl.OnDockOver := @pnl.pnlDockOver;
        pnl.OnUnDock := @pnl.pnlUnDock;
        pnl.OnGetSiteInfo := @pnl.pnlGetSiteInfo;
      end;
    end;
  end;
end;

function TDockMaster.CreateDockable(const AName: string;
  fMultiInst: boolean; fWrap: boolean): TForm;
var
  basename, instname: string;
  i, l, instno: integer;
  fc: TFormClass;
  img: TImage;
  r: TRect;
  Site: TFloatingSite;
  ctl: TControl;
const
  digits = ['0'..'9'];
begin
(* Create a dockable form, based on its name.
  Used also to restore a layout.
  fMultiInst allows to auto-create new versions (if True),
  otherwise an already existing instance is returned. (really returned?)

  The name is split into basename and instance number.
  A component of T<basename> is created (and named AName - automatic!).
*)
  if AName = '' then begin
    //test!
    Result := TForm.Create(self); //named Form1, Form2...
  end else begin
    //basename := AName;
  //find the instance number, if present
    instno := 0;
    l := Length(AName);
    i := l;
    while AName[i] in digits do begin
      dec(i);
    end;
    //i now is the position of the last non-digit in the name
  //extract the instance number
    basename := Copy(AName, 1, i);
    while i < l do begin
      inc(i);
      instno := instno * 10 + ord(AName[i])-ord('0');
    end;
    if instno = 0 then
      instno := 1; //default instance number for forms
  //lookup existing instance
    instname := basename + IntToStr(instno);
  {$IFDEF old}
    if false then
      TWinControlAccess(Site).ReloadDockedControl(instname, ctl);
    //Result := nil;
    for i := 0 to ComponentCount - 1 do begin
      Result := TForm(Components[i]);
      if Result.Name = instname then
        exit; //found it
    end;
  {$ELSE}
    //Factory.ReloadDockedControl
  {$ENDIF}
    if FindComponent(instname) <> nil then
      exit;
  //create new instance
    basename := 'T' + basename;
    fc := TFormClass(GetClass(basename)); //must be registered class name!
    if not assigned(fc) then
      exit;
    Result := fc.Create(self);
    if Result.Name <> AName then
      Result.Name := AName; //???
  end;
  if Result.DragKind <> dkDock then begin
  //make it dockable
    Result.DragKind := dkDock;
    Result.OnEndDock := @FormEndDock; //float into default host site
  end;
  if fWrap then begin
  //wrap into dock site
    Site := WrapDockable(Result);
  //create a docking handle - should become a component?
    img := TImage.Create(Result); //we could own the img, and be notified when its parent becomes nil
    img.Parent := Result;
    img.Align := alNone;
    img.Anchors := [akTop, akRight];
    r := Result.ClientRect;
    r.bottom := 16;
    r.Left := r.Right - 16;
    img.BoundsRect := r;
    img.Picture := Site.Image1.Picture;
    img.OnMouseMove := @DockHandleMouseMove;
    img.Visible := True;
  end;
  Result.Visible := True;
  //Result.OnEndDock();
end;

procedure TDockMaster.FormEndDock(Sender, Target: TObject; X, Y: Integer);
var
  ctl: TControl;
  Site: TFloatingSite;
begin
(* Handler for Form.OnEndDock.
  When a form becomes floating, dock immediately into a new floating host docksite.
*)
  if Target <> nil then
    exit; //docked, not floating
  ctl := Sender as TControl;
  if ctl.HostDockSite = nil then begin
    //DebugLn('--- floating');
    WrapDockable(ctl);
  end else begin
    //DebugLn('--- in ' + HostDockSite.Name);
  end;
end;

procedure TDockMaster.LoadFromStream(Stream: TStream);
var
  ctl, pre: TControl;
  site: TFloatingSite;

  procedure MakeForm;
  begin
    pre := ctl;
    ctl := CreateDockable('', True, False);
  end;

begin
  //Test0;
  site := TFloatingSite.Create(self);
  MakeForm; ctl.ManualDock(site, nil, alClient);
  MakeForm; ctl.ManualDock(site, pre, alRight);
  MakeForm; ctl.ManualDock(site, pre, alBottom);
  //MakeForm; ctl.ManualDock(site, pre, alCustom);
end;

procedure TDockMaster.SaveToStream(Stream: TStream);
begin

end;

function TDockMaster.WrapDockable(Client: TControl): TFloatingSite;
var
  Site: TForm absolute Result;
begin
  {$IFDEF ownSites}
    Site := TFloatingSite.Create(Self); //the new site
  {$ELSE}
    Site := TFloatingSite.Create(Application); //the new site
  {$ENDIF}
    Site.BoundsRect := Client.BoundsRect; //the new position and extension
    Client.Align := alClient;
    Client.Visible := True; //otherwise docking may be rejected
    Client.ManualDock(Site);
    //Site.DockManager.ResetBounds(True); //does not work on first attempt?
end;

procedure TDockMaster.DockHandleMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  ctl: TControl;  // absolute Sender;
begin
(* Handler for DockHandle.OnMouseMove.
  When the left button is pressed, start dragging (for docking).
*)
  if ssLeft in Shift then begin
    ctl := Sender as TControl;
    ctl.Parent.BeginDrag(False);
  end;
end;

procedure TDockMaster.DumpSites;
var
  i, j: integer;
  Site: TWinControl;
  ctl: TControl;
  cmp: TComponent;
  n, s: string;
  hds: boolean;
const
  OrientString: array[TDockOrientation] of char = (
    'N','H','V' {$IFDEF FPC} ,'P' {$ENDIF}
  );
  AlignString: array[TAlign] of char = (
    //(alNone, alTop, alBottom, alLeft, alRight, alClient, alCustom);
    'n', 't', 'B', 'L', 'R', 'C', 'c'
  );

  function SiteName(ph: TControl): string;
  begin
    if ph = nil then
      exit('<nil>');
    Result := ph.Name;
    if Result = '' then
      Result := '<' + ph.ClassName + '>';
  end;

begin
(* Dump registered docking sites.
  Elastic panels have no name.
  Dump of docked clients by DockManager (structural info!)
  Notebooks are docked, i.e. HostDockSite<>nil.
    Pages are DockSites???
    EditPages contain Files -> include (full?) filename
--> dump-levels
  dock sites[] and clients[]
    contents[]
*)
  DebugLn('--- dump sites ---');
  for i := 0 to ComponentCount - 1 do begin
    cmp := Components[i];
    if cmp is TWinControl then begin
    //path
      Site := TWinControl(cmp);
      if Site.DockSite then begin
      //reached only when ownSites is defined!
        ctl := Site;
        s := Format('Site=%s (%d,%d)[%d,%d]', [SiteName(ctl),
          ctl.Left, ctl.Top, ctl.Width, ctl.Height]);
        while ctl <> nil do begin
          hds := ctl.HostDockSite <> nil;
          if hds then begin
            Site := ctl.HostDockSite;
            if Site <> nil then
              n := ' in ' + SiteName(Site) + '@' + OrientString[ctl.DockOrientation];
          end else begin
            Site := ctl.Parent;
            if Site <> nil then
              n := ' at ' + SiteName(Site) + '@' + AlignString[ctl.Align];
          end;
          if Site = nil then
            break;
          s := s + n;
          ctl := Site;
        end;
        DebugLn(s);
      //clients
        Site := TWinControl(cmp);
        for j := 0 to site.DockClientCount - 1 do begin
          ctl := site.DockClients[j];
          s := OrientString[ctl.DockOrientation];
          DebugLn('  Client=%s@%s (%d,%d)[%d,%d]', [SiteName(ctl), s,
            ctl.Left, ctl.Top, ctl.Width, ctl.Height]);
          //if ctl is TFloatingSite then
        end;
      end else begin
        ctl := Site;
        DebugLn('Client=%s in %s (%d,%d)[%d,%d]', [SiteName(ctl), SiteName(ctl.HostDockSite),
          ctl.Left, ctl.Top, ctl.Width, ctl.Height]);
      end;
    end;
  end;
  DebugLn('--- end dump ---');
end;


{ TDockPanel }

procedure TDockPanel.pnlDockDrop(Sender: TObject; Source: TDragDockObject;
  X, Y: Integer);
var
  w: integer;
  r: TRect;
begin
(* Adjust docksite extent, if required.
  H/V depending on align LR/TB.
  Take 1/3 of the form's extent for the dock site.
  When changed, ensure that the form layout is updated.
*)
  if (DockClientCount > 1)
  or ((Width > 1) and (Height > 1)) //NoteBook!
  then
    exit; //no adjustments of the dock site required

//this is the first drop - handle AutoExpand
  with Source do begin
    if Align in [alLeft, alRight] then begin
      w := Parent.Width div 3;
      if Width < w then begin
      //enlarge docksite
        Parent.DisableAlign; //form(?)
        Width := w;
        if Align = alRight then begin
          if AutoExpand then begin
            r := Parent.BoundsRect;
            inc(r.Right, w);
            Parent.BoundsRect := r;
          end else begin
            Left := Left-w;
            Splitter.Left := Splitter.Left-w;
          end;
        end else if AutoExpand then begin
        //enlarge left
          r := Parent.BoundsRect;
          dec(r.Left, w);
          Parent.BoundsRect := r;
        end;
        Parent.EnableAlign;
      end;
    end else begin //alBottom
      w := Parent.Height div 3;
      if Height < w then begin
      //enlarge docksite
        Parent.DisableAlign; //form(?)
        Height := w;
        if Align = alBottom then begin
          if AutoExpand then begin
            r := Parent.BoundsRect;
            inc(r.Bottom, w);
            Parent.BoundsRect := r;
          end else begin
            Splitter.Top := Splitter.Top-w;
            Top := Top-w;
          end;
        end;
        Parent.EnableAlign;
      end;
    end;
  end;
end;

procedure TDockPanel.pnlDockOver(Sender: TObject; Source: TDragDockObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  r: TRect;

  procedure Adjust(dw, dh: integer);
  begin
  (* r.TopLeft in screen coords, r.BottomRight is W/H(?)
    negative values mean expansion towards screen origin
  *)
    if dw <> 0 then begin
      r.Right := r.Left;
      inc(r.Bottom, r.Top);
      if dw > 0 then
        inc(r.Right, dw)
      else
        inc(r.Left, dw);
    end else begin
      r.Bottom := r.Top;
      inc(r.Right, r.Left);
      if dh > 0 then
        inc(r.Bottom, dh)
      else
        inc(r.Top, dh);
    end;
  end;

var
  dw, dh: integer;
const
  d = 10; //shift mousepos with InfluenceRect
begin
(* This handler has to determine the intended DockRect,
  and the alignment within this rectangle.
*)
  if Source.DragTarget = nil then begin
  //DragManager signals deny!
    exit;
  end;
  if State = dsDragMove then begin
    if DockClientCount > 0 then
      exit; //everything should be okay
  //make DockRect reflect the docking area
    r := BoundsRect; //XYWH
    r.TopLeft := Parent.ClientToScreen(r.TopLeft);
    dw := Parent.Width div 3;  //r.Right := r.Left + dw;
    dh := Parent.Height div 3; //r.Bottom := r.Top + dh;
  //dock inside/outside depending on mouse position
    case Align of
    alLeft:
      begin
        AutoExpand := Source.DragPos.x + d < r.Left;
        if AutoExpand then Adjust(-dw, 0) else Adjust(dw, 0);
      end;
    alRight:
      begin
        AutoExpand := Source.DragPos.x + d >= r.Left;
        if AutoExpand then Adjust(dw, 0) else Adjust(-dw, 0);
      end;
    alBottom:
      begin
        AutoExpand := Source.DragPos.y + d > r.Top;
        if AutoExpand then Adjust(0, dh) else Adjust(0, -dh);
      end
    else
      exit;
    end;
    Source.DockRect := r;
    Accept := True;
  end;
end;

procedure TDockPanel.pnlGetSiteInfo(Sender: TObject; DockClient: TControl;
  var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
begin
(* Signal acceptance.
  Inflate InfluenceRect, for easier docking into a shrinked site.
*)
  CanDock := True;
  InflateRect(InfluenceRect, 20, 20);
end;

procedure TDockPanel.pnlUnDock(Sender: TObject; Client: TControl;
  NewTarget: TWinControl; var Allow: Boolean);
var
  wh: integer;
  r: TRect;
begin
(* When the last client is undocked, shrink the dock site to zero extent.
  Called *before* the dock client is removed.
*)
  if DockClientCount <= 1 then begin
  //become empty, hide the dock site
    Parent.DisableAlign;
    case Align of
    alLeft:
      begin
        wh := Width;
        Width := 0; //behaves as expected
        if AutoExpand then begin
          r := Parent.BoundsRect;
          inc(r.Left, wh);
          Parent.BoundsRect := r;
        end;
      end;
    alRight:
      begin
        wh := Width;
        Width := 0;
        if AutoExpand then begin
          r := Parent.BoundsRect;
          dec(r.Right, wh);
          Parent.BoundsRect := r;
        end else begin
          Left := Left+wh;
          Splitter.Left := Splitter.Left+wh;
        end;
      end;
    alBottom:
      begin
        wh := Height;
        Height := 0;
        if AutoExpand then begin
          r := Parent.BoundsRect;
          dec(r.Bottom, wh);
          Parent.BoundsRect := r;
          Splitter.Top := Splitter.Top-wh;
        end else begin
          Top := Top+wh;
          Splitter.Top := Top - Splitter.Height - 10;
        end;
      end;
    end;
    Parent.EnableAlign;
  end;
end;

end.

