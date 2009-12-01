unit fElasticSite;
(* Demonstrate elastic dock sites.
  This form has dock sites (panels) on its left, right and bottom.

  Empty panels should be invisible, what's a bit tricky. They cannot have
  Visible=False, because this would disallow to dock anything into them.
  So the width/height of the panels is set to zero instead.

  When a first control is docked, the dock site is enlarged.
  Fine adjustment can be made with the splitters beneath the controls.

  When the last control is undocked, the dock site is shrinked again.
*)

(* Observed problems:

Object Inspector says: the bottom panel's OnGetSiteInfo method is incompatible
  with other OnGetSiteInfo methods.

ManualFloat does not properly align the client - should become alClient.

Undocked controls do not restore to their undocked size!
  Hack: use ManualDock into created floating host.

Undocking controls from a FloatingDockHostSite will undock the control,
  but it will become a child of the same site,
  and the entire site is moved.
*)

(* AutoExpand by mouse position
Requires a flag in the DockSite, set on first dock.
Hack: use Site.Tag for AutoExpanded.
*)

{$mode objfpc}{$H+}

{.$DEFINE ExpandFlag} //using AutoExpand property?
{.$DEFINE sb} //have StatusBar?

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, ComCtrls,
  EasyDockSite;

type
  TDockingSite = class(TForm)
    pnlBottom: TPanel;
    pnlLeft: TPanel;
    pnlRight: TPanel;
    splitBottom: TSplitter;
    splitLeft: TSplitter;
    splitRight: TSplitter;
    StatusBar1: TStatusBar;
    procedure pnlLeftDockDrop(Sender: TObject; Source: TDragDockObject;
      X, Y: Integer);
    procedure pnlLeftDockOver(Sender: TObject; Source: TDragDockObject;
      X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure pnlLeftGetSiteInfo(Sender: TObject; DockClient: TControl;
      var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
    procedure pnlLeftUnDock(Sender: TObject; Client: TControl;
      NewTarget: TWinControl; var Allow: Boolean);
  protected
    FAutoExpand: boolean;
    function AutoExpanded(Site: TWinControl): boolean;
{$IFDEF ExpandFlag}
  published
    property AutoExpand: boolean read FAutoExpand write FAutoExpand default True;
{$ELSE}
  //become property of the docksite (panel)
{$ENDIF}
  end;

//var DockingSite: TDockingSite;

procedure Register;

implementation

uses
  LCLIntf, LCLProc;

//uses  fDockClient;  //test only

procedure Register;
begin
  RegisterComponents('DoDi', [TDockingSite]);
end;

{ TDockingSite }

function TDockingSite.AutoExpanded(Site: TWinControl): boolean;
begin
  Result := Site.Tag <> 0;
end;

procedure TDockingSite.pnlLeftDockDrop(Sender: TObject;
  Source: TDragDockObject; X, Y: Integer);
var
  w: integer;
  r: TRect;
  Site: TWinControl absolute Sender;
begin
(* Adjust docksite extent, if required.
  H/V depending on align LR/TB.
  Take 1/3 of the form's extent for the dock site.
  When changed, ensure that the form layout is updated.
*)
  if (TWinControl(Source.DragTarget).DockClientCount > 1)
  or ((Site.Width > 1) and (Site.Height > 1)) //NoteBook!
  then
    exit; //no adjustments of the dock site required

//this is the first drop - handle AutoExpand
(* Hack AutoExpand by mouse position:
  Set Site.Tag to the FAutoExpand state determined in DockOver.
*)
  Site.Tag := ord(FAutoExpand);

  with Source do begin
    if DragTarget.Align in [alLeft, alRight] then begin
      w := self.Width div 3;
      if DragTarget.Width < w then begin
      //enlarge docksite
        DisableAlign; //form(?)
        DragTarget.Width := w;
        if DragTarget.Align = alRight then begin
          if FAutoExpand then begin
            r := self.BoundsRect;
            inc(r.Right, w);
            BoundsRect := r;
          end else begin
            DragTarget.Left:=DragTarget.Left-w;
            splitRight.Left:=splitRight.Left-w;
          end;
        end else if FAutoExpand then begin
        //enlarge left
          r := BoundsRect;
          dec(r.Left, w);
          BoundsRect := r;
        end;
        EnableAlign;
      end;
    end else begin //alBottom
      w := self.Height div 3;
      if DragTarget.Height < w then begin
      //enlarge docksite
        DisableAlign; //form(?)
        DragTarget.Height := w;
        if DragTarget.Align = alBottom then begin
          if FAutoExpand then begin
            //dec(self.Left, w);
            r := self.BoundsRect;
            inc(r.Bottom, w);
            BoundsRect := r;
            StatusBar1.Top:=StatusBar1.Top+w;
          end else begin
            splitBottom.Top:=splitBottom.Top-w;
            DragTarget.Top:=DragTarget.Top-w;
          end;
        end;
        EnableAlign;
      end;
    end;
  end;
end;

procedure TDockingSite.pnlLeftDockOver(Sender: TObject;
  Source: TDragDockObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);
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
  Site: TWinControl;  // absolute Sender;
  dw, dh: integer;
const
  d = 10; //shift mousepos with InfluenceRect
begin
(* This handler has to determine the intended DockRect,
  and the alignment within this rectangle.

  This is impossible when the mouse leaves the InfluenceRect,
  i.e. when the site is not yet expanded :-(

  For a shrinked site we only can display the intended DockRect,
  and signal alClient.

  On the first drop, AutoExpand can be determined from the mouse position,
  inside or outside the form.
*)
  if Source.DragTarget = nil then begin
  //DragManager signals deny!
    exit;
  end;
  if State = dsDragMove then begin
    TObject(Site) := Source.DragTarget;
    if Site.DockClientCount > 0 then
      exit; //everything should be okay
  //make DockRect reflect the docking area
    r := Site.BoundsRect; //XYWH
    r.TopLeft := Site.Parent.ClientToScreen(r.TopLeft);
    dw := Width div 3;  //r.Right := r.Left + dw;
    dh := Height div 3; //r.Bottom := r.Top + dh;
  //determine inside/outside
  {$IFDEF ExpandFlag}
  //using AutoExpand flag
    case Site.Align of
    alLeft:   if AutoExpand then Adjust(-dw, 0) else Adjust(dw, 0);
    alRight:  if AutoExpand then Adjust(dw, 0) else Adjust(-dw, 0);
    alBottom: if AutoExpand then Adjust(0, dh) else Adjust(0, -dh);
    else      exit;
    end;
  {$ELSE}
  //dock inside/outside depending on mouse position
  //set temporary FAutoExpand
    case Site.Align of
    alLeft:
      begin
        FAutoExpand := Source.DragPos.x + d < r.Left;
        if FAutoExpand then Adjust(-dw, 0) else Adjust(dw, 0);
      end;
    alRight:
      begin
        FAutoExpand := Source.DragPos.x + d >= r.Left;
        if FAutoExpand then Adjust(dw, 0) else Adjust(-dw, 0);
      end;
    alBottom:
      begin
        FAutoExpand := Source.DragPos.y + d > r.Top;
        if FAutoExpand then Adjust(0, dh) else Adjust(0, -dh);
      end
    else
      exit;
    end;
  {$ENDIF}
    Source.DockRect := r;
    Accept := True;
  end;
end;

procedure TDockingSite.pnlLeftGetSiteInfo(Sender: TObject;
  DockClient: TControl; var InfluenceRect: TRect; MousePos: TPoint;
  var CanDock: Boolean);
begin
(* Signal acceptance.
  Inflate InfluenceRect, for easier docking into a shrinked site.
*)
  CanDock := True;
  InflateRect(InfluenceRect, 10, 10);
  //OffsetRect(InfluenceRect, 10, 10); //collides with other sites?
end;

procedure TDockingSite.pnlLeftUnDock(Sender: TObject; Client: TControl;
  NewTarget: TWinControl; var Allow: Boolean);
var
  Site: TWinControl absolute Sender;
  wh: integer;
  r: TRect;
begin
(* When the last client is undocked, shrink the dock site to zero extent.
  Called *before* the dock client is removed.
*)
  if Site.DockClientCount <= 1 then begin
  //become empty, hide the dock site
    DisableAlign;
    case Site.Align of
    alLeft:
      begin
        wh := Site.Width;
        Site.Width := 0; //behaves as expected
        if AutoExpanded(Site) then begin
          r := BoundsRect;
          inc(r.Left, wh);
          BoundsRect := r;
        end;
      end;
    alRight:
      begin
        wh := Site.Width;
        Site.Width := 0;
        if AutoExpanded(Site) then begin
          r := BoundsRect;
          dec(r.Right, wh);
          BoundsRect := r;
        end else begin
          Site.Left:=Site.Left+wh;
          splitRight.Left:=splitRight.Left+wh;
        end;
      end;
    alBottom:
      begin
        wh := Site.Height;
        Site.Height := 0;
        if AutoExpanded(Site) then begin
          r := BoundsRect;
          dec(r.Bottom, wh);
          BoundsRect := r;
          splitBottom.Top:=splitBottom.Top-wh;
        {$IFDEF sb}
          StatusBar1.Top:=StatusBar1.Top-wh;
        {$ELSE}
        {$ENDIF}
        end else begin
          Site.Top:=Site.Top+wh;
          splitBottom.Top := Site.Top - splitBottom.Height - 10;
        end;
      end;
    end;
    EnableAlign;
  end;
end;


initialization
  {$I felasticsite.lrs}
  //DefaultDockManagerClass := TEasyTree;
end.

