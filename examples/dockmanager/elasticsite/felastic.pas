unit fElastic;
(* Demonstrate elastic dock sites.
  This form has dock sites (panels) on its left, right and bottom.

  Empty panels should be invisible, what's a bit tricky. They cannot have
  Visible=False, because this would disallow to dock anything into them.
  So the width/height of the panels is set to zero instead.

  When a first control is docked, the dock site is enlarged.
  Fine adjustment can be made with the splitters beneath the controls.

  When the last control is undocked, the dock site is shrinked again.

Planned extensions:
- using an DockManager, so that more than only one control can be docked
  into every panel.
*)

(* Observed problems:

The right panel does not shrink.
Form doesn't shrink at the right side.

Object Inspector says: the bottom panel's OnGetSiteInfo method is incompatible
  with other OnGetSiteInfo methods.
*)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls,
  EasyDockSite;

type
  TDockingSite = class(TForm)
    buNewForm: TButton;
    Panel1: TPanel;
    pnlBottom: TPanel;
    pnlLeft: TPanel;
    pnlRight: TPanel;
    splitLeft: TSplitter;
    splitBottom: TSplitter;
    splitRight: TSplitter;
    StatusBar1: TStatusBar;
    procedure buNewFormClick(Sender: TObject);
    procedure pnlLeftDockDrop(Sender: TObject; Source: TDragDockObject;
      X, Y: Integer);
    procedure pnlLeftDockOver(Sender: TObject; Source: TDragDockObject;
      X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure pnlLeftGetSiteInfo(Sender: TObject; DockClient: TControl;
      var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
    procedure pnlLeftUnDock(Sender: TObject; Client: TControl;
      NewTarget: TWinControl; var Allow: Boolean);
  public
  end;

var
  DockingSite: TDockingSite;

implementation

uses
  fDockClient;  //test only

{ TDockingSite }

procedure TDockingSite.buNewFormClick(Sender: TObject);
begin
  TDockingClient.Create(self);
end;

procedure TDockingSite.pnlLeftDockDrop(Sender: TObject;
  Source: TDragDockObject; X, Y: Integer);
var
  w: integer;
  r: TRect;
begin
(* Adjust docksite extent, if required.
  H/V depending on align LR/TB.
  Take 1/3 of the form's extent for the dock site.
  When changed, ensure that the form layout is updated.
*)
  with Source do begin
    if DragTarget.Align in [alLeft, alRight] then begin
      w := self.Width div 3;
      if DragTarget.Width < w then begin
      //enlarge docksite
        DisableAlign; //form(?)
        DragTarget.Width := w;
        if DragTarget.Align = alRight then begin
          if AutoSize then begin
            r := self.BoundsRect;
            inc(r.Right, w);
            BoundsRect := r;
          end else begin
            dec(DragTarget.Left, w);
            dec(splitRight.Left, w);
          end;
        end else if AutoSize then begin
        //enlarge left
          r := BoundsRect;
          dec(r.Left, w);
          BoundsRect := r;
        end;
        EnableAlign;
      end;
    end else begin
      w := self.Height div 3;
      if DragTarget.Height < w then begin
      //enlarge docksite
        DisableAlign; //form(?)
        DragTarget.Height := w;
        if DragTarget.Align = alBottom then begin
          if AutoSize then begin
            //dec(self.Left, w);
            r := self.BoundsRect;
            inc(r.Bottom, w);
            BoundsRect := r;
            inc(StatusBar1.Top, w);
          end else begin
            dec(splitBottom.Top, w);
            dec(DragTarget.Top, w);
          end;
        end;
        EnableAlign;
      end;
    end;
    //Control.Align := alClient;
  end;
end;

procedure TDockingSite.pnlLeftDockOver(Sender: TObject;
  Source: TDragDockObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);
begin
  if Source.DragTarget = nil then
    exit;
  if State = dsDragMove then begin
    Accept := True;
  //make DockRect reflect the docking area
    with Source do begin
      StatusBar1.SimpleText := AlignNames[DropAlign];
      DockRect := DragTarget.ClientRect;
    { TODO : AutoSize }
      if DragTarget.Width <= 0 then begin
        dec(DockRect.Left, 10);
        inc(DockRect.Right, 20);
      end else if DragTarget.Height <= 0 then begin
        dec(DockRect.Top, 10);
        inc(DockRect.Bottom, 20);
      end;
      DockRect.TopLeft := TWinControl(DragTarget).ClientToScreen(DockRect.TopLeft);
      inc(DockRect.Bottom, DockRect.Top);
      inc(DockRect.Right, DockRect.Left);
    end;
  end;
end;

procedure TDockingSite.pnlLeftGetSiteInfo(Sender: TObject;
  DockClient: TControl; var InfluenceRect: TRect; MousePos: TPoint;
  var CanDock: Boolean);
begin
  CanDock := True;
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
        if AutoSize then begin
          r := BoundsRect;
          inc(r.Left, wh);
          BoundsRect := r;
        end;
      end;
    alRight:
      begin //problem: does NOT resize?
        wh := Site.Width;
        Site.Width := 0;
        if AutoSize then begin
          r := BoundsRect;
          dec(r.Right, wh);
          BoundsRect := r; //does not resize :-(
        end else begin
          inc(Site.Left, wh);
          inc(splitRight.Left, wh); //doesn't help :-(
        end;
      end;
    alBottom:
      begin
        wh := Site.Height;
        Site.Height := 0;
        if AutoSize then begin
          r := BoundsRect;
          dec(r.Bottom, wh);
          BoundsRect := r;
          dec(splitBottom.Top, wh);
          dec(StatusBar1.Top, wh);
        end else begin
          inc(Site.Top, wh);
          splitBottom.Top := Site.Top - splitBottom.Height - 10;
        end;
      end;
    end;
    EnableAlign;
    //RecreateWnd(self); - doesn't help
  end;
end;

initialization
  {$I felastic.lrs}
  DefaultDockTreeClass := TEasyTree;

end.

