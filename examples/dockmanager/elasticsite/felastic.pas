unit fElastic;
(* Demonstrate elastic dock sites.
  This form has dock sites (panels) on its left, right and bottom.

  Empty panels should be invisible, what's a bit tricky. They cannot have
  Visible=False, because this would disallow to dock anything into them.
  So the width/height of the panels is set to zero instead.

  When a control is docked, the dock site is enlarged. Fine adjustment can be
  made with the splitters beneath the controls.

  When a control is undocked, the dock site is shrinked again.

Planned extensions:
- allow to enlarge the form together with the dock sites, so that the form's
  client area is unchanged.
- using an DockManager, so that more than only one control can be docked
  into every panel.
*)

(* Observed problems:

The right panel does not shrink.

Object Inspector says: the bottom panel's OnGetSiteInfo method is incompatible
  with other OnGetSiteInfo methods.
*)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls;

type
  TDockingSite = class(TForm)
    buNewForm: TButton;
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
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  DockingSite: TDockingSite;

implementation

uses
  fDockClient;

{ TDockingSite }

procedure TDockingSite.buNewFormClick(Sender: TObject);
begin
  TDockingClient.Create(self);
end;

procedure TDockingSite.pnlLeftDockDrop(Sender: TObject;
  Source: TDragDockObject; X, Y: Integer);
var
  w: integer;
begin
(* Adjust docksite extent, if required.
  H/V depending on align LR/TB.
  Take 1/3 of the form's extent for the dock site.
  When changed, ensure that the form layout is updated.

  To come: enlarge the form as well, when docked outside.
*)
  with Source do begin
    if DragTarget.Align in [alLeft, alRight] then begin
      w := self.Width div 3;
      if DragTarget.Width < w then begin
        DisableAlign; //form(?)
        DragTarget.Width := w;
        if DragTarget.Align = alRight then
          dec(DragTarget.Left, w);
        EnableAlign;
      end;
    end else begin
      w := self.Height div 3;
      if DragTarget.Height < w then begin
        DisableAlign; //form(?)
        DragTarget.Height := w;
        if DragTarget.Align = alBottom then
          dec(DragTarget.Top, w);
        EnableAlign;
      end;
    end;
    Control.Align := alClient;
  end;
end;

procedure TDockingSite.pnlLeftDockOver(Sender: TObject;
  Source: TDragDockObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);
begin
  if State = dsDragMove then begin
    Accept := True;
  //make DockRect reflect the docking area
    with Source do begin
      StatusBar1.SimpleText := AlignNames[DropAlign];
      DockRect := DragTarget.ClientRect;
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
begin
(* When the last client is undocked, shrink the dock site to zero extent.
  Called *before* the dock client is removed.
*)
  if Site.DockClientCount <= 1 then begin
  //hide the dock site
    DisableAlign;
    case Site.Align of
    alLeft:
      begin
        Site.Width := 0; //behaves as expected
      end;
    alRight:
      begin //problem: does NOT resize?
        inc(Site.Left, Site.Width);
        Site.Width := 0;
      //moving the splitter too, seems to be required? Doesn't help :-(
        splitRight.Left := Site.Left - splitRight.Width;
      end;
    alBottom:
      begin
        inc(Site.Top, Site.Height);
        Site.Height := 0;
      end;
    end;
    EnableAlign;
  end;
end;

initialization
  {$I felastic.lrs}

end.

