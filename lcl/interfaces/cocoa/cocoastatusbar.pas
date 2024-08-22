unit CocoaStatusBar;

{$mode objfpc}{$H+}
{$modeswitch objectivec2}
{$interfaces corba}

interface

uses
  Classes, SysUtils,
  CocoaAll, CocoaPrivate, CocoaCallback, CocoaCustomControl, CocoaUtils;

type
  TStatusItemData = record
    Text  : NSString;
    Width : Integer;
    Align : TAlignment;
  end;

  TStatusItemDataArray = array of TStatusItemData;

  TCocoaStatusBar = objcclass(TCocoaCustomControl)
  public
    //StatusBar : TStatusBar;
    barcallback : IStatusBarCallback;
    panelCell   : NSCell;
    procedure drawRect(dirtyRect: NSRect); override;
    procedure dealloc; override;
  end;

implementation

{ TCocoaStatusBar }

procedure TCocoaStatusBar.drawRect(dirtyRect: NSRect);
var
  R    : TRect;
  i    : Integer;
  cs   : NSString;
  nr   : NSRect;
  dr   : NSRect;
  al   : TAlignment;
  x    : Integer;
  txt  : string;
  cnt  : Integer;
  w    : Integer;
const
  CocoaAlign: array [TAlignment] of Integer = (NSNaturalTextAlignment, NSRightTextAlignment, NSCenterTextAlignment);
begin
  if not Assigned(barcallback) then Exit;

  if not Assigned(panelCell) then Exit;

  panelCell.setControlView(Self);

  r := lclClientFrame();
  nr.origin.y := 0;
  nr.size.height := self.lclFrame.Height;

  x:=0;
  cnt := barcallback.GetBarsCount;
  for i:=0 to cnt - 1 do begin

    txt := '';
    w := 0;
    al := taLeftJustify;

    if not barcallback.GetBarItem(i, txt, w, al) then Continue;

    if i = cnt - 1 then w := r.Right - x;
    nr.size.width := w;
    nr.origin.x := x;

    // dr - draw rect. should be 1 pixel wider
    // and 1 pixel taller, than the actual rect.
    // to produce a better visual effect
    dr := nr;
    dr.size.width := dr.size.width + 1;
    dr.size.height := dr.size.height + 1;
    dr.origin.y := dr.origin.y-1;

    cs := NSStringUtf8(txt);
    panelCell.setTitle(cs);
    panelCell.setAlignment(CocoaAlign[al]);
    panelCell.drawWithFrame_inView(dr, Self);
    cs.release;
    barcallback.DrawPanel(i, NSRectToRect(nr));
    inc(x, w);
    if x > r.Right then break; // no place left
  end;
end;

procedure TCocoaStatusBar.dealloc;
begin
  if Assigned(panelCell) then panelCell.release;
  inherited;
end;

end.

