unit CocoaStatusBar;

{$mode objfpc}{$H+}
{$modeswitch objectivec2}
{$interfaces corba}

interface

uses
  Classes, SysUtils,
  LCLType, LMessages, LCLMessageGlue, ComCtrls,
  CocoaAll, CocoaPrivate, CocoaCallback, CocoaGDIObjects ,CocoaWSCommon, CocoaUtils,
  CocoaCustomControl;

type
  { IStatusBarCallback }

  IStatusBarCallback = interface {(ICommonCallback) // not needed to inherit from ICommonCallback}
    function GetBarsCount: Integer;
    //todo: consider the use Cocoa native types, instead of FPC TAlignment
    function GetBarItem(idx: Integer; var txt: String;
      var width: Integer; var align: TAlignment): Boolean;
    procedure DrawPanel(idx: Integer; const r: TRect);
  end;

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

  { TStatusBarCallback }

  TStatusBarCallback = class(TLCLCommonCallback, IStatusBarCallback, ICommonCallback)
    function GetBarsCount: Integer;
    function GetBarItem(idx: Integer; var txt: String; var width: Integer; var align: TAlignment): Boolean;
    procedure DrawPanel(idx: Integer; const r: TRect);
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

{ TStatusBarCallback }

function TStatusBarCallback.GetBarsCount: Integer;
begin
  if TStatusBar(Target).SimplePanel
    then Result := 1
    else Result := TStatusBar(Target).Panels.Count;
end;

function TStatusBarCallback.GetBarItem(idx: Integer; var txt: String;
  var width: Integer; var align: TAlignment): Boolean;
var
  sb : TStatusBar;
begin
  sb := TStatusBar(Target);
  if sb.SimplePanel then begin
    Result := idx = 0;
    if not Result then Exit;
    txt := sb.SimpleText;
    width := sb.Width;
    align := taLeftJustify; // todo: RTL?
  end else begin
    Result := (idx >= 0) and (idx < sb.Panels.Count);
    if not Result then Exit;
    if sb.Panels[idx].Style = psOwnerDraw
      then txt := ''
      else txt := sb.Panels[idx].Text;
    width := sb.Panels[idx].Width;
    align := sb.Panels[idx].Alignment;
  end;
end;

procedure TStatusBarCallback.DrawPanel(idx: Integer; const r: TRect);
var
  sb  : TStatusBar;
  msg : TLMDrawItems;
  ctx : TCocoaContext;
  dr  : TDrawItemStruct;
  fr  : TRect;
  sv  : Integer;
begin
  sb := TStatusBar(Target);
  if sb.SimplePanel then Exit;
  if (idx<0) or (idx >= sb.Panels.Count) then Exit;
  if sb.Panels[idx].Style <> psOwnerDraw then Exit;

  ctx := TCocoaContext.Create(NSGraphicsContext.currentContext);
  sv := ctx.SaveDC;
  try
    FillChar(msg, sizeof(msg), 0);
    FillChar(dr, sizeof(dr), 0);
    msg.Ctl := Target.Handle;
    msg.Msg := LM_DRAWITEM;
    msg.DrawItemStruct := @dr;
    dr.itemID := idx;
    dr._hDC := HDC(ctx);
    dr.rcItem := r;
    fr := NSView(Owner).lclFrame;
    ctx.InitDraw(fr.Right-fr.Left, fr.Bottom-fr.Top);
    LCLMessageGlue.DeliverMessage(Target, msg);
  finally
    ctx.RestoreDC(sv);
    ctx.Free;
  end;
end;

end.

