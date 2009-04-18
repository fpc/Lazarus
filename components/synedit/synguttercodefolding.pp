unit SynGutterCodeFolding;

{$I synedit.inc}

interface

uses
  Classes, Controls, Graphics, LCLIntf, SynGutterBase, SynEditMiscProcs,
  SynEditFoldedView;

type

  TSynGutterFoldClickConf = record
    Enabled: Boolean;
    Button: TMouseButton;
    Shift: TShiftState;
    ShiftMask: TShiftState;
    Enabled2: Boolean;
    Button2: TMouseButton;
    Shift2: TShiftState;
    ShiftMask2: TShiftState;
  end;

  TSynGutterFoldClickType = (
    sgctFoldOne,
    sgctFoldAll,
    sgctUnFoldOne,
    sgctUnFoldAll
    );

  TSynGutterFoldClickConfList = Array [TSynGutterFoldClickType] of TSynGutterFoldClickConf;

  { TSynGutterCodeFolding }

  TSynGutterCodeFolding = class(TSynGutterPartBase)
  private
    FFoldView: TSynEditFoldedView;
    FExpandedClickConf,
    FCollapsedClickConf: TSynGutterFoldClickConfList;
  protected
    procedure DoChange(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Paint(Canvas: TCanvas; AClip: TRect; FirstLine, LastLine: integer);
      override;
    function RealGutterWidth(CharWidth: integer): integer;  override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DoOnGutterClick(X, Y: integer); override;
    property ExpandedClickConf: TSynGutterFoldClickConfList read FExpandedClickConf;
    property CollapsedClickConf: TSynGutterFoldClickConfList read FCollapsedClickConf;

  published
    property MarkupInfo;
  end;

implementation
uses
  SynEdit;

{ TSynGutterCodeFolding }

procedure TSynGutterCodeFolding.DoChange(Sender: TObject);
begin
  if AutoSize then
    FWidth := 10;
  inherited DoChange(Sender);
end;

constructor TSynGutterCodeFolding.Create(AOwner: TComponent);
var
  i: TSynGutterFoldClickType;
begin
  inherited Create(AOwner);
  FFoldView := Gutter.FoldView;

  MarkupInfo.Background := clNone;
  MarkupInfo.Foreground := clDkGray;
  MarkupInfo.FrameColor := clNone;

  FWidth := 10;

  for i := low(TSynGutterFoldClickType) to high(TSynGutterFoldClickType) do begin
    FExpandedClickConf[i].Enabled := False;
    FCollapsedClickConf[i].Enabled := False;
    FExpandedClickConf[i].Enabled2 := False;
    FCollapsedClickConf[i].Enabled2 := False;
  end;
  with FExpandedClickConf[sgctFoldOne] do begin
    Enabled := True;
    Button := mbLeft;
    Shift := [];
    ShiftMask := [];
    Enabled2 := True;
    Button2 := mbMiddle;
    Shift2 := [];
    ShiftMask2 := [];
  end;
  with FCollapsedClickConf[sgctUnFoldAll] do begin
    Enabled := True;
    Button := mbLeft;
    Shift := [];
    ShiftMask := [ssShift, ssCtrl];
  end;

  with FCollapsedClickConf[sgctUnFoldOne] do begin
    Enabled := True;
    Button := mbLeft;
    Shift := [ssCtrl];
    ShiftMask := [ssShift, ssCtrl];
  end;
  with FCollapsedClickConf[sgctFoldOne] do begin
    Enabled := True;
    Button := mbMiddle;
    Shift := [];
    ShiftMask := [];
    Enabled2 := True;
    Button2 := mbLeft;
    Shift2 := [ssShift];
    ShiftMask2 := [ssShift, ssCtrl];
  end;
end;

destructor TSynGutterCodeFolding.Destroy;
begin
  inherited Destroy;
end;

function TSynGutterCodeFolding.RealGutterWidth(CharWidth : integer) : integer;
begin
  If Visible then
    Result := Width
  else
    Result := 0;
end;

procedure TSynGutterCodeFolding.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
  function isClick(conf : TSynGutterFoldClickConf): Boolean;
  begin
    Result := ( conf.Enabled and (Button = conf.Button) and
                (Shift * conf.ShiftMask = conf.Shift) ) or
              ( conf.Enabled2 and (Button = conf.Button2) and
                (Shift * conf.ShiftMask2 = conf.Shift2) );
  end;
var
  line  : integer;
begin
  line := TSynEdit(SynEdit).PixelsToRowColumn(Point(X, Y)).Y;
  if line > SynEdit.Lines.Count then exit;

  case FFoldView.FoldType[FFoldView.TextIndexToScreenLine(Line-1)] of
    cfCollapsed :
      begin
        if isClick(FCollapsedClickConf[sgctUnFoldOne]) then
          FFoldView.UnFoldAtTextIndex(line-1, 0, 1, True);
        if isClick(FCollapsedClickConf[sgctUnFoldAll]) then
          FFoldView.UnFoldAtTextIndex(line-1);
        if isClick(FCollapsedClickConf[sgctFoldOne]) then
          FFoldView.FoldAtTextIndex(Line-1, -1, 1, True);
        if isClick(FCollapsedClickConf[sgctFoldAll]) then
          FFoldView.FoldAtTextIndex(Line-1, -1, 0);
      end;
    cfExpanded  :
      begin
        if isClick(FExpandedClickConf[sgctFoldOne]) then
          FFoldView.FoldAtTextIndex(Line-1, -1, 1, True);
        if isClick(FExpandedClickConf[sgctFoldAll]) then
          FFoldView.FoldAtTextIndex(Line-1, -1, 0);
      end;
  end;
end;

procedure TSynGutterCodeFolding.DoOnGutterClick(X, Y : integer);
begin
  // Do Nothing
end;

procedure TSynGutterCodeFolding.Paint(Canvas : TCanvas; AClip : TRect; FirstLine, LastLine : integer);
const cNodeOffset = 1;
var
  iLine: integer;
  rcLine: TRect;
  rcCodeFold: TRect;
  tmp: TSynEditCodeFoldType;
  LineHeight, LineOffset, BoxSize: Integer;

  procedure DrawNodeBox(rcCodeFold: TRect; Collapsed: boolean);
  var
    rcNode: TRect;
    ptCenter : TPoint;
  begin
    //center of the draw area
    ptCenter.X := (rcCodeFold.Left + rcCodeFold.Right) div 2;
    ptCenter.Y := (rcCodeFold.Top + rcCodeFold.Bottom) div 2;

    //area of drawbox
    rcNode.Left   := ptCenter.X - (BoxSize div 2) + 1;
    rcNode.Right  := ptCenter.X + (BoxSize div 2);
    rcNode.Top    := ptCenter.Y - (BoxSize div 2) + 1;
    rcNode.Bottom := ptCenter.Y + (BoxSize div 2);

    Canvas.Brush.Color:=clWhite;
    Canvas.Rectangle(rcNode);

    //draw bottom handle to paragraph line
    Canvas.MoveTo(ptCenter.X, rcNode.Bottom);
    Canvas.LineTo(ptCenter.X, rcCodeFold.Bottom);

    //draw unfolded sign in node box
    Canvas.MoveTo(ptCenter.X - 2, ptCenter.Y);
    Canvas.LineTo(ptCenter.X + 3, ptCenter.Y);

    //draw folded sign
    if Collapsed then
    begin
      Canvas.MoveTo(ptCenter.X, ptCenter.Y - 2);
      Canvas.LineTo(ptCenter.X, ptCenter.Y + 3);
    end;
    LineOffset := 0;
  end;

  procedure DrawParagraphContinue(rcCodeFold: TRect);
  var
    iCenter : integer;
  begin
    //center of the draw area
    iCenter := (rcCodeFold.Left + rcCodeFold.Right) div 2;

    Canvas.MoveTo(iCenter, rcCodeFold.Top + LineOffset);
    Canvas.LineTo(iCenter, rcCodeFold.Bottom);
    LineOffset := 0;
  end;

  procedure DrawParagraphEnd(rcCodeFold: TRect);
  var
    X : Integer;
  begin
    //center of the draw area
    X := (rcCodeFold.Left + rcCodeFold.Right) div 2;

    Canvas.MoveTo(X, rcCodeFold.Top + LineOffset);
    Canvas.LineTo(X, rcCodeFold.Bottom - 1);
    Canvas.LineTo(rcCodeFold.Right, rcCodeFold.Bottom - 1);
    LineOffset := min(2, (rcCodeFold.Top + rcCodeFold.Bottom) div 2);
  end;

begin
  if not Visible then exit;
  LineHeight := TSynEdit(SynEdit).LineHeight;
  LineOffset := 0;
  if (FirstLine > 0) and (FFoldView.FoldType[FirstLine-1] = cfEnd) then
    LineOffset := 2;
  BoxSize := Min(Width, LineHeight - cNodeOffset*2);

  if MarkupInfo.Background <> clNone then
  begin
    Canvas.Brush.Color := MarkupInfo.Background;
  {$IFDEF SYN_LAZARUS}
    LCLIntf.SetBkColor(Canvas.Handle, Canvas.Brush.Color);
  {$ENDIF}
    Canvas.FillRect(AClip);
  end;

  with Canvas do
  begin
    Pen.Color := MarkupInfo.Foreground;
    Pen.Width := 1;

    rcLine.Bottom := FirstLine * LineHeight;
    for iLine := FirstLine to LastLine do
    begin
      // next line rect
      rcLine.Top := rcLine.Bottom;
      Inc(rcLine.Bottom, LineHeight);

      rcCodeFold.Left := AClip.Left;
      rcCodeFold.Right := AClip.Left + self.Width;
      rcCodeFold.Top := rcLine.Top;
      rcCodeFold.Bottom := rcLine.Bottom;

      tmp := FFoldView.FoldType[iLine];

      case tmp of
        cfCollapsed: DrawNodeBox(rcCodeFold, True);
        cfExpanded: DrawNodeBox(rcCodeFold, False);
        cfContinue: DrawParagraphContinue(rcCodeFold);
        cfEnd: DrawParagraphEnd(rcCodeFold);
        else LineOffset := 0;
      end;
    end;
  end;

end;

end.

