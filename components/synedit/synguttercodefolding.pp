unit SynGutterCodeFolding;

{$I synedit.inc}

interface

uses
  sysutils, Classes, Controls, Graphics, Menus, LCLIntf, SynGutterBase, SynEditMiscProcs,
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
    FClickDone: Boolean;
    FClickLine: Integer;
    FFoldView: TSynEditFoldedView;
    FExpandedClickConf,
    FCollapsedClickConf: TSynGutterFoldClickConfList;
    FPopUp: TPopupMenu;
    FMenuInf: Array of TFoldViewNodeInfo;
  protected
    procedure DoChange(Sender: TObject); override;
    procedure PopClicked(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Paint(Canvas: TCanvas; AClip: TRect; FirstLine, LastLine: integer);
      override;
    function RealGutterWidth(CharWidth: integer): integer;  override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
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
  FPopUp := TPopupMenu.Create(nil);

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
  FreeAndNil(FPopUp);
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
var
  line: Integer;

  function isClick(conf : TSynGutterFoldClickConf): Boolean;
  begin
    if FClickDone then exit(False);
    Result := ( conf.Enabled and (Button = conf.Button) and
                (Shift * conf.ShiftMask = conf.Shift) ) or
              ( conf.Enabled2 and (Button = conf.Button2) and
                (Shift * conf.ShiftMask2 = conf.Shift2) );
    FClickDone := Result;
  end;
begin
  line := TSynEdit(SynEdit).PixelsToRowColumn(Point(X, Y)).Y;
  FClickDone := False;;
  FClickLine := line;
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

procedure TSynGutterCodeFolding.PopClicked(Sender: TObject);
var
  inf: TFoldViewNodeInfo;
begin
   inf := FMenuInf[(Sender as TMenuItem).tag];
   if inf.Folded then
     FFoldView.UnFoldAtTextIndex(inf.LineNum-1, inf.ColIndex, 1, False)
   else
     FFoldView.FoldAtTextIndex(inf.LineNum-1, inf.ColIndex, 1, False);
end;

procedure TSynGutterCodeFolding.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  c, i, line: Integer;
  inf: TFoldViewNodeInfo;
  m: TMenuItem;
  s, s2: String;
begin
  inherited MouseUp(Button, Shift, X, Y);
  line := FClickLine;
  if line > SynEdit.Lines.Count then exit;

  if not FClickDone then begin
    FPopUp.Items.Clear;
    c := FFoldView.OpenFoldCount(line-1);
    SetLength(FMenuInf,c);
    for i := c-1 downto 0 do begin
      inf := FFoldView.OpenFoldInfo(line-1, i);
      FMenuInf[i] := inf;
      if (i < c-1) and (FMenuInf[i+1].LineNum = line) and (inf.LineNum <> line)
      then begin
        m := TMenuItem.Create(FPopUp);
        m.Caption := cLineCaption;
        m.Tag := -1;
        FPopUp.Items.Add(m);
      end;
      s := copy(inf.Text, 1, inf.HNode.LogXStart-1);
      if length(s) > 30 then s := copy(s,1,15) + '...' + copy(s, inf.HNode.LogXStart-11,10);
      s := s + copy(inf.Text, inf.HNode.LogXStart, 30 + (30 - length(s)));
      s2 := '';
      if inf.OpenCount > 1 then
        s2 := format(' (%d/%d)', [inf.ColIndex+1, inf.OpenCount]);
      m := TMenuItem.Create(FPopUp);
      m.Caption := format('%4d %-12s %s', [ inf.LineNum, inf.Keyword+s2+':', s]);
      m.ShowAlwaysCheckable := true;
      m.Checked := inf.Folded;
      m.Tag := i;
      m.OnClick := {$IFDEF FPC}@{$ENDIF}PopClicked;
      FPopUp.Items.Add(m);
    end;
    FPopUp.PopUp;
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

