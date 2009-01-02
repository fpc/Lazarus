unit SynGutterCodeFolding;

{$I synedit.inc}

interface

uses
  Classes, SysUtils, Graphics, LCLType, LCLIntf, SynGutter,
  SynEditMiscClasses, SynEditMiscProcs, SynEditFoldedView;

type

  { TSynGutterCodeFolding }

  TSynGutterCodeFolding = class(TSynGutterPartBase)
  private
    FEdit: TSynEditBase;
    FFoldView: TSynEditFoldedView;
    FMarkupInfoCodeFoldingTree: TSynSelectedColor;
  public
    constructor Create(AOwner : TSynEditBase; AFoldView : TSynEditFoldedView);
    destructor Destroy; override;

    procedure Paint(Canvas: TCanvas; AClip: TRect; FirstLine, LastLine: integer);
      override;
    function RealGutterWidth(CharWidth: integer): integer;  override;
    procedure DoOnGutterClick(X, Y: integer); override;
  public
    property MarkupInfoCodeFoldingTree: TSynSelectedColor read FMarkupInfoCodeFoldingTree;
  end;

implementation
uses
  SynEdit;

{ TSynGutterCodeFolding }

constructor TSynGutterCodeFolding.Create(AOwner : TSynEditBase;
  AFoldView : TSynEditFoldedView);
begin
  inherited Create;
  FEdit := AOwner;
  FFoldView := AFoldView;

  FMarkupInfoCodeFoldingTree := TSynSelectedColor.Create;
  FMarkupInfoCodeFoldingTree.Background := clNone;
  FMarkupInfoCodeFoldingTree.Foreground := clDkGray;
  FMarkupInfoCodeFoldingTree.FrameColor := clNone;
  FMarkupInfoCodeFoldingTree.OnChange := @DoChange;

  Width := 14;
end;

destructor TSynGutterCodeFolding.Destroy;
begin
  FMarkupInfoCodeFoldingTree.Free;
  inherited Destroy;
end;

function TSynGutterCodeFolding.RealGutterWidth(CharWidth : integer) : integer;
begin
  If Visible then
    Result := Width
  else
    Result := 0;
end;

procedure TSynGutterCodeFolding.DoOnGutterClick(X, Y : integer);
var
  line  : integer;
begin
  line := TSynEdit(FEdit).PixelsToRowColumn(Point(X, Y)).Y;
  if line <= TSynEdit(FEdit).Lines.Count then
    TSynEdit(FEdit).CodeFoldAction(line);
end;

procedure TSynGutterCodeFolding.Paint(Canvas : TCanvas; AClip : TRect; FirstLine, LastLine : integer);
var
  iLine: integer;
  rcLine: TRect;
  rcCodeFold: TRect;
  tmp: TSynEditCodeFoldType;
  LineHeight, LineOffset: Integer;

  procedure DrawNodeBox(rcCodeFold: TRect; Collapsed: boolean);
  const cNodeOffset = 3;
  var
    rcNode: TRect;
    ptCenter : TPoint;
    iSquare: integer;
  begin
    //center of the draw area
    ptCenter.X := (rcCodeFold.Left + rcCodeFold.Right) div 2;
    ptCenter.Y := (rcCodeFold.Top + rcCodeFold.Bottom) div 2;

    //make node rect square
    iSquare := Max(0, rcCodeFold.Bottom - rcCodeFold.Top - 14) div 2;

    //area of drawbox
    rcNode.Right := rcCodeFold.Right - cNodeOffset + 1;
    rcNode.Left := rcCodeFold.Left + cNodeOffset;
    rcNode.Top := rcCodeFold.Top + cNodeOffset + iSquare;
    rcNode.Bottom := rcCodeFold.Bottom - cNodeOffset - iSquare + 1;

    Canvas.Brush.Color:=clWhite;
    Canvas.Rectangle(rcNode);

    //draw bottom handle to paragraph line
    Canvas.MoveTo((rcNode.Left + rcNode.Right) div 2, rcNode.Bottom);
    Canvas.LineTo((rcNode.Left + rcNode.Right) div 2, rcCodeFold.Bottom);

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
  LineHeight := TSynEdit(FEdit).LineHeight;
  LineOffset := 0;

  if MarkupInfoCodeFoldingTree.Background <> clNone then
  begin
    Canvas.Brush.Color := MarkupInfoCodeFoldingTree.Background;
    {$IFDEF SYN_LAZARUS}
    LCLIntf.SetBkColor(Canvas.Handle, Canvas.Brush.Color);
    {$ENDIF}
    Canvas.FillRect(AClip);
  end;

  with Canvas do
  begin
    Pen.Color := MarkupInfoCodeFoldingTree.Foreground;
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

