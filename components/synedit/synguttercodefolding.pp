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

  public
    constructor Create(AOwner : TSynEditBase; AFoldView : TSynEditFoldedView);

    procedure Paint(Canvas: TCanvas; AClip: TRect; FirstLine, LastLine: integer);
      override;
    function RealGutterWidth(CharWidth: integer): integer;  override;
    procedure DoOnGutterClick(X, Y: integer); override;
  public
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

  Width := 14;
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
  dc: HDC;
  rcCodeFold: TRect;
  tmp: TSynEditCodeFoldType;
  LineHeight: Integer;

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
  end;

  procedure DrawParagraphContinue(rcCodeFold: TRect);
  var
    iCenter : integer;
  begin
    //center of the draw area
    iCenter := (rcCodeFold.Left + rcCodeFold.Right) div 2;

    Canvas.MoveTo(iCenter, rcCodeFold.Top);
    Canvas.LineTo(iCenter, rcCodeFold.Bottom);
  end;

  procedure DrawParagraphEnd(rcCodeFold: TRect);
  var
    ptCenter : TPoint;
  begin
    //center of the draw area
    ptCenter.X := (rcCodeFold.Left + rcCodeFold.Right) div 2;
    ptCenter.Y := (rcCodeFold.Top + rcCodeFold.Bottom) div 2;

    Canvas.MoveTo(ptCenter.X, rcCodeFold.Top);
    Canvas.LineTo(ptCenter.X, ptCenter.Y);
    Canvas.LineTo(rcCodeFold.Right, ptCenter.Y);
  end;

begin
  if not Visible then exit;
  LineHeight := TSynEdit(FEdit).LineHeight;
  Canvas.Brush.Color := Color;
  dc := Canvas.Handle;
  {$IFDEF SYN_LAZARUS}
  LCLIntf.SetBkColor(dc,Canvas.Brush.Color);
  {$ENDIF}

  with Canvas do
  begin
    Pen.Color := clDkGray;
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
      end;
    end;
  end;

end;

end.

