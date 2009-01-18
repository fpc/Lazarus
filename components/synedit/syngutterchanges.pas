unit SynGutterChanges;

{$I synedit.inc}

interface

uses
  Classes, SysUtils, Graphics, LCLType, LCLIntf, SynGutter,
  SynEditMiscProcs, SynEditMiscClasses, SynTextDrawer, SynEditFoldedView;

type
  { TSynGuterChanges }

  TSynGutterChanges = class(TSynGutterPartBase)
  private
    FFoldView: TSynEditFoldedView;

  public
    constructor Create(AOwner: TSynEditBase; AFoldView: TSynEditFoldedView);
    destructor Destroy; override;

    procedure Paint(Canvas: TCanvas; AClip: TRect; FirstLine, LastLine: integer); override;
    function RealGutterWidth(CharWidth: integer): integer;  override;

  end;

implementation
uses
  SynEdit;

{ TSynGutterChanges }

constructor TSynGutterChanges.Create(AOwner : TSynEditBase; AFoldView : TSynEditFoldedView);
begin
  inherited Create(AOwner);
  FFoldView := AFoldView;

  MarkupInfo.Background := clNone;
  MarkupInfo.Foreground := clGreen;
  MarkupInfo.FrameColor := $00E9FC;

  Width := 6;
end;

destructor TSynGutterChanges.Destroy;
begin
  inherited Destroy;
end;

function TSynGutterChanges.RealGutterWidth(CharWidth: integer): integer;
begin
  if not Visible then
  begin
    Result := 0;
    Exit;
  end;

  if AutoSize then
    Width := 4;
  Result := Width;
end;

procedure TSynGutterChanges.Paint(Canvas: TCanvas; AClip: TRect; FirstLine, LastLine: integer);
var
  i, iLine: integer;
  LineHeight: Integer;
  rcLine: TRect;
begin
  if not Visible then exit;

  LineHeight := TSynEdit(SynEdit).LineHeight;

  if MarkupInfo.Background <> clNone then
  begin
    Canvas.Brush.Color := MarkupInfo.Background;
    Canvas.FillRect(AClip);
  end;

  Canvas.Pen.Width := Width;
  Canvas.Pen.EndCap:= pecFlat;

  rcLine := AClip;
  rcLine.Left := rcLine.Left + Width div 2;
  rcLine.Bottom := FirstLine * LineHeight;
  for i := FirstLine to LastLine do
  begin
    iLine := FFoldView.TextIndex[i];
    // next line rect
    rcLine.Top := rcLine.Bottom;
    Inc(rcLine.Bottom, LineHeight);

    case TCustomSynEdit(SynEdit).GetLineState(iLine) of
      slsNone: ;
      slsSaved:
        begin
          Canvas.Pen.Color := MarkupInfo.Foreground;
          Canvas.Line(rcLine.Left, rcLine.Top, rcLine.Left, rcLine.Bottom);
        end;
      slsUnsaved:
        begin
          Canvas.Pen.Color := MarkupInfo.FrameColor;
          Canvas.Line(rcLine.Left, rcLine.Top, rcLine.Left, rcLine.Bottom);
        end;
    end;
  end;
end;

end.

