unit SynGutterChanges;

{$I synedit.inc}

interface

uses
  Classes, SysUtils, Graphics, LCLType, LCLIntf, SynGutterBase,
  SynTextDrawer, SynEditFoldedView;

type
  { TSynGutterChanges }

  TSynGutterChanges = class(TSynGutterPartBase)
  private
    FFoldView: TSynEditFoldedView;
    function GetModifiedColor: TColor;
    function GetSavedColor: TColor;
    procedure SetModifiedColor(const AValue: TColor);
    procedure SetSavedColor(const AValue: TColor);
  protected
    procedure DoChange(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Paint(Canvas: TCanvas; AClip: TRect; FirstLine, LastLine: integer); override;
    function RealGutterWidth(CharWidth: integer): integer;  override;
  published
    property ModifiedColor: TColor read GetModifiedColor write SetModifiedColor;
    property SavedColor: TColor read GetSavedColor write SetSavedColor;
  end;

implementation
uses
  SynEdit;

{ TSynGutterChanges }

constructor TSynGutterChanges.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFoldView := Gutter.FoldView;

  MarkupInfo.Background := clNone;
  MarkupInfo.Foreground := clGreen;
  MarkupInfo.FrameColor := $00E9FC;

  FWidth := 4;
end;

destructor TSynGutterChanges.Destroy;
begin
  inherited Destroy;
end;

function TSynGutterChanges.GetModifiedColor: TColor;
begin
  Result := MarkupInfo.FrameColor;
end;

function TSynGutterChanges.GetSavedColor: TColor;
begin
  Result := MarkupInfo.Foreground;
end;

procedure TSynGutterChanges.SetModifiedColor(const AValue: TColor);
begin
  MarkupInfo.FrameColor := AValue;
end;

procedure TSynGutterChanges.SetSavedColor(const AValue: TColor);
begin
  MarkupInfo.Foreground := AValue;
end;

procedure TSynGutterChanges.DoChange(Sender: TObject);
begin
  if AutoSize then
    FWidth := 4;
  inherited DoChange(Sender);
end;

function TSynGutterChanges.RealGutterWidth(CharWidth: integer): integer;
begin
  if not Visible then
  begin
    Result := 0;
    Exit;
  end;

  if AutoSize then
    RealWidth := 4;
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

