unit SynGutterChanges;

{$I synedit.inc}

interface

uses
  Classes, FPCanvas, Graphics, LCLType, LCLIntf, SynGutterBase,
  SynEditMiscProcs, LazSynEditText;

type
  { TSynGutterChanges }

  TSynGutterChanges = class(TSynGutterPartBase)
  private
    function GetModifiedColor: TColor;
    function GetSavedColor: TColor;
    procedure SetModifiedColor(const AValue: TColor);
    procedure SetSavedColor(const AValue: TColor);
  protected
    function  PreferedWidth: Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint(Canvas: TCanvas; AClip: TRect; FirstLine, LastLine: integer); override;
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

  MarkupInfo.Background := clNone;
  MarkupInfo.Foreground := clGreen;
  MarkupInfo.FrameColor := $00E9FC;
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

function TSynGutterChanges.PreferedWidth: Integer;
begin
  Result := 4;
end;

procedure TSynGutterChanges.Paint(Canvas: TCanvas; AClip: TRect; FirstLine, LastLine: integer);
var
  i, c, iLine: integer;
  LineHeight, t: Integer;
  rcLine: TRect;
  AliasMode: TAntialiasingMode;
begin
  if not Visible then exit;

  LineHeight := TCustomSynEdit(SynEdit).LineHeight;
  c := TCustomSynEdit(SynEdit).Lines.Count;
  t := ToIdx(GutterArea.TextArea.TopLine);

  if MarkupInfo.Background <> clNone then
  begin
    Canvas.Brush.Color := MarkupInfo.Background;
    Canvas.FillRect(AClip);
  end;

  Canvas.Pen.Width := Width;
  Canvas.Pen.EndCap:= pecFlat;
  AliasMode := Canvas.AntialiasingMode;
  Canvas.AntialiasingMode:=amOff;

  rcLine := AClip;
  rcLine.Bottom := AClip.Top;
  rcLine.Left := rcLine.Left + Width div 2;
  for i := t + FirstLine to t + LastLine do
  begin
    iLine := ViewedTextBuffer.DisplayView.ViewToTextIndex(i);
    if (iLine < 0) or (iLine >= c) then break;
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
  Canvas.AntialiasingMode := AliasMode;
end;

end.

