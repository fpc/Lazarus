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
    FEdit: TSynEditBase;
    FFoldView: TSynEditFoldedView;
    FMarkupInfoModifiedLine: TSynSelectedColor;
  public
    constructor Create(AOwner: TSynEditBase; AFoldView: TSynEditFoldedView);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure Paint(Canvas: TCanvas; AClip: TRect; FirstLine, LastLine: integer); override;
    function RealGutterWidth(CharWidth: integer): integer;  override;
  public
    property MarkupInfoModifiedLine: TSynSelectedColor read FMarkupInfoModifiedLine;
  end;

implementation
uses
  SynEdit;

{ TSynGutterChanges }

constructor TSynGutterChanges.Create(AOwner : TSynEditBase; AFoldView : TSynEditFoldedView);
begin
  inherited Create;
  FEdit := AOwner;
  FFoldView := AFoldView;

  FMarkupInfoModifiedLine := TSynSelectedColor.Create;
  FMarkupInfoModifiedLine.Background := clNone;
  FMarkupInfoModifiedLine.Foreground := clGreen;
  FMarkupInfoModifiedLine.FrameColor := $00E9FC;
  FMarkupInfoModifiedLine.OnChange := @DoChange;

  Width := 6;
end;

destructor TSynGutterChanges.Destroy;
begin
  FMarkupInfoModifiedLine.Free;
  inherited Destroy;
end;

procedure TSynGutterChanges.Assign(Source : TPersistent);
var
  Src: TSynGutterChanges;
begin
  if Assigned(Source) and (Source is TSynGutterChanges) then
  begin
    Src := TSynGutterChanges(Source);
    FMarkupInfoModifiedLine.Assign(Src.FMarkupInfoModifiedLine);
  end;
  inherited;
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

  LineHeight := TSynEdit(FEdit).LineHeight;

  if MarkupInfoModifiedLine.Background <> clNone then
  begin
    Canvas.Brush.Color := MarkupInfoModifiedLine.Background;
    Canvas.FillRect(AClip);
  end;

  Canvas.Pen.Width := Width ;
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

    case TCustomSynEdit(FEdit).GetLineState(iLine) of
      slsNone: ;
      slsSaved:
        begin
          Canvas.Pen.Color := MarkupInfoModifiedLine.Foreground;
          Canvas.Line(rcLine.Left, rcLine.Top, rcLine.Left, rcLine.Bottom);
        end;
      slsUnsaved:
        begin
          Canvas.Pen.Color := MarkupInfoModifiedLine.FrameColor;
          Canvas.Line(rcLine.Left, rcLine.Top, rcLine.Left, rcLine.Bottom);
        end;
    end;
  end;
end;

end.

