unit SynGutterMarks;

{$I synedit.inc}

interface

uses
  Classes, SysUtils, Graphics, LCLType, LCLIntf, SynGutterBase,
  SynEditMiscClasses, SynEditMiscProcs, SynEditFoldedView, SynEditMarks;

type

  { TSynGutterMarks }

  TSynGutterMarks = class(TSynGutterPartBase)
  private
    FDebugMarksImageIndex: Integer;
    FInternalImage: TSynInternalImage;
  protected
    FFoldView: TSynEditFoldedView;
    FBookMarkOpt: TSynBookMarkOpt;
    procedure DoChange(Sender: TObject); override;
    function PaintMarks(aScreenLine: Integer; Canvas : TCanvas; AClip : TRect;
                       var aGutterOffs: integer): Boolean;
    Procedure PaintLine(aScreenLine: Integer; Canvas : TCanvas; AClip : TRect); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Paint(Canvas: TCanvas; AClip: TRect; FirstLine, LastLine: integer); override;
    function RealGutterWidth(CharWidth: integer): integer;  override;
    property DebugMarksImageIndex: Integer read FDebugMarksImageIndex write FDebugMarksImageIndex;
  end;

implementation
uses
  SynEdit;


{ TSynGutterMarks }

procedure TSynGutterMarks.DoChange(Sender: TObject);
begin
  if AutoSize then
    FWidth := RealGutterWidth(0);
  inherited DoChange(Sender);
end;

constructor TSynGutterMarks.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFoldView := Gutter.FoldView;
  FBookMarkOpt := TSynEdit(SynEdit).BookMarkOptions;
  FInternalImage := nil;
  FDebugMarksImageIndex := -1;

  FWidth := 23;
end;

destructor TSynGutterMarks.Destroy;
begin
  FreeAndNil(FInternalImage);
  inherited Destroy;
end;

function TSynGutterMarks.RealGutterWidth(CharWidth: integer) : integer;
begin
  if Visible then
    Result := 22 + FBookMarkOpt.LeftMargin
  else
    Result := 0;
end;

function TSynGutterMarks.PaintMarks(aScreenLine: Integer; Canvas : TCanvas;
  AClip : TRect; var aGutterOffs: integer): Boolean;
var
  LineHeight: Integer;

  procedure DoPaintMark(CurMark: TSynEditMark);
  var
    itop : Longint;
  begin
    iTop := 0;
    if Assigned(FBookMarkOpt.BookmarkImages) and not CurMark.InternalImage then
    begin
      if (CurMark.ImageIndex <= FBookMarkOpt.BookmarkImages.Count) and
         (CurMark.ImageIndex >= 0) then
      begin
        if CurMark.IsBookmark = FBookMarkOpt.DrawBookmarksFirst then
          aGutterOffs := AClip.Left
        else
        if aGutterOffs = 0 then
          aGutterOffs := FBookMarkOpt.BookmarkImages.Width + AClip.Left;
        if LineHeight > FBookMarkOpt.BookmarkImages.Height then
          iTop := (LineHeight - FBookMarkOpt.BookmarkImages.Height) div 2;
        with FBookMarkOpt do
          BookmarkImages.Draw(Canvas, LeftMargin + aGutterOffs,
                              iTop + aScreenLine * LineHeight, CurMark.ImageIndex, True);

        Inc(aGutterOffs, FBookMarkOpt.BookmarkImages.Width);
      end;
    end
    else
    begin
      if CurMark.ImageIndex in [0..9] then
      begin
        if not Assigned(FInternalImage) then
          FInternalImage := TSynInternalImage.Create('SynEditInternalImages',10);
        if aGutterOffs = 0 then
          aGutterOffs := AClip.Left;
        FInternalImage.DrawMark(Canvas, CurMark.ImageIndex,
            FBookMarkOpt.LeftMargin + aGutterOffs, aScreenLine * LineHeight,
            LineHeight);
        Inc(aGutterOffs, FBookMarkOpt.Xoffset);
      end;
    end;
  end;

var
  iLine, j: Integer;
  Marks: TSynEditMarks;
begin
  LineHeight := TSynEdit(SynEdit).LineHeight;
  iLine := FFoldView.TextIndex[aScreenLine] + 1;
  TSynEdit(SynEdit).Marks.GetMarksForLine(iLine, Marks);
  Result := False;
  for j := Low(Marks) to High(Marks) do
  begin
    if Marks[j] = nil then
      break;
    if not Marks[j].Visible then
      continue;
    if Marks[j].IsBookmark and not FBookMarkOpt.GlyphsVisible then
      continue;
    DoPaintMark(Marks[j]);
    Result := Result or not Marks[j].IsBookmark;
  end;
end;

procedure TSynGutterMarks.PaintLine(aScreenLine: Integer; Canvas: TCanvas; AClip: TRect);
var
  aGutterOffs: Integer;
begin
  aGutterOffs := 0;
  PaintMarks(aScreenLine, Canvas, AClip, aGutterOffs);
end;

procedure TSynGutterMarks.Paint(Canvas : TCanvas; AClip : TRect; FirstLine, LastLine : integer);
var
  i, LineHeight: integer;
begin
  if not Visible then exit;
  LineHeight := TSynEdit(SynEdit).LineHeight;
  if MarkupInfo.Background <> clNone then
    Canvas.Brush.Color := MarkupInfo.Background
  else
    Canvas.Brush.Color := Gutter.Color;
  LCLIntf.SetBkColor(Canvas.Handle, Canvas.Brush.Color);

  // now the gutter marks
  if FBookMarkOpt.GlyphsVisible and (LastLine >= FirstLine) then
  begin
    for i := FirstLine to LastLine do
      PaintLine(i, Canvas, AClip);
  end;
end;

end.

