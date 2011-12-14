unit SynGutterMarks;

{$I synedit.inc}

interface

uses
  Classes, SysUtils, Graphics, LCLType, LCLIntf, LCLProc, Controls, math,
  SynGutterBase, SynEditMiscClasses, SynEditMarks;

type

  { TSynGutterMarks }

  TSynGutterMarks = class(TSynGutterPartBase)
  private
    FColumnCount: Integer;
    FColumnWidth: Integer;
    FDebugMarksImageIndex: Integer;
    FInternalImage: TSynInternalImage;
    FNoInternalImage: Boolean;
  protected
    FBookMarkOpt: TSynBookMarkOpt;
    procedure Init; override;
    function  PreferedWidth: Integer; override;
    // PaintMarks: True, if it has any Mark, that is *not* a bookmark
    function  PaintMarks(aScreenLine: Integer; Canvas : TCanvas; AClip : TRect;
                       var aFirstCustomColumnIdx: integer): Boolean;
    Procedure PaintLine(aScreenLine: Integer; Canvas : TCanvas; AClip : TRect); virtual;

    property ColumnWidth: Integer read FColumnWidth;
    property ColumnCount: Integer read FColumnCount;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Paint(Canvas: TCanvas; AClip: TRect; FirstLine, LastLine: integer); override;
    property DebugMarksImageIndex: Integer read FDebugMarksImageIndex write FDebugMarksImageIndex;
  end;

implementation
uses
  SynEdit;


{ TSynGutterMarks }

constructor TSynGutterMarks.Create(AOwner: TComponent);
begin
  FInternalImage := nil;
  FDebugMarksImageIndex := -1;
  FNoInternalImage := False;
  inherited Create(AOwner);
end;

procedure TSynGutterMarks.Init;
begin
  inherited Init;
  FBookMarkOpt := TCustomSynEdit(SynEdit).BookMarkOptions;
end;

function TSynGutterMarks.PreferedWidth: Integer;
begin
  Result := 22 + FBookMarkOpt.LeftMargin
end;

destructor TSynGutterMarks.Destroy;
begin
  FreeAndNil(FInternalImage);
  inherited Destroy;
end;

function TSynGutterMarks.PaintMarks(aScreenLine: Integer; Canvas : TCanvas;
  AClip : TRect; var aFirstCustomColumnIdx: integer): Boolean;
var
  LineHeight: Integer;

  procedure DoPaintMark(CurMark: TSynEditMark; aRect: TRect);
  var
    img: TImageList;
  begin
    if CurMark.InternalImage or
       ( (not assigned(FBookMarkOpt.BookmarkImages)) and
         (not assigned(CurMark.ImageList)) )
    then begin
      // draw internal image
      if CurMark.ImageIndex in [0..9] then
      begin
        try
          if (not Assigned(FInternalImage)) and (not FNoInternalImage) then
            FInternalImage := TSynInternalImage.Create('SynEditInternalImages',10);
        except
          FNoInternalImage := True;
        end;
        if Assigned(FInternalImage) then
          FInternalImage.DrawMark(Canvas, CurMark.ImageIndex, aRect.Left, aRect.Top,
                                LineHeight);
      end;
    end
    else begin
      // draw from ImageList
      if assigned(CurMark.ImageList) then
        img := CurMark.ImageList
      else
        img := FBookMarkOpt.BookmarkImages;

      if (CurMark.ImageIndex <= img.Count) and (CurMark.ImageIndex >= 0) then begin
        if LineHeight > img.Height then
          aRect.Top := (aRect.Top + aRect.Bottom - img.Height) div 2;

        img.Draw(Canvas, aRect.Left, aRect.Top, CurMark.ImageIndex, True);
      end;
    end
  end;

var
  j: Integer;
  MLine: TSynEditMarkLine;
  MarkRect: TRect;
  LastMarkIsBookmark: Boolean;
begin
  Result := False;
  aFirstCustomColumnIdx := 0;
  if FBookMarkOpt.DrawBookmarksFirst then
    aFirstCustomColumnIdx := 1;
  MLine := TCustomSynEdit(SynEdit).Marks.Line[FoldView.TextIndex[aScreenLine] + 1];
  if MLine = nil then
    exit;

  if FBookMarkOpt.DrawBookmarksFirst then
    MLine.Sort(smsoBookmarkFirst, smsoPriority)
  else
    MLine.Sort(smsoBookMarkLast, smsoPriority);

  LineHeight := TCustomSynEdit(SynEdit).LineHeight;
  //Gutter.Paint always supplies AClip.Left = GutterPart.Left
  MarkRect := Rect(AClip.Left + FBookMarkOpt.LeftMargin,
                   aScreenLine * LineHeight,
                   AClip.Left + FColumnWidth,
                   (aScreenLine+1) * LineHeight);


  LastMarkIsBookmark := FBookMarkOpt.DrawBookmarksFirst;
  for j := 0 to MLine.Count - 1 do begin
    if (not MLine[j].Visible) or
       (MLine[j].IsBookmark and (not FBookMarkOpt.GlyphsVisible))
    then
      continue;

    if (MLine[j].IsBookmark <> LastMarkIsBookmark) and
       (j = 0) and (FColumnCount > 1)
    then begin
      // leave one column empty
      MarkRect.Left := MarkRect.Right;
      MarkRect.Right := Max(MarkRect.Right + FColumnWidth, AClip.Right);
    end;

    DoPaintMark(MLine[j], MarkRect);
    MarkRect.Left := MarkRect.Right;
    MarkRect.Right := Max(MarkRect.Right + FColumnWidth, AClip.Right);

    Result := Result or (not MLine[j].IsBookmark); // Line has a none-bookmark glyph
    if (MLine[j].IsBookmark <> LastMarkIsBookmark)  and
       (not MLine[j].IsBookmark) and (j > 0)
    then
      aFirstCustomColumnIdx := j; // first none-bookmark column

    if j > ColumnCount then break;
    LastMarkIsBookmark := MLine[j].IsBookmark;
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
  i: integer;
begin
  if not Visible then exit;
  if MarkupInfo.Background <> clNone then
    Canvas.Brush.Color := MarkupInfo.Background
  else
    Canvas.Brush.Color := Gutter.Color;
  LCLIntf.SetBkColor(Canvas.Handle, TColorRef(Canvas.Brush.Color));

  if assigned(FBookMarkOpt) and assigned(FBookMarkOpt.BookmarkImages) then
    FColumnWidth := FBookMarkOpt.BookmarkImages.Width
  else
    FColumnWidth := Width;
  FColumnCount := Max((Width+1) div FColumnWidth, 1); // full columns

  if FBookMarkOpt.GlyphsVisible and (LastLine >= FirstLine) then
  begin
    for i := FirstLine to LastLine do
      PaintLine(i, Canvas, AClip);
  end;
end;

end.

