unit SynGutterMarks;

{$I synedit.inc}

interface

uses
  Classes, SysUtils, Graphics, LCLType, LCLIntf, Controls, ImgList, FPCanvas,
  SynGutterBase, SynEditMiscClasses, SynEditMarks, LazSynEditText,
  SynEditMiscProcs;

type

  { TSynGutterMarks }

  TSynGutterMarks = class(TSynGutterPartBase)
  private
    FColumnCount: Integer;
    FWantedColumns: integer;
    FColumnWidth: Integer;
    FDebugMarksImageIndex: Integer;
    FInternalImage: TSynInternalImage;
    FNoInternalImage: Boolean;
  protected
    FBookMarkOpt: TSynBookMarkOpt;
    procedure Init; override;
    function  PreferedWidth: Integer; override;
    function  LeftMarginAtCurrentPPI: Integer;
    function GetImgListRes(const ACanvas: TCanvas;
      const AImages: TCustomImageList): TScaledImageListResolution; virtual;
    // PaintMarks: True, if it has any Mark, that is *not* a bookmark
    function  PaintMarks(aScreenLine: Integer; Canvas : TCanvas; AClip : TRect;
                       var aFirstCustomColumnIdx: integer): Boolean;
    Procedure PaintLine(aScreenLine: Integer; Canvas : TCanvas; AClip : TRect); virtual;
    property WantedColumns: integer read FWantedColumns;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Paint(Canvas: TCanvas; AClip: TRect; FirstLine, LastLine: integer); override;
    procedure SetWidthForColumns(AValue: Integer);
    property DebugMarksImageIndex: Integer read FDebugMarksImageIndex write FDebugMarksImageIndex;
    property ColumnWidth: Integer read FColumnWidth; // initialized in Paint
    property ColumnCount: Integer read FColumnCount;
  published
    property MarkupInfoCurrentLine;
  end;

implementation

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
  FBookMarkOpt := SynEdit.BookMarkOptions;
end;

function TSynGutterMarks.PreferedWidth: Integer;
begin
  Result := 22 + FBookMarkOpt.LeftMargin;
  if FWantedColumns > 0 then begin
    if assigned(FBookMarkOpt) and assigned(FBookMarkOpt.BookmarkImages) then begin
      FColumnWidth := GetImgListRes(FriendEdit.Canvas, FBookMarkOpt.BookmarkImages).Width;
      Result := FWantedColumns*FColumnWidth;
    end;
  end;
end;

function TSynGutterMarks.LeftMarginAtCurrentPPI: Integer;
begin
  Result := Scale96ToFont(FBookMarkOpt.LeftMargin);
end;

destructor TSynGutterMarks.Destroy;
begin
  FreeAndNil(FInternalImage);
  inherited Destroy;
end;

function TSynGutterMarks.GetImgListRes(const ACanvas: TCanvas;
  const AImages: TCustomImageList): TScaledImageListResolution;
var
  Scale: Double;
  PPI: Integer;
begin
  if ACanvas is TControlCanvas then
  begin
    Scale := TControlCanvas(ACanvas).Control.GetCanvasScaleFactor;
    PPI := TControlCanvas(ACanvas).Control.Font.PixelsPerInch;
  end else
  begin
    Scale := 1;
    PPI := ACanvas.Font.PixelsPerInch;
  end;
  Result := AImages.ResolutionForPPI[0, PPI, Scale];
end;

function TSynGutterMarks.PaintMarks(aScreenLine: Integer; Canvas : TCanvas;
  AClip : TRect; var aFirstCustomColumnIdx: integer): Boolean;
var
  LineHeight: Integer;

  procedure DoPaintMark(CurMark: TSynEditMark; aRect: TRect);
  var
    img: TScaledImageListResolution;
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
        img := GetImgListRes(Canvas, CurMark.ImageList)
      else
        img := GetImgListRes(Canvas, FBookMarkOpt.BookmarkImages);

      if (CurMark.ImageIndex <= img.Count) and (CurMark.ImageIndex >= 0) then begin
        if LineHeight > img.Height then
          aRect.Top := (aRect.Top + aRect.Bottom - img.Height) div 2;

        img.Draw(Canvas, aRect.Left, aRect.Top, CurMark.ImageIndex, True);
      end;
    end
  end;

var
  j, lm, StoredColumnWidth, lx: Integer;
  MLine: TSynEditMarkLine;
  MarkRect: TRect;
  LastMarkIsBookmark: Boolean;
  iRange: TLineRange;
begin
  Result := False;
  aScreenLine := aScreenLine + ToIdx(GutterArea.TextArea.TopLine);
  j := ViewedTextBuffer.DisplayView.ViewToTextIndexEx(aScreenLine, iRange);
  if aScreenLine <> iRange.Top then
    exit;
  if (j < 0) or (j >= SynEdit.Lines.Count) then
    exit;
  MLine := (SynEdit.Marks as TSynEditMarkList).Line[j + 1];
  if MLine = nil then
    exit;

  if FBookMarkOpt.DrawBookmarksFirst then
    MLine.Sort(smsoBookmarkFirst, smsoPriority)
  else
    MLine.Sort(smsoBookMarkLast, smsoPriority);

  aFirstCustomColumnIdx := 0;
  if (FBookMarkOpt.DrawBookmarksFirst) and (MLine.Count < ColumnCount) then
    aFirstCustomColumnIdx := 1;

  LineHeight := SynEdit.LineHeight;
  //Gutter.Paint always supplies AClip.Left = GutterPart.Left
  lm := LeftMarginAtCurrentPPI;
  StoredColumnWidth := FColumnWidth;
  try
    lx := 0;
    if MLine.Count > ColumnCount then begin
      lx := FColumnWidth;
      FColumnWidth := Min(lx, Max(2, (Width + MLine.Count - 1) div MLine.Count));
      lx := (lx - FColumnWidth - 1) div 2;
    end;
    MarkRect := Rect(AClip.Left + lm - lx,
                     AClip.Top,
                     AClip.Left + lm - lx + FColumnWidth,
                     AClip.Top + LineHeight);


    LastMarkIsBookmark := FBookMarkOpt.DrawBookmarksFirst;
    for j := 0 to MLine.Count - 1 do begin
      if (not MLine[j].Visible) or
         (MLine[j].IsBookmark and (not FBookMarkOpt.GlyphsVisible))
      then
        continue;

      if (MLine[j].IsBookmark <> LastMarkIsBookmark) and
         (j = 0) and (aFirstCustomColumnIdx >= 1)
      then begin
        // leave one column empty
        MarkRect.Left := MarkRect.Right;
        MarkRect.Right := Min(MarkRect.Right + FColumnWidth, AClip.Right);
      end;

      DoPaintMark(MLine[j], MarkRect);
      MarkRect.Left := MarkRect.Right;
      MarkRect.Right := Min(MarkRect.Right + FColumnWidth, AClip.Right);

      Result := Result or (not MLine[j].IsBookmark); // Line has a none-bookmark glyph
      if (MLine[j].IsBookmark <> LastMarkIsBookmark)  and
         (not MLine[j].IsBookmark) and (j > 0)
      then
        aFirstCustomColumnIdx := j; // first none-bookmark column

      //if j >= ColumnCount then break;
      LastMarkIsBookmark := MLine[j].IsBookmark;
    end;
  finally
    FColumnWidth := StoredColumnWidth;
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
  LineHeight: Integer;
  rcLine, clpr: TRect;
  clp: Boolean;
begin
  if not Visible then exit;
  PaintBackground(Canvas, AClip);

  if assigned(FBookMarkOpt) and assigned(FBookMarkOpt.BookmarkImages) then
    FColumnWidth := GetImgListRes(Canvas, FBookMarkOpt.BookmarkImages).Width
  else
    FColumnWidth := Width;
  if FColumnWidth = 0 then
    FColumnCount := 0
  else
    FColumnCount := Max((Width+1) div FColumnWidth, 1); // full columns

  if FBookMarkOpt.GlyphsVisible and (LastLine >= FirstLine) then
  begin
    clp  := Canvas.Clipping;
    clpr := Canvas.ClipRect;
    try
      rcLine := AClip;
      Canvas.ClipRect := rcLine;
      Canvas.Clipping := True;
      LineHeight := SynEdit.LineHeight;
      rcLine := AClip;
      rcLine.Bottom := rcLine.Top;
      for i := FirstLine to LastLine do begin
        rcLine.Top := rcLine.Bottom;
        rcLine.Bottom := Min(AClip.Bottom, rcLine.Top + LineHeight);
        PaintLine(i, Canvas, rcLine);
      end;
    finally
      Canvas.ClipRect := clpr;
      Canvas.Clipping := clp;
    end;
  end;
end;

procedure TSynGutterMarks.SetWidthForColumns(AValue: Integer);
begin
  if FWantedColumns = AValue then
    exit;
  FWantedColumns := AValue;
  if not AutoSize then
    AutoSize := True
  else
    DoAutoSize;
end;

end.

