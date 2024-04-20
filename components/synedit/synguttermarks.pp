unit SynGutterMarks;

{$I synedit.inc}

interface

uses
  Classes, SysUtils, Graphics, LCLType, LCLIntf, Controls, ImgList, FPCanvas,
  SynGutterBase, SynEditMiscClasses, SynEditMarks, LazSynEditText,
  SynEditMiscProcs;

type

  TSynGutterMarksOption = (
    sgmoDeDuplicateMarks,            // don't show consecutive marks with same icon
    sgmoDeDuplicateMarksKeepTwo,     // show max 2 consecutive, if not limited by MaxMarksCount
    sgmoDeDuplicateMarksOnOverflow   // remove consecutive dups, until there are more than MaxMarksCount marks
  );
  TSynGutterMarksOptions = set of TSynGutterMarksOption;

  { TSynGutterMarks }

  TSynGutterMarks = class(TSynGutterPartBase)
  private
    FColumnCount: Integer;
    FMaxExtraMarksColums: Integer;
    FOptions: TSynGutterMarksOptions;
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
    // Max amount of marks show in addition to ColumnCount
    property MaxExtraMarksColums: Integer read FMaxExtraMarksColums write FMaxExtraMarksColums;
    property Options: TSynGutterMarksOptions read FOptions write FOptions;
    property MarkupInfoCurrentLine;
  end;

implementation

{ TSynGutterMarks }

constructor TSynGutterMarks.Create(AOwner: TComponent);
begin
  FInternalImage := nil;
  FDebugMarksImageIndex := -1;
  FNoInternalImage := False;
  FOptions := [sgmoDeDuplicateMarksOnOverflow];
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
  j, lm, StoredColumnWidth, lx, vcnt, VCntU, VCnt2, k2cnt, del3cnt: Integer;
  MLine: TSynEditMarkLine;
  MarkRect: TRect;
  iRange: TLineRange;
  prev_iidx, pprev_iidx: LongInt;
begin
  Result := False;
  aFirstCustomColumnIdx := 0;
  if (FBookMarkOpt.DrawBookmarksFirst) then
    aFirstCustomColumnIdx := 1;
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

  vcnt := MLine.VisibleCount;

  if (FOptions * [sgmoDeDuplicateMarks, sgmoDeDuplicateMarksKeepTwo] <> []) or
     ((sgmoDeDuplicateMarksOnOverflow in FOptions) and (vcnt > ColumnCount))
  then begin
    VCntU := 1;
    VCnt2 := 0;
    for j := 1 to MLine.Count - 1 do
      if MLine[j].ImageIndex <> MLine[j-1].ImageIndex then
        inc(VCntU)  // Uniq
      else
      if (j=1) or (MLine[j].ImageIndex <> MLine[j-2].ImageIndex) then
        inc(VCnt2); // sgmoDeDuplicateMarksKeepTwo

    if (sgmoDeDuplicateMarks in FOptions) then begin
      vcnt := Min(vcntU, ColumnCount + MaxExtraMarksColums);
      k2cnt := 0;
    end
    else
    if (sgmoDeDuplicateMarksKeepTwo in FOptions) then begin
      k2cnt := Min(VCnt2, Max(0, ColumnCount + MaxExtraMarksColums - vcntU));
      vcnt := Min(vcntU+k2cnt, ColumnCount + MaxExtraMarksColums);
    end
    else begin
      // only dedup for MaxExtraMarksColums
      k2cnt := Min(VCnt2, Max(0, ColumnCount + MaxExtraMarksColums - vcntU));
      vcnt := Min(vcntU+k2cnt, ColumnCount + MaxExtraMarksColums);
    end;
    del3cnt := MLine.VisibleCount - vcnt;
  end
  else begin
    vcnt := Min(vcnt, ColumnCount + MaxExtraMarksColums);
    k2cnt := MaxInt; // keep duplicate if exactly 2nd
    del3cnt := 0;    // del 3rd or later
  end;

  aFirstCustomColumnIdx := 0;
  LineHeight := SynEdit.LineHeight;
  //Gutter.Paint always supplies AClip.Left = GutterPart.Left
  lm := LeftMarginAtCurrentPPI;
  StoredColumnWidth := FColumnWidth;
  prev_iidx := low(integer);
  pprev_iidx := low(integer);
  try
    lx := 0;
    if vcnt > ColumnCount then begin
      lx := FColumnWidth;
      FColumnWidth := Min(lx, Max(2, (Width + vcnt - 1) div vcnt));
      lx := (lx - FColumnWidth - 1) div 2;
    end;
    MarkRect := Rect(AClip.Left + lm - lx,
                     AClip.Top,
                     AClip.Left + lm - lx + FColumnWidth,
                     AClip.Top + LineHeight);


    for j := 0 to MLine.Count - 1 do begin
      if (not MLine[j].Visible) or
         (MLine[j].IsBookmark and (not FBookMarkOpt.GlyphsVisible))
      then
        continue;

      if (j = 0) and FBookMarkOpt.DrawBookmarksFirst and
         (vcnt < ColumnCount) and (not MLine[j].IsBookmark)
      then begin
        // leave one column empty
        MarkRect.Left := MarkRect.Right;
        MarkRect.Right := Min(MarkRect.Right + FColumnWidth, AClip.Right);
        inc(aFirstCustomColumnIdx);
      end;

      if MLine[j].ImageIndex = prev_iidx then begin
        if (MLine[j].ImageIndex = pprev_iidx) and (del3cnt > 0) then begin
          dec(del3cnt);
          continue;
        end;
        if k2cnt = 0 then
          Continue;
        dec(k2cnt);
      end;

      DoPaintMark(MLine[j], MarkRect);
      MarkRect.Left := MarkRect.Right;
      MarkRect.Right := Min(MarkRect.Right + FColumnWidth, AClip.Right);

      Result := Result or (not MLine[j].IsBookmark); // Line has a none-bookmark glyph
      inc(aFirstCustomColumnIdx);

      if aFirstCustomColumnIdx > ColumnCount + MaxExtraMarksColums then
        break;

      pprev_iidx := prev_iidx;
      prev_iidx := MLine[j].ImageIndex;
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

