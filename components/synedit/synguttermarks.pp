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
  protected type
    TSynEditMarkDrawInfo = record
      Mark: TSynEditMark;
      Images: TCustomImageList;
      IconIdx: integer;
    end;
    TSynEditMarkDrawInfoArray = array of TSynEditMarkDrawInfo;
  protected
    FBookMarkOpt: TSynBookMarkOpt;
    FTempDrawInfo: TSynEditMarkDrawInfoArray;
    procedure Init; override;
    function  PreferedWidth: Integer; override;
    function  LeftMarginAtCurrentPPI: Integer;
    function GetImgListRes(const ACanvas: TCanvas; const AImages: TCustomImageList): TScaledImageListResolution; virtual;
    function MarksToDrawInfo(AMLine: TSynEditMarkLine; var ADrawInfo: TSynEditMarkDrawInfoArray;
                              AMaxEntries: integer;
                              out aFirstCustomColumnIdx: integer; out AHasNonBookmark: boolean): integer; virtual;
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

function TSynGutterMarks.GetImgListRes(const ACanvas: TCanvas; const AImages: TCustomImageList
  ): TScaledImageListResolution;
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

function TSynGutterMarks.MarksToDrawInfo(AMLine: TSynEditMarkLine;
  var ADrawInfo: TSynEditMarkDrawInfoArray; AMaxEntries: integer; out
  aFirstCustomColumnIdx: integer; out AHasNonBookmark: boolean): integer;
var
  i, CntUniq, CntRep2, CntKeepRep2, CntDel3: Integer;
  prev_iidx, pprev_iidx: LongInt;
begin
  Result := AMLine.VisibleCount;
  if (FOptions * [sgmoDeDuplicateMarks, sgmoDeDuplicateMarksKeepTwo] <> []) or
     ((sgmoDeDuplicateMarksOnOverflow in FOptions) and (Result > ColumnCount))
  then begin
    CntUniq := 0;
    CntRep2 := 0;
    prev_iidx := low(integer);
    pprev_iidx := low(integer);
    for i := 0 to AMLine.Count - 1 do begin
      if (not AMLine[i].Visible) or
         (AMLine[i].IsBookmark and (not FBookMarkOpt.GlyphsVisible))
      then
        continue;
      if AMLine[i].ImageIndex <> prev_iidx then
        inc(CntUniq)  // Uniq
      else
      if (i=1) or (AMLine[i].ImageIndex <> pprev_iidx) then
        inc(CntRep2); // sgmoDeDuplicateMarksKeepTwo
      pprev_iidx := prev_iidx;
      prev_iidx := AMLine[i].ImageIndex;
    end;

    if (sgmoDeDuplicateMarks in FOptions) then begin
      Result := Min(cntUniq, AMaxEntries);
      CntKeepRep2 := 0;
    end
    else
    if (sgmoDeDuplicateMarksKeepTwo in FOptions) then begin
      CntKeepRep2 := Min(CntRep2, Max(0, AMaxEntries- cntUniq));
      Result := Min(cntUniq+CntKeepRep2, AMaxEntries);
    end
    else begin
      // only dedup for MaxExtraMarksColums
      CntKeepRep2 := Min(CntRep2, Max(0, AMaxEntries - cntUniq));
      Result := Min(cntUniq+CntKeepRep2, AMaxEntries);
    end;
    CntDel3 := AMLine.VisibleCount - Result;
  end
  else begin
    Result := Min(AMLine.VisibleCount, AMaxEntries);
    CntKeepRep2 := MaxInt; // keep duplicate if exactly 2nd
    CntDel3 := 0;    // del 3rd or later
  end;

  if Length(ADrawInfo) < Result then
    SetLength(ADrawInfo, AMaxEntries); // Expand to max needed (for further runs)

  aFirstCustomColumnIdx := 0;
  AHasNonBookmark := False;
  prev_iidx := low(integer);
  pprev_iidx := low(integer);
  for i := 0 to AMLine.Count - 1 do begin
    if (not AMLine[i].Visible) or
       (AMLine[i].IsBookmark and (not FBookMarkOpt.GlyphsVisible))
    then
      continue;

    if (i = 0) and FBookMarkOpt.DrawBookmarksFirst and
       (Result < ColumnCount) and (not AMLine[i].IsBookmark)
    then begin
      ADrawInfo[aFirstCustomColumnIdx].Mark := nil;
      ADrawInfo[aFirstCustomColumnIdx].IconIdx := 0;
      ADrawInfo[aFirstCustomColumnIdx].Images  := nil;
      inc(Result);
      inc(aFirstCustomColumnIdx);
    end;

    if AMLine[i].ImageIndex = prev_iidx then begin
      if (AMLine[i].ImageIndex = pprev_iidx) and (CntDel3 > 0) then begin
        dec(CntDel3);
        continue;
      end;
      if CntKeepRep2 = 0 then
        Continue;
      dec(CntKeepRep2);
    end;

    AHasNonBookmark := AHasNonBookmark or (not AMLine[i].IsBookmark); // Line has a none-bookmark glyph

    ADrawInfo[aFirstCustomColumnIdx].Mark := AMLine[i];
    ADrawInfo[aFirstCustomColumnIdx].IconIdx := 0;   //AMLine[i].ImageIndex;
    ADrawInfo[aFirstCustomColumnIdx].Images  := nil; //AMLine[i].ImageList;
    inc(aFirstCustomColumnIdx);
    if aFirstCustomColumnIdx >= AMaxEntries then
      break;

    pprev_iidx := prev_iidx;
    prev_iidx := AMLine[i].ImageIndex;
  end;
end;

function TSynGutterMarks.PaintMarks(aScreenLine: Integer; Canvas : TCanvas;
  AClip : TRect; var aFirstCustomColumnIdx: integer): Boolean;
var
  LineHeight: Integer;
  BkMkOptImg: TScaledImageListResolution;
  BkMkOptImgDone: Boolean;

  procedure DoPaintMark(const CurMarkInfo: TSynEditMarkDrawInfo; aRect: TRect);
  var
    img: TScaledImageListResolution;
    CurMark: TSynEditMark;
    idx: Integer;
  begin
    CurMark := CurMarkInfo.Mark;
    if (CurMark <> nil) and
       ( CurMark.InternalImage or
         ( (not assigned(FBookMarkOpt.BookmarkImages)) and
           (not assigned(CurMark.ImageList)) )
       )
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
      if CurMark = nil then begin
        if CurMarkInfo.Images = nil then
          exit;
        img := GetImgListRes(Canvas, CurMarkInfo.Images);
        idx := CurMarkInfo.IconIdx;
      end
      else
      if assigned(CurMark.ImageList) then begin
        img := GetImgListRes(Canvas, CurMark.ImageList);
        idx := CurMark.ImageIndex;
      end
      else begin
        if not BkMkOptImgDone then begin
          BkMkOptImg := GetImgListRes(Canvas, FBookMarkOpt.BookmarkImages);
          BkMkOptImgDone := True;
        end;
        img := BkMkOptImg;
        idx := CurMark.ImageIndex;
      end;

      if (idx <= img.Count) and (idx >= 0) then begin
        if LineHeight > img.Height then
          aRect.Top := (aRect.Top + aRect.Bottom - img.Height) div 2;

        img.Draw(Canvas, aRect.Left, aRect.Top, idx, True);
      end;
    end
  end;

var
  j, lm, StoredColumnWidth, lx, vcnt: Integer;
  MLine: TSynEditMarkLine;
  MarkRect: TRect;
  iRange: TLineRange;
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

  vcnt := MarksToDrawInfo(MLine, FTempDrawInfo, ColumnCount + MaxExtraMarksColums, aFirstCustomColumnIdx, Result);

  //Gutter.Paint always supplies AClip.Left = GutterPart.Left
  BkMkOptImgDone := False;
  LineHeight := SynEdit.LineHeight;
  StoredColumnWidth := FColumnWidth;
  try
    lm := LeftMarginAtCurrentPPI;
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

    for j := 0 to vcnt - 1 do begin
      DoPaintMark(FTempDrawInfo[j], MarkRect);

      MarkRect.Left := MarkRect.Right;
      MarkRect.Right := Min(MarkRect.Right + FColumnWidth, AClip.Right);
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

