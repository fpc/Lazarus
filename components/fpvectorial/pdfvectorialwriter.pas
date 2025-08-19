unit pdfvectorialwriter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Types, Graphics, Math, fpvectorial, fpPDF, fpTTF;

const
  {$IFDEF WINDOWS}
    FONT_DIR = 'C:\Windows\Fonts\';
  {$ELSE}
    {$IFDEF LINUX}  // TODO: there are different font locations for linux
      FONT_DIR = '/usr/local/share/fonts/';
    {$ELSE}
      FONT_DIR = '~/Library/Fonts';
    {$ENDIF}
  {$ENDIF}

  cInchToMM = 25.4;
  cInchToCM = 2.54;

type
  {
              T
          ____v____
          |       |
          |       |
      L > |       | < R
          |       |
          |_______|
              ^
              B

    Every entity being rendered should have a bound box
    limiting the space it can use. Those limits are set by
    left, top, right, bottom spacings (in mm). X and Y
    shall be used as cursors to add padding and navigate

  }
  TvBoundBox = record
    L, T, R, B: Double;
    X, Y: Double;  // cursor
  end;

  TvEntityKind = (ekParagraph, ekList, ekTable, ekText, ekField, ekImage, ekNone);

  {
    This record contains the entity kind for faster
    checking, a bounding box for that entity and the
    exact height and width (not yet for all entities).

    Height and width shall be the exact measurements of a
    given entity while the bounding box should be the
    space the entity theoretically has to it's disposition.

    This should enable future line wrapping if width is
    larger than the available space.
  }
  TvEntityInfo = record
    Kind: TvEntityKind;
    Box: TvBoundBox;
    Height: Double;
  end;

  {
    This record summarises all font information needed.
    The TvFont contains all relevant font information for
    drawing, the Cache contains a cache item used to
    calculate height and width of texts.
    The ID contains the font identifier to access the
    font from the pdf document.
  }
  TvPDFFont = record
    Font: TvFont;
    Cache: TFPFontCacheItem;
    ID: Integer;
  end;

  TvPDFFontArray = array of TvPDFFont;

  TvPDFVectorialWriter = class(TvCustomVectorialWriter)
  private
    FPointSeparator: TFormatSettings;

    FDocument: TPDFDocument;
    FSection: TPDFSection;
    FPage: TPDFPage;

    FFonts: TvPDFFontArray;

    // default style
    FStyle: TvStyle;

    // line space (TODO: customisability)
    FLineSpace: Double;
    FSpacing: Double;

    { utilities }
    function GetBorderWidth(AType: TvTableBorderType): Double;
    function GetStyle(AText: TvText; APara: TvParagraph): TvStyle;
    function UnitToMM(AValue: Double; AUnit: TvUnits; Box: TvBoundBox): Double;
    function GetEntityInfo(AEntity, AParent: TvEntity; ABox: TvBoundBox): TvEntityInfo;
    function GetFontID(AName: String; IsBold, IsItalic: Boolean): Integer;
    function GetFontIDnew(AName: String; IsBold, IsItalic: Boolean): Integer;
    function GetFont(AName: String; IsBold, IsItalic: Boolean): TvPDFFont;
    function TabsToSpaces(AText: String): String;
    function GetListNum(ANum: TIntegerDynArray): String;

    { entity measurements }
    function GetHeight(Entity, AParent: TvEntity): Double;
    function GetHeight(AEntity, AParent: TvEntity; AKind: TvEntityKind): Double;
    function GetTextHeight(AText: TvText; APara: TvParagraph): Double;
    function GetTextWidth(AText: TvText; APara: TvParagraph): Double;
    function GetFontHeight(AText: TvText): Double;
    function GetFontWidth(AText: TvText): Double;
    function GetParagraphHeight(APara: TvParagraph): Double;
    function GetListHeight(AList: TvList): Double;
    function GetTableHeight(ATable: TvTable): Double;
    function GetRowHeight(ARow: TvTableRow; ALast: Boolean): Double;
    function GetCellHeight(ACell: TvTableCell; ALast: Boolean): Double;
    function GetFieldHeight(AField: TvField): Double;
    function GetImageHeight(AImage: TvRasterImage): Double;
    function GetWidth(AEntity, AParent: TvEntity; AKind: TvEntityKind; ABox: TvBoundBox): Double;
    function GetImageWidth(AImage: TvRasterImage): Double;

    { add entities }
    procedure AddFonts(AData: TvVectorialDocument);
    function AddPage(APage: TvTextPageSequence): TvBoundBox;
    procedure AddParagraph(APara: TvParagraph; ABox: TvBoundBox);
    procedure AddList(AList: TvList; ABox: TvBoundBox; ANum: TIntegerDynArray = nil);
    procedure AddTable(ATable: TvTable; APage: TvTextPageSequence; AData: TvVectorialDocument; ABox: TvBoundBox);
    procedure AddText(AText: TvText; APara: TvParagraph; ABox: TvBoundBox);
    procedure AddField(AField: TvField; ABox: TvBoundBox);
    procedure AddImage(AImage: TvRasterImage; ABox: TvBoundBox);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure WriteToFile(AFileName: String; AData: TvVectorialDocument); override;
    procedure WriteToStream(AStream: TStream; AData: TvVectorialDocument); override;
  end;

implementation

{ TvPDFVectorialWriter }

function Inc(var AValue: Double; AIncrement: Double): Double;
begin
  AValue := AValue + AIncrement;
  Result := AValue;
end;

function Dec(var AValue: Double; ADecrement: Double): Double;
begin
  AValue := AValue - ADecrement;
  Result := AValue;
end;

// definitions for the border widths
function TvPDFVectorialWriter.GetBorderWidth(AType: TvTableBorderType): Double;
begin
  case AType of
    tbtNone: Result := 0;
    tbtSingle: Result := 1;
    tbtDefault: Result := 0.4;
    tbtDouble: Result := 2;
    tbtDashed: Result := 0;  // TODO: add dashed line
  end;
end;

function TvPDFVectorialWriter.GetStyle(AText: TvText; APara: TvParagraph): TvStyle;
begin
  Result := AText.GetCombinedStyle(APara);
  if Result = nil then Result := FStyle;
end;

function TvPDFVectorialWriter.GetEntityInfo(AEntity, AParent: TvEntity; ABox: TvBoundBox): TvEntityInfo;
begin
  Result.Box := ABox;
  if (AEntity is TvParagraph) then
    Result.Kind := ekParagraph
  else if (AEntity is TvList) then
    Result.Kind := ekList
  else if (AEntity is TvTable) then
    Result.Kind := ekTable
  else if (AEntity is TvText) then
    Result.Kind := ekText
  else if (AEntity is TvField) then
    Result.Kind := ekField
  else if (AEntity is TvRasterImage) then
    Result.Kind := ekImage;
  Result.Height := GetHeight(AEntity, AParent, Result.Kind);
  Result.Box.B := Result.Box.T + Result.Height;
  Result.Box.R := GetWidth(AEntity, AParent, Result.Kind, Result.Box);
  Result.Box.X := Result.Box.L;
  Result.Box.Y := Result.Box.T;
end;

// convert tabs to spaces (really only working for monospace fonts)
function TvPDFVectorialWriter.TabsToSpaces(AText: String): String;
const
  TAB_SIZE = 6;
var
  TabPos, i: Integer;
  Tmp: String;
begin
  for i := 0 to AText.CountChar(#9) do
  begin
    TabPos := Pos(#9, AText) - 1;
    Tmp := AText.Substring(0, TabPos);
    while Tmp.Length mod TAB_SIZE <> 0 do
      Tmp := Tmp + ' ';
    AText := AText.Replace(AText.Substring(0, TabPos) + #9, Tmp);
  end;
  Result := AText;
end;

function TvPDFVectorialWriter.GetListNum(ANum: TIntegerDynArray): String;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Length(ANum) - 1 do
    Result := Result + ANum[i].ToString + '.';
  Result := TrimRightSet(Result, ['.']);
end;

function TvPDFVectorialWriter.GetHeight(AEntity, AParent: TvEntity; AKind: TvEntityKind): Double;
begin
  case AKind of
    ekParagraph: Result := GetParagraphHeight(TvParagraph(AEntity));
    ekList: Result := GetListHeight(TvList(AEntity));
    ekTable: Result := GetTableHeight(TvTable(AEntity));
    ekText: Result := GetTextHeight(TvText(AEntity), TvParagraph(AParent));
    ekField: Result := GetFieldHeight(TvField(AEntity));
    ekImage: Result := GetImageHeight(TvRasterImage(AEntity));
  end;
end;

function TvPDFVectorialWriter.GetWidth(AEntity, AParent: TvEntity; AKind: TvEntityKind; ABox: TvBoundBox): Double;
begin
  case AKind of
    ekText: Result := ABox.L + GetTextWidth(TvText(AEntity), TvParagraph(AParent));
    ekImage: Result := ABox.L + GetImageWidth(TvRasterImage(AEntity));
    else Result := ABox.R;  // TODO: add width for other entities
  end;
end;

function TvPDFVectorialWriter.GetHeight(Entity, AParent: TvEntity): Double;
begin
  if Entity is TvText then
    Exit(GetTextHeight(TvText(Entity), TvParagraph(AParent)));
  if Entity is TvParagraph then
    Exit(GetParagraphHeight(TvParagraph(Entity)));
  if Entity is TvList then
    Exit(GetListHeight(TvList(Entity)));
  if Entity is TvTable then
    Exit(GetTableHeight(TvTable(Entity)));
  if Entity is TvField then
    Exit(GetFieldHeight(TvField(Entity)));
  if Entity is TvRasterImage then
    Exit(GetImageHeight(TvRasterImage(Entity)));
end;

function TvPDFVectorialWriter.GetTextHeight(AText: TvText; APara: TvParagraph): Double;
begin
  AText.Style := GetStyle(AText, APara);
  Result := AText.Style.MarginTop + GetFontHeight(AText) + AText.Style.MarginBottom + FSpacing;
end;

function TvPDFVectorialWriter.GetTextWidth(AText: TvText; APara: TvParagraph): Double;
begin
  AText.Style := GetStyle(AText, APara);
  Result := AText.Style.MarginLeft + GetFontWidth(AText) + AText.Style.MarginLeft;
end;

function TvPDFVectorialWriter.GetFontHeight(AText: TvText): Double;
var
  Descender: Single;  // how far does the text dip below the baseline
  Font: TvPDFFont;
begin
  Font := GetFont(AText.Style.Font.Name, AText.Style.Font.Bold, AText.Style.Font.Italic);
  Assert(Font.Cache <> nil, 'no font cache found');
  Result := Font.Cache.TextHeight(AText.Value.Text, AText.Style.Font.Size, Descender);
  //Result := (Result * cInchToMM) / gTTFontCache.DPI;
  //Result := PDFToMM(AText.Style.Font.Size);
  Result := PDFToMM(Result);  // seems to be the best way
end;

function TvPDFVectorialWriter.GetFontWidth(AText: TvText): Double;
var
  Font: TvPDFFont;
begin
  Font := GetFont(AText.Style.Font.Name, AText.Style.Font.Bold, AText.Style.Font.Italic);
  Assert(Font.Cache <> nil, 'no font cache found');
  Result := Font.Cache.TextWidth(AText.Value.Text, AText.Style.Font.Size);
  Result := (Result * cInchToMM) / gTTFontCache.DPI;
end;

function TvPDFVectorialWriter.GetParagraphHeight(APara: TvParagraph): Double;
var
  Tmp: Double;
  i: Integer;
begin
  Result := 0;
  if APara.GetEntitiesCount = 0 then
  begin
    if APara.Style <> nil then
    begin
      // empty paragraphs need to take space as if there
      // is some text, like in odt and docx documents
      APara.Style.MarginTop := 0;
      APara.Style.MarginBottom := 0;
      APara.AddText('');
    end;
  end;
  for i := 0 to APara.GetEntitiesCount - 1 do
  begin
    Tmp := GetHeight(APara.GetEntity(i), APara);
    if Tmp > Result then Result := Tmp;
  end;
end;

function TvPDFVectorialWriter.GetListHeight(AList: TvList): Double;
var
  i: Integer;
  Entity: TvEntity;
begin
  Result := 0;
  if not Assigned(AList.Style) then
    raise Exception.Create('PDFVectorialWriter: List.Style not set');
  if not Assigned(AList.ListStyle) then
    raise Exception.Create('PDFVectorialWriter: List.ListStyle not set');
  for i := 0 to AList.GetEntitiesCount - 1 do
  begin
    Entity := AList.GetEntity(i);
    Inc(Result, GetHeight(Entity, AList));
    if (Entity is TvParagraph) then
      Inc(Result, AList.Style.MarginTop + AList.Style.MarginBottom);
  end;
end;

function TvPDFVectorialWriter.GetTableHeight(ATable: TvTable): Double;
var
  i: Integer;
begin
  Result := 0;
  for I := 0 to ATable.GetRowCount - 1 do
  begin
    if i = ATable.GetRowCount - 1 then
      Inc(Result, GetRowHeight(ATable.GetRow(i), true))
    else
      Inc(Result, GetRowHeight(ATable.GetRow(i), false));
  end;
end;

function TvPDFVectorialWriter.GetRowHeight(ARow: TvTableRow; ALast: Boolean): Double;
var
  Tmp: Double;
  i: Integer;
begin
  Result := 0;
  for i := 0 to ARow.GetCellCount - 1 do
  begin  // get height of tallest cell
    Tmp := GetCellHeight(ARow.GetCell(i), ALast);
    if Tmp > Result then Result := Tmp;
  end;
end;

function TvPDFVectorialWriter.GetCellHeight(ACell: TvTableCell; ALast: Boolean): Double;
var
  Para: TvParagraph;
  i: Integer;
begin
  Result := ACell.SpacingTop + ACell.SpacingBottom;
  for i := 0 to ACell.GetEntitiesCount - 1 do
    Inc(Result, GetHeight(ACell.GetEntity(i), ACell));
  if ALast and (ACell.GetEntity(ACell.GetEntitiesCount - 1) is TvParagraph) then
  begin
    Para := TvParagraph(ACell.GetEntity(ACell.GetEntitiesCount - 1));
    if (Para.GetEntity(Para.GetEntitiesCount - 1) is TvText) then
      Inc(Result, FLineSpace - FSpacing);
  end;
end;

function TvPDFVectorialWriter.GetFieldHeight(AField: TvField): Double;
begin
  Result := 0;  // TODO: add field height
end;

function TvPDFVectorialWriter.GetImageHeight(AImage: TvRasterImage): Double;
begin
  if IsZero(AImage.Width) or IsZero(AImage.Height) then
    Result := (AImage.RasterImage.Height * cInchToCM) / 96
  else
    Result := AImage.Height * 10;
end;

function TvPDFVectorialWriter.GetImageWidth(AImage: TvRasterImage): Double;
begin
  if IsZero(AImage.Width) or IsZero(AImage.Height) then
    Result := (AImage.RasterImage.Width * cInchToCM) / 96
  else
    Result := AImage.Width * 10;
end;

function TvPDFVectorialWriter.UnitToMM(AValue: Double; AUnit: TvUnits; Box: TvBoundBox): Double;
begin
  case AUnit of
    dimMillimeter: Result := AValue;
    dimPercent: Result := (Box.R - Box.L) * (AValue / 100);
    dimPoint: Result := PDFTomm(AValue);
  end;
end;

// returns the font ID for a specific style
function TvPDFVectorialWriter.GetFontID(AName: String; IsBold, IsItalic: Boolean): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Length(FFonts) - 1 do
  begin
    if FFonts[i].Font.Name = AName then
    begin
      if (FFonts[i].Font.Bold = IsBold) and (FFonts[i].Font.Italic = IsItalic) then
        Exit(FFonts[i].ID);
    end;
  end;
end;

function TvPDFVectorialWriter.GetFontIDnew(AName: String; IsBold, IsItalic: Boolean): Integer;
var
  Search: String;
begin
  Search := AName;
  if IsBold then Search := Search + ' Bold';
  if IsItalic then Search := Search + ' Italic';
  Result := FDocument.Fonts.FindFont(Search);
  if Result = -1 then Result := 0;
end;

function TvPDFVectorialWriter.GetFont(AName: String; IsBold, IsItalic: Boolean): TvPDFFont;
var
  i: Integer;
begin
  Result := FFonts[0];
  for i := 0 to Length(FFonts) - 1 do
  begin
    if FFonts[i].Font.Name = AName then
    begin
      if (FFonts[i].Font.Bold = IsBold) and (FFonts[i].Font.Italic = IsItalic) then
        Exit(FFonts[i]);
    end;
  end;
end;

procedure TvPDFVectorialWriter.AddFonts(AData: TvVectorialDocument);
var
  Cache: TFPFontCacheItem;
  NewName: String;
  Font: TvFont;
  i, j: Integer;
begin
  SetLength(FFonts, 1);
  FFonts[0].Font := FStyle.Font;
  FFonts[0].Cache := gTTFontCache.Find(FStyle.Font.Name, FStyle.Font.Bold, FStyle.Font.Italic);
  FFonts[0].ID := FDocument.AddFont(FFonts[0].Cache.FileName, FStyle.Font.Name);
  j := 1;
  for i := 0 to AData.GetStyleCount - 1 do
  begin
    Font := AData.GetStyle(i).Font;
    if (Font.Name = '') then Continue;
    NewName := Font.Name;
    if Font.Bold then
      NewName := NewName + ' Bold';
    if Font.Italic then NewName := NewName + ' Italic';
    if FDocument.Fonts.FindFont(NewName) = -1 then
    begin
      Cache := gTTFontCache.Find(Font.Name, Font.Bold, Font.Italic);
      if Cache = nil then Continue;
      SetLength(FFonts, Length(FFonts) + 1);
      Assert(Length(FFonts) > j, 'array should be large enough');
      FFonts[j].Font := Font;
      FFonts[j].ID := FDocument.AddFont(Cache.FileName, NewName);
      FFonts[j].Cache := Cache;
      j := j + 1;
    end;
  end;
end;

// add pdf page to document
function TvPDFVectorialWriter.AddPage(APage: TvTextPageSequence): TvBoundBox;
begin
  if APage = nil then
    raise Exception.Create('[TvPDFVectorialWriter.AddPage] APage is nil.');

  // add new page
  FPage := FDocument.Pages.AddPage;
  FSection.AddPage(FPage);

  // page options could be made selectable
  FPage.PaperType := ptA4;
  FPage.Orientation := ppoPortrait;
  FPage.UnitOfMeasure := uomMillimeters;

  // set bounds for page
  Result.T := APage.MarginTop;
  Result.L := APage.MarginLeft;
  Result.B := PDFTomm(FPage.Paper.H) - APage.MarginBottom;
  Result.R := PDFTomm(FPage.Paper.W) - APage.MarginRight;
  Result.X := Result.L;
  Result.Y := Result.T;
end;

procedure TvPDFVectorialWriter.AddParagraph(APara: TvParagraph; ABox: TvBoundBox);
var
  Info: TvEntityInfo;
  Entity: TvEntity;
  i, di: Integer;
begin
  if APara.Style = nil then APara.Style := FStyle;
  if APara.Style.Alignment = vsaRight then
  begin
    i := APara.GetEntitiesCount - 1;
    di := -1;
  end else
  begin
    i := 0;
    di := +1;
  end;
  while (i >= 0) and (i < APara.GetEntitiesCount) do
  begin
    Entity := APara.GetEntity(i);
    Info := GetEntityInfo(Entity, APara, ABox);
    case Info.Kind of
      ekText:
        begin
          case TvText(Entity).Style.Alignment of
            vsaRight:
              begin
                Info.Box.R := ABox.R;
                Info.Box.L := ABox.R - TvText(Entity).Style.MarginRight - GetFontWidth(TvText(Entity));
                Info.Box.X := Info.Box.L;
                ABox.R := Info.Box.L;
              end;
            vsaCenter:
              begin
                Info.Box.R := ABox.R;
                Info.Box.X := Info.Box.X + ((Info.Box.R - Info.Box.L) / 2) - (GetFontWidth(TvText(Entity)) / 2) - TvText(Entity).Style.MarginLeft;
              end;
          end;
          AddText(TvText(Entity), APara, Info.Box);
        end;
      ekField: AddField(TvField(Entity), Info.Box);
      ekImage:
        begin
          case APara.Style.Alignment of
            vsaRight:
              begin
                Info.Box.R := ABox.R;
                Info.Box.L := ABox.R - GetImageWidth(TvRasterImage(Entity));
                Info.Box.X := Info.Box.L;
                ABox.R := Info.Box.L;
              end;
            vsaCenter:
              begin
                Info.Box.R := ABox.R;
                Info.Box.X := Info.Box.X + ((Info.Box.R - Info.Box.L) / 2) - (GetImageWidth(TvRasterImage(Entity)) / 2);
              end;
          end;
          AddImage(TvRasterImage(Entity), Info.Box);
        end;
    end;
    // move to the right
    ABox.L := Info.Box.R;
    i := i + di;  // di is negative for right alignment.
  end;
end;

procedure TvPDFVectorialWriter.AddList(AList: TvList; ABox: TvBoundBox; ANum: TIntegerDynArray);
var
  LevelStyle: TvListLevelStyle;
  Info, Text: TvEntityInfo;
  Para: TvParagraph;
  Entity: TvEntity;
  Bullet: String;
  X, Y: Double;
  i: Integer;
begin
  if Length(ANum) > 0 then
  begin  // only needed for numeric bullet style
    SetLength(ANum, Length(ANum) + 1);
    ANum[Length(ANum)] := 0;
  end
  else
  begin
    SetLength(ANum, 1);
    ANum[0] := 0;
  end;
  for i := 0 to AList.GetEntitiesCount - 1 do
  begin
    Entity := AList.GetEntity(i);
    Info := GetEntityInfo(Entity, AList, ABox);
    case Info.Kind of
      ekParagraph:
        begin
          Para := TvParagraph(Entity);
          if not Assigned(Para.Style) then
            Para.Style := AList.Style;

          // get bullet style
          ANum[Length(ANum) - 1] := ANum[Length(ANum) - 1] + 1;
          LevelStyle := AList.ListStyle.GetListLevelStyle(AList.GetLevel);
          case LevelStyle.Kind of
            vlskBullet: Bullet := LevelStyle.Bullet;
            vlskNumeric: Bullet := LevelStyle.Prefix + GetListNum(ANum) + LevelStyle.Suffix;
          end;

          // add margin
          Inc(Info.Box.B, AList.Style.MarginTop + AList.Style.MarginBottom);
          Inc(Info.Box.T, AList.Style.MarginTop);
          Inc(Info.Box.L, LevelStyle.MarginLeft);
          Info.Box.X := Info.Box.L;
          Info.Box.Y := Info.Box.T;

          // write bullet
          if (Para.GetEntitiesCount > 0) and (Para.GetEntity(0) is TvText) then  // Text
          begin
            Text := GetEntityInfo(Para.GetEntity(0), Para, Info.Box);
            X := Info.Box.L - LevelStyle.HangingIndent;
            Y := Info.Box.T + Text.Height / 2 + 0.9;
          end
          else  // Text is not the first entity (-> centered bullet)
          begin
            X := Info.Box.L - LevelStyle.HangingIndent;
            Y := Info.Box.T + Info.Height / 2 + 0.9;
          end;

          if Bullet = '&#183;' then  // point (this char can't be printed to pdf)
            FPage.DrawEllipse(X, Y - 0.25, 1, 1, 1)  // default bullet char
          else
            FPage.WriteText(X, Y, Bullet);

          AddParagraph(Para, Info.Box);
        end;
      ekList: AddList(TvList(Entity), ABox, ANum);
    end;
    ABox.T := Info.Box.B;  // next item
  end;
end;

procedure TvPDFVectorialWriter.AddTable(ATable: TvTable; APage: TvTextPageSequence; AData: TvVectorialDocument; ABox: TvBoundBox);
var
  RBox, CBox, EBox: TvBoundBox;
  CWidth, CLineWidth: Double;
  Info: TvEntityInfo;
  Cell: TvTableCell;
  i, j, k, l: Integer;
  Entity: TvEntity;
  Row: TvTableRow;
  CSame: Boolean;
begin
  // set table bounds
  RBox := ABox;
  CSame := (ATable.ColWidths = nil);
  CWidth := UnitToMM(100 / ATable.GetColCount(), dimPercent, ABox);
  for i := 0 to ATable.GetRowCount - 1 do
  begin
    // current row
    Row := ATable.GetRow(i);
    if i = ATable.GetRowCount - 1 then
      RBox.B := RBox.T + GetRowHeight(Row, true)
    else
      RBox.B := RBox.T + GetRowHeight(Row, false);
    CBox := RBox;
    l := 0;  // counter for col widths
    for j := 0 to Row.GetCellCount - 1 do
    begin
      // current cell
      Cell := Row.GetCell(j);

      // set cursor
      CBox.X := CBox.L + Cell.SpacingLeft;
      CBox.Y := CBox.T + Cell.SpacingTop;

      if not CSame then  // get cell width
        CWidth := UnitToMM(ATable.ColWidths[l], ATable.ColWidthsUnits, ABox);

      // setup bounds + spanning
      CBox.R := CBox.L + CWidth;
      if Cell.SpannedCols > 1 then
      begin
        if not CSame then
        begin
          for k := 1 to Cell.SpannedCols - 1 do
            Inc(CBox.R, UnitToMM(ATable.ColWidths[l + k], ATable.ColWidthsUnits, ABox));
        end
        else
          Inc(CBox.R, CWidth * (Cell.SpannedCols - 1));
      end;

      // top border
      FPage.SetColor(FPColorToTColorRef(Cell.Borders.Top.Color));
      CLineWidth := GetBorderWidth(Cell.Borders.Top.LineType);
      if CLineWidth > 0 then
        FPage.DrawLine(CBox.L, CBox.T, CBox.R, CBox.T, CLineWidth);

      // bottom border
      FPage.SetColor(FPColorToTColorRef(Cell.Borders.Bottom.Color));
      CLineWidth := GetBorderWidth(Cell.Borders.Bottom.LineType);
      if CLineWidth > 0 then
        FPage.DrawLine(CBox.L, CBox.B, CBox.R, CBox.B, CLineWidth);

      // left border
      FPage.SetColor(FPColorToTColorRef(Cell.Borders.Left.Color));
      CLineWidth := GetBorderWidth(Cell.Borders.Left.LineType);
      if CLineWidth > 0 then
        FPage.DrawLine(CBox.L, CBox.T, CBox.L, CBox.B, CLineWidth);

      // right border
      FPage.SetColor(FPColorToTColorRef(Cell.Borders.Right.Color));
      CLineWidth := GetBorderWidth(Cell.Borders.Right.LineType);
      if CLineWidth > 0 then
        FPage.DrawLine(CBox.R, CBox.T, CBox.R, CBox.B, CLineWidth);

      // handle entities
      for k := 0 to Cell.GetEntitiesCount - 1 do
      begin
        Entity := Cell.GetEntity(k);
        EBox := CBox;
        EBox.T := CBox.Y;
        EBox.L := CBox.X;
        EBox.R := EBox.R - Cell.SpacingRight;
        EBox.B := EBox.B - Cell.SpacingBottom;
        Info := GetEntityInfo(Entity, Cell, EBox);
        case Info.Kind of
          ekParagraph: AddParagraph(TvParagraph(Entity), EBox);
          ekList: AddList(TvList(Entity), EBox);
          ekTable: AddTable(TvTable(Entity), APage, AData, EBox);
        end;
        CBox.Y := Info.Box.B;
      end;
      // next cell
      CBox.L := CBox.R;
      l := l + Cell.SpannedCols;
    end;
    // next row
    RBox.T := RBox.B;
  end;
  // update coordinates
  ABox.T := ABox.B;
end;

procedure TvPDFVectorialWriter.AddText(AText: TvText; APara: TvParagraph; ABox: TvBoundBox);
var
  Text: String;
  FID: Integer;
begin
  // margins
  Inc(ABox.Y, AText.Style.MarginTop);
  Inc(ABox.X, AText.Style.MarginLeft);

  // move textbaseline down
  Inc(ABox.Y, GetFontHeight(AText));

  // write text
  FID := GetFontIDnew(AText.Style.Font.Name, AText.Style.Font.Bold, AText.Style.Font.Italic);
  FPage.SetFont(FID, Round(AText.Style.Font.Size));
  Text := TabsToSpaces(AText.Value.Text);  // replace tabs (cannot be printed)
  FPage.WriteText(ABox.X, ABox.Y, Text, 0, AText.Style.Font.Underline, AText.Style.Font.StrikeThrough);
end;

procedure TvPDFVectorialWriter.AddField(AField: TvField; ABox: TvBoundBox);
begin
  // TODO: add support for fields
  raise Exception.Create('Unsupported Entity: TvField');
end;

procedure TvPDFVectorialWriter.AddImage(AImage: TvRasterImage; ABox: TvBoundBox);
var
  Image: TPDFImageItem;
  H, W: Double;
begin
  Image := FDocument.Images.AddImageItem;
  Image.Image := AImage.RasterImage;
  H := GetImageHeight(AImage);
  W := GetImageWidth(AImage);
  Inc(ABox.Y, H);
  FPage.DrawImage(ABox.X, ABox.Y, W, H, Image.ID);
end;

constructor TvPDFVectorialWriter.Create;
begin
  inherited Create;

  FPointSeparator := DefaultFormatSettings;
  FPointSeparator.DecimalSeparator := '.';
  FPointSeparator.ThousandSeparator := '#';  // disable

  FDocument := TPDFDocument.Create(nil);
  FDocument.FontDirectory := FONT_DIR;

  FLineSpace := 1;  // default line space
  // only using half of the spacing to ensure compatibility
  FSpacing := FLineSpace * 50/100;  // 50%

  // build font cache
  gTTFontCache.SearchPath.Add(FONT_DIR);
  gTTFontCache.BuildFontCache;
end;

destructor TvPDFVectorialWriter.Destroy;
begin
  inherited;
  FDocument.Free;
end;

procedure TvPDFVectorialWriter.WriteToFile(AFileName: String; AData: TvVectorialDocument);
var
  OStream: TFileStream;
begin
  if ExtractFileExt(AFilename) = '' then
    AFilename := AFilename + STR_PDF_EXTENSION;
  OStream := TFileStream.Create(AFileName, fmCreate);
  try
    WriteToStream(OStream, AData);
  finally
    FreeAndNil(OStream);
  end;
end;

procedure TvPDFVectorialWriter.WriteToStream(AStream: TStream; AData: TvVectorialDocument);
var
  TextPage: TvTextPageSequence;
  Info: TvEntityInfo;
  Empty: TvBoundBox;
  Entity: TvEntity;
  i, j: Integer;
begin
  // default style
  FStyle := AData.AddStyle;
  FStyle.Font.Name := 'Times New Roman';
  FStyle.Font.Size := 11;
  FStyle.MarginLeft := 0;
  FStyle.MarginRight := 0;
  FStyle.MarginTop := FSpacing / 2;
  FStyle.MarginBottom := FSpacing / 2;

  FDocument.StartDocument;
  AddFonts(AData);
  FDocument.Options := FDocument.Options + [poPageOriginAtTop];
  FSection := FDocument.Sections.AddSection;
  AData.GuessDocumentSize();
  for i := 0 to AData.GetPageCount - 1 do  // iterate through pages
  begin
    TextPage := AData.GetPageAsText(i);
    {
    if TextPage = nil then
      raise Exception.Create('No text page found.');
      }
    Empty := AddPage(TextPage);
    for j := 0 to TextPage.GetEntitiesCount - 1 do  // iterate through entities
    begin
      Entity := TextPage.GetEntity(j);
      Info := GetEntityInfo(Entity, nil, Empty);
      case Info.Kind of
        ekParagraph: AddParagraph(TvParagraph(Entity), Info.Box);
        ekList: AddList(TvList(Entity), Info.Box);
        ekTable: AddTable(TvTable(Entity), TextPage, AData, Info.Box);
        else raise Exception.Create('Unsupported Entity');
      end;
      Empty.T := Info.Box.B;  // decrease free space
    end;
  end;
  FDocument.SaveToStream(AStream);
end;

initialization
  RegisterVectorialWriter(TvPDFVectorialWriter, vfPDF);

end.

