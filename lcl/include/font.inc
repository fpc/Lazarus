// included by graphics.pp
{******************************************************************************
                                     TFONT
 ******************************************************************************
 
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

const
  FontCharsets: array[0..18] of TIdentMapEntry = (
    (Value: ANSI_CHARSET;        Name: 'ANSI_CHARSET'),
    (Value: DEFAULT_CHARSET;     Name: 'DEFAULT_CHARSET'),
    (Value: SYMBOL_CHARSET;      Name: 'SYMBOL_CHARSET'),
    (Value: MAC_CHARSET;         Name: 'MAC_CHARSET'),
    (Value: SHIFTJIS_CHARSET;    Name: 'SHIFTJIS_CHARSET'),
    (Value: HANGEUL_CHARSET;     Name: 'HANGEUL_CHARSET'),
    (Value: JOHAB_CHARSET;       Name: 'JOHAB_CHARSET'),
    (Value: GB2312_CHARSET;      Name: 'GB2312_CHARSET'),
    (Value: CHINESEBIG5_CHARSET; Name: 'CHINESEBIG5_CHARSET'),
    (Value: GREEK_CHARSET;       Name: 'GREEK_CHARSET'),
    (Value: TURKISH_CHARSET;     Name: 'TURKISH_CHARSET'),
    (Value: VIETNAMESE_CHARSET;  Name: 'VIETNAMESE_CHARSET'),
    (Value: HEBREW_CHARSET;      Name: 'HEBREW_CHARSET'),
    (Value: ARABIC_CHARSET;      Name: 'ARABIC_CHARSET'),
    (Value: BALTIC_CHARSET;      Name: 'BALTIC_CHARSET'),
    (Value: RUSSIAN_CHARSET;     Name: 'RUSSIAN_CHARSET'),
    (Value: THAI_CHARSET;        Name: 'THAI_CHARSET'),
    (Value: EASTEUROPE_CHARSET;  Name: 'EASTEUROPE_CHARSET'),
    (Value: OEM_CHARSET;         Name: 'OEM_CHARSET'));

procedure GetCharsetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I:=Low(FontCharsets) to High(FontCharsets) do
    Proc(FontCharsets[I].Name);
end;

function CharsetToIdent(Charset: Longint; var Ident: string): Boolean;
begin
  Result:=IntToIdent(Charset, Ident, FontCharsets);
end;

function IdentToCharset(const Ident: string; var Charset: Longint): Boolean;
begin
  Result:=IdentToInt(Ident, CharSet, FontCharsets);
end;

function GetFontData(Font: HFont): TFontData;
var
  ALogFont: TLogFont;
begin
  Result := DefFontData;
  if Font <> 0 then
  begin
    if GetObject(Font, SizeOf(ALogFont), @ALogFont) <> 0 then
      with Result, ALogFont do
      begin
        Height := lfHeight;
        if lfWeight >= FW_BOLD then
          Include(Style, fsBold);
        if lfItalic = 1 then
          Include(Style, fsItalic);
        if lfUnderline = 1 then
          Include(Style, fsUnderline);
        if lfStrikeOut = 1 then
          Include(Style, fsStrikeOut);
        Charset := TFontCharset(lfCharSet);
        Name := lfFaceName;
        case lfPitchAndFamily and $F of
          VARIABLE_PITCH: Pitch := fpVariable;
          FIXED_PITCH: Pitch := fpFixed;
        else
          Pitch := fpDefault;
        end;
        Handle := Font;
      end;
  end;
end;

function GetDefFontCharSet: TFontCharSet;
//var
//  DisplayDC: HDC;
//  TxtMetric: TTEXTMETRIC;
begin
  Result := DEFAULT_CHARSET;
  {DisplayDC := GetDC(0);
  if (DisplayDC <> 0) then begin
    if (SelectObject(DisplayDC, StockFont) <> 0) then
      if (GetTextMetrics(DisplayDC, TxtMetric)) then
        Result := TxtMetric.tmCharSet;
    ReleaseDC(0, DisplayDC);
  end;}
end;

{------------------------------------------------------------------------------
  function:  FindXLFDItem
  Params:  const XLFDName: string; Index: integer;
           var ItemStart, ItemEnd: integer
  Returns: boolean

  Searches the XLFD item on position Index. Index starts from 0.
  Returns true on sucess.
  ItemStart will be on the first character and ItemEnd after the last character.
 ------------------------------------------------------------------------------}
function FindXLFDItem(const XLFDName: string; Index: integer;
  var ItemStart, ItemEnd: integer): boolean;
begin
  if Index<0 then begin
    Result:=false;
    exit;
  end;
  ItemStart:=1;
  ItemEnd:=ItemStart;
  while true do begin
    if (ItemEnd>length(XLFDName)) then begin
      dec(Index);
      break;
    end;
    if XLFDName[ItemEnd]='-' then begin
      dec(Index);
      if Index<0 then break;
      ItemStart:=ItemEnd+1;
    end;
    inc(ItemEnd);
  end;
  Result:=(Index=-1);
end;

{------------------------------------------------------------------------------
  function:  ExtractXLFDItem
  Params:  const XLFDName: string; Index: integer
  Returns: string

  Parses a font name in XLFD format and extracts one item.
  (see http://wwwinfo.cern.ch/umtf/working-groups/X11/fonts/hp_xlfd.html)

  An XLFD name is
  FontNameRegistry-Foundry-FamilyName-WeightName-Slant-SetwidthName
  -AddStyleName-PixelSize-PointSize-ResolutionX-ResolutionY-Spacing
  -AverageWidth-CharSetRegistry-CharSetCoding

 ------------------------------------------------------------------------------}
function ExtractXLFDItem(const XLFDName: string; Index: integer): string;
var StartPos, EndPos: integer;
begin
  if FindXLFDItem(XLFDName,Index,StartPos,EndPos) then
    Result:=copy(XLFDName,StartPos,EndPos-StartPos)
  else
    Result:='';
end;

{------------------------------------------------------------------------------
  function:  ExtractFamilyFromXLFDName
  Params:  const XLFDName: string
  Returns: string

  Parses a font name in XLFD format and extracts the FamilyName.
  (see http://wwwinfo.cern.ch/umtf/working-groups/X11/fonts/hp_xlfd.html)

  An XLFD name is
  FontNameRegistry-Foundry-FamilyName-WeightName-Slant-SetwidthName
  -AddStyleName-PixelSize-PointSize-ResolutionX-ResolutionY-Spacing
  -AverageWidth-CharSetRegistry-CharSetCoding

 ------------------------------------------------------------------------------}
function ExtractFamilyFromXLFDName(const XLFDName: string): string;
var StartPos, EndPos: integer;
begin
  if FindXLFDItem(XLFDName,2,StartPos,EndPos) then
    Result:=copy(XLFDName,StartPos,EndPos-StartPos)
  else
    Result:='';
end;

{------------------------------------------------------------------------------
  Method:  XLFDNameToLogFont
  Params:  const XLFDName: string
  Returns: TLogFont

  Parses a font name in XLFD format and creates a TLogFont record from it.
  (see http://wwwinfo.cern.ch/umtf/working-groups/X11/fonts/hp_xlfd.html)

  An XLFD name is
  FontNameRegistry-Foundry-FamilyName-WeightName-Slant-SetwidthName
  -AddStyleName-PixelSize-PointSize-ResolutionX-ResolutionY-Spacing
  -AverageWidth-CharSetRegistry-CharSetCoding

 ------------------------------------------------------------------------------}
function XLFDNameToLogFont(const XLFDName: string): TLogFont;
type
  TWeightMapEntry = record
    Name: string;
    Weight: integer;
  end;
const
  WeightMap: array[1..15] of TWeightMapEntry = (
    (Name: 'DONTCARE'; Weight: FW_DONTCARE),
    (Name: 'THIN'; Weight: FW_THIN),
    (Name: 'EXTRALIGHT'; Weight: FW_EXTRALIGHT),
    (Name: 'LIGHT'; Weight: FW_LIGHT),
    (Name: 'NORMAL'; Weight: FW_NORMAL),
    (Name: 'MEDIUM'; Weight: FW_MEDIUM),
    (Name: 'SEMIBOLD'; Weight: FW_SEMIBOLD),
    (Name: 'BOLD'; Weight: FW_BOLD),
    (Name: 'EXTRABOLD'; Weight: FW_EXTRABOLD),
    (Name: 'HEAVY'; Weight: FW_HEAVY),
    (Name: 'ULTRALIGHT'; Weight: FW_ULTRALIGHT),
    (Name: 'REGULAR'; Weight: FW_REGULAR),
    (Name: 'DEMIBOLD'; Weight: FW_DEMIBOLD),
    (Name: 'ULTRABOLD'; Weight: FW_ULTRABOLD),
    (Name: 'BLACK'; Weight: FW_BLACK)
    );
var
  ItemStart, ItemEnd: integer;
  Item: string;

  procedure GetNextItem;
  begin
    ItemStart:=ItemEnd+1;
    ItemEnd:=ItemStart;
    while (ItemEnd<=length(XLFDName)) and (XLFDName[ItemEnd]<>'-') do
      inc(ItemEnd);
    Item:=copy(XLFDName,ItemStart,ItemEnd-ItemStart);
  end;
  
  function WeightNameToWeightID(const WeightName: string): integer;
  var i: integer;
  begin
    for i:=Low(WeightMap) to High(WeightMap) do begin
      if AnsiCompareText(WeightMap[i].Name,WeightName)=0 then begin
        Result:=WeightMap[i].Weight;
        exit;
      end;
    end;
    Result:=FW_DONTCARE;
  end;

var l, FaceNameMax, PixelSize, PointSize, Resolution: integer;
begin
  FillChar(Result,SizeOf(TLogFont),0);
  ItemEnd:=0;
  GetNextItem; // read FontNameRegistry
  // ToDo
  
  GetNextItem; // read Foundry
  // ToDo

  GetNextItem; // read FamilyName
  l:=length(Item);
  FaceNameMax:=High(Result.lfFaceName)-Low(Result.lfFaceName); // max without #0
  if l>FaceNameMax then l:=FaceNameMax;
  if l>0 then Move(Item[1],Result.lfFaceName[Low(Result.lfFaceName)],l);
  Result.lfFaceName[Low(Result.lfFaceName)+l]:=#0;

  GetNextItem; // read WeightName
  Result.lfWeight:=WeightNameToWeightID(Item);

  GetNextItem; // read Slant
  if (AnsiCompareText(Item,'I')=0) or (AnsiCompareText(Item,'RI')=0)
  or (AnsiCompareText(Item,'O')=0) then
    // I = italic, RI = reverse italic, O = oblique
    Result.lfItalic:=1
  else
    Result.lfItalic:=0;

  GetNextItem; // read SetwidthName
  // ToDO: NORMAL, CONDENSED, NARROW, WIDE, EXPANDED

  GetNextItem; // read AddStyleName
  // calculate Style name extentions (=rotation)
  //        API                 XLFD
  // --------------------- --------------
  // Orientation 1/10 deg  1/64 deg
  Result.lfOrientation:=(StrToIntDef(Item,0)*10) div 64;
      
  GetNextItem; // read PixelSize
  PixelSize:=StrToIntDef(Item,0);
  GetNextItem; // read PointSize
  PointSize:=StrToIntDef(Item,0) div 10;
  GetNextItem; // read ResolutionX
  Resolution:=StrToIntDef(Item,0);
  if Resolution<=0 then Resolution:=72;

  if PixelSize=0 then begin
    if PointSize<=0 then
      Result.lfHeight:=(12*Resolution) div 72
    else
      Result.lfHeight:=(PointSize*Resolution) div 72;
  end else begin
    Result.lfHeight:=PixelSize;
  end;

  GetNextItem; // read ResolutionY
  Resolution:=StrToIntDef(Item,0);
  if Resolution<=0 then Resolution:=72;

  if PixelSize=0 then begin
    if PointSize<=0 then
      Result.lfWidth:=(12*Resolution) div 72
    else
      Result.lfWidth:=(PointSize*Resolution) div 72;
  end else begin
    Result.lfWidth:=PixelSize;
  end;

  GetNextItem; // read Spacing
  {M       Monospaced (fixed pitch)
   P       Proportional spaced (variable pitch)
   C       Character cell.  The glyphs of the font can be thought of as
           "boxes" of the same width and height that are stacked side by
           side or top to bottom.}
  if AnsiCompareText(Item,'M')=0 then
    Result.lfPitchAndFamily:=FIXED_PITCH
  else if AnsiCompareText(Item,'P')=0 then
    Result.lfPitchAndFamily:=VARIABLE_PITCH
  else if AnsiCompareText(Item,'C')=0 then
    Result.lfPitchAndFamily:=VARIABLE_PITCH;

  GetNextItem; // read AverageWidth
  // ToDo

  GetNextItem; // read CharSetRegistry
  // ToDo

  GetNextItem; // read CharSetCoding
  // ToDo

end;

{------------------------------------------------------------------------------
  function: ClearXLFDItem
  Params:   const LongFontName: string; Index: integer
  Returns:  string

  Replaces an item of a font name in XLFD format with a '*'.
 ------------------------------------------------------------------------------}
function ClearXLFDItem(const LongFontName: string; Index: integer): string;
var ItemStart, ItemEnd: integer;
begin
  if FindXLFDItem(LongFontName,Index,ItemStart,ItemEnd)
  and ((ItemEnd-ItemStart<>1) or (LongFontName[ItemStart]<>'*')) then
    Result:=LeftStr(LongFontName,ItemStart-1)+'*'
            +RightStr(LongFontName,length(LongFontName)-ItemEnd+1)
  else
    Result:=LongFontName;
end;

{------------------------------------------------------------------------------
  function: ClearXLFDHeight
  Params:   const LongFontName: string
  Returns:  string

  Replaces the PixelSize, PointSize, ResolutionX, ResolutionY and AverageWidth
  of a font name in XLFD format with '*'.
  
  An XLFD name is
  FontNameRegistry-Foundry-FamilyName-WeightName-Slant-SetwidthName
  -AddStyleName-PixelSize-PointSize-ResolutionX-ResolutionY-Spacing
  -AverageWidth-CharSetRegistry-CharSetCoding
 ------------------------------------------------------------------------------}
function ClearXLFDHeight(const LongFontName: string): string;
begin
  Result:=ClearXLFDItem(LongFontName,7); // PixelSize
  Result:=ClearXLFDItem(Result,8);       // PointSize
  Result:=ClearXLFDItem(Result,9);       // ResolutionX
  Result:=ClearXLFDItem(Result,10);      // ResolutionY
  Result:=ClearXLFDItem(Result,12);      // AverageWidth
end;

{------------------------------------------------------------------------------
  function: ClearXLFDPitch
  Params:   const LongFontName: string
  Returns:  string

  Replaces the spacing a font name in XLFD format with a '*'.
 ------------------------------------------------------------------------------}
function ClearXLFDPitch(const LongFontName: string): string;
begin
  Result:=ClearXLFDItem(LongFontName,11);
end;

{------------------------------------------------------------------------------
  function: ClearXLFDStyle
  Params:   const LongFontName: string
  Returns:  string

  Replaces the WeightName, Slant and SetwidthName of a font name in XLFD format
  with '*'.
 ------------------------------------------------------------------------------}
function ClearXLFDStyle(const LongFontName: string): string;
begin
  Result:=ClearXLFDItem(ClearXLFDItem(ClearXLFDItem(LongFontName,3),4),5);
end;

{------------------------------------------------------------------------------
  function: IsFontNameXLogicalFontDesc
  Params:   const LongFontName: string
  Returns:  boolean

  Checks if font name is in X Logical Font Description format.
  (see http://wwwinfo.cern.ch/umtf/working-groups/X11/fonts/hp_xlfd.html)

  An XLFD name is
  FontNameRegistry-Foundry-FamilyName-WeightName-Slant-SetwidthName
  -AddStyleName-PixelSize-PointSize-ResolutionX-ResolutionY-Spacing
  -AverageWidth-CharSetRegistry-CharSetCoding
 ------------------------------------------------------------------------------}
function IsFontNameXLogicalFontDesc(const LongFontName: string): boolean;
// Quick test: check if LongFontName contains 14 times the char '-'
var MinusCnt, p: integer;
begin
  MinusCnt:=0;
  for p:=1 to length(LongFontName) do
    if LongFontName[p]='-' then inc(MinusCnt);
  Result:=(MinusCnt=14);
end;


{ TFont }


{------------------------------------------------------------------------------
  Method:  TFont.Create
  Params:  none
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TFont.Create;
begin
  inherited Create;
  FFontData:=DefFontData;

  FColor := clWindowText;
  FPixelsPerInch := ScreenInfo.PixelsPerInchX;
end;

{------------------------------------------------------------------------------
  Method: TFont.Assign
  Params: Source: Another font
  Returns:  nothing

  Copies the source font to itself
 ------------------------------------------------------------------------------}
Procedure TFont.Assign(Source : TPersistent);
begin
  if Source is TFont then begin
    //TODO:lock;
    try
      //TODO: TFont(Source).Lock;
      try
        BeginUpdate;
        try
          CharSet:= TFont(Source).CharSet;
          Color := TFont(Source).Color;
          Height := TFont(Source).Height;
          Name := TFont(Source).Name;
          Pitch := TFont(Source).Pitch;
          Style := TFont(Source).Style;
        finally
          EndUpdate;
        end;
      finally
        //TODO: TFont(Source).UnLock;
      end;
    finally
      //TODO: UnLock;
    end;
    Exit;
  end;

  inherited Assign(Source);
end;

{------------------------------------------------------------------------------
  Method: TFont.Assign
  Params: ALogFont: TLogFont
  Returns:  nothing

  Copies the logfont settings to itself
 ------------------------------------------------------------------------------}
procedure TFont.Assign(const ALogFont: TLogFont);
var
  AStyle: TFontStyles;
begin
  BeginUpdate;
  try
    with ALogFont do
    begin
      Height := ALogFont.lfHeight;
      Charset := TFontCharset(ALogFont.lfCharSet);
      AStyle := [];
      with ALogFont do
      begin
        if lfWeight >= FW_SEMIBOLD then Include(AStyle, fsBold);
        if lfItalic <> 0 then Include(AStyle, fsItalic);
        if lfUnderline <> 0 then Include(AStyle, fsUnderline);
        if lfStrikeOut <> 0 then Include(AStyle, fsStrikeOut);
      end;
      Style := AStyle;
      Name := ALogFont.lfFaceName;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TFont.BeginUpdate;
begin
  inc(FUpdateCount);
end;

procedure TFont.EndUpdate;
begin
  if FUpdateCount=0 then exit;
  dec(FUpdateCount);
  if (FUpdateCount=0) and FChanged then Changed;
end;

{------------------------------------------------------------------------------
  Method: TFont.HandleAllocated
  Params: none
  Returns: boolean

  Resturns True on handle allocated.
 ------------------------------------------------------------------------------}
function TFont.HandleAllocated: boolean;
begin
  Result:=FFontData.Handle<>0;
end;

{------------------------------------------------------------------------------
  Method: TFont.SetSize
  Params: Value: the new value
  Returns:  nothing

  Converts the size in points to pixels and sets the height
 ------------------------------------------------------------------------------}
procedure TFont.SetSize(value : Integer);
begin
  Height := -(Value * FPixelsPerInch) div 72;
end;

{------------------------------------------------------------------------------
  Function: TFont.GetSize
  Params: none
  Returns:  The font size

  Calculates the size based on height
 ------------------------------------------------------------------------------}
function TFont.GetSize : Integer;
begin
  Result := -(Height * 72) div FPixelsPerInch;
end;

{------------------------------------------------------------------------------
  Method: TFont.SetPitch
  Params: Value: the new value
  Returns:  nothing

  Sets the pitch of a font
 ------------------------------------------------------------------------------}
Procedure TFont.SetPitch(Value : TFontPitch);
Begin
  if FFontData.Pitch <> Value
  then begin
    BeginUpdate;
    FreeHandle;
    FFontData.Pitch := Value;
    if IsFontNameXLogicalFontDesc(FFontName) then
      Name:=ClearXLFDPitch(Name);
    Changed;
    EndUpdate;
  end;
end;

{------------------------------------------------------------------------------
  Method: TFont.SetHeight
  Params: Value: the new value
  Returns:  nothing

  Sets the height of a font
 ------------------------------------------------------------------------------}
procedure TFont.SetHeight(Value: Integer);
begin
  if FFontData.Height <> Value
  then begin
    BeginUpdate;
    FreeHandle;
    FFontData.Height := Value;
    if IsFontNameXLogicalFontDesc(FFontName) then
      Name:=ClearXLFDHeight(Name);
    Changed;
    EndUpdate;
  end;
end;

{------------------------------------------------------------------------------
  Method: TFont.SetStyle
  Params: Value: the new value
  Returns:  nothing

  Sets the style of a font
 ------------------------------------------------------------------------------}
procedure TFont.SetStyle(value : TFontStyles);
begin
  if FFontData.Style <> Value 
  then begin
    BeginUpdate;
    FreeHandle;
    FFontData.Style := Value;
    if IsFontNameXLogicalFontDesc(Name) then
      Name:=ClearXLFDStyle(Name);
    Changed;
    EndUpdate;
  end;
end;

{------------------------------------------------------------------------------
  Method: TFont.SetColor
  Params: Value: the new value
  Returns:  nothing

  Sets the pencolor of a font
 ------------------------------------------------------------------------------}
procedure TFont.SetColor(Value : TColor);
begin
  if FColor <> Value 
  then begin
    FColor := Value;
    Changed;
  end;
end;

{------------------------------------------------------------------------------
  Function: TFont.GetName
  Params: none
  Returns:  The font name

  Returns the name of the font
 ------------------------------------------------------------------------------}
function TFont.GetName: TFontName;
begin
  if FFontName<>'' then
    Result:=FFontName
  else
    Result := FFontdata.Name;
end;

{------------------------------------------------------------------------------
  Method: TFont.SetName
  Params: Value: the new value
  Returns:  nothing

  Sets the name of a font
 ------------------------------------------------------------------------------}
procedure TFont.SetName(const AValue : TFontName);
begin
  if FFontName <> AValue
  then begin
    FreeHandle;
    FFontData.Name := LeftStr(AValue,SizeOf(FFontData.Name));
    FFontName:=AValue;
    Changed;
  end;
end;

{------------------------------------------------------------------------------
  Method: TFont.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TFont.Destroy;
begin
  FreeHandle;
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method: TFont.SetHandle
  Params:   a font handle
  Returns:  nothing

  sets the font to an external created font 
 ------------------------------------------------------------------------------}
procedure TFont.SetHandle(const Value: HFONT);
begin
  SetData(GetFontData(Value));
end;

{------------------------------------------------------------------------------
  Function: TFont.GetHandle
  Params:   none
  Returns:  a handle to a font gdiobject

  Creates a font if needed
 ------------------------------------------------------------------------------}
function TFont.GetHandle: HFONT;
const
  LF_BOOL: array[Boolean] of Byte = (0, 255);
  LF_WEIGHT: array[Boolean] of Integer = (FW_NORMAL, FW_BOLD);
var
  ALogFont: TLogFont;

  procedure SetLogFontName(const NewName: string);
  var l: integer;
    aName: string;
  begin
    if IsFontNameXLogicalFontDesc(NewName) then
      aName:=ExtractFamilyFromXLFDName(NewName)
    else
      aName:=NewName;
    l:=High(ALogFont.lfFaceName)-Low(ALogFont.lfFaceName);
    if l>length(aName) then l:=length(aName);
    if l>0 then
      Move(aName[1],ALogFont.lfFaceName[Low(ALogFont.lfFaceName)],l);
    ALogFont.lfFaceName[Low(ALogFont.lfFaceName)+l]:=#0;
  end;

begin
  if FFontData.Handle = 0 then with ALogFont do
  begin
    lfHeight := Height;
    lfWidth := 0; 
    lfEscapement := 0; 
    lfOrientation := 0; 
    lfWeight := LF_WEIGHT[fsBold in Style];
    lfItalic := LF_BOOL[fsItalic in Style];
    lfUnderline := LF_BOOL[fsUnderline in Style];
    lfStrikeOut := LF_BOOL[fsStrikeOut in Style];
    lfCharSet := Byte(FFontData.Charset);
    SetLogFontName(Name);

    lfQuality := DEFAULT_QUALITY;
    lfOutPrecision := OUT_DEFAULT_PRECIS;
    lfClipPrecision := CLIP_DEFAULT_PRECIS;
    case Pitch of
      fpVariable: lfPitchAndFamily := VARIABLE_PITCH;
      fpFixed: lfPitchAndFamily := FIXED_PITCH;
    else
      lfPitchAndFamily := DEFAULT_PITCH;
    end;

    // ask the interface for the nearest font
    FFontData.Handle := CreateFontIndirectEx(ALogFont,Name);
  end;
  
  Result := FFontData.Handle;
end;

{------------------------------------------------------------------------------
  Method:  TFont.FreeHandle
  Params:  none
  Returns: Nothing

  Frees a fonthandle if needed
 ------------------------------------------------------------------------------}
procedure TFont.FreeHandle;
begin
  if FFontData.Handle <> 0 
  then begin
    //TODO: what if a font is currently selected
    DeleteObject(FFontData.Handle);
    FFontData.Handle := 0;
  end;
end;

function TFont.GetCharSet: TFontCharSet;
begin
  Result:=FFontData.CharSet;
end;

procedure TFont.SetCharSet(const AValue: TFontCharSet);
begin
  if FFontData.CharSet <> AValue
  then begin
    FreeHandle;
    FFontData.CharSet := AValue;
    Changed;
  end;
end;

procedure TFont.GetData(var FontData: TFontData);
begin
  FontData := FFontData;
  FontData.Handle := 0;
end;

function TFont.IsNameStored: boolean;
begin
  Result:=DefFontData.Name<>Name;
end;

procedure TFont.SetData(const FontData: TFontData);
begin
  if FFontData.Handle <> FontData.Handle then begin
    FreeHandle;
    FFontData.Handle := FontData.Handle;
    FFontName:=FontData.Name;
    //TODO: query new parameters
    Changed;
  end;

  {Lock;
  try
    FontManager.ChangeResource(Self, FontData);
  finally
    Unlock;
  end;}
end;

function TFont.GetHeight: Integer;
begin
  Result:=FFontData.Height;
end;

function TFont.GetPitch: TFontPitch;
begin
  Result:=FFontData.Pitch;
end;

function TFont.GetStyle: TFontStyles;
begin
  Result:=FFontData.Style;
end;

procedure TFont.Changed;
begin
  if FUpdateCount>0 then begin
    FChanged:=true;
    exit;
  end;
  FChanged:=false;
  inherited Changed;
  // ToDo: we need interfaces:
  // if FNotify <> nil then FNotify.Changed;
end;

// included by graphics.pp

{ =============================================================================

  $Log$
  Revision 1.10  2002/12/07 08:42:08  mattias
  improved ExtTxtOut: support for char dist array

  Revision 1.9  2002/10/01 18:00:04  lazarus
  AJ: Initial TUpDown, minor property additions to improve reading Delphi created forms.

  Revision 1.8  2002/09/05 12:11:43  lazarus
  MG: TNotebook is now streamable

  Revision 1.7  2002/06/05 12:33:57  lazarus
  MG: fixed fonts in XLFD format and styles

  Revision 1.6  2002/06/04 15:17:22  lazarus
  MG: improved TFont for XLFD font names

  Revision 1.5  2002/05/10 06:05:52  lazarus
  MG: changed license to LGPL

  Revision 1.4  2001/09/30 08:34:49  lazarus
  MG: fixed mem leaks and fixed range check errors

  Revision 1.3  2001/03/19 14:40:49  lazarus
  MG: fixed many unreleased DC and GDIObj bugs

  Revision 1.1  2000/07/13 10:28:25  michael
  + Initial import

  Revision 1.1  2000/04/02 20:49:56  lazarus
  MWE:
    Moved lazarus/lcl/*.inc files to lazarus/lcl/include

  Revision 1.15  2000/01/17 20:36:25  lazarus
  Fixed Makefile again.
  Made implementation of TScreen and screen info saner.
  Began to implemented DeleteObject in GTKWinAPI.
  Fixed a bug in GDI allocation which in turn fixed A LOT of other bugs :-)

  Revision 1.14  1999/12/03 00:26:47  lazarus
  MWE:
    fixed control location
    added gdiobject reference counter

  Revision 1.13  1999/12/02 19:00:59  lazarus
  MWE:
    Added (GDI)Pen
    Changed (GDI)Brush
    Changed (GDI)Font (color)
    Changed Canvas to use/create pen/brush/font
    Hacked mwedit to allow setting the number of chars (till it get a WM/LM_SIZE event)
    The editor shows a line !

  Revision 1.12  1999/11/25 23:45:08  lazarus
  MWE:
    Added font as GDIobject
    Added some API testcode to testform
    Commented out some more IFDEFs in mwCustomEdit

  Revision 1.11  1999/11/17 01:16:39  lazarus
  MWE:
    Added some more API stuff
    Added an initial TBitmapCanvas
    Added some DC stuff
    Changed and commented out, original gtk linedraw/rectangle code. This
      is now called through the winapi wrapper.

  Revision 1.10  1999/11/09 17:16:44  lazarus
  Added PITCH to TFONT>
  Doesn't do anything yet.
  Shane

  Revision 1.9  1999/11/05 17:48:17  lazarus
  Added a mwedit1 component to lazarus (MAIN.PP)
  It crashes on create.
  Shane

  Revision 1.8  1999/08/26 23:36:02  peter
    + paintbox
    + generic keydefinitions and gtk conversion
    * gtk state -> shiftstate conversion

  Revision 1.7  1999/08/16 20:48:02  lazarus
  Added a changed event for TFOnt and code to get the average size of the font.  Doesn't seem to work very well yet.
  The "average size" code is found in gtkobject.inc.

  Revision 1.6  1999/08/16 18:45:39  lazarus
  Added a TFont Dialog plus minor additions.

  Shane Aug 16th 1999  14:07 CST

  Revision 1.5  1999/08/07 17:59:19  lazarus

        buttons.pp   the DoLeave and DoEnter were connected to the wrong
                     event.

        The rest were modified to use the new SendMessage function.   MAH

  Revision 1.4  1999/08/01 21:46:25  lazarus
  Modified the GETWIDTH and GETHEIGHT of TFOnt so you can use it to calculate the length in Pixels of a string.  This is now used in the editor.

  Shane

  Revision 1.3  1999/07/31 06:39:25  lazarus

       Modified the IntSendMessage3 to include a data variable. It isn't used
       yet but will help in merging the Message2 and Message3 features.

       Adjusted TColor routines to match Delphi color format

       Added a TGdkColorToTColor routine in gtkproc.inc

       Finished the TColorDialog added to comDialog example.        MAH

 }
