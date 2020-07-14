{
 *****************************************************************************
  This file is part of LazUtils.

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit LazFreeTypeFontCollection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Laz_AVL_Tree,
  // LazUtils
  EasyLazFreeType, LazFreeType, TTTypes;

type
  { TFontCollectionItem }

  TFontCollectionItem = class(TCustomFontCollectionItem)
  private
    FFilename: string;
    FSourceStream: TStream;
    FSourceStreamOwned: boolean;
    FInformation: array[TFreeTypeInformation] of string;
    FVersionNumber: string;
    FStyleList: array of string;
    FFace: TT_Face;
    FFaceUsage: integer;
    FUsePostscriptStyle: boolean;
    FDestroyListeners: array of TFontCollectionItemDestroyListener;
    procedure UpdateStyles;
    procedure SetInformation(AIndex: TFreeTypeInformation; AValue: string);
    procedure SetUsePostscriptStyle(AValue: boolean);
  protected
    function GetFilename: string; override;
    function GetBold: boolean; override;
    function GetInformation(AIndex: TFreeTypeInformation): string; override;
    function GetItalic: boolean; override;
    function GetStyleCount: integer; override;
    function GetStyles: string; override;
    function GetStyle(AIndex: integer): string; override;
    function GetVersionNumber: string; override;
    procedure NotifyDestroy; override;
    procedure Init;
  public
    constructor Create(AFilename: string);
    constructor Create(AStream: TStream; AOwner: Boolean);
    destructor Destroy; override;
    function HasStyle(AStyle: string): boolean; override;
    property Information[AIndex: TFreeTypeInformation]: string read GetInformation write SetInformation;
    property VersionNumber: string read GetVersionNumber write FVersionNumber;
    function CreateFont: TFreeTypeFont; override;
    function QueryFace(AListener: TFontCollectionItemDestroyListener): TT_Face; override;
    procedure ReleaseFace(AListener: TFontCollectionItemDestroyListener); override;
    property UsePostscriptStyle: boolean read FUsePostscriptStyle write SetUsePostscriptStyle;
  end;

  { TFamilyCollectionItem }

  TFamilyCollectionItem = class(TCustomFamilyCollectionItem)
  private
    FFamilyName: string;
    FFonts: array of TFontCollectionItem;
    FFontCount: integer;
    FStyles: array of string;
    FStyleCount: integer;
    FUsePostscriptStyle: boolean;
  protected
    function GetFontByIndex(AIndex: integer): TCustomFontCollectionItem; override;
    function GetFontByStyles(AStyles: string): TCustomFontCollectionItem;
    function GetFontIndexByStyles(AStyles: string): integer;
    function GetStyle(AIndex: integer): string; override;
    procedure AddStyle(AName: string);
    function RemoveStyle(AName: string): boolean;
    function GetStyles: string; override;
    function GetFamilyName: string; override;
    function GetFontCount: integer; override;
    function GetStyleCount: integer; override;
    procedure RebuildStyleList(out ADuplicates: boolean);
  public
    constructor Create(AName: string);
    procedure AddFont(AFontItem: TFontCollectionItem);
    function RemoveFont(AFontItem: TCustomFontCollectionItem): boolean;
    function GetFont(const AStyles: array of string; NeedAllStyles: boolean = false; NoMoreStyle: boolean = false): TCustomFontCollectionItem; override;
    function GetFont(AStyle: string; NeedAllStyles: boolean = false; NoMoreStyle: boolean = false): TCustomFontCollectionItem; override;
    function GetFontIndex(const AStyles: array of string; NeedAllStyles: boolean = false; NoMoreStyle: boolean = false): integer; override;
    function GetFontIndex(AStyle: string; NeedAllStyles: boolean = false; NoMoreStyle: boolean = false): integer; override;
    function HasStyle(AName: string): boolean; override;
  end;

  { TFreeTypeFontCollection }

  TFreeTypeFontCollection = class(TCustomFreeTypeFontCollection)
  private
    FFontList: TAvlTree;
    FTempFont: TFreeTypeFont;
    FUpdateCount: integer;

    FFamilyList: TAvlTree;

    function AddFamily(AName: string): TFamilyCollectionItem;
    function FindFamily(AName: string): TFamilyCollectionItem;
    function FindFont(AFileName: string): TFontCollectionItem;

    function CompareFontFileName({%H-}Tree: TAvlTree; Data1, Data2: Pointer): integer;
    function CompareFamilyName({%H-}Tree: TAvlTree; Data1, Data2: Pointer): integer;

  protected
    function GetFont(AFileName: string): TCustomFontCollectionItem; override;
    function GetFamily(AName: string): TCustomFamilyCollectionItem; override;
    function GetFamilyCount: integer; override;
    function GetFontCount: integer; override;
    function RemoveAndFreeFamily(AFamily: TCustomFamilyCollectionItem): boolean;
  public
    constructor Create; override;
    procedure Clear; override;
    procedure BeginUpdate; override;
    procedure AddFolder(AFolder: string; AIncludeSubdirs: Boolean = false); override;
    procedure RemoveFolder(AFolder: string); override;
    function AddFile(AFilename: string): TCustomFontCollectionItem; override;
    function RemoveFile(AFilename: string): boolean; override;
    function AddStream(AStream: TStream; AOwned: boolean): boolean; override;
    procedure EndUpdate; override;
    destructor Destroy; override;
    function FontFileEnumerator: IFreeTypeFontEnumerator; override;
    function FamilyEnumerator: IFreeTypeFamilyEnumerator; override;
  end;

procedure SetDefaultFreeTypeFontCollection(ACollection : TCustomFreeTypeFontCollection);

implementation

uses
  FileUtil;

const
  //one of these files will be used as a default font
  ArialLikeFonts: array[0..8] of string = ('Helvetica', 'Helvetica Neue',
  'Arial', 'Nimbus Sans L', 'Microsoft Sans Serif', 'FreeSans',
  'Liberation Sans', 'DejaVu Sans Condensed', 'Tahoma');

type
  { TFamilyEnumerator }

   TFamilyEnumerator = class(TInterfacedObject,IFreeTypeFamilyEnumerator)
   private
     FNodeEnumerator: TAvlTreeNodeEnumerator;
   public
     constructor Create(ANodeEnumerator: TAvlTreeNodeEnumerator);
     destructor Destroy; override;
     function MoveNext: boolean;
     function GetCurrent: TCustomFamilyCollectionItem;
   end;

  { TFontEnumerator }

   TFontEnumerator = class(TInterfacedObject,IFreeTypeFontEnumerator)
   private
     FNodeEnumerator: TAvlTreeNodeEnumerator;
   public
     constructor Create(ANodeEnumerator: TAvlTreeNodeEnumerator);
     destructor Destroy; override;
     function MoveNext: boolean;
     function GetCurrent: TCustomFontCollectionItem;
   end;

procedure SetDefaultFreeTypeFontCollection(
  ACollection: TCustomFreeTypeFontCollection);
begin
  EasyLazFreeType.FontCollection := ACollection;
end;

{ TFontCollectionItem }

function TFontCollectionItem.GetStyles: string;
var i: integer;
begin
  if StyleCount = 0 then
    result := 'Regular'
  else
  begin
    result := '';
    for i := 0 to StyleCount-1 do
    begin
      if i > 0 then result += ' ';
      result += Style[i];
    end;
  end;
end;

function TFontCollectionItem.GetInformation(AIndex: TFreeTypeInformation): string;
begin
  if (AIndex < low(TFreeTypeInformation)) or (AIndex > high(TFreeTypeInformation)) then
    result := ''
  else
    result := FInformation[AIndex];
end;

function TFontCollectionItem.GetBold: boolean;
begin
  result := HasStyle('Bold');
end;

function TFontCollectionItem.GetItalic: boolean;
begin
  result := HasStyle('Italic') or HasStyle('Oblique');
end;

function TFontCollectionItem.GetStyleCount: integer;
begin
  result := length(FStyleList);
end;

procedure TFontCollectionItem.SetInformation(AIndex: TFreeTypeInformation;
  AValue: string);
begin
  if (AIndex >= low(TFreeTypeInformation)) and (AIndex <= high(TFreeTypeInformation)) then
  begin
    FInformation[AIndex] := AValue;
    if ((AIndex = ftiStyle) and not FUsePostscriptStyle) or
       ((AIndex = ftiPostscriptName) and FUsePostscriptStyle) then UpdateStyles;
  end;
end;

procedure TFontCollectionItem.SetUsePostscriptStyle(AValue: boolean);
begin
  if AValue <> FUsePostscriptStyle then
  begin
    FUsePostscriptStyle:= AValue;
    UpdateStyles;
  end;
end;

function TFontCollectionItem.GetFilename: string;
begin
  result := FFilename;
end;

function TFontCollectionItem.GetStyle(AIndex: integer): string;
begin
  if (AIndex < 0) or (AIndex > high(FStyleList)) then
    result := ''
  else
    result := FStyleList[AIndex];
end;

function TFontCollectionItem.GetVersionNumber: string;
begin
  result := FVersionNumber;
end;

procedure TFontCollectionItem.UpdateStyles;
var
  StyleStr: string;
  idx,i: integer;
begin
  if not FUsePostscriptStyle then
    StyleStr := Information[ftiStyle]
  else
  begin
    StyleStr := Information[ftiPostscriptName];
    idx := pos('-',StyleStr);
    if idx = 0 then StyleStr := 'Regular' else
    begin
      StyleStr := copy(StyleStr,idx+1,length(StyleStr)-idx);
      for i := length(StyleStr) downto 2 do
        if (StyleStr[i] = UpCase(StyleStr[i])) and
          (StyleStr[i-1] <> UpCase(StyleStr[i-1])) then
            Insert(' ',StyleStr,i);
      if (length(StyleStr) > 2) and (copy(StyleStr, length(StyleStr)-2,3)=' MT') then
        delete(StyleStr, length(StyleStr)-2,3);
    end;
  end;
  FStyleList := StylesToArray(StyleStr);
end;

constructor TFontCollectionItem.Create(AFilename: string);
begin
  Init;
  FFilename:= AFilename;
end;

constructor TFontCollectionItem.Create(AStream: TStream; AOwner: Boolean);
begin
  Init;
  FSourceStream := AStream;
  FSourceStreamOwned:= AOwner;
end;

procedure TFontCollectionItem.NotifyDestroy;
var
  listener: TFontCollectionItemDestroyListener;
begin
  //the list of listeners may change during the process
  //so it is safer to avoid the 'for' loop
  while length(FDestroyListeners) > 0 do
  begin
    listener := FDestroyListeners[high(FDestroyListeners)];
    setlength(FDestroyListeners, length(FDestroyListeners)-1);
    listener.NotifyProc();
  end;
  FDestroyListeners := nil;
end;

procedure TFontCollectionItem.Init;
begin
  FStyleList := nil;
  FFaceUsage := 0;
  FUsePostscriptStyle:= false;
  FDestroyListeners := nil;
  FFilename := '';
end;

destructor TFontCollectionItem.Destroy;
begin
  NotifyDestroy;
  if FSourceStreamOwned then FSourceStream.Free;
  if FFaceUsage <> 0 then
  begin
    TT_Close_Face(FFace);
    FFaceUsage := 0;
  end;
  inherited Destroy;
end;

function TFontCollectionItem.HasStyle(AStyle: string): boolean;
var i: integer;
begin
  if CompareText(AStyle,'Regular')=0 then
  begin
    result := length(FStyleList)=0;
    exit;
  end;
  for i := 0 to high(FStyleList) do
    if CompareText(FStyleList[i],AStyle)=0 then
    begin
      result := true;
      exit;
    end;
  result := false;
end;

function TFontCollectionItem.CreateFont: TFreeTypeFont;
begin
  result := TFreeTypeFont.Create;
  if FSourceStream <> nil then
    result.AccessFromStream(FSourceStream,False)
  else
    result.Name := Filename;
end;

function TFontCollectionItem.QueryFace(AListener: TFontCollectionItemDestroyListener): TT_Face;
var errorNum: TT_Error;
begin
  if FFaceUsage = 0 then
  begin
    if FSourceStream <> nil then
      errorNum := TT_Open_Face(FSourceStream,false,FFace)
    else
      errorNum := TT_Open_Face(Filename,FFace);
    if errorNum <> TT_Err_Ok then
      raise exception.Create('Cannot open font (TT_Error ' + intToStr(errorNum)+')');
  end;
  result := FFace;
  inc(FFaceUsage);
  if Assigned(AListener.NotifyProc) then
  begin
    setlength(FDestroyListeners,length(FDestroyListeners)+1);
    FDestroyListeners[high(FDestroyListeners)] := AListener;
  end;
end;

procedure TFontCollectionItem.ReleaseFace(AListener: TFontCollectionItemDestroyListener);
var i,j: integer;
begin
  for i := 0 to high(FDestroyListeners) do
    if (FDestroyListeners[i].TargetObject = AListener.TargetObject) and
       (FDestroyListeners[i].NotifyProc = AListener.NotifyProc) then
    begin
      for j := i to high(FDestroyListeners)-1 do
        FDestroyListeners[j] := FDestroyListeners[j+1];
      setlength(FDestroyListeners, length(FDestroyListeners)-1);
      break;
    end;
  if FFaceUsage > 0 then
  begin
    dec(FFaceUsage);
    if FFaceUsage = 0 then TT_Close_Face(FFace);
  end;
end;

constructor TFontEnumerator.Create(ANodeEnumerator: TAvlTreeNodeEnumerator);
begin
  FNodeEnumerator := ANodeEnumerator;
end;

destructor TFontEnumerator.Destroy;
begin
  FNodeEnumerator.Free;
end;

function TFontEnumerator.MoveNext: boolean;
begin
  result := FNodeEnumerator.MoveNext;
end;

function TFontEnumerator.GetCurrent: TCustomFontCollectionItem;
begin
  result := TCustomFontCollectionItem(FNodeEnumerator.Current.Data);
end;

{ TFamilyEnumerator }

function TFamilyEnumerator.GetCurrent: TCustomFamilyCollectionItem;
begin
  result := TCustomFamilyCollectionItem(FNodeEnumerator.Current.Data);
end;

constructor TFamilyEnumerator.Create(ANodeEnumerator: TAvlTreeNodeEnumerator);
begin
  FNodeEnumerator := ANodeEnumerator;
end;

destructor TFamilyEnumerator.Destroy;
begin
  FNodeEnumerator.Free;
end;

function TFamilyEnumerator.MoveNext: boolean;
begin
  result := FNodeEnumerator.MoveNext;
end;

{ TFamilyCollectionItem }

function TFamilyCollectionItem.GetFontByIndex(AIndex: integer): TCustomFontCollectionItem;
begin
  if AIndex = -1 then
    result := GetFont('Regular')
  else
  if (AIndex < 0) or (AIndex >= FFontCount) then
    result := nil
  else
    result := FFonts[AIndex];
end;

function TFamilyCollectionItem.GetFontByStyles(AStyles: string): TCustomFontCollectionItem;
var i: integer;
begin
  for i := 0 to FFontCount-1 do
    if CompareText(FFonts[i].Styles,AStyles)= 0 then
    begin
      result := FFonts[i];
      exit;
    end;
  result := nil;
end;

function TFamilyCollectionItem.GetFontIndexByStyles(AStyles: string): integer;
var i: integer;
begin
  for i := 0 to FFontCount-1 do
    if CompareText(FFonts[i].Styles,AStyles)= 0 then
    begin
      result := i;
      exit;
    end;
  result := -1;
end;

function TFamilyCollectionItem.GetStyle(AIndex: integer): string;
begin
  if (AIndex < 0) or (AIndex >= FStyleCount) then
    result := ''
  else
    result := FStyles[AIndex];
end;

procedure TFamilyCollectionItem.AddStyle(AName: string);
begin
  if HasStyle(AName) then exit;
  if FStyleCount = length(FStyles) then
    setlength(FStyles, length(FStyles)+4);
  FStyles[FStyleCount] := AName;
  inc(FStyleCount);
end;

function TFamilyCollectionItem.RemoveStyle(AName: string): boolean;
var i,j: integer;
begin
  for i := 0 to FStyleCount-1 do
    if CompareText(FStyles[i],AName)=0 then
    begin
      for j := i to FStyleCount-2 do
        FStyles[j] := FStyles[j+1];
      dec(FStyleCount);
      result := true;
      exit;
    end;
  result := false;
end;

function TFamilyCollectionItem.GetStyles: string;
var i: integer;
begin
  result := '';
  for i := 0 to StyleCount-1 do
  begin
    if i <> 0 then result += ' ';
    result += Style[i];
  end;
end;

function TFamilyCollectionItem.GetFamilyName: string;
begin
  result := FFamilyName;
end;

function TFamilyCollectionItem.GetFontCount: integer;
begin
  result := FFontCount;
end;

function TFamilyCollectionItem.GetStyleCount: integer;
begin
  result := FStyleCount;
end;

procedure TFamilyCollectionItem.RebuildStyleList(out ADuplicates: boolean);
var
  i: Integer;
  j: Integer;
begin
  FStyleCount := 0;
  ADuplicates := false;
  for i := 0 to FFontCount-1 do
  begin
    FFonts[i].UsePostscriptStyle := true;
    for j := 0 to FFonts[i].StyleCount -1 do
     AddStyle(FFonts[i].Style[j]);

    //add regular style if no other style
    if FFonts[i].StyleCount = 0 then AddStyle('Regular');

    for j := 0 to i-1 do
      if FFonts[j].Styles = FFonts[i].Styles then
      begin
        ADuplicates:= true;
        break;
      end;
  end;
end;

constructor TFamilyCollectionItem.Create(AName: string);
begin
  FFamilyName:= AName;
  FFontCount := 0;
  FFonts := nil;
  FStyleCount := 0;
  FStyles := nil;
  FUsePostscriptStyle:= false;
end;

procedure TFamilyCollectionItem.AddFont(AFontItem: TFontCollectionItem);
var i: integer;
    DuplicateStyle: boolean;
    StyleNumber: integer;
    TempStyles,BaseStyle: string;
begin
  if AFontItem = nil then exit;

  for i := 0 to FFontCount-1 do
    if FFonts[i] = AFontItem then exit;

  if AFontItem.Family <> nil then
    raise exception.Create('This font already belongs to another family');

  if FFontCount = length(FFonts) then
    setlength(FFonts, length(FFonts)+4);

  FFonts[FFontCount] := AFontItem;
  AFontItem.Family := self;
  inc(FFontCount);

  if FUsePostscriptStyle then AFontItem.UsePostscriptStyle := true;

  for i := 0 to AFontItem.StyleCount -1 do
    AddStyle(AFontItem.Style[i]);

  //add regular style if no other style
  if AFontItem.StyleCount = 0 then AddStyle('Regular');

  DuplicateStyle := false;
  for i := 0 to FFontCount-2 do
    if FFonts[i].Styles = AFontItem.Styles then
    begin
      DuplicateStyle:= true;
      break;
    end;

  if DuplicateStyle and not FUsePostscriptStyle then
  begin //try with postscript styles instead
    FUsePostscriptStyle:= true;
    RebuildStyleList(DuplicateStyle);
  end;

  if DuplicateStyle then
  begin
    StyleNumber := 1;
    BaseStyle := AFontItem.Styles;
    if BaseStyle = 'Regular' then BaseStyle := 'Unknown';
    repeat
      if StyleNumber = 1 then
        TempStyles := BaseStyle
      else
        TempStyles := BaseStyle+' '+IntToStr(StyleNumber);
      DuplicateStyle := false;
      for i := 0 to FFontCount-2 do
        if FFonts[i].Styles = TempStyles then
        begin
          DuplicateStyle:= true;
          inc(StyleNumber);
          break;
        end;
    until not DuplicateStyle;
    AFontItem.Information[ftiStyle] := TempStyles;
  end;
end;

function TFamilyCollectionItem.GetFont(const AStyles: array of string;
  NeedAllStyles: boolean; NoMoreStyle: boolean): TCustomFontCollectionItem;
var idx: integer;
begin
  idx := GetFontIndex(AStyles,NeedAllStyles,NoMoreStyle);
  if idx = -1 then result := nil
  else result := Font[idx];
end;

function TFamilyCollectionItem.GetFontIndex(const AStyles: array of string; NeedAllStyles: boolean; NoMoreStyle: boolean): integer;
var curCount,curMissing,maxStyleCount,minMissingCount: integer;
    bestMatch: integer;
    i,j: integer;
begin
  maxStyleCount := -1;
  minMissingCount := 0;
  bestMatch := -1;
  for i := 0 to FontCount-1 do
  begin
    curCount := 0;
    curMissing := 0;
    for j := 0 to high(AStyles) do
      if Font[i].HasStyle(AStyles[j]) then
        inc(curCount);
    curMissing := Font[i].StyleCount-curCount;
    if NeedAllStyles and (curCount <> length(AStyles)) then continue;
    if NoMoreStyle and (curMissing > 0) then continue;
    if (curCount > maxStyleCount) or ((curCount = maxStyleCount) and (curMissing < minMissingCount)) then
    begin
      maxStyleCount := curCount;
      minMissingCount:= curMissing;
      bestMatch := i;
    end;
  end;

  for i := 0 to FontCount-1 do
  begin
    curCount := 0;
    curMissing := 0;
    for j := 0 to high(AStyles) do
      if Font[i].HasStyle(AStyles[j]) or
         ((CompareText(AStyles[j],'Italic')=0) and Font[i].HasStyle('Oblique')) or
         ((CompareText(AStyles[j],'Oblique')=0) and Font[i].HasStyle('Italic')) then
        inc(curCount);
    curMissing := Font[i].StyleCount-curCount;
    if NeedAllStyles and (curCount <> length(AStyles)) then continue;
    if NoMoreStyle and (curMissing > 0) then continue;
    if (curCount > maxStyleCount) or ((curCount = maxStyleCount) and (curMissing < minMissingCount)) then
    begin
      maxStyleCount := curCount;
      minMissingCount:= curMissing;
      bestMatch := i;
    end;
  end;
  result := bestMatch;
end;

function TFamilyCollectionItem.GetFontIndex(AStyle: string;
  NeedAllStyles: boolean; NoMoreStyle: boolean): integer;
begin
  result := GetFontIndexByStyles(AStyle); //exact match
  if result = -1 then
    result := GetFontIndex(StylesToArray(AStyle),NeedAllStyles,NoMoreStyle);
end;

function TFamilyCollectionItem.RemoveFont(AFontItem: TCustomFontCollectionItem
  ): boolean;
var i,j: integer;
  dummy: boolean;
begin
  for i := 0 to FFontCount-1 do
    if FFonts[i] = AFontItem then
    begin
      for j := i to FFontCount-2 do
        FFonts[j] := FFonts[j+1];
      dec(FFontCount);
      AFontItem.Family := nil;
      RebuildStyleList(dummy);
      result := true;
      exit;
    end;
  result := false;
end;

function TFamilyCollectionItem.GetFont(AStyle: string; NeedAllStyles: boolean; NoMoreStyle: boolean): TCustomFontCollectionItem;
begin
  result := GetFontByStyles(AStyle); //exact match
  if result = nil then
    result := GetFont(StylesToArray(AStyle),NeedAllStyles,NoMoreStyle);
end;

function TFamilyCollectionItem.HasStyle(AName: string): boolean;
var i: integer;
begin
  for i := 0 to FStyleCount-1 do
    if CompareText(FStyles[i],AName)=0 then
    begin
      result := true;
      exit;
    end;
  result := false;
end;

{ TFontCollection }

function TFreeTypeFontCollection.GetFontCount: integer;
begin
  result := FFontList.Count;
end;

function TFreeTypeFontCollection.RemoveAndFreeFamily(
  AFamily: TCustomFamilyCollectionItem): boolean;
begin
  if FFamilyList.Remove(Pointer(AFamily)) then
  begin
    result := true;
    AFamily.Free;
  end else
    result := false;
end;

function TFreeTypeFontCollection.GetFamilyCount: integer;
begin
  result := FFamilyList.Count;
end;

function TFreeTypeFontCollection.FindFont(AFileName: string): TFontCollectionItem;
var
  Comp: integer;
  node: TAvlTreeNode;
begin
  node:= FFontList.Root;
  while (node<>nil) do begin
    Comp:=CompareStr(AFileName,TFontCollectionItem(node.Data).Filename);
    if Comp=0 then break;
    if Comp<0 then begin
      node:=node.Left
    end else begin
      node:=node.Right
    end;
  end;
  if node = nil then
    result := nil
  else
    result := TFontCollectionItem(node.Data);
end;

function TFreeTypeFontCollection.GetFamily(AName: string): TCustomFamilyCollectionItem;
var
  i,j: Integer;
begin
  if AName = '' then
  begin
    result := GetFamily('Arial');
    exit;
  end;
  result := FindFamily(AName);
  if result <> nil then exit;

  if (CompareText(AName,'Arial')=0) then
  begin
    if result = nil then result := FindFamily('Helvetica Neue');
    if result = nil then result := FindFamily('Helvetica');
  end else
  if (CompareText(AName,'Helvetica')=0) then
  begin
    if result = nil then result := FindFamily('Helvetica Neue');
    if result = nil then result := FindFamily('Arial');
  end else
  if (CompareText(AName,'Helvetica Neue')=0) then
  begin
    if result = nil then result := FindFamily('Helvetica');
    if result = nil then result := FindFamily('Arial');
  end;
  if result <> nil then exit;

  for i := low(ArialLikeFonts) to high(ArialLikeFonts) do
    if CompareText(AName,ArialLikeFonts[i])=0 then
    begin
      for j := 0 to high(ArialLikeFonts) do
      begin
        result := FindFamily(ArialLikeFonts[j]);
        if result <> nil then break;
      end;
      break;
    end;

  if (result = nil) and (CompareText(AName,'Courier New')=0) then result := FindFamily('Nimbus Monospace');
  if (result = nil) and (CompareText(AName,'Courier New')=0) then result := FindFamily('Courier');
  if (result = nil) and (CompareText(AName,'Nimbus Monospace')=0) then result := FindFamily('Courier New');
  if (result = nil) and (CompareText(AName,'Nimbus Monospace')=0) then result := FindFamily('Courier');
  if (result = nil) and (CompareText(AName,'Courier')=0) then result := FindFamily('Courier New');
  if (result = nil) and (CompareText(AName,'Courier')=0) then result := FindFamily('Nimbus Monospace');

  if (result = nil) and (CompareText(AName,'Times')=0) then result := FindFamily('Times New Roman');
  if (result = nil) and (CompareText(AName,'Times')=0) then result := FindFamily('CG Times');
  if (result = nil) and (CompareText(AName,'Times New Roman')=0) then result := FindFamily('Times');
  if (result = nil) and (CompareText(AName,'Times New Roman')=0) then result := FindFamily('CG Times');
  if (result = nil) and (CompareText(AName,'CG Times')=0) then result := FindFamily('Times');
  if (result = nil) and (CompareText(AName,'CG Times')=0) then result := FindFamily('Times New Roman');
end;

function TFreeTypeFontCollection.AddFamily(AName: string): TFamilyCollectionItem;
var
  f: TFamilyCollectionItem;
begin
  f := FindFamily(AName);
  if f = nil then
  begin
    result := TFamilyCollectionItem.Create(AName);
    FFamilyList.Add(result);
  end else
    result := f;
end;

function TFreeTypeFontCollection.FindFamily(AName: string): TFamilyCollectionItem;
var
  Comp: integer;
  node: TAvlTreeNode;
begin
  node:= FFamilyList.Root;
  while (node<>nil) do begin
    Comp:=CompareText(AName,TFamilyCollectionItem(node.Data).FamilyName);
    if Comp=0 then break;
    if Comp<0 then begin
      node:=node.Left
    end else begin
      node:=node.Right
    end;
  end;
  if node = nil then
    result := nil
  else
    result := TFamilyCollectionItem(node.Data);
end;

function TFreeTypeFontCollection.CompareFontFileName(Tree: TAvlTree; Data1,
  Data2: Pointer): integer;
begin
  result := CompareStr(TFontCollectionItem(Data1).Filename,TFontCollectionItem(Data2).Filename);
end;

function TFreeTypeFontCollection.CompareFamilyName(Tree: TAvlTree; Data1,
  Data2: Pointer): integer;
begin
  result := CompareText(TFamilyCollectionItem(Data1).FamilyName,TFamilyCollectionItem(Data2).FamilyName);
end;

function TFreeTypeFontCollection.GetFont(AFileName: string): TCustomFontCollectionItem;
begin
  AFilename:= ExpandFileName(AFilename);
  result := FindFont(AFilename);
end;

constructor TFreeTypeFontCollection.Create;
begin
  FUpdateCount := 0;
  FTempFont := nil;
  FFontList := TAvlTree.CreateObjectCompare(@CompareFontFileName);
  FFamilyList := TAvlTree.CreateObjectCompare(@CompareFamilyName);
end;

procedure TFreeTypeFontCollection.Clear;
begin
  FFamilyList.FreeAndClear;
  FFontList.FreeAndClear;
end;

procedure TFreeTypeFontCollection.BeginUpdate;
begin
  if (FUpdateCount = 0) and (FTempFont = nil) then
    FTempFont := TFreeTypeFont.Create;
  inc(FUpdateCount);
end;

procedure TFreeTypeFontCollection.AddFolder(AFolder: string;
  AIncludeSubdirs: Boolean = false);
var
  files: TStringList;
  i: integer;
begin
  AFolder := ExpandFileName(AFolder);
  if (length(AFolder) <> 0) and (AFolder[length(AFolder)] <> PathDelim) then
    AFolder += PathDelim;

  files := TStringList.Create;
  BeginUpdate;
  try
    FindAllFiles(files, AFolder, '*.ttf', AIncludeSubdirs);
    files.Sort;
    for i := 0 to files.Count-1 do
      AddFile(files[i]);
  finally
    EndUpdate;
  end;
  files.Free;
end;

procedure TFreeTypeFontCollection.RemoveFolder(AFolder: string);
var toBeDeleted: TStringList;
    enumerator: TAvlTreeNodeEnumerator;
    i: Integer;
begin
  AFolder := ExpandFileName(AFolder);
  if (length(AFolder) <> 0) and (AFolder[length(AFolder)] <> PathDelim) then
    AFolder += PathDelim;

  toBeDeleted := TStringList.Create;
  enumerator := FFontList.GetEnumerator;
  while enumerator.MoveNext do
  begin
    with TCustomFontCollectionItem(enumerator.Current.Data) do
    begin
      if copy(Filename, 1, length(AFolder)) = AFolder then
        toBeDeleted.Add(Filename);
    end;
  end;
  enumerator.Free;

  for i := 0 to toBeDeleted.Count-1 do
    RemoveFile(toBeDeleted[i]);
  toBeDeleted.Free;
end;

function TFreeTypeFontCollection.AddFile(AFilename: string
  ): TCustomFontCollectionItem;
var info: TFreeTypeInformation;
    fName: string;
    item: TFontCollectionItem;
    f: TFamilyCollectionItem;
begin
  AFilename:= ExpandFileName(AFilename);
  result := nil;
  BeginUpdate;
  try
    FTempFont.Name := AFilename;
    fName := FTempFont.Family;
    if fName <> '' then
    begin
      f := AddFamily(fName);
      item := TFontCollectionItem.Create(AFilename);
      FFontList.Add(item);
      with item do
      begin
        VersionNumber:= FTempFont.VersionNumber;
        for info := low(TFreeTypeInformation) to high(TFreeTypeInformation) do
          Information[info] := FTempFont.Information[info];
      end;
      f.AddFont(item);
      result := item;
    end;
  finally
    EndUpdate;
  end;
end;

function TFreeTypeFontCollection.RemoveFile(AFilename: string): boolean;
var fontItem : TCustomFontCollectionItem;
    f: TFamilyCollectionItem;
begin
  AFilename:= ExpandFileName(AFilename);
  fontItem := GetFont(AFilename);
  if (fontItem = nil) or not (fontItem.Family is TFamilyCollectionItem) then
  begin
    result := false;
    exit;
  end;
  FFontList.Remove(Pointer(fontItem));
  f := fontItem.Family as TFamilyCollectionItem;
  result := f.RemoveFont(fontItem);
  if result then
  begin
    if f.FontCount = 0 then
      RemoveAndFreeFamily(f);
  end;
end;

function TFreeTypeFontCollection.AddStream(AStream: TStream; AOwned: boolean): boolean;
var info: TFreeTypeInformation;
    fName: string;
    item: TFontCollectionItem;
    f: TFamilyCollectionItem;
begin
  result := false;
  BeginUpdate;
  try
    FTempFont.AccessFromStream(AStream,False);
    fName := FTempFont.Family;
    if fName <> '' then
    begin
      f := AddFamily(fName);
      item := TFontCollectionItem.Create(AStream,AOwned);
      FFontList.Add(item);
      with item do
      begin
        VersionNumber:= FTempFont.VersionNumber;
        for info := low(TFreeTypeInformation) to high(TFreeTypeInformation) do
          Information[info] := FTempFont.Information[info];
      end;
      f.AddFont(item);
      result := true;
    end;
  finally
    FTempFont.Name := '';
    EndUpdate;
  end;
end;

procedure TFreeTypeFontCollection.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    dec(FUpdateCount);
    if FUpdateCount = 0 then FreeAndNil(FTempFont);
  end;
end;

destructor TFreeTypeFontCollection.Destroy;
begin
  Clear;
  FFontList.Free;
  FFamilyList.Free;
  FTempFont.Free;
  inherited Destroy;
end;

function TFreeTypeFontCollection.FontFileEnumerator: IFreeTypeFontEnumerator;
begin
  result := TFontEnumerator.Create(FFontList.GetEnumerator);
end;

function TFreeTypeFontCollection.FamilyEnumerator: IFreeTypeFamilyEnumerator;
begin
  result := TFamilyEnumerator.Create(FFamilyList.GetEnumerator);
end;

var
  InternalDefaultFontCollection : TFreeTypeFontCollection;

initialization

  InternalDefaultFontCollection := TFreeTypeFontCollection.Create;
  FontCollection := InternalDefaultFontCollection;

finalization

  InternalDefaultFontCollection.Free;

end.

