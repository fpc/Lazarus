{
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
unit GtkFontCache;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LCLType, AvgLvlTree, gdk, gtkdef;
  
type
  TGdkFontCache = class;

  { TGdkFontCacheItem }
  
  TGdkFontCacheItem = class
  private
    FReferenceCount: integer;
    FCache: TGdkFontCache;
  public
    GdkFont: PGDKFont;
    
    // font identification
    ID: integer;
    LogFont: TLogFont;
    LongFontName: string;
    xlfd: string;
    
    // metrics
    MetricsValid: boolean;
    lBearing: LongInt;
    rBearing: LongInt;
    TextMetric: TTextMetric;
    IsDoubleByteChar: boolean;
    
    constructor Create(TheCache: TGdkFontCache; TheID: integer;
                       TheGdkFont: PGDKFont);
    destructor Destroy; override;
    procedure IncreaseRefCount;
    procedure DecreaseRefCount;
  end;

  { TGdkFontCache }

  TGdkFontCache = class
  private
    FCacheSize: integer;
    FItemsSortedForGdkFont: TAvgLvlTree;
    FItemsSortedForLogFont: TAvgLvlTree;
    FIDCount: integer;
    fDestroying: boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function FindGDKFont(TheGdkFont: PGDKFont): TGdkFontCacheItem;
    function FindGDKFont(const LogFont: TLogFont;
                         const LongFontName: string): TGdkFontCacheItem;
    procedure Remove(Item: TGdkFontCacheItem);
    procedure Add(Item: TGdkFontCacheItem);
    procedure Add(TheGdkFont: PGDKFont);
    procedure Add(TheGdkFont: PGDKFont; const LogFont: TLogFont;
                  const LongFontName: string);
    function CreateNewItem(TheGdkFont: PGDKFont): TGdkFontCacheItem;
    procedure Ref(TheGdkFont: PGDKFont);
    procedure UnRef(TheGdkFont: PGDKFont);
    procedure UnrefUnusedOldest;
    property CacheSize: integer read FCacheSize write FCacheSize;
  end;
  
var
  FontCache: TGdkFontCache;

implementation

type
  TLogFontAndName = record
    LogFont: TLogFont;
    LongFontName: string;
  end;
  PLogFontAndName = ^TLogFontAndName;

function CompareGdkFonts(Item1, Item2: TGdkFontCacheItem): integer;
begin
  Result:=ComparePointers(Item1.GdkFont,Item2.GdkFont);
end;

function CompareLogFonts(Item1, Item2: TGdkFontCacheItem): integer;
begin
  Result:=CompareStr(Item1.LongFontName,Item2.LongFontName);
  if Result<>0 then exit;
  Result:=CompareMemRange(@Item1.LogFont,@Item2.LogFont,SizeOf(Item1.LogFont));
  if Result<>0 then exit;
  if Item1.ID<0 then Result:=-1
  else if Item1.ID>0 then Result:=1;
end;

function CompareGdkFontWithItem(Font: PGDKFont;
  Item: TGdkFontCacheItem): integer;
begin
  Result:=ComparePointers(Font,Item.GdkFont);
end;

function CompareLogFontAndNameWithItem(Key: PLogFontAndName;
  Item: TGdkFontCacheItem): integer;
begin
  Result:=CompareStr(Key^.LongFontName,Item.LongFontName);
  if Result=0 then
    Result:=CompareMemRange(@Key^.LogFont,@Item.LogFont,SizeOf(Item.LogFont));
  //writeln('CompareLogFontAndNameWithItem Result=',Result,' Key=',Key^.LogFont.lfWeight,'/',Key^.LongFontName,
  //' Item=',Item.LogFont.lfWeight,'/',Item.LongFontName);
end;


{ TGdkFontCacheItem }

constructor TGdkFontCacheItem.Create(TheCache: TGdkFontCache;
   TheID: integer; TheGdkFont: PGDKFont);
begin
  FCache:=TheCache;
  ID:=TheID;
  GdkFont:=TheGdkFont;
  gdk_font_ref(GdkFont);
  FReferenceCount:=2; // one for adding and one for caching
end;

destructor TGdkFontCacheItem.Destroy;
begin
  gdk_font_unref(GdkFont);
  if FCache<>nil then
    FCache.Remove(Self);
  inherited Destroy;
end;

procedure TGdkFontCacheItem.IncreaseRefCount;

  procedure WarnRef;
  begin
    debugln('warning: TGdkFontCacheItem.IncreaseRefCount '
      +'It seems a font is not unreferenced. '
      +'FontName=',LongFontName,' ',LogFont.lfFaceName,
      +' RefCnt=',dbgs(FReferenceCount));
    //RaiseGDBException('TGdkFontCacheItem.IncreaseRefCount');
  end;
  
begin
  inc(FReferenceCount);
  if (FReferenceCount=100) or (FReferenceCount=1000) then
    WarnRef;
end;

procedure TGdkFontCacheItem.DecreaseRefCount;
begin
  if FReferenceCount=0 then
    RaiseGDBException('TGdkFontCacheItem.DecreaseRefCount');
  dec(FReferenceCount);
  if FReferenceCount=0 then
    Free;
end;

{ TGdkFontCache }

constructor TGdkFontCache.Create;
begin
  FCacheSize:=30;
  FItemsSortedForGdkFont:=TAvgLvlTree.Create(@CompareGdkFonts);
  FItemsSortedForLogFont:=TAvgLvlTree.Create(@CompareLogFonts);
end;

destructor TGdkFontCache.Destroy;
begin
  fDestroying:=true;
  // free all items
  FItemsSortedForGdkFont.FreeAndClear;
  // free trees
  FItemsSortedForGdkFont.Free;
  FItemsSortedForLogFont.Free;
  inherited Destroy;
end;

function TGdkFontCache.FindGDKFont(TheGdkFont: PGDKFont): TGdkFontCacheItem;
var
  ANode: TAvgLvlTreeNode;
begin
  ANode:=FItemsSortedForGdkFont.FindKey(TheGdkFont,@CompareGdkFontWithItem);
  if ANode<>nil then
    Result:=TGdkFontCacheItem(ANode.Data)
  else
    Result:=nil;
end;

function TGdkFontCache.FindGDKFont(const LogFont: TLogFont;
  const LongFontName: string): TGdkFontCacheItem;
var
  Key: TLogFontAndName;
  ANode: TAvgLvlTreeNode;
begin
  Key.LogFont:=LogFont;
  Key.LongFontName:=LongFontName;
  ANode:=FItemsSortedForLogFont.FindKey(@Key,@CompareLogFontAndNameWithItem);
  if ANode<>nil then
    Result:=TGdkFontCacheItem(ANode.Data)
  else
    Result:=nil;
end;

procedure TGdkFontCache.Remove(Item: TGdkFontCacheItem);
begin
  {$IFDEF VerboseFontCache}
  debugln('TGdkFontCache.Remove ',HexStr(Cardinal(Item.GdkFont),8),' LongFontName=',Item.LongFontName,' lfFaceName=',Item.LogFont.lfFaceName);
  {$ENDIF}
  if not fDestroying then begin
    FItemsSortedForGdkFont.Remove(Item);
    FItemsSortedForLogFont.Remove(Item);
  end;
end;

procedure TGdkFontCache.Add(Item: TGdkFontCacheItem);
var
  OldItem: TGdkFontCacheItem;
  //ANode: TAvgLvlTreeNode;
begin
  {$IFDEF VerboseFontCache}
  debugln('TGdkFontCache.Add ',HexStr(Cardinal(Item.GdkFont),8),
    ' LongFontName=',Item.LongFontName,' lfFaceName=',Item.LogFont.lfFaceName);
  {$ENDIF}
  OldItem:=FindGDKFont(Item.GdkFont);
  if OldItem<>nil then begin
    debugln('TGdkFontCache.Add New=',Item.LongFontName,'/',Item.LogFont.lfFaceName);
    debugln('TGdkFontCache.Add Old=',OldItem.LongFontName,'/',OldItem.LogFont.lfFaceName);
    RaiseGDBException('TGdkFontCache.Add');
  end;
  FItemsSortedForGdkFont.Add(Item);
  FItemsSortedForLogFont.Add(Item);
  
  {ANode:=FItemsSortedForGdkFont.FindLowest;
  while ANode<>nil do begin
    Item:=TGdkFontCacheItem(ANode.Data);
    writeln('TGdkFontCache.Add DumpA ',Item.LongFontName,' ',Item.LogFont.lfWeight);
    ANode:=FItemsSortedForGdkFont.FindSuccessor(ANode);
  end;
  ANode:=FItemsSortedForLogFont.FindLowest;
  while ANode<>nil do begin
    Item:=TGdkFontCacheItem(ANode.Data);
    writeln('TGdkFontCache.Add DumpB ',Item.LongFontName,' ',Item.LogFont.lfWeight);
    ANode:=FItemsSortedForLogFont.FindSuccessor(ANode);
  end;}
end;

procedure TGdkFontCache.Add(TheGdkFont: PGDKFont);
var
  NewItem: TGdkFontCacheItem;
begin
  //debugln('TGdkFontCache.Add ',HexStr(Cardinal(TheGdkFont),8));
  NewItem:=CreateNewItem(TheGdkFont);
  Add(NewItem);
end;

procedure TGdkFontCache.Add(TheGdkFont: PGDKFont; const LogFont: TLogFont;
  const LongFontName: string);
var
  NewItem: TGdkFontCacheItem;
  i: Integer;
begin
  if FindGDKFont(LogFont,LongFontName)<>nil then
    RaiseGDBException('TGdkFontCache.Add Already exists');
  NewItem:=CreateNewItem(TheGdkFont);
  NewItem.LogFont:=LogFont;
  NewItem.LongFontName:=LongFontName;
  {debugln('TGdkFontCache.Add ',HexStr(Cardinal(TheGdkFont),8),
  ' LongFontName=',LongFontName,' lfFaceName=',LogFont.lfFaceName,
     ' '+dbgs(LogFont.lfCharSet)
    +' '+dbgs(LogFont.lfClipPrecision)
    +' '+dbgs(LogFont.lfEscapement)
    +' '+dbgs(LogFont.lfHeight)
    +' '+dbgs(LogFont.lfItalic)
    +' '+dbgs(LogFont.lfOrientation)
    +' '+dbgs(LogFont.lfOutPrecision)
    +' '+dbgs(LogFont.lfPitchAndFamily)
    +' '+dbgs(LogFont.lfQuality)
    +' '+dbgs(LogFont.lfStrikeOut)
    +' '+dbgs(LogFont.lfUnderline)
    +' '+dbgs(LogFont.lfWeight)
    +' '+dbgs(LogFont.lfWidth));
  for i:=0 to SizeOf(LogFont)-1 do
    write(hexstr(ord(PChar(@LogFont)[i]),2));
  writeln('');}
  Add(NewItem);
  if FindGDKFont(LogFont,LongFontName)=nil then
    RaiseGDBException('TGdkFontCache.Add added where?');
end;

function TGdkFontCache.CreateNewItem(TheGdkFont: PGDKFont): TGdkFontCacheItem;
begin
  if FIDCount=High(integer) then
    FIDCount:=Low(integer);
  inc(FIDCount);
  Result:=TGdkFontCacheItem.Create(Self,FIDCount,TheGdkFont);
end;

procedure TGdkFontCache.Ref(TheGdkFont: PGDKFont);
var
  Item: TGdkFontCacheItem;
begin
  Item:=FindGDKFont(TheGdkFont);
  if Item<>nil then
    Item.IncreaseRefCount
  else
    gdk_font_ref(TheGdkFont);
end;

procedure TGdkFontCache.UnRef(TheGdkFont: PGDKFont);
var
  Item: TGdkFontCacheItem;
begin
  Item:=FindGDKFont(TheGdkFont);
  if Item<>nil then
    Item.DecreaseRefCount
  else
    gdk_font_unref(TheGdkFont);
  if FItemsSortedForLogFont.Count>FCacheSize then
    UnrefUnusedOldest;
end;

procedure TGdkFontCache.UnrefUnusedOldest;
var
  ANode: TAvgLvlTreeNode;
  Item, UnusedItem: TGdkFontCacheItem;
begin
  ANode:=FItemsSortedForGdkFont.FindLowest;
  UnusedItem:=nil;
  while ANode<>nil do begin
    Item:=TGdkFontCacheItem(ANode.Data);
    if (Item.FReferenceCount=1) then begin
      // this item is unused
      if (UnusedItem=nil) or (UnusedItem.ID>Item.ID) then
        UnusedItem:=Item;
    end;
    ANode:=FItemsSortedForGdkFont.FindSuccessor(ANode);
  end;
  if UnusedItem<>nil then
    UnusedItem.DecreaseRefCount;
end;

initialization
  FontCache:=TGdkFontCache.Create;
  
finalization
  FontCache.Free;
  FontCache:=nil;

end.

