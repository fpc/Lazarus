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
  Classes, SysUtils, FPCAdds, LCLProc, LCLType, AvgLvlTree, gdk, gtkdef;
  
type
  TResourceCache = class;
  TResourceCacheDescriptor = class;

  { TResourceCacheItem }

  TResourceCacheItem = class
  private
    FReferenceCount: integer;
  public
    Handle: THandle;
    Cache: TResourceCache;
    FirstDescriptor, LastDescriptor: TResourceCacheDescriptor;
    Next, Prev: TResourceCacheItem;
    constructor Create(TheCache: TResourceCache; TheHandle: THandle);
    destructor Destroy; override;
    procedure IncreaseRefCount;
    procedure DecreaseRefCount;
    procedure AddToList(var First, Last: TResourceCacheItem);
    procedure RemoveFromList(var First, Last: TResourceCacheItem);
    procedure WarnReferenceHigh; virtual;
  public
    property ReferenceCount: integer read FReferenceCount;
  end;
  TResourceCacheItemClass = class of TResourceCacheItem;
  
  
  { TResourceCacheDescriptor }
  
  TResourceCacheDescriptor = class
  public
    Item: TResourceCacheItem;
    Cache: TResourceCache;
    Next, Prev: TResourceCacheDescriptor;
    constructor Create(TheCache: TResourceCache; TheItem: TResourceCacheItem);
    destructor Destroy; override;
    procedure AddToList(var First, Last: TResourceCacheDescriptor);
    procedure RemoveFromList(var First, Last: TResourceCacheDescriptor);
  end;
  TResourceCacheDescriptorClass = class of TResourceCacheDescriptor;


  { TResourceCache }
  
  TResourceCache = class
  protected
    FItems: TAvgLvlTree;
    FDescriptors: TAvgLvlTree;
    FDestroying: boolean;
    FResourceCacheDescriptorClass: TResourceCacheDescriptorClass;
    FResourceCacheItemClass: TResourceCacheItemClass;
    FMaxUnusedItem: integer; // how many freed resources to keep
    FFirstUnusedItem, FLastUnusedItem: TResourceCacheItem;
    FUnUsedItemCount: integer;
    procedure RemoveItem(Item: TResourceCacheItem); virtual;
    procedure RemoveDescriptor(Desc: TResourceCacheDescriptor); virtual;
    procedure ItemUsed(Item: TResourceCacheItem);
    procedure ItemUnused(Item: TResourceCacheItem);
    function ItemIsUsed(Item: TResourceCacheItem): boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function CompareItems(Tree: TAvgLvlTree; Item1, Item2: Pointer): integer; virtual;
    function CompareDescriptors(Tree: TAvgLvlTree; Desc1, Desc2: Pointer): integer; virtual; abstract;
    procedure ConsistencyCheck;
  public
    property MaxUnusedItem: integer read FMaxUnusedItem
                                           write FMaxUnusedItem;
    property ResourceCacheItemClass: TResourceCacheItemClass
                                                   read FResourceCacheItemClass;
    property ResourceCacheDescriptorClass: TResourceCacheDescriptorClass
                                             read FResourceCacheDescriptorClass;
  end;
  
  
  { THandleResourceCache }
  
  THandleResourceCache = class(TResourceCache)
  public
    function FindItem(Handle: THandle): TResourceCacheItem;
  end;
  
  
  { TBlockResourceCacheDescriptor }

  TBlockResourceCacheDescriptor = class(TResourceCacheDescriptor)
  public
    Data: Pointer;
    destructor Destroy; override;
  end;


  { TBlockResourceCache }
  
  TBlockResourceCache = class(THandleResourceCache)
  private
    FDataSize: integer;
  protected
    FOnCompareDescPtrWithDescriptor: TListSortCompare;
  public
    constructor Create(TheDataSize: integer);
    function FindDescriptor(DescPtr: Pointer): TBlockResourceCacheDescriptor;
    function AddResource(Handle: THandle; DescPtr: Pointer
                         ): TBlockResourceCacheDescriptor;
    function CompareDescriptors(Tree: TAvgLvlTree;
                                Desc1, Desc2: Pointer): integer; override;
  public
    property DataSize: integer read FDataSize;
    property OnCompareDescPtrWithDescriptor: TListSortCompare
                                           read FOnCompareDescPtrWithDescriptor;
  end;
  
function ComparePHandleWithResourceCacheItem(HandlePtr: PHandle;
  Item: TResourceCacheItem): integer;
function CompareDescPtrWithBlockResDesc(DescPtr: Pointer;
  Item: TBlockResourceCacheDescriptor): integer;


type
  TGdkFontCacheDescriptor = class;

  { TGdkFontCacheItem }
  
  TGdkFontCacheItem = class(TResourceCacheItem)
  public
    GdkFont: PGDKFont;

    // metrics
    MetricsValid: boolean;
    lBearing: LongInt;
    rBearing: LongInt;
    TextMetric: TTextMetric;
    IsDoubleByteChar: boolean;
    procedure WarnReferenceHigh; override;
  end;
  
  
  { TGdkFontCacheDescriptor }
  
  TGdkFontCacheDescriptor = class(TResourceCacheDescriptor)
  public
    LogFont: TLogFont;
    LongFontName: string;
    xlfd: string;
  end;
  
  
  { TGdkFontCache }
  
  TGdkFontCache = class(TResourceCache)
  protected
    procedure RemoveItem(Item: TResourceCacheItem); override;
  public
    constructor Create;
    destructor Destroy; override;
    function CompareItems(Tree: TAvgLvlTree; Item1, Item2: Pointer): integer; override;
    function CompareDescriptors(Tree: TAvgLvlTree; Desc1, Desc2: Pointer): integer; override;
    function FindGDKFont(TheGdkFont: PGDKFont): TGdkFontCacheItem;
    function FindGDKFontDesc(const LogFont: TLogFont;
                           const LongFontName: string): TGdkFontCacheDescriptor;
    function Add(TheGdkFont: PGDKFont; const LogFont: TLogFont;
                 const LongFontName: string): TGdkFontCacheDescriptor;
    procedure Reference(TheGdkFont: PGDKFont);
    procedure Unreference(TheGdkFont: PGDKFont);
    procedure DumpDescriptors;
  end;

function LogFontToString(const LogFont: TLogFont): string;
  
var
  FontCache: TGdkFontCache;

implementation

type
  TLogFontAndName = record
    LogFont: TLogFont;
    LongFontName: string;
  end;
  PLogFontAndName = ^TLogFontAndName;

function ComparePHandleWithResourceCacheItem(HandlePtr: PHandle;
  Item: TResourceCacheItem): integer;
begin
  Result:=CompareHandles(HandlePtr^,Item.Handle);
end;

function CompareDescPtrWithBlockResDesc(DescPtr: Pointer;
  Item: TBlockResourceCacheDescriptor): integer;
begin
  Result:=CompareMemRange(DescPtr,Item.Data,
                          TBlockResourceCache(Item.Cache).DataSize);
end;

function LogFontToString(const LogFont: TLogFont): string;
var
  i: Integer;
begin
  Result:=''
    +' lfFaceName="'+LogFont.lfFaceName+'" '
    +' CharSet='+dbgs(LogFont.lfCharSet)
    +' ClipPrecision='+dbgs(LogFont.lfClipPrecision)
    +' Escapement='+dbgs(LogFont.lfEscapement)
    +' Height='+dbgs(LogFont.lfHeight)
    +' Italic='+dbgs(LogFont.lfItalic)
    +' Orientation='+dbgs(LogFont.lfOrientation)
    +' OutPrecision='+dbgs(LogFont.lfOutPrecision)
    +' PitchAndFamily='+dbgs(LogFont.lfPitchAndFamily)
    +' Quality='+dbgs(LogFont.lfQuality)
    +' StrikeOut='+dbgs(LogFont.lfStrikeOut)
    +' Underline='+dbgs(LogFont.lfUnderline)
    +' Weight='+dbgs(LogFont.lfWeight)
    +' Width='+dbgs(LogFont.lfWidth)
    +#13#10;
  for i:=0 to SizeOf(LogFont)-1 do
    Result:=Result+hexstr(ord(PChar(@LogFont)[i]),2);
  Result:=Result+#13#10;
end;


{ TResourceCacheItem }

constructor TResourceCacheItem.Create(TheCache: TResourceCache;
  TheHandle: THandle);
begin
  Cache:=TheCache;
  Handle:=TheHandle;
end;

destructor TResourceCacheItem.Destroy;
begin
  Cache.RemoveItem(Self);
  inherited Destroy;
end;

procedure TResourceCacheItem.IncreaseRefCount;
begin
  inc(FReferenceCount);
  if FReferenceCount=1 then
    Cache.ItemUsed(Self);
  if (FReferenceCount=100) or (FReferenceCount=1000) then
    WarnReferenceHigh;
end;

procedure TResourceCacheItem.DecreaseRefCount;
begin
  if FReferenceCount=0 then
    RaiseGDBException('TResourceCacheItem.DecreaseRefCount=0');
  dec(FReferenceCount);
  if FReferenceCount=0 then
    Cache.ItemUnused(Self);
end;

procedure TResourceCacheItem.AddToList(var First, Last: TResourceCacheItem
  );
// add as last
begin
  Next:=nil;
  Prev:=Last;
  Last:=Self;
  if First=nil then First:=Self;
  if Prev<>nil then Prev.Next:=Self;
end;

procedure TResourceCacheItem.RemoveFromList(var First,Last: TResourceCacheItem);
begin
  if First=Self then First:=Next;
  if Last=Self then Last:=Prev;
  if Next<>nil then Next.Prev:=Prev;
  if Prev<>nil then Prev.Next:=Next;
  Next:=nil;
  Prev:=nil;
end;

procedure TResourceCacheItem.WarnReferenceHigh;
begin
  debugln('WARNING: TResourceCacheItem.IncreaseRefCount ',dbgs(FReferenceCount));
end;

{ TResourceCacheDescriptor }

constructor TResourceCacheDescriptor.Create(TheCache: TResourceCache;
  TheItem: TResourceCacheItem);
begin
  Cache:=TheCache;
  Item:=TheItem;
  Item.IncreaseRefCount;
  AddToList(Item.FirstDescriptor,Item.LastDescriptor);
end;

destructor TResourceCacheDescriptor.Destroy;
begin
  Cache.RemoveDescriptor(Self);
  inherited Destroy;
end;

procedure TResourceCacheDescriptor.AddToList(
  var First, Last: TResourceCacheDescriptor);
// add as last
begin
  Next:=nil;
  Prev:=Last;
  Last:=Self;
  if First=nil then First:=Self;
  if Prev<>nil then Prev.Next:=Self;
end;

procedure TResourceCacheDescriptor.RemoveFromList(
  var First, Last: TResourceCacheDescriptor);
begin
  if First=Self then First:=Next;
  if Last=Self then Last:=Prev;
  if Next<>nil then Next.Prev:=Prev;
  if Prev<>nil then Prev.Next:=Next;
  Next:=nil;
  Prev:=nil;
end;

{ TResourceCache }

procedure TResourceCache.RemoveItem(Item: TResourceCacheItem);
begin
  if FDestroying then exit;
  while Item.FirstDescriptor<>nil do Item.FirstDescriptor.Free;
  FItems.Remove(Item);
end;

procedure TResourceCache.RemoveDescriptor(Desc: TResourceCacheDescriptor);
begin
  if FDestroying then exit;
  Desc.RemoveFromList(Desc.Item.FirstDescriptor,Desc.Item.LastDescriptor);
  FDescriptors.Remove(Desc);
  if Desc.Item.FirstDescriptor=nil then
    Desc.Item.Free;
end;

procedure TResourceCache.ItemUsed(Item: TResourceCacheItem);
// called after creation or when Item is used again
begin
  if not ItemIsUsed(Item) then begin
    Item.RemoveFromList(FFirstUnusedItem,FLastUnusedItem);
    dec(FUnUsedItemCount);
  end;
end;

procedure TResourceCache.ItemUnused(Item: TResourceCacheItem);
// called when Item is not used any more
begin
  if not ItemIsUsed(Item) then
    raise Exception.Create('TResourceCache.ItemUnused');
  Item.AddToList(FFirstUnusedItem,FLastUnusedItem);
  inc(FUnUsedItemCount);
  if FUnUsedItemCount>FMaxUnusedItem then
    // maximum unused resources reached -> free the oldest
    FFirstUnusedItem.Free;
end;

function TResourceCache.ItemIsUsed(Item: TResourceCacheItem): boolean;
begin
  Result:=(FFirstUnusedItem<>Item) and (Item.Next=nil)
          and (Item.Prev=nil)
end;

constructor TResourceCache.Create;
begin
  FMaxUnusedItem:=100;
  FItems:=TAvgLvlTree.CreateObjectCompare(@CompareItems);
  FDescriptors:=TAvgLvlTree.CreateObjectCompare(@CompareDescriptors);
  FResourceCacheItemClass:=TResourceCacheItem;
  FResourceCacheDescriptorClass:=TResourceCacheDescriptor;
end;

destructor TResourceCache.Destroy;
begin
  FDestroying:=true;
  FItems.FreeAndClear;
  FItems.Free;
  FItems:=nil;
  FDescriptors.FreeAndClear;
  FDescriptors.Free;
  FDescriptors:=nil;
  inherited Destroy;
end;

function TResourceCache.CompareItems(Tree: TAvgLvlTree; Item1, Item2: Pointer
  ): integer;
begin
  Result:=CompareHandles(TResourceCacheItem(Item1).Handle,
                         TResourceCacheItem(Item2).Handle);
end;

procedure TResourceCache.ConsistencyCheck;
var
  ANode: TAvgLvlTreeNode;
  Item: TResourceCacheItem;
begin
  if (FFirstUnusedItem=nil) xor (FLastUnusedItem=nil) then
    RaiseGDBException('');

  // check items
  ANode:=FItems.FindLowest;
  while ANode<>nil do begin
    Item:=TResourceCacheItem(ANode.Data);
    if Item.FirstDescriptor=nil then
      RaiseGDBException('');
    if Item.LastDescriptor=nil then
      RaiseGDBException('');
    ANode:=FItems.FindSuccessor(ANode);
  end;
end;

{ THandleResourceCache }

function THandleResourceCache.FindItem(Handle: THandle): TResourceCacheItem;
var
  ANode: TAvgLvlTreeNode;
begin
  ANode:=FItems.FindKey(@Handle,@ComparePHandleWithResourceCacheItem);
  if ANode<>nil then
    Result:=TResourceCacheItem(ANode.Data)
  else
    Result:=nil;
end;

{ TBlockResourceCache }

constructor TBlockResourceCache.Create(TheDataSize: integer);
begin
  inherited Create;
  FDataSize:=DataSize;
  FResourceCacheDescriptorClass:=TBlockResourceCacheDescriptor;
  FOnCompareDescPtrWithDescriptor:=@CompareDescPtrWithBlockResDesc;
end;

function TBlockResourceCache.FindDescriptor(DescPtr: Pointer
  ): TBlockResourceCacheDescriptor;
var
  ANode: TAvgLvlTreeNode;
begin
  ANode:=FDescriptors.FindKey(DescPtr,FOnCompareDescPtrWithDescriptor);
  if ANode<>nil then
    Result:=TBlockResourceCacheDescriptor(ANode.Data)
  else
    Result:=nil;
end;

function TBlockResourceCache.AddResource(Handle: THandle; DescPtr: Pointer
  ): TBlockResourceCacheDescriptor;
var
  Item: TResourceCacheItem;

  procedure RaiseDescriptorAlreadyAdded;
  var
    Msg: String;
    i: Integer;
  begin
    Msg:='TBlockResourceCache.AddResource Descriptor Already Added '#13;
    for i:=0 to DataSize-1 do
      Msg:=Msg+hexstr(ord(PChar(DescPtr)[i]),2);
    raise Exception.Create(Msg);
  end;
  
begin
  Result:=FindDescriptor(DescPtr);
  if Result<>nil then
    RaiseDescriptorAlreadyAdded;
    
  Item:=FindItem(Handle);
  if Item=nil then begin
    Item:=FResourceCacheItemClass.Create(Self,Handle);
    FItems.Add(Item);
  end;
  Result:=TBlockResourceCacheDescriptor(
                               FResourceCacheDescriptorClass.Create(Self,Item));
  ReAllocMem(Result.Data,DataSize);
  System.Move(DescPtr^,Result.Data^,DataSize);
  FDescriptors.Add(Result);
end;

function TBlockResourceCache.CompareDescriptors(Tree: TAvgLvlTree; Desc1,
  Desc2: Pointer): integer;
begin
  Result:=CompareMemRange(TBlockResourceCacheDescriptor(Desc1).Data,
                          TBlockResourceCacheDescriptor(Desc2).Data,
                          DataSize);
end;

{ TBlockResourceCacheDescriptor }

destructor TBlockResourceCacheDescriptor.Destroy;
begin
  ReAllocMem(Data,0);
  inherited Destroy;
end;

{ TGdkFontCache }

function CompareGdkFontWithResItem(Font: PGDKFont;
  Item: TGdkFontCacheItem): integer;
begin
  Result:=ComparePointers(Font,Item.GdkFont);
end;

function CompareLogFontAndNameWithResDesc(Key: PLogFontAndName;
  Desc: TGdkFontCacheDescriptor): integer;
begin
  Result:=CompareStr(Key^.LongFontName,Desc.LongFontName);
  //writeln('CompareLogFontAndNameWithResDesc A ',Key^.LongFontName,' ',Desc.LongFontName,' ',HexStr(Cardinal(Desc),8),' Result=',Result);
  if Result=0 then
    Result:=CompareMemRange(@Key^.LogFont,@Desc.LogFont,SizeOf(Desc.LogFont));
  //writeln('CompareLogFontAndNameWithResDesc END Result=',Result);
end;

procedure TGdkFontCache.RemoveItem(Item: TResourceCacheItem);
begin
  gdk_font_unref(TGdkFontCacheItem(Item).GdkFont);
  inherited RemoveItem(Item);
end;

constructor TGdkFontCache.Create;
begin
  inherited Create;
  FResourceCacheItemClass:=TGdkFontCacheItem;
  FResourceCacheDescriptorClass:=TGdkFontCacheDescriptor;
end;

destructor TGdkFontCache.Destroy;
begin
  inherited Destroy;
end;

function TGdkFontCache.CompareItems(Tree: TAvgLvlTree; Item1, Item2: Pointer
  ): integer;
begin
  Result:=ComparePointers(TGdkFontCacheItem(Item1).GdkFont,
                          TGdkFontCacheItem(Item2).GdkFont);
end;

function TGdkFontCache.CompareDescriptors(Tree: TAvgLvlTree; Desc1,
  Desc2: Pointer): integer;
var
  Descriptor1: TGdkFontCacheDescriptor;
  Descriptor2: TGdkFontCacheDescriptor;
begin
  Descriptor1:=TGdkFontCacheDescriptor(Desc1);
  Descriptor2:=TGdkFontCacheDescriptor(Desc2);
  Result:=CompareStr(Descriptor1.LongFontName,Descriptor2.LongFontName);
  if Result<>0 then exit;
  Result:=CompareMemRange(@Descriptor1.LogFont,@Descriptor2.LogFont,
                          SizeOf(Descriptor1.LogFont));
end;

function TGdkFontCache.FindGDKFont(TheGdkFont: PGDKFont): TGdkFontCacheItem;
var
  ANode: TAvgLvlTreeNode;
begin
  ANode:=FItems.Findkey(TheGdkFont,@CompareGdkFontWithResItem);
  if ANode<>nil then
    Result:=TGdkFontCacheItem(ANode.Data)
  else
    Result:=nil;
end;

function TGdkFontCache.FindGDKFontDesc(const LogFont: TLogFont;
  const LongFontName: string): TGdkFontCacheDescriptor;
var
  LogFontAndName: TLogFontAndName;
  ANode: TAvgLvlTreeNode;
begin
  LogFontAndName.LogFont:=LogFont;
  LogFontAndName.LongFontName:=LongFontName;
  ANode:=FDescriptors.Findkey(@LogFontAndName,
                              @CompareLogFontAndNameWithResDesc);
  if ANode<>nil then
    Result:=TGdkFontCacheDescriptor(ANode.Data)
  else
    Result:=nil;
end;

function TGdkFontCache.Add(TheGdkFont: PGDKFont; const LogFont: TLogFont;
  const LongFontName: string): TGdkFontCacheDescriptor;
var
  Item: TGdkFontCacheItem;
begin
  if FindGDKFontDesc(LogFont,LongFontName)<>nil then
    RaiseGDBException('TGdkFontCache.Add font desc added twice');
    
  // find cache item with TheGdkFont
  Item:=FindGDKFont(TheGdkFont);
  if Item=nil then begin
    // create new item
    Item:=TGdkFontCacheItem.Create(Self,0);
    Item.GdkFont:=TheGdkFont;
    gdk_font_ref(TheGdkFont);
    FItems.Add(Item);
  end;
  if FindGDKFont(TheGdkFont)=nil then
    RaiseGDBException('');
    
  // create descriptor
  Result:=TGdkFontCacheDescriptor.Create(Self,Item);
  Result.LongFontName:=LongFontName;
  Result.LogFont:=LogFont;
  FDescriptors.Add(Result);
  if FindGDKFontDesc(LogFont,LongFontName)=nil then begin
    debugln('TGdkFontCache.Add Added: ',HexStr(Cardinal(Result),8),' LongFontName=',Result.LongFontName,' ',LogFontToString(Result.LogFont));
    DumpDescriptors;
    RaiseGDBException('');
  end;
end;

procedure TGdkFontCache.Reference(TheGdkFont: PGDKFont);
var
  Item: TGdkFontCacheItem;
begin
  Item:=FindGDKFont(TheGdkFont);
  if Item=nil then
    gdk_font_ref(TheGdkFont)
  else
    Item.IncreaseRefCount;
end;

procedure TGdkFontCache.Unreference(TheGdkFont: PGDKFont);
var
  Item: TGdkFontCacheItem;
begin
  Item:=FindGDKFont(TheGdkFont);
  if Item=nil then
    gdk_font_unref(TheGdkFont)
  else
    Item.DecreaseRefCount;
end;

procedure TGdkFontCache.DumpDescriptors;
var
  ANode: TAvgLvlTreeNode;
  Desc: TGdkFontCacheDescriptor;
  i: Integer;
begin
  ANode:=FDescriptors.FindLowest;
  i:=1;
  while ANode<>nil do begin
    Desc:=TGdkFontCacheDescriptor(ANode.Data);
    debugln('TGdkFontCache.DumpDescriptors ',dbgs(i),' ',HexStr(Cardinal(Desc),8),' ',Desc.LongFontName,' ',LogFontToString(Desc.LogFont));
    ANode:=FDescriptors.FindSuccessor(ANode);
    inc(i);
  end;
end;

{ TGdkFontCacheItem }

procedure TGdkFontCacheItem.WarnReferenceHigh;
begin
  inherited WarnReferenceHigh;
  debugln(' GdkFont='+HexStr(Cardinal(GdkFont),8));
  if FirstDescriptor<>nil then
    debugln('  '+TGdkFontCacheDescriptor(FirstDescriptor).LongFontName
            +' '+LogFontToString(TGdkFontCacheDescriptor(FirstDescriptor).LogFont));
end;

initialization
  FontCache:=TGdkFontCache.Create;
  
finalization
  FontCache.Free;
  FontCache:=nil;

end.

