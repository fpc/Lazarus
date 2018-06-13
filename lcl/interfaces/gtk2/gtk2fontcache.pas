{
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit Gtk2FontCache;

{$mode objfpc}{$H+}

interface

uses
  // RTL
  Classes, SysUtils, glib2, pango, Laz_AVL_Tree,
  // LazUtils
  LazLoggerBase, LazUtilities,
  // LCL
  LCLProc, LCLType, Gtk2Def, LCLResCache;
  
type
  TGtkFontCacheDescriptor = class;

  { TGtkFontCacheItem }
  
  TGtkFontCacheItem = class(TResourceCacheItem)
  public
    GtkFont: TGtkIntfFont;

    // metrics
    MetricsValid: boolean;
    lBearing: LongInt;
    rBearing: LongInt;
    TextMetric: TTextMetric;
    IsDoubleByteChar: boolean;
    IsMonoSpace: boolean;
    procedure WarnReferenceHigh; override;
  end;
  
  
  { TGtkFontCacheDescriptor }
  
  TGtkFontCacheDescriptor = class(TResourceCacheDescriptor)
  public
    LogFont: TLogFont;
    LongFontName: string;
    PangoFontDescription: PPangoFontDescription;
    destructor Destroy; override;
  end;
  
  
  { TGtkFontCache
    Notes:
    Each font can be used by several Device Contexts.
    Each font can have several font descriptors.
    A font descriptor has one font.
    }
  
  TGtkFontCache = class(TResourceCache)
  protected
    procedure RemoveItem(Item: TResourceCacheItem); override;
  public
    constructor Create;
    function CompareItems({%H-}Tree: TAvlTree; Item1, Item2: Pointer): integer; override;
    function CompareDescriptors({%H-}Tree: TAvlTree; Desc1, Desc2: Pointer): integer; override;
    function FindGtkFont(TheGtkFont: TGtkIntfFont): TGtkFontCacheItem;
    function FindGtkFontDesc(const LogFont: TLogFont;
                           const LongFontName: string): TGtkFontCacheDescriptor;
    function FindADescriptor(TheGtkFont: TGtkIntfFont): TGtkFontCacheDescriptor;
    function Add(TheGtkFont: TGtkIntfFont; const LogFont: TLogFont;
                 const LongFontName: string): TGtkFontCacheDescriptor;
    function AddWithoutName(TheGtkFont: TGtkIntfFont): TGtkFontCacheDescriptor;
    procedure Reference(TheGtkFont: TGtkIntfFont);
    procedure Unreference(TheGtkFont: TGtkIntfFont);
    procedure DumpDescriptors;
  end;

function LogFontToString(const LogFont: TLogFont): string;

procedure ReferenceGtkIntfFont(AFont: TGtkIntfFont);
procedure UnreferenceGtkIntfFont(AFont: TGtkIntfFont);
  
var
  FontCache: TGtkFontCache;

implementation

type
  TLogFontAndName = record
    LogFont: TLogFont;
    LongFontName: string;
  end;
  PLogFontAndName = ^TLogFontAndName;

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
    Result:=Result+HexStr(ord(PChar(@LogFont)[i]),2);
  Result:=Result+#13#10;
end;

procedure ReferenceGtkIntfFont(AFont: TGtkIntfFont);
begin
  //DebugLn(['ReferenceGtkIntfFont ',dbgs(AFont)]);
  g_object_ref(AFont);
end;

procedure UnreferenceGtkIntfFont(AFont: TGtkIntfFont);
begin
  //DebugLn(['UnreferenceGtkIntfFont ',dbgs(AFont)]);
  g_object_unref(AFont);
end;

{ TGtkFontCache }

function CompareGtkFontWithResItem(Font: TGtkIntfFont;
  Item: TGtkFontCacheItem): integer;
begin
  Result := ComparePointers(Font, Item.GtkFont);
end;

function CompareLogFontAndNameWithResDesc(Key: PLogFontAndName;
  Desc: TGtkFontCacheDescriptor): integer;
begin
  Result:=CompareStr(Key^.LongFontName,Desc.LongFontName);
  //debugln('CompareLogFontAndNameWithResDesc A ',Key^.LongFontName,' ',Desc.LongFontName,' ',DbgS(Desc),' Result=',Result);
  if Result=0 then
    Result:=CompareMemRange(@Key^.LogFont,@Desc.LogFont,SizeOf(Desc.LogFont));
  //debugln('CompareLogFontAndNameWithResDesc END Result=',Result);
end;

procedure TGtkFontCache.RemoveItem(Item: TResourceCacheItem);
begin
  UnreferenceGtkIntfFont(TGtkFontCacheItem(Item).GtkFont);
  inherited RemoveItem(Item);
end;

constructor TGtkFontCache.Create;
begin
  inherited Create;
  FResourceCacheItemClass:=TGtkFontCacheItem;
  FResourceCacheDescriptorClass:=TGtkFontCacheDescriptor;
end;

function TGtkFontCache.CompareItems(Tree: TAvlTree; Item1, Item2: Pointer): integer;
begin
  Result:=ComparePointers(TGtkFontCacheItem(Item1).GtkFont,
                          TGtkFontCacheItem(Item2).GtkFont);
end;

function TGtkFontCache.CompareDescriptors(Tree: TAvlTree; Desc1, Desc2: Pointer): integer;
var
  Descriptor1: TGtkFontCacheDescriptor;
  Descriptor2: TGtkFontCacheDescriptor;
begin
  Descriptor1:=TGtkFontCacheDescriptor(Desc1);
  Descriptor2:=TGtkFontCacheDescriptor(Desc2);
  Result:=CompareStr(Descriptor1.LongFontName,Descriptor2.LongFontName);
  if Result<>0 then exit;
  Result:=CompareMemRange(@Descriptor1.LogFont,@Descriptor2.LogFont,
                          SizeOf(Descriptor1.LogFont));
end;

function TGtkFontCache.FindGtkFont(TheGtkFont: TGtkIntfFont): TGtkFontCacheItem;
var
  ANode: TAvlTreeNode;
begin
  ANode:=FItems.Findkey(TheGtkFont,TListSortCompare(@CompareGtkFontWithResItem));
  if ANode<>nil then
    Result:=TGtkFontCacheItem(ANode.Data)
  else
    Result:=nil;
end;

function TGtkFontCache.FindGtkFontDesc(const LogFont: TLogFont;
  const LongFontName: string): TGtkFontCacheDescriptor;
var
  LogFontAndName: TLogFontAndName;
  ANode: TAvlTreeNode;
begin
  LogFontAndName.LogFont:=LogFont;
  LogFontAndName.LongFontName:=LongFontName;
  ANode:=FDescriptors.Findkey(@LogFontAndName,
                           TListSortCompare(@CompareLogFontAndNameWithResDesc));
  if ANode<>nil then
    Result:=TGtkFontCacheDescriptor(ANode.Data)
  else
    Result:=nil;
end;

function TGtkFontCache.FindADescriptor(TheGtkFont: TGtkIntfFont): TGtkFontCacheDescriptor;
var
  Item: TGtkFontCacheItem;
begin
  Item:=FindGtkFont(TheGtkFont);
  if Item=nil then
    Result:=nil
  else
    Result:=TGtkFontCacheDescriptor(Item.FirstDescriptor);
end;

function TGtkFontCache.Add(TheGtkFont: TGtkIntfFont; const LogFont: TLogFont;
  const LongFontName: string): TGtkFontCacheDescriptor;
var
  Item: TGtkFontCacheItem;
begin
  if TheGtkFont=nil then
    RaiseGDBException('TGtkFontCache.Add TheGtkFont=nil');
  if FindGtkFontDesc(LogFont,LongFontName)<>nil then
    RaiseGDBException('TGtkFontCache.Add font desc added twice');
    
  // find cache item with TheGtkFont
  Item:=FindGtkFont(TheGtkFont);
  if Item=nil then begin
    // create new item
    Item:=TGtkFontCacheItem.Create(Self,0);
    Item.GtkFont:=TheGtkFont;
    ReferenceGtkIntfFont(TheGtkFont);
    FItems.Add(Item);
  end;

  // create descriptor
  Result:=TGtkFontCacheDescriptor.Create(Self,Item);
  Result.LongFontName:=LongFontName;
  Result.LogFont:=LogFont;
  FDescriptors.Add(Result);
  if FindGtkFontDesc(LogFont,LongFontName)=nil then begin
    DebugLn('TGtkFontCache.Add Added: %p LongFontName=%s LogFont=%s', [Pointer(Result), Result.LongFontName, LogFontToString(Result.LogFont)]);
    DumpDescriptors;
    RaiseGDBException('');
  end;
end;

function TGtkFontCache.AddWithoutName(TheGtkFont: TGtkIntfFont): TGtkFontCacheDescriptor;
var
  LogFont: TLogFont;
  LongFontName: string;
begin
  FillChar(LogFont{%H-},SizeOf(LogFont),0);
  LongFontName:=dbghex({%H-}PtrUInt(TheGtkFont));
  Result:=Add(TheGtkFont,LogFont,LongFontName);
end;

procedure TGtkFontCache.Reference(TheGtkFont: TGtkIntfFont);
var
  Item: TGtkFontCacheItem;
begin
  Item:=FindGtkFont(TheGtkFont);
  if Item=nil then
    ReferenceGtkIntfFont(TheGtkFont)
  else
    Item.IncreaseRefCount;
end;

procedure TGtkFontCache.Unreference(TheGtkFont: TGtkIntfFont);
var
  Item: TGtkFontCacheItem;
begin
  Item:=FindGtkFont(TheGtkFont);
  if Item=nil then
    UnreferenceGtkIntfFont(TheGtkFont)
  else
    Item.DecreaseRefCount;
end;

procedure TGtkFontCache.DumpDescriptors;
var
  ANode: TAvlTreeNode;
  Desc: TGtkFontCacheDescriptor;
  i: Integer;
begin
  ANode:=FDescriptors.FindLowest;
  i:=1;
  while ANode<>nil do begin
    Desc:=TGtkFontCacheDescriptor(ANode.Data);
    DebugLn('TGtkFontCache.DumpDescriptors %d %p %s %s', [i, Pointer(Desc), Desc.LongFontName, LogFontToString(Desc.LogFont)]);
    ANode:=FDescriptors.FindSuccessor(ANode);
    inc(i);
  end;
end;

{ TGtkFontCacheItem }

procedure TGtkFontCacheItem.WarnReferenceHigh;
begin
  inherited WarnReferenceHigh;
  debugln(' GtkFont='+DbgS(GtkFont));
  if FirstDescriptor<>nil then
    debugln('  '+TGtkFontCacheDescriptor(FirstDescriptor).LongFontName
            +' '+LogFontToString(TGtkFontCacheDescriptor(FirstDescriptor).LogFont));
end;

{ TGtkFontCacheDescriptor }

destructor TGtkFontCacheDescriptor.Destroy;
begin
  if PangoFontDescription<>nil then begin
    pango_font_description_free(PangoFontDescription);
    PangoFontDescription:=nil;
  end;
  inherited Destroy;
end;

initialization
  FontCache:=TGtkFontCache.Create;
  
finalization
  FontCache.Free;
  FontCache:=nil;

end.
