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
  Classes, SysUtils, FPCAdds, LCLProc, LCLType, AvgLvlTree, gdk, gtkdef,
  LCLResCache;
  
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
    function CompareItems(Tree: TAvgLvlTree; Item1, Item2: Pointer): integer; override;
    function CompareDescriptors(Tree: TAvgLvlTree; Desc1, Desc2: Pointer): integer; override;
    function FindGDKFont(TheGdkFont: PGDKFont): TGdkFontCacheItem;
    function FindGDKFontDesc(const LogFont: TLogFont;
                           const LongFontName: string): TGdkFontCacheDescriptor;
    function FindADescriptor(TheGdkFont: PGDKFont): TGdkFontCacheDescriptor;
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


{ TGdkFontCache }

function CompareGdkFontWithResItem(Font: PGDKFont;
  Item: TGdkFontCacheItem): integer;
begin
  Result := ComparePointers(Font, Item.GdkFont);
end;

function CompareLogFontAndNameWithResDesc(Key: PLogFontAndName;
  Desc: TGdkFontCacheDescriptor): integer;
begin
  Result:=CompareStr(Key^.LongFontName,Desc.LongFontName);
  //debugln('CompareLogFontAndNameWithResDesc A ',Key^.LongFontName,' ',Desc.LongFontName,' ',DbgS(Desc),' Result=',Result);
  if Result=0 then
    Result:=CompareMemRange(@Key^.LogFont,@Desc.LogFont,SizeOf(Desc.LogFont));
  //debugln('CompareLogFontAndNameWithResDesc END Result=',Result);
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
  ANode:=FItems.Findkey(TheGdkFont,TListSortCompare(@CompareGdkFontWithResItem));
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
                           TListSortCompare(@CompareLogFontAndNameWithResDesc));
  if ANode<>nil then
    Result:=TGdkFontCacheDescriptor(ANode.Data)
  else
    Result:=nil;
end;

function TGdkFontCache.FindADescriptor(TheGdkFont: PGDKFont
  ): TGdkFontCacheDescriptor;
var
  Item: TGdkFontCacheItem;
begin
  Item:=FindGDKFont(TheGdkFont);
  if Item=nil then
    Result:=nil
  else
    Result:=TGdkFontCacheDescriptor(Item.FirstDescriptor);
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

  // create descriptor
  Result:=TGdkFontCacheDescriptor.Create(Self,Item);
  Result.LongFontName:=LongFontName;
  Result.LogFont:=LogFont;
  FDescriptors.Add(Result);
  if FindGDKFontDesc(LogFont,LongFontName)=nil then begin
    DebugLn('TGdkFontCache.Add Added: %p LongFontName=%s LogFont=%s', [Pointer(Result), Result.LongFontName, LogFontToString(Result.LogFont)]);
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
    DebugLn('TGdkFontCache.DumpDescriptors %d %p %s %s', [i, Pointer(Desc), Desc.LongFontName, LogFontToString(Desc.LogFont)]);
    ANode:=FDescriptors.FindSuccessor(ANode);
    inc(i);
  end;
end;

{ TGdkFontCacheItem }

procedure TGdkFontCacheItem.WarnReferenceHigh;
begin
  inherited WarnReferenceHigh;
  debugln(' GdkFont='+DbgS(GdkFont));
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

