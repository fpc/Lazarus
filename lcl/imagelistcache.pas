{
 /***************************************************************************
                               ImageListCache.pp
                               ----------------
                   Initial Revision  : Sun Nov 18 00:04:00 GMT+07 2007


 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

unit ImageListCache;

{$mode objfpc}{$H+}
{ $DEFINE VerboseImageListCache}

interface

uses
  Classes, SysUtils, Graphics, ImgList, LCLProc, Forms;

type
  // interface that cache user should have to listen for cache changes
  IImageCacheListener = interface
    procedure CacheSetImageList(AImageList: TCustomImageList);
    procedure CacheSetImageIndex(AIndex, AImageIndex: Integer);
  end;

  // cache item
  TImageCacheItem = record
    FImageList: TCustomImageList;    // link to imagelist
    FListener: IImageCacheListener;  // link to listener
    FImageIndexes: array of Integer; // indexes of imagelist that listener reserved
  end;
  PImageCacheItem = ^TImageCacheItem;
  
  { TImageCacheItems }

  TImageCacheItems = class(TList)
  private
    function GetItem(AIndex: Integer): PImageCacheItem;
    function GetItemForListener(AListener: IImageCacheListener): PImageCacheItem;
    procedure SetItem(AIndex: Integer; const AValue: PImageCacheItem);
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    function GetNew: PImageCacheItem;
    property Items[AIndex: Integer]: PImageCacheItem read GetItem write SetItem; default;
  end;

  { TImageListCache }

  TImageListCache = class
  private
    FItems: TImageCacheItems;
    FImages: TList;
    FListeners: TInterfaceList;
    FObsoletedCount: Integer;
    procedure CheckRebuildNeed;
    function GetImageListFor(AWidth, AHeight: Integer): TCustomImageList;

    procedure UnregisterBitmaps(AListener: IImageCacheListener);
  public
    constructor Create;
    destructor Destroy; override;
    
    function RegisterListener(AListener: IImageCacheListener): Integer;
    procedure UnregisterListener(AListener: IImageCacheListener);
    procedure RegisterBitmap(AListener: IImageCacheListener; ABitmap: TBitmap; ABitmapCount: Integer = 1);
    procedure Rebuild;
  end;
  
  function GetImageListCache: TImageListCache;
  
implementation

const
  // number of cache changes that can happen w/o rebuild
{$IFDEF VerboseImageListCache}
  ImageListCacheRebuildThreshold = 1;
{$ELSE}
  ImageListCacheRebuildThreshold = 20;
{$ENDIF}

var
  FImageListCache: TImageListCache = nil;
  
function GetImageListCache: TImageListCache;
begin
  if FImageListCache = nil then
    FImageListCache := TImageListCache.Create;
  Result := FImageListCache;
end;


{ TImageListCache }

procedure TImageListCache.CheckRebuildNeed;
begin
  if (FObsoletedCount >= ImageListCacheRebuildThreshold) and not Application.Terminated then
    Rebuild;
end;

function TImageListCache.GetImageListFor(AWidth, AHeight: Integer): TCustomImageList;
var
  i: integer;
begin
  for i := 0 to FImages.Count - 1 do
    if (TCustomImageList(FImages[i]).Height = AHeight) and
       (TCustomImageList(FImages[i]).Width = AWidth) then
    begin
      Result := TCustomImageList(FImages[i]);
      exit;
    end;
  Result := TCustomImageList.Create(nil);
  FImages.Add(Result);
  with Result do
  begin
    Width := AWidth;
    Height := AHeight;
{$IFDEF VerboseImageListCache}
    debugln('Creating new imagelist in cache for Width=',Width,' Height=', Height, ' Count = ', FImages.Count);
    if (Width <> 16) and (Width <> 24) then
      DumpStack;
{$ENDIF}
  end;
end;

procedure TImageListCache.UnregisterBitmaps(AListener: IImageCacheListener);
var
  Item: PImageCacheItem;
begin
  Item := FItems.GetItemForListener(AListener);

  if (Item <> nil) then
  begin
    Item^.FListener := nil;
    inc(FObsoletedCount, Length(Item^.FImageIndexes));
  end;
  CheckRebuildNeed;
end;

constructor TImageListCache.Create;
begin
  FObsoletedCount := 0;
  FItems := TImageCacheItems.Create;
  FImages := TList.Create;
  FListeners := TInterfaceList.Create;
end;

destructor TImageListCache.Destroy;
var
  i: integer;
begin
  FItems.Free;
  for i := 0 to FImages.Count - 1 do
    TObject(FImages[i]).Free;
  FImages.Free;
  FListeners.Free;
  inherited Destroy;
end;

function TImageListCache.RegisterListener(AListener: IImageCacheListener): Integer;
begin
  Result := FListeners.IndexOf(AListener);
  if Result = -1 then
    Result := FListeners.Add(AListener);
end;

procedure TImageListCache.UnregisterListener(AListener: IImageCacheListener);
var
  Index: Integer;
begin
  Index := FListeners.IndexOf(AListener);
  if Index <> -1 then
  begin
    UnregisterBitmaps(AListener);
    FListeners.Remove(AListener);
  end;
  if FListeners.Count = 0 then
  begin
    FImageListCache := nil;
    Free;
  end;
end;

procedure TImageListCache.RegisterBitmap(AListener: IImageCacheListener; ABitmap: TBitmap; ABitmapCount: Integer = 1);
var
  i, AStart, OldLen: Integer;
  Item: PImageCacheItem;
  OldOnChange: TNotifyEvent;
begin
  OldOnChange := ABitmap.OnChange;
  ABitmap.OnChange := nil; // prevent further updates

  try
    RegisterListener(AListener);
    Item := FItems.GetItemForListener(AListener);
    if Item = nil then
    begin
      Item := FItems.GetNew;
      Item^.FImageList := GetImageListFor(ABitmap.Width div ABitmapCount, ABitmap.Height);
      Item^.FListener := AListener;
    end;

    AStart := Item^.FImageList.Add(ABitmap, nil);
    AListener.CacheSetImageList(Item^.FImageList);
    OldLen := Length(Item^.FImageIndexes);
    SetLength(Item^.FImageIndexes, OldLen + Item^.FImageList.Count - AStart);

    for i := AStart to Item^.FImageList.Count - 1 do
    begin
      Item^.FImageIndexes[OldLen + i - AStart] := i;
      AListener.CacheSetImageIndex(OldLen + i - AStart, i);
    end;
  finally
    ABitmap.OnChange := OldOnChange;
  end;
end;

// cache rebuild
procedure TImageListCache.Rebuild;
var
  i, j, k, ACount: integer;
  AListener: IImageCacheListener;
  ADeleted: TBits;
  AChanged: Boolean;
  AIndexes: array of Integer;
  AUpdates: TList;
begin
  // 1. check what items to be deleted (their listerners are not assigned)
  // 2. delete no more needed images from imagelists
  // 3. notify listeners about new image indexes
  
  // traverse all ImageLists
  for i := 0 to FImages.Count - 1 do
  begin
    ACount := TCustomImageList(FImages[i]).Count;
    ADeleted := TBits.Create(ACount);
    AChanged := False;
    AUpdates := TList.Create;
    // traverse for all items
    // if item is to be deleted then set flag in ADeleted, else add item to AUpdates array
    for j := FItems.Count - 1 downto 0 do
      if FItems[j]^.FImageList = TCustomImageList(FImages[i]) then
      begin
        for k := 0 to High(FItems[j]^.FImageIndexes) do
          ADeleted.Bits[FItems[j]^.FImageIndexes[k]] := FItems[j]^.FListener = nil;
        if FItems[j]^.FListener = nil then
        begin
          FItems.Delete(j);
          AChanged := True;
        end
        else
          AUpdates.Add(FItems[j]);
      end;
    // is something has been deleted from current imagelist then
    // we continue processing
    if AChanged then
    begin
      // AIndexes is our old=>new image indexes map
      // at first step we set old=old and at same moment clearing our imagelist
      SetLength(AIndexes, ACount);
      for j := High(AIndexes) downto 0 do
      begin
        AIndexes[j] := j;
        if ADeleted[j] then
          TCustomImageList(FImages[i]).Delete(j);
      end;
      // we traversing our indexes map and set new values for old values
      for j := 0 to High(AIndexes) do
        if ADeleted[j] then
        begin
          for k := j + 1 to High(AIndexes) do
            dec(AIndexes[k]);
        end;
      // all preparation done - we have old=>new map
      // process all Items that needs to be updated
      for j := 0 to AUpdates.Count - 1 do
      begin
        AListener := PImageCacheItem(AUpdates[j])^.FListener;
        for k := 0 to High(PImageCacheItem(AUpdates[j])^.FImageIndexes) do
        begin
          // update cache item and notify listener
          PImageCacheItem(AUpdates[j])^.FImageIndexes[k] := AIndexes[PImageCacheItem(AUpdates[j])^.FImageIndexes[k]];
          AListener.CacheSetImageIndex(k, PImageCacheItem(AUpdates[j])^.FImageIndexes[k]);
        end;
      end;
    end;
    AUpdates.Free;
    ADeleted.Free;
    SetLength(AIndexes, 0);
  end;

  FObsoletedCount := 0;
end;

{ TImageCacheItems }

function TImageCacheItems.GetItem(AIndex: Integer): PImageCacheItem;
begin
  Result := inherited Get(AIndex)
end;

procedure TImageCacheItems.SetItem(AIndex: Integer;
  const AValue: PImageCacheItem);
begin
  inherited Put(AIndex, AValue);
end;

procedure TImageCacheItems.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if (Action = lnDeleted) and (Ptr <> nil) then
    Dispose(PImageCacheItem(Ptr));
end;

function TImageCacheItems.GetNew: PImageCacheItem;
begin
  New(Result);
  Add(Result);
end;

function TImageCacheItems.GetItemForListener(AListener: IImageCacheListener): PImageCacheItem;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i]^.FListener = AListener then
    begin
      Result := Items[i];
      break;
    end;
end;

end.


