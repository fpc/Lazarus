{ $Id: oifavouriteproperties.pas 17395 2008-11-15 03:53:22Z paul $}
{
 *****************************************************************************
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
unit OIFavouriteProperties;

{$MODE OBJFPC}{$H+}

interface

uses
  // FCL
  SysUtils, Classes,
  // LCL
  LCLProc, InterfaceBase, LazConfigStorage, PropEdits;

type
  TWidgetSetRestrictionsArray = array [TLCLPlatform] of Integer;

  { TOIFavouriteProperty
    BaseClassName }
  TOIFavouriteProperty = class
  protected
    BaseClass: TPersistentClass;
    BaseClassname: string;
    PropertyName: string;
    Include: boolean; // include or exclude
  public
    constructor Create(ABaseClass: TPersistentClass;
                       const APropertyName: string; TheInclude: boolean);
    function Constrains(AnItem: TOIFavouriteProperty): boolean;
    function IsFavourite(AClass: TPersistentClass;
                         const APropertyName: string): boolean;
    function Compare(AFavourite: TOIFavouriteProperty): integer;
    procedure SaveToConfig(ConfigStore: TConfigStorage; const Path: string);
    procedure Assign(Src: TOIFavouriteProperty); virtual;
    function CreateCopy: TOIFavouriteProperty;
    function DebugReportAsString: string;
  end;

  { TOIRestrictedProperty}
  TOIRestrictedProperty = class(TOIFavouriteProperty)
  protected
    FWidgetSets: TLCLPlatforms;
  public

    function IsRestricted(AClass: TPersistentClass;
                          const APropertyName: string): TLCLPlatforms;
    procedure CheckRestrictions(
      AClass: TClass; var ARestrictions: TWidgetSetRestrictionsArray);

    property WidgetSets: TLCLPlatforms read FWidgetSets write FWidgetSets;
  end;

  { TOIFavouriteProperties }

  TOIFavouriteProperties = class
  private
    FItems: TFPList; // list of TOIFavouriteProperty
    FModified: Boolean;
    FSorted: Boolean;
    FDoublesDeleted: Boolean;
  protected
    function GetCount: integer; virtual;
    function GetItems(Index: integer): TOIFavouriteProperty; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; virtual;
    procedure Assign(Src: TOIFavouriteProperties); virtual;
    function CreateCopy: TOIFavouriteProperties;
    function Contains(AnItem: TOIFavouriteProperty): Boolean; virtual;
    procedure Add(NewItem: TOIFavouriteProperty); virtual;
    procedure AddNew(NewItem: TOIFavouriteProperty);
    procedure Remove(AnItem: TOIFavouriteProperty); virtual;
    procedure DeleteConstraints(AnItem: TOIFavouriteProperty); virtual;
    function IsFavourite(
      AClass: TPersistentClass; const PropertyName: string): boolean;
    function AreFavourites(
      Selection: TPersistentSelectionList; const PropertyName: string): boolean;
    procedure LoadFromConfig(ConfigStore: TConfigStorage; const Path: string);
    procedure SaveToConfig(ConfigStore: TConfigStorage; const Path: string);
    procedure MergeConfig(ConfigStore: TConfigStorage; const Path: string);
    procedure SaveNewItemsToConfig(
      ConfigStore: TConfigStorage;
      const Path: string; BaseFavourites: TOIFavouriteProperties);
    procedure Sort; virtual;
    procedure DeleteDoubles; virtual;
    function IsEqual(TheFavourites: TOIFavouriteProperties): boolean;
    function GetSubtractList(FavouritesToSubtract: TOIFavouriteProperties): TList;
    procedure WriteDebugReport;
  public
    property Items[Index: integer]: TOIFavouriteProperty read GetItems; default;
    property Count: integer read GetCount;
    property Modified: Boolean read FModified write FModified;
    property Sorted: Boolean read FSorted;
    property DoublesDeleted: boolean read FDoublesDeleted;
  end;
  TOIFavouritePropertiesClass = class of TOIFavouriteProperties;

  { TOIRestrictedProperties }

  TOIRestrictedProperties = class(TOIFavouriteProperties)
  protected
    FWidgetSetRestrictions: TWidgetSetRestrictionsArray;
  public
    constructor Create;

    function IsRestricted(AClass: TPersistentClass;
                          const PropertyName: string): TLCLPlatforms;
    function AreRestricted(Selection: TPersistentSelectionList;
                           const PropertyName: string): TLCLPlatforms;
    property WidgetSetRestrictions: TWidgetSetRestrictionsArray
      read FWidgetSetRestrictions;
  end;

implementation

function CompareOIFavouriteProperties(Data1, Data2: Pointer): integer;
var
  Favourite1: TOIFavouriteProperty;
  Favourite2: TOIFavouriteProperty;
begin
  Favourite1:=TOIFavouriteProperty(Data1);
  Favourite2:=TOIFavouriteProperty(Data2);
  Result:=Favourite1.Compare(Favourite2)
end;

{ TOIFavouriteProperties }

function TOIFavouriteProperties.GetCount: integer;
begin
  Result:=FItems.Count;
end;

function TOIFavouriteProperties.GetItems(Index: integer): TOIFavouriteProperty;
begin
  Result:=TOIFavouriteProperty(FItems[Index]);
end;

constructor TOIFavouriteProperties.Create;
begin
  FItems:=TFPList.Create;
end;

destructor TOIFavouriteProperties.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TOIFavouriteProperties.Clear;
var
  i: Integer;
begin
  for i:=0 to FItems.Count-1 do
    TObject(FItems[i]).Free;
  FItems.Clear;
  FSorted:=true;
end;

procedure TOIFavouriteProperties.Assign(Src: TOIFavouriteProperties);
var
  i: Integer;
begin
  Clear;
  for i:=0 to Src.Count-1 do
    FItems.Add(Src[i].CreateCopy);
  FModified:=Src.Modified;
  FDoublesDeleted:=Src.DoublesDeleted;
  FSorted:=Src.Sorted;
end;

function TOIFavouriteProperties.CreateCopy: TOIFavouriteProperties;
begin
  Result:=TOIFavouriteProperties.Create;
  Result.Assign(Self);
end;

function TOIFavouriteProperties.Contains(AnItem: TOIFavouriteProperty
  ): Boolean;
var
  i: Integer;
begin
  for i:=Count-1 downto 0 do begin
    if Items[i].Compare(AnItem)=0 then begin
      Result:=true;
      exit;
    end;
  end;
  Result:=false;
end;

procedure TOIFavouriteProperties.Add(NewItem: TOIFavouriteProperty);
begin
  FItems.Add(NewItem);
  FSorted:=(Count<=1)
           or (FSorted and (Items[Count-1].Compare(Items[Count-2])<0));
  FDoublesDeleted:=FSorted
             and ((Count<=1) or (Items[Count-1].Compare(Items[Count-2])<>0));
  Modified:=true;
end;

procedure TOIFavouriteProperties.AddNew(NewItem: TOIFavouriteProperty);
begin
  if Contains(NewItem) then
    NewItem.Free
  else
    Add(NewItem);
end;

procedure TOIFavouriteProperties.Remove(AnItem: TOIFavouriteProperty);
begin
  Modified:=FItems.Remove(AnItem)>=0;
end;

procedure TOIFavouriteProperties.DeleteConstraints(
  AnItem: TOIFavouriteProperty);
// delete all items, that would constrain AnItem
var
  i: Integer;
  CurItem: TOIFavouriteProperty;
begin
  for i:=Count-1 downto 0 do begin
    CurItem:=Items[i];
    if CurItem.Constrains(AnItem) then begin
      FItems.Delete(i);
      Modified:=true;
      CurItem.Free;
    end;
  end;
end;

function TOIFavouriteProperties.IsFavourite(AClass: TPersistentClass;
  const PropertyName: string): boolean;
var
  i: Integer;
  CurItem: TOIFavouriteProperty;
  BestItem: TOIFavouriteProperty;
begin
  if (AClass=nil) or (PropertyName='') then begin
    Result:=false;
    exit;
  end;
  BestItem:=nil;
  for i:=0 to Count-1 do begin
    CurItem:=Items[i];
    if not CurItem.IsFavourite(AClass,PropertyName) then continue;
    if (BestItem=nil)
    or (AClass.InheritsFrom(BestItem.BaseClass)) then begin
      //debugln('TOIFavouriteProperties.IsFavourite ',AClass.ClassName,' ',PropertyName);
      BestItem:=CurItem;
    end;
  end;
  Result:=(BestItem<>nil) and BestItem.Include;
end;

function TOIFavouriteProperties.AreFavourites(
  Selection: TPersistentSelectionList; const PropertyName: string): boolean;
var
  i: Integer;
begin
  Result:=(Selection<>nil) and (Selection.Count>0);
  if not Result then exit;
  for i:=0 to Selection.Count-1 do begin
    if not IsFavourite(TPersistentClass(Selection[i].ClassType),PropertyName)
    then begin
      Result:=false;
      exit;
    end;
  end;
end;

procedure TOIFavouriteProperties.LoadFromConfig(ConfigStore: TConfigStorage;
  const Path: string);
var
  NewCount: LongInt;
  i: Integer;
  NewItem: TOIFavouriteProperty;
  p: String;
  NewPropertyName: String;
  NewInclude: Boolean;
  NewBaseClassname: String;
  NewBaseClass: TPersistentClass;
begin
  Clear;
  NewCount:=ConfigStore.GetValue(Path+'Count',0);
  for i:=0 to NewCount-1 do begin
    p:=Path+'Item'+IntToStr(i)+'/';
    NewPropertyName:=ConfigStore.GetValue(p+'PropertyName','');
    if (NewPropertyName='') or (not IsValidIdent(NewPropertyName)) then
      continue;
    NewInclude:=ConfigStore.GetValue(p+'Include',true);
    NewBaseClassname:=ConfigStore.GetValue(p+'BaseClass','');
    if (NewBaseClassname='') or (not IsValidIdent(NewBaseClassname))  then
      continue;
    NewBaseClass:=GetClass(NewBaseClassname);
    NewItem:=TOIFavouriteProperty.Create(NewBaseClass,NewPropertyName,
                                         NewInclude);
    NewItem.BaseClassName:=NewBaseClassname;
    Add(NewItem);
  end;
  {$IFDEF DebugFavouriteroperties}
  debugln('TOIFavouriteProperties.LoadFromConfig END');
  WriteDebugReport;
  {$ENDIF}
end;

procedure TOIFavouriteProperties.SaveToConfig(ConfigStore: TConfigStorage;
  const Path: string);
var
  i: Integer;
begin
  ConfigStore.SetDeleteValue(Path+'Count',Count,0);
  for i:=0 to Count-1 do
    Items[i].SaveToConfig(ConfigStore,Path+'Item'+IntToStr(i)+'/');
end;

procedure TOIFavouriteProperties.MergeConfig(ConfigStore: TConfigStorage;
  const Path: string);
var
  NewFavourites: TOIFavouriteProperties;
  OldItem: TOIFavouriteProperty;
  NewItem: TOIFavouriteProperty;
  cmp: LongInt;
  NewIndex: Integer;
  OldIndex: Integer;
begin
  NewFavourites:=TOIFavouritePropertiesClass(ClassType).Create;
  {$IFDEF DebugFavouriteroperties}
  debugln('TOIFavouriteProperties.MergeConfig ',dbgsName(NewFavourites),' ',dbgsName(NewFavourites.FItems));
  {$ENDIF}
  try
    // load config
    NewFavourites.LoadFromConfig(ConfigStore,Path);
    // sort both to see the differences
    NewFavourites.DeleteDoubles; // descending
    DeleteDoubles;               // descending
    // add all new things from NewFavourites
    NewIndex:=0;
    OldIndex:=0;
    while (NewIndex<NewFavourites.Count) do begin
      NewItem:=NewFavourites[NewIndex];
      if OldIndex>=Count then begin
        // item only exists in config -> move to this list
        NewFavourites.FItems[NewIndex]:=nil;
        inc(NewIndex);
        FItems.Insert(OldIndex,NewItem);
        inc(OldIndex);
      end else begin
        OldItem:=Items[OldIndex];
        cmp:=OldItem.Compare(NewItem);
        //debugln('TOIFavouriteProperties.MergeConfig cmp=',dbgs(cmp),' OldItem=[',OldItem.DebugReportAsString,'] NewItem=[',NewItem.DebugReportAsString,']');
        if cmp=0 then begin
          // item already exists in this list
          inc(NewIndex);
          inc(OldIndex);
        end else if cmp<0 then begin
          // item exists only in old favourites
          // -> next old
          inc(OldIndex);
        end else begin
          // item only exists in config -> move to this list
          NewFavourites.FItems[NewIndex]:=nil;
          inc(NewIndex);
          FItems.Insert(OldIndex,NewItem);
          inc(OldIndex);
        end;
      end;
    end;
  finally
    NewFavourites.Free;
  end;
  {$IFDEF DebugFavouriteroperties}
  debugln('TOIFavouriteProperties.MergeConfig END');
  WriteDebugReport;
  {$ENDIF}
end;

procedure TOIFavouriteProperties.SaveNewItemsToConfig(
  ConfigStore: TConfigStorage; const Path: string;
  BaseFavourites: TOIFavouriteProperties);
// Save all items, that are in this list and not in BaseFavourites
// It does not save, if an item in BaseFavourites is missing in this list
var
  SubtractList: TList;
  i: Integer;
  CurItem: TOIFavouriteProperty;
begin
  SubtractList:=GetSubtractList(BaseFavourites);
  try
    ConfigStore.SetDeleteValue(Path+'Count',SubtractList.Count,0);
    {$IFDEF DebugFavouriteroperties}
    debugln('TOIFavouriteProperties.SaveNewItemsToConfig A Count=',dbgs(SubtractList.Count));
    {$ENDIF}
    for i:=0 to SubtractList.Count-1 do begin
      CurItem:=TOIFavouriteProperty(SubtractList[i]);
      CurItem.SaveToConfig(ConfigStore,Path+'Item'+IntToStr(i)+'/');
      {$IFDEF DebugFavouriteroperties}
      debugln(' i=',dbgs(i),' ',CurItem.DebugReportAsString);
      {$ENDIF}
    end;
  finally
    SubtractList.Free;
  end;
end;

procedure TOIFavouriteProperties.Sort;
begin
  if FSorted then exit;
  FItems.Sort(@CompareOIFavouriteProperties);
end;

procedure TOIFavouriteProperties.DeleteDoubles;
// This also sorts
var
  i: Integer;
begin
  if FDoublesDeleted then exit;
  Sort;
  for i:=Count-1 downto 1 do begin
    if Items[i].Compare(Items[i-1])=0 then begin
      Items[i].Free;
      FItems.Delete(i);
    end;
  end;
  FDoublesDeleted:=true;
end;

function TOIFavouriteProperties.IsEqual(TheFavourites: TOIFavouriteProperties
  ): boolean;
var
  i: Integer;
begin
  Result:=false;
  DeleteDoubles;
  TheFavourites.DeleteDoubles;
  if Count<>TheFavourites.Count then exit;
  for i:=Count-1 downto 1 do
    if Items[i].Compare(TheFavourites.Items[i])<>0 then exit;
  Result:=true;
end;

function TOIFavouriteProperties.GetSubtractList(
  FavouritesToSubtract: TOIFavouriteProperties): TList;
// create a list of TOIFavouriteProperty of all items in this list
// and not in FavouritesToSubtract
var
  SelfIndex: Integer;
  SubtractIndex: Integer;
  CurItem: TOIFavouriteProperty;
  cmp: LongInt;
begin
  Result:=TList.Create;
  DeleteDoubles; // this also sorts descending
  FavouritesToSubtract.DeleteDoubles; // this also sorts descending
  SelfIndex:=0;
  SubtractIndex:=0;
  while SelfIndex<Count do begin
    CurItem:=Items[SelfIndex];
    if SubtractIndex>=FavouritesToSubtract.Count then begin
      // item does not exist in SubtractIndex -> add it
      Result.Add(CurItem);
      inc(SelfIndex);
    end else begin
      cmp:=CurItem.Compare(FavouritesToSubtract[SubtractIndex]);
      //debugln('TOIFavouriteProperties.GetSubtractList cmp=',dbgs(cmp),' CurItem=[',CurItem.DebugReportAsString,'] SubtractItem=[',FavouritesToSubtract[SubtractIndex].DebugReportAsString,']');
      if cmp=0 then begin
        // item exists in SubtractIndex -> skip
        inc(SubtractIndex);
        inc(SelfIndex);
      end else if cmp>0 then begin
        // item does not exist in FavouritesToSubtract -> add it
        Result.Add(CurItem);
        inc(SelfIndex);
      end else begin
        // item exists only in FavouritesToSubtract -> skip
        inc(SubtractIndex);
      end;
    end;
  end;
end;

procedure TOIFavouriteProperties.WriteDebugReport;
var
  i: Integer;
begin
  debugln('TOIFavouriteProperties.WriteDebugReport Count=',dbgs(Count));
  for i:=0 to Count-1 do
    debugln('  i=',dbgs(i),' ',Items[i].DebugReportAsString);
end;

{ TOIFavouriteProperty }

constructor TOIFavouriteProperty.Create(ABaseClass: TPersistentClass;
  const APropertyName: string; TheInclude: boolean);
begin
  BaseClass:=ABaseClass;
  PropertyName:=APropertyName;
  Include:=TheInclude;
end;

function TOIFavouriteProperty.Constrains(AnItem: TOIFavouriteProperty
  ): boolean;
// true if this item constrains AnItem
// This item constrains AnItem, if this is the opposite (Include) and
// AnItem has the same or greater scope
begin
  Result:=(Include<>AnItem.Include)
          and (CompareText(PropertyName,AnItem.PropertyName)=0)
          and (BaseClass.InheritsFrom(AnItem.BaseClass));
end;

function TOIFavouriteProperty.IsFavourite(AClass: TPersistentClass;
  const APropertyName: string): boolean;
begin
  Result:=(CompareText(PropertyName,APropertyName)=0)
          and (AClass.InheritsFrom(BaseClass));
end;

function TOIFavouriteProperty.Compare(AFavourite: TOIFavouriteProperty
  ): integer;

  function CompareBaseClass: integer;
  begin
    if BaseClass<>nil then begin
      if AFavourite.BaseClass<>nil then
        Result:=ComparePointers(BaseClass,AFavourite.BaseClass)
      else
        Result:=CompareText(BaseClass.ClassName,AFavourite.BaseClassName);
    end else begin
      if AFavourite.BaseClass<>nil then
        Result:=CompareText(BaseClassName,AFavourite.BaseClass.ClassName)
      else
        Result:=CompareText(BaseClassName,AFavourite.BaseClassName);
    end;
  end;

begin
  // first compare PropertyName
  Result:=CompareText(PropertyName,AFavourite.PropertyName);
  if Result<>0 then exit;
  // then compare Include
  if Include<>AFavourite.Include then begin
    if Include then
      Result:=1
    else
      Result:=-1;
    exit;
  end;
  // then compare BaseClass and BaseClassName
  Result:=CompareBaseClass;
end;

procedure TOIFavouriteProperty.SaveToConfig(ConfigStore: TConfigStorage;
  const Path: string);
begin
  if BaseClass<>nil then
    ConfigStore.SetDeleteValue(Path+'BaseClass',BaseClass.ClassName,'')
  else
    ConfigStore.SetDeleteValue(Path+'BaseClass',BaseClassName,'');
  ConfigStore.SetDeleteValue(Path+'PropertyName',PropertyName,'');
  ConfigStore.SetDeleteValue(Path+'Include',Include,true);
end;

procedure TOIFavouriteProperty.Assign(Src: TOIFavouriteProperty);
begin
  BaseClassName:=Src.BaseClassName;
  BaseClass:=Src.BaseClass;
  PropertyName:=Src.PropertyName;
  Include:=Src.Include;
end;

function TOIFavouriteProperty.CreateCopy: TOIFavouriteProperty;
begin
  Result:=TOIFavouriteProperty.Create(BaseClass,PropertyName,Include);
  Result.BaseClass:=BaseClass;
end;

function TOIFavouriteProperty.DebugReportAsString: string;
begin
  Result:='PropertyName="'+PropertyName+'"'
      +' Include='+dbgs(Include)
      +' BaseClassName="'+BaseClassName+'"'
      +' BaseClass='+dbgsName(BaseClass);
end;

{ TOIRestrictedProperty }

procedure TOIRestrictedProperty.CheckRestrictions(
  AClass: TClass; var ARestrictions: TWidgetSetRestrictionsArray);
var
  lclPlatform: TLCLPlatform;
begin
  if AClass.InheritsFrom(BaseClass) and (PropertyName = '') then
    for lclPlatform := Low(TLCLPlatform) to High(TLCLPlatform) do
      if lclPlatform in WidgetSets then
        Inc(ARestrictions[lclPlatform]);
end;

function TOIRestrictedProperty.IsRestricted(AClass: TPersistentClass;
  const APropertyName: string): TLCLPlatforms;
begin
  //DebugLn('IsRestricted ', AClass.ClassName, ' ?= ', BaseClass.ClassName, ' ', APropertyName, ' ?= ', PropertyName);
  Result := [];
  if (CompareText(PropertyName,APropertyName) = 0)
    and (AClass.InheritsFrom(BaseClass)) then Result := WidgetSets;
end;


{ TOIRestrictedProperties }

constructor TOIRestrictedProperties.Create;
var
  P: TLCLPlatform;
begin
  inherited Create;

  for P := Low(TLCLPlatform) to High(TLCLPlatform) do
    FWidgetSetRestrictions[P] := 0;
end;

function TOIRestrictedProperties.IsRestricted(AClass: TPersistentClass;
  const PropertyName: string): TLCLPlatforms;
var
  I: Integer;
  CurItem: TOIRestrictedProperty;
begin
  Result := [];
  if (AClass=nil) or (PropertyName='') then Exit;
  for I := 0 to Count - 1 do
  begin
    if not (Items[I] is TOIRestrictedProperty) then Continue;
    CurItem:=Items[I] as TOIRestrictedProperty;
    Result := Result + CurItem.IsRestricted(AClass,PropertyName);
  end;
end;

function TOIRestrictedProperties.AreRestricted(
  Selection: TPersistentSelectionList;
  const PropertyName: string): TLCLPlatforms;
var
  I: Integer;
begin
  Result := [];
  if Selection = nil then Exit;
  for i:=0 to Selection.Count-1 do
  begin
    Result := Result + IsRestricted(TPersistentClass(Selection[i].ClassType), PropertyName);
  end;
end;

end.

