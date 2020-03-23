{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit OIFavoriteProperties;

{$MODE OBJFPC}{$H+}

interface

uses
  // FCL
  Classes, SysUtils,
  // LCL
  LCLPlatformDef,
  // LazUtils
  LazUtilities, LazConfigStorage, LazLoggerBase,
  // IdeIntf
  PropEdits;

type
  TWidgetSetRestrictionsArray = array [TLCLPlatform] of Integer;

  { TOIFavoriteProperty
    BaseClassName }
  TOIFavoriteProperty = class
  protected
    BaseClass: TPersistentClass;
    BaseClassname: string;
    PropertyName: string;
    Include: boolean; // include or exclude
  public
    constructor Create(ABaseClass: TPersistentClass;
                       const APropertyName: string; TheInclude: boolean);
    function Constrains(AnItem: TOIFavoriteProperty): boolean;
    function IsFavorite(AClass: TPersistentClass;
                         const APropertyName: string): boolean;
    function Compare(AFavorite: TOIFavoriteProperty): integer;
    procedure SaveToConfig(ConfigStore: TConfigStorage; const Path: string);
    procedure Assign(Src: TOIFavoriteProperty); virtual;
    function CreateCopy: TOIFavoriteProperty;
    function DebugReportAsString: string;
  end;

  { TOIRestrictedProperty}
  TOIRestrictedProperty = class(TOIFavoriteProperty)
  protected
    FWidgetSets: TLCLPlatforms;
  public
    function IsRestricted(AClass: TPersistentClass;
                          const APropertyName: string): TLCLPlatforms;
    procedure CheckRestrictions(AClass: TClass;
      var ARestrictions: TWidgetSetRestrictionsArray);// check restrictions for non properties

    property WidgetSets: TLCLPlatforms read FWidgetSets write FWidgetSets;
  end;

  { TOIFavoriteProperties }

  TOIFavoriteProperties = class
  private
    FItems: TFPList; // list of TOIFavoriteProperty
    FModified: Boolean;
    FSorted: Boolean;
    FDoublesDeleted: Boolean;
  protected
    function GetCount: integer; virtual;
    function GetItems(Index: integer): TOIFavoriteProperty; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; virtual;
    procedure Assign(Src: TOIFavoriteProperties); virtual;
    function CreateCopy: TOIFavoriteProperties;
    function Contains(AnItem: TOIFavoriteProperty): Boolean; virtual;
    procedure Add(NewItem: TOIFavoriteProperty); virtual;
    procedure AddNew(NewItem: TOIFavoriteProperty);
    procedure Remove(AnItem: TOIFavoriteProperty); virtual;
    procedure DeleteConstraints(AnItem: TOIFavoriteProperty); virtual;
    function IsFavorite(
      AClass: TPersistentClass; const PropertyName: string): boolean;
    function AreFavorites(
      Selection: TPersistentSelectionList; const PropertyName: string): boolean;
    procedure LoadFromConfig(ConfigStore: TConfigStorage; const Path: string);
    procedure SaveToConfig(ConfigStore: TConfigStorage; const Path: string);
    procedure MergeConfig(ConfigStore: TConfigStorage; const Path: string);
    procedure SaveNewItemsToConfig(
      ConfigStore: TConfigStorage;
      const Path: string; BaseFavorites: TOIFavoriteProperties);
    procedure Sort; virtual;
    procedure DeleteDoubles; virtual;
    function IsEqual(TheFavorites: TOIFavoriteProperties): boolean;
    function GetSubtractList(FavoritesToSubtract: TOIFavoriteProperties): TList;
    procedure WriteDebugReport;
  public
    property Items[Index: integer]: TOIFavoriteProperty read GetItems; default;
    property Count: integer read GetCount;
    property Modified: Boolean read FModified write FModified;
    property Sorted: Boolean read FSorted;
    property DoublesDeleted: boolean read FDoublesDeleted;
  end;
  TOIFavoritePropertiesClass = class of TOIFavoriteProperties;

  { TOIRestrictedProperties }

  TOIRestrictedProperties = class(TOIFavoriteProperties)
  public
    WidgetSetRestrictions: TWidgetSetRestrictionsArray;
    constructor Create;

    function IsRestricted(AClass: TPersistentClass;
                          const PropertyName: string): TLCLPlatforms;
    function AreRestricted(Selection: TPersistentSelectionList;
                           const PropertyName: string): TLCLPlatforms;
  end;

implementation

function CompareOIFavoriteProperties(Data1, Data2: Pointer): integer;
var
  Favorite1: TOIFavoriteProperty;
  Favorite2: TOIFavoriteProperty;
begin
  Favorite1:=TOIFavoriteProperty(Data1);
  Favorite2:=TOIFavoriteProperty(Data2);
  Result:=Favorite1.Compare(Favorite2)
end;

{ TOIFavoriteProperties }

function TOIFavoriteProperties.GetCount: integer;
begin
  Result:=FItems.Count;
end;

function TOIFavoriteProperties.GetItems(Index: integer): TOIFavoriteProperty;
begin
  Result:=TOIFavoriteProperty(FItems[Index]);
end;

constructor TOIFavoriteProperties.Create;
begin
  FItems:=TFPList.Create;
end;

destructor TOIFavoriteProperties.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TOIFavoriteProperties.Clear;
var
  i: Integer;
begin
  for i:=0 to FItems.Count-1 do
    TObject(FItems[i]).Free;
  FItems.Clear;
  FSorted:=true;
end;

procedure TOIFavoriteProperties.Assign(Src: TOIFavoriteProperties);
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

function TOIFavoriteProperties.CreateCopy: TOIFavoriteProperties;
begin
  Result:=TOIFavoriteProperties.Create;
  Result.Assign(Self);
end;

function TOIFavoriteProperties.Contains(AnItem: TOIFavoriteProperty
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

procedure TOIFavoriteProperties.Add(NewItem: TOIFavoriteProperty);
begin
  FItems.Add(NewItem);
  FSorted:=(Count<=1)
           or (FSorted and (Items[Count-1].Compare(Items[Count-2])<0));
  FDoublesDeleted:=FSorted
             and ((Count<=1) or (Items[Count-1].Compare(Items[Count-2])<>0));
  Modified:=true;
end;

procedure TOIFavoriteProperties.AddNew(NewItem: TOIFavoriteProperty);
begin
  if Contains(NewItem) then
    NewItem.Free
  else
    Add(NewItem);
end;

procedure TOIFavoriteProperties.Remove(AnItem: TOIFavoriteProperty);
begin
  Modified:=FItems.Remove(AnItem)>=0;
end;

procedure TOIFavoriteProperties.DeleteConstraints(
  AnItem: TOIFavoriteProperty);
// delete all items, that would constrain AnItem
var
  i: Integer;
  CurItem: TOIFavoriteProperty;
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

function TOIFavoriteProperties.IsFavorite(AClass: TPersistentClass;
  const PropertyName: string): boolean;
var
  i: Integer;
  CurItem: TOIFavoriteProperty;
  BestItem: TOIFavoriteProperty;
begin
  if (AClass=nil) or (PropertyName='') then begin
    Result:=false;
    exit;
  end;
  BestItem:=nil;
  for i:=0 to Count-1 do begin
    CurItem:=Items[i];
    if not CurItem.IsFavorite(AClass,PropertyName) then continue;
    if (BestItem=nil)
    or (AClass.InheritsFrom(BestItem.BaseClass)) then begin
      //debugln('TOIFavoriteProperties.IsFavorite ',AClass.ClassName,' ',PropertyName);
      BestItem:=CurItem;
    end;
  end;
  Result:=(BestItem<>nil) and BestItem.Include;
end;

function TOIFavoriteProperties.AreFavorites(
  Selection: TPersistentSelectionList; const PropertyName: string): boolean;
var
  i: Integer;
begin
  Result:=(Selection<>nil) and (Selection.Count>0);
  if not Result then exit;
  for i:=0 to Selection.Count-1 do begin
    if not IsFavorite(TPersistentClass(Selection[i].ClassType),PropertyName)
    then begin
      Result:=false;
      exit;
    end;
  end;
end;

procedure TOIFavoriteProperties.LoadFromConfig(ConfigStore: TConfigStorage;
  const Path: string);
var
  NewCount: LongInt;
  i: Integer;
  NewItem: TOIFavoriteProperty;
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
    if not IsValidIdent(NewPropertyName) then
      continue;
    NewInclude:=ConfigStore.GetValue(p+'Include',true);
    NewBaseClassname:=ConfigStore.GetValue(p+'BaseClass','');
    if not IsValidIdent(NewBaseClassname) then
      continue;
    NewBaseClass:=GetClass(NewBaseClassname);
    NewItem:=TOIFavoriteProperty.Create(NewBaseClass,NewPropertyName,
                                         NewInclude);
    NewItem.BaseClassName:=NewBaseClassname;
    Add(NewItem);
  end;
  {$IFDEF DebugFavoriteroperties}
  debugln('TOIFavoriteProperties.LoadFromConfig END');
  WriteDebugReport;
  {$ENDIF}
end;

procedure TOIFavoriteProperties.SaveToConfig(ConfigStore: TConfigStorage;
  const Path: string);
var
  i: Integer;
begin
  ConfigStore.SetDeleteValue(Path+'Count',Count,0);
  for i:=0 to Count-1 do
    Items[i].SaveToConfig(ConfigStore,Path+'Item'+IntToStr(i)+'/');
end;

procedure TOIFavoriteProperties.MergeConfig(ConfigStore: TConfigStorage;
  const Path: string);
var
  NewFavorites: TOIFavoriteProperties;
  OldItem: TOIFavoriteProperty;
  NewItem: TOIFavoriteProperty;
  cmp: LongInt;
  NewIndex: Integer;
  OldIndex: Integer;
begin
  NewFavorites:=TOIFavoritePropertiesClass(ClassType).Create;
  {$IFDEF DebugFavoriteroperties}
  debugln('TOIFavoriteProperties.MergeConfig ',dbgsName(NewFavorites),' ',dbgsName(NewFavorites.FItems));
  {$ENDIF}
  try
    // load config
    NewFavorites.LoadFromConfig(ConfigStore,Path);
    // sort both to see the differences
    NewFavorites.DeleteDoubles; // descending
    DeleteDoubles;               // descending
    // add all new things from NewFavorites
    NewIndex:=0;
    OldIndex:=0;
    while (NewIndex<NewFavorites.Count) do begin
      NewItem:=NewFavorites[NewIndex];
      if OldIndex>=Count then begin
        // item only exists in config -> move to this list
        NewFavorites.FItems[NewIndex]:=nil;
        inc(NewIndex);
        FItems.Insert(OldIndex,NewItem);
        inc(OldIndex);
      end else begin
        OldItem:=Items[OldIndex];
        cmp:=OldItem.Compare(NewItem);
        //debugln('TOIFavoriteProperties.MergeConfig cmp=',dbgs(cmp),' OldItem=[',OldItem.DebugReportAsString,'] NewItem=[',NewItem.DebugReportAsString,']');
        if cmp=0 then begin
          // item already exists in this list
          inc(NewIndex);
          inc(OldIndex);
        end else if cmp<0 then begin
          // item exists only in old favorites
          // -> next old
          inc(OldIndex);
        end else begin
          // item only exists in config -> move to this list
          NewFavorites.FItems[NewIndex]:=nil;
          inc(NewIndex);
          FItems.Insert(OldIndex,NewItem);
          inc(OldIndex);
        end;
      end;
    end;
  finally
    NewFavorites.Free;
  end;
  {$IFDEF DebugFavoriteroperties}
  debugln('TOIFavoriteProperties.MergeConfig END');
  WriteDebugReport;
  {$ENDIF}
end;

procedure TOIFavoriteProperties.SaveNewItemsToConfig(
  ConfigStore: TConfigStorage; const Path: string;
  BaseFavorites: TOIFavoriteProperties);
// Save all items, that are in this list and not in BaseFavorites
// It does not save, if an item in BaseFavorites is missing in this list
var
  SubtractList: TList;
  i: Integer;
  CurItem: TOIFavoriteProperty;
begin
  SubtractList:=GetSubtractList(BaseFavorites);
  try
    ConfigStore.SetDeleteValue(Path+'Count',SubtractList.Count,0);
    {$IFDEF DebugFavoriteroperties}
    debugln('TOIFavoriteProperties.SaveNewItemsToConfig A Count=',dbgs(SubtractList.Count));
    {$ENDIF}
    for i:=0 to SubtractList.Count-1 do begin
      CurItem:=TOIFavoriteProperty(SubtractList[i]);
      CurItem.SaveToConfig(ConfigStore,Path+'Item'+IntToStr(i)+'/');
      {$IFDEF DebugFavoriteroperties}
      debugln(' i=',dbgs(i),' ',CurItem.DebugReportAsString);
      {$ENDIF}
    end;
  finally
    SubtractList.Free;
  end;
end;

procedure TOIFavoriteProperties.Sort;
begin
  if FSorted then exit;
  FItems.Sort(@CompareOIFavoriteProperties);
end;

procedure TOIFavoriteProperties.DeleteDoubles;
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

function TOIFavoriteProperties.IsEqual(TheFavorites: TOIFavoriteProperties
  ): boolean;
var
  i: Integer;
begin
  Result:=false;
  DeleteDoubles;
  TheFavorites.DeleteDoubles;
  if Count<>TheFavorites.Count then exit;
  for i:=Count-1 downto 1 do
    if Items[i].Compare(TheFavorites.Items[i])<>0 then exit;
  Result:=true;
end;

function TOIFavoriteProperties.GetSubtractList(
  FavoritesToSubtract: TOIFavoriteProperties): TList;
// create a list of TOIFavoriteProperty of all items in this list
// and not in FavoritesToSubtract
var
  SelfIndex: Integer;
  SubtractIndex: Integer;
  CurItem: TOIFavoriteProperty;
  cmp: LongInt;
begin
  Result:=TList.Create;
  DeleteDoubles; // this also sorts descending
  FavoritesToSubtract.DeleteDoubles; // this also sorts descending
  SelfIndex:=0;
  SubtractIndex:=0;
  while SelfIndex<Count do begin
    CurItem:=Items[SelfIndex];
    if SubtractIndex>=FavoritesToSubtract.Count then begin
      // item does not exist in SubtractIndex -> add it
      Result.Add(CurItem);
      inc(SelfIndex);
    end else begin
      cmp:=CurItem.Compare(FavoritesToSubtract[SubtractIndex]);
      //debugln('TOIFavoriteProperties.GetSubtractList cmp=',dbgs(cmp),' CurItem=[',CurItem.DebugReportAsString,'] SubtractItem=[',FavoritesToSubtract[SubtractIndex].DebugReportAsString,']');
      if cmp=0 then begin
        // item exists in SubtractIndex -> skip
        inc(SubtractIndex);
        inc(SelfIndex);
      end else if cmp>0 then begin
        // item does not exist in FavoritesToSubtract -> add it
        Result.Add(CurItem);
        inc(SelfIndex);
      end else begin
        // item exists only in FavoritesToSubtract -> skip
        inc(SubtractIndex);
      end;
    end;
  end;
end;

procedure TOIFavoriteProperties.WriteDebugReport;
var
  i: Integer;
begin
  debugln('TOIFavoriteProperties.WriteDebugReport Count=',dbgs(Count));
  for i:=0 to Count-1 do
    debugln('  i=',dbgs(i),' ',Items[i].DebugReportAsString);
end;

{ TOIFavoriteProperty }

constructor TOIFavoriteProperty.Create(ABaseClass: TPersistentClass;
  const APropertyName: string; TheInclude: boolean);
begin
  BaseClass:=ABaseClass;
  PropertyName:=APropertyName;
  Include:=TheInclude;
end;

function TOIFavoriteProperty.Constrains(AnItem: TOIFavoriteProperty
  ): boolean;
// true if this item constrains AnItem
// This item constrains AnItem, if this is the opposite (Include) and
// AnItem has the same or greater scope
begin
  Result:=(Include<>AnItem.Include)
          and (CompareText(PropertyName,AnItem.PropertyName)=0)
          and (BaseClass.InheritsFrom(AnItem.BaseClass));
end;

function TOIFavoriteProperty.IsFavorite(AClass: TPersistentClass;
  const APropertyName: string): boolean;
begin
  Result:=(CompareText(PropertyName,APropertyName)=0)
          and (AClass.InheritsFrom(BaseClass));
end;

function TOIFavoriteProperty.Compare(AFavorite: TOIFavoriteProperty): integer;

  function CompareBaseClass: integer;
  begin
    if BaseClass<>nil then begin
      if AFavorite.BaseClass<>nil then
        Result:=ComparePointers(BaseClass,AFavorite.BaseClass)
      else
        Result:=CompareText(BaseClass.ClassName,AFavorite.BaseClassName);
    end else begin
      if AFavorite.BaseClass<>nil then
        Result:=CompareText(BaseClassName,AFavorite.BaseClass.ClassName)
      else
        Result:=CompareText(BaseClassName,AFavorite.BaseClassName);
    end;
  end;

begin
  // first compare PropertyName
  Result:=CompareText(PropertyName,AFavorite.PropertyName);
  if Result<>0 then exit;
  // then compare Include
  if Include<>AFavorite.Include then begin
    if Include then
      Result:=1
    else
      Result:=-1;
    exit;
  end;
  // then compare BaseClass and BaseClassName
  Result:=CompareBaseClass;
end;

procedure TOIFavoriteProperty.SaveToConfig(ConfigStore: TConfigStorage;
  const Path: string);
begin
  if BaseClass<>nil then
    ConfigStore.SetDeleteValue(Path+'BaseClass',BaseClass.ClassName,'')
  else
    ConfigStore.SetDeleteValue(Path+'BaseClass',BaseClassName,'');
  ConfigStore.SetDeleteValue(Path+'PropertyName',PropertyName,'');
  ConfigStore.SetDeleteValue(Path+'Include',Include,true);
end;

procedure TOIFavoriteProperty.Assign(Src: TOIFavoriteProperty);
begin
  BaseClassName:=Src.BaseClassName;
  BaseClass:=Src.BaseClass;
  PropertyName:=Src.PropertyName;
  Include:=Src.Include;
end;

function TOIFavoriteProperty.CreateCopy: TOIFavoriteProperty;
begin
  Result:=TOIFavoriteProperty.Create(BaseClass,PropertyName,Include);
  Result.BaseClass:=BaseClass;
end;

function TOIFavoriteProperty.DebugReportAsString: string;
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
    WidgetSetRestrictions[P] := 0;
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
    CurItem:=TOIRestrictedProperty(Items[I]);
    Result := Result + CurItem.IsRestricted(AClass,PropertyName);
  end;
end;

function TOIRestrictedProperties.AreRestricted(Selection: TPersistentSelectionList;
  const PropertyName: string): TLCLPlatforms;
var
  I: Integer;
begin
  Result := [];
  if Selection = nil then Exit;
  for i:=0 to Selection.Count-1 do
    Result := Result + IsRestricted(TPersistentClass(Selection[i].ClassType), PropertyName);
end;

end.

