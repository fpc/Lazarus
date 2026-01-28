{***************************************************************************
 *                                                                         *
 * This unit is distributed under the LGPL version 2                       *
 *                                                                         *
 * Additionally this unit can be used under any newer version (3 or up)    *
 * of the LGPL                                                             *
 *                                                                         *
 * Users are also granted the same "linking exception" as defined          *
 * for the LCL.                                                            *
 * See the LCL license for details                                         *
 *                                                                         *
 *                                                                         *
 ***************************************************************************
 @author(Martin Friebe)
}
unit LazDebuggerIntfExcludedRoutines;

{$mode objfpc}{$H+}
{$Interfaces CORBA}

interface

uses
  Classes, fgl, SysUtils, LazClasses, LazDebuggerIntfSynchronizedList;

type

  TDbgExeProcMatchFileKind = (
    mfkFileName,
    //mfkPathGlob,
    //mfkPathRegEx
    //mfkFilePathGlob,
    mfkFilePathRegEx
  );
  TDbgExeProcMatchNameKind = (
    mpkName,
    mpkRegEx
    //mpkAddress
    //mpkAddressRange
    //mpkSourceLine
  );

  IDbgExeProcSelectorIntf = interface ['{BAB83BC7-1376-48B3-BB6D-FE7BC5539EB8}']
    function  GetFileMatchKind: TDbgExeProcMatchFileKind;
    function  GetFileMatchValue: String;
    function  GetNameMatchKind: TDbgExeProcMatchNameKind;
    function  GetNameMatchValue: TStringArray;
  end;

  //IDbgExeProcSelectorSyncOriginIntf = interface(IDbgExeProcSelectorIntf) ['{1E0BDA5D-B503-4C7C-9F57-BD4633D0B17E}']
  //  function GetSyncOrigin: IDbgSynchronizedOriginEntryIntf;
  //end;


  IDbgExeProcSelectorListIntf = interface(IDbgSyncAbleOriginIntf) ['{24D3B4F5-CB17-4F37-B7B6-347277E5D3FB}']
    function GetEntry(AnIndex: Integer): IDbgExeProcSelectorIntf;
    function Count: integer;

    property Entries[AnIndex: integer]: IDbgExeProcSelectorIntf read GetEntry; default;
  end;

  IDbgExcludedRoutinesIntf = interface;

  IDbgExcludedRoutinesNotifyHandlerIntf = interface ['{D92922FA-CBCF-4379-AD9C-1ACFBA34CE9B}']
    procedure DoChanged(Sender: IDbgExcludedRoutinesIntf);
  end;

  IDbgExcludedRoutinesIntf = interface(IDbgExeProcSelectorListIntf) ['{6551682A-EBA2-4C84-96C9-BE3077CBBB34}']
    procedure AddNotifyHandler(AHandler: IDbgExcludedRoutinesNotifyHandlerIntf);
    procedure RemoveNotifyHandler(AHandler: IDbgExcludedRoutinesNotifyHandlerIntf);
  end;


  { TDbgExeProcSelectorTemplate }

  generic TDbgExeProcSelectorTemplate<_Base: TObject> = class(_Base, IDbgExeProcSelectorIntf)
  strict private
    FFileMatchKind: TDbgExeProcMatchFileKind;
    FFileMatchValue: String;
    FNameMatchKind: TDbgExeProcMatchNameKind;
    FNameMatchValue: TStringArray;
  private
    function GetFileMatchKind: TDbgExeProcMatchFileKind;
    function GetFileMatchValue: String;
    function GetNameMatchKind: TDbgExeProcMatchNameKind;
    function GetNameMatchValue: TStringArray;
    procedure SetFileMatchKind(AValue: TDbgExeProcMatchFileKind);
    procedure SetFileMatchValue(AValue: String);
    procedure SetNameMatchKind(AValue: TDbgExeProcMatchNameKind);
    procedure SetNameMatchValue(AValue: TStringArray);
  protected
    procedure Changed; virtual;
  public
    property FileMatchKind: TDbgExeProcMatchFileKind read GetFileMatchKind write SetFileMatchKind;
    property FileMatchValue: String read GetFileMatchValue write SetFileMatchValue;
    property NameMatchKind: TDbgExeProcMatchNameKind read GetNameMatchKind write SetNameMatchKind;
    property NameMatchValue: TStringArray read GetNameMatchValue write SetNameMatchValue;
  end;

  { TDbgExeProcSelectorListTemplate }

  generic TDbgExeProcSelectorListTemplate<_BASE: TObject; _ITM: class> = class(_BASE)
  private type
    TDbgExeProcSelectorObjectList = specialize TFPGObjectList<_ITM>;
  public type
    TDbgExeProcSelectorListTemplate_Item = _ITM;
  private
    FList: TDbgExeProcSelectorObjectList;
    function GetEntry(AnIndex: integer): _ITM;
  protected
//    function GetSynchronizedListIntf: IDbgSynchronizedOriginListIntf; virtual; abstract;
    procedure DoDestroy;
    property List: TDbgExeProcSelectorObjectList read FList;
  public
    constructor Create;
    procedure Clear;
    function Count: integer;
    property Entries[AnIndex: integer]: _ITM read GetEntry; default;

  end;

  { TDbgExcludedRoutinesNotifyHandlerTemplate }

  generic TDbgExcludedRoutinesNotifyHandlerTemplate<_Base: TObject> = class(_Base, IDbgExcludedRoutinesNotifyHandlerIntf)
  public
    procedure DoChanged(Sender: IDbgExcludedRoutinesIntf);
  end;

  { TDbgExcludedRoutinesTemplate }

  generic TDbgExcludedRoutinesTemplate<_Base: TObject> = class(_Base
    //specialize TRefCountedGeneric<_Base>
    {, IDbgExeProcSelectorListIntf}
  )
  private type
    THandlerList = specialize TFPGList<IDbgExcludedRoutinesNotifyHandlerIntf>;
  private
    FHandlerList: THandlerList;
  protected
    procedure Init;
    procedure CallHandlersDoChange(ASender: IDbgExcludedRoutinesIntf);
  public
    constructor Create;
    procedure DoDestroy; inline;

    procedure AddNotifyHandler(AHandler: IDbgExcludedRoutinesNotifyHandlerIntf);
    procedure RemoveNotifyHandler(AHandler: IDbgExcludedRoutinesNotifyHandlerIntf);
  end;

  { TDbgExeProcSelectorSyncTargetList
    This list can NOT be used as source for another sync-target.
    It does not change the timestamp of any newly added entry. Therefore such
    a timestamp would not be seen by a further sync-target.
    To add this entries will need 2 ChangeStamps, one to compare with their source,
    and one to show if they have been newly added to the list.
    The "newly added" would have to follow the global change-counter to be valid
    for all sync-target lists.
  }

  { TDbgExeProcSelectorSyncTargetgTemplate }

  generic TDbgExeProcSelectorSyncTargetgTemplate<_BASE: TObject> = class(
    specialize TDbgExeProcSelectorTemplate<_BASE>,
    IDbgSynchronizedTargetEntryIntf
  )
  private
    FMainEntry: IDbgExeProcSelectorIntf;
    FOriginEntry: IDbgExeProcSelectorIntf;
    FSyncEntry: IDbgSynchronizedOriginEntryIntf;
  protected
    function GetID: QWord;
    function HasDeletedOrigin: boolean;
    procedure DoDestroy;
  public
    constructor Create(AnOriginEntry: IDbgExeProcSelectorIntf; ASyncEntry: IDbgSynchronizedOriginEntryIntf); virtual;

    procedure Update;
    property OriginEntry: IDbgExeProcSelectorIntf read FMainEntry;
    property SyncEntry: IDbgSynchronizedOriginEntryIntf read FSyncEntry;
    property ID: QWord read GetID;
  end;

  generic TDbgExeProcSelectorSyncTargetListTemplate<_BASE, _ITEM: class> = class(
    specialize TDbgExeProcSelectorListTemplate<
       {Base} specialize TDbgSynchronizedListExTemplate<_BASE, IDbgExeProcSelectorListIntf>,
       {Item} specialize TDbgExeProcSelectorSyncTargetgTemplate<_ITEM>
     >
  )
  public type
    TDbgExeProcSelectorSyncTargetEntryClass = class of TDbgExeProcSelectorListTemplate_Item;
    TDbgExeProcSelectorSyncUpdateAction = (uaChanged, uaAdded, uaDelete);
  private
    FChangeStamp: QWord;
  protected
    procedure BeginUpdatingFrom(AList: IDbgExeProcSelectorListIntf; ARemoveAllDangling: boolean); virtual;
    procedure EndUpdatingFrom(AList: IDbgExeProcSelectorListIntf; ARemoveAllDangling: boolean); virtual;
    (* EntryClass MUST return a subclass in which DESTROY calls DoDestroy *)
    function  EntryClass: TDbgExeProcSelectorSyncTargetEntryClass; virtual;

    function  GetSyncTargetEntry(AnIndex: integer): IDbgSynchronizedTargetEntryIntf; override;
    procedure AddSyncedEntryFor(AnOriginIndex: integer; ASrcList: IDbgExeProcSelectorListIntf; AnOrigin: IDbgSynchronizedOriginEntryIntf); override;
    procedure UpdateSyncedEntryAt(AnOriginIndex, ATargetIndex: integer; ASrcList: IDbgExeProcSelectorListIntf; AnOrigin: IDbgSynchronizedOriginEntryIntf); override;
    procedure DeleteSyncedEntryAt(AnOriginIndex, ATargetIndex: integer; ASrcList: IDbgExeProcSelectorListIntf; AnOrigin: IDbgSynchronizedOriginEntryIntf); override;

    procedure DoEntryChanged(AnManagedIndex: Integer; ANewValue: IDbgExeProcSelectorIntf;
      AnAction: TDbgExeProcSelectorSyncUpdateAction); virtual;
  public
    procedure UpdateFrom(AList: IDbgExeProcSelectorListIntf; ARemoveAllDangling: boolean = True);
    procedure RemoveItemsWithDeletedMain;
    property ChangeStamp: QWord read FChangeStamp;
  end;

  TDbgExeProcSelectorSyncTargetList = class(
    specialize TDbgExeProcSelectorSyncTargetListTemplate<TObject, TObject>
  )
  public type
    TDbgExeProcSelectorSyncTargetList_Entry = class(TDbgExeProcSelectorListTemplate_Item)
    public
      destructor Destroy; override; // call DoDestroy
    end;
  private
    FMainList: IDbgExeProcSelectorListIntf;
  protected
    procedure SetMainList(AValue: IDbgExeProcSelectorListIntf);
    function EntryClass: TDbgExeProcSelectorSyncTargetEntryClass; override;
  public
    destructor Destroy; override;
    procedure Update;
    procedure UpdateFrom(AList: IDbgExeProcSelectorListIntf; ARemoveAllDangling: boolean = True);

    property MainList: IDbgExeProcSelectorListIntf read FMainList write SetMainList;
  end;

implementation

{ TDbgExeProcSelectorTemplate }

function TDbgExeProcSelectorTemplate.GetFileMatchKind: TDbgExeProcMatchFileKind;
begin
  Result := FFileMatchKind;
end;

function TDbgExeProcSelectorTemplate.GetFileMatchValue: String;
begin
  Result := FFileMatchValue;
end;

function TDbgExeProcSelectorTemplate.GetNameMatchKind: TDbgExeProcMatchNameKind;
begin
  Result := FNameMatchKind;
end;

function TDbgExeProcSelectorTemplate.GetNameMatchValue: TStringArray;
begin
  Result := FNameMatchValue;
end;

procedure TDbgExeProcSelectorTemplate.SetFileMatchKind(
  AValue: TDbgExeProcMatchFileKind);
begin
  if FFileMatchKind = AValue then
    exit;
  FFileMatchKind := AValue;
  Changed;
end;

procedure TDbgExeProcSelectorTemplate.SetFileMatchValue(AValue: String);
begin
  if FFileMatchValue = AValue then
    exit;
  FFileMatchValue := AValue;
  Changed;
end;

procedure TDbgExeProcSelectorTemplate.SetNameMatchKind(
  AValue: TDbgExeProcMatchNameKind);
begin
  if FNameMatchKind = AValue then
    exit;
  FNameMatchKind := AValue;
  Changed;
end;

procedure TDbgExeProcSelectorTemplate.SetNameMatchValue(AValue: TStringArray);
begin
  if FNameMatchValue = AValue then
    exit;
  FNameMatchValue := AValue;
  Changed;
end;

procedure TDbgExeProcSelectorTemplate.Changed;
begin
  //
end;

{ TDbgExeProcSelectorListTemplate }

function TDbgExeProcSelectorListTemplate.GetEntry(AnIndex: integer): _ITM;
begin
  Result := FList[AnIndex];
end;

procedure TDbgExeProcSelectorListTemplate.DoDestroy;
begin
  FList.Free;
end;

constructor TDbgExeProcSelectorListTemplate.Create;
begin
  FList := TDbgExeProcSelectorObjectList.Create(True);
  inherited;
end;

procedure TDbgExeProcSelectorListTemplate.Clear;
begin
  FList.Clear;
end;

function TDbgExeProcSelectorListTemplate.Count: integer;
begin
  Result := FList.Count;
end;

{ TDbgExcludedRoutinesNotifyHandlerTemplate }

procedure TDbgExcludedRoutinesNotifyHandlerTemplate.DoChanged(Sender: IDbgExcludedRoutinesIntf);
begin
  //
end;

{ TDbgExcludedRoutinesTemplate }

procedure TDbgExcludedRoutinesTemplate.Init;
begin
  FHandlerList := THandlerList.Create;
end;

procedure TDbgExcludedRoutinesTemplate.CallHandlersDoChange(ASender: IDbgExcludedRoutinesIntf);
var
  i: Integer;
begin
  for i := 0 to FHandlerList.Count - 1 do
    FHandlerList[i].DoChanged(ASender);
end;

constructor TDbgExcludedRoutinesTemplate.Create;
begin
  Init;
  inherited;
end;

procedure TDbgExcludedRoutinesTemplate.DoDestroy;
begin
  FHandlerList.Destroy;
  inherited;
end;

procedure TDbgExcludedRoutinesTemplate.AddNotifyHandler(
  AHandler: IDbgExcludedRoutinesNotifyHandlerIntf);
begin
  FHandlerList.Add(AHandler);
end;

procedure TDbgExcludedRoutinesTemplate.RemoveNotifyHandler(
  AHandler: IDbgExcludedRoutinesNotifyHandlerIntf);
begin
  FHandlerList.Remove(AHandler);
end;

{ TDbgExeProcSelectorSyncTargetgTemplate }

function TDbgExeProcSelectorSyncTargetgTemplate.GetID: QWord;
begin
  Result := FSyncEntry.ID;
end;

function TDbgExeProcSelectorSyncTargetgTemplate.HasDeletedOrigin: boolean;
begin
  Result := FSyncEntry.IsDeleted;
end;

constructor TDbgExeProcSelectorSyncTargetgTemplate.Create(AnOriginEntry: IDbgExeProcSelectorIntf;
  ASyncEntry: IDbgSynchronizedOriginEntryIntf);
begin
  FOriginEntry := AnOriginEntry;
  FSyncEntry := ASyncEntry;
  FSyncEntry.AddReference;

  inherited Create;
  Update;
end;

procedure TDbgExeProcSelectorSyncTargetgTemplate.DoDestroy;
begin
  FSyncEntry.ReleaseReference;
  inherited;
end;

procedure TDbgExeProcSelectorSyncTargetgTemplate.Update;
begin
  FileMatchKind  := FOriginEntry.GetFileMatchKind;
  FileMatchValue := FOriginEntry.GetFileMatchValue;
  NameMatchKind  := FOriginEntry.GetNameMatchKind;
  NameMatchValue := FOriginEntry.GetNameMatchValue;
end;

{ TDbgExeProcSelectorSyncTargetListTemplate }

function DoCompareEntries(const Item1, Item2: TDbgExeProcSelectorSyncTargetList.TDbgExeProcSelectorListTemplate_Item): Integer;
begin
  if Item1.GetID < Item2.GetID then
    Result := -1
  else
  if Item1.GetID > Item2.GetID then
    Result := 1
  else
    Result := 0;
end;

procedure TDbgExeProcSelectorSyncTargetListTemplate.BeginUpdatingFrom(AList: IDbgExeProcSelectorListIntf;
  ARemoveAllDangling: boolean);
begin
  //
end;

procedure TDbgExeProcSelectorSyncTargetListTemplate.EndUpdatingFrom(AList: IDbgExeProcSelectorListIntf;
  ARemoveAllDangling: boolean);
begin
  //
end;

procedure TDbgExeProcSelectorSyncTargetListTemplate.RemoveItemsWithDeletedMain;
begin
  RemoveEntriesWithDeletedOrigin(Count);
end;

function TDbgExeProcSelectorSyncTargetListTemplate.EntryClass: TDbgExeProcSelectorSyncTargetEntryClass;
begin
  Result := TDbgExeProcSelectorListTemplate_Item;
end;

procedure TDbgExeProcSelectorSyncTargetListTemplate.AddSyncedEntryFor(AnOriginIndex: integer;
  ASrcList: IDbgExeProcSelectorListIntf; AnOrigin: IDbgSynchronizedOriginEntryIntf);
var
  Src: IDbgExeProcSelectorIntf;
  n: Integer;
begin
  Src := ASrcList.Entries[AnOriginIndex];
  n := FList.Add(EntryClass.Create(Src, AnOrigin));
  DoEntryChanged(n, Src, uaAdded);
end;

procedure TDbgExeProcSelectorSyncTargetListTemplate.UpdateSyncedEntryAt(AnOriginIndex,
  ATargetIndex: integer; ASrcList: IDbgExeProcSelectorListIntf;
  AnOrigin: IDbgSynchronizedOriginEntryIntf);
var
  Src: IDbgExeProcSelectorIntf;
begin
  Src := ASrcList.Entries[AnOriginIndex];
  DoEntryChanged(ATargetIndex, Src, uaChanged);
  FList[ATargetIndex].Update;
end;

procedure TDbgExeProcSelectorSyncTargetListTemplate.DeleteSyncedEntryAt(AnOriginIndex,
  ATargetIndex: integer; ASrcList: IDbgExeProcSelectorListIntf;
  AnOrigin: IDbgSynchronizedOriginEntryIntf);
var
  Src: IDbgExeProcSelectorIntf;
begin
  Src := ASrcList.Entries[AnOriginIndex];
  DoEntryChanged(ATargetIndex, Src, uaDelete);
  FList.Delete(ATargetIndex);
end;

function TDbgExeProcSelectorSyncTargetListTemplate.GetSyncTargetEntry(AnIndex: integer
  ): IDbgSynchronizedTargetEntryIntf;
begin
  Result := FList[AnIndex];
end;

procedure TDbgExeProcSelectorSyncTargetListTemplate.DoEntryChanged(AnManagedIndex: Integer;
  ANewValue: IDbgExeProcSelectorIntf; AnAction: TDbgExeProcSelectorSyncUpdateAction);
begin
  //
end;

procedure TDbgExeProcSelectorSyncTargetListTemplate.UpdateFrom(AList: IDbgExeProcSelectorListIntf;
  ARemoveAllDangling: boolean);
begin
  if ARemoveAllDangling then
    RemoveItemsWithDeletedMain;
  UpdateListFrom(AList, AList.Count, FChangeStamp);
end;

procedure TDbgExeProcSelectorSyncTargetList.SetMainList(AValue: IDbgExeProcSelectorListIntf);
var
  SyncList: IDbgSynchronizedOriginListIntf;
  i: Integer;
begin
  if FMainList = AValue then Exit;
  FMainList := AValue;
  List.Clear;
  SyncList := FMainList.GetSynchronizedListIntf;
  for i := 0 to FMainList.Count - 1 do
    List.Add(EntryClass.Create(FMainList[i], SyncList[i]));
  FChangeStamp := SyncList.ChangeStamp;
end;

function TDbgExeProcSelectorSyncTargetList.EntryClass: TDbgExeProcSelectorSyncTargetEntryClass;
begin
  Result := TDbgExeProcSelectorSyncTargetList_Entry;
end;

destructor TDbgExeProcSelectorSyncTargetList.Destroy;
begin
  inherited Destroy;
  DoDestroy;
end;

procedure TDbgExeProcSelectorSyncTargetList.Update;
begin
  List.Sort(@DoCompareEntries);
  UpdateListFrom(FMainList, FList.Count, FChangeStamp);
end;

procedure TDbgExeProcSelectorSyncTargetList.UpdateFrom(AList: IDbgExeProcSelectorListIntf;
  ARemoveAllDangling: boolean);
begin
  List.Sort(@DoCompareEntries);
  inherited UpdateFrom(AList, ARemoveAllDangling);
end;

{ TDbgExeProcSelectorSyncTargetList.TDbgExeProcSelectorSyncTargetList_Entry }

destructor TDbgExeProcSelectorSyncTargetList.TDbgExeProcSelectorSyncTargetList_Entry.Destroy;
begin
  inherited Destroy;
  DoDestroy;
end;

end.

