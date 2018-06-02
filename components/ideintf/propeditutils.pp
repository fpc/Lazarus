{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit PropEditUtils;

{$mode objfpc}

interface

uses
  Classes, SysUtils, TypInfo, LazLoggerBase;

type
  {
    The TPersistentSelectionList is simply a list of TPersistent references.
    It will never create or free any object. It is used by the property
    editors, the object inspector and the form editor.
  }
  TPersistentSelectionList = class
  private
    FForceUpdate: Boolean;
    FUpdateLock: integer;
    FPersistentList: TFPList;
    function GetItems(AIndex: integer): TPersistent;
    procedure SetItems(AIndex: integer; const APersistent: TPersistent);
    function GetCount: integer;
    function GetCapacity:integer;
    procedure SetCapacity(const NewCapacity:integer);
  public
    constructor Create;
    destructor Destroy;  override;
    procedure BeginUpdate;
    procedure EndUpdate;
    function UpdateLock: integer;
    function IndexOf(APersistent: TPersistent): integer;
    procedure Clear;
    function IsEqual(SourceSelectionList: TPersistentSelectionList): boolean;
    procedure SortLike(SortedList: TPersistentSelectionList);
    property Count:integer read GetCount;
    property Capacity:integer read GetCapacity write SetCapacity;
    function Add(APersistent: TPersistent): integer;
    function Remove(APersistent: TPersistent): integer;
    procedure Delete(Index: Integer);
    procedure Assign(SourceSelectionList: TPersistentSelectionList);
    property Items[AIndex: integer]: TPersistent read GetItems write SetItems; default;
    procedure WriteDebugReport;
    property ForceUpdate: Boolean read FForceUpdate write FForceUpdate;
  end;

  TBackupComponentList = class
  private
    FComponentList: TList;
    FLookupRoot: TPersistent;
    FSelection: TPersistentSelectionList;
    function GetComponents(Index: integer): TComponent;
    procedure SetComponents(Index: integer; const AValue: TComponent);
    procedure SetLookupRoot(const AValue: TPersistent);
    procedure SetSelection(const AValue: TPersistentSelectionList);
  protected
  public
    constructor Create;
    destructor Destroy;  override;
    function IndexOf(AComponent: TComponent): integer;
    procedure Clear;
    function ComponentCount: integer;
    function IsEqual(ALookupRoot: TPersistent;
                     ASelection: TPersistentSelectionList): boolean;
  public
    property LookupRoot: TPersistent read FLookupRoot write SetLookupRoot;
    property Components[Index: integer]: TComponent read GetComponents write SetComponents;
    property Selection: TPersistentSelectionList read FSelection write SetSelection;
  end;

function GetLookupRootForComponent(APersistent: TPersistent): TPersistent;

type
  TGetLookupRoot = function(APersistent: TPersistent): TPersistent;

procedure RegisterGetLookupRoot(const OnGetLookupRoot: TGetLookupRoot);
procedure UnregisterGetLookupRoot(const OnGetLookupRoot: TGetLookupRoot);
function StrToBoolOI(S: string): Boolean;

implementation

type
  TPersistentAccess = class(TPersistent);
var
  GetLookupRoots: TFPList = nil; // list of TGetLookupRoot

function GetLookupRootForComponent(APersistent: TPersistent): TPersistent;
var
  AOwner: TPersistent = nil;
  i: Integer;
begin
  Result := APersistent;
  if Result = nil then
    Exit;
  repeat
    if Result is TPersistent then
      AOwner := TPersistentAccess(Result).GetOwner;
    if (AOwner=nil) and (GetLookupRoots<>nil) then begin
      for i:=GetLookupRoots.Count-1 downto 0 do begin
        AOwner:=TGetLookupRoot(GetLookupRoots[i])(Result);
        if AOwner<>nil then break;
      end;
    end;
    if AOwner = nil then
      exit;
    Result := AOwner
  until False;
end;

procedure RegisterGetLookupRoot(const OnGetLookupRoot: TGetLookupRoot);
begin
  if GetLookupRoots=nil then
    GetLookupRoots:=TFPList.Create;
  GetLookupRoots.Add(OnGetLookupRoot);
end;

procedure UnregisterGetLookupRoot(const OnGetLookupRoot: TGetLookupRoot);
begin
  if GetLookupRoots=nil then exit;
  GetLookupRoots.Remove(OnGetLookupRoot);
end;

function StrToBoolOI(S: string): Boolean;
// Like StrToBool but accepts also '(False)' and '(True)'.
begin
  if S = '' then Exit(False);
  if (Length(S) > 2) and (S[1] = '(') and (S[Length(S)] = ')') then
    S := Copy(S, 2, Length(S)-2);
  Result := StrToBool(S);
end;

{ TPersistentSelectionList }

function TPersistentSelectionList.Add(APersistent: TPersistent): integer;
begin
  Result:=FPersistentList.Add(APersistent);
end;

function TPersistentSelectionList.Remove(APersistent: TPersistent): integer;
begin
  Result:=IndexOf(APersistent);
  if Result>=0 then
    FPersistentList.Delete(Result);
end;

procedure TPersistentSelectionList.Delete(Index: Integer);
begin
  FPersistentList.Delete(Index);
end;

procedure TPersistentSelectionList.Clear;
begin
  FPersistentList.Clear;
end;

constructor TPersistentSelectionList.Create;
begin
  inherited Create;
  FPersistentList := TFPList.Create;
end;

destructor TPersistentSelectionList.Destroy;
begin
  FreeAndNil(FPersistentList);
  inherited Destroy;
end;

function TPersistentSelectionList.GetCount: integer;
begin
  Result:=FPersistentList.Count;
end;

function TPersistentSelectionList.GetItems(AIndex: integer): TPersistent;
begin
  Result:=TPersistent(FPersistentList[AIndex]);
end;

procedure TPersistentSelectionList.SetItems(AIndex: integer;
  const APersistent: TPersistent);
begin
  FPersistentList[AIndex]:=APersistent;
end;

function TPersistentSelectionList.GetCapacity:integer;
begin
  Result:=FPersistentList.Capacity;
end;

procedure TPersistentSelectionList.SetCapacity(const NewCapacity:integer);
begin
  FPersistentList.Capacity:=NewCapacity;
end;

procedure TPersistentSelectionList.BeginUpdate;
begin
  inc(FUpdateLock);
end;

procedure TPersistentSelectionList.EndUpdate;
begin
  dec(FUpdateLock);
end;

function TPersistentSelectionList.UpdateLock: integer;
begin
  Result:=FUpdateLock;
end;

function TPersistentSelectionList.IndexOf(APersistent: TPersistent): integer;
begin
  Result:=Count-1;
  while (Result>=0) and (Items[Result]<>APersistent) do dec(Result);
end;

procedure TPersistentSelectionList.Assign(SourceSelectionList: TPersistentSelectionList);
var
  a: integer;
begin
  if SourceSelectionList = Self then Exit;
  Clear;
  if Assigned(SourceSelectionList) then
  begin
    FForceUpdate := SourceSelectionList.ForceUpdate;
    FPersistentList.Count := SourceSelectionList.Count;
    for a := 0 to SourceSelectionList.Count - 1 do
      FPersistentList[a] := SourceSelectionList[a];
  end;
end;

procedure TPersistentSelectionList.WriteDebugReport;
var
  i: Integer;
begin
  DebugLn(['TPersistentSelectionList.WriteDebugReport Count=',Count]);
  for i:=0 to Count-1 do
    DebugLn(['  ',i,' ',dbgsName(Items[i])]);
end;

function TPersistentSelectionList.IsEqual(SourceSelectionList: TPersistentSelectionList): boolean;
var
  a: integer;
begin
  if (SourceSelectionList=nil) and (Count=0) then begin
    Result:=true;
    exit;
  end;
  Result:=false;
  if FPersistentList.Count<>SourceSelectionList.Count then exit;
  for a:=0 to FPersistentList.Count-1 do
    if Items[a]<>SourceSelectionList[a] then exit;
  Result:=true;
end;

procedure TPersistentSelectionList.SortLike(SortedList: TPersistentSelectionList);
// sort this list
var
  NewIndex: Integer;
  j: Integer;
  OldIndex: LongInt;
begin
  NewIndex:=0;
  j:=0;
  while (j<SortedList.Count) do begin
    OldIndex:=IndexOf(SortedList[j]);
    if OldIndex>=0 then begin
      // the j-th element of SortedList exists here
      if OldIndex<>NewIndex then
        FPersistentList.Move(OldIndex,NewIndex);
      inc(NewIndex);
    end;
    inc(j);
  end;
end;

{ TBackupComponentList }

function TBackupComponentList.GetComponents(Index: integer): TComponent;
begin
  Result:=TComponent(FComponentList[Index]);
end;

procedure TBackupComponentList.SetComponents(Index: integer;
  const AValue: TComponent);
begin
  FComponentList[Index]:=AValue;
end;

procedure TBackupComponentList.SetLookupRoot(const AValue: TPersistent);
var
  i: Integer;
begin
  FLookupRoot:=AValue;
  FComponentList.Clear;
  if (FLookupRoot<>nil) and (FLookupRoot is TComponent) then
    for i:=0 to TComponent(FLookupRoot).ComponentCount-1 do
      FComponentList.Add(TComponent(FLookupRoot).Components[i]);
  FSelection.Clear;
end;

procedure TBackupComponentList.SetSelection(
  const AValue: TPersistentSelectionList);
begin
  if FSelection=AValue then exit;
  FSelection.Assign(AValue);
end;

constructor TBackupComponentList.Create;
begin
  FSelection := TPersistentSelectionList.Create;
  FComponentList := TList.Create;
end;

destructor TBackupComponentList.Destroy;
begin
  FreeAndNil(FSelection);
  FreeAndNil(FComponentList);
  inherited Destroy;
end;

function TBackupComponentList.IndexOf(AComponent: TComponent): integer;
begin
  Result:=FComponentList.IndexOf(AComponent);
end;

procedure TBackupComponentList.Clear;
begin
  LookupRoot:=nil;
end;

function TBackupComponentList.ComponentCount: integer;
begin
  Result:=FComponentList.Count;
end;

function TBackupComponentList.IsEqual(ALookupRoot: TPersistent;
  ASelection: TPersistentSelectionList): boolean;
var
  i: Integer;
begin
  Result := False;
  if ALookupRoot <> LookupRoot then Exit;
  if not FSelection.IsEqual(ASelection) then Exit;
  if (ALookupRoot <> nil) and (FLookupRoot is TComponent) then
  begin
    if ComponentCount <> TComponent(ALookupRoot).ComponentCount then
      Exit;
    for i := 0 to FComponentList.Count - 1 do
      if TComponent(FComponentList[i]) <> TComponent(ALookupRoot).Components[i] then
        Exit;
  end;
  Result := True;
end;

initialization
  GetLookupRoots:=nil;
finalization
  FreeAndNil(GetLookupRoots);

end.

