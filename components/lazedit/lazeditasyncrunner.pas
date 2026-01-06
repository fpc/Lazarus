{
 *****************************************************************************
  This file is part of the LazEdit package from the Lazarus IDE.

  This content of this file is licensed: Modified LGPL-2
  Or at the users choice: Modified LGPL-3
  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.

  Alternatively, the contents of this file may be used under the terms of the
  Mozilla Public License Version 1.1 or 2.0 http://www.mozilla.org/MPL/

  A copy used under either License can have the other Licenses removed from this
  header. A note should be added that the original file is available with the
  above choice of License.
  Used units may have to be amended according to the choice.
 *****************************************************************************
}
unit LazEditASyncRunner;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  fgl, Math, SysUtils, Forms;

type
  TLazEditEditorId = type pointer; // Can hold instance pointer as ID
  TLazEditTaskId = type pointer; // Can hold instance pointer as ID

  TTLazEditEditorPriority = (
    epHasFocused,   // Editor has focus
    epIsVisible,    // Editor is visible
    epHasHandle,    // Editor has handle
    epCreated
  );

  TTLazEditTaskPriority = (
    // paint in the visible (visible if editor becomes visible) area needed:
    tpPaintText,     // Task needed for painting main text area / incl non-overview gutter
    tpPaintExtra,    // Task needed to paint extra markup (matching items, ...)
    tpPaintOverview, // Task needed to paint overview
    tpLayout,        // Scrollbar, wrap, longest line (outside visible area)

    tpPriorHigh, tpPriorTop // relative within the above sections
  );
  TTLazEditTaskPriorities = set of TTLazEditTaskPriority;

  TLazEditTaskCallback = procedure(AnEditorId: TLazEditEditorId; ATaskId: TLazEditTaskId; AMaxTime: integer;
    var AData: Pointer; var ADone: boolean;
    var APriorities: TTLazEditTaskPriorities) of object;

  { TLazEditASyncRunner }

  TLazEditASyncRunner = class
  private type

    TASyncTask = record
      CallBack: TLazEditTaskCallback;
      EditorId: TLazEditEditorId;
      TaskId: TLazEditTaskId;
      TaskPriorities: TTLazEditTaskPriorities;
      Data: Pointer;
      class operator = (a, b: TASyncTask): boolean;
    end;
    TASyncEditorList = specialize TFPGMap<TLazEditEditorId, TTLazEditEditorPriority>;

    TASyncTaskList = class(specialize TFPGList<TASyncTask>)
    private
      FNeedsSorting: boolean;
    public
      function IndexOf(ACallBack: TLazEditTaskCallback; ALastIndexOf: integer = -1): integer;
      function IndexOf(ACallBack: TLazEditTaskCallback; AnEditorId: TLazEditEditorId; ATaskId: TLazEditTaskId): integer;
      procedure Update(AnIndex: integer; ATaskPriorities: TTLazEditTaskPriorities);
      procedure Update(AnIndex: integer; ATaskPriorities: TTLazEditTaskPriorities; AData: Pointer);
      function Add(ACallBack: TLazEditTaskCallback; AnEditorId: TLazEditEditorId; ATaskId: TLazEditTaskId;
                   ATaskPriorities: TTLazEditTaskPriorities = []; AData: Pointer = nil): integer;
      function Add(AnItem: TASyncTask): integer;
      procedure MaybeSort; inline;
      property NeedsSorting: boolean read FNeedsSorting write FNeedsSorting;
    end;
  private const
    TIME_PER_LOOP = 120;
    TIME_PER_CALL = 25;
  private
    FEditors: TASyncEditorList;
    FTasks: array [TTLazEditEditorPriority] of TASyncTaskList;
    FAutoFree: boolean;

    function GetEditorPriority(AnEditorId: TLazEditEditorId): TTLazEditEditorPriority;
    function GetTaskPriority(ACallBack: TLazEditTaskCallback; AnEditorId: TLazEditEditorId; ATaskId: TLazEditTaskId): TTLazEditTaskPriorities;
    procedure SetEditorPriority(AnEditorId: TLazEditEditorId; AValue: TTLazEditEditorPriority);
    procedure SetTaskPriority(ACallBack: TLazEditTaskCallback; AnEditorId: TLazEditEditorId;
      ATaskId: TLazEditTaskId; AValue: TTLazEditTaskPriorities);
    procedure EditorChangedPriority(AnEditorId: TLazEditEditorId; AnOldPrior, ANewPrior: TTLazEditEditorPriority);
  private
    FIsRunning: boolean;
    FTicksPerASyncLoop: integer;
    FTicksPerASyncTask: integer;
    procedure StartRunning;
    procedure StopRunning;
    procedure MaybeStartRunning;
    procedure MaybeStopRunning;
    procedure DoRunAsync(Data: PtrInt);
  protected
    procedure Release;
  public
    // Editor settigs MUST NOT be changed while in ASync callback
    procedure RegisterEditor(AnEditorId: TLazEditEditorId; AnEditorPriorities: TTLazEditEditorPriority = epCreated);
    procedure UnregisterEditor(AnEditorId: TLazEditEditorId);
    property  EditorPriority [AnEditorId: TLazEditEditorId]: TTLazEditEditorPriority read GetEditorPriority write SetEditorPriority;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddOrReplaceTask(ACallBack: TLazEditTaskCallback; AnEditorId: TLazEditEditorId;
      ATaskPriorities: TTLazEditTaskPriorities = []; ATaskId: TLazEditTaskId = nil; AData: Pointer = nil);
    procedure RemoveTask(ACallBack: TLazEditTaskCallback; AnEditorId: TLazEditEditorId; ATaskId: TLazEditTaskId= nil);
    procedure RemoveAllTaskFor(ACallBack: TLazEditTaskCallback);
    property  TaskPriority [ACallBack: TLazEditTaskCallback; AnEditorId: TLazEditEditorId; ATaskId: TLazEditTaskId]: TTLazEditTaskPriorities read GetTaskPriority write SetTaskPriority;

    property TicksPerASyncLoop: integer read FTicksPerASyncLoop write FTicksPerASyncLoop;
    property TicksPerASyncTask: integer read FTicksPerASyncTask write FTicksPerASyncTask;
  end;

function GetGlobalASyncRunner: TLazEditASyncRunner;

property GlobalASyncRunner: TLazEditASyncRunner read GetGlobalASyncRunner;

implementation

var
  TheGlobalASyncRunner : TLazEditASyncRunner = nil;

function GetGlobalASyncRunner: TLazEditASyncRunner;
begin
  if TheGlobalASyncRunner = nil then
    TheGlobalASyncRunner := TLazEditASyncRunner.Create;
  Result := TheGlobalASyncRunner;
end;

function DoCompareTaskPrior(p1, p2: TTLazEditTaskPriorities): integer;
  var
    a1, b1: TTLazEditTaskPriorities;
  function c(a,b: boolean): integer;
  begin
    if a xor b then begin
      if a
      then Result := -1
      else Result :=  1;
    end
    else begin
      Result := 0;
      if a and (a1 <> b1) then begin
        if tpPriorHigh in a1 then dec(Result, 1);
        if tpPriorHigh in b1 then inc(Result, 1);
        if tpPriorTop in a1 then dec(Result, 2);
        if tpPriorTop in b1 then inc(Result, 2);
      end;
    end;
  end;
begin
  a1 := p1 * [tpPriorHigh, tpPriorTop];
  b1 := p2 * [tpPriorHigh, tpPriorTop];
  Result := c(tpPaintText in p1, tpPaintText in p2);
  if Result <> 0 then exit;
  Result := c(tpPaintExtra in p1, tpPaintExtra in p2);
  if Result <> 0 then exit;
  Result := c(tpLayout in p1, tpLayout in p2);
  if Result <> 0 then exit;
  Result := c(tpPaintOverview in p1, tpPaintOverview in p2);
  if Result <> 0 then exit;
  Result := c(True, True); // tpPrior...
end;

{ TLazEditASyncRunner.TASyncTask }

function DoCompareTasks(const Item1, Item2: TLazEditASyncRunner.TASyncTask): Integer;
begin
  Result := DoCompareTaskPrior(Item1.TaskPriorities, Item2.TaskPriorities);
end;

class operator TLazEditASyncRunner.TASyncTask. = (a, b: TASyncTask): boolean;
begin
  Result := (a.CallBack = b.CallBack) and
            (a.EditorId = b.EditorId) and
            (a.TaskId   = b.TaskId);
end;

{ TLazEditASyncRunner.TASyncTaskList }

function TLazEditASyncRunner.TASyncTaskList.IndexOf(ACallBack: TLazEditTaskCallback;
  ALastIndexOf: integer): integer;
begin
  if ALastIndexOf >= 0 then
    Result := ALastIndexOf - 1
  else
    Result := Count - 1;
  while (Result >= 0) and
        (Items[Result].CallBack <> ACallBack)
  do
    dec(Result);
end;

function TLazEditASyncRunner.TASyncTaskList.IndexOf(ACallBack: TLazEditTaskCallback;
  AnEditorId: TLazEditEditorId; ATaskId: TLazEditTaskId): integer;
begin
  Result := Count - 1;
  while (Result >= 0) and (
          (Items[Result].CallBack <> ACallBack) or
          (Items[Result].EditorId <> AnEditorId) or
          (Items[Result].TaskId   <> ATaskId)
        )
  do
    dec(Result);
end;

procedure TLazEditASyncRunner.TASyncTaskList.Update(AnIndex: integer;
  ATaskPriorities: TTLazEditTaskPriorities);
var
  Itm: TASyncTask;
begin
  Itm := Items[AnIndex];
  if Itm.TaskPriorities = ATaskPriorities then
    exit;
  Itm.TaskPriorities   := ATaskPriorities;
  Items[AnIndex] := Itm;
  FNeedsSorting := True;
end;

procedure TLazEditASyncRunner.TASyncTaskList.Update(AnIndex: integer;
  ATaskPriorities: TTLazEditTaskPriorities; AData: Pointer);
var
  Itm: TASyncTask;
begin
  Itm := Items[AnIndex];
  if (Itm.TaskPriorities = ATaskPriorities) and
     (Itm.Data           = AData)
  then
    exit;
  Itm.TaskPriorities   := ATaskPriorities;
  Itm.Data     := AData;
  Items[AnIndex] := Itm;
  FNeedsSorting := True;
end;

function TLazEditASyncRunner.TASyncTaskList.Add(ACallBack: TLazEditTaskCallback;
  AnEditorId: TLazEditEditorId; ATaskId: TLazEditTaskId; ATaskPriorities: TTLazEditTaskPriorities;
  AData: Pointer): integer;
var
  NewItem: TASyncTask;
begin
  NewItem := Default(TASyncTask);
  NewItem.CallBack := ACallBack;
  NewItem.EditorId := AnEditorId;
  NewItem.TaskId   := ATaskId;
  NewItem.TaskPriorities   := ATaskPriorities;
  NewItem.Data     := AData;
  Result := Add(NewItem);
end;

function TLazEditASyncRunner.TASyncTaskList.Add(AnItem: TASyncTask): integer;
begin
  Result := inherited Add(AnItem);
  FNeedsSorting := True;
end;

procedure TLazEditASyncRunner.TASyncTaskList.MaybeSort;
begin
  if not FNeedsSorting then exit;
  inherited Sort(@DoCompareTasks);
  FNeedsSorting := False;
end;

{ TLazEditASyncRunner }

function TLazEditASyncRunner.GetEditorPriority(AnEditorId: TLazEditEditorId
  ): TTLazEditEditorPriority;
var
  i: Integer;
begin
  i := FEditors.IndexOf(AnEditorId);
  if i >= 0 then
    Result := FEditors.Data[i]
  else
    Result := epCreated;
end;

function TLazEditASyncRunner.GetTaskPriority(ACallBack: TLazEditTaskCallback;
  AnEditorId: TLazEditEditorId; ATaskId: TLazEditTaskId): TTLazEditTaskPriorities;
var
  p: TTLazEditEditorPriority;
  i: Integer;
begin
  p := EditorPriority[AnEditorId];
  i := FTasks[p].IndexOf(ACallBack, AnEditorId, ATaskId);
  Result := FTasks[p][i].TaskPriorities;
end;

procedure TLazEditASyncRunner.SetEditorPriority(AnEditorId: TLazEditEditorId;
  AValue: TTLazEditEditorPriority);
var
  p: TTLazEditEditorPriority;
begin
  p := EditorPriority[AnEditorId];
  if p = AValue then
    exit;
  FEditors[AnEditorId] := AValue;
  EditorChangedPriority(AnEditorId, p, AValue);
end;

procedure TLazEditASyncRunner.SetTaskPriority(ACallBack: TLazEditTaskCallback;
  AnEditorId: TLazEditEditorId; ATaskId: TLazEditTaskId; AValue: TTLazEditTaskPriorities);
var
  p: TTLazEditEditorPriority;
  i: Integer;
begin
  p := EditorPriority[AnEditorId];
  i := FTasks[p].IndexOf(ACallBack, AnEditorId, ATaskId);
  FTasks[p].Update(i, AValue)
end;

procedure TLazEditASyncRunner.EditorChangedPriority(AnEditorId: TLazEditEditorId; AnOldPrior,
  ANewPrior: TTLazEditEditorPriority);
var
  i: Integer;
  Itm: TASyncTask;
begin
  if ANewPrior = AnOldPrior then
    exit;
  i := FTasks[AnOldPrior].Count - 1;
  while i >= 0 do begin
    Itm := FTasks[AnOldPrior][i];
    if Itm.EditorId = AnEditorId then begin
      FTasks[ANewPrior].Add(Itm);
      FTasks[AnOldPrior].Delete(i);
    end;
    dec(i);
  end;
end;

procedure TLazEditASyncRunner.StartRunning;
begin
  if FIsRunning or FAutoFree then exit;
  FIsRunning := True;
  Application.QueueAsyncCall(@DoRunAsync, 0);
end;

procedure TLazEditASyncRunner.StopRunning;
begin
  if not FIsRunning then exit;
  Application.RemoveAsyncCalls(Self);
  FIsRunning := False;
end;

procedure TLazEditASyncRunner.MaybeStartRunning;
var
  p: TTLazEditEditorPriority;
begin
  if FIsRunning then exit;
  for p in TTLazEditEditorPriority do
    if FTasks[p].Count <> 0 then begin
      StartRunning;
      exit;
    end;
end;

procedure TLazEditASyncRunner.MaybeStopRunning;
var
  p: TTLazEditEditorPriority;
begin
  if not FIsRunning then exit;
  for p in TTLazEditEditorPriority do
    if FTasks[p].Count <> 0 then exit;
  StopRunning;
end;

procedure TLazEditASyncRunner.DoRunAsync(Data: PtrInt);
var
  p: TTLazEditEditorPriority;
  ItmIdx, PerItmTime, i: Integer;
  Itm: TASyncTask;
  TheData: Pointer;
  ThePrior, OrigPrior: TTLazEditTaskPriorities;
  IsDone: Boolean;
  EndTime: QWord;
begin
  FIsRunning := False;
  EndTime := GetTickCount64 + FTicksPerASyncLoop;

  for p in TTLazEditEditorPriority do begin
    if FTasks[p].Count = 0 then Continue;
    FTasks[p].MaybeSort;

    ItmIdx := 0;
    Itm := FTasks[p][ItmIdx];
    If FTasks[p].Count = 1 then
      PerItmTime := Max(FTicksPerASyncTask, integer(EndTime - GetTickCount64))
    else
      PerItmTime := FTicksPerASyncTask;
    repeat

      TheData  := Itm.Data;
      ThePrior := Itm.TaskPriorities;
      OrigPrior := ThePrior;
      IsDone   := False;
      Itm.CallBack(Itm.EditorId, Itm.TaskId, PerItmTime, TheData, IsDone, ThePrior);

      if IsDone then begin
        FTasks[p].Delete(ItmIdx);
        if FTasks[p].Count = 0 then
          break; // repeat
      end
      else begin
        if (ThePrior <> Itm.TaskPriorities) then begin
          Itm.TaskPriorities := ThePrior;
          Itm.Data := TheData;
          i := DoCompareTaskPrior(ThePrior, OrigPrior);
          if i < 0 then begin  // new prior is smaller
            if ItmIdx > 0 then begin
              FTasks[p].Delete(ItmIdx);
              FTasks[p].Insert(0,Itm); // still sorted // this is now the only item with this prior
              PerItmTime := Max(FTicksPerASyncTask, integer(EndTime - GetTickCount64));
              ItmIdx := 0;
            end
            else
              FTasks[p][ItmIdx] := Itm;
            Continue;
          end
          else begin
            if ItmIdx < FTasks[p].Count-1 then begin
              FTasks[p].Delete(ItmIdx);
              FTasks[p].Add(Itm);
              dec(ItmIdx);
            end
            else
              FTasks[p][ItmIdx] := Itm;
          end;
        end
        else
        if (TheData <> Itm.Data) then begin
          Itm.Data := TheData;
          FTasks[p][ItmIdx] := Itm;
        end;

        inc(ItmIdx);
      end;
      assert(FTasks[p].Count > 0, 'TLazEditASyncRunner.DoRunAsync: FTasks[p].Count > 0');

      if ItmIdx >= FTasks[p].Count then
        ItmIdx := 0;
      Itm := FTasks[p][ItmIdx];

      if (Itm.TaskPriorities <> OrigPrior) then begin
        if (ItmIdx > 0) then begin
          if ItmIdx = 1 then
            PerItmTime := Max(FTicksPerASyncTask, integer(EndTime - GetTickCount64));
          ItmIdx := 0;
          Itm := FTasks[p][ItmIdx];
        end
        else begin
          FTasks[p].MaybeSort;
          PerItmTime := FTicksPerASyncTask;
        end;
      end;
    until (FTasks[p].Count = 0) or (GetTickCount64 >= EndTime);

    if (GetTickCount64 >= EndTime) then break;
  end;

  MaybeStartRunning;
end;

procedure TLazEditASyncRunner.Release;
begin
  if Self = nil then
    exit;
  FAutoFree := (FEditors.Count > 0);
  StopRunning;
  if FAutoFree then
    exit;

  if Self = TheGlobalASyncRunner then
    TheGlobalASyncRunner := nil;
  Destroy;
end;

procedure TLazEditASyncRunner.RegisterEditor(AnEditorId: TLazEditEditorId;
  AnEditorPriorities: TTLazEditEditorPriority);
begin
  FEditors.Add(AnEditorId, AnEditorPriorities);
end;

procedure TLazEditASyncRunner.UnregisterEditor(AnEditorId: TLazEditEditorId);
var
  p: TTLazEditEditorPriority;
begin
  p := EditorPriority[AnEditorId];
  FEditors.Remove(AnEditorId);
  if not FAutoFree then
    EditorChangedPriority(AnEditorId, p, epCreated);
  if FAutoFree and (FEditors.Count = 0) then Destroy;
end;

constructor TLazEditASyncRunner.Create;
var
  p: TTLazEditEditorPriority;
begin
  FEditors := TASyncEditorList.Create;
  FEditors.Sorted := True;
  for p in TTLazEditEditorPriority do
    FTasks[p] := TASyncTaskList.Create;

  FTicksPerASyncLoop := TIME_PER_LOOP;
  FTicksPerASyncTask := TIME_PER_CALL;
  inherited Create;
end;

destructor TLazEditASyncRunner.Destroy;
var
  p: TTLazEditEditorPriority;
begin
  StopRunning;
  inherited Destroy;
  for p in TTLazEditEditorPriority do
    FTasks[p].Free;
  FEditors.Free;
end;

procedure TLazEditASyncRunner.AddOrReplaceTask(ACallBack: TLazEditTaskCallback;
  AnEditorId: TLazEditEditorId; ATaskPriorities: TTLazEditTaskPriorities; ATaskId: TLazEditTaskId;
  AData: Pointer);
var
  p: TTLazEditEditorPriority;
  i: Integer;
begin
  p := EditorPriority[AnEditorId];
  i := FTasks[p].IndexOf(ACallBack, AnEditorId, ATaskId);
  if i < 0 then
    FTasks[p].Add(ACallBack, AnEditorId, ATaskId, ATaskPriorities, AData)
  else
    FTasks[p].Update(i, ATaskPriorities, AData);
  StartRunning;
end;

procedure TLazEditASyncRunner.RemoveTask(ACallBack: TLazEditTaskCallback;
  AnEditorId: TLazEditEditorId; ATaskId: TLazEditTaskId);
var
  p: TTLazEditEditorPriority;
  i: Integer;
begin
  p := EditorPriority[AnEditorId];
  i := FTasks[p].IndexOf(ACallBack, AnEditorId, ATaskId);
  if i >= 0 then begin
    FTasks[p].Delete(i);
    if (i=0) then
      MaybeStopRunning;
  end;
end;

procedure TLazEditASyncRunner.RemoveAllTaskFor(ACallBack: TLazEditTaskCallback);
var
  p: TTLazEditEditorPriority;
  i: Integer;
begin
  for p in TTLazEditEditorPriority do begin
    i := FTasks[p].IndexOf(ACallBack);
    while i >= 0 do begin
      FTasks[p].Delete(i);
      i := FTasks[p].IndexOf(ACallBack, i);
    end;
  end;
  MaybeStopRunning;
end;

finalization
  TheGlobalASyncRunner.Release; // editors may be destroyed in async by ReleaseComponents
end.

