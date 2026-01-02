unit TestAsyncRunner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazEditASyncRunner, Forms, fpcunit, testutils, testregistry;

type

  { TTestASyncRunner }

  TTestASyncRunner = class(TTestCase)
  private
    FTaskRun: String;
    procedure DoTask(AnEditorId: TLazEditEditorId; ATaskId: TLazEditTaskId; AMaxTime: integer; var AData: Pointer;
      var ADone: boolean; var APriorities: TTLazEditTaskPriorities);
    procedure DoTask2(AnEditorId: TLazEditEditorId; ATaskId: TLazEditTaskId; AMaxTime: integer; var AData: Pointer;
      var ADone: boolean; var APriorities: TTLazEditTaskPriorities);
  published
    procedure TestAddRemove;
    procedure TestOrder;
    procedure TestOrder2;
    procedure TestChangePrior;
    procedure TestChangeEditor;
  end;

implementation

const
  E1 = pointer(1);
  E2 = pointer(2);
  T1 = pointer(1);
  T2 = pointer(2);
  T3 = pointer(3);
  T4 = pointer(4);
  T5 = pointer(5);
  TChange1 = pointer(256);
  TChange2 = pointer(257);
  TMove1   = pointer(512);
  TMove2   = pointer(513);
  TMove3   = pointer(514);
  TMoveX1   = pointer(520);
  TMoveX2   = pointer(521);

procedure TTestASyncRunner.DoTask(AnEditorId: TLazEditEditorId; ATaskId: TLazEditTaskId;
  AMaxTime: integer; var AData: Pointer; var ADone: boolean;
  var APriorities: TTLazEditTaskPriorities);
begin
  if ATaskId = TMove1 then APriorities := [tpPaintText];
  if ATaskId = TMove2 then APriorities := [tpPaintText];
  if ATaskId = TMove3 then APriorities := [];
  if (ATaskId = TMoveX1) and (FTaskRun<>'') then APriorities := [tpPaintText];
  if (ATaskId = TMoveX2) and (FTaskRun<>'') then APriorities := [tpPaintText];

  FTaskRun := FTaskRun + IntToStr(PtrInt(AData) and 255) +',';

  if ATaskId = TChange1 then AData := pointer(PtrInt(AData) + 1);
  if ATaskId = TChange2 then AData := pointer(PtrInt(AData) + 1);

  if PtrInt(AData) and $FF00 <> 0 then
    AData := pointer(PtrInt(AData) - $100)
  else
    ADone := True;
end;

procedure TTestASyncRunner.DoTask2(AnEditorId: TLazEditEditorId; ATaskId: TLazEditTaskId;
  AMaxTime: integer; var AData: Pointer; var ADone: boolean;
  var APriorities: TTLazEditTaskPriorities);
begin
  FTaskRun := FTaskRun + IntToStr(PtrInt(AData) and 255) +',';
  if PtrInt(AData) and $FF00 <> 0 then
    AData := pointer(PtrInt(AData) - $100)
  else
    ADone := True;
end;

procedure TTestASyncRunner.TestAddRemove;
var
  Runner: TLazEditASyncRunner;
begin
  Runner := TLazEditASyncRunner.Create;
  Runner.TicksPerASyncLoop := 0;

  Runner.RegisterEditor(E1, epHasHandle);

  Runner.AddOrReplaceTask(@DoTask,  E1, [tpPaintExtra],     T1,  pointer(1 + $F00));
  FTaskRun := '';

  Application.Idle(False); // run async tasks
  AssertEquals('', '1,', FTaskRun);
  Application.Idle(False); // run async tasks
  AssertEquals('', '1,1,', FTaskRun);

  // all wrong
  Runner.RemoveTask(@DoTask, E2, T1);
  Runner.RemoveTask(@DoTask, E1, T2);
  Runner.RemoveTask(@DoTask2, E1, T1);

  Application.Idle(False); // run async tasks
  AssertEquals('', '1,1,1,', FTaskRun);

  Runner.RemoveTask(@DoTask, E1, T1); // remove

  Application.Idle(False); // run async tasks
  AssertEquals('', '1,1,1,', FTaskRun);

  Runner.AddOrReplaceTask(@DoTask,  E1, [tpPaintExtra],     T1,  pointer(1 + $F00));
  Runner.AddOrReplaceTask(@DoTask,  E1, [tpPaintExtra],     T2,  pointer(1 + $F00));
  Runner.AddOrReplaceTask(@DoTask,  E2, [tpPaintExtra],     T1,  pointer(1 + $F00));
  Runner.AddOrReplaceTask(@DoTask2, E1, [],                 T1,  pointer(2 + $F00));
  FTaskRun := '';

  Application.Idle(False); // run async tasks
  AssertEquals('', '1,', FTaskRun);

  Runner.RemoveAllTaskFor(@DoTask);

  Application.Idle(False); // run async tasks
  AssertEquals('', '1,2,', FTaskRun);

  Runner.Free;
end;

procedure TTestASyncRunner.TestOrder;
var
  Runner: TLazEditASyncRunner;
begin
  Runner := TLazEditASyncRunner.Create;
  Runner.TicksPerASyncLoop := MaxInt-1;

  Runner.RegisterEditor(E1, epHasHandle);
  Runner.RegisterEditor(E2, epHasFocused);

  Runner.AddOrReplaceTask(@DoTask,  E1, [],            T1,  pointer(1));
  Runner.AddOrReplaceTask(@DoTask,  E2, [],            nil, pointer(2));   // run first by editor prior
  Runner.AddOrReplaceTask(@DoTask2, E1, [tpPriorTop],  T1,  pointer(3));
  Runner.AddOrReplaceTask(@DoTask,  E1, [tpPriorHigh], T2,  pointer(4+256)); // run twice
  Runner.AddOrReplaceTask(@DoTask,  E1, [tpPaintText], T3,  pointer(5)); // run 2nd
  FTaskRun := '';
  Application.Idle(False); // run async tasks

  AssertEquals('', '2,5,3,4,4,1,', FTaskRun);

  Runner.Free;
end;

procedure TTestASyncRunner.TestOrder2;
var
  Runner: TLazEditASyncRunner;
begin
  Runner := TLazEditASyncRunner.Create;
  Runner.TicksPerASyncLoop := 0;

  Runner.RegisterEditor(E1, epHasHandle);
  Runner.RegisterEditor(E2, epHasFocused);

  Runner.AddOrReplaceTask(@DoTask,  E1, [],            T1,  pointer(1+$F00));
  Runner.AddOrReplaceTask(@DoTask,  E1, [tpPriorTop],  T2,  pointer(2+$F00));
  Runner.AddOrReplaceTask(@DoTask,  E1, [tpPriorHigh], T3,  pointer(3+$F00));
  Runner.AddOrReplaceTask(@DoTask,  E1, [tpPaintText], T4,  pointer(4+$F00));
  FTaskRun := '';

  Application.Idle(False); // run async tasks
  AssertEquals('', '4,', FTaskRun);
  Application.Idle(False); // run async tasks
  AssertEquals('', '4,4,', FTaskRun);

  Runner.TaskPriority[@DoTask, E1, T4] := [];

  Application.Idle(False); // run async tasks
  AssertEquals('', '4,4,2,', FTaskRun);

  Runner.TaskPriority[@DoTask, E1, T1] := [tpPaintText, tpPriorHigh];

  Application.Idle(False); // run async tasks
  AssertEquals('', '4,4,2,1,', FTaskRun);

  Runner.AddOrReplaceTask(@DoTask, E1, [], T1, pointer(1+$F00));

  Application.Idle(False); // run async tasks
  AssertEquals('', '4,4,2,1,2,', FTaskRun);


  Runner.Free;
end;

procedure TTestASyncRunner.TestChangePrior;
var
  Runner: TLazEditASyncRunner;
begin
  Runner := TLazEditASyncRunner.Create;
  Runner.TicksPerASyncLoop := MaxInt-1;

  Runner.RegisterEditor(E1, epHasHandle);
  Runner.RegisterEditor(E2, epHasFocused);

  Runner.AddOrReplaceTask(@DoTask,  E1, [tpPaintExtra],     TChange1,  pointer(1 + 512)); // run 3 times
  Runner.AddOrReplaceTask(@DoTask,  E1, [tpPaintText],      TMove3,    pointer(10 + 256)); // move to end for 2nd run
  Runner.AddOrReplaceTask(@DoTask,  E1, [tpPaintOverview],  TMove2,    pointer(20 + 256)); // movo te front / alreayd there
  Runner.AddOrReplaceTask(@DoTask,  E1, [tpPriorHigh],      TMove1,    pointer(30 + 256)); // move to front / alreayd there
  FTaskRun := '';
  Application.Idle(False); // run async tasks

  AssertEquals('', '10,1,2,3,20,20,30,30,10,', FTaskRun);

  Runner.AddOrReplaceTask(@DoTask,  E1, [tpPaintExtra],  TMoveX1,    pointer(10 + 256));
  Runner.AddOrReplaceTask(@DoTask,  E1, [tpPaintExtra],  TMoveX2,    pointer(20 + 256));
  FTaskRun := '';
  Application.Idle(False); // run async tasks

  if FTaskRun[1] = '1' then
    AssertEquals('', '10,20,20,10,', FTaskRun)
  else
    AssertEquals('', '20,10,10,20,', FTaskRun);


  Runner.Free;
end;

procedure TTestASyncRunner.TestChangeEditor;
var
  Runner: TLazEditASyncRunner;
begin
  Runner := TLazEditASyncRunner.Create;
  Runner.TicksPerASyncLoop := 0;  // single item run

  Runner.RegisterEditor(E1, epIsVisible);
  Runner.RegisterEditor(E2, epHasFocused);

  Runner.AddOrReplaceTask(@DoTask,  E1, [tpPaintText], TChange1, pointer(10 + $800)); // run 3 times
  Runner.AddOrReplaceTask(@DoTask,  E2, [tpPaintText], TChange2, pointer(20 + $800)); // move to end for 2nd run
  FTaskRun := '';

  Application.Idle(False); // run async tasks
  AssertEquals('', '20,', FTaskRun);
  Application.Idle(False); // run async tasks
  AssertEquals('', '20,21,', FTaskRun);

  Runner.EditorPriority[E2] := epHasHandle;
  Application.Idle(False); // run async tasks
  AssertEquals('', '20,21,10,', FTaskRun);
  Application.Idle(False); // run async tasks
  AssertEquals('', '20,21,10,11,', FTaskRun);
  Application.Idle(False); // run async tasks
  AssertEquals('', '20,21,10,11,12,', FTaskRun);

  Runner.UnregisterEditor(E1);
  Application.Idle(False); // run async tasks
  AssertEquals('', '20,21,10,11,12,22,', FTaskRun);

  Runner.Free;
end;



initialization

  RegisterTest(TTestASyncRunner);
end.

