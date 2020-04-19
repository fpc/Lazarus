unit TestListView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, math, fpcunit, Interfaces, LCLType, LCLIntf, Forms, ComCtrls,
  Controls, StdCtrls, LMessages, LCLProc, Menus, testglobals, Keyboard,
  MouseAndKeyInput, MouseInputIntf, LazLogger
  {$IFDEF WINDOWS} ,JwaWinUser, WinMouseInput {$ENDIF}
  ;

const
  // vsIcon, vsSmallIcon, vsList, vsReport
  MinColumnPerStyle: array [TViewStyle] of integer = (0, 0, 0, 1);
  MaxColumnPerStyle: array [TViewStyle] of integer = (0, 0, 2, 2);

  ts_e = [];
  ts_s = [ssShift];
  ts_c = [ssCtrl];
  ts_sc= [ssShift, ssCtrl];
  TestShiftStates: array [0..2] of TShiftState = (
    ts_e, ts_s, ts_c
  );

type

  TLvTestEventType = (
    evMouseDown, evMouseUp, evClick, evDblClick, evContextPop,
    evStartDrag, evEndDrag, evMoveDrag,
    evStoreSelection,
    evMsgContext,
    evMsgLDown, evMsgLUp, evMsgLDbl,
    evMsgRDown, evMsgRUp, evMsgRDbl,
    evMenu, evMenuItem,
    evMarker
  );

const
  TSelMaskMarker = 31;
type
  TSelMaskVal = 0..TSelMaskMarker;
  TSelMask = set of TSelMaskVal;
const
  NO_SEL = TSelMask([]);
type

  TLvTestEvent = record
    Event: TLvTestEventType;
    x, y, ItemIdx: Integer;
    Shift: TShiftState;
    SelMask: TSelMask;
    SelIdx: integer;
  end;

  TClickInnerPos = (ipTopLeft, ipCenter, ipRightBottom, ipOutsideRight, ipOutsideBelow);
  TClickPos = record
    ItemIdx, SubIdx: integer;
    ItemPart: TDisplayCode;
    ItemInnerPos: TClickInnerPos;
    //XOffs, YOffs: Integer;
  end;
  //TDisplayCode = (drBounds, drIcon, drLabel, drSelectBounds);

  { TTestMouseInput }

  {$IFDEF WINDOWS}
  TTestMouseInput = class(WinMouseInput.TWinMouseInput)
  // no processmessages => so it can run in thread
  private
    FInput: Array of JwaWinUser.TInput;
    procedure DoSendInput;
    procedure AddKeyInput(AFlag: DWORD; AKey: Word);
    procedure SendMouseInput(AFlag: DWORD);
    procedure SendMouseInput(X, Y: Integer);
    procedure ApplyKey(Shift: TShiftState);
    procedure UnApplyKey(Shift: TShiftState);
  public
    procedure Down(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Down(Button: TMouseButton; Shift: TShiftState; Control: TControl; X, Y: Integer);
    procedure Move(Shift: TShiftState; X, Y: Integer);
    procedure Move(Shift: TShiftState; Control: TControl; X, Y: Integer);
    procedure Up(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Up(Button: TMouseButton; Shift: TShiftState; Control: TControl; X, Y: Integer);
  end;
  {$ENDIF}

  { TListViewForTest }

  TListViewForTest = class(TListView)
  protected
    procedure WMContextMenu(var Message: TLMContextMenu);     message LM_CONTEXTMENU;
    procedure WMLButtonDown(var Message: TLMLButtonDown);     message LM_LBUTTONDOWN;
    procedure WMLButtonUp(var Message: TLMLButtonUp);         message LM_LBUTTONUP;
    procedure WMLButtonDBLCLK(var Message: TLMLButtonDblClk); message LM_LBUTTONDBLCLK;
    procedure WMRButtonDown(var Message: TLMRButtonDown);     message LM_RBUTTONDOWN;
    procedure WMRButtonUp(var Message: TLMRButtonUp);         message LM_RBUTTONUP;
    procedure WMRButtonDBLCLK(var Message: TLMRButtonDblClk); message LM_RBUTTONDBLCLK;
  end;

  TClickPoint = record
    x, y: Integer;
    btn: TMouseButton;
  end;

  { TTestThread }

  TTestThread = class(TTHread)
  private
    FButton: TMouseButton;
    FPos, FupPos: TPoint;
    FExtraPos: Array of TPoint;
    FClick2Pos: Array of TClickPoint;
    FShiftDown, FShiftUp: TShiftState;

    FWaitForMainProcessMessages: cardinal;
    FDone, FStepDone: Cardinal;

    FRunMode: (rmClick, rmClickMove);

    procedure WaitForMain;
    function  GotSignalAfterProcessMessages: Boolean;
  public
    constructor Create(Button: TMouseButton; Pos: TPoint; ShiftDown, ShiftUp: TShiftState);
    constructor Create(Button: TMouseButton; DownPos, UpPos: TPoint; ShiftDown, ShiftUp: TShiftState);
    constructor Create(Button: TMouseButton; DownPos, UpPos: TPoint; ExtraPos: array of TPoint; ShiftDown, ShiftUp: TShiftState);
    constructor Create(Button: TMouseButton; DownPos, UpPos: TPoint; ExtraPos: array of TPoint; Click2Pos: array of TClickPoint; ShiftDown, ShiftUp: TShiftState);

    class procedure Run(Button: TMouseButton; Pos: TPoint; ShiftDown, ShiftUp: TShiftState);
    class procedure Run(Button: TMouseButton; DownPos, UpPos: TPoint; ShiftDown, ShiftUp: TShiftState);
    class procedure Run(Button: TMouseButton; DownPos, UpPos: TPoint; ExtraPos: array of TPoint; ShiftDown, ShiftUp: TShiftState);
    class procedure Run(Button: TMouseButton; DownPos, UpPos: TPoint; ExtraPos: array of TPoint; Click2Pos: array of TClickPoint; ShiftDown, ShiftUp: TShiftState);


    procedure Execute; override;


  public // from main thread
    procedure MainThreadLoop;
    procedure SetSignalAfterProcessMessages;
    function StepDone: Boolean;
    function IsDone: Boolean;
  end;

  { TTestListView }

  TTestListView = class(TTestCase)
  private
    FForm : TForm;
    FButton: TButton;
    FListView: TListViewForTest;
    FPopUp: TPopupMenu;
    FTestEvents: array of TLvTestEvent;
    FTestError: String;
    FInDrag: (idFalse, idStarted, idTrue);

    procedure LvContextPop(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure Lvkeydown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Lvkeyup(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure LvMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LvMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure LvMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LvClick(Sender: TObject);
    procedure LvDblClick(Sender: TObject);
    procedure LvStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure LvDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure LvEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure OnMenuItemClick(Sender: TObject);
    procedure OnMenuPopUp(Sender: TObject);

  protected
    property  Form: TForm read FForm;
    property  ListView: TListViewForTest read FListView;

    function GetSelMask: TSelMask;
    function GetSelIdx: Integer;
    function ItemIdx(X, Y: Integer): Integer;
    function ItemXY(Idx: TClickPos): TPoint;
    function ItemScreenXY(Idx: TClickPos): TPoint;

    procedure MouseToIdx(AIdx: TClickPos; Shift: TShiftState = []; AXOffs: integer = 0; AYOffs: integer = 0);
    procedure MouseDownOnIdx(Button: TMouseButton; AIdx: TClickPos; Shift: TShiftState = []);
    procedure MouseUpOnIdx(Button: TMouseButton; AIdx: TClickPos; Shift: TShiftState = []);
    procedure ClickButton;

    procedure AddTestEvent(AnEvent: TLvTestEvent);
    procedure ClearTestEvents;
    function  CheckTestEvent(AName: string; AnEvent: TLvTestEvent; AnIndex: Integer): Boolean;
    procedure CheckTestEvents(AName: string; AnEvents: array of TLvTestEvent);

    procedure StoreSelectionState;
  public
    procedure SetUp; override;
    procedure TearDown; override;
    procedure RecreateForm;
    procedure CreateListView(
      AViewStyle: TViewStyle = vsList;
      AColumnCnt: Integer = 0;
      ADrgMode: TDragMode = dmManual;
      MultiSel: Boolean = False;
      RowSel: Boolean = False;
      ReadOnly: Boolean = True
  );
    function  CreatePopUp: TPopupMenu;
    procedure AttachPopUp;
  published
    procedure TestClick;
    procedure TestClickRight;
    procedure TestClickNoneDragJitter;
    procedure TestDrag;
    procedure TestPopUp;
  end;

implementation
var
{$IFDEF WINDOWS}
  FTestMouseInput: TTestMouseInput;
{$ELSE}
  FTestMouseInput: TMouseInput;
{$ENDIF}

  TheTestCase: TTestListView;

operator := (p: TPoint): TClickPoint;
begin
  result.x := p.x;
  result.y := p.y;
  result.btn := mbLeft;
end;

function RBtn(p: TPoint): TClickPoint;
begin
  result := p;
  result.btn := mbRight;
end;

function cp(AIdx, ASubIdx: Integer; APart: TDisplayCode = drLabel; APos: TClickInnerPos = ipTopLeft): TClickPos;
begin
  Result.ItemIdx := AIdx;
  Result.SubIdx := ASubIdx;
  Result.ItemPart := APart;
  Result.ItemInnerPos := APos;
end;

function cp(AIdx: Integer; APart: TDisplayCode = drLabel; APos: TClickInnerPos = ipTopLeft): TClickPos;
begin
  Result := cp(AIdx, -1, APart, APos);
end;

operator := (a: Integer): TClickPos;
begin
  Result := cp(a);
end;

function ShiftToStr(AShift: TShiftState): string;
var
  i: TShiftStateEnum;
  s: string;
begin
  Result := '';
  for i := low(TShiftState) to high(TShiftState) do
    if (i in [ssShift, ssAlt, ssCtrl, ssLeft, ssRight, ssMiddle, ssDouble]) then
    if (i in AShift) then
      begin
        if Result <> '' then
          Result := Result + ',';
        writestr(s, i);
        Result := Result + s;
      end;
end;

function ev(t: TLvTestEventType;
  x, y: Integer;
  ItemIdx: Integer; // for expectation only
  Shift: TShiftState = [];
  SelMask: TSelMask = [TSelMaskMarker]; SelIdx: integer = -2
): TLvTestEvent; overload;
begin
  Result.Event := t;
  Result.x:= x;
  Result.y:= y;
  Result.ItemIdx := ItemIdx;
  Result.Shift := Shift;
  Result.SelMask := SelMask;
  Result.SelIdx := SelIdx;
end;

function ev(t: TLvTestEventType;
  xy: TPoint;
  ItemIdx: Integer; // for expectation only
  Shift: TShiftState = [];
  SelMask: TSelMask = [TSelMaskMarker]; SelIdx: integer = -2
): TLvTestEvent; overload;
begin
  Result := ev(t, xy.x, xy.y, ItemIdx, Shift, SelMask, SelIdx);
end;

function ev(t: TLvTestEventType;
  ItemIdx: Integer;
  Shift: TShiftState = [];
  SelMask: TSelMask = [TSelMaskMarker]; SelIdx: integer = -2
): TLvTestEvent; overload;
begin
  Result := ev(t, -1, -1, ItemIdx, Shift, SelMask, SelIdx);
end;

function ev(t: TLvTestEventType;
  x, y: Integer;
  Shift: TShiftState;
  SelMask: TSelMask = [TSelMaskMarker]; SelIdx: integer = -2
): TLvTestEvent; overload;
begin
  Result := ev(t, x, y, -2, Shift, SelMask, SelIdx);
end;

function ev(t: TLvTestEventType;
  Shift: TShiftState;
  SelMask: TSelMask = [TSelMaskMarker]; SelIdx: integer = -2
): TLvTestEvent; overload;
begin
  Result := ev(t, -1, -1, -2, Shift, SelMask, SelIdx);
end;

function ev(t: TLvTestEventType;
  SelMask: TSelMask = [TSelMaskMarker]; SelIdx: integer = -2
): TLvTestEvent; overload;
begin
  Result := ev(t, -1, -1, -2, [], SelMask, SelIdx);
end;

operator := (t: TLvTestEventType): TLvTestEvent;
begin
  Result := ev(t);
end;

function AddToPoint(p :TPoint; x,y: Integer): TPoint;
begin
  Result.x := p.x + x;
  Result.y := p.y + y;
end;

function dbgs(t: TLvTestEventType): string; overload;
begin
  WriteStr(Result, t);
end;

function dbgs(t: TViewStyle): string; overload;
begin
  WriteStr(Result, t);
end;

function dbgs(t: TDragMode): string; overload;
begin
  WriteStr(Result, t);
end;

function dbgs(t: Boolean): string; overload;
begin
  WriteStr(Result, t);
end;

function TextEv(t: TLvTestEvent): String;
var
  sm: String;
  i: Integer;
begin
  sm := '';
  if not(TSelMaskMarker in t.SelMask) then
    for i := 0 to TSelMaskMarker-1 do
      if (i in t.SelMask) then
        sm := sm + IntToStr(i) + ',';
  Result := Format('%-15s [%s]  XY: (%d, %d) ItmIdx: %d   Sel: %d [%s]',
    [dbgs(t.Event), ShiftToStr(t.Shift), t.x,t.y, t.ItemIdx, t.SelIdx, sm ]
  );
end;
procedure DumpEv(t: TLvTestEvent);
begin
  DebugLn(TextEv(t));
end;

{ TListViewForTest }

procedure TListViewForTest.WMContextMenu(var Message: TLMContextMenu);
begin
  TheTestCase.AddTestEvent(evMsgContext);
  inherited WMContextMenu(Message);
end;

procedure TListViewForTest.WMLButtonDown(var Message: TLMLButtonDown);
begin
  TheTestCase.AddTestEvent(evMsgLDown);
  inherited WMLButtonDown(Message);
end;

procedure TListViewForTest.WMLButtonUp(var Message: TLMLButtonUp);
begin
  TheTestCase.AddTestEvent(evMsgLUp);
  inherited WMLButtonUp(Message);
end;

procedure TListViewForTest.WMLButtonDBLCLK(var Message: TLMLButtonDblClk);
begin
  TheTestCase.AddTestEvent(evMsgLDbl);
  inherited WMLButtonDBLCLK(Message);
end;

procedure TListViewForTest.WMRButtonDown(var Message: TLMRButtonDown);
begin
  TheTestCase.AddTestEvent(evMsgRDown);
  inherited WMRButtonDown(Message);
end;

procedure TListViewForTest.WMRButtonUp(var Message: TLMRButtonUp);
begin
  TheTestCase.AddTestEvent(evMsgRUp);
  inherited WMRButtonUp(Message);
end;

procedure TListViewForTest.WMRButtonDBLCLK(var Message: TLMRButtonDblClk);
begin
  TheTestCase.AddTestEvent(evMsgRDbl);
  inherited WMRButtonDBLCLK(Message);
end;

{ TTestThread }

constructor TTestThread.Create(Button: TMouseButton; Pos: TPoint; ShiftDown,
  ShiftUp: TShiftState);
begin
  FButton := Button;
  FPos := Pos;
  FupPos := Pos;
  FShiftDown := ShiftDown;
  FShiftUp := ShiftUp;
  FRunMode := rmClick;
  inherited Create(False);
end;

constructor TTestThread.Create(Button: TMouseButton; DownPos, UpPos: TPoint;
  ShiftDown, ShiftUp: TShiftState);
begin
  FButton := Button;
  FPos := DownPos;
  FupPos := UpPos;
  FShiftDown := ShiftDown;
  FShiftUp := ShiftUp;
  FRunMode := rmClickMove;
  inherited Create(False);
end;

constructor TTestThread.Create(Button: TMouseButton; DownPos, UpPos: TPoint;
  ExtraPos: array of TPoint; ShiftDown, ShiftUp: TShiftState);
begin
  FButton := Button;
  FPos := DownPos;
  FupPos := UpPos;
  SetLength(FExtraPos, Length(ExtraPos));
  Move(ExtraPos[0], FExtraPos[0], SizeOf(FExtraPos[0])*Length(ExtraPos));
  FShiftDown := ShiftDown;
  FShiftUp := ShiftUp;
  FRunMode := rmClickMove;
  inherited Create(False);
end;

constructor TTestThread.Create(Button: TMouseButton; DownPos, UpPos: TPoint;
  ExtraPos: array of TPoint; Click2Pos: array of TClickPoint; ShiftDown,
  ShiftUp: TShiftState);
begin
  FButton := Button;
  FPos := DownPos;
  FupPos := UpPos;
  SetLength(FExtraPos, Length(ExtraPos));
  Move(ExtraPos[0], FExtraPos[0], SizeOf(FExtraPos[0])*Length(ExtraPos));
  SetLength(FClick2Pos, Length(Click2Pos));
  Move(Click2Pos[0], FClick2Pos[0], SizeOf(FClick2Pos[0])*Length(Click2Pos));
  FShiftDown := ShiftDown;
  FShiftUp := ShiftUp;
  FRunMode := rmClickMove;
  inherited Create(False);
end;

class procedure TTestThread.Run(Button: TMouseButton; Pos: TPoint; ShiftDown,
  ShiftUp: TShiftState);
var
  s: TTestThread;
begin
  s := TTestThread.Create(Button, Pos, ShiftDown, ShiftUp);
  s.MainThreadLoop;
  s.Destroy;
end;

class procedure TTestThread.Run(Button: TMouseButton; DownPos, UpPos: TPoint;
  ShiftDown, ShiftUp: TShiftState);
var
  s: TTestThread;
begin
  s := TTestThread.Create(Button, DownPos, UpPos, ShiftDown, ShiftUp);
  s.MainThreadLoop;
  s.Destroy;
end;

class procedure TTestThread.Run(Button: TMouseButton; DownPos, UpPos: TPoint;
  ExtraPos: array of TPoint; ShiftDown, ShiftUp: TShiftState);
var
  s: TTestThread;
begin
  s := TTestThread.Create(Button, DownPos, UpPos, ExtraPos, ShiftDown, ShiftUp);
  s.MainThreadLoop;
  s.Destroy;
end;

class procedure TTestThread.Run(Button: TMouseButton; DownPos, UpPos: TPoint;
  ExtraPos: array of TPoint; Click2Pos: array of TClickPoint; ShiftDown,
  ShiftUp: TShiftState);
var
  s: TTestThread;
begin
  s := TTestThread.Create(Button, DownPos, UpPos, ExtraPos, Click2Pos, ShiftDown, ShiftUp);
  s.MainThreadLoop;
  s.Destroy;
end;

procedure TTestThread.Execute;
var
  i: Integer;
begin
  FTestMouseInput.Down(FButton, FShiftDown, FPos.x, FPos.y);
  WaitForMain;

  for i := 0 to Length(FExtraPos) - 1 do begin
    FTestMouseInput.Move([], FExtraPos[i].x, FExtraPos[i].y);
    WaitForMain;
  end;

  if FRunMode in [rmClickMove] then begin
    FTestMouseInput.Move([], FUpPos.x, FUpPos.y);
    WaitForMain;
  end;

  FTestMouseInput.Up(FButton, FShiftUp, FUpPos.x, FupPos.y);
  WaitForMain;

  FTestMouseInput.UnApplyKey([ssShift, ssCtrl]);
  WaitForMain;

  for i := 0 to length(FClick2Pos) - 1 do begin
    FTestMouseInput.Down(FClick2Pos[i].btn, [], FClick2Pos[i].x, FClick2Pos[i].y);
    WaitForMain;
    FTestMouseInput.Up(FClick2Pos[i].btn, [], FClick2Pos[i].x, FClick2Pos[i].y);
    WaitForMain;
  end;

  InterLockedExchange(FDone, 1);
end;

procedure TTestThread.MainThreadLoop;
var
  t: Boolean;
begin
  while not IsDone do begin
    t := StepDone;
    Application.ProcessMessages;
    if t then
      SetSignalAfterProcessMessages;
  end;
  WaitFor;
  Application.ProcessMessages;
end;

procedure TTestThread.SetSignalAfterProcessMessages;
begin
  InterLockedExchange(FWaitForMainProcessMessages, 1);
end;

function TTestThread.StepDone: Boolean;
begin
  Result := InterLockedExchange(FStepDone, 0) = 1;
end;

function TTestThread.IsDone: Boolean;
begin
  Result := InterLockedExchange(FDone, 0) = 1;
end;

procedure TTestThread.WaitForMain;
var
  t: QWord;
begin
  InterLockedExchange(FWaitForMainProcessMessages, 0); // clear
  InterLockedExchange(FStepDone, 1);
  sleep(5);
  t := GetTickCount64;
  while (not GotSignalAfterProcessMessages) and (GetTickCount64 - t < 200) do
    sleep(5);
//if not (GetTickCount64 - t < 200) then debugln('&&&&&&&&&&&&&&& TimeOut');
  InterLockedExchange(FStepDone, 0);
end;

function TTestThread.GotSignalAfterProcessMessages: Boolean;
begin
  Result := InterLockedExchange(FWaitForMainProcessMessages, 0) = 1;
end;

{$IFDEF WINDOWS}
{ TTestMouseInput }

procedure TTestMouseInput.DoSendInput;
var
  i: int64;
begin
  i := SendInput(length(FInput), @FInput[0], SizeOf(JwaWinUser.TInput));
  if i <> Length(FInput)then DebugLn(['***** ERROR: SendInput failed: ', i ,' of ', Length(FInput)]);
end;

procedure TTestMouseInput.AddKeyInput(AFlag: DWORD; AKey: Word);
var
  l: Integer;
begin
  l := length(FInput);
  SetLength(FInput, l+1);
  FillChar(FInput[l], SizeOf(FInput[0]), 0);
  FInput[l].type_ := JwaWinUser.INPUT_KEYBOARD;
  FInput[l].ki.dwFlags := AFlag;
  FInput[l].ki.wVk := AKey;
  FInput[l].ki.time := GetTickCount64 + l;
end;

procedure TTestMouseInput.SendMouseInput(AFlag: DWORD);
var
  l: Integer;
begin
  l := length(FInput);
  SetLength(FInput, l+1);
  FillChar(FInput[l], SizeOf(FInput[0]), 0);
  FInput[l].type_ := JwaWinUser.INPUT_MOUSE;
  FInput[l].mi.dwFlags := AFlag;
  FInput[l].mi.mouseData := 0;
end;

procedure TTestMouseInput.SendMouseInput(X, Y: Integer);
var
  l: Integer;
begin
  l := length(FInput);
  SetLength(FInput, l+1);
  FillChar(FInput[l], SizeOf(FInput[0]), 0);
  FInput[l].type_ := JwaWinUser.INPUT_MOUSE;
  FInput[l].mi.dx := MulDiv(X, 65535, Screen.Width - 1); // screen horizontal coordinates: 0 - 65535
  FInput[l].mi.dy := MulDiv(Y, 65535, Screen.Height - 1); // screen vertical coordinates: 0 - 65535
  FInput[l].mi.dwFlags := JwaWinUser.MOUSEEVENTF_MOVE or JwaWinUser.MOUSEEVENTF_ABSOLUTE;
end;

procedure TTestMouseInput.ApplyKey(Shift: TShiftState);
begin
  if ssCtrl in Shift  then AddKeyInput(0, VK_CONTROL);
  if ssAlt in Shift   then AddKeyInput(0, VK_MENU);
  if ssShift in Shift then AddKeyInput(0, VK_SHIFT);
end;

procedure TTestMouseInput.UnApplyKey(Shift: TShiftState);
begin
  if ssCtrl in Shift  then AddKeyInput(JwaWinUser.KEYEVENTF_KEYUP, VK_CONTROL);
  if ssAlt in Shift   then AddKeyInput(JwaWinUser.KEYEVENTF_KEYUP, VK_MENU);
  if ssShift in Shift then AddKeyInput(JwaWinUser.KEYEVENTF_KEYUP, VK_SHIFT);
end;

procedure TTestMouseInput.Down(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  Flag: DWORD;
begin
  FInput := nil;

  ApplyKey(Shift);

  case Button of
    mbRight: Flag := MOUSEEVENTF_RIGHTDOWN;
    mbMiddle: Flag := MOUSEEVENTF_MIDDLEDOWN;
  else
    Flag := MOUSEEVENTF_LEFTDOWN;
  end;
  SendMouseInput(x, y);
  SendMouseInput(Flag);

  UnApplyKey(Shift);

  DoSendInput;
end;

procedure TTestMouseInput.Down(Button: TMouseButton; Shift: TShiftState;
  Control: TControl; X, Y: Integer);
var
  P: TPoint;
begin
  P := Control.ClientToScreen(Point(X, Y));
  Down(Button, Shift, p.x, p.y);
end;

procedure TTestMouseInput.Move(Shift: TShiftState; X, Y: Integer);
begin
  FInput := nil;
  SendMouseInput(x, y);
  DoSendInput;
end;

procedure TTestMouseInput.Move(Shift: TShiftState; Control: TControl; X,
  Y: Integer);
var
  P: TPoint;
begin
  P := Control.ClientToScreen(Point(X, Y));
  FInput := nil;
  SendMouseInput(p.x, p.y);

  DoSendInput;
end;

procedure TTestMouseInput.Up(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  Flag: DWORD;
begin
  FInput := nil;

  ApplyKey(Shift);

  case Button of
    mbRight: Flag := MOUSEEVENTF_RIGHTUP;
    mbMiddle: Flag := MOUSEEVENTF_MIDDLEUP;
  else
    Flag := MOUSEEVENTF_LEFTUP;
  end;
  SendMouseInput(x, y);
  SendMouseInput(Flag);

  UnApplyKey(Shift);

  DoSendInput;
end;

procedure TTestMouseInput.Up(Button: TMouseButton; Shift: TShiftState;
  Control: TControl; X, Y: Integer);
var
  P: TPoint;
begin
  P := Control.ClientToScreen(Point(X, Y));
  Up(Button, Shift, p.X, p.Y);
end;
{$ENDIF}

{ TTestListView }

procedure TTestListView.LvMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  AddTestEvent(ev(evMouseDown, x, y, Shift, GetSelMask, GetSelIdx));
end;

procedure TTestListView.Lvkeydown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
end;

procedure TTestListView.LvContextPop(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin
  AddTestEvent(ev(evContextPop, MousePos.x, MousePos.y, [], GetSelMask, GetSelIdx));
end;

procedure TTestListView.Lvkeyup(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
end;

procedure TTestListView.LvMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if (FInDrag <> idFalse) and (Screen.RealCursor <> crDefault) then begin
    if FInDrag = idStarted then
      AddTestEvent(ev(evStartDrag, GetSelMask, GetSelIdx))
    else
      AddTestEvent(ev(evMoveDrag, GetSelMask, GetSelIdx));
    FInDrag := idTrue;
  end;
end;

procedure TTestListView.LvMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  AddTestEvent(ev(evMouseUp, x, y, Shift, GetSelMask, GetSelIdx));
end;

procedure TTestListView.LvClick(Sender: TObject);
begin
  AddTestEvent(ev(evClick, GetSelMask, GetSelIdx));
end;

procedure TTestListView.LvDblClick(Sender: TObject);
begin
  AddTestEvent(ev(evDblClick, GetSelMask, GetSelIdx));
end;

procedure TTestListView.LvStartDrag(Sender: TObject; var DragObject: TDragObject);
begin
  // OnStartDrag is always called right on mouse down...
//  AddTestEvent(ev(evStartDrag, GetSelMask, GetSelIdx));
  FInDrag := idStarted;
end;

procedure TTestListView.LvDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
end;

procedure TTestListView.LvEndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  if FInDrag = idTrue then
    AddTestEvent(ev(evEndDrag, x, y, [], GetSelMask, GetSelIdx));
  FInDrag := idFalse;
end;

procedure TTestListView.OnMenuItemClick(Sender: TObject);
begin
  AddTestEvent(ev(evMenuItem, TMenuItem(Sender).Tag));
end;

procedure TTestListView.OnMenuPopUp(Sender: TObject);
var
  p: TPoint;
begin
  p := FListView.ScreenToClient(Point(TPopupMenu(Sender).PopupPoint.x, TPopupMenu(Sender).PopupPoint.y));
  AddTestEvent(ev(evMenu, p.x, p.y, [], GetSelMask, GetSelIdx));
end;

function TTestListView.GetSelMask: TSelMask;
var
  i: Integer;
begin
  Result := [];
  for i := 0 to FListView.Items.Count -1 do
    if FListView.Items[i].Selected then
      Include(Result, i);
end;

function TTestListView.GetSelIdx: Integer;
var
  s: TListItem;
begin
  s := FListView.Selected;
  if s <> nil then
    Result := s.Index
  else
    Result := -1;
end;

function TTestListView.ItemIdx(X, Y: Integer): Integer;
var
  r: TRect;
begin
  Result := FListView.Items.Count - 1;
  while Result >= 0 do begin
    r := FListView.Items[Result].DisplayRect(drBounds);
    if (x >= r.Left) and (x < r.Right) and (y >= r.Top) and (y < r.Bottom) then
      break;
    dec(Result);
  end;
end;

function TTestListView.ItemXY(Idx: TClickPos): TPoint;
var
  r: TRect;
begin
  if Idx.ItemIdx > FListView.Items.Count then begin
    r := FListView.Items[FListView.Items.Count - 1].DisplayRect(drBounds);
    Result.x := (r.Left + r.Right) div 2;
    Result.y := (r.Bottom + FListView.Height) div 2;
    exit;
  end;

(*
debugln('%25s %25s %25s %25s', [dbgs(FListView.Items[Idx.ItemIdx].DisplayRect(drIcon)),
dbgs(FListView.Items[Idx.ItemIdx].DisplayRect(drLabel)),
dbgs(FListView.Items[Idx.ItemIdx].DisplayRect(drBounds)),
dbgs(FListView.Items[Idx.ItemIdx].DisplayRect(drSelectBounds))
]); // *)
  if Idx.SubIdx >= 0 then
    r := FListView.Items[Idx.ItemIdx].DisplayRectSubItem(Idx.SubIdx+1, Idx.ItemPart)
  else
    r := FListView.Items[Idx.ItemIdx].DisplayRect(Idx.ItemPart);
  case Idx.ItemInnerPos of
    ipTopLeft: begin
        Result.x := r.Left + Min(3, (r.Right - r.Left) div 2);
        Result.y := r.Top  + Min(3, (r.Bottom - r.Top) div 2);
      end;
    ipCenter: begin
        Result.x := (r.Right + r.Left) div 2;
        Result.y := (r.Bottom + r.Top) div 2;
      end;
    ipRightBottom: begin
        Result.x := r.Right  - Min(3, (r.Right - r.Left) div 2);
        Result.y := r.Bottom - Min(3, (r.Bottom - r.Top) div 2);
      end;
    ipOutsideRight: begin
        Result.x := FListView.ClientWidth - 3;
        Result.y := (r.Bottom + r.Top) div 2;
      end;
    ipOutsideBelow: begin
        Result.x := (r.Right + r.Left) div 2;
        Result.y := FListView.ClientHeight - 3;
      end;
  end;
end;

function TTestListView.ItemScreenXY(Idx: TClickPos): TPoint;
begin
  Result := FListView.ClientToScreen(ItemXY(Idx));
end;

procedure TTestListView.MouseToIdx(AIdx: TClickPos; Shift: TShiftState;
  AXOffs: integer; AYOffs: integer);
var
  p: TPoint;
begin
  p := ItemXY(AIdx);
  FTestMouseInput.Move(Shift, FListView, p.x + AXOffs, p.y + AYOffs);
  //Application.ProcessMessages;
  //sleep(20);Application.ProcessMessages;
end;

procedure TTestListView.MouseDownOnIdx(Button: TMouseButton; AIdx: TClickPos;
  Shift: TShiftState);
var
  p: TPoint;
begin
  p := ItemXY(AIdx);
  FTestMouseInput.Down(Button, Shift, FListView, p.x, p.y);
  //Application.ProcessMessages;
end;

procedure TTestListView.MouseUpOnIdx(Button: TMouseButton; AIdx: TClickPos;
  Shift: TShiftState);
var
  p: TPoint;
begin
  p := ItemXY(AIdx);
  //FTest
  FTestMouseInput.Up(Button, Shift, FListView, p.x, p.y);
  //Application.ProcessMessages;
end;

procedure TTestListView.ClickButton;
begin
  MouseAndKeyInput.MouseInput.Click(mbLeft, [], FButton, 5, 5);
end;

procedure TTestListView.AddTestEvent(AnEvent: TLvTestEvent);
var
  i: Integer;
begin
  i := Length(FTestEvents);
  SetLength(FTestEvents, i+1);
  FTestEvents[i] := AnEvent;
end;

function TTestListView.CheckTestEvent(AName: string; AnEvent: TLvTestEvent;
  AnIndex: Integer): Boolean;
var
  s1, s2: string;
  e, g: TLvTestEvent;
begin
  Result := true;
  if AnIndex < 0 then
    AnIndex := AnIndex + Length(FTestEvents);
  AssertTrue(AName + ' / index ', (AnIndex >= 0) and (AnIndex < Length(FTestEvents)));

  try
    e := AnEvent;
    g := FTestEvents[AnIndex];

    writestr(s1, e.Event);
    writestr(s2, g.Event);
    AssertEquals(AName, s1, s2);

    if e.x >= 0 then
      AssertEquals(AName + ' X', e.x, g.x);
    if e.y >= 0 then
      AssertEquals(AName + ' Y', e.y, g.y);

    if e.ItemIdx <> -2 then
      if e.Event = evMenuItem then
        AssertEquals(AName + ' ItemIDx', e.ItemIdx, g.ItemIdx)
      else
        AssertEquals(AName + ' ItemIDx', e.ItemIdx, ItemIdx(g.x, g.y));

    AssertEquals(AName + ' Shift', ShiftToStr(e.Shift), ShiftToStr(g.Shift));

    if e.SelIdx <> -2 then
      AssertEquals(AName + ' SelIdx', e.SelIdx, g.SelIdx);
    if not (TSelMaskMarker in e.SelMask) then
      AssertTrue(AName + ' SelMask', e.SelMask >< g.SelMask = []);
  except
    on E: Exception do begin
      Result := false;
      DebugLn(['>>>>', AName, ' ', E.Message]);
      DbgOut('GOT: '); DumpEv(FTestEvents[AnIndex]);
      DbgOut('Exp: '); DumpEv(AnEvent);
      FTestError := FTestError + AName + ' ' + E.Message +
        LineEnding + 'GOT '+TextEv(FTestEvents[AnIndex]) +
        LineEnding + 'Exp '+TextEv(AnEvent) +
        LineEnding
      ;
    end;
  end;
end;

procedure TTestListView.CheckTestEvents(AName: string;
  AnEvents: array of TLvTestEvent);
var
  i: Integer;
  r: Boolean;
begin
  try
    r := true;
    AssertEquals(AName + ' / count ', Length(AnEvents), Length(FTestEvents));
    for i := 0 to Length(FTestEvents) - 1 do
      if not CheckTestEvent(AName, AnEvents[i], i) then
        r := false;
  except
    r := false;
  end;
  if not r then begin
    DebugLnEnter('>>> GOT: '+AName);
    for i := 0 to Length(FTestEvents) - 1 do DumpEv(FTestEvents[i]);
    DebugLn('> Exp: ');
    for i := 0 to Length(AnEvents) - 1 do DumpEv(AnEvents[i]);
    DebugLnExit('<<<');
    FTestError := FTestError + AName + LineEnding + 'GOT';
    for i := 0 to Length(FTestEvents) - 1 do FTestError := FTestError + TextEv(FTestEvents[i]) + LineEnding;
    FTestError := FTestError + 'Exp' + LineEnding;
    for i := 0 to Length(AnEvents) - 1 do FTestError := FTestError + TextEv(AnEvents[i]) + LineEnding;
    FTestError := FTestError + LineEnding;
  end;
end;

procedure TTestListView.StoreSelectionState;
begin
  AddTestEvent(ev(evStoreSelection, GetSelMask, GetSelIdx));
end;

procedure TTestListView.ClearTestEvents;
begin
  FTestEvents := nil;
  FInDrag := idFalse;
end;

procedure TTestListView.SetUp;
begin
  inherited SetUp;
  TheTestCase := Self;

  // Set defaults
  // TListView does not respect DragImmediate => it always start dragging at Threshold
  Mouse.DragImmediate := True;
  Mouse.DragThreshold := 5;

  RecreateForm;
  ClearTestEvents;
end;

procedure TTestListView.TearDown;
begin
  inherited TearDown;
  FreeAndNil(FForm);
end;

procedure TTestListView.RecreateForm;
begin
  FreeAndNil(FForm);
  FPopUp := nil;
  FListView := nil;

  FForm := TForm.CreateNew(nil);
  FForm.Top := Screen.Monitors[0].Top + 1;
  FForm.Left := Screen.Monitors[0].Left + 1;
  FForm.Height := 300;
  FForm.Width := 500;
  FForm.Caption := 'Do NOT move your mouse !';

  FButton := TButton.Create(FForm);
  FButton.Parent := FForm;
  FButton.Align := alBottom;

  FForm.Show;
  CreateListView;
end;

procedure TTestListView.CreateListView(AViewStyle: TViewStyle;
  AColumnCnt: Integer; ADrgMode: TDragMode; MultiSel: Boolean; RowSel: Boolean;
  ReadOnly: Boolean);
var
  i: Integer;
begin
  FreeAndNil(FListView);
  FListView := TListViewForTest.Create(FForm);
  FListView.Parent := FForm;
  FListView.Left := 0;
  FListView.Top  := 0;
  FListView.Width  := 300;
  FListView.Height := 170;

  for i := 1 to 5 do begin
    FListView.AddItem('Test ' + IntToStr(i), nil);
    if AViewStyle = vsReport then
      FListView.Items[i-1].SubItems.Add('Sub');
  end;

  FListView.OnMouseDown := @LvMouseDown;
  FListView.OnMouseUp   := @LvMouseUp;
  FListView.OnClick     := @LvClick;
  FListView.OnDblClick := @LvDblClick;
  FListView.OnContextPopup := @LvContextPop;
  FListView.OnMouseMove := @LvMouseMove;
  FListView.OnStartDrag := @LvStartDrag;
  FListView.OnDragOver := @LvDragOver;
  FListView.OnEndDrag := @LvEndDrag;

  FListView.OnKeyDown := @Lvkeydown;
  FListView.OnKeyUp := @Lvkeyup;

  FListView.ViewStyle := AViewStyle;
  for i := 1 to AColumnCnt do
    FListView.Columns.Add.Width := FListView.Width div AColumnCnt;

  FListView.DragMode := ADrgMode;
  FListView.MultiSelect := MultiSel;
  FListView.RowSelect := RowSel;
  FListView.ReadOnly := ReadOnly;
  Application.ProcessMessages;
end;

function TTestListView.CreatePopUp: TPopupMenu;
var
  i: Integer;
  m: TMenuItem;
begin
  Result := TPopupMenu.Create(FForm);
  Result.OnPopup := @OnMenuPopUp;
  for i := 1 to 3 do begin
    m := TMenuItem.Create(FForm);
    m.Tag := i;
    m.Caption := 'menu ' + IntToStr(i);
    m.OnClick := @OnMenuItemClick;
    Result.Items.Add(m);
  end;
end;

procedure TTestListView.AttachPopUp;
begin
  if FPopUp = nil then
    FPopUp := CreatePopUp;
  FListView.PopupMenu := FPopUp;
end;

procedure TTestListView.TestClick;
var
  Multi, RowSel: Boolean;
  ViewSt: TViewStyle;
  ColCnt: Integer;
  TstName: String;
  ExpSel: TSelMask;
  p: TPoint;
begin
  sleep(500);
  FTestError := '';
  // TODO: scrolled  // with icons // popup // enter editor on dbl click
  // TODO: change key between mouse-down and -up

  for Multi  in Boolean do
  for RowSel in Boolean do
  for ViewSt in TViewStyle do // (vsIcon, vsSmallIcon, vsList, vsReport);
  for ColCnt := MinColumnPerStyle[ViewSt] to MaxColumnPerStyle[ViewSt] do
  begin
    CreateListView(ViewSt, ColCnt, dmManual, Multi, RowSel);
    TstName := Format('TestClick  %s Cols: %d Multi: %s RowSel: %s ',
      [dbgs(ViewSt), ColCnt, dbgs(Multi), dbgs(RowSel) ]
    );
//debugln([TstName]);


    (* ****
      Test a simple click on an Item
     * ****)
    ClearTestEvents;
    TTestThread.Run(mbLeft, ItemScreenXY(2), [], []);
    StoreSelectionState;

    CheckTestEvents('Click text '+TstName, [
      ev(evMsgLDown),
      ev(evMouseDown, [ssLeft], [2], 2),
      ev(evMsgLUp),
      ev(evClick, [2], 2),
      ev(evMouseUp, [], [2], 2),
      ev(evStoreSelection, [2], 2)
    ]);

    (* ****
      Continue the above click inte a DoubleClick
     * ****)
    ClearTestEvents;
    TTestThread.Run(mbLeft, ItemScreenXY(2), [], []);
    StoreSelectionState;
    ClickButton; // Avoid double-click in next test

    CheckTestEvents('Double Click text '+TstName, [
      ev(evMsgLDbl),
      ev(evMouseDown, [ssLeft, ssDouble], [2], 2),
      ev(evDblClick, [2], 2),
      ev(evMsgLUp),
      ev(evMouseUp, [ssDouble], [2], 2),
      ev(evStoreSelection, [2], 2)
    ]);

    (* ****
      Test a click with a Modifier Key
      - The key must be reported in the events
      - For MultiSelect: The current selection remains, and a 2nd item is selected
     * ****)
    ClearTestEvents;
    KeyInput.Apply([ssCtrl]);
    TTestThread.Run(mbLeft, ItemScreenXY(3), [], []);
    KeyInput.Unapply([ssCtrl]);
    StoreSelectionState;
    ClickButton;

    if Multi then
      ExpSel := [2,3]
    else
      ExpSel := [3];
    CheckTestEvents('Ctrl Click text '+TstName, [
      ev(evMsgLDown),
      ev(evMouseDown, [ssLeft, ssCtrl], ExpSel),
      ev(evMsgLUp),
      ev(evClick, ExpSel),
      ev(evMouseUp, [ ssCtrl], ExpSel),
      ev(evStoreSelection, ExpSel)
    ]);

    if Multi and (ViewSt in [vsIcon, vsList, vsReport]) and (ColCnt = MinColumnPerStyle[ViewSt]) then begin
      (* ****
        MultiSelect only:
        Expand selection with Shift-Click
       * ****)
      ClearTestEvents;
      KeyInput.Apply([ssShift]);
      TTestThread.Run(mbLeft, ItemScreenXY(0), [], []);
      KeyInput.Unapply([ssShift]);
      StoreSelectionState;

      ExpSel := [0,1,2,3];
      CheckTestEvents('Shift Click text '+TstName, [
        ev(evMsgLDown),
        ev(evMouseDown, [ssLeft, ssShift], ExpSel),
        ev(evMsgLUp),
        ev(evClick, ExpSel),
        ev(evMouseUp, [ssShift], ExpSel),
        ev(evStoreSelection, ExpSel)
      ]);

      (* ****
        MultiSelect only:
        Click (no Shift) on not yet selected item
        - clears old selection, and sets new selection
       * ****)
      ClearTestEvents;
      TTestThread.Run(mbLeft, ItemScreenXY(4), [], []);
      StoreSelectionState;
      ClickButton;

      CheckTestEvents('Click multi to other'+TstName, [
        ev(evMsgLDown),
        ev(evMouseDown, [ssLeft]),  // , [0,1,2,3]),  // TODO: should still be old selection
        ev(evMsgLUp),
        ev(evClick, [4], 4),
        ev(evMouseUp, [], [4], 4),
        ev(evStoreSelection, [4], 4)
      ]);

    end;



    if (ViewSt = vsReport) and (ColCnt = 2) then begin
      (* ****
        Report-view only:
        Click on sub-item
        - does not select an item (Except in RowSelect)
       * ****)
      ClearTestEvents;
      FListView.ClearSelection; // The old selection may clear only after the events ?
      TTestThread.Run(mbLeft, ItemScreenXY(cp(1, 0)), [], []); // Sub Item
      StoreSelectionState;
      ClickButton;

      if RowSel then
        ExpSel := [1]
      else
        ExpSel := NO_SEL;
      CheckTestEvents('Click sub item  '+TstName, [
        ev(evMsgLDown),
        ev(evMouseDown, [ssLeft], ExpSel),
        ev(evMsgLUp),
        ev(evClick, ExpSel),
        ev(evMouseUp, [], ExpSel),
        ev(evStoreSelection, ExpSel)
      ]);
    end;


    (* ****
      Moving mouse between Down and Up (Up occurs over different Item)
      - Selects the Item on which the mouse-down occurred
     * ****)
    ClearTestEvents;
    FListView.ClearSelection;
    TTestThread.Run(mbLeft, ItemScreenXY(1), ItemScreenXY(0), [], []);
    StoreSelectionState;
    ClickButton; // Avoid double-click in next test

    CheckTestEvents('Click/Select 1 text => Up on 0 '+TstName, [
      ev(evMsgLDown),
      ev(evMouseDown, [ssLeft], [1], 1),
      ev(evMsgLUp),
      ev(evClick, [1], 1),
      ev(evMouseUp, [], [1], 1),
      ev(evStoreSelection, [1], 1)
    ]);



    if (not RowSel) and (ColCnt = MaxColumnPerStyle[ViewSt]) then begin
      (* ****
        Moving mouse between Down and Up => Up outside of listview
        - Selects the Item on which the mouse-down occurred
       * ****)
      ClearTestEvents;
      FListView.ClearSelection;
      TTestThread.Run(mbLeft, ItemScreenXY(1),
        FListView.ClientToScreen(point(FListView.Left + FListView.Width+10, 10)),
        [], []);
      StoreSelectionState;
      ClickButton; // Avoid double-click in next test

      CheckTestEvents('Click/Select 1 text => Up on form '+TstName, [
        ev(evMsgLDown),
        ev(evMouseDown, [ssLeft], [1], 1),
//        ev(evMsgLUp),
//        ev(evClick, [1], 1),
//        ev(evMouseUp, [], [1], 1),  // TODO: should it happen, or only if dragging?
        ev(evStoreSelection, [1], 1)
      ]);


      (* ****
        Moving mouse between Down and Up => Up outside of form
        - Selects the Item on which the mouse-down occurred
       * ****)
      ClearTestEvents;
      FListView.ClearSelection;
      TTestThread.Run(mbLeft, ItemScreenXY(1), AddToPoint(ItemScreenXY(0), 0, FForm.Height), [], []);
      StoreSelectionState;
      ClickButton; // Avoid double-click in next test

      CheckTestEvents('Click/Select 1 text => Up on screen '+TstName, [
        ev(evMsgLDown),
        ev(evMouseDown, [ssLeft], [1], 1),
//        ev(evMsgLUp),
//        ev(evClick, [1], 1),
//        ev(evMouseUp, [], [1], 1),  // TODO: should it happen, or only if dragging?
        ev(evStoreSelection, [1], 1)
      ]);
    end;


    (* ****
      Click in empty space, below last Item
      - Does not select any item
     * ****)
    ClearTestEvents;
    FListView.ClearSelection;
    TTestThread.Run(mbLeft, ItemScreenXY(99), [], []);
    StoreSelectionState;
    ClickButton;

    CheckTestEvents('Click below item  '+TstName, [
      ev(evMsgLDown),
      ev(evMouseDown, [ssLeft], NO_SEL),
      ev(evMsgLUp),
      ev(evClick, NO_SEL),
      ev(evMouseUp, [], NO_SEL),
      ev(evStoreSelection, NO_SEL)
    ]);


    (* ****
      Mouse-down in empty space, then move to last Item, Mouse Up over Item
      - MultiSelect = False: Does not select any item
      - MultiSelect = True:  Selects last item (rubber band)
     * ****)
    ClearTestEvents;
    FListView.ClearSelection;
    p := FListView.ClientToScreen(Point(FListView.ClientWidth-5, FListView.ClientHeight - 5));
    TTestThread.Run(mbLeft, p, ItemScreenXY(4), [], []);
    StoreSelectionState;
    ClickButton;

    if Multi then
      ExpSel := [4]
    else
      ExpSel := NO_SEL;
    CheckTestEvents('Click below item -> move to last '+TstName, [
      ev(evMsgLDown),
      ev(evMouseDown, [ssLeft], ExpSel),
      ev(evMsgLUp),
      ev(evClick, ExpSel),
      ev(evMouseUp, [], ExpSel),
      ev(evStoreSelection, ExpSel)
    ]);


  end;
  AssertEquals('', FTestError);
end;

procedure TTestListView.TestClickRight;
var
  Multi, RowSel: Boolean;
  ViewSt: TViewStyle;
  TstName: String;
  ExpSel: TSelMask;
begin
  sleep(500);
  FTestError := '';

  for Multi  in Boolean do
  for RowSel in Boolean do
  for ViewSt in TViewStyle do // (vsIcon, vsSmallIcon, vsList, vsReport);
  begin
    CreateListView(ViewSt, MaxColumnPerStyle[ViewSt], dmManual, Multi, RowSel);
    TstName := Format('Test-RIGHT-Click  %s Cols: %d Multi: %s RowSel: %s ',
      [dbgs(ViewSt), MaxColumnPerStyle[ViewSt], dbgs(Multi), dbgs(RowSel) ]
    );


    ClearTestEvents;
    TTestThread.Run(mbRight, ItemScreenXY(2), [], []);
    StoreSelectionState;

    CheckTestEvents('Click text '+TstName, [
      ev(evMsgRDown),
      ev(evMouseDown, [ssRight], [2], 2),
      ev(evMsgRUp),
      ev(evMouseUp, [], [2], 2),
      ev(evMsgContext),
      ev(evContextPop, 2, [], [2], 2),
      ev(evStoreSelection, [2], 2)
    ]);

    // make it a double click
    ClearTestEvents;
    TTestThread.Run(mbRight, ItemScreenXY(2), [], []);
    StoreSelectionState;
    ClickButton;

    CheckTestEvents('Double Click text '+TstName, [
      ev(evMsgRDbl),
      ev(evMouseDown, [ssRight, ssDouble], [2], 2),
      ev(evMsgRUp),
      ev(evMouseUp, [ssDouble], [2], 2),
      ev(evMsgContext),
      ev(evContextPop, 2, [], [2], 2),
      ev(evStoreSelection, [2], 2)
    ]);


    // with ctrl key // ctrl + right in MULTI does not change selection
    ClearTestEvents;
    KeyInput.Apply([ssCtrl]);
    TTestThread.Run(mbRight, ItemScreenXY(3), [], []);
    KeyInput.Unapply([ssCtrl]);
    StoreSelectionState;
    ClickButton;

    if Multi then
      ExpSel := [2]
    else
      ExpSel := [3];
    CheckTestEvents('Ctrl Click text '+TstName, [
      ev(evMsgRDown),
      ev(evMouseDown, [ssRight, ssCtrl], ExpSel),
      ev(evMsgRUp),
      ev(evMouseUp, [ ssCtrl], ExpSel),
      ev(evMsgContext),
      ev(evContextPop, 3, [], ExpSel),
      ev(evStoreSelection, ExpSel)
    ]);


    (* ****
      Moving mouse between Down and Up (Up occurs over different Item)
     * ****)
    ClearTestEvents;
    TTestThread.Run(mbRight, ItemScreenXY(2), ItemScreenXY(1), [], []);
    StoreSelectionState;

    CheckTestEvents('Click, move, up '+TstName, [
      ev(evMsgRDown),
      ev(evMouseDown, [ssRight], [2], 2),
      ev(evMsgRUp),
      ev(evMouseUp, [], [2], 2),
      ev(evMsgContext),
      ev(evContextPop, 1, [], [2], 2),
      ev(evStoreSelection, [2], 2)
    ]);



    if (ViewSt = vsReport) then begin
      ClearTestEvents;
      FListView.ClearSelection; // The old selection may clear only after the events ?
      TTestThread.Run(mbRight, ItemScreenXY(cp(1, 0)), [], []); // Sub Item
      StoreSelectionState;
      ClickButton;

      if RowSel then
        ExpSel := [1]
      else
        ExpSel := NO_SEL;
      CheckTestEvents('Click sub item  '+TstName, [
        ev(evMsgRDown),
        ev(evMouseDown, [ssRight], ExpSel),
        ev(evMsgRUp),
        ev(evMouseUp, [], ExpSel),
        ev(evMsgContext),
        ev(evContextPop, 1, [], ExpSel),
        ev(evStoreSelection, ExpSel)
      ]);
    end;


    // below items
    ClearTestEvents;
    FListView.ClearSelection; // The old selection may clear only after the events ?
    TTestThread.Run(mbRight, ItemScreenXY(99), [], []);
    StoreSelectionState;
    ClickButton;

    CheckTestEvents('Click below item  '+TstName, [
      ev(evMsgRDown),
      ev(evMouseDown, [ssRight], NO_SEL),
      ev(evMsgRUp),
      ev(evMouseUp, [], NO_SEL),
      ev(evMsgContext),
      ev(evContextPop, -1, [], NO_SEL),
      ev(evStoreSelection, NO_SEL)
    ]);

  end;
  AssertEquals('', FTestError);
end;

procedure TTestListView.TestClickNoneDragJitter;
var
  i: Integer;
  p1, p2: TPoint;
  TstName: String;
  ViewSt: TViewStyle;
  Multi: Boolean;
  DragM: TDragMode;
begin
  sleep(500);
  FTestError := '';
  (* *****
     Click an item, but move the mouse (within the item bounds)
     LCL has dmManual => no dragging
     Windows may still report drag-start, but LCL events should report normal click
   * *****)

  Mouse.DragImmediate := False; // TListView foreces this to false anyway
  Mouse.DragThreshold := 11; // will not be reached
  try

  for ViewSt in TViewStyle do // (vsIcon, vsSmallIcon, vsList, vsReport);
  for Multi  in Boolean do
  for DragM  in TDragMode do
  begin
    CreateListView(ViewSt, MaxColumnPerStyle[ViewSt], DragM, Multi, False);

    for i := 1 to 10 do begin
      TstName := Format('%s x-offs: %d Multi: %s Mode: ', [dbgs(ViewSt), i, dbgs(Multi), dbgs(DragM)]);
      p1 := ItemScreenXY(2);
      p2 := p1;
      p2.x := p2.x + i;

      ClearTestEvents;
      FListView.ClearSelection;
      TTestThread.Run(mbLeft, p1, p2, [], []);
      StoreSelectionState;
      ClickButton;

      CheckTestEvents('Click text '+TstName, [
        ev(evMsgLDown),
        ev(evMouseDown, [ssLeft], [2], 2),
        ev(evMsgLUp),
        ev(evClick, [2], 2),
        ev(evMouseUp, [], [2], 2),
        ev(evStoreSelection, [2], 2)
      ]);


      // Test that the dragmanager gets the correct coordinates (< DragThreshold)
      if i in [2,9,10] then begin
        p1 := ItemScreenXY(99);
        p2 := p1;
        p2.x := p2.x + i;

        ClearTestEvents;
        FListView.ClearSelection;
        TTestThread.Run(mbLeft, p1, p2, [], []);
        StoreSelectionState;
        ClickButton;

        CheckTestEvents('Click below '+TstName, [
          ev(evMsgLDown),
          ev(evMouseDown, [ssLeft], NO_SEL),
          ev(evMsgLUp),
          ev(evClick, NO_SEL),
          ev(evMouseUp, [], NO_SEL),
          ev(evStoreSelection, NO_SEL)
        ]);
      end;

    end;
  end;

  finally
    // Set defaults
    Mouse.DragImmediate := True;
    Mouse.DragThreshold := 5;
  end;
  AssertEquals('', FTestError);
end;

procedure TTestListView.TestDrag;
var
  Multi: Boolean;
  ViewSt: TViewStyle;
  TstName: String;
  ExpSel: TSelMask;
  p1, p2, d1, d2: TPoint;
begin
  sleep(500);
  FTestError := '';
  (* *****
     Drag Tests
     - Any test that causes a Drag, should *NOT* have a evClick event
   * *****)

  for Multi  in Boolean do
  for ViewSt in TViewStyle do // (vsIcon, vsSmallIcon, vsList, vsReport);
  begin
    CreateListView(ViewSt, MaxColumnPerStyle[ViewSt], dmAutomatic, Multi, False);
    TstName := Format('TestClick %s  Multi: %s ',
      [dbgs(ViewSt), dbgs(Multi) ]
    );

    p1 := ItemScreenXY(2);
    p2 := AddToPoint(p1, 10, 0);

    (* *****
       Start Drag - Remain over the same Item as
       - no OnClick
     * *****)
    ClearTestEvents;
    FListView.ClearSelection;
    //TTestThread.Run(mbLeft, p1,p2, [], []);
    TTestThread.Run(mbLeft, p1,p2, [AddToPoint(p2, 6,0)], [], []);
    StoreSelectionState;
    ClickButton;

    CheckTestEvents('Drag text, mouse remains over item'+TstName, [
      ev(evMsgLDown),
      ev(evMouseDown, [ssLeft], [2], 2),
      ev(evStartDrag, [], [2], 2), // d1
      ev(evMsgLUp),
      ev(evClick, [2], 2),                     // TODO: should not happen
      ev(evEndDrag, [], [2], 2),
      ev(evMouseUp, [], [2], 2),
      ev(evStoreSelection, [2], 2)
    ]);



    (* **********
       Move mouse outside window to test MouseCapture / Mouse-up outside the window
       - There should be a evMoveDrag for each simulated point of the mouse.
     * **********)
    p1 := ItemScreenXY(2);
    d1 := AddToPoint(p1,  0, FForm.Height);
    d2 := AddToPoint(d1, 10, 0);
    p2 := AddToPoint(d1, 100, 0);

    (* *****
       Start Drag with no previous selection
     * *****)
    ClearTestEvents;
    FListView.ClearSelection;
    //TTestThread.Run(mbLeft, p1,p2, [d1, d2], [], []);
    TTestThread.Run(mbLeft, p1,p2, [AddToPoint(d1, -6,0), d1, d2], [], []);
    StoreSelectionState;
    ClickButton;

    CheckTestEvents('Drag text, move away '+TstName, [
      ev(evMsgLDown),
      ev(evMouseDown, [ssLeft], [2], 2),
      ev(evStartDrag, [], [2], 2), // d1
      ev(evMoveDrag, [], [2], 2), // d2
      ev(evMoveDrag, [], [2], 2), // p2
      ev(evMsgLUp),
      ev(evEndDrag, [], [2], 2),
      ev(evMouseUp, [], [2], 2),
      ev(evStoreSelection, [2], 2)
    ]);


    (* *****
       Start Drag - Existing selection of one Item / Drag-Click on other Item
       - Selection is changed (even in MultiSelect)
     * *****)
    ClearTestEvents;
    FListView.ClearSelection;
    FListView.Items[0].Selected := True;
    //TTestThread.Run(mbLeft, p1,p2, [d1, d2], [], []);
    TTestThread.Run(mbLeft, p1,p2, [AddToPoint(d1, -6,0), d1, d2], [], []);
    StoreSelectionState;
    ClickButton;

    CheckTestEvents('Drag text - change selection '+TstName, [
      ev(evMsgLDown),
      ev(evMouseDown, [ssLeft], [2], 2),
      ev(evStartDrag, [], [2], 2), // d1
      ev(evMoveDrag, [], [2], 2), // d2
      ev(evMoveDrag, [], [2], 2), // p2
      ev(evMsgLUp),
      ev(evEndDrag, [], [2], 2),
      ev(evMouseUp, [], [2], 2),
      ev(evStoreSelection, [2], 2)
    ]);


    if Multi then begin
      (* *****
         Start Drag - Existing selection of two Item - Drag click on selected Item
         - Selection is kept (both items)
       * *****)
      ClearTestEvents;
      FListView.ClearSelection;
      FListView.Items[0].Selected := True;
      FListView.Items[2].Selected := True;
      //TTestThread.Run(mbLeft, p1,p2, [d1, d2], [], []);
      TTestThread.Run(mbLeft, p1,p2, [AddToPoint(d1, -6,0), d1, d2], [], []);
      StoreSelectionState;
      ClickButton;

      CheckTestEvents('Drag text - keep multi selection '+TstName, [
        ev(evMsgLDown),
        ev(evMouseDown, [ssLeft], [0, 2]),
        ev(evStartDrag, [], [0, 2]), // d1
        ev(evMoveDrag, [], [0, 2]), // d2
        ev(evMoveDrag, [], [0, 2]), // p2
        ev(evMsgLUp),
        ev(evEndDrag, [], [0, 2]),
        ev(evMouseUp, [], [0, 2]),
        ev(evStoreSelection, [0, 2])
      ]);

      // TODO: Shift click to extend selection
    end;


    (* ****
      Mouse-down in empty space, then move to last Item, Mouse Up over Item
      - MultiSelect = False: Drag
      - MultiSelect = True:  NO Drag // Selects last item (rubber band)
     * ****)
    p1 := FListView.ClientToScreen(Point(FListView.ClientWidth-5, FListView.ClientHeight - 5));
    p2 := ItemScreenXY(4);

    ClearTestEvents;
    FListView.ClearSelection;
    TTestThread.Run(mbLeft, p1, p2, [], []);
    //TTestThread.Run(mbLeft, p1, p2, [AddToPoint(p2, 6,0)], [], []);
    StoreSelectionState;
    ClickButton;

    if Multi then begin
      ExpSel := [4];
      CheckTestEvents('Click below item -> move to last '+TstName, [
        ev(evMsgLDown),
        ev(evMouseDown, [ssLeft], ExpSel),
        ev(evMsgLUp),
        ev(evClick, ExpSel),  // OK: not dragging, should click
        ev(evMouseUp, [], ExpSel),
        ev(evStoreSelection, ExpSel)
      ]);
    end
    else begin
      CheckTestEvents('Click below item -> move to last '+TstName, [
        ev(evMsgLDown),
        ev(evMouseDown, [ssLeft], NO_SEL),
        ev(evStartDrag, [], NO_SEL),
        ev(evMsgLUp),
        ev(evClick, NO_SEL),  // should not click
        ev(evEndDrag, [], NO_SEL),
        ev(evMouseUp, [], NO_SEL),
        ev(evStoreSelection, NO_SEL)
      ]);
    end;

  end;

  AssertEquals('', FTestError);
end;

procedure TTestListView.TestPopUp;
var
  Multi: Boolean;
  ViewSt: TViewStyle;
  TstName: String;
  ExpSel: TSelMask;
  p99, p98, p1, p2, p3, p4,
  pp99, pp98, pp1, pp2, pp3, pp4,
  xy98, xy99, xy1, xy2, xy4: TPoint;
begin
  sleep(500);
  FTestError := '';
  (* *****
     PopUpMenu
   * *****)

  for Multi  in Boolean do
  for ViewSt in TViewStyle do // (vsIcon, vsSmallIcon, vsList, vsReport);
  begin
    CreateListView(ViewSt, MaxColumnPerStyle[ViewSt], dmManual, Multi, False);
    AttachPopUp;

    TstName := Format('TestClick %s  Multi: %s ',
      [dbgs(ViewSt), dbgs(Multi) ]
    );

    p1  := ItemScreenXY(1);
    p2  := ItemScreenXY(2);
    p3  := ItemScreenXY(3);
    p4  := ItemScreenXY(4);
    p99 := ItemScreenXY(99);
    p98 := AddToPoint(ItemScreenXY(99), -10, 0);
    xy1  := ItemXY(1);
    xy2  := ItemXY(2);
    xy4  := ItemXY(4);
    xy99 := ItemXY(99);
    xy98 := AddToPoint(ItemXY(99), -10, 0);

    pp1  := AddToPoint(p1, 5, 5); // menu item
    pp2  := AddToPoint(p2, 5, 5);
    pp3  := AddToPoint(p3, 5, 5);
    pp4  := AddToPoint(p4, 5, 5);
    pp99 := AddToPoint(p99, 5, 5);
    pp98 := AddToPoint(p98, 5, 5);


    (* *****
       Simple pop up click
     * *****)
    ClearTestEvents;
    FListView.ClearSelection;
    TTestThread.Run(mbRight, p2, p2, [], [pp2], [], []);
    StoreSelectionState;
    ClickButton;

    CheckTestEvents('pop item '+TstName, [
      ev(evMsgRDown),
      ev(evMouseDown, [ssRight], [2], 2),
      ev(evMsgRUp),
      ev(evMouseUp, [], [2], 2),
      ev(evMsgContext),
      ev(evContextPop, 2, [], [2], 2),
      ev(evMenu, xy2, 2, [], [2], 2),
      ev(evMenuItem, 1),
      ev(evStoreSelection, [2], 2)
    ]);

    (* *********
       right down, then mouse move, then pop
     * *********)

    (* *****
       item => other item
     * *****)
    ClearTestEvents;
    FListView.ClearSelection;
    TTestThread.Run(mbRight, p3, p2, [], [pp2], [], []);
    StoreSelectionState;
    ClickButton;

    CheckTestEvents('down item, pop other item '+TstName, [
      ev(evMsgRDown),
      ev(evMouseDown, [ssRight], [3], 3),
      ev(evMsgRUp),
      ev(evMouseUp, [], [3], 3),
      ev(evMsgContext),
      ev(evContextPop, 2, [], [3], 3),
      ev(evMenu, xy2, 2, [], [3], 3),
      ev(evMenuItem, 1),
      ev(evStoreSelection, [3], 3)
    ]);

    (* *****
       item => empty
     * *****)
    ClearTestEvents;
    FListView.ClearSelection;
    TTestThread.Run(mbRight, p3, p99, [], [pp99], [], []);
    StoreSelectionState;
    ClickButton;

    CheckTestEvents('down item, pop empty'+TstName, [
      ev(evMsgRDown),
      ev(evMouseDown, [ssRight], [3], 3),
      ev(evMsgRUp),
      ev(evMouseUp, [], [3], 3),
      ev(evMsgContext),
      ev(evContextPop, -1, [], [3], 3),
      ev(evMenu, xy99, -1, [], [3], 3),
      ev(evMenuItem, 1),
      ev(evStoreSelection, [3], 3)
    ]);



    (* *********
       pop first menu, then while open pop 2nd menu
     * *********)

    (* *****
       pop up item, then other item
     * *****)
    ClearTestEvents;
    FListView.ClearSelection;
    TTestThread.Run(mbRight, p2, p2, [], [RBtn(p1), pp1], [], []);
    StoreSelectionState;
    ClickButton;

    CheckTestEvents('pop item, pop other item '+TstName, [
      ev(evMsgRDown),
      ev(evMouseDown, [ssRight], [2], 2),
      ev(evMsgRUp),
      ev(evMouseUp, [], [2], 2),
      ev(evMsgContext),
      ev(evContextPop, 2, [], [2], 2),
      ev(evMenu, xy2, 2, [], [2], 2),

      ev(evMsgRDown),
      ev(evMouseDown, [ssRight], [1], 1),
      ev(evMsgRUp),
      ev(evMouseUp, [], [1], 1),
      ev(evMsgContext),
      ev(evContextPop, 1, [], [1], 1),
      ev(evMenu, xy1, 1, [], [1], 1),
      ev(evMenuItem, 1),
      ev(evStoreSelection, [1], 1)
    ]);

    (* *****
       pop up item, then empty
     * *****)
    ClearTestEvents;
    FListView.ClearSelection;
    TTestThread.Run(mbRight, p2, p2, [], [RBtn(p99), pp99], [], []);
    StoreSelectionState;
    ClickButton;

    CheckTestEvents('pop item, pop empty '+TstName, [
      ev(evMsgRDown),
      ev(evMouseDown, [ssRight], [2], 2),
      ev(evMsgRUp),
      ev(evMouseUp, [], [2], 2),
      ev(evMsgContext),
      ev(evContextPop, 2, [], [2], 2),
      ev(evMenu, xy2, 2, [], [2], 2),

      ev(evMsgRDown),
      ev(evMouseDown, [ssRight], NO_SEL),
      ev(evMsgRUp),
      ev(evMouseUp, [], NO_SEL),
      ev(evMsgContext),
      ev(evContextPop, -1, [], NO_SEL),
      ev(evMenu, xy99, -1, [], NO_SEL),
      ev(evMenuItem, 1),
      ev(evStoreSelection, NO_SEL)
    ]);




    (* *********
     * *********)

    (* *****
       pop up over empty
     * *****)
    ClearTestEvents;
    FListView.ClearSelection;
    FListView.Items[1].Selected := True;
    TTestThread.Run(mbRight, p99, p99, [], [pp99], [], []);
    StoreSelectionState;
    ClickButton;

    CheckTestEvents('pop empty '+TstName, [
      ev(evMsgRDown),
      ev(evMouseDown, [ssRight], NO_SEL),
      ev(evMsgRUp),
      ev(evMouseUp, [], NO_SEL),
      ev(evMsgContext),
      ev(evContextPop, -1, [], NO_SEL),
      ev(evMenu, xy99, -1, [], NO_SEL),
      ev(evMenuItem, 1),
      ev(evStoreSelection, NO_SEL)
    ]);


    (* *********
       right down, then mouse move, then pop
     * *********)

    (* *****
       empty => other empty
     * *****)
    ClearTestEvents;
    FListView.ClearSelection;
    TTestThread.Run(mbRight, p99, p98, [], [pp98], [], []);
    StoreSelectionState;
    ClickButton;

    CheckTestEvents('down empty, pop other empty'+TstName, [
      ev(evMsgRDown),
      ev(evMouseDown, [ssRight], NO_SEL),
      ev(evMsgRUp),
      ev(evMouseUp, [], NO_SEL),
      ev(evMsgContext),
      ev(evContextPop, -1, [], NO_SEL),
      ev(evMenu, xy98, -1, [], NO_SEL),
      ev(evMenuItem, 1),
      ev(evStoreSelection, NO_SEL)
    ]);

    (* *****
       empty => item
     * *****)
    ClearTestEvents;
    FListView.ClearSelection;
    TTestThread.Run(mbRight, p99, p4, [], [pp4], [], []);
    StoreSelectionState;
    ClickButton;

    if Multi then
      ExpSel := [4]
    else
      ExpSel := NO_SEL;

    CheckTestEvents('down empty, pop item '+TstName, [
      ev(evMsgRDown),
      ev(evMouseDown, [ssRight], ExpSel),
      ev(evMsgRUp),
      ev(evMouseUp, [], ExpSel),
      ev(evMsgContext),
      ev(evContextPop, 4, [], ExpSel),
      ev(evMenu, xy4, 4, [], ExpSel),
      ev(evMenuItem, 1),
      ev(evStoreSelection, ExpSel)
    ]);


    (* *********
       pop first menu, then while open pop 2nd menu
     * *********)

    (* *****
       pop up empty, then other empty
     * *****)
    ClearTestEvents;
    FListView.ClearSelection;
    TTestThread.Run(mbRight, p99, p99, [], [RBtn(p98), pp98], [], []);
    StoreSelectionState;
    ClickButton;

    CheckTestEvents('pop empty, pop other item '+TstName, [
      ev(evMsgRDown),
      ev(evMouseDown, [ssRight], NO_SEL),
      ev(evMsgRUp),
      ev(evMouseUp, [], NO_SEL),
      ev(evMsgContext),
      ev(evContextPop, -1, [], NO_SEL),
      ev(evMenu, xy99, -1, [], NO_SEL),

      ev(evMsgRDown),
      ev(evMouseDown, [ssRight], NO_SEL),
      ev(evMsgRUp),
      ev(evMouseUp, [], NO_SEL),
      ev(evMsgContext),
      ev(evContextPop, -1, [], NO_SEL),
      ev(evMenu, xy98, -1, [], NO_SEL),
      ev(evMenuItem, 1),
      ev(evStoreSelection, NO_SEL)
    ]);

    (* *****
       pop up empty, then item
     * *****)
    ClearTestEvents;
    FListView.ClearSelection;
    TTestThread.Run(mbRight, p99, p99, [], [RBtn(p1), pp1], [], []);
    StoreSelectionState;
    ClickButton;

    CheckTestEvents('pop empty, pop other item '+TstName, [
      ev(evMsgRDown),
      ev(evMouseDown, [ssRight], NO_SEL),
      ev(evMsgRUp),
      ev(evMouseUp, [], NO_SEL),
      ev(evMsgContext),
      ev(evContextPop, -1, [], NO_SEL),
      ev(evMenu, xy99, -1, [], NO_SEL),

      ev(evMsgRDown),
      ev(evMouseDown, [ssRight], [1], 1),
      ev(evMsgRUp),
      ev(evMouseUp, [], [1], 1),
      ev(evMsgContext),
      ev(evContextPop, 1, [], [1], 1),
      ev(evMenu, xy1, 1, [], [1], 1),
      ev(evMenuItem, 1),
      ev(evStoreSelection, [1], 1)
    ]);


  end;

  AssertEquals('', FTestError);
end;

initialization
  AddToLCLTestSuite(TTestListView);
{$IFDEF WINDOWS}
  FTestMouseInput := TTestMouseInput.Create;
{$ELSE}
  FTestMouseInput := MouseAndKeyInput.MouseInput;
{$ENDIF}

end.

