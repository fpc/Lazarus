unit WatchInspectToolbar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, Buttons, StdCtrls, ExtCtrls,
  Menus, LCLType, EditBtn, SpinEx, IDEImagesIntf, LazUTF8, LazClasses,
  LazDebuggerIntf, IdeDebuggerStringConstants, ArrayNavigationFrame,
  IdeDebuggerOpts, Debugger, IdeDebuggerBackendValueConv, IdeDebuggerBase;

type

  TEvalHistDirection=(EHDNone,EHDUp,EHDDown);
  TEvalHistDirectionChangedEvent = procedure(Sender: TObject; NewDir: TEvalHistDirection) of object;

  TInspectWatchBeforeUdateEvent = function(ASender: TObject): boolean of object;
  TInspectWatchUpdatedEvent = procedure (const ASender: TIdeWatches; const AWatch: TIdeWatch) of object;

  { TWatchInspectNav }

  TWatchInspectNav = class(TFrame)
    btnDisplayFormat: TToolButton;
    edFilter: TEditButton;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    mnuHistory: TPopupMenu;
    Panel1: TPanel;
    EdInspect: TComboBox;
    popConverter: TPopupMenu;
    popDispForm: TPopupMenu;
    BtnExecute: TSpeedButton;
    ToolBar1: TToolBar;
    btnPower: TToolButton;
    tbDivPower: TToolButton;
    btnBackward: TToolButton;
    btnForward: TToolButton;
    tbDivForwBackw: TToolButton;
    btnUseInstance: TToolButton;
    btnFunctionEval: TToolButton;
    btnUseConverter: TToolButton;
    tbDivFlags: TToolButton;
    btnColClass: TToolButton;
    btnColType: TToolButton;
    btnColVisibility: TToolButton;
    tbDivCol: TToolButton;
    ArrayNavigationBar1: TArrayNavigationBar;
    tbDivArray: TToolButton;
    BtnAddWatch: TToolButton;
    BtnInspect: TToolButton;
    BtnEvaluate: TToolButton;
    tbDivAdd: TToolButton;
    btnEvalHistory: TToolButton;
    procedure BtnAddWatchClick(Sender: TObject);
    procedure btnBackwardClick(Sender: TObject);
    procedure btnColTypeClick(Sender: TObject);
    procedure BtnEvaluateClick(Sender: TObject);
    procedure btnForwardClick(Sender: TObject);
    procedure btnFunctionEvalClick(Sender: TObject);
    procedure BtnInspectClick(Sender: TObject);
    procedure btnPowerClick(Sender: TObject);
    procedure EdInspectChange(Sender: TObject);
    procedure EdInspectEditingDone(Sender: TObject);
    procedure EdInspectKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FrameResize(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure BtnExecuteClick(Sender: TObject);
  private const
    MAX_HISTORY = 1000;
  private
    FDefaultEvalOpts: TWatcheEvaluateFlags;
    FOnAddEvaluateClicked: TNotifyEvent;
    FOnAddInspectClicked: TNotifyEvent;
    FOnAddWatchClicked: TNotifyEvent;
    FOnBeforeEvaluate: TInspectWatchBeforeUdateEvent;
    FOnClear: TNotifyEvent;
    FOnColumnsChanged: TNotifyEvent;
    FOnDisplayFormatChanged: TNotifyEvent;
    FOnEvalHistDirectionChanged: TEvalHistDirectionChangedEvent;
    FOnWatchUpdated: TInspectWatchUpdatedEvent;
    FCurrentWatchValue: TIdeWatchValue;
    FEvalHistDirection: TEvalHistDirection;
    FExpression: String;
    FHistoryListMaxCount: integer;
    FHistoryList: TStrings;
    FBrowseHistoryIndex: Integer;
    FBrowseHistory: TStringList;
    FPowerImgIdx, FPowerImgIdxGrey: Integer;

    FThreadsMonitor:   TIdeThreadsMonitor;
    FCallStackMonitor: TIdeCallStackMonitor;
    FInspectWatches: TCurrentWatches;
    FUpdateCount: Integer;
    FExecAfterUpdate: Boolean;

    procedure ArrayNavSizeChanged(Sender: TObject);
    procedure DoDbpConvMenuClicked(Sender: TObject);
    procedure DoDispFormatClicked(Sender: TObject);
    function  GetButtonDown(AIndex: Integer): Boolean;
    function GetButtonEnabled(AIndex: Integer): Boolean;
    function GetDisplayFormat: TWatchDisplayFormat;
    function GetDropDownOpen: boolean;
    function GetExpression: String;
    function  GetOnArrayNavChanged(AIndex: Integer): TArrayNavChangeEvent;
    procedure SetButtonEnabled(AIndex: Integer; AValue: Boolean);
    procedure SetHistoryList(AValue: TStrings);
    procedure SetOnArrayNavChanged(AIndex: Integer; AValue: TArrayNavChangeEvent);
    function  GetShowButtons(AIndex: Integer): Boolean;
    procedure SetShowButtons(AIndex: Integer; AValue: Boolean);

    function  DisplayFormatName(ADispFormat: TWatchDisplayFormat): string;
    procedure DoDbgOptChanged(Sender: TObject; Restore: boolean);
    procedure AddToHistory(AnExpression: String);
    procedure DoClear;
    procedure DoWatchUpdated(AWatch: TIdeWatch);
  protected
    procedure VisibleChanged; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Init(AWatchesMonitor:   TIdeWatchesMonitor;
                   AThreadsMonitor:   TIdeThreadsMonitor;
                   ACallStackMonitor: TIdeCallStackMonitor;
                   ADefaultEvalOpts: TWatcheEvaluateFlags
    );

    procedure BeginUpdate;
    procedure EndUpdate;

    procedure InitWatch(AWatch: TIdeWatch);
    procedure ReadFromWatch(AWatch: TWatch; AlternateExpression: String = '');

    procedure Execute(const AnExpression: ansistring; ASkipHistory: Boolean = False);
    procedure UpdateData(AForceClear: Boolean = False);// context changed instead
    procedure DoContextChanged;

    procedure FocusEnterExpression;
    procedure GoPrevBrowseEntry;
    procedure GoNextBrowseEntry;
    procedure DeleteLastHistoryIf(AnExpression: String);

    property Expression: String read GetExpression;
    property Watches: TCurrentWatches read FInspectWatches;
    property CurrentWatchValue: TIdeWatchValue read FCurrentWatchValue;
    property DefaultEvalOpts: TWatcheEvaluateFlags read FDefaultEvalOpts write FDefaultEvalOpts;
    property HistoryList: TStrings read FHistoryList write SetHistoryList;
    property HistoryListMaxCount: integer read FHistoryListMaxCount write FHistoryListMaxCount;
    property DisplayFormat: TWatchDisplayFormat read GetDisplayFormat;
    property EvalHistDirection: TEvalHistDirection read FEvalHistDirection;
    property DropDownOpen: boolean read GetDropDownOpen;
  published
    property ShowInspectColumns: Boolean index 0 read GetShowButtons write SetShowButtons;
    property ShowArrayNav:       Boolean index 1 read GetShowButtons write SetShowButtons;
    property ShowAddWatch:       Boolean index 2 read GetShowButtons write SetShowButtons;
    property ShowAddInspect:     Boolean index 3 read GetShowButtons write SetShowButtons;
    property ShowAddEval:        Boolean index 4 read GetShowButtons write SetShowButtons;
    property ShowEvalHist:       Boolean index 5 read GetShowButtons write SetShowButtons;
    property ShowCallFunction:   Boolean index 6 read GetShowButtons write SetShowButtons;
    property ShowDisplayFormat:  Boolean index 7 read GetShowButtons write SetShowButtons;

    property ColClassIsDown:        Boolean index 0 read GetButtonDown;
    property ColTypeIsDown:         Boolean index 1 read GetButtonDown;
    property ColVisibilityIsDown:   Boolean index 2 read GetButtonDown;
    property PowerIsDown:           Boolean index 3 read GetButtonDown;
    property UseInstanceIsDown:     Boolean index 4 read GetButtonDown;

    property ColClassEnabled:        Boolean index 0 read GetButtonEnabled write SetButtonEnabled;
    property ColTypeEnabled:         Boolean index 1 read GetButtonEnabled write SetButtonEnabled;
    property ColVisibilityEnabled:   Boolean index 2 read GetButtonEnabled write SetButtonEnabled;

    property OnBeforeEvaluate: TInspectWatchBeforeUdateEvent read FOnBeforeEvaluate write FOnBeforeEvaluate;
    property OnWatchUpdated: TInspectWatchUpdatedEvent read FOnWatchUpdated write FOnWatchUpdated;
    property OnClear: TNotifyEvent read FOnClear write FOnClear;
    property OnColumnsChanged: TNotifyEvent read FOnColumnsChanged write FOnColumnsChanged;
    property OnAddWatchClicked: TNotifyEvent read FOnAddWatchClicked write FOnAddWatchClicked;
    property OnAddEvaluateClicked: TNotifyEvent read FOnAddEvaluateClicked write FOnAddEvaluateClicked;
    property OnAddInspectClicked: TNotifyEvent read FOnAddInspectClicked write FOnAddInspectClicked;
    property OnArrayIndexChanged: TArrayNavChangeEvent index 0 read GetOnArrayNavChanged write SetOnArrayNavChanged;
    property OnArrayPageSize:     TArrayNavChangeEvent index 1 read GetOnArrayNavChanged write SetOnArrayNavChanged;
    property OnDisplayFormatChanged: TNotifyEvent read FOnDisplayFormatChanged write FOnDisplayFormatChanged;
    property OnEvalHistDirectionChanged: TEvalHistDirectionChangedEvent read FOnEvalHistDirectionChanged write FOnEvalHistDirectionChanged;
  end;

implementation

{$R *.lfm}

{ TWatchInspectNav }

function TWatchInspectNav.GetShowButtons(AIndex: Integer): Boolean;
begin
  case AIndex of
    0: Result := btnColClass.Visible;
    1: Result := ArrayNavigationBar1.Visible;
    2: Result := BtnAddWatch.Visible;
    3: Result := BtnInspect.Visible;
    4: Result := BtnEvaluate.Visible;
    5: Result := btnEvalHistory.Visible;
    6: Result := btnFunctionEval .Visible;
    7: Result := btnDisplayFormat.Visible;
  end;
end;

procedure TWatchInspectNav.SetOnArrayNavChanged(AIndex: Integer;
  AValue: TArrayNavChangeEvent);
begin
  case AIndex of
    0: ArrayNavigationBar1.OnIndexChanged := AValue;
    1: ArrayNavigationBar1.OnPageSize     := AValue;
  end;
end;

procedure TWatchInspectNav.MenuItem1Click(Sender: TObject);
begin
  if Sender = MenuItem3 then begin
    FEvalHistDirection := EHDDown;
    btnEvalHistory.ImageIndex := IDEImages.LoadImage('callstack_goto');
  end
  else
  if Sender = MenuItem2 then begin
    FEvalHistDirection := EHDUp;
    btnEvalHistory.ImageIndex := IDEImages.LoadImage('evaluate_up');
  end
  else begin
    FEvalHistDirection := EHDNone;
    btnEvalHistory.ImageIndex := IDEImages.LoadImage('evaluate_no_hist');
  end;
  if FOnEvalHistDirectionChanged <> nil then
    FOnEvalHistDirectionChanged(Self, FEvalHistDirection);
  FrameResize(nil);
end;

procedure TWatchInspectNav.BtnExecuteClick(Sender: TObject);
begin
  DoClear;
  ReleaseRefAndNil(FCurrentWatchValue);
  FInspectWatches.Clear;
  Execute(EdInspect.Text);
end;

procedure TWatchInspectNav.EdInspectKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) then
    EdInspectEditingDone(nil)
  else
  if (Key = VK_UP) or (Key = VK_DOWN) then
    EdInspect.DroppedDown := True;
end;

procedure TWatchInspectNav.FrameResize(Sender: TObject);
var
  w, h: Integer;
begin
  ToolBar1.InvalidatePreferredSize;
  Height := ToolBar1.Top + ToolBar1.Height;
end;

procedure TWatchInspectNav.EdInspectEditingDone(Sender: TObject);
begin
  if FExpression = EdInspect.Text then
    exit;
  Execute(EdInspect.Text);
end;

procedure TWatchInspectNav.btnColTypeClick(Sender: TObject);
begin
  if FOnColumnsChanged <> nil then
    FOnColumnsChanged(Self);
end;

procedure TWatchInspectNav.BtnEvaluateClick(Sender: TObject);
begin
  if FOnAddEvaluateClicked <> nil then
    FOnAddEvaluateClicked(Self);
end;

procedure TWatchInspectNav.btnForwardClick(Sender: TObject);
begin
  GoNextBrowseEntry;
end;

procedure TWatchInspectNav.btnFunctionEvalClick(Sender: TObject);
begin
  UpdateData;
end;

procedure TWatchInspectNav.BtnInspectClick(Sender: TObject);
begin
  if FOnAddInspectClicked <> nil then
    FOnAddInspectClicked(Self);
end;

procedure TWatchInspectNav.btnBackwardClick(Sender: TObject);
begin
  GoPrevBrowseEntry;
end;

procedure TWatchInspectNav.BtnAddWatchClick(Sender: TObject);
begin
  if FOnAddWatchClicked <> nil then
    FOnAddWatchClicked(Self);
end;

procedure TWatchInspectNav.btnPowerClick(Sender: TObject);
begin
  if btnPower.Down
  then begin
    btnPower.ImageIndex := FPowerImgIdx;
    DoClear; // todo: only get result data released
    ReleaseRefAndNil(FCurrentWatchValue);
    FInspectWatches.Clear;
    UpdateData;
  end
  else begin
    btnPower.ImageIndex := FPowerImgIdxGrey;
  end;
end;

procedure TWatchInspectNav.EdInspectChange(Sender: TObject);
begin
  BtnExecute.Enabled := EdInspect.Text <> '';
  BtnAddWatch.Enabled := EdInspect.Text <> '';
  BtnEvaluate.Enabled := EdInspect.Text <> '';
  BtnInspect.Enabled := EdInspect.Text <> '';
end;

function TWatchInspectNav.GetOnArrayNavChanged(AIndex: Integer
  ): TArrayNavChangeEvent;
begin
  case AIndex of
    0: Result := ArrayNavigationBar1.OnIndexChanged;
    1: Result := ArrayNavigationBar1.OnPageSize;
  end;
end;

procedure TWatchInspectNav.SetButtonEnabled(AIndex: Integer; AValue: Boolean);
begin
  case AIndex of
    0: btnColClass.Enabled := AValue;
    1: btnColType.Enabled := AValue;
    2: btnColVisibility.Enabled := AValue;
  end;
end;

function TWatchInspectNav.GetButtonDown(AIndex: Integer): Boolean;
begin
  case AIndex of
    0: Result := btnColClass.Down;
    1: Result := btnColType.Down;
    2: Result := btnColVisibility.Down;
    3: Result := btnPower.Down;
    4: Result := btnUseInstance.Down;
  end;
end;

function TWatchInspectNav.GetButtonEnabled(AIndex: Integer): Boolean;
begin
  case AIndex of
    0: Result := btnColClass.Enabled;
    1: Result := btnColType.Enabled;
    2: Result := btnColVisibility.Enabled;
  end;
end;

function TWatchInspectNav.GetDisplayFormat: TWatchDisplayFormat;
begin
  Result := TWatchDisplayFormat(btnDisplayFormat.Tag);
end;

function TWatchInspectNav.GetDropDownOpen: boolean;
begin
  Result := EdInspect.Focused and EdInspect.DroppedDown;
end;

function TWatchInspectNav.GetExpression: String;
begin
  Result := EdInspect.Text;
end;

procedure TWatchInspectNav.DoDbpConvMenuClicked(Sender: TObject);
begin
  btnUseConverter.Tag := TMenuItem(Sender).Tag;
  btnUseConverter.Caption := TMenuItem(Sender).Caption;
  FrameResize(nil);
  UpdateData;
end;

procedure TWatchInspectNav.DoDispFormatClicked(Sender: TObject);
begin
  btnDisplayFormat.Caption := TMenuItem(Sender).Caption;
  btnDisplayFormat.Tag := TMenuItem(Sender).Tag;
  if FOnDisplayFormatChanged <> nil then
    FOnDisplayFormatChanged(Self);

  FrameResize(nil);
end;

procedure TWatchInspectNav.ArrayNavSizeChanged(Sender: TObject);
begin
  FrameResize(nil);
end;

procedure TWatchInspectNav.SetHistoryList(AValue: TStrings);
begin
  if FHistoryList = AValue then Exit;
  FHistoryList := AValue;
  if FHistoryList <> nil then
    EdInspect.Items.Assign(FHistoryList);
end;

procedure TWatchInspectNav.SetShowButtons(AIndex: Integer; AValue: Boolean);
begin
  case AIndex of
    0: begin
       btnColClass.Visible         := AValue;
       btnColType.Visible          := AValue;
       btnColVisibility.Visible    := AValue;
       end;
    1: ArrayNavigationBar1.Visible := AValue;
    2: BtnAddWatch.Visible         := AValue;
    3: BtnInspect.Visible          := AValue;
    4: BtnEvaluate.Visible         := AValue;
    5: btnEvalHistory.Visible      := AValue;
    6: begin
       btnFunctionEval.Visible     := AValue;
       if not AValue then
         btnFunctionEval.Down      := False;
       end;
    7: btnDisplayFormat.Visible    := AValue;
  end;

  tbDivCol.Visible   := (ShowInspectColumns or ShowDisplayFormat);
  tbDivArray.Visible := (ShowArrayNav);
  tbDivAdd.Visible   := (ShowAddWatch or ShowAddEval or ShowAddInspect) and
                        ShowEvalHist;
  FrameResize(nil);
end;

function TWatchInspectNav.DisplayFormatName(ADispFormat: TWatchDisplayFormat
  ): string;
begin
  Result := '?';
  case ADispFormat of
    wdfDefault:   Result := dbgDispFormatDefault     ;
    wdfChar:      Result := dbgDispFormatCharacter   ;
    wdfString:    Result := dbgDispFormatString      ;
    wdfDecimal:   Result := dbgDispFormatDecimal     ;
    wdfUnsigned:  Result := dbgDispFormatUnsigned    ;
    wdfHex:       Result := dbgDispFormatHexadecimal ;
    wdfBinary:    Result := dbgDispFormatBinary      ;
    wdfFloat:     Result := dbgDispFormatFloatingPoin;
    wdfPointer:   Result := dbgDispFormatPointer     ;
    wdfStructure: Result := dbgDispFormatRecordStruct;
    wdfMemDump:   Result := dbgDispFormatMemoryDump  ;
  end;
end;

procedure TWatchInspectNav.DoDbgOptChanged(Sender: TObject; Restore: boolean);
var
  m: TMenuItem;
  i: Integer;
begin
  popConverter.Items.Clear;

  m := TMenuItem.Create(Self);
  m.Caption := drsDebugConverter;
  m.OnClick := @DoDbpConvMenuClicked;
  m.Tag := -2;
  popConverter.Items.Add(m);

  m := TMenuItem.Create(Self);
  m.Caption := drsNoDebugConverter;
  m.OnClick := @DoDbpConvMenuClicked;
  m.Tag := -1;
  popConverter.Items.Add(m);

  for i := 0 to DebuggerOptions.BackendConverterConfig.Count - 1 do begin
    m := TMenuItem.Create(Self);
    m.Caption := DebuggerOptions.BackendConverterConfig.IdeItems[i].Name;
    m.OnClick := @DoDbpConvMenuClicked;
    m.Tag := i;
    popConverter.Items.Add(m)
  end;

  btnUseConverter.Visible := DebuggerOptions.BackendConverterConfig.Count > 0;
  btnUseConverter.Tag := -2;
  btnUseConverter.Caption := drsDebugConverter;
  FrameResize(nil);
  UpdateData;
end;

procedure TWatchInspectNav.AddToHistory(AnExpression: String);
var
  i: Integer;
begin
  inc(FBrowseHistoryIndex);
  while FBrowseHistory.Count > FBrowseHistoryIndex do
    FBrowseHistory.Delete(FBrowseHistoryIndex);
  while FBrowseHistory.Count > MAX_HISTORY - 1 do
    FBrowseHistory.Delete(0);
  if (FBrowseHistory.Count = 0) or (FBrowseHistory[FBrowseHistory.Count-1] <> AnExpression)
  then
    FBrowseHistoryIndex := FBrowseHistory.Add(AnExpression);

  if FHistoryList <> nil then begin
    if (FHistoryList.Count = 0) or
       (UTF8CompareLatinTextFast(FHistoryList[0], AnExpression) <> 0)
    then begin
      for i:=FHistoryList.Count-1 downto 0 do
        if UTF8CompareLatinTextFast(FHistoryList[i], AnExpression) = 0 then
          FHistoryList.Delete(i);

      FHistoryList.Insert(0, AnExpression);

      if FHistoryListMaxCount > 0 then
        while FHistoryList.Count > FHistoryListMaxCount do
          FHistoryList.Delete(FHistoryList.Count-1);
    end;
    EdInspect.Items.Assign(FHistoryList);
  end
  else
    EdInspect.Items.Insert(0, AnExpression);
end;

constructor TWatchInspectNav.Create(TheOwner: TComponent);
var
  df: TWatchDisplayFormat;
  m: TMenuItem;
begin
  inherited Create(TheOwner);
  FBrowseHistory := TStringList.Create;

  EdInspect.TextHint := drsEnterExpression;

  ToolBar1.Images := IDEImages.Images_16;

  FPowerImgIdx     := IDEImages.LoadImage('debugger_power');
  FPowerImgIdxGrey := IDEImages.LoadImage('debugger_power_grey');
  btnPower.ImageIndex := FPowerImgIdx;
  btnPower.Caption := '';
  btnPower.Hint := drsDisableEnableUpdatesForTh;

  btnBackward.ImageIndex := IDEImages.LoadImage('arrow_left');
  btnBackward.Caption := '';
  btnForward.ImageIndex := IDEImages.LoadImage('arrow_right');
  btnForward.Caption := '';

  btnUseInstance.Caption := drsUseInstanceClass;
  btnUseInstance.Hint := drsUseInstanceClassHint;

  BtnEvaluate.Caption := drsUseFunctionCalls;
  BtnEvaluate.Hint := drsUseFunctionCallsHint;

  btnColClass.Hint       := lisInspectShowColClass;
  btnColType.Hint        := lisInspectShowColType;
  btnColVisibility.Hint  := lisInspectShowColVisibility;

  BtnAddWatch.ImageIndex := IDEImages.LoadImage('debugger_watches');
  BtnAddWatch.Caption := drsAddWatch;
  BtnEvaluate.ImageIndex := IDEImages.LoadImage('debugger_evaluate');
  BtnEvaluate.Caption := drsEvaluate;
  BtnInspect.ImageIndex := IDEImages.LoadImage('debugger_inspect');
  BtnInspect.Caption := drsInspect;

  edFilter.Images := IDEImages.Images_16;
  edFilter.ImageIndex := IDEImages.LoadImage('btnfiltercancel');

  btnEvalHistory.Caption := drsHistory;
  btnEvalHistory.ImageIndex := IDEImages.LoadImage('evaluate_no_hist');
  mnuHistory.Items[0].Caption := drsNoHistoryKept;
  mnuHistory.Items[1].Caption := drsInsertResultAtTopOfHistor;
  mnuHistory.Items[2].Caption := drsAppendResultAtBottomOfHis;

  for df := low(TWatchDisplayFormat) to high(TWatchDisplayFormat) do begin
    if df = wdfMemDump then
      continue;
    m := TMenuItem.Create(Self);
    m.Caption := DisplayFormatName(df);
    m.Tag := ord(df);
    m.OnClick := @DoDispFormatClicked;
    popDispForm.Items.Add(m);
  end;
  btnDisplayFormat.Caption := DisplayFormatName(wdfStructure);
  btnDisplayFormat.Tag := Ord(wdfStructure);


  btnBackward.Enabled := False;
  btnForward.Enabled  := False;

  btnColClass.Enabled := False;
  btnColType.Enabled := False;
  btnColVisibility.Enabled := False;

  FEvalHistDirection:=EHDNone;

  btnBackward.Enabled := False;
  btnForward.Enabled := False;

  ArrayNavigationBar1.OnSizeChanged := @ArrayNavSizeChanged;

  DebuggerOptions.AddHandlerAfterWrite(@DoDbgOptChanged);
  DoDbgOptChanged(nil, False);

  EdInspectChange(nil);
end;

destructor TWatchInspectNav.Destroy;
begin
  DebuggerOptions.RemoveHandlerAfterWrite(@DoDbgOptChanged);
  inherited Destroy;
  FBrowseHistory.Free;
  ReleaseRefAndNil(FCurrentWatchValue);
  FreeAndNil(FInspectWatches);
end;

procedure TWatchInspectNav.Init(AWatchesMonitor: TIdeWatchesMonitor;
  AThreadsMonitor: TIdeThreadsMonitor; ACallStackMonitor: TIdeCallStackMonitor;
  ADefaultEvalOpts: TWatcheEvaluateFlags);
begin
  FInspectWatches := TCurrentWatches.Create(AWatchesMonitor);
  FCallStackMonitor := ACallStackMonitor;
  FThreadsMonitor := AThreadsMonitor;
  FDefaultEvalOpts := ADefaultEvalOpts;
end;

procedure TWatchInspectNav.BeginUpdate;
begin
  if FUpdateCount = 0 then
    FExecAfterUpdate := False;
  inc(FUpdateCount);
end;

procedure TWatchInspectNav.EndUpdate;
begin
  if FUpdateCount > 0 then
    dec(FUpdateCount);
  if (FUpdateCount = 0) and FExecAfterUpdate then
    UpdateData(True);
end;

procedure TWatchInspectNav.InitWatch(AWatch: TIdeWatch);
var
  Opts: TWatcheEvaluateFlags;
  Conv: TIdeDbgValueConvertSelector;
begin
  Opts := AWatch.EvaluateFlags;
  if btnUseInstance.Down then
    include(Opts, defClassAutoCast);
  if btnFunctionEval.Down then
    include(Opts, defAllowFunctionCall);

  Conv := nil;
  case btnUseConverter.Tag of
    -2: ;
    -1: include(Opts, defSkipValConv);
    otherwise begin
      Conv := DebuggerOptions.BackendConverterConfig.IdeItems[btnUseConverter.Tag];
    end
  end;

  AWatch.EvaluateFlags := Opts;
  AWatch.DbgBackendConverter := Conv;
end;

procedure TWatchInspectNav.ReadFromWatch(AWatch: TWatch;
  AlternateExpression: String);
var
  i: Integer;
begin
  BeginUpdate;
  try

    btnUseInstance.Down := defClassAutoCast in AWatch.EvaluateFlags;
    btnFunctionEval.Down := defAllowFunctionCall in AWatch.EvaluateFlags;

    if defSkipValConv in AWatch.EvaluateFlags then begin
      if popConverter.Items.Count > 1 then
        popConverter.Items[1].Click;
    end
    else if AWatch.DbgBackendConverter = nil then begin
      if popConverter.Items.Count > 0 then
        popConverter.Items[0].Click;
    end
    else begin
      i := DebuggerOptions.BackendConverterConfig.Count - 1;
      while i >= 0 do begin
        if DebuggerOptions.BackendConverterConfig.IdeItems[i] = AWatch.DbgBackendConverter then begin
          if popConverter.Items.Count > i+2 then
            popConverter.Items[i+2].Click;
          break;
        end;
        dec(i);
      end;
    end;

    if AlternateExpression <> '' then
      Execute(AlternateExpression)
    else
      Execute(AWatch.Expression);
  finally
    EndUpdate;
  end;
end;

procedure TWatchInspectNav.Execute(const AnExpression: ansistring;
  ASkipHistory: Boolean);
begin
  if not ASkipHistory then
    AddToHistory(AnExpression);

  FExpression := AnExpression;
  EdInspect.Text := FExpression;
  EdInspectChange(nil);
  ArrayNavigationBar1.Index := 0;
  UpdateData;
end;

procedure TWatchInspectNav.DoContextChanged;
begin
  if (not btnPower.Down) or (not Visible) then exit;
  UpdateData;
end;

procedure TWatchInspectNav.FocusEnterExpression;
begin
  if IsVisible then
    EdInspect.SetFocus;
end;

procedure TWatchInspectNav.GoPrevBrowseEntry;
begin
  if FBrowseHistoryIndex <= 0 then
    exit;

  if FBrowseHistoryIndex >= FBrowseHistory.Count then
    FBrowseHistoryIndex := FBrowseHistory.Count - 1;
  if FBrowseHistoryIndex <= 0 then
    exit;

  dec(FBrowseHistoryIndex);
  Execute(FBrowseHistory[FBrowseHistoryIndex], True);
end;

procedure TWatchInspectNav.GoNextBrowseEntry;
begin
  if FBrowseHistoryIndex >= FBrowseHistory.Count -1 then
    exit;

  if FBrowseHistoryIndex < 0 then
    FBrowseHistoryIndex := 0;

  inc(FBrowseHistoryIndex);
  Execute(FBrowseHistory[FBrowseHistoryIndex], True);
end;

procedure TWatchInspectNav.DeleteLastHistoryIf(AnExpression: String);
begin
  if (FBrowseHistory.Count > 0) and (FBrowseHistory[FBrowseHistory.Count-1] = AnExpression)
  then
    FBrowseHistory.Delete(FBrowseHistory.Count-1);

  if (FHistoryList <> nil) and (FHistoryList.Count > 0) and (FHistoryList[0] = AnExpression)
  then
    FHistoryList.Delete(0);
end;

procedure TWatchInspectNav.UpdateData(AForceClear: Boolean);
var
  Opts: TWatcheEvaluateFlags;
  AWatch: TCurrentWatch;
  tid, idx: Integer;
  stack: TIdeCallStack;
  expr: String;
  Conv: TIdeDbgValueConvertSelector;
begin
  if FUpdateCount > 0 then begin
    FExecAfterUpdate := True;
    exit;
  end;
  FExecAfterUpdate := False;

  if AForceClear then
    FInspectWatches.Clear;

  btnBackward.Enabled := FBrowseHistoryIndex > 0;
  btnForward.Enabled := FBrowseHistoryIndex < FBrowseHistory.Count - 1;

  expr := trim(FExpression);
  if expr = '' then begin
    ReleaseRefAndNil(FCurrentWatchValue);
    DoClear;
    exit;
  end;

  if FOnBeforeEvaluate = nil then
    exit;
  if not FOnBeforeEvaluate(Self) then
    exit;

  if (FCallStackMonitor = nil) or (FThreadsMonitor = nil)
//     or (DebugBoss.State <> dsPause)
  then
    exit;

  tid    := FThreadsMonitor.CurrentThreads.CurrentThreadId;
  stack  := FCallStackMonitor.CurrentCallStackList.EntriesForThreads[tid];
  idx := 0;
  if stack <> nil then
    idx := stack.CurrentIndex;

  Opts := FDefaultEvalOpts;
  if btnUseInstance.Down then
    include(Opts, defClassAutoCast);
  if btnFunctionEval.Down then
    include(Opts, defAllowFunctionCall);

  Conv := nil;
  case btnUseConverter.Tag of
    -2: ;
    -1: include(Opts, defSkipValConv);
    otherwise begin
      Conv := DebuggerOptions.BackendConverterConfig.IdeItems[btnUseConverter.Tag];
    end
  end;

  if (FCurrentWatchValue <> nil) and
     (FCurrentWatchValue.Expression = expr) and
     (FCurrentWatchValue.EvaluateFlags = Opts) and
     (FCurrentWatchValue.ThreadId = tid) and
     (FCurrentWatchValue.StackFrame = idx) and
     (FCurrentWatchValue.Watch <> nil) and
     (FCurrentWatchValue.Watch.DbgBackendConverter = Conv)
  then begin
    FCurrentWatchValue.Value;
    DoWatchUpdated(FCurrentWatchValue.Watch);
    exit;
  end;

  ReleaseRefAndNil(FCurrentWatchValue);

  FInspectWatches.BeginUpdate;
  AWatch := FInspectWatches.Find(expr);
  if AWatch = nil then begin
    FInspectWatches.Clear;
    AWatch := FInspectWatches.Add(expr);
    ArrayNavigationBar1.Index := 0;
  end;
  AWatch.EvaluateFlags := Opts;
  AWatch.DbgBackendConverter := Conv;
  AWatch.Enabled := True;
  AWatch.RepeatCount := 0;
  if ArrayNavigationBar1.Visible then
    AWatch.RepeatCount := ArrayNavigationBar1.PageSize;
  FInspectWatches.EndUpdate;
  FCurrentWatchValue := AWatch.Values[tid, idx];
  if FCurrentWatchValue <> nil then begin
    FCurrentWatchValue.AddReference;
    FCurrentWatchValue.Value;
  end;
  DoWatchUpdated(AWatch);
end;

procedure TWatchInspectNav.DoClear;
begin
  if FOnClear <> nil then
    FOnClear(Self);
end;

procedure TWatchInspectNav.DoWatchUpdated(AWatch: TIdeWatch);
begin
  if FOnWatchUpdated <> nil then
    FOnWatchUpdated(FInspectWatches, AWatch);
end;

procedure TWatchInspectNav.VisibleChanged;
begin
  inherited VisibleChanged;
  if IsControlVisible then
    FrameResize(nil);
end;

end.

