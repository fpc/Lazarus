unit WatchInspectToolbar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  Forms, Controls, ComCtrls, Buttons, StdCtrls, ExtCtrls, Menus, LCLType, EditBtn,
  // LazUtils
  LazClasses,
  // LazControls
  SpinEx,
  // IdeIntf
  IDEImagesIntf, IdeDebuggerWatchValueIntf, SynEdit,
  // Debugger
  LazDebuggerIntf, IdeDebuggerStringConstants, ArrayNavigationFrame,
  IdeDebuggerOpts, Debugger, IdeDebuggerBackendValueConv, IdeDebuggerBase,
  IdeDebuggerDisplayFormats, WatchPropertyDlg, ProjectDebugLink, IdeDebuggerValueFormatter;

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
    mnuWatchFormatPresets: TPopupMenu;
    popValFormatter: TPopupMenu;
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
    btnUseValFormatter: TToolButton;
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
    btnWordWrap: TToolButton;
    procedure BtnAddWatchClick(Sender: TObject);
    procedure btnBackwardClick(Sender: TObject);
    procedure btnColTypeClick(Sender: TObject);
    procedure btnDisplayFormatClick(Sender: TObject);
    procedure BtnEvaluateClick(Sender: TObject);
    procedure btnForwardClick(Sender: TObject);
    procedure btnFunctionEvalClick(Sender: TObject);
    procedure BtnInspectClick(Sender: TObject);
    procedure btnPowerClick(Sender: TObject);
    procedure btnWordWrapClick(Sender: TObject);
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
    FAllowMemDump: Boolean;
    FDisplayFormat: TWatchDisplayFormat;
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
    FOnWordWrapChanged: TNotifyEvent;
    FPowerImgIdx, FPowerImgIdxGrey: Integer;

    FThreadsMonitor:   TIdeThreadsMonitor;
    FCallStackMonitor: TIdeCallStackMonitor;
    FInspectWatches: TCurrentWatches;
    FUpdateCount: Integer;
    FExecAfterUpdate: Boolean;
    FInSetButtonDown: Boolean;

    procedure ArrayNavSizeChanged(Sender: TObject);
    procedure DoDbgValFormatterMenuClicked(Sender: TObject);
    procedure DoBackConverterChanged(Sender: TObject);
    procedure ApplyPreset(APreset: TWatchDisplayFormatPreset);
    procedure DoFormatPresetClickedIde(Sender: TObject);
    procedure DoFormatPresetClickedProject(Sender: TObject);
    procedure DoValFormatterChanged(Sender: TObject);
    procedure DoDbpConvMenuClicked(Sender: TObject);
    function GetDbgValueFormatter: TIdeDbgValueFormatterSelector;
    function GetSkipDbgValueFormatter: Boolean;
    function GetDisplayFormat: TWatchDisplayFormat;
    function  GetButtonDown(AIndex: Integer): Boolean;
    function GetButtonEnabled(AIndex: Integer): Boolean;
    function GetDropDownOpen: boolean;
    function GetExpression: String;
    function  GetOnArrayNavChanged(AIndex: Integer): TArrayNavChangeEvent;
    procedure SetButtonEnabled(AIndex: Integer; AValue: Boolean);
    procedure SetButtorDown(AIndex: Integer; AValue: Boolean);
    procedure SetHistoryList(AValue: TStrings);
    procedure SetOnArrayNavChanged(AIndex: Integer; AValue: TArrayNavChangeEvent);
    function  GetShowButtons(AIndex: Integer): Boolean;
    procedure SetShowButtons(AIndex: Integer; AValue: Boolean);

    procedure AddToHistory(AnExpression: String);
    procedure DoClear;
    procedure DoWatchUpdated(AWatch: TIdeWatch);
    procedure DoDisplaySettingsUpdated;
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
    function  CanReadFromDragObject(AnObject: TObject): boolean;
    procedure ReadFromDragObject(AnObject: TObject);

    procedure Execute(const AnExpression: ansistring; ASkipHistory: Boolean = False);
    procedure UpdateData(AForceClear: Boolean = False);// context changed instead
    procedure DoContextChanged;
    procedure UpdateFormatPresets;

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
    property DbgValueFormatter: TIdeDbgValueFormatterSelector read GetDbgValueFormatter;
    property SkipDbgValueFormatter: Boolean read GetSkipDbgValueFormatter;
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
    property ShowWordWrap:       Boolean index 8 read GetShowButtons write SetShowButtons;
    property ShowFormatPresets:  Boolean index 9 read GetShowButtons write SetShowButtons;
    property AllowMemDump:       Boolean read FAllowMemDump write FAllowMemDump;

    property ColClassIsDown:        Boolean index 0 read GetButtonDown;
    property ColTypeIsDown:         Boolean index 1 read GetButtonDown;
    property ColVisibilityIsDown:   Boolean index 2 read GetButtonDown;
    property PowerIsDown:           Boolean index 3 read GetButtonDown;
    property UseInstanceIsDown:     Boolean index 4 read GetButtonDown;
    property WordWrapIsDown:        Boolean index 5 read GetButtonDown write SetButtorDown;

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
    property OnWordWrapChanged: TNotifyEvent read FOnWordWrapChanged write FOnWordWrapChanged;
  end;

implementation

{$R *.lfm}

procedure AddToStr(var txt: string; add, sep: string);
begin
  if (txt = add) then
    exit;
  if txt <> '' then txt := txt + sep;
  txt := txt + add;
end;

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
    8: Result := btnWordWrap.Visible;
    9: Result := btnDisplayFormat.Style = tbsDropDown;
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

procedure TWatchInspectNav.btnDisplayFormatClick(Sender: TObject);
var
  r: TWatchResultDataKind;
  d: TWatchPropertyDlg;
begin
  r := rdkUnknown;
  if (FCurrentWatchValue <> nil) and (FCurrentWatchValue.Validity = ddsValid) and
     (FCurrentWatchValue.ResultData <> nil)
  then
    r := FCurrentWatchValue.ResultData.ValueKind;
  d := TWatchPropertyDlg.Create(Self.Owner, FDisplayFormat, r, FAllowMemDump, False);
  if d.ShowModal = mrOK then begin
    FDisplayFormat := d.DisplayFormat;
    // DoDisplaySettingsUpdated;
    if FOnDisplayFormatChanged <> nil then
      FOnDisplayFormatChanged(Self);
  end;
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

procedure TWatchInspectNav.btnWordWrapClick(Sender: TObject);
begin
  if Assigned(FOnWordWrapChanged) and not FInSetButtonDown then
    FOnWordWrapChanged(Self);
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

procedure TWatchInspectNav.SetButtorDown(AIndex: Integer; AValue: Boolean);
begin
  FInSetButtonDown := True;
  case AIndex of
    5: btnWordWrap.Down := AValue;
  end;
  FInSetButtonDown := False;
end;

function TWatchInspectNav.GetButtonDown(AIndex: Integer): Boolean;
begin
  case AIndex of
    0: Result := btnColClass.Down;
    1: Result := btnColType.Down;
    2: Result := btnColVisibility.Down;
    3: Result := btnPower.Down;
    4: Result := btnUseInstance.Down;
    5: Result := btnWordWrap.Down;
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

function TWatchInspectNav.GetDropDownOpen: boolean;
begin
  Result := EdInspect.Focused and EdInspect.DroppedDown;
end;

function TWatchInspectNav.GetExpression: String;
begin
  Result := EdInspect.Text;
end;

procedure TWatchInspectNav.DoDbgValFormatterMenuClicked(Sender: TObject);
begin
  btnUseValFormatter.Tag := TMenuItem(Sender).Tag;
  btnUseValFormatter.Caption := TMenuItem(Sender).Caption;
  FrameResize(nil);
  DoDisplaySettingsUpdated;
end;

procedure TWatchInspectNav.DoValFormatterChanged(Sender: TObject);
  function MnItm(c: String; e: TNotifyEvent; t: SizeInt): TMenuItem;
  begin
    Result := TMenuItem.Create(Self);
    Result.Caption := c;
    Result.OnClick := e;
    Result.Tag := t;
  end;

var
  i: Integer;
begin
  popValFormatter.Items.Clear;

  popValFormatter.Items.Add(MnItm(drsDebugValFormatter, @DoDbgValFormatterMenuClicked, -2));
  popValFormatter.Items.Add(MnItm(drsNoValFormatter, @DoDbgValFormatterMenuClicked, -1));

  for i := 0 to DebuggerOptions.ValueFormatterConfig.Count - 1 do
    popValFormatter.Items.Add(MnItm(DebuggerOptions.ValueFormatterConfig[i].Name, @DoDbgValFormatterMenuClicked, PtrInt(DebuggerOptions.ValueFormatterConfig[i])));
  for i := 0 to DbgProjectLink.ValueFormatterConfig.Count - 1 do
    popValFormatter.Items.Add(MnItm(DbgProjectLink.ValueFormatterConfig[i].Name, @DoDbgValFormatterMenuClicked, PtrInt(DbgProjectLink.ValueFormatterConfig[i])));

  btnUseValFormatter.Visible := DebuggerOptions.ValueFormatterConfig.Count + DbgProjectLink.ValueFormatterConfig.Count > 0;
  btnUseValFormatter.Tag := -2;
  btnUseValFormatter.Caption := drsDebugValFormatter;
  FrameResize(nil);
  DoDisplaySettingsUpdated;
end;

function TWatchInspectNav.GetDisplayFormat: TWatchDisplayFormat;
begin
  if btnDisplayFormat.Visible then
    Result := FDisplayFormat
  else
    Result := DefaultWatchDisplayFormat;
end;

procedure TWatchInspectNav.ArrayNavSizeChanged(Sender: TObject);
begin
  FrameResize(nil);
end;

procedure TWatchInspectNav.DoDbpConvMenuClicked(Sender: TObject);
begin
  btnUseConverter.Tag := TMenuItem(Sender).Tag;
  btnUseConverter.Caption := TMenuItem(Sender).Caption;
  FrameResize(nil);
  UpdateData;
end;

function TWatchInspectNav.GetDbgValueFormatter: TIdeDbgValueFormatterSelector;
begin
  Result := nil;
  if (btnUseValFormatter.Tag=-1) or (btnUseValFormatter.Tag=-2) then
    exit;
  Result := TIdeDbgValueFormatterSelector(btnUseValFormatter.Tag);
  if (DebuggerOptions.ValueFormatterConfig.IndexOf(Result) < 0) and
     (DbgProjectLink.ValueFormatterConfig.IndexOf(Result) < 0)
  then
    Result := nil;
end;

function TWatchInspectNav.GetSkipDbgValueFormatter: Boolean;
begin
  Result := btnUseValFormatter.Tag = -1;
end;

procedure TWatchInspectNav.DoBackConverterChanged(Sender: TObject);
  function MnItm(c: String; e: TNotifyEvent; t: SizeInt): TMenuItem;
  begin
    Result := TMenuItem.Create(Self);
    Result.Caption := c;
    Result.OnClick := e;
    Result.Tag := t;
  end;

var
  i: Integer;
begin
  popConverter.Items.Clear;

  popConverter.Items.Add(MnItm(drsDebugConverter, @DoDbpConvMenuClicked, -2));
  popConverter.Items.Add(MnItm(drsNoDebugConverter, @DoDbpConvMenuClicked, -1));

  for i := 0 to DebuggerOptions.BackendConverterConfig.Count - 1 do
    popConverter.Items.Add(MnItm(DebuggerOptions.BackendConverterConfig[i].Name, @DoDbpConvMenuClicked, PtrInt(DebuggerOptions.BackendConverterConfig[i])));
  for i := 0 to DbgProjectLink.BackendConverterConfig.Count - 1 do
    popConverter.Items.Add(MnItm(DbgProjectLink.BackendConverterConfig[i].Name, @DoDbpConvMenuClicked, PtrInt(DbgProjectLink.BackendConverterConfig[i])));

  btnUseConverter.Visible := DebuggerOptions.BackendConverterConfig.Count + DbgProjectLink.BackendConverterConfig.Count > 0;
  btnUseConverter.Tag := -2;
  btnUseConverter.Caption := drsDebugConverter;
  FrameResize(nil);
  UpdateData;
end;

procedure TWatchInspectNav.ApplyPreset(APreset: TWatchDisplayFormatPreset);
var
  d: TWatchDisplayFormat;
begin
  d := APreset.DisplayFormat;
  d.CopyInheritedFrom(FDisplayFormat);
  FDisplayFormat := d;

  // DoDisplaySettingsUpdated;
  if FOnDisplayFormatChanged <> nil then
    FOnDisplayFormatChanged(Self);
end;

procedure TWatchInspectNav.DoFormatPresetClickedIde(Sender: TObject);
var
  i: PtrInt;
begin
  if (Sender = nil) or not(Sender is TMenuItem) then
    exit;
  i := TMenuItem(Sender).Tag;
  if (i < 0) or (i >= DebuggerOptions.DisplayFormatConfigs.DisplayFormatPresetCount) then
    exit;

  ApplyPreset(DebuggerOptions.DisplayFormatConfigs.DisplayFormatPresets[i]);
end;

procedure TWatchInspectNav.DoFormatPresetClickedProject(Sender: TObject);
var
  i: PtrInt;
begin
  if (Sender = nil) or not(Sender is TMenuItem) then
    exit;
  i := TMenuItem(Sender).Tag;
  if (i < 0) or (i >= DbgProjectLink.DisplayFormatConfigs.DisplayFormatPresetCount) then
    exit;

  ApplyPreset(DbgProjectLink.DisplayFormatConfigs.DisplayFormatPresets[i]);
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
    8: btnWordWrap.Visible         := AValue;
    9: if AValue then
        btnDisplayFormat.Style := tbsDropDown
      else
        btnDisplayFormat.Style := tbsButton;
  end;

  tbDivCol.Visible   := (ShowInspectColumns or ShowDisplayFormat);
  tbDivArray.Visible := (ShowArrayNav);
  tbDivAdd.Visible   := (ShowAddWatch or ShowAddEval or ShowAddInspect) and
                        ShowEvalHist;
  FrameResize(nil);
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
       (AnsiCompareText(FHistoryList[0], AnExpression) <> 0)
    then begin
      for i:=FHistoryList.Count-1 downto 0 do
        if AnsiCompareText(FHistoryList[i], AnExpression) = 0 then
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
begin
  inherited Create(TheOwner);
  FBrowseHistory := TStringList.Create;

  EdInspect.TextHint := drsEnterExpression;

  ToolBar1.Images := IDEImages.Images_16;
  ToolBar1.OnPaint := @FrameResize;

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
  btnWordWrap.Hint       := lisWordWrapBtnHint;

  btnWordWrap.ImageIndex := IDEImages.LoadImage('line_wrap');

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

  FDisplayFormat := DefaultWatchDisplayFormat;

  btnDisplayFormat.Caption := 'Display-Format';

  btnBackward.Enabled := False;
  btnForward.Enabled  := False;

  btnColClass.Enabled := False;
  btnColType.Enabled := False;
  btnColVisibility.Enabled := False;

  FEvalHistDirection:=EHDNone;

  btnBackward.Enabled := False;
  btnForward.Enabled := False;

  ArrayNavigationBar1.OnSizeChanged := @ArrayNavSizeChanged;

  DebuggerOptions.BackendConverterConfig.AddChangeNotification(@DoBackConverterChanged);
  DbgProjectLink.BackendConverterConfig.AddChangeNotification(@DoBackConverterChanged);
  DoBackConverterChanged(nil);

  DebuggerOptions.ValueFormatterConfig.AddChangeNotification(@DoValFormatterChanged);
  DbgProjectLink.ValueFormatterConfig.AddChangeNotification(@DoValFormatterChanged);
  DoValFormatterChanged(nil);

  EdInspectChange(nil);
end;

destructor TWatchInspectNav.Destroy;
begin
  DebuggerOptions.BackendConverterConfig.RemoveChangeNotification(@DoBackConverterChanged);
  DbgProjectLink.BackendConverterConfig.RemoveChangeNotification(@DoBackConverterChanged);
  DebuggerOptions.ValueFormatterConfig.RemoveChangeNotification(@DoValFormatterChanged);
  DbgProjectLink.ValueFormatterConfig.RemoveChangeNotification(@DoValFormatterChanged);
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
  ValForm: TIdeDbgValueFormatterSelector;
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
      Conv := TIdeDbgValueConvertSelector(btnUseConverter.Tag);
      if (DebuggerOptions.BackendConverterConfig.IndexOf(Conv) < 0) and
         (DbgProjectLink.BackendConverterConfig.IndexOf(Conv) < 0)
      then
        Conv := nil;
    end
  end;
  AWatch.DbgBackendConverter := Conv;

  ValForm := nil;
  case btnUseValFormatter.Tag of
    -2: ;
    -1: include(Opts, defSkipValueFormatter);
    otherwise begin
      ValForm := TIdeDbgValueFormatterSelector(btnUseValFormatter.Tag);
      if (DebuggerOptions.ValueFormatterConfig.IndexOf(ValForm) < 0) and
         (DbgProjectLink.ValueFormatterConfig.IndexOf(ValForm) < 0)
      then
        ValForm := nil;
    end
  end;
  AWatch.DbgValueFormatter := ValForm;

  AWatch.EvaluateFlags := Opts;
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
      i := popConverter.Items.Count - 1;
      while i >= 0 do begin
        if popConverter.Items[i].Tag = PtrUInt(AWatch.DbgBackendConverter) then begin
          popConverter.Items[i].Click;
          break;
        end;
        dec(i);
      end;
    end;

    if defSkipValueFormatter in AWatch.EvaluateFlags then begin
      if popValFormatter.Items.Count > 1 then
        popValFormatter.Items[1].Click;
    end
    else if AWatch.DbgValueFormatter = nil then begin
      if popValFormatter.Items.Count > 0 then
        popValFormatter.Items[0].Click;
    end
    else begin
      i := popValFormatter.Items.Count - 1;
      while i >= 0 do begin
        if popValFormatter.Items[i].Tag = PtrUInt(AWatch.DbgValueFormatter) then begin
          popValFormatter.Items[i].Click;
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

function TWatchInspectNav.CanReadFromDragObject(AnObject: TObject): boolean;
var
  IDrag: IIdeDbgDragDropWatchSource;
begin
  Result := ( (AnObject is TSynEdit) and (TSynEdit(AnObject).SelAvail) ) or
            ( (AnObject is TCustomEdit) and (TCustomEdit(AnObject).SelText <> '') );

  if (not Result) and (
      AnObject.GetInterface(IIdeDbgDragDropWatchSource, IDrag) or
      ( (AnObject is TComponent) and
        TComponent(AnObject).Owner.GetInterface(IIdeDbgDragDropWatchSource, IDrag) )
  )
  then begin
    Result := IDrag.DragWatchCount(AnObject) = 1; // must be exactly one
    IDrag.DragWatchDone(AnObject);
  end;
end;

procedure TWatchInspectNav.ReadFromDragObject(AnObject: TObject);
var
  s: String;
  IDrag: IIdeDbgDragDropWatchSource;
  AWatch: TCurrentWatch;
begin
  if (AnObject is TSynEdit) then begin
    if TSynEdit(AnObject).SelAvail then
      Execute(TSynEdit(AnObject).SelText);
    exit;
  end;

  if (AnObject is TCustomEdit) then begin
    s := TCustomEdit(AnObject).SelText;
    if s <> '' then
      Execute(s);
    exit;
  end;

  if AnObject.GetInterface(IIdeDbgDragDropWatchSource, IDrag) or
     ( (AnObject is TComponent) and
       TComponent(AnObject).Owner.GetInterface(IIdeDbgDragDropWatchSource, IDrag) )
  then begin
    if IDrag.DragWatchCount(AnObject) = 1 then begin
      FInspectWatches.BeginUpdate;
      AWatch := FInspectWatches.Add(''); // Do not destroy the watch, UpdateData will take care of it
      IDrag.DragWatchInit(AnObject, 0, AWatch);
      FInspectWatches.EndUpdate;
      ReadFromWatch(AWatch);
    end;
    IDrag.DragWatchDone(AnObject);
    exit;
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

procedure TWatchInspectNav.UpdateFormatPresets;
var
  i: Integer;
  m: TMenuItem;
begin
  mnuWatchFormatPresets.Items.Clear;
  for i := 0 to DbgProjectLink.DisplayFormatConfigs.DisplayFormatPresetCount - 1 do begin
    m := TMenuItem.Create(Self);
    m.Caption := DbgProjectLink.DisplayFormatConfigs.DisplayFormatPresets[i].Name;
    m.Tag := i;
    m.OnClick := @DoFormatPresetClickedProject;
    mnuWatchFormatPresets.Items.Add(m);
  end;
  if (DbgProjectLink.DisplayFormatConfigs.DisplayFormatPresetCount > 0) and
     (DebuggerOptions.DisplayFormatConfigs.DisplayFormatPresetCount > 0)
  then
    mnuWatchFormatPresets.Items.AddSeparator;
  for i := 0 to DebuggerOptions.DisplayFormatConfigs.DisplayFormatPresetCount - 1 do begin
    m := TMenuItem.Create(Self);
    m.Caption := DebuggerOptions.DisplayFormatConfigs.DisplayFormatPresets[i].Name;
    m.Tag := i;
    m.OnClick := @DoFormatPresetClickedIde;
    mnuWatchFormatPresets.Items.Add(m);
  end;
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
      Conv := TIdeDbgValueConvertSelector(btnUseConverter.Tag);
      if (DebuggerOptions.BackendConverterConfig.IndexOf(Conv) < 0) and
         (DbgProjectLink.BackendConverterConfig.IndexOf(Conv) < 0)
      then
        Conv := nil;
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

procedure TWatchInspectNav.DoDisplaySettingsUpdated;
begin
  if FOnDisplayFormatChanged <> nil then
    FOnDisplayFormatChanged(Self)
  else
  if FCurrentWatchValue <> nil then
    DoWatchUpdated(FCurrentWatchValue.Watch)
  else
    UpdateData;
end;

procedure TWatchInspectNav.VisibleChanged;
begin
  inherited VisibleChanged;
  if IsControlVisible then
    FrameResize(nil);
end;

end.

