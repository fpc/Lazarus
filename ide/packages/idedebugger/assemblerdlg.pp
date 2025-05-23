unit AssemblerDlg;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, Math, types, fgl,
  // LCL
  Forms, Controls, Graphics, ComCtrls, StdCtrls, ExtCtrls, Menus, ActnList,
  Clipbrd, LclType, LCLIntf,
  // LazUtils
  LazLoggerBase,
  // Codetools
  CodeToolManager, CodeCache,
  // LazEdit
  LazEditTextAttributes,
  // SynEdit
  SynEdit, SynEditHighlighter, SynEditMiscClasses,
  // IdeIntf
  IDEWindowIntf, IDECommands, IDEImagesIntf, SrcEditorIntf, IDEOptEditorIntf,
  IdeIntfStrConsts, EditorSyntaxHighlighterDef,
  // DebuggerIntf
  DbgIntfDebuggerBase,
  // LazDebuggerIntf
  LazDebuggerIntfBaseTypes,
  // IDEDebugger
  DebuggerDlg, Debugger, BaseDebugManager, IdeDebuggerStringConstants;

type

  { TAssemblerDlg }

  TAsmDlgLineMapState = (
    lmsUnknown,
    lmsInvalid,    // debugger couldn't disassemble this address
    lmsStatement,  // display line as assembler
    lmsSource,     // display line as source
    lmsFuncName    // Name of function
  );

  TAsmDlgLineEntry = record
    State: TAsmDlgLineMapState;
    Addr: TDbgPtr;
    Offset: Integer;
    Dump: String;
    Statement: String;
    PasCode: String;
    FileName, FullFileName: String;
    SourceLine: Integer;
    ImageIndex: Integer;
    TargetAddr: TDbgPtr;             // Absolute Addr for relative jump/call
    IsJump: Boolean;
  end;
  TAsmDlgLineEntries = Array of TAsmDlgLineEntry;

  { THistoryEntry }

  THistoryEntry = record
    Addr, DispAddr: TDBGPtr;
    class operator = (a,b: THistoryEntry): boolean;
  end;
  TDBGPtrList = specialize TFPGList<THistoryEntry>;

  const
    MAX_ASM_HIST = 32;

  type

  TAssemblerDlg = class(TDebuggerDlg)
    actCurrentInstr: TAction;
    actGotoAddr: TAction;
    actCopy: TAction;
    actStepOverInstr: TAction;
    actStepIntoInstr: TAction;
    ActionList1: TActionList;
    CopyToClipboard: TMenuItem;
    EditGotoAddr: TEdit;
    ImageList1: TImageList;
    popCopyAddr: TMenuItem;
    pnlToolAddr: TPanel;
    pbAsm: TPaintBox;
    PopupMenu1: TPopupMenu;
    sbHorizontal: TScrollBar;
    sbVertical: TScrollBar;
    Timer1: TTimer;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    tbJumpBack: TToolButton;
    tbJumpForward: TToolButton;
    ToolButtonCopy: TToolButton;
    ToolButtonGoto: TToolButton;
    ToolButtonGotoCurrent: TToolButton;
    ToolButtonStepOverInstr: TToolButton;
    ToolButtonStepIntoInstr: TToolButton;
    ToolButton4: TToolButton;
    ToolButtonPower: TToolButton;
    ToolButton2: TToolButton;
    procedure actCurrentInstrExecute(Sender: TObject);
    procedure actGotoAddrExecute(Sender: TObject);
    procedure actStepIntoInstrExecute(Sender: TObject);
    procedure actStepOverInstrExecute(Sender: TObject);
    procedure CopyToClipboardClick(Sender: TObject);
    procedure EditGotoAddrChange(Sender: TObject);
    procedure EditGotoAddrKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure pbAsmClick(Sender: TObject);
    procedure pbAsmMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; {%H-}X, Y: Integer);
    procedure pbAsmMouseMove(Sender: TObject; {%H-}Shift: TShiftState; {%H-}X, Y: Integer);
    procedure pbAsmMouseUp(Sender: TObject; {%H-}Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X,
      {%H-}Y: Integer);
    procedure pbAsmMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; {%H-}MousePos: TPoint; var Handled: Boolean);
    procedure GetColors(ASrc: TSynHighlighterAttributes; AMod: array of TSynHighlighterAttributesModifier; out AText, ABack, AFrame: TColor; out AStyle: TFontStyles);
    procedure pbAsmPaint(Sender: TObject);
    procedure popCopyAddrClick(Sender: TObject);
    procedure sbHorizontalChange(Sender: TObject);
    procedure sbVerticalChange(Sender: TObject);
    procedure sbVerticalScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure tbJumpBackClick(Sender: TObject);
    procedure tbJumpForwardClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ToolButtonPowerClick(Sender: TObject);
  private
    FWheelAccu: Integer;
    FDebugger: TDebuggerIntf;
    FDebugManager: TBaseDebugManager;
    FDisassembler: TIDEDisassembler;
    FDisassemblerNotification: TIDEDisassemblerNotification;
    FCurrentLocation: TDBGPtr; // current view location (lines are relative to this location)
    FLocation: TDBGPtr;  // the actual PC, green "=>" execution mark
    FMouseIsDown: Boolean;
    FMouseDownX, FMouseDownY: Integer;
    FLinkLine, FTargetLine: integer;
    FIsVScrollTrack: Boolean;
    FVScrollCounter, FVScrollPos: Integer;
    FHistory: TDBGPtrList;
    FHistoryIdx: Integer;
    FHistoryLock: boolean;
    FInternalUpdateLock: integer;

    FTopLine: Integer;
    FLastTopLine: Integer;
    FLastTopLineIdx: Integer;
    FLastTopLineIsSrc: Boolean; // The Source In Fron of Idx
    FLastTopLineValid: Boolean;

    FSelectLine: Integer;
    FSelectionEndLine: Integer;
    FLineCount: Integer;
    FLineMap: TAsmDlgLineEntries;

    FLineHeight: Integer;
    FCharWidth: Integer;
    FGutterWidth: Integer;
    FUpdating: Boolean;
    FUpdateNeeded, FVisibleChanged: Boolean;

    FPowerImgIdx, FPowerImgIdxGrey: Integer;
    FCurLineImgIdx: Integer;
    FImgSourceLine: Integer;
    FImgNoSourceLine: Integer;
    FImageTarget: Integer;

    FHighLigther: TSynCustomHighlighter;
    FDefAttrib, FSrcCodeAttrib, FSrcFuncAttrib: TSynHighlighterAttributes;
    FSelAttrib, FCurLineAttrib, FJmpLinkAttrib, FJmpTargetAttrib: TSynHighlighterAttributesModifier;
    FMergeCol: TSynSelectedColorMergeResult;

    function LineForAddr(AnAddr: TDBGPtr):Integer;
    procedure BreakPointChanged(const {%H-}ASender: TIDEBreakPoints;
      const {%H-}ABreakpoint: TIDEBreakPoint);
    function  GetBreakpointFor(AnAsmDlgLineEntry: TAsmDlgLineEntry): TIDEBreakPoint;
    procedure CheckImageIndexFor(var AnAsmDlgLineEntry: TAsmDlgLineEntry);
    procedure DoDebuggerDestroyed(Sender: TObject);
    procedure ClearLineMap(AState: TAsmDlgLineMapState = lmsUnknown);
    procedure ClearImageIdx;
    procedure DisassemblerChanged(Sender: TObject);
    procedure SetDisassembler(const AValue: TIDEDisassembler);
    procedure SetDebugger(const AValue: TDebuggerIntf);
    function FormatLine(ALine: TAsmDlgLineEntry; W: Integer): String;
    procedure UpdateView;
    procedure UpdateActionEnabled;
    procedure UpdateLineData;
    procedure UpdateLineDataEx(ALineMap: TAsmDlgLineEntries;
                               AFirstLine, ALineCount: Integer;
                               var ACachedLine, ACachedIdx: Integer;
                               var ACachedIsSrc, ACachedValid: Boolean;
                               ACachedUpdate: Boolean;
                               ANoExtraHeader: Boolean = False
                              );
    procedure SetSelection(ALine: Integer; AMakeVisible: Boolean; AKeepSelEnd: Boolean = False);
    procedure SetLineCount(ALineCount: Integer);
    procedure SetTopLine(ALine: Integer);
    function IndexOfAddr(const AnAddr: TDBGPtr): Integer;
    procedure UpdateLocation(const AAddr: TDBGPtr);
    procedure DoEditorOptsChanged(Sender: TObject);
  protected
    function GetSourceCodeLine(SrcFileName: string; SrcLineNumber: Integer): string;
    procedure DoBeginUpdate; override;
    procedure DoEndUpdate; override;
    procedure UpdateShowing; override;
    function GetLinMapEntryForLine(ALine: Integer; out AnEntry: TAsmDlgLineEntry): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetLocation(ADebugger: TDebuggerIntf; const AAddr: TDBGPtr; const ADispAddr: TDBGPtr = 0);
    property Disassembler: TIDEDisassembler read FDisassembler write SetDisassembler;
    property DebugManager: TBaseDebugManager read FDebugManager write FDebugManager;
    property BreakPoints;
  end;


var
  IdeAsmWinHlId: integer;

implementation

{$R *.lfm}

var
  AsmWindowCreator: TIDEWindowCreator;

{ THistoryEntry }

class operator THistoryEntry. = (a, b: THistoryEntry): boolean;
begin
  Result := (a.Addr = b.Addr) and (a.DispAddr = b.DispAddr);
end;

{ TAssemblerDlg }

procedure TAssemblerDlg.ClearLineMap(AState: TAsmDlgLineMapState = lmsUnknown);
var
  n: Integer;
begin
  FLastTopLineValid := False;
  for n := Low(FLineMap) to High(FLineMap) do
  begin
    FLineMap[n].State := AState;
    FLineMap[n].Dump := '';
    FLineMap[n].Statement := '';
    FLineMap[n].ImageIndex := -1;
    FLineMap[n].Offset := 0;
    if AState = lmsUnknown
    then FLineMap[n].Addr := 0;
  end;
end;

procedure TAssemblerDlg.ClearImageIdx;
var
  n: Integer;
begin
  FLastTopLineValid := False;
  for n := Low(FLineMap) to High(FLineMap) do
  begin
    FLineMap[n].ImageIndex := -1;
  end;
end;

procedure TAssemblerDlg.SetDisassembler(const AValue: TIDEDisassembler);
begin
  if FDisassembler = AValue then exit;
  BeginUpdate;
  try
    if FDisassembler <> nil
    then begin
      FDisassembler.RemoveNotification(FDisassemblerNotification);
    end;

    FDisassembler := AValue;

    if FDisassembler <> nil
    then begin
      FDisassembler.AddNotification(FDisassemblerNotification);
    end;

    DisassemblerChanged(FDisassembler);
  finally
    EndUpdate;
  end;
  UpdateActionEnabled;
end;

procedure TAssemblerDlg.SetDebugger(const AValue: TDebuggerIntf);
begin
  if FDebugger = AValue
  then exit;

  if FDebugger <> nil
  then FDebugger.RemoveNotifyEvent(dnrDestroy, @DoDebuggerDestroyed);
  FDebugger := AValue;
  if FDebugger <> nil
  then FDebugger.AddNotifyEvent(dnrDestroy, @DoDebuggerDestroyed);
  UpdateActionEnabled;
end;

constructor TAssemblerDlg.Create(AOwner: TComponent);
begin
  FCurrentLocation := 0;
  FLocation := 0;
  FLineCount := 0;
  FLineHeight := 10;
  SetLength(FLineMap, FLineCount + 1);
  FGutterWidth := 32;
  FDisassemblerNotification := TIDEDisassemblerNotification.Create;
  FDisassemblerNotification.AddReference;
  FDisassemblerNotification.OnChange  := @DisassemblerChanged;
  BreakpointsNotification.OnAdd    := @BreakPointChanged;
  BreakpointsNotification.OnUpdate := @BreakPointChanged;
  BreakpointsNotification.OnRemove  := @BreakPointChanged;
  FIsVScrollTrack := False;
  FVScrollCounter := 0;
  FHistory := TDBGPtrList.Create;
  FLinkLine := -1;

  FDefAttrib       := TSynHighlighterAttributes.Create;
  FSrcCodeAttrib   := TSynHighlighterAttributes.Create;
  FSrcFuncAttrib   := TSynHighlighterAttributes.Create;
  FSelAttrib       := TSynHighlighterAttributesModifier.Create;
  FCurLineAttrib   := TSynHighlighterAttributesModifier.Create;
  FJmpLinkAttrib   := TSynHighlighterAttributesModifier.Create;
  FJmpTargetAttrib := TSynHighlighterAttributesModifier.Create;
  FMergeCol:= TSynSelectedColorMergeResult.Create;

  inherited Create(AOwner);
//  DoubleBuffered := True;

  Caption := lisDisAssAssembler;

  SourceEditorManagerIntf.RegisterChangeEvent(semEditorOptsChanged, @DoEditorOptsChanged);
  Caption := lisMenuViewAssembler;
  CopyToClipboard.Caption := lisDbgAsmCopyToClipboard;
  popCopyAddr.Caption := lisDbgAsmCopyAddressToClipboard;


  ToolBar1.Images := IDEImages.Images_16;
  PopupMenu1.Images := IDEImages.Images_16;

  actStepOverInstr.Caption := lisMenuStepOverInstr;
  actStepOverInstr.Hint := lisMenuStepOverInstrHint;
  actStepOverInstr.ImageIndex := IDEImages.LoadImage('menu_stepover_instr');

  actStepIntoInstr.Caption := lisMenuStepIntoInstr;
  actStepIntoInstr.Hint := lisMenuStepIntoInstrHint;
  actStepIntoInstr.ImageIndex := IDEImages.LoadImage('menu_stepinto_instr');

  actCurrentInstr.Caption := lisDisAssGotoCurrentAddress;
  actCurrentInstr.Hint := lisDisAssGotoCurrentAddressHint;
  actCurrentInstr.ImageIndex := IDEImages.LoadImage('debugger_current_line');

  actGotoAddr.Caption := lisDisAssGotoAddress;
  actGotoAddr.Hint := lisDisAssGotoAddressHint;
  actGotoAddr.ImageIndex := IDEImages.LoadImage('callstack_show');
  EditGotoAddr.TextHint := lisDisAssGotoAddrEditTextHint;

  actCopy.Caption := lisCopy;
  actCopy.Hint := lisCopy;
  actCopy.ImageIndex := IDEImages.LoadImage('laz_copy');


  FPowerImgIdx := IDEImages.LoadImage('debugger_power');
  FPowerImgIdxGrey := IDEImages.LoadImage('debugger_power_grey');
  ToolButtonPower.ImageIndex := FPowerImgIdx;

  tbJumpBack.Enabled := False;
  tbJumpBack.Caption := '';
  tbJumpBack.ImageIndex := IDEImages.LoadImage('menu_search_jumpback');

  tbJumpForward.Enabled := False;
  tbJumpForward.Caption := '';
  tbJumpForward.ImageIndex := IDEImages.LoadImage('menu_search_jumpforward');

  FCurLineImgIdx := IDEImages.LoadImage('debugger_current_line');
  //

  FImgSourceLine := IDEImages.LoadImage('debugger_source_line');
  FImgNoSourceLine := IDEImages.LoadImage('debugger_nosource_line');
  FImageTarget := IDEImages.LoadImage('menu_run');
end;

destructor TAssemblerDlg.Destroy;
begin
  if SourceEditorManagerIntf <> nil then
    SourceEditorManagerIntf.UnRegisterChangeEvent(semEditorOptsChanged, @DoEditorOptsChanged);
  SetDisassembler(nil);
  SetDebugger(nil);
  FDisassemblerNotification.OnChange := nil;
  FDisassemblerNotification.ReleaseReference;
  FHistory.Free;
  FreeAndNil(FMergeCol);
  FreeAndNil(FDefAttrib);
  FreeAndNil(FSrcCodeAttrib);
  FreeAndNil(FSrcFuncAttrib);
  FreeAndNil(FSelAttrib);
  FreeAndNil(FCurLineAttrib);
  FreeAndNil(FJmpLinkAttrib);
  FreeAndNil(FJmpTargetAttrib);

  inherited Destroy;
end;

procedure TAssemblerDlg.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  i: LongInt;
begin
  if (Shift - [ssShift] <> []) then begin
    inherited;
    Exit;
  end;
  pbAsm.Invalidate;
  case Key of
    VK_UP:   begin
      ToolButtonPower.Down := True;
      ToolButtonPowerClick(nil);
      SetSelection(FSelectLine - 1, True, ssShift in Shift);
      Key := 0;
    end;
    VK_DOWN: begin
      ToolButtonPower.Down := True;
      ToolButtonPowerClick(nil);
      SetSelection(FSelectLine + 1, True, ssShift in Shift);
      Key := 0;
    end;
    VK_PRIOR: begin
      ToolButtonPower.Down := True;
      ToolButtonPowerClick(nil);
      i := FTopLine;
      SetSelection(FSelectLine - FLineCount, False, ssShift in Shift);
      SetTopline(i - FLineCount);
      Key := 0;
    end;
    VK_NEXT: begin
      ToolButtonPower.Down := True;
      ToolButtonPowerClick(nil);
      i := FTopLine;
      SetSelection(FSelectLine + FLineCount, False, ssShift in Shift);
      SetTopline(i + FLineCount);
      Key := 0;
    end;
    VK_LEFT: begin
      if not EditGotoAddr.Focused then begin
        sbHorizontal.Position := sbHorizontal.Position - sbHorizontal.SmallChange;
        Key := 0;
      end;
    end;
    VK_RIGHT: begin
      if not EditGotoAddr.Focused then begin
        sbHorizontal.Position := sbHorizontal.Position + sbHorizontal.SmallChange;
        Key := 0;
      end;
    end;
    VK_HOME: begin
      if not EditGotoAddr.Focused then begin
        sbHorizontal.Position := 0;
        Key := 0;
      end;
    end;
    else
      inherited;
  end;
end;

procedure TAssemblerDlg.CopyToClipboardClick(Sender: TObject);
var
  ALineMap: TAsmDlgLineEntries;
  i, w: Integer;
  s: String;
begin
  SetLength(ALineMap{%H-}, abs(FSelectionEndLine - FSelectLine)+1);
  UpdateLineDataEx(ALineMap, Min(FSelectionEndLine, FSelectLine),
    abs(FSelectionEndLine - FSelectLine)+1,
    FLastTopLine, FLastTopLineIdx, FLastTopLineIsSrc, FLastTopLineValid, False, True);
  if FDebugger = nil
  then W := 16
  else W := FDebugger.TargetWidth div 4;
  s := '';
  for i := 0 to length(ALineMap)-1 do
  begin
    s := s + FormatLine(ALineMap[i], W) + LineEnding;
  end;
  Clipboard.AsText := s;
end;

procedure TAssemblerDlg.EditGotoAddrChange(Sender: TObject);
var
  HasDisassembler: Boolean;
begin
  HasDisassembler := (FDebugger <> nil) and (FDisassembler <> nil);
  actGotoAddr.Enabled := HasDisassembler and (StrToQWordDef(EditGotoAddr.Text, 0) <> 0);
end;

procedure TAssemblerDlg.EditGotoAddrKeyPress(Sender: TObject; var Key: char);
begin
  if (key = #13) and (StrToQWordDef(EditGotoAddr.Text, 0) <> 0)
  then actGotoAddr.Execute;
end;

procedure TAssemblerDlg.actStepOverInstrExecute(Sender: TObject);
var
  Handled: Boolean;
begin
  Handled:=false;
  if Assigned(OnProcessCommand)
  then OnProcessCommand(Self, ecStepOverInstr, Handled);
end;

procedure TAssemblerDlg.BreakPointChanged(const ASender: TIDEBreakPoints;
  const ABreakpoint: TIDEBreakPoint);
begin
  ClearImageIdx;
  pbAsm.Invalidate;
end;

function TAssemblerDlg.GetBreakpointFor(AnAsmDlgLineEntry: TAsmDlgLineEntry): TIDEBreakPoint;
begin
  Result := nil;
  if BreakPoints = nil then exit;
  case AnAsmDlgLineEntry.State of
    lmsStatement: Result := BreakPoints.Find(AnAsmDlgLineEntry.Addr);
    lmsSource:    Result := BreakPoints.Find(AnAsmDlgLineEntry.FullFileName, AnAsmDlgLineEntry.SourceLine);
  end;
end;

procedure TAssemblerDlg.CheckImageIndexFor(var AnAsmDlgLineEntry: TAsmDlgLineEntry);
begin
  if BreakPoints = nil then exit;
  if AnAsmDlgLineEntry.ImageIndex > 0 then exit;
  if not (AnAsmDlgLineEntry.State  in [lmsStatement, lmsSource]) then exit;

  AnAsmDlgLineEntry.ImageIndex := GetBreakPointImageIndex(GetBreakpointFor(AnAsmDlgLineEntry),
                                  (AnAsmDlgLineEntry.State = lmsStatement) and
                                  (AnAsmDlgLineEntry.Addr = FLocation));
  if AnAsmDlgLineEntry.ImageIndex >= 0
  then exit;

  if AnAsmDlgLineEntry.State = lmsStatement
  then AnAsmDlgLineEntry.ImageIndex := FImgNoSourceLine
  else AnAsmDlgLineEntry.ImageIndex := FImgSourceLine;
end;

procedure TAssemblerDlg.actStepIntoInstrExecute(Sender: TObject);
var
  Handled: Boolean;
begin
  Handled:=false;
  if Assigned(OnProcessCommand)
  then OnProcessCommand(Self, ecStepIntoInstr, Handled);
end;

procedure TAssemblerDlg.actCurrentInstrExecute(Sender: TObject);
begin
  if FDisassembler.BaseAddr <> FLocation
  then begin
    ToolButtonPower.Down := True;
    ToolButtonPowerClick(nil);
  end;
  UpdateLocation(FLocation);
end;

procedure TAssemblerDlg.actGotoAddrExecute(Sender: TObject);
var
  Addr: TDBGPtr;
begin
  ToolButtonPower.Down := True;
  ToolButtonPowerClick(nil);
  Addr := StrToQWordDef(EditGotoAddr.Text, 0);
  if Addr <> 0
  then UpdateLocation(Addr);
end;

procedure TAssemblerDlg.DisassemblerChanged(Sender: TObject);
begin
  if (FDisassembler = nil) or (FCurrentLocation = 0) or (FLineCount = 0)
  then exit;
  if (FDebugger <> nil) and (FDebugger.State <> dsPause)
  then begin
    // only for F9, not for F8,F7 single stepping with assembler is no good, if it clears all the time
    //ClearLineMap;
    FCurrentLocation := 0;
    FLocation := 0;
  end
  else begin
    UpdateView;
  end;
  pbAsm.Invalidate;
end;

procedure TAssemblerDlg.FormResize(Sender: TObject);
begin
  sbHorizontal.PageSize    := pbAsm.Width;
  sbHorizontal.LargeChange := pbAsm.Width div 3;

  if FLineHeight <> 0
  then SetLineCount(pbAsm.Height div FLineHeight);
end;

procedure TAssemblerDlg.pbAsmClick(Sender: TObject);
var
  P: TPoint;
  Line: Integer;
  b: TIDEBreakPoint;
  Ctrl: Boolean;
begin
  P := pbAsm.ScreenToClient(Mouse.CursorPos);
  debugln(['TAssemblerDlg.pbAsmClick ',dbgs(p)]);
  if P.x > FGutterWidth then exit;
  Line := P.Y div FLineHeight;
  if FMouseIsDown and (FMouseDownY <> Line) then
    exit;

  if not (FLineMap[Line].State in [lmsStatement, lmsSource])
  then exit;

  b := GetBreakpointFor(FLineMap[Line]);
  Ctrl := ssCtrl in GetKeyShiftState;

  if b = nil then begin
    DebugBoss.LockCommandProcessing;
    try
      if (FLineMap[Line].State = lmsStatement)
      then DebugBoss.DoCreateBreakPoint(FLineMap[Line].Addr, True, b)
      else DebugBoss.DoCreateBreakPoint(FLineMap[Line].FullFileName, FLineMap[Line].SourceLine, True, b);
      if Ctrl and (b <> nil)
      then b.Enabled := False;
    finally
      DebugBoss.UnLockCommandProcessing;
    end;
  end else begin
    if Ctrl
    then b.Enabled := not b.Enabled
    else b.ReleaseReference;
  end;
end;

procedure TAssemblerDlg.DoBeginUpdate;
begin
  FVisibleChanged := False;
  inherited DoBeginUpdate;
end;

procedure TAssemblerDlg.DoEndUpdate;
begin
  if FInternalUpdateLock > 0 then
    exit;
  inc(FInternalUpdateLock);
  try
  inherited DoEndUpdate;
  if FVisibleChanged then begin
    DoEditorOptsChanged(nil);
    if FCurrentLocation <> 0 then
      UpdateLocation(FCurrentLocation);
  end;
  FVisibleChanged := False;

  finally
    dec(FInternalUpdateLock);
    UpdateView;
  end;
end;

procedure TAssemblerDlg.UpdateShowing;
begin
  inherited UpdateShowing;
  if IsVisible then begin
    if IsUpdating then begin
      FVisibleChanged := True
    end else begin
      DoEditorOptsChanged(nil);
      if FCurrentLocation <> 0 then
        UpdateLocation(FCurrentLocation);
    end;
  end;
end;

function TAssemblerDlg.GetLinMapEntryForLine(ALine: Integer; out
  AnEntry: TAsmDlgLineEntry): Boolean;
begin
  AnEntry := Default(TAsmDlgLineEntry);
  Result := ALine >= FTopLine;
  if not Result then
    exit;
  {$PUSH}{$Q-}{$R-} // if FTopLine is close to Low(integer) it can overflow
  ALine := ALine - FTopLine;
  {$POP}
  Result := (ALine > 0) and (ALine < length(FLineMap));
  if Result then
    AnEntry := FLineMap[ALine];
end;

procedure TAssemblerDlg.pbAsmMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  y := y div FLineHeight;
  case Button of
    mbLeft: begin
      FMouseIsDown := True;
      FMouseDownX := X;
      FMouseDownY := Y;
      if FLinkLine >= 0 then
        Invalidate;
      FTargetLine := -1;
      FLinkLine := -1;
      pbAsm.Cursor := crDefault;
      if X <= FGutterWidth then exit;
      if not(ssCtrl in Shift) then begin
        SetSelection(FTopLine + Y, False, ssShift in Shift);
      end
      else
      if (y >= 0) and (y <= FLineCount) and
         FLineMap[y].IsJump and
         (FLineMap[y].TargetAddr <> 0) and (FDebugger <> nil)
      then begin
        FLinkLine := y;
        FTargetLine := LineForAddr(FLineMap[y].TargetAddr);
        Invalidate;
        pbAsm.Cursor := crHandPoint;
      end;
    end;
    mbRight: ;
    mbMiddle: ;
    mbExtra1: tbJumpBackClick(nil);
    mbExtra2: tbJumpForwardClick(nil);
  end;
end;

procedure TAssemblerDlg.pbAsmMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  y := Y div FLineHeight;
  case FMouseIsDown of
    True: begin
      if (FLinkLine < 0) and (y >= 0) and (y < FLineCount) and (FMouseDownX > FGutterWidth)
      then SetSelection(FTopLine + Y, False, True);
    end;
    False: begin
      if (ssCtrl in Shift) and
         (Y >= 0) and (y <= FLineCount) and
         (FLineMap[y].TargetAddr <> 0) and (FDebugger <> nil) and
         FLineMap[y].IsJump and
         (X > FGutterWidth)
      then begin
        if (FLinkLine <> y) then
          Invalidate;
        FLinkLine := y;
        FTargetLine := LineForAddr(FLineMap[y].TargetAddr);
      end
      else begin
        if FLinkLine >= 0 then
          Invalidate;
        FLinkLine := -1;
        FTargetLine := -1;
      end;
    end;
  end;

  if y = FLinkLine then
    pbAsm.Cursor := crHandPoint
  else
    pbAsm.Cursor := crDefault;
end;

procedure TAssemblerDlg.pbAsmMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FMouseIsDown := False;

  if (ssCtrl in Shift) and (FLinkLine = y div FLineHeight) and
     (FLinkLine >= 0) and (FLinkLine <= FLineCount) and
     (FLineMap[FLinkLine].TargetAddr <> 0) and (FDebugger <> nil) and
     FLineMap[FLinkLine].IsJump and
     (X > FGutterWidth) and (FMouseDownX > FGutterWidth)
  then begin
    SetLocation(FDebugger, FLineMap[FLinkLine].TargetAddr);
  end;
  if FLinkLine >= 0 then
    Invalidate;
  FLinkLine := -1;
  pbAsm.Cursor := crDefault;
end;

procedure TAssemblerDlg.pbAsmMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  j: LongInt;
begin
  if not ToolButtonPower.Down then exit;
  Handled := True;
  FLinkLine := -1;
  pbAsm.Cursor := crDefault;

  FWheelAccu := FWheelAccu + WheelDelta;
  j := FWheelAccu div 120;
  if j = 0 then
    exit;

  FWheelAccu := FWheelAccu - j * 120;
  SetTopLine(FTopLine - j);
  pbAsm.Invalidate;
end;

procedure TAssemblerDlg.GetColors(ASrc: TSynHighlighterAttributes;
  AMod: array of TSynHighlighterAttributesModifier; out AText, ABack,
  AFrame: TColor; out AStyle: TFontStyles);
var
  i: Integer;
begin
  FMergeCol.CleanupMergeInfo;
  FMergeCol.Assign(ASrc);
  for i := low(AMod) to high(AMod) do
    if AMod[i] <> nil then FMergeCol.Merge(AMod[i]);

  FMergeCol.ProcessMergeInfo;

  ABack  := FMergeCol.Background;
  if (ABack = clNone) or (ABack = clDefault) then ABack := FDefAttrib.Background;
  if (ABack = clNone) or (ABack = clDefault) then ABack := clWhite;
  AText  := FMergeCol.Foreground;
  if (AText = clNone) or (AText = clDefault) then AText := FDefAttrib.FrameColor;
  if (AText = clNone) or (AText = clDefault) then AText := clBlack;
  AFrame := FMergeCol.FrameSideColors[bsBottom];
  AStyle := FMergeCol.Style;
end;

procedure TAssemblerDlg.pbAsmPaint(Sender: TObject);
var
  R: TRect;
  n, X, Y, Line, W: Integer;
  S: String;
  TextStyle: TTextStyle;
  Fore, Back, Frame: TColor;
  St: TFontStyles;
  m0, m1, m2, m3: TSynHighlighterAttributesModifier;
begin
  R := pbAsm.ClientRect;
  TextStyle := pbAsm.Canvas.TextStyle;
  TextStyle.Wordbreak := False;
  TextStyle.SingleLine := True;
  pbAsm.Canvas.TextStyle := TextStyle;

  GetColors(FDefAttrib, [], Fore, Back, Frame, St);
  pbAsm.Canvas.Brush.Color := Back;
  pbAsm.Canvas.FillRect(R);
  Inc(R.Left, FGutterWidth);

  X := FGutterWidth - sbHorizontal.Position;
  Y := 0;
  Line := FTopLine;

  if FDebugger = nil
  then W := 16
  else W := FDebugger.TargetWidth div 4;

  for n := 0 to FLineCount do
  begin
    m0 := nil;
    m1 := nil;
    m2 := nil;
    m3 := nil;
    // Current-line is only used, if multiple lines are selected
    if (Line = FSelectLine) or
       ( (FSelectionEndLine <> FSelectLine) and
         (line >= Min(FSelectLine, FSelectionEndLine)) and
         (line <= Max(FSelectLine, FSelectionEndLine))
       )
    then
      m0 := FSelAttrib;
    if (Line = FSelectLine) and (FSelectionEndLine <> FSelectLine) then
      m1 := FCurLineAttrib;
    if FLinkLine = n then
      m2 := FJmpLinkAttrib;
    if (FLinkLine >=0) and (n = FTargetLine) then
      m3 := FJmpTargetAttrib;

    if (FLineMap[n].State in [lmsSource]) then
      GetColors(FSrcCodeAttrib, [m0, m1, m2, m3], Fore, Back, Frame, St)
    else
    if (FLineMap[n].State in [lmsFuncName]) then
      GetColors(FSrcFuncAttrib, [m0, m1, m2, m3], Fore, Back, Frame, St)
    else
      GetColors(FDefAttrib, [m0, m1, m2, m3], Fore, Back, Frame, St);

    pbAsm.Canvas.Font.Color :=  Fore;
    pbAsm.Canvas.Font.Style :=  st;

    pbAsm.Canvas.Brush.Color := Back;
    pbAsm.Canvas.FillRect(R.Left, n * FLineHeight, R.Right, (n + 1) * FLineHeight);

    if (Frame <> clNone) and (Frame <> clDefault)
    then begin
      pbAsm.Canvas.Brush.Color := Frame;
      pbAsm.Canvas.Pen.Color := Frame;
      pbAsm.Canvas.Brush.Style := bsClear;
      pbAsm.Canvas.Rectangle(R.Left, n * FLineHeight, R.Right, (n + 1) * FLineHeight);
      pbAsm.Canvas.Brush.Style := bsSolid;
      pbAsm.Canvas.Brush.Color := Back;
      pbAsm.Canvas.Pen.Color := Back;
    end;

    if (FLinkLine >=0) and (n = FTargetLine)
    then begin
      if (FImageTarget >= 0)
      then IDEImages.Images_16.Draw(pbAsm.Canvas, FGutterWidth - 16, Y, FImageTarget, True);
    end
    else begin
      CheckImageIndexFor(FLineMap[n]);
      if (FLineMap[n].ImageIndex >= 0)
      then IDEImages.Images_16.Draw(pbAsm.Canvas, FGutterWidth - 16, Y, FLineMap[n].ImageIndex, True);
    end;

    S := FormatLine(FLineMap[n], W);
    pbAsm.Canvas.TextRect(R, X, Y, S);

    Inc(Y, FLineHeight);
    Inc(Line);
  end;
end;

procedure TAssemblerDlg.popCopyAddrClick(Sender: TObject);
var
  Entry: TAsmDlgLineEntry;
  W: Integer;
begin
  if FDebugger = nil
  then W := 16
  else W := FDebugger.TargetWidth div 4;

  if GetLinMapEntryForLine(FSelectLine, Entry) then
    Clipboard.AsText := '$'+IntToHex(Entry.Addr, W);
end;

function TAssemblerDlg.FormatLine(ALine: TAsmDlgLineEntry; W: Integer) : String;
begin
  Result := '';
  //Result :=  Format('[a:%8.8u l:%8.8d i:%3.3u] ', [Cardinal(ALine.Addr), Line, n]);
  Result := Result + HexStr(ALine.Addr, W) + ' ';

  case ALine.State of
    lmsUnknown: Result := Result + '??????';
    lmsInvalid: Result := Result + '......';
    lmsStatement: Result := Result + Copy(ALine.Dump + '                         ', 1, 24) + ' ' + ALine.Statement;
    lmsSource: begin
      if ALine.SourceLine = 0
      then Result := '---'
      else Result :=  Format('%-'+IntToStr(W+25)+'s %s',
                             [Format('%s:%u %s', [ALine.FileName, ALine.SourceLine, ALine.Statement]),
                              ALine.PasCode]);
    end;
    lmsFuncName: if ALine.SourceLine > 0 then
      Result:= Format('%s+%u %s', [ALine.FileName, ALine.SourceLine, ALine.Statement])
    else
      Result:= ALine.FileName + ' ' + ALine.Statement;
  end;
end;

procedure TAssemblerDlg.UpdateView;
begin
  if (not ToolButtonPower.Down) or (FInternalUpdateLock > 0) then
    exit;

    if (FDisassembler <> nil) and (FCurrentLocation <> 0)
    then begin
      inc(FInternalUpdateLock);
      try
        FDisassembler.PrepareRange(FCurrentLocation, Max(0, -(FTopLine - 5)), Max(0, FTopLine + FLineCount + 1 + 5));
      finally
        dec(FInternalUpdateLock);
      end;
      UpdateLineData;
    end
    else ClearLineMap;
    pbAsm.Invalidate;
end;

procedure TAssemblerDlg.UpdateActionEnabled;
var
  HasDisassembler: Boolean;
  dummy: TAsmDlgLineEntry;
begin
  HasDisassembler := (FDebugger <> nil) and (FDisassembler <> nil);
  actCurrentInstr.Enabled := HasDisassembler and (FLocation <> 0);
  actGotoAddr.Enabled := HasDisassembler and (StrToQWordDef(EditGotoAddr.Text, 0) <> 0);
  actCopy.Enabled := HasDisassembler;
  popCopyAddr.Enabled := HasDisassembler and GetLinMapEntryForLine(FSelectLine, dummy);
  actStepOverInstr.Enabled := HasDisassembler;
  actStepIntoInstr.Enabled := HasDisassembler;
end;

procedure TAssemblerDlg.sbHorizontalChange(Sender: TObject);
begin
  pbAsm.Invalidate;
end;

procedure TAssemblerDlg.sbVerticalChange(Sender: TObject);
begin
  ToolButtonPower.Down := True;
  ToolButtonPowerClick(nil);
  pbAsm.Invalidate;
  Timer1.Enabled := True;
end;

procedure TAssemblerDlg.sbVerticalScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  FIsVScrollTrack := False;
  case ScrollCode of
    scLineUp: begin
      SetTopline(FTopLine - 1);
    end;
    scLineDown: begin
      SetTopline(FTopLine + 1);
    end;
    scPageUp: begin
      SetTopline(FTopLine - FLineCount);
    end;
    scPageDown: begin
      SetTopline(FTopLine + FLineCount);
    end;
    scPosition: begin
      // doesn't work on gtk
    end;
    scTrack: begin
      FVScrollPos := ScrollPos;
      FIsVScrollTrack := True;
    end;
//    scTop,      // = SB_TOP
//    scBottom,   // = SB_BOTTOM
//    scEndScroll // = SB_ENDSCROLL
  end;
  Timer1.Enabled := True;
end;

procedure TAssemblerDlg.tbJumpBackClick(Sender: TObject);
begin
  if (FHistoryIdx <= 0) or (FDebugger = nil) then
    exit;
  dec(FHistoryIdx);

  FHistoryLock := True;
  if FHistoryIdx < FHistory.Count then
    SetLocation(FDebugger, FHistory[FHistoryIdx].Addr, FHistory[FHistoryIdx].DispAddr);
  FHistoryLock := False;
end;

procedure TAssemblerDlg.tbJumpForwardClick(Sender: TObject);
begin
  if (FHistoryIdx < 0) then
    FHistoryIdx := -1;
  if (FHistoryIdx >= FHistory.Count - 1) or (FDebugger = nil) then
    exit;
  inc(FHistoryIdx);

  FHistoryLock := True;
  if FHistoryIdx < FHistory.Count then
    SetLocation(FDebugger, FHistory[FHistoryIdx].Addr, FHistory[FHistoryIdx].DispAddr);
  FHistoryLock := False;
end;

procedure TAssemblerDlg.Timer1Timer(Sender: TObject);
var
  i: Integer;
begin
  if (GetCaptureControl <> sbVertical) then begin
debugln('----------------');
    sbVertical.Position := 475;
    pbAsm.Invalidate;
    FIsVScrollTrack := False;
    Timer1.Enabled := False;
    FVScrollCounter := 0;
  end else
  if FIsVScrollTrack then begin
    i := (FVScrollPos - 475);
    if i < 0 then dec(i, 35);
    if i > 0 then inc(i, 35);
    FVScrollCounter := FVScrollCounter + (i div 35);
    if (FVScrollCounter <= -10) or (FVScrollCounter >= 10) then begin
      i := FVScrollCounter div 10;
      SetTopline(FTopLine + i);
      FVScrollCounter := FVScrollCounter -(10 * i);
      pbAsm.Invalidate;
    end;
  end;
end;

procedure TAssemblerDlg.ToolButtonPowerClick(Sender: TObject);
begin
  if ToolButtonPower.Down
  then begin
    ToolButtonPower.ImageIndex := FPowerImgIdx;
    UpdateView;
  end
  else ToolButtonPower.ImageIndex := FPowerImgIdxGrey;
end;

function TAssemblerDlg.LineForAddr(AnAddr: TDBGPtr): Integer;
begin
  Result := FLineCount;
  while (Result >= 0) and (FLineMap[Result].Addr <> AnAddr) do
    dec(Result);
end;

procedure TAssemblerDlg.DoDebuggerDestroyed(Sender: TObject);
begin
  FDebugger := nil;
  UpdateView;
end;

function TAssemblerDlg.IndexOfAddr(const AnAddr: TDBGPtr): Integer;
begin
  Result := length(FLineMap) - 1;
  while Result >= 0  do begin
    if (FLineMap[Result].State = lmsStatement) and (FLineMap[Result].Addr = AnAddr)
    then exit;
    dec(Result);
  end;
end;

procedure TAssemblerDlg.UpdateLocation(const AAddr: TDBGPtr);
var
  i: Integer;
begin
  if FCurrentLocation <> AAddr
  then begin
    FCurrentLocation := AAddr;
    FLastTopLineValid := False;
  end;

  i := IndexOfAddr(FCurrentLocation);
  if (i >= 0) and (i < FLineCount - 1)
  then begin
    FSelectLine := FTopLine + i;
  end
  else begin
    FTopLine := -(FLineCount div 2);
    FSelectLine := 0;
  end;
  FSelectionEndLine := FSelectLine;
  UpdateActionEnabled;
  UpdateView;
end;

procedure TAssemblerDlg.DoEditorOptsChanged(Sender: TObject);
var
  TM: TTextMetric;
  Syn: TSynEdit;
  i: Integer;
begin
  FHighLigther := TSynCustomHighlighter(IdeSyntaxHighlighters.SharedInstances[IdeAsmWinHlId]);
  IDEEditorOptions.GetHighlighterObjSettings(FHighLigther);

  Syn := TSynEdit.Create(nil);
  IDEEditorOptions.GetSynEditorSettings(Syn);

  Font := Syn.Font;
  if HandleAllocated and GetTextMetrics(pbAsm.Canvas.Handle, TM{%H-}) then
  begin
    FCharWidth := TM.tmMaxCharWidth; // EditorOpts.ExtraCharSpacing +
    sbHorizontal.SmallChange := FCharWidth;
    FLineHeight := Max(6, Syn.ExtraLineSpacing + TM.tmHeight);
    SetLineCount(pbAsm.Height div FLineHeight);
  end;

  for i := 0 to FHighLigther.AttrCount - 1 do
    case FHighLigther.Attribute[i].StoredName of
      'ahaDefault':        FDefAttrib.Assign(FHighLigther.Attribute[i]);
      'ahaAsmSourceLine':  FSrcCodeAttrib.Assign(FHighLigther.Attribute[i]);
      'ahaAsmSourceFunc':  FSrcFuncAttrib.Assign(FHighLigther.Attribute[i]);
      'ahaTextBlock':      FSelAttrib.Assign(FHighLigther.Attribute[i]);
      'ahaLineHighlight':  FCurLineAttrib.Assign(FHighLigther.Attribute[i]);
      'ahaMouseLink':      FJmpLinkAttrib.Assign(FHighLigther.Attribute[i]);
      'ahaAsmLinkTarget':  FJmpTargetAttrib.Assign(FHighLigther.Attribute[i]);
    end;

  Invalidate;

  Syn.Free;
end;

procedure TAssemblerDlg.SetLocation(ADebugger: TDebuggerIntf; const AAddr: TDBGPtr;
  const ADispAddr: TDBGPtr);
var
  i: Integer;
  HistEntry: THistoryEntry;
begin
  SetDebugger(ADebugger);

  if (not FHistoryLock) and (ADebugger<>nil) and (AAddr<>0) then begin
    if FHistoryIdx >= 0 then
      while  FHistory.Count > FHistoryIdx + 1 do
        FHistory.Delete(FHistoryIdx + 1);

    HistEntry.Addr := AAddr;
    HistEntry.DispAddr := ADispAddr;
    FHistoryIdx := FHistory.Add(HistEntry);

    while FHistory.Count > MAX_ASM_HIST do
      FHistory.Delete(0);

  end;
  tbJumpBack.Enabled := FHistoryIdx > 0;
  tbJumpForward.Enabled := FHistoryIdx < FHistory.Count - 1;

  if ADispAddr <> 0
  then FCurrentLocation := ADispAddr
  else FCurrentLocation := AAddr;
  FLocation := AAddr;
  FLastTopLineValid := False;

  if not ToolButtonPower.Down
  then begin
    i := IndexOfAddr(FCurrentLocation);
    if (i >= 0)
    then FSelectLine := FTopLine + i
    else FSelectLine := MaxInt;
    FSelectionEndLine := FSelectLine;

    pbAsm.Invalidate;
    exit;
  end;

  FTopLine := -(FLineCount div 2);
  FSelectLine := 0;
  FSelectionEndLine := 0;

  UpdateActionEnabled;
  if Visible then // otherwhise in resize
    UpdateView
  else
    ClearLineMap;
end;

procedure TAssemblerDlg.SetSelection(ALine: Integer; AMakeVisible: Boolean;
  AKeepSelEnd: Boolean = False);
var
  OldLine: Integer;
begin
  if Aline = FSelectLine then Exit;

  // UpdateLineData may cause eventhandling, so we enter here again
  // set variable first
  OldLine := FSelectLine;
  FSelectLine := Aline;

  if not AKeepSelEnd
  then FSelectionEndLine := FSelectLine;

  if AMakeVisible
  then begin
    if FSelectLine < OldLine
    then begin
      if FTopLine > FSelectLine
      then SetTopLine(FSelectLine);
    end
    else begin
      if FTopLine + FLineCount <= FSelectLine
      then SetTopLine(FSelectLine - FLineCount + 1);
    end;
  end;

  pbAsm.Invalidate;
end;

procedure TAssemblerDlg.SetLineCount(ALineCount: Integer);
begin
  if FLineCount = ALineCount
  then exit;
  FLineCount := ALineCount;
  SetLength(FLineMap, FLineCount + 1);
  UpdateView;
end;

procedure TAssemblerDlg.SetTopLine(ALine: Integer);
var
  PadFront, PadEnd: Integer;
begin
  if not ToolButtonPower.Down
  then exit;

  if FTopLine = ALine then Exit;
  // scrolled by user, get more padding lines
  PadFront := 5;
  PadEnd := 5;
  if ALine < FTopLine
  then PadFront := 20
  else PadEnd := 20;
  FTopLine := ALine;
  if (FDisassembler <> nil)
  and ( (FDisassembler.CountBefore < Max(0, -(FTopLine - 1)))
     or (FDisassembler.CountAfter < Max(0, FTopLine + FLineCount + 2)) )
  then FDisassembler.PrepareRange(FCurrentLocation, Max(0, -(FTopLine - PadFront)), Max(0, FTopLine + FLineCount + 1 + PadEnd));
  UpdateLineData;
end;

function TAssemblerDlg.GetSourceCodeLine(SrcFileName: string; SrcLineNumber: Integer): string;
var
  PasSource: TCodeBuffer;
  Editor: TSourceEditorInterface;
begin
  Result := '';
  if SrcLineNumber < 1 then exit;
  if not FDebugManager.GetFullFilename(SrcFileName, False) // TODO: maybe ask user?
  then exit;
  PasSource := CodeToolBoss.LoadFile(SrcFileName, true, false);
  if PasSource = nil
  then exit;

  Editor := SourceEditorManagerIntf.SourceEditorIntfWithFilename(SrcFileName);
  if Editor <> nil then
    SrcLineNumber := Editor.DebugToSourceLine(SrcLineNumber);

  Result := Trim(PasSource.GetLine(SrcLineNumber - 1,false));
end;

procedure TAssemblerDlg.UpdateLineData;
begin
  UpdateLineDataEx(FLineMap, FTopLine, FLineCount + 1,
    FLastTopLine, FLastTopLineIdx, FLastTopLineIsSrc, FLastTopLineValid, True);
end;

procedure TAssemblerDlg.UpdateLineDataEx(ALineMap: TAsmDlgLineEntries; AFirstLine,
  ALineCount: Integer; var ACachedLine, ACachedIdx: Integer;
  var ACachedIsSrc, ACachedValid: Boolean; ACachedUpdate: Boolean;
  ANoExtraHeader: Boolean = False);

  function GetItem(AIdx: Integer): PDisassemblerEntry;
  begin
    Result := nil;
    if (AIdx >= -FDisassembler.CountBefore) and (AIdx < FDisassembler.CountAfter)
    then Result := FDisassembler.EntriesPtr[AIdx];
  end;

  function IsSourceBeforeItem(AItm: PDisassemblerEntry;
    APrvItm: PDisassemblerEntry): Boolean;
  begin
    if AItm = nil
    then exit(False);

    if AItm^.SrcFileName <> '' then begin
      Result := AItm^.SrcStatementIndex = 0;
      if (not Result) and  (APrvItm <> nil)
      then Result := (AItm^.SrcFileName <> APrvItm^.SrcFileName)
                  or (AItm^.SrcFileLine <> APrvItm^.SrcFileLine);
    end
    else begin
      Result :=  (AItm^.FuncName <> '');
      if Result
      then Result := (AItm^.Offset = 0)
                  or ( (APrvItm <> nil) and (AItm^.FuncName <> APrvItm^.FuncName) )
                  or (APrvItm = nil);
    end;
  end;

var
  DoneLocation: TDBGPtr;
  DoneTopLine, DoneLineCount: Integer;
  DoneCountBefore, DoneCountAfter: Integer;
  Line, Idx, W: Integer;
  Itm, NextItm, PrevItm: PDisassemblerEntry;
  LineIsSrc, HasLineOutOfRange: Boolean;
  s: String;
begin
  if FInternalUpdateLock > 0 then
    exit;
  if (FDebugger = nil) or (FDisassembler = nil) or (FDebugger.State <> dsPause)
  then begin
    ClearLineMap;  // set all to lmsUnknown;
    exit;
  end;
  if FDisassembler.BaseAddr <> FCurrentLocation
  then begin
    ClearLineMap(lmsInvalid);
    exit;
  end;

  if FUpdating
  then begin
    FUpdateNeeded := True;
    Exit;
  end;
  FUpdating := True;

  if FDebugger = nil
  then W := 16
  else W := FDebugger.TargetWidth div 4;


  try
    FUpdateNeeded := False;
    DoneLocation    := FCurrentLocation;
    DoneTopLine     := AFirstLine;
    DoneLineCount   := ALineCount;
    DoneCountBefore := FDisassembler.CountBefore;
    DoneCountAfter  := FDisassembler.CountAfter;

    // Find Idx for topline
    Line := 0;
    Idx := 0;
    LineIsSrc := False;
    if ACachedValid
    and (abs(AFirstLine - ACachedLine) < AFirstLine)
    then begin
      Line := ACachedLine;
      Idx := ACachedIdx;
      LineIsSrc := ACachedIsSrc;
    end;

    Itm := GetItem(Idx);
    NextItm := GetItem(Idx + 1);

    while AFirstLine > Line
    do begin
      NextItm := GetItem(Idx+1);
      if LineIsSrc
      then begin
        LineIsSrc := False;
      end
      else if IsSourceBeforeItem(NextItm, Itm)
      then begin
        inc(Idx);
        Itm := NextItm;
        NextItm := GetItem(Idx + 1);
        LineIsSrc := True;
      end
      else begin
        inc(Idx);
        Itm := NextItm;
        NextItm := GetItem(Idx + 1);
      end;
      inc(Line);
    end;

    Itm := GetItem(Idx);
    PrevItm := GetItem(Idx - 1);
    while AFirstLine < line
    do begin
      if LineIsSrc
      then begin
        dec(Idx);
        Itm := PrevItm;
        PrevItm := GetItem(Idx - 1);
        LineIsSrc := False;
      end
      else if IsSourceBeforeItem(Itm, PrevItm)
      then begin
        LineIsSrc := True;
      end
      else begin
        dec(Idx);
        Itm := PrevItm;
        PrevItm := GetItem(Idx - 1);
      end;
      Dec(Line);
    end;

    if ACachedUpdate
    then begin
      ACachedLine := AFirstLine;
      ACachedIdx := Idx;
      ACachedIsSrc := LineIsSrc;
      ACachedValid := True;
    end;

    // Fill LineMap
    HasLineOutOfRange := False;
    Line := 0;
    PrevItm := GetItem(Idx - 1);
    NextItm := GetItem(Idx);
    while Line < ALineCount do begin
      PrevItm := Itm;
      Itm := NextItm;
      NextItm := GetItem(Idx+1);
      ALineMap[Line].ImageIndex := -1;
      ALineMap[Line].Offset     := 0;

      if Itm = nil
      then begin
        ALineMap[Line].State := lmsInvalid;
        HasLineOutOfRange := True;
        inc(Line);
        inc(idx);
        continue;
      end;

      if ( (Line = 0) and LineIsSrc )
      or ( (Line <> 0) and IsSourceBeforeItem(Itm, PrevItm) )
      then begin
        ALineMap[Line].Dump      := '';
        ALineMap[Line].Statement := '';
        if Itm^.SrcFileName <> ''
        then begin
          s := Itm^.SrcFileName;
          if not FDebugManager.GetFullFilename(s, False)
          then s := Itm^.SrcFileName;
          ALineMap[Line].State := lmsSource;
          ALineMap[Line].SourceLine := Itm^.SrcFileLine;
          ALineMap[Line].FileName   := Itm^.SrcFileName;
          ALineMap[Line].FullFileName := s;
          ALineMap[Line].PasCode := GetSourceCodeLine(Itm^.SrcFileName, Itm^.SrcFileLine);
        end
        else begin
          ALineMap[Line].State := lmsFuncName;
          ALineMap[Line].SourceLine := Itm^.Offset;
          ALineMap[Line].FileName   := Itm^.FuncName;
        end;
        inc(Line);
      end
      else
      if (Line = 0) and (not ANoExtraHeader)  // but it's not LineIsSrc
      and ( ( (Itm^.SrcFileName <> '') and (Itm^.SrcStatementIndex <> Itm^.SrcStatementCount-1) )
         or ( (Itm^.SrcFileName = '') and (Itm^.FuncName <> '') and (NextItm <> nil) and (Itm^.Offset < NextItm^.Offset) )
      )
      then begin
        ALineMap[Line].Dump      := '';
        ALineMap[Line].Statement := '';
        if Itm^.SrcFileName <> ''
        then begin
          s := Itm^.SrcFileName;
          if not FDebugManager.GetFullFilename(s, False)
          then s := Itm^.SrcFileName;
          ALineMap[Line].State := lmsSource;
          ALineMap[Line].SourceLine := Itm^.SrcFileLine;
          ALineMap[Line].FileName   := Itm^.SrcFileName;
          ALineMap[Line].FullFileName := s;
          if NextItm <> nil
          then ALineMap[Line].Statement := Format('(%d of %d)', [NextItm^.SrcStatementIndex, NextItm^.SrcStatementCount])
          else ALineMap[Line].Statement := Format('(??? of %d)', [Itm^.SrcStatementCount]);
          ALineMap[Line].PasCode := GetSourceCodeLine(Itm^.SrcFileName, Itm^.SrcFileLine);
        end
        else begin
          ALineMap[Line].State := lmsFuncName;
          ALineMap[Line].SourceLine := 0;
          if NextItm <> nil
          then ALineMap[Line].SourceLine := NextItm^.Offset;
          ALineMap[Line].FileName := Itm^.FuncName;
          if NextItm <> nil
          then ALineMap[Line].Statement := Format('(%d)', [NextItm^.Offset])
          else ALineMap[Line].Statement := '(???)';
        end;
        inc(Line);
        inc(idx); // displayed source-info, instead of asm (topline substituted)
        LineIsSrc := False;
        continue;
      end;
      LineIsSrc := False; // only for topline

      if Line >= ALineCount
      then break;

      ALineMap[Line].Addr       := Itm^.Addr;
      ALineMap[Line].Offset     := Itm^.Offset;
      ALineMap[Line].State      := lmsStatement;
      ALineMap[Line].Dump       := Itm^.Dump;
      ALineMap[Line].Statement  := Itm^.Statement;
      ALineMap[Line].SourceLine := Itm^.SrcFileLine;
      ALineMap[Line].ImageIndex := -1;
      ALineMap[Line].TargetAddr := Itm^.TargetAddr;
      ALineMap[Line].IsJump     := Itm^.IsJump;

      s := StringOfChar(' ', min(4, 40 - Length(ALineMap[Line].Statement))) + '#';
      if Itm^.TargetAddr <> 0 then begin
        ALineMap[Line].Statement := ALineMap[Line].Statement + s + ' $' + IntToHex(Itm^.TargetAddr, W);
        s := '';
      end;
      if Itm^.TargetName <> '' then begin
        ALineMap[Line].Statement := ALineMap[Line].Statement + s + ' ' + Itm^.TargetName;
        s := '';
      end;
      if Itm^.TargetFile <> '' then begin
        ALineMap[Line].Statement := ALineMap[Line].Statement + s + ' ' + ExtractFileName(Itm^.TargetFile);
        if Itm^.TargetLine <> 0 then
          ALineMap[Line].Statement := ALineMap[Line].Statement + ':' + IntToStr(Itm^.TargetLine);
        s := '';
      end;

      inc(Line);
      inc(idx);
    end;

  finally
    FUpdating := False;
    if FUpdateNeeded
    and ( (DoneLocation    <> FCurrentLocation)
       or (DoneTopLine     <> AFirstLine)
       or (DoneLineCount   <> ALineCount)
       or (HasLineOutOfRange
          and ( (DoneCountBefore <> FDisassembler.CountBefore)
             or (DoneCountAfter  <> FDisassembler.CountAfter) )  )
    )
    then UpdateLineData;
  end;
end;

initialization

  AsmWindowCreator := IDEWindowCreators.Add(DebugDialogNames[ddtAssembler]);
  AsmWindowCreator.OnCreateFormProc := @CreateDebugDialog;
  AsmWindowCreator.CreateSimpleLayout;

end.

