unit AssemblerDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics,
  IDEWindowIntf, DbgIntfBaseTypes, DbgIntfDebuggerBase,
  ComCtrls, StdCtrls, ExtCtrls, LclType, LCLIntf, DebuggerDlg, Debugger,
  BaseDebugManager, EditorOptions, Math, types, LCLProc, Menus, Clipbrd, ActnList,
  IDECommands, IDEImagesIntf, CodeToolManager, CodeCache, SourceEditor;

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
  end;
  TAsmDlgLineEntries = Array of TAsmDlgLineEntry;


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
    pnlToolAddr: TPanel;
    pbAsm: TPaintBox;
    PopupMenu1: TPopupMenu;
    sbHorizontal: TScrollBar;
    sbVertical: TScrollBar;
    Timer1: TTimer;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
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
    procedure pbAsmPaint(Sender: TObject);
    procedure sbHorizontalChange(Sender: TObject);
    procedure sbVerticalChange(Sender: TObject);
    procedure sbVerticalScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
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
    FIsVScrollTrack: Boolean;
    FVScrollCounter, FVScrollPos: Integer;

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
    procedure DoEditorOptsChanged(Sender: TObject; Restore: boolean);
  protected
    function GetSourceCodeLine(SrcFileName: string; SrcLineNumber: Integer): string;
    procedure DoBeginUpdate; override;
    procedure DoEndUpdate; override;
    procedure UpdateShowing; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetLocation(ADebugger: TDebuggerIntf; const AAddr: TDBGPtr; const ADispAddr: TDBGPtr = 0);
    property Disassembler: TIDEDisassembler read FDisassembler write SetDisassembler;
    property DebugManager: TBaseDebugManager read FDebugManager write FDebugManager;
    property BreakPoints;
  end;

implementation

{$R *.lfm}

uses
  LazarusIDEStrConsts;

var
  AsmWindowCreator: TIDEWindowCreator;

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

  inherited Create(AOwner);
//  DoubleBuffered := True;

  Caption := lisDisAssAssembler;

  EditorOpts.AddHandlerAfterWrite(@DoEditorOptsChanged);
  Caption := lisMenuViewAssembler;
  CopyToClipboard.Caption := lisDbgAsmCopyToClipboard;


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

  actCopy.Caption := lisCopy;
  actCopy.Hint := lisCopy;
  actCopy.ImageIndex := IDEImages.LoadImage('laz_copy');


  FPowerImgIdx := IDEImages.LoadImage('debugger_power');
  FPowerImgIdxGrey := IDEImages.LoadImage('debugger_power_grey');
  ToolButtonPower.ImageIndex := FPowerImgIdx;

  FCurLineImgIdx := IDEImages.LoadImage('debugger_current_line');
  //

  FImgSourceLine := IDEImages.LoadImage('debugger_source_line');
  FImgNoSourceLine := IDEImages.LoadImage('debugger_nosource_line');
end;

destructor TAssemblerDlg.Destroy;
begin
  EditorOpts.RemoveHandlerAfterWrite(@DoEditorOptsChanged);
  SetDisassembler(nil);
  SetDebugger(nil);
  FDisassemblerNotification.OnChange := nil;
  FDisassemblerNotification.ReleaseReference;
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
  SetLength(ALineMap, abs(FSelectionEndLine - FSelectLine)+1);
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
  inherited DoEndUpdate;
  if FVisibleChanged then begin
    DoEditorOptsChanged(nil, False);
    if FCurrentLocation <> 0 then
      UpdateLocation(FCurrentLocation);
  end;
  FVisibleChanged := False;
end;

procedure TAssemblerDlg.UpdateShowing;
begin
  inherited UpdateShowing;
  if IsVisible then begin
    if IsUpdating then begin
      FVisibleChanged := True
    end else begin
      DoEditorOptsChanged(nil, False);
      if FCurrentLocation <> 0 then
        UpdateLocation(FCurrentLocation);
    end;
  end;
end;

procedure TAssemblerDlg.pbAsmMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button <> mbLeft then exit;

  SetSelection(FTopLine + Y div FLineHeight, False, ssShift in Shift);
  FMouseIsDown := True;
end;

procedure TAssemblerDlg.pbAsmMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  y := Y div FLineHeight;
  if FMouseIsDown and (y >= 0) and (y < FLineCount)
  then SetSelection(FTopLine + Y, False, True);
end;

procedure TAssemblerDlg.pbAsmMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FMouseIsDown := False;
end;

procedure TAssemblerDlg.pbAsmMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  i, j: LongInt;
begin
  if not ToolButtonPower.Down then exit;
  Handled := True;

  FWheelAccu := FWheelAccu + WheelDelta;
  j := FWheelAccu div 120;
  if j = 0 then
    exit;

  FWheelAccu := FWheelAccu - j * 120;
  i := FTopLine ;
  if FSelectLine <> MaxInt
  then SetSelection(FSelectLine - j, False, ssShift in Shift);
  SetTopline(i - j);
end;

procedure TAssemblerDlg.pbAsmPaint(Sender: TObject);
var
  R: TRect;
  n, X, Y, Line, W: Integer;
  S: String;
  TextStyle: TTextStyle;
begin
  R := pbAsm.ClientRect;
  TextStyle := pbAsm.Canvas.TextStyle;
  TextStyle.Wordbreak := False;
  TextStyle.SingleLine := True;
  pbAsm.Canvas.TextStyle := TextStyle;

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
    if Line = FSelectLine
    then begin
      pbAsm.Canvas.Brush.Color := clHighlight;
      pbAsm.Canvas.Font.Color := clHighlightText;
      pbAsm.Canvas.FillRect(R.Left, n * FLineHeight, R.Right, (n + 1) * FLineHeight);
      if (FSelectionEndLine <> FSelectLine)
      then begin
        pbAsm.Canvas.Brush.Color := clHotLight;
        pbAsm.Canvas.Brush.Style := bsClear;
        pbAsm.Canvas.Rectangle(R.Left, n * FLineHeight, R.Right, (n + 1) * FLineHeight);
        pbAsm.Canvas.Brush.Style := bsSolid;
        pbAsm.Canvas.Brush.Color := clHighlight;
      end;
    end
    else if (FSelectionEndLine <> FSelectLine)
    and (line >= Min(FSelectLine, FSelectionEndLine))
    and (line <= Max(FSelectLine, FSelectionEndLine))
    then begin
      pbAsm.Canvas.Brush.Color := clHighlight;
      pbAsm.Canvas.Font.Color := clHighlightText;
      pbAsm.Canvas.FillRect(R.Left, n * FLineHeight, R.Right, (n + 1) * FLineHeight);
    end
    else begin
      pbAsm.Canvas.Brush.Color := pbAsm.Color;
      pbAsm.Canvas.Font.Color := pbAsm.Font.Color;
    end;
    pbAsm.Canvas.Font.Bold := (FLineMap[n].State in [lmsSource, lmsFuncName]);

    CheckImageIndexFor(FLineMap[n]);
    if (FLineMap[n].ImageIndex >= 0)
    then IDEImages.Images_16.Draw(pbAsm.Canvas, FGutterWidth - 16, Y, FLineMap[n].ImageIndex, True);

    S := FormatLine(FLineMap[n], W);
    pbAsm.Canvas.TextRect(R, X, Y, S);

    Inc(Y, FLineHeight);
    Inc(Line);
  end;
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
    lmsFuncName: Result:= ALine.FileName + ' ' + ALine.Statement;
  end;
end;

procedure TAssemblerDlg.UpdateView;
begin
  if not ToolButtonPower.Down
  then exit;

  if (FDisassembler <> nil) and (FCurrentLocation <> 0)
  then begin
    FDisassembler.PrepareRange(FCurrentLocation, Max(0, -(FTopLine - 5)), Max(0, FTopLine + FLineCount + 1 + 5));
    UpdateLineData;
  end
  else ClearLineMap;
  pbAsm.Invalidate;
end;

procedure TAssemblerDlg.UpdateActionEnabled;
var
  HasDisassembler: Boolean;
begin
  HasDisassembler := (FDebugger <> nil) and (FDisassembler <> nil);
  actCurrentInstr.Enabled := HasDisassembler and (FLocation <> 0);
  actGotoAddr.Enabled := HasDisassembler and (StrToQWordDef(EditGotoAddr.Text, 0) <> 0);
  actCopy.Enabled := HasDisassembler;
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

procedure TAssemblerDlg.DoEditorOptsChanged(Sender: TObject; Restore: boolean);
var
  TM: TTextMetric;
begin
  if Restore then exit;
  pbAsm.Font.Size := EditorOpts.EditorFontSize;
  pbAsm.Font.Name := EditorOpts.EditorFont;
  if EditorOpts.DisableAntialiasing then
    pbAsm.Font.Quality := fqNonAntialiased
  else
    pbAsm.Font.Quality := fqDefault;

  if GetTextMetrics(pbAsm.Canvas.Handle, TM{%H-}) then
  begin
    FCharWidth := TM.tmMaxCharWidth; // EditorOpts.ExtraCharSpacing +
    sbHorizontal.SmallChange := FCharWidth;
    FLineHeight := Max(6,EditorOpts.ExtraLineSpacing + TM.tmHeight);
    SetLineCount(pbAsm.Height div FLineHeight);
  end;
end;

procedure TAssemblerDlg.SetLocation(ADebugger: TDebuggerIntf; const AAddr: TDBGPtr;
  const ADispAddr: TDBGPtr);
var
  i: Integer;
begin
  SetDebugger(ADebugger);

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
  Editor: TSourceEditor;
begin
  Result := '';
  if SrcLineNumber < 1 then exit;
  if not FDebugManager.GetFullFilename(SrcFileName, False) // TODO: maybe ask user?
  then exit;
  PasSource := CodeToolBoss.LoadFile(SrcFileName, true, false);
  if PasSource = nil
  then exit;

  Editor := SourceEditorManager.SourceEditorIntfWithFilename(SrcFileName);
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
                  or ( (APrvItm <> nil) and (AItm^.FuncName <> APrvItm^.FuncName) );
    end;
  end;

var
  DoneLocation: TDBGPtr;
  DoneTopLine, DoneLineCount: Integer;
  DoneCountBefore, DoneCountAfter: Integer;
  Line, Idx: Integer;
  Itm, NextItm, PrevItm: PDisassemblerEntry;
  LineIsSrc, HasLineOutOfRange: Boolean;
  s: String;
begin
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

