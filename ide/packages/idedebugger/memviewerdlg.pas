unit MemViewerDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, LCLType, Menus, IDEWindowIntf, IDEImagesIntf,
  IdeDebuggerWatchValueIntf, SrcEditorIntf, EditorSyntaxHighlighterDef,
  IDEOptEditorIntf, SynEdit, DbgIntfDebuggerBase, LazClasses, LazDebuggerIntf,
  LazDebuggerIntfBaseTypes, LazDebuggerUtils, DebuggerDlg, BaseDebugManager, Debugger,
  IdeDebuggerStringConstants, SynGutterLineNumber, SynEditHighlighter,
  LazNumEdit;

type

  { TMemViewDlg }

  TMemViewDlg = class(TDebuggerDlg)
    edAddressBase: TEdit;
    edMemViewer: TSynEdit;
    edDataLen: TLazIntegerEdit;
    edAddressOffs: TLazIntegerEdit;
    menByte: TMenuItem;
    menQWordBE: TMenuItem;
    menWordLE: TMenuItem;
    menDWordLE: TMenuItem;
    menQWordLE: TMenuItem;
    menWordBE: TMenuItem;
    menDWordBE: TMenuItem;
    popGroup: TPopupMenu;
    Separator1: TMenuItem;
    Separator2: TMenuItem;
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    btnAddrDown: TToolButton;
    btnDiv1: TToolButton;
    btnPower: TToolButton;
    btnAddrUp: TToolButton;
    btnDiv2: TToolButton;
    btnMemLenDown: TToolButton;
    btnMemLenUp: TToolButton;
    btnDIv3: TToolButton;
    btnGrouping: TToolButton;
    procedure btnAddrDownClick(Sender: TObject);
    procedure btnAddrUpClick(Sender: TObject);
    procedure btnMemLenDownClick(Sender: TObject);
    procedure btnMemLenUpClick(Sender: TObject);
    procedure btnPowerClick(Sender: TObject);
    procedure edAddressBaseEditingDone(Sender: TObject);
    procedure edAddressOffsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edDataLenEditingDone(Sender: TObject);
    procedure edAddressOffsEditingDone(Sender: TObject);
    procedure edDataLenKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edMemViewerDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure edMemViewerDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean);
    procedure menuGroupItemClicked(Sender: TObject);
    procedure SynGutterLineNumber1FormatLineNumber(
      Sender: TSynGutterLineNumber; ALine: integer; out AText: string;
      const {%H-}ALineInfo: TSynEditGutterLineInfo);
    procedure btnCancelOffsClick(Sender: TObject);
  private
    FPowerImgIdx, FPowerImgIdxGrey: Integer;
    FUpdatingAddr: boolean;
    FWatches: TCurrentWatches;
    FCurrentWatchValue: TIdeWatchValue;
    FCurrentDisplayedWatchExpr: String;
    FCurrentDisplayedWatchMemAddr: TDBGPtr;
    FCurrentWatchMode: (wmNone, wmInit, wmInitAddress, wmBefore, wmAfter);
    FTargetWidth: Integer;
    FCurrentMemAddress: TDBGPtr;     // Address for the actual known data i FCurrentMemData
    FCurrentMemData: RawByteString;
    FCurrentEditAddrBaseText: String;    // Value for the start field
    FCurrentEditAddrBase: TDBGPtr; // Known Address for the start (WITHOUT OFFSET)
    FBeforeErrorAddress, FAfterErrorAddress: TDBGPtr;
    FGrouping: integer;

    function IsPlainAddress(AText: String; out AnAddr: TDBGPtr): boolean;
    procedure DoDebuggerState(ADebugger: TDebuggerIntf; AnOldState: TDBGState);
    procedure DoWatchUpdated(const ASender: TIdeWatches; const AWatch: TIdeWatch);
    procedure FetchInitialMem(AnUseCurrentEditAddr: Boolean = False);
    procedure FetchBeforeCurrent(ANewStart: TDBGPtr);
    procedure FetchAfterCurrent(ANewStart: TDBGPtr; ACount: integer);
    procedure DoAddrChanged;
    procedure UpdateSynText(AKeepLines: Boolean = False);
  protected
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute(AnExpression: String);
  end;

procedure CreateDebugDialog(Sender: TObject; aFormName: string;
                            var AForm: TCustomForm; DoDisableAutoSizing: boolean);

implementation

var
  MemViewWindowCreator: TIDEWindowCreator;

procedure CreateDebugDialog(Sender: TObject; aFormName: string;
  var AForm: TCustomForm; DoDisableAutoSizing: boolean);
begin
  DebugBoss.CreateDebugDialog(Sender, aFormName, AForm, DoDisableAutoSizing);
end;

{$R *.lfm}

{ TMemViewDlg }

procedure TMemViewDlg.btnCancelOffsClick(Sender: TObject);
begin
end;

function TMemViewDlg.IsPlainAddress(AText: String; out AnAddr: TDBGPtr): boolean;
begin
  AText := Trim(AText);
  Result := (AText <> '') and (AText[1] in ['$', '0'..'9']) and TryStrToQWord(AText, AnAddr);
end;

procedure TMemViewDlg.SynGutterLineNumber1FormatLineNumber(
  Sender: TSynGutterLineNumber; ALine: integer; out AText: string;
  const ALineInfo: TSynEditGutterLineInfo);
begin
  AText := Dec64ToNumb(FCurrentMemAddress + 16 * (ALine-1), FTargetWidth*2, 16);
end;

procedure TMemViewDlg.edAddressBaseEditingDone(Sender: TObject);
begin
  DoAddrChanged;
end;

procedure TMemViewDlg.edAddressOffsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  v: Int64;
begin
  if not edAddressOffs.Valid then
    exit;

  v := 0;
  case Key of
    VK_UP: begin
        if ssCtrl in Shift then v := 64
        else
        if ssAlt in Shift  then v :=  1
        else                    v := 16;
      end;
    VK_DOWN: begin
        if ssCtrl in Shift then v := -64
        else
        if ssAlt in Shift  then v :=  -1
        else                    v := -16;
      end;
    VK_PRIOR: begin
        if ssCtrl in Shift then v := 1024
        else                    v :=  512;
      end;
    VK_NEXT: begin
        if ssCtrl in Shift then v := -1024
        else                    v :=  -512;
      end;
  end;
  if v <> 0 then begin
    Key := 0;
    edAddressOffs.Value := edAddressOffs.CurrentValue + v;
    if not (ssShift in Shift) then
      edDataLen.Value := edDataLen.CurrentValue - v;
  end;
end;

procedure TMemViewDlg.edDataLenEditingDone(Sender: TObject);
begin
  DoAddrChanged;
end;

procedure TMemViewDlg.edAddressOffsEditingDone(Sender: TObject);
begin
  DoAddrChanged;
end;

procedure TMemViewDlg.edDataLenKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  v: Int64;
begin
  if not edDataLen.Valid then
    exit;

  v := 0;
  case Key of
    VK_UP: begin
        if ssCtrl in Shift then v := 64
        else
        if ssAlt in Shift  then v :=  1
        else                    v := 16;
      end;
    VK_DOWN: begin
        if ssCtrl in Shift then v := -64
        else
        if ssAlt in Shift  then v :=  -1
        else                    v := -16;
      end;
    VK_PRIOR: begin
        if ssCtrl in Shift then v := 1024
        else                    v :=  512;
      end;
    VK_NEXT: begin
        if ssCtrl in Shift then v := -1024
        else                    v :=  -512;
      end;
  end;
  if v <> 0 then begin
    Key := 0;
    edDataLen.Value := edDataLen.CurrentValue + v;
  end;
end;

procedure TMemViewDlg.edMemViewerDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  IDrag: IIdeDbgDragDropWatchSource;
begin
  Accept := ( (Source is TSynEdit) and (TSynEdit(Source).SelAvail) ) or
            ( (Source is TCustomEdit) and (TCustomEdit(Source).SelText <> '') );

  if (not Accept) and (
      Source.GetInterface(IIdeDbgDragDropWatchSource, IDrag) or
      ( (Source is TComponent) and
        TComponent(Source).Owner.GetInterface(IIdeDbgDragDropWatchSource, IDrag) )
  )
  then begin
    Accept := IDrag.DragWatchCount(Source) = 1; // must be exactly one
    IDrag.DragWatchDone(Source);
  end;
end;

procedure TMemViewDlg.edMemViewerDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  s: String;
  IDrag: IIdeDbgDragDropWatchSource;
  AWatch: TCurrentWatch;
begin
  if (Source is TSynEdit) then begin
    if TSynEdit(Source).SelAvail then
      Execute(TSynEdit(Source).SelText);
    exit;
  end;

  if (Source is TCustomEdit) then begin
    s := TCustomEdit(Source).SelText;
    if s <> '' then
      Execute(s);
    exit;
  end;

  if Source.GetInterface(IIdeDbgDragDropWatchSource, IDrag) or
     ( (Source is TComponent) and
       TComponent(Source).Owner.GetInterface(IIdeDbgDragDropWatchSource, IDrag) )
  then begin
    if IDrag.DragWatchCount(Source) = 1 then begin
      FWatches.BeginUpdate;
      AWatch := FWatches.Add(''); // Do not destroy the watch, UpdateData will take care of it
      IDrag.DragWatchInit(Source, 0, AWatch);
      s := AWatch.Expression;
      AWatch.Destroy;
      FWatches.EndUpdate;
      Execute(s);
    end;
    IDrag.DragWatchDone(Source);
    exit;
  end;
end;

procedure TMemViewDlg.menuGroupItemClicked(Sender: TObject);
begin
  FGrouping := TMenuItem(Sender).Tag;
  btnGrouping.Caption := TMenuItem(Sender).Caption;
  UpdateSynText(True);
end;

procedure TMemViewDlg.btnAddrDownClick(Sender: TObject);
begin
  FUpdatingAddr := True;
  if ssCtrl in GetKeyShiftState then
    edAddressOffs.Value := edAddressOffs.Value - 64
  else
    edAddressOffs.Value := edAddressOffs.Value - 16;
  if not (ssShift in GetKeyShiftState) then
    edDataLen.Value := edDataLen.Value + 16;
  FUpdatingAddr := False;
  DoAddrChanged;
end;

procedure TMemViewDlg.btnAddrUpClick(Sender: TObject);
begin
  FUpdatingAddr := True;
  if ssCtrl in GetKeyShiftState then
    edAddressOffs.Value := edAddressOffs.Value + 64
  else
    edAddressOffs.Value := edAddressOffs.Value + 16;
  if not (ssShift in GetKeyShiftState) then
    edDataLen.Value := edDataLen.Value - 16;
  FUpdatingAddr := False;
  DoAddrChanged;
end;

procedure TMemViewDlg.btnMemLenDownClick(Sender: TObject);
begin
  FUpdatingAddr := True;
  if ssCtrl in GetKeyShiftState then
    edDataLen.Value := edDataLen.Value - 64
  else
    edDataLen.Value := edDataLen.Value - 16;
  FUpdatingAddr := False;
  DoAddrChanged;
end;

procedure TMemViewDlg.btnMemLenUpClick(Sender: TObject);
begin
  FUpdatingAddr := True;
  if ssCtrl in GetKeyShiftState then
    edDataLen.Value := edDataLen.Value + 64
  else
    edDataLen.Value := edDataLen.Value + 16;
  FUpdatingAddr := False;
  DoAddrChanged;
end;

procedure TMemViewDlg.btnPowerClick(Sender: TObject);
begin
  if btnPower.Down then begin
    btnPower.ImageIndex := FPowerImgIdx;
    FCurrentMemData := '';
    DoAddrChanged;
  end
  else begin
    btnPower.ImageIndex := FPowerImgIdxGrey;
  end;
end;

procedure TMemViewDlg.DoDebuggerState(ADebugger: TDebuggerIntf;
  AnOldState: TDBGState);
begin
  if (not btnPower.Down) or (not Visible) then
    exit;
  if (ADebugger.State = dsPause) and (AnOldState <> dsPause) then begin
    FCurrentMemData := '';
    DoAddrChanged;
  end;
end;

procedure TMemViewDlg.UpdateSynText(AKeepLines: Boolean);
const
  HDIG = '0123456789ABCDEF';
  L1 = 3*16+1;
  L2 = 4 + 16;
var
  s: String;
  CurLnIdx, CurLineChrIdx, mpos, mlen, b, line: Integer;
  grp, g: Integer;
begin
  if FCurrentMemData = '' then
    exit;

  if (edAddressBase.Text = FCurrentDisplayedWatchExpr) and
     (FCurrentDisplayedWatchMemAddr = FCurrentMemAddress)
  then
    AKeepLines := True;
  FCurrentDisplayedWatchExpr := edAddressBase.Text;
  FCurrentDisplayedWatchMemAddr := FCurrentMemAddress;

  edMemViewer.BeginUpdate;
  if not AKeepLines then
    edMemViewer.Lines.Clear;

  line := 0;
  s := StringOfChar(' ', L1 + L2);
  CurLnIdx := 1;
  CurLineChrIdx := L1 + 4;
  mpos := 1;
  mlen := Length(FCurrentMemData);
  grp := abs(FGrouping);
  while mPos <= mlen do begin
    while grp > (mlen-mpos+1) do grp := grp div 2;
    if FGrouping < 0 then begin
      // big endian
      for g := 1 to grp do begin
        b := ord(FCurrentMemData[mPos+g-1]);
        s[CurLnIdx] := HDIG[(b div 16)+1];      inc(CurLnIdx);
        s[CurLnIdx] := HDIG[(b and 15)+1];      inc(CurLnIdx);
      end
    end
    else begin
      // little endian
      for g := 1 to grp do begin
        b := ord(FCurrentMemData[mPos+FGrouping-g]);
        s[CurLnIdx] := HDIG[(b div 16)+1];      inc(CurLnIdx);
        s[CurLnIdx] := HDIG[(b and 15)+1];      inc(CurLnIdx);
      end
    end;

    // chars
    for g := 1 to grp do begin
      b := ord(FCurrentMemData[mPos+g-1]);
      if (b >= 32) and (b <= 127) then
        s[CurLineChrIdx] := char(b)
      else
        s[CurLineChrIdx] := '.';
      inc(CurLineChrIdx);
    end;

    inc(mpos, grp);


    if ((mPos and 15) = 1) then begin
      if AKeepLines and (line < edMemViewer.Lines.Count) then
        edMemViewer.Lines[line] := s
      else
        edMemViewer.Lines.Add(s);
      inc(line);
      s := StringOfChar(' ', L1 + L2);
      CurLnIdx := 1;
      CurLineChrIdx := L1 + 4;
    end
    else
    if ((mPos and 7) = 1) then
      inc(CurLnIdx, 2)
    else
      inc(CurLnIdx);
  end;

  if CurLnIdx > 1 then begin
    SetLength(s, CurLineChrIdx);
    if AKeepLines and (line < edMemViewer.Lines.Count) then begin
      edMemViewer.Lines[line] := s;
      inc(line);
      while edMemViewer.Lines.Count > line do
        edMemViewer.Lines.Delete(line);
    end
    else
      edMemViewer.Lines.Add(s);
  end;

  edMemViewer.EndUpdate;
end;

procedure TMemViewDlg.DoWatchUpdated(const ASender: TIdeWatches;
  const AWatch: TIdeWatch);
var
  AWatchAddr, dummy: TDBGPtr;
  t: RawByteString;
  r: Int64;
begin
  if (FCurrentWatchValue = nil) or
     (AWatch <> FCurrentWatchValue.Watch) or
     (FCurrentWatchValue.Watch = nil) or
     (ASender <> FWatches) or
     (not (FCurrentWatchValue.Validity in [ddsError, ddsInvalid, ddsValid]))
  then
    exit;

  if (FCurrentWatchMode in [wmInit, wmInitAddress]) or (FCurrentMemData = '') then begin
    FCurrentMemData := '';
    if (FCurrentWatchValue.ResultData <> nil) and
       (FCurrentWatchValue.ResultData.ValueKind = rdkMemDump) and
       (FCurrentWatchValue.ResultData.DataAddress <> 0)
    then begin
      FCurrentMemAddress     := FCurrentWatchValue.ResultData.DataAddress;
      if FCurrentEditAddrBase = 0 then
        FCurrentEditAddrBase := FCurrentMemAddress;
      FCurrentMemData := FCurrentWatchValue.ResultData.AsString;
      if FCurrentWatchMode = wmInit then begin
        edMemViewer.Gutter.Visible := True;
        FTargetWidth := DebugBoss.TargetWidth div 8;
        TSynGutterLineNumber(edMemViewer.Gutter.Parts.ByClass[TSynGutterLineNumber,0]).DigitCount := FTargetWidth * 2;
        UpdateSynText;
        StatusBar1.Visible := not IsPlainAddress(FCurrentEditAddrBaseText, dummy);
        StatusBar1.SimpleText := FCurrentEditAddrBaseText + ' @ $' + Dec64ToNumb(FCurrentEditAddrBase, FTargetWidth*2, 16);
      end;
    end
    else begin
      FCurrentMemAddress     := 0;
      edMemViewer.Gutter.Visible := False;
      edMemViewer.Text := FCurrentWatchValue.Value;
      StatusBar1.Visible := False;
      exit;
    end;
  end

  else
  if (FCurrentWatchMode = wmBefore) and
     (IsPlainAddress(FCurrentWatchValue.Watch.Expression, AWatchAddr))
  then begin
    if (FCurrentWatchValue.ResultData = nil) or
       (FCurrentWatchValue.ResultData.ValueKind <> rdkMemDump) or
       (FCurrentWatchValue.ResultData.DataAddress = 0) or
       (FCurrentWatchValue.Validity <> ddsValid)
    then begin
      FBeforeErrorAddress := AWatchAddr;
    end
    else begin
      AWatchAddr := FCurrentWatchValue.ResultData.DataAddress;
      t := FCurrentWatchValue.ResultData.AsString;
      if Length(t) > edDataLen.Value then
        SetLength(t, edDataLen.Value);
      if Length(t) = edDataLen.Value then begin
        FCurrentMemData := t;
        FCurrentMemAddress := AWatchAddr;
      end
      else
      if AWatchAddr < FCurrentMemAddress then begin
        {$PUSH}{$Q-}{$R-}
        if AWatchAddr + Length(t) > FCurrentMemAddress then
          SetLength(t, FCurrentMemAddress - AWatchAddr);
        if AWatchAddr + Length(t) = FCurrentMemAddress then begin
          FCurrentMemData := t + FCurrentMemData;
          FCurrentMemAddress := AWatchAddr;
        end;
        {$POP}
      end;
      UpdateSynText;
    end;
  end

  else
  if (FCurrentWatchMode = wmAfter) and
     (IsPlainAddress(FCurrentWatchValue.Watch.Expression, AWatchAddr))
  then begin
    {$PUSH}{$Q-}{$R-}
    if (FCurrentWatchValue.ResultData = nil) or
       (FCurrentWatchValue.ResultData.ValueKind <> rdkMemDump) or
       (FCurrentWatchValue.ResultData.DataAddress = 0) or
       (FCurrentWatchValue.Validity <> ddsValid)
    then begin
      r := FCurrentWatchValue.Watch.RepeatCount;
      if high(AWatchAddr) - AWatchAddr >= r then
        FAfterErrorAddress := high(AWatchAddr)
      else
        FAfterErrorAddress := AWatchAddr + FCurrentWatchValue.Watch.RepeatCount;
    end
    else begin
      AWatchAddr := FCurrentWatchValue.ResultData.DataAddress;
      if AWatchAddr > FCurrentMemAddress then begin
        if AWatchAddr < FCurrentMemAddress + Length(FCurrentMemData) then
           SetLength(FCurrentMemData, AWatchAddr - FCurrentMemAddress);
        if AWatchAddr = FCurrentMemAddress + Length(FCurrentMemData) then
          FCurrentMemData := FCurrentMemData + FCurrentWatchValue.ResultData.AsString;
        UpdateSynText;
      end;
    end;
    {$POP}
  end;

  FCurrentWatchMode := wmNone;
  if FCurrentWatchValue.Validity = ddsValid then
    DoAddrChanged;
end;

procedure TMemViewDlg.FetchInitialMem(AnUseCurrentEditAddr: Boolean);
var
  AWatch: TCurrentWatch;
  tid, idx: integer;
  Offs, DLen: Int64;
  stack: TIdeCallStack;
  ANewAddr: TDBGPtr;
  WTxt: String;
  wv: TIdeWatchValue;
begin
  tid    := ThreadsMonitor.CurrentThreads.CurrentThreadId;
  stack  := CallStackMonitor.CurrentCallStackList.EntriesForThreads[tid];
  idx := 0;
  if stack <> nil then
    idx := stack.CurrentIndex;

  if (FCurrentWatchValue <> nil) and
     (FCurrentWatchValue.Watch <> nil) and
     (FCurrentWatchValue.Watch.Expression = edAddressBase.Text) and
     (FCurrentWatchValue.Watch.RepeatCount >= edDataLen.Value) and
     (FCurrentWatchValue.ThreadId = tid) and
     (FCurrentWatchValue.StackFrame = idx) and
     (FCurrentWatchValue.Validity = ddsRequested)
  then
    exit;

  FCurrentEditAddrBaseText   := edAddressBase.Text;
  if not AnUseCurrentEditAddr then
    FCurrentEditAddrBase := 0;
  FCurrentMemAddress     := 0;
  FCurrentMemData        := '';
  FBeforeErrorAddress    := 0;
  FCurrentWatchMode      := wmInit;

  WTxt := FCurrentEditAddrBaseText;
  Offs := edAddressOffs.Value;
  DLen := edDataLen.Value;

  {$PUSH}{$Q-}{$R-}
  if AnUseCurrentEditAddr then begin
    WTxt := '$' + Dec64ToNumb(FCurrentEditAddrBase + Offs, 1, 16);
  end
  else
  if IsPlainAddress(FCurrentEditAddrBaseText, ANewAddr) then begin
    FCurrentEditAddrBase := ANewAddr;
    ANewAddr := ANewAddr + Offs;
    WTxt := '$' + Dec64ToNumb(ANewAddr, 1, 16);
  end
  else begin
    // if offs > 0 then fetch the extra .... TODO: get only address
    if Offs > Max(1024, DLen div 2) then begin
      DLen := 1;  // address only
      FCurrentWatchMode := wmInitAddress;
    end
    else
    if Offs < 0 then begin
      if -Offs >= DLen then
        FCurrentWatchMode := wmInitAddress;
      DLen := Max(1, DLen + Offs);
    end
  end;
  {$POP}

  ReleaseRefAndNil(FCurrentWatchValue);
  FWatches.BeginUpdate;
  FWatches.Clear;
  AWatch := FWatches.Add(WTxt);
  AWatch.EvaluateFlags := [defMemDump];
  AWatch.RepeatCount := DLen;
  AWatch.Enabled := True;
  FWatches.EndUpdate;

  FCurrentWatchValue := AWatch.Values[tid, idx];
  wv := FCurrentWatchValue;
  wv.AddReference;
  if FCurrentWatchValue <> nil then begin
    FCurrentWatchValue.AddReference;
    FCurrentWatchValue.Value;
  end;
  wv.ReleaseReference;
  if pointer(wv) <> pointer(FCurrentWatchValue) then // wv may have been freed, but the pointer will still be unique
    exit;

  DoWatchUpdated(FWatches, AWatch);
end;

procedure TMemViewDlg.FetchBeforeCurrent(ANewStart: TDBGPtr);
var
  AWatch: TCurrentWatch;
  tid: Integer;
  AWatchAddr: TDBGPtr;
  wv: TIdeWatchValue;
begin
  if (FBeforeErrorAddress <> 0) and
     (ANewStart <= FBeforeErrorAddress)
  then
    exit;

  tid    := ThreadsMonitor.CurrentThreads.CurrentThreadId;

  if (FCurrentWatchValue <> nil) and
     (FCurrentWatchValue.Watch <> nil) and
     (FCurrentWatchValue.Validity = ddsRequested)
  then begin
    if (FCurrentWatchMode in [wmNone, wmInit]) then begin
      FetchInitialMem;
      exit;
    end;

    if (FCurrentWatchMode = wmBefore) and
       (IsPlainAddress(FCurrentWatchValue.Watch.Expression, AWatchAddr)) and
       (AWatchAddr >= FCurrentMemAddress) and
       {$PUSH}{$Q-}{$R-}
       (AWatchAddr + FCurrentWatchValue.Watch.RepeatCount < FCurrentMemAddress)
       {$POP}
    then
      exit;
  end;

  FCurrentWatchMode := wmBefore;

  ReleaseRefAndNil(FCurrentWatchValue);
  FWatches.BeginUpdate;
  FWatches.Clear;
  AWatch := FWatches.Add('$'+Dec64ToNumb(ANewStart, 1, 16));
  AWatch.EvaluateFlags := [defMemDump];
  {$PUSH}{$Q-}{$R-}
  AWatch.RepeatCount := FCurrentMemAddress - ANewStart;
  {$POP}
  AWatch.Enabled := True;
  FWatches.EndUpdate;

  FCurrentWatchValue := AWatch.Values[tid, 0];
  wv := FCurrentWatchValue;
  wv.AddReference;
  if FCurrentWatchValue <> nil then begin
    FCurrentWatchValue.AddReference;
    FCurrentWatchValue.Value;
  end;
  wv.ReleaseReference;
  if pointer(wv) <> pointer(FCurrentWatchValue) then // wv may have been freed, but the pointer will still be unique
    exit;

  DoWatchUpdated(FWatches, AWatch);
end;

procedure TMemViewDlg.FetchAfterCurrent(ANewStart: TDBGPtr; ACount: integer);
var
  AWatch: TCurrentWatch;
  tid: Integer;
  AWatchAddr: TDBGPtr;
  wv: TIdeWatchValue;
begin
  {$PUSH}{$Q-}{$R-}
  if (FAfterErrorAddress <> 0) and
     (ANewStart + ACount >= FAfterErrorAddress)
  then
    exit;

  tid    := ThreadsMonitor.CurrentThreads.CurrentThreadId;

  if (FCurrentWatchValue <> nil) and
     (FCurrentWatchValue.Watch <> nil) and
     (FCurrentWatchValue.Validity = ddsRequested)
  then begin
    if (FCurrentWatchMode in [wmNone, wmInit]) then begin
      FetchInitialMem;
      exit;
    end;

    if (FCurrentWatchMode = wmAfter) and
       (IsPlainAddress(FCurrentWatchValue.Watch.Expression, AWatchAddr)) and
       (AWatchAddr <= FCurrentMemAddress + Length(FCurrentMemData)) and
       (AWatchAddr + FCurrentWatchValue.Watch.RepeatCount > FCurrentMemAddress + Length(FCurrentMemData))
    then
      exit;
  end;
  {$POP}

  FCurrentWatchMode := wmAfter;

  ReleaseRefAndNil(FCurrentWatchValue);
  FWatches.BeginUpdate;
  FWatches.Clear;
  AWatch := FWatches.Add('$'+Dec64ToNumb(ANewStart, 1, 16));
  AWatch.EvaluateFlags := [defMemDump];
  AWatch.RepeatCount := ACount;
  AWatch.Enabled := True;
  FWatches.EndUpdate;

  FCurrentWatchValue := AWatch.Values[tid, 0];
  wv := FCurrentWatchValue;
  wv.AddReference;
  if FCurrentWatchValue <> nil then begin
    FCurrentWatchValue.AddReference;
    FCurrentWatchValue.Value;
  end;
  wv.ReleaseReference;
  if pointer(wv) <> pointer(FCurrentWatchValue) then // wv may have been freed, but the pointer will still be unique
    exit;

  DoWatchUpdated(FWatches, AWatch);
end;

procedure TMemViewDlg.DoAddrChanged;
const
  IGN_OVERLAP = 8;
var
  NewMemStart: TDBGPtr;
  Offs, MLen: integer;
  IsKnownAddr: Boolean;
begin
  if (not btnPower.Down) or FUpdatingAddr then
    exit;

  IsKnownAddr := (edAddressBase.Text = FCurrentEditAddrBaseText) and  // FCurrentEditAddrBase is valid
                 (FCurrentEditAddrBase <> 0);

  if (FCurrentMemData = '') or (FCurrentMemAddress = 0) or (not IsKnownAddr)
  then begin
    FetchInitialMem;
    exit;
  end;

  // FCurrentEditAddrBase is valid

  {$PUSH}{$Q-}{$R-}
  Offs := edAddressOffs.Value;
  MLen := edDataLen.Value;
  NewMemStart := FCurrentEditAddrBase + Offs;

  if (NewMemStart <= FCurrentMemAddress - MLen + IGN_OVERLAP) or
     (NewMemStart >= FCurrentMemAddress + Length(FCurrentMemData) - IGN_OVERLAP) or
     (Length(FCurrentMemData) < 2 * IGN_OVERLAP)
  then begin
    FetchInitialMem(True);
    exit;
  end;

  // There is an overlap with existing data

  if NewMemStart <= FBeforeErrorAddress then begin
    NewMemStart := FBeforeErrorAddress + (FCurrentMemAddress - FBeforeErrorAddress) div 2;
    if (NewMemStart > FCurrentMemAddress) or (NewMemStart <= FBeforeErrorAddress) then
      NewMemStart := FCurrentMemAddress;
  end;

  if NewMemStart > FCurrentMemAddress then begin
    Delete(FCurrentMemData, 1, NewMemStart-FCurrentMemAddress);
    FCurrentMemAddress := NewMemStart;
    UpdateSynText;
  end
  else
  if NewMemStart < FCurrentMemAddress then begin
    FetchBeforeCurrent(NewMemStart);
    exit;
  end;

  if Length(FCurrentMemData) > MLen then begin
    SetLength(FCurrentMemData, MLen);
    UpdateSynText;
  end
  else
  if Length(FCurrentMemData) < MLen then begin
    FetchAfterCurrent(FCurrentMemAddress + Length(FCurrentMemData), MLen - Length(FCurrentMemData));
  end;
  {$POP}
end;

constructor TMemViewDlg.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ThreadsMonitor := DebugBoss.Threads;
  CallStackMonitor := DebugBoss.CallStack;
  WatchesMonitor := DebugBoss.Watches;

  FWatches := TCurrentWatches.Create(DebugBoss.Watches);
  WatchesNotification.OnUpdate    := @DoWatchUpdated;
  DebugBoss.RegisterStateChangeHandler(@DoDebuggerState);

  Caption := liswlMemView;
  ToolBar1.Images := IDEImages.Images_16;

  FPowerImgIdx := IDEImages.LoadImage('debugger_power');
  FPowerImgIdxGrey := IDEImages.LoadImage('debugger_power_grey');
  btnPower.ImageIndex := FPowerImgIdx;

  btnAddrDown.ImageIndex := IDEImages.LoadImage('NavArrow_L');
  btnAddrUp.ImageIndex   := IDEImages.LoadImage('NavArrow_R');
  btnMemLenDown.ImageIndex := IDEImages.LoadImage('NavArrow_L');
  btnMemLenUp.ImageIndex   := IDEImages.LoadImage('NavArrow_R');

  edAddressBase.Hint := rsBaseAddress;
  edAddressOffs.Hint := rsAddressOffset;
  edDataLen.Hint := rsLength;

  btnGrouping.Caption:= MemViewGroupByte;
  menByte.Caption    := MemViewGroupByte;
  menWordLE.Caption  := MemViewGroupWordLittleEndian;
  menDWordLE.Caption := MemViewGroupDWordLittleEndian;
  menQWordLE.Caption := MemViewGroupQWordLittleEndian;
  menWordBE.Caption  := MemViewGroupWordBigEndian;
  menDWordBE.Caption := MemViewGroupDWordBigEndian;
  menQWordBE.Caption := MemViewGroupQWordBigEndian;
  FGrouping := 1;

  FTargetWidth := DebugBoss.TargetWidth div 8;
  TSynGutterLineNumber(edMemViewer.Gutter.Parts.ByClass[TSynGutterLineNumber,0]).DigitCount := FTargetWidth * 2;
end;

destructor TMemViewDlg.Destroy;
begin
  DebugBoss.UnregisterStateChangeHandler(@DoDebuggerState);
  ReleaseRefAndNil(FCurrentWatchValue);
  FreeAndNil(FWatches);
  inherited Destroy;
end;

procedure TMemViewDlg.Execute(AnExpression: String);
begin
  if not btnPower.Down then
    exit;
  edAddressBase.Text := AnExpression;
  DoAddrChanged;
end;

initialization

  MemViewWindowCreator := IDEWindowCreators.Add(DebugDialogNames[ddtMemViewer]);
  MemViewWindowCreator.OnCreateFormProc := @CreateDebugDialog;
  MemViewWindowCreator.CreateSimpleLayout;

end.

