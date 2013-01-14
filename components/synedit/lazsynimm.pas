unit LazSynIMM;

{$mode objfpc}{$H+}

{off $DEFINE WinIMEDebug}

interface

uses
  windows, imm, Classes, SysUtils, LazLoggerBase, LCLType, LCLProc, Controls,
  Graphics, SynEditMiscClasses, SynTextDrawer, SynEditPointClasses, SynEditMarkupSelection,
  SynEditMarkup, SynEditTypes, SynEditKeyCmds, LazSynEditText;

{$IFDEF WINCE} {$IF (FPC_FULLVERSION < 20700)}
function ImmSetCompositionFontA(_himc:HIMC; lplf:LPLOGFONT):BOOL; external ImmDLL name 'ImmSetCompositionFontA';
const
  WM_IME_REQUEST = $0288;
{$ENDIF}{$ENDIF}
type

  { LazSynIme }

  LazSynIme = class(TSynEditFriend)
  private
    FInvalidateLinesMethod: TInvalidateLines;
  protected
    FInCompose: Boolean;
    procedure InvalidateLines(FirstLine, LastLine: integer);
    procedure StopIme(Success: Boolean);
  public
    constructor Create(AOwner: TSynEditBase); reintroduce;
    procedure WMImeRequest(var Msg: TMessage); virtual;
    procedure WMImeNotify(var Msg: TMessage); virtual;
    procedure WMImeComposition(var Msg: TMessage); virtual;
    procedure WMImeStartComposition(var Msg: TMessage); virtual;
    procedure WMImeEndComposition(var Msg: TMessage); virtual;
    procedure FocusKilled; virtual;
    property InvalidateLinesMethod : TInvalidateLines write FInvalidateLinesMethod;
  end;

  { LazSynImeSimple }

  LazSynImeSimple = class(LazSynIme)
  private
    FImeBlockSelection: TSynEditSelection;
    FImeWinX, FImeWinY: Integer;
    FTextDrawer: TheTextDrawer;
    procedure SetTextDrawer(AValue: TheTextDrawer);
    procedure UpdateImeWinXY(aX, aY: Integer; aImc: HIMC = 0; aForce: Boolean = False);
    procedure UpdateImeWinFont(aImc: HIMC = 0);
    procedure DoStatusChanged(Sender: TObject; Changes: TSynStatusChanges);
    procedure DoDrawerFontChanged(Sender: TObject);
    procedure DoOnCommand(Sender: TObject; AfterProcessing: boolean; var Handled: boolean;
      var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer;
      HandlerData: pointer);
    procedure DoOnMouse(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
  public
    constructor Create(AOwner: TSynEditBase);
    destructor Destroy; override;
    procedure WMImeNotify(var Msg: TMessage); override;
    procedure WMImeComposition(var Msg: TMessage); override;
    procedure WMImeStartComposition(var Msg: TMessage); override;
    procedure WMImeEndComposition(var Msg: TMessage); override;
    procedure FocusKilled; override;
    property TextDrawer: TheTextDrawer read FTextDrawer write SetTextDrawer;
  end;

  { LazSynImeFull }

  LazSynImeFull = class(LazSynIme)
  private
    FImeBlockSelection, FImeBlockSelection2: TSynEditSelection;
    FImeMarkupSelection, FImeMarkupSelection2: TSynEditMarkupSelection;
    FInImeMsg: Boolean;
    procedure SetImeTempText(const s: string);
    procedure DoOnCommand(Sender: TObject; AfterProcessing: boolean; var Handled: boolean;
      var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer;
      HandlerData: pointer);
    procedure DoOnMouse(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure DoStatusChanged(Sender: TObject; Changes: TSynStatusChanges);
  public
    constructor Create(AOwner: TSynEditBase);
    destructor Destroy; override;
    procedure WMImeRequest(var Msg: TMessage); override;
    procedure WMImeNotify(var Msg: TMessage); override;
    procedure WMImeComposition(var Msg: TMessage); override;
    procedure WMImeStartComposition(var Msg: TMessage); override;
    procedure WMImeEndComposition(var Msg: TMessage); override;
    procedure FocusKilled; override;
  end;

implementation
uses SynEdit;

{ LazSynIme }

procedure LazSynIme.InvalidateLines(FirstLine, LastLine: integer);
begin
  FInvalidateLinesMethod(FirstLine, LastLine);
end;

procedure LazSynIme.StopIme(Success: Boolean);
var
  imc: HIMC;
begin
  if (not FInCompose) or (not FriendEdit.HandleAllocated) then exit;

  imc := ImmGetContext(FriendEdit.Handle);
  if (imc <> 0) then begin
    if Success then
      ImmNotifyIME(imc, NI_COMPOSITIONSTR, CPS_COMPLETE, 0)
    else
      ImmNotifyIME(imc, NI_COMPOSITIONSTR, CPS_CANCEL, 0);
    ImmReleaseContext(FriendEdit.Handle, imc);
  end;
end;

constructor LazSynIme.Create(AOwner: TSynEditBase);
begin
  FriendEdit := AOwner;
end;

procedure LazSynIme.WMImeRequest(var Msg: TMessage);
begin
end;

procedure LazSynIme.WMImeComposition(var Msg: TMessage);
begin
end;

procedure LazSynIme.WMImeNotify(var Msg: TMessage);
begin
end;

procedure LazSynIme.WMImeStartComposition(var Msg: TMessage);
begin
end;

procedure LazSynIme.WMImeEndComposition(var Msg: TMessage);
begin
end;

procedure LazSynIme.FocusKilled;
begin
end;

{ LazSynImeSimple }

procedure LazSynImeSimple.DoStatusChanged(Sender: TObject; Changes: TSynStatusChanges);
begin
  UpdateImeWinXY(TCustomSynEdit(FriendEdit).CaretXPix, TCustomSynEdit(FriendEdit).CaretYPix);
  if Changes * [scCaretX, scCaretY] <> [] then
    StopIme(False);
end;

procedure LazSynImeSimple.DoDrawerFontChanged(Sender: TObject);
var
  imc: HIMC;
begin
  if not FriendEdit.HandleAllocated then
    exit;
  imc := ImmGetContext(FriendEdit.Handle);
  if (imc <> 0) then begin
    UpdateImeWinFont(imc);
    UpdateImeWinXY(TCustomSynEdit(FriendEdit).CaretXPix, TCustomSynEdit(FriendEdit).CaretYPix, imc);
    ImmReleaseContext(FriendEdit.Handle, imc);
  end;
end;

procedure LazSynImeSimple.DoOnCommand(Sender: TObject; AfterProcessing: boolean;
  var Handled: boolean; var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer;
  HandlerData: pointer);
begin
  StopIme(True);
end;

procedure LazSynImeSimple.DoOnMouse(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  StopIme(True);
end;

procedure LazSynImeSimple.UpdateImeWinXY(aX, aY: Integer; aImc: HIMC; aForce: Boolean);
var
  cf: CompositionForm;
  imc: HIMC;
begin
  if not FriendEdit.HandleAllocated then exit;
  if (not aForce) and (aX = FImeWinX) and (aY = FImeWinY) then exit;
  FImeWinX := aX;
  FImeWinY := aY;

  cf.dwStyle := CFS_POINT;
  cf.ptCurrentPos := Point(aX, aY);
  if aImc = 0 then
    imc := ImmGetContext(FriendEdit.Handle)
  else
    imc := aImc;
  if (imc <> 0) then begin
    ImmSetCompositionWindow(imc, @cf);
    if (aImc = 0) then
      ImmReleaseContext(FriendEdit.Handle, imc);
  end;
end;

procedure LazSynImeSimple.SEtTextDrawer(AValue: TheTextDrawer);
begin
  if FTextDrawer = AValue then Exit;
  if FTextDrawer <> nil then
    FTextDrawer.UnRegisterOnFontChangeHandler(@DoDrawerFontChanged);
  FTextDrawer := AValue;
  if FTextDrawer <> nil then
    FTextDrawer.RegisterOnFontChangeHandler(@DoDrawerFontChanged);
end;

procedure LazSynImeSimple.UpdateImeWinFont(aImc: HIMC);
var
  imc: HIMC;
  logFont: TLogFont;
begin
  if not FriendEdit.HandleAllocated then exit;
  if aImc = 0 then
    imc := ImmGetContext(FriendEdit.Handle)
  else
    imc := aImc;
  if (imc <> 0) then begin
    GetObject(FriendEdit.Font.Handle, SizeOf(TLogFont), @logFont);
    ImmSetCompositionFontA(imc, @logFont);
    if (aImc = 0) then
      ImmReleaseContext(FriendEdit.Handle, imc);
  end;
end;

constructor LazSynImeSimple.Create(AOwner: TSynEditBase);
begin
  inherited Create(AOwner);
  FImeBlockSelection := TSynEditSelection.Create(ViewedTextBuffer, False);
  FImeBlockSelection.InvalidateLinesMethod := @InvalidateLines;

  TCustomSynEdit(FriendEdit).RegisterStatusChangedHandler(@DoStatusChanged, [scCaretX, scCaretY, scLeftChar, scTopLine, scModified]);
  TCustomSynEdit(FriendEdit).RegisterCommandHandler(@DoOnCommand, nil, [hcfInit]);
  TCustomSynEdit(FriendEdit).RegisterBeforeMouseDownHandler(@DoOnMouse);
end;

destructor LazSynImeSimple.Destroy;
begin
  TextDrawer := nil;
  FreeAndNil(FImeBlockSelection);
  TCustomSynEdit(FriendEdit).UnregisterBeforeMouseDownHandler(@DoOnMouse);
  TCustomSynEdit(FriendEdit).UnregisterCommandHandler(@DoOnCommand);
  TCustomSynEdit(FriendEdit).UnRegisterStatusChangedHandler(@DoStatusChanged);
  inherited Destroy;
end;

procedure LazSynImeSimple.WMImeNotify(var Msg: TMessage);
var
  imc: HIMC;
{$IFDEF WinIMEDebug}
  s: String;
{$ENDIF}
begin
  {$IFDEF WinIMEDebug}
  case msg.wParam of
    IMN_CLOSESTATUSWINDOW: s:= 'IMN_CLOSESTATUSWINDOW, ';
    IMN_OPENSTATUSWINDOW: s:= 'IMN_OPENSTATUSWINDOW, ';
    IMN_CHANGECANDIDATE: s:= 'IMN_CHANGECANDIDATE, ';
    IMN_CLOSECANDIDATE: s:= 'IMN_CLOSECANDIDATE, ';
    IMN_OPENCANDIDATE: s:= 'IMN_OPENCANDIDATE, ';
    IMN_SETCONVERSIONMODE: s:= 'IMN_SETCONVERSIONMODE, ';
    IMN_SETSENTENCEMODE: s:= 'IMN_SETSENTENCEMODE, ';
    IMN_SETOPENSTATUS: s:= 'IMN_SETOPENSTATUS, ';
    IMN_SETCANDIDATEPOS: s:= 'IMN_SETCANDIDATEPOS, ';
    IMN_SETCOMPOSITIONFONT: s:= 'IMN_SETCOMPOSITIONFONT, ';
    IMN_SETCOMPOSITIONWINDOW: s:= 'IMN_SETCOMPOSITIONWINDOW, ';
    IMN_SETSTATUSWINDOWPOS: s:= 'IMN_SETSTATUSWINDOWPOS, ';
    IMN_GUIDELINE: s:= 'IMN_GUIDELINE, ';
    IMN_PRIVATE: s:= 'IMN_PRIVATE, ';
  end;
  debugln(['TCustomSynEdit.WMImeNotify ',s,' ', dbgHex(Msg.lParam), ' ,  ', dbgHex(Msg.wParam)]);
  {$ENDIF}

  case Msg.WParam of
    IMN_SETOPENSTATUS: begin
      imc := ImmGetContext(FriendEdit.Handle);
      if (imc <> 0) then begin
        UpdateImeWinFont(imc);
        UpdateImeWinXY(TCustomSynEdit(FriendEdit).CaretXPix, TCustomSynEdit(FriendEdit).CaretYPix, imc, True);
        ImmReleaseContext(FriendEdit.Handle, imc);
      end;
    end;
    IMN_CLOSESTATUSWINDOW:
      StopIme(True);
  end;
end;

procedure LazSynImeSimple.WMImeComposition(var Msg: TMessage);
var
  imc: HIMC;
  ImeCount: LongWord;
  p: PChar;
begin
  if ((Msg.LParam and GCS_RESULTSTR) <> 0) then begin
    imc := ImmGetContext(FriendEdit.Handle);
    try
      if imc <> 0 then begin
        ImeCount := ImmGetCompositionStringW(imc, GCS_RESULTSTR, nil, 0);
        {$IFDEF WinIMEDebug}
        DebugLn(['--- GCS_RESULTSTR  ', dbgHex(ImeCount)]);
        {$ENDIF}
        if ImeCount > 0 then begin
          GetMem(p, ImeCount + 2);
          try
            TCustomSynEdit(FriendEdit).BeginUpdate;
            ImmGetCompositionStringW(imc, GCS_RESULTSTR, p, ImeCount + 2);
            p[ImeCount] := #0;
            p[ImeCount+1] := #0;
            FImeBlockSelection.SelText := UTF16ToUTF8(PWCHAR(p));
            FImeBlockSelection.StartLineBytePos := FImeBlockSelection.EndLineBytePos;
            CaretObj.LineBytePos := FImeBlockSelection.StartLineBytePos;
            Msg.Result := 1;
          finally
            FreeMem(p, ImeCount + 2);
            TCustomSynEdit(FriendEdit).EndUpdate;
          end;
        end;
      end;
    finally
      ImmReleaseContext(FriendEdit.Handle, imc);
    end;
  end;


end;

procedure LazSynImeSimple.WMImeStartComposition(var Msg: TMessage);
var
  imc: HIMC;
begin
  //debugln(['TCustomSynEdit.WMImeStartComposition ']);
  imc := ImmGetContext(FriendEdit.Handle);
  if (imc <> 0) then begin
    UpdateImeWinFont(imc);
    UpdateImeWinXY(TCustomSynEdit(FriendEdit).CaretXPix, TCustomSynEdit(FriendEdit).CaretYPix, imc, True);
    ImmReleaseContext(FriendEdit.Handle, imc);
  end;
  FInCompose := True;
  FImeBlockSelection.StartLineBytePos := CaretObj.LineBytePos;
end;

procedure LazSynImeSimple.WMImeEndComposition(var Msg: TMessage);
begin
  FInCompose := False;
end;

procedure LazSynImeSimple.FocusKilled;
begin
  StopIme(True);
end;

{ LazSynImeFull }

procedure LazSynImeFull.DoOnCommand(Sender: TObject; AfterProcessing: boolean;
  var Handled: boolean; var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer;
  HandlerData: pointer);
begin
  StopIme(True);
end;

procedure LazSynImeFull.DoOnMouse(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  StopIme(True);
end;

procedure LazSynImeFull.DoStatusChanged(Sender: TObject; Changes: TSynStatusChanges);
begin
  if FInImeMsg then exit;
  StopIme(True);
end;

procedure LazSynImeFull.SetImeTempText(const s: string);
var
  p1, p2: TPoint;
  f: Boolean;
begin
  p1 := FImeBlockSelection.FirstLineBytePos;

  f := FInImeMsg;
  FInImeMsg := True;
  ViewedTextBuffer.UndoList.Lock;
  ViewedTextBuffer.RedoList.Lock;
  FImeBlockSelection.SelText := s;
  ViewedTextBuffer.UndoList.Unlock;
  ViewedTextBuffer.RedoList.Unlock;
  FInImeMsg := f;

  p2 := FImeBlockSelection.FirstLineBytePos;
  FImeBlockSelection.StartLineBytePos := p1;
  FImeBlockSelection.EndLineBytePos := p2;
end;

constructor LazSynImeFull.Create(AOwner: TSynEditBase);
begin
  inherited Create(AOwner);

  FImeBlockSelection := TSynEditSelection.Create(ViewedTextBuffer, False);
  FImeBlockSelection.InvalidateLinesMethod := @InvalidateLines;
  FImeBlockSelection2 := TSynEditSelection.Create(ViewedTextBuffer, False);
  FImeBlockSelection2.InvalidateLinesMethod := @InvalidateLines;

  FImeMarkupSelection  := TSynEditMarkupSelection.Create(FriendEdit, FImeBlockSelection);
  FImeMarkupSelection2 := TSynEditMarkupSelection.Create(FriendEdit, FImeBlockSelection2);

  TSynEditMarkupManager(MarkupMgr).AddMarkUp(FImeMarkupSelection);
  TSynEditMarkupManager(MarkupMgr).AddMarkUp(FImeMarkupSelection2);

  FImeMarkupSelection.MarkupInfo.Clear;
  FImeMarkupSelection.MarkupInfo.FramePriority := 999;
  FImeMarkupSelection.MarkupInfo.FrameColor := clDefault;
  FImeMarkupSelection.MarkupInfo.FrameStyle := slsDotted;
  FImeMarkupSelection.MarkupInfo.FrameEdges := sfeAround;

  FImeMarkupSelection2.MarkupInfo.Clear;
  FImeMarkupSelection2.MarkupInfo.FramePriority := 999+1;
  FImeMarkupSelection2.MarkupInfo.FrameColor := clDefault;
  FImeMarkupSelection2.MarkupInfo.FrameStyle := slsSolid;
  FImeMarkupSelection2.MarkupInfo.FrameEdges := sfeBottom;

  TCustomSynEdit(FriendEdit).RegisterStatusChangedHandler(@DoStatusChanged, [scCaretX, scCaretY, scModified]);
  TCustomSynEdit(FriendEdit).RegisterCommandHandler(@DoOnCommand, nil, [hcfInit]);
  TCustomSynEdit(FriendEdit).RegisterBeforeMouseDownHandler(@DoOnMouse);

end;

destructor LazSynImeFull.Destroy;
begin
  TCustomSynEdit(FriendEdit).UnregisterBeforeMouseDownHandler(@DoOnMouse);
  TCustomSynEdit(FriendEdit).UnregisterCommandHandler(@DoOnCommand);
  TCustomSynEdit(FriendEdit).UnRegisterStatusChangedHandler(@DoStatusChanged);

  FreeAndNil(FImeBlockSelection);
  FreeAndNil(FImeBlockSelection2);
  inherited Destroy;
end;

procedure LazSynImeFull.WMImeRequest(var Msg: TMessage);
var
  {$IFDEF WinIMEDebug}
  s: String;
  {$ENDIF}
  cp: PIMECHARPOSITION;
  p1: TPoint;
  CWidth: TPhysicalCharWidths;
  i, x: integer;
begin
  {$IFDEF WinIMEDebug}
  case msg.wParam of
    IMR_COMPOSITIONWINDOW: s:= 'IMR_COMPOSITIONWINDOW, ';
    IMR_CANDIDATEWINDOW: s:= 'IMR_CANDIDATEWINDOW, ';
    IMR_COMPOSITIONFONT: s:= 'IMR_COMPOSITIONFONT, ';
    IMR_RECONVERTSTRING: s:= 'IMR_RECONVERTSTRING, ';
    IMR_CONFIRMRECONVERTSTRING: s:= 'IMR_CONFIRMRECONVERTSTRING, ';
    IMR_DOCUMENTFEED: s:= 'IMR_DOCUMENTFEED, ';
  end;
  if s <> '' then debugln(['TCustomSynEdit.WMImeRequest ', s,' ' , dbgHex(Msg.lParam)]);
  {$ENDIF}

  case msg.wParam of
    IMR_QUERYCHARPOSITION: begin
        cp := PIMECHARPOSITION(Msg.lParam);
        p1 := FImeBlockSelection.StartLineBytePos;

        CWidth := ViewedTextBuffer.GetPhysicalCharWidths(FImeBlockSelection.StartLinePos - 1);
        x := p1.x - 1;
        i := cp^.dwCharPos;
        while (i > 0) and (x < length(CWidth)) do begin
          inc(x);
          while (x < length(CWidth)) and ((CWidth[x] and PCWMask) = 0) do
            inc(x);
          dec(i);
        end;
        p1.x := x + i + 1;
        p1 := FriendEdit.ClientToScreen(TCustomSynEdit(FriendEdit).RowColumnToPixels(ViewedTextBuffer.LogicalToPhysicalPos(p1)));

        cp^.pt.y := p1.y;
        cp^.pt.x :=  p1.x;
        cp^.cLineHeight := TCustomSynEdit(FriendEdit).LineHeight;
        cp^.rcDocument.TopLeft := FriendEdit.ClientToScreen(FriendEdit.ClientRect.TopLeft);
        cp^.rcDocument.BottomRight := FriendEdit.ClientToScreen(FriendEdit.ClientRect.BottomRight);
        {$IFDEF WinIMEDebug}
        debugln(['--- TCustomSynEdit.WMImeRequest ** IMR_QUERYCHARPOSITION ', dbgs(cp^.dwCharPos), '  ', dbgs(x),'   ', dbgs(p1.x)]);
        {$ENDIF}
        Msg.Result := 1;
      end;
  end;
end;

procedure LazSynImeFull.WMImeNotify(var Msg: TMessage);
{$IFDEF WinIMEDebug}
var
  s: String;
{$ENDIF}
begin
  {$IFDEF WinIMEDebug}
  case msg.wParam of
    IMN_CLOSESTATUSWINDOW: s:= 'IMN_CLOSESTATUSWINDOW, ';
    IMN_OPENSTATUSWINDOW: s:= 'IMN_OPENSTATUSWINDOW, ';
    IMN_CHANGECANDIDATE: s:= 'IMN_CHANGECANDIDATE, ';
    IMN_CLOSECANDIDATE: s:= 'IMN_CLOSECANDIDATE, ';
    IMN_OPENCANDIDATE: s:= 'IMN_OPENCANDIDATE, ';
    IMN_SETCONVERSIONMODE: s:= 'IMN_SETCONVERSIONMODE, ';
    IMN_SETSENTENCEMODE: s:= 'IMN_SETSENTENCEMODE, ';
    IMN_SETOPENSTATUS: s:= 'IMN_SETOPENSTATUS, ';
    IMN_SETCANDIDATEPOS: s:= 'IMN_SETCANDIDATEPOS, ';
    IMN_SETCOMPOSITIONFONT: s:= 'IMN_SETCOMPOSITIONFONT, ';
    IMN_SETCOMPOSITIONWINDOW: s:= 'IMN_SETCOMPOSITIONWINDOW, ';
    IMN_SETSTATUSWINDOWPOS: s:= 'IMN_SETSTATUSWINDOWPOS, ';
    IMN_GUIDELINE: s:= 'IMN_GUIDELINE, ';
    IMN_PRIVATE: s:= 'IMN_PRIVATE, ';
  end;
  debugln(['TCustomSynEdit.WMImeNotify ',s,' ', dbgHex(Msg.lParam), ' ,  ', dbgHex(Msg.wParam)]);
  {$ENDIF}
end;

procedure LazSynImeFull.WMImeComposition(var Msg: TMessage);
var
  CWidth: TPhysicalCharWidths;

  function CharToByte(AStart, AChars: integer): integer;
  begin
    if length(CWidth) = 0 then
      CWidth := ViewedTextBuffer.GetPhysicalCharWidths(FImeBlockSelection.StartLinePos - 1);
    dec(AStart);
    Result := AStart;
    while (AChars > 0) and (Result < length(CWidth)) do begin
      inc(Result);
      while (Result < length(CWidth)) and ((CWidth[Result] and PCWMask) = 0) do
        inc(Result);
      dec(AChars);
    end;
    Result := Result - AStart + AChars;
  end;
var
  {$IFDEF WinIMEDebug}
  s: String;
  {$ENDIF}
  imc: HIMC;
  p: PChar;
  ImeCount: LongWord;
  x, i: Integer;
  xy: TPoint;
begin
  {$IFDEF WinIMEDebug}
  s := '';
  if (Msg.lparam and GCS_COMPREADSTR)<>0 then s := s + 'GCS_COMPREADSTR, ';
  if (Msg.lparam and GCS_COMPREADATTR)<>0 then s := s + 'GCS_COMPREADATTR, ';
  if (Msg.lparam and GCS_COMPREADCLAUSE)<>0 then s := s + 'GCS_COMPREADCLAUSE, ';
  //if (Msg.lparam and GCS_COMPSTR)<>0 then s := s + 'GCS_COMPSTR, ';
  if (Msg.lparam and GCS_COMPATTR)<>0 then s := s + 'GCS_COMPATTR, ';
  if (Msg.lparam and GCS_COMPCLAUSE)<>0 then s := s + 'GCS_COMPCLAUSE, ';
  //if (Msg.lparam and GCS_CURSORPOS)<>0 then s := s + 'GCS_CURSORPOS, ';
  //if (Msg.lparam and GCS_DELTASTART)<>0 then s := s + 'GCS_DELTASTART, ';
  if (Msg.lparam and GCS_RESULTREADSTR)<>0 then s := s + 'GCS_RESULTREADSTR, ';
  if (Msg.lparam and GCS_RESULTREADCLAUSE)<>0 then s := s + 'GCS_RESULTREADCLAUSE, ';
  //if (Msg.lparam and GCS_RESULTSTR)<>0 then s := s + 'GCS_RESULTSTR, ';
  if (Msg.lparam and GCS_RESULTCLAUSE)<>0 then s := s + 'GCS_RESULTCLAUSE, ';
  if (Msg.lparam and CS_INSERTCHAR)<>0 then s := s + ' ** CS_INSERTCHAR, ';
  if (Msg.lparam and CS_NOMOVECARET)<>0 then s := s + ' ** CS_NOMOVECARET, ';
  if s <> '' then debugln(['TCustomSynEdit.WMImeComposition ', s]);
  {$ENDIF}

  if ((Msg.LParam and (GCS_RESULTSTR or GCS_COMPSTR or GCS_CURSORPOS or GCS_COMPATTR)) = 0) then
    exit;

  imc := 0;
  FInImeMsg := True;
  SetLength(CWidth, 0);
  try

    if ((Msg.LParam and GCS_RESULTSTR) <> 0) then begin
      if imc = 0 then
        imc := ImmGetContext(FriendEdit.Handle);
      ImeCount := ImmGetCompositionStringW(imc, GCS_RESULTSTR, nil, 0);
      {$IFDEF WinIMEDebug}
      DebugLn(['--- GCS_RESULTSTR  ', dbgHex(ImeCount)]);
      {$ENDIF}
      if ImeCount > 0 then begin
        GetMem(p, ImeCount + 2);
        try
          CaretObj.LineBytePos := FImeBlockSelection.StartLineBytePos;
          TCustomSynEdit(FriendEdit).BeginUpdate;
          ImmGetCompositionStringW(imc, GCS_RESULTSTR, p, ImeCount + 2);
          p[ImeCount] := #0;
          p[ImeCount+1] := #0;
          SetImeTempText('');
          FImeBlockSelection.SelText := UTF16ToUTF8(PWCHAR(p));
          FImeBlockSelection.StartLineBytePos := FImeBlockSelection.EndLineBytePos;
          CaretObj.LineBytePos := FImeBlockSelection.StartLineBytePos;
          Msg.Result := 1;
        finally
          TCustomSynEdit(FriendEdit).EndUpdate;
          FreeMem(p, ImeCount + 2);
        end;
      end;
    end;

    if ((Msg.LParam and GCS_COMPSTR) <> 0) then begin
      if imc = 0 then
        imc := ImmGetContext(FriendEdit.Handle);
      ImeCount := ImmGetCompositionStringW(imc, GCS_COMPSTR, nil, 0);
      {$IFDEF WinIMEDebug}
      DebugLn(['--- GCS_COMPSTR  ', dbgHex(ImeCount)]);
      {$ENDIF}
      if ImeCount > 0 then begin
        GetMem(p, ImeCount + 2);
        try
          ImmGetCompositionStringW(imc, GCS_COMPSTR, p, ImeCount + 2);
          p[ImeCount] := #0;
          p[ImeCount+1] := #0;
          SetImeTempText(UTF16ToUTF8(PWCHAR(p)));
          Msg.Result := 1;
        finally
          FreeMem(p, ImeCount + 2);
        end;
      end;
    end;

    if ((Msg.LParam and GCS_COMPATTR) <> 0) then begin
      if imc = 0 then
        imc := ImmGetContext(FriendEdit.Handle);
      ImeCount := ImmGetCompositionStringW(imc, GCS_COMPATTR, nil, 0);
      {$IFDEF WinIMEDebug}
      DebugLn(['***** GCS_COMPATTR  ', dbgHex(ImeCount)]);
      {$ENDIF}
      if ImeCount > 0 then begin
        xy := FImeBlockSelection.StartLineBytePos;
        FImeBlockSelection2.StartLineBytePos := xy;
        FImeBlockSelection2.EndLineBytePos := xy;
        GetMem(p, ImeCount + 2);
        try
          ImmGetCompositionStringW(imc, GCS_COMPATTR, p, ImeCount + 2);
          //DebugLn(dbgMemRange(PByte( p), ImeCount));
          i := 0;
          while {%H-}i < ImeCount do begin
            if ord(p[i]) = ATTR_TARGET_CONVERTED then begin
              x := FImeBlockSelection.StartBytePos;
              xy.x := x + CharToByte(x, i);
              FImeBlockSelection2.StartLineBytePos := xy;
              inc(i);
              while {%H-}i < ImeCount do begin
                if (ord(p[i]) <> ATTR_TARGET_CONVERTED) or (i = ImeCount-1) then begin
                  if (ord(p[i]) = ATTR_TARGET_CONVERTED) then
                    inc(i);
                  xy.x := x + CharToByte(x, i);
                  FImeBlockSelection2.EndLineBytePos := xy;
                  break;
                end;
                inc(i);
              end;
              break;
            end;
            inc(i);
          end;

          Msg.Result := 1;
        finally
          FreeMem(p, ImeCount + 2);
        end;
      end;
    end;

    if ((Msg.LParam and GCS_CURSORPOS) <> 0) then begin
      if imc = 0 then
        imc := ImmGetContext(FriendEdit.Handle);

      ImeCount := ImmGetCompositionStringW(imc, GCS_CURSORPOS, nil, 0);
      {$IFDEF WinIMEDebug}
      DebugLn(['--- GCS_CURSORPOS ', dbgs(ImeCount)]);
      {$ENDIF}
      if ImeCount >= 0 then begin
        ImeCount := ImeCount and $ffff;
        x := FImeBlockSelection.StartBytePos;
        x := x + CharToByte(x, ImeCount);
        CaretObj.CharPos := ViewedTextBuffer.LogicalToPhysicalPos(Point(x, FImeBlockSelection.StartLinePos)).x;
      end;
    end;

  finally
    if imc <> 0 then
      ImmReleaseContext(FriendEdit.Handle, imc);
    FInImeMsg := False;
  end;
  inherited;
end;

procedure LazSynImeFull.WMImeStartComposition(var Msg: TMessage);
begin
  //debugln(['TCustomSynEdit.WMImeStartComposition ']);
  FImeBlockSelection.StartLineBytePos := CaretObj.LineBytePos;
  FInCompose := True;
  Msg.Result := 1;
end;

procedure LazSynImeFull.WMImeEndComposition(var Msg: TMessage);
begin
  //debugln(['TCustomSynEdit.WMImeEndComposition ']);
  SetImeTempText('');
  CaretObj.LineBytePos := FImeBlockSelection.LastLineBytePos;
  FImeBlockSelection.StartLineBytePos := CaretObj.LineBytePos;
  FImeBlockSelection2.StartLineBytePos := CaretObj.LineBytePos;
  FInCompose := False;
  Msg.Result := 1;
end;

procedure LazSynImeFull.FocusKilled;
begin
  StopIme(True);
end;

end.

