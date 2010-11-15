unit AssemblerDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, Grids, ExtCtrls, LclType, LCLIntf, DebuggerDlg, Debugger,
  EditorOptions, Maps, Math, LCLProc, Menus, Clipbrd;

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
    Dump: String;
    Statement: String;
    FileName: String;
    SourceLine: Integer;
  end;
  TAsmDlgLineEntries = Array of TAsmDlgLineEntry;

  TAssemblerDlg = class(TDebuggerDlg)
    CopyToClipboard: TMenuItem;
    pbAsm: TPaintBox;
    PopupMenu1: TPopupMenu;
    sbHorizontal: TScrollBar;
    sbVertical: TScrollBar;
    procedure CopyToClipboardClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure pbAsmMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pbAsmMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure pbAsmMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure pbAsmMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure pbAsmPaint(Sender: TObject);
    procedure sbHorizontalChange(Sender: TObject);
    procedure sbVerticalChange(Sender: TObject);
    procedure sbVerticalScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
  private
    FDebugger: TDebugger;
    FDisassembler: TIDEDisassembler;
    FDisassemblerNotification: TIDEDisassemblerNotification;
    FLocation: TDBGPtr;
    FMouseIsDown: Boolean;

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
    FUpdateNeeded: Boolean;
    procedure DoDebuggerDestroyed(Sender: TObject);
    procedure ClearLineMap(AState: TAsmDlgLineMapState = lmsUnknown);
    procedure DisassemblerChanged(Sender: TObject);
    procedure SetDisassembler(const AValue: TIDEDisassembler);
    procedure SetDebugger(const AValue: TDebugger);
    function FormatLine(ALine: TAsmDlgLineEntry; W: Integer): String;
    procedure UpdateLineData;
    procedure UpdateLineDataEx(ALineMap: TAsmDlgLineEntries;
                               AFirstLine, ALineCount: Integer;
                               var ACachedLine, ACachedIdx: Integer;
                               var ACachedIsSrc, ACachedValid: Boolean;
                               ACachedUpdate: Boolean
                              );
    procedure SetSelection(ALine: Integer; AMakeVisible: Boolean; AKeepSelEnd: Boolean = False);
    procedure SetLineCount(ALineCount: Integer);
    procedure SetTopLine(ALine: Integer);
  protected
    procedure InitializeWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetLocation(ADebugger: TDebugger; const AAddr: TDBGPtr);
    property Disassembler: TIDEDisassembler read FDisassembler write SetDisassembler;
  end;

implementation

{$R *.lfm}

uses
  LazarusIDEStrConsts;

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
    if AState = lmsUnknown
    then FLineMap[n].Addr := 0;
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
end;

procedure TAssemblerDlg.SetDebugger(const AValue: TDebugger);
begin
  if FDebugger = AValue
  then exit;

  if FDebugger <> nil
  then FDebugger.RemoveNotifyEvent(dnrDestroy, @DoDebuggerDestroyed);
  FDebugger := AValue;
  if FDebugger <> nil
  then FDebugger.AddNotifyEvent(dnrDestroy, @DoDebuggerDestroyed);
end;

constructor TAssemblerDlg.Create(AOwner: TComponent);
begin
  FGutterWidth := 32;
  FDisassemblerNotification := TIDEDisassemblerNotification.Create;
  FDisassemblerNotification.AddReference;
  FDisassemblerNotification.OnChange  := @DisassemblerChanged;

  inherited Create(AOwner);
//  DoubleBuffered := True;

  pbAsm.Font.Height := EditorOpts.EditorFontHeight;
  pbAsm.Font.Name := EditorOpts.EditorFont;
  Caption := lisMenuViewAssembler;
  CopyToClipboard.Caption := lisDbgAsmCopyToClipboard;
end;

destructor TAssemblerDlg.Destroy;
begin
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
  if Shift - [ssShift] <> [] then Exit;
  case Key of
    VK_UP:   begin
      SetSelection(FSelectLine - 1, True, ssShift in Shift);
    end;
    VK_DOWN: begin
      SetSelection(FSelectLine + 1, True, ssShift in Shift);
    end;
    VK_PRIOR: begin
      i := FTopLine;
      SetSelection(FSelectLine - FLineCount, False, ssShift in Shift);
      SetTopline(i - FLineCount);
    end;
    VK_NEXT: begin
      i := FTopLine;
      SetSelection(FSelectLine + FLineCount, False, ssShift in Shift);
      SetTopline(i + FLineCount);
    end;
    VK_LEFT: sbHorizontal.Position := sbHorizontal.Position - sbHorizontal.SmallChange;
    VK_RIGHT: sbHorizontal.Position := sbHorizontal.Position + sbHorizontal.SmallChange;
    VK_HOME: sbHorizontal.Position := 0;
  end;
  Key := 0;
  pbAsm.Invalidate;
end;

procedure TAssemblerDlg.CopyToClipboardClick(Sender: TObject);
var
  ALineMap: TAsmDlgLineEntries;
  i, w: Integer;
  s: String;
begin
  if FSelectionEndLine = FSelectLine
  then exit;
  SetLength(ALineMap, abs(FSelectionEndLine - FSelectLine)+1);
  UpdateLineDataEx(ALineMap, Min(FSelectionEndLine, FSelectLine),
    abs(FSelectionEndLine - FSelectLine),
    FLastTopLine, FLastTopLineIdx, FLastTopLineIsSrc, FLastTopLineValid, False );
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

procedure TAssemblerDlg.DisassemblerChanged(Sender: TObject);
begin
  if (FDisassembler = nil) or (FLocation = 0) or (FLineCount = 0)
  then exit;
  if (FDebugger <> nil) and (FDebugger.State <> dsPause)
  then begin
    // only for F9, not for F8,F7 single stepping with assembler is no good, if it clears all the time
    //ClearLineMap;
  end
  else begin
    // Check if anything is there, update BaseAddr
    if FDisassembler <> nil
    then FDisassembler.PrepareRange(FLocation, Max(0, -(FTopLine - 5)), Max(0, FTopLine + FLineCount + 1 + 5));
    UpdateLineData;
  end;
  pbAsm.Invalidate;
end;

procedure TAssemblerDlg.FormResize(Sender: TObject);
var
  R: TRect;
begin
  R := ClientRect;
  Dec(R.Right, sbVertical.Width);
  Dec(R.Bottom, sbHorizontal.Height);
  sbVertical.Left := R.Right;
  sbVertical.Height := R.Bottom;

  sbHorizontal.Top := R.Bottom;
  sbHorizontal.Width := R.Right;
  sbHorizontal.PageSize := R.Right;
  sbHorizontal.LargeChange := R.Right div 3;

  if FLineHeight <> 0
  then SetLineCount(R.Bottom div FLineHeight);

  pbAsm.SetBounds(0, 0, R.Right, R.Bottom);
end;

procedure TAssemblerDlg.InitializeWnd;
var
  TM: TTextMetric;
begin
  inherited InitializeWnd;

  GetTextMetrics(pbAsm.Canvas.Handle, TM);
  FCharWidth := EditorOpts.ExtraCharSpacing + TM.tmMaxCharWidth;
  sbHorizontal.SmallChange := FCHarWidth;

  FLineHeight := EditorOpts.ExtraLineSpacing + TM.tmHeight;
  SetLineCount(pbAsm.Height div FLineHeight);
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
  Handled := True;

  j := WheelDelta div 120;
  i := FTopLine ;
  SetSelection(FSelectLine - j, False, ssShift in Shift);
  SetTopline(i - j);
end;

procedure TAssemblerDlg.pbAsmPaint(Sender: TObject);
var
  R: TRect;
  n, X, Y, Line, W: Integer;
  S: String;
begin

  R := pbAsm.ClientRect;
//  pbAsm.Canvas.Brush.Color := clWindow;
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
      else Result :=  Format('%s:%u %s', [ALine.FileName, ALine.SourceLine, ALine.Statement]);
    end;
    lmsFuncName: Result:= ALine.FileName + ' ' + ALine.Statement;
  end;
end;

procedure TAssemblerDlg.sbHorizontalChange(Sender: TObject);
begin
  pbAsm.Invalidate;
end;

procedure TAssemblerDlg.sbVerticalChange(Sender: TObject);
begin
  sbVertical.Position := 475;
  pbAsm.Invalidate;
end;

procedure TAssemblerDlg.sbVerticalScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
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
      // --- remove when scPosition works
      ScrollPos := 475;
      Exit;
      // ---

      if ScrollPos = 475 then Exit;
      if ScrollPos < 475
      then SetTopline(FTopLine - 1)
      else SetTopline(FTopLine + 1);
    end;
//    scTop,      // = SB_TOP
//    scBottom,   // = SB_BOTTOM
//    scEndScroll // = SB_ENDSCROLL
  end;
end;

procedure TAssemblerDlg.DoDebuggerDestroyed(Sender: TObject);
begin
  FDebugger := nil;
  FDisassembler := nil;
  UpdateLineData;
  pbAsm.Invalidate;
end;

procedure TAssemblerDlg.SetLocation(ADebugger: TDebugger; const AAddr: TDBGPtr);
begin
  SetDebugger(ADebugger);
  FTopLine := -(FLineCount div 2);
  FSelectLine := 0;
  FSelectionEndLine := 0;
  FLocation := AAddr;
  FLastTopLineValid := False;

  if Visible then begin
    // otherwhise in resize
    if FDisassembler <> nil
    then FDisassembler.PrepareRange(FLocation, Max(0, -(FTopLine - 5)), Max(0, FTopLine + FLineCount + 1 + 5));
    UpdateLineData;
    pbAsm.Invalidate;
  end
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
  if FLocation <> 0
  then begin
    if FDisassembler <> nil
    then FDisassembler.PrepareRange(FLocation, Max(0, -(FTopLine - 5)), Max(0, FTopLine + FLineCount + 1 + 5));
    UpdateLineData;
  end;
  pbAsm.Invalidate;
end;

procedure TAssemblerDlg.SetTopLine(ALine: Integer);
var
  PadFront, PadEnd: Integer;
begin
  if FTopLine = ALine then Exit;
  // scrolled by user, get more padding lines
  PadFront := 5;
  PadEnd := 5;
  if ALine < FTopLine
  then PadFront := 25
  else PadEnd := 25;
  FTopLine := ALine;
  if (FDisassembler <> nil)
  and ( (FDisassembler.CountBefore < Max(0, -(FTopLine - PadFront)))
     or (FDisassembler.CountAfter < Max(0, FTopLine + FLineCount + 1 + PadEnd)) )
  then FDisassembler.PrepareRange(FLocation, Max(0, -(FTopLine - PadFront)), Max(0, FTopLine + FLineCount + 1 + PadEnd));
  UpdateLineData;
end;


procedure TAssemblerDlg.UpdateLineData;
begin
  UpdateLineDataEx(FLineMap, FTopLine, FLineCount,
    FLastTopLine, FLastTopLineIdx, FLastTopLineIsSrc, FLastTopLineValid, True);
end;

procedure TAssemblerDlg.UpdateLineDataEx(ALineMap: TAsmDlgLineEntries; AFirstLine,
  ALineCount: Integer; var ACachedLine, ACachedIdx: Integer;
  var ACachedIsSrc, ACachedValid: Boolean; ACachedUpdate: Boolean);

  function GetItem(AIdx: Integer): PDisassemblerEntry;
  begin
    Result := nil;
    if (AIdx >= -FDisassembler.CountBefore) and (AIdx < FDisassembler.CountAfter)
    then Result := FDisassembler.EntriesPtr[AIdx];
  end;

  function IsSourceBeforeItem(AItm: PDisassemblerEntry;
    APrvItm: PDisassemblerEntry = nil): Boolean;
  begin
    Result := (AItm <> nil)
    and (   ( (AItm^.SrcFileName <> '') and (AItm^.SrcStatementIndex = 0) )
         or (    (AItm^.FuncName <> '')
             and (   (AItm^.Offset = 0)
                  or ( (APrvItm <> nil) and (AItm^.FuncName <> APrvItm^.FuncName) )
                 )
            )
        );
  end;

  function IsSourceBeforeItem(AIdx: Integer): Boolean;
  begin
    Result := IsSourceBeforeItem(GetItem(AIdx));
  end;

var
  DoneLocation: TDBGPtr;
  DoneTopLine, DoneLineCount: Integer;
  DoneCountBefore, DoneCountAfter: Integer;
  Line, Idx: Integer;
  Itm, NextItm: PDisassemblerEntry;
  LineIsSrc, HasLineOutOfRange: Boolean;
begin
  if (FDebugger = nil) or (FDisassembler = nil) or (FDebugger.State <> dsPause)
  then begin
    ClearLineMap;  // set all to lmsUnknown;
    exit;
  end;
  if FDisassembler.BaseAddr <> FLocation
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
    DoneLocation    := FLocation;
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

    while AFirstLine > Line
    do begin
      if LineIsSrc
      then begin
        LineIsSrc := False;
      end
      else if IsSourceBeforeItem(Idx+1)
      then begin
        inc(Idx);
        LineIsSrc := True;
      end
      else begin
        inc(Idx);
      end;
      inc(Line);
    end;
    while AFirstLine < line
    do begin
      if LineIsSrc
      then begin
        dec(Idx);
        LineIsSrc := False;
      end
      else if IsSourceBeforeItem(Idx)
      then begin
        LineIsSrc := True;
      end
      else begin
        dec(Idx);
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
    NextItm := GetItem(Idx);
    while Line <= ALineCount do begin
      Itm := NextItm;
      NextItm := GetItem(Idx+1);

      if Itm = nil
      then begin
        ALineMap[Line].State := lmsInvalid;
        HasLineOutOfRange := True;
        inc(Line);
        inc(idx);
        continue;
      end;

      if ( (Line = 0) and LineIsSrc )
      or ( (Line <> 0) and IsSourceBeforeItem(Itm) )
      then begin
        ALineMap[Line].Dump       := '';
        ALineMap[Line].Statement  := '';
        if Itm^.SrcFileName <> ''
        then begin
          ALineMap[Line].State := lmsSource;
          ALineMap[Line].SourceLine := Itm^.SrcFileLine;
          ALineMap[Line].FileName   := Itm^.SrcFileName;
        end
        else begin
          ALineMap[Line].State := lmsFuncName;
          ALineMap[Line].SourceLine := Itm^.Offset;
          ALineMap[Line].FileName   := Itm^.FuncName;
        end;
        inc(Line);
      end
      else
      if (Line = 0) // but it's not LineIsSrc
      and ( ( (Itm^.SrcFileName <> '') and (Itm^.SrcStatementIndex <> Itm^.SrcStatementCount-1) )
         or ( (Itm^.SrcFileName = '') and (Itm^.FuncName <> '') and (NextItm <> nil) and (Itm^.Offset < NextItm^.Offset) )
      )
      then begin
        ALineMap[Line].Dump       := '';
        ALineMap[Line].Statement  := '';
        if Itm^.SrcFileName <> ''
        then begin
          ALineMap[Line].State := lmsSource;
          ALineMap[Line].SourceLine := Itm^.SrcFileLine;
          ALineMap[Line].FileName   := Itm^.SrcFileName;
          if NextItm <> nil
          then ALineMap[Line].Statement  := Format('(%d of %d)', [NextItm^.SrcStatementIndex, NextItm^.SrcStatementCount])
          else ALineMap[Line].Statement  := Format('(??? of %d)', [Itm^.SrcStatementCount]);
        end
        else begin
          ALineMap[Line].State := lmsFuncName;
          ALineMap[Line].SourceLine := 0;
          if NextItm <> nil
          then ALineMap[Line].SourceLine := NextItm^.Offset;
          ALineMap[Line].FileName   := Itm^.FuncName;
          if NextItm <> nil
          then ALineMap[Line].Statement  := Format('(%d)', [NextItm^.Offset])
          else ALineMap[Line].Statement  := '(???)';
        end;
        inc(Line);
        inc(idx); // displayed source-info, instead of asm (topline substituted)
        LineIsSrc := False;
        continue;
      end;
      LineIsSrc := False; // only for topline

      if Line > ALineCount
      then break;

      ALineMap[Line].Addr       := Itm^.Addr;
      ALineMap[Line].State      := lmsStatement;
      ALineMap[Line].Dump       := Itm^.Dump;
      ALineMap[Line].Statement  := Itm^.Statement;
      ALineMap[Line].SourceLine := Itm^.SrcFileLine;

      inc(Line);
      inc(idx);
    end;

  finally
    FUpdating := False;
    if FUpdateNeeded
    and ( (DoneLocation    <> FLocation)
       or (DoneTopLine     <> AFirstLine)
       or (DoneLineCount   <> ALineCount)
       or (HasLineOutOfRange
          and ( (DoneCountBefore <> FDisassembler.CountBefore)
             or (DoneCountAfter  <> FDisassembler.CountAfter) )  )
    )
    then UpdateLineData;
  end;
end;

end.

