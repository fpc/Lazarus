unit AssemblerDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, Grids, ExtCtrls, LclType, LCLIntf, DebuggerDlg, Debugger,
  EditorOptions, Maps, Math;

type

  { TAssemblerDlg }

  TAsmDlgLineMapState = (
    lmsUnknown,
    lmsInvalid,    // debugger couldn't disassemble this address
    lmsStatement,  // display line as assembler
    lmsSource      // display line as source
  );

  TAssemblerDlg = class(TDebuggerDlg)
    pbAsm: TPaintBox;
    sbHorizontal: TScrollBar;
    sbVertical: TScrollBar;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure pbAsmMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pbAsmMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure pbAsmPaint(Sender: TObject);
    procedure sbHorizontalChange(Sender: TObject);
    procedure sbVerticalChange(Sender: TObject);
    procedure sbVerticalScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
  private
    FDebugger: TDebugger;
    FTopLine: Integer;
    FSelectLine: Integer;
    FLineCount: Integer;
    FLineHeight: Integer;
    FLineMap: array[Byte] of record // cyclic buffer
      State: TAsmDlgLineMapState;
      Line: Integer;
      Addr: TDbgPtr;
      Dump: String;
      Statement: String;
      FileName: String;
      SourceLine: Integer;
    end;
    FLineMapMin: Byte;
    FLineMapMax: Byte;
    FLineMapValid: Boolean;
    FCharWidth: Integer;
    FGutterWidth: Integer;
    FUpdating: Boolean;
    FUpdateNeeded: Boolean;
    procedure ClearLineMap;
    procedure UpdateLineData(ALine: Integer);
    procedure SetSelection(ALine: Integer; AMakeVisible: Boolean);
    procedure SetTopLine(ALine: Integer);
  protected
    procedure InitializeWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetLocation(ADebugger: TDebugger; const AAddr: TDBGPtr);
  end;

implementation
uses
  LazarusIDEStrConsts;

{ TAssemblerDlg }

procedure TAssemblerDlg.ClearLineMap;
var
  n: Integer;
begin
  FLineMapMin := 0;
  FLineMapMax := 0;
  FLineMapValid := False;
  for n := Low(FLineMap) to High(FLineMap) do
  begin
    FLineMap[n].Line := n;
    FLineMap[n].State := lmsUnknown;
  end;
end;

constructor TAssemblerDlg.Create(AOwner: TComponent);
begin
  FGutterWidth := 32;

  inherited Create(AOwner);
//  DoubleBuffered := True;

  pbAsm.Font.Height := EditorOpts.EditorFontHeight;
  pbAsm.Font.Name := EditorOpts.EditorFont;
  Caption := lisMenuViewAssembler;
end;

destructor TAssemblerDlg.Destroy;
begin
  inherited Destroy;
end;

procedure TAssemblerDlg.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Shift <> [] then Exit;
  case Key of
    VK_UP:   begin
      SetSelection(FSelectLine - 1, True);
      Exit;
    end;
    VK_DOWN: begin
      SetSelection(FSelectLine + 1, True);
      Exit;
    end;
    VK_PRIOR: begin
      Dec(FSelectLine, FLineCount);
      SetTopline(FTopLine - FLineCount);
    end;
    VK_NEXT: begin
      Inc(FSelectLine, FLineCount);
      SetTopline(FTopLine + FLineCount);
    end;
    VK_LEFT: sbHorizontal.Position := sbHorizontal.Position - sbHorizontal.SmallChange;
    VK_RIGHT: sbHorizontal.Position := sbHorizontal.Position + sbHorizontal.SmallChange;
    VK_HOME: sbHorizontal.Position := 0;
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
  then begin
    FLineCount := R.Bottom div FLineHeight;
    UpdateLineData(FTopLine + FLineCount);
  end;

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
  FLineCount := pbAsm.Height div FLineHeight;
  UpdateLineData(FTopLine + FLineCount);
end;

procedure TAssemblerDlg.pbAsmMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button <> mbLeft then exit;

  SetSelection(FTopLine + Y div FLineHeight, True);
end;

procedure TAssemblerDlg.pbAsmMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  Handled := True;

  SetSelection(FSelectLine - (WheelDelta div 120), True);
end;

procedure TAssemblerDlg.pbAsmPaint(Sender: TObject);
var
  R: TRect;
  n, X, Y, Line, idx, W: Integer;
  S: String;
begin

  R := pbAsm.ClientRect;
//  pbAsm.Canvas.Brush.Color := clWindow;
  pbAsm.Canvas.FillRect(R);

  Inc(R.Left, FGutterWidth);

  X := FGutterWidth - sbHorizontal.Position;
  Y := 0;
  Line := FTopLine;
  idx := Cardinal(Line) mod Length(FLineMap);

  for n := 0 to FLineCount do
  begin
    if Line = FSelectLine
    then begin
      pbAsm.Canvas.Brush.Color := clHighlight;
      pbAsm.Canvas.Font.Color := clHighlightText;
      pbAsm.Canvas.FillRect(R.Left, n * FLineHeight, R.Right, (n + 1) * FLineHeight);
    end
    else begin
      pbAsm.Canvas.Brush.Color := pbAsm.Color;
      pbAsm.Canvas.Font.Color := pbAsm.Font.Color;
    end;
    pbAsm.Canvas.Font.Bold := (FLineMap[idx].State = lmsSource);

    S := '';
    //S :=  Format('[a:%8.8u l:%8.8d i:%3.3u] ', [Cardinal(FLineMap[idx].Addr), Line, idx]);
    if FDebugger = nil
    then W := 16
    else W := FDebugger.TargetWidth div 4;

    S := S + HexStr(FLineMap[idx].Addr, W) + ' ';

    if FLineMap[idx].Line = Line
    then begin
      case FLineMap[idx].State of
        lmsUnknown: S := S + '??????';
        lmsInvalid: S := S + '......';
        lmsStatement: S := S + Copy(FLineMap[idx].Dump + '                         ', 1, 25) + FLineMap[idx].Statement;
        lmsSource: begin
          if FLineMap[idx].SourceLine = 0
          then S := '---'
          else S :=  Format('%s:%u', [FLineMap[idx].FileName, FLineMap[idx].SourceLine]);
        end;
      end;
    end;
    pbAsm.Canvas.TextRect(R, X, Y, S);
    Inc(Y, FLineHeight);
    Inc(Line);
    if idx < High(FLineMap)
    then Inc(Idx)
    else Idx := 0;
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

procedure TAssemblerDlg.SetLocation(ADebugger: TDebugger; const AAddr: TDBGPtr);
begin
  FDebugger := ADebugger;
  ClearLineMap;
  FLineMapMin := 0;
  FLineMapMax := 0;
  FLineMap[0].Addr := AAddr;
  FLineMap[0].Line := 0;
  FLineMapValid := True;
  FTopLine := 0;
  FSelectLine := FTopLine;
  UpdateLineData(0); // start
  UpdateLineData(FLineCount); // rest

  pbAsm.Invalidate;
end;

procedure TAssemblerDlg.SetSelection(ALine: Integer; AMakeVisible: Boolean);
var
  OldLine: Integer;
begin
  if Aline = FSelectLine then Exit;

  // UpdateLineData may cause eventhandling, so we enter here again
  // set variable first
  OldLine := FSelectLine;
  FSelectLine := Aline;

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

procedure TAssemblerDlg.SetTopLine(ALine: Integer);
var
  OldTop: Integer;
begin
  if FTopLine = ALine then Exit;

  if not FLineMapValid
  then begin
    FTopLine := ALine;
    pbAsm.Invalidate;
    Exit;
  end;

  // UpdateLineData may cause eventhandling, so we enter here again
  // set variable first
  OldTop := FTopLine;
  FTopLine := ALine;

  if FTopLine < OldTop
  then begin
    // before
    UpdateLineData(FTopLine);
  end
  else begin
    // after
    UpdateLineData(FTopLine + FLineCount);
  end;

  pbAsm.Invalidate;
end;


procedure TAssemblerDlg.UpdateLineData(ALine: Integer);
  procedure DecLineMapMin;
  begin
    if FLineMapMin = 0
    then FLineMapMin := High(FLineMap)
    else Dec(FLineMapMin);
    // check if we overlap with our end
    if FLineMapMin = FLineMapMax
    then begin
      if FLineMapMax = 0
      then FLineMapMax := High(FLineMap)
      else Dec(FLineMapMax);
    end;
  end;

  procedure IncLineMapMax;
  begin
    if FLineMapMax = High(FLineMap)
    then FLineMapMax := 0
    else Inc(FLineMapMax);
    // check if we overlap with our start
    if FLineMapMin = FLineMapMax
    then begin
      if FLineMapMin = High(FLineMap)
      then FLineMapMin := 0
      else Inc(FLineMapMin);
    end;
  end;

var
  Addr, NextAddr: TDbgPtr;
  OK, SameFile, SameSource: Boolean;
  Line, Line2, SourceLine: Integer;
  Idx, OldIdx: Byte;
  Dump, Statement, FileName: String;

begin
  if FDebugger = nil then Exit;
  if not FLineMapValid then Exit;

  // while accessing external debugger, events are handled, so we could enter here again
  if FUpdating
  then begin
    FUpdateNeeded := True;
    Exit;
  end;
  FUpdating := True;
  try
    while FLineMap[FLineMapMin].Line > ALine do
    begin
      // get lines before min
      Addr := FLineMap[FLineMapMin].Addr;
      Line := FLineMap[FLineMapMin].Line - 1;
      Line2 := Line;
      OldIdx := FLineMapMin;
      DecLineMapMin;

      FLineMap[FLineMapMin].State := lmsUnknown;
      FLineMap[FLineMapMin].Line := Line;
      OK := FDebugger.Disassemble(Addr, True, NextAddr, Dump, Statement, FileName, SourceLine);
      if OK
      then begin
        SameFile := FileName = FLineMap[OldIdx].FileName;
        // to reduce the amount of unique duplicated strings,
        // use "our" copy of the filename and not the given one.
        if SameFile
        then FLineMap[FLineMapMin].FileName := FLineMap[OldIdx].FileName
        else FLineMap[FLineMapMin].FileName := FileName;

        SameSource := SameFile and (SourceLine = FLineMap[OldIdx].SourceLine);
        if SameSource
        then begin
          // Check if OldIdx points to a Source line
          if FLineMap[OldIdx].State = lmsSource
          then begin
            FLineMap[FLineMapMin] := FLineMap[OldIdx];
            FLineMap[FLineMapMin].Line := Line;
            Inc(Line);
            Idx := OldIdx;
          end
          else Idx := FLineMapMin;
        end
        else begin
          // Insert source line
          Idx := FLineMapMin;
          DecLineMapMin;
          FLineMap[FLineMapMin].State := lmsSource;
          FLineMap[FLineMapMin].Line := Line - 1;
          FLineMap[FLineMapMin].Dump := '';
          FLineMap[FLineMapMin].Statement := '';
          FLineMap[FLineMapMin].SourceLine := SourceLine;
          FLineMap[FLineMapMin].FileName := FLineMap[idx].FileName;
        end;

        FLineMap[idx].Addr := NextAddr;
        FLineMap[idx].State := lmsStatement;
        FLineMap[idx].Dump := Dump;
        FLineMap[idx].Statement := Statement;
        FLineMap[idx].SourceLine := SourceLine;
      end
      else begin
        FLineMap[FLineMapMin].State := lmsInvalid;
        NextAddr := Addr - 1;
      end;
      FLineMap[FLineMapMin].Addr := NextAddr;
      if OK and ((Line = ALine) or (Line2 = ALine)) then Exit;
    end;

    if FLineMap[FLineMapMax].Line < ALine
    then begin
      // get lines after max
      // get startingpoint
      Addr := FLineMap[FLineMapMax].Addr;
      if not FDebugger.Disassemble(Addr, False, NextAddr, Dump, Statement, FileName, SourceLine)
      then NextAddr := Addr + 1;

      while FLineMap[FLineMapMax].Line < ALine do
      begin
        Addr := NextAddr;
        Line := FLineMap[FLineMapMax].Line + 1;
        Line2 := Line;
        OldIdx := FLineMapMax;
        IncLineMapMax;

        FLineMap[FLineMapMax].State := lmsUnknown;
        FLineMap[FLineMapMax].Line := Line;
        FLineMap[FLineMapMax].Addr := Addr;
        OK := FDebugger.Disassemble(Addr, False, NextAddr, Dump, Statement, FileName, SourceLine);
        if OK
        then begin
          SameFile := FileName = FLineMap[OldIdx].FileName;
          // to reduce the amount of unique duplicated strings,
          // use "our" copy of the filename and not the given one.
          if SameFile
          then FLineMap[FLineMapMax].FileName := FLineMap[OldIdx].FileName
          else FLineMap[FLineMapMax].FileName := FileName;

          SameSource := SameFile and (SourceLine = FLineMap[OldIdx].SourceLine);
          if not SameSource
          then begin
            // Insert source line first
            FLineMap[FLineMapMax].State := lmsSource;
            FLineMap[FLineMapMax].Dump := '';
            FLineMap[FLineMapMax].Statement := '';
            FLineMap[FLineMapMax].SourceLine := SourceLine;

            OldIdx := FLineMapMax;
            IncLineMapMax;
            Inc(Line);
            FLineMap[FLineMapMax].Line := Line;
            FLineMap[FLineMapMax].Addr := Addr;
          end;

          FLineMap[FLineMapMax].State := lmsStatement;
          FLineMap[FLineMapMax].Dump := Dump;
          FLineMap[FLineMapMax].Statement := Statement;
          FLineMap[FLineMapMax].SourceLine := SourceLine;
          FLineMap[FLineMapMax].FileName := FLineMap[OldIdx].FileName;
        end
        else begin
          FLineMap[FLineMapMax].State := lmsInvalid;
          NextAddr := Addr + 1;
        end;
        if OK and ((Line = ALine) or (Line2 = ALine)) then Exit;
      end;
    end;

    idx := Cardinal(ALine) mod Length(FLineMap);
    if FLineMap[idx].State <> lmsUnknown then Exit;

    FLineMap[idx].Line := ALine;
    Addr := FLineMap[idx].Addr;
    if FDebugger.Disassemble(Addr, False, NextAddr, Dump, Statement, FileName, SourceLine)
    then begin
      OldIdx := Cardinal(ALine-1) mod Length(FLineMap);

      SameFile := FileName = FLineMap[OldIdx].FileName;
      // to reduce the amount of unique duplicated strings,
      // use "our" copy of the filename and not the given one.
      if SameFile
      then FLineMap[idx].FileName := FLineMap[OldIdx].FileName
      else FLineMap[idx].FileName := FileName;

      SameSource := SameFile and (SourceLine = FLineMap[OldIdx].SourceLine);
      if not SameSource
      then begin
        // Insert source line first
        FLineMap[idx].State := lmsSource;
        FLineMap[idx].Dump := '';
        FLineMap[idx].Statement := '';
        FLineMap[idx].SourceLine := SourceLine;

        if idx = FLineMapMax then IncLineMapMax;
        OldIdx := idx;
        Line := ALine + 1;
        idx := Cardinal(Line) mod Length(FLineMap);
        FLineMap[idx].Line := Line;
        FLineMap[idx].Addr := Addr;
      end;


      FLineMap[idx].State := lmsStatement;
      FLineMap[idx].Dump := Dump;
      FLineMap[idx].Statement := Statement;
      FLineMap[idx].FileName := FLineMap[OldIdx].FileName;
      FLineMap[idx].SourceLine := SourceLine;
    end
    else begin
      FLineMap[idx].State := lmsUnknown;
    end;
  finally
    FUpdating := False;
    if FUpdateNeeded
    then begin
      FUpdateNeeded := False;
      // check begin and end
      UpdateLineData(FTopLine);
      UpdateLineData(FTopLine + FLineCount);
    end;
  end;
end;



initialization
  {$I assemblerdlg.lrs}

end.

