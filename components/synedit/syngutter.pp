unit SynGutter;

{$I synedit.inc}

interface

uses
  SysUtils, Classes, Controls, Graphics, LCLType, LCLIntf, Menus,
  SynEditMarks, SynEditMiscClasses, SynEditMiscProcs, SynEditFoldedView,
  SynTextDrawer, SynGutterBase, SynGutterLineNumber, SynGutterCodeFolding,
  SynGutterMarks, SynGutterChanges, SynEditMouseCmds;

type

  TSynGutterSeparator = class;

  { TSynGutter }

  TSynGutter = class(TSynGutterBase)
  private
    // List of all gutters
    FEdit: TSynEditBase;

    FWidth: integer;
    FRightOffset, FLeftOffset: integer;
    FOnChange: TNotifyEvent;
    FCursor: TCursor;
    FVisible: boolean;
    FAutoSize: boolean;
    FOnGutterClick: TGutterClickEvent;
    FMouseDownPart: Integer;
    procedure SetAutoSize(const Value: boolean);
    procedure SetLeftOffset(Value: integer);
    procedure SetRightOffset(Value: integer);
    procedure SetVisible(Value: boolean);
    procedure SetWidth(Value: integer);
    function PixelToPartIndex(X: Integer): Integer;
  protected
    procedure DoChange(Sender: TObject); override;
    procedure DoDefaultGutterClick(Sender: TObject; X, Y, Line: integer;
      mark: TSynEditMark); override;
  public
    property SynEdit: TSynEditBase read FEdit;
  public
    constructor Create(AOwner : TSynEditBase; AFoldedLinesView: TSynEditFoldedView;
                      ATextDrawer: TheTextDrawer);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Paint(Canvas: TCanvas; AClip: TRect; FirstLine, LastLine: integer);
    function  RealGutterWidth(CharWidth: integer): integer;
    function HasCustomPopupMenu(out PopMenu: TPopupMenu): Boolean;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MouseMove(Shift: TShiftState; X, Y: Integer);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    function MaybeHandleMouseAction(var AnInfo: TSynEditMouseActionInfo;
                         HandleActionProc: TSynEditMouseActionHandler): Boolean;
    function DoHandleMouseAction(AnAction: TSynEditMouseAction;
                                 var AnInfo: TSynEditMouseActionInfo): Boolean;
    procedure DoOnGutterClick(X, Y: integer);
    property  OnGutterClick: TGutterClickEvent
      read FOnGutterClick write FOnGutterClick;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    // A Methods for access from the Gutter; to be replaced
    procedure AutoSizeDigitCount(LinesCount: integer); // Forward to Line Number
  public
    // Access to well known parts
    Function LineNumberPart(Index: Integer = 0): TSynGutterLineNumber;
    Function CodeFoldPart(Index: Integer = 0): TSynGutterCodeFolding;
    Function ChangesPart(Index: Integer = 0): TSynGutterChanges;
    Function MarksPart(Index: Integer = 0): TSynGutterMarks;
    Function SeparatorPart(Index: Integer = 0): TSynGutterSeparator;
  published
    property AutoSize: boolean read FAutoSize write SetAutoSize default True;
    property Color;
    property Cursor: TCursor read FCursor write FCursor default crDefault;
    property LeftOffset: integer read FLeftOffset write SetLeftOffset
      default 0;
    property RightOffset: integer read FRightOffset write SetRightOffset
      default 0;
    property Visible: boolean read FVisible write SetVisible default TRUE;
    property Width: integer read FWidth write SetWidth default 30;
    property Parts;
    property MouseActions;
  end;

  { TSynGutterSeparator }

  TSynGutterSeparator = class(TSynGutterPartBase)
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint(Canvas: TCanvas; AClip: TRect; FirstLine, LastLine: integer); override;
    function RealGutterWidth(CharWidth: integer): integer;  override;
  end;

  { TSynEditMouseActionsGutter }

  TSynEditMouseActionsGutter = class(TSynEditMouseActions)
  public
    procedure ResetDefaults; override;
  end;


implementation
uses
  SynEdit;

{ TSynGutter }

constructor TSynGutter.Create(AOwner: TSynEditBase;
  AFoldedLinesView: TSynEditFoldedView; ATextDrawer: TheTextDrawer);
begin
  FEdit := AOwner;
  inherited Create(AOwner, AFoldedLinesView, ATextDrawer);

  FMouseActions := TSynEditMouseActionsGutter.Create(self);
  FMouseActions.ResetDefaults;
  Visible := True;
  Width := 30;
  LeftOffset := 0;
  FRightOffset := 0;
  AutoSize := True;

  if not(csLoading in AOwner.ComponentState) then begin
    TSynGutterMarks.Create(Parts);
    TSynGutterLineNumber.Create(Parts);
    TSynGutterChanges.Create(Parts);
    TSynGutterSeparator.Create(Parts);
    TSynGutterCodeFolding.Create(Parts);
  end;
end;

destructor TSynGutter.Destroy;
begin
  FreeAndNil(FMouseActions);
  FOnChange := nil;
  inherited Destroy;
end;

procedure TSynGutter.Assign(Source: TPersistent);
var
  Src: TSynGutter;
begin
  inherited;
  if Assigned(Source) and (Source is TSynGutter) then
  begin
    Src := TSynGutter(Source);
    FVisible := Src.FVisible;
    FWidth := Src.FWidth;
    FRightOffset := Src.FRightOffset;
    FAutoSize := Src.FAutoSize;

    DoChange(Self);
  end else
end;

function TSynGutter.RealGutterWidth(CharWidth: integer): integer;
var
  i: Integer;
begin
  if not FVisible then
  begin
    Result := 0;
    Exit;
  end;

  Result := FLeftOffset + FRightOffset;

  for i := PartCount-1 downto 0 do
    Result := Result + Parts[i].RealGutterWidth(CharWidth);
end;

procedure TSynGutter.SetLeftOffset(Value: integer);
begin
  Value := Max(0, Value);
  if FLeftOffset <> Value then
  begin
    FLeftOffset := Value;
    DoChange(Self);
  end;
end;

procedure TSynGutter.SetAutoSize(const Value: boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    DoChange(Self);
  end;
end;

procedure TSynGutter.SetRightOffset(Value: integer);
begin
  Value := Max(0, Value);
  if FRightOffset <> Value then
  begin
    FRightOffset := Value;
    DoChange(Self);
  end;
end;

procedure TSynGutter.SetVisible(Value: boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    DoChange(Self);
  end;
end;

procedure TSynGutter.SetWidth(Value: integer);
begin
  if FAutoSize then
    Value := RealGutterWidth(TextDrawer.CharWidth);
  Value := Max(0, Value);
  if (FWidth <> Value) then
  begin
    FWidth := Value;
    DoChange(Self);
  end;
end;

function TSynGutter.PixelToPartIndex(X: Integer): Integer;
begin
  Result := 0;
  x := x - FLeftOffset;
  while Result < PartCount-1 do begin
    if Parts[Result].Visible then begin
      if x >= Parts[Result].Width then
        x := x - Parts[Result].Width
      else
        break;
    end;
    inc(Result)
  end;
end;

procedure TSynGutter.DoChange(Sender: TObject);
begin
  If FAutoSize then
    FWidth := RealGutterWidth(TextDrawer.CharWidth);
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TSynGutter.DoDefaultGutterClick(Sender: TObject; X, Y, Line: integer;
  mark: TSynEditMark);
begin
end;

procedure TSynGutter.DoOnGutterClick(X, Y: integer);
begin
  Parts[PixelToPartIndex(X)].DoOnGutterClick(X, Y);
end;

procedure TSynGutter.Paint(Canvas: TCanvas; AClip: TRect; FirstLine, LastLine: integer);
var
  i: integer;
  rcLine: TRect;
  dc: HDC;
begin
  Canvas.Brush.Color := Color;
  dc := Canvas.Handle;
  {$IFDEF SYN_LAZARUS}
  LCLIntf.SetBkColor(dc,Canvas.Brush.Color);
  {$ENDIF}

  // Clear all
  TextDrawer.BeginDrawing(dc);
  TextDrawer.SetBackColor(Color);
  TextDrawer.SetForeColor(TSynEdit(FEdit).Font.Color);
  TextDrawer.SetFrameColor(clNone);
   with AClip do
     TextDrawer.ExtTextOut(Left, Top, ETO_OPAQUE, AClip, nil, 0);
  TextDrawer.EndDrawing;

  AClip.Left := FLeftOffset;
  rcLine := AClip;
  rcLine.Right := rcLine.Left;
  for i := 0 to PartCount -1 do
  begin
    if rcLine.Right >= AClip.Right then break;
    if Parts[i].Visible then
    begin
      rcLine.Left := rcLine.Right;
      rcLine.Right := min(rcLine.Left + Parts[i].Width, AClip.Right);
      Parts[i].Paint(Canvas, rcLine, FirstLine, LastLine);
    end;
  end;
end;

procedure TSynGutter.AutoSizeDigitCount(LinesCount : integer);
var
  i: Integer;
begin
  for i := 0 to Parts.ByClassCount[TSynGutterLineNumber] - 1 do
    TSynGutterLineNumber(Parts.ByClass[TSynGutterLineNumber, i]).AutoSizeDigitCount(LinesCount);
end;

function TSynGutter.HasCustomPopupMenu(out PopMenu: TPopupMenu): Boolean;
begin
  Result := Parts[FMouseDownPart].HasCustomPopupMenu(PopMenu);
end;

procedure TSynGutter.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FMouseDownPart := PixelToPartIndex(X);
  Parts[FMouseDownPart].MouseDown(Button, Shift, X, Y);
  if (Button=mbLeft) then
    DoOnGutterClick(X, Y);
end;

procedure TSynGutter.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  Parts[FMouseDownPart].MouseMove(Shift, X, Y);
end;

procedure TSynGutter.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Parts[FMouseDownPart].MouseUp(Button, Shift, X, Y);
end;

function TSynGutter.MaybeHandleMouseAction(var AnInfo: TSynEditMouseActionInfo;
  HandleActionProc: TSynEditMouseActionHandler): Boolean;
var
  MouseDownPart: LongInt;
begin
  MouseDownPart := PixelToPartIndex(AnInfo.MouseX);
  Result := Parts[MouseDownPart].MaybeHandleMouseAction(AnInfo, HandleActionProc);
  if not Result then
    Result := HandleActionProc(MouseActions, AnInfo);
end;

function TSynGutter.DoHandleMouseAction(AnAction: TSynEditMouseAction;
  var AnInfo: TSynEditMouseActionInfo): Boolean;
var
  i: Integer;
  ACommand: Word;
begin
  Result := False;
  for i := 0 to Parts.Count - 1 do begin
    Result := Parts[i].DoHandleMouseAction(AnAction, AnInfo);
    if Result then exit;;
  end;

  if AnAction = nil then exit;
  ACommand := AnAction.Command;
  if (ACommand = emcNone) then exit;

  case ACommand of
    emcOnMainGutterClick:
      begin
        if Assigned(FOnGutterClick) then begin
          FOnGutterClick(Self, AnInfo.MouseX, AnInfo.MouseY, AnInfo.NewCaret.LinePos, nil);
          Result := True;
        end;
      end;
  end;
end;

function TSynGutter.LineNumberPart(Index: Integer = 0): TSynGutterLineNumber;
begin
  Result := TSynGutterLineNumber(Parts.ByClass[TSynGutterLineNumber, Index]);
end;

function TSynGutter.CodeFoldPart(Index: Integer = 0): TSynGutterCodeFolding;
begin
  Result := TSynGutterCodeFolding(Parts.ByClass[TSynGutterCodeFolding, Index]);
end;

function TSynGutter.ChangesPart(Index: Integer = 0): TSynGutterChanges;
begin
  Result := TSynGutterChanges(Parts.ByClass[TSynGutterChanges, Index]);
end;

function TSynGutter.MarksPart(Index: Integer = 0): TSynGutterMarks;
begin
  Result := TSynGutterMarks(Parts.ByClass[TSynGutterMarks, Index]);
end;

function TSynGutter.SeparatorPart(Index: Integer = 0): TSynGutterSeparator;
begin
  Result := TSynGutterSeparator(Parts.ByClass[TSynGutterSeparator, Index]);
end;

{ TSynGutterSeparator }

constructor TSynGutterSeparator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWidth := 2;
  MarkupInfo.Background := clWhite;
  MarkupInfo.Foreground := clDkGray;
end;

procedure TSynGutterSeparator.Paint(Canvas: TCanvas; AClip: TRect; FirstLine, LastLine: integer);
begin
  with Canvas do
  begin
    Pen.Color := MarkupInfo.Background;
    Pen.Width := 1;
    with AClip do
    begin
      MoveTo(AClip.Left, AClip.Top);
      LineTo(AClip.Left, AClip.Bottom);
      Pen.Color := MarkupInfo.Foreground;
      MoveTo(AClip.Left+1, AClip.Top);
      LineTo(AClip.Left+1, AClip.Bottom);
    end;
  end;
end;

function TSynGutterSeparator.RealGutterWidth(CharWidth: integer): integer;
begin
  If Visible then
    Result := Width
  else
    Result := 0;
end;

{ TSynEditMouseActionsGutter }

procedure TSynEditMouseActionsGutter.ResetDefaults;
begin
  Clear;
  AddCommand(emcOnMainGutterClick, False, mbLeft, ccAny, cdDown, [], []);
  AddCommand(emcContextMenu, False, mbRight, ccSingle, cdUp, [], []);
end;

end.

