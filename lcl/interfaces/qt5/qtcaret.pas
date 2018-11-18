{
 /***************************************************************************
                        QtCaret.pas  -  Qt Caret Emulation
                       -------------------------------------

 copyright (c) Andreas Hausladen

 adopted for Lazarus and Qt5 by Lazarus Team

 ***************************************************************************/

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

unit QtCaret;
{$mode objfpc}{$H+}

interface

{off $DEFINE VerboseQtCaret}

{$I qtdefines.inc}

uses
  // Bindings
  qt5,
  // Free Pascal
  Classes, SysUtils, Types,
  // Widgetset
  QtObjects, QtWidgets,
  // LCL
  LCLType, LCLIntf, Graphics, ExtCtrls;

type

  { TEmulatedCaret }

  TEmulatedCaret = class(TComponent)
  private
    FCaretDirty: Boolean;
    FCaretDirtyPos: TQtPoint;
    FTimer: TTimer;
    FOldRect: TRect;
    FWidget: TQtWidget;
    FPixmap: QPixmapH;
    FLastValidWidth, FLastValidHeight: Integer;
    FWidth, FHeight: Integer;
    FPos: TQtPoint;
    FVisible: Boolean;
    FVisibleState: Boolean;
    FRespondToFocus: Boolean;
    FCritSect: TCriticalSection;
    procedure SetPos(const Value: TQtPoint);
  protected
    procedure DoTimer(Sender: TObject);
    procedure DrawCaret; virtual;
    function CreateColorPixmap(Color: PtrUInt): QPixmapH;
    procedure SetWidget(AWidget: TQtWidget);
    procedure UpdateCaret(const AForceUpdate: Boolean = False);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Lock;
    procedure Unlock;

    function CreateCaret(AWidget: TQtWidget; Pixmap: QPixmapH; Width, Height: Integer): Boolean;
    function DestroyCaret: Boolean;

    function IsValid: Boolean;

    function Show(AWidget: TQtWidget): Boolean;
    function Hide: Boolean;

    property CaretDirty: Boolean read FCaretDirty;
    property Timer: TTimer read FTimer;
    property Pos: TQtPoint read FPos write SetPos;
    property RespondToFocus: Boolean read FRespondToFocus write FRespondToFocus;
  end;

function CreateCaret(Widget: TQtWidget; Pixmap: QPixmapH; Width, Height: Integer): Boolean; overload;
function CreateCaret(Widget: TQtWidget; ColorCaret: PtrUInt; Width, Height: Integer): Boolean; overload;
function HideCaret(Widget: TQtWidget): Boolean;
function ShowCaret(Widget: TQtWidget): Boolean;
function SetCaretPos(X, Y: Integer): Boolean;
function GetCaretPos(var Pt: TPoint): Boolean;
function GetQtCaretRespondToFocus: Boolean;
procedure SetQtCaretRespondToFocus(Value: Boolean);
function DestroyCaret: Boolean;
procedure DrawCaret;
procedure DestroyGlobalCaret;
function GlobalCaretDirty: Boolean;

implementation
uses
  qtint
  {$IFDEF VerboseQtCaret}
  ,LCLProc
  {$ENDIF}
  ;
var
  GlobalCaret: TEmulatedCaret = nil;
  
procedure GlobalCaretNeeded;
begin
  if GlobalCaret = nil then
    GlobalCaret := TEmulatedCaret.Create(nil);
end;

function GlobalCaretDirty: Boolean;
begin
  Result := False;
  if GlobalCaret <> nil then
    Result := GlobalCaret.CaretDirty;
end;

procedure DrawCaret;
begin
  GlobalCaretNeeded;
  if Assigned(GlobalCaret) then
  begin
    GlobalCaret.Lock;
    try
      GlobalCaret.DrawCaret;
    finally
      GlobalCaret.Unlock;
    end;
  end;
end;

procedure DestroyGlobalCaret;
begin
  FreeAndNil(GlobalCaret);
end;

function CreateCaret(Widget: TQtWidget; Pixmap: QPixmapH; Width, Height: Integer): Boolean;
begin
  GlobalCaretNeeded;
  GlobalCaret.Lock;
  try
    Result := GlobalCaret.CreateCaret(Widget, Pixmap, Width, Height);
  finally
    GlobalCaret.Unlock;
  end;
end;

function CreateCaret(Widget: TQtWidget; ColorCaret: PtrUInt; Width, Height: Integer): Boolean;
begin
  Result := CreateCaret(Widget, QPixmapH(ColorCaret), Width, Height);
end;

function GetCaretBlinkTime: Cardinal;
var
  FlashTime: Integer;
begin
  FlashTime := QApplication_cursorFlashTime;
  if FlashTime > 0 then
    Result := FlashTime div 2
  else
    Result := 600; // our default value
end;

function SetCaretBlinkTime(uMSeconds: Cardinal): LongBool;
begin
  Result := True;
  try
    QApplication_setCursorFlashTime(uMSeconds);
    if assigned(GlobalCaret) then
    begin
      GlobalCaret.Lock;
      try
        GlobalCaret.Timer.Interval := GetCaretBlinkTime;
      finally
        GlobalCaret.Unlock;
      end;
    end;
  except
    Result := False;
  end;
end;

function HideCaret(Widget: TQtWidget): Boolean;
begin
  GlobalCaretNeeded;
  if Assigned(GlobalCaret) then
  begin
    GlobalCaret.Lock;
    try
      Result := GlobalCaret.Hide;
    finally
      GlobalCaret.Unlock;
    end;
  end
  else
    Result := false;
end;

function ShowCaret(Widget: TQtWidget): Boolean;
begin
  GlobalCaretNeeded;
  GlobalCaret.Lock;
  try
    Result := GlobalCaret.Show(Widget);
  finally
    GlobalCaret.Unlock;
  end;
end;

function SetCaretPos(X, Y: Integer): Boolean;
begin
  Result := True;
  GlobalCaretNeeded;
  GlobalCaret.Lock;
  try
    GlobalCaret.Pos := QtPoint(X, Y);
  finally
    GlobalCaret.Unlock;
  end;
end;

function GetCaretPos(var Pt: TPoint): Boolean;
begin
  Result := True;
  GlobalCaretNeeded;
  GlobalCaret.Lock;
  try
    with GlobalCaret.Pos do
    begin
      Pt.x := X;
      Pt.y := Y;
    end;
  finally
    GlobalCaret.Unlock;
  end;
end;

function GetQtCaretRespondToFocus: Boolean;
begin
  if Assigned(GlobalCaret) then
    Result := GlobalCaret.RespondToFocus
  else
    Result := False;
end;

procedure SetQtCaretRespondToFocus(Value: Boolean);
begin
  if Assigned(GlobalCaret) then
    GlobalCaret.RespondToFocus := Value;
end;

function DestroyCaret: Boolean;
begin
  if Assigned(GlobalCaret) then
  begin
    GlobalCaret.Lock;
    try
      Result := GlobalCaret.DestroyCaret;
    finally
      GlobalCaret.Unlock;
    end;
  end
  else
    Result := False;
end;

{ TEmulatedCaret }

constructor TEmulatedCaret.Create(AOwner: TComponent);
begin
  {$IFDEF VerboseQtCaret}
  writeln('TEmulatedCaret.Create()');
  {$ENDIF}
  inherited Create(AOwner);
  InitializeCriticalSection(FCritSect);

  FLastValidWidth := 0;
  FLastValidHeight := 0;
  FCaretDirty := False;
  FOldRect := Rect(0, 0, 1, 1);
  FPos := QtPoint(0, 0);
  FCaretDirtyPos := FPos;
  
  FTimer := TTimer.Create(self);
  FTimer.Enabled := False;
  FTimer.Interval := GetCaretBlinkTime;
  FTimer.OnTimer := @DoTimer;
  
  FRespondToFocus := False;
end;

destructor TEmulatedCaret.Destroy;
begin
  {$IFDEF VerboseQtCaret}
  writeln('TEmulatedCaret.Destroy()');
  {$ENDIF}
  DestroyCaret;
  DeleteCriticalSection(FCritSect);
  inherited Destroy;
end;

function TEmulatedCaret.CreateCaret(AWidget: TQtWidget; Pixmap: QPixmapH;
  Width, Height: Integer): Boolean;
begin
  DestroyCaret;
  SetWidget(AWidget);
  FWidth := Width;
  FHeight := Height;
  if FWidth > 0 then
    FLastValidWidth := FWidth;
  if FHeight > 0 then
    FLastValidHeight := FHeight;

  if PtrUInt(Pixmap) > $FFFF then
    FPixmap := QPixmap_create(Pixmap)
  else
    FPixmap := CreateColorPixmap(PtrUInt(Pixmap));

  {$IFDEF VerboseQtCaret}
  writeln('TEmulatedCaret.CreateCaret IsValid=',IsValid,' FVis=',FVisible,
    ' FVisState=',FVisibleState,' FPixmap=',dbghex(PtrUInt(FPixmap)),
    ' FWidth=',FWidth,' FHeight=',FHeight,' FWidget=',dbghex(PtrUInt(FWidget)));
  {$ENDIF}

  Result := Assigned(FPixmap);
  FTimer.Enabled := True;
end;

function TEmulatedCaret.DestroyCaret: Boolean;
begin
  {$IFDEF VerboseQtCaret}
  writeln('TEmulatedCaret.DestroyCaret IsValid=',IsValid,' FVis=',FVisible,
    ' FVisState=',FVisibleState,' FPixmap=',dbghex(PtrUInt(FPixmap)),
    ' FWidth=',FWidth,' FHeight=',FHeight,' FWidget=',dbghex(PtrUInt(FWidget)));
  {$ENDIF}
  FTimer.Enabled := False;
  Hide;
  if Assigned(FPixmap) then
    QPixmap_destroy(FPixmap);
  FWidget := nil;
  FPixmap := nil;
  FWidth := 0;
  FHeight := 0;
  Result := not IsValid;
end;

procedure TEmulatedCaret.DrawCaret;
var
  R: TRect;
begin
  {$IFDEF VerboseQtCaret}
  writeln('TEmulatedCaret.DrawCaret IsValid=',IsValid,' FVis=',FVisible,
    ' FVisState=',FVisibleState,' FPixmap=',dbghex(PtrUInt(FPixmap)),
    ' FWidth=',FWidth,' FHeight=',FHeight,' FWidget=',dbghex(PtrUInt(FWidget)));
  {$ENDIF}
  if IsValid and FVisible and FVisibleState then
  begin
    FCaretDirty := False;
    R := Rect(0, 0, QPixmap_width(FPixmap), QPixmap_height(FPixmap));
    TQtDeviceContext(FWidget.Context).save;
    TQtDeviceContext(FWidget.Context).setCompositionMode(QPainterRasterOp_NotSourceXorDestination);
    TQtDeviceContext(FWidget.Context).drawPixmap(PQtPoint(@FPos), FPixmap, PRect(@R));
    TQtDeviceContext(FWidget.Context).restore;
  end;
end;

function TEmulatedCaret.Show(AWidget: TQtWidget): Boolean;
var
  Pt: TQtPoint;
begin
  {$IFDEF VerboseQtCaret}
  writeln('TEmulatedCaret.Show AWidget=',dbghex(PtrUInt(AWidget)));
  {$ENDIF}
  if FWidget <> AWidget then
  begin
    {$IFDEF VerboseQtCaret}
    writeln('TEmulatedCaret.Show Widget changed IsValid=',IsValid);
    {$ENDIF}
    Hide;
    SetWidget(AWidget);
    if (FCaretDirty or (FPixmap=nil))
    and (AWidget <> nil) and (FWidget.Context<>0) then
    begin
      CreateCaret(FWidget, nil, FLastValidWidth, FLastValidHeight);
      if (FCaretDirtyPos.X > 0) or (FCaretDirtyPos.Y > 0) then
      begin
        if (FWidget.LastCaretPos.X >= 0) and (FWidget.LastCaretPos.Y >= 0) then
          Pt := FWidget.LastCaretPos
        else
          Pt := FPos;
        SetPos(FCaretDirtyPos);
        FCaretDirtyPos := QtPoint(0, 0);
        QCoreApplication_processEvents(QEventLoopExcludeUserInputEvents);
        SetPos(Pt);
      end else
        SetPos(FPos);
    end;
  end;
  Result := Assigned(FPixmap);
  FVisible := Result;
  SetPos(FPos);
  //writeln('TEmulatedCaret.Show END Result=',Result,' IsValid=',IsValid,' Handle=',QtWidgetSet.IsValidHandle(HWND(FWidget)),' FPixmap=',FPixmap<>nil,' FWidget.Context=',FWidget.Context<>0);
end;

function TEmulatedCaret.Hide: Boolean;
begin
  {$IFDEF VerboseQtCaret}
  writeln('TEmulatedCaret.Hide IsValid=',IsValid,' FVisible=',FVisible);
  {$ENDIF}
  Result := IsValid;
  if Result and FVisible then
  begin
    FVisible := False;
    UpdateCaret;
  end;
end;

procedure TEmulatedCaret.SetPos(const Value: TQtPoint);
begin
  {$IFDEF VerboseQtCaret}
  writeln('TEmulatedCaret.SetPos FWidget=',dbghex(PtrUInt(FWidget)),' X=',Value.X,
  ' Y=',Value.Y,' OldX=',FPos.X,' OldY=',FPos.Y,' Dirty=',FCaretDirty);
  {$ENDIF}

  if not QtWidgetSet.IsValidHandle(HWND(FWidget)) or (FWidget.Widget = nil) then
  begin
    // oops, our caret is dirty here.
    FCaretDirtyPos := FPos;
    FCaretDirty := True;
    FPos := Value;
    exit;
  end;

  if ((FPos.x <> Value.x) or (FPos.y <> Value.y)) or FCaretDirty then
  begin
    FWidget.LastCaretPos := FPos;
    FPos := Value;
    FTimer.Enabled := False;
    FVisibleState := FWidget.Context = 0;
    {$note remove complete property RespondToFocus after testing}
    // if RespondToFocus and not FCaretDirty then
    if not FCaretDirty then
      UpdateCaret(True);
    if FCaretDirty then
    begin
      FVisible := True;
      if FWidget.Context <> 0 then
        DoTimer(FTimer);
      FTimer.Enabled := True;
    end;
  end else
  begin
    if FWidget.Context = 0 then
    begin
      FVisibleState := True;
      FTimer.Enabled := False;
      UpdateCaret;
    end else
    begin
      if not FTimer.Enabled then
        FTimer.Enabled := True;
    end;
  end;
end;

procedure TEmulatedCaret.DoTimer(Sender: TObject);
begin
  FVisibleState := not FVisibleState;
  if FVisible then
    UpdateCaret;
end;

procedure TEmulatedCaret.Lock;
begin
  {$note TEmulatedCaret.Lock()
    remove this routine later, after
   testing on all platforms}
  // EnterCriticalSection(FCritSect);
end;

procedure TEmulatedCaret.Unlock;
begin
  {$note TEmulatedCaret.UnLock()
   remove this routine later, after
   testing on all platforms}
  // LeaveCriticalSection(FCritSect);
end;

function TEmulatedCaret.CreateColorPixmap(Color: PtrUInt): QPixmapH;
var
  QC: TQColor;
  AColor: TColor;
begin
  {$IFDEF VerboseQtCaret}
  writeln('TEmulatedCaret.CreateColorPixmap FWidget=',dbghex(PtrUInt(FWidget)),' Width=',FWidth,
    ' FHeight=',FHeight);
  {$ENDIF}
  if (FWidth <= 0) or (FHeight <= 0) then
    Result := nil
  else
  begin
    case Color of
      0: AColor := clBlack;
      1: AColor := clGray;
    else
    begin
      Result := nil;
      Exit;
    end;
    end;
    Result := QPixmap_create(FWidth, FHeight);
    try
      QColor_fromRgb(@QC,Red(AColor),Green(AColor),Blue(AColor));
      QPixmap_fill(Result, @QC);
    except
      QPixmap_destroy(Result);
      Result := nil;
    end;
  end;
end;

function TEmulatedCaret.IsValid: Boolean;
begin
  Result := QtWidgetSet.IsValidHandle(HWND(FWidget));
  if Result then
    Result := (FPixmap <> nil) and (FWidget.Context <> 0);
end;

procedure TEmulatedCaret.SetWidget(AWidget: TQtWidget);
begin
  if FWidget <> nil then
    FWidget.HasCaret := False;

  if AWidget is TQtCustomControl then
    AWidget := TQtCustomControl(AWidget).viewport;

  FWidget := AWidget;
  if FWidget <> nil then
    FWidget.HasCaret := True;
end;

procedure TEmulatedCaret.UpdateCaret(const AForceUpdate: Boolean = False);
var
  R: TRect;
begin
  if not QtWidgetSet.IsValidHandle(HWND(FWidget)) then
    exit;
  if (FWidget.Widget <> nil) then
  begin
    if FPos.X < 0 then
      FPos.X := 0;
    if FPos.Y < 0 then
      FPos.Y := 0;
    R.Left := FPos.x;
    R.Top := FPos.y;
    R.Right := R.Left + FWidth + 2;
    R.Bottom := R.Top + FHeight + 2;
    if (FWidget.Context = 0) then
    begin
      if not EqualRect(FOldRect, R) then
        FWidget.Update(@FOldRect);
      FWidget.Update(@R);
      FOldRect := R;
    end else
    begin
      if AForceUpdate and not FVisible and not FTimer.Enabled then
        FTimer.Enabled := True;
    end;
  end;
end;

end.
