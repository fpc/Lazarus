{
 /***************************************************************************
                        QtCaret.pas  -  Qt Caret Emulation
                       -------------------------------------

 copyright (c) Andreas Hausladen

 adopted for Lazarus and Qt4 by Lazarus Team

 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

unit QtCaret;
{$mode objfpc}{$H+}

interface

uses
  // Bindings
{$ifdef USE_QT_4_3}
  qt43,
{$else}
  qt4,
{$endif}
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
    FTimer: TTimer;
    FWndId: Cardinal;
    FWidget: TQtWidget;
    FPixmap: QPixmapH;
    FWidth, FHeight: Integer;
    FPos: TQtPoint;
    FVisible: Boolean;
    FVisibleState: Boolean;
    FCritSect: TCriticalSection;
    procedure SetPos(const Value: TQtPoint);
  protected
    procedure DoTimer(Sender: TObject);
    procedure DrawCaret; virtual;
    function CreateColorPixmap(Color: Cardinal): QPixmapH;
    procedure SetWidget(AWidget: TQtWidget);
    procedure UpdateCaret;
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

    property Timer: TTimer read FTimer;
    property Pos: TQtPoint read FPos write SetPos;
  end;

function CreateCaret(Widget: TQtWidget; Pixmap: QPixmapH; Width, Height: Integer): Boolean; overload;
function CreateCaret(Widget: TQtWidget; ColorCaret: Cardinal; Width, Height: Integer): Boolean; overload;
function HideCaret(Widget: TQtWidget): Boolean;
function ShowCaret(Widget: TQtWidget): Boolean;
function SetCaretPos(X, Y: Integer): Boolean;
function GetCaretPos(var Pt: TPoint): Boolean;
function DestroyCaret: Boolean;
procedure DrawCaret;
procedure DestroyGlobalCaret;

implementation

var
  GlobalCaret: TEmulatedCaret = nil;
  
procedure GlobalCaretNeeded;
begin
  if GlobalCaret = nil then
    GlobalCaret := TEmulatedCaret.Create(nil);
end;

function QtPoint(X, Y: Integer): TQtPoint;
begin
  Result.X := X;
  Result.Y := Y;
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

function CreateCaret(Widget: TQtWidget; ColorCaret: Cardinal; Width, Height: Integer): Boolean;
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
  inherited Create(AOwner);
  InitializeCriticalSection(FCritSect);

  FTimer := TTimer.Create(self);
  FTimer.Enabled := False;
  FTimer.Interval := GetCaretBlinkTime;
  FTimer.OnTimer := @DoTimer;
end;

destructor TEmulatedCaret.Destroy;
begin
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
  if Cardinal(Pixmap) > $FFFF then
    FPixmap := QPixmap_create(Pixmap)
  else
    FPixmap := CreateColorPixmap(Integer(Pixmap));

  Result := IsValid;
  FTimer.Enabled := True;
end;

function TEmulatedCaret.DestroyCaret: Boolean;
begin
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
  DestDev: QPaintDeviceH;
  Painter: QPainterH;
  R: TRect;
begin
  if IsValid and FVisible and FVisibleState then
  begin
    DestDev := QWidget_to_QPaintDevice(FWidget.Widget);
    Painter := QPainter_create(DestDev);
    R := Rect(0, 0, QPixmap_width(FPixmap), QPixmap_height(FPixmap));
    QPainter_drawPixmap(Painter, PQtPoint(@FPos), FPixmap, PRect(@R));
    QPainter_destroy(Painter);
  end;
end;

function TEmulatedCaret.Show(AWidget: TQtWidget): Boolean;
begin
  if FWidget <> AWidget then
  begin
    Hide;
    SetWidget(AWidget);
  end;
  Result := IsValid;
  if Result then
    FVisible := True;
end;

function TEmulatedCaret.Hide: Boolean;
begin
  Result := IsValid;
  if Result and FVisible then
  begin
    FVisible := False;
    UpdateCaret;
  end;
end;

procedure TEmulatedCaret.SetPos(const Value: TQtPoint);
begin
  if FVisible and ((FPos.x <> Value.x) or (FPos.y <> Value.y)) then
  begin
    Hide;
    try
      FPos := Value;
    finally
      Show(FWidget);
    end;
  end
  else
    FPos := Value;
end;

procedure TEmulatedCaret.DoTimer(Sender: TObject);
begin
  FVisibleState := not FVisibleState;
  if FVisible then
    UpdateCaret;
end;

procedure TEmulatedCaret.Lock;
begin
  EnterCriticalSection(FCritSect);
end;

procedure TEmulatedCaret.Unlock;
begin
  LeaveCriticalSection(FCritSect);
end;

function TEmulatedCaret.CreateColorPixmap(Color: Cardinal): QPixmapH;
var
  QC: TQColor;
begin
  if (FWidth <= 0) or (FHeight <= 0) then
    Result := nil
  else
  begin
    case Color of
      0: ColorRefToTQColor(clBlack, QC);
      1: ColorRefToTQColor(clGray, QC);
    else
      Result := nil;
      Exit;
    end;
    Result := QPixmap_create(FWidth, FHeight);
    try
      QPixmap_fill(Result, @QC);
    except
      QPixmap_destroy(Result);
      Result := nil;
    end;
  end;
end;

function TEmulatedCaret.IsValid: Boolean;
begin
  Result := (FWidget <> nil) and (FPixmap <> nil) and
    (QWidget_find(FWndId) <> nil);
end;

procedure TEmulatedCaret.SetWidget(AWidget: TQtWidget);
begin
  if FWidget <> nil then
    FWidget.HasCaret := False;

  FWidget := AWidget;
  if FWidget <> nil then
  begin
    FWndId := QWidget_winId(FWidget.Widget);
    FWidget.HasCaret := True;
  end
  else
    FWndId := 0;
end;

procedure TEmulatedCaret.UpdateCaret;
var
  R: TRect;
begin
  if (FWidget <> nil) and not FWidget.InPaint then
  begin
    R.Left := FPos.x;
    R.Top := FPos.y;
    R.Right := R.Left + FWidth;
    R.Bottom := R.Top + FHeight;
    FWidget.Update(@R);
  end;
end;

end.
