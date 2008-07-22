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
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
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

{$I qtdefines.inc}

uses
  // Bindings
  qt4,
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
    FOldRect: TRect;
    FWidget: TQtWidget;
    FPixmap: QPixmapH;
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

implementation

var
  GlobalCaret: TEmulatedCaret = nil;
  
procedure GlobalCaretNeeded;
begin
  if GlobalCaret = nil then
    GlobalCaret := TEmulatedCaret.Create(nil);
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
  Result := GlobalCaret.RespondToFocus;
end;

procedure SetQtCaretRespondToFocus(Value: Boolean);
begin
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
  inherited Create(AOwner);
  InitializeCriticalSection(FCritSect);

  FOldRect := Rect(0, 0, 1, 1);
  
  FTimer := TTimer.Create(self);
  FTimer.Enabled := False;
  FTimer.Interval := GetCaretBlinkTime;
  FTimer.OnTimer := @DoTimer;
  
  FRespondToFocus := False;
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
  if PtrUInt(Pixmap) > $FFFF then
    FPixmap := QPixmap_create(Pixmap)
  else
    FPixmap := CreateColorPixmap(PtrUInt(Pixmap));

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
  R: TRect;
begin
  if IsValid and FVisible and FVisibleState then
  begin
    R := Rect(0, 0, QPixmap_width(FPixmap), QPixmap_height(FPixmap));
    TQtDeviceContext(FWidget.Context).drawPixmap(PQtPoint(@FPos), FPixmap, PRect(@R));
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
  if (FWidget = nil) or (FWidget.Widget = nil) then
  begin
    FPos.X := 0;
    FPos.Y := 0;
    exit;
  end;
  
  if ((FPos.x <> Value.x) or (FPos.y <> Value.y)) then
  begin
    FPos := Value;
    FTimer.Enabled := False;
    FVisibleState := FWidget.Context = 0;
    if RespondToFocus then
      UpdateCaret;
  end else
  begin
    if FWidget.Context = 0 then
    begin
      FTimer.Enabled := False;
      FVisibleState := True;
    end else
      if not FTimer.Enabled then
        FTimer.Enabled := True;
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
  EnterCriticalSection(FCritSect);
end;

procedure TEmulatedCaret.Unlock;
begin
  LeaveCriticalSection(FCritSect);
end;

function TEmulatedCaret.CreateColorPixmap(Color: PtrUInt): QPixmapH;
var
  QC: TQColor;
  AColor: TColor;
begin
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
      QColor_setRgb(QColorH(@QC),Red(AColor),Green(AColor),Blue(AColor));
      QPixmap_fill(Result, @QC);
    except
      QPixmap_destroy(Result);
      Result := nil;
    end;
  end;
end;

function TEmulatedCaret.IsValid: Boolean;
begin
  Result := (FWidget <> nil) and (FPixmap <> nil) and (FWidget.Context <> 0);
end;

procedure TEmulatedCaret.SetWidget(AWidget: TQtWidget);
begin
  if FWidget <> nil then
    FWidget.HasCaret := False;

  if AWidget is TQtAbstractScrollArea then
    AWidget := TQtAbstractScrollArea(AWidget).viewport;

  FWidget := AWidget;
  if FWidget <> nil then
    FWidget.HasCaret := True;
end;

procedure TEmulatedCaret.UpdateCaret;
var
  R: TRect;
begin
  if (FWidget <> nil) then
  begin
    R.Left := FPos.x;
    R.Top := FPos.y;
    R.Right := R.Left + FWidth + 2;
    R.Bottom := R.Top + FHeight + 2;
    if (FWidget.Context = 0) and (FWidget.Widget <> nil) then
    begin
      if not EqualRect(FOldRect, R) then
        FWidget.Update(@FOldRect);
      FWidget.Update(@R);
    end;
    FOldRect := R;
  end;
end;

end.
