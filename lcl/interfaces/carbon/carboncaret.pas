{
 /***************************************************************************
                    CarbonCaret.pas  -  Carbon Caret Emulation
                    ------------------------------------------

 copyright (c) Andreas Hausladen

 adopted for Lazarus and Carbon by Lazarus Team

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

unit CarbonCaret;
{$mode objfpc}{$H+}

interface

// debugging defines
{$I carbondebug.inc}

uses
  // Bindings
  FPCMacOSAll,
  // Free Pascal
  Classes, SysUtils, Types,
  // Widgetset
  CarbonDef, CarbonGDIObjects,
  // LCL
  LCLType, LCLIntf, LCLProc, Graphics, ExtCtrls;

type
  { TEmulatedCaret }

  TEmulatedCaret = class(TComponent)
  private
    FTimer: TTimer;
    FUpdating: Boolean;
    FOldRect: TRect;
    FWidget: TCarbonWidget;
    FBitmap: TCarbonBitmap;
    FWidth, FHeight: Integer;
    FPos: TPoint;
    FVisible: Boolean;
    FVisibleState: Boolean;
    FRespondToFocus: Boolean;
    procedure SetPos(const Value: TPoint);
  protected
    procedure DoTimer(Sender: TObject);
    procedure DrawCaret; virtual;
    procedure SetWidget(AWidget: TCarbonWidget);
    procedure UpdateCaret;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function CreateCaret(AWidget: TCarbonWidget; Bitmap: PtrUInt; Width, Height: Integer): Boolean;
    function DestroyCaret: Boolean;

    function IsValid: Boolean;

    function Show(AWidget: TCarbonWidget): Boolean;
    function Hide: Boolean;

    property Timer: TTimer read FTimer;
    property Pos: TPoint read FPos write SetPos;
    property RespondToFocus: Boolean read FRespondToFocus write FRespondToFocus;
  end;

function CreateCaret(Widget: TCarbonWidget; Bitmap: PtrUInt; Width, Height: Integer): Boolean; overload;
function HideCaret(Widget: TCarbonWidget): Boolean;
function ShowCaret(Widget: TCarbonWidget): Boolean;
function SetCaretPos(X, Y: Integer): Boolean;
function GetCaretPos(var P: TPoint): Boolean;
function GetCarbonCaretRespondToFocus: Boolean;
procedure SetCarbonCaretRespondToFocus(Value: Boolean);
function DestroyCaret: Boolean;
procedure DrawCaret;
procedure DestroyGlobalCaret;

implementation

uses CarbonCanvas;

var
  GlobalCaret: TEmulatedCaret = nil;
  
procedure GlobalCaretNeeded;
begin
  if GlobalCaret = nil then GlobalCaret := TEmulatedCaret.Create(nil);
end;

procedure DrawCaret;
begin
  GlobalCaretNeeded;
  if Assigned(GlobalCaret) then GlobalCaret.DrawCaret;
end;

procedure DestroyGlobalCaret;
begin
  FreeAndNil(GlobalCaret);
end;

function CreateCaret(Widget: TCarbonWidget; Bitmap: PtrUInt; Width, Height: Integer): Boolean;
begin
  GlobalCaretNeeded;

  Result := GlobalCaret.CreateCaret(Widget, Bitmap, Width, Height);
end;

function GetCaretBlinkTime: Cardinal;
begin
  // TODO: use FPCMacOSAll.GetCaretTime
  Result := 600; // our default value
end;

function HideCaret(Widget: TCarbonWidget): Boolean;
begin
  GlobalCaretNeeded;
  if Assigned(GlobalCaret) then
    Result := GlobalCaret.Hide
  else
    Result := False;
end;

function ShowCaret(Widget: TCarbonWidget): Boolean;
begin
  GlobalCaretNeeded;

  Result := GlobalCaret.Show(Widget);
end;

function SetCaretPos(X, Y: Integer): Boolean;
begin
  Result := True;
  GlobalCaretNeeded;

  GlobalCaret.Pos := Classes.Point(X, Y);
end;

function GetCaretPos(var P: TPoint): Boolean;
begin
  Result := True;
  GlobalCaretNeeded;
  
  with GlobalCaret.Pos do
  begin
    P.x := X;
    P.y := Y;
  end;
end;

function GetCarbonCaretRespondToFocus: Boolean;
begin
  Result := GlobalCaret.RespondToFocus;
end;

procedure SetCarbonCaretRespondToFocus(Value: Boolean);
begin
  GlobalCaret.RespondToFocus := Value;
end;

function DestroyCaret: Boolean;
begin
  if Assigned(GlobalCaret) then
    Result := GlobalCaret.DestroyCaret
  else
    Result := False;
end;

{ TEmulatedCaret }

constructor TEmulatedCaret.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FOldRect := Rect(0, 0, 1, 1);
  
  FUpdating := False;
  FTimer := TTimer.Create(self);
  FTimer.Enabled := False;
  FTimer.Interval := GetCaretBlinkTime;
  FTimer.OnTimer := @DoTimer;
  
  FRespondToFocus := False;
end;

destructor TEmulatedCaret.Destroy;
begin
  DestroyCaret;

  inherited Destroy;
end;

function TEmulatedCaret.CreateCaret(AWidget: TCarbonWidget; Bitmap: PtrUInt;
  Width, Height: Integer): Boolean;
begin
  DestroyCaret;
  SetWidget(AWidget);
  
  FWidth := Width;
  FHeight := Height;
  if Bitmap > 1 then
    FBitmap := TCarbonBitmap.Create(TCarbonBitmap(Bitmap))
  else
    FBitmap := nil;

  Result := IsValid;
  FTimer.Enabled := True;
end;

function TEmulatedCaret.DestroyCaret: Boolean;
begin
  FTimer.Enabled := False;
  Hide;
  if Assigned(FBitmap) then FBitmap.Free;
  FWidget := nil;
  FBitmap := nil;
  FWidth := 0;
  FHeight := 0;
  Result := not IsValid;
end;

procedure TEmulatedCaret.DrawCaret;
begin
  if IsValid and FVisible and FVisibleState and FWidget.Painting then
  begin
    if FBitmap = nil then
      (FWidget.Context as TCarbonDeviceContext).InvertRectangle(FPos.X, FPos.Y,
        FPos.X + FWidth, FPos.Y + FHeight)
    else
      (FWidget.Context as TCarbonDeviceContext).DrawCGImage(FPos.X, FPos.Y,
        FBitmap.Width, FBitmap.Height, FBitmap.CGImage);
  end;
end;

function TEmulatedCaret.Show(AWidget: TCarbonWidget): Boolean;
var
  ShowVisible: Boolean;
begin
  Result := False;
  if FUpdating then Exit;

  if FWidget <> AWidget then
  begin
    Hide;
    SetWidget(AWidget);
    ShowVisible := True;
  end
  else ShowVisible := not FVisible;
  
  Result := IsValid;
  
  if Result then
  begin
    FVisible := True;
    FVisibleState := ShowVisible;
    UpdateCaret;
    FTimer.Enabled := True;
  end;
end;

function TEmulatedCaret.Hide: Boolean;
begin
  Result := False;
  if FUpdating then Exit;

  Result := IsValid;
  if Result and FVisible then
  begin
    FVisible := False;
    UpdateCaret;
    FTimer.Enabled := False;
  end;
end;

procedure TEmulatedCaret.SetPos(const Value: TPoint);
begin
  if FUpdating then Exit;
  if FWidget = nil then
  begin
    FPos.X := 0;
    FPos.Y := 0;
    Exit;
  end;
  
  if ((FPos.x <> Value.x) or (FPos.y <> Value.y)) then
  begin
    FPos := Value;
    FVisibleState := True;
    UpdateCaret;
  end;
end;

procedure TEmulatedCaret.DoTimer(Sender: TObject);
begin
  FVisibleState := not FVisibleState;
  if FVisible then UpdateCaret;
end;

function TEmulatedCaret.IsValid: Boolean;
begin
  Result := (FWidth > 0) and (FHeight > 0) and (FWidget <> nil) and FWidget.IsVisible;
end;

procedure TEmulatedCaret.SetWidget(AWidget: TCarbonWidget);
begin
  if FUpdating then Exit;
  if FWidget <> nil then FWidget.HasCaret := False;

  FWidget := AWidget;
  if FWidget <> nil then FWidget.HasCaret := True;
end;

procedure TEmulatedCaret.UpdateCaret;
var
  R: TRect;
begin
  if FUpdating then Exit;
  if (FWidget <> nil) and FWidget.Painting then Exit;
  FUpdating := True;
  try
    if FWidget <> nil then
    begin
      //DebugLn('TEmulatedCaret.UpdateCaret ' + DbgS(FPos) + ' ' + DbgS(FVisible) + ' ' + DbgS(FVisibleState));
      R.Left := FPos.x;
      R.Top := FPos.y;
      R.Right := R.Left + FWidth + 2;
      R.Bottom := R.Top + FHeight + 2;
      
      if not EqualRect(FOldRect, R) then FWidget.Invalidate(@FOldRect);
      FWidget.Invalidate(@R);
      FWidget.Update;
        
      FOldRect := R;
    end;
  finally
    FUpdating := False;
  end;
end;


finalization

  DestroyGlobalCaret;

end.
