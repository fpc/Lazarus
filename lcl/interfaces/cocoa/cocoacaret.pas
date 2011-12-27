{
 /***************************************************************************
                    CocoaCaret.pas  -  Cocoa Caret Emulation
                    ------------------------------------------

 copyright (c) Andreas Hausladen

 adopted for Lazarus and Cocoa by Lazarus Team

 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

unit CocoaCaret;
{$mode objfpc}{$H+}

interface

uses
  // Bindings
  MacOSAll,
  // Free Pascal
  Classes, SysUtils, Types,
  // Widgetset
  CarbonDef, CarbonGDIObjects, CarbonInt,
  // LCL
  LCLType, LCLIntf, LCLProc, Graphics, ExtCtrls, Forms;

type
  { TEmulatedCaret }

  TEmulatedCaret = class(TComponent)
  private
    FTimer: TTimer;
    FOldRect: TRect;
    FWidget: TCarbonWidget;
    FBitmap: TCarbonBitmap;
    FWidth, FHeight: Integer;
    FPos: TPoint;
    FVisible: Boolean;
    FVisibleRealized: Boolean;
    FVisibleState: Boolean;
    FRespondToFocus: Boolean;
    FCaretCS: System.PRTLCriticalSection;
    FWidgetSetReleased: Boolean;
    procedure SetPos(const Value: TPoint);
    procedure UpdateCall(Data: PtrInt);
  protected
    procedure DoTimer(Sender: TObject);
    procedure DrawCaret; virtual;
    procedure SetWidget(AWidget: TCarbonWidget);
    procedure UpdateCaret;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    
    procedure Lock;
    procedure UnLock;

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
//todo: make a better solution for the Widgetset released before GlobalCaret
procedure CaretWidgetSetReleased;

implementation

uses CocoaGDIObjects;

var
  GlobalCaret: TEmulatedCaret = nil;
  
procedure GlobalCaretNeeded;
begin
  if GlobalCaret = nil then GlobalCaret := TEmulatedCaret.Create(nil);
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
      GlobalCaret.UnLock;
    end;
  end;
end;

procedure DestroyGlobalCaret;
begin
  FreeAndNil(GlobalCaret);
end;

function CreateCaret(Widget: TCarbonWidget; Bitmap: PtrUInt; Width, Height: Integer): Boolean;
begin
  GlobalCaretNeeded;

  if Assigned(GlobalCaret) then
  begin
    GlobalCaret.Lock;
    try
      Result := GlobalCaret.CreateCaret(Widget, Bitmap, Width, Height);
    finally
      GlobalCaret.UnLock;
    end;
  end;
end;

function GetCaretBlinkTime: Cardinal;
begin
  // TODO: use MacOSAll.GetCaretTime
  Result := 600; // our default value
end;

function HideCaret(Widget: TCarbonWidget): Boolean;
begin
  Result := False;
  GlobalCaretNeeded;
  
  if Assigned(GlobalCaret) then
  begin
    GlobalCaret.Lock;
    try
      Result := GlobalCaret.Hide;
    finally
      GlobalCaret.UnLock;
    end;
  end;
end;

function ShowCaret(Widget: TCarbonWidget): Boolean;
begin
  Result := False;
  GlobalCaretNeeded;

  if Assigned(GlobalCaret) then
  begin
    GlobalCaret.Lock;
    try
      Result := GlobalCaret.Show(Widget);
    finally
      GlobalCaret.UnLock;
    end;
  end;
end;

function SetCaretPos(X, Y: Integer): Boolean;
begin
  Result := True;
  GlobalCaretNeeded;

  if Assigned(GlobalCaret) then
  begin
    GlobalCaret.Lock;
    try
      GlobalCaret.Pos := Classes.Point(X, Y);
    finally
      GlobalCaret.UnLock;
    end;
  end;
end;

function GetCaretPos(var P: TPoint): Boolean;
begin
  Result := True;
  GlobalCaretNeeded;
  
  if Assigned(GlobalCaret) then
  begin
    GlobalCaret.Lock;
    try
      with GlobalCaret.Pos do
      begin
        P.x := X;
        P.y := Y;
      end;
    finally
      GlobalCaret.UnLock;
    end;
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
  Result := False;
   
  if Assigned(GlobalCaret) then
  begin
    GlobalCaret.Lock;
    try
      Result := GlobalCaret.DestroyCaret;
    finally
      GlobalCaret.UnLock;
    end;
  end;
end;

{ TEmulatedCaret }

constructor TEmulatedCaret.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FOldRect := Rect(0, 0, 1, 1);
  
  FTimer := TTimer.Create(self);
  FTimer.Enabled := False;
  FTimer.Interval := GetCaretBlinkTime;
  FTimer.OnTimer := @DoTimer;
  FVisibleRealized := True;
  
  FRespondToFocus := False;
  
  New(FCaretCS);
  System.InitCriticalSection(FCaretCS^);
end;

destructor TEmulatedCaret.Destroy;
begin
  DestroyCaret;
  
  System.DoneCriticalsection(FCaretCS^);
  Dispose(FCaretCS);

  inherited Destroy;
end;

procedure TEmulatedCaret.Lock;
begin
  System.EnterCriticalsection(FCaretCS^);
end;

procedure TEmulatedCaret.UnLock;
begin
  System.LeaveCriticalsection(FCaretCS^);
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
end;

function TEmulatedCaret.DestroyCaret: Boolean;
begin
  if FTimer<>nil then FTimer.Enabled := False;
  FVisible := False;
  FVisibleRealized := False;
  FVisibleState := False;
  UpdateCaret;
  
  if Assigned(FBitmap) then FBitmap.Free;
  FWidget := nil;
  FBitmap := nil;
  FWidth := 0;
  FHeight := 0;
  Result := not IsValid;
end;

procedure TEmulatedCaret.DrawCaret;
begin
  //DebugLn('DrawCaret ' + DbgSName(FWidget.LCLObject) + ' ' + DbgS(FPos) + ' ' + DbgS(FVisible) + ' ' + DbgS(FVisibleState));
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
begin
  Result := False;
  if AWidget = nil then Exit;

  //DebugLn('ShowCaret ' + DbgSName(AWidget.LCLObject));
  
  if FWidget <> AWidget then
  begin
    Hide;
    SetWidget(AWidget);
    
    UpdateCaret;
  end;
  
  Result := IsValid;
  
  if Result then
  begin
    if FVisible then Exit;
    
    FVisible := True;
    FTimer.Enabled := False;
    FTimer.Enabled := True;
    if FVisibleRealized then
    begin
      FVisibleState := True;
      FVisibleRealized := True;
    end;

    UpdateCaret;
  end;
end;

function TEmulatedCaret.Hide: Boolean;
begin
  Result := IsValid;
  
  if FVisible then
  begin
    FTimer.Enabled := False;
    FVisible := False;
    UpdateCaret;
    FVisibleRealized := (FWidget = nil) or not FWidget.Painting;
  end;
end;

procedure TEmulatedCaret.SetPos(const Value: TPoint);
begin
  //DebugLn('SetCaretPos ' + DbgSName(FWidget.LCLObject));
  if FWidget = nil then
  begin
    FPos.X := 0;
    FPos.Y := 0;
    Exit;
  end;
  
  if ((FPos.x <> Value.x) or (FPos.y <> Value.y)) then
  begin
    FPos := Value;
    FTimer.Enabled := False;
    FTimer.Enabled := True;
    if not FWidget.Painting then FVisibleState := True;
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
  Result := (FWidth > 0) and (FHeight > 0) and (FWidget <> nil) and FWidget.IsVisible and
    not (csDestroying in FWidget.LCLObject.ComponentState);
end;

procedure TEmulatedCaret.SetWidget(AWidget: TCarbonWidget);
begin
  //DebugLn('SetCaretWidget ', DbgSName(AWidget.LCLObject));
  if FWidget <> nil then FWidget.HasCaret := False;

  FWidget := AWidget;
  if FWidget <> nil then FWidget.HasCaret := True;
  FTimer.Enabled := False;
  FTimer.Enabled := FWidget <> nil;
end;

procedure TEmulatedCaret.UpdateCaret;
var
  R: TRect;
begin
  if (FWidget = nil) or FWidgetSetReleased then Exit;
  if FWidget.Painting then Exit;
  if not IsValid then Exit;

  //DebugLn('UpdateCaret ' + DbgSName(FWidget.LCLObject) + ' ' + DbgS(FPos) + ' ' + DbgS(FVisible) + ' ' + DbgS(FVisibleState));
  R.Left := FPos.x;
  R.Top := FPos.y;
  R.Right := R.Left + FWidth + 2;
  R.Bottom := R.Top + FHeight + 2;
  
  if not EqualRect(FOldRect, R) then FWidget.Invalidate(@FOldRect);
  FWidget.Invalidate(@R);
  FWidget.Update;
    
  FOldRect := R;
end;

procedure TEmulatedCaret.UpdateCall(Data: PtrInt);
begin
  UpdateCaret;
end;

procedure CaretWidgetSetReleased;
begin
  if Assigned(GlobalCaret) then begin
    GlobalCaret.fTimer.Free;
    GlobalCaret.fTimer:=nil;
    GlobalCaret.FWidgetSetReleased:=True;
  end;
end;

finalization
  DestroyGlobalCaret;

end.
