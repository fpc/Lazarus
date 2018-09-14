{
 /***************************************************************************
                    CocoaCaret.pas  -  Cocoa Caret Emulation
                    ------------------------------------------

 copyright (c) Andreas Hausladen

 adopted for Lazarus and Cocoa by Lazarus Team

 ***************************************************************************/

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

unit CocoaCaret;
{$mode objfpc}{$H+}
{$modeswitch objectivec1}

interface

uses
  // Bindings
  CocoaAll,
  // Free Pascal
  Classes, SysUtils, Types,
  // Widgetset
  CocoaGDIObjects, CocoaPrivate;

type

  { TEmulatedCaret }

  TEmulatedCaret = class(TObject)
  private
    FTimerTarget: NSObject;
    FTimer: NSTimer;
    FOldRect: TRect;
    FView: NSView;
    FBitmap: TCocoaBitmap;
    FWidth, FHeight: Integer;
    FPos: TPoint;
    FVisible: Boolean;
    FVisibleState: Boolean;
    FWidgetSetReleased: Boolean;
    FHideCount: Integer;
    procedure SetPos(const Value: TPoint);
    procedure ResetTimer;
  protected
    procedure DoTimer(Sender: TObject);
    procedure DrawCaret; virtual;
    procedure SetView(AView: NSView);
    procedure InvalidateView;
  public
    constructor Create;
    destructor Destroy; override;
    
    function CreateCaret(AView: NSView; Bitmap: PtrUInt; Width, Height: Integer): Boolean;
    function DestroyCaret: Boolean;

    function IsValid: Boolean;

    function Show: Boolean;
    function Hide: Boolean;

    property Pos: TPoint read FPos write SetPos;
  end;

function CreateCaret(View: NSView; Bitmap: PtrUInt; Width, Height: Integer): Boolean; overload;
function HideCaret(View: NSView): Boolean;
function ShowCaret(View: NSView): Boolean;
function SetCaretPos(X, Y: Integer): Boolean;
function GetCaretPos(var P: TPoint): Boolean;
function DestroyCaret(View: NSView): Boolean;
procedure DrawCaret;
procedure DestroyGlobalCaret;

implementation

type
  { TCaretTimerTarget }

  TCaretTimerTarget = objcclass(NSObject)
    fCaret: TEmulatedCaret;
    procedure CaretEvent(sender: id); message 'CaretEvent:';
  end;

var
  GlobalCaret: TEmulatedCaret = nil;

procedure GlobalCaretNeeded;
begin
  if GlobalCaret = nil then GlobalCaret := TEmulatedCaret.Create;
end;

procedure DrawCaret;
begin
  GlobalCaretNeeded;
  if Assigned(GlobalCaret) then
    GlobalCaret.DrawCaret;
end;

procedure DestroyGlobalCaret;
begin
  FreeAndNil(GlobalCaret);
end;

function CreateCaret(View: NSView; Bitmap: PtrUInt; Width, Height: Integer): Boolean;
begin
  GlobalCaretNeeded;

  if Assigned(GlobalCaret) then
    Result := GlobalCaret.CreateCaret(View, Bitmap, Width, Height);
end;

function GetCaretBlinkTime: Cardinal;
begin
  // TODO: use MacOSAll.GetCaretTime
  Result := 600; // our default value
end;

function HideCaret(View: NSView): Boolean;
begin
  Result := False;
  GlobalCaretNeeded;
  
  if Assigned(GlobalCaret) then
  begin
    Result := not Assigned(View) or (View = GlobalCaret.FView);
    if Result then
      Result := GlobalCaret.Hide;
  end;
end;

function ShowCaret(View: NSView): Boolean;
begin
  //writeln('ShowCaret: ', PtrUInt(View));
  Result := False;
  GlobalCaretNeeded;

  if Assigned(GlobalCaret) then
  begin
    Result := not Assigned(View) or (view = GlobalCaret.FView);
    if Result then
      Result := GlobalCaret.Show;
  end;
end;

function SetCaretPos(X, Y: Integer): Boolean;
begin
  Result := True;
  GlobalCaretNeeded;
  if Assigned(GlobalCaret) then
    GlobalCaret.Pos := Classes.Point(X, Y);
end;

function GetCaretPos(var P: TPoint): Boolean;
begin
  Result := True;
  GlobalCaretNeeded;
  
  if Assigned(GlobalCaret) then
  begin
    with GlobalCaret.Pos do
    begin
      P.x := X;
      P.y := Y;
    end;
  end;
end;

function DestroyCaret(View: NSView): Boolean;
begin
  Result := False;
   
  if Assigned(GlobalCaret) then
  begin
    Result := not Assigned(View) or (GlobalCaret.FView = View);
    if Result then
      Result := GlobalCaret.DestroyCaret;
  end;
end;

procedure CocoaDisableTimer(var ATimer: NSTimer);
begin
  if not Assigned(ATimer) then Exit;
  ATimer.invalidate;
  ATimer := nil;
end;

function CocoaEnableTimer(trg: id): NSTimer;
begin
  Result:=NSTimer.scheduledTimerWithTimeInterval_target_selector_userInfo_repeats(
    GetCaretBlinkTime / 1000, trg, ObjCSelector('CaretEvent:'), nil, true);
end;

{ TCaretTimerTarget }

procedure TCaretTimerTarget.CaretEvent(sender: id);
begin
  if not Assigned(fCaret) then Exit;
  fCaret.DoTimer(nil);
end;

{ TEmulatedCaret }

constructor TEmulatedCaret.Create;
begin
  inherited Create;

  FOldRect := Rect(0, 0, 1, 1);

  FTimerTarget := TCaretTimerTarget.alloc.init;
  TCaretTimerTarget(FTimerTarget).fCaret := Self;
end;

destructor TEmulatedCaret.Destroy;
begin
  DestroyCaret;
  FTimerTarget.release;

  inherited Destroy;
end;

function TEmulatedCaret.CreateCaret(AView: NSView; Bitmap: PtrUInt;
  Width, Height: Integer): Boolean;
begin
  DestroyCaret;
  SetView(AView);

  FVisible := false;
  FHideCount := 0;

  FWidth := Width;
  FHeight := Height;
  if Bitmap > 1 then
    FBitmap := TCocoaBitmap.Create(TCocoaBitmap(Bitmap))
  else
    FBitmap := nil;

  Result := IsValid;
end;

function TEmulatedCaret.DestroyCaret: Boolean;
begin
  if Assigned(FView) then
  begin
    InvalidateView;
    if Assigned(FView.lclGetCallback) then
      FView.lclGetCallback.SetHasCaret(false);
  end;

  CocoaDisableTimer(FTimer);
  
  FreeAndNil(FBitmap);
  FView := nil;
  FWidth := 0;
  FHeight := 0;
  Result := not IsValid;
end;

procedure TEmulatedCaret.DrawCaret;
begin
  //DebugLn('DrawCaret ' + DbgSName(FView.LCLObject) + ' ' + DbgS(FPos) + ' ' + DbgS(FVisible) + ' ' + DbgS(FVisibleState));
  //writeln('draw ', FHideCount);
  if IsValid and FVisible and FVisibleState and FView.lclIsPainting and (FHideCount = 0) then
  begin
    if FBitmap = nil then
      FView.lclGetCallback.GetContext.InvertRectangle(FPos.X, FPos.Y,
        FPos.X + FWidth, FPos.Y + FHeight)
    else
      FView.lclGetCallback.GetContext.DrawBitmap(FPos.X, FPos.Y,
        FBitmap);
  end;
end;

function TEmulatedCaret.Show: Boolean;
begin
  //writeln('car: ', (AView = FView),' ',(IsValid),' ',Assigned(FView));
  Result := (IsValid) and Assigned(FView);
  if not Result then Exit;

  if (FHideCount > 0) then dec(FHideCount);
  //DebugLn('ShowCaret ' + DbgSName(AView.LCLObject));

  if not FVisible then
  begin
    // was not previously visible
    InvalidateView;
    FVisible := True;
    FVisibleState := true;
  end;

  if not Assigned(FTimer) then ResetTimer;
end;

function TEmulatedCaret.Hide: Boolean;
begin
  Result := IsValid;

  // inside of paint, there's no need to stop timer and invalidate the drawing
  inc(FHideCount);
  if Assigned(FView) and (FView.lclIsPainting) then Exit;

  if FVisible then
  begin
    CocoaDisableTimer(FTimer);
    FVisible := False;
    InvalidateView;
  end;
end;

procedure TEmulatedCaret.SetPos(const Value: TPoint);
begin
  //DebugLn('SetCaretPos ' + DbgSName(FView.LCLObject));
  if FView = nil then
  begin
    FPos.X := 0;
    FPos.Y := 0;
    Exit;
  end;
  
  if ((FPos.x <> Value.x) or (FPos.y <> Value.y)) then
  begin
    FPos := Value;
    // the caret must remain visible while changing position
    FVisibleState := True;
    ResetTimer;
    if not FView.lclIsPainting then InvalidateView;
  end;
end;

procedure TEmulatedCaret.DoTimer(Sender: TObject);
begin
  FVisibleState := not FVisibleState;
  if FVisible then InvalidateView;
end;

function TEmulatedCaret.IsValid: Boolean;
begin
  Result := (FWidth > 0) and (FHeight > 0) and (FView <> nil) and FView.lclIsVisible
    and Assigned(FView.lclGetTarget);
end;

procedure TEmulatedCaret.SetView(AView: NSView);
begin
  FView := AView;
  if FView <> nil then FView.lclGetCallback.HasCaret := True;
  CocoaDisableTimer(FTimer);
  if Assigned(FView) then
    FTimer:=CocoaEnableTimer(FTimerTarget);
end;

procedure TEmulatedCaret.InvalidateView;
var
  R: TRect;
begin
  if (FView = nil) or FWidgetSetReleased then Exit;
  if FView.lclIsPainting then Exit;
  if not IsValid then Exit;

  //DebugLn('UpdateCaret ' + DbgSName(FView.LCLObject) + ' ' + DbgS(FPos) + ' ' + DbgS(FVisible) + ' ' + DbgS(FVisibleState));
  R.Left := FPos.x;
  R.Top := FPos.y;
  R.Right := R.Left + FWidth + 2;
  R.Bottom := R.Top + FHeight + 2;
  
  if not EqualRect(FOldRect, R) then FView.lclInvalidateRect(FOldRect);
  FView.lclInvalidateRect(R);
    
  FOldRect := R;
end;

procedure TEmulatedCaret.ResetTimer;
begin
  CocoaDisableTimer(FTimer);
  FTimer:=CocoaEnableTimer(FTimerTarget);
end;

finalization
  DestroyGlobalCaret;

end.
