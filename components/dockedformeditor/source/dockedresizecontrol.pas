{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 Authors: Maciej Izak
          Michael W. Vogel

}

unit DockedResizeControl;

{$mode objfpc}{$H+}
{ $define DEBUGDOCKEDFORMEDITOR}

interface

uses
  // RTL
  Classes, Types, SysUtils, FPCanvas,
  // LCL
  Forms, ExtCtrls, StdCtrls, Controls, ComCtrls, LCLType, Menus, Graphics, LCLIntf,
  LMessages, LCLProc, Buttons,
  // DockedFormEditor
  DockedOptionsIDE, DockedDesignForm, DockedGrip;

type

  { TResizeControl }

  TResizeControl = class(TWinControl)
  private
    FBitmapBarActive: TBitmap;
    FBitmapBarInactive: TBitmap;
    FDesignForm: TDesignForm;
    FDesignerModified: Boolean;
    FFakeFocusControl: TWinControl;
    FNewFormSize: TPoint;
    FOldBounds: TRect;
    FOldFakeMenuNeeded: Boolean;
    FOldMousePos: TPoint;
    FOnResized: TNotifyEvent;
    FResizeContainer: TResizeContainer;
    FResizing: Boolean;

    procedure AdjustFormContainer;
    procedure AppOnIdle(Sender: TObject; var {%H-}Done: Boolean);
    procedure BeginFormSizeUpdate(Sender: TObject);
    procedure CreateBarBitmaps;
    function  CurrentSizingOffset(Sender: TObject): TPoint;
    procedure EndFormSizeUpdate(Sender: TObject);
    procedure FakeExitEnter(Sender: TObject);
    procedure FakeKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FakeKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FakeMenuEnter(Sender: TObject);
    function  FakeMenuNeeded: Boolean;
    procedure FakeUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    function  GetAnchorContainer: TWinControl;
    function  GetFakeMenu: TToolBar;
    function  GetFormClient: TWinControl;
    function  GetFormContainer: TResizeFormContainer;
    function  GetSizerGripSize: Integer;
    procedure RefreshAnchorDesigner;
    procedure ResizeBarPaint(Sender: TObject);
    procedure SizerMouseDown(Sender: TObject; {%H-}Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure SizerMouseMove(Sender: TObject; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure SizerMouseUp(Sender: TObject; {%H-}Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure SetDesignForm(const AValue: TDesignForm);
    procedure TryBoundDesignForm;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure AdjustBounds(ScrollOffset: TPoint);
    procedure ClientChangeBounds(Sender: TObject); overload;
    procedure DesignerSetFocus;
    procedure OnModified;
    function  IsFocused: Boolean;
  public
    property AnchorContainer: TWinControl read GetAnchorContainer;
    property DesignForm: TDesignForm read FDesignForm write SetDesignForm;
    property FakeMenu: TToolBar read GetFakeMenu;
    property FormClient: TWinControl read GetFormClient;
    property FormContainer: TResizeFormContainer read GetFormContainer;
    property NewFormSize: TPoint read FNewFormSize;
    property OnResized: TNotifyEvent read FOnResized write FOnResized;
    property Resizing: Boolean read FResizing;
    property SizerGripSize: Integer read GetSizerGripSize;
  end;

implementation

{ TResizerFrame }

procedure TResizeControl.AdjustFormContainer;
var
  LLeft, LTop, LWidth, LHeight: Integer;
begin
  LLeft   := - FDesignForm.Form.Left        // real form left - aka Form1.Left in OI
             - FDesignForm.ClientOffset.X;  // offset of frame of form to client rect
  LTop    := - FDesignForm.Form.Top
             - FDesignForm.ClientOffset.Y;
  LWidth  :=   FDesignForm.Form.Width
             + Abs(FDesignForm.Form.Left)
             + FDesignForm.ClientOffset.X;
  LHeight :=   FDesignForm.Form.Height
             + FakeMenu.Height
             + Abs(FDesignForm.Form.Top)
             + FDesignForm.ClientOffset.Y;
  FormContainer.SetBounds(LLeft, LTop, LWidth, LHeight);
  RefreshAnchorDesigner;
end;

procedure TResizeControl.AppOnIdle(Sender: TObject; var Done: Boolean);
var
  LFakeMenuNeeded: Boolean;
  Mess : TLMessage;
begin
  if FDesignerModified then
  begin
    LFakeMenuNeeded := FakeMenuNeeded;
    if LFakeMenuNeeded <> FOldFakeMenuNeeded then
    begin
      FOldFakeMenuNeeded := LFakeMenuNeeded;
      TryBoundDesignForm;
      if Assigned(OnResized) then
        OnResized(Self);
      FillChar(Mess,SizeOf(Mess),0);
      Mess.Msg := 0;
      Application.NotifyUserInputHandler(Self, Mess); // force repaint invisible components
    end else
    if LFakeMenuNeeded then
      TryBoundDesignForm; // always repaint menu on modification
    RefreshAnchorDesigner;
    FDesignerModified := False;
  end;
end;

procedure TResizeControl.BeginFormSizeUpdate(Sender: TObject);
begin
  FDesignForm.BeginUpdate;
end;

procedure TResizeControl.CreateBarBitmaps;
begin
  FBitmapBarActive := TBitmap.Create;
  FBitmapBarActive.SetSize(2, 2);
  FBitmapBarActive.Canvas.Pixels[0, 0] := DockedOptions.ResizerColor;
  FBitmapBarActive.Canvas.Pixels[0, 1] := clBtnFace;
  FBitmapBarActive.Canvas.Pixels[1, 0] := clBtnFace;
  FBitmapBarActive.Canvas.Pixels[1, 1] := DockedOptions.ResizerColor;

  FBitmapBarInactive := TBitmap.Create;
  FBitmapBarInactive.SetSize(2, 2);
  FBitmapBarInactive.Canvas.Pixels[0, 0] := clGray;
  FBitmapBarInactive.Canvas.Pixels[0, 1] := clBtnFace;
  FBitmapBarInactive.Canvas.Pixels[1, 0] := clBtnFace;
  FBitmapBarInactive.Canvas.Pixels[1, 1] := clGray;
end;

function TResizeControl.CurrentSizingOffset(Sender: TObject): TPoint;
var
  LNewPos: TPoint;
begin
  Result := Point(0, 0);
  LNewPos := Result;
  GetCursorPos(LNewPos);
  if LNewPos = FOldMousePos then Exit;
  if FResizeContainer.IsHorzSizer(Sender) then Result.X := LNewPos.X - FOldMousePos.X;
  if FResizeContainer.IsVertSizer(Sender) then Result.Y := LNewPos.Y - FOldMousePos.Y;
end;

procedure TResizeControl.EndFormSizeUpdate(Sender: TObject);
begin
  FDesignForm.EndUpdate;
end;

procedure TResizeControl.FakeExitEnter(Sender: TObject);
begin
  Repaint;
end;

procedure TResizeControl.FakeKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  LWndProc: TWndMethod;
  LMsg: TLMKeyUp;
begin
  case Key of
    VK_ESCAPE:
      if Assigned(DesignForm) and Assigned(DesignForm.AnchorDesigner) and AnchorContainer.Visible then
      begin
        DesignForm.AnchorDesigner.Abort;
        Exit;
      end;
  end;

  LWndProc := FDesignForm.Form.WindowProc;
  FillChar(LMsg{%H-}, SizeOf(LMsg), 0);
  LMsg.msg := CN_KEYDOWN;
  LMsg.CharCode := Key;
  LWndProc(TLMessage(LMsg));
  Key := LMsg.CharCode;
end;

procedure TResizeControl.FakeKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  LWndProc: TWndMethod;
  LMsg: TLMKeyUp;
begin
  LWndProc := FDesignForm.Form.WindowProc;
  FillChar(LMsg{%H-}, SizeOf(LMsg), 0);
  LMsg.msg := CN_KEYUP;
  LMsg.CharCode := Key;
  LWndProc(TLMessage(LMsg));
  Key := LMsg.CharCode;
end;

function TResizeControl.FakeMenuNeeded: Boolean;
begin
  // check if MainMenu is there and designer doesn't paint it
  Result := False;
  if not Assigned(FDesignForm) then Exit;
  Result := FDesignForm.MainMenuFaked;
  if Result then FakeMenu.Menu := FDesignForm.Form.Menu else FakeMenu.Menu := nil;
end;

procedure TResizeControl.FakeUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
begin
  FDesignForm.Form.IntfUTF8KeyPress(UTF8Key, 1, False);
end;

procedure TResizeControl.FakeMenuEnter(Sender: TObject);
begin
  DesignerSetFocus;
end;

function TResizeControl.GetAnchorContainer: TWinControl;
begin
  Result := FResizeContainer.AnchorContainer;
end;

function TResizeControl.GetFakeMenu: TToolBar;
begin
  Result := FResizeContainer.FakeMenu;
end;

function TResizeControl.GetFormClient: TWinControl;
begin
  Result := FResizeContainer.FormClient;
end;

function TResizeControl.GetFormContainer: TResizeFormContainer;
begin
  Result := FResizeContainer.FormContainer;
end;

function TResizeControl.GetSizerGripSize: Integer;
begin
  Result := FResizeContainer.ResizeGrips.GripSize;
end;

procedure TResizeControl.RefreshAnchorDesigner;
begin
  if Assigned(DesignForm) and Assigned(DesignForm.AnchorDesigner) and AnchorContainer.Visible then
    DesignForm.AnchorDesigner.Refresh;
end;

procedure TResizeControl.ResizeBarPaint(Sender: TObject);
var
  LPanel: TPanel;
begin
  if FResizing then Exit;
  if not (Sender is TPanel) then Exit;
  LPanel := TPanel(Sender);
  LPanel.Canvas.Brush.Style := bsImage;
  if IsFocused then
    LPanel.Canvas.Brush.Bitmap := FBitmapBarActive
  else
    LPanel.Canvas.Brush.Bitmap := FBitmapBarInactive;
  LPanel.Canvas.FillRect(1, 1, LPanel.ClientWidth - 1, LPanel.ClientHeight - 1);
end;

procedure TResizeControl.SizerMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if not Enabled then Exit;
  if not (Sender is TWinControl) then Exit;

  FResizing := True;
  BeginFormSizeUpdate(Sender);

  {$IFDEF LCLWin32}
  SetCapture(TWinControl(Sender).Handle);
  {$ENDIF}
  GetCursorPos(FOldMousePos);
  FOldBounds := FResizeContainer.BoundsRect;
end;

procedure TResizeControl.SizerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  SizeOffset: TPoint;
begin
  if not FResizing then Exit;
  if not (Sender is TPanel) then Exit;
  SizeOffset := CurrentSizingOffset(Sender);
  FResizeContainer.SetBounds(FOldBounds.Left, FOldBounds.Top, FOldBounds.Width + SizeOffset.X, FOldBounds.Height + SizeOffset.Y);

  if DockedOptions.ForceRefreshing then
  begin
    ClientChangeBounds(nil);
    if Assigned(OnResized) and FormClient.Visible then
      OnResized(Sender);
  end;
end;

procedure TResizeControl.SizerMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if not FResizing then Exit;
  FResizing := False;
  ClientChangeBounds(nil);

  Screen.Cursor := crDefault;
  {$IFDEF LCLWin32}
  ReleaseCapture;
  {$ENDIF}

  if Assigned(OnResized) then OnResized(Sender);
  EndFormSizeUpdate(Sender);
  Invalidate;
  DesignerSetFocus;
end;

procedure TResizeControl.SetDesignForm(const AValue: TDesignForm);
begin
  FDesignForm := AValue;
  if Assigned(AValue) then
  begin
    // special for QT (at start "design form" has wrong position)
    TryBoundDesignForm;
    Application.AddOnIdleHandler(@AppOnIdle);
  end else
    Application.RemoveOnIdleHandler(@AppOnIdle);
end;

procedure TResizeControl.TryBoundDesignForm;
var
  f: Boolean;
begin
  if DesignForm = nil then Exit;
  f := FakeMenuNeeded;
  FakeMenu.AutoSize := f;
  if not f then
    FakeMenu.Height := 0;
end;

constructor TResizeControl.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FResizeContainer := TResizeContainer.Create(Self);
  FResizeContainer.ResizeGrips.OnMouseDown := @SizerMouseDown;
  FResizeContainer.ResizeGrips.OnMouseMove := @SizerMouseMove;
  FResizeContainer.ResizeGrips.OnMouseUp := @SizerMouseUp;
  FResizeContainer.ResizeBars.OnMouseDown := @SizerMouseDown;
  FResizeContainer.ResizeBars.OnMouseMove := @SizerMouseMove;
  FResizeContainer.ResizeBars.OnMouseUp := @SizerMouseUp;
  FResizeContainer.ResizeBars.OnPaint := @ResizeBarPaint;
  FResizeContainer.FakeMenu.OnEnter  := @FakeMenuEnter;

  FFakeFocusControl := TEdit.Create(Self);
  FFakeFocusControl.Parent := Self;
  FFakeFocusControl.Top := -100;
  FFakeFocusControl.OnKeyDown := @FakeKeyDown;
  FFakeFocusControl.OnKeyUp := @FakeKeyUp;
  FFakeFocusControl.OnUTF8KeyPress := @FakeUTF8KeyPress;
  FFakeFocusControl.OnEnter := @FakeExitEnter;
  FFakeFocusControl.OnExit := @FakeExitEnter;

  CreateBarBitmaps;

  FormClient.OnChangeBounds := @ClientChangeBounds;
  AnchorContainer.OnChangeBounds := @ClientChangeBounds;
  AdjustBounds(Point(0, 0));
end;

destructor TResizeControl.Destroy;
begin
  DesignForm := nil;
  FBitmapBarInactive.Free;
  FBitmapBarActive.Free;
  inherited Destroy;
end;

procedure TResizeControl.AdjustBounds(ScrollOffset: TPoint);
var
  LWidth, LHeight: Integer;
begin
  if FDesignForm = nil then Exit;
  TryBoundDesignForm;
  LWidth := FDesignForm.Width + 2 * SizerGripSize;
  LHeight := FDesignForm.Height + 2 * SizerGripSize + FakeMenu.Height;
  {$IFDEF DEBUGDOCKEDFORMEDITOR} DebugLn('TResizeControl.AdjustBounds: New ResizeControl Width:', DbgS(Width), ' Height: ', DbgS(Height)); {$ENDIF}
  FResizeContainer.SetBounds(-ScrollOffset.x, -ScrollOffset.y, LWidth, LHeight);
  AdjustFormContainer;
end;

procedure TResizeControl.ClientChangeBounds(Sender: TObject);
begin
  if (DesignForm = nil) then Exit;
  if not DockedOptions.ForceRefreshing and Resizing then Exit;
  {$IFDEF DEBUGDOCKEDFORMEDITOR} DebugLn('TResizeControl.ClientChangeBounds Form Width:', DbgS(FormClient.Width), ' Height: ', DbgS(FormClient.Height)); {$ENDIF}
  TryBoundDesignForm;
  if FormClient.Visible then
  begin
    FNewFormSize.X := FormClient.Width;
    FNewFormSize.Y := FormClient.Height;
  end else if AnchorContainer.Visible then
  begin
    FNewFormSize.X := AnchorContainer.Width;
    FNewFormSize.Y := AnchorContainer.Height;
  end;
end;

procedure TResizeControl.DesignerSetFocus;
begin
  if FFakeFocusControl.CanSetFocus then
    FFakeFocusControl.SetFocus;
end;

procedure TResizeControl.OnModified;
begin
  FDesignerModified := True;
  Invalidate;
end;

function TResizeControl.IsFocused: Boolean;
begin
  Result := FFakeFocusControl.Focused;
end;

end.

