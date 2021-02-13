{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 Authors: Maciej Izak
          Michael W. Vogel

 Don't change DesignTimePPI of ResizeFrame (dockedresizeframe.lfm)!
 There always has to be the default (none entry = 96 PPI) value!

 Size grips:           1
                0 +----+----+ 2
                  |         |
                7 +         + 3
                  |         |
                6 +----+----+ 4
                       5
 Only grips 3, 4, and 5 are used for sizing

}

unit DockedResizeFrame;

{$mode objfpc}{$H+}
{ $define DEBUGDOCKEDFORMEDITOR}

interface

uses
  // RTL
  Classes, Types, SysUtils, FPCanvas,
  // LCL
  Forms, ExtCtrls, StdCtrls, Controls, LCLType, Menus, Graphics, LCLIntf,
  LMessages, LCLProc,
  // DockedFormEditor
  DockedOptionsIDE, DockedDesignForm;

type

  { TResizeFrame }

  TResizeFrame = class(TFrame)
    PanelAnchorContainer: TPanel;
    PanelBarBottomLeft: TPanel;
    PanelBarLeftTop: TPanel;
    PanelBarRightBottom: TPanel;
    PanelBarBottomRight: TPanel;
    PanelBarLeftBottom: TPanel;
    PanelBarTopRight: TPanel;
    PanelBarRightTop: TPanel;
    PanelGripLeftCenter: TPanel;
    PanelGripBottomRight: TPanel;
    PanelGripBottomCenter: TPanel;
    PanelGripBottomLeft: TPanel;
    PanelGripTopRight: TPanel;
    PanelGripRightCenter: TPanel;
    PanelResizer: TPanel;
    PanelBarTopLeft: TPanel;
    PanelGripTopLeft: TPanel;
    PanelGripTopCenter: TPanel;
    PanelFormContainer: TPanel;
    PanelFakeMenu: TPanel;
    PanelBackground: TPanel;
    PanelFormClient: TPanel;
    ShapeGripLeftCenter: TShape;
    ShapeGripBottomRight: TShape;
    ShapeGripBottomCenter: TShape;
    ShapeGripBottomLeft: TShape;
    ShapeGripTopRight: TShape;
    ShapeGripRightCenter: TShape;
    ShapeGripTopLeft: TShape;
    ShapeGripTopCenter: TShape;
    procedure PanelBarPaint(Sender: TObject);
    procedure PanelFakeMenuPaint(Sender: TObject);
  private const
    SIZER_GRIP_SIZE = 8;
  private
    FBitmapBarActive: TBitmap;
    FBitmapBarInactive: TBitmap;
    FDesignForm: TDesignForm;
    FDesignerModified: Boolean;
    FFakeFocusControl: TWinControl;
    FHorzScrollPos: Integer;
    FNewFormSize: TPoint;
    FOldFakeMenuNeeded: Boolean;
    FOldMousePos: TPoint;
    FOldResizerBounds: TRect;
    FOnResized: TNotifyEvent;
    FResizing: Boolean;
    FSizerGripSize: Integer;
    FVertScrollPos: Integer;

    procedure ActivateResizeGrip(APanel: TPanel; ACursor: TCursor);
    procedure ActivateResizeGrips;
    procedure AdjustFormContainer;
    procedure AppOnIdle(Sender: TObject; var {%H-}Done: Boolean);
    procedure BeginFormSizeUpdate(Sender: TObject);
    procedure CreateBarBitmaps;
    function  CurrentSizingOffset(Sender: TObject): TPoint;
    procedure EndFormSizeUpdate(Sender: TObject);
    procedure FakeExitEnter(Sender: TObject);
    procedure FakeKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FakeKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    function  FakeMenuNeeded: Boolean;
    procedure FakeUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    function  GetMenuHeight: Integer;
    function  IsHorzSizer(Sender: TObject): Boolean;
    function  IsVertSizer(Sender: TObject): Boolean;
    procedure RefreshAnchorDesigner;
    procedure SizerMouseDown(Sender: TObject; {%H-}Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure SizerMouseMove(Sender: TObject; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure SizerMouseUp(Sender: TObject; {%H-}Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure SetDesignForm(const AValue: TDesignForm);
    procedure TryBoundDesignForm;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure AdjustPanelResizer;
    procedure ClientChangeBounds(Sender: TObject); overload;
    procedure DesignerSetFocus;
    procedure OnModified;
    function IsFocused: Boolean;
  public
    property DesignForm: TDesignForm read FDesignForm write SetDesignForm;
    property HorzScrollPos: Integer read FHorzScrollPos write FHorzScrollPos;
    property VertScrollPos: Integer read FVertScrollPos write FVertScrollPos;
    property NewFormSize: TPoint read FNewFormSize;
    property Resizing: Boolean read FResizing;
    property OnResized: TNotifyEvent read FOnResized write FOnResized;
    property SizerGripSize: Integer read FSizerGripSize;
  end;

implementation

{$R *.lfm}

{ TResizerFrame }

procedure TResizeFrame.PanelBarPaint(Sender: TObject);
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

procedure TResizeFrame.PanelFakeMenuPaint(Sender: TObject);
var
  MenuRect: Types.TRect;
  Menu: TMainMenu;
  X, Y, I: Integer;
  LCanvas: TCanvas;
begin
  if not FakeMenuNeeded then Exit;

  MenuRect := PanelFakeMenu.ClientRect;
  LCanvas := PanelFakeMenu.Canvas;
  LCanvas.Brush.Color := clMenuBar;
  LCanvas.FillRect(MenuRect);

  Menu := FDesignForm.Form.Menu;
  LCanvas.Font.Color := clMenuText;

  X := 5;
  Y := (MenuRect.Top+MenuRect.Bottom-LCanvas.TextHeight('Hg')) div 2;
  for I := 0 to Menu.Items.Count-1 do
    if Menu.Items[I].Visible then
    begin
      LCanvas.TextOut(X, Y, Menu.Items[I].Caption);
      Inc(X, LCanvas.TextWidth(Menu.Items[I].Caption) + 10);
    end;
end;

procedure TResizeFrame.ActivateResizeGrip(APanel: TPanel; ACursor: TCursor);
begin
  APanel.OnMouseDown := @SizerMouseDown;
  APanel.OnMouseMove := @SizerMouseMove;
  APanel.OnMouseUp := @SizerMouseUp;
  APanel.Cursor := ACursor;
end;

procedure TResizeFrame.ActivateResizeGrips;
begin
  ActivateResizeGrip(PanelBarRightTop, crSizeWE);
  ActivateResizeGrip(PanelGripRightCenter, crSizeWE);
  ActivateResizeGrip(PanelBarRightBottom, crSizeWE);
  // on mac there is no cursor for crNWSE ( https://bugs.freepascal.org/view.php?id=32194#c101876 )
  ActivateResizeGrip(PanelGripBottomRight, {$IFDEF MACOS}crSizeAll{$ELSE}crSizeNWSE{$ENDIF});
  ActivateResizeGrip(PanelBarBottomRight, crSizeNS);
  ActivateResizeGrip(PanelGripBottomCenter, crSizeNS);
  ActivateResizeGrip(PanelBarBottomLeft, crSizeNS);
end;

procedure TResizeFrame.AdjustFormContainer;
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
             + Abs(FDesignForm.Form.Top)
             + FDesignForm.ClientOffset.Y;
  PanelFormContainer.SetBounds(LLeft, LTop, LWidth, LHeight);
  RefreshAnchorDesigner;
end;

procedure TResizeFrame.AppOnIdle(Sender: TObject; var Done: Boolean);
var
  LFakeMenuNeeded: Boolean;
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
      Application.NotifyUserInputHandler(Self, 0); // force repaint invisible components
    end else
    if LFakeMenuNeeded then
      PanelFakeMenu.Invalidate; // always repaint menu on modification
    RefreshAnchorDesigner;
    FDesignerModified := False;
  end;
end;

procedure TResizeFrame.BeginFormSizeUpdate(Sender: TObject);
begin
//  PanelBackground.Visible := False;
  FDesignForm.BeginUpdate;
end;

procedure TResizeFrame.CreateBarBitmaps;
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

function TResizeFrame.CurrentSizingOffset(Sender: TObject): TPoint;
var
  LNewPos: TPoint;
begin
  Result := Point(0, 0);
  LNewPos := Result;
  GetCursorPos(LNewPos);
  if LNewPos = FOldMousePos then Exit;
  if IsHorzSizer(Sender) then Result.X := LNewPos.X - FOldMousePos.X;
  if IsVertSizer(Sender) then Result.Y := LNewPos.Y - FOldMousePos.Y;
end;

procedure TResizeFrame.EndFormSizeUpdate(Sender: TObject);
begin
  FDesignForm.EndUpdate;
//  PanelBackground.Visible := True;
end;

procedure TResizeFrame.FakeExitEnter(Sender: TObject);
begin
  Repaint;
end;

procedure TResizeFrame.FakeKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  LWndProc: TWndMethod;
  LMsg: TLMKeyUp;
begin
  case Key of
    VK_ESCAPE:
      if Assigned(DesignForm) and Assigned(DesignForm.AnchorDesigner) and PanelAnchorContainer.Visible then
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

procedure TResizeFrame.FakeKeyUp(Sender: TObject; var Key: Word;
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

function TResizeFrame.FakeMenuNeeded: Boolean;
var
  i: Integer;
begin
  // check if MainMenu is there and designer doesn't paint it
  Result := False;
//  {$IF DEFINED(LCLWin32) OR DEFINED(LCLWin64) OR DEFINED(LCLGtk2) OR DEFINED(LCLQt) OR DEFINED(LCLQt5)}
  {$IF DEFINED(LCLQt) OR DEFINED(LCLQt5)}
    // Menu is already shown in designer
    Exit;
  {$ENDIF}
  if Assigned(FDesignForm) and Assigned(FDesignForm.Form.Menu)
  and not (csDestroying in FDesignForm.Form.Menu.ComponentState)
  and (FDesignForm.Form.Menu.Items.Count > 0)
  then
    for i := 0 to FDesignForm.Form.Menu.Items.Count - 1 do
      if FDesignForm.Form.Menu.Items[i].Visible then
        Exit(True);
end;

procedure TResizeFrame.FakeUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
begin
  FDesignForm.Form.IntfUTF8KeyPress(UTF8Key, 1, False);
end;

function TResizeFrame.GetMenuHeight: Integer;
begin
  // some WS (Gtk2) return too big SM_CYMENU, just set it according to font height
  // no problem, it is used only for the fake main menu

  {$IFDEF LCLWin32}
  Result := lclintf.GetSystemMetrics(SM_CYMENU);
  {$ELSE}
  if PanelBackground.HandleAllocated then
    Result := PanelBackground.Canvas.TextHeight('Hg') * 4 div 3
  else
    Result := 20;
  {$ENDIF}
end;

function TResizeFrame.IsHorzSizer(Sender: TObject): Boolean;
var
  LPanel: TPanel absolute Sender;
begin
  Result := False;
  if not (Sender is TPanel) then Exit;
  if LPanel = PanelBarRightTop     then Exit(True);
  if LPanel = PanelBarRightBottom  then Exit(True);
  if LPanel = PanelGripRightCenter then Exit(True);
  if LPanel = PanelGripBottomRight then Exit(True);
end;

function TResizeFrame.IsVertSizer(Sender: TObject): Boolean;
var
  LPanel: TPanel absolute Sender;
begin
  Result := False;
  if not (Sender is TPanel) then Exit;
  if LPanel = PanelBarBottomLeft    then Exit(True);
  if LPanel = PanelBarBottomRight   then Exit(True);
  if LPanel = PanelGripBottomRight  then Exit(True);
  if LPanel = PanelGripBottomCenter then Exit(True);
end;

procedure TResizeFrame.RefreshAnchorDesigner;
begin
  if Assigned(DesignForm) and Assigned(DesignForm.AnchorDesigner) and PanelAnchorContainer.Visible then
    DesignForm.AnchorDesigner.Refresh;
end;

procedure TResizeFrame.SizerMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if not Enabled then Exit;
  if not (Sender is TWinControl) then Exit;

  FResizing := True;
  BeginFormSizeUpdate(Sender);

  {$IF Defined(LCLWin32) or Defined(LCLWin64)}
  SetCapture(TWinControl(Sender).Handle);
  {$ENDIF}
  GetCursorPos(FOldMousePos);
  FOldResizerBounds := PanelResizer.BoundsRect;
end;

procedure TResizeFrame.SizerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  SizeOffset: TPoint;
begin
  if not FResizing then Exit;
  if not (Sender is TPanel) then Exit;
  SizeOffset := CurrentSizingOffset(Sender);
  PanelResizer.SetBounds(FOldResizerBounds.Left, FOldResizerBounds.Top, FOldResizerBounds.Width + SizeOffset.X, FOldResizerBounds.Height + SizeOffset.Y);

  if DockedOptions.ForceRefreshing then
  begin
    ClientChangeBounds(nil);
    if Assigned(OnResized) and PanelFormClient.Visible then
      OnResized(Sender);
  end;
end;

procedure TResizeFrame.SizerMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if not FResizing then Exit;
  FResizing := False;
  ClientChangeBounds(nil);

  Screen.Cursor := crDefault;
  {$IF Defined(LCLWin32) or Defined(LCLWin64)}
  ReleaseCapture;
  {$ENDIF}

  if Assigned(OnResized) then OnResized(Sender);
  EndFormSizeUpdate(Sender);
  Invalidate;
  DesignerSetFocus;
end;

procedure TResizeFrame.SetDesignForm(const AValue: TDesignForm);
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

procedure TResizeFrame.TryBoundDesignForm;
begin
  if DesignForm = nil then Exit;
  if FakeMenuNeeded then
    PanelFakeMenu.Height := GetMenuHeight
  else
    PanelFakeMenu.Height := 0;
end;

constructor TResizeFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FSizerGripSize := ScaleX(SIZER_GRIP_SIZE, 96);

  FFakeFocusControl := TEdit.Create(Self);
  FFakeFocusControl.Parent := Self;
  FFakeFocusControl.Top := -100;
  FFakeFocusControl.OnKeyDown := @FakeKeyDown;
  FFakeFocusControl.OnKeyUp := @FakeKeyUp;
  FFakeFocusControl.OnUTF8KeyPress := @FakeUTF8KeyPress;
  FFakeFocusControl.OnEnter := @FakeExitEnter;
  FFakeFocusControl.OnExit := @FakeExitEnter;

  ActivateResizeGrips;
  CreateBarBitmaps;

  PanelFormClient.OnChangeBounds := @ClientChangeBounds;
  PanelAnchorContainer.OnChangeBounds := @ClientChangeBounds;
  AdjustPanelResizer;
end;

destructor TResizeFrame.Destroy;
begin
  DesignForm := nil;
  FBitmapBarInactive.Free;
  FBitmapBarActive.Free;
  inherited Destroy;
end;

procedure TResizeFrame.AdjustPanelResizer;
var
  LWidth, LHeight: Integer;
begin
  if FDesignForm = nil then Exit;
  LWidth := FDesignForm.Width + 2 * SizerGripSize;
  LHeight := FDesignForm.Height + 2 * SizerGripSize + PanelFakeMenu.Height;
  {$IFDEF DEBUGDOCKEDFORMEDITOR} DebugLn('TResizeFrame.AdjustPanelResizer: New Resizer Panel Width:', DbgS(Width), ' Height: ', DbgS(Height)); {$ENDIF}
  PanelResizer.SetBounds(-FHorzScrollPos, -FVertScrollPos, LWidth, LHeight);
  AdjustFormContainer;
end;

procedure TResizeFrame.ClientChangeBounds(Sender: TObject);
begin
  if (DesignForm = nil) then Exit;
  if not DockedOptions.ForceRefreshing and Resizing then Exit;
  {$IFDEF DEBUGDOCKEDFORMEDITOR} DebugLn('TResizeFrame.ClientChangeBounds Form Width:', DbgS(PanelFormClient.Width), ' Height: ', DbgS(PanelFormClient.Height)); {$ENDIF}
  if PanelFormClient.Visible then
  begin
    FNewFormSize.X := PanelFormClient.Width;
    FNewFormSize.Y := PanelFormClient.Height;
  end else if PanelAnchorContainer.Visible then
  begin
    FNewFormSize.X := PanelAnchorContainer.Width;
    FNewFormSize.Y := PanelAnchorContainer.Height;
  end;
end;

procedure TResizeFrame.DesignerSetFocus;
begin
  if FFakeFocusControl.CanSetFocus then
    FFakeFocusControl.SetFocus;
end;

procedure TResizeFrame.OnModified;
begin
  FDesignerModified := True;
  Invalidate;
end;

function TResizeFrame.IsFocused: Boolean;
begin
  Result := FFakeFocusControl.Focused;
end;

end.

