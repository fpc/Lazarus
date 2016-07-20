unit sparta_BasicResizeFrame;

{$mode delphi}{$H+}

interface

uses
  Classes, contnrs, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls,
  Graphics, LCLType, lclintf, Menus, LMessages, Math, Types, sparta_InterfacesMDI;

type
  TPositioningCode = (pcPositioning, pcPositioningEnd);
  TPositioningKind = set of (pkBottom, pkRight);
  TPositioningEvent = procedure(Sender: TObject; PositioningKind: TPositioningKind; PositioningCode: TPositioningCode) of object;

  { TBasicResizeFrame }

  TResizerFrameClass = class of TBasicResizeFrame;
  TBasicResizeFrame = class(TFrame, IResizeFrame)
    iResizerLineImg: TImage;
    pFakeMenu: TPanel;
    pBG: TPanel;
    pB: TPanel;
    pClient: TPanel;
    pL: TPanel;
    pMarginB: TPanel;
    pMarginL: TPanel;
    pMarginR: TPanel;
    pMarginT: TPanel;
    pR: TPanel;
    pT: TPanel;
    procedure pFakeMenuPaint(Sender: TObject);
    procedure sbVerticalScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure sbHorizontalScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
  public const
    SIZER_RECT_SIZE = 8;
    SIZER_LINE_WIDTH = 8;
  private
    FVerticalScrollPos: Integer;
    FHorizontalScrollPos: Integer;
    FDesignedForm: IDesignedForm;
    FBackground: IDesignedFormBackground;
    FFakeFocusControl: TWinControl;

    procedure FakeExitEnter(Sender: TObject);
    procedure FakeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FakeKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FakeUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
  private
    { private declarations }
    FOnNodePositioning: TPositioningEvent;
    FOnHorizontalScroll, FOnVerticalScroll: TScrollEvent;
    FLastRightMarign: Integer;
    FLastBottomMarign: Integer;
    FNodePositioning: Boolean;
    FOldPos, FDelta: TPoint;
    FPositioningKind: TPositioningKind;
    FMaxWidth, FMaxHeight: Integer;
    FLastClientWidth, FLastClientHeight: Integer;
    FOldHasMainMenu: Boolean;
    FDesignerModified: Boolean;

    function HasMainMenu: Boolean;
    procedure AppOnIdle(Sender: TObject; var Done: Boolean);

    procedure PanelPaint(Sender: TObject);
    procedure BGChangeBounds(Sender: TObject);

    procedure CreateNodes;
    procedure NodeMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure NodeMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure NodeMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    function GetRightMargin: Integer;
    function GetBottomMargin: Integer;

    // dependent on scroll position
    // for Vertical
    function BottomSizerRectHeight: Integer;
    function BottomSizerLineWidth: Integer;
    function TopSizerRectTop: Integer;
    function TopSizerLineWidth: Integer;
    function VerticalSizerLineLength: Integer;
    // for Horizontal
    function RightSizerRectWidth: Integer;
    function RightSizerLineWidth: Integer;
    function LeftSizerRectLeft: Integer;
    function LeftSizerLineWidth: Integer;
    function HorizontalSizerLineLength: Integer;

    function GetMenuHeight: Integer;
  protected
    FNodes: TObjectList;
  protected
    procedure TryBoundDesignedForm; virtual;
    procedure BeginFormSizeUpdate(Sender: TObject); virtual;
    procedure EndFormSizeUpdate(Sender: TObject); virtual;
  protected { IResizeFrame }
    procedure HideSizeRects;
    procedure ShowSizeRects;
    procedure PositionNodes; overload;
    function DesignedWidthToScroll: Integer;
    function DesignedHeightToScroll: Integer;
    procedure ClientChangeBounds; overload;

    function GetFrame: TCustomFrame;
    function GetVerticalScrollPos: Integer;
    procedure SetVerticalScrollPos(AValue: Integer);
    function GetHorizontalScrollPos: Integer;
    procedure SetHorizontalScrollPos(AValue: Integer);

    function GetBackgroundPanel: TPanel;
    function GetBackgroundMargin(const AIndex: Integer): Integer;

    function GetClientPanel: TPanel;
    function GetNodePositioning: Boolean;
    function GetDesignedForm: IDesignedForm;
    procedure SetDesignedForm(const AValue: IDesignedForm);

    function GetSizerRectSize: Integer;
    function GetSizerLineWidth: Integer;
  public { IResizeFrame }
    procedure DesignerSetFocus;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    property DesignedForm: IDesignedForm read GetDesignedForm write SetDesignedForm;

    procedure PositionNodes(AroundControl: TWinControl); overload;
    property NodePositioning: Boolean read GetNodePositioning;
    procedure ClientChangeBounds(Sender: TObject); overload;

    property RightMargin: Integer read GetRightMargin;
    property BottomMargin: Integer read GetBottomMargin;
    property OnNodePositioning: TPositioningEvent read FOnNodePositioning write FOnNodePositioning;

    property BgLeftMargin: Integer index 0 read GetBackgroundMargin;
    property BgTopMargin: Integer index 1 read GetBackgroundMargin;
    property BgRightMargin: Integer index 2 read GetBackgroundMargin;
    property BgBottomMargin: Integer index 3 read GetBackgroundMargin;

    procedure HideSizeControls;
    procedure ShowSizeControls;

    procedure OnModified;

    property VerticalScrollPos: Integer read GetVerticalScrollPos write SetVerticalScrollPos;
    property HorizontalScrollPos: Integer read GetHorizontalScrollPos write SetHorizontalScrollPos;
  end;

implementation

{$R *.lfm}

{ Node grip indices are as follows:

       1
0 +----+----+ 2
  |         |
7 +         + 3
  |         |
6 +----+----+ 4
       5

Only grips 3, 4, and 5 are sizeable }

procedure TBasicResizeFrame.pFakeMenuPaint(Sender: TObject);
var
  MenuRect: Types.TRect;
  Menu: TMainMenu;
  X, Y, I: Integer;
  LCanvas: TCanvas;
begin
  //fake paint menu

  MenuRect := pFakeMenu.ClientRect;
  LCanvas := pFakeMenu.Canvas;
  LCanvas.Brush.Color := clMenuBar;
  LCanvas.FillRect(MenuRect);

  // pFakeMenu is visible only when HasMainMenu is true
  // but FDesignedForm can be nil if the designer is painted before it has been assigned
  if not HasMainMenu then
    Exit;

  Menu := FDesignedForm.Form.Menu;
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

procedure TBasicResizeFrame.sbVerticalScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  if ScrollCode <> scEndScroll then
    HideSizeRects
  else
    ShowSizeRects;

  FVerticalScrollPos := ScrollPos;

  PositionNodes(Self);

  if Assigned(FOnVerticalScroll)
    // for refresh from this class, pass sender as nil.
    // In other case program will go into infinity loop
    and (Sender <> nil) then
    FOnVerticalScroll(Sender, ScrollCode, ScrollPos);
end;

procedure TBasicResizeFrame.sbHorizontalScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  if ScrollCode <> scEndScroll then
    HideSizeRects
  else
    ShowSizeRects;

  FHorizontalScrollPos := ScrollPos;

  PositionNodes(Self);

  if Assigned(FOnHorizontalScroll)
    // for refresh from this class, pass sender as nil.
    // In other case program will go into infinity loop
    and (Sender <> nil) then
    FOnHorizontalScroll(Sender, ScrollCode, ScrollPos);
end;

{ TResizerFrame }

// Tiles the source image over the given target canvas
procedure TileImage(const ASource: TImage; ATarget: TCanvas; AX, AY,
  AWidth, AHeight: Integer);
var
  LX, LY, LDeltaX, LDeltaY: Integer;
begin
  LDeltaX := ASource.Width;
  LDeltaY := ASource.Height;
  LY := 0;
  while LY < AHeight do
  begin
    LX := 0;
    while LX < AWidth do
    begin
      ATarget.Draw(AX + LX, AY + LY, ASource.Picture.graphic);
      Inc(LX, LDeltaX);
    end;
    Inc(LY, LDeltaY);
  end;
end;

procedure TBasicResizeFrame.PanelPaint(Sender: TObject);
var
  LWidth, LHeight: Integer;
  LOldColor: TColor;
  LCanvas: TCanvas;
begin
  if FNodePositioning then
    Exit;
  if (Sender = pR) or (Sender = pL) then
  begin
    LWidth := SIZER_LINE_WIDTH;
    LHeight := Height;
  end else
  begin
    LWidth := Width;
    LHeight := SIZER_LINE_WIDTH;
  end;
  LCanvas := (Sender as TPanel).Canvas;
  if FFakeFocusControl.Focused then
  begin
    LOldColor := LCanvas.Brush.Color;
    LCanvas.Brush.Color := $FFEEDD;
    LCanvas.FillRect(0, 0, LWidth, LHeight);
    LCanvas.Brush.Color := LOldColor;
  end;
  TileImage(iResizerLineImg, LCanvas, 0, 0, LWidth, LHeight);
end;

procedure TBasicResizeFrame.ClientChangeBounds(Sender: TObject);
begin
  if (DesignedForm = nil) or FNodePositioning then
    Exit;

  FLastClientWidth  := pClient.Width;
  FLastClientHeight := pClient.Height;

(*
  DesignedForm.BeginUpdate;

  DesignedForm.RealLeft := 0;
  DesignedForm.RealTop := 0;
  DesignedForm.RealWidth  := pClient.Width;
  DesignedForm.RealHeight := pClient.Height;
  DesignedForm.EndUpdate;
*)
end;

procedure TBasicResizeFrame.BGChangeBounds(Sender: TObject);
begin
  PositionNodes(Self);
end;

procedure TBasicResizeFrame.HideSizeRects;
var
  p: TObject;
  wc: TWinControl absolute p;
begin
  for p in FNodes do
    if not (wc is TPanel) then
      wc.Visible := False;
end;

procedure TBasicResizeFrame.HideSizeControls;
begin
  pL.Repaint;
  pT.Repaint;
  pR.Repaint;
  pB.Repaint;

  HideSizeRects;
  pBG.Visible := False;
end;

procedure TBasicResizeFrame.ShowSizeRects;
var
  p: TObject;
  wc: TWinControl absolute p;
begin
  for p in FNodes do
    wc.Visible := True;
end;

procedure TBasicResizeFrame.PositionNodes;
begin
  PositionNodes(Self);
end;

procedure TBasicResizeFrame.ShowSizeControls;
begin
  pL.Repaint;
  pT.Repaint;
  pR.Repaint;
  pB.Repaint;

  ShowSizeRects;
  pBG.Visible := True;
end;

procedure TBasicResizeFrame.CreateNodes;
var
  Node: Integer;
  Panel: TPanel;
begin
  for Node := 0 to 7 do
  begin
    Panel := TPanel.Create(self);
    with Panel do
    begin
      BevelOuter := bvNone;
      Color := clBlack;

      Name := 'Node' + IntToStr(Node);
      Caption:='';
      Width := SIZER_RECT_SIZE;
      Height := SIZER_RECT_SIZE;
      Parent := Self;
      Visible := True;
      FNodes.Add(Panel);

      case Node of
        {0,}4: Cursor := crSizeNWSE;
        {1,}5: Cursor := crSizeNS;
        //{2,}6: Cursor := crSizeNESW;
        3{,7}: Cursor := crSizeWE;
      end;
      if Node in [3,4,5] then
      begin
        OnMouseDown := NodeMouseDown;
        OnMouseMove := NodeMouseMove;
        OnMouseUp := NodeMouseUp;
      end;

      with TShape.Create(Panel) do
      begin
        Parent := Panel;
        Align:= alClient;
        Enabled := False;

        if Node in [3,4,5] then
          Brush.Color:=clBtnFace
        else
          Brush.Color:=clGray;
      end;
    end;
  end;
  // extra resizers
  pB.OnMouseDown := NodeMouseDown;
  pB.OnMouseMove := NodeMouseMove;
  pB.OnMouseUp := NodeMouseUp;

  pR.OnMouseDown := NodeMouseDown;
  pR.OnMouseMove := NodeMouseMove;
  pR.OnMouseUp := NodeMouseUp;

  FNodes.Add(pL);
  FNodes.Add(pT);
  FNodes.Add(pR);
  FNodes.Add(pB);
end;

procedure TBasicResizeFrame.NodeMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  LCtrlPoint: TPoint;
begin
  { TShape in TBasicResizeFrame.CreateNodes is disabled, anyway in future can be usefull
  if Sender is TGraphicControl then
    Sender := TGraphicControl(Sender).Parent;}

  if (Enabled) and (Sender is TWinControl) then
  begin
    FNodePositioning:=True;

    // when we start resizing the rules do not apply to us :)
    FMaxWidth := Constraints.MaxWidth;
    FMaxHeight := Constraints.MaxHeight;
    Constraints.MaxWidth := 0;
    Constraints.MaxHeight := 0;
    with pClient do
    begin
      Align := alClient;
      if pBG.Left + BgLeftMargin <= 0 then
        BorderSpacing.Left := Max(-pBG.Left - (FHorizontalScrollPos - SIZER_RECT_SIZE), 0)
      else
        BorderSpacing.Left := Max(pBG.Left + BgLeftMargin, 0);

      if pBG.Top + BgTopMargin <= 0 then
        BorderSpacing.Top := Max(-pBG.Top - (FVerticalScrollPos - SIZER_RECT_SIZE), 0)
      else
        BorderSpacing.Top := Max(pBG.Top + BgTopMargin, 0);

      BorderSpacing.Right := Max(Self.Width - (pR.Left - BgRightMargin), 0);
      BorderSpacing.Bottom := Max(Self.Height - (pB.Top - BgBottomMargin), 0);
    end;

    BeginFormSizeUpdate(Sender);

    {$IF Defined(LCLWin32) or Defined(LCLWin64)}
    SetCapture(TWinControl(Sender).Handle);
    {$ENDIF}
    GetCursorPos(FOldPos);
    // perform first "click delta" to reduce leap
    // + calculate delta created by scrollbars and theirs position...
    FillChar(FDelta, SizeOf(FDelta), #0);
    LCtrlPoint := (Sender as TWinControl).ScreenToClient(Mouse.CursorPos);
    if Sender = pR then
    begin
      FDelta.X := -(LCtrlPoint.x - RightSizerLineWidth) + RightMargin + Left;
      FPositioningKind := [pkRight];
    end
    else if Sender = pB then
    begin
      FDelta.Y := -(LCtrlPoint.y - BottomSizerLineWidth) + BottomMargin + Top;
      FPositioningKind := [pkBottom];
    end
    else
      case FNodes.IndexOf(Sender) of
        3: // middle right
          begin
            FDelta.X := -(LCtrlPoint.x - RightSizerRectWidth) + RightMargin + Left;
            FPositioningKind := [pkRight];
          end;
        4: // right bottom
          begin
            FDelta.X := -(LCtrlPoint.x - RightSizerRectWidth) + RightMargin + Left;
            FDelta.Y := -(LCtrlPoint.y - BottomSizerRectHeight) + BottomMargin + Top;
            FPositioningKind := [pkRight, pkBottom];
          end;
        5: // middle bottom
          begin
            FDelta.Y := -(LCtrlPoint.y - BottomSizerRectHeight) + BottomMargin + Top;
            FPositioningKind := [pkBottom];
          end;
      end;
  end;
end;

procedure TBasicResizeFrame.NodeMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  newPos: TPoint;
  frmPoint : TPoint;
  OldRect: TRect;
  AdjL,AdjR,AdjT,AdjB: Boolean;
begin
  { TShape in TBasicResizeFrame.CreateNodes is disabled, anyway in future can be usefull
  // handle TPanel for resizing rectangles
  if Sender is TGraphicControl then
    Sender := TGraphicControl(Sender).Parent;}

  if FNodePositioning then
  begin
    begin
      with TWinControl(Sender) do
      begin
      GetCursorPos(newPos);

      if (newPos.x = FOldPos.x) and (newPos.y = FOldPos.y) then
        Exit;

      HideSizeControls;
      UpdateWindow(pBG.Handle);
      UpdateWindow(Self.Handle);
      UpdateWindow(Self.Parent.Handle);
      with Self do
      begin //resize
        frmPoint := Self.ScreenToClient(Mouse.CursorPos);
        frmPoint.x:= frmPoint.x + FDelta.x;
        frmPoint.y:= frmPoint.y + FDelta.y;

        OldRect := Self.BoundsRect;
        AdjL := False;
        AdjR := False;
        AdjT := False;
        AdjB := False;
        case FNodes.IndexOf(TWinControl(Sender)) of
          0: begin
               //AdjL := True;
               //AdjT := True;
             end;
          1: begin
               //AdjT := True;
             end;
          2: begin
               //AdjR := True;
               //AdjT := True;
             end;
          3, 10: begin
               AdjR := True;
             end;
          4: begin
               AdjR := True;
               AdjB := True;
             end;
          5, 11: begin
               AdjB := True;
             end;
          6: begin
               //AdjL := True;
               //AdjB := True;
             end;
          7: begin
               //AdjL := True;
             end;
        end;

        if AdjL then
          OldRect.Left := frmPoint.X;
        if AdjR then
          OldRect.Right := frmPoint.X;
        if AdjT then
          OldRect.Top := frmPoint.Y;
        if AdjB then
          OldRect.Bottom := frmPoint.Y;

        SetBounds(OldRect.Left,OldRect.Top,OldRect.Right - OldRect.Left,OldRect.Bottom - OldRect.Top);
      end;
      //move node
      Left := Left - FOldPos.X + newPos.X;
      Top := Top - FOldPos.Y + newPos.Y;
      FOldPos := newPos;
      end;
    end;
    PositionNodes(Self);
    if Assigned(OnNodePositioning) then
      OnNodePositioning(Self, FPositioningKind, pcPositioning);

    // the same operation as belowe exist in ClientChangeBounds but it is
    // disabled for FNodePositioning = true
    // we need to refresh this values after OnNodePositioning
    FLastClientWidth := pClient.Width;
    FLastClientHeight:= pClient.Height;
  end;
end;

procedure TBasicResizeFrame.NodeMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  { TShape in TBasicResizeFrame.CreateNodes is disabled, anyway in future can be usefull
  if Sender is TGraphicControl then
    Sender := TGraphicControl(Sender).Parent;}

  if FNodePositioning then
  begin
    Screen.Cursor := crDefault;
    {$IF Defined(LCLWin32) or Defined(LCLWin64)}
    ReleaseCapture;
    {$ENDIF}

    Constraints.MaxWidth := FMaxWidth;
    Constraints.MaxHeight := FMaxHeight;
    FNodePositioning := False;
    ShowSizeControls;
    if Assigned(OnNodePositioning) then
      OnNodePositioning(Sender, FPositioningKind, pcPositioningEnd);
    FPositioningKind := [];

    pClient.Align := alNone;
    BorderSpacing.Left := 0;
    BorderSpacing.Top := 0;
    BorderSpacing.Right := 0;
    BorderSpacing.Bottom := 0;
    PositionNodes(Self);

    EndFormSizeUpdate(Sender);

    // after resizing, TFrame is frozen in Windows OS
    // this is trick to workaraund IDE bug. Also for proper size for normal form
    TryBoundDesignedForm;
  end;
end;

procedure TBasicResizeFrame.OnModified;
begin
  FDesignerModified := True;
end;

function TBasicResizeFrame.GetRightMargin: Integer;
begin
  if not FNodePositioning then
    FLastRightMarign := Width - (pR.Left + pR.Width);
  Result := FLastRightMarign;
end;

function TBasicResizeFrame.HasMainMenu: Boolean;
var
  I: Integer;
begin
  Result := False;
  if  (FDesignedForm<>nil) and (FDesignedForm.Form.Menu<>nil)
  and (FDesignedForm.Form.Menu.Items.Count>0)
  then
    for I := 0 to FDesignedForm.Form.Menu.Items.Count-1 do
      if FDesignedForm.Form.Menu.Items[I].Visible then
        Exit(True);
end;

function TBasicResizeFrame.GetBottomMargin: Integer;
begin
  if not FNodePositioning then
    FLastBottomMarign := Height - (pB.Top + pB.Height);
  Result := FLastBottomMarign;
end;

{-----------------------------------------------------------------------------------------------------------------------
  for Vertical scroll
{----------------------------------------------------------------------------------------------------------------------}

function TBasicResizeFrame.BottomSizerRectHeight: Integer;
begin
  Result := SIZER_RECT_SIZE;
end;

function TBasicResizeFrame.BottomSizerLineWidth: Integer;
begin
  Result := SIZER_LINE_WIDTH;
end;

function TBasicResizeFrame.TopSizerRectTop: Integer;
begin
  Result := -FVerticalScrollPos;
end;

function TBasicResizeFrame.TopSizerLineWidth: Integer;
begin
  Result := SIZER_LINE_WIDTH;
end;

function TBasicResizeFrame.VerticalSizerLineLength: Integer;
begin
  Result := Height - BottomMargin;
end;

{-----------------------------------------------------------------------------------------------------------------------
  for Horizontal scroll
{----------------------------------------------------------------------------------------------------------------------}

function TBasicResizeFrame.RightSizerRectWidth: Integer;
begin
  Result := SIZER_RECT_SIZE;
end;

function TBasicResizeFrame.RightSizerLineWidth: Integer;
begin
  Result := SIZER_LINE_WIDTH;
end;

function TBasicResizeFrame.LeftSizerRectLeft: Integer;
begin
  Result := -FHorizontalScrollPos;
end;

function TBasicResizeFrame.LeftSizerLineWidth: Integer;
begin
  Result := SIZER_LINE_WIDTH;
end;

function TBasicResizeFrame.HorizontalSizerLineLength: Integer;
begin
  Result := Width - RightMargin;
end;

function TBasicResizeFrame.GetBackgroundMargin(const AIndex: Integer): Integer;
begin
  if FBackground = nil then
    Result := 0
  else
    Result := FBackground.GetMargin(AIndex);

  if (AIndex = 1) and HasMainMenu then
    Result := Result + GetMenuHeight;
end;

function TBasicResizeFrame.GetClientPanel: TPanel;
begin
  Result := pClient;
end;

function TBasicResizeFrame.GetNodePositioning: Boolean;
begin
  Result := FNodePositioning;
end;

function TBasicResizeFrame.GetDesignedForm: IDesignedForm;
begin
  Result := FDesignedForm;
end;

procedure TBasicResizeFrame.SetDesignedForm(const AValue: IDesignedForm);
begin
  FDesignedForm := AValue;
  if FDesignedForm = nil then
  begin
    if Assigned(FBackground) then
      FBackground.ResizeFrame := nil;
    FBackground := nil;
  end
  else
    if Supports(FDesignedForm, IDesignedFormBackground, FBackground) then
    begin
      FBackground.Parent := pBG;
      FBackground.ResizeFrame := Self;
    end;
  // special for QT (at start "design form" has wrong position)
  TryBoundDesignedForm;
end;

function TBasicResizeFrame.GetMenuHeight: Integer;
begin
  // some WS (Gtk2) return too big SM_CYMENU, just set it according to font height
  // no problem, it is used only for the fake main menu

  {$IFDEF LCLWin32}
  Result := lclintf.GetSystemMetrics(SM_CYMENU);
  {$ELSE}
  if pBG.HandleAllocated then
    Result := pBG.Canvas.TextHeight('Hg') * 4 div 3
  else
    Result := 20;
  {$ENDIF}
end;

procedure TBasicResizeFrame.TryBoundDesignedForm;
begin
  if DesignedForm = nil then
    Exit;

  HideSizeControls;
  ShowSizeControls;

  // for GTK2 resizing form (pClient is hidden under pBG)
  {$IF DEFINED(LCLGtk2) OR DEFINED(LCLQt)}
  pClient.SendToBack; // <--- this is a must.
  {$ENDIF}
  pClient.BringToFront;

  pFakeMenu.Visible := HasMainMenu;
  if pFakeMenu.Visible then
  begin
    pFakeMenu.Height := GetMenuHeight;
    pFakeMenu.BorderSpacing.Left := BgLeftMargin;
    pFakeMenu.BorderSpacing.Top := BgTopMargin - pFakeMenu.Height;
    pFakeMenu.BorderSpacing.Right := BgRightMargin;
    pFakeMenu.BringToFront;
  end;
end;

procedure TBasicResizeFrame.BeginFormSizeUpdate(Sender: TObject);
begin
end;

procedure TBasicResizeFrame.EndFormSizeUpdate(Sender: TObject);
begin
end;

function TBasicResizeFrame.GetFrame: TCustomFrame;
begin
  Result := Self;
end;

function TBasicResizeFrame.GetVerticalScrollPos: Integer;
begin
  Result := FVerticalScrollPos;
end;

procedure TBasicResizeFrame.SetVerticalScrollPos(AValue: Integer);
begin
  FVerticalScrollPos := AValue;
end;

function TBasicResizeFrame.GetHorizontalScrollPos: Integer;
begin
  Result := FHorizontalScrollPos;
end;

procedure TBasicResizeFrame.SetHorizontalScrollPos(AValue: Integer);
begin
  FHorizontalScrollPos := AValue;
end;

function TBasicResizeFrame.GetSizerRectSize: Integer;
begin
  Result := SIZER_RECT_SIZE;
end;

function TBasicResizeFrame.GetSizerLineWidth: Integer;
begin
  Result := SIZER_LINE_WIDTH;
end;

function TBasicResizeFrame.GetBackgroundPanel: TPanel;
begin
  Result := pBG;
end;

function TBasicResizeFrame.DesignedWidthToScroll: Integer;
begin
  if DesignedForm = nil then
    Exit(0);

  Result := DesignedForm.Width - FLastClientWidth;
  //Result := DesignedForm.Width - DesignedForm.RealWidth;
end;

procedure TBasicResizeFrame.DesignerSetFocus;
begin
  if FFakeFocusControl.CanSetFocus then
    FFakeFocusControl.SetFocus;
end;

function TBasicResizeFrame.DesignedHeightToScroll: Integer;
begin
  if DesignedForm = nil then
    Exit(0);

  Result := DesignedForm.Height - FLastClientHeight;
  //Result := DesignedForm.Height - DesignedForm.RealHeight;
end;

procedure TBasicResizeFrame.ClientChangeBounds;
begin
  ClientChangeBounds(nil);
end;

{}

constructor TBasicResizeFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FFakeFocusControl := TEdit.Create(Self);
  FFakeFocusControl.Parent := Self;
  FFakeFocusControl.Top := -100;
  FFakeFocusControl.OnKeyDown := FakeKeyDown;
  FFakeFocusControl.OnKeyUp := FakeKeyUp;
  FFakeFocusControl.OnUTF8KeyPress := FakeUTF8KeyPress;
  FFakeFocusControl.OnEnter := FakeExitEnter;
  FFakeFocusControl.OnExit := FakeExitEnter;

  FNodes := TObjectList.Create(False);
  CreateNodes;

  pL.OnPaint := PanelPaint;
  pT.OnPaint := PanelPaint;
  pR.OnPaint := PanelPaint;
  pB.OnPaint := PanelPaint;

  pClient.OnChangeBounds := ClientChangeBounds;
  pBG.OnChangeBounds     := BGChangeBounds;
  PositionNodes(Self);

  Application.AddOnIdleHandler(AppOnIdle);
end;

procedure TBasicResizeFrame.AppOnIdle(Sender: TObject; var Done: Boolean);
begin
  if FDesignerModified then
  begin
    if FOldHasMainMenu <> HasMainMenu then
    begin
      TryBoundDesignedForm;
      if Assigned(OnNodePositioning) then
        OnNodePositioning(Self, [pkBottom], pcPositioningEnd);
      Application.NotifyUserInputHandler(Self, 0); // force repaint invisible components
    end else
    if pFakeMenu.Visible then
      pFakeMenu.Invalidate; // always repaint menu on modification

    FDesignerModified := False;
  end;
end;

destructor TBasicResizeFrame.Destroy;
begin
  Pointer(FDesignedForm) := nil;
  Pointer(FBackground) := nil;
  Application.RemoveOnIdleHandler(AppOnIdle);
  FNodes.Free;
  inherited Destroy;
end;

procedure TBasicResizeFrame.FakeExitEnter(Sender: TObject);
begin
  pL.Repaint;
  pT.Repaint;
  pR.Repaint;
  pB.Repaint;
end;

procedure TBasicResizeFrame.FakeKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  LWndProc: TWndMethod;
  LMsg: TLMKeyUp;
begin
  LWndProc := FDesignedForm.Form.WindowProc;
  FillChar(LMsg{%H-}, SizeOf(LMsg), 0);
  LMsg.msg := CN_KEYDOWN;
  LMsg.CharCode := Key;
  LWndProc(TLMessage(LMsg));
  Key := LMsg.CharCode;
end;

procedure TBasicResizeFrame.FakeKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  LWndProc: TWndMethod;
  LMsg: TLMKeyUp;
begin
  LWndProc := FDesignedForm.Form.WindowProc;
  FillChar(LMsg{%H-}, SizeOf(LMsg), 0);
  LMsg.msg := CN_KEYUP;
  LMsg.CharCode := Key;
  LWndProc(TLMessage(LMsg));
  Key := LMsg.CharCode;
end;

procedure TBasicResizeFrame.FakeUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char
  );
begin
  FDesignedForm.Form.IntfUTF8KeyPress(UTF8Key, 1, False);
end;

procedure TBasicResizeFrame.PositionNodes(AroundControl: TWinControl);
var
  Node,T,L,CT,CL,FR,FB,FT,FL: Integer;
  TopLeft: TPoint;
begin
  if FDesignedForm = nil then
    Exit;

  // positions of bars
  if not FNodePositioning then
  begin
    pL.Left := -FHorizontalScrollPos;
    pR.Left := FDesignedForm.Width - FHorizontalScrollPos + pL.Width + BgRightMargin + BgLeftMargin;
    pT.Top := -FVerticalScrollPos;
    pB.Top := FDesignedForm.Height - FVerticalScrollPos + pT.Height + BgBottomMargin + BgTopMargin;

    // width and height
    pL.Top:=0;
    pL.Height := FDesignedForm.Height + 2*SIZER_RECT_SIZE + BgTopMargin + BgBottomMargin;
    pR.Top:=0;
    pR.Height := FDesignedForm.Height + 2*SIZER_RECT_SIZE + BgTopMargin + BgBottomMargin;
    pT.Left:=0;
    pT.Width := FDesignedForm.Width + 2*SIZER_RECT_SIZE + BgLeftMargin + BgRightMargin;
    pB.Left:=0;
    pB.Width := FDesignedForm.Width + 2*SIZER_RECT_SIZE + BgLeftMargin + BgRightMargin;

    // client
    if pBG.Left + BgLeftMargin <= 0 then
      pClient.Left := -(pBG.Left) - (FHorizontalScrollPos - SIZER_RECT_SIZE)
    else
      pClient.Left := pBG.Left + BgLeftMargin;
    if pBG.Top + BgTopMargin <= 0 then
      pClient.Top := -(pBG.Top) - (FVerticalScrollPos - SIZER_RECT_SIZE)
    else
      pClient.Top := pBG.Top + BgTopMargin;

    pClient.Height := Height - pClient.Top - Max(Height - (pB.Top - BgBottomMargin), 0);
    pClient.Width := Width - pClient.Left - Max(Width - (pR.Left - BgRightMargin), 0);
  end;

  FOldHasMainMenu := HasMainMenu;

  for Node := 0 to 7 do
  begin
    with AroundControl do
    begin
      FR := Width - RightSizerRectWidth - RightMargin;
      FB := Height - BottomSizerRectHeight - BottomMargin;

      FT := TopSizerRectTop;
      FL := LeftSizerRectLeft;

      CL := (FR - FL) div 2 + FL;
      CT := (FB - FT) div 2 + FT;

      case Node of
        0: begin
             T := FT;
             L := FL;
           end;
        1: begin
             T := FT;
             L := CL;
           end;
        2: begin
             T := FT;
             L := FR;
           end;
        3: begin
             T := CT;
             L := FR;
           end;
        4: begin
             T := FB;
             L := FR;
           end;
        5: begin
             T := FB;
             L := CL;
           end;
        6: begin
             T := FB;
             L := FL;
           end;
        7: begin
             T := CT;
             L := FL;
           end;
        else
          T := 0;
          L := 0;
      end;

      TopLeft := (Classes.Point(L,T));
    end;
    with TPanel(FNodes[Node]) do
    begin
      Top := TopLeft.Y;
      Left := TopLeft.X;
      Repaint;
    end;
  end;
end;

end.

