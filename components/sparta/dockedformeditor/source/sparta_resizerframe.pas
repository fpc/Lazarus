{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Maciej Izak

  DaThoX 2004-2015
  FreeSparta.com
}

unit sparta_ResizerFrame;

{$mode delphi}{$H+}

interface

uses
  Classes, contnrs, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls, Graphics, LCLType,
  lclintf, sparta_DesignedForm, Math, FormEditingIntf, PropEdits;

type

  { TResizerFrame }
  TPositioningCode = (pcPositioning, pcPositioningEnd);
  TPositioningKind = set of (pkBottom, pkRight);
  TPositioningEvent = procedure(Sender: TObject; PositioningKind: TPositioningKind; PositioningCode: TPositioningCode) of object;

  TResizerFrame = class(TFrame)
    iResizerLineImg: TImage;
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

    procedure SetDesignedForm(const AValue: IDesignedForm);
  private
    { private declarations }
    FOnNodePositioning: TPositioningEvent;
    FOnHorizontalScroll, FOnVerticalScroll: TScrollEvent;
    FLastRightMarign: Integer;
    FLastBottomMarign: Integer;
    FNodes: TObjectList;
    FNodePositioning: Boolean;
    FOldPos, FDelta: TPoint;
    FPositioningKind: TPositioningKind;
    FMaxWidth, FMaxHeight: Integer;
    FActivePropertyGridItemIndex: Integer;
    FLastClientWidth, FLastClientHeight: Integer;

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

    function GetBackgroundMargin(const AIndex: Integer): Integer;

    procedure TryBoundDesignedForm;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    property DesignedForm: IDesignedForm read FDesignedForm write SetDesignedForm;

    procedure PositionNodes(AroundControl: TWinControl);
    property NodePositioning: Boolean read FNodePositioning;
    procedure ClientChangeBounds(Sender: TObject);

    property RightMargin: Integer read GetRightMargin;
    property BottomMargin: Integer read GetBottomMargin;
    property OnNodePositioning: TPositioningEvent read FOnNodePositioning write FOnNodePositioning;

    property BgLeftMargin: Integer index 0 read GetBackgroundMargin;
    property BgTopMargin: Integer index 1 read GetBackgroundMargin;
    property BgRightMargin: Integer index 2 read GetBackgroundMargin;
    property BgBottomMargin: Integer index 3 read GetBackgroundMargin;

    function DesignedWidthToScroll: Integer;
    function DesignedHeightToScroll: Integer;

    procedure HideSizeRects;
    procedure HideSizeControls;
    procedure ShowSizeRects;
    procedure ShowSizeControls;

    property VerticalScrollPos: Integer read FVerticalScrollPos write FVerticalScrollPos;
    property HorizontalScrollPos: Integer read FHorizontalScrollPos write FHorizontalScrollPos;
  end;

resourcestring
  SArgumentOutOfRange = 'Argument out of range';

implementation

{$R *.lfm}

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

procedure TResizerFrame.sbVerticalScroll(Sender: TObject;
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

procedure TResizerFrame.sbHorizontalScroll(Sender: TObject;
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

procedure TResizerFrame.SetDesignedForm(const AValue: IDesignedForm);
begin
  FDesignedForm := AValue;
  if FDesignedForm = nil then
    FBackground := nil
  else
    if Supports(FDesignedForm, IDesignedFormBackground, FBackground) then
    begin
      FBackground.Parent := pBG;
    end;
  // special for QT (at start "design form" has wrong position)
  TryBoundDesignedForm;
end;

procedure TResizerFrame.PanelPaint(Sender: TObject);
begin
  if FNodePositioning then
    Exit;
  if Sender = pR then
    TileImage(iResizerLineImg, pR.Canvas, 0, 0, SIZER_LINE_WIDTH, Height)
  else if Sender = pB then
    TileImage(iResizerLineImg, pB.Canvas, 0, 0, Width, SIZER_LINE_WIDTH)
  else if Sender = pL then
    TileImage(iResizerLineImg, pL.Canvas, 0, 0, SIZER_LINE_WIDTH, Height)
  else if Sender = pT then
    TileImage(iResizerLineImg, pT.Canvas, 0, 0, Width, SIZER_LINE_WIDTH);
end;

procedure TResizerFrame.ClientChangeBounds(Sender: TObject);
{$IFDEF USE_POPUP_PARENT_DESIGNER}
var
  p: TPoint;
{$ENDIF}
begin
  if (DesignedForm = nil) or FNodePositioning then
    Exit;

  FLastClientWidth  := pClient.Width;
  FLastClientHeight := pClient.Height;

(*
  DesignedForm.BeginUpdate;

{$IFDEF USE_POPUP_PARENT_DESIGNER}
  p := Point(0, 0);
  p := pClient.ClientToScreen(p);
  DesignedForm.RealLeft   := p.x;
  DesignedForm.RealTop   := p.y;
{$ELSE}
  DesignedForm.RealLeft := 0;
  DesignedForm.RealTop := 0;
{$ENDIF}
  DesignedForm.RealWidth  := pClient.Width;
  DesignedForm.RealHeight := pClient.Height;
  DesignedForm.EndUpdate;
*)
end;

procedure TResizerFrame.BGChangeBounds(Sender: TObject);
begin
  PositionNodes(Self);
end;

procedure TResizerFrame.HideSizeRects;
var
  p: TObject;
  wc: TWinControl absolute p;
begin
  for p in FNodes do
    if not (wc is TPanel) then
      wc.Visible := False;
end;

procedure TResizerFrame.HideSizeControls;
begin
  pL.Repaint;
  pT.Repaint;
  pR.Repaint;
  pB.Repaint;

  HideSizeRects;
  pBG.Visible := False;
end;

procedure TResizerFrame.ShowSizeRects;
var
  p: TObject;
  wc: TWinControl absolute p;
begin
  for p in FNodes do
    wc.Visible := True;
end;

procedure TResizerFrame.ShowSizeControls;
begin
  pL.Repaint;
  pT.Repaint;
  pR.Repaint;
  pB.Repaint;

  ShowSizeRects;
  pBG.Visible := True;
end;

procedure TResizerFrame.CreateNodes;
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

      with TShape.Create(Panel) do
      begin
        Parent := Panel;
        Align:= alClient;

        if Node in [3,4,5] then
          Brush.Color:=clBtnFace
        else
          Brush.Color:=clGray;

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

procedure TResizerFrame.NodeMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  LCtrlPoint: TPoint;
begin
  if Sender is TGraphicControl then
    Sender := TGraphicControl(Sender).Parent;

  if (Enabled) AND (Sender is TWinControl) then
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

    // when was active ActivePropertyGrid.ItemIndex for height or width during scaling
    // there was problem with values :<
    if ((Sender = pR) or (Sender = pB) or (FNodes.IndexOf(Sender) in [3,4,5])) and (FormEditingHook.GetCurrentObjectInspector <> nil) then
    begin
      FActivePropertyGridItemIndex := FormEditingHook.GetCurrentObjectInspector.GetActivePropertyGrid.ItemIndex;
      FormEditingHook.GetCurrentObjectInspector.GetActivePropertyGrid.ItemIndex := -1;
    end
    else
      FActivePropertyGridItemIndex := -1;

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
      FDelta.X := -(LCtrlPoint.x - RightSizerLineWidth) + RightMargin;
      FPositioningKind := [pkRight];
    end
    else if Sender = pB then
    begin
      FDelta.Y := -(LCtrlPoint.y - BottomSizerLineWidth) + BottomMargin;
      FPositioningKind := [pkBottom];
    end
    else
      case FNodes.IndexOf(Sender) of
        3: // middle right
          begin
            FDelta.X := -(LCtrlPoint.x - RightSizerRectWidth) + RightMargin;
            FPositioningKind := [pkRight];
          end;
        4: // right bottom
          begin
            FDelta.X := -(LCtrlPoint.x - RightSizerRectWidth) + RightMargin;
            FDelta.Y := -(LCtrlPoint.y - BottomSizerRectHeight) + BottomMargin;
            FPositioningKind := [pkRight, pkBottom];
          end;
        5: // middle bottom
          begin
            FDelta.Y := -(LCtrlPoint.y - BottomSizerRectHeight) + BottomMargin;
            FPositioningKind := [pkBottom];
          end;
      end;
  end;
end;

procedure TResizerFrame.NodeMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  newPos: TPoint;
  frmPoint : TPoint;
  OldRect: TRect;
  AdjL,AdjR,AdjT,AdjB: Boolean;
begin
  // handle TPanel for resizing rectangles
  if Sender is TGraphicControl then
    Sender := TGraphicControl(Sender).Parent;

  if FNodePositioning then
  begin
    begin
      with TWinControl(Sender) do
      begin
      GetCursorPos(newPos);

      if (newPos.x = FOldPos.x) and (newPos.y = FOldPos.y) then
        Exit;

      HideSizeControls;

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

procedure TResizerFrame.NodeMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Sender is TGraphicControl then
    Sender := TGraphicControl(Sender).Parent;

  if FNodePositioning then
  begin
    Screen.Cursor := crDefault;
    {$IF Defined(LCLWin32) or Defined(LCLWin64)}
    ReleaseCapture;
    {$ENDIF}

    // restore last selected item in OI.
    if FActivePropertyGridItemIndex <> -1 then
    begin
      if FormEditingHook.GetCurrentObjectInspector <> nil then
        FormEditingHook.GetCurrentObjectInspector.GetActivePropertyGrid.ItemIndex := FActivePropertyGridItemIndex;
      FActivePropertyGridItemIndex := -1;
    end;

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

    GlobalDesignHook.RefreshPropertyValues;

    // after resizing, TFrame is frozen in Windows OS
    // this is trick to workaraund IDE bug. Also for proper size for normal form
    TryBoundDesignedForm;
  end;
end;

function TResizerFrame.GetRightMargin: Integer;
begin
  if not FNodePositioning then
    FLastRightMarign := Width - (pR.Left + pR.Width);
  Result := FLastRightMarign;
end;

function TResizerFrame.GetBottomMargin: Integer;
begin
  if not FNodePositioning then
    FLastBottomMarign := Height - (pB.Top + pB.Height);
  Result := FLastBottomMarign;
end;

{-----------------------------------------------------------------------------------------------------------------------
  for Vertical scroll
{----------------------------------------------------------------------------------------------------------------------}

function TResizerFrame.BottomSizerRectHeight: Integer;
begin
  Result := SIZER_RECT_SIZE;
end;

function TResizerFrame.BottomSizerLineWidth: Integer;
begin
  Result := SIZER_LINE_WIDTH;
end;

function TResizerFrame.TopSizerRectTop: Integer;
begin
  Result := -FVerticalScrollPos;
end;

function TResizerFrame.TopSizerLineWidth: Integer;
begin
  Result := SIZER_LINE_WIDTH;
end;

function TResizerFrame.VerticalSizerLineLength: Integer;
begin
  Result := Height - BottomMargin;
end;

{-----------------------------------------------------------------------------------------------------------------------
  for Horizontal scroll
{----------------------------------------------------------------------------------------------------------------------}

function TResizerFrame.RightSizerRectWidth: Integer;
begin
  Result := SIZER_RECT_SIZE;
end;

function TResizerFrame.RightSizerLineWidth: Integer;
begin
  Result := SIZER_LINE_WIDTH;
end;

function TResizerFrame.LeftSizerRectLeft: Integer;
begin
  Result := -FHorizontalScrollPos;
end;

function TResizerFrame.LeftSizerLineWidth: Integer;
begin
  Result := SIZER_LINE_WIDTH;
end;

function TResizerFrame.HorizontalSizerLineLength: Integer;
begin
  Result := Width - RightMargin;
end;

function TResizerFrame.GetBackgroundMargin(const AIndex: Integer): Integer;
begin
  if FBackground = nil then
    Result := 0
  else
    Result := FBackground.GetMargin(AIndex);
end;

procedure TResizerFrame.TryBoundDesignedForm;
begin
  if DesignedForm = nil then
    Exit;

  DesignedForm.BeginUpdate;
  DesignedForm.RealWidth := DesignedForm.RealWidth + 1;
  DesignedForm.RealWidth := DesignedForm.RealWidth - 1;
  DesignedForm.EndUpdate;

  HideSizeControls;
  ShowSizeControls;

  // for GTK2 resizing form (pClient is hidden under pBG)
  {$IF DEFINED(LCLGtk2) OR DEFINED(LCLQt)}
  pClient.SendToBack; // <--- this is a must.
  {$ENDIF}
  pClient.BringToFront;
end;

function TResizerFrame.DesignedWidthToScroll: Integer;
begin
  if DesignedForm = nil then
    Exit(0);

  Result := DesignedForm.Width - FLastClientWidth;
  //Result := DesignedForm.Width - DesignedForm.RealWidth;
end;

function TResizerFrame.DesignedHeightToScroll: Integer;
begin
  if DesignedForm = nil then
    Exit(0);

  Result := DesignedForm.Height - FLastClientHeight;
  //Result := DesignedForm.Height - DesignedForm.RealHeight;
end;

{}

constructor TResizerFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FNodes := TObjectList.Create(False);
  CreateNodes;

  pL.OnPaint := PanelPaint;
  pT.OnPaint := PanelPaint;
  pR.OnPaint := PanelPaint;
  pB.OnPaint := PanelPaint;

  pClient.OnChangeBounds := ClientChangeBounds;
  pBG.OnChangeBounds     := BGChangeBounds;
  PositionNodes(Self);
end;

destructor TResizerFrame.Destroy;
begin
  FNodes.Free;
  inherited Destroy;
end;

procedure TResizerFrame.PositionNodes(AroundControl: TWinControl);
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

