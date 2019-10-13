unit sparta_AbstractResizer;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Math,
  LCLType, Controls, ExtCtrls, Forms, StdCtrls, Buttons, Dialogs,
  sparta_InterfacesMDI, sparta_BasicResizeFrame, sparta_MDI_StrConsts;

type

  { TAbstractResizer }

  TAbstractResizer = class(TComponent, IResizer)
  private
    procedure FunnyButtonClick(Sender: TObject);
  protected { IResizer }
    function GetActiveResizeFrame: IResizeFrame; virtual; abstract;
    function GetActiveDesignedForm: IDesignedForm; virtual; abstract;
  public { IResizer }
    procedure TryBoundSizerToDesignedForm(Sender: TObject); virtual;
  protected
    // To perform proper behaviour for scroolbar with "PageSize" we need to remember real
    // maximal values (is possible to scroll outside of range 0..(Max - PageSize),
    // after mouse click in button responsible for changing value of scrollbar,
    // our value is equal to Max :\). Workaround: we need to remember real max value in our own place
    FRealMaxH: Integer;
    FRealMaxV: Integer;

    FSpecialMargin: array[0..3] of Integer;
    FDesignScroll: array[0..1] of Boolean;

    FParent: TWinControl;
    FResizerFrameClass: TResizerFrameClass;

    function CreateResizeFrame: TBasicResizeFrame; virtual;
    procedure NodePositioning(Sender: TObject; {%H-}PositioningKind: TPositioningKind; PositioningCode: TPositioningCode);

    function GetActiveFormAndFrame(out AForm: IDesignedForm; out AFrame: IResizeFrame): Boolean;

    procedure SetDesignScroll(AIndex: Integer; AValue: Boolean);

    procedure sbScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
  public
    pMainDTU: TPanel;
    pMain: TPanel;
    pAddons: TPanel;
    pComponents: TPanel;
    lInfo: TLabel;
    sbShowComponents  : TSpeedButton;
    sbShowFormEditor: TSpeedButton;
    sbShowAnchorEditor: TSpeedButton;
    sbShowNonVisualEditor: TSpeedButton;
    pDesignTimeUtils: TPanel;
    sbV: TScrollBar;
    sbH: TScrollBar;
    bR: TButton;

    constructor Create(AParent: TWinControl; AResizerFrameClass: TResizerFrameClass); virtual; reintroduce;
    property DesignScrollRight: Boolean index SB_Vert read FDesignScroll[SB_Vert] write SetDesignScroll;
    property DesignScrollBottom: Boolean index SB_Horz read FDesignScroll[SB_Horz] write SetDesignScroll;

    property ActiveResizeFrame: IResizeFrame read GetActiveResizeFrame;
    property ActiveDesignedForm: IDesignedForm read GetActiveDesignedForm;
  end;

implementation

{ TAbstractResizer }

procedure TAbstractResizer.FunnyButtonClick(Sender: TObject);
begin
  ShowMessage('Funny button with no functionality!'
              + sLineBreak
              + sLineBreak +
              'Regards'
              + sLineBreak +
              'Maciej Izak'
              + sLineBreak
              + sLineBreak + 'DaThoX team FreeSparta.com project');
end;

procedure TAbstractResizer.TryBoundSizerToDesignedForm(Sender: TObject);
var
  LWidth, LHeight: Integer;
  LScrollPos: Integer;
  LResizeFrame: IResizeFrame;
  LFrame: TCustomFrame;
  LForm: IDesignedForm;
begin
  if not GetActiveFormAndFrame(LForm, LResizeFrame) then
    Exit;

  LFrame := LResizeFrame.Frame;
  LFrame.Constraints.MaxWidth := pMain.Width;
  LFrame.Constraints.MaxHeight := pMain.Height;

  LWidth  := LForm.Width + LResizeFrame.BgLeftMargin + LResizeFrame.BgRightMargin + 2*LResizeFrame.SizerRectSize;
  LHeight := LForm.Height + LResizeFrame.BgTopMargin + LResizeFrame.BgBottomMargin + 2*LResizeFrame.SizerRectSize;
  if not LResizeFrame.NodePositioning then
  begin
    LFrame.Width := LWidth;
    LFrame.Height := LHeight;
    // after enlargement and after reducing constrait not work for frame (LCL bug)
    if LFrame.Width > LFrame.Constraints.MaxWidth then
      LFrame.Width := LFrame.Constraints.MaxWidth;
    if LFrame.Height > LFrame.Constraints.MaxHeight then
      LFrame.Height := LFrame.Constraints.MaxHeight;
  end;

  LResizeFrame.PositionNodes;

  DesignScrollBottom := LFrame.Width < LWidth;
  sbH.Max := LWidth;
  FRealMaxH := LWidth - LFrame.Width;
  sbH.PageSize := LFrame.Width;
  if LResizeFrame.HorizontalScrollPos > FRealMaxH then
  begin
    LResizeFrame.HorizontalScrollPos := FRealMaxH;
    LScrollPos := LResizeFrame.HorizontalScrollPos;
    sbScroll(sbH, scEndScroll, LScrollPos);
  end;

  DesignScrollRight := LFrame.Height < LHeight;
  sbV.Max := LHeight;
  FRealMaxV := LHeight - LFrame.Height;
  sbV.PageSize := LFrame.Height;
  if LResizeFrame.VerticalScrollPos > FRealMaxV then
  begin
    LResizeFrame.VerticalScrollPos := FRealMaxV;
    LScrollPos := LResizeFrame.VerticalScrollPos;
    sbScroll(sbV, scEndScroll, LScrollPos);
  end;

  {!}
  LResizeFrame.ClientChangeBounds;

  // each editor can have scrolls in different positions.
  // this is our place where we can call event to set scroll positions.
  LScrollPos := LResizeFrame.VerticalScrollPos;
  sbScroll(sbV, scEndScroll, LScrollPos);
  LScrollPos := LResizeFrame.HorizontalScrollPos;
  sbScroll(sbH, scEndScroll, LScrollPos);

  if Supports(LForm, IDesignedFormBackground) then
    (LForm as IDesignedFormBackground).RefreshValues;

  LResizeFrame.DesignerSetFocus;
end;

procedure TAbstractResizer.sbScroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
var
  LScrollPos: Integer;
  LFrame: IResizeFrame;
  LForm: IDesignedForm;
begin
  if not GetActiveFormAndFrame(LForm, LFrame) then
    Exit;

  if ScrollCode <> scEndScroll then
    LFrame.HideSizeRects
  else
    LFrame.ShowSizeRects;


  LForm.BeginUpdate;
  if Sender = sbV then
  begin
    // Warning - don't overflow the range! (go to description for FRealMaxV)
    ScrollPos := Min(ScrollPos, FRealMaxV);
    LFrame.VerticalScrollPos := ScrollPos;
    // scroll for form
    with LFrame do // -8 when we scaling the form and we don't need to scroll -> there is Max
      LScrollPos := Max(ifthen(BgPanel.Top + BgTopMargin <= 0, ScrollPos - SizerRectSize - BgTopMargin, 0), 0);
    LForm.VertScrollPosition := LScrollPos;
  end;
  if Sender = sbH then
  begin
    ScrollPos := Min(ScrollPos, FRealMaxH);
    LFrame.HorizontalScrollPos := ScrollPos;
    // scroll for form
    with LFrame do
      LScrollPos := Max(ifthen(BgPanel.Left + BgLeftMargin <= 0, ScrollPos - SizerRectSize - BgLeftMargin, 0), 0);
    LForm.HorzScrollPosition := LScrollPos;
  end;
  LForm.EndUpdate;

  LFrame.PositionNodes;

  LForm.Form.Invalidate;
end;

function TAbstractResizer.CreateResizeFrame: TBasicResizeFrame;
begin
  Result := FResizerFrameClass.Create(FParent);
  Result.Name := '';
  Result.Parent := pMain;
  Result.Left := 0;
  Result.Top := 0;
  Result.OnNodePositioning := NodePositioning;
end;

procedure TAbstractResizer.NodePositioning(Sender: TObject;
  PositioningKind: TPositioningKind; PositioningCode: TPositioningCode);

var
  LForm: IDesignedForm;
  LFrame: IResizeFrame;

  (*procedure Positioning;
  var
    LHiddenHeight, LNewHeight: Integer;
    LHiddenWidth, LNewWidth: Integer;
  begin
    LForm.BeginUpdate;

    //if pkRight in PositioningKind then
    begin
      LHiddenWidth := sbH.Position;
      if LHiddenWidth > LFrame.DesignedWidthToScroll then
        LHiddenWidth := LFrame.DesignedWidthToScroll;

      // TODO - better handling of min width - same in TDesignedFormImpl.SetPublishedBounds (sparta_FakeCustom.pas)

      LNewWidth := LFrame.NewSize.X + LHiddenWidth;
      LForm.Width := LNewWidth;
      LForm.RealWidth := LNewWidth;

      // perform minimal width (TODO)
      {if LNewWidth < DesignedForm.Width then
      begin
        FResizerFrame.pClient.Width := DesignedForm.Width;
        Application.HandleMessage;
        Application.ProcessMessages;
      end;}
    end;

    //if pkBottom in PositioningKind then
    begin
      LHiddenHeight := sbV.Position;
      if LHiddenHeight > LFrame.DesignedHeightToScroll then
        LHiddenHeight := LFrame.DesignedHeightToScroll;

      LNewHeight := LFrame.NewSize.Y+ LHiddenHeight;
      LForm.Height := LNewHeight;
      LForm.RealHeight := LNewHeight;

      // perform minimal height (TODO)
      {if LNewHeight < DesignedForm.RealHeight then
      begin
        if FResizerFrame.pClient.Height < DesignedForm.RealHeight then
          FResizerFrame.pClient.Height := DesignedForm.RealHeight;
        Application.ProcessMessages;
      end;}
    end;

    LForm.EndUpdate;
  end;*)

  procedure PositioningEnd;
  var
    LHiddenHeight, LNewHeight: Integer;
    LHiddenWidth, LNewWidth: Integer;
  begin
    LHiddenWidth := sbH.Position;
    if LHiddenWidth > LFrame.DesignedWidthToScroll then
      LHiddenWidth := LFrame.DesignedWidthToScroll;

    LNewWidth := LFrame.NewSize.X + LHiddenWidth;

    LHiddenHeight := sbV.Position;
    if LHiddenHeight > LFrame.DesignedHeightToScroll then
      LHiddenHeight := LFrame.DesignedHeightToScroll;

    LNewHeight := LFrame.NewSize.Y + LHiddenHeight;

    LForm.Form.Width := LNewWidth;
    LForm.Form.Height := LNewHeight;
  end;

begin
  if not GetActiveFormAndFrame(LForm, LFrame) then
    Exit;

  case PositioningCode of
    pcPositioningEnd: PositioningEnd;
    //pcPositioning: Positioning;
  end;
end;

function TAbstractResizer.GetActiveFormAndFrame(out AForm: IDesignedForm; out
  AFrame: IResizeFrame): Boolean;
begin
  AForm := GetActiveDesignedForm;
  if AForm = nil then
    Exit(False);

  AFrame := GetActiveResizeFrame;
  Result := True;
end;

procedure TAbstractResizer.SetDesignScroll(AIndex: Integer; AValue: Boolean);

  procedure PerformScroll(AScroll: TScrollBar);
  begin
    AScroll.Visible := AValue;
    AScroll.Position:=0;
  end;

begin
  if FDesignScroll[AIndex] = AValue then
    Exit;

  FDesignScroll[AIndex] := AValue;

  case AIndex of
    SB_Horz: PerformScroll(sbH);
    SB_Vert: PerformScroll(sbV);
  else
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  end;
end;

constructor TAbstractResizer.Create(AParent: TWinControl;
  AResizerFrameClass: TResizerFrameClass);
begin
  inherited Create(AParent);

  FResizerFrameClass := AResizerFrameClass;
  FParent := AParent;
  // create layout

  pMainDTU := TPanel.Create(Self);
  with pMainDTU do
  begin
    Parent := AParent;
    Align := alTop;
    BevelOuter := bvNone;
    Height := 0;
  end;

  pAddons := TPanel.Create(Self);
  pAddons.Parent := AParent;
  pAddons.Align := alRight;
  pAddons.BevelOuter := bvNone;
  pAddons.Width:=0;

  // Funny button
  bR := TButton.Create(Self);
  with bR do
  begin
    Parent := AParent;
    Height := 17;
    Width := 17;
    AnchorSideRight.Control := pAddons;
    AnchorSideBottom.Control := AParent;
    AnchorSideBottom.Side := asrBottom;
    Anchors := [akRight, akBottom];
    Caption := 'R';
    Visible := True;
    OnClick := FunnyButtonClick;
  end;

  sbV := TScrollBar.Create(Self);
  with sbV do
  begin
    Kind := sbVertical;
    Parent := AParent;
    AnchorSideTop.Control := pMainDTU;
    AnchorSideTop.Side := asrBottom;
    AnchorSideRight.Control := pAddons;
    AnchorSideBottom.Control := bR;
    Width := 17;
    Anchors := [akTop, akRight, akBottom];
    Visible := False;
    OnScroll := sbScroll;
  end;

  sbH := TScrollBar.Create(Self);
  with sbH do
  begin
    Parent := AParent;
    AnchorSideLeft.Control := AParent;
    AnchorSideRight.Control := bR;
    AnchorSideBottom.Control := AParent;
    AnchorSideBottom.Side := asrBottom;
    Anchors := [akLeft, akRight, akBottom];
    Visible := False;
    OnScroll := sbScroll;
  end;

  pMain := TPanel.Create(Self);
  with pMain do
  begin
    Parent := AParent;
    AnchorSideLeft.Control := AParent;
    AnchorSideTop.Control := pMainDTU;
    AnchorSideTop.Side := asrBottom;
    AnchorSideRight.Control := sbV;
    AnchorSideBottom.Control := sbH;
    Anchors := [akTop, akLeft, akRight, akBottom];
    BevelOuter := bvNone;
  end;

  pMain.OnChangeBounds:=TryBoundSizerToDesignedForm;
end;

end.

