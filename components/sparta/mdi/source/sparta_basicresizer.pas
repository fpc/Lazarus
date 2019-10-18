unit sparta_BasicResizer;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils,
  LCLType, Controls, ExtCtrls, Forms, StdCtrls, Buttons, Dialogs,
  sparta_BasicResizeFrame, sparta_InterfacesMDI, sparta_AbstractResizer;

type

  { TBasicResizer }

  TBasicResizer = class(TAbstractResizer)
  private
    FDesignedForm: IDesignedForm;
    FResizerFrame: TBasicResizeFrame;
  protected
    function GetActiveResizeFrame: IResizeFrame; override;
    function GetActiveDesignedForm: IDesignedForm; override;
    procedure SetDesignedForm(const AValue: IDesignedForm); virtual;
  public
    constructor Create(AParent: TWinControl; AResizerFrameClass: TResizerFrameClass); override;
    destructor Destroy; override;
    //procedure TryBoundSizerToDesignedForm(Sender: TObject); override;

    property DesignedForm: IDesignedForm read FDesignedForm write SetDesignedForm;
  end;

implementation

{ TBasicResizer }

procedure TBasicResizer.SetDesignedForm(const AValue: IDesignedForm);

  function FindFirstFormParent: TCustomForm;
  begin
    Result := TCustomForm(FResizerFrame.Parent);
    while not (Result is TCustomForm) do
      Result := TCustomForm(Result.Parent);
  end;

begin
  if FDesignedForm <> nil then
    FDesignedForm.OnChangeHackedBounds := nil;
  FDesignedForm := AValue;
  if FDesignedForm <> nil then
  begin
    FDesignedForm.BeginUpdate;
    FDesignedForm.Form.Parent := FResizerFrame.pFormHandler;
    // for big forms (bigger than screen resolution) we need to refresh Real* values
    DesignedForm.RealWidth := DesignedForm.Width;
    DesignedForm.RealHeight := DesignedForm.Height;

    FDesignedForm.EndUpdate;
    FDesignedForm.OnChangeHackedBounds := TryBoundSizerToDesignedForm;
  end;
  FResizerFrame.DesignedForm := AValue;
end;

constructor TBasicResizer.Create(AParent: TWinControl;
  AResizerFrameClass: TResizerFrameClass);
begin
  inherited Create(AParent, AResizerFrameClass);
  FResizerFrame := CreateResizeFrame;
end;

destructor TBasicResizer.Destroy;
begin
  Pointer(FDesignedForm) := nil;
  inherited Destroy;
end;

(*procedure TBasicResizer.TryBoundSizerToDesignedForm(Sender: TObject);
var
  LWidth, LHeight: Integer;
  LScrollPos: Integer;
begin
  if DesignedForm = nil then
    Exit;

  FResizerFrame.Constraints.MaxWidth := pMain.Width;
  FResizerFrame.Constraints.MaxHeight := pMain.Height;

  LWidth  := DesignedForm.Width + FResizerFrame.BgLeftMargin + FResizerFrame.BgRightMargin + 2*FResizerFrame.SizerRectSize;
  LHeight := DesignedForm.Height + FResizerFrame.BgTopMargin + FResizerFrame.BgBottomMargin + 2*FResizerFrame.SizerRectSize;
  if not FResizerFrame.NodePositioning then
  begin
    FResizerFrame.Width := LWidth;
    FResizerFrame.Height := LHeight;
    // after enlargement and after reducing constrait not work for frame (LCL bug)
    if FResizerFrame.Width > FResizerFrame.Constraints.MaxWidth then
      FResizerFrame.Width := FResizerFrame.Constraints.MaxWidth;
    if FResizerFrame.Height > FResizerFrame.Constraints.MaxHeight then
      FResizerFrame.Height := FResizerFrame.Constraints.MaxHeight;
  end;

  FResizerFrame.PositionNodes(FResizerFrame);

  DesignScrollBottom := FResizerFrame.Width < LWidth;
  sbH.Max := LWidth;
  FRealMaxH := LWidth - FResizerFrame.Width;
  sbH.PageSize := FResizerFrame.Width;
  if FResizerFrame.HorizontalScrollPos > FRealMaxH then
  begin
    FResizerFrame.HorizontalScrollPos := FRealMaxH;
    LScrollPos := FResizerFrame.HorizontalScrollPos;
    sbScroll(sbH, scEndScroll, LScrollPos);
  end;

  DesignScrollRight := FResizerFrame.Height < LHeight;
  sbV.Max := LHeight;
  FRealMaxV := LHeight - FResizerFrame.Height;
  sbV.PageSize := FResizerFrame.Height;
  if FResizerFrame.VerticalScrollPos > FRealMaxV then
  begin
    FResizerFrame.VerticalScrollPos := FRealMaxV;
    LScrollPos := FResizerFrame.VerticalScrollPos;
    sbScroll(sbV, scEndScroll, LScrollPos);
  end;

  {!}
  FResizerFrame.ClientChangeBounds(nil);

  // each editor can have scrolls in different positions.
  // this is our place where we can call event to set scroll positions.
  LScrollPos := FResizerFrame.VerticalScrollPos;
  sbScroll(sbV, scEndScroll, LScrollPos);
  LScrollPos := FResizerFrame.HorizontalScrollPos;
  sbScroll(sbH, scEndScroll, LScrollPos);

  if Supports(FDesignedForm, IDesignedFormBackground) then
    (FDesignedForm as IDesignedFormBackground).RefreshValues;

  FResizerFrame.DesignerSetFocus;
end;*)

function TBasicResizer.GetActiveResizeFrame: IResizeFrame;
begin
  Result := FResizerFrame;
end;

function TBasicResizer.GetActiveDesignedForm: IDesignedForm;
begin
  Result := FDesignedForm;
end;


end.

