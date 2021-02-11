{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 Authors: Maciej Izak
          Michael W. Vogel

 The Resizer is a visual control that own two ScrollBars and a window
 (ResizerFrame) that shows the design form.
}

unit DockedResizer;

{$mode objfpc}{$H+}
{ $define DEBUGDOCKEDFORMEDITOR}

interface

uses
  // RTL, FCL
  Classes, SysUtils, Math,
  // LCL
  LCLType, Controls, ExtCtrls, Forms, StdCtrls, Buttons, Dialogs, LCLIntf,
  LCLProc,
  // DockedFormEditor
  DockedResizeFrame, DockedDesignForm, DockedStrConsts;

type

  { TResizer }

  TResizer = class(TWinControl)
  private
    // To perform proper behaviour for scroolbar with "PageSize" we need to remember real
    // maximal values (is possible to scroll outside of range 0..(Max - PageSize),
    // after mouse click in button responsible for changing value of scrollbar,
    // our value is equal to Max :\). Workaround: we need to remember real max value in our own place
    FRealMaxH: Integer;
    FRealMaxV: Integer;
    FPostponedAdjustPanelResizer: Boolean;
    FDesignScroll: array[0..1] of Boolean;
    FDesignForm: TDesignForm;
    procedure FormResized(Sender: TObject);
    function GetFormContainer: TWinControl;
    procedure SetDesignForm(AValue: TDesignForm);
    procedure SetDesignScroll(AIndex: Integer; AValue: Boolean);
    procedure ScrollBarScroll(Sender: TObject; {%H-}ScrollCode: TScrollCode; var ScrollPos: Integer);
  public
    ResizeFrame: TResizeFrame;
    ScrollBarVert: TScrollBar;
    ScrollBarHorz: TScrollBar;

    constructor Create(TheOwner: TWinControl); reintroduce;
    destructor Destroy; override;
    procedure AdjustResizer(Sender: TObject);
    procedure DesignerSetFocus;
  public
    property DesignForm: TDesignForm read FDesignForm write SetDesignForm;
    property DesignScrollRight: Boolean index SB_Vert read FDesignScroll[SB_Vert] write SetDesignScroll;
    property DesignScrollBottom: Boolean index SB_Horz read FDesignScroll[SB_Horz] write SetDesignScroll;
    property FormContainer: TWinControl read GetFormContainer;
  end;

implementation

{ TResizer }

procedure TResizer.FormResized(Sender: TObject);
begin
  DesignForm.Form.Width  := ResizeFrame.NewFormSize.X;
  DesignForm.Form.Height := ResizeFrame.NewFormSize.Y;
  SetTimer(DesignForm.Form.Handle, WM_BOUNDTODESIGNTABSHEET, 10, nil);
end;

function TResizer.GetFormContainer: TWinControl;
begin
  Result := ResizeFrame.PanelFormContainer;
end;

procedure TResizer.SetDesignForm(AValue: TDesignForm);
begin
  if FDesignForm <> nil then
    FDesignForm.OnChangeHackedBounds := nil;
  FDesignForm := AValue;
  if Assigned(FDesignForm) then
  begin
    FDesignForm.BeginUpdate;
    FDesignForm.Form.Parent := ResizeFrame.PanelFormContainer;
    FDesignForm.EndUpdate;
    FDesignForm.OnChangeHackedBounds := @AdjustResizer;
  end;
  ResizeFrame.DesignForm := AValue;
end;

procedure TResizer.SetDesignScroll(AIndex: Integer; AValue: Boolean);

  procedure PerformScroll(AScroll: TScrollBar);
  begin
    AScroll.Visible  := AValue;
    AScroll.Position := 0;
  end;

begin
  if FDesignScroll[AIndex] = AValue then Exit;
  FDesignScroll[AIndex] := AValue;
  case AIndex of
    SB_Horz: PerformScroll(ScrollBarHorz);
    SB_Vert: PerformScroll(ScrollBarVert);
  else
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  end;
end;

procedure TResizer.ScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
var
  LScrollPos: Integer;
begin
  DesignForm.BeginUpdate;
  if Sender = ScrollBarVert then
  begin
    // Warning - don't overflow the range! (go to description for FRealMaxV)
    ScrollPos := Min(ScrollPos, FRealMaxV);
    ResizeFrame.VertScrollPos := ScrollPos;
    // scroll for form
    LScrollPos := Max(ScrollPos - ResizeFrame.SizerGripSize, 0);
    DesignForm.VertScrollPosition := LScrollPos;
  end;
  if Sender = ScrollBarHorz then
  begin
    ScrollPos := Min(ScrollPos, FRealMaxH);
    ResizeFrame.HorzScrollPos := ScrollPos;
    // scroll for form
    LScrollPos := Max(ScrollPos - ResizeFrame.SizerGripSize, 0);
    DesignForm.HorzScrollPosition := LScrollPos;
  end;
  DesignForm.EndUpdate;
  if not FPostponedAdjustPanelResizer then
  begin
    ResizeFrame.AdjustPanelResizer;
    ResizeFrame.DesignerSetFocus;
  end;
  DesignForm.Form.Invalidate;
end;

constructor TResizer.Create(TheOwner: TWinControl);
begin
  inherited Create(TheOwner);

  Align := alClient;
  FPostponedAdjustPanelResizer := False;

  ScrollBarVert := TScrollBar.Create(nil);
  ScrollBarVert.Kind := sbVertical;
  ScrollBarVert.Parent := Self;
  ScrollBarVert.AnchorSideTop.Control := Self;
  ScrollBarVert.AnchorSideRight.Control := Self;
  ScrollBarVert.AnchorSideRight.Side := asrRight;
  ScrollBarVert.AnchorSideBottom.Side := asrBottom;
  ScrollBarVert.Anchors := [akTop, akRight, akBottom];
  ScrollBarVert.Visible := False;
  ScrollBarVert.OnScroll := @ScrollBarScroll;

  ScrollBarHorz := TScrollBar.Create(nil);
  ScrollBarHorz.Parent := Self;
  ScrollBarHorz.AnchorSideLeft.Control := Self;
  ScrollBarHorz.AnchorSideRight.Side := asrRight;
  ScrollBarHorz.AnchorSideBottom.Control := Self;
  ScrollBarHorz.AnchorSideBottom.Side := asrBottom;
  ScrollBarHorz.Anchors := [akLeft, akRight, akBottom];
  ScrollBarHorz.Visible := False;
  ScrollBarHorz.OnScroll := @ScrollBarScroll;

  ResizeFrame := TResizeFrame.Create(nil);
  ResizeFrame.Name := '';
  ResizeFrame.Parent := Self;
  ResizeFrame.AnchorSideLeft.Control := Self;
  ResizeFrame.AnchorSideTop.Control := Self;
  ResizeFrame.AnchorSideRight.Control := ScrollBarVert;
  ResizeFrame.AnchorSideBottom.Control := ScrollBarHorz;
  ResizeFrame.Anchors := [akTop, akLeft, akRight, akBottom];

  ScrollBarVert.AnchorSideBottom.Control := ResizeFrame;
  ScrollBarHorz.AnchorSideRight.Control := ResizeFrame;

  ResizeFrame.OnResized := @FormResized;
  ResizeFrame.OnChangeBounds := @AdjustResizer;
end;

destructor TResizer.Destroy;
begin
  Pointer(FDesignForm) := nil;
  FreeAndNil(ResizeFrame);
  FreeAndNil(ScrollBarVert);
  FreeAndNil(ScrollBarHorz);
  inherited Destroy;
end;

procedure TResizer.AdjustResizer(Sender: TObject);
var
  LWidth, LHeight: Integer;
  LScrollPos: Integer;
begin
  if not Assigned(FDesignForm) then Exit;
  LWidth  := FDesignForm.Width  + 2 * ResizeFrame.SizerGripSize;
  LHeight := FDesignForm.Height + 2 * ResizeFrame.SizerGripSize + ResizeFrame.PanelFakeMenu.Height;
  {$IFDEF DEBUGDOCKEDFORMEDITOR} DebugLn('TResizer.AdjustResizer Resizer Width:', DbgS(LWidth), ' Height:', DbgS(LHeight)); {$ENDIF}

  FPostponedAdjustPanelResizer := True;
  if ResizeFrame.Width < LWidth then
  begin
    // if designer frame is smaller as scrollbar, show scrollbar
    DesignScrollBottom := True;
    ScrollBarHorz.Max := LWidth;
    FRealMaxH := LWidth - ResizeFrame.Width;
    ScrollBarHorz.PageSize := ResizeFrame.Width;
    if ResizeFrame.HorzScrollPos > FRealMaxH then
    begin
      ResizeFrame.HorzScrollPos := FRealMaxH;
      LScrollPos := ResizeFrame.HorzScrollPos;
      ScrollBarScroll(ScrollBarHorz, scEndScroll, LScrollPos);
    end;
  end else begin
    // invisible ScrollBar
    DesignScrollBottom := False;
    LScrollPos := 0;
    ScrollBarScroll(ScrollBarHorz, scEndScroll, LScrollPos);
  end;

  if ResizeFrame.Height < LHeight then
  begin
    // if designer frame is higher as scrollbar, show scrollbar
    DesignScrollRight := True;
    ScrollBarVert.Max := LHeight;
    FRealMaxV := LHeight - ResizeFrame.Height;
    ScrollBarVert.PageSize := ResizeFrame.Height;
    if ResizeFrame.VertScrollPos > FRealMaxV then
    begin
      ResizeFrame.VertScrollPos := FRealMaxV;
      LScrollPos := ResizeFrame.VertScrollPos;
      ScrollBarScroll(ScrollBarVert, scEndScroll, LScrollPos);
    end;
  end else begin
    DesignScrollRight := False;
    LScrollPos := 0;
    ScrollBarScroll(ScrollBarVert, scEndScroll, LScrollPos);
  end;
  FPostponedAdjustPanelResizer := False;

  ResizeFrame.AdjustPanelResizer;
  ResizeFrame.ClientChangeBounds(nil);
end;

procedure TResizer.DesignerSetFocus;
begin
  ResizeFrame.DesignerSetFocus;
end;

end.

