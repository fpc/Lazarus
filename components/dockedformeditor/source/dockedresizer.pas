{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 Authors: Maciej Izak
          Michael W. Vogel

 The Resizer is a visual control that own two ScrollBars and the ResizeControl
 that shows the design form.
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
  DockedResizeControl, DockedDesignForm, DockedStrConsts;

type

  { TResizer }

  TResizer = class(TWinControl)
  private
    FDesignScroll: array[0..1] of Boolean;
    FDesignForm: TDesignForm;
    FPostponedAdjustResizeControl: Boolean;
    // To perform proper behaviour for scroolbar with "PageSize" we need to remember real
    // maximal values (is possible to scroll outside of range 0..(Max - PageSize),
    // after mouse click in button responsible for changing value of scrollbar,
    // our value is equal to Max :\). Workaround: we need to remember real max value in our own place
    FRealMaxH: Integer;
    FRealMaxV: Integer;
    FResizeControl: TResizeControl;
    FScrollBarHorz: TScrollBar;
    FScrollBarVert: TScrollBar;
    FScrollPos: TPoint;
    procedure FormResized(Sender: TObject);
    function GetFormContainer: TWinControl;
    procedure ScrollBarHorzMouseWheel(Sender: TObject; {%H-}Shift: TShiftState;
      WheelDelta: Integer; {%H-}MousePos: TPoint; var {%H-}Handled: Boolean);
    procedure ScrollBarVertMouseWheel(Sender: TObject; {%H-}Shift: TShiftState;
      WheelDelta: Integer; {%H-}MousePos: TPoint; var {%H-}Handled: Boolean);
    procedure SetDesignForm(AValue: TDesignForm);
    procedure SetDesignScroll(AIndex: Integer; AValue: Boolean);
    procedure ScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
  public
    constructor Create(TheOwner: TWinControl); reintroduce;
    destructor Destroy; override;
    procedure AdjustResizer(Sender: TObject);
    procedure DesignerSetFocus;
  public
    property DesignForm: TDesignForm read FDesignForm write SetDesignForm;
    property DesignScrollRight: Boolean index SB_Vert read FDesignScroll[SB_Vert] write SetDesignScroll;
    property DesignScrollBottom: Boolean index SB_Horz read FDesignScroll[SB_Horz] write SetDesignScroll;
    property FormContainer: TWinControl read GetFormContainer;
    property ResizeControl: TResizeControl read FResizeControl;
  end;

implementation

{ TResizer }

procedure TResizer.FormResized(Sender: TObject);
begin
  DesignForm.Form.Width  := ResizeControl.NewFormSize.X;
  DesignForm.Form.Height := ResizeControl.NewFormSize.Y;
  SetTimer(DesignForm.Form.Handle, WM_BOUNDTODESIGNTABSHEET, 10, nil);
end;

function TResizer.GetFormContainer: TWinControl;
begin
  Result := ResizeControl.FormContainer;
end;

procedure TResizer.ScrollBarHorzMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  LScrollPos: Integer;
begin
  LScrollPos := FScrollPos.x - WheelDelta;
  FScrollBarHorz.Position := LScrollPos;
  ScrollBarScroll(FScrollBarHorz, scEndScroll, LScrollPos);
  Handled := True;
end;

procedure TResizer.ScrollBarVertMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  LScrollPos: Integer;
begin
  LScrollPos := FScrollPos.y - WheelDelta;
  FScrollBarVert.Position := LScrollPos;
  ScrollBarScroll(FScrollBarVert, scEndScroll, LScrollPos);
  Handled := True;
end;

procedure TResizer.SetDesignForm(AValue: TDesignForm);
begin
  {$IFDEF DEBUGDOCKEDFORMEDITOR}
  if Assigned(AValue) then DebugLn('TResizer.SetDesignForm: New Designform: ', DbgSName(AValue.Form))
                      else DebugLn('TResizer.SetDesignForm: New Designform: nil');
  {$ENDIF}
  if FDesignForm <> nil then
    FDesignForm.OnChangeHackedBounds := nil;
  FDesignForm := AValue;
  if Assigned(FDesignForm) then
  begin
    FDesignForm.BeginUpdate;
    FDesignForm.Form.Parent := ResizeControl.FormContainer;
    FDesignForm.EndUpdate;
    FDesignForm.OnChangeHackedBounds := @AdjustResizer;
    if Assigned(FDesignForm.AnchorDesigner) then
      FDesignForm.AnchorDesigner.Parent := ResizeControl.AnchorContainer;
  end;
  ResizeControl.DesignForm := AValue;
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
    SB_Horz: PerformScroll(FScrollBarHorz);
    SB_Vert: PerformScroll(FScrollBarVert);
  else
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  end;
end;

procedure TResizer.ScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  case ScrollCode of
    scLineDown: ScrollPos := ScrollPos + 50;
    scLineUp:   ScrollPos := ScrollPos - 50;
    scPageDown:
      begin
        if Sender = FScrollBarHorz then ScrollPos := ScrollPos + ResizeControl.Width;
        if Sender = FScrollBarVert then ScrollPos := ScrollPos + ResizeControl.Height;
      end;
    scPageUp:
      begin
        if Sender = FScrollBarHorz then ScrollPos := ScrollPos - ResizeControl.Width;
        if Sender = FScrollBarVert then ScrollPos := ScrollPos - ResizeControl.Height;
      end;
  end;

  DesignForm.BeginUpdate;
  if Sender = FScrollBarVert then
  begin
    // Warning - don't overflow the range! (go to description for FRealMaxV)
    ScrollPos := Min(ScrollPos, FRealMaxV);
    ScrollPos := Max(ScrollPos, 0);
    FScrollPos.y := ScrollPos;
  end;
  if Sender = FScrollBarHorz then
  begin
    ScrollPos := Min(ScrollPos, FRealMaxH);
    ScrollPos := Max(ScrollPos, 0);
    FScrollPos.x := ScrollPos;
  end;
  DesignForm.EndUpdate;
  if not FPostponedAdjustResizeControl then
  begin
    ResizeControl.AdjustBounds(FScrollPos);
    ResizeControl.DesignerSetFocus;
  end;
  DesignForm.Form.Invalidate;
end;

constructor TResizer.Create(TheOwner: TWinControl);
begin
  inherited Create(TheOwner);

  Align := alClient;
  FPostponedAdjustResizeControl := False;
  FScrollPos := Point(0, 0);

  FScrollBarVert := TScrollBar.Create(nil);
  FScrollBarVert.Kind := sbVertical;
  FScrollBarVert.Parent := Self;
  FScrollBarVert.AnchorSideTop.Control := Self;
  FScrollBarVert.AnchorSideRight.Control := Self;
  FScrollBarVert.AnchorSideRight.Side := asrRight;
  FScrollBarVert.AnchorSideBottom.Side := asrBottom;
  FScrollBarVert.Anchors := [akTop, akRight, akBottom];
  FScrollBarVert.Visible := False;
  FScrollBarVert.OnScroll := @ScrollBarScroll;
  FScrollBarVert.AddHandlerOnMouseWheel(@ScrollBarVertMouseWheel);

  FScrollBarHorz := TScrollBar.Create(nil);
  FScrollBarHorz.Parent := Self;
  FScrollBarHorz.AnchorSideLeft.Control := Self;
  FScrollBarHorz.AnchorSideRight.Side := asrRight;
  FScrollBarHorz.AnchorSideBottom.Control := Self;
  FScrollBarHorz.AnchorSideBottom.Side := asrBottom;
  FScrollBarHorz.Anchors := [akLeft, akRight, akBottom];
  FScrollBarHorz.Visible := False;
  FScrollBarHorz.OnScroll := @ScrollBarScroll;
  FScrollBarHorz.AddHandlerOnMouseWheel(@ScrollBarHorzMouseWheel);

  FResizeControl := TResizeControl.Create(nil);
  FResizeControl.Name := '';
  FResizeControl.Parent := Self;
  FResizeControl.AnchorSideLeft.Control := Self;
  FResizeControl.AnchorSideTop.Control := Self;
  FResizeControl.AnchorSideRight.Control := FScrollBarVert;
  FResizeControl.AnchorSideBottom.Control := FScrollBarHorz;
  FResizeControl.Anchors := [akTop, akLeft, akRight, akBottom];

  FScrollBarVert.AnchorSideBottom.Control := ResizeControl;
  FScrollBarHorz.AnchorSideRight.Control := ResizeControl;

  FResizeControl.OnResized := @FormResized;
  FResizeControl.OnChangeBounds := @AdjustResizer;
end;

destructor TResizer.Destroy;
begin
  Pointer(FDesignForm) := nil;
  FreeAndNil(FResizeControl);
  FreeAndNil(FScrollBarVert);
  FreeAndNil(FScrollBarHorz);
  inherited Destroy;
end;

procedure TResizer.AdjustResizer(Sender: TObject);
var
  LWidth, LHeight: Integer;
  LScrollPos: Integer;
begin
  if not Assigned(FDesignForm) then Exit;
  if ResizeControl.Resizing then
  begin
    DesignForm.BeginUpdate;
    DesignForm.EndUpdate;
    ResizeControl.AdjustBounds(FScrollPos);
    Exit;
  end;

  LWidth  := FDesignForm.Width  + 2 * ResizeControl.SizerGripSize;
  LHeight := FDesignForm.Height + 2 * ResizeControl.SizerGripSize;
  {$IFDEF DEBUGDOCKEDFORMEDITOR} DebugLn('TResizer.AdjustResizer Resizer Width:', DbgS(LWidth), ' Height:', DbgS(LHeight)); {$ENDIF}

  FPostponedAdjustResizeControl := True;
  if ResizeControl.Width < LWidth then
  begin
    // if designer frame is smaller as scrollbar, show scrollbar
    DesignScrollBottom := True;
    FScrollBarHorz.Max := LWidth;
    FRealMaxH := LWidth - ResizeControl.Width;
    FScrollBarHorz.PageSize := ResizeControl.Width;
    if FScrollPos.x > FRealMaxH then
    begin
      FScrollPos.x := FRealMaxH;
      LScrollPos := FScrollPos.x;
      ScrollBarScroll(FScrollBarHorz, scEndScroll, LScrollPos);
    end;
  end else begin
    // invisible ScrollBar
    DesignScrollBottom := False;
    LScrollPos := 0;
    ScrollBarScroll(FScrollBarHorz, scEndScroll, LScrollPos);
  end;

  if ResizeControl.Height < LHeight then
  begin
    // if designer frame is higher as scrollbar, show scrollbar
    DesignScrollRight := True;
    FScrollBarVert.Max := LHeight;
    FRealMaxV := LHeight - ResizeControl.Height;
    FScrollBarVert.PageSize := ResizeControl.Height;
    if FScrollPos.y > FRealMaxV then
    begin
      FScrollPos.y := FRealMaxV;
      LScrollPos := FScrollPos.y;
      ScrollBarScroll(FScrollBarVert, scEndScroll, LScrollPos);
    end;
  end else begin
    DesignScrollRight := False;
    LScrollPos := 0;
    ScrollBarScroll(FScrollBarVert, scEndScroll, LScrollPos);
  end;
  FPostponedAdjustResizeControl := False;

  ResizeControl.AdjustBounds(FScrollPos);
  ResizeControl.ClientChangeBounds(nil);
end;

procedure TResizer.DesignerSetFocus;
begin
  ResizeControl.DesignerSetFocus;
  if Assigned(FDesignForm) and Assigned(FDesignForm.AnchorDesigner) then
    FDesignForm.AnchorDesigner.OnMouseWheel := @ScrollBarVertMouseWheel;
end;

end.

