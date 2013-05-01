{
 /***************************************************************************
                               Arrow.pp
                             -------------------
                             Component Library Calendar Component
                   Initial Revision  : Wed Dec 05 2001

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

{
@abstract(Arrow component)
@author(Shane Miller)
@created(05 Dev 2001)
}
unit Arrow;

{$mode objfpc}{$H+}

interface

uses
  Types, SysUtils, Classes, LCLType, Controls, Graphics;
  
type

  TArrowType = (atUp, atDown, atLeft, atRight);
  TShadowType = (stNone, stIn, stOut, stEtchedIn, stEtchedOut);

  { TArrow }

  TArrow = class(TCustomControl)
  private
    fArrowType : TArrowType;
    fShadowType : TShadowType;
    procedure SetShadowType(const AValue: TShadowType);
    procedure SetArrowType(const AValue: TArrowType);
    procedure SetProps;
  protected
    class procedure WSRegisterClass; override;
    procedure Paint; override;
    class function GetControlClassDefaultSize: TSize; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Loaded; override;
    procedure InitializeWnd; override;
  published
    property ArrowType: TArrowType read fArrowType write SetArrowType default atLeft;
    property ShadowType: TShadowType read fShadowType write SetShadowType default stEtchedIn;
    property Align;
    property Anchors;
    property BorderSpacing;
    // The drawing code changes Color. More changes are needed if this is published.
    //property Color;
    property OnChangeBounds;
    property PopupMenu;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
    property OnResize;
  end;
  
procedure Register;
  
implementation

uses
  InterfaceBase, WSArrow;

procedure Register;
begin
  RegisterComponents('Misc',[TArrow]);
end;

{ TArrow }

constructor TArrow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csDoubleClicks];
  fCompStyle := csArrow;
  fArrowType := atLeft;
  fShadowType := stEtchedIn;
  Canvas.Brush.Color := clBtnFace;
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
end;

procedure TArrow.Loaded;
begin
  inherited Loaded;
  SetProps;
end;

procedure TArrow.Paint;
begin
  TWSArrowClass(WidgetSetClass).DrawArrow(Self, Canvas);
  inherited Paint;
end;

class function TArrow.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 10;
  Result.CY := 10;
end;

procedure TArrow.InitializeWnd;
begin
  inherited InitializeWnd;
  SetProps;
end;

procedure TArrow.SetArrowType(const AValue: TArrowType);
begin
  if fArrowType = aValue then Exit;
  fArrowType := AValue;
  SetProps;
  Invalidate;
end;

procedure TArrow.SetShadowType(const AValue: TShadowType);
begin
  if fShadowType = aValue then Exit;
  fShadowType := aValue;
  SetProps;
  Invalidate;
end;

procedure TArrow.SetProps;
begin
  if HandleAllocated and (not (csLoading in ComponentState)) then
    TWSArrowClass(WidgetSetClass).SetType(Self, fArrowType, fShadowType);
end;

class procedure TArrow.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterArrow;
end;

end.
