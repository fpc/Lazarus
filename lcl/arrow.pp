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
  Types, SysUtils, Classes, LCLType, Controls;
  
type

  TArrowType = (atUp, atDown, atLeft, atRight);
  TShadowType = (stNone, stIn, stOut, stEtchedIn, stEtchedOut);

  { TArrow }

  TArrow = class(TCustomControl)
  private
    FArrowType : TArrowType;
    FShadowType : TShadowType;
    function GetShadowType: TShadowType;
    procedure SetShadowType(const AValue: TShadowType);
    function GetArrowType: TArrowType;
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
    property Align;
    property Anchors;
    property ArrowType: TArrowType read GetArrowType write SetArrowType default atLeft;
    property BorderSpacing;
    property ShadowType: TShadowType read fShadowType write SetShadowType default stEtchedIn;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
    property OnChangeBounds;
    property OnResize;
    property OnContextPopup;
    property PopupMenu;
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

function TArrow.GetArrowType: TArrowType;
begin
  Result := FArrowType;
end;

procedure TArrow.SetArrowType(const AValue: TArrowType);
begin
  fArrowType := AValue;
  SetProps;
end;

function TArrow.GetShadowType: TShadowType;
begin
  Result := FShadowType;
end;

procedure TArrow.SetProps;
begin
  if HandleAllocated and (not (csLoading in ComponentState)) then
    TWSArrowClass(WidgetSetClass).SetType(Self, FArrowType, FShadowType);
end;

class procedure TArrow.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterArrow;
end;

procedure TArrow.SetShadowType(const AValue: TShadowType);
begin
  FShadowType := aValue;
  SetProps;
end;

end.
