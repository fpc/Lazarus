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
 *  See the file COPYING.LCL, included in this distribution,                 *
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
  SysUtils, Classes, Controls, vclGlobals, lMessages;
  
  
Type

  TArrowType = (atUp, atDown, atLeft, atRight);
  TShadowType = (stNone, stIn, stOut, stEtchedIn, stEtchedOut);

  TLMArrow = record
    ArrowType : TArrowType;
    ShadowType : TShadowType;
  end;

  TArrow = class(TCustomControl)
  private
    FArrowType : TArrowType;
    FShadowType : TShadowType;
    //FArrowChanged: TNotifyEvent;
    function GetShadowType: TShadowType;
    procedure SetShadowType(const AValue: TShadowType);
    function GetArrowType: TArrowType;
    procedure SetArrowType(const AValue: TArrowType);
    procedure SetProps;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    Property ArrowType : TArrowType read GetArrowType write SetArrowType;
    property ShadowType : TShadowType read fShadowType write SetShadowType;
    property Visible;
    property OnClick;
    property OnMouseMove;
    property OnMouseDown;
  end;
  
implementation


{ TArrow }

constructor TArrow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {create the control}
  fCompStyle := csArrow;
  setbounds(0,0,10,10);
  fArrowType := atLeft;
  fShadowType := stEtchedIn;
end;

destructor TArrow.Destroy;
begin
  inherited;
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
var
  Temp : TLMArrow;
begin

  if HandleAllocated then
     begin
       Temp.ArrowType := FArrowType;
       Temp.ShadowType := FShadowType;
       CNSendMessage(LM_SetValue,self,@Temp);
     end;
end;

procedure TArrow.SetShadowType(const AValue: TShadowType);
begin
  FShadowType := aValue;
  SetProps;
end;

end.

