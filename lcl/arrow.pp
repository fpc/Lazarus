{
 /***************************************************************************
                               Arrow.pp
                             -------------------
                             Component Library Calendar Component
                   Initial Revision  : Wed Dec 05 2001

 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
}

{
@abstract(Arrow component)
@author(Shane Miller)
@created(05 Dev 2001)
}
unit arrow;

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
    procedure AttachSignals; override;
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

procedure TArrow.AttachSignals;
begin
   inherited;
end;

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

