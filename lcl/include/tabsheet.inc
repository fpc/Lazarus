// included by comctrls.pp

{******************************************************************************
                                  TTabSheet
 ******************************************************************************

  Author: Mattias Gaertner

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

  Abstract:
    TTabSheet for LCL

  ToDo:
    - all
}


{ TTabSheet }

function TTabSheet.GetPageIndex: Integer;
begin
  Result:=FPageIndex;
end;

function TTabSheet.GetTabIndex: Integer;
begin
  // ToDo
  Result:=-1;
end;

procedure TTabSheet.SetImageIndex(AValue: TImageIndex);
begin
  if FImageIndex=AValue then exit;
  FImageIndex:=AValue;
  if not HandleAllocated or (csLoading in ComponentState) then exit;
  CNSendMessage(LM_NB_UpdateTab, Self, nil);
end;

procedure TTabSheet.SetPageControl(APageControl: TPageControl);
var
  OldPageControl: TPageControl;
begin
  OldPageControl:=PageControl;
  if OldPageControl=APageControl then exit;
  
end;

procedure TTabSheet.SetPageIndex(Value: Integer);
begin

end;

procedure TTabSheet.SetTabVisible(Value: Boolean);
begin

end;

procedure TTabSheet.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
end;

procedure TTabSheet.DoHide;
begin

end;

procedure TTabSheet.DoShow;
begin

end;

procedure TTabSheet.ReadState(Reader: TReader);
begin
  inherited ReadState(Reader);
end;

procedure TTabSheet.WMPaint(var Msg: TLMPaint);
begin
  if (PageControl<>nil) then begin
    if PageControl.ActivePage=Self then
      inherited WMPaint(Msg);
  end else
    inherited WMPaint(Msg);
end;

constructor TTabSheet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fImageIndex:=-1;
  FPageIndex:=-1;
  fCompStyle := csPage;
  fTabVisible := False;
  ControlStyle := ControlStyle + [csAcceptsControls];

  // set the default height and width
  if (AOwner<>nil) and (AOwner is TControl) then begin
    Align:=alClient;
  end else begin
    SetInitialBounds(0,0,120,100);
  end;

  Caption := '';
end;

destructor TTabSheet.Destroy;
begin
  inherited Destroy;
end;

// included by comctrls.pp

