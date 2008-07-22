{%MainUnit ../extctrls.pp}

{******************************************************************************
                                  TIdleTimer
 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

}

{ TIdleTimer }

procedure TIdleTimer.SetAutoEnabled(const AValue: boolean);
begin
  if FAutoEnabled=AValue then exit;
  FAutoEnabled:=AValue;
  UpdateHandlers;
end;

procedure TIdleTimer.DoOnIdle(Sender: TObject; var Done: Boolean);
begin
  if not AutoEnabled then exit;
  // automatic start, stop or restart
  if AutoEndEvent=itaOnIdle then
    Enabled:=false;
  if AutoStartEvent=itaOnIdle then
    Enabled:=true;
end;

procedure TIdleTimer.DoOnIdleEnd(Sender: TObject);
begin
  if not AutoEnabled then exit;
  // automatic start, stop or restart
  if AutoEndEvent=itaOnIdleEnd then
    Enabled:=false;
  if AutoStartEvent=itaOnIdleEnd then
    Enabled:=true;
end;

procedure TIdleTimer.DoOnUserInput(Sender: TObject; Msg: Cardinal);
begin
  if not AutoEnabled then exit;
  // automatic start, stop or restart
  if AutoEndEvent=itaOnUserInput then
    Enabled:=false;
  if AutoStartEvent=itaOnUserInput then
    Enabled:=true;
end;

procedure TIdleTimer.Loaded;
begin
  inherited Loaded;
  UpdateHandlers;
end;

constructor TIdleTimer.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FAutoStartEvent:=itaOnIdle;
  FAutoEndEvent:=itaOnUserInput;
end;

destructor TIdleTimer.Destroy;
begin
  AutoEnabled:=false;
  inherited Destroy;
end;

procedure TIdleTimer.UpdateHandlers;
begin
  if FAutoEnabled
  and ([csDesigning,csLoading,csDestroying]*ComponentState=[]) then begin
    // connect handlers
    if FHandlersConnected or (Application=nil) then exit;
    Application.AddOnIdleHandler(@DoOnIdle,true);
    Application.AddOnIdleEndHandler(@DoOnIdleEnd,true);
    Application.AddOnUserInputHandler(@DoOnUserInput,true);
    FHandlersConnected:=true;
  end else begin
    // disconnect handlers
    if (not FHandlersConnected) or (Application=nil) then exit;
    Application.RemoveOnIdleHandler(@DoOnIdle);
    Application.RemoveOnIdleEndHandler(@DoOnIdleEnd);
    Application.RemoveOnUserInputHandler(@DoOnUserInput);
    FHandlersConnected:=false;
  end;
end;

procedure TIdleTimer.SetAutoEndEvent(const AValue: TIdleTimerAutoEvent);
begin
  if FAutoEndEvent=AValue then exit;
  FAutoEndEvent:=AValue;
end;

procedure TIdleTimer.SetAutoStartEvent(const AValue: TIdleTimerAutoEvent);
begin
  if FAutoStartEvent=AValue then exit;
  FAutoStartEvent:=AValue;
end;


// included by extctrls.pp
