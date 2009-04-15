{%MainUnit ../extctrls.pp}

{******************************************************************************
                                  TIdleTimer
 ******************************************************************************

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

{ TIdleTimer }

procedure TCustomIdleTimer.SetAutoEnabled(const AValue: boolean);
begin
  if FAutoEnabled=AValue then exit;
  FAutoEnabled:=AValue;
  UpdateHandlers;
end;

procedure TCustomIdleTimer.DoOnIdle(Sender: TObject; var Done: Boolean);
begin
  if FFireOnIdle then begin
    DoOnTimer;
    Done:=not FFireOnIdle;
  end;
  if not AutoEnabled then exit;
  // automatic start, stop or restart
  if AutoEndEvent=itaOnIdle then
    Enabled:=false;
  if AutoStartEvent=itaOnIdle then
    Enabled:=true;
end;

procedure TCustomIdleTimer.DoOnIdleEnd(Sender: TObject);
begin
  if not AutoEnabled then exit;
  // automatic start, stop or restart
  if AutoEndEvent=itaOnIdleEnd then
    Enabled:=false;
  if AutoStartEvent=itaOnIdleEnd then
    Enabled:=true;
end;

procedure TCustomIdleTimer.DoOnUserInput(Sender: TObject; Msg: Cardinal);
begin
  FireOnIdle:=false;
  if not AutoEnabled then exit;
  // automatic start, stop or restart
  if AutoEndEvent=itaOnUserInput then
    Enabled:=false;
  if AutoStartEvent=itaOnUserInput then
    Enabled:=true;
end;

procedure TCustomIdleTimer.Loaded;
begin
  inherited Loaded;
  UpdateHandlers;
end;

constructor TCustomIdleTimer.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FAutoEnabled := False;
  FAutoStartEvent := itaOnIdle;
  FAutoEndEvent := itaOnUserInput;
end;

destructor TCustomIdleTimer.Destroy;
begin
  AutoEnabled := False;
  inherited Destroy;
end;

procedure TCustomIdleTimer.UpdateHandlers;
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

// included by extctrls.pp
