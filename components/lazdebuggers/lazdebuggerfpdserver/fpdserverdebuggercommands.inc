{ TFPDSendFilenameCommand }

procedure TFPDSendFilenameCommand.ComposeJSon(AJsonObject: TJSONObject);
begin
  inherited ComposeJSon(AJsonObject);
  AJsonObject.Add('command','filename');
  AJsonObject.Add('filename',FFileName);
end;

constructor TFPDSendFilenameCommand.create(AFileName: string);
begin
  inherited create;
  FFileName:=AFileName;
end;

{ TFPDSendRunCommand }

procedure TFPDSendRunCommand.DoOnCommandFailed(ACommandResponse: TJSonObject);
begin
  FServerDebugger.DoOnRunFailed;
end;

procedure TFPDSendRunCommand.ComposeJSon(AJsonObject: TJSONObject);
begin
  inherited ComposeJSon(AJsonObject);
  AJsonObject.Add('command','run');
end;

{ TFPDSendStepOutCommand }

procedure TFPDSendStepOutCommand.ComposeJSon(AJsonObject: TJSONObject);
begin
  inherited ComposeJSon(AJsonObject);
  AJsonObject.Add('command','stepout');
end;

{ TFPDSendStepOverInstrCommand }

procedure TFPDSendStepOverInstrCommand.ComposeJSon(AJsonObject: TJSONObject);
begin
  inherited ComposeJSon(AJsonObject);
  AJsonObject.Add('command','stepoverinstr');
end;

{ TFPDSendStepIntoInstrCommand }

procedure TFPDSendStepIntoInstrCommand.ComposeJSon(AJsonObject: TJSONObject);
begin
  inherited ComposeJSon(AJsonObject);
  AJsonObject.Add('command','stepintoinstr');
end;

{ TFPDSendStepCommand }

procedure TFPDSendStepCommand.ComposeJSon(AJsonObject: TJSONObject);
begin
  inherited ComposeJSon(AJsonObject);
  AJsonObject.Add('command','step');
end;

{ TFPDSendStopCommand }

procedure TFPDSendStopCommand.ComposeJSon(AJsonObject: TJSONObject);
begin
  inherited ComposeJSon(AJsonObject);
  AJsonObject.Add('command','stop');
end;

{ TFPDSendNextCommand }

procedure TFPDSendNextCommand.ComposeJSon(AJsonObject: TJSONObject);
begin
  inherited ComposeJSon(AJsonObject);
  AJsonObject.Add('command','next');
end;

{ TFPDSendDoCurrentCommand }

procedure TFPDSendDoCurrentCommand.ComposeJSon(AJsonObject: TJSONObject);
begin
  inherited ComposeJSon(AJsonObject);
  AJsonObject.Add('command','getlocationinfo');
end;

procedure TFPDSendDoCurrentCommand.DoOnCommandSuccesfull(ACommandResponse: TJSonObject);
var
  LocRecJSon: TJSONObject;
  LocRec: TDBGLocationRec;
begin
  LocRecJSon := ACommandResponse.Find('locationRec') as TJSONObject;
  if assigned(LocRecJSon) then
    begin
    LocRec.Address:=Hex2Dec(LocRecJSon.Get('address','0'));
    LocRec.FuncName:=LocRecJSon.Get('funcName','');
    LocRec.SrcFile:=LocRecJSon.Get('srcFile','');
    LocRec.SrcFullName:=LocRecJSon.Get('srcFullName','');
    LocRec.SrcLine:=LocRecJSon.Get('srcLine',-1);
    FServerDebugger.DoOnDoCurrentSuccessfull(LocRec);
    end;
end;

{ TFPDSendAddBreakpointCommand }

procedure TFPDSendAddBreakpointCommand.DoOnCommandFailed(ACommandResponse: TJSonObject);
var
  ABreakpoint: TFPBreakpoint;
begin
  ABreakpoint := TFPBreakpoints(FServerDebugger.BreakPoints).FindByUID(CommandUID);
  if assigned(ABreakpoint) then
    begin
    ABreakpoint.Address:=0;
    ABreakpoint.SetInvalid;
    ABreakpoint.DoChanged;
    end;
end;

procedure TFPDSendAddBreakpointCommand.DoOnCommandSuccesfull(ACommandResponse: TJSonObject);
var
  ABreakpoint: TFPBreakpoint;
begin
  ABreakpoint := TFPBreakpoints(FServerDebugger.BreakPoints).FindByUID(CommandUID);
  if assigned(ABreakpoint) then
    begin
    ABreakpoint.Address:=Hex2Dec(ACommandResponse.get('breakpointLocation','0'));
    ABreakpoint.SetValid;
    ABreakpoint.DoChanged;
    end;
end;

procedure TFPDSendAddBreakpointCommand.ComposeJSon(AJsonObject: TJSONObject);
begin
  inherited ComposeJSon(AJsonObject);
  AJsonObject.Add('command','addbreakpoint');
  if FFileName<>'' then
    begin
    AJsonObject.Add('filename',FFileName);
    AJsonObject.Add('line',FLineNr);
    end
  else
    begin
    AJsonObject.Add('location', Dec2Numb(FLocation, 8, 16));
    end;
end;

constructor TFPDSendAddBreakpointCommand.create(AFileName: string; ALineNr: integer);
begin
  inherited create;
  FFileName:=AFileName;
  FLineNr:=ALineNr;
end;

constructor TFPDSendAddBreakpointCommand.create(ALocation: TDBGPtr);
begin
  inherited create;
  FLocation:=ALocation;
end;

{ TFPDSendContinueCommand }

procedure TFPDSendContinueCommand.ComposeJSon(AJsonObject: TJSONObject);
begin
  inherited ComposeJSon(AJsonObject);
  AJsonObject.Add('command','continue');
end;

