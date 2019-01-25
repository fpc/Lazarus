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
    ABreakpoint.ServerId:=0;
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
    ABreakpoint.ServerId :=StrToInt(ACommandResponse.get('BreakpointServerIdr','0'));
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

procedure TFPDSendContinueCommand.DoOnCommandSuccesfull(ACommandResponse: TJSonObject);
begin
  inherited DoOnCommandSuccesfull(ACommandResponse);
  FServerDebugger.DoOnContinueSuccessfull;
end;

{ TFPDSendCallStackCommand }

procedure TFPDSendCallStackCommand.DoCallStackFreed(Sender: TObject);
begin
  FCallStack:=nil;
end;

procedure TFPDSendCallStackCommand.ComposeJSon(AJsonObject: TJSONObject);
begin
  inherited ComposeJSon(AJsonObject);
  AJsonObject.Add('command','stacktrace');
end;

constructor TFPDSendCallStackCommand.create(ACallStack: TCallStackBase; ACallStackSupplier: TCallStackSupplier);
begin
  inherited create(True);
  ACallStack.AddFreeNotification(@DoCallStackFreed);
  FCallStack := ACallStack;
  FCallStackSupplier := ACallStackSupplier;
end;

destructor TFPDSendCallStackCommand.Destroy;
begin
  if assigned(FCallStack) then
    FCallStack.RemoveFreeNotification(@DoCallStackFreed);
  inherited Destroy;
end;

procedure TFPDSendCallStackCommand.DoOnCommandSuccesfull(ACommandResponse: TJSonObject);
var
  JSonCallStackArr: TJSONArray;
  JSonCallStackEntryObj: TJSONObject;
  e: TCallStackEntry;
  It: TMapIterator;
  AnAddress: TDBGPtr;
  FunctionName: string;
  SourceFile: string;
  Line: integer;

begin
  inherited DoOnCommandSuccesfull(ACommandResponse);
  if assigned(FCallStack) then
    begin
    JSonCallStackArr := ACommandResponse.Get('callstack', TJSONArray(nil));
    if assigned(JSonCallStackArr) and (JSonCallStackArr.Count>0) then
      begin
      FCallStack.Count:=JSonCallStackArr.Count;
      FCallStack.SetCountValidity(ddsValid);

      It := TMapIterator.Create(FCallstack.RawEntries);

      if not It.Locate(FCallstack.LowestUnknown )
      then if not It.EOM
      then It.Next;

      while (not IT.EOM) and (TCallStackEntry(It.DataPtr^).Index < FCallstack.HighestUnknown) do
        begin
        e := TCallStackEntry(It.DataPtr^);
        if e.Validity = ddsRequested then
          begin
          JSonCallStackEntryObj := JSonCallStackArr.Items[e.Index] as TJSONObject;

          AnAddress:=Hex2Dec(JSonCallStackEntryObj.Get('address','0'));
          FunctionName:=JSonCallStackEntryObj.Get('functionname','');
          SourceFile:=JSonCallStackEntryObj.Get('sourcefile','');
          Line:=JSonCallStackEntryObj.get('line',-1);

          e.Init(AnAddress, nil, FunctionName, SourceFile, '', Line, ddsValid);
          end;
        It.Next;
        end;
      It.Free;
      FCallStack.SetCountValidity(ddsValid);
      TFPCallStackSupplier(FCallStackSupplier).DoUpdate;
      end
    else
      begin
      FCallStack.SetCountValidity(ddsInvalid);
      FCallStack.SetHasAtLeastCountInfo(ddsInvalid);
      end;
    end;
end;

{ TFPDSendDisassembleCommand }

procedure TFPDSendDisassembleCommand.ComposeJSon(AJsonObject: TJSONObject);
begin
  inherited ComposeJSon(AJsonObject);
  AJsonObject.Add('command','disassemble');
  if FStartAddr>0 then
    AJsonObject.Add('address', Dec2Numb(FStartAddr, 8, 16));
  if FLinesAfter>0 then
    AJsonObject.Add('linesafter', FLinesAfter);
  if FLinesBefore>0 then
    AJsonObject.Add('linesbefore', FLinesBefore);
end;

constructor TFPDSendDisassembleCommand.create(ADisassembler: TDBGDisassembler; AStartAddr: TDBGPtr; ALinesBefore, ALinesAfter: integer);
begin
  inherited create(true);
  FDisassembler := ADisassembler;
  FLinesBefore:=ALinesBefore;
  FLinesAfter:=ALinesAfter;
  FStartAddr:=AStartAddr;
end;

procedure TFPDSendDisassembleCommand.DoOnCommandSuccesfull(ACommandResponse: TJSonObject);
var
  JSonCallStackArr: TJSONArray;
  JSonCallStackEntryObj: TJSONObject;
  ARange: TDBGDisassemblerEntryRange;
  AnEntry: TDisassemblerEntry;
  i: Integer;
begin
  if assigned(FDisassembler) then
    begin
    JSonCallStackArr := ACommandResponse.Get('disassembly', TJSONArray(nil));
    if assigned(JSonCallStackArr) and (JSonCallStackArr.Count>0) then
      begin
      ARange := TDBGDisassemblerEntryRange.Create;
      ARange.RangeStartAddr:=Hex2Dec(ACommandResponse.Get('startaddress', Dec2Numb(FStartAddr, 8, 16)));
      for i := 0 to JSonCallStackArr.Count-1 do
        begin
        JSonCallStackEntryObj := JSonCallStackArr.Items[i] as TJSONObject;
        AnEntry.Addr:=Hex2Dec(JSonCallStackEntryObj.Get('address', '0'));
        AnEntry.Dump:=JSonCallStackEntryObj.Get('dump', '');
        AnEntry.Statement:=JSonCallStackEntryObj.Get('statement', '');
        AnEntry.SrcFileName:=JSonCallStackEntryObj.Get('srcfilename', '');
        AnEntry.SrcFileLine:=JSonCallStackEntryObj.Get('srcfileline', 0);
        AnEntry.SrcStatementIndex:=JSonCallStackEntryObj.Get('srcstatementindex', 0);
        AnEntry.SrcStatementCount:=JSonCallStackEntryObj.Get('srcstatementcount', 0);
        AnEntry.FuncName:=JSonCallStackEntryObj.Get('functionname', '');
        AnEntry.Offset:=JSonCallStackEntryObj.Get('offset', 0);
        ARange.Append(@AnEntry);
        end;
      ARange.RangeEndAddr:=Hex2Dec(ACommandResponse.Get('endaddress', Dec2Numb(AnEntry.Addr, 8, 16)));
      ARange.LastEntryEndAddr:=Hex2Dec(ACommandResponse.Get('lastentryendaddress', '0'));
      TFPDBGDisassembler(FDisassembler).AddRange(ARange);
      end
    end;
end;

procedure TFPDSendCallStackCommand.DoOnCommandFailed(ACommandResponse: TJSonObject);
begin
  FCallStack.SetCountValidity(ddsInvalid);
end;


