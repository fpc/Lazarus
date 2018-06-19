{%MainUnit ../dbctrls.pp}
{
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

{ TDBMemo }

function TDBMemo.GetDataField: string;
begin
  Result:=FDataLink.FieldName;
end;

function TDBMemo.GetDataSource: TDataSource;
begin
  Result:=FDataLink.DataSource;
end;

function TDBMemo.GetField: TField;
begin
  Result:=FDataLink.Field;
end;

function TDBMemo.GetReadOnly: Boolean;
begin
  Result:=FDataLink.ReadOnly;
end;

procedure TDBMemo.SetAutoDisplay(const AValue: Boolean);
begin
  if FAutoDisplay=AValue then exit;
  FAutoDisplay:=AValue;
  if FAutoDisplay then LoadMemo;
end;

procedure TDBMemo.SetDataField(const AValue: string);
begin
  FDataLink.FieldName:=AValue;
end;

procedure TDBMemo.SetDataSource(const AValue: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    ChangeDataSource(Self,FDataLink,AValue);
end;

procedure TDBMemo.CMGetDataLink(var Message: TLMessage);
begin
  Message.Result := PtrUInt(FDataLink);
end;

procedure TDBMemo.SetReadOnly(AValue: Boolean);
begin
  inherited;
  FDataLink.ReadOnly:=AValue;
end;

procedure TDBMemo.DataChange(Sender: TObject);
var
  DataLinkField: TField;
begin
  DataLinkField := FDataLink.Field;
  if DataLinkField<>nil then begin
    if DataLinkField.IsBlob then begin
      if FAutoDisplay or (FDataLink.Editing and FDBMemoLoaded) then begin
        FDBMemoLoaded:=False;
        LoadMemo;
      end else begin
        Text:=Format('(%s)', [DataLinkField.DisplayLabel]);
        FDBMemoLoaded:=False;
      end;
    end else begin
      if FDBMemoFocused and FDataLink.CanModify then
        Text:=DataLinkField.Text
      else
        Text:=DataLinkField.DisplayText;
      FDBMemoLoaded:=True;
    end
  end else begin
    if csDesigning in ComponentState then
      Text:=Name
    else
      Text:='';
    FDBMemoLoaded:=False;
  end;
end;

procedure TDBMemo.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) and (AComponent=DataSource) then
      DataSource:=nil;
end;

procedure TDBMemo.UpdateData(Sender: TObject);
begin
  if not FDBMemoLoaded then exit;
  if not FDataLink.CanModify then exit;
  
  // issue #33498: possibility to change field text in OnSetText 
  if Assigned(FDatalink.Field.OnSetText) then
    FDataLink.Field.Text := Text
  else
    FDataLink.Field.AsString := Text;
end;

constructor TDBMemo.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ControlStyle:=ControlStyle+[csReplicatable];
  FAutoDisplay:=True;
  FDataLink:=TFieldDataLink.Create;
  FDataLink.Control:=Self;
  FDataLink.OnDataChange:=@DataChange;
  FDataLink.OnUpdateData:=@UpdateData;
end;

procedure TDBMemo.DoEditingDone;
begin
  if FDataLink.CanModify and FDatalink.Editing then begin
    FDataLink.UpdateRecord;
    inherited DoEditingDone;
  end else
    FDatalink.Reset;
end;

procedure TDBMemo.Change;
begin
  FDatalink.Modified;
  inherited Change;
end;

procedure TDBMemo.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  case key of
    VK_ESCAPE:
      begin
       //cancel out of editing by reset on esc
       FDataLink.Reset;
       SelectAll;
       Key := VK_UNKNOWN;
      end;
    VK_DELETE, VK_BACK:
      begin
        if not FieldIsEditable(FDatalink.Field) or not FDataLink.Edit then
          Key := VK_UNKNOWN;
      end;
  end;
end;

procedure TDBMemo.KeyPress(var Key: Char);
  function CheckValidChar: boolean;
  begin
    result := FDBMemoLoaded and FieldCanAcceptKey(FDatalink.Field, Key);
    if Result then
      FDatalink.Edit
    else
      Key := #0;
  end;
  function CheckEditingKey: boolean;
  begin
    result := FDbMemoLoaded;
    if Result then
      FDatalink.Edit
    else
      Key := #0;
  end;
begin
  inherited KeyPress(Key);

  if not FieldCanAcceptKey(FDataLink.Field, Key) or not FDatalink.Edit then
    Key := #0;

  case key of
    ^X, ^V, ^Z, ^I, ^J, ^H, #32..#255: // alphabetic characters
      CheckValidChar;
    ^M: // enter key
      if not CheckEditingKey then
        LoadMemo;
    #27: // escape
      if FDbMemoLoaded then
        FDatalink.Reset
      else
        Key:=#0;
    // Verifyes if we are in edit mode for special keys may change the text
    // Ctrl+I = Tab
    // Ctrl+J = LineFeed
    // Ctrl+H = Backspace
    // Don't do anything for special keys that don't change the text
    // Like Ctrl+C for example
  end;
end;

procedure TDBMemo.WndProc(var Message: TLMessage);
begin
  case Message.Msg of
    LM_CLEAR,
    LM_CUT,
    LM_PASTE:
      if FDataLink.CanModify then
      begin
        //LCL changes the Text before LM_PASTE is called and not after like Delphi. Issue 20330
        //When Edit is called the Text property is reset to the previous value
        //Add a workaround while bug is not fixed
        FDataLink.OnDataChange := nil;
        FDatalink.Edit;
        FDataLink.Modified;
        FDataLink.OnDataChange := @DataChange;
        inherited WndProc(Message);
      end
      else
        Message.Result := 1; // prevent calling default window proc
  end;
  inherited WndProc(Message);
end;

class procedure TDBMemo.WSRegisterClass;
const
  Done: Boolean = False;
begin
  if Done then
    Exit;
  inherited WSRegisterClass;
  RegisterPropertyToSkip(TDBMemo, 'Lines', 'Removed in 0.9.29. DB control should not save/load their data from stream.', '');
  Done := True;
end;

destructor TDBMemo.Destroy;
begin
  FDataLink.Destroy;
  inherited Destroy;
end;

procedure TDBMemo.LoadMemo;
begin
  if not FDBMemoLoaded and (FDataLink.Field<>nil)
  and FDataLink.Field.IsBlob then begin
    try
      // issue #33598: Possibility to change field text in OnGetText
      if Assigned(FDataLink.Field.OnGetText) then
        Lines.Text := FDataLink.Field.Text
      else 
        Lines.Text := FDataLink.Field.AsString;
      FDBMemoLoaded:=True;
    except
      on E:EInvalidOperation do
        Lines.Text:='('+E.Message+')';
    end;
  end;
end;

function TDBMemo.ExecuteAction(AAction: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(AAction) or
            (FDataLink <> nil) and FDataLink.ExecuteAction(AAction);
end;

function TDBMemo.UpdateAction(AAction: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(AAction) or
            (FDataLink <> nil) and FDataLink.UpdateAction(AAction);
end;

// included by dbctrls.pp
