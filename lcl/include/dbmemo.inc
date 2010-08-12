{%MainUnit ../dbctrls.pp}
{
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
begin
  if FDataLink.Field<>nil then begin
    if FDataLink.Field.IsBlob then begin
      if FAutoDisplay or (FDataLink.Editing and FDBMemoLoaded) then begin
        FDBMemoLoaded:=False;
        LoadMemo;
      end else begin
        Text:=Format('(%s)', [FDataLink.Field.DisplayLabel]);
        FDBMemoLoaded:=False;
      end;
    end else begin
      if FDBMemoFocused and FDataLink.CanModify then
        Text:=FDataLink.Field.Text
      else
        Text:=FDataLink.Field.DisplayText;
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

procedure TDBMemo.ActiveChange(Sender: TObject);
begin
  if FDatalink.Active then datachange(sender)
  else
    begin
    Lines.Clear;
    FDataLink.reset;
    end;
end;

procedure TDBMemo.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) then begin
    if (FDataLink<>nil) and (AComponent=DataSource) then
      DataSource:=nil;
  end;
end;

procedure TDBMemo.UpdateData(Sender: TObject);
begin
  if not FDBMemoLoaded then exit;
  if FDataLink=nil then exit;
  if not FDataLink.CanModify then exit;
  FDataLink.Field.AsString:=Text;
end;

constructor TDBMemo.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ControlStyle:=ControlStyle+[csReplicatable];
  FAutoDisplay:=True;
  FDataLink:=TFieldDataLink.Create;
  FDataLink.Control:=Self;
  FDataLink.OnDataChange:=@DataChange;
  FDataLInk.OnActiveChange := @ActiveChange;
  FDataLink.OnUpdateData:=@UpdateData;
end;

procedure TDBMemo.FocusRequest(Sender: TObject);
begin
  //the FieldLink has requested the control
  //recieve focus for some reason..
  //perhaps an error occured?
  SetFocus;
end;

procedure TDBMemo.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then
    DataChange(Self);
end;

procedure TDBMemo.EditingDone;
begin
  if FDataLink.CanModify and FDatalink.Editing then begin
    FDataLink.UpdateRecord;
    inherited EditingDone;
  end else
    FDatalink.Reset;
end;

procedure TDBMemo.Change;
begin
  FDatalink.Modified;
  inherited Change;
end;

procedure TDBMemo.KeyPress(var Key: Char);
  function CheckValidChar: boolean;
  begin
    result := FDBMemoLoaded and (FDatalink.Field<>nil) and
      FDatalink.Field.IsValidChar(Key);
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
  
  case key of
    #32..#255: // alphabetic characters
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
    ^X, ^V, ^Z, ^I, ^J, ^H:
      CheckEditingKey;
    // Don't do anything for special keys that don't change the text
    // Like Ctrl+C for example
  end;
end;

procedure TDBMemo.WndProc(var AMessage: TLMessage);
begin
  case AMessage.Msg of
    LM_CLEAR,
    LM_CUT,
    LM_PASTE:
      FDatalink.Edit;
  end;
  inherited WndProc(AMessage);
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
  FDataLink.Free;
  FDataLink:=nil;
  inherited Destroy;
end;

procedure TDBMemo.LoadMemo;
begin
  if not FDBMemoLoaded and (FDataLink.Field<>nil)
  and FDataLink.Field.IsBlob then begin
    try
      Lines.Text:=FDataLink.Field.AsString;
      FDBMemoLoaded:=True;
    except
      on E:EInvalidOperation do
        Lines.Text:='('+E.Message+')';
    end;
  end;
end;

// included by dbctrls.pas
