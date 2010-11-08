{%MainUnit ../dbctrls.pas}

{******************************************************************************
                                     TDBLookupComboBox
                    data aware lookup Combo Box, base found in dbctrls.pp
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

// included by dbctrls.pp

{ TDBLookupComboBox }

constructor TDBLookupComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLookup:= TDBLookup.Create(Self);
  FLookup.OnClearSelection:=@RemoveSelection;
  FDataLink.OnActiveChange:= @ActiveChange;
end;

procedure TDBLookupComboBox.UpdateData(Sender: TObject);
begin
  FLookup.UpdateData(Text);
end;

procedure TDBLookupComboBox.ActiveChange(Sender: TObject);
begin
  if (csLoading in ComponentState) then
    Exit;
  FLookup.Initialize(FDataLink, Items);
  UpdateText;
end;

procedure TDBLookupComboBox.UpdateText;
begin
  Text:= FLookup.ListFieldValue;
end;

procedure TDBLookupComboBox.InitializeWnd;
begin
  inherited InitializeWnd;
  ActiveChange(Self);
end;

function TDBLookupComboBox.GetKeyField: string;
begin
  Result := FLookup.KeyField;
end;

function TDBLookupComboBox.GetKeyValue: variant;
begin
  result := FLookup.GetKeyValue(Text);
end;

function TDBLookupComboBox.GetListField: string;
begin
  Result := FLookup.ListField;
end;

function TDBLookupComboBox.GetListFieldIndex: Integer;
begin
  Result := FLookup.ListFieldIndex;
end;

function TDBLookupComboBox.GetListSource: TDataSource;
begin
  Result := FLookup.ListSource;
end;

function TDBLookupComboBox.GetLookupCache: boolean;
begin
  Result := FLookup.LookupCache;
end;

function TDBLookupComboBox.GetNullValueKey: TShortCut;
begin
  result := FLookup.NullValueKey;
end;

procedure TDBLookupComboBox.SetKeyField(const Value: string);
begin
  FLookup.KeyField:= Value;
  ActiveChange(Self);
end;

procedure TDBLookupComboBox.SetKeyValue(const AValue: variant);
var
  AItemIndex: Integer;
begin
  if FLookup.GetListValue(AValue, AItemIndex) then
    ItemIndex := AItemIndex;
end;

procedure TDBLookupComboBox.SetListField(const Value: string);
begin
  FLookup.ListField:= Value;
  ActiveChange(Self);
end;

procedure TDBLookupComboBox.SetListFieldIndex(const Value: Integer);
begin
  FLookup.ListFieldIndex:= Value;
  ActiveChange(Self);
end;

procedure TDBLookupComboBox.SetListSource(const Value: TDataSource);
begin
  FLookup.ListSource:= Value;
  ActiveChange(Self);
end;

procedure TDBLookupComboBox.SetLookupCache(const Value: boolean);
begin
  FLookup.LookupCache := Value;
  ActiveChange(Self);
end;

procedure TDBLookupComboBox.SetNullValueKey(const AValue: TShortCut);
begin
  FLookup.NullValueKey := AValue;
end;

procedure TDBLookupComboBox.RemoveSelection(Sender: TObject);
begin
  ItemIndex := -1;
  Text := '';
end;


