{%MainUnit ../extctrls.pp}

{******************************************************************************
                               TCustomCheckbox
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

{ TCustomCheckGroup }

constructor TCustomCheckGroup.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FCreatingWnd := false;
  ControlStyle := ControlStyle + [csCaptureMouse, csClickEvents, csSetCaption,
                                  csDoubleClicks];
  FItems      := TStringList.Create;
  //TStringList(FItems).OnChanging := @ItemsChanged;
  TStringList(FItems).OnChange := @ItemsChanged;
  FButtonList := TList.Create;
  FColumnLayout := clHorizontalThenVertical;
  FColumns  := 1;
  FAutoFill := true;
  ChildSizing.Layout:=cclLeftToRightThenTopToBottom;
  ChildSizing.ControlsPerLine:=FColumns;
  ChildSizing.ShrinkHorizontal:=crsScaleChilds;
  ChildSizing.ShrinkVertical:=crsScaleChilds;
  ChildSizing.EnlargeHorizontal:=crsHomogenousChildResize;
  ChildSizing.EnlargeVertical:=crsHomogenousChildResize;
  ChildSizing.LeftRightSpacing:=6;
  ChildSizing.TopBottomSpacing:=6;
end;

destructor TCustomCheckGroup.Destroy;
begin
  FreeAndNil(FItems);
  FreeAndNil(FButtonList);
  inherited Destroy;
end;

procedure TCustomCheckGroup.ItemsChanged(Sender: TObject);
begin
  UpdateItems;
  UpdateControlsPerLine;
  OwnerFormDesignerModified(Self);
end;

procedure TCustomCheckGroup.RaiseIndexOutOfBounds(Index: integer ) ;
begin
  raise Exception.CreateFmt(rsIndexOutOfBounds,
                            [ClassName, Index, FItems.Count - 1]);
end;

procedure TCustomCheckGroup.SetAutoFill(const AValue: boolean);
begin
  if FAutoFill=AValue then exit;
  FAutoFill:=AValue;
  DisableAlign;
  try
    if FAutoFill then begin
      ChildSizing.EnlargeHorizontal:=crsHomogenousChildResize;
      ChildSizing.EnlargeVertical:=crsHomogenousChildResize;
    end else begin
      ChildSizing.EnlargeHorizontal:=crsAnchorAligning;
      ChildSizing.EnlargeVertical:=crsAnchorAligning;
    end;
  finally
    EnableAlign;
  end;
end;

procedure TCustomCheckGroup.Clicked(Sender: TObject);
var
  Index: Integer;
begin
  Index:=FButtonList.IndexOf(Sender);
  if Index<0 then exit;
  DoClick(Index);
end;

procedure TCustomCheckGroup.DoClick(Index: integer);
begin
  if [csLoading,csDestroying,csDesigning]*ComponentState<>[] then exit;
  EditingDone;
  if Assigned(OnItemClick) then OnItemClick(Self,Index);
end;

procedure TCustomCheckGroup.UpdateItems;
var
  i       : integer;
  CheckBox: TCheckBox;
begin
  if FUpdatingItems then exit;
  FUpdatingItems:=true;
  try
    // destroy checkboxes, if there are too many
    while FButtonList.Count>FItems.Count do begin
      TObject(FButtonList[FButtonList.Count-1]).Free;
      FButtonList.Delete(FButtonList.Count-1);
    end;

    // create as many TCheckBox as needed
    while (FButtonList.Count<FItems.Count) do begin
      CheckBox := TCheckBox.Create(Self);
      with CheckBox do begin
        Name:='CheckBox'+IntToStr(FButtonList.Count);
        AutoSize := False;
        BorderSpacing.CellAlignHorizontal:=ccaLeftTop;
        BorderSpacing.CellAlignVertical:=ccaCenter;
        Parent := Self;
        OnClick :=@Self.Clicked;
        ParentFont := true;
        ControlStyle := ControlStyle + [csNoDesignSelectable];
      end;
      FButtonList.Add(CheckBox);
    end;
    for i:=0 to FItems.Count-1 do begin
      CheckBox:=TCheckBox(FButtonList[i]);
      CheckBox.Caption:=FItems[i];
    end;
  finally
    FUpdatingItems:=false;
  end;
end;

procedure TCustomCheckGroup.UpdateControlsPerLine;
var
  NewControlsPerLine: LongInt;
begin
  if ChildSizing.Layout=cclLeftToRightThenTopToBottom then
    NewControlsPerLine:=Max(1,FColumns)
  else
    NewControlsPerLine:=((FItems.Count-1) div Max(1,FColumns))+1;
  ChildSizing.ControlsPerLine:=NewControlsPerLine;
  //DebugLn('TCustomCheckGroup.UpdateControlsPerLine ',dbgs(ChildSizing.Layout=cclLeftToRightThenTopToBottom),' ',dbgs(ChildSizing.ControlsPerLine));
end;

class procedure TCustomCheckGroup.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterCustomCheckGroup;
end;

function TCustomCheckGroup.GetCheckEnabled(Index: integer): boolean;
begin
  if (Index < -1) or (Index >= FItems.Count) then
    RaiseIndexOutOfBounds(Index);
  Result:=TCheckBox(FButtonList[Index]).Enabled;
end;

procedure TCustomCheckGroup.SetCheckEnabled(Index: integer;
  const AValue: boolean);
begin
  if (Index < -1) or (Index >= FItems.Count) then
    RaiseIndexOutOfBounds(Index);
  TCheckBox(FButtonList[Index]).Enabled:=AValue;
end;

procedure TCustomCheckGroup.SetColumnLayout(const AValue: TColumnLayout);
begin
  if FColumnLayout=AValue then exit;
  FColumnLayout:=AValue;
  if FColumnLayout=clHorizontalThenVertical then
    ChildSizing.Layout:=cclLeftToRightThenTopToBottom
  else
    ChildSizing.Layout:=cclTopToBottomThenLeftToRight;
  UpdateControlsPerLine;
end;

function TCustomCheckGroup.GetChecked(Index: integer): boolean;
begin
  if (Index < -1) or (Index >= FItems.Count) then
    RaiseIndexOutOfBounds(Index);
  Result:=TCheckBox(FButtonList[Index]).Checked;
end;

procedure TCustomCheckGroup.SetChecked(Index: integer; const AValue: boolean);
begin
  if (Index < -1) or (Index >= FItems.Count) then
    RaiseIndexOutOfBounds(Index);
  // disable OnClick
  TCheckBox(FButtonList[Index]).OnClick:=nil;
  // set value
  TCheckBox(FButtonList[Index]).Checked:=AValue;
  // enable OnClick
  TCheckBox(FButtonList[Index]).OnClick:=@Clicked;
end;

procedure TCustomCheckGroup.SetItems(Value: TStrings);
begin
  if (Value <> FItems) then
  begin
    FItems.Assign(Value);
    UpdateItems;
    UpdateControlsPerLine;
  end;
end;

procedure TCustomCheckGroup.SetColumns(Value: integer);
begin
  if Value <> FColumns then
  begin
    if (Value < 1)
       then raise Exception.Create('TCustomCheckGroup: Columns must be >= 1');
    FColumns := Value;
    UpdateControlsPerLine;
  end;
end;

procedure TCustomCheckGroup.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('Data', @ReadData, @WriteData, FItems.Count > 0);
end;

procedure TCustomCheckGroup.ReadData(Stream: TStream);
var
  ChecksCount: integer;
  Checks: string;
  i: Integer;
  v: Integer;
begin
  ChecksCount:=ReadLRSInteger(Stream);
  if ChecksCount>0 then begin
    SetLength(Checks,ChecksCount);
    Stream.ReadBuffer(Checks[1], ChecksCount);
    for i:=0 to ChecksCount-1 do begin
      v:=ord(Checks[i+1]);
      Checked[i]:=((v and 1)>0);
      CheckEnabled[i]:=((v and 2)>0);
    end;
  end;
end;

procedure TCustomCheckGroup.WriteData(Stream: TStream);
var
  ChecksCount: integer;
  Checks: string;
  i: Integer;
  v: Integer;
begin
  ChecksCount:=FItems.Count;
  WriteLRSInteger(Stream,ChecksCount);
  if ChecksCount>0 then begin
    SetLength(Checks,ChecksCount);
    for i:=0 to ChecksCount-1 do begin
      v:=0;
      if Checked[i] then inc(v,1);
      if CheckEnabled[i] then inc(v,2);
      Checks[i+1]:=chr(v);
    end;
    Stream.WriteBuffer(Checks[1], ChecksCount);
  end;
end;

procedure TCustomCheckGroup.Loaded;
begin
  inherited Loaded;
  UpdateItems;
end;

procedure TCustomCheckGroup.DoOnResize;
begin
  inherited DoOnResize;
end;

function TCustomCheckGroup.Rows: integer;
begin
  if FItems.Count>0 then
    Result:=((FItems.Count-1) div Columns)+1
  else
    Result:=0;
end;

// included by extctrls.pp

