{%MainUnit ../extctrls.pp}

{******************************************************************************
                               TCustomCheckbox
 ******************************************************************************

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
  SetInitialBounds(0,0,250,200);
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
  // TODO: Remove RecreateWnd
  if HandleAllocated then RecreateWnd(Self);
  OwnerFormDesignerModified(Self);
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
  // destroy checkboxes, if there are too many
  while FButtonList.Count>FItems.Count do begin
    TCheckBox(FButtonList[FButtonList.Count-1]).Free;
    FButtonList.Delete(FButtonList.Count-1);
  end;

  // create as many TCheckBox as needed
  while (FButtonList.Count<FItems.Count) do begin
    CheckBox := TCheckBox.Create (self);
    with CheckBox do begin
      Name:='CheckBox'+IntToStr(FButtonList.Count);
      AutoSize := False;
      Parent := Self;
      OnClick :=@Clicked;
      include(ControlStyle, csNoDesignSelectable);
    end;
    FButtonList.Add(CheckBox);
  end;
  for i:=0 to FItems.Count-1 do begin
    CheckBox:=TCheckBox(FButtonList[i]);
    CheckBox.Caption:=FItems[i];
  end;
  
  DoPositionButtons;
end;

function TCustomCheckGroup.GetCheckEnabled(Index: integer): boolean;
begin
  if (Index < -1) or (Index >= FItems.Count) then
    raise Exception.CreateFmt(rsIndexOutOfBounds,[ClassName,Index,FItems.Count]);
  Result:=TCheckBox(FButtonList[Index]).Enabled;
end;

procedure TCustomCheckGroup.SetCheckEnabled(Index: integer;
  const AValue: boolean);
begin
  if (Index < -1) or (Index >= FItems.Count) then
    raise Exception.CreateFmt(rsIndexOutOfBounds,[ClassName,Index,FItems.Count]);
  TCheckBox(FButtonList[Index]).Enabled:=AValue;
end;

procedure TCustomCheckGroup.SetColumnLayout(const AValue: TColumnLayout);
begin
  if FColumnLayout=AValue then exit;
  FColumnLayout:=AValue;
  DoPositionButtons;
end;

function TCustomCheckGroup.GetChecked(Index: integer): boolean;
begin
  if (Index < -1) or (Index >= FItems.Count) then
    raise Exception.CreateFmt(rsIndexOutOfBounds,[ClassName,Index,FItems.Count]);
  Result:=TCheckBox(FButtonList[Index]).Checked;
end;

procedure TCustomCheckGroup.DoPositionButtons;
var
  i       : integer;
  CheckBox: TCheckBox;
  nextTop : integer;
  nextLeft: integer;
  vertDist: integer;
  horzDist: integer;
  rbWidth : integer;
  MaxRows: Integer;
begin
  if (FItems<>nil) and (FItems.Count>0) and (FColumns>0) then begin
    // position in rows and columns
    vertDist := (Height - 20) DIV (((FItems.Count-1) DIV FColumns)+1);
    horzDist := (Width - 20) DIV FColumns;
    nextTop  := 0;
    nextLeft := 10;
    rbWidth  := horzDist;
    MaxRows := (FItems.Count+FColumns-1) div FColumns;
    i := 0;
    while i < FItems.Count do begin
      CheckBox := TCheckBox(FButtonList[i]);
      CheckBox.SetBounds(nextLeft,nextTop,rbWidth,vertDist);

      inc (i);
      if FColumnLayout=clHorizontalThenVertical then begin
        if (i mod FColumns) = 0 then begin
          inc(nextTop, vertDist);
          nextLeft := 10;
        end else begin
          inc(nextLeft, horzDist);
        end;
      end else begin
        if (i mod MaxRows) = 0 then begin
          inc(nextLeft, horzDist);
          nextTop := 0;
        end else begin
          inc(nextTop, vertDist);
        end;
      end;
    end;
  end;
end;

procedure TCustomCheckGroup.SetChecked(Index: integer; const AValue: boolean);
begin
  if (Index < -1) or (Index >= FItems.Count) then
    raise Exception.CreateFmt(rsIndexOutOfBounds,[ClassName,Index,FItems.Count]);
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
    // TODO: Remove RecreateWnd
    if HandleAllocated then RecreateWnd(Self);
  end;
end;

procedure TCustomCheckGroup.SetColumns(Value: integer);
begin
  if Value <> FColumns then
  begin
    if (Value < 1)
       then raise Exception.Create('TCustomCheckGroup: Columns must be >= 1');
    FColumns := Value;
    DoPositionButtons;
    // TODO: Remove RecreateWnd
    if HandleAllocated then RecreateWnd(self);
  end;
end;

procedure TCustomCheckGroup.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('Data', @ReadData, @WriteData,FItems.Count>0);
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
  DoPositionButtons;
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


