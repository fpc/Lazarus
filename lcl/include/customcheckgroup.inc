// included by extctrls.pp

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

procedure TCustomCheckGroup.ItemsChanged(Sender: TObject);
begin
  UpdateItems;
  if HandleAllocated then RecreateWnd;
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
    end;
    FButtonList.Add(CheckBox);
  end;
  for i:=0 to FItems.Count-1 do begin
    CheckBox:=TCheckBox(FButtonList[i]);
    CheckBox.Caption:=FItems[i];
  end;
  
  DoPositionButtons;
end;

function TCustomCheckGroup.GetChecked(Index: integer): boolean;
begin
  if (Index < -1) or (Index >= FItems.Count) then
    raise Exception.CreateFmt(rsIndexOutOfRange,[ClassName,Index,FItems.Count]);
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
  CurRows: Integer;
begin
  if (FItems<>nil) and (FItems.Count>0)
  and (not (csLoading in ComponentState)) then begin
    // position in rows and columns
    CurRows  := Rows;
    vertDist := ClientHeight DIV CurRows;
    horzDist := (ClientWidth-8) DIV FColumns;
    nextLeft := 4;
    nextTop  := 0;
    rbWidth  := horzDist;
    i := 0;
    while i < FItems.Count do begin
      CheckBox := TCheckBox(FButtonList[i]);
      CheckBox.SetBounds(nextLeft,nextTop,rbWidth,vertDist);

      inc (i);
      if (i mod CurRows) = 0 then begin
        inc(nextLeft, horzDist);
        nextTop := 0;
      end else begin
        inc(nextTop, vertDist);
      end;
    end;
  end;
end;

procedure TCustomCheckGroup.SetChecked(Index: integer; const AValue: boolean);
begin
  if (Index < -1) or (Index >= FItems.Count) then
    raise Exception.CreateFmt(rsIndexOutOfRange,[ClassName,Index,FItems.Count]);
  TCheckBox(FButtonList[Index]).Checked:=AValue;
end;

procedure TCustomCheckGroup.SetItems(Value: TStrings);
begin
  if (Value <> FItems) then
  begin
    FItems.Assign(Value);
    UpdateItems;
    if HandleAllocated then RecreateWnd;
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
    if HandleAllocated then RecreateWnd;
  end;
end;

procedure TCustomCheckGroup.WMSize(var Message: TLMSize);
begin
  DoPositionButtons;
  inherited;
end;

procedure TCustomCheckGroup.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('Data', @ReadData, @WriteData,true);
end;

procedure TCustomCheckGroup.ReadData(Stream: TStream);
var
  ChecksCount: integer;
  Checks: string;
  i: Integer;
begin
  Stream.ReadBuffer(ChecksCount, SizeOf(Integer));
  if ChecksCount>0 then begin
    SetLength(Checks,ChecksCount);
    Stream.ReadBuffer(Checks[1], ChecksCount);
    for i:=0 to ChecksCount-1 do
      Checked[i]:=(Checks[i+1]='1');
  end;
end;

procedure TCustomCheckGroup.WriteData(Stream: TStream);
var
  ChecksCount: integer;
  Checks: string;
  i: Integer;
begin
  ChecksCount:=FItems.Count;
  Stream.WriteBuffer(ChecksCount, SizeOf(Integer));
  if ChecksCount>0 then begin
    SetLength(Checks,ChecksCount);
    for i:=0 to ChecksCount-1 do
      if Checked[i] then
        Checks[i+1]:='1'
      else
        Checks[i+1]:='0';
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

constructor TCustomCheckGroup.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FCreatingWnd := false;
  ControlStyle := ControlStyle + [csCaptureMouse, csClickEvents, csSetCaption,
                                  csDoubleClicks];
  FItems      := TStringList.Create;
  TStringList(FItems).OnChanging := @ItemsChanged;
  TStringList(FItems).OnChange := @ItemsChanged;
  FButtonList := TList.Create;
  FColumns  := 1;
  Width:= 250;
  Height := 200;
end;

destructor TCustomCheckGroup.Destroy;
begin
  FreeAndNil(FItems);
  FreeAndNil(FButtonList);
  inherited Destroy;
end;

function TCustomCheckGroup.Rows: integer;
begin
  if FItems.Count>0 then
    Result:=((FItems.Count-1) div Columns)+1
  else
    Result:=0;
end;

// included by extctrls.pp

{
  $Log$
  Revision 1.1  2003/03/17 23:39:30  mattias
  added TCheckGroup

}

