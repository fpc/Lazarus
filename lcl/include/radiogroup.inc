{%MainUnit ../extctrls.pas}
{******************************************************************************
                                TCustomRadioGroup
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

  Delphi compatibility:

   - the interface is almost like in delphi 5
   - FlipChildren procedure is missing
}

{------------------------------------------------------------------------------
  Method: TCustomRadioGroup.Create
  Params:  TheOwner: the owner of the class
  Returns: Nothing

  Constructor for the radiogroup
 ------------------------------------------------------------------------------}
constructor TCustomRadioGroup.Create(TheOwner : TComponent);
begin
  inherited Create (TheOwner);
  ControlStyle := ControlStyle + [csCaptureMouse, csClickEvents, csSetCaption,
                                  csDoubleClicks];
  FItems := TStringList.Create;
  FAutoFill := true;
  //TStringList(FItems).OnChanging := @ItemsChanged;
  TStringList(FItems).OnChange := @ItemsChanged;
  FItemIndex  := -1;
  FLastClickedItemIndex := -1;
  FButtonList := TList.Create;
  FColumns  := 1;
  FColumnLayout := clHorizontalThenVertical;
  ChildSizing.Layout:=cclLeftToRightThenTopToBottom;
  ChildSizing.ControlsPerLine:=FColumns;
  ChildSizing.ShrinkHorizontal:=crsScaleChilds;
  ChildSizing.ShrinkVertical:=crsScaleChilds;
  ChildSizing.EnlargeHorizontal:=crsHomogenousChildResize;
  ChildSizing.EnlargeVertical:=crsHomogenousChildResize;
  ChildSizing.LeftRightSpacing:=6;
  ChildSizing.TopBottomSpacing:=6;
end;


{------------------------------------------------------------------------------
  Method: TCustomRadioGroup.Destroy
  Params:  none
  Returns: Nothing

  Destructor for the radiogroup
 ------------------------------------------------------------------------------}
destructor TCustomRadioGroup.Destroy;
begin
  FreeAndNil(FItems);
  FreeAndNil(FButtonList);
  FreeAndNil(FHiddenButton);
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method: TCustomRadioGroup.InitializeWnd
  Params:  none
  Returns: Nothing

  Create the visual component of the Radiogroup.
 ------------------------------------------------------------------------------}
procedure TCustomRadioGroup.InitializeWnd;

  procedure RealizeItemIndex;
  var
    i: Integer;
  begin
    if (FItemIndex <> -1) and (FItemIndex<FButtonList.Count) then
      TRadioButton(FButtonList[FItemIndex]).Checked := true
    else if FHiddenButton<>nil then
      FHiddenButton.Checked:=true;
    for i:=0 to FItems.Count-1 do begin
      TRadioButton(FButtonList[i]).Checked := fItemIndex = i;
    end;
  end;

begin
  if FCreatingWnd then RaiseGDBException('TCustomRadioGroup.InitializeWnd');
  FCreatingWnd := true;
  //DebugLn('[TCustomRadioGroup.InitializeWnd] A ',Name,':',ClassName,' FItems.Count=',FItems.Count,' HandleAllocated=',HandleAllocated,' ItemIndex=',ItemIndex);
  
  inherited InitializeWnd;
  
  if (FHiddenButton<>nil) and (FHiddenButton.Parent=Self) then
    FHiddenButton.HandleNeeded;
  RealizeItemIndex;

  FCreatingWnd := false;
end;

function TCustomRadioGroup.Rows: integer;
begin
  if FItems.Count>0 then
    Result:=((FItems.Count-1) div Columns)+1
  else
    Result:=0;
end;

procedure TCustomRadioGroup.ItemEnter(Sender: TObject);
begin
  DoEnter;
end;

procedure TCustomRadioGroup.ItemExit(Sender: TObject);
begin
  DoExit;
end;

procedure TCustomRadioGroup.ItemResize(Sender: TObject);
begin

end;

procedure TCustomRadioGroup.UpdateItems;
var
  i: integer;
  ARadioButton: TRadioButton;
begin
  if FUpdatingItems then exit;
  FUpdatingItems:=true;
  try
    // destroy radiobuttons, if there are too many
    while FButtonList.Count>FItems.Count do
    begin
      TRadioButton(FButtonList[FButtonList.Count-1]).Free;
      FButtonList.Delete(FButtonList.Count-1);
    end;

    // create as many TRadioButton as needed
    while (FButtonList.Count<FItems.Count) do
    begin
      ARadioButton := TRadioButton.Create(Self);
      with ARadioButton do
      begin
        Name := 'RadioButton'+IntToStr(FButtonList.Count);
        AutoSize := False;
        OnClick := @Self.Clicked;
        OnChange := @Self.Changed;
        OnEnter :=@Self.ItemEnter;
        OnExit :=@Self.ItemExit;
        OnKeyDown :=@Self.ItemKeyDown;
        OnResize := @Self.ItemResize;
        ParentFont := True;
        BorderSpacing.CellAlignHorizontal:=ccaLeftTop;
        BorderSpacing.CellAlignVertical:=ccaCenter;
        ControlStyle := ControlStyle + [csNoDesignSelectable];
      end;
      FButtonList.Add(ARadioButton);
    end;
    if FHiddenButton=nil then begin
      FHiddenButton:=TRadioButton.Create(nil);
      with FHiddenButton do
      begin
        Name := 'HiddenRadioButton';
        Visible := False;
        ControlStyle := ControlStyle + [csNoDesignSelectable, csNoDesignVisible];
      end;
    end;

    if (FItemIndex>=FItems.Count) then FItemIndex:=FItems.Count-1;

    if FItems.Count>0 then
    begin
      // to reduce overhead do it in several steps

      // assign Caption and then Parent
      for i:=0 to FItems.Count-1 do
      begin
        ARadioButton := TRadioButton(FButtonList[i]);
        ARadioButton.Caption := FItems[i];
        ARadioButton.Parent := Self;
      end;
      with FHiddenButton do
      begin
        FHiddenButton.Visible:=false;
        Parent:=Self;
        if HandleAllocated then
          FHiddenButton.HandleNeeded;
      end;

      // the checked and unchecked states can be applied only after all other
      for i := 0 to FItems.Count-1 do
      begin
        ARadioButton := TRadioButton(FButtonList[i]);
        ARadioButton.Checked := (i = FItemIndex);
        ARadioButton.Visible := true;
      end;
      FHiddenButton.Checked := (FItemIndex = -1);
      UpdateTabStops;
    end;
  finally
    FUpdatingItems:=false;
  end;
end;

procedure TCustomRadioGroup.UpdateControlsPerLine;
var
  NewControlsPerLine: LongInt;
begin
  if ChildSizing.Layout=cclLeftToRightThenTopToBottom then
    NewControlsPerLine:=Max(1,FColumns)
  else
    NewControlsPerLine:=Max(1,Rows);
  ChildSizing.ControlsPerLine:=NewControlsPerLine;
  //DebugLn('TCustomRadioGroup.UpdateControlsPerLine ',dbgs(ChildSizing.ControlsPerLine),' ',dbgs(NewControlsPerLine),' FColumns=',dbgs(FColumns),' FItems.Count=',dbgs(FItems.Count),' ',dbgs(ChildSizing.Layout=cclLeftToRightThenTopToBottom));
end;

procedure TCustomRadioGroup.Loaded;
begin
  inherited Loaded;
  UpdateItems;
end;

procedure TCustomRadioGroup.ItemKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);

  procedure MoveSelection(HorzDiff, VertDiff: integer);
  var
    Count: integer;
    StepSize: integer;
    BlockSize : integer;
    NewIndex : integer;
    WrapOffset: integer;
  begin
    Count := FButtonList.Count;
    if FColumnLayout=clHorizontalThenVertical then begin
      //add a row for ease wrapping
      BlockSize := Columns * (Rows+1);
      StepSize := HorzDiff + VertDiff * Columns;
      WrapOffSet := VertDiff;
    end
    else begin
      //add a column for ease wrapping
      BlockSize := (Columns+1) * Rows;
      StepSize := HorzDiff * Rows + VertDiff;
      WrapOffSet := HorzDiff;
    end;
    NewIndex := ItemIndex + StepSize;
    if (NewIndex>=Count) or (NewIndex<0) then begin
      NewIndex := (NewIndex + WrapOffSet + BlockSize) mod BlockSize;
      // Keep moving in the same direction until in valid range
      while NewIndex>=Count do
        NewIndex := (NewIndex + StepSize) mod BlockSize;
    end;
    ItemIndex := NewIndex;
    TRadioButton(FButtonList[ItemIndex]).SetFocus;
    Key := 0;
  end;
  
begin
  if Shift=[] then begin
    case Key of
      VK_LEFT: MoveSelection(-1,0);
      VK_RIGHT: MoveSelection(1,0);
      VK_UP: MoveSelection(0,-1);
      VK_DOWN: MoveSelection(0,1);
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomRadioGroup.ItemsChanged
  Params:  sender : object calling this proc. (in fact the FItems instance)
  Returns: Nothing

 ------------------------------------------------------------------------------}
procedure TCustomRadioGroup.ItemsChanged (Sender : TObject);
begin
  UpdateItems;
  UpdateControlsPerLine;
  OwnerFormDesignerModified(Self);
end;

{------------------------------------------------------------------------------
  Method: TCustomRadioGroup.SetColumns
  Params:  value - no of columns of the radiogroup
  Returns: Nothing

  Set the FColumns property which determines the number of columns in
  which the radiobuttons should be arranged.
  Range: 1 .. ???
 ------------------------------------------------------------------------------}
procedure TCustomRadioGroup.SetColumns(value : integer);
begin
  if Value <> FColumns then begin
    if (Value < 1)
       then raise Exception.Create('TCustomRadioGroup: Columns must be >= 1');
    FColumns := Value;
    UpdateControlsPerLine;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomRadioGroup.SetItems
  Params:  value - Stringlist containing items to be displayed as radiobuttons
  Returns: Nothing

  Assign items from a stringlist.
 ------------------------------------------------------------------------------}
procedure TCustomRadioGroup.SetItems(Value: TStrings);
begin
  if (Value <> FItems) then
  begin
    FItems.Assign(Value);
    UpdateItems;
    UpdateControlsPerLine;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomRadioGroup.SetItemIndex
  Params:  value - index of RadioButton to be selected
  Returns: Nothing

  Select one of the radiobuttons
 ------------------------------------------------------------------------------}
procedure TCustomRadioGroup.SetItemIndex(Value : integer);
var
  OldItemIndex: LongInt;
  OldIgnoreClicks: Boolean;
begin
  //DebugLn('TCustomRadioGroup.SetItemIndex ',dbgsName(Self),' Old=',dbgs(FItemIndex),' New=',dbgs(Value));
  if Value = FItemIndex then exit;
  if FReading then
    FItemIndex:=Value
  else begin
    if (Value < -1) or (Value >= FItems.Count) then
      raise Exception.CreateFmt(rsIndexOutOfBounds,[ClassName,Value,FItems.Count-1]);

    if (HandleAllocated) then
    begin
      // the radiobuttons are grouped by the widget interface
      // and some does not allow to uncheck all buttons in a group
      // Therefore there is a hidden button
      OldItemIndex:=FItemIndex;
      FItemIndex:=Value;
      OldIgnoreClicks:=FIgnoreClicks;
      FIgnoreClicks:=true;
      try
        if (FItemIndex <> -1) then
          TRadioButton(FButtonList[FItemIndex]).Checked := true
        else
          FHiddenButton.Checked:=true;
        // uncheck old radiobutton
        if (OldItemIndex <> -1) then begin
          if (OldItemIndex>=0) and (OldItemIndex<FButtonList.Count) then
            TRadioButton(FButtonList[OldItemIndex]).Checked := false
        end else
          FHiddenButton.Checked:=false;
      finally
        FIgnoreClicks:=OldIgnoreClicks;
      end;
      // this has automatically unset the old button. But they do not recognize
      // it. Update the states.
      CheckItemIndexChanged;
      UpdateTabStops;

      OwnerFormDesignerModified(Self);
    end
    else
      FItemIndex := Value;
  end;
  //DebugLn('TCustomRadioGroup.SetItemIndex ',dbgsName(Self),' END Old=',dbgs(FItemIndex),' New=',dbgs(Value));
end;

{------------------------------------------------------------------------------
  Method: TCustomRadioGroup.GetItemIndex
  Params:  value - index of RadioButton to be selected
  Returns: Nothing

  Retrieve the index of the radiobutton currently selected.
 ------------------------------------------------------------------------------}
function TCustomRadioGroup.GetItemIndex : integer;
begin
  //debugln('TCustomRadioGroup.GetItemIndex ',dbgsName(Self),' FItemIndex=',dbgs(FItemIndex));
  Result := FItemIndex;
end;

procedure TCustomRadioGroup.CheckItemIndexChanged;
begin
  if FCreatingWnd or FUpdatingItems then exit;
  if [csLoading,csDestroying]*ComponentState<>[] then exit;
  UpdateRadioButtonStates;
  if [csDesigning]*ComponentState<>[] then exit;
  if FLastClickedItemIndex=FItemIndex then exit;
  FLastClickedItemIndex:=FItemIndex;
  EditingDone;
  // for Delphi compatility: OnClick should be invoked, whenever ItemIndex
  // has changed
  if Assigned (FOnClick) then FOnClick(Self);
end;

{------------------------------------------------------------------------------
  Method: TCustomRadioGroup.CanModify
  Params:  none
  Returns: always true

  Is the user allowed to select a different radiobutton?
 ------------------------------------------------------------------------------}
function TCustomRadioGroup.CanModify : boolean;
begin
  Result := true;
end;

{------------------------------------------------------------------------------
  Method: TCustomRadioGroup.ReadState
  Params:  Reader: TReader

  executed when component is read from stream
 ------------------------------------------------------------------------------}
procedure TCustomRadioGroup.ReadState(Reader: TReader);
begin
  FReading := True;
  inherited ReadState(Reader);
  FReading := False;
  if (fItemIndex<-1) or (fItemIndex>=FItems.Count) then fItemIndex:=-1;
end;

{------------------------------------------------------------------------------
  Method: TCustomRadioGroup.Clicked
  Params: sender - the calling object

  This is the callback for all radiobuttons in the group. If an OnClick
  handler is assigned it will be called
 ------------------------------------------------------------------------------}
procedure TCustomRadioGroup.Clicked(Sender : TObject);
Begin
  if FIgnoreClicks then exit;
  CheckItemIndexChanged;
end;

{------------------------------------------------------------------------------
  Method: TCustomRadioGroup.Changed
  Params: sender - the calling object

  Checks for changes. Does the same as Clicked for Delphi compatibility.
 ------------------------------------------------------------------------------}
procedure TCustomRadioGroup.Changed(Sender : TObject);
Begin
  CheckItemIndexChanged;
end;

procedure TCustomRadioGroup.UpdateTabStops;
var
  i: Integer;
  RadioBtn: TRadioButton;
begin
  for i := 0 to FButtonList.Count - 1 do
  begin
    RadioBtn := TRadioButton(FButtonList[i]);
    RadioBtn.TabStop := RadioBtn.Checked;
  end;
  if (FItemIndex=-1) and (Items.Count>0) then
    TRadioButton(FButtonList[0]).TabStop := True;
end;

class procedure TCustomRadioGroup.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterCustomRadioGroup;
end;

procedure TCustomRadioGroup.SetAutoFill(const AValue: Boolean);
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

procedure TCustomRadioGroup.SetColumnLayout(const AValue: TColumnLayout);
begin
  if FColumnLayout=AValue then exit;
  FColumnLayout:=AValue;
  if FColumnLayout=clHorizontalThenVertical then
    ChildSizing.Layout:=cclLeftToRightThenTopToBottom
  else
    ChildSizing.Layout:=cclTopToBottomThenLeftToRight;
  UpdateControlsPerLine;
end;

{------------------------------------------------------------------------------
  procedure TCustomRadioGroup.UpdateRadioButtonStates;
  
  Read all Checked properties of all radiobuttons, to update any changes in
  the interface to the LCL.
 ------------------------------------------------------------------------------}
procedure TCustomRadioGroup.UpdateRadioButtonStates;
var
  i: Integer;
begin
  FItemIndex:=-1;
  FHiddenButton.Checked;
  for i:=0 to FButtonList.Count-1 do
    if TRadioButton(FButtonList[i]).Checked then FItemIndex:=i;
  UpdateTabStops;
end;
