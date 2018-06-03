{%MainUnit ../extctrls.pas}
{******************************************************************************
                                TCustomRadioGroup
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Delphi compatibility:

   - the interface is almost like in delphi 5
}


type

  { TRadioGroupStringList }

  TRadioGroupStringList = class(TStringList)
  private
    FRadioGroup: TCustomRadioGroup;
  protected
    procedure Changed; override;
  public
    constructor Create(TheRadioGroup: TCustomRadioGroup);
    procedure Assign(Source: TPersistent); override;
  end;

{ TRadioGroupStringList }

procedure TRadioGroupStringList.Changed;
begin
  inherited Changed;
  if (UpdateCount = 0) then
    FRadioGroup.UpdateAll
  else
    FRadioGroup.UpdateInternalObjectList;
  FRadioGroup.FLastClickedItemIndex := FRadioGroup.FItemIndex;
end;

constructor TRadioGroupStringList.Create(TheRadioGroup: TCustomRadioGroup);
begin
  inherited Create;
  FRadioGroup := TheRadioGroup;
end;

procedure TRadioGroupStringList.Assign(Source: TPersistent);
var
  SavedIndex: Integer;
begin
  SavedIndex := FRadioGroup.ItemIndex;
  inherited Assign(Source);
  if SavedIndex < Count then FRadioGroup.ItemIndex := SavedIndex;
end;


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
  FItems := TRadioGroupStringList.Create(Self);
  FAutoFill := true;
  FItemIndex  := -1;
  FLastClickedItemIndex := -1;
  FButtonList := TFPList.Create;
  FColumns  := 1;
  FColumnLayout := clHorizontalThenVertical;
  ChildSizing.Layout:=cclLeftToRightThenTopToBottom;
  ChildSizing.ControlsPerLine:=FColumns;
  ChildSizing.ShrinkHorizontal:=crsScaleChilds;
  ChildSizing.ShrinkVertical:=crsScaleChilds;
  ChildSizing.EnlargeHorizontal:=crsHomogenousChildResize;
  ChildSizing.EnlargeVertical:=crsHomogenousChildResize;
  ChildSizing.LeftRightSpacing:=6;
  ChildSizing.TopBottomSpacing:=0;
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
  //DebugLn(['[TCustomRadioGroup.InitializeWnd] A ',DbgSName(Self),' FItems.Count=',FItems.Count,' HandleAllocated=',HandleAllocated,' ItemIndex=',ItemIndex]);
  UpdateItems;
  inherited InitializeWnd;
  RealizeItemIndex;
  //debugln(['TCustomRadioGroup.InitializeWnd END']);
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
        OnClick := @Self.Clicked;
        OnChange := @Self.Changed;
        OnEnter := @Self.ItemEnter;
        OnExit := @Self.ItemExit;
        OnKeyDown := @Self.ItemKeyDown;
        OnKeyUp := @Self.ItemKeyUp;
        OnKeyPress := @Self.ItemKeyPress;
        OnUTF8KeyPress := @Self.ItemUTF8KeyPress;
        OnResize := @Self.ItemResize;
        ParentFont := True;
        BorderSpacing.CellAlignHorizontal := ccaLeftTop;
        BorderSpacing.CellAlignVertical := ccaCenter;
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

    if (FItemIndex>=FItems.Count) and not (csLoading in ComponentState) then FItemIndex:=FItems.Count-1;

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
      FHiddenButton.Parent:=Self;

      // the checked and unchecked states can be applied only after all other
      for i := 0 to FItems.Count-1 do
      begin
        ARadioButton := TRadioButton(FButtonList[i]);
        ARadioButton.Checked := (i = FItemIndex);
        ARadioButton.Visible := true;
      end;
      //FHiddenButton must remain the last item in Controls[], so that Controls[] is in sync with Items[]
      Self.RemoveControl(FHiddenButton);
      Self.InsertControl(FHiddenButton);
      if HandleAllocated then
        FHiddenButton.HandleNeeded;
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
    NewIndex := ItemIndex;
    repeat
      Inc(NewIndex, StepSize);
      if (NewIndex >= Count) or (NewIndex < 0) then begin
        NewIndex := (NewIndex + WrapOffSet + BlockSize) mod BlockSize;
        // Keep moving in the same direction until in valid range
        while NewIndex >= Count do
           NewIndex := (NewIndex + StepSize) mod BlockSize;
      end;
    until (NewIndex = ItemIndex) or TRadioButton(FButtonList[NewIndex]).Enabled;
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
  if Key <> 0 then
    KeyDown(Key, Shift);
end;

procedure TCustomRadioGroup.ItemKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key <> 0 then
    KeyUp(Key, Shift);
end;

procedure TCustomRadioGroup.ItemKeyPress(Sender: TObject; var Key: Char);
begin
  if Key <> #0 then
    KeyPress(Key);
end;

procedure TCustomRadioGroup.ItemUTF8KeyPress(Sender: TObject;
  var UTF8Key: TUTF8Char);
begin
  UTF8KeyPress(UTF8Key);
end;

{------------------------------------------------------------------------------
  Method: TCustomRadioGroup.SetColumns
  Params:  value - no of columns of the radiogroup
  Returns: Nothing

  Set the FColumns property which determines the number of columns in
  which the radiobuttons should be arranged.
  Range: 1 .. ???
 ------------------------------------------------------------------------------}
procedure TCustomRadioGroup.SetColumns(Value: integer);
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
  // needed later if handle isn't allocated
  OldItemIndex := FItemIndex;
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
    end else
    begin
      FItemIndex := Value;
      // maybe handle was recreated. issue #26714
      FLastClickedItemIndex := -1;

      // trigger event to be delphi compat, even if handle isn't allocated.
      // issue #15989
      if (Value <> OldItemIndex) and not FCreatingWnd then
      begin
        if Assigned(FOnClick) then FOnClick(Self);
        if Assigned(FOnSelectionChanged) then FOnSelectionChanged(Self);
        FLastClickedItemIndex := FItemIndex;
      end;
    end;
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
  if FCreatingWnd or FUpdatingItems then
    exit;
  if [csLoading,csDestroying]*ComponentState<>[] then exit;
  UpdateRadioButtonStates;
  if [csDesigning]*ComponentState<>[] then exit;
  if FLastClickedItemIndex=FItemIndex then exit;
  FLastClickedItemIndex:=FItemIndex;
  EditingDone;
  // for Delphi compatibility: OnClick should be invoked, whenever ItemIndex
  // has changed
  if Assigned (FOnClick) then FOnClick(Self);
  // And a better named LCL equivalent
  if Assigned (FOnSelectionChanged) then FOnSelectionChanged(Self);
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
  FLastClickedItemIndex:=FItemIndex;
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
end;

class procedure TCustomRadioGroup.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterCustomRadioGroup;
end;

procedure TCustomRadioGroup.UpdateInternalObjectList;
begin
  UpdateItems;
end;

procedure TCustomRadioGroup.UpdateAll;
begin
  UpdateItems;
  UpdateControlsPerLine;
  OwnerFormDesignerModified(Self);
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

procedure TCustomRadioGroup.FlipChildren(AllLevels: Boolean);
begin
  // no flipping
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
