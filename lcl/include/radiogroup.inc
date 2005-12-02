{%MainUnit ../extctrls.pas}
{******************************************************************************
                                TCustomRadioBox
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

  Delphi compatibility:

   - the interface is almost like in delphi 5
   - with the GTK-bindings there must always one button active,
     ItemIndex= -1 can't be handled
   - FlipChildren procedure is missing
   
  TODO:
    - faster CreateWnd

  Possible improvements:
       - The current implementation often recreates the group even
         if it might not be neccessary. This could be solved if with
         an approach like Marc Weustink suggested:

         "Why not on SetColumn/SetItems/FItems.Onchange create the necessary
         checkboxes and align them. This way the RadioGroup is just a control
         with other controls in it. It doesn't matter if the the gtk control is
         created or not.
         If not created and you already have added checkboxes, they will be 
         created when the groupbox is created and will be destroyed when the
         groupbox is destroyed. This way you internally allways deal with
         TCheckboxes and you dont have to mess with creating/destroying them.
         Besides that, you dont have to recreate the control on every change."
  
         On the other side this might have the following disadvantages:
           - requires some work to find out which buttons to add/delete
           - the TButtonList and the group property of affected buttons
             have to be updated according to the new order of buttons
           - works only if the interface library supports reordering of
             radiobuttons

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
  FCreatingWnd := false;
  ControlStyle := ControlStyle + [csCaptureMouse, csClickEvents, csSetCaption,
                                  csDoubleClicks];
  FItems      := TStringList.Create;
  //TStringList(FItems).OnChanging := @ItemsChanged;
  TStringList(FItems).OnChange := @ItemsChanged;
  FItemIndex  := -1;
  FLastClickedItemIndex := -1;
  FButtonList := TList.Create;
  FColumns  := 1;
  FColumnLayout := clHorizontalThenVertical;
  SetInitialBounds(0,0,250,200);
  TabStop := True;
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
var
  i       : integer;
  temp    : TRadioButton;
begin
  if FCreatingWnd then RaiseGDBException('TCustomRadioGroup.InitializeWnd');
  FCreatingWnd := true;
  //DebugLn('[TCustomRadioGroup.InitializeWnd] A ',Name,':',ClassName,' FItems.Count=',FItems.Count,' HandleAllocated=',HandleAllocated,' ItemIndex=',ItemIndex);
  
  inherited InitializeWnd;

  // destroy radiobuttons, if there are too many
  while FButtonList.Count>FItems.Count do begin
    TRadioButton(FButtonList[FButtonList.Count-1]).Free;
    FButtonList.Delete(FButtonList.Count-1);
  end;
  //DebugLn('[TCustomRadioGroup.InitializeWnd] B ',Name,':',ClassName,' FItems.Count=',FItems.Count,' HandleAllocated=',HandleAllocated);

  // create as many TRadioButton as needed
  while (FButtonList.Count<FItems.Count) do begin
    Temp := TRadioButton.Create(Self);
    Temp.Name:='RadioButton'+IntToStr(FButtonList.Count);
    Temp.AutoSize := True;
    Temp.OnClick := @Clicked;
    Temp.OnChange := @Changed;
    Temp.OnEnter :=@ItemEnter;
    Temp.OnExit :=@ItemExit;
    Temp.OnKeyDown :=@ItemKeyDown;
    Temp.OnResize := @ItemResize;
    include(temp.ControlStyle, csNoDesignSelectable);
    FButtonList.Add(Temp);
  end;
  if FHiddenButton=nil then begin
    FHiddenButton:=TRadioButton.Create(nil);
    with FHiddenButton do begin
      Name:='HiddenRadioButton';
      Visible:=false;
      include(ControlStyle, csNoDesignSelectable);
    end;
  end;
    
  //DebugLn('[TCustomRadioGroup.InitializeWnd] C ',Name,':',ClassName,' FItems.Count=',FItems.Count,' HandleAllocated=',HandleAllocated);
  if (FItemIndex>=FItems.Count) then FItemIndex:=FItems.Count-1;

  if FItems.Count>0 then begin
    for i:=0 to FItems.Count-1 do begin
      Temp := TRadioButton(FButtonList[i]);
      Temp.Caption := FItems[i];
      Temp.Parent:=Self;
      Temp.SetZOrder(false);
    end;
    with FHiddenButton do begin
      FHiddenButton.Visible:=false;
      Parent:=Self;
      FHiddenButton.HandleNeeded;
    end;
    
    PositionButtons;

    for i:=0 to FItems.Count-1 do begin
      Temp := TRadioButton(FButtonList[i]);
      Temp.Checked := (i = FItemIndex);
      Temp.Visible := true;
    end;
    FHiddenButton.Checked:=(fItemIndex=-1);
    UpdateTabStops;
  end;
  //DebugLn('[TCustomRadioGroup.InitializeWnd] F ',Name,':',ClassName,' FItems.Count=',FItems.Count,' HandleAllocated=',HandleAllocated,' ItemIndex=',ItemIndex);
  
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
  PositionButtons;
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
      BlockSize := FColumns * (Rows+1);
      StepSize := HorzDiff + VertDiff * FColumns;
      WrapOffSet := VertDiff;
    end
    else begin
      //add a column for ease wrapping
      BlockSize := (FColumns+1) * Rows;
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
  // TODO: Remove RecreateWnd
  if HandleAllocated and (not (csLoading in ComponentState)) then
    RecreateWnd(Self);
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
procedure TCustomRadioGroup.SetColumns (value : integer);
begin
  if Value <> FColumns then begin
    if (Value < 1)
       then raise Exception.Create('TCustomRadioGroup: Columns must be >= 1');
    FColumns := Value;
    if HandleAllocated and (not (csLoading in ComponentState)) then
      PositionButtons;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomRadioGroup.SetItem
  Params:  value - Stringlist containing items to be displayed as radiobuttons
  Returns: Nothing

  Assign items from a stringlist.
 ------------------------------------------------------------------------------}
procedure TCustomRadioGroup.SetItem(Value: TStrings);
begin
  if (Value <> FItems) then
  begin
    FItems.Assign(Value);
    // TODO: Remove RecreateWnd
    if HandleAllocated and (not (csLoading in ComponentState)) then
      RecreateWnd(Self);
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
  if Value = FItemIndex then exit;
  if FReading then
    FItemIndex:=Value
  else begin
    if (Value < -1) or (Value >= FItems.Count) then
      raise Exception.CreateFmt(rsIndexOutOfBounds,[ClassName,Value,FItems.Count]);

    if (HandleAllocated) then
    begin
      // the radiobuttons are grouped by the widget interface
      // and some does not allow to uncheck all buttons in a group
      // Therefore there is a hidden button
      OldItemIndex:=FItemIndex;
      FItemIndex:=Value;
      OldIgnoreClicks:=fIgnoreClicks;
      fIgnoreClicks:=true;
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
        fIgnoreClicks:=OldIgnoreClicks;
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

{------------------------------------------------------------------------------
  Method: TCustomRadioGroup.Resize

  Reposition buttons on resize
 ------------------------------------------------------------------------------}
procedure TCustomRadioGroup.Resize;
begin
  if HandleAllocated then PositionButtons;
  inherited Resize;
end;

procedure TCustomRadioGroup.CheckItemIndexChanged;
begin
  if FCreatingWnd then exit;
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
  // TODO: Remove RecreateWnd
  if HandleAllocated then RecreateWnd(Self);
end;

{------------------------------------------------------------------------------
  Method: TCustomRadioGroup.Clicked
  Params: sender - the calling object

  This is the callback for all radiobuttons in the group. If an OnClick
  handler is assigned it will be called
 ------------------------------------------------------------------------------}
Procedure TCustomRadioGroup.Clicked(Sender : TObject);
Begin
  if fIgnoreClicks then exit;
  CheckItemIndexChanged;
end;

{------------------------------------------------------------------------------
  Method: TCustomRadioGroup.Changed
  Params: sender - the calling object

  Checks for changes. Does the same as Clicked for Delphi compatibility.
 ------------------------------------------------------------------------------}
Procedure TCustomRadioGroup.Changed(Sender : TObject);
Begin
  CheckItemIndexChanged;
end;

{------------------------------------------------------------------------------
  Method: TCustomRadioGroup.PositionButtons
  Params: none

  Set bounds of radio buttons
 ------------------------------------------------------------------------------}
procedure TCustomRadioGroup.PositionButtons;
var
  i: integer;
  CurButton: TRadioButton;
  nextTop: integer;
  nextLeft: integer;
  vertDist: integer;
  horzDist: integer;
  MaxRows: Integer;
  UsedColumnCount: integer;
  UsedRowCount: integer;
begin
  if FButtonList.Count=0 then exit;
  
  if TRadioButton(FButtonList[0]).AutoSizeDelayed then exit;
  DisableAutoSizing;
  try
    UsedColumnCount:=FColumns;
    if UsedColumnCount<1 then UsedColumnCount:=1;
    if AutoFill and (UsedColumnCount>FButtonList.Count) then
      UsedColumnCount:=FButtonList.Count;

    UsedRowCount:=((FButtonList.Count-1) div UsedColumnCount)+1;
    
    // position in rows and columns
    vertDist := (Height - 20) DIV UsedRowCount;
    horzDist := (Width - 20) DIV UsedColumnCount;
    nextTop  := 0;
    nextLeft := 10;
    MaxRows := (FButtonList.Count+FColumns-1) div FColumns;
    i := 0;
    while i < FButtonList.Count do begin
      CurButton := TRadioButton(FButtonList[i]);
      CurButton.SetBounds(nextLeft,nextTop,CurButton.Width,CurButton.Height);

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
  finally
    EnableAutoSizing;
  end;
end;

procedure TCustomRadioGroup.UpdateTabStops;
var
  i: Integer;
  RadioBtn: TRadioButton;
begin
  for i := 0 to FButtonList.Count-1 do begin
    RadioBtn := TRadioButton(FButtonList[i]);
    RadioBtn.TabStop := RadioBtn.Checked;
  end;
  if (FItemIndex=-1) and (Items.Count>0) then
    TRadioButton(FButtonList[0]).TabStop := true;
end;

procedure TCustomRadioGroup.SetAutoFill(const AValue: Boolean);
begin
  if FAutoFill=AValue then exit;
  FAutoFill:=AValue;
  PositionButtons;
end;

procedure TCustomRadioGroup.SetColumnLayout(const AValue: TColumnLayout);
begin
  if FColumnLayout=AValue then exit;
  FColumnLayout:=AValue;
  PositionButtons;
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

