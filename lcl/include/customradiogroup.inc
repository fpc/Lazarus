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
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Constructor for the radiogroup
 ------------------------------------------------------------------------------}
constructor TCustomRadioGroup.Create (AOwner : TComponent);
begin
  inherited Create (AOwner);
  FCreatingWnd := false;
  ControlStyle := ControlStyle + [csCaptureMouse, csClickEvents, csSetCaption,
                                  csDoubleClicks];
  FItems      := TStringList.Create;
  TStringList(FItems).OnChanging := @ItemsChanged;
  TStringList(FItems).OnChange := @ItemsChanged;
  FItemIndex  := -1;
  FButtonList := TList.Create;
  FColumns  := 1;
  Width:= 250;
  Height := 200;
end;


{------------------------------------------------------------------------------
  Method: TCustomRadioGroup.Destroy
  Params:  none
  Returns: Nothing

  Destructor for the radiogroup
 ------------------------------------------------------------------------------}
destructor TCustomRadioGroup.Destroy;
begin
  FItems.Free;
  FButtonList.Free;
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method: TCustomRadioGroup.CreateWnd
  Params:  none
  Returns: Nothing

  Create the visual component of the Radiogroup.
 ------------------------------------------------------------------------------}
procedure TCustomRadioGroup.CreateWnd;
var
  i       : integer;
  temp    : TRadioButton;
begin
  if FCreatingWnd then exit;
  FCreatingWnd := true;
  //writeln('[TCustomRadioGroup.CreateWnd] A ',Name,':',ClassName,' FItems.Count=',FItems.Count,' HandleAllocated=',HandleAllocated);
  // destroy radiobuttons, if there are too many
  while FButtonList.Count>FItems.Count do begin
    TRadioButton(FButtonList[FButtonList.Count-1]).Free;
    FButtonList.Delete(FButtonList.Count-1);
  end;
  //writeln('[TCustomRadioGroup.CreateWnd] B ',Name,':',ClassName,' FItems.Count=',FItems.Count,' HandleAllocated=',HandleAllocated);
  // create as many TRadioButton as needed
  while (FButtonList.Count<FItems.Count) do begin
    Temp := TRadioButton.Create (self);
    Temp.Name:='RadioButton'+IntToStr(FButtonList.Count);
    Temp.AutoSize := False;
    Temp.OnClick := @Clicked;
    FButtonList.Add(Temp);
  end;
    
  //writeln('[TCustomRadioGroup.CreateWnd] C ',Name,':',ClassName,' FItems.Count=',FItems.Count,' HandleAllocated=',HandleAllocated);
  if FItems.Count>=0 then begin
    if (FItemIndex>=FItems.Count) then FItemIndex:=0;
  end else
    FItemIndex:=-1;

  //writeln('[TCustomRadioGroup.CreateWnd] D ',Name,':',ClassName,' ',FItems.Count);
  inherited CreateWnd;
  //writeln('[TCustomRadioGroup.CreateWnd] E ',Name,':',ClassName,' ',FItems.Count,' ',FButtonList.Count);

  if FItems.Count>0 then begin
    for i:=0 to FItems.Count-1 do begin
      Temp := TRadioButton(FButtonList[i]);
      Temp.Caption := FItems[i];
      Temp.Parent := Self;
    end;
    DoPositionButtons;

    for i:=0 to FItems.Count-1 do begin
      Temp := TRadioButton(FButtonList[i]);
      Temp.Checked := (i = FItemIndex);
      Temp.Visible := true;
    end;
  end;
  //writeln('[TCustomRadioGroup.CreateWnd] F ',Name,':',ClassName,' ',FItems.Count);

  FCreatingWnd := false;
end;

{------------------------------------------------------------------------------
  Method: TCustomRadioGroup.ItemsChanged
  Params:  sender : object calling this proc. (in fact the FItems instance)
  Returns: Nothing

  Set the FColumns property which determines the no columns in
  which the radiobuttons should be arranged.
  Range: 1 .. ???
 ------------------------------------------------------------------------------}
procedure TCustomRadioGroup.ItemsChanged (Sender : TObject);
begin
  if HandleAllocated then RecreateWnd;
  OwnerFormDesignerModified(Self);
end;

{------------------------------------------------------------------------------
  Method: TCustomRadioGroup.SetColumns
  Params:  value - no of columns of the radiogroup
  Returns: Nothing

  Set the FColumns property which determines the no columns in
  which the radiobuttons should be arranged.
  Range: 1 .. ???
 ------------------------------------------------------------------------------}
procedure TCustomRadioGroup.SetColumns (value : integer);
begin
  if Value <> FColumns then 
  begin
     if (Value < 1) 
        then raise Exception.Create('TCustomRadioGroup: Columns must be >= 1');
     FColumns := Value;
     if HandleAllocated then RecreateWnd;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomRadioGroup.SetItem
  Params:  value - Stringlist containing items to be displayed as radiobuttons
  Returns: Nothing

  Assign items from a stringlist.
 ------------------------------------------------------------------------------}
procedure TCustomRadioGroup.SetItem (value : TStrings);
begin
  if (Value <> FItems) then
  begin
     FItems.Assign(Value);
     if HandleAllocated then RecreateWnd;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomRadioGroup.SetItemIndex
  Params:  value - index of RadioButton to be selected
  Returns: Nothing

  Select one of the radiobuttons
 ------------------------------------------------------------------------------}
procedure TCustomRadioGroup.SetItemIndex (Value : integer);
begin
  if FReading then FItemIndex:=Value
  else begin
    if (Value < -1) or (Value >= FItems.Count) then
        raise Exception.Create('TCustomRadioGroup.SetItemIndex : Index out of bounds');

    if (HandleAllocated) and (Value <> FItemIndex) then
    begin
      if (FItemIndex <> -1) then
         TRadioButton (FButtonList [FItemIndex]).Checked := false;
      FItemIndex := Value;
      if (Value <> -1) then
         TRadioButton (FButtonList [Value]).Checked := true;
         
      ItemsChanged(self);
    end
    else FItemIndex := Value;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomRadioGroup.GetItemIndex
  Params:  value - index of RadioButton to be selected
  Returns: Nothing

  Retrieve the index of the radiobutton currently selected.
 ------------------------------------------------------------------------------}
function TCustomRadioGroup.GetItemIndex : integer;
var
   i       : integer;
begin
   result := -1;
   if (HandleAllocated) then
   begin
      i := 0;
      // This nasty little loop is neccessary because the group is not informed
      // when a button is pressed	
      while (i < FButtonList.Count) and (Result = -1) do
      begin	// find the actice button
        if TRadioButton (FButtonList [i]).Checked
          then result := i;
        inc (i);
      end;
      FItemIndex := Result;
   end
   else
      Result := FItemIndex;
end;

{------------------------------------------------------------------------------
  Method: TCustomRadioGroup.WMSize
  Params:  Message: TLMSize
  Returns: none

  Reposition buttons on resize
 ------------------------------------------------------------------------------}
procedure TCustomRadioGroup.WMSize(var Message: TLMSize);
begin
  if HandleAllocated then DoPositionButtons;
  inherited;
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
  if HandleAllocated then RecreateWnd;
end;

{------------------------------------------------------------------------------
  Method: TCustomRadioGroup.Clicked
  Params: sender - the calling object

  This is the callback for all radiobuttons in the group. If an OnClick
  handler is assigned it will be called
 ------------------------------------------------------------------------------}
Procedure TCustomRadioGroup.Clicked(Sender : TObject);
Begin
  GetItemIndex;
  if FCreatingWnd then exit;
  if Assigned (FOnClick) then FOnClick(Self);
end;

{------------------------------------------------------------------------------
  Method: TCustomRadioGroup.DoPositionButtons
  Params: none

  Set bounds of radio buttons
 ------------------------------------------------------------------------------}
procedure TCustomRadioGroup.DoPositionButtons;
var
  i       : integer;
  temp    : TRadioButton;
  nextTop : integer;
  nextLeft: integer;
  vertDist: integer;
  horzDist: integer;
  rbWidth : integer;
begin
  if FItems.Count>0 then begin
    // position in rows and columns
    vertDist := (Height - 20) DIV (((FItems.Count-1) DIV FColumns)+1);
    horzDist := (Width - 20) DIV FColumns;
    nextTop  := 0;
    nextLeft := 10;
    rbWidth  := horzDist;
    i := 0;
    while i < FItems.Count do begin
      Temp := TRadioButton(FButtonList[i]);
      Temp.SetBounds(nextLeft,nextTop,rbWidth,vertDist);

      inc (i);
      if (i MOD FColumns) = 0 then begin
        inc(nextTop, vertDist);
        nextLeft := 10;
      end else begin
        inc(nextLeft, horzDist);
      end;
    end;
  end;
end;

{
  $Log$
  Revision 1.15  2003/01/24 13:53:53  mattias
  fixed TRadioGroup.Items editing in IDE

  Revision 1.14  2002/09/03 08:07:19  lazarus
  MG: image support, TScrollBox, and many other things from Andrew

  Revision 1.13  2002/08/30 12:32:20  lazarus
  MG: MoveWindowOrgEx, Splitted FWinControls/FControls, TControl drawing, Better DesignerDrawing, ...

  Revision 1.12  2002/08/17 15:45:32  lazarus
  MG: removed ClientRectBugfix defines

  Revision 1.11  2002/05/13 14:47:00  lazarus
  MG: fixed client rectangles, TRadioGroup, RecreateWnd

  Revision 1.10  2002/05/13 06:12:57  lazarus
  MG: fixed saving unitlinks after changing fpc soure path

  Revision 1.9  2002/05/10 06:05:52  lazarus
  MG: changed license to LGPL

  Revision 1.8  2001/12/31 22:43:00  lazarus
  Added a TViewColumn editor to be used in the object inspector as TViewColumn's property editor.
  Shane

  Revision 1.7  2001/10/19 14:27:43  lazarus
  MG: fixed customradiogroup OnClick + ItemIndex

  Revision 1.6  2001/04/17 21:33:52  lazarus
  + added working OnClick support for TCustomRadiogroup, stoppok

  Revision 1.5  2001/03/15 14:42:20  lazarus
  MG: customradiogroup is now streamable

  Revision 1.4  2001/02/06 13:38:58  lazarus
  Fixes from Mattias for EditorOPtions
  Fixes to COmpiler that should allow people to compile if their path is set up.
  Changes to code completion.
  Shane

  Revision 1.3  2001/02/01 19:34:50  lazarus
  TScrollbar created and a lot of code added.

  It's cose to working.
  Shane

  Revision 1.2  2000/12/29 15:04:07  lazarus
  Added more images to the resource.
  Shane

  Revision 1.1  2000/07/13 10:28:25  michael
  + Initial import

  Revision 1.2  2000/06/22 20:57:07  lazarus
  *** empty log message ***

  Revision 1.1  2000/04/02 20:49:56  lazarus
  MWE:
    Moved lazarus/lcl/*.inc files to lazarus/lcl/include

  Revision 1.3  2000/01/06 01:10:36  lazarus
  Stoppok:
     - changed ReadState to match current definition in fcl
       (affects TPage & TCustomNotebook)
     - added callback FItems.OnChanging to TCustomRadiogroup

  Revision 1.2  2000/01/02 00:25:12  lazarus
  Stoppok:
    - enhanced TCustomradiogroup & TCustomgroupbox

  Revision 1.1  1999/12/31 02:20:57  lazarus
    Initial implementation of TCustomRadioGroup / TRadioGroup
      stoppok

}
