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
  FButtonList := TList.Create;
  FColumns  := 1;
  FColumnLayout := clHorizontalThenVertical;
  SetInitialBounds(0,0,250,200);
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
  //DebugLn('[TCustomRadioGroup.CreateWnd] A ',Name,':',ClassName,' FItems.Count=',FItems.Count,' HandleAllocated=',HandleAllocated,' ItemIndex=',ItemIndex);

  // destroy radiobuttons, if there are too many
  while FButtonList.Count>FItems.Count do begin
    TRadioButton(FButtonList[FButtonList.Count-1]).Free;
    FButtonList.Delete(FButtonList.Count-1);
  end;
  //DebugLn('[TCustomRadioGroup.CreateWnd] B ',Name,':',ClassName,' FItems.Count=',FItems.Count,' HandleAllocated=',HandleAllocated);

  // create as many TRadioButton as needed
  while (FButtonList.Count<FItems.Count) do begin
    Temp := TRadioButton.Create(Self);
    Temp.Name:='RadioButton'+IntToStr(FButtonList.Count);
    Temp.AutoSize := False;
    Temp.OnClick := @Clicked;
    FButtonList.Add(Temp);
  end;
  if FHiddenButton=nil then begin
    FHiddenButton:=TRadioButton.Create(nil);
    with FHiddenButton do begin
      Name:='HiddenRadioButton';
      Visible:=false;
    end;
  end;
    
  //DebugLn('[TCustomRadioGroup.CreateWnd] C ',Name,':',ClassName,' FItems.Count=',FItems.Count,' HandleAllocated=',HandleAllocated);
  if (FItemIndex>=FItems.Count) then FItemIndex:=FItems.Count-1;

  //DebugLn('[TCustomRadioGroup.CreateWnd] D ',Name,':',ClassName,' ',FItems.Count);
  inherited CreateWnd;
  //DebugLn('[TCustomRadioGroup.CreateWnd] E ',Name,':',ClassName,' ',FItems.Count,' ',FButtonList.Count);

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
    
    DoPositionButtons;

    for i:=0 to FItems.Count-1 do begin
      Temp := TRadioButton(FButtonList[i]);
      Temp.Checked := (i = FItemIndex);
      Temp.Visible := true;
    end;
    FHiddenButton.Checked:=(fItemIndex=-1);
  end;
  //DebugLn('[TCustomRadioGroup.CreateWnd] F ',Name,':',ClassName,' FItems.Count=',FItems.Count,' HandleAllocated=',HandleAllocated,' ItemIndex=',ItemIndex);
  
  FCreatingWnd := false;
end;

function TCustomRadioGroup.Rows: integer;
begin
  if FItems.Count>0 then
    Result:=((FItems.Count-1) div Columns)+1
  else
    Result:=0;
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

  Set the FColumns property which determines the no columns in
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
      DoPositionButtons;
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
      FItemIndex:=Value;
      if (FItemIndex <> -1) then
        TRadioButton(FButtonList[FitemIndex]).Checked := true
      else
        FHiddenButton.Checked:=true;
      // this has automatically unset the old button. But they do not recognize
      // it. Update the states.
      UpdateRadioButtonStates;

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
  if HandleAllocated then DoPositionButtons;
  inherited Resize;
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
  if FCreatingWnd then exit;
  UpdateRadioButtonStates;
  if [csLoading,csDestroying,csDesigning]*ComponentState<>[] then exit;
  EditingDone;
  if Assigned (FOnClick) then FOnClick(Self);
end;

{------------------------------------------------------------------------------
  Method: TCustomRadioGroup.DoPositionButtons
  Params: none

  Set bounds of radio buttons
 ------------------------------------------------------------------------------}
procedure TCustomRadioGroup.DoPositionButtons;
var
  i: integer;
  CurButton: TRadioButton;
  nextTop: integer;
  nextLeft: integer;
  vertDist: integer;
  horzDist: integer;
  rbWidth: integer;
  MaxRows: Integer;
begin
  if (FItems.Count>0) and (FColumns>0) then begin
    // position in rows and columns
    vertDist := (Height - 20) DIV (((FItems.Count-1) DIV FColumns)+1);
    horzDist := (Width - 20) DIV FColumns;
    nextTop  := 0;
    nextLeft := 10;
    rbWidth  := horzDist;
    MaxRows := (FItems.Count+FColumns-1) div FColumns;
    i := 0;
    while i < FItems.Count do begin
      CurButton := TRadioButton(FButtonList[i]);
      CurButton.SetBounds(nextLeft,nextTop,rbWidth,vertDist);

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

procedure TCustomRadioGroup.SetColumnLayout(const AValue: TColumnLayout);
begin
  if FColumnLayout=AValue then exit;
  FColumnLayout:=AValue;
  DoPositionButtons;
end;

{------------------------------------------------------------------------------
  procedure TCustomRadioGroup.UpdateRadioButtonStates;
 ------------------------------------------------------------------------------}
procedure TCustomRadioGroup.UpdateRadioButtonStates;
var
  i: Integer;
begin
  FItemIndex:=-1;
  FHiddenButton.Checked;
  for i:=0 to FButtonList.Count-1 do
    if TRadioButton(FButtonList[i]).Checked then FItemIndex:=i;
end;

{
  $Log$
  Revision 1.7  2005/02/26 17:08:41  marc
  * Reworked listviews to match new interface

  Revision 1.6  2005/01/29 14:36:04  mattias
  reactivated fast xml units without widestrings

  Revision 1.5  2004/09/09 09:35:44  mattias
  renamed customradiogroup.inc to radiogroup.inc

  Revision 1.33  2004/09/04 22:24:16  mattias
  added default values for compiler skip options and improved many parts of synedit for UTF8

  Revision 1.32  2004/07/16 21:49:00  mattias
  added RTTI controls

  Revision 1.31  2004/07/13 10:34:15  mattias
  fixed lcl package unit file name checklist.pas

  Revision 1.30  2004/07/11 23:08:43  mattias
  updated russian translation  from vasily

  Revision 1.29  2004/07/10 18:17:30  mattias
  added Delphi ToDo support, Application.WndProc, small bugfixes  from Colin

  Revision 1.28  2004/05/11 11:42:27  mattias
  replaced writeln by debugln

  Revision 1.27  2004/04/10 17:58:57  mattias
  implemented mainunit hints for include files

  Revision 1.26  2004/02/08 11:31:32  mattias
  TMenuItem.Bitmap is now auto created on read. Added TMenuItem.HasBitmap

  Revision 1.25  2003/12/21 16:01:58  mattias
  workaround for inherited bug in fpc 1.9

  Revision 1.24  2003/11/15 13:38:21  mattias
  fixed using protected member

  Revision 1.23  2003/10/15 18:01:10  mattias
  implemented extract proc, check lfm and convert delphi unit

  Revision 1.22  2003/07/10 15:17:43  mattias
  fixed reading empty stream and TCustomRadioGroup.ItemsChanged

  Revision 1.21  2003/06/19 09:26:58  mattias
  fixed changing unitname during update

  Revision 1.20  2003/03/17 23:39:30  mattias
  added TCheckGroup

  Revision 1.19  2003/03/17 20:50:30  mattias
  fixed TRadioGroup.ItemIndex=-1

  Revision 1.18  2003/03/17 09:41:52  mattias
  fixed TCustomRadioGroup.SetItemIndex

  Revision 1.17  2003/03/17 09:33:51  mattias
  fixed TCustomRadioGroup.GetItemIndex

  Revision 1.16  2003/03/11 07:46:43  mattias
  more localization for gtk- and win32-interface and lcl

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
