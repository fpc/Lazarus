{%MainUnit ../menus.pp}

{******************************************************************************
                                  TMenu
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

{------------------------------------------------------------------------------
  Method: TMenu.Create
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TMenu.Create(AOwner: TComponent);
begin
  FItems := TMenuItem.Create(Self);
  FItems.FOnChange := @MenuChanged;
  FItems.FMenu := Self;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := @ImageListChange;
  Inherited Create(AOwner);
end;

{------------------------------------------------------------------------------
  procedure TMenu.SetImages(const AValue: TCustomImageList);

  Creates the handle ( = object).
 ------------------------------------------------------------------------------}
procedure TMenu.SetImages(const AValue: TCustomImageList);
begin
  // ToDo
  FImages:=AValue;
end;

{------------------------------------------------------------------------------
  procedure TMenu.SetParent(const AValue: TComponent);

 ------------------------------------------------------------------------------}
procedure TMenu.SetParent(const AValue: TComponent);
begin
  if FParent=AValue then exit;
  FParent:=AValue;
  if (Items<>nil) and Items.HandleAllocated then
    DestroyHandle;
end;

procedure TMenu.ImageListChange(Sender: TObject);
begin
  if Sender = Images then UpdateItems;
end;

{------------------------------------------------------------------------------
  Method:  TMenu.CreateHandle
  Params:  None
  Returns: Nothing

  Creates the handle ( = object).
 ------------------------------------------------------------------------------}
procedure TMenu.CreateHandle;
var i: integer;
begin
  FItems.Handle := TWSMenuClass(WidgetSetClass).CreateHandle(Self);
  // initiate creation of subitems
  // Note: FItems is a TMenuItem. Using HandleNeeded will create all subitems.
  for i:=0 to Items.Count-1 do
    Items[i].HandleNeeded;
end;

procedure TMenu.DestroyHandle;
begin
  Items.DestroyHandle;
end;

procedure TMenu.DoChange(Source: TMenuItem; Rebuild: Boolean);
begin
  if Assigned(FOnChange) then FOnChange(Self, Source, Rebuild);
end;

{------------------------------------------------------------------------------
  Method: TMenu.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TMenu.Destroy;
begin
  FreeThenNil(FItems);
  FreeThenNil(FImageChangeLink);
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Function: TMenu.FindItem
  Params:
  Returns:


 ------------------------------------------------------------------------------}
function TMenu.FindItem(AValue: Integer; Kind: TFindItemKind): TMenuItem;

  function Find(Item: TMenuItem): TMenuItem;
  var
    I: Integer;
  begin
    Result := nil;
    if Item=nil then exit;
    if ((Kind = fkCommand) and (AValue = Item.Command))
    or ((Kind = fkHandle) and (AValue = Integer(Item.FHandle)))
    or ((Kind = fkShortCut) and (AValue = Item.ShortCut)) then
    begin
      Result := Item;
    end
    else
      for I := 0 to Item.GetCount - 1 do begin
        Result:=Find(Item[I]);
        if Result<>nil then
          Exit;
      end;
  end;

begin
  Result:=Find(Items);
end;

{------------------------------------------------------------------------------
  Function: TMenu.GetHandle
  Params:   none
  Returns:  String containing output from the function.

  Description of the function for the class.
 ------------------------------------------------------------------------------}
function TMenu.GetHandle: HMenu;
begin
  Result := FItems.Handle;
end;


{------------------------------------------------------------------------------
  Function: TMenu.GetChildren
  Params:   proc - procedure which has to be called for every item
  	    root - root component
  Returns:  nothing

  Helper function for streaming.
 ------------------------------------------------------------------------------}
procedure TMenu.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
   i : integer;
begin
   for i := 0 to FItems.Count - 1
      do if FItems[i].Owner = Root
       	    then Proc(TComponent (FItems [i]));
end;

procedure TMenu.MenuChanged(Sender: TObject; Source: TMenuItem; Rebuild: Boolean
  );
begin
  if ComponentState * [csLoading, csDestroying] = [] then
    DoChange(Source, Rebuild);
end;

procedure TMenu.UpdateItems;
{
  function UpdateItem(MenuItem: TMenuItem): Boolean;
  begin
    Result := False;
    IterateMenus(@UpdateItem, MenuItem.FMerged, MenuItem);
    MenuItem.SubItemChanged(MenuItem, MenuItem, True);
  end;
}
begin
  //IterateMenus(@UpdateItem, Items.FMerged, Items);
end;

{------------------------------------------------------------------------------
  Function: TMenu.HandleAllocated
  Params:   None
  Returns:  True if handle is allocated

  Checks if a handle is allocated. I.E. if the control is created
 ------------------------------------------------------------------------------}
function TMenu.HandleAllocated : Boolean;
begin
   Result := FItems.HandleAllocated;
end;

{------------------------------------------------------------------------------
  Method:  TMenu.HandleNeeded
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Description of the procedure for the class.
 ------------------------------------------------------------------------------}
procedure TMenu.HandleNeeded;
begin
  if not HandleAllocated then CreateHandle;
end;

function TMenu.DispatchCommand(ACommand: Word): Boolean;
var
  Item: TMenuItem;
begin
  Result := False;
  Item := FindItem(ACommand, fkCommand);
  if Item <> nil then
  begin
    Item.Click;
    Result := True;
  end;
end;

{------------------------------------------------------------------------------
  Function: TMenu.IsRightToLeft
  Params:
  Returns:


 ------------------------------------------------------------------------------}
function TMenu.IsRightToLeft : Boolean;
Begin
  //TODO: Make sure it should return FALSE!!!!!!!!!!
  Result := False;
end;

// included by menus.pp

{ =============================================================================

  $Log$
  Revision 1.27  2004/12/05 13:20:29  mattias
  destroying TMenu Handle when Set Parent=nil

  Revision 1.26  2004/11/10 20:53:18  vincents
  Destroy menu handle, when destroying form handle.

  Revision 1.25  2004/09/24 21:34:14  micha
  convert LM_CREATE message to interface methods
  remove SendMsgToInterface, CNSendMessage and related methods
  remove TWidgetSet.IntSendMessage3; all LCL to interface messages have been converted

  Revision 1.24  2004/04/10 17:58:57  mattias
  implemented mainunit hints for include files

  Revision 1.23  2004/02/23 08:19:04  micha
  revert intf split

  Revision 1.21  2003/12/18 08:50:13  micha
  attachmenutowindow cleanup

  Revision 1.20  2003/10/26 17:34:41  micha
  new interface method to attach a menu to window

  Revision 1.19  2003/10/22 17:50:16  mattias
  updated rpm scripts

  Revision 1.18  2003/06/24 15:57:55  mattias
  applied win32 menu patch from Micha Nelissen

  Revision 1.17  2003/06/13 06:05:49  mattias
  started context diff

  Revision 1.16  2002/11/12 10:16:17  lazarus
  MG: fixed TMainMenu creation

  Revision 1.15  2002/10/26 15:15:48  lazarus
  MG: broke LCL<->interface circles

  Revision 1.14  2002/10/24 09:37:39  lazarus
  MG: broke menus.pp <-> controls.pp circle

  Revision 1.13  2002/09/19 16:45:54  lazarus
  MG: fixed Menu.Free and gdkwindow=nil bug

  Revision 1.12  2002/08/22 13:45:58  lazarus
  MG: fixed non AutoCheck menuitems and editor bookmark popupmenu

  Revision 1.11  2002/08/12 15:32:29  lazarus
  MG: started enhanced menuitem

  Revision 1.10  2002/05/30 21:33:10  lazarus
  + added / fixed streaming functions for TMenu & TMenuItem, stoppok

  Revision 1.9  2002/05/19 08:27:43  lazarus
  + added helper functions to enabled streaming of TMenu /TMenuItem
    stoppok

  Revision 1.8  2002/05/16 14:23:20  lazarus
  MG: reuced output

  Revision 1.7  2002/05/15 15:38:15  lazarus
  MG: improved lresources for big files

  Revision 1.5  2002/05/10 06:05:53  lazarus
  MG: changed license to LGPL

  Revision 1.4  2002/05/09 12:41:28  lazarus
  MG: further clientrect bugfixes

  Revision 1.3  2001/03/12 12:17:01  lazarus
  MG: fixed random function results

  Revision 1.2  2000/12/22 19:55:37  lazarus
  Added the Popupmenu code to the LCL.
  Now you can right click on the editor and a PopupMenu appears.
  Shane

  Revision 1.1  2000/07/13 10:28:26  michael
  + Initial import

  Revision 1.1  2000/04/02 20:49:56  lazarus
  MWE:
    Moved lazarus/lcl/*.inc files to lazarus/lcl/include

  Revision 1.9  1999/12/20 21:37:12  lazarus
  Added ISRIGHTTOLEFT in menus file.
  Added ISACCEL in forms.pp
  Shane

  Revision 1.8  1999/12/10 00:47:01  lazarus
  MWE:
    Fixed some samples
    Fixed Dialog parent is no longer needed
    Fixed (Win)Control Destruction
    Fixed MenuClick


}
