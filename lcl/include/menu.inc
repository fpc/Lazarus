// included by menus.pp

{******************************************************************************
                                  TMenu
 ******************************************************************************}

{------------------------------------------------------------------------------
  Method: TMenu.Create
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TMenu.Create(AOwner: TComponent);
begin
  Inherited Create(AOwner);
  FItems := TMenuItem.Create(Self);
  FItems.FMenu := Self;
end;

{------------------------------------------------------------------------------
  Method:  TMenu.CreateHandle
  Params:  None
  Returns: Nothing

  Creates the handle ( = object).
 ------------------------------------------------------------------------------}
procedure TMenu.CreateHandle;
begin
  InterfaceObject.IntSendMessage3(LM_CREATE, Self, nil); // CreateComponent(nil);
  // initiate creation of subitems
  // Note: FItems is a MenuItem, by using Createhandle all subitems will be
  //       created.
  FItems.CreateHandle;
end;

{------------------------------------------------------------------------------
  Method: TMenu.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TMenu.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Function: TMenu.FindItem
  Params:   
  Returns:  

  
 ------------------------------------------------------------------------------}
function TMenu.FindItem(Value: Integer; Kind: TFindItemKind): TMenuItem;
begin
  //TODO: FINISH TMenu:FINDITEM
  Result:=nil;
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
