// included by menus.pp

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
  Inherited Create(AOwner);
  FItems := TMenuItem.Create(Self);
  FItems.FMenu := Self;
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

  Creates the handle ( = object).
 ------------------------------------------------------------------------------}
procedure TMenu.SetParent(const AValue: TComponent);
begin
  // ToDo
  FParent:=AValue;
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
  InterfaceObject.IntSendMessage3(LM_CREATE, Self, nil); // CreateComponent(nil);
  // initiate creation of subitems
  // Note: FItems is a MenuItem, by using Createhandle all subitems will be
  //       created.
  for i:=0 to Items.Count-1 do
    Items[i].HandleNeeded;
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
