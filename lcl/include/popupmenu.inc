{%MainUnit ../menu.pp}
{******************************************************************************
                                  TPopupMenu
 ******************************************************************************
 
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

{------------------------------------------------------------------------------
  procedure TPopupMenu.DoPopup(Sender: TObject);

  Creates the popup window and shows it.
 ------------------------------------------------------------------------------}
procedure TPopupMenu.DoPopup(Sender: TObject);
begin
  if Assigned(FOnPopup) then FOnPopup(Sender);
end;

{------------------------------------------------------------------------------
  Method: TPopupMenu.Create
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TPopupMenu.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FCompStyle := csPopupMenu; 
  FAutoPopup := True;
end;

destructor TPopupMenu.Destroy;
begin
  Close;
  inherited Destroy;
end;

procedure TPopupMenu.PopUp(X,Y : Integer);
begin
  if ActivePopupMenu<>nil then ActivePopupMenu.Close;
  FPopupPoint := Point(X, Y);
  ReleaseCapture;
  DoPopup(Self);
  if Items.Count=0 then exit;
  ActivePopupMenu:=Self;
  Items.InitiateActions;
  HandleNeeded;
  if Assigned(OnMenuPopupHandler) then OnMenuPopupHandler(Self);
  TWSPopupMenuClass(WidgetSetClass).Popup(Self, X, Y);
end;

procedure TPopupMenu.Close;
begin
  if HandleAllocated then
    FItems.DestroyHandle;
  if ActivePopupMenu = Self then
  begin
    DoClose;
    ActivePopupMenu := nil;
  end;
end;

procedure TPopupMenu.DoClose;
begin
  if Assigned(OnClose) then OnClose(Self);
end;


