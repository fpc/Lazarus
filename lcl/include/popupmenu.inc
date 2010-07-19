{%MainUnit ../menu.pp}
{******************************************************************************
                                  TPopupMenu
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
}

function TPopupMenu.GetHelpContext: THelpContext;
begin
  Result := Items.HelpContext;
end;

procedure TPopupMenu.SetHelpContext(const AValue: THelpContext);
begin
  Items.HelpContext := AValue;
end;

class procedure TPopupMenu.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterPopupMenu;
end;

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
  DestroyHandle;
  Close;
  inherited Destroy;
end;

procedure TPopupMenu.PopUp;
var
  MousePos: TPoint;
begin
  GetCursorPos(MousePos);
  PopUp(MousePos.x, MousePos.y);
end;

procedure TPopupMenu.PopUp(X, Y: Integer);
begin
  if ActivePopupMenu <> nil then ActivePopupMenu.Close;
  FPopupPoint := Point(X, Y);
  ReleaseCapture;
  DoPopup(Self);
  if Items.Count = 0 then exit;
  ActivePopupMenu := Self;
  Items.InitiateActions;
  DestroyHandle;
  CreateHandle;
  if Assigned(OnMenuPopupHandler) then OnMenuPopupHandler(Self);
  TWSPopupMenuClass(WidgetSetClass).Popup(Self, X, Y);
end;

procedure TPopupMenu.Close;
begin
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


