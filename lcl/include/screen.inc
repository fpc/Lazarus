{******************************************************************************
                                  TScreen
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
  Method: TScreen.Create
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TScreen.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FFormList := TList.Create;
  FPixelsPerInch:= ScreenInfo.PixelsPerInchX;
  FHintFont := TFont.Create;
//  FHintFont.Name := 'courier';
  FHintFont.Style := [];
  FHintFont.Size := 12;
  FHintFont.Pitch := fpDefault;
  FSaveFocusedList := TList.Create;
end;

{------------------------------------------------------------------------------
  Method: TScreen.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
Destructor TScreen.Destroy;
begin
  FHintFont.Free;
  FHintFont:=nil;
  FFormList.Free;
  FFormList:=nil;
  FSaveFocusedList.Free;
  FSaveFocusedList:=nil;
  inherited Destroy;
end;
{

     FUNCTIONS



}


{------------------------------------------------------------------------------
  Function: TScreen.AddForm
  Params:   FForm: The form to be added
  Returns:  Nothing

  Do not use this procedure. This procedure is used by TScreen internally.
 ------------------------------------------------------------------------------}
procedure TScreen.AddForm(FForm: TCustomForm);
begin
  FFormList.Add(FForm);
end;


{------------------------------------------------------------------------------
  Function: TScreen.GetFormCount
  Params:   none
  Returns:  The count of forms. (TODO: discribe this better; my English is not perfect)

  Returns the count of forms. (TODO: discribe this better; my English is not perfect)
 ------------------------------------------------------------------------------}
Function TScreen.GetFormCount: Integer;
begin
  Result := FFormList.Count;
end;


{------------------------------------------------------------------------------
  Function: TScreen.GetForms
  Params:   IIndex: The index of the form
  Returns:  A form stored in FFormList

  This function is used by the Forms property.
 ------------------------------------------------------------------------------}
Function TScreen.GetForms(IIndex: Integer): TForm;
begin
  Result := TForm(FFormList.Items[IIndex]);
end;


{------------------------------------------------------------------------------
  Method:  TScreen.GetWidth
  Params:  none
  Returns: Screen Width

  returns the screen width
 ------------------------------------------------------------------------------}
Function TScreen.GetWidth : Integer;
begin
  Result := GetSystemMetrics(SM_CXSCREEN);
end;


{------------------------------------------------------------------------------
  Method:  TScreen.GetHeight
  Params:  none
  Returns: Screen Height

  Returns the Screen Height
 ------------------------------------------------------------------------------}
Function TScreen.GetHeight : Integer;
begin
  Result := GetSystemMetrics(SM_CYSCREEN);
end;

{------------------------------------------------------------------------------

  Function: TScreen.RemoveForm
  Params:   FForm: The form to be removed
  Returns:  Nothing

  Do not use this procedure. This procedure is used by TScreen internally.
 ------------------------------------------------------------------------------}
procedure TScreen.RemoveForm(FForm: TCustomForm);
begin
  FFormList.Remove(FForm);
end;


{------------------------------------------------------------------------------
  Function: TScreen.MyFunction
  Params:   AOwner: the owner of the class
  Returns:  String containing output from the function.

  Description of the function for the class.
 ------------------------------------------------------------------------------}
{function TScreen.MyFunction(AOwner: TComponent): String;
begin

end;}
