{******************************************************************************
                         All GNOME Winapi implementations.
                   Initial Revision  : Thu Oct 3 1:35:53 2002


  !! Keep alphabetical !!

 ******************************************************************************
 Implementation
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
{$IFOPT C-}
// Uncomment for local trace
//  {$C+}
//  {$DEFINE ASSERT_IS_ON}
{$EndIf}

Function TGnomeObject.LoadStockPixmap(StockID: longint) : HBitmap;
var
  Pixmap : PGDIObject;
  StockName : PChar;
begin
  Case StockID Of
    idButtonOk : StockName := GNOME_STOCK_BUTTON_OK;
    idButtonCancel : StockName := GNOME_STOCK_BUTTON_CANCEL;
    idButtonYes : StockName := GNOME_STOCK_BUTTON_YES;
    idButtonNo : StockName := GNOME_STOCK_BUTTON_NO;
    idButtonHelp : StockName := GNOME_STOCK_BUTTON_HELP;
    idButtonAbort : StockName := GNOME_STOCK_BUTTON_CANCEL;
    idButtonClose : StockName := GNOME_STOCK_PIXMAP_QUIT;
    else begin
      Result := inherited LoadStockPixmap(StockID);
      exit;
    end;
  end;
  Pixmap := NewGDIObject(gdiBitmap);
  With Pixmap^ do begin
    GDIBitmapType := gbPixmap;
    gnome_stock_pixmap_gdk(STOCKName, nil, @GDIPixmapObject, @GDIBitmapMaskObject);
  end;
  Result := HBitmap(Pixmap);
end;

//----------------------------------------------------------------------------//
//-------------------------TGnomeObject.RequestInput--------------------------//
Type
  PRequestInputObject = ^TRequestInputObject;
  TRequestInputObject = Record
    Finished : Boolean;
    NewValue : String;
  end;

procedure RequestInputFinishCallback(NewString:PChar; data: PRequestInputObject);cdecl;
var
  I, Len : Longint;
begin
  If Data <> nil then
    with Data^ do begin
      If NewString = nil then
        NewValue := ''
      else begin
        Len := StrLen(NewString);
        SetLength(NewValue, Len);
        For I := 0 to Len - 1 do
          NewValue[I + 1] := NewString[I];
      end;
      Finished := True;
    end;
end;

Function TGnomeObject.RequestInput(const InputCaption, InputPrompt : String;
  MaskInput : Boolean; var Value : String) : Boolean;
var
  MainWidget,
  RequestWidget : Pointer;
  RequestObject : TRequestInputObject;
begin
  Result := False;

  If (Application.MainForm <> nil) and
    (Application.MainForm.HandleAllocated)
  then
    MainWidget := Pointer(Application.MainForm.Handle);

  With RequestObject do begin
    Finished := False;
    NewValue := Value;
  end;

  RequestWidget := gnome_request_dialog(MaskInput, PChar(InputPrompt), PChar(Value), 256,
    TGnomeStringCallback(@RequestInputFinishCallback), @RequestObject, MainWidget);

  gtk_window_set_title(RequestWidget,PChar(InputCaption));

  If gnome_dialog_run_and_close(RequestWidget) = 0 then
    If RequestObject.Finished then begin
      Result := True;
      Value := RequestObject.NewValue;
    end;
end;

{$IfDef ASSERT_IS_ON}
  {$UNDEF ASSERT_IS_ON}
  {$C-}
{$EndIf}

{
  $Log$
  Revision 1.3  2002/10/11 16:00:39  lazarus
  AJ: made InputQuery Interface Dependant

  Revision 1.2  2002/10/10 13:29:08  lazarus
  AJ: added LoadStockPixmap routine & minor fixes to/for GNOMEInt

  
}
