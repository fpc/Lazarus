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
    idButtonAll : StockName := LAZARUS_STOCK_BUTTON_ALL;
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

{------------------------------------------------------------------------------
  Method: TGnomeObject.PromptUser
  Params:
         DialogCaption - Dialog Caption to use if is a Custom Dialog
         DialogMessage - Message/Error/Question to display
         DialogType - type of dialog (warning/error/question/inform/custom)
         Buttons - array of what Buttons to include

   Returns: the Button clicked, or -1 if window was closed

   this routine produces a dialog for the purpose of prompting the user to make
   a choice, aka a Dialog consisting of an icon, a message, and several buttons
   such as OK/Cancel Yes/No etc.. It can be used to display errors, warnings,
   or other information, or to ask questions.
 ------------------------------------------------------------------------------}
Function PromptUserWidget(const DialogCaption, DialogMessage : String;
  DialogType : longint; Buttons : PLongint; ButtonCount, DefaultIndex : Longint) : Pointer;
var
  BoxType : PChar;
  MainWidget : Pointer;
  MsgTitle : PChar;
  BTNArray : PPgChar;
  StockName : PgChar;
  I : Longint;
  TXTList : PGList;
  TXTLayout : PGnomeIconTextInfo;
  NewMessage : AnsiString;
begin
  If (Application.MainForm <> nil) and
    (Application.MainForm.HandleAllocated)
  then
    MainWidget := Pointer(Application.MainForm.Handle)
  else
    MainWidget := nil;
  Case DialogType of
    idDialogInfo :
      BoxType := GNOME_MESSAGE_BOX_INFO;
    idDialogWarning:
      BoxType := GNOME_MESSAGE_BOX_WARNING;
    idDialogError:
      BoxType := GNOME_MESSAGE_BOX_ERROR;
    idDialogConfirm:
      BoxType := GNOME_MESSAGE_BOX_QUESTION;
    else
      BoxType := GNOME_MESSAGE_BOX_GENERIC;
  end;
  If DialogCaption <> '' then
    MsgTitle := PChar(DialogCaption)
  else
    MsgTitle := nil;
    
  BTNArray := nil;
  ReallocMem(BTNArray, SizeOf(PgChar)*(ButtonCount + 1));
  For I := 0 to ButtonCount - 1 do begin
    Case Buttons[I] Of
      idButtonOk : StockName := GNOME_STOCK_BUTTON_OK;
      idButtonCancel : StockName := GNOME_STOCK_BUTTON_CANCEL;
      idButtonYes : StockName := GNOME_STOCK_BUTTON_YES;
      idButtonNo : StockName := GNOME_STOCK_BUTTON_NO;
      idButtonHelp : StockName := GNOME_STOCK_BUTTON_HELP;
      idButtonClose : StockName := GNOME_STOCK_BUTTON_CLOSE;
      idButtonAll : StockName := LAZARUS_STOCK_BUTTON_ALL;
      idButtonYesToAll : StockName := LAZARUS_STOCK_BUTTON_YESALL;
      idButtonNoToAll : StockName := LAZARUS_STOCK_BUTTON_NOALL;
      idButtonAbort : StockName := LAZARUS_STOCK_BUTTON_ABORT;
      idButtonRetry : StockName := LAZARUS_STOCK_BUTTON_RETRY;
      idButtonIgnore : StockName := LAZARUS_STOCK_BUTTON_IGNORE;
      else
        StockName := '';
    end;
    BTNArray[I] := StockName;
  end;
  BTNArray[ButtonCount] := nil;

  TXTLayout := gnome_icon_layout_text(PGDIObject(GetStockObject(SYSTEM_FONT))^.GDIFontObject,
    PgChar(DialogMessage), ' ', Screen.Width div 3, False);

  TXTList := TXTLayout^.Rows;
  While TXTList <> nil do begin
    If TXTList^.Data <> nil then
      NewMessage := NewMessage + AnsiString(PGnomeIconTextInfoRow(TXTList^.Data)^.thetext);
    TXTList := TXTList^.Next;
    If TXTList <> nil then
      If NewMessage[Length(NewMessage)] <> #10 then
        NewMessage := NewMessage + #10;
  end;
  
  NewMessage := Copy(NewMessage, 1, Length(NewMessage));

  gnome_icon_text_info_free(TXTLayout);

  Result := gnome_message_box_newv(PgChar(NewMessage), BoxType, BTNArray);

  If (DefaultIndex >= ButtonCount) or (DefaultIndex < 0)
  then
    DefaultIndex := 0;

  gnome_dialog_set_default(Result, DefaultIndex);
  gnome_dialog_grab_focus(Result, DefaultIndex);
  
  If MsgTitle <> nil then
    gtk_window_set_title(Result, PgChar(MsgTitle));

  If MainWidget <> nil then
    gnome_dialog_set_parent(Result, MainWidget);

  ReallocMem(BTNArray, 0);
end;

Function TGnomeObject.PromptUser(const DialogCaption, DialogMessage : String;
  DialogType : longint; Buttons : PLongint; ButtonCount,
  DefaultIndex, EscapeResult : Longint) : Longint;
var
  MsgBox : Pointer;
  MSGResult : Longint;
begin
  MsgBox := PromptUserWidget(DialogCaption, DialogMessage, DialogType, Buttons, ButtonCount, DefaultIndex);
  MSGResult := gnome_dialog_run_and_close(MsgBox);
  Case MSGResult of
    -1 : Result := EscapeResult;
    else
      Result := Buttons[MSGResult]
  end;
end;

{------------------------------------------------------------------------------
  Method: TGnomeObject.PromptUserAtXY
  Params:
         DialogCaption - Dialog Caption to use if is a Custom Dialog
         DialogMessage - Message/Error/Question to display
         DialogType - type of dialog (warning/error/question/inform/custom)
         Buttons - array of what Buttons to include
         X, Y - Position to display dialog at

   Returns: the Button clicked, or -1 if window was closed

   this routine produces a dialog, at a given position on screen, for the
   purpose of prompting the user to make a choice, aka a Dialog consisting of
   an icon, a message, and several buttons such as OK/Cancel Yes/No etc.. It
   can be used to display errors, warnings, or other information, or to ask
   questions.
 ------------------------------------------------------------------------------}
Function TGnomeObject.PromptUserAtXY(const DialogCaption, DialogMessage : String;
   DialogType : longint; Buttons : PLongint; ButtonCount, DefaultIndex, EscapeResult : Longint;
   X, Y : Longint) : Longint;
var
  MsgBox : Pointer;
  MSGResult : Longint;
begin
  MsgBox := PromptUserWidget(DialogCaption, DialogMessage, DialogType, Buttons, ButtonCount, DefaultIndex);
  gtk_widget_set_uposition(MsgBox, X, Y);
  MSGResult := gnome_dialog_run_and_close(MsgBox);
  Case MSGResult of
    -1 : Result := EscapeResult;
    else
      Result := Buttons[MSGResult]
  end;
end;

{------------------------------------------------------------------------------
  Method: TGnomeObject.RequestInput
  Params:
         InputCaption - Dialog Caption
         InputPrompt - caption of input label
         MaskInput - hide input(AKA Password)
         Value - default/return value

   Returns: If User clicked OK

   this routines produces a input dialog consisting of an Edit field,
   an Ok button, and a Cancel Button. If MaskInput is set, the Edit's
   text is hidden like in a Password prompt. The initial Value is used
   as the default value of the edit, and if result is true, is replaced
   with the new value the user has typed in(if any).
 ------------------------------------------------------------------------------}
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
  Revision 1.10  2002/10/23 14:36:53  lazarus
  AJ:Fixes to PromptUser;Switched ShowMessage* to use NotifyUser*;
     fixed TGraphicPropertyEditor for when Property is nil.

  Revision 1.9  2002/10/21 03:23:34  lazarus
  AJ: rearranged GTK init stuff for proper GNOME init & less duplication between interfaces

  Revision 1.8  2002/10/15 17:09:54  lazarus
  AJ: fixed GTK DrawText to use WordWrap, and add DT_EditControl

  Revision 1.7  2002/10/14 18:36:57  lazarus
  AJ: Improvements/Fixes to new PromptUser API

  Revision 1.6  2002/10/14 14:29:50  lazarus
  AJ: Improvements to TUpDown; Added TStaticText & GNOME DrawText

  Revision 1.5  2002/10/13 16:06:49  lazarus
  AJ: fixed  GNOME Prompt/Notify dialogs to wrap long messages.

  Revision 1.4  2002/10/12 16:36:40  lazarus
  AJ: added new QueryUser/NotifyUser

  Revision 1.3  2002/10/11 16:00:39  lazarus
  AJ: made InputQuery Interface Dependant

  Revision 1.2  2002/10/10 13:29:08  lazarus
  AJ: added LoadStockPixmap routine & minor fixes to/for GNOMEInt

  
}
