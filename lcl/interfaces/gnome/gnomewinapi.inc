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

function TGnomeObject.DrawText(DC: HDC; Str: PChar; Count: Integer; var Rect: TRect; Flags: Cardinal): Integer;
var
  TM : TTextmetric;
  theRect     : TRect;
  Lines : PGNOMEIconTextInfo;
  Line : PgList;
  LineInfo : TGnomeIconTextInfoRow;
  TempDC,
  TempPen,
  TempBrush : Longint;

  Function LeftOffset : Longint;
  begin
    If (Flags and DT_Right) = DT_Right then
      Result := DT_Right
    else
      If (Flags and DT_CENTER) = DT_CENTER then
        Result := DT_CENTER
    else
      Result := DT_LEFT;
  end;

  Function TopOffset : Longint;
  begin
    If (Flags and DT_BOTTOM) = DT_BOTTOM then
      Result := DT_BOTTOM
    else
      If (Flags and DT_VCENTER) = DT_VCENTER then
        Result := DT_VCENTER
    else
      Result := DT_Top;
  end;

  Function CalcRect : Boolean;
  begin
    Result := (Flags and DT_CalcRect) = DT_CalcRect;
  end;

  Procedure DoCalcRect;
  var
    AP : TSize;
    MaxLength : Integer;
    Width, Height : Longint;
  begin
    theRect := Rect;
    
    MaxLength := theRect.Right - theRect.Left;

    If (Flags and DT_SingleLine) = DT_SingleLine then begin
      GetTextExtentPoint(DC, Str, Count, AP);
      theRect.Right := theRect.Left + Min(MaxLength, AP.cX);
      theRect.Bottom := theRect.Top + TM.tmHeight;

      If not CalcRect then
        Case TopOffset of
          DT_VCENTER :
            OffsetRect(theRect, 0, (Rect.Bottom - theRect.Bottom) div 2);
          DT_Bottom :
            OffsetRect(theRect, 0, Rect.Bottom - theRect.Bottom);
        end;
    end
    else begin
      If (Flags and DT_WordBreak) <> DT_WordBreak then
        MaxLength := Count*TM.tmMaxCharWidth;

      {Use GNOME to Calculate Text Layout}
      Lines := gnome_icon_layout_text(PGDIObject(TDeviceContext(DC).CurrentFont)^.GDIFontObject,
        PgChar(Str), ' ', MaxLength, False);

      If Lines = nil then
        exit;

      theRect.Right := theRect.Left + Lines^.Width;
      theRect.Bottom := theRect.Top + Lines^.Height;
    end;

    If not CalcRect then
      Case LeftOffset of
        DT_CENTER :
          OffsetRect(theRect, (Rect.Right - theRect.Right) div 2, 0);
        DT_Right :
          OffsetRect(theRect, Rect.Right - theRect.Right, 0);
      end;
  end;

  Procedure DrawLine(theLine : PChar; LineLength, TopPos : Longint);
  var
    Points : Array[0..1] of TSize;
    LogP : TLogPen;
    pIndex : Longint;
    AStr : String;
    LeftPos : Longint;
  begin
    AStr := Copy(String(theLine), 1, LineLength);

    If (Flags and DT_NoPrefix) <> DT_NoPrefix then
      pIndex := DeleteAmpersands(aStr)
    else
      pIndex := -1;

    If TempBrush = -1 then
      TempBrush := SelectObject(DC, GetStockObject(NULL_BRUSH));
      
    If LeftOffset <> DT_Left then
      GetTextExtentPoint(DC, PChar(aStr), Length(aStr), Points[0]);

    Case LeftOffset of
      DT_Left:
        LeftPos := theRect.Left;
      DT_Center:
        LeftPos := theRect.Left + (theRect.Right - theRect.Left) div 2
                 - Points[0].cX div 2;
      DT_Right:
        LeftPos := theRect.Right - Points[0].cX;
    end;

    {Draw line of Text}
    TextOut(DC, LeftPos, TopPos, PChar(aStr), Length(aStr));

    {Draw Prefix}
    If pIndex > 0 then begin
      {Create & select pen of font color}
      If TempPen = -1 then begin
        LogP.lopnStyle := PS_SOLID;
        LogP.lopnWidth.X := 1;
        LogP.lopnColor := GetTextColor(DC);
        TempPen := SelectObject(DC, CreatePenIndirect(LogP));
      end;

      {Get prefix line position}
      GetTextExtentPoint(DC, PChar(aStr), pIndex - 1, Points[0]);
      Points[0].cX := LeftPos + Points[0].cX;
      Points[0].cY := TopPos + tm.tmHeight - TM.tmDescent + 1;

      GetTextExtentPoint(DC, @aStr[pIndex], 1, Points[1]);
      Points[1].cX := Points[0].cX + Points[1].cX;
      Points[1].cY := Points[0].cY;

      {Draw prefix line}
      Polyline(DC, @Points[0], 2);
    end;
  end;
  
begin
  Result := 0;
  Lines := nil;
  TempDC := -1;
  TempPen := -1;
  TempBrush := -1;

  Count := Min(StrLen(Str), Count);

  GetTextMetrics(DC, TM);

  DoCalcRect;

  If (Flags and DT_CalcRect) <> DT_CalcRect then begin
    TempDC := SaveDC(DC);

    If (Flags and DT_NOCLIP) <> DT_NOCLIP then begin
      If theRect.Right > Rect.Right then
        theRect.Right := Rect.Right;
      If theRect.Bottom > Rect.Bottom then
        theRect.Bottom := Rect.Bottom;
      IntersectClipRect(DC, theRect.Left, theRect.Top,
        theRect.Right, theRect.Bottom);
    end;

    If (Flags and DT_SingleLine) = DT_SingleLine then begin
      DrawLine(Str, Count, theRect.Top);
      Result := 1;
    end
    else
      If Lines <> nil then begin
        Line := Lines^.Rows;
        While Line <> nil do begin
          If Line^.Data <> nil then begin
            LineInfo := PGnomeIconTextInfoRow(Line^.Data)^;
            DrawLine(LineInfo.theText, LineInfo.Text_Length, theRect.Top);
            Inc(theRect.Top, TM.tmHeight);
          end;
          Line := Line^.Next;
          If theRect.Top > theRect.Bottom then
            Break;
        end;
        Result := 1;
      end;
  end
  else begin
    CopyRect(Rect, theRect);
    Result := 1;
  end;

  If Lines <> nil then
    gnome_icon_text_info_free(Lines);

  If TempBrush <> -1 then
    SelectObject(DC, TempBrush);

  If TempPen <> -1 then
    DeleteObject(SelectObject(DC, TempPen));

  If TempDC <> -1 then
    RestoreDC(DC, TempDC);
end;

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
  DialogType : longint; Buttons : Array of Longint) : Pointer;
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
  MsgTitle := nil;
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
    else begin
      BoxType := GNOME_MESSAGE_BOX_GENERIC;
      MsgTitle := PChar(DialogCaption);
    end;
  end;
  BTNArray := nil;
  ReallocMem(BTNArray, SizeOf(PgChar)*(High(Buttons) - Low(Buttons) + 2));
  For I := Low(Buttons) to High(Buttons) do begin
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
    BTNArray[I - Low(Buttons)] := StockName;
  end;
  BTNArray[High(Buttons) - Low(Buttons) + 1] := nil;

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

  If MsgTitle <> nil then
    gtk_window_set_title(Result, PgChar(MsgTitle));

  If MainWidget <> nil then
    gnome_dialog_set_parent(Result, MainWidget);

  ReallocMem(BTNArray, 0);
end;

Function TGnomeObject.PromptUser(const DialogCaption, DialogMessage : String;
  DialogType : longint; Buttons : Array of Longint) : Longint;
var
  MsgBox : Pointer;
  MSGResult : Longint;
begin
  MsgBox := PromptUserWidget(DialogCaption, DialogMessage, DialogType, Buttons);
  MSGResult := gnome_dialog_run_and_close(MsgBox);
  Case MSGResult of
    -1 : Result := -1;
    else
      Result := Buttons[MSGResult + Low(Buttons)]
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
   DialogType : longint; Buttons : Array of Longint; X, Y : Longint) : Longint;
var
  MsgBox : Pointer;
  MSGResult : Longint;
begin
  MsgBox := PromptUserWidget(DialogCaption, DialogMessage, DialogType, Buttons);
  gtk_widget_set_uposition(MsgBox, X, Y);
  MSGResult := gnome_dialog_run_and_close(MsgBox);
  Case MSGResult of
    -1 : Result := -1;
    else
      Result := Buttons[MSGResult + Low(Buttons)]
  end;
end;

{------------------------------------------------------------------------------
  Method: TGnomeObject.NotifyUser
  Params:
         DialogCaption - Dialog Caption to use if is a Custom Dialog
         DialogMessage - Message/Error/Question to display
         DialogType - type of dialog (warning/error/question/inform/custom)

   Returns: Nothing

   this routines produces a notification dialog, aka a Dialog consisting of
   an icon, a message, and an Ok Button. It really only makes sense to use
   with an Error, Info or Custom Dialog Type.
 ------------------------------------------------------------------------------}
Procedure TGnomeObject.NotifyUser(const DialogCaption, DialogMessage : String; DialogType : longint);
var
  MsgBox : Pointer;
begin
  MsgBox := PromptUserWidget(DialogCaption, DialogMessage, DialogType, [idButtonOk]);
  gnome_dialog_run_and_close(MsgBox);
end;

{------------------------------------------------------------------------------
  Method: TGnomeObject.NotifyUserAtXY
  Params:
         DialogCaption - Dialog Caption to use if is a Custom Dialog
         DialogMessage - Message/Error/Question to display
         DialogType - type of dialog (warning/error/question/inform/custom)
         X, Y - Position to display dialog at

   Returns: Nothing

   this routines produces a notification dialog at a given position on screen,
   aka a Dialog consisting of an icon, a message, and an Ok Button. It really
   only makes sense to use with an Error, Info or Custom Dialog Type.
 ------------------------------------------------------------------------------}
Procedure TGnomeObject.NotifyUserAtXY(const DialogCaption, DialogMessage : String; DialogType : longint; X, Y : Longint);
var
  MsgBox : Pointer;
begin
  MsgBox := PromptUserWidget(DialogCaption, DialogMessage, DialogType, [idButtonOk]);
  gtk_widget_set_uposition(MsgBox, X, Y);
  gnome_dialog_run_and_close(MsgBox);
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
