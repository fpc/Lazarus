// included by dialogs.pp

{******************************************************************************
                                  MessageDialogs
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
  current design flaws:

  - ??? There has to be at least one :-)

  Delphi compatibility:

   - the interface is almost like in delphi 5
     
  TODO:
    - Help Context
    - Help-button
    - User ability to customize Button order

}
Function ModalEscapeValue(Buttons : TMsgDlgButtons) : TModalResult;
begin
  If mbNo in Buttons then
    Result := mrNo
  else
    If mbAbort in Buttons then
      Result := mrAbort
  else
    If mbCancel in Buttons then
      Result := mrCancel
  else
    If mbIgnore in Buttons then
      Result := mrIgnore
  else
    If mbNoToAll in Buttons then
      Result := mrNoToAll
  else
    If mbYes in Buttons then
      Result := mrYes
  else
    If mbOk in Buttons then
      Result := mrOk
  else
    If mbRetry in Buttons then
      Result := mrRetry
  else
    If mbAll in Buttons then
      Result := mrAll
  else
    If mbYesToAll in Buttons then
      Result := mrYesToAll;
end;

Function ModalDefaultButton(Buttons : TMsgDlgButtons) : TMsgDlgbtn;
begin
  If mbYes in Buttons then
    Result := mbYes
  else
    If mbOk in Buttons then
      Result := mbOk
  else
    If mbYesToAll in Buttons then
      Result := mbYesToAll
  else
    If mbAll in Buttons then
      Result := mbAll
  else
    If mbRetry in Buttons then
      Result := mbRetry
  else
    If mbCancel in Buttons then
      Result := mbCancel
  else
    If mbNo in Buttons then
      Result := mbNo
  else
    If mbNoToAll in Buttons then
      Result := mbNoToAll
  else
    If mbAbort in Buttons then
      Result := mbAbort
  else
    If mbIgnore in Buttons then
      Result := mbIgnore;
end;

const
  DialogIds : Array[mtWarning..mtCustom] of Longint = (idDialogWarning,
    idDialogError, idDialogInfo, idDialogConfirm, idDialogBase);

  ButtonIds : Array[TMsgDlgbtn] of Longint = (idButtonYes, idButtonNo,
    idButtonOK, idButtonCancel, idButtonAbort, idButtonRetry, idButtonIgnore,
    idButtonAll, idButtonNoToAll, idButtonYesToAll, idButtonHelp);

  DialogResults : Array[idButtonOK..idButtonNoToAll] of TModalResult = (
    mrOk, mrCancel, mrOk{CLOSE!!}, mrYes, mrNo, -1{HELP!!}, mrAbort, mrRetry,
    mrIgnore, mrAll, mrYesToAll, mrNoToAll);

  ButtonResults : Array[mrNone..mrYesToAll] of Longint = (
    -1, idButtonOK, idButtonCancel, idButtonAbort, idButtonRetry,
    idButtonIgnore, idButtonYes,idButtonNo, idButtonAll, idButtonNoToAll,
    idButtonYesToAll);

Function GetPromptUserButtons(Buttons: TMsgDlgButtons; var CancelValue,
  DefaultIndex, ButtonCount : Longint) : PLongint;
var
  CurBtn      : TMsgDlgBtn; // variable to loop through TMsgDlgButtons
  DefaultButton : TMsgDlgBtn;
begin
  If (Buttons = []) or (Buttons = [mbHelp]) then
    Buttons := Buttons + [mbOk];
  CancelValue := ButtonResults[ModalEscapeValue (Buttons)];
  DefaultButton := ModalDefaultButton(Buttons);
  DefaultIndex := 0;
  ButtonCount := 0;
  Result := nil;
  For CurBtn := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do begin
    If CurBtn in Buttons then begin
      ReallocMem(Result, (ButtonCount + 1)*SizeOf(Longint));
      Result[ButtonCount] := ButtonIds[CurBtn];
      If DefaultButton = CurBtn then
        DefaultIndex := ButtonCount;
      Inc(ButtonCount)
    end;
  end;
end;

function MessageDlg(const aMsg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer;
var
  DefaultIndex,
  CancelValue,
  ButtonCount : Longint;
  Btns : PLongint;
begin
  Btns := GetPromptUserButtons(Buttons, CancelValue, DefaultIndex, ButtonCount);
  Result := DialogResults[PromptUser(aMsg, DialogIds[DlgType], Btns, ButtonCount,
    DefaultIndex, CancelValue)];
  ReallocMem(Btns, 0);
end;

function MessageDlg(const aCaption, aMsg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer;
var
  DefaultIndex,
  CancelValue,
  ButtonCount : Longint;
  Btns : PLongint;
begin
  Btns := GetPromptUserButtons(Buttons, CancelValue, DefaultIndex, ButtonCount);
  Result := DialogResults[PromptUser(aCaption, aMsg, DialogIds[DlgType], Btns,
    ButtonCount, DefaultIndex, CancelValue)];
  ReallocMem(Btns, 0);
end;

function MessageDlgPos(const aMsg: String; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; Helpctx : Longint; X,Y : Integer): Integer;
var
  DefaultIndex,
  CancelValue,
  ButtonCount : Longint;
  Btns : PLongint;
begin
  Btns := GetPromptUserButtons(Buttons, CancelValue, DefaultIndex, ButtonCount);
  Result := DialogResults[PromptUserAtXY(aMsg, DialogIds[DlgType], Btns,
    ButtonCount, DefaultIndex, CancelValue, X, Y)];
  ReallocMem(Btns, 0);
end;

function MessageDlgPosHelp(const aMsg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer;
  const HelpFileName: string): Integer;
begin
  writeln ('MessageDlgPosHelp ****** NOT YET FULLY IMPLEMENTED ********');
//TODO: set helpcontext and helpfile
  result := MessageDlgPos(aMsg, DlgType, buttons, helpctx, X, Y);
end;

procedure ShowMessage(const aMsg: string);
begin
  NotifyUser(aMsg, idDialogBase);
end;

procedure ShowMessageFmt(const aMsg: string; Params: array of const);
begin
  NotifyUser(Format(aMsg, Params), idDialogBase);
end;

procedure ShowMessagePos(const aMsg: string; X, Y: Integer);
begin
  NotifyUserAtXY(aMsg, idDialogBase, X, Y);
end;

//----------------------------------------------------------------------------//
//-----------------------Prompt User For Information--------------------------//
Function InputBox(const ACaption, APrompt, ADefault : String) : String;
begin
  Result := ADefault;
  InputQuery(ACaption, APrompt, Result);
end;

Function PasswordBox(const ACaption, APrompt : String) : String;
begin
  Result := '';
  InputQuery(ACaption, APrompt, True, Result);
end;

Function InputQuery(const ACaption, APrompt : String; MaskInput : Boolean;
  var Value : String) : Boolean;
begin
  Result := LCLLinux.RequestInput(ACaption, APrompt, MaskInput, Value);
end;

Function InputQuery(const ACaption, APrompt : String; var Value : String) : Boolean;
begin
  Result := InputQuery(ACaption, APrompt, False, Value);
end;

// included by dialogs.pp

{
   $Log$
   Revision 1.21  2002/10/25 14:59:11  lazarus
   AJ: MessageDlgs -> PromptUser, better Cancel/Default order

   Revision 1.20  2002/10/25 10:06:34  lazarus
   MG: broke interfacebase uses circles

   Revision 1.19  2002/10/25 09:47:38  lazarus
   MG: added inputdialog.inc

   Revision 1.18  2002/10/23 20:47:26  lazarus
   AJ: Started Form Scrolling
       Started StaticText FocusControl
       Fixed Misc Dialog Problems
       Added TApplication.Title

   Revision 1.17  2002/10/23 14:36:52  lazarus
   AJ:Fixes to PromptUser;Switched ShowMessage* to use NotifyUser*;
      fixed TGraphicPropertyEditor for when Property is nil.

   Revision 1.16  2002/10/11 16:00:39  lazarus
   AJ: made InputQuery Interface Dependant

   Revision 1.15  2002/10/10 13:29:08  lazarus
   AJ: added LoadStockPixmap routine & minor fixes to/for GNOMEInt

   Revision 1.14  2002/09/27 20:52:23  lazarus
   MWE: Applied patch from "Andrew Johnson" <aj_genius@hotmail.com>

   Here is the run down of what it includes -

    -Vasily Volchenko's Updated Russian Localizations

    -improvements to GTK Styles/SysColors
    -initial GTK Palette code - (untested, and for now useless)

    -Hint Windows and Modal dialogs now try to stay transient to
     the main program form, aka they stay on top of the main form
     and usually minimize/maximize with it.

    -fixes to Form BorderStyle code(tool windows needed a border)

    -fixes DrawFrameControl DFCS_BUTTONPUSH to match Win32 better
     when flat

    -fixes DrawFrameControl DFCS_BUTTONCHECK to match Win32 better
     and to match GTK theme better. It works most of the time now,
     but some themes, noteably Default, don't work.

    -fixes bug in Bitmap code which broke compiling in NoGDKPixbuf
     mode.

    -misc other cleanups/ fixes in gtk interface

    -speedbutton's should now draw correctly when flat in Win32

    -I have included an experimental new CheckBox(disabled by
     default) which has initial support for cbGrayed(Tri-State),
     and WordWrap, and misc other improvements. It is not done, it
     is mostly a quick hack to test DrawFrameControl
     DFCS_BUTTONCHECK, however it offers many improvements which
     can be seen in cbsCheck/cbsCrissCross (aka non-themed) state.

    -fixes Message Dialogs to more accurately determine
     button Spacing/Size, and Label Spacing/Size based on current
     System font.
    -fixes MessageDlgPos, & ShowMessagePos in Dialogs
    -adds InputQuery & InputBox to Dialogs

    -re-arranges & somewhat re-designs Control Tabbing, it now
     partially works - wrapping around doesn't work, and
     subcontrols(Panels & Children, etc) don't work. TabOrder now
     works to an extent. I am not sure what is wrong with my code,
     based on my other tests at least wrapping and TabOrder SHOULD
     work properly, but.. Anyone want to try and fix?

    -SynEdit(Code Editor) now changes mouse cursor to match
     position(aka over scrollbar/gutter vs over text edit)

    -adds a TRegion property to Graphics.pp, and Canvas. Once I
     figure out how to handle complex regions(aka polygons) data
     properly I will add Region functions to the canvas itself
     (SetClipRect, intersectClipRect etc.)

    -BitBtn now has a Stored flag on Glyph so it doesn't store to
     lfm/lrs if Glyph is Empty, or if Glyph is not bkCustom(aka
     bkOk, bkCancel, etc.) This should fix most crashes with older
     GDKPixbuf libs.

   Revision 1.13  2002/09/03 20:02:01  lazarus
   Intermediate UI patch to show a bug.

   Revision 1.12  2002/08/30 10:06:07  lazarus
   Fixed alignment of multiline TLabel.
   Simplified and prettified MessageBoxen.

   Revision 1.11  2002/07/29 13:39:07  lazarus
   MG: removed ambigious TBitmap from LCLType and added Escape key to MessageDlgs

   Revision 1.10  2002/06/06 07:23:24  lazarus
   MG: small fixes to reduce form repositioing

   Revision 1.9  2002/05/10 06:05:53  lazarus
   MG: changed license to LGPL

   Revision 1.8  2001/12/10 22:39:37  lazarus
   MG: added perl highlighter

   Revision 1.7  2001/10/16 10:51:10  lazarus
   MG: added clicked event to TButton, MessageDialog reacts to return key

   Revision 1.6  2001/10/07 07:35:28  lazarus
   MG: minor fix

   Revision 1.4  2001/07/31 18:40:24  lazarus
   MG: added unit info, arrow xpms, and many changes from jens arm

   Revision 1.3  2001/06/14 14:57:58  lazarus
   MG: small bugfixes and less notes

   Revision 1.1  2001/03/03 00:50:34  lazarus
   + added support for message dialogs (messagedialogs.inc)
   + added some pixmaps for message dialogs(messagedialogpixmaps.inc)
   stoppok

}   
