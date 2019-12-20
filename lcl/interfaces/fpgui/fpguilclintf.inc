{%MainUnit fpguiint.pp}

{******************************************************************************
                 All FPGUI interface communication implementations.
                   Initial Revision  : Sun Nov 23 23:53:53 2003


  !! Keep alphabetical !!

  Support routines go to gtkproc.pp

 ******************************************************************************
 Implementation
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

//##apiwiz##sps##   // Do not remove

const
  IdButtonToFPGuiStandardButton: array[idButtonOK..idButtonShield] of TfpgMsgDlgBtn = (
 { idButtonOk       } mbOK,
 { idButtonCancel   } mbCancel,
 { idButtonHelp     } mbHelp,
 { idButtonYes      } mbYes,
 { idButtonNo       } mbNo,
 { idButtonClose    } mbClose,
 { idButtonAbort    } mbAbort,
 { idButtonRetry    } mbRetry,
 { idButtonIgnore   } mbIgnore,
 { idButtonAll      } mbAll,
 { idButtonYesToAll } mbYesToAll,
 { idButtonNoToAll  } mbNoToAll,
 { idButtonOpen     } mbNoButton,
 { idButtonSave     } mbNoButton,
 { idButtonShield   } mbNoButton
  );


function TFpGuiWidgetSet.PromptUser(const DialogCaption, DialogMessage: String;
  DialogType: longint; Buttons: PLongint; ButtonCount, DefaultIndex,
  EscapeResult: Longint): Longint;
var
  btns: TfpgMsgDlgButtons;
  BtnIdx, BtnID: LongInt;
  ResultBtn: TfpgMsgDlgBtn;

  function ResponseMappingfpGUItoLCL(const AResultBtn: TfpgMsgDlgBtn): Integer;
  begin
    { LCL is so confusing with modal results, button types, button results and
      Integer/LongInt as possible return results in various location is the
      LCL. I based the following mapping on trial-and-error with LCL-GTK2, and
      a best guess. }
    case AResultBtn of
      mbNoButton:   Result := idButtonBase; // ???
      mbOK:         Result := idButtonOK;
      mbCancel:     Result := idButtonCancel;
      mbYes:        Result := idButtonYes;
      mbNo:         Result := idButtonNo;
      mbAbort:      Result := idButtonAbort;
      mbRetry:      Result := idButtonRetry;
      mbIgnore:     Result := idButtonIgnore;
      mbAll:        Result := idButtonAll;
      mbNoToAll:    Result := idButtonNoToAll;
      mbYesToAll:   Result := idButtonYesToAll;
      mbHelp:       Result := idButtonHelp;
      mbClose:      Result := idButtonClose;
    end;
  end;

begin
  ResultBtn := mbOK;
  btns := [];
  for BtnIdx := 0 to ButtonCount-1 do
  begin
    BtnID := Buttons[BtnIdx];
    if (BtnID >= Low(IdButtonToFPGuiStandardButton)) and
       (BtnID <= High(IdButtonToFPGuiStandardButton)) and
       (IdButtonToFPGuiStandardButton[BtnID] <> mbNoButton) then
      Include(btns, IdButtonToFPGuiStandardButton[BtnID])
    else
      btns := [mbOK];
  end;

  case DialogType of
    idDialogWarning: ResultBtn := TfpgMessageDialog.Warning(DialogCaption, DialogMessage, btns);
    idDialogError: ResultBtn := TfpgMessageDialog.Critical(DialogCaption, DialogMessage, btns);
    idDialogInfo: ResultBtn :=TfpgMessageDialog.Information(DialogCaption, DialogMessage, btns);
    idDialogConfirm: ResultBtn := TfpgMessageDialog.Question(DialogCaption, DialogMessage, btns);
    else
    begin
      fpg_dialogs.ShowMessage(DialogMessage, DialogCaption);
    end;
  end;

  Result := ResponseMappingfpGUItoLCL(ResultBtn);
end;

{------------------------------------------------------------------------------
  Function: RawImage_QueryDescription
  Params: AFlags:
          ADesc:
  Returns:

 ------------------------------------------------------------------------------}
//function TFpguiWidgetSet.RawImage_QueryDescription(AFlags: TRawImageQueryFlags; var ADesc: TRawImageDescription): Boolean;
//begin
//  // override only when queried formats are different from screen description
//end;

//##apiwiz##eps##   // Do not remove, no wizard declaration after this line

