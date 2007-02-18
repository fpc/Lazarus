{%MainUnit qtint.pp}
{ $Id$ }

{******************************************************************************
                  All QT interface support routines
                   Initial Revision  : Sat Jan 17 19:00:00 2004


  !! Keep alphabetical !!

  Support routines go to qtproc.pp

 ******************************************************************************
 Implementation
 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

//##apiwiz##sps##   // Do not remove

procedure TQtWidgetSet.FillStandardDescription(var Desc: TRawImageDescription);
begin
  FillChar(Desc, SizeOf(Desc), 0);

  Desc.Format := ricfRGBA;
  Desc.HasPalette := False;
//  Desc.Depth := 32;
  // Width and Height not relevant
  Desc.PaletteColorCount := 0;
  Desc.BitOrder := riboReversedBits;
  Desc.ByteOrder := riboLSBFirst;
  Desc.LineOrder := riloTopToBottom;
  Desc.ColorCount := Desc.PaletteColorCount;
  Desc.BitsPerPixel := 32;
  Desc.LineEnd := rileDWordBoundary;

  // 0-8-8-8 mode, high byte is not used
  Desc.RedPrec := 8;
  Desc.GreenPrec := 8;
  Desc.BluePrec := 8;
  Desc.RedShift := 16;
  Desc.GreenShift := 8;
  Desc.BlueShift := 0;
  Desc.Depth := 24;

  Desc.AlphaPrec := 0;
  Desc.AlphaSeparate := False;
  // Qt wants dword-aligned data
  Desc.AlphaLineEnd := rileDWordBoundary;
  Desc.AlphaShift := 0;
end;

function TQtWidgetSet.PromptUser(const DialogCaption : string;
                                 const DialogMessage : string;
                                       DialogType    : LongInt;
                                       Buttons       : PLongInt;
                                       ButtonCount   : LongInt;
                                       DefaultIndex  : LongInt;
                                       EscapeResult  : LongInt) : LongInt;
 {Note: Qt appears to map Esc key to Cancel button, so no need for EscapeResult.}
var
  QBtns      : QMessageBoxStandardButtons;
  DefQBtn    : QMessageBoxStandardButton;
  CurQBtn    : QMessageBoxStandardButton;
  BtnIdx     : LongInt;
  CaptionStr : WideString;
  MessageStr : WideString;
  QResult    : QMessageBoxStandardButton;
begin
  QBtns := 0;
  DefQBtn := QMessageBoxNoButton;
   {Convert LCL "id" button values to Qt values}
  for BtnIdx := 0 to ButtonCount-1 do
    begin
    case Buttons[BtnIdx] of
      idButtonYes      : CurQBtn := QMessageBoxYes;
      idButtonNo       : CurQBtn := QMessageBoxNo;
      idButtonOK       : CurQBtn := QMessageBoxOk;
      idButtonCancel   : CurQBtn := QMessageBoxCancel;
      idButtonAbort    : CurQBtn := QMessageBoxAbort;
      idButtonRetry    : CurQBtn := QMessageBoxRetry;
      idButtonIgnore   : CurQBtn := QMessageBoxIgnore;
//      idButtonAll      : no equivalent
      idButtonNoToAll  : CurQBtn := QMessageBoxNoToAll;
      idButtonYesToAll : CurQBtn := QMessageBoxYesToAll;
      idButtonHelp     : CurQBtn := QMessageBoxHelp;
      idButtonClose    : CurQBtn := QMessageBoxClose;
      else
        CurQBtn := QMessageBoxNoButton;
      end;
    QBtns := QBtns or CurQBtn;
    if BtnIdx = DefaultIndex then  {Default button?}
      DefQBtn := CurQBtn;
    end;

  CaptionStr := DialogCaption;
  MessageStr := DialogMessage;

  case DialogType of
    idDialogWarning : 
     QResult := QMessageBox_warning(nil, @CaptionStr, @MessageStr, QBtns, DefQBtn);
    idDialogError : 
     QResult := QMessageBox_critical(nil, @CaptionStr, @MessageStr, QBtns, DefQBtn);
    idDialogInfo : 
     QResult := QMessageBox_information(nil, @CaptionStr, @MessageStr, QBtns, DefQBtn);
    idDialogConfirm : 
     QResult := QMessageBox_question(nil, @CaptionStr, @MessageStr, QBtns, DefQBtn);
    end;

   {Convert Qt result to LCL "id" dialog result}
  case QResult of
    QMessageBoxOk       : Result := idButtonOK;
    QMessageBoxCancel   : Result := idButtonCancel;
    QMessageBoxAbort    : Result := idButtonAbort;
    QMessageBoxRetry    : Result := idButtonRetry;
    QMessageBoxIgnore   : Result := idButtonIgnore;
    QMessageBoxYes      : Result := idButtonYes;
    QMessageBoxNo       : Result := idButtonNo;
//    QMessageBoxAll      : no equivalent
    QMessageBoxNoToAll  : Result := idButtonNoToAll;
    QMessageBoxYesToAll : Result := idButtonYesToAll;
    else
      Result := -1;
    end;
end;  {TQtWidgetSet.PromptUser}

//##apiwiz##eps##   // Do not remove, no wizard declaration after this line
