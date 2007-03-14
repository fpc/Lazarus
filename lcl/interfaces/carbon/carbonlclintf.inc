{%MainUnit carbonint.pas}

{ $Id$ }
{******************************************************************************
  All Carbon interface communication implementations.
  This are the implementation of the overrides of the Carbon Interface for the
  methods defined in the
  lcl/include/lclintf.inc


  !! Keep alphabetical !!

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

function TCarbonWidgetSet.CreateStandardCursor(ACursor: SmallInt): hCursor;
var
  AThemeCursor: ThemeCursor;
begin
  Result := 0;
  if (ACursor >= crLow) and (ACursor <= crHigh) then
  begin
    AThemeCursor := CursorToThemeCursor[TCursor(ACursor)];
    if AThemeCursor <> kThemeUndefCursor then
      Result := hCursor(TCarbonCursor.CreateThemed(AThemeCursor));
  end;
end;

function TCarbonWidgetSet.DrawSplitter(DC: HDC; const ARect: TRect;
  Horizontal: boolean): boolean;
begin
  Result:=inherited DrawSplitter(DC, ARect, Horizontal);
end;

function TCarbonWidgetSet.ExtUTF8Out(DC: HDC; X, Y: Integer; Options: Longint;
  Rect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;
begin
  Result:=inherited ExtUTF8Out(DC, X, Y, Options, Rect, Str, Count, Dx);
end;

function TCarbonWidgetSet.FontCanUTF8(Font: HFont): boolean;
begin
  Result:=inherited FontCanUTF8(Font);
end;

function TCarbonWidgetSet.GetAcceleratorString(const AVKey: Byte;
  const AShiftState: TShiftState): String;
begin
  Result:=inherited GetAcceleratorString(AVKey, AShiftState);
end;

function TCarbonWidgetSet.GetControlConstraints(Constraints: TObject): boolean;
begin
  Result:=inherited GetControlConstraints(Constraints);
end;

function TCarbonWidgetSet.GetLCLOwnerObject(Handle: HWnd): TObject;
begin
  Result:=inherited GetLCLOwnerObject(Handle);
end;

function TCarbonWidgetSet.GetListBoxIndexAtY(ListBox: TComponent; y: integer
  ): integer;
begin
  Result:=inherited GetListBoxIndexAtY(ListBox, y);
end;

function TCarbonWidgetSet.GetListBoxItemRect(ListBox: TComponent;
  Index: integer; var ARect: TRect): boolean;
begin
  Result:=inherited GetListBoxItemRect(ListBox, Index, ARect);
end;

function TCarbonWidgetSet.IntfSendsUTF8KeyPress: boolean;
begin
  Result:=true;
end;

function TCarbonWidgetSet.PromptUser(const DialogCaption : string;
                                     const DialogMessage : string;
                                           DialogType    : LongInt;
                                           Buttons       : PLongInt;
                                           ButtonCount   : LongInt;
                                           DefaultIndex  : LongInt;
                                           EscapeResult  : LongInt) : LongInt;
 {Implements MessageDlg.
  Since Carbon's standard alert box only supports 3 buttons (plus optional
  help button), this method currently supports only these combinations of
  buttons: mbOK, mbOKCancel, mbYesNo and mbYesNoCancel.
  Possible TODO: mbAbortRetryIgnore, although this is not normally used on OS X.
  Note that alert's help button is not supported at this time since no help context
  is passed to this method.}

  function HasButton(BtnNum : LongInt) : Boolean;
   {Return True if specified LCL "id" value is in Buttons.}
  var
    BtnIdx : LongInt;
  begin
    Result := False;
    for BtnIdx := 0 to ButtonCount-1 do
    begin
      if Buttons[BtnIdx] = BtnNum then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;  {HasButton}

const
  OKKey     = 'OK';
  CancelKey = 'Cancel';
  YesKey    = 'Yes';
  NoKey     = 'No';
 {Note: Not using Pointer(kAlertDefaultOKText) or Pointer(kAlertDefaultCancelText)
  since this just passes in -1, which tells button to use its normal text and
  we need to override with Yes and No. If Localizable.strings file is in app 
  bundle's .lproj folder, will use localized strings for above keys if they
  are defined in .strings file.}
var
  ParamRec    : AlertStdCFStringAlertParamRec;
  OKStr       : CFStringRef;
  CancelStr   : CFStringRef;
  YesStr      : CFStringRef;
  NoStr       : CFStringRef;
  RightBtnID  : LongInt;
  MiddleBtnID : LongInt;
  LeftBtnID   : LongInt;
  CaptionStr  : CFStringRef;
  MessageStr  : CFStringRef;
  AlertCode   : AlertType;
  AlertRef    : DialogRef;
  AlertBtnIdx : DialogItemIndex;

begin
  Result := -1;

  {Initialize record}
  ParamRec.version := kStdCFStringAlertVersionOne;
  ParamRec.movable := True;
  ParamRec.helpButton := False;
  ParamRec.defaultText := nil;
  ParamRec.cancelText := nil;
  ParamRec.otherText := nil;
  ParamRec.defaultButton := kAlertStdAlertOKButton;  {Right button}
  ParamRec.cancelButton := 0;
  ParamRec.position := kWindowDefaultPosition;
  ParamRec.flags := 0; 

  {English defaults to use if no Localizable.strings translations to use}
  CreateCarbonString(OKKey, OKStr);
  CreateCarbonString(CancelKey, CancelStr);
  CreateCarbonString(YesKey, YesStr);
  CreateCarbonString(NoKey, NoStr);

  {Convert LCL "id" button values to Carbon values}
  if HasButton(idButtonOK) then
  begin  {Make OK the right button}
    ParamRec.defaultText := CFCopyLocalizedString(OKStr, nil);
    RightBtnID := idButtonOK;
  end;

  if HasButton(idButtonCancel) then
  begin  {Make Cancel the middle button}
    ParamRec.cancelText := CFCopyLocalizedString(CancelStr, nil);
    ParamRec.cancelButton := kAlertStdAlertCancelButton;
    MiddleBtnID := idButtonCancel;
  end;

  if not HasButton(idButtonOK) then  {No OK? check if other buttons}
  begin
    if HasButton(idButtonNo) then
    begin  {Make No the right button}
      begin
        ParamRec.defaultText := CFCopyLocalizedString(NoStr, nil);
        ParamRec.cancelButton := kAlertStdAlertOKButton;
        RightBtnID := idButtonNo;
      end;
    end;

    if HasButton(idButtonYes) then
    begin
      if HasButton(idButtonCancel) then
      begin  {Make Yes the left button}
        ParamRec.otherText := CFCopyLocalizedString(YesStr, nil);
        ParamRec.defaultButton := kAlertStdAlertOtherButton;
        LeftBtnID := idButtonYes;
      end
      else  {No Cancel button, so make Yes the middle button}
      begin
        ParamRec.cancelText := CFCopyLocalizedString(YesStr, nil);
        ParamRec.defaultButton := kAlertStdAlertCancelButton;
        MiddleBtnID := idButtonYes;
      end;
    end;
  end;

  CreateCarbonString(DialogCaption, CaptionStr);
  CreateCarbonString(DialogMessage, MessageStr);

  {Note: kAlertCautionAlert displays alert icon and app's icon.
         kAlertStopAlert and kAlertNoteAlert only display app's icon.
         kAlertPlainAlert doesn't display any icon.}
  case DialogType of
    idDialogWarning : AlertCode := kAlertCautionAlert;
    idDialogError   : AlertCode := kAlertCautionAlert;
    idDialogInfo    : AlertCode := kAlertNoteAlert;
    idDialogConfirm : AlertCode := kAlertNoteAlert;
  else
    AlertCode := kAlertNoteAlert;
  end;

  try
    DebugLn('TCarbonWidgetSet.PromptUser: CreateStandardAlert');
    CreateStandardAlert(AlertCode, CaptionStr, MessageStr, @ParamRec, AlertRef);
    RunStandardAlert(AlertRef, nil, AlertBtnIdx);

    {Convert Carbon result to LCL "id" dialog result}
    case AlertBtnIdx of
      kAlertStdAlertOKButton     : Result := RightBtnID;
      kAlertStdAlertCancelButton : Result := MiddleBtnID;
      kAlertStdAlertOtherButton  : Result := LeftBtnID;
    end;

  finally
    FreeCarbonString(OKStr);
    FreeCarbonString(CancelStr);
    FreeCarbonString(YesStr);
    FreeCarbonString(NoStr);
    FreeCarbonString(ParamRec.defaultText);
    FreeCarbonString(ParamRec.cancelText);
    FreeCarbonString(ParamRec.otherText);
    FreeCarbonString(CaptionStr);
    FreeCarbonString(MessageStr);
  end;
end;  {TCarbonWidgetSet.PromptUser}

function TCarbonWidgetSet.ReplaceBitmapMask(var Image, Mask: HBitmap;
  NewMask: HBitmap): boolean;
begin
  Result:=inherited ReplaceBitmapMask(Image, Mask, NewMask);
end;

//##apiwiz##eps##   // Do not remove, no wizard declaration after this line

// included by carbonint.pas
