{%MainUnit carbonprivate.pp}
{ $Id$}
{
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

// ==================================================================
// H A N D L E R S
// ==================================================================

function CarbonPrivateEdit_TextChanged(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AInfo: TCarbonWidgetInfo): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
var
  S: String;
  M: Integer;
  Msg: TLMessage;
  MemoStrings: TCarbonMemoStrings;
begin
  DebugLn('PrivateEdit_TextChanged: ', (AInfo.LCLObject as TComponent).Name);
  Result := CallNextEventHandler(ANextHandler, AEvent);
  
  if (AInfo.LCLObject is TCustomEdit) then
  begin
    // limit the text according to MaxLength
    M := (AInfo.LCLObject as TCustomEdit).MaxLength;
    if M > 0 then
    begin
      if TCarbonWSCustomEdit.GetText(AInfo.LCLObject as TWinControl, S) then
        if UTF8Length(S) > M then
        begin
          S := UTF8Copy(S, 1, M);
          TCarbonWSCustomEdit.SetText(AInfo.LCLObject as TWinControl, S);
        end;
    end;
    
    if (AInfo.LCLObject is TCustomMemo) then
    begin
      MemoStrings := (AInfo.LCLObject as TCustomMemo).Lines as TCarbonMemoStrings;
      if MemoStrings <> nil then MemoStrings.ExternalChanged;
    end;
    
    FillChar(Msg, SizeOf(Msg), 0);
    Msg.Msg := CM_TEXTCHANGED;
    DeliverMessage(AInfo.LCLObject, Msg);
  end;
end;

// ==================================================================
// C L A S S
// ==================================================================

class procedure TCarbonPrivateEdit.RegisterEvents(AInfo: TCarbonWidgetInfo);
var
  TmpSpec: EventTypeSpec;
begin
  inherited;
  
  TmpSpec := MakeEventSpec(kEventClassTextField, kEventTextDidChange);
  InstallControlEventHandler(AInfo.Widget,
    RegisterEventHandler(@CarbonPrivateEdit_TextChanged),
    1, @TmpSpec, Pointer(AInfo), nil);
end;

class procedure TCarbonPrivateEdit.UnRegisterEvents;
begin
  UnregisterEventHandler(@CarbonPrivateEdit_TextChanged);
  
  inherited;
end;
