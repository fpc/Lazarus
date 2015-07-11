// included by gtkcallback.pp

{
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

function gtkComboBoxShowAfter(widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
var
  Mess : TLMCommand;
  AComboBox: TCustomComboBox;
begin
  Result := True;
  EventTrace('ComboBoxShowAfter', data);

  AComboBox:=TObject(Data) as TCustomComboBox;
  AComboBox.IntfGetItems;
  
  if (Widget=nil) then ;
  FillChar(Mess,SizeOf(Mess),0);
  Mess.Msg := CN_Command;
  Mess.NotifyCode := CBN_DROPDOWN;

  Result := DeliverMessage(Data, Mess) = 0;
end;

function gtkComboBoxHideAfter(widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
var
  Mess : TLMCommand;
begin
  Result := True;
  if (Widget=nil) then ;
  EventTrace('ComboBoxHideAfter', data);
  FillChar(Mess,SizeOf(Mess),0);
  Mess.Msg := CN_Command;
  Mess.NotifyCode := CBN_CLOSEUP;

  Result := DeliverMessage(Data, Mess) = 0;
end;

// included by gtkcallback.pp
