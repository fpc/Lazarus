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


class procedure TCarbonPrivateValueControl.RegisterEvents(AInfo: TCarbonWidgetInfo);
var
  TmpSpec: EventTypeSpec;
begin
  inherited;

  TmpSpec := MakeEventSpec(kEventClassControl, kEventControlValueFieldChanged);
  InstallControlEventHandler(AInfo.Widget,
    RegisterEventHandler(@CarbonPrivateCommon_ValueChanged),
    1, @TmpSpec, Pointer(AInfo), nil);
  DebugLn(TWinControl(AInfo.LCLObject).Name,':', TWinControl(Ainfo.LCLObject).ClassName ,' Value Changed Events set')
end;

class procedure TCarbonPrivateValueControl.UnRegisterEvents;
begin
  UnregisterEventHandler(@CarbonPrivateCommon_ValueChanged);
  
  inherited;
end;
