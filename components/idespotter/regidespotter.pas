{ Register IDE Spotter unit

  Copyright (C) 2018  Michael van Canneyt  michael@freepascal.org

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}
unit RegIDESpotter;

{$mode objfpc}{$H+}

interface

uses
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf, IDEUtils, Classes, SysUtils;

Procedure Register;

implementation

uses IDESpotterOptions,forms, graphics,lcltype,idecommands,toolbarintf,menuintf, frmspotter;

Procedure IdeMenuClicked(Sender : TObject);

begin
  ShowSpotterForm;
end;

Procedure Register;

Const
{$IFDEF DARWIN}
  ShiftKeys = [ssMeta,ssShift];
{$ELSE}
  ShiftKeys = [ssAlt,ssShift];
{$ENDIF}


var
  IDEShortCutX: TIDEShortCut;
  IDECommandCategory: TIDECommandCategory;
  IDECommand: TIDECommand;
  IDESpotteroptionsFrameID: Integer = 2000;

begin
  LoadSpotterOptions;
  IDEShortCutX := IDEShortCut(VK_P, ShiftKeys, VK_UNKNOWN, []);
  IDECommandCategory := IDECommandList.FindCategoryByName(CommandCategoryViewName);
  if IDECommandCategory <> nil then
  begin
    IDECommand := RegisterIDECommand(IDECommandCategory, 'Spotter', 'Open Spotter',
      IDEShortCutX, nil, @IDEMenuClicked);
    if IDECommand <> nil then
      RegisterIDEButtonCommand(IDECommand);
  end;
  IDESpotteroptionsFrameID:=RegisterIDEOptionsEditor(GroupEnvironment,TIDESpotterOptionsFrame,
                                              IDESpotteroptionsFrameID)^.Index;
  RegisterIDEMenuCommand(itmViewIDEInternalsWindows, 'Spotter', 'Spotter', nil,
    @IDEMenuClicked,IDECommand);
end;

Initialization
  SettingsClass:=TIDESpotterOptionsFrame;
end.

