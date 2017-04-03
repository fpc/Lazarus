{ Lazarus package manager - UI interface

  Copyright (C) 2011 Darius Blaszyk

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
unit lazfppkgmanagerintf;

interface

uses
  LCLtype, LResources, LCLProc,
  fppkg_mainfrm, fppkg_const;

procedure Register;

implementation

uses
  MenuIntf, IDECommands, Controls, Forms, Dialogs;

var
  CmdFppkgFrm : TIDECommand;

procedure ProcFppkgForm(Sender: TObject);
begin
  try
    FppkgForm := TFppkgForm.Create(nil);
    FppkgForm.ShowModal;
  finally
    FreeThenNil(FppkgForm);
  end;
end;

procedure Register;
var
  Key: TIDEShortCut;
  Cat: TIDECommandCategory;
  mnuFppkgSection : TIDEMenuSection;
begin
  Key:=IDEShortCut(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  {$ifndef USECustomCategory}
    Cat:=IDECommandList.CreateCategory(nil, 'LazFppkg', 'Fppkg package manager', IDECmdScopeDesignerOnly);
  {$else}
    Cat:=nil;
  {$endif}

  CmdFppkgFrm:=RegisterIDECommand(Cat, 'Fppkg Package Manager', rsShowFppkgPackageManager, Key, nil, @ProcFppkgForm);

  mnuFppkgSection:=RegisterIDESubMenu(itmPkgGraphSection, 'lazfppkg', 'Extra packages', nil, nil);
  RegisterIDEMenuCommand(mnuFppkgSection, 'Fppkg Package Manager', rsShowFppkgPackageManager, nil, nil, CmdFppkgFrm);
end;

end.
