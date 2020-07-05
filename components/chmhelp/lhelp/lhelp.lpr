{ Copyright (C) <2005-2014> <Andrew Haines>, Lazarus contributors lhelp.lpr

  Lhelp CHM help viewer application

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
  Boston, MA 02110-1335, USA.
}
program lhelp;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  SysUtils, Classes, Controls, Dialogs, Forms,
  SimpleIPC, TurboPowerIPro, chmpopup, lhelpcontrolpkg, lhelpcore, lhelpstrconsts;

var
  X: Integer;
  S: TStringList;

{$R *.res}

begin
  Application.Title:='';
  Application.Initialize;
  for X := 1 to ParamCount do
  begin
    if LowerCase(ParamStr(X)) = '--help' then 
    begin
      S := TStringList.Create;
      S.Add(slhelp_LHelpOptions);
      S.Add('');
      S.Add(slhelp_UsageLhelpFilenameContextIdHideIpcnameLhelpMyapp);
      S.Add('');
      S.Add(slhelp_HelpShowThisInformation);
      S.Add(slhelp_HideStartHiddenButAcceptCommunicationsViaIPC);
      S.Add(slhelp_ContextShowTheHelpInformationRelatedToThisContext);
      S.Add(Format(slhelp_IpcnameTheNameOfTheIPCServerToListenOnForProgramsW, [LineEnding]));

      if TextRec(Output).Mode = fmClosed then
        MessageDlg(S.Text, mtInformation, [mbOk], 0)
      else
        WriteLn(S.Text);

      S.Free;
      Exit;
    end;
  end;
  Application.CreateForm(THelpForm, HelpForm);
  Application.CreateForm(THelpPopupForm, HelpPopupForm);

  try
    Application.Run;
  except
    // try to remove stale named pipes so that a new instance can use them
    if IPCServer <> nil then
    begin
      try
        FreeAndNil(IPCServer);
      except
        // ignore
      end;
    end;

    if IPCClient <> nil then
    begin
      try
        FreeAndNil(IPCClient);
      except
        // ignore
      end;
    end;
  end;
end.

