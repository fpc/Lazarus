{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Dialog to edit System-Variables-User-Overrides.
    Used by the run parameter dialog
}
unit SysVarUserOverrideDlg;

{$mode objfpc}{$H+}

{$I ide.inc}

interface

uses
  {$IFDEF IDE_MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils,
  // LCL
  Controls, Forms, Dialogs,
  // IdeIntf
  IDEDialogs,
  // IDE
  LazarusIDEStrConsts;

function ShowSysVarUserOverrideDialog(var AName, AValue: string): TModalResult;

implementation

function ShowSysVarUserOverrideDialog(var AName, AValue: string): TModalResult;
var
  ok: boolean;
  Vals: array of string;
begin
  SetLength(Vals, 2);
  Vals[0]:= AName;
  Vals[1]:= AValue;

  repeat
    ok:= InputQuery(lisSVUOOverrideSystemVariable,
      [lisVariable, lisValue], Vals);
    if not ok then exit(mrCancel);

    AName:= Trim(Vals[0]);
    AValue:= Vals[1];
    if IsValidIdent(AName) then
      exit(mrOk)
    else
      if IDEMessageDialog(lisSVUOInvalidVariableName,
        Format(lisSVUOisNotAValidIdentifier, [AName]),
        mtWarning, [mbCancel, mbIgnore])=mrIgnore then exit(mrOk);
  until false;
end;

end.

