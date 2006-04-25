{ Copyright (C) 2004

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Mattias Gaertner
  
  Abstract:
    Common IDE dialogs.
}
unit IDEDialogs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Dialogs;
  
function LazSelectDirectory(const Title: string; const InitialDir: string = ''
  ): string;

type
  TIDESelectDirectory = function(const Title, InitialDir: string): string of object;
var
  LazIDESelectDirectory: TIDESelectDirectory;// set by the IDE


implementation

function LazSelectDirectory(const Title: string; const InitialDir: string
  ): string;
begin
  Result:=LazIDESelectDirectory(Title,InitialDir);
end;

end.

