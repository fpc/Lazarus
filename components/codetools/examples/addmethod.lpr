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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Simple demonstrating, how to add a method to a class.
}
program AddMethod;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, CodeCache, CodeToolManager, SimpleUnit1, FileProcs;
  
type
  TMyMethodType = function(Sender: TObject; AValue: integer): string of object;

var
  Code: TCodeBuffer;
  Filename: string;
begin
  // Example: find declaration of 'TObject'

  // Step 1: load the file
  Filename:=AppendPathDelim(GetCurrentDir)
            +'scanexamples'+PathDelim+'simpleunit1.pas';
  Code:=CodeToolBoss.LoadFile(Filename,false,false);
  if Code=nil then
    raise Exception.Create('loading failed '+Filename);

  // Step 2: add a method compatible to TMyMethodType
  if CodeToolBoss.CreatePublishedMethod(Code,'TMyClass','NewMethod',
    typeinfo(TMyMethodType),true) then
  begin
    writeln('Method added: ');
    writeln(Code.Source);
  end else begin
    writeln('Adding method failed: ',CodeToolBoss.ErrorMessage);
  end;
end.


