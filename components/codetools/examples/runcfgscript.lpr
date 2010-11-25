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
    Demo for automatic indentation.
}
program RunCfgScript;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, DefineTemplates, CodeToolsConfig, FileProcs,
  CodeToolsStructs, CodeToolManager, CodeCache, CodeBeautifier,
  CodeToolsCfgScript;

var
  Code: TCodeBuffer;
  Filename: String;
  Src: String;
  Engine: TCTConfigScriptEngine;
  i: Integer;
begin
  if Paramcount>0 then begin
    if Paramcount<>1 then begin
      writeln('Usage: '+ParamStrUTF8(0)+' filename');
      exit;
    end;
    Filename:=ExpandFileNameUTF8(ParamStrUTF8(1));

    // load the example unit
    Code:=CodeToolBoss.LoadFile(Filename,false,false);
    if Code=nil then
      raise Exception.Create('unable to read '+Filename);
    Src:=Code.Source;
  end else begin
    //Src:='a:=2; b:=3; if a+b=5 then Result:=13';
    //Src:='if (TargetOS=''win32'') then Result:=3';
    //Src:='a:=2; b:=3; b+=a;';
    //Src:='a:=2; b:=''3''; b+=a;';
    Src:='a:=''1''; if a=1 then b:=3;';
  end;

  Engine:=TCTConfigScriptEngine.Create;
  try
    //Engine.MaxErrorCount:=0;
    if not Engine.Execute(Src) then begin
      writeln('Script failed to run:');
      for i:=0 to Engine.ErrorCount-1 do
        writeln(Engine.GetErrorStr(i));
    end else begin
      writeln('Result="',Engine.Variables['Result'],'"');
    end;
    Engine.Variables.WriteDebugReport('Variables');
  except
    on E: Exception do begin
      writeln(E.Message);
    end;
  end;
  Engine.Free;
end.

