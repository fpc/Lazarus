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
    A tool to help converting C header files to pascal bindings.
}
unit H2PasTool;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileProcs,CCodeParserTool, NonPascalCodeTools,
  CodeCache;
  
type

  { TH2PasTool }

  TH2PasTool = class
  public
    CTool: TCCodeParserTool;
    function Convert(CCode, PascalCode: TCodeBuffer): boolean;
    procedure WriteDebugReport;
  end;

implementation

{ TH2PasTool }

function TH2PasTool.Convert(CCode, PascalCode: TCodeBuffer): boolean;
begin
  Result:=false;
  
  CTool:=TCCodeParserTool.Create;
  try
    // pare C header file
    CTool.Parse(CCode);
    CTool.WriteDebugReport;
    
  finally
    CTool.Free;
  end;
  
  Result:=true;
end;

procedure TH2PasTool.WriteDebugReport;
begin
  DebugLn(['TH2PasTool.WriteDebugReport ']);
  if CTool<>nil then
    CTool.WriteDebugReport;
end;

end.

