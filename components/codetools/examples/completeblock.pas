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
    Demo for block completion. For example adding the 'end;' after a 'begin'.
}
program CompleteBlock;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, DefineTemplates, CodeToolsConfig, FileProcs,
  CodeToolManager, CodeCache;

var
  Code: TCodeBuffer;
  Filename: String;
  Y: LongInt;
  X: LongInt;
  p: integer;
  NewCode: TCodeBuffer;
  NewX, NewY, NewTopLine: integer;
  Defines: TStringList;
  DirDef: TDefineTemplate;
begin
  Defines:=TStringList.Create;
  try
    if Paramcount>0 then begin
      if Paramcount<4 then begin
        writeln('Usage: '+ParamStrUTF8(0)+' filename line column define1 define2');
        exit;
      end;
      Filename:=ExpandFileNameUTF8(ParamStrUTF8(1));
      Y:=StrToInt(ParamStrUTF8(2));
      X:=StrToInt(ParamStrUTF8(3));
      for p:=4 to Paramcount do
        Defines.Add(ParamStrUTF8(p));
    end else begin
      Filename:=ExpandFileNameUTF8('testscompleteblock/procedurebegin1.inc');
      X:=1;
      Y:=4;
      Defines.Add('procedurebegin');
    end;

    // load the example unit
    Code:=CodeToolBoss.LoadFile(Filename,false,false);
    if Code=nil then
      raise Exception.Create('unable to read '+Filename);

    Code.LineColToPosition(Y,X,p);
    if p<1 then begin
      writeln('ERROR: invalid position: X=',X,' Y=',Y);
      exit;
    end;

    // set defines
    DirDef:=TDefineTemplate.Create('completeblock','','',ExtractFilePath(Code.Filename),da_Directory);
    for p:=0 to Defines.Count-1 do begin
      DirDef.AddChild(TDefineTemplate.Create(Defines[p],'',Defines[p],'1',da_DefineRecurse));
    end;
    CodeToolBoss.DefineTree.Add(DirDef);

    if Defines.IndexOf('fpcunit')<0 then begin
      writeln('StartFile=',Code.Filename);
      writeln('StartX=',X);
      writeln('StartY=',Y);
    end;
    if not CodeToolBoss.CompleteBlock(Code,X,Y,false,NewCode,NewX,NewY,NewTopLine) then begin
      writeln('ERROR: complete block failed at ',Code.Filename,' X=',X,' Y=',Y);
      exit;
    end;
    if Defines.IndexOf('fpcunit')<0 then begin
      writeln('File=',NewCode.Filename);
      writeln('X=',NewX);
      writeln('Y=',NewY);
      writeln('TopLine=',NewTopLine);
    end;
    writeln(NewCode.Source);
  finally
    Defines.Free;
  end;
end.

