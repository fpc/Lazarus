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
    Demo for automatic fixing the filenames of include directives and uses
    section.
}
program FixFilenames;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, DefineTemplates, CodeToolsConfig, FileProcs,
  CodeToolsStructs, CodeToolManager, CodeCache;

const
  ConfigFilename = 'codetools.config';
var
  Code: TCodeBuffer;
  Filename: String;
  MissingUnits: TStrings;
  ReplaceUnits: TStringToStringTree;
  MissingIncludeFilesCodeXYPos: TFPList;
begin
  // init the codetools
  CodeToolBoss.SimpleInit(ConfigFilename);

  // load the example unit
  Filename:=ExpandFileNameUTF8('scanexamples/brokenfilenames.pas');
  Code:=CodeToolBoss.LoadFile(Filename,false,false);
  if Code=nil then
    raise Exception.Create('unable to read '+Filename);
    
  // fix the filenames in the include directives
  MissingIncludeFilesCodeXYPos:=nil;
  if not CodeToolBoss.FixIncludeFilenames(Code,true,
                                          MissingIncludeFilesCodeXYPos)
  then
    raise Exception.Create('unable to fix include filesnames in '+Filename+' '+CodeToolBoss.ErrorMessage);
  CodeToolBoss.FreeListOfPCodeXYPosition(MissingIncludeFilesCodeXYPos);

  // replace some unit names
  ReplaceUnits:=TStringToStringTree.Create(false);
  ReplaceUnits['classes']:='Classes, SysUtils';
  ReplaceUnits['CustApp']:='';
  if not CodeToolBoss.ReplaceUsedUnits(Code,ReplaceUnits) then
    raise Exception.Create('unable to fix unit names in '+Filename+' '+CodeToolBoss.ErrorMessage);
  ReplaceUnits.Free;

  // fix the unitnames in the uses section
  MissingUnits:=nil;
  if not CodeToolBoss.FindMissingUnits(Code,MissingUnits,true) then
    raise Exception.Create('unable to fix unit names in '+Filename+' '+CodeToolBoss.ErrorMessage);

  if MissingUnits<>nil then begin
    writeln('MissingUnits=',MissingUnits.Text);
    if not CodeToolBoss.CommentUnitsInUsesSections(Code,MissingUnits) then
      raise Exception.Create('unable to comment units in uses section in '+Filename+' '+CodeToolBoss.ErrorMessage);
    MissingUnits.Free;
  end;
  
  writeln('==================================================================');
  writeln(Code.Source);
end.

