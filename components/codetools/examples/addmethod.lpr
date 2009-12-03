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
  Classes, SysUtils, CodeCache, CodeToolManager, SimpleUnit1, FileProcs,
  CodeCompletionTool;
  
type
  TMyMethodType = function(Sender: TObject; AValue: integer): string of object;

var
  Filename: string;
  Code: TCodeBuffer;
  Tool: TCodeTool;
  AClassName: String;
  MethodName: String;
  MethodDefinition: String;
  CleanMethodDefinition: String;
  i: Integer;
begin
  // Example: find declaration of 'TObject'

  // load the file
  Filename:=AppendPathDelim(GetCurrentDir)+'scanexamples'+PathDelim
    +'simpleunit1.pas';
  Code:=CodeToolBoss.LoadFile(Filename,false,false);
  if Code=nil then
    raise Exception.Create('loading failed '+Filename);
    
  // Example 1: add a method compatible to TMyMethodType
  if CodeToolBoss.CreatePublishedMethod(Code,'TMyClass','NewMethod',
    typeinfo(TMyMethodType),true) then
  begin
    writeln('Method added: ');
    writeln(Code.Source);
  end else begin
    raise Exception.Create('Adding method failed');
  end;

  // Example 2: adding methods directly, but several at a time
  AClassName:='TMyClass';
  if not CodeToolBoss.InitClassCompletion(Code,UpperCase(AClassName),Tool) then
    raise Exception.Create('Explore failed');

  for i:=1 to 3 do begin
    MethodName:='NewProc'+IntToStr(i);
    MethodDefinition:='procedure '+MethodName
                                +'(Sender:TObject; AValue:integer);';
    // check, to not add an already existing method.
    // Create a search mask: only method names, parameter types and semicolon.
    // no class name, no 'procedure' keyword, no comments, no defaults,
    // no unneeded spaces, no result type, no parameter names, uppercase,
    // no modifiers (e.g. virtual)
    CleanMethodDefinition:=UpperCase(MethodName+'(:TObject;:integer);');
    if not Tool.ProcExistsInCodeCompleteClass(CleanMethodDefinition) then
      Tool.AddClassInsertion(CleanMethodDefinition, MethodDefinition, MethodName,
                             ncpPublishedProcs);
  end;

  if not Tool.ApplyClassCompletion(true) then
    raise Exception.Create('Explore failed');
  writeln('Method added: ');
  writeln(Code.Source);
end.


