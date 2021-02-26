{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 Authors: Michael W. Vogel

}

unit DockedTools;

{$mode objfpc}{$H+}

interface

uses
  // RTL, FCL
  Classes, SysUtils,
  // LCL
  LCLProc, Forms,
  // IDEIntf
  IDEMsgIntf, SrcEditorIntf, IDEExternToolIntf,
  // DockedFormEditor
  DockedDesignForm;

function  EnumerationString(Str1, Str2: String): String;
function  FindSourceEditorForDesigner(ADesigner: TIDesigner): TSourceEditorInterface;
procedure IDEMessage(AString: String);
function  LinedString(Str1, Str2: String): String;

implementation

function EnumerationString(Str1, Str2: String): String;
begin
  if Str1.IsEmpty then
    Result := Str2
  else
    Result := Str1 + ', ' + Str2;
end;

function FindSourceEditorForDesigner(ADesigner: TIDesigner): TSourceEditorInterface;
var
  i: Integer;
begin
  for i := 0 to SourceEditorManagerIntf.SourceEditorCount - 1 do
    if SourceEditorManagerIntf.SourceEditors[i].GetDesigner(False) = ADesigner then
      Exit(SourceEditorManagerIntf.SourceEditors[i]);
  Result := nil;
end;

procedure IDEMessage(AString: String);
begin
  DebugLn(AString);
  if Assigned(IDEMessagesWindow) then
    IDEMessagesWindow.AddCustomMessage(mluNone, AString, '');
end;

function LinedString(Str1, Str2: String): String;
begin
  if Str1.IsEmpty then
    Result := Str2
  else
    Result := Str1 + LineEnding + Str2;
end;

end.

