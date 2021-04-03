{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 Authors: Michael W. Vogel

}

unit DockedTools;

{$mode objfpc}{$H+}
{ $define DEBUGDOCKEDFORMEDITORINIDE}

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

{$IFDEF DEBUGDOCKEDFORMEDITORINIDE}
procedure DebugLn(s: String); overload;
procedure DebugLn(s1, s2: String); overload;
procedure DebugLn(s1, s2, s3: String); overload;
{$ENDIF}

function  EnumerationString(Str1, Str2: String): String;
function  FindSourceEditorForDesigner(ADesigner: TIDesigner): TSourceEditorInterface;
procedure IDEMessage(AString: String);
function  LinedString(Str1, Str2: String): String;

implementation

{$IFDEF DEBUGDOCKEDFORMEDITORINIDE}
procedure DebugLn(s: String);
begin
  IDEMessage(s);
end;

procedure DebugLn(s1, s2: String);
begin
  IDEMessage(s1 + s2);
end;

procedure DebugLn(s1, s2, s3: String);
begin
  IDEMessage(s1 + s2 + s3);
end;
{$ENDIF}

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
  LCLProc.DebugLn(AString);
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

