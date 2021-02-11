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

function FindSourceEditorForDesigner(ADesigner: TIDesigner): TSourceEditorInterface;
procedure IDEMessage(AString: String);

implementation

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

end.

