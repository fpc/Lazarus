{
  Author: Mattias Gaertner
}
unit CopyAsPasDemoUnit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FormEditingIntf, IDECommands, MenuIntf, IDEDialogs,
  LazLoggerBase, CompWriterPas, Forms, Dialogs, Clipbrd;

resourcestring
  rsCopyFormAsPascal = 'Copy form as Pascal';

procedure Register;

implementation

procedure CopyFormAsPascal(Sender: TObject);
var
  aDesigner: TIDesigner;
  MemStream: TMemoryStream;
  Writer: TCompWriterPas;
  s: string;
begin
  debugln(['CopyFormAsPascal ',DbgSName(Sender)]);

  // get currently active designer form
  aDesigner:=FormEditingHook.GetCurrentDesigner;
  if aDesigner=nil then
  begin
    IDEMessageDialog('Missing Designer','Copy form as Pascal needs a designer form.',
      mtError,[mbOK]);
  end;

  // create a TMemoryStream as storage
  MemStream:=TMemoryStream.Create;

  // create TCompWriterPas and set its options
  Writer:=TCompWriterPas.Create(MemStream);
  try
    // Writer has various options to make the Pascal suitable for various contexts:
    // Writer.CurIndent:=2;
    // Writer.Options:=[...]
    // Writer.MaxColumn:=80;

    // generate the Pascal
    FormEditingHook.SaveComponentAsPascal(aDesigner,Writer);

    // copy the MemStream to a string
    SetLength(s,MemStream.Size);
    if s<>'' then
      system.Move(MemStream.Memory^,s[1],length(s));
    debugln(['CopyFormAsPascal :START=============================================']);
    debugln(s);
    debugln(['CopyFormAsPascal :END===============================================']);

    // copy the Pascal to the Clipboard
    Clipboard.AsText:=s;

    // Hint:
    // Writer.NeededUnits now contains all units that the generated Pascal needs
  finally
    Writer.Free;
    MemStream.Free;
  end;
end;

procedure Register;
var
  CmdCatDesignerMenu: TIDECommandCategory;
  CopyFormAsPascalCommand: TIDECommand;
begin
  // get the designer popup menu
  CmdCatDesignerMenu:=IDECommandList.FindCategoryByName('Designer');
  if CmdCatDesignerMenu=nil then
    raise Exception.Create('CopyAsPasDemoUnit1: command category Designer not found');

  // add an IDE command, so the user can set a keyboard shortcut
  CopyFormAsPascalCommand:=RegisterIDECommand(CmdCatDesignerMenu, 'CopyFormAsPascal',
    rsCopyFormAsPascal,
    CleanIDEShortCut,CleanIDEShortCut,nil,@CopyFormAsPascal);

  // add an IDE menu item in the designer's popup menu
  RegisterIDEMenuCommand(DesignerMenuSectionClipboard,'CopyFormAsPascal',rsCopyFormAsPascal,
    nil,nil,CopyFormAsPascalCommand);
end;

end.

