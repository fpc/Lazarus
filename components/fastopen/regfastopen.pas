unit regfastopen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms, Dialogs, lclintf, LCLType,
  LazIDEIntf, MenuIntf, IDECommands, IDEWindowIntf, BaseIDEIntf;

procedure register;

implementation

Resourcestring

  SUnitNotFound = 'Unit "%s" not found.';
  SFastOpenCaption = 'Fast open unit';
  SEnterUnitName = 'Enter the unit name to search and open';

procedure ShowFastOpenUnit(Sender: TObject);
var
  lUnitName : string;
  lFileName : string;
begin
  lUnitName:='';
  if not InputQuery(SFastOpenCaption,SEnterUnitName,lUnitName) then exit;
  lFileName:=LazarusIDE.FindUnitFile(lUnitName);
  if lFileName='' then
    MessageDlg(SFastOpenCaption,Format(SUnitNotFound,[lUnitName]),mtInformation,[mbOK],0)
  else
    LazarusIDE.DoOpenEditorFile(lFileName,0,0,[ofAddToRecent,ofOnlyIfExists]);
end;


procedure Register;

var
  ParentCat: TIDECommandCategory;
  FastOpenCommand: TIDECommand;
begin
  // search shortcut category
  ParentCat:=IDECommandList.FindCategoryByName(CommandCategoryToolMenuName);
  // register shortcut
  FastOpenCommand:=RegisterIDECommand(ParentCat,
    'ViewFastOpen',SFastOpenCaption,
    IDEShortCut(VK_RETURN, [ssctrl,ssShift]),nil,@ShowFastOpenUnit);
  RegisterIDEMenuCommand(mnuSearch,
    FastOpenCommand.Name,
    SFastOpenCaption, nil, nil, FastOpenCommand);
end;

end.

