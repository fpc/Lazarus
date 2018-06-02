{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Your name

  Abstract:
    This unit installs the TemplateName window in the Lazarus IDE.
}
unit TemplateIDEDockableWindow;

{$mode objfpc}{$H+}

interface

uses
// UsesStart
  Classes, SysUtils, LazLoggerBase, FileUtil, Forms, Controls, Graphics, Dialogs,
  LCLType, IDECommands, IDEWindowIntf, LazIDEIntf, MenuIntf
// UsesEnd
  ;

// InterfaceStart
type
  TTemplateName = class(TForm)
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  TemplateName: TTemplateName;
  TemplateNameCreator: TIDEWindowCreator; // set by Register procedure

procedure ShowTemplateName(Sender: TObject);
procedure Register; // Check the "Register Unit" of this unit in the package editor.

// InterfaceEnd
// ImplementationStart
implementation

{$R *.lfm}

procedure ShowTemplateName(Sender: TObject);
begin
  IDEWindowCreators.ShowForm(TemplateNameCreator.FormName,true);
end;

procedure CreateTemplateName(Sender: TObject; aFormName: string;
  var AForm: TCustomForm; DoDisableAutoSizing: boolean);
begin
  // sanity check to avoid clashing with another package that has registered a window with the same name
  if CompareText(aFormName,'TemplateName')<>0 then begin
    DebugLn(['ERROR: CreateTemplateName: there is already a form with this name']);
    exit;
  end;
  IDEWindowCreators.CreateForm(AForm,TTemplateName,DoDisableAutoSizing,
    LazarusIDE.OwningComponent);
  AForm.Name:=aFormName;
  TemplateName:=AForm as TTemplateName;
end;

procedure Register;
var
  CmdCatViewMenu: TIDECommandCategory;
  ViewTemplateNameCommand: TIDECommand;
  MenuItemCaption: String;
begin
  // register shortcut and menu item
  MenuItemCaption:='Caption of TemplateName'; // <- this caption should be replaced by a resourcestring
  // search shortcut category
  CmdCatViewMenu:=IDECommandList.FindCategoryByName(CommandCategoryViewName);
  // register shortcut
  ViewTemplateNameCommand:=RegisterIDECommand(CmdCatViewMenu,
    'ViewTemplateName',
    MenuItemCaption,
    IDEShortCut(VK_UNKNOWN,[]), // <- set here your default shortcut
    CleanIDEShortCut,nil,@ShowTemplateName);
  // register menu item in View menu
  RegisterIDEMenuCommand(itmViewMainWindows,
    'ViewTemplateName',
    MenuItemCaption, nil, nil, ViewTemplateNameCommand);

  // register dockable Window
  TemplateNameCreator:=IDEWindowCreators.Add(
    'TemplateName',
    @CreateTemplateName,nil,
    '100','100','300','300'  // default place at left=100, top=100, right=300, bottom=300
     // you can also define percentage values of screen or relative positions, see wiki
    );
end;
// ImplementationEnd

end.

