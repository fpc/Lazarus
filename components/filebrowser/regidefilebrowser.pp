unit RegIDEFileBrowser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  LazLoggerBase,
  Controls, Forms,
  LazIDEIntf, MenuIntf, IDECommands, IDEWindowIntf, BaseIDEIntf,
  IDEOptionsIntf, IDEOptEditorIntf,
  frmFileBrowser, ctrlfilebrowser, frmconfigfilebrowser;

procedure Register;

implementation

uses lcltype,frmfilesearcher,filebrowsertypes;

var
  FileBrowserOptionsFrameID: integer = 2000;
  FileBrowserCreator: TIDEWindowCreator; // set by Register procedure

procedure ShowFileBrowser(Sender: TObject);
begin
  IDEWindowCreators.ShowForm(FileBrowserCreator.FormName,true);
end;

procedure ShowFileSearcher(Sender: TObject);

var
  Entries : TFileEntryArray;

var
  C: TFileBrowserController;

begin
  Entries:=[];
  With TFileSearcherForm.Create(Application) do
    try
      if ShowModal=mrOK then
        Entries:=GetSelectedItems;
    finally
      Free;
    end;
  if Length(Entries)>0 then
    begin
    C := LazarusIDE.OwningComponent.FindComponent('IDEFileBrowserController') as TFileBrowserController;
    if Assigned(C) then
      C.OpenFiles(Entries);
    end;
end;


procedure CreateFileBrowser(Sender: TObject; aFormName: string; var AForm: TCustomForm; DoDisableAutoSizing: boolean);

var
  C: TFileBrowserController;

begin
  // sanity check to avoid clashing with another package that has registered a window with the same name
  if CompareText(aFormName,'FileBrowser')<>0 then begin
    DebugLn(['ERROR: CreateFileBrowser: there is already a form with this name']);
    exit;
  end;
  C := LazarusIDE.OwningComponent.FindComponent('IDEFileBrowserController') as TFileBrowserController;
  IDEWindowCreators.CreateForm(AForm,TFileBrowserForm,true,C);
  AForm.Name:=aFormName;
  FileBrowserForm:=AForm as TFileBrowserForm;
  C.ConfigWindow(FileBrowserForm);
  FileBrowserForm.ShowFiles;
  if not DoDisableAutoSizing then
    AForm.EnableAutoSizing;
end;


procedure CreateController;

var
  C: TFileBrowserController;

begin
  C := LazarusIDE.OwningComponent.FindComponent('IDEFileBrowserController') as TFileBrowserController;
  if (C = nil) then
    begin
    C := TFileBrowserController.Create(LazarusIDE.OwningComponent);
    C.Name:='IDEFileBrowserController';
    if C.GetResolvedRootDir<>'' then
      C.IndexRootDir;
    end;
  C.ConfigFrame:=TFileBrowserOptionsFrame;
end;

procedure Register;

var
  CmdCatViewMenu: TIDECommandCategory;
  ViewFileBrowserCommand: TIDECommand;
  ViewFileSearcherCommand: TIDECommand;
begin
  // search shortcut category
  CmdCatViewMenu:=IDECommandList.FindCategoryByName(CommandCategoryViewName);
  // register shortcut
  ViewFileBrowserCommand:=RegisterIDECommand(CmdCatViewMenu,
    'ViewFileBrowser',SFileBrowserIDEMenuCaption,
    CleanIDEShortCut,nil,@ShowFileBrowser);
  // register shortcut
  ViewFileSearcherCommand:=RegisterIDECommand(CmdCatViewMenu,
    'ViewFileSearcher',SFileSearcherIDEMenuCaption,
    IDEShortCut(Ord('P'), [ssctrl], VK_UNKNOWN,[]),nil,@ShowFileSearcher);
  // register menu item in View menu
  RegisterIDEMenuCommand(itmViewMainWindows,
    ViewFileBrowserCommand.Name,
    SFileBrowserIDEMenuCaption, nil, nil, ViewFileBrowserCommand);
  RegisterIDEMenuCommand(itmViewMainWindows,
    ViewFileSearcherCommand.Name,
    SFileSearcherIDEMenuCaption, nil, nil, ViewFileSearcherCommand);

  CreateController;

  // register dockable Window
  FileBrowserCreator:=IDEWindowCreators.Add(
    'FileBrowser',
    @CreateFileBrowser,nil,
    '200','100','400','400'
    );
  // add IDE options frame
  FileBrowserOptionsFrameID:=RegisterIDEOptionsEditor(GroupEnvironment,TFileBrowserOptionsFrame,
                                              FileBrowserOptionsFrameID)^.Index;

end;

end.

