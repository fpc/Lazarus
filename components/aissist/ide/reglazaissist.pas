{ Copyright (C) 2024

 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Michael Van Canneyt

  Abstract: AI Assistant registration
}
unit RegLazAIssist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazLoggerBase, FileUtil, IDECommands, IDEWindowIntf, LazIDEIntf, MenuIntf, frmaissistchat;

var
  AIssistChatForm: TAIssistChatForm;
  AIssistChatFormCreator: TIDEWindowCreator; // set by Register procedure
  AIssistOptionsFrameID: integer = 3000;

procedure ShowAIssistChatForm(Sender: TObject);
procedure Register; // Check the "Register Unit" of this unit in the package editor.implementation

implementation

uses
  IDEOptionsIntf, IDEOptEditorIntf, AIssistController,SrcEditorIntf,
  fraAIssistConfig, Forms, StrAIssist;

procedure ShowAIssistChatForm(Sender: TObject);
begin
  IDEWindowCreators.ShowForm(AIssistChatFormCreator.FormName, true);
end;

procedure CreateAIssistChatForm(Sender: TObject; aFormName: string;
  var AForm: TCustomForm; DoDisableAutoSizing: boolean);
begin
  // sanity check to avoid clashing with another package that has registered a window with the same name
  if CompareText(aFormName, SAISSistChatForm)<>0 then begin
    DebugLn(['ERROR: CreateAIssistChatForm: there is already a form with this name']);
    exit;
  end;
  IDEWindowCreators.CreateForm(AForm, TAIssistChatForm, DoDisableAutoSizing,
    LazarusIDE.OwningComponent);
  AForm.Name:=aFormName;
  AIssistChatForm:=AForm as TAIssistChatForm;
  AIssistChatForm.OnConfigure:=@AIController.HandleShowConfig;
end;

procedure ShowAIxplainForm(Sender : TObject);

begin
  AIController.ExplainCurrentSelection(SourceEditorManagerIntf.ActiveEditor);
end;

procedure RegisterAIChatWindow;

var
  CmdCatViewMenu: TIDECommandCategory;
  ViewAIssistChatFormCommand: TIDECommand;

begin
  // search shortcut category
  CmdCatViewMenu:=IDECommandList.FindCategoryByName(CommandCategoryViewName);
  // register shortcut
  ViewAIssistChatFormCommand:=RegisterIDECommand(CmdCatViewMenu,
    SCMDViewAIssistChatForm,
    SAIssistChatMenuCaption,
    IDEShortCut(Ord('I'), [ssctrl,ssAlt],Ord('C'), [ssctrl,ssAlt]), // <- set here your default shortcut
    CleanIDEShortCut, nil, @ShowAIssistChatForm);
  // register menu item in View menu
  RegisterIDEMenuCommand(itmViewMainWindows,
    SCMDViewAIssistChatForm,
    SAIssistChatMenuCaption, nil, nil, ViewAIssistChatFormCommand);

  // register dockable Window
  AIssistChatFormCreator:=IDEWindowCreators.Add(
    'AIssistChatForm',
    @CreateAIssistChatForm, nil,
    '100', '100', '900', '700'  // default place at left=100, top=100, right=300, bottom=300
      // you can also define percentage values of screen or relative positions, see wiki
    );
  // add IDE options frame
  AIssistOptionsFrameID:=RegisterIDEOptionsEditor(GroupEnvironment,TAIAssistentConfigFrame,
                                              AIssistOptionsFrameID)^.Index;
end;

procedure RegisterExplainCommand;

var
  CatTextEditing: TIDECommandCategory;
  AIExplainSelectionCommand: TIDECommand;

begin
  CatTextEditing:=IDECommandList.FindCategoryByName(CommandCategoryTextEditingName);
  // register shortcut
  AIExplainSelectionCommand:=RegisterIDECommand(CatTextEditing,
    SCMDExplainSelectedCode,
    SAIExplainSelectedCodeCaption,
    IDEShortCut(Ord('I'), [ssctrl,ssAlt],Ord('E'), [ssctrl,ssAlt]), // <- set here your default shortcut
    CleanIDEShortCut, nil, @ShowAIxplainForm);
  // register menu item in View menu
  RegisterIDEMenuCommand(itmSourceCodeToolChecks,
    SCMDExplainSelectedCode,
    SAIExplainSelectedCodeCaption, nil, nil, AIExplainSelectionCommand);
end;

procedure Register;
begin
  AIController.LoadConfig;
  AIController.ConfigFrame:=TAIAssistentConfigFrame;
  RegisterAIChatWindow;
  RegisterExplainCommand;
end;

end.

