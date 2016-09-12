unit RegProjectGroup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ProjectGroupIntf, MenuIntf,
  ProjectGroupStrConst, ProjectGroup, ProjectGroupEditor;

procedure RegisterStandardProjectGroupMenuItems;
procedure Register;

implementation

const
  ProjectGroupEditorMenuRootName = 'ProjectGroupEditorMenu';

procedure RegisterStandardProjectGroupMenuItems;
var
  Section,Root : TIDEMenuSection;
begin
  Root:=RegisterIDEMenuRoot(ProjectGroupEditorMenuRootName);
  ProjectGroupMenuRoot:=Root;
  PGEditMenuSectionFiles:=RegisterIDEMenuSection(Root,'File');

  Section:=RegisterIDEMenuSection(Root,'Compile');
  PGEditMenuSectionCompile:=Section;
  cmdTargetCompile:=RegisterIDEMenuCommand(Section,'TargetCompile',lisTargetCompile);
  cmdTargetCompileClean:=RegisterIDEMenuCommand(Section,'TargetCompileClean',lisTargetCompileClean);
  cmdTargetCompileFromHere:=RegisterIDEMenuCommand(Section,'TargetCompileFromHere',lisTargetCompileFromHere);
  // ToDo: clean ... -> clean up dialog
  // ToDo: set build mode of all projects

  Section:=RegisterIDEMenuSection(Root,'AddRemove');
  PGEditMenuSectionAddRemove:=Section;
  cmdTargetAdd:=RegisterIDEMenuCommand(Section,'TargetAdd',lisTargetAdd);
  cmdTargetRemove:=RegisterIDEMenuCommand(Section,'TargetRemove',lisTargetRemove);
  // ToDo: re-add

  Section:=RegisterIDEMenuSection(Root,'Use');
  PGEditMenuSectionUse:=Section;
  cmdTargetInstall:=RegisterIDEMenuCommand(Section,'TargetInstall',lisTargetInstall);// ToDo
  cmdTargetUninstall:=RegisterIDEMenuCommand(Section,'TargetUninstall',lisTargetUninstall);// ToDo
  cmdTargetEarlier:=RegisterIDEMenuCommand(Section,'TargetEarlier',lisTargetEarlier);// ToDo: Ctrl+Up
  cmdTargetLater:=RegisterIDEMenuCommand(Section,'TargetLater',lisTargetLater);// ToDo: Ctrl+Down
  cmdTargetActivate:=RegisterIDEMenuCommand(Section,'TargetActivate',lisTargetActivate);
  cmdTargetOpen:=RegisterIDEMenuCommand(Section,'TargetOpen',lisTargetOpen);
  cmdTargetRun:=RegisterIDEMenuCommand(Section,'TargetRun',lisTargetRun);
  cmdTargetProperties:=RegisterIDEMenuCommand(Section,'TargetProperties',lisTargetProperties);

  Section:=RegisterIDEMenuSection(Root,'Misc');
  PGEditMenuSectionMisc:=Section;

  cmdTargetCopyFilename:=RegisterIDEMenuCommand(Section,'CopyFilename',lisTargetCopyFilename);
  // ToDo: View source (project)

  // ToDo: find in files
  // ToDo: find references in files

  // ToDo: D&D order compile targets
end;

procedure Register;
begin
  RegisterStandardProjectGroupMenuItems;
  IDEProjectGroupManager:=TIDEProjectGroupManager.Create;
  IDEProjectGroupManager.Options.LoadSafe;

  cmdCreateProjectGroup:=RegisterIDEMenuCommand(itmProjectNewSection,
    'NewProjectGroup',lisNewProjectGroupMenuC,@IDEProjectGroupManager.DoNewClick);
  cmdOpenProjectGroup:=RegisterIDEMenuCommand(itmProjectOpenSection,
    'OpenProjectGroup',lisOpenProjectGroup,@IDEProjectGroupManager.DoOpenClick);
  OpenRecentProjectGroupSubMenu:=RegisterIDESubMenu(itmProjectOpenSection,
    'OpenRecentProjectGroup',lisOpenRecentProjectGroup);
  cmdSaveProjectGroup:=RegisterIDEMenuCommand(itmProjectSaveSection,
    'SaveProjectGroup',lisSaveProjectGroup,@IDEProjectGroupManager.DoSaveClick);
  cmdSaveProjectGroup.Enabled:=false;
  cmdSaveProjectGroupAs:=RegisterIDEMenuCommand(itmProjectSaveSection,
    'SaveProjectGroupAs',lisSaveProjectGroupAs,@IDEProjectGroupManager.DoSaveAsClick);
  cmdSaveProjectGroupAs.Enabled:=false;

  IDEProjectGroupManager.UpdateRecentProjectGroupMenu;

  ProjectGroupManager:=IDEProjectGroupManager;
  SetProjectGroupEditorCallBack;
end;

finalization
  FreeAndNil(IDEProjectGroupManager);

end.

