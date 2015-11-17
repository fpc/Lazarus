unit RegProjectGroup;

{$mode objfpc}{$H+}

{$IFNDEF IwrotethiscodePG}
  {$ERROR This package is under construction}
{$ENDIF}

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
  cmdTargetCompile:=RegisterIDEMenuCommand(Section,'TargetCompile',lisTargetCompile);// ToDo: caption: compile "target"
  cmdTargetCompileClean:=RegisterIDEMenuCommand(Section,'TargetCompileClean',lisTargetCompileClean);// ToDo
  // ToDo: clean ... -> clean up dialog
  // ToDo: compile all from here
  // ToDo: compile all clean from here
  // ToDo: set build mode of all projects

  Section:=RegisterIDEMenuSection(Root,'AddRemove');
  PGEditMenuSectionAddRemove:=Section;
  cmdTargetAdd:=RegisterIDEMenuCommand(Section,'TargetAdd',lisTargetAdd);// ToDo
  cmdTargetRemove:=RegisterIDEMenuCommand(Section,'TargetRemove',lisTargetRemove);// ToDo

  Section:=RegisterIDEMenuSection(Root,'Use');
  PGEditMenuSectionUse:=Section;
  cmdTargetInstall:=RegisterIDEMenuCommand(Section,'TargetInstall',lisTargetInstall);// ToDo
  cmdTargetUninstall:=RegisterIDEMenuCommand(Section,'TargetUninstall',lisTargetUninstall);// ToDo
  cmdTargetEarlier:=RegisterIDEMenuCommand(Section,'TargetEarlier',lisTargetEarlier);// ToDo: Ctrl+Up
  cmdTargetLater:=RegisterIDEMenuCommand(Section,'TargetLater',lisTargetLater);// ToDo: Ctrl+Down
  cmdTargetActivate:=RegisterIDEMenuCommand(Section,'TargetActivate',lisTargetActivate);// ToDo
  cmdTargetOpen:=RegisterIDEMenuCommand(Section,'TargetOpen',lisTargetOpen);// ToDo
  // ToDo: Save
  // ToDo: Close (package editor)
  cmdTargetRun:=RegisterIDEMenuCommand(Section,'TargetRun',lisTargetRun);// ToDo
  cmdTargetProperties:=RegisterIDEMenuCommand(Section,'TargetProperties',lisTargetProperties);// ToDo

  Section:=RegisterIDEMenuSection(Root,'Misc');
  PGEditMenuSectionMisc:=Section;

  // ToDo: Copy filename
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
    'NewProjectGroup',lisNewProjectGroup,@IDEProjectGroupManager.DoNewClick);
  cmdOpenProjectGroup:=RegisterIDEMenuCommand(itmProjectOpenSection,
    'OpenProjectGroup',lisOpenProjectGroup,@IDEProjectGroupManager.DoOpenClick);
  OpenRecentProjectGroupSubMenu:=RegisterIDESubMenu(itmProjectOpenSection,
    'OpenRecentProjectGroup',lisOpenRecentProjectGroup);
  cmdSaveProjectGroup:=RegisterIDEMenuCommand(itmProjectSaveSection,
    'SaveProjectGroup',lisSaveProjectGroup,@IDEProjectGroupManager.DoSaveClick);
  cmdSaveProjectGroupAs:=RegisterIDEMenuCommand(itmProjectSaveSection,
    'SaveProjectGroupAs',lisSaveProjectGroupAs,@IDEProjectGroupManager.DoSaveAsClick);

  IDEProjectGroupManager.UpdateRecentProjectGroupMenu;

  ProjectGroupManager:=IDEProjectGroupManager;
  SetProjectGroupEditorCallBack;
end;

end.

