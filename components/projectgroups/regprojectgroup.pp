unit RegProjectGroup;

{$mode objfpc}{$H+}

{$IFNDEF IwrotethiscodePG}
  {$ERROR This package is under construction}
{$ENDIF}

interface

uses
  Classes, SysUtils, ProjectGroupIntf, MenuIntf,
  ProjectGroup, ProjectGroupEditor;

procedure RegisterStandardProjectGroupMenuItems;
Procedure Register;

implementation

Const
  ProjectGroupEditorMenuRootName = 'ProjectGroupEditorMenu';

Resourcestring
  lisTargetAdd          = 'Add target';
  lisTargetRemove       = 'Remove target';
  lisTargetCompile      = 'Compile';
  lisTargetCompileClean = 'Compile clean';
  lisTargetInstall      = 'Install';
  lisTargetUnInstall    = 'Uninstall';
  lisTargetActivate     = 'Activate target';
  lisTargetOpen         = 'Open Target';
  lisTargetRun          = 'Run Target';
  lisTargetProperties   = 'Target properties';
  lisTargetLater        = 'Compile target later';
  lisTargetEarlier      = 'Compile target earlier';
  lisNewProjectGroup    = 'New Project group';
  lisOpenProjectGroup   = 'Open Project group';
  lisSaveProjectGroup   = 'Save Project group';
  lisSaveProjectGroupAs = 'Save Project group as';


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
  // ToDo: Copy filename
  // ToDo: View source (project)
end;

Procedure Register;
begin
  RegisterStandardProjectGroupMenuItems;
  IDEProjectGroupManager:=TIDEProjectGroupManager.Create;

  cmdCreateProjectGroup:=RegisterIDEMenuCommand(itmProjectNewSection,
    'NewProjectGroup',lisNewProjectGroup,@IDEProjectGroupManager.DoNewClick);
  cmdOpenProjectGroup:=RegisterIDEMenuCommand(itmProjectOpenSection,
    'OpenProjectGroup',lisOpenProjectGroup,@IDEProjectGroupManager.DoOpenClick);
  cmdSaveProjectGroup:=RegisterIDEMenuCommand(itmProjectSaveSection,
    'SaveProjectGroup',lisSaveProjectGroup,@IDEProjectGroupManager.DoSaveClick);
  cmdSaveProjectGroupAs:=RegisterIDEMenuCommand(itmProjectSaveSection,
    'SaveProjectGroupAs',lisSaveProjectGroupAs,@IDEProjectGroupManager.DoSaveAsClick);

  ProjectGroupManager:=IDEProjectGroupManager;
  SetProjectGroupEditorCallBack;
end;

end.

