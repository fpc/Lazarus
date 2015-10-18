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
  P,S : TIDEMenuSection;

begin
  S:=RegisterIDEMenuRoot(ProjectGroupEditorMenuRootName);
  ProjectGroupMenuRoot:=S;
  PGEditMenuSectionFiles:=RegisterIDEMenuSection(S,'File');

  P:=RegisterIDEMenuSection(S,'Compile');
  PGEditMenuSectionCompile:=P;
  cmdTargetCompile:=RegisterIDEMenuCommand(P,'TargetCompile',lisTargetCompile);
  cmdTargetCompileClean:=RegisterIDEMenuCommand(P,'TargetCompileClean',lisTargetCompileClean);

  P:=RegisterIDEMenuSection(S,'AddRemove');
  PGEditMenuSectionAddRemove:=p;
  cmdTargetAdd:=RegisterIDEMenuCommand(P,'TargetAdd',lisTargetAdd);
  cmdTargetRemove:=RegisterIDEMenuCommand(P,'TargetRemove',lisTargetRemove);

  P:=RegisterIDEMenuSection(S,'Use');
  PGEditMenuSectionUse:=P;
  cmdTargetInstall:=RegisterIDEMenuCommand(P,'TargetInstall',lisTargetInstall);
  cmdTargetUninstall:=RegisterIDEMenuCommand(P,'TargetUninstall',lisTargetUninstall);
  cmdTargetEarlier:=RegisterIDEMenuCommand(P,'TargetEarlier',lisTargetEarlier);
  cmdTargetLater:=RegisterIDEMenuCommand(P,'TargetLater',lisTargetLater);
  cmdTargetActivate:=RegisterIDEMenuCommand(P,'TargetActivate',lisTargetActivate);
  cmdTargetOpen:=RegisterIDEMenuCommand(P,'TargetOpen',lisTargetOpen);
  cmdTargetRun:=RegisterIDEMenuCommand(P,'TargetRun',lisTargetRun);
  cmdTargetProperties:=RegisterIDEMenuCommand(P,'TargetProperties',lisTargetProperties);
end;

Procedure Register;

begin
  RegisterStandardProjectGroupMenuItems;
  IDEProjectGroupManager:=TIDEProjectGroupManager.Create;

  cmdCreateProjectGroup:=RegisterIDEMenuCommand(itmProjectNewSection,'NewProjectGroup',lisNewProjectGroup,@IDEProjectGroupManager.DoNewClick);
  cmdOpenProjectGroup:=RegisterIDEMenuCommand(itmProjectOpenSection,'OpenProjectGroup',lisOpenProjectGroup,@IDEProjectGroupManager.DoOpenClick);
  cmdSaveProjectGroup:=RegisterIDEMenuCommand(itmProjectSaveSection,'SaveProjectGroup',lisSaveProjectGroup,@IDEProjectGroupManager.DoSaveClick);
  cmdSaveProjectGroupAs:=RegisterIDEMenuCommand(itmProjectSaveSection,'SaveProjectGroupAs',lisSaveProjectGroupAs,@IDEProjectGroupManager.DoSaveAsClick);

  ProjectGroupManager:=IDEProjectGroupManager;
  SetProjectGroupEditorCallBack;
end;

end.

