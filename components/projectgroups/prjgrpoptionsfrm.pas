{ IDE options frame for project groups options

  Author: Mattias Gaertner
}
unit PrjGrpOptionsFrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  Forms, StdCtrls, Dialogs,
  // LazUtils
  LazFileCache, LazFileUtils, LazLoggerBase,
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf, ProjectGroup;

type

  { TProjGrpOptionsFrame }

  TProjGrpOptionsFrame = class(TAbstractIDEOptionsEditor)
    OpenLastGroupOnStartCheckBox: TCheckBox;
    ShowTargetPathsCheckBox: TCheckBox;
  private
    FLastOpenLastGroupOnStart, FLastShowTargetPaths: Boolean;
  public
    function GetTitle: String; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings({%H-}AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings({%H-}AOptions: TAbstractIDEOptions); override;
    procedure RestoreSettings({%H-}AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ TProjGrpOptionsFrame }

function TProjGrpOptionsFrame.GetTitle: String;
begin
  Result:='Project Groups';
end;

procedure TProjGrpOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  OpenLastGroupOnStartCheckBox.Caption:='Open last group on start';
  OpenLastGroupOnStartCheckBox.Hint:='On IDE start reopen last open group.';

  ShowTargetPathsCheckBox.Caption:='Show target paths';
  ShowTargetPathsCheckBox.Hint:='Enable to show target filenames with paths.';
end;

procedure TProjGrpOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  Opts: TIDEProjectGroupOptions;
begin
  Opts:=IDEProjectGroupManager.Options;

  FLastOpenLastGroupOnStart:=Opts.OpenLastGroupOnStart;
  OpenLastGroupOnStartCheckBox.Checked:=FLastOpenLastGroupOnStart;
  FLastShowTargetPaths:=Opts.ShowTargetPaths;
  ShowTargetPathsCheckBox.Checked:=FLastShowTargetPaths;
end;

procedure TProjGrpOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  Opts: TIDEProjectGroupOptions;
begin
  Opts:=IDEProjectGroupManager.Options;
  Opts.OpenLastGroupOnStart:=OpenLastGroupOnStartCheckBox.Checked;
  Opts.ShowTargetPaths:=ShowTargetPathsCheckBox.Checked;

  if Opts.Modified then begin
    Opts.SaveSafe;
    if IDEProjectGroupManager.OnEditorOptionsChanged<>nil then
      IDEProjectGroupManager.OnEditorOptionsChanged(true,true);
  end;
end;

procedure TProjGrpOptionsFrame.RestoreSettings(AOptions: TAbstractIDEOptions);
begin
  OpenLastGroupOnStartCheckBox.Checked:=FLastOpenLastGroupOnStart;
  ShowTargetPathsCheckBox.Checked:=FLastShowTargetPaths;
end;

class function TProjGrpOptionsFrame.
  SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result:=IDEEditorGroups.GetByIndex(GroupEnvironment)^.GroupClass;
end;

end.

