unit uIntf;

{
 **********************************************************************
  This file is part of a Lazarus Package, Examples Window.

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 **********************************************************************

This unit provides the interface between Lazarus and the Package.

}

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils,
    // LazUtils
    LazFileUtils, LazConfigStorage, LazLoggerBase,
    // LCL,
    LCLType,
    // BuildIntf
    BaseIDEIntf, IDEOptionsIntf,
    // IdeIntf
    LazIDEIntf, MenuIntf, IDECommands, ToolBarIntf, IDEOptEditorIntf;

procedure Register;

implementation

uses
    uLaz_Examples, uConst, ExWinSettings;

// Note : IDEEnvironmentOptions.GetParsedLazarusDirectory is the Lazarus STC tree.

function GetExamplesHomeDir() : string;
var
  Config: TConfigStorage;
begin
    try
      Config := GetIDEConfigStorage(cConfigFileName, true);
      try
        Result := Config.GetValue('Examples/Directory',
            AppendPathDelim(LazarusIDE.GetPrimaryConfigPath));
            // + AppendPathDelim(cExamplesDir));

      finally
        Config.Free;
      end;
    except
      on E: Exception do begin
        DebugLn('Examples UIntf GetExamplesDirectory Loading ' +  cConfigFileName + ' failed: ' + E.Message);
        Result := IDEEnvironmentOptions.GetParsedLazarusDirectory;
      end;
    end;

end;

procedure IDEMenuSectionClicked(Sender: TObject);
var
  ProjectFFile : string;
begin
  FormLazExam := TFormLazExam.Create(nil);
  try
    FormLazExam.ExamplesHome := GetExamplesHomeDir();
    FormLazExam.RemoteRepo := cRemoteRepository;
    FormLazExam.LazConfigDir := AppendPathDelim(LazarusIDE.GetPrimaryConfigPath);
    FormLazExam.ShowModal;
    ProjectFFile := FormLazExam.ProjectToOpen;
  finally
    FormLazExam.Free;
    FormLazExam := nil;
  end;
  if ProjectFFile <> '' then
    LazarusIDE.DoOpenProjectFile(ProjectFFile, [ofProjectLoading]);
end;

procedure Register;
var
  IDEShortCutX: TIDEShortCut;
  IDECommandCategory: TIDECommandCategory;
  IDECommand: TIDECommand;
begin
  IDEShortCutX := IDEShortCut(VK_E, [ssCtrl, ssAlt], VK_UNKNOWN, []);
  IDECommandCategory := IDECommandList.FindCategoryByName('ToolMenu');
  IDECommand := nil;
  if IDECommandCategory <> nil then
  begin
    IDECommand := RegisterIDECommand(IDECommandCategory, rsExampleProjects, rsExampleProjects, IDEShortCutX, nil, @IDEMenuSectionClicked);
    if IDECommand <> nil then
      RegisterIDEButtonCommand(IDECommand);
  end;
  RegisterIDEMenuCommand(itmSecondaryTools, rsExampleProjects, rsExampleProjects + ' ...', nil, @IDEMenuSectionClicked, IDECommand, 'pkg_oep');
  RegisterIDEMenuCommand(ComponentPalettePageDropDownExtraEntries, rsExampleProjects, rsExampleProjects + ' ...', nil, @IDEMenuSectionClicked, nil, 'pkg_oep');

  ExWinOptionsFrameID := RegisterIDEOptionsEditor(ExWindowOptionsGroup, TExWinSettingsFrame, 9999)^.Index;  // AIndex = what ???

end;

initialization


finalization


end.

