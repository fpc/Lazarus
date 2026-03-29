{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  Author: Juha Manninen

  Abstract:
    Main unit for Delphi conversion embedded in Lazarus IDE.
    Uses LCL and IdeIntf dependencies freely.
    ToDo: Turn this into a IDE plugin package.
}
unit ConvertMainPlugin;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  Forms, Controls, Dialogs, Buttons, ComCtrls,
  // LazUtils
  FileUtil, LazFileUtils, LazLoggerBase,
  // CodeTools
  CodeToolManager,
  // BuildIntf
  IDEExternToolIntf,
  // IdeIntf
  IDEMsgIntf, LazIDEIntf, IDEDialogs,
  // IdeConfig
  ProjPackCommon, IdeConfStrConsts,
  // IdeUtils
  InputHistory,
  // IdeProject
  Project,
  // IdePackager
  PackageDefs, BasePkgManager,
  // IDE
  LazarusIDEStrConsts, EditableProject,
  // Converter
  ConvertBase, ConvertDelphi, ConvertSettings, UsedUnits, MissingUnits;

type

  { TConvertSettingsPlugin }

  TConvertSettingsPlugin = class(TConvertSettingsGui)
  private
  public
    constructor Create(const ATitle: string); override;
    destructor Destroy; override;

    function BeginCodeTools: boolean; override;
    // Files
    function CloseEditorFile(Filename: string): TModalResult; override;
    function CloseEditorFile(AUnitInfo: TUnitInfo; Quiet: Boolean): TModalResult; override;
    function OpenEditorFile(AFileName: string): TModalResult; override;
    function SaveEditorFile(Filename: string): TModalResult; override;
    function SaveEditorFile(AUnitInfo: TUnitInfo; QuietUnitCheck: Boolean
      ): TModalResult; override;
    // Project
    function NewProject(AInfoFilename: string): iProjPack; override;
    function SaveProject(QuietUnitCheck: Boolean): TModalResult; override;

    function JumpToMessage: boolean; override;
    procedure JumpToCodeToolBossError; override;

    function CheckFailed(PrevResult: TModalResult; aFilename: string): TModalResult; override;
    function AskUnitPath(aConv: TConvertDelphiPBase; ATool: TUsedUnitsToolBase;
      AUnitName: string; ATargetDelphi: boolean): TModalResult; override;
    function MsgDialog(const aCaption, aMsg: string;
                DlgType: TMsgDlgType; Buttons: TMsgDlgButtons): Integer; override;
    procedure AddMessage(Urgency: TMessageLineUrgency; Msg: string;
      SrcFilename: string; LineNumber: integer; Column: integer); override;
    procedure ClearMessagesWin; override;
    procedure ProcessMessages; override;
    procedure BeginWaitCursor; override;
    procedure EndWaitCursor; override;
  end;

function RunConvertUnit: TModalResult;
function RunConvertProject: TModalResult;
function RunConvertPackage: TModalResult;


implementation

procedure InitConversion;
begin
  ConvertBase.TheSettingsClass:=TConvertSettingsPlugin;
  ConvertDelphi.UnitInfoClass:=TEditableUnitInfo;
end;

function RunConvertUnit: TModalResult;
var
  OpenDialog: TIDEOpenDialog;
  Converter: TConvertDelphiUnit;
  OldChange: Boolean;
begin
  Result:=mrCancel;
  InitConversion;
  OpenDialog:=IDEOpenDialogClass.Create(nil);
  try
    InputHistories.ApplyFileDialogSettings(OpenDialog);
    OpenDialog.Title:=lisChooseDelphiUnit;
    OpenDialog.Options:=OpenDialog.Options+[ofPathMustExist,ofFileMustExist,ofAllowMultiSelect];
    OpenDialog.Filter:=dlgFilterDelphiUnit+' (*.pas)|*.pas|'+
                       dlgFilterAll+' ('+GetAllFilesMask+')|' + GetAllFilesMask;
    if InputHistories.LastConvertDelphiUnit<>'' then begin
      OpenDialog.InitialDir:=ExtractFilePath(InputHistories.LastConvertDelphiUnit);
      OpenDialog.Filename  :=ExtractFileName(InputHistories.LastConvertDelphiUnit);
    end;
    if OpenDialog.Execute and (OpenDialog.Files.Count>0) then begin
      InputHistories.LastConvertDelphiUnit:=OpenDialog.Files[0];
      OldChange:=LazarusIDE.OpenEditorsOnCodeToolChange;
      LazarusIDE.OpenEditorsOnCodeToolChange:=true;
      Converter:=TConvertDelphiUnit.Create(OpenDialog.Files);
      try
        Result:=Converter.Convert;
      finally
        Converter.Free;
        LazarusIDE.OpenEditorsOnCodeToolChange:=OldChange;
      end;
      InputHistories.StoreFileDialogSettings(OpenDialog);
    end;
  finally
    OpenDialog.Free;
  end;
end;

function RunConvertProject: TModalResult;
var
  OpenDialog: TIDEOpenDialog;
  Converter: TConvertDelphiProject;
  FN: string;
  OldChange: Boolean;
begin
  Result:=mrCancel;
  InitConversion;
  OpenDialog:=IDEOpenDialogClass.Create(nil);
  try
    InputHistories.ApplyFileDialogSettings(OpenDialog);
    OpenDialog.Title:=lisChooseDelphiProject;
    OpenDialog.Options:=OpenDialog.Options+[ofPathMustExist,ofFileMustExist];
    OpenDialog.Filter:=dlgFilterDelphiProject+' (*.dpr)|*.dpr|'+
                       dlgFilterLazarusProject+' (*.lpr)|*.lpr|'+
                       dlgFilterAll+' ('+GetAllFilesMask+')|' + GetAllFilesMask;
    if InputHistories.LastConvertDelphiProject<>'' then begin
      OpenDialog.InitialDir:=ExtractFilePath(InputHistories.LastConvertDelphiProject);
      OpenDialog.Filename  :=ExtractFileName(InputHistories.LastConvertDelphiProject);
    end;
    if OpenDialog.Execute then begin
      FN:=CleanAndExpandFilename(OpenDialog.Filename);
      if FileExistsUTF8(FN) then begin
        InputHistories.LastConvertDelphiProject:=FN;
        OldChange:=LazarusIDE.OpenEditorsOnCodeToolChange;
        LazarusIDE.OpenEditorsOnCodeToolChange:=true;
        debugln(['RunConvertProject Old CodeToolChange=', OldChange]);
        Converter:=TConvertDelphiProject.Create(FN);
        try
          Result:=Converter.Convert;
        finally
          Converter.Free;
          LazarusIDE.OpenEditorsOnCodeToolChange:=OldChange;
        end;
        InputHistories.StoreFileDialogSettings(OpenDialog);
      end;
    end;
  finally
    OpenDialog.Free;
  end;
end;

function RunConvertPackage: TModalResult;
var
  OpenDialog: TIDEOpenDialog;
  Converter: TConvertDelphiPackage;
  FN: string;
  OldChange: Boolean;
begin
  Result:=mrCancel;
  InitConversion;
  OpenDialog:=IDEOpenDialogClass.Create(nil);
  try
    InputHistories.ApplyFileDialogSettings(OpenDialog);
    OpenDialog.Title:=lisChooseDelphiPackage;
    OpenDialog.Options:=OpenDialog.Options+[ofPathMustExist,ofFileMustExist];
    OpenDialog.Filter:=dlgFilterDelphiPackage+' (*.dpk)|*.dpk|'+
                       dlgFilterAll+' ('+GetAllFilesMask+')|' + GetAllFilesMask;
    if InputHistories.LastConvertDelphiPackage<>'' then begin
      OpenDialog.InitialDir:=ExtractFilePath(InputHistories.LastConvertDelphiPackage);
      OpenDialog.Filename  :=ExtractFileName(InputHistories.LastConvertDelphiPackage);
    end;
    if OpenDialog.Execute then begin
      FN:=CleanAndExpandFilename(OpenDialog.Filename);
      if FileExistsUTF8(FN) then begin
        InputHistories.LastConvertDelphiPackage:=FN;
        OldChange:=LazarusIDE.OpenEditorsOnCodeToolChange;
        LazarusIDE.OpenEditorsOnCodeToolChange:=true;
        Converter:=TConvertDelphiPackage.Create(FN);
        try
          Result:=Converter.Convert;
        finally
          Converter.Free;
          LazarusIDE.OpenEditorsOnCodeToolChange:=OldChange;
        end;
        InputHistories.StoreFileDialogSettings(OpenDialog);
      end;
    end;
  finally
    OpenDialog.Free;
  end;
end;

{ TConvertSettingsPlugin }

constructor TConvertSettingsPlugin.Create(const ATitle: string);
begin
  inherited Create(ATitle);
end;

destructor TConvertSettingsPlugin.Destroy;
begin
  inherited Destroy;
end;

function TConvertSettingsPlugin.BeginCodeTools: boolean;
begin
  Result:=LazarusIDE.BeginCodeTools;
end;

function TConvertSettingsPlugin.CloseEditorFile(Filename: string): TModalResult;
begin
  Result:=LazarusIDE.DoCloseEditorFile(Filename,[cfSaveFirst]);
end;

function TConvertSettingsPlugin.CloseEditorFile(AUnitInfo: TUnitInfo;
  Quiet: Boolean): TModalResult;
var
  Flags: TCloseFlags;
begin
  if Quiet then
    Flags:=[cfQuiet]
  else
    Flags:=[];
  Result:=LazarusIDE.DoCloseEditorFile(
           TEditableUnitInfo(AUnitInfo).OpenEditorInfo[0].EditorComponent, Flags);
end;

function TConvertSettingsPlugin.OpenEditorFile(AFileName: string): TModalResult;
begin
  Result:=LazarusIDE.DoOpenEditorFile(AFileName,0,0,[ofQuiet]);
end;

function TConvertSettingsPlugin.SaveEditorFile(Filename: string): TModalResult;
begin
  Result:=LazarusIDE.DoSaveEditorFile(Filename,[]);
end;

function TConvertSettingsPlugin.SaveEditorFile(AUnitInfo: TUnitInfo;
  QuietUnitCheck: Boolean): TModalResult;
var
  Flags: TSaveFlags;
begin
  if QuietUnitCheck then
    Flags:=[sfCheckAmbiguousFiles,sfQuietUnitCheck]
  else
    Flags:=[];
  Result:=LazarusIDE.DoSaveEditorFile(
           TEditableUnitInfo(AUnitInfo).OpenEditorInfo[0].EditorComponent, Flags);
end;

function TConvertSettingsPlugin.NewProject(AInfoFilename: string): iProjPack;
var
  Desc: TConvertedDelphiProjectDescriptor;
begin
  // create a new lazarus project
  Desc:=TConvertedDelphiProjectDescriptor.Create;
  try
    LazarusIDE.DoNewProject(Desc);
  finally
    Desc.Free;
  end;
  Assert(Assigned(Project1), 'TConvertSettingsPlugin.NewProject: Project1=Nil');
  Project1.ProjectInfoFile:=AInfoFilename;
  Result:=Project1;
end;

function TConvertSettingsPlugin.SaveProject(QuietUnitCheck: Boolean): TModalResult;
var
  Flags: TSaveFlags;
begin
  if QuietUnitCheck then
    Flags:=[sfQuietUnitCheck]
  else
    Flags:=[];
  Result:=LazarusIDE.DoSaveProject(Flags);
end;

function TConvertSettingsPlugin.JumpToMessage: boolean;
begin
  Result:=LazarusIDE.DoJumpToCompilerMessage(true);
end;

procedure TConvertSettingsPlugin.JumpToCodeToolBossError;
begin
  LazarusIDE.DoJumpToCodeToolBossError;
end;

function TConvertSettingsPlugin.CheckFailed(PrevResult: TModalResult;
  aFilename: string): TModalResult;
begin
  Result:=PrevResult;
  if Result=mrCancel then begin
    Result:=QuestionDlg(lisConvDelphiFailedConvertingUnit,
        Format(lisConvDelphiFailedToConvertUnit, [aFilename]),
        mtWarning, [mrIgnore, lisIgnoreAndContinue, mrAbort], 0);
    case Result  of
      mrIgnore : Result:=mrOK;
      mrAbort : // ToDo: fOwnerConverter.ErrorMsg:=Format(lisConvUserSelectedToEndConversion,
                //                                 [aFilename]);
    end;
  end;
end;

function TConvertSettingsPlugin.AskUnitPath(aConv: TConvertDelphiPBase;
  ATool: TUsedUnitsToolBase; AUnitName: string; ATargetDelphi: boolean): TModalResult;
// Ask the user what to do with missing units.
var
  TryAgain: Boolean;
  CacheUnitsThread: TCacheUnitsThread;
  UnitDirDialog: TSelectDirectoryDialog;
begin
  repeat
    TryAgain:=False;
    Result:=AskMissingUnits(TUsedUnitsTool(ATool).MainUsedUnits.MissingUnits,
                            TUsedUnitsTool(ATool).ImplUsedUnits.MissingUnits,
                            AUnitName, ATargetDelphi);
    case Result of
      // mrOK means: Comment out.
      mrOK:
        TUsedUnitsTool(ATool).MoveMissingToComment(aConv.AllCommentedUnits);
      // mrYes means: Search for unit path.
      mrYes: begin
        UnitDirDialog:=TSelectDirectoryDialog.Create(nil);
        try
          UnitDirDialog.InitialDir:=aConv.PrevSelectedPath;
          UnitDirDialog.Title:=lisConvDelphiAllSubDirsScanned;
          if UnitDirDialog.Execute then
          begin
            aConv.PrevSelectedPath:=UnitDirDialog.FileName;
            // Add the new path to project if missing units are found.
            // We use a thread here only to reuse its code. No parallel operations now.
            CacheUnitsThread:=TCacheUnitsThread.Create(aConv, aConv.PrevSelectedPath);
            CacheUnitsThread.Start;
            CacheUnitsThread.WaitFor; // Make sure the thread has finished before continuing.
            TryAgain:=aConv.DoMissingUnits(ATool)>0;
          end
          else
            TryAgain:=true;  // User canceled. Stay with the same unit.
        finally
          UnitDirDialog.Free;
        end;
        Result:=mrOK;        // Caller will check for Result<>mrOK
      end;
      // mrIgnore means: Skip this unit. The missing units list is already cleared.
      mrIgnore:
        Result:=mrOK;
      // Abort the whole conversion.
      mrAbort:
        ;
    end;
  until not TryAgain;
end;

function TConvertSettingsPlugin.MsgDialog(const aCaption, aMsg: string;
  DlgType: TMsgDlgType; Buttons: TMsgDlgButtons): Integer;
begin
  Result:=IDEMessageDialog(aCaption, aMsg, DlgType, Buttons);
end;

procedure TConvertSettingsPlugin.AddMessage(Urgency: TMessageLineUrgency;
  Msg: string; SrcFilename: string; LineNumber: integer; Column: integer);
begin
  IDEMessagesWindow.AddCustomMessage(Urgency, Msg, SrcFilename, LineNumber, Column);
end;

procedure TConvertSettingsPlugin.ClearMessagesWin;
begin
  IDEMessagesWindow.Clear;
end;

procedure TConvertSettingsPlugin.ProcessMessages;
begin
  Application.ProcessMessages;
end;

procedure TConvertSettingsPlugin.BeginWaitCursor;
begin
  Screen.BeginWaitCursor;
end;

procedure TConvertSettingsPlugin.EndWaitCursor;
begin
  Screen.EndWaitCursor;
end;


end.

