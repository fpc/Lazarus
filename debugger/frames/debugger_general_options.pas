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
}
unit debugger_general_options;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  // LCL
  Controls, StdCtrls, ExtCtrls, Dialogs, Menus,
  // LazUtils
  FileUtil, LazFileUtils,
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf,
  // IdeConfig
  SearchPathProcs, EnvironmentOpts, IDEProcs,
  // IdeDebugger
  BaseDebugManager, IdeDebuggerOpts, EnvDebuggerOptions,
  // IDE
  LazarusIDEStrConsts, PathEditorDlg;

type

  { TDebuggerGeneralOptionsFrame }

  TDebuggerGeneralOptionsFrame = class(TAbstractIDEOptionsEditor)
    cmdOpenAdditionalPath: TButton;
    gbAdditionalSearchPath: TGroupBox;
    gcbDebuggerGeneralOptions: TCheckGroup;
    gcbDebuggerDialogSettings: TCheckGroup;
    txtAdditionalPath: TEdit;
    procedure cmdOpenAdditionalPathClick(Sender: TObject);
  private
    fOldDebuggerSearchPath: string;
    procedure FetchDebuggerGeneralOptions;
  public
    function Check: Boolean; override;
    function GetTitle: String; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings({%H-}AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings({%H-}AOptions: TAbstractIDEOptions); override;
    procedure RestoreSettings({%H-}AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ TDebuggerGeneralOptionsFrame }

procedure TDebuggerGeneralOptionsFrame.cmdOpenAdditionalPathClick(
  Sender: TObject);
begin
  PathEditorDialog.Path:=txtAdditionalPath.Text;
  PathEditorDialog.Templates:=GetForcedPathDelims(
        '$(LazarusDir)/include/$(TargetOS)'
      +';$(FPCSrcDir)/rtl/inc/'
      +';$(FPCSrcDir)/rtl/$(SrcOS)'
      +';$(FPCSrcDir)/rtl/$(TargetOS)'
      );
  if PathEditorDialog.ShowModal=mrOk then
    txtAdditionalPath.Text:=PathEditorDialog.Path;
end;

procedure TDebuggerGeneralOptionsFrame.FetchDebuggerGeneralOptions;
begin
  with EnvironmentDebugOpts do
  begin
    // IMPORTANT if more items are added the indexes must be updated here!
    gcbDebuggerGeneralOptions.Checked[0] := DebuggerResetAfterRun;
    gcbDebuggerGeneralOptions.Checked[1] := DebuggerAutoCloseAsm;
    gcbDebuggerGeneralOptions.Checked[2] := DebuggerAutoSetInstanceFromClass;
    gcbDebuggerGeneralOptions.Checked[3] := DebuggerAllowFunctionCalls;
    gcbDebuggerGeneralOptions.Checked[4] := DebuggerOptions.AlwaysBringDbgDialogsToFront;

    gcbDebuggerDialogSettings.Checked[0] := DebuggerShowStopMessage;
    gcbDebuggerDialogSettings.Checked[1] := DebuggerShowExitCodeMessage;
    gcbDebuggerDialogSettings.Checked[2] := ConfirmDeleteAllWatches;
    gcbDebuggerDialogSettings.Checked[3] := ConfirmDeleteAllBreakPoints;
    gcbDebuggerDialogSettings.Checked[4] := ConfirmDeleteFileBreakPoints;
    gcbDebuggerDialogSettings.Checked[5] := ConfirmDeleteAllHistory;
  end;
  txtAdditionalPath.Text:=EnvironmentOptions.GetParsedDebuggerSearchPath;
end;

function TDebuggerGeneralOptionsFrame.Check: Boolean;
begin
  Result := true;
end;

function TDebuggerGeneralOptionsFrame.GetTitle: String;
begin
  Result := lisGeneral;
end;

procedure TDebuggerGeneralOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  gbAdditionalSearchPath.Caption := lisDebugOptionsFrmAdditionalSearchPath;
  gcbDebuggerGeneralOptions.Caption := lisDebugOptionsFrmDebuggerGeneralOptions;
  gcbDebuggerGeneralOptions.Items.Add(lisDebugOptionsFrmResetDebuggerOnEachRun); // 0 reset dbg after each run
  gcbDebuggerGeneralOptions.Items.Add(lisDebugOptionsFrmAutoCloseAsm);           // 1 auto close asm
  gcbDebuggerGeneralOptions.Items.Add(lisDebugOptionsFrmAutoInstanceClass);      // 2 auto set class-from-instance
  gcbDebuggerGeneralOptions.Items.Add(lisDebugOptionsFrmAllowFunctionCalls);     // 3 allow function calls
  gcbDebuggerGeneralOptions.Items.Add(lisDebugOptionsFrmDialogsToFront);         // 4 bring dialogs to front

  gcbDebuggerDialogSettings.Caption := lisDebugOptionsFrmDebuggerDialogSettings;
  gcbDebuggerDialogSettings.Items.Add(lisDebugOptionsFrmShowMessageOnStop);      // 0 Message on stop
  gcbDebuggerDialogSettings.Items.Add(lisDebugOptionsFrmShowExitCodeOnStop);     // 1 Exit-code on stop
  gcbDebuggerDialogSettings.Items.Add(lisDebugDialogConfirmDelWatches);          // Confirm delete watches
  gcbDebuggerDialogSettings.Items.Add(lisDebugDialogConfirmDelBreaks);           // Confirm delete breakpoints
  gcbDebuggerDialogSettings.Items.Add(lisDebugDialogConfirmDelBreaksFile);       // Confirm delete breakpoints in same file
  gcbDebuggerDialogSettings.Items.Add(lisDebugDialogConfirmDelHistory);          // Confirm delete history
end;

procedure TDebuggerGeneralOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  with EnvironmentOptions do
  begin
    fOldDebuggerSearchPath := DebuggerSearchPath;

    FetchDebuggerGeneralOptions;
  end;
end;

procedure TDebuggerGeneralOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  EnvironmentOptions.DebuggerSearchPath := TrimSearchPath(txtAdditionalPath.Text,'');
  with EnvironmentDebugOpts do
  begin
    // IMPORTANT if more items are added the indexes must be updated here!
    DebuggerResetAfterRun            := gcbDebuggerGeneralOptions.Checked[0];
    DebuggerAutoCloseAsm             := gcbDebuggerGeneralOptions.Checked[1];
    DebuggerAutoSetInstanceFromClass := gcbDebuggerGeneralOptions.Checked[2];
    DebuggerAllowFunctionCalls       := gcbDebuggerGeneralOptions.Checked[3];
    DebuggerOptions.AlwaysBringDbgDialogsToFront := gcbDebuggerGeneralOptions.Checked[4];

    DebuggerShowStopMessage          := gcbDebuggerDialogSettings.Checked[0];
    DebuggerShowExitCodeMessage      := gcbDebuggerDialogSettings.Checked[1];
    ConfirmDeleteAllWatches          := gcbDebuggerDialogSettings.Checked[2];
    ConfirmDeleteAllBreakPoints      := gcbDebuggerDialogSettings.Checked[3];
    ConfirmDeleteFileBreakPoints     := gcbDebuggerDialogSettings.Checked[4];
    ConfirmDeleteAllHistory          := gcbDebuggerDialogSettings.Checked[5];
  end;
end;

procedure TDebuggerGeneralOptionsFrame.RestoreSettings(
  AOptions: TAbstractIDEOptions);
begin
  with EnvironmentOptions do begin
    DebuggerSearchPath := fOldDebuggerSearchPath;
  end;
end;

class function TDebuggerGeneralOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TDebuggerOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupDebugger, TDebuggerGeneralOptionsFrame, DbgOptionsGeneral);
end.

