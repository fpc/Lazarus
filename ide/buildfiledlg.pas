unit BuildFileDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, LCLType, Forms, Controls, Graphics, Dialogs,
  Buttons, ExtCtrls, StdCtrls;

type
  TBuildFileDialog = class(TForm)
    AlwaysCompileFirstCheckbox: TCHECKBOX;
    CancelButton: TBUTTON;
    CompileAddMacroButton: TBUTTON;
    CompileCommandGroupbox: TGROUPBOX;
    CompileCommandMemo: TMEMO;
    CompileMacrosGroupbox: TGROUPBOX;
    CompileMacrosListbox: TLISTBOX;
    CompilePage: TPAGE;
    CompileScanForFPCMsgCheckbox: TCHECKBOX;
    CompScanForMakeMsgCheckbox: TCHECKBOX;
    GeneralPage: TPAGE;
    Notebook1: TNOTEBOOK;
    OkButton: TBUTTON;
    OverrideBuildProjectCheckbox: TCHECKBOX;
    OverrideRunProjectCheckbox: TCHECKBOX;
    RunAddMacroButton: TBUTTON;
    RunCommandGroupbox: TGROUPBOX;
    RunCommandMemo: TMEMO;
    RunMacrosGroupbox: TGROUPBOX;
    RunMacrosListbox: TLISTBOX;
    RunPage: TPAGE;
    RunScanForFPCMsgCheckbox: TCHECKBOX;
    RunScanForMakeMsgCheckbox: TCHECKBOX;
    procedure BuildFileDialogKEYDOWN(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
  public
  end;

var
  BuildFileDialog: TBuildFileDialog;

implementation

{ TBuildFileDialog }

procedure TBuildFileDialog.BuildFileDialogKEYDOWN(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key=VK_ESCAPE then ModalResult:=mrCancel;
end;

initialization
  {$I buildfiledlg.lrs}

end.

