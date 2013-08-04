unit compiler_messages_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, ListFilterEdit, StdCtrls, CheckLst, Dialogs,
  IDEOptionsIntf, IDEMsgIntf, IDEExternToolIntf,
  LazarusIDEStrConsts, CompilerOptions, IDEDialogs;

type

  { TCompilerMessagesOptionsFrame }

  TCompilerMessagesOptionsFrame = class(TAbstractIDEOptionsEditor)
    chklistCompMsg: TCheckListBox;
    editMsgFilter: TListFilterEdit;
    grpCompilerMessages: TGroupBox;
    lblFilter: TLabel;
    MsgFileBrowseButton: TButton;
    MsgFileEdit: TEdit;
    UseMsgFileCheckBox: TCheckBox;
    procedure chklistCompMsgItemClick(Sender: TObject; Index: integer);
    function CheckItem(Item: TObject): Boolean;
    procedure MsgFileBrowseButtonClick(Sender: TObject);
    procedure UseMsgFileCheckBoxChange(Sender: TObject);
  private
    TempMessages: TCompilerMessagesList;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    function GetTitle: String; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ TCompilerMessagesOptionsFrame }

procedure TCompilerMessagesOptionsFrame.chklistCompMsgItemClick(Sender: TObject; Index: integer);
const
  BoolToMessageState: array[Boolean] of TCompilerMessageState = (msOff, msOn);
var
  m: TCompilerMessageConfig;
begin
  if (Index >= 0) and (Index < chklistCompMsg.Items.Count) then begin
    m := TCompilerMessageConfig(chklistCompMsg.Items.Objects[Index]);
    if (m.DefIgnored <> chklistCompMsg.Checked[Index]) then
      m.State := msDefault
    else
      m.State := BoolToMessageState[chklistCompMsg.Checked[Index]];
  end;
end;

function TCompilerMessagesOptionsFrame.CheckItem(Item: TObject): Boolean;
var
  m: TCompilerMessageConfig;
begin
  m := Item as TCompilerMessageConfig;
  if m.State = msDefault then
    Result := not m.DefIgnored
  else
    Result := m.State = msOn;
end;

procedure TCompilerMessagesOptionsFrame.MsgFileBrowseButtonClick(Sender: TObject
  );
var
  OpenDialog: TOpenDialog;
begin
  OpenDialog:=TOpenDialog.Create(nil);
  try
    InitIDEFileDialog(OpenDialog);
    OpenDialog.Title:=lisChooseAnFPCMessageFile;
    OpenDialog.Options:=OpenDialog.Options+[ofFileMustExist];
    OpenDialog.Filter:=lisFPCMessageFile+' (*.msg)|*.msg|'+dlgAllFiles+'|'+
      GetAllFilesMask;
    if OpenDialog.Execute then
      MsgFileEdit.Text:=OpenDialog.FileName;
  finally
    OpenDialog.Free;
  end;
end;

procedure TCompilerMessagesOptionsFrame.UseMsgFileCheckBoxChange(Sender: TObject);
begin
  MsgFileEdit.Enabled:=UseMsgFileCheckBox.Checked;
  MsgFileBrowseButton.Enabled:=UseMsgFileCheckBox.Checked;
end;

constructor TCompilerMessagesOptionsFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  TempMessages:=TCompilerMessagesList.Create;
end;

destructor TCompilerMessagesOptionsFrame.Destroy;
begin
  editMsgFilter.Items.Clear;
  chklistCompMsg.Clear;
  TempMessages.Free;
  inherited Destroy;
end;

function TCompilerMessagesOptionsFrame.GetTitle: String;
begin
  Result:=dlgCOCfgCmpMessages;
end;

procedure TCompilerMessagesOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  grpCompilerMessages.Caption:=dlgCompilerMessage;
  lblFilter.Caption:=lisFilter;
  UseMsgFileCheckBox.Caption:=lisUseMessageFile;
  MsgFileBrowseButton.Caption:=lisPathEditBrowse;
end;

procedure TCompilerMessagesOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  topidx, i: Integer;
  m: TCompilerMessageConfig;
  CompOpts: TBaseCompilerOptions;
begin
  CompOpts:=AOptions as TBaseCompilerOptions;
  TempMessages.Assign(CompOpts.CompilerMessages);
  topidx := chklistCompMsg.TopIndex;
  UseMsgFileCheckBox.Checked:=CompOpts.UseMsgFile;
  MsgFileEdit.Text:=CompOpts.MsgFileName;
  MsgFileEdit.Enabled:=UseMsgFileCheckBox.Checked;
  MsgFileBrowseButton.Enabled:=UseMsgFileCheckBox.Checked;

  // Copy data to filter component
  editMsgFilter.Items.Clear;
  for i := 0 to TempMessages.Count - 1 do
  begin
    m := TempMessages.Msg[i];
    case m.MsgType of
    {$IFDEF EnableNewExtTools}mluHint{$ELSE}etHint{$ENDIF}:
      editMsgFilter.Items.AddObject('(H) '+m.MsgText, m);
    {$IFDEF EnableNewExtTools}mluNote{$ELSE}etNote{$ENDIF}:
      editMsgFilter.Items.AddObject('(N) '+m.MsgText, m);
    {$IFDEF EnableNewExtTools}mluWarning{$ELSE}etWarning{$ENDIF}:
      editMsgFilter.Items.AddObject('(W) '+m.MsgText, m);
    end;
  end;
  editMsgFilter.InvalidateFilter;
  chkListCompMsg.TopIndex := topidx;
end;

procedure TCompilerMessagesOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  with AOptions as TBaseCompilerOptions do
  begin
    UseMsgFile:=UseMsgFileCheckBox.Checked;
    MsgFileName:=MsgFileEdit.Text;
    CompilerMessages.Assign(TempMessages);
  end;
end;

class function TCompilerMessagesOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result:=TBaseCompilerOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupCompiler, TCompilerMessagesOptionsFrame, CompilerOptionsMessages);
  RegisterIDEOptionsEditor(GroupPkgCompiler, TCompilerMessagesOptionsFrame, CompilerOptionsMessages);

end.

