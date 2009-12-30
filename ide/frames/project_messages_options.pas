unit project_messages_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, StdCtrls, CheckLst,
  Dialogs,
  IDEOptionsIntf, Project,
  LazarusIDEStrConsts,
  CompilerOptions;

type

  { TProjectMessagesOptionsFrame }

  TProjectMessagesOptionsFrame = class(TAbstractIDEOptionsEditor)
    btnBrowseMsg: TButton;
    chklistCompMsg: TCheckListBox;
    chkUseMsgFile: TCheckBox;
    editMsgFileName: TEdit;
    grpCompilerMessages: TGroupBox;
    procedure btnBrowseMsgClick(Sender: TObject);
    procedure chklistCompMsgClick(Sender: TObject);
    procedure chkUseMsgFileChange(Sender: TObject);
  private
    { private declarations }
    TempMessages: TCompilerMessagesList;
    procedure UpdateMessages;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    function GetTitle: String; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{ TProjectMessagesOptionsFrame }

procedure TProjectMessagesOptionsFrame.chkUseMsgFileChange(Sender: TObject);
begin
  UpdateMessages;
end;

procedure TProjectMessagesOptionsFrame.chklistCompMsgClick(Sender: TObject);
var
  i : Integer;
begin
  for i := 0 to chklistCompMsg.Count - 1 do
    TCompilerMessageConfig(chklistCompMsg.Items.Objects[i]).Ignored := not chklistCompMsg.Checked[i];
end;

procedure TProjectMessagesOptionsFrame.btnBrowseMsgClick(Sender: TObject);
var
  dlg : TOpenDialog;
begin
  dlg := TOpenDialog.Create(Self);
  try
    dlg.Filter := dlgBrowseMsgFilter;
    if not dlg.Execute then Exit;
    editMsgFileName.Caption := dlg.FileName;
    UpdateMessages;
  finally
    dlg.Free
  end;
end;

procedure TProjectMessagesOptionsFrame.UpdateMessages;
const
  MaxIndexLen = 5;
var
  i : Integer;
  j : Integer;
  topidx  : Integer;
  m : TCompilerMessageConfig;


  function IntToStrLen(idx, strlen: integer): string;
  var
    s : string;
  begin
    Result := IntToStr(idx);
    if length(Result) < strlen then  begin
      SetLength(s, strlen - length(Result));
      FillChar(s[1], length(s), '0');
      Result := s + Result;
    end;
  end;

begin
  topidx := chklistCompMsg.TopIndex;
  chklistCompMsg.Items.BeginUpdate;
  try
    if chkUseMsgFile.Checked and FileExistsUTF8(editMsgFileName.Caption) and (editMsgFileName.Caption <> '') then begin
      try
        // FPC messages file is expected to be UTF8 encoded, no matter for the current code page is
        TempMessages.LoadMsgFile(editMsgFileName.Caption, true);
      except
        TempMessages.SetDefault;
      end;
    end else
      TempMessages.SetDefault;

    chklistCompMsg.Clear;
    chklistCompMsg.Items.Clear;
    for i := 0 to TempMessages.Count - 1 do begin
      m := TempMessages.Msg[i];
      j := chklistCompMsg.Items.AddObject( Format('(%s) %s', [m.MsgType, m.GetUserText]), m);
      chklistCompMsg.Checked[j] := not m.Ignored;
    end;

  finally
    chklistCompMsg.Items.EndUpdate;
    chkListCompMsg.TopIndex := topidx;
  end;
end;

constructor TProjectMessagesOptionsFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  TempMessages:=TCompilerMessagesList.Create;
end;

destructor TProjectMessagesOptionsFrame.Destroy;
begin
  TempMessages.Free;
  inherited Destroy;
end;

function TProjectMessagesOptionsFrame.GetTitle: String;
begin
  Result:=dlgCOCfgCmpMessages;
end;

procedure TProjectMessagesOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  grpCompilerMessages.Caption := dlgCompilerMessage;
  chkUseMsgFile.Caption := dlgUseMsgFile;
  editMsgFileName.Caption := '';
end;

procedure TProjectMessagesOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  chkUseMsgFile.OnChange := nil;
  try
    with AOptions as TProject do begin
      chkUseMsgFile.Checked := CompilerOptions.UseMsgFile;
      editMsgFileName.Caption := CompilerOptions.MsgFileName;
      TempMessages.Assign(CompilerOptions.CompilerMessages);
      UpdateMessages;
    end;
  finally
    chkUseMsgFile.OnChange := @chkUseMsgFileChange;
  end;
end;

procedure TProjectMessagesOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  with AOptions as TProject do
  begin
    CompilerOptions.UseMsgFile := chkUseMsgFile.Checked;
    CompilerOptions.MsgFileName := editMsgFileName.Caption;
    CompilerOptions.CompilerMessages.Assign(TempMessages);
  end;
end;

class function TProjectMessagesOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result:=TProject;
end;

initialization
  {$I project_messages_options.lrs}
  RegisterIDEOptionsEditor(GroupCompiler, TProjectMessagesOptionsFrame, 500{todo: CompilerOptionsMessages});

end.

