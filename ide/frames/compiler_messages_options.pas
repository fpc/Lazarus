unit compiler_messages_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, StdCtrls, CheckLst,
  Dialogs,
  IDEOptionsIntf, Project,
  LazarusIDEStrConsts,
  CompilerOptions;

type

  { TCompilerMessagesOptionsFrame }

  TCompilerMessagesOptionsFrame = class(TAbstractIDEOptionsEditor)
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

{ TCompilerMessagesOptionsFrame }

procedure TCompilerMessagesOptionsFrame.chkUseMsgFileChange(Sender: TObject);
begin
  UpdateMessages;
end;

procedure TCompilerMessagesOptionsFrame.chklistCompMsgClick(Sender: TObject);
var
  i : Integer;
begin
  for i := 0 to chklistCompMsg.Count - 1 do
    TCompilerMessageConfig(chklistCompMsg.Items.Objects[i]).Ignored := not chklistCompMsg.Checked[i];
end;

procedure TCompilerMessagesOptionsFrame.btnBrowseMsgClick(Sender: TObject);
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

procedure TCompilerMessagesOptionsFrame.UpdateMessages;
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

constructor TCompilerMessagesOptionsFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  TempMessages:=TCompilerMessagesList.Create;
end;

destructor TCompilerMessagesOptionsFrame.Destroy;
begin
  TempMessages.Free;
  inherited Destroy;
end;

function TCompilerMessagesOptionsFrame.GetTitle: String;
begin
  Result:=dlgCOCfgCmpMessages;
end;

procedure TCompilerMessagesOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  grpCompilerMessages.Caption := dlgCompilerMessage;
  chkUseMsgFile.Caption := dlgUseMsgFile;
  editMsgFileName.Caption := '';
end;

procedure TCompilerMessagesOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  chkUseMsgFile.OnChange := nil;
  try
    with AOptions as TProjectCompilerOptions do 
    begin
      chkUseMsgFile.Checked := UseMsgFile;
      editMsgFileName.Caption := MsgFileName;
      TempMessages.Assign(CompilerMessages);
      UpdateMessages;
    end;
  finally
    chkUseMsgFile.OnChange := @chkUseMsgFileChange;
  end;
end;

procedure TCompilerMessagesOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  with AOptions as TProjectCompilerOptions do
  begin
    UseMsgFile := chkUseMsgFile.Checked;
    MsgFileName := editMsgFileName.Caption;
    CompilerMessages.Assign(TempMessages);
  end;
end;

class function TCompilerMessagesOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TProjectCompilerOptions;
end;

initialization
  {$I compiler_messages_options.lrs}
  RegisterIDEOptionsEditor(GroupCompiler, TCompilerMessagesOptionsFrame, CompilerOptionsMessages);

end.

