unit compiler_messages_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, ListFilterEdit, StdCtrls, CheckLst, IDEOptionsIntf,
  LazarusIDEStrConsts, EnvironmentOpts, CompilerOptions, IDEMsgIntf;

type

  { TCompilerMessagesOptionsFrame }

  TCompilerMessagesOptionsFrame = class(TAbstractIDEOptionsEditor)
    chklistCompMsg: TCheckListBox;
    editMsgFilter: TListFilterEdit;
    grpCompilerMessages: TGroupBox;
    lblFilter: TLabel;
    procedure chklistCompMsgItemClick(Sender: TObject; Index: integer);
    function CheckItem(Item: TObject): Boolean;
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

constructor TCompilerMessagesOptionsFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  TempMessages:=TCompilerMessagesList.Create;
end;

destructor TCompilerMessagesOptionsFrame.Destroy;
begin
  editMsgFilter.Data.Clear;
  chklistCompMsg.Clear;
  chklistCompMsg.Items.Clear;
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
end;

procedure TCompilerMessagesOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
const     // todo: should be translated
  MsgTypeStr: array [TFPCErrorType] of String = ('-','H','N','W','E','F','P');
var
  topidx, i: Integer;
  m: TCompilerMessageConfig;
  s: String;
begin
  TempMessages.Assign((AOptions as TBaseCompilerOptions).CompilerMessages);
  topidx := chklistCompMsg.TopIndex;
  if FileExistsUTF8(EnvironmentOptions.CompilerMessagesFilename) then begin
    try
      // FPC messages file is expected to be UTF8 encoded, no matter for the current code page is
      TempMessages.LoadMsgFile(EnvironmentOptions.CompilerMessagesFilename);
    except
      TempMessages.SetDefault;
    end;
  end else
    TempMessages.SetDefault;

  // Copy data to filter component
  editMsgFilter.Data.Clear;
  for i := 0 to TempMessages.Count - 1 do
  begin
    m := TempMessages.Msg[i];
    if m.MsgType in [etNote, etHint, etWarning] then
    begin
      s := Format('(%s) %s', [MsgTypeStr[m.MsgType], m.GetUserText([])]);
      editMsgFilter.Data.AddObject(s, m);
    end;
  end;
  editMsgFilter.InvalidateFilter;
  chkListCompMsg.TopIndex := topidx;
end;

procedure TCompilerMessagesOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  with AOptions as TBaseCompilerOptions do
  begin
    UseMsgFile:=True;
    MsgFileName:=EnvironmentOptions.CompilerMessagesFilename;
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

