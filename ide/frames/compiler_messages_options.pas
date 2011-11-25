unit compiler_messages_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, StdCtrls, CheckLst, LCLProc,
  Dialogs,
  IDEOptionsIntf, Project,
  LazarusIDEStrConsts,
  EnvironmentOpts, CompilerOptions, IDEMsgIntf;

type

  { TCompilerMessagesOptionsFrame }

  TCompilerMessagesOptionsFrame = class(TAbstractIDEOptionsEditor)
    chklistCompMsg: TCheckListBox;
    editMsgFilter: TEdit;
    grpCompilerMessages: TGroupBox;
    lblFilter: TLabel;
    procedure btnBrowseMsgClick(Sender: TObject);
    procedure chklistCompMsgItemClick(Sender: TObject; Index: integer);
    procedure chkUseMsgFileChange(Sender: TObject);
    procedure editMsgFilterChange(Sender: TObject);
  private
    fLoaded: Boolean;
    FSaved: Boolean;
    { private declarations }
    TempMessages: TCompilerMessagesList;
    procedure UpdateMessages;
    procedure UpdateFilter;
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

{$R *.lfm}

{ TCompilerMessagesOptionsFrame }

procedure TCompilerMessagesOptionsFrame.chkUseMsgFileChange(Sender: TObject);
begin
  UpdateMessages;
end;

procedure TCompilerMessagesOptionsFrame.editMsgFilterChange(Sender: TObject);
begin
  UpdateFilter;
end;

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

procedure TCompilerMessagesOptionsFrame.btnBrowseMsgClick(Sender: TObject);
var
  dlg : TOpenDialog;
begin
  dlg := TOpenDialog.Create(Self);
  try
    dlg.Filter := dlgBrowseMsgFilter;
    if not dlg.Execute then Exit;
    editMsgFilter.Caption := dlg.FileName;
    UpdateMessages;
  finally
    dlg.Free
  end;
end;

procedure TCompilerMessagesOptionsFrame.UpdateMessages;
const
  MaxIndexLen = 5;
var
  topidx    : Integer;
begin
  topidx := chklistCompMsg.TopIndex;
  chklistCompMsg.Items.BeginUpdate;
  try
    //debugln(['TCompilerMessagesOptionsFrame.UpdateMessages ',EnvironmentOptions.CompilerMessagesFilename]);
    if FileExistsUTF8(EnvironmentOptions.CompilerMessagesFilename) then begin
      try
        // FPC messages file is expected to be UTF8 encoded, no matter for the current code page is
        TempMessages.LoadMsgFile(EnvironmentOptions.CompilerMessagesFilename);
      except
        TempMessages.SetDefault;
      end;
    end else
      TempMessages.SetDefault;

    chklistCompMsg.Clear;
    chklistCompMsg.Items.Clear;
    
    UpdateFilter;

  finally
    chklistCompMsg.Items.EndUpdate;
    chkListCompMsg.TopIndex := topidx;
  end;
end;

procedure TCompilerMessagesOptionsFrame.UpdateFilter; 
var
  i     : Integer;
  j     : Integer;
  m     : TCompilerMessageConfig;
  add   : Boolean;
  srch  : AnsiString;
const
  //todo: should be translated
  MsgTypeStr : array [TFPCErrorType] of String = ('-','H','N','W','E','F','P');
begin
  chklistCompMsg.Items.BeginUpdate;
  try
    chklistCompMsg.Clear;
    srch:=UTF8UpperCase(editMsgFilter.Text);
    for i := 0 to TempMessages.Count - 1 do
    begin
      m := TempMessages.Msg[i];
      add:=m.MsgType in [etNote, etHint, etWarning];
      
      if add and (srch<>'') then 
        add:=System.Pos(srch, UTF8UpperCase(m.GetUserText))>0;
        
      if add then
      begin
        j := chklistCompMsg.Items.AddObject( Format('(%s) %s', [MsgTypeStr[m.MsgType], m.GetUserText([])]), m);
        if m.State = msDefault then
          chklistCompMsg.Checked[j] := not m.DefIgnored
        else
          chklistCompMsg.Checked[j] := m.State = msOn;
      end;
    end;
  finally            
    chklistCompMsg.Items.EndUpdate;
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
  editMsgFilter.Caption := '';
  grpCompilerMessages.Caption:=dlgCompilerMessage;
  lblFilter.Caption:=lisFilter;
end;

procedure TCompilerMessagesOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  if fLoaded then exit;
  fLoaded:=true;
  with AOptions as TBaseCompilerOptions do 
  begin
    TempMessages.Assign(CompilerMessages);
    UpdateMessages;
  end;
end;

procedure TCompilerMessagesOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  if FSaved then exit;
  FSaved:=true;
  with AOptions as TBaseCompilerOptions do
  begin
    UseMsgFile:=True;
    MsgFileName:=EnvironmentOptions.CompilerMessagesFilename;
    CompilerMessages.Assign(TempMessages);
  end;
end;

class function TCompilerMessagesOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TBaseCompilerOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupCompiler, TCompilerMessagesOptionsFrame, CompilerOptionsMessages);
  RegisterIDEOptionsEditor(GroupPkgCompiler, TCompilerMessagesOptionsFrame, CompilerOptionsMessages);

end.

