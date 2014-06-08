unit compiler_messages_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazFileCache, LazLoggerBase, ListFilterEdit,
  StdCtrls, CheckLst, Dialogs, IDEOptionsIntf, IDEMsgIntf, IDEExternToolIntf,
  MacroIntf, IDEDialogs, CompOptsIntf, CodeToolsFPCMsgs, CompilerOptions,
  LazarusIDEStrConsts
  {$IFNDEF EnableOldExtTools}
  ,etFPCMsgParser
  {$ENDIF}
  ;

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
    TempMessages: {$IFNDEF EnableOldExtTools}TCompilerMsgIDFlags{$ELSE}TCompilerMessagesList{$ENDIF};
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
{$IFNDEF EnableOldExtTools}
var
  MsgId: Integer;
{$ELSE}
const
  BoolToMessageState: array[Boolean] of TCompilerMessageState = (msOff, msOn);
var
  m: TCompilerMessageConfig;
{$ENDIF}
begin
  if (Index < 0) or (Index >= chklistCompMsg.Items.Count) then exit;
  {$IFNDEF EnableOldExtTools}
  MsgId:=Integer(PtrUInt(Pointer(chklistCompMsg.Items.Objects[Index])));
  if MsgId<=0 then exit;
  if chklistCompMsg.Checked[Index] then begin
    // show message, this is the default
    TempMessages[MsgId]:=cfvNone
  end else
    TempMessages[MsgId]:=cfvHide;
  {$ELSE}
  m := TCompilerMessageConfig(chklistCompMsg.Items.Objects[Index]);
  if (m.DefIgnored <> chklistCompMsg.Checked[Index]) then
    m.State := msDefault
  else
    m.State := BoolToMessageState[chklistCompMsg.Checked[Index]];
  {$ENDIF}
end;

function TCompilerMessagesOptionsFrame.CheckItem(Item: TObject): Boolean;
{$IFNDEF EnableOldExtTools}
var
  MsgId: Integer;
begin
  Result:=true;
  if TempMessages=nil then exit;
  MsgId:=Integer(PtrUInt(Pointer(Item)));
  if MsgId<=0 then exit;
  Result:=TempMessages[MsgId]<>cfvHide;
end;
{$ELSE}
var
  m: TCompilerMessageConfig;
begin
  m := Item as TCompilerMessageConfig;
  if m.State = msDefault then
    Result := not m.DefIgnored
  else
    Result := m.State = msOn;
end;
{$ENDIF}

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
  {$IFNDEF EnableOldExtTools}
  TempMessages:=TCompilerMsgIDFlags.Create;
  UseMsgFileCheckBox.Visible:=false;
  MsgFileEdit.Visible:=false;
  MsgFileBrowseButton.Visible:=false;
  {$ELSE}
  TempMessages:=TCompilerMessagesList.Create;
  {$ENDIF}
end;

destructor TCompilerMessagesOptionsFrame.Destroy;
begin
  {$IFNDEF EnableOldExtTools}
  {$ELSE}
  editMsgFilter.Items.Clear;
  chklistCompMsg.Clear;
  {$ENDIF}
  FreeAndNil(TempMessages);
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
  {$IFNDEF EnableOldExtTools}
  {$ELSE}
  UseMsgFileCheckBox.Caption:=lisUseMessageFile;
  MsgFileBrowseButton.Caption:=lisPathEditBrowse;
  {$ENDIF}
end;

procedure TCompilerMessagesOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  topidx: Integer;
  CompOpts: TBaseCompilerOptions;
  {$IFNDEF EnableOldExtTools}
  FPCMsgFile: TFPCMsgFilePoolItem;
  i: Integer;
  Item: TFPCMsgItem;
  Urgency: TMessageLineUrgency;
  s: String;
  {$ELSE}
  i: integer;
  m: TCompilerMessageConfig;
  {$ENDIF}
begin
  CompOpts:=AOptions as TBaseCompilerOptions;

  topidx := chklistCompMsg.TopIndex;
  {$IFNDEF EnableOldExtTools}
  TempMessages.Assign(CompOpts.MessageFlags);
  editMsgFilter.Items.Clear;
  FPCMsgFile:=FPCMsgFilePool.LoadCurrentEnglishFile(true,nil);
  if FPCMsgFile<>nil then begin
    try
      for i:=0 to FPCMsgFile.MsgFile.Count-1 do begin
        Item:=FPCMsgFile.MsgFile[i];
        if Item.ID<=0 then continue;
        Urgency:=FPCMsgToMsgUrgency(Item);
        case Urgency of
        mluHint: s:='Hint';
        mluNote: s:='Note';
        mluWarning: s:='Warning';
        else continue;
        end;
        s+=': '+Item.Pattern;
        editMsgFilter.Items.AddObject(s,TObject(Pointer(PtrUInt(Item.ID))));
      end;
    finally
      FPCMsgFilePool.UnloadFile(FPCMsgFile);
    end;
  end;
  {$ELSE}
  TempMessages.Assign(CompOpts.CompilerMessages);
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
    etHint: editMsgFilter.Items.AddObject('(H) '+m.MsgText, m);
    etNote: editMsgFilter.Items.AddObject('(N) '+m.MsgText, m);
    etWarning: editMsgFilter.Items.AddObject('(W) '+m.MsgText, m);
    end;
  end;
  {$ENDIF}
  editMsgFilter.InvalidateFilter;
  chkListCompMsg.TopIndex := topidx;
end;

procedure TCompilerMessagesOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  with AOptions as TBaseCompilerOptions do
  begin
    {$IFNDEF EnableOldExtTools}
    MessageFlags.Assign(TempMessages);
    {$ELSE}
    UseMsgFile:=UseMsgFileCheckBox.Checked;
    MsgFileName:=MsgFileEdit.Text;
    CompilerMessages.Assign(TempMessages);
    {$ENDIF}
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

