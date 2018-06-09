unit compiler_messages_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  StdCtrls, CheckLst, Dialogs,
  // LazUtils
  FileUtil, LazFileCache, LazLoggerBase,
  // LazControls
  ListFilterEdit,
  // CodeTools
  CodeToolsFPCMsgs,
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf, IDEExternToolIntf, CompOptsIntf, IDEImagesIntf,
  IDEDialogs,
  // IDE
  CompilerOptions, LazarusIDEStrConsts, etFPCMsgParser;

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
    TempMessages: TCompilerMsgIDFlags;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    function GetTitle: String; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ TCompilerMessagesOptionsFrame }

procedure TCompilerMessagesOptionsFrame.chklistCompMsgItemClick(Sender: TObject; Index: integer);
var
  MsgId: Integer;
begin
  if (Index < 0) or (Index >= chklistCompMsg.Items.Count) then exit;
  MsgId:=Integer({%H-}PtrUInt(chklistCompMsg.Items.Objects[Index]));
  if MsgId<=0 then exit;
  if chklistCompMsg.Checked[Index] then begin
    // show message, this is the default
    TempMessages[MsgId]:=cfvNone
  end else
    TempMessages[MsgId]:=cfvHide;
end;

function TCompilerMessagesOptionsFrame.CheckItem(Item: TObject): Boolean;
var
  MsgId: Integer;
begin
  Result:=true;
  if TempMessages=nil then exit;
  MsgId:=Integer({%H-}PtrUInt(Pointer(Item)));
  if MsgId<=0 then exit;
  Result:=TempMessages[MsgId]<>cfvHide;
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
    OpenDialog.Filter:=dlgFilterFPCMessageFile+' (*.msg)|*.msg|'+dlgFilterAll+'|'+
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
  TempMessages:=TCompilerMsgIDFlags.Create;
  UseMsgFileCheckBox.Visible:=false;
  MsgFileEdit.Visible:=false;
  MsgFileBrowseButton.Visible:=false;
end;

destructor TCompilerMessagesOptionsFrame.Destroy;
begin
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
end;

procedure TCompilerMessagesOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  topidx: Integer;
  CompOpts: TBaseCompilerOptions;
  FPCMsgFile: TFPCMsgFilePoolItem;
  i: Integer;
  Item: TFPCMsgItem;
  Urgency: TMessageLineUrgency;
  s: String;
begin
  CompOpts:=AOptions as TBaseCompilerOptions;

  topidx := chklistCompMsg.TopIndex;
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
        editMsgFilter.Items.AddObject(s,TObject({%H-}Pointer(PtrUInt(Item.ID))));
      end;
    finally
      FPCMsgFilePool.UnloadFile(FPCMsgFile);
    end;
  end;
  editMsgFilter.InvalidateFilter;
  chkListCompMsg.TopIndex := topidx;
end;

procedure TCompilerMessagesOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  with AOptions as TBaseCompilerOptions do
  begin
    MessageFlags.Assign(TempMessages);
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

