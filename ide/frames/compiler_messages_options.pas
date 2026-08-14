unit compiler_messages_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  StdCtrls, CheckLst, Dialogs,
  // LazUtils
  LazLoggerBase,
  // LazControls
  ListFilterEdit,
  // CodeTools
  CodeToolsFPCMsgs,
  // BuildIntf
  IDEOptionsIntf, IDEExternToolIntf, CompOptsIntf,
  // IdeIntf
  IDEOptEditorIntf,
  // IdeConfig
  etFPCMsgFilePool, CompilerOptions, EnvironmentOpts,
  // IDE
  LazarusIDEStrConsts;

type

  { TCompilerMessagesOptionsFrame }

  TCompilerMessagesOptionsFrame = class(TAbstractIDEOptionsEditor)
    cbTranslate: TCheckBox;
    chklistCompMsg: TCheckListBox;
    editMsgFilter: TListFilterEdit;
    grpCompilerMessages: TGroupBox;
    lblFilter: TLabel;
    procedure cbTranslateChange(Sender: TObject);
    procedure chklistCompMsgItemClick(Sender: TObject; Index: integer);
    function CheckItem(Item: TObject): Boolean;
  private
    TempMessages: TCompilerMsgIDFlags;
    FTranslationFN: string;
    FMsgFile: TFPCMsgFilePoolItem;
    FTransFile: TFPCMsgFilePoolItem;
    procedure UpdateMessages;
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

procedure TCompilerMessagesOptionsFrame.cbTranslateChange(Sender: TObject);
begin
  UpdateMessages;
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

constructor TCompilerMessagesOptionsFrame.Create(TheOwner: TComponent);
var
  EnglishFN: string;
begin
  inherited Create(TheOwner);
  TempMessages:=TCompilerMsgIDFlags.Create;
  FPCMsgFilePool.GetMsgFileNames(EnvironmentOptions.GetParsedCompilerFilename,
                                 '','', EnglishFN, FTranslationFN);
  //debugln(['TCompilerMessagesOptionsFrame.Create EnglishFN=', EnglishFN,
  //         ', TranslationFN=', FTranslationFN]);
  try
    FMsgFile:=FPCMsgFilePool.LoadFile(EnglishFN,true,nil);
    if FTranslationFN<>'' then
      FTransFile:=FPCMsgFilePool.LoadFile(FTranslationFN,true,nil);
  except
    on E: Exception do
      debugln(['WARNING: TCompilerMessagesOptionsFrame.ReadSettings failed to load file: '
               + E.Message]);
  end;
end;

destructor TCompilerMessagesOptionsFrame.Destroy;
begin
  FPCMsgFilePool.UnloadFile(FMsgFile);
  if Assigned(FTransFile) then
    FPCMsgFilePool.UnloadFile(FTransFile);
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
  CompOpts: TBaseCompilerOptions;
begin
  CompOpts:=AOptions as TBaseCompilerOptions;
  TempMessages.Assign(CompOpts.MessageFlags);
  if FTranslationFN<>'' then
    cbTranslate.Caption:=Format(dlgTranslateUsing,[ExtractFileName(FTranslationFN)])
  else begin
    cbTranslate.Caption:=dlgTranslateWithHint;
    cbTranslate.Enabled:=False;
  end;
  cbTranslate.Checked:=CompOpts.TranslateMessages;
  UpdateMessages;
end;

procedure TCompilerMessagesOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  CompOpts: TBaseCompilerOptions;
begin
  CompOpts:=AOptions as TBaseCompilerOptions;
  CompOpts.TranslateMessages:=cbTranslate.Checked;
  // Typecast here ensures the correct Assign methow is called.
  (CompOpts.MessageFlags as TCompilerMsgIDFlags).Assign(TempMessages);
end;

procedure TCompilerMessagesOptionsFrame.UpdateMessages;
var
  Item: TFPCMsgItem;
  MessagesFile: TFPCMsgFilePoolItem;
  Urgency: TMessageLineUrgency;
  i, TopIdx: Integer;
  s: String;
begin
  TopIdx:=chklistCompMsg.TopIndex;
  editMsgFilter.Items.Clear;
  if cbTranslate.Checked then
    MessagesFile:=FTransFile
  else
    MessagesFile:=FMsgFile;
  if MessagesFile<>nil then
    for i:=0 to MessagesFile.MsgFile.Count-1 do begin
      Item:=MessagesFile.MsgFile[i];
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
  editMsgFilter.InvalidateFilter;
  chkListCompMsg.TopIndex:=TopIdx;
end;

class function TCompilerMessagesOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result:=TBaseCompilerOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupCompiler, TCompilerMessagesOptionsFrame, CompilerOptionsMessages);
  RegisterIDEOptionsEditor(GroupPkgCompiler, TCompilerMessagesOptionsFrame, CompilerOptionsMessages);

end.

