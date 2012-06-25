unit EditorMacroListViewer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Laz2_XMLCfg, SynMacroRecorder, SynEdit, SynEditKeyCmds,
  FileProcs, Forms, Controls, Graphics, Dialogs, StdCtrls, ButtonPanel, ComCtrls, ExtCtrls,
  Spin, Menus, MainBar, IDEWindowIntf, IDEImagesIntf, LazarusIDEStrConsts, ProjectDefs,
  LazConf, Project, SrcEditorIntf;

type

  { TEditorMacro }

  TEditorMacro = class(TSynMacroRecorder)
  private
    FHasError: Boolean;
    FFailedText: String;
  public
    constructor Create(aOwner: TComponent); override;
    function GetAsText: String;
    procedure SetFromText(const AText: String);
    procedure WriteToXmlConf(AConf: TXMLConfig; const APath: String);
    procedure ReadFromXmlConf(AConf: TXMLConfig; const APath: String);
    property  HasError: Boolean read FHasError;
  end;

  { TSynMacroEventWriter }

  { TIdeMacroEventWriter }

  TIdeMacroEventWriter = class(TSynMacroEventWriter)
  private
    FText: String;
    FCmdName, FParams: String;
    FUseLineFeed: Boolean;
  public
    constructor Create;
    procedure BeginEvent;
    procedure FinishEvent;
    procedure WriteEventCommand(const ACmd: TSynEditorCommand); override;
    procedure WriteEventParam(const AParam: string); override;
    procedure WriteEventParam(const AParam: integer); override;
    property Text: String read FText;
    property UseLineFeed: Boolean read FUseLineFeed write FUseLineFeed;
  end;

  { TIdeMacroEventReader }

  TIdeMacroEventReader = class(TSynMacroEventReader)
  private
    FEventName: String;
    FHasError: Boolean;
    FText: String;
    FEventCommand: TSynEditorCommand;
    FParams: Array of record
        ParamType: TSynEventParamType;
        Text : String;
        Num: Integer;
      end;
  protected
    function GetParamAsInt(Index: Integer): Integer; override;
    function GetParamAsString(Index: Integer): String; override;
    function GetParamType(Index: Integer): TSynEventParamType; override;
  public
    constructor Create(const Atext: String);
    function  EventCommand: TSynEditorCommand; override;
    function  ParamCount: Integer; override;
    function  ParseNextEvent: Boolean;
    property  EventName: String read FEventName;
    property  HasError: Boolean read FHasError;
  end;

  { TEditorMacroList }

  TEditorMacroList = class
  private
    FList: TList;
    FOnChange: TNotifyEvent;
    function GetMacros(Index: Integer): TEditorMacro;
    procedure DoChanged;
  public
    constructor Create;
    destructor Destroy; override;
    procedure WriteToXmlConf(AConf: TXMLConfig; const APath: String);
    procedure ReadFromXmlConf(AConf: TXMLConfig; const APath: String);
    procedure ClearAndFreeMacros;
    function Count: Integer;
    function IndexOf(AMacro: TEditorMacro): Integer;
    function Add(AMacro: TEditorMacro): Integer;
    procedure Delete(AnIndex: Integer);
    procedure Remove(AMacro: TEditorMacro);
    property Macros[Index: Integer]: TEditorMacro read GetMacros;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  { TMacroListView }

  TMacroListView = class(TForm)
    btnPlay: TButton;
    btnRecord: TButton;
    btnRecordStop: TButton;
    btnDelete: TButton;
    btnSelect: TButton;
    btnRename: TButton;
    ButtonPanel1: TButtonPanel;
    chkRepeat: TCheckBox;
    lbMoveTo: TLabel;
    lbRecordedView: TListView;
    mnExport: TMenuItem;
    mnImport: TMenuItem;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    pnlButtons: TPanel;
    PopupMenu1: TPopupMenu;
    RenameButton: TPanelBitBtn;
    edRepeat: TSpinEdit;
    SaveDialog1: TSaveDialog;
    ToolBar1: TToolBar;
    tbRecorded: TToolButton;
    tbProject: TToolButton;
    tbIDE: TToolButton;
    ToolBar2: TToolBar;
    tbMoveProject: TToolButton;
    tbMoveIDE: TToolButton;
    procedure btnDeleteClick(Sender: TObject);
    procedure btnPlayClick(Sender: TObject);
    procedure btnRecordClick(Sender: TObject);
    procedure btnRecordStopClick(Sender: TObject);
    procedure btnRenameClick(Sender: TObject);
    procedure btnSelectClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure lbRecordedViewSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure mnExportClick(Sender: TObject);
    procedure mnImportClick(Sender: TObject);
    procedure tbIDEClick(Sender: TObject);
    procedure tbMoveIDEClick(Sender: TObject);
    procedure tbMoveProjectClick(Sender: TObject);
    procedure tbProjectClick(Sender: TObject);
    procedure tbRecordedClick(Sender: TObject);
  private
    FImageRec: Integer;
    FImagePlay: Integer;
    FImageSel: Integer;
    FImageErr: Integer;
    FIsPlaying: Boolean;
    procedure DoOnMacroListChange(Sender: TObject);
    procedure UpdateDisplay;
    procedure UpdateButtons;
  protected
    procedure DoEditorMacroStateChanged;
  public
    constructor Create(TheOwner: TComponent); override;
  end;

procedure ShowMacroListViewer;
procedure DoEditorMacroStateChanged;

procedure LoadProjectSpecificInfo(XMLConfig: TXMLConfig; Merge: boolean);
procedure SaveProjectSpecificInfo(XMLConfig: TXMLConfig; Flags: TProjectWriteFlags);
procedure LoadGlobalInfo;
procedure SaveGlobalInfo;

var
  EditorMacroRecorder: TEditorMacro = nil; // set by SourceEditor

implementation

var
  MacroListView: TMacroListView = nil;
  CurrentEditorMacroList: TEditorMacroList = nil;
  EditorMacroListRec, EditorMacroListProj, EditorMacroListGlob: TEditorMacroList;
  CurrentRecordingMacro: TEditorMacro = nil; // Points to a Macro in the list (copy)
  CurrentActiveMacro: TEditorMacro = nil; // Points to a Macro in the list (copy)
  MacroRecCounter: Integer = 1;
const
  GlobalConfFileName = 'EditorMacros.xml';

procedure ShowMacroListViewer;
begin
  if MacroListView = nil then
    MacroListView := TMacroListView.Create(Application);
  IDEWindowCreators.ShowForm(MacroListView, True);
end;

procedure DoEditorMacroStateChanged;
begin
  if EditorMacroRecorder= nil then exit;

  if not(EditorMacroRecorder.State  in [msRecording, msPaused]) and
    (CurrentRecordingMacro <> nil)
  then begin
    // finished recarding
    if EditorMacroRecorder.IsEmpty then begin
      EditorMacroListRec.Remove(CurrentRecordingMacro);
      FreeAndNil(CurrentRecordingMacro);
    end else begin
      CurrentRecordingMacro.AssignEventsFrom(EditorMacroRecorder);
      CurrentActiveMacro := CurrentRecordingMacro;
      CurrentRecordingMacro := nil;
    end;
  end;

  if (EditorMacroRecorder.State = msRecording) and (CurrentRecordingMacro = nil) then begin
    CurrentRecordingMacro := TEditorMacro.Create(nil);
    CurrentRecordingMacro.MacroName := Format(lisNewMacroName, [MacroRecCounter]);
    inc(MacroRecCounter);
    EditorMacroListRec.Add(CurrentRecordingMacro);
  end;

  if MacroListView <> nil then
    MacroListView.DoEditorMacroStateChanged;
end;

procedure LoadProjectSpecificInfo(XMLConfig: TXMLConfig; Merge: boolean);
begin
  EditorMacroListProj.ReadFromXmlConf(XMLConfig, '');
end;

procedure SaveProjectSpecificInfo(XMLConfig: TXMLConfig; Flags: TProjectWriteFlags);
begin
  if not (pwfSkipSeparateSessionInfo in Flags) then
  begin
    EditorMacroListProj.WriteToXmlConf(XMLConfig, '');
  end;
end;

procedure LoadGlobalInfo;
var
  Filename: String;
  XMLConfig: TXMLConfig;
begin
  Filename := TrimFilename(AppendPathDelim(GetPrimaryConfigPath)+GlobalConfFileName);
  XMLConfig := TXMLConfig.Create(Filename);
  EditorMacroListGlob.ReadFromXmlConf(XMLConfig, '');
  XMLConfig.Free;
end;

procedure SaveGlobalInfo;
var
  Filename: String;
  XMLConfig: TXMLConfig;
begin
  Filename := TrimFilename(AppendPathDelim(GetPrimaryConfigPath)+GlobalConfFileName);
  XMLConfig := TXMLConfig.CreateClean(Filename);
  EditorMacroListGlob.WriteToXmlConf(XMLConfig, '');
  XMLConfig.Free;
end;

{ TEditorMacro }

constructor TEditorMacro.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FHasError := False;
end;

function TEditorMacro.GetAsText: String;
var
  i : integer;
  W: TIdeMacroEventWriter;
begin
  if FHasError then begin
    Result := FFailedText;
    exit;
  end;

  W := TIdeMacroEventWriter.Create;
  try
    if Assigned(fEvents) then
    begin
      for i := 0 to fEvents.Count -1 do
      begin
        W.BeginEvent;
        Events[i].SaveToWriter(W);
        W.FinishEvent;
      end;
    end;
    Result := w.Text;
  finally
    W.Free;
  end;
end;

procedure TEditorMacro.SetFromText(const AText: String);
var
  iEvent: TSynMacroEvent;
  R: TIdeMacroEventReader;
begin
  Stop;
  Clear;
  fEvents := TList.Create;

  R := TIdeMacroEventReader.Create(AText);
  try
    while R.ParseNextEvent do begin
      iEvent := CreateMacroEvent(R.EventCommand);
      iEvent.LoadFromReader(R);
      fEvents.Add(iEvent);
    end;
    if R.HasError then begin
      FHasError := True;
      FFailedText := AText;
    end;
  finally
    R.Free;
  end;
end;

procedure TEditorMacro.WriteToXmlConf(AConf: TXMLConfig; const APath: String);
begin
  AConf.SetValue(APath + 'Name', MacroName);
  AConf.SetValue(APath + 'Code/Value', GetAsText);
end;

procedure TEditorMacro.ReadFromXmlConf(AConf: TXMLConfig; const APath: String);
var
  s: String;
begin
  s := AConf.GetValue(APath + 'Name', '');
  if s <> '' then MacroName := s;
  s := AConf.GetValue(APath + 'Code/Value', '');
  SetFromText(s);
  if (not FHasError) and (EventCount = 0) then begin
    FHasError := True;
    FFailedText := s;
  end;
end;

{ TIdeMacroEventReader }

function TIdeMacroEventReader.GetParamAsInt(Index: Integer): Integer;
begin
  if (Index < 0) or (Index >= Length(FParams)) or (ParamType[Index] <> ptInteger)
  then begin
    FHasError := True;
    exit; // TODO error
  end;

  Result := FParams[Index].Num;
end;

function TIdeMacroEventReader.GetParamAsString(Index: Integer): String;
begin
  if (Index < 0) or (Index >= Length(FParams)) or (ParamType[Index] <> ptString)
  then begin
    FHasError := True;
    exit; // TODO error
  end;

  Result := FParams[Index].Text;
end;

function TIdeMacroEventReader.GetParamType(Index: Integer): TSynEventParamType;
begin
  if (Index < 0) or (Index >= Length(FParams)) then begin
    FHasError := True;
    exit; // TODO error
  end;

  Result := FParams[Index].ParamType;
end;

constructor TIdeMacroEventReader.Create(const Atext: String);
begin
  FText := Atext;
  FHasError := False;
end;

function TIdeMacroEventReader.EventCommand: TSynEditorCommand;
begin
  Result := FEventCommand;
end;

function TIdeMacroEventReader.ParamCount: Integer;
begin
  Result := Length(FParams);
end;

function TIdeMacroEventReader.ParseNextEvent: Boolean;
  procedure SkipNum(var i: integer);
  begin
    while (i <= Length(FText)) and (FText[i] in ['0'..'9']) do inc (i);
  end;
  procedure SkipSpace(var i: integer);
  begin
    while (i <= Length(FText)) and (FText[i] in [' '..#9, #13, #10]) do inc (i);
  end;
var
  c,i,j,k: Integer;
  s: String;
begin
  FEventName := '';
  FText := Trim(FText);
  Result := (FText <> '') and (not FHasError);
  if not Result then exit;
  Result := False;
  FHasError := True; // Assume the worst

  i := 1;
  while (i <= Length(FText)) and (FText[i] in ['a'..'z','A'..'Z','0'..'9','_']) do inc (i);
  if i = 1 then exit; // Todo error

  s := Copy(FText, 1, i-1);
  if not IdentToEditorCommand(s, j) then exit; // Todo error
  FEventCommand := j;
  FEventName := s;

  while (i <= Length(FText)) and (FText[i] in [' ', #9]) do inc (i);
  if (i > Length(FText)) then exit; // Todo error

  SetLength(FParams, 0);
  c := 0;

  if (FText[i] = '(') then begin
    inc(i);
    repeat
      SkipSpace(i);
      if (i > Length(FText)) then exit; // Todo error

      if FText[i] in ['0'..'9'] then begin
        // Parse number
        j := i;
        SkipNum(i);
        SetLength(FParams, c + 1);
        FParams[c].ParamType := ptInteger;
        FParams[c].Num := StrToInt(copy(FText, i, j-i));
        inc(c);
      end
      else
      if FText[i] in ['#', ''''] then begin
        // Parse string
        s := '';
        repeat
          case FText[i] of
            '#': begin
                j := i;
                SkipNum(i);
                k := StrToInt(copy(FText, i, j-i));
                if k > 255 then exit; // TODO error, todo utf8
                s := s + chr(k);
              end;
            '''':  begin
                inc(i);
                repeat
                  case FText[i] of
                    '''': begin
                        if (i+1 <= Length(FText)) and (FText[i+1] = '''') then begin
                          s := s + '''';
                          inc(i,2);
                        end
                        else begin
                          inc(i);
                          break;
                        end;
                      end;
                    else begin
                        s := s + FText[i];
                        inc(i);
                      end;
                  end;
                until i > Length(FText);
              end;
            else
              break;
          end;
        until i > Length(FText);

        SetLength(FParams, c + 1);
        FParams[c].ParamType := ptString;
        FParams[c].Text := s;
        inc(c);
      end
      else
        exit; // Todo error

      SkipSpace(i);
      if (i >= Length(FText)) then exit; // Todo error
      if FText[i] = ')' then break;
      if not(FText[i] = ',') then exit; // Todo error
      inc(i);
    until i > Length(FText);
    inc(i);
  end;

  if (i > Length(FText)) then exit; // Todo error
  if (FText[i] = ';') then begin
    Delete(FText, 1, i);
    Result := True;
    FHasError := False;
    exit;
  end;
end;

{ TSynMacroEventWriter }

constructor TIdeMacroEventWriter.Create;
begin
  FUseLineFeed := False;
end;

procedure TIdeMacroEventWriter.BeginEvent;
begin
  FCmdName := '';
  FParams := '';
end;

procedure TIdeMacroEventWriter.FinishEvent;
begin
  FText := FText + FCmdName;
  if FParams <> '' then
    FText := FText + '(' + FParams + ')';
  FText := FText + ';';
  if FUseLineFeed then
    FText := FText + LineEnding;
end;

procedure TIdeMacroEventWriter.WriteEventCommand(const ACmd: TSynEditorCommand);
begin
  EditorCommandToIdent(ACmd, FCmdName);
end;

procedure TIdeMacroEventWriter.WriteEventParam(const AParam: string);
var
  s: String;
  i: Integer;
  InQuotes: Boolean;
begin
  if FParams <> '' then
    FParams := FParams + ', ';
  s := '';
  InQuotes := False;
  for i := 1 to length(AParam) do
    case AParam[i] of
      #0..#31: begin
          if InQuotes then s := s + '''';
          InQuotes := False;
          s := s + '#' + IntToStr(ord(AParam[i]));
        end;
      '''': begin
          if not InQuotes then s := s + '''';
          InQuotes := True;
          s := s + '''''';
        end;
      else begin
          if not InQuotes then s := s + '''';
          InQuotes := True;
          s := s + AParam[i];
        end;
    end;
  if InQuotes then s := s + '''';
  FParams := FParams + s;
end;

procedure TIdeMacroEventWriter.WriteEventParam(const AParam: integer);
begin
  if FParams <> '' then
    FParams := FParams + ', ';
  FParams := FParams + IntToStr(AParam);
end;

{ TMacroListView }

procedure TMacroListView.btnRenameClick(Sender: TObject);
var
  s: String;
  M: TEditorMacro;
begin
  if lbRecordedView.ItemIndex < 0 then exit;
  M := CurrentEditorMacroList.Macros[lbRecordedView.ItemIndex];
  s := M.MacroName;
  if InputQuery('New Macroname', Format('Enter new mawe for Macro "%s"', [m.MacroName]), s)
  then begin
    M.MacroName := s;
    UpdateDisplay;
  end;
end;

procedure TMacroListView.btnPlayClick(Sender: TObject);
var
  i: Integer;
begin
  if EditorMacroRecorder.State <> msStopped then exit;
  if lbRecordedView.ItemIndex < 0 then exit;

  i := 1;
  if chkRepeat.Enabled then i := edRepeat.Value;
  FIsPlaying := True;
  UpdateButtons;
  Application.ProcessMessages;

  try
    EditorMacroRecorder.AssignEventsFrom(CurrentEditorMacroList.Macros[lbRecordedView.ItemIndex]);
    while i > 0 do begin
      EditorMacroRecorder.PlaybackMacro(TCustomSynEdit(SourceEditorManagerIntf.ActiveEditor.EditorControl));
      Application.ProcessMessages;
      dec(i);
      if not FIsPlaying then break;
    end;
  finally
    FIsPlaying := False;
    EditorMacroRecorder.AssignEventsFrom(CurrentActiveMacro);
    UpdateButtons;
  end;

end;

procedure TMacroListView.btnDeleteClick(Sender: TObject);
begin
  if lbRecordedView.ItemIndex < 0 then exit;
  if MessageDlg(lisDeleteSelectedMacro, mtConfirmation, [mbYes, mbNo], 0) = mrYes
  then begin
    if CurrentActiveMacro = CurrentEditorMacroList.Macros[lbRecordedView.ItemIndex] then begin
      CurrentActiveMacro := nil;
      EditorMacroRecorder.Clear;
    end;
    CurrentEditorMacroList.Delete(lbRecordedView.ItemIndex);
    UpdateDisplay;
  end;
end;

procedure TMacroListView.btnRecordClick(Sender: TObject);
begin
  if EditorMacroRecorder.State = msStopped then begin
    EditorMacroRecorder.RecordMacro(TCustomSynEdit(SourceEditorManagerIntf.ActiveEditor.EditorControl));
  end
  else
  if EditorMacroRecorder.State = msRecording then begin
    EditorMacroRecorder.Pause;
  end
  else
  if EditorMacroRecorder.State = msPaused then begin
    EditorMacroRecorder.Resume;
  end;
  SourceEditorManagerIntf.ActiveEditor.EditorControl.SetFocus;
end;

procedure TMacroListView.btnRecordStopClick(Sender: TObject);
begin
  FIsPlaying := False;
  EditorMacroRecorder.Stop;
end;

procedure TMacroListView.btnSelectClick(Sender: TObject);
begin
  if EditorMacroRecorder.State <> msStopped then exit;
  if lbRecordedView.ItemIndex >= 0 then
    CurrentActiveMacro := CurrentEditorMacroList.Macros[lbRecordedView.ItemIndex]
  else
    CurrentActiveMacro := nil;
  EditorMacroRecorder.AssignEventsFrom(CurrentActiveMacro);
  UpdateDisplay;
end;

procedure TMacroListView.FormActivate(Sender: TObject);
begin
  lbRecordedView.HideSelection := Active;
end;

procedure TMacroListView.lbRecordedViewSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  UpdateButtons;
end;

procedure TMacroListView.mnExportClick(Sender: TObject);
var
  Conf: TXMLConfig;
begin
  if lbRecordedView.ItemIndex < 0 then exit;

  if SaveDialog1.Execute then begin
    Conf := TXMLConfig.Create(SaveDialog1.FileName);
    try
      Conf.Clear;
      Conf.SetValue('EditorMacros/Count', 1);
      CurrentEditorMacroList.Macros[lbRecordedView.ItemIndex].WriteToXmlConf(Conf, 'EditorMacros/Macro1/');
    finally
      Conf.Free;
    end;
  end;
end;

procedure TMacroListView.mnImportClick(Sender: TObject);
var
  Conf: TXMLConfig;
  NewMacro: TEditorMacro;
begin
  if OpenDialog1.Execute then begin
    Conf := TXMLConfig.Create(OpenDialog1.FileName);
    try
      NewMacro := TEditorMacro.Create(nil);
      NewMacro.ReadFromXmlConf(Conf, 'EditorMacros/Macro1/');
      if NewMacro.EventCount > 0 then
        CurrentEditorMacroList.Add(NewMacro)
      else
        NewMacro.Free;
    finally
      Conf.Free;
    end;
  end;
end;

procedure TMacroListView.tbIDEClick(Sender: TObject);
begin
  CurrentEditorMacroList := EditorMacroListGlob;
  UpdateDisplay;
end;

procedure TMacroListView.tbMoveIDEClick(Sender: TObject);
var
  i: Integer;
begin
  if (lbRecordedView.ItemIndex < 0) or (CurrentEditorMacroList = EditorMacroListGlob) then exit;
  i := lbRecordedView.ItemIndex;
  EditorMacroListGlob.Add(CurrentEditorMacroList.Macros[i]);
  CurrentEditorMacroList.Delete(i);
  UpdateDisplay;
end;

procedure TMacroListView.tbMoveProjectClick(Sender: TObject);
var
  i: Integer;
begin
  if (lbRecordedView.ItemIndex < 0) or (CurrentEditorMacroList = EditorMacroListProj) then exit;
  i := lbRecordedView.ItemIndex;
  EditorMacroListProj.Add(CurrentEditorMacroList.Macros[i]);
  CurrentEditorMacroList.Delete(i);
  UpdateDisplay;
end;

procedure TMacroListView.tbProjectClick(Sender: TObject);
begin
  CurrentEditorMacroList := EditorMacroListProj;
  UpdateDisplay;
end;

procedure TMacroListView.tbRecordedClick(Sender: TObject);
begin
  CurrentEditorMacroList := EditorMacroListRec;
  UpdateDisplay;
end;

procedure TMacroListView.DoOnMacroListChange(Sender: TObject);
begin
  UpdateDisplay;

  if Sender = EditorMacroListProj then
    Project1.SessionModified := True;
end;

procedure TMacroListView.UpdateDisplay;
var
  NewItem: TListItem;
  i, idx: Integer;
  M: TEditorMacro;
begin
  idx := lbRecordedView.ItemIndex;
  lbRecordedView.Items.Clear;

  for i := 0 to CurrentEditorMacroList.Count - 1 do begin
    M := CurrentEditorMacroList.Macros[i];
    NewItem := lbRecordedView.Items.Add;
    NewItem.Caption := M.MacroName;
    if M.HasError then
      NewItem.ImageIndex := FImageErr
    else
    if (m = CurrentRecordingMacro) then
      NewItem.ImageIndex := FImageRec
    else
    if (CurrentRecordingMacro = nil) and (m = CurrentActiveMacro)  then begin
      if (EditorMacroRecorder.State = msPlaying) then
        NewItem.ImageIndex := FImagePlay
      else
        NewItem.ImageIndex := FImageSel;
    end;
  end;
  if idx < lbRecordedView.Items.Count then
    lbRecordedView.ItemIndex := idx
  else
    lbRecordedView.ItemIndex := -1;

  lbRecordedViewSelectItem(nil, nil, False);
  UpdateButtons;
end;

procedure TMacroListView.UpdateButtons;
var
  IsSel, IsErr, IsStopped: Boolean;
  M: TEditorMacro;
begin
  IsSel := (lbRecordedView.ItemIndex >= 0);
  IsStopped := (EditorMacroRecorder.State = msStopped);
  if IsSel then
    M := CurrentEditorMacroList.Macros[lbRecordedView.ItemIndex];
  IsErr := IsSel and M.HasError;


  btnSelect.Enabled := IsStopped and IsSel and (not FIsPlaying) and (not IsErr);
  btnRename.Enabled := IsStopped and IsSel and (not FIsPlaying) and (not IsErr);
  btnDelete.Enabled := IsStopped and IsSel and (not FIsPlaying);

  btnPlay.Enabled := IsStopped and IsSel and (not FIsPlaying) and (not IsErr);
  chkRepeat.Enabled := IsStopped and (not FIsPlaying);
  edRepeat.Enabled := IsStopped and (not FIsPlaying);

  btnRecord.Enabled := (EditorMacroRecorder.State in [msStopped, msPaused, msRecording]) and (not FIsPlaying);
  btnRecordStop.Enabled := (not IsStopped) or FIsPlaying;

  if (EditorMacroRecorder.State = msRecording) then
    btnRecord.Caption := lisPause
  else if (EditorMacroRecorder.State = msPaused) then
    btnRecord.Caption := lisContinue
  else
    btnRecord.Caption := lisRecord;

  mnImport.Enabled := IsStopped and (not FIsPlaying);
  mnExport.Enabled := IsStopped and IsSel and (not FIsPlaying);

  tbMoveProject.Visible := CurrentEditorMacroList <> EditorMacroListProj;
  tbMoveProject.Enabled := IsStopped and IsSel and (not FIsPlaying);
  tbMoveIDE.Visible := CurrentEditorMacroList <> EditorMacroListGlob;
  tbMoveIDE.Enabled := IsStopped and IsSel and (not FIsPlaying);
end;

procedure TMacroListView.DoEditorMacroStateChanged;
begin
  UpdateDisplay;
end;

constructor TMacroListView.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Caption := lisEditorMacros;
  EditorMacroListRec.OnChange := @DoOnMacroListChange;
  EditorMacroListProj.OnChange := @DoOnMacroListChange;
  EditorMacroListGlob.OnChange := @DoOnMacroListChange;

  tbRecorded.Caption := lisRecordedMacros;
  tbProject.Caption := lisProjectMacro;
  tbIDE.Caption := lisIDE;
  tbRecorded.Hint := lisNewRecordedMacrosNotToBeSaved;
  tbProject.Hint := lisSavedWithProjectSession;
  tbIDE.Hint := lisSavedWithIDESettings;
  tbMoveProject.Caption := lisProjectMacro;
  tbMoveIDE.Caption := lisIDE;
  lbMoveTo.Caption := lisMoveTo;

  btnSelect.Caption := lisMenuSelect;
  btnRename.Caption := lisRename2;
  btnDelete.Caption := lisDelete;
  btnPlay.Caption := lisPlay;
  chkRepeat.Caption := lisRepeat;
  btnRecord.Caption := lisRecord;
  btnRecordStop.Caption := lisStop;

  SaveDialog1.Title := lisSaveMacroAs;
  OpenDialog1.Title := lisLoadMacroFrom;
  mnImport.Caption := lisImport;
  mnExport.Caption := lisExport;

  lbRecordedView.SmallImages := IDEImages.Images_16;
  FImageRec := IDEImages.LoadImage(16, 'Record');  // red dot
  FImagePlay := IDEImages.LoadImage(16, 'menu_run');  // green triangle
  FImageSel := IDEImages.LoadImage(16, 'arrow_right');
  FImageErr := IDEImages.LoadImage(16, 'state_error');
  FIsPlaying := False;

  UpdateButtons;
end;

{ TEditorMacroList }

function TEditorMacroList.GetMacros(Index: Integer): TEditorMacro;
begin
  Result := TEditorMacro(FList[Index]);
end;

procedure TEditorMacroList.DoChanged;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TEditorMacroList.Create;
begin
  FList := TList.Create;
end;

destructor TEditorMacroList.Destroy;
begin
  FOnChange := nil;
  ClearAndFreeMacros;
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TEditorMacroList.WriteToXmlConf(AConf: TXMLConfig; const APath: String);
var
  i: Integer;
begin
  AConf.SetValue(APath + 'EditorMacros/Count', Count);
  for i := 0 to Count - 1 do
    Macros[i].WriteToXmlConf(AConf, 'EditorMacros/Macro'+IntToStr(i+1)+'/');
end;

procedure TEditorMacroList.ReadFromXmlConf(AConf: TXMLConfig; const APath: String);
var
  c, i: Integer;
  NewMacro: TEditorMacro;
begin
  ClearAndFreeMacros;
  c := AConf.GetValue(APath + 'EditorMacros/Count', 0);
  for i := 0 to c -1 do begin
    NewMacro := TEditorMacro.Create(nil);
    try
      NewMacro.ReadFromXmlConf(AConf, 'EditorMacros/Macro'+IntToStr(i+1)+'/');
    finally
      Add(NewMacro)
    end;
  end;
end;

procedure TEditorMacroList.ClearAndFreeMacros;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do Macros[i].Free;
  FList.Clear;
end;

function TEditorMacroList.Count: Integer;
begin
  Result := FList.Count;
end;

function TEditorMacroList.IndexOf(AMacro: TEditorMacro): Integer;
begin
  Result := FList.IndexOf(AMacro);
end;

function TEditorMacroList.Add(AMacro: TEditorMacro): Integer;
begin
  Result := FList.Add(AMacro);
  DoChanged;
end;

procedure TEditorMacroList.Delete(AnIndex: Integer);
begin
  FList.Delete(AnIndex);
  DoChanged;
end;

procedure TEditorMacroList.Remove(AMacro: TEditorMacro);
begin
  FList.Remove(AMacro);
  DoChanged;
end;

// itmMacroListView.enabled

{$R *.lfm}

initialization
  EditorMacroListRec := TEditorMacroList.Create;
  EditorMacroListProj := TEditorMacroList.Create;
  EditorMacroListGlob := TEditorMacroList.Create;
  CurrentEditorMacroList := EditorMacroListRec;

finalization
  CurrentEditorMacroList := nil;
  FreeAndNil(EditorMacroListRec);
  FreeAndNil(EditorMacroListProj);
  FreeAndNil(EditorMacroListGlob);

end.

