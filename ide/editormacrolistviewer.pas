unit EditorMacroListViewer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynMacroRecorder, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, ComCtrls, ExtCtrls, MainBar, IDEWindowIntf, IDEImagesIntf, LazarusIDEStrConsts;

type

  TEditorMacro = TSynMacroRecorder;

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
    btnSelect: TButton;
    btnRename: TButton;
    ButtonPanel1: TButtonPanel;
    lblRecordedTitle: TLabel;
    lbRecordedView: TListView;
    Panel1: TPanel;
    pnlButtons: TPanel;
    RenameButton: TPanelBitBtn;
    procedure btnRenameClick(Sender: TObject);
    procedure btnSelectClick(Sender: TObject);
    procedure lbRecordedViewSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
  private
    FImageRec: Integer;
    FImagePlay: Integer;
    FImageSel: Integer;
    procedure DoOnMacroListChange(Sender: TObject);
    procedure UpdateDisplay;
  protected
    procedure DoEditorMacroStateChanged;
  public
    constructor Create(TheOwner: TComponent); override;
  end;

procedure ShowMacroListViewer;
procedure DoEditorMacroStateChanged;

var
  EditorMacroRecorder: TEditorMacro = nil; // set by SourceEditor

implementation

var
  MacroListView: TMacroListView = nil;
  EditorMacroList: TEditorMacroList = nil;
  CurrentRecordingMacro: TEditorMacro = nil; // Points to a Macro in the list (copy)
  CurrentActiveMacro: TEditorMacro = nil; // Points to a Macro in the list (copy)


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
      EditorMacroList.Remove(CurrentRecordingMacro);
      FreeAndNil(CurrentRecordingMacro);
    end else begin
      CurrentRecordingMacro.AssignEventsFrom(EditorMacroRecorder);
      CurrentActiveMacro := CurrentRecordingMacro;
      CurrentRecordingMacro := nil;
    end;
  end;

  if (EditorMacroRecorder.State = msRecording) and (CurrentRecordingMacro = nil) then begin
    CurrentRecordingMacro := TEditorMacro.Create(nil);
    CurrentRecordingMacro.MacroName := Format(lisNewMacroName, [EditorMacroList.Count+1]);
    EditorMacroList.Add(CurrentRecordingMacro);
  end;

  if MacroListView <> nil then
    MacroListView.DoEditorMacroStateChanged;
end;

{ TMacroListView }

procedure TMacroListView.btnRenameClick(Sender: TObject);
var
  s: String;
  M: TSynMacroRecorder;
begin
  if lbRecordedView.ItemIndex < 0 then exit;
  M := EditorMacroList.Macros[lbRecordedView.ItemIndex];
  s := M.MacroName;
  if InputQuery('New Macroname', Format('Enter new mawe for Macro "%s"', [m.MacroName]), s)
  then begin
    M.MacroName := s;
    UpdateDisplay;
  end;
end;

procedure TMacroListView.btnSelectClick(Sender: TObject);
begin
  if EditorMacroRecorder.State <> msStopped then exit;
  if lbRecordedView.ItemIndex >= 0 then
    CurrentActiveMacro := EditorMacroList.Macros[lbRecordedView.ItemIndex]
  else
    CurrentActiveMacro := nil;
  EditorMacroRecorder.AssignEventsFrom(CurrentActiveMacro);
  UpdateDisplay;
end;

procedure TMacroListView.lbRecordedViewSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  btnSelect.Enabled := (EditorMacroRecorder.State = msStopped) and (lbRecordedView.ItemIndex >= 0);
  btnRename.Enabled := (lbRecordedView.ItemIndex >= 0);
end;

procedure TMacroListView.DoOnMacroListChange(Sender: TObject);
begin
  UpdateDisplay;
end;

procedure TMacroListView.UpdateDisplay;
var
  NewItem: TListItem;
  i, idx: Integer;
  M: TSynMacroRecorder;
begin
  idx := lbRecordedView.ItemIndex;
  lbRecordedView.Items.Clear;
  for i := 0 to EditorMacroList.Count - 1 do begin
    M := EditorMacroList.Macros[i];
    NewItem := lbRecordedView.Items.Add;
    NewItem.Caption := M.MacroName;
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
end;

procedure TMacroListView.DoEditorMacroStateChanged;
begin
  UpdateDisplay;
end;

constructor TMacroListView.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Caption := lisEditorMacros;
  EditorMacroList.OnChange := @DoOnMacroListChange;
  lblRecordedTitle.Caption := lisRecordedMacros;
  btnSelect.Caption := lisMenuSelect;
  btnRename.Caption := lisRename2;
  lbRecordedView.SmallImages := IDEImages.Images_16;
  FImageRec := IDEImages.LoadImage(16, 'Record');  // red dot
  FImagePlay := IDEImages.LoadImage(16, 'menu_run');  // green triangle
  FImageSel := IDEImages.LoadImage(16, 'arrow_right');

  lbRecordedViewSelectItem(nil, nil, False);
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
  ClearAndFreeMacros;
  FreeAndNil(FList);
  inherited Destroy;
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
  EditorMacroList := TEditorMacroList.Create;

finalization
  FreeAndNil(EditorMacroList);

end.

