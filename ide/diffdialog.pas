{
 /***************************************************************************
                              diffdialog.pas
                              --------------


 ***************************************************************************/

 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:
    The TDiffDlg is a dialog for showing differences between two files.

}

unit DiffDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  Forms, Controls, Buttons, StdCtrls, ExtCtrls, Dialogs, ComCtrls, LCLType,
  // LazUtils
  FileUtil, lazutf8classes,
  // SynEdit
  SynEdit, SynHighlighterDiff,
  // IdeIntf
  IDEWindowIntf, IDEHelpIntf, IDEImagesIntf,
  // IDE
  LazarusIDEStrConsts, EditorOptions, InputHistory, DiffPatch, SourceEditor, EnvironmentOpts;

type

  { TAvailableDiffFile }

  TAvailableDiffFile = class
  private
    Name: string;
    Editor: TSourceEditor;
    SelectionAvailable: boolean;
  public
    constructor Create(const NewName: string; NewEditor: TSourceEditor;
      NewSelectionAvailable: boolean);
  end;

  { TAvailableDiffFiles }

  TAvailableDiffFiles = class(TList)
  private
    function GetItems(Index: integer): TAvailableDiffFile;
    procedure SetItems(Index: integer; const AValue: TAvailableDiffFile);
  public
    procedure Clear; override;
    function Add(DiffFile: TAvailableDiffFile): integer;
    function IndexOfName(const Name: string): integer;
  public
    property Items[Index: integer]: TAvailableDiffFile read GetItems write SetItems; default;
  end;

  TDiffDlg = class;

  { TSelectedDiffFile }

  TSelectedDiffFile = class
  private
    fOwner: TDiffDlg;
    fFile: TAvailableDiffFile;        // Selected File
    fCombobox: TComboBox;             // Reference for the user selection GUI.
    fOnlySelCheckBox: TCheckBox;
    function TextContents: string;
    procedure SetIndex(NewIndex: integer);
    procedure SetFileName(aFileName: string);
    procedure UpdateIndex;
  public
    constructor Create(aOwner: TDiffDlg; aCombobox: TComboBox; aOnlySelCheckBox: TCheckBox);
  end;

  { TDiffDlg }
  
  TDiffDlg = class(TForm)
    HelpButton: TBitBtn;
    CloseButton: TBitBtn;
    DiffSynEdit: TSynEdit;
    OpenInEditorButton: TBitBtn;
    ProgressBar1: TProgressBar;
    SynDiffSyn1: TSynDiffSyn;
    Text1FileOpenButton: TButton;
    CancelScanningButton: TBitBtn;
    dlgOpen: TOpenDialog;

    Text2FileOpenButton: TButton;

    // text 1
    Text1GroupBox: TGroupBox;
    Text1Combobox: TComboBox;
    Text1OnlySelectionCheckBox: TCheckBox;
    
    // text 2
    Text2GroupBox: TGroupBox;
    Text2Combobox: TComboBox;
    Text2OnlySelectionCheckBox: TCheckBox;

    // options
    OptionsGroupBox: TCheckGroup;

    procedure CancelScanningButtonClick(Sender: TObject);
    procedure FileOpenClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure OnChangeFlag(Sender: TObject);
    procedure Text1ComboboxChange(Sender: TObject);
    procedure Text2ComboboxChange(Sender: TObject);
  private
    fUpdating: Boolean;
    fIdleConnected: boolean;
    fCancelled: boolean;
    fAvailableFiles: TAvailableDiffFiles;
    fSelectedFile1: TSelectedDiffFile;
    fSelectedFile2: TSelectedDiffFile;
    procedure SetupComponents;
    procedure UpdateDiff;
    procedure SetIdleConnected(const AValue: boolean);
    procedure OnIdle(Sender: TObject; var {%H-}Done: Boolean);
    procedure UpdateProgress(aPosition: Integer);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Init;
    procedure FillTextComboBoxes;
    procedure SaveSettings;
    procedure SetDiffOptions(NewOptions: TTextDiffFlags);
    function GetDiffOptions: TTextDiffFlags;

    property IdleConnected: boolean read fIdleConnected write SetIdleConnected;
  end;
  
function ShowDiffDialog(Text1Index: integer; out Diff: string): TModalResult;

const
  IgnoreCaseCheckBox = 0;
  IgnoreEmptyLineChangesCheckBox = 1;
  IgnoreHeadingSpacesCheckBox = 2;
  IgnoreLineEndsCheckBox = 3;
  IgnoreSpaceCharAmountCheckBox = 4;
  IgnoreSpaceCharsCheckBox = 5;
  IgnoreTrailingSpacesCheckBox = 6;

implementation

{$R *.lfm}

function ShowDiffDialog(Text1Index: integer; out Diff: string): TModalResult;
var
  DiffDlg: TDiffDlg;
  Files: TAvailableDiffFiles;
  i: Integer;
  SrcEdit: TSourceEditor;
begin
  DiffDlg := TDiffDlg.Create(nil);
  Files := TAvailableDiffFiles.Create;
  try
    // Get available files
    for i:=0 to SourceEditorManager.SourceEditorCount - 1 do begin
      SrcEdit := SourceEditorManager.SourceEditors[i]; // FindSourceEditorWithPageIndex(i);
      Files.Add(TAvailableDiffFile.Create(SrcEdit.PageName, SrcEdit, SrcEdit.SelectionAvailable));
    end;
    DiffDlg.fAvailableFiles := Files;
    DiffDlg.fSelectedFile1.SetIndex(Text1Index);
    DiffDlg.Init;
    Result := DiffDlg.ShowModal;
    DiffDlg.SaveSettings;
    // "Open in editor" button returns mrYes.
    if Result=mrYes then
      Diff := DiffDlg.DiffSynEdit.Text;
  finally
    Files.Free;
    DiffDlg.Free;
  end;
end;

{ TSelectedDiffFile }

constructor TSelectedDiffFile.Create(aOwner: TDiffDlg; aCombobox: TComboBox;
  aOnlySelCheckBox: TCheckBox);
begin
  inherited Create;
  fOwner := aOwner;
  fCombobox := aCombobox;
  fOnlySelCheckBox := aOnlySelCheckBox;
end;

function TSelectedDiffFile.TextContents: string;
var
  dat: TStringListUTF8;
begin
  if fFile = nil then Exit('');
  if fFile.Editor = nil then
  begin
    dat := TStringListUTF8.Create;
    try
      dat.LoadFromFile(fFile.Name);
      Result := dat.Text;
    finally
      dat.Free;
    end;
  end
  else begin
    if (fFile.SelectionAvailable and fOnlySelCheckBox.Checked) then
      Result := fFile.Editor.EditorComponent.SelText
    else
      Result := fFile.Editor.EditorComponent.Lines.Text;
  end;
end;

procedure TSelectedDiffFile.SetIndex(NewIndex: integer);
var
  OldFile: TAvailableDiffFile;
begin
  OldFile:=fFile;
  if (NewIndex>=0) and (NewIndex<fOwner.fAvailableFiles.Count) then begin
    fFile:=fOwner.fAvailableFiles[NewIndex];
    fCombobox.Text:=fFile.Name;
    fOnlySelCheckBox.Enabled:=fFile.SelectionAvailable;
  end else begin
    fFile:=nil;
    fCombobox.Text:='';
    fOnlySelCheckBox.Enabled:=false;
  end;
  if fFile<>OldFile then fOwner.UpdateDiff;
end;

procedure TSelectedDiffFile.SetFileName(aFileName: string);
// Assumes that aFileName is already in fCombobox.Items.
begin
  fCombobox.ItemIndex := fCombobox.Items.IndexOf(aFileName);
  SetIndex(fCombobox.Items.IndexOf(aFileName));
end;

procedure TSelectedDiffFile.UpdateIndex;
begin
  SetIndex(fCombobox.Items.IndexOf(fCombobox.Text));
end;

{ TDiffDlg }

constructor TDiffDlg.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  fUpdating := False;
  fCancelled := False;
  fSelectedFile1 := TSelectedDiffFile.Create(Self, Text1Combobox, Text1OnlySelectionCheckBox);
  fSelectedFile2 := TSelectedDiffFile.Create(Self, Text2Combobox, Text2OnlySelectionCheckBox);
  Caption := lisCaptionCompareFiles;
  IDEDialogLayoutList.ApplyLayout(Self,600,500);
  SetupComponents;
end;

destructor TDiffDlg.Destroy;
begin
  fSelectedFile2.Free;
  fSelectedFile1.Free;
  inherited Destroy;
end;

procedure TDiffDlg.OnChangeFlag(Sender: TObject);
begin
  UpdateDiff;
end;

procedure TDiffDlg.FileOpenClick(Sender: TObject);
begin
  if dlgOpen.Execute then
  begin
    //only add new files
    if Text1ComboBox.Items.IndexOf(dlgOpen.FileName) = -1 then
    begin
      fAvailableFiles.Add(TAvailableDiffFile.Create(dlgOpen.FileName,nil,False));
      Text1ComboBox.Items.Add(dlgOpen.FileName);
      Text2ComboBox.Items.Add(dlgOpen.FileName);
    end;
    //set the combobox and make the diff
    if TButton(Sender) = Text1FileOpenButton then
      fSelectedFile1.SetFileName(dlgOpen.FileName)
    else
      fSelectedFile2.SetFileName(dlgOpen.FileName);
  end;
end;

procedure TDiffDlg.CancelScanningButtonClick(Sender: TObject);
begin
  fCancelled := True;
end;

procedure TDiffDlg.HelpButtonClick(Sender: TObject);
begin
  LazarusHelp.ShowHelpForIDEControl(Self);
end;

procedure TDiffDlg.Text1ComboboxChange(Sender: TObject);
begin
  fSelectedFile1.UpdateIndex;
end;

procedure TDiffDlg.Text2ComboboxChange(Sender: TObject);
begin
  fSelectedFile2.UpdateIndex;
end;

procedure TDiffDlg.SetupComponents;
begin
  // text 1
  Text1GroupBox.Caption:=lisDiffDlgFile1;
  Text1OnlySelectionCheckBox.Caption:=lisDiffDlgOnlySelection;
  Text1FileOpenButton.Caption:='...';

  // text 2
  Text2GroupBox.Caption:=lisDiffDlgFile2;
  Text2OnlySelectionCheckBox.Caption:=lisDiffDlgOnlySelection;
  Text2FileOpenButton.Caption:='...';

  // options
  with OptionsGroupBox do
  begin
    Caption:=lisOptions;
    Items.Add(lisDiffDlgCaseInsensitive);
    Items.Add(lisDiffDlgIgnoreIfEmptyLinesWereAdd);
    Items.Add(lisDiffDlgIgnoreSpacesAtStartOfLine);
    Items.Add(lisDiffDlgIgnoreSpacesAtEndOfLine);
    Items.Add(lisDiffDlgIgnoreIfLineEndCharsDiffe);
    Items.Add(lisDiffDlgIgnoreIfSpaceCharsWereAdd);
    Items.Add(lisDiffDlgIgnoreSpaces);
  end;

  // buttons
  IDEImages.AssignImage(CancelScanningButton, 'btn_cancel');
  CloseButton.Caption:=lisClose;
  OpenInEditorButton.Caption:=lisDiffDlgOpenDiffInEditor;
  HelpButton.Caption:=lisMenuHelp;

  OpenInEditorButton.LoadGlyphFromStock(idButtonOpen);
  if OpenInEditorButton.Glyph.Empty then
    IDEImages.AssignImage(OpenInEditorButton, 'laz_open');
  
  // dialogs
  dlgOpen.Title:=lisOpenExistingFile;
  dlgOpen.Filter:=dlgFilterAll+' ('+GetAllFilesMask+')|'+GetAllFilesMask
                 +'|'+dlgFilterLazarusUnit+' (*.pas;*.pp)|*.pas;*.pp'
                 +'|'+dlgFilterLazarusProject+' (*.lpi)|*.lpi'
                 +'|'+dlgFilterLazarusForm+' (*.lfm;*.dfm)|*.lfm;*.dfm'
                 +'|'+dlgFilterLazarusPackage+' (*.lpk)|*.lpk'
                 +'|'+dlgFilterLazarusProjectSource+' (*.lpr)|*.lpr';

  // diff
  EditorOpts.GetSynEditSettings(DiffSynEdit);
end;

procedure TDiffDlg.UpdateDiff;
begin
  IdleConnected:=True;
end;

procedure TDiffDlg.UpdateProgress(aPosition: Integer);
begin
  ProgressBar1.Position := aPosition;
  Application.ProcessMessages;
end;

procedure TDiffDlg.SetIdleConnected(const AValue: boolean);
begin
  if fIdleConnected=AValue then exit;
  fIdleConnected:=AValue;
  if fIdleConnected then
    Application.AddOnIdleHandler(@OnIdle)
  else
    Application.RemoveOnIdleHandler(@OnIdle);
end;

procedure TDiffDlg.OnIdle(Sender: TObject; var Done: Boolean);
var
  Text1Src, Text2Src: string;
  DiffOutput: TDiffOutput;
begin
  IdleConnected := false;
  if fUpdating then Exit;
  fUpdating := True;
  DiffSynEdit.Lines.Text := '';
  Text1Src := fSelectedFile1.TextContents;
  Text2Src := fSelectedFile2.TextContents;
  if (Text1Src <> '') and (Text2Src <> '') then
  begin
    Text1GroupBox.Enabled := False;
    Text2GroupBox.Enabled := False;
    OpenInEditorButton.Enabled := False;
    //CancelScanningButton.Enabled := True;

    DiffOutput := TDiffOutput.Create(Text1Src, Text2Src, GetDiffOptions);
    try
      ProgressBar1.Max := DiffOutput.GetProgressMax;
      DiffOutput.OnProgressPos := @UpdateProgress;
      DiffSynEdit.Lines.Text := DiffOutput.CreateTextDiff;
    finally
      DiffOutput.Free;
    end;

    //CancelScanningButton.Enabled := False;
    OpenInEditorButton.Enabled := True;
    Text2GroupBox.Enabled := True;
    Text1GroupBox.Enabled := True;
  end;
  fUpdating:=False;
end;

procedure TDiffDlg.Init;
var
  LastText2Name: String;
  i: Integer;
begin
  // fill all diff file names
  FillTextComboBoxes;
  
  // get recent Text 2
  i:=0;
  LastText2Name:=InputHistories.DiffText2;
  if LastText2Name<>'' then
    i:=fAvailableFiles.IndexOfName(LastText2Name);
  if i<0 then i:=0;
  if i=fAvailableFiles.IndexOf(fSelectedFile2.fFile) then inc(i);
  fSelectedFile2.SetIndex(i);
  
  // set recent options
  SetDiffOptions(InputHistories.DiffFlags);

  // and action ...
  UpdateDiff;
end;

procedure TDiffDlg.FillTextComboBoxes;
var
  i: Integer;
begin
  // Text 1
  Text1Combobox.Items.BeginUpdate;
  Text1Combobox.Items.Clear;
  for i:=0 to fAvailableFiles.Count-1 do
    Text1Combobox.Items.Add(fAvailableFiles[i].Name);
  Text1Combobox.Items.EndUpdate;

  // Text 2
  Text2Combobox.Items.BeginUpdate;
  Text2Combobox.Items.Clear;
  for i:=0 to fAvailableFiles.Count-1 do
    Text2Combobox.Items.Add(fAvailableFiles[i].Name);
  Text2Combobox.Items.EndUpdate;
end;

procedure TDiffDlg.FormCreate(Sender: TObject);
begin
  Text1Combobox.DropDownCount:=EnvironmentOptions.DropDownCount;
  Text2Combobox.DropDownCount:=EnvironmentOptions.DropDownCount;
end;

procedure TDiffDlg.SaveSettings;
begin
  InputHistories.DiffFlags:=GetDiffOptions;
  if (fSelectedFile2<>nil) and (fSelectedFile2.fFile<>nil) then begin
    InputHistories.DiffText2:=fSelectedFile2.fFile.Name;
    InputHistories.DiffText2OnlySelection:=Text2OnlySelectionCheckBox.Checked;
  end else begin
    InputHistories.DiffText2:='';
    InputHistories.DiffText2OnlySelection:=false;
  end;
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TDiffDlg.SetDiffOptions(NewOptions: TTextDiffFlags);
begin
  OptionsGroupBox.Checked[IgnoreCaseCheckBox]:=tdfIgnoreCase in NewOptions;
  OptionsGroupBox.Checked[IgnoreEmptyLineChangesCheckBox]:=tdfIgnoreEmptyLineChanges in NewOptions;
  OptionsGroupBox.Checked[IgnoreHeadingSpacesCheckBox]:=tdfIgnoreHeadingSpaces in NewOptions;
  OptionsGroupBox.Checked[IgnoreLineEndsCheckBox]:=tdfIgnoreLineEnds in NewOptions;
  OptionsGroupBox.Checked[IgnoreSpaceCharAmountCheckBox]:=tdfIgnoreSpaceCharAmount in NewOptions;
  OptionsGroupBox.Checked[IgnoreSpaceCharsCheckBox]:=tdfIgnoreSpaceChars in NewOptions;
  OptionsGroupBox.Checked[IgnoreTrailingSpacesCheckBox]:=tdfIgnoreTrailingSpaces in NewOptions;
end;

function TDiffDlg.GetDiffOptions: TTextDiffFlags;
begin
  Result:=[];
  if OptionsGroupBox.Checked[IgnoreCaseCheckBox] then
    Include(Result,tdfIgnoreCase);
  if OptionsGroupBox.Checked[IgnoreEmptyLineChangesCheckBox] then
    Include(Result,tdfIgnoreEmptyLineChanges);
  if OptionsGroupBox.Checked[IgnoreHeadingSpacesCheckBox] then
    Include(Result,tdfIgnoreHeadingSpaces);
  if OptionsGroupBox.Checked[IgnoreLineEndsCheckBox] then
    Include(Result,tdfIgnoreLineEnds);
  if OptionsGroupBox.Checked[IgnoreSpaceCharAmountCheckBox] then
    Include(Result,tdfIgnoreSpaceCharAmount);
  if OptionsGroupBox.Checked[IgnoreSpaceCharsCheckBox] then
    Include(Result,tdfIgnoreSpaceChars);
  if OptionsGroupBox.Checked[IgnoreTrailingSpacesCheckBox] then
    Include(Result,tdfIgnoreTrailingSpaces);
end;

{ TAvailableDiffFile }

constructor TAvailableDiffFile.Create(const NewName: string; NewEditor: TSourceEditor;
  NewSelectionAvailable: boolean);
begin
  Name:=NewName;
  Editor:=NewEditor;
  SelectionAvailable:=NewSelectionAvailable;
end;

{ TAvailableDiffFiles }

function TAvailableDiffFiles.GetItems(Index: integer): TAvailableDiffFile;
begin
  Result:=TAvailableDiffFile(inherited Items[Index]);
end;

procedure TAvailableDiffFiles.SetItems(Index: integer; const AValue: TAvailableDiffFile);
begin
  inherited Items[Index]:=AValue;
end;

procedure TAvailableDiffFiles.Clear;
var
  i: Integer;
begin
  for i:=0 to Count-1 do
    Items[i].Free;
  inherited Clear;
end;

function TAvailableDiffFiles.Add(DiffFile: TAvailableDiffFile): integer;
begin
  Result:=inherited Add(DiffFile);
end;

function TAvailableDiffFiles.IndexOfName(const Name: string): integer;
begin
  Result:=Count-1;
  while (Result>=0) and (Items[Result].Name<>Name) do dec(Result);
end;

end.

