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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
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
  Classes, SysUtils, Math, Forms, Controls, Buttons, StdCtrls, FileUtil,
  lazutf8classes, LazarusIDEStrConsts, EditorOptions, LCLType, IDEWindowIntf,
  IDEHelpIntf, InputHistory, DiffPatch, ExtCtrls, Dialogs, ComCtrls, SynEdit,
  SynHighlighterDiff, SourceEditor;

type

  { TAvailableDiffFile }

  TAvailableDiffFile = class
  public
    Name: string;
    Editor: TSourceEditor;
    SelectionAvailable: boolean;
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
    procedure HelpButtonClick(Sender: TObject);
    procedure OnChangeFlag(Sender: TObject);
    procedure Text1ComboboxChange(Sender: TObject);
    procedure Text2ComboboxChange(Sender: TObject);
  private
    fUpdating: Boolean;
    fIdleConnected: boolean;
    fCancelled: boolean;
    fAvailableFiles: TAvailableDiffFiles;
    fSelectedFile1: TAvailableDiffFile;
    fSelectedFile2: TAvailableDiffFile;
    procedure SetupComponents;
    procedure UpdateDiff;
    procedure SetIdleConnected(const AValue: boolean);
    procedure OnIdle(Sender: TObject; var Done: Boolean);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Init;
    procedure FillTextComboBoxes;
    procedure SetText1Index(NewIndex: integer);
    procedure SetText2Index(NewIndex: integer);
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
    DiffDlg.SetText1Index(Text1Index);
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

{ TDiffDlg }

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
    if TButton(Sender).Name = 'Text1FileOpenButton'then
    with Text1ComboBox do
    begin
      ItemIndex := Items.IndexOf(dlgOpen.FileName);
      SetText1Index(Items.IndexOf(dlgOpen.FileName));
    end
    else
    with Text2ComboBox do
    begin
      ItemIndex := Items.IndexOf(dlgOpen.FileName);
      SetText2Index(Items.IndexOf(dlgOpen.FileName));
    end;
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
  SetText1Index(Text1Combobox.Items.IndexOf(Text1Combobox.Text));
end;

procedure TDiffDlg.Text2ComboboxChange(Sender: TObject);
begin
  SetText2Index(Text2Combobox.Items.IndexOf(Text2Combobox.Text));
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
    Caption:=dlgFROpts;
    Items.Add(lisDiffDlgCaseInsensitive);
    Items.Add(lisDiffDlgIgnoreIfEmptyLinesWereAdd);
    Items.Add(lisDiffDlgIgnoreSpacesAtStartOfLine);
    Items.Add(lisDiffDlgIgnoreSpacesAtEndOfLine);
    Items.Add(lisDiffDlgIgnoreIfLineEndCharsDiffe);
    Items.Add(lisDiffDlgIgnoreIfSpaceCharsWereAdd);
    Items.Add(lisDiffDlgIgnoreSpaces);
  end;

  // buttons
  CancelScanningButton.LoadGlyphFromResourceName(hInstance, 'btn_cancel');
  CloseButton.Caption:=lisClose;
  OpenInEditorButton.Caption:=lisDiffDlgOpenDiffInEditor;
  HelpButton.Caption:=lisMenuHelp;

  OpenInEditorButton.LoadGlyphFromStock(idButtonOpen);
  if OpenInEditorButton.Glyph.Empty then
    OpenInEditorButton.LoadGlyphFromResourceName(HInstance, 'laz_open');
  
  // dialogs
  dlgOpen.Title:=lisOpenExistingFile;
  dlgOpen.Filter:=dlgAllFiles+' ('+GetAllFilesMask+')|'+GetAllFilesMask
                 +'|'+lisLazarusUnit+' (*.pas;*.pp)|*.pas;*.pp'
                 +'|'+lisLazarusProject+' (*.lpi)|*.lpi'
                 +'|'+lisLazarusForm+' (*.lfm;*.dfm)|*.lfm;*.dfm'
                 +'|'+lisLazarusPackage+' (*.lpk)|*.lpk'
                 +'|'+lisLazarusProjectSource+' (*.lpr)|*.lpr';

  // diff
  EditorOpts.GetSynEditSettings(DiffSynEdit);
end;

procedure TDiffDlg.UpdateDiff;
begin
  IdleConnected:=True;
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
  dat: TStringListUTF8;
  DiffOutput: TDiffOutput;
begin
  IdleConnected := false;
  if fUpdating then Exit;
  fUpdating := True;
  DiffSynEdit.Lines.Text := '';
  if (fSelectedFile1 <> nil) and (fSelectedFile2 <> nil) then begin
    if fSelectedFile1.Editor = nil then
      begin
        dat := TStringListUTF8.Create;
        dat.LoadFromFile(fSelectedFile1.Name);
        Text1Src := dat.Text;
        dat.Free;
      end
    else begin
      if (fSelectedFile1.SelectionAvailable and Text1OnlySelectionCheckBox.Checked) then
        Text1Src := fSelectedFile1.Editor.EditorComponent.SelText
      else
        Text1Src := fSelectedFile1.Editor.EditorComponent.Lines.Text;
    end;

    if fSelectedFile2.Editor = nil then
      begin
        dat := TStringListUTF8.Create;
        dat.LoadFromFile(fSelectedFile2.Name);
        Text2Src := dat.Text;
        dat.Free;
      end
    else begin
      if (fSelectedFile2.SelectionAvailable and Text2OnlySelectionCheckBox.Checked) then
        Text2Src := fSelectedFile2.Editor.EditorComponent.SelText
      else
        Text2Src := fSelectedFile2.Editor.EditorComponent.Lines.Text;
    end;

    Text1GroupBox.Enabled := False;
    Text2GroupBox.Enabled := False;
    OpenInEditorButton.Enabled := False;
    //CancelScanningButton.Enabled := True;

    DiffOutput:=TDiffOutput.Create(Text1Src, Text2Src, GetDiffOptions, ProgressBar1);
    try
      DiffSynEdit.Lines.Text:=DiffOutput.CreateTextDiff;
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

constructor TDiffDlg.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  fUpdating := False;
  fCancelled := False;
  Caption := lisCaptionCompareFiles;
  IDEDialogLayoutList.ApplyLayout(Self,600,500);
  SetupComponents;
end;

destructor TDiffDlg.Destroy;
begin
  inherited Destroy;
end;

procedure TDiffDlg.Init;
var
  LastText2Name: String;
  i: Integer;
begin
  // fill all diff file names
  FillTextComboBoxes;
  
  // get recent Text 2
  LastText2Name:=InputHistories.DiffText2;
  if LastText2Name<>'' then
    i:=fAvailableFiles.IndexOfName(LastText2Name);
  if i<0 then i:=0;
  if i=fAvailableFiles.IndexOf(fSelectedFile2) then inc(i);
  SetText2Index(i);
  
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

procedure TDiffDlg.SetText1Index(NewIndex: integer);
var
  OldText1: TAvailableDiffFile;
begin
  OldText1:=fSelectedFile1;
  if (NewIndex>=0) and (NewIndex<fAvailableFiles.Count) then begin
    fSelectedFile1:=fAvailableFiles[NewIndex];
    Text1Combobox.Text:=fSelectedFile1.Name;
    Text1OnlySelectionCheckBox.Enabled:=fSelectedFile1.SelectionAvailable;
  end else begin
    fSelectedFile1:=nil;
    Text1Combobox.Text:='';
    Text1OnlySelectionCheckBox.Enabled:=false;
  end;
  if fSelectedFile1<>OldText1 then UpdateDiff;
end;

procedure TDiffDlg.SetText2Index(NewIndex: integer);
var
  OldText2: TAvailableDiffFile;
begin
  OldText2:=fSelectedFile2;
  if (NewIndex>=0) and (NewIndex<fAvailableFiles.Count) then begin
    fSelectedFile2:=fAvailableFiles[NewIndex];
    Text2Combobox.Text:=fSelectedFile2.Name;
    Text2OnlySelectionCheckBox.Enabled:=fSelectedFile2.SelectionAvailable;
  end else begin
    fSelectedFile2:=nil;
    Text2Combobox.Text:='';
    Text2OnlySelectionCheckBox.Enabled:=false;
  end;
  if fSelectedFile2<>OldText2 then UpdateDiff;
end;

procedure TDiffDlg.SaveSettings;
begin
  InputHistories.DiffFlags:=GetDiffOptions;
  if fSelectedFile2<>nil then begin
    InputHistories.DiffText2:=fSelectedFile2.Name;
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

