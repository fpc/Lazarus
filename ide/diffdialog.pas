{  $Id$  }
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
    The TDiffDlg is the dialog for showing the differences between two files.

}

unit DiffDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, Forms, Controls, Buttons, StdCtrls, FileUtil,
  LazarusIDEStrConsts, EditorOptions, IDEWindowIntf, LCLType,
  InputHistory, DiffPatch, ExtCtrls, Dialogs, SynEdit, SynHighlighterDiff, IDEContextHelpEdit,
  SourceEditor;

type

  { TDiffFile }

  TDiffFile = class
  public
    Name: string;
    Editor: TSourceEditor;
    SelectionAvailable: boolean;
    constructor Create(const NewName: string; NewEditor: TSourceEditor;
      NewSelectionAvailable: boolean);
  end;


  { TDiffFiles }

  TDiffFiles = class(TList)
  private
    function GetItems(Index: integer): TDiffFile;
    procedure SetItems(Index: integer; const AValue: TDiffFile);
  public
    procedure Clear; override;
    function Add(DiffFile: TDiffFile): integer;
    function IndexOfName(const Name: string): integer;
  public
    property Items[Index: integer]: TDiffFile read GetItems write SetItems; default;
  end;


  { TDiffDlg }
  
  TDiffDlg = class(TForm)
    HelpButton: TBitBtn;
    CloseButton: TBitBtn;
    DiffSynEdit: TSynEdit;
    OpenInEditorButton: TBitBtn;
    SaveDiffButton: TBitBtn;
    SynDiffSyn1: TSynDiffSyn;
    Text1FileOpenButton: TButton;
    dlgSave: TSaveDialog;
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

    procedure FileOpenClick(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure OnChangeFlag(Sender: TObject);
    procedure SaveDiffButtonClick(Sender: TObject);
    procedure Text1ComboboxChange(Sender: TObject);
    procedure Text2ComboboxChange(Sender: TObject);
  private
    fDiffNeedsUpdate: boolean;
    FLockCount: integer;
    procedure SetupComponents;
    procedure UpdateDiff;
  public
    Files: TDiffFiles;
    Text1: TDiffFile;
    Text2: TDiffFile;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Init;
    procedure FillTextComboBoxes;
    procedure SetText1Index(NewIndex: integer);
    procedure SetText2Index(NewIndex: integer);
    procedure SaveSettings;
    procedure SetDiffOptions(NewOptions: TTextDiffFlags);
    function GetDiffOptions: TTextDiffFlags;
    procedure BeginUpdate;
    procedure EndUpdate;
  end;
  
function ShowDiffDialog(Text1Index: integer;
  var OpenDiffInEditor: boolean; var Diff: string): TModalResult;

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

function ShowDiffDialog(Text1Index: integer;
  var OpenDiffInEditor: boolean; var Diff: string): TModalResult;
var
  DiffDlg: TDiffDlg;
  Files: TDiffFiles;
  i: Integer;
  SrcEdit: TSourceEditor;
begin
  Files := TDiffFiles.Create;
  for i:=0 to SourceEditorManager.SourceEditorCount - 1 do begin
    SrcEdit := SourceEditorManager.SourceEditors[i]; // FindSourceEditorWithPageIndex(i);
    Files.Add(TDiffFile.Create(SrcEdit.PageName, SrcEdit, SrcEdit.SelectionAvailable));
  end;

  OpenDiffInEditor:=false;
  DiffDlg:=TDiffDlg.Create(nil);
  DiffDlg.BeginUpdate;
  DiffDlg.Files:=Files;
  DiffDlg.SetText1Index(Text1Index);
  DiffDlg.Init;
  DiffDlg.EndUpdate;
  Result:=DiffDlg.ShowModal;
  DiffDlg.SaveSettings;

  if Result=mrYes then begin
    OpenDiffInEditor:=true;
    Diff:=DiffDlg.DiffSynEdit.Text;
    Result:=mrOk;
  end;

  Files.Free;
  DiffDlg.Free;
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
      Files.Add(TDiffFile.Create(dlgOpen.FileName,nil,False));
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

procedure TDiffDlg.HelpButtonClick(Sender: TObject);
begin
  ShowContextHelpForIDE(Self);
end;

procedure TDiffDlg.SaveDiffButtonClick(Sender: TObject);
begin
  if dlgSave.Execute then
    DiffSynEdit.Lines.SaveToFile(UTF8ToSys(dlgSave.FileName));
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
  Text1GroupBox.Caption:=lisDiffDlgText1;
  Text1OnlySelectionCheckBox.Caption:=lisDiffDlgOnlySelection;
  Text1FileOpenButton.Caption:='...';

  // text 2
  Text2GroupBox.Caption:=lisDiffDlgText2;
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
  CloseButton.Caption:=lisMenuClose;
  OpenInEditorButton.Caption:=lisDiffDlgOpenDiffInEditor;
  SaveDiffButton.Caption:=lisSave;
  HelpButton.Caption:=lisMenuHelp;

  OpenInEditorButton.LoadGlyphFromStock(idButtonOpen);
  if OpenInEditorButton.Glyph.Empty then
    OpenInEditorButton.LoadGlyphFromLazarusResource('laz_open');
  
  SaveDiffButton.LoadGlyphFromStock(idButtonSave);
  if SaveDiffButton.Glyph.Empty then
    SaveDiffButton.LoadGlyphFromLazarusResource('laz_save');

  // dialogs
  dlgOpen.Title:=lisOpenExistingFile;
  dlgOpen.Filter:=dlgAllFiles+' ('+GetAllFilesMask+')|'+GetAllFilesMask
                 +'|'+lisLazarusUnit+' (*.pas;*.pp)|*.pas;*.pp'
                 +'|'+lisLazarusProject+' (*.lpi)|*.lpi'
                 +'|'+lisLazarusForm+' (*.lfm;*.dfm)|*.lfm;*.dfm'
                 +'|'+lisLazarusPackage+' (*.lpk)|*.lpk'
                 +'|'+lisLazarusProjectSource+' (*.lpr)|*.lpr';
  dlgSave.Title:=lisSaveFileAs;
  dlgSave.Filter:=dlgAllFiles+' ('+GetAllFilesMask+')|'+GetAllFilesMask;

  // diff
  EditorOpts.GetSynEditSettings(DiffSynEdit);
end;

procedure TDiffDlg.UpdateDiff;
var
  Text1Src, Text2Src: string;
  DiffTxt: String;
  dat : TStrings;
begin
  if FLockCount>0 then begin
    fDiffNeedsUpdate:=true;
    exit;
  end;
  fDiffNeedsUpdate:=false;
  if (Text1=nil) or (Text2=nil) then begin
    DiffSynEdit.Lines.Text:='';
  end else begin
    if Text1.Editor = nil then
      begin
        dat := TStringList.Create;
        dat.LoadFromFile(UTF8ToSys(Text1.Name));
        Text1Src := dat.Text;
        dat.Free;
      end
    else begin
      if (Text1.SelectionAvailable and Text1OnlySelectionCheckBox.Checked) then
        Text1Src := Text1.Editor.EditorComponent.SelText
      else
        Text1Src := Text1.Editor.EditorComponent.Lines.Text;
    end;

    if Text2.Editor = nil then
      begin
        dat := TStringList.Create;
        dat.LoadFromFile(UTF8ToSys(Text2.Name));
        Text2Src := dat.Text;
        dat.Free;
      end
    else begin
      if (Text2.SelectionAvailable and Text2OnlySelectionCheckBox.Checked) then
        Text2Src := Text2.Editor.EditorComponent.SelText
      else
        Text2Src := Text2.Editor.EditorComponent.Lines.Text;
    end;

    DiffTxt:=CreateTextDiff(Text1Src,Text2Src,GetDiffOptions,tdoContext);
    
    DiffSynEdit.Lines.Text:=DiffTxt;
  end;
end;

constructor TDiffDlg.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Caption := lisMenuDiff;
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
    i:=Files.IndexOfName(LastText2Name);
  if i<0 then i:=0;
  if i=Files.IndexOf(Text2) then inc(i);
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
  for i:=0 to Files.Count-1 do
    Text1Combobox.Items.Add(Files[i].Name);
  Text1Combobox.Items.EndUpdate;

  // Text 2
  Text2Combobox.Items.BeginUpdate;
  Text2Combobox.Items.Clear;
  for i:=0 to Files.Count-1 do
    Text2Combobox.Items.Add(Files[i].Name);
  Text2Combobox.Items.EndUpdate;
end;

procedure TDiffDlg.SetText1Index(NewIndex: integer);
var
  OldText1: TDiffFile;
begin
  OldText1:=Text1;
  if (NewIndex>=0) and (NewIndex<Files.Count) then begin
    Text1:=Files[NewIndex];
    Text1Combobox.Text:=Text1.Name;
    Text1OnlySelectionCheckBox.Enabled:=Text1.SelectionAvailable;
  end else begin
    Text1:=nil;
    Text1Combobox.Text:='';
    Text1OnlySelectionCheckBox.Enabled:=false;
  end;
  if Text1<>OldText1 then UpdateDiff;
end;

procedure TDiffDlg.SetText2Index(NewIndex: integer);
var
  OldText2: TDiffFile;
begin
  OldText2:=Text2;
  if (NewIndex>=0) and (NewIndex<Files.Count) then begin
    Text2:=Files[NewIndex];
    Text2Combobox.Text:=Text2.Name;
    Text2OnlySelectionCheckBox.Enabled:=Text2.SelectionAvailable;
  end else begin
    Text2:=nil;
    Text2Combobox.Text:='';
    Text2OnlySelectionCheckBox.Enabled:=false;
  end;
  if Text2<>OldText2 then UpdateDiff;
end;

procedure TDiffDlg.SaveSettings;
begin
  InputHistories.DiffFlags:=GetDiffOptions;
  if Text2<>nil then begin
    InputHistories.DiffText2:=Text2.Name;
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

procedure TDiffDlg.BeginUpdate;
begin
  inc(FLockCount);
end;

procedure TDiffDlg.EndUpdate;
begin
  dec(FLockCount);
  if (FLockCount=0) and fDiffNeedsUpdate then UpdateDiff;
end;

{ TDiffFile }

constructor TDiffFile.Create(const NewName: string; NewEditor: TSourceEditor;
  NewSelectionAvailable: boolean);
begin
  Name:=NewName;
  Editor:=NewEditor;
  SelectionAvailable:=NewSelectionAvailable;
end;

{ TDiffFiles }

function TDiffFiles.GetItems(Index: integer): TDiffFile;
begin
  Result:=TDiffFile(inherited Items[Index]);
end;

procedure TDiffFiles.SetItems(Index: integer; const AValue: TDiffFile);
begin
  inherited Items[Index]:=AValue;
end;

procedure TDiffFiles.Clear;
var
  i: Integer;
begin
  for i:=0 to Count-1 do
    Items[i].Free;
  inherited Clear;
end;

function TDiffFiles.Add(DiffFile: TDiffFile): integer;
begin
  Result:=inherited Add(DiffFile);
end;

function TDiffFiles.IndexOfName(const Name: string): integer;
begin
  Result:=Count-1;
  while (Result>=0) and (Items[Result].Name<>Name) do dec(Result);
end;

end.

