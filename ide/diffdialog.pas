{  $Id$  }
{
 /***************************************************************************
                              diffdialog.pp
                              -------------


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
    The TDiffDialog is the dialog for showing the differences between two files.

}
unit DiffDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Buttons, ExtCtrls, StdCtrls, SynEdit,
  LResources, LazarusIDEStrConsts, EditorOptions, IDEOptionDefs, InputHistory,
  DiffPatch;

type
  TOnGetDiffFile = procedure(TextID: integer; OnlySelection: boolean;
                             var Source: string) of object;


  { TDiffFile }

  TDiffFile = class
  public
    Name: string;
    ID: integer;
    SelectionAvailable: boolean;
    constructor Create(const NewName: string; NewID: integer;
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


  { TDiffDialog }
  
  TDiffDialog = class(TForm)
    // text 1
    Text1GroupBox: TGroupBox;
    Text1Combobox: TComboBox;
    Text1OnlySelectionCheckBox: TCheckBox;
    
    // text 2
    Text2GroupBox: TGroupBox;
    Text2Combobox: TComboBox;
    Text2OnlySelectionCheckBox: TCheckBox;
    
    // diff preview
    DiffGroupbox: TGroupBox;
    DiffSynEdit: TSynEdit;
    
    // options
    OptionsGroupBox: TGroupBox;
    IgnoreCaseCheckBox: TCheckBox;
    IgnoreEmptyLineChangesCheckBox: TCheckBox;
    IgnoreHeadingSpacesCheckBox: TCheckBox;
    IgnoreLineEndsCheckBox: TCheckBox;
    IgnoreSpaceCharAmountCheckBox: TCheckBox;
    IgnoreSpaceCharsCheckBox: TCheckBox;
    IgnoreTrailingSpacesCheckBox: TCheckBox;
    
    // buttons
    CloseButton: TButton;
    OpenInEditorButton: TButton;

    procedure CloseButtonClick(Sender: TObject);
    procedure DiffDialogResize(Sender: TObject);
    procedure OnChangeFlag(Sender: TObject);
    procedure OpenInEditorButtonClick(Sender: TObject);
    procedure OptionsGroupBoxResize(Sender: TObject);
    procedure Text1ComboboxChange(Sender: TObject);
    procedure Text1GroupBoxResize(Sender: TObject);
    procedure Text2ComboboxChange(Sender: TObject);
    procedure Text2GroupBoxResize(Sender: TObject);
  private
    FOnGetDiffFile: TOnGetDiffFile;
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
  public
    property OnGetDiffFile: TOnGetDiffFile
                                       read FOnGetDiffFile write FOnGetDiffFile;
  end;
  
function ShowDiffDialog(Files: TDiffFiles; Text1Index: integer;
  OnGetDiffFile: TOnGetDiffFile;
  var OpenDiffInEditor: boolean; var Diff: string): TModalResult;


implementation

uses
  Math;


function ShowDiffDialog(Files: TDiffFiles; Text1Index: integer;
  OnGetDiffFile: TOnGetDiffFile;
  var OpenDiffInEditor: boolean; var Diff: string): TModalResult;
var
  DiffDlg: TDiffDialog;
begin
  DiffDlg:=TDiffDialog.Create(Application);
  DiffDlg.BeginUpdate;
  DiffDlg.OnGetDiffFile:=OnGetDiffFile;
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

  DiffDlg.Free;
end;

{ TDiffDialog }

procedure TDiffDialog.DiffDialogResize(Sender: TObject);
begin
  // text 1
  with Text1GroupBox do begin
    SetBounds(3,3,(Parent.ClientWidth-3*3) div 2,65);
  end;

  // text 2
  with Text2GroupBox do begin
    SetBounds(Text1GroupBox.Left+Text1GroupBox.Width+3,Text1GroupBox.Top,
              Text1GroupBox.Width,Text1GroupBox.Height);
  end;

  // diff preview
  with DiffGroupbox do begin
    SetBounds(Text1GroupBox.Left,Text1GroupBox.Top+Text1GroupBox.Height+5,
              Parent.ClientWidth-2*Text1GroupBox.Left,Max(1,Parent.ClientHeight-226));
  end;

  // options
  with OptionsGroupBox do begin
    SetBounds(Text1GroupBox.Left,DiffGroupbox.Top+DiffGroupbox.Height+5,
              DiffGroupbox.Width,106);
  end;

  // buttons
  with CloseButton do begin
    SetBounds(Parent.ClientWidth-300,Parent.ClientHeight-32,75,Height);
  end;
  
  with OpenInEditorButton do begin
    SetBounds(CloseButton.Left+CloseButton.Width+10,CloseButton.Top,
              150,CloseButton.Height);
  end;
end;

procedure TDiffDialog.OnChangeFlag(Sender: TObject);
begin
  UpdateDiff;
end;

procedure TDiffDialog.OpenInEditorButtonClick(Sender: TObject);
begin
  ModalResult:=mrYes;
end;

procedure TDiffDialog.CloseButtonClick(Sender: TObject);
begin
  ModalResult:=mrOk;
end;

procedure TDiffDialog.OptionsGroupBoxResize(Sender: TObject);
var
  y: Integer;
  x: Integer;
  W: Integer;
begin
  y:=0;
  x:=4;
  W:=(OptionsGroupBox.ClientWidth div 2)-8;
  
  with IgnoreCaseCheckBox do begin
    SetBounds(x,y,w,Height);
    inc(y,Height+2);
  end;

  with IgnoreEmptyLineChangesCheckBox do begin
    SetBounds(x,y,w,Height);
    inc(y,Height+2);
  end;
  
  with IgnoreHeadingSpacesCheckBox do begin
    SetBounds(x,y,w,Height);
    inc(y,Height+2);
  end;

  with IgnoreLineEndsCheckBox do begin
    SetBounds(x,y,w+w,Height);
    inc(y,Height+2);
  end;

  x:=(OptionsGroupBox.ClientWidth div 2)+4;
  y:=2;
  with IgnoreSpaceCharAmountCheckBox do begin
    SetBounds(x,y,w,Height);
    inc(y,Height+2);
  end;

  with IgnoreSpaceCharsCheckBox do begin
    SetBounds(x,y,w,Height);
    inc(y,Height+2);
  end;

  with IgnoreTrailingSpacesCheckBox do begin
    SetBounds(x,y,w,Height);
  end;
end;

procedure TDiffDialog.Text1ComboboxChange(Sender: TObject);
begin
  SetText1Index(Text1Combobox.Items.IndexOf(Text1Combobox.Text));
end;

procedure TDiffDialog.Text1GroupBoxResize(Sender: TObject);
begin
  with Text1Combobox do begin
    SetBounds(0,0,Parent.ClientWidth,Height);
  end;
  
  with Text1OnlySelectionCheckBox do begin
    SetBounds(10,Text1Combobox.Top+Text1Combobox.Height+1,
              Parent.ClientWidth-20,Height);
  end;
end;

procedure TDiffDialog.Text2ComboboxChange(Sender: TObject);
begin
  SetText2Index(Text2Combobox.Items.IndexOf(Text2Combobox.Text));
end;

procedure TDiffDialog.Text2GroupBoxResize(Sender: TObject);
begin
  with Text2Combobox do begin
    SetBounds(0,0,Parent.ClientWidth,Height);
  end;

  with Text2OnlySelectionCheckBox do begin
    SetBounds(10,Text1Combobox.Top+Text1Combobox.Height+1,
              Parent.ClientWidth-20,Height);
  end;
end;

procedure TDiffDialog.SetupComponents;
begin
  // text 1
  Text1GroupBox:=TGroupBox.Create(Self);
  with Text1GroupBox do begin
    Name:='Text1GroupBox';
    Parent:=Self;
    Caption:=lisDiffDlgText1;
    OnResize:=@Text1GroupBoxResize;
  end;
  
  Text1Combobox:=TComboBox.Create(Self);
  with Text1Combobox do begin
    Name:='Text1Combobox';
    Parent:=Text1GroupBox;
    OnChange:=@Text1ComboboxChange;
  end;
  
  Text1OnlySelectionCheckBox:=TCheckBox.Create(Self);
  with Text1OnlySelectionCheckBox do begin
    Name:='Text1OnlySelectionCheckBox';
    Parent:=Text1GroupBox;
    Caption:=lisDiffDlgOnlySelection;
    OnClick:=@OnChangeFlag;
  end;

  // text 2
  Text2GroupBox:=TGroupBox.Create(Self);
  with Text2GroupBox do begin
    Name:='Text2GroupBox';
    Parent:=Self;
    Caption:=lisDiffDlgText2;
    OnResize:=@Text2GroupBoxResize;
  end;

  Text2Combobox:=TComboBox.Create(Self);
  with Text2Combobox do begin
    Name:='Text2Combobox';
    Parent:=Text2GroupBox;
    OnChange:=@Text2ComboboxChange;
  end;

  Text2OnlySelectionCheckBox:=TCheckBox.Create(Self);
  with Text2OnlySelectionCheckBox do begin
    Name:='Text2OnlySelectionCheckBox';
    Parent:=Text2GroupBox;
    Caption:=lisDiffDlgOnlySelection;
    OnClick:=@OnChangeFlag;
  end;

  // diff preview
  DiffGroupbox:=TGroupBox.Create(Self);
  with DiffGroupbox do begin
    Name:='DiffGroupbox';
    Parent:=Self;
    Caption:=lisMenuDiff;
  end;
  
  DiffSynEdit:=TSynEdit.Create(Self);
  with DiffSynEdit do begin
    Name:='DiffSynEdit';
    Parent:=DiffGroupbox;
    Gutter.Visible:=false;
    Align:=alClient;
  end;

  // options
  OptionsGroupBox:=TGroupBox.Create(Self);
  with OptionsGroupBox do begin
    Name:='OptionsGroupBox';
    Parent:=Self;
    Caption:=dlgFROpts;
    OnResize:=@OptionsGroupBoxResize;
  end;
  
  IgnoreCaseCheckBox:=TCheckBox.Create(Self);
  with IgnoreCaseCheckBox do begin
    Name:='IgnoreCaseCheckBox';
    Parent:=OptionsGroupBox;
    Caption:=lisDiffDlgCaseInsensitive;
    OnClick:=@OnChangeFlag;
  end;
  
  IgnoreEmptyLineChangesCheckBox:=TCheckBox.Create(Self);
  with IgnoreEmptyLineChangesCheckBox do begin
    Name:='IgnoreEmptyLineChangesCheckBox';
    Parent:=OptionsGroupBox;
    Caption:=lisDiffDlgIgnoreIfEmptyLinesWereAdd;
    OnClick:=@OnChangeFlag;
  end;
  
  IgnoreHeadingSpacesCheckBox:=TCheckBox.Create(Self);
  with IgnoreHeadingSpacesCheckBox do begin
    Name:='IgnoreHeadingSpacesCheckBox';
    Parent:=OptionsGroupBox;
    Caption:=lisDiffDlgIgnoreSpacesAtStartOfLine;
    OnClick:=@OnChangeFlag;
  end;

  IgnoreTrailingSpacesCheckBox:=TCheckBox.Create(Self);
  with IgnoreTrailingSpacesCheckBox do begin
    Name:='IgnoreTrailingSpacesCheckBox';
    Parent:=OptionsGroupBox;
    Caption:=lisDiffDlgIgnoreSpacesAtEndOfLine;
    OnClick:=@OnChangeFlag;
  end;

  IgnoreLineEndsCheckBox:=TCheckBox.Create(Self);
  with IgnoreLineEndsCheckBox do begin
    Name:='IgnoreLineEndsCheckBox';
    Parent:=OptionsGroupBox;
    Caption:=lisDiffDlgIgnoreIfLineEndCharsDiffe;
    OnClick:=@OnChangeFlag;
  end;

  IgnoreSpaceCharAmountCheckBox:=TCheckBox.Create(Self);
  with IgnoreSpaceCharAmountCheckBox do begin
    Name:='IgnoreSpaceCharAmountCheckBox';
    Parent:=OptionsGroupBox;
    Caption:=lisDiffDlgIgnoreIfSpaceCharsWereAdd;
    OnClick:=@OnChangeFlag;
  end;

  IgnoreSpaceCharsCheckBox:=TCheckBox.Create(Self);
  with IgnoreSpaceCharsCheckBox do begin
    Name:='IgnoreSpaceCharsCheckBox';
    Parent:=OptionsGroupBox;
    Caption:=lisDiffDlgIgnoreSpaces;
    OnClick:=@OnChangeFlag;
  end;

  // buttons
  CloseButton:=TButton.Create(Self);
  with CloseButton do begin
    Name:='CloseButton';
    Parent:=Self;
    Caption:=lisMenuClose;
    OnClick:=@CloseButtonClick;
  end;
  
  OpenInEditorButton:=TButton.Create(Self);
  with OpenInEditorButton do begin
    Name:='OpenInEditorButton';
    Parent:=Self;
    Caption:=lisDiffDlgOpenDiffInEditor;
    OnClick:=@OpenInEditorButtonClick;
  end;
end;

procedure TDiffDialog.UpdateDiff;
var
  Text1Src, Text2Src: string;
  DiffTxt: String;
begin
  if FLockCount>0 then begin
    fDiffNeedsUpdate:=true;
    exit;
  end;
  fDiffNeedsUpdate:=false;
  if (Text1=nil) or (Text2=nil) then begin
    DiffSynEdit.Lines.Text:='';
  end else begin
    OnGetDiffFile(Text1.ID,
      Text1.SelectionAvailable and Text1OnlySelectionCheckBox.Checked,
      Text1Src);
    OnGetDiffFile(Text2.ID,
      Text2.SelectionAvailable and Text2OnlySelectionCheckBox.Checked,
      Text2Src);
    DiffTxt:=CreateTextDiff(Text1Src,Text2Src,GetDiffOptions);
    DiffSynEdit.Lines.Text:=DiffTxt;
  end;
end;

constructor TDiffDialog.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  if LazarusResources.Find(Classname)=nil then begin
    Name:='DiffDialog';
    Caption := lisMenuDiff;
    Width:=600;
    Height:=400;
    Position:=poScreenCenter;
    OnResize:=@DiffDialogResize;
    SetupComponents;
  end;
  IDEDialogLayoutList.ApplyLayout(Self,600,400);
  OnResize(nil);
end;

destructor TDiffDialog.Destroy;
begin
  inherited Destroy;
end;

procedure TDiffDialog.Init;
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

procedure TDiffDialog.FillTextComboBoxes;
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

procedure TDiffDialog.SetText1Index(NewIndex: integer);
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

procedure TDiffDialog.SetText2Index(NewIndex: integer);
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

procedure TDiffDialog.SaveSettings;
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

procedure TDiffDialog.SetDiffOptions(NewOptions: TTextDiffFlags);
begin
  IgnoreCaseCheckBox.Checked:=tdfIgnoreCase in NewOptions;
  IgnoreEmptyLineChangesCheckBox.Checked:=tdfIgnoreEmptyLineChanges in NewOptions;
  IgnoreHeadingSpacesCheckBox.Checked:=tdfIgnoreHeadingSpaces in NewOptions;
  IgnoreLineEndsCheckBox.Checked:=tdfIgnoreLineEnds in NewOptions;
  IgnoreSpaceCharAmountCheckBox.Checked:=tdfIgnoreSpaceCharAmount in NewOptions;
  IgnoreSpaceCharsCheckBox.Checked:=tdfIgnoreSpaceChars in NewOptions;
  IgnoreTrailingSpacesCheckBox.Checked:=tdfIgnoreTrailingSpaces in NewOptions;
end;

function TDiffDialog.GetDiffOptions: TTextDiffFlags;
begin
  Result:=[];
  if IgnoreCaseCheckBox.Checked then
    Include(Result,tdfIgnoreCase);
  if IgnoreEmptyLineChangesCheckBox.Checked then
    Include(Result,tdfIgnoreEmptyLineChanges);
  if IgnoreHeadingSpacesCheckBox.Checked then
    Include(Result,tdfIgnoreHeadingSpaces);
  if IgnoreLineEndsCheckBox.Checked then
    Include(Result,tdfIgnoreLineEnds);
  if IgnoreSpaceCharAmountCheckBox.Checked then
    Include(Result,tdfIgnoreSpaceCharAmount);
  if IgnoreSpaceCharsCheckBox.Checked then
    Include(Result,tdfIgnoreSpaceChars);
  if IgnoreTrailingSpacesCheckBox.Checked then
    Include(Result,tdfIgnoreTrailingSpaces);
end;

procedure TDiffDialog.BeginUpdate;
begin
  inc(FLockCount);
end;

procedure TDiffDialog.EndUpdate;
begin
  dec(FLockCount);
  if (FLockCount=0) and fDiffNeedsUpdate then UpdateDiff;
end;

{ TDiffFile }

constructor TDiffFile.Create(const NewName: string; NewID: integer;
  NewSelectionAvailable: boolean);
begin
  Name:=NewName;
  ID:=NewID;
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

