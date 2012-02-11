{
 /***************************************************************************
                          patheditordlg.pp
                          ----------------

 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

 Abstract:
   Defines the TPathEditorDialog, which is a form to edit search paths
 
}
unit PathEditorDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, SynEdit, Buttons, StdCtrls, Dialogs,
  FileUtil, ButtonPanel, ExtCtrls, EditBtn, MacroIntf, LCLType,
  LazarusIDEStrConsts, EditorOptions;

type

  { TPathEditorDialog }

  TPathEditorDialog = class(TForm)
    AddTemplateButton: TBitBtn;
    ButtonPanel1: TButtonPanel;
    ReplaceButton: TBitBtn;
    AddButton: TBitBtn;
    DeleteInvalidPathsButton: TBitBtn;
    DirectoryEdit: TDirectoryEdit;
    Splitter1: TSplitter;
    DeleteButton: TBitBtn;
    PathListBox: TListBox;
    MoveDownButton: TBitBtn;
    MoveUpButton: TBitBtn;
    TemplatesListBox: TListBox;
    TemplateGroupBox: TGroupBox;
    PathGroupBox: TGroupBox;
    BrowseDialog: TSelectDirectoryDialog;
    procedure AddButtonClick(Sender: TObject);
    procedure AddTemplateButtonClick(Sender: TObject);
    procedure DeleteInvalidPathsButtonClick(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure DirectoryEditChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure MoveDownButtonClick(Sender: TObject);
    procedure MoveUpButtonClick(Sender: TObject);
    procedure PathListBoxKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure PathListBoxSelectionChange(Sender: TObject; User: boolean);
    procedure ReplaceButtonClick(Sender: TObject);
    procedure TemplatesListBoxDblClick(Sender: TObject);
    procedure TemplatesListBoxSelectionChange(Sender: TObject; User: boolean);
  private
    FBaseDirectory: string;
    FEffectiveBaseDirectory: string;
    function GetPath: string;
    function GetTemplates: string;
    function PathToText(const APath: string): string;
    function RelativePathHelper: String;
    function AbsolutePathHelper: String;
    procedure SetBaseDirectory(const AValue: string);
    procedure SetPath(const AValue: string);
    procedure SetTemplates(const AValue: string);
    function TextToPath(const AText: string): string;
    procedure UpdateButtons;
  public
    property BaseDirectory: string read FBaseDirectory write SetBaseDirectory;
    property EffectiveBaseDirectory: string read FEffectiveBaseDirectory;
    property Path: string read GetPath write SetPath;
    property Templates: string read GetTemplates write SetTemplates;
  end;

  TOnPathEditorExecuted = TNotifyEvent;

  TPathEditorButton = class(TButton)
  private
    FCurrentPathEditor: TPathEditorDialog;
    FOnExecuted: TOnPathEditorExecuted;
  protected
    procedure DoOnPathEditorExecuted;
  public
    procedure Click; override;
    property CurrentPathEditor: TPathEditorDialog read FCurrentPathEditor;
    property OnExecuted: TOnPathEditorExecuted
      read FOnExecuted write FOnExecuted;
  end;

function PathEditorDialog: TPathEditorDialog;


implementation

{$R *.lfm}

uses 
  Math;

var PathEditor: TPathEditorDialog;

function PathEditorDialog: TPathEditorDialog;
begin
  if PathEditor=nil then
    PathEditor:=TPathEditorDialog.Create(Application);
  Result:=PathEditor;
end;

{ TPathEditorDialog }

function TPathEditorDialog.RelativePathHelper: String;
begin
  Result:=DirectoryEdit.Text;
  if (FEffectiveBaseDirectory<>'') and FilenameIsAbsolute(FEffectiveBaseDirectory) then
    Result:=CreateRelativePath(Result, FEffectiveBaseDirectory);
end;

function TPathEditorDialog.AbsolutePathHelper: String;
begin
  Result:=PathListBox.Items[PathListBox.ItemIndex];
  if (FEffectiveBaseDirectory<>'') and FilenameIsAbsolute(FEffectiveBaseDirectory) then
    Result:=CreateAbsolutePath(Result, FEffectiveBaseDirectory);
end;

procedure TPathEditorDialog.AddButtonClick(Sender: TObject);
var
  y: integer;
begin
  y:=PathListBox.ItemIndex+1;
  if y=0 then
    y:=PathListBox.Count;
  PathListBox.Items.Insert(y,Trim(RelativePathHelper));
  PathListBox.ItemIndex:=y;
  UpdateButtons;
end;

procedure TPathEditorDialog.ReplaceButtonClick(Sender: TObject);
begin
  if PathListBox.ItemIndex>-1 then begin
    PathListBox.Items[PathListBox.ItemIndex]:=Trim(RelativePathHelper);
    UpdateButtons;
  end;
end;

procedure TPathEditorDialog.DeleteButtonClick(Sender: TObject);
var
  y: integer;
begin
  y:=PathListBox.ItemIndex;
  if (y>=0) and (y<PathListBox.Count) then begin
    PathListBox.Items.Delete(y);
    UpdateButtons;
  end;
end;

procedure TPathEditorDialog.DeleteInvalidPathsButtonClick(Sender: TObject);
begin
  ;
end;

procedure TPathEditorDialog.AddTemplateButtonClick(Sender: TObject);
var
  i, y: integer;
begin
  y:=-1;
  for i:=0 to TemplatesListBox.Items.Count-1 do begin
    if TemplatesListBox.Selected[i]
    and (PathListBox.Items.IndexOf(TemplatesListBox.Items[i])=-1) then begin
      PathListBox.Items.Add(TemplatesListBox.Items[i]);
      y:=PathListBox.Count-1;
    end;
  end;
  if y>=1 then
    PathListBox.ItemIndex:=y;
  UpdateButtons;
end;

procedure TPathEditorDialog.DirectoryEditChange(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TPathEditorDialog.FormCreate(Sender: TObject);
begin
  Caption:=dlgDebugOptionsPathEditorDlgCaption;

  PathGroupBox.Caption:=lisPathEditSearchPaths;
  MoveUpButton.Hint:=lisPathEditMovePathUp;
  MoveDownButton.Hint:=lisPathEditMovePathDown;

  ReplaceButton.Caption:=lisMenuReplace;
  AddButton.Caption:=lisAdd;
  DeleteButton.Caption:=dlgEdDelete;
  DeleteInvalidPathsButton.Caption:=lisPathEditDeleteInvalidPaths;

  TemplateGroupBox.Caption:=lisPathEditPathTemplates;
  AddTemplateButton.Caption:=lisCodeTemplAdd;

  MoveUpButton.LoadGlyphFromLazarusResource('arrow_up');
  MoveDownButton.LoadGlyphFromLazarusResource('arrow_down');
  ReplaceButton.LoadGlyphFromLazarusResource('menu_reportingbug');
  AddButton.LoadGlyphFromLazarusResource('laz_add');
  DeleteButton.LoadGlyphFromLazarusResource('laz_delete');
  DeleteInvalidPathsButton.LoadGlyphFromLazarusResource('menu_clean');
  AddTemplateButton.LoadGlyphFromLazarusResource('laz_add');

  PathListBox.ItemIndex:=-1;
  TemplatesListBox.ItemIndex:=-1;
end;

procedure TPathEditorDialog.FormResize(Sender: TObject);
var
  PathGroupBoxHeight: integer;
begin
  PathGroupBoxHeight:=((ClientHeight-70)*2) div 3;
  if PathGroupBoxHeight<10 then
    PathGroupBoxHeight:=10;
  PathGroupBox.Height:=PathGroupBoxHeight;
  UpdateButtons;
end;

procedure TPathEditorDialog.MoveDownButtonClick(Sender: TObject);
var
  y: integer;
begin
  y:=PathListBox.ItemIndex;
  if (y>-1) and (y<PathListBox.Count-1) then begin
    PathListBox.Items.Move(y,y+1);
    PathListBox.ItemIndex:=y+1;
    UpdateButtons;
  end;
end;

procedure TPathEditorDialog.MoveUpButtonClick(Sender: TObject);
var
  y: integer;
begin
  y:=PathListBox.ItemIndex;
  if (y>0) and (y<PathListBox.Count) then begin
    PathListBox.Items.Move(y,y-1);
    PathListBox.ItemIndex:=y-1;
    UpdateButtons;
  end;
end;

procedure TPathEditorDialog.PathListBoxKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (ssCtrl in shift ) and ((Key = VK_UP) or (Key = VK_DOWN)) then begin
    if Key = VK_UP then
      MoveUpButtonClick(Nil)
    else
      MoveDownButtonClick(Nil);
    Key:=VK_UNKNOWN;
  end;
end;

procedure TPathEditorDialog.PathListBoxSelectionChange(Sender: TObject; User: boolean);
begin
  if PathListBox.ItemIndex>-1 then begin
    DirectoryEdit.Text:=AbsolutePathHelper;
    UpdateButtons;
  end;
end;

procedure TPathEditorDialog.TemplatesListBoxSelectionChange(Sender: TObject; User: boolean);
begin
  UpdateButtons;
end;

procedure TPathEditorDialog.TemplatesListBoxDblClick(Sender: TObject);
var
  i: integer;
begin
  i := TemplatesListBox.ItemIndex;
  if i>=0 then begin
    PathListBox.Items.Add(TemplatesListBox.Items[i]);
    PathListBox.ItemIndex:=PathListBox.Count-1;
    UpdateButtons;
  end;
end;

function TPathEditorDialog.GetPath: string;
begin
  Result:=TextToPath(PathListBox.Items.Text);
end;

function TPathEditorDialog.GetTemplates: string;
begin
  Result:=TextToPath(TemplatesListBox.Items.Text);
end;

procedure TPathEditorDialog.SetPath(const AValue: string);
begin
  PathListBox.Items.Text:=PathToText(AValue);
end;

procedure TPathEditorDialog.SetTemplates(const AValue: string);
begin
  TemplatesListBox.Items.Text:=PathToText(AValue);
  TemplateGroupBox.Visible:=TemplatesListBox.Count>0;
end;

function TPathEditorDialog.TextToPath(const AText: string): string;
var
  i, j: integer;
  PathAsText: string;
begin
  PathAsText:=AText;
  Result:=PathAsText;
  // convert all line ends to semicolons, remove empty paths and trailing spaces
  i:=1;
  j:=1;
  while i<=length(PathAsText) do begin
    if PathAsText[i] in [#10,#13] then begin
      // new line -> new path
      inc(i);
      if (i<=length(PathAsText)) and (PathAsText[i] in [#10,#13])
      and (PathAsText[i]<>PathAsText[i-1]) then
        inc(i);
      // skip spaces at end of path
      while (j>1) and (Result[j-1]=' ') do
        dec(j);
      // skip empty paths
      if (j=1) or (Result[j-1]<>';') then begin
        Result[j]:=';';
        inc(j);
      end;
    end else if ord(PathAsText[i])<32 then begin
      // skip trailing spaces
      inc(i)
    end else if PathAsText[i]=' ' then begin
      // space -> skip spaces at beginning of path
      if (j>1) and (Result[j-1]<>';') then begin
        Result[j]:=PathAsText[i];
        inc(j);
      end;
      inc(i);
    end else begin
      // path char -> just copy
      Result[j]:=PathAsText[i];
      inc(j);
      inc(i);
    end;
  end;
  if (j>1) and (Result[j-1]=';') then dec(j);
  SetLength(Result,j-1);
end;

function TPathEditorDialog.PathToText(const APath: string): string;
var
  i: integer;
  NewPath: string;
begin
  NewPath:=APath;
  for i:=1 to length(NewPath) do
    if NewPath[i]=';' then
      NewPath[i]:=#13;
  Result:=NewPath;
end;

procedure TPathEditorDialog.UpdateButtons;
var
  i: integer;
begin
  // Replace / add / delete / Delete Invalid Paths
  ReplaceButton.Enabled:=(DirectoryEdit.Text<>'') and (DirectoryEdit.Text<>FEffectiveBaseDirectory)
      and (PathListBox.Items.IndexOf(RelativePathHelper)=-1);
  AddButton.Enabled:=ReplaceButton.Enabled;
  DeleteButton.Enabled:=PathListBox.ItemIndex>-1;
  DeleteInvalidPathsButton.Enabled:=False;
  AddTemplateButton.Enabled:=(TemplatesListBox.SelCount>1) or ((TemplatesListBox.ItemIndex>-1)
      and (PathListBox.Items.IndexOf(TemplatesListBox.Items[TemplatesListBox.ItemIndex])=-1));
  // Move up / down buttons
  i := PathListBox.ItemIndex;
  MoveUpButton.Enabled := i > 0;
  MoveDownButton.Enabled := (i > -1) and (i < PathListBox.Count-1);
end;

procedure TPathEditorDialog.SetBaseDirectory(const AValue: string);
begin
  if FBaseDirectory=AValue then exit;
  FBaseDirectory:=AValue;
  FEffectiveBaseDirectory:=FBaseDirectory;
  IDEMacros.SubstituteMacros(FEffectiveBaseDirectory);
  DirectoryEdit.Directory:=FEffectiveBaseDirectory;
end;

{ TPathEditorButton }

procedure TPathEditorButton.Click;
begin
  FCurrentPathEditor:=PathEditorDialog;
  try
    inherited Click;
    FCurrentPathEditor.ShowModal;
    DoOnPathEditorExecuted;
  finally
    FCurrentPathEditor:=nil;
  end;
end;

procedure TPathEditorButton.DoOnPathEditorExecuted;
begin
  if Assigned(OnExecuted) then OnExecuted(Self);
end;

end.

