{
 /***************************************************************************
                          packageslisteditordlg.pas
                          -------------------------

 ***************************************************************************/

 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 Abstract:
   Defines the TPckListEditorDialog, which is a form to edit search lists of packages
 
}
unit PackageListEditorDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  LCLType, LCLProc, Forms, Controls, Buttons, StdCtrls, Dialogs, Menus, Graphics,
  ButtonPanel, Clipbrd, Grids,
  // LazUtils
  FileUtil, LazFileUtils, LazStringUtils, LazFileCache, LazUTF8,
  // IdeIntf
  IDEImagesIntf,
  // IDE
  TransferMacros, GenericListSelect, LazarusIDEStrConsts;

type
  TPkgPatternPrefix = (
    pppInclude,
    pppIncludeRecursive,
    pppExclude
    );
const
  PkgPatternPrefixes: array[TPkgPatternPrefix] of string = (
    '',
    '>',
    '-'
    );

type

  { TPkgListEditorDialog }

  TPkgListEditorDialog = class(TForm)
    AddTemplateButton: TBitBtn;
    AvailableGroupBox: TGroupBox;
    AvailableListBox: TListBox;
    ButtonPanel1: TButtonPanel;
    CopyMenuItem: TMenuItem;
    MoveDownButton: TSpeedButton;
    MoveUpButton: TSpeedButton;
    OpenDialog1: TOpenDialog;
    PkgsStringGrid: TStringGrid;
    SaveDialog1: TSaveDialog;
    ExportMenuItem: TMenuItem;
    ImportMenuItem: TMenuItem;
    SeparMenuItem: TMenuItem;
    PasteMenuItem: TMenuItem;
    PopupMenu1: TPopupMenu;
    AddButton: TBitBtn;
    DeleteButton: TBitBtn;
    PkgsGroupBox: TGroupBox;
    procedure AddButtonClick(Sender: TObject);
    procedure AddTemplateButtonClick(Sender: TObject);
    procedure CopyMenuItemClick(Sender: TObject);
    procedure ExportMenuItemClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PasteMenuItemClick(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MoveDownButtonClick(Sender: TObject);
    procedure MoveUpButtonClick(Sender: TObject);
    procedure ImportMenuItemClick(Sender: TObject);
  private
    FTemplateList: TStringListUTF8Fast;
    procedure AddPkg(PkgName: string; PkgAction: TPkgPatternPrefix);
    function GetPath: string;
    procedure SetPath(const AValue: string);
    procedure SetTemplates(const AValue: string);
    procedure UpdateButtons;
    function SaveToList: TStringListUTF8Fast;
    procedure LoadFromList(sl: TStrings);
    function PatternActionToCaption(PkgAction: TPkgPatternPrefix): string;
    function CaptionToPatternAction(const PkgAction: string): TPkgPatternPrefix;
    function RowOfPkgName(const PkgName: string): integer;
  public
    property Path: string read GetPath write SetPath;
    property Templates: string {read GetTemplates} write SetTemplates;
  end;

  TOnPkgListEditorExecuted = function (Context: String; var NewPath: String): Boolean of object;

  { TPkgListEditorButton }

  TPkgListEditorButton = class(TButton)
  private
    FAssociatedComboBox: TCustomComboBox;
    FCurrentPkgListEditor: TPkgListEditorDialog;
    FAssociatedEdit: TCustomEdit;
    FContextCaption: String;
    FTemplates: String;
    FOnExecuted: TOnPkgListEditorExecuted;
    function GetAssociatedText: string;
  protected
    procedure DoOnPathEditorExecuted;
  public
    procedure Click; override;
    property CurrentPathEditor: TPkgListEditorDialog read FCurrentPkgListEditor;
    property AssociatedComboBox: TCustomComboBox read FAssociatedComboBox write FAssociatedComboBox;
    property AssociatedEdit: TCustomEdit read FAssociatedEdit write FAssociatedEdit;
    property AssociatedText: string read GetAssociatedText;
    property ContextCaption: String read FContextCaption write FContextCaption;
    property Templates: String read FTemplates write FTemplates;
    property OnExecuted: TOnPkgListEditorExecuted read FOnExecuted write FOnExecuted;
  end;

function PkgListEditorDialog: TPkgListEditorDialog;
procedure SetPkgListTextAndHint(aPath: String; aEdit: TWinControl);
function ParsePkgPattern(const Pattern: string; out Action: TPkgPatternPrefix): string; // returns packagename
function MakeParsePkgPattern(const PkgName: string; Action: TPkgPatternPrefix): string;

implementation

{$R *.lfm}

var PkgListEditor: TPkgListEditorDialog;

function PkgListEditorDialog: TPkgListEditorDialog;
begin
  if PkgListEditor=nil then
    PkgListEditor:=TPkgListEditorDialog.Create(Application);
  Result:=PkgListEditor;
end;

procedure SetPkgListTextAndHint(aPath: String; aEdit: TWinControl);
var
  sl: TStrings;
begin
  if aEdit is TCustomEdit then
    TCustomEdit(aEdit).Text := aPath
  else if aEdit is TCustomComboBox then
    TCustomComboBox(aEdit).Text := aPath;
  if Pos(';', aPath) > 0 then
  begin
    sl := SplitString(aPath, ';');
    aEdit.Hint := sl.Text;
    sl.Free;
  end
  else
    aEdit.Hint := lisDelimiterIsSemicolon;
end;

function ParsePkgPattern(const Pattern: string; out Action: TPkgPatternPrefix
  ): string;
var
  ap: String;
begin
  for Action in TPkgPatternPrefix do
  begin
    ap:=PkgPatternPrefixes[Action];
    if ap='' then continue;
    if LeftStr(Pattern,length(ap))=ap then
    begin
      Result:=copy(Pattern,length(ap)+1,length(Pattern));
      exit;
    end;
  end;
  Result:=Pattern;
  Action:=pppInclude;
end;

function MakeParsePkgPattern(const PkgName: string; Action: TPkgPatternPrefix
  ): string;
begin
  if PkgName='' then
    exit('');
  Result:=PkgPatternPrefixes[Action]+PkgName;
end;

{ TPkgListEditorDialog }

procedure TPkgListEditorDialog.AddButtonClick(Sender: TObject);
begin
  // ToDo
end;

procedure TPkgListEditorDialog.DeleteButtonClick(Sender: TObject);
begin
  PkgsStringGrid.DeleteRow(PkgsStringGrid.Row);
  UpdateButtons;
end;

procedure TPkgListEditorDialog.AddTemplateButtonClick(Sender: TObject);
var
  TemplateForm: TGenericListSelectForm;
  i, j: Integer;
begin
  TemplateForm := TGenericListSelectForm.Create(Nil);
  try
    TemplateForm.Caption := lisPathEditPathTemplates;
    // Let a user select only templates which are not in the list already.
    for i := 0 to FTemplateList.Count-1 do
    begin
      j:=PkgsStringGrid.RowCount-1;
      while (j>0) and not SameText(PkgsStringGrid.Cells[0,j],FTemplateList[i]) do dec(j);
      if j<1 then
        TemplateForm.ListBox.Items.Add(FTemplateList[i]);
    end;
    if TemplateForm.ShowModal = mrOK then
      with TemplateForm.ListBox do
        AddPkg(Items[ItemIndex], pppInclude);
  finally
    TemplateForm.Free;
  end;
end;

procedure TPkgListEditorDialog.CopyMenuItemClick(Sender: TObject);
var
  Paths: TStringList;
begin
  Paths := SaveToList;
  try
    Clipboard.AsText := Paths.Text;
  finally
    Paths.Free;
  end;
end;

procedure TPkgListEditorDialog.ExportMenuItemClick(Sender: TObject);
var
  Paths: TStringList;
begin
  if not SaveDialog1.Execute then Exit;
  Paths := SaveToList;
  try
    Paths.SaveToFile(SaveDialog1.FileName);
  finally
    Paths.Free;
  end;
end;

procedure TPkgListEditorDialog.LoadFromList(sl: TStrings);
var
  PkgName: string;
  i: integer;
  PkgAction: TPkgPatternPrefix;
begin
  for i:=sl.Count-1 downto 0 do
  begin
    PkgName:=ParsePkgPattern(Trim(sl[i]),PkgAction);
    if PkgName='' then sl.Delete(i);
  end;
  PkgsStringGrid.RowCount:=sl.Count+1;
  for i:=0 to sl.Count-1 do
  begin
    PkgName:=ParsePkgPattern(Trim(sl[i]),PkgAction);
    PkgsStringGrid.Cells[0,i+1]:=PkgName;
    PkgsStringGrid.Cells[1,i+1]:=PatternActionToCaption(PkgAction);
  end;
  UpdateButtons;
end;

procedure TPkgListEditorDialog.PasteMenuItemClick(Sender: TObject);
var
  Paths: TStringList;
begin
  Paths := TStringList.Create;
  try
    Paths.Text := Clipboard.AsText;
    LoadFromList(Paths);
  finally
    Paths.Free;
  end;
end;

procedure TPkgListEditorDialog.ImportMenuItemClick(Sender: TObject);
var
  Paths: TStringList;
begin
  if not OpenDialog1.Execute then Exit;
  Paths := TStringList.Create;
  try
    Paths.LoadFromFile(OpenDialog1.FileName);
    LoadFromList(Paths);
  finally
    Paths.Free;
  end;
end;

procedure TPkgListEditorDialog.AddPkg(PkgName: string;
  PkgAction: TPkgPatternPrefix);
var
  i: Integer;
begin
  for i:=1 to PkgsStringGrid.RowCount-1 do
  begin
    if not SameText(PkgsStringGrid.Cells[0,i],PkgName) then continue;
    PkgsStringGrid.Cells[1,i]:=PatternActionToCaption(PkgAction);
    exit;
  end;
  i:=PkgsStringGrid.RowCount;
  PkgsStringGrid.RowCount:=i+1;
  PkgsStringGrid.Cells[0,i]:=PkgName;
  PkgsStringGrid.Cells[1,i]:=PatternActionToCaption(PkgAction);
end;

procedure TPkgListEditorDialog.FormCreate(Sender: TObject);
const
  Filt = 'Text file (*.txt)|*.txt|All files (*)|*';
var
  PkgAction: TPkgPatternPrefix;
  s: String;
begin
  FTemplateList := TStringListUTF8Fast.Create;
  Caption:='Search in Packages Editor';
  PkgsGroupBox.Caption:=lisPathEditSearchPaths;
  MoveUpButton.Hint:=lisPathEditMovePathUp;
  MoveDownButton.Hint:=lisPathEditMovePathDown;
  AddButton.Caption:=lisAdd;
  AddButton.Hint:=lisPathEditorAddHint;
  DeleteButton.Caption:=lisDelete;
  DeleteButton.Hint:=lisPathEditorDeleteHint;
  AddTemplateButton.Caption:=lisCodeTemplAdd;
  AddTemplateButton.Hint:=lisPathEditorTemplAddHint;

  s:='';
  for PkgAction in TPkgPatternPrefix do
    s:=s+PatternActionToCaption(PkgAction)+sLineBreak;
  PkgsStringGrid.Columns[1].PickList.Text:=s;

  PopupMenu1.Images:=IDEImages.Images_16;
  CopyMenuItem.Caption:=lisCopyAllItemsToClipboard;
  CopyMenuItem.ImageIndex:=IDEImages.LoadImage('laz_copy');
  PasteMenuItem.Caption:=lisMenuPasteFromClipboard;
  PasteMenuItem.ImageIndex:=IDEImages.LoadImage('laz_paste');
  ExportMenuItem.Caption:=lisExportAllItemsToFile;
  ExportMenuItem.ImageIndex:=IDEImages.LoadImage('laz_save');
  ImportMenuItem.Caption:=lisImportFromFile;
  ImportMenuItem.ImageIndex:=IDEImages.LoadImage('laz_open');

  OpenDialog1.Filter:=Filt;
  SaveDialog1.Filter:=Filt;

  IDEImages.AssignImage(MoveUpButton, 'arrow_up');
  IDEImages.AssignImage(MoveDownButton, 'arrow_down');
  //IDEImages.AssignImage(ReplaceButton, 'menu_reportingbug');
  IDEImages.AssignImage(AddButton, 'laz_add');
  IDEImages.AssignImage(DeleteButton, 'laz_delete');
  //IDEImages.AssignImage(DeleteInvalidPkgsButton, 'menu_clean');
  IDEImages.AssignImage(AddTemplateButton, 'laz_add');
end;

procedure TPkgListEditorDialog.FormDestroy(Sender: TObject);
begin
  FTemplateList.Free;
end;

procedure TPkgListEditorDialog.FormShow(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TPkgListEditorDialog.MoveDownButtonClick(Sender: TObject);
var
  y: integer;
begin
  y:=PkgsStringGrid.Row;
  if (y>0) and (y<PkgsStringGrid.RowCount-1) then begin
    PkgsStringGrid.MoveColRow(false,y,y+1);
    UpdateButtons;
  end;
end;

procedure TPkgListEditorDialog.MoveUpButtonClick(Sender: TObject);
var
  y: integer;
begin
  y:=PkgsStringGrid.Row;
  if (y>2) and (y<PkgsStringGrid.RowCount) then begin
    PkgsStringGrid.MoveColRow(false,y+1,y);
    UpdateButtons;
  end;
end;

function TPkgListEditorDialog.GetPath: string;
var
  i: Integer;
  PkgAction: TPkgPatternPrefix;
begin
  Result:='';
  for i:=1 to PkgsStringGrid.RowCount-1 do
  begin
    PkgAction:=CaptionToPatternAction(PkgsStringGrid.Cells[1,i]);
    if Result<>'' then
      Result:=Result+';';
    Result:=PkgPatternPrefixes[PkgAction]+Trim(PkgsStringGrid.Cells[0,i]);
  end;
end;

procedure TPkgListEditorDialog.SetPath(const AValue: string);
var
  sl: TStrings;
begin
  sl := SplitString(AValue, ';');
  try
    LoadFromList(sl);
  finally
    sl.Free;
  end;
end;

procedure TPkgListEditorDialog.SetTemplates(const AValue: string);
begin
  SplitString(GetForcedPathDelims(AValue), ';', FTemplateList, True);
  AddTemplateButton.Enabled := FTemplateList.Count > 0;
end;

procedure TPkgListEditorDialog.UpdateButtons;
var
  i: integer;
begin
  AddButton.Enabled:=(AvailableListBox.ItemIndex>=0)
       and (RowOfPkgName(AvailableListBox.Items[AvailableListBox.ItemIndex])<1);
  DeleteButton.Enabled:=PkgsStringGrid.Row>0;

  // Move up / down buttons
  i := PkgsStringGrid.Row;
  MoveUpButton.Enabled := i > 1;
  MoveDownButton.Enabled := (i > -1) and (i < PkgsStringGrid.RowCount-1);
end;

function TPkgListEditorDialog.SaveToList: TStringListUTF8Fast;
var
  i: Integer;
  PkgAction: TPkgPatternPrefix;
begin
  Result:=TStringListUTF8Fast.Create;
  for i:=1 to PkgsStringGrid.RowCount-1 do
  begin
    PkgAction:=CaptionToPatternAction(PkgsStringGrid.Cells[1,i]);
    Result.Add(PkgPatternPrefixes[PkgAction]+PkgsStringGrid.Cells[0,i]);
  end;
end;

function TPkgListEditorDialog.PatternActionToCaption(
  PkgAction: TPkgPatternPrefix): string;
begin
  case PkgAction of
    pppIncludeRecursive: Result:=lisIncludeRecursive;
    pppExclude: Result:=lisExclude;
  else
    Result:=lisPckOptsInclude;
  end;
end;

function TPkgListEditorDialog.CaptionToPatternAction(const PkgAction: string
  ): TPkgPatternPrefix;
begin
  if PkgAction=lisIncludeRecursive then
    Result:=pppIncludeRecursive
  else if PkgAction=lisExclude then
    Result:=pppExclude
  else
    Result:=pppInclude;
end;

function TPkgListEditorDialog.RowOfPkgName(const PkgName: string): integer;
begin
  for Result:=1 to PkgsStringGrid.RowCount-1 do
    if SameText(PkgsStringGrid.Cells[0,Result],PkgName) then
      exit;
  Result:=-1;
end;

{ TPkgListEditorButton }

procedure TPkgListEditorButton.Click;
begin
  FCurrentPkgListEditor:=PkgListEditorDialog;
  try
    inherited Click;
    FCurrentPkgListEditor.Templates := FTemplates;
    FCurrentPkgListEditor.Path := AssociatedText;
    FCurrentPkgListEditor.ShowModal;
    DoOnPathEditorExecuted;
  finally
    FCurrentPkgListEditor:=nil;
  end;
end;

function TPkgListEditorButton.GetAssociatedText: string;
begin
  if AssociatedEdit<>nil then
    Result:=AssociatedEdit.Text
  else if AssociatedComboBox<>nil then
    Result:=AssociatedComboBox.Text
  else
    Result:='';
end;

procedure TPkgListEditorButton.DoOnPathEditorExecuted;
var
  Ok: Boolean;
  NewPath: String;
begin
  NewPath := FCurrentPkgListEditor.Path;
  Ok := (FCurrentPkgListEditor.ModalResult = mrOk) and (AssociatedText <> NewPath);
  if Ok and Assigned(OnExecuted) then
    Ok := OnExecuted(ContextCaption, NewPath);
  // Assign value only if old <> new and OnExecuted allows it.
  if not Ok then exit;
  if AssociatedEdit<>nil then
    SetPkgListTextAndHint(NewPath, AssociatedEdit)
  else if AssociatedComboBox<>nil then
    SetPkgListTextAndHint(NewPath, AssociatedComboBox);
end;

end.

