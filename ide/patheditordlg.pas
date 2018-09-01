{
 /***************************************************************************
                          patheditordlg.pp
                          ----------------

 ***************************************************************************/

 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 Abstract:
   Defines the TPathEditorDialog, which is a form to edit search paths
 
}
unit PathEditorDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types,
  // LCL
  LCLType, LCLProc, Forms, Controls, Buttons, StdCtrls, Dialogs, Menus, Graphics,
  ButtonPanel, Clipbrd,
  // LazUtils
  FileUtil, LazFileUtils, LazStringUtils,
  // LazControls
  ShortPathEdit,
  // IdeIntf
  MacroIntf, IDEImagesIntf, IDEUtils,
  // IDE
  TransferMacros, GenericListSelect, LazarusIDEStrConsts;

type

  { TPathEditorDialog }

  TPathEditorDialog = class(TForm)
    AddTemplateButton: TBitBtn;
    ButtonPanel1: TButtonPanel;
    CopyMenuItem: TMenuItem;
    MoveDownButton: TSpeedButton;
    MoveUpButton: TSpeedButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    ExportMenuItem: TMenuItem;
    ImportMenuItem: TMenuItem;
    SeparMenuItem: TMenuItem;
    PasteMenuItem: TMenuItem;
    PopupMenu1: TPopupMenu;
    ReplaceButton: TBitBtn;
    AddButton: TBitBtn;
    DeleteInvalidPathsButton: TBitBtn;
    DirectoryEdit: TShortPathEdit;
    DeleteButton: TBitBtn;
    PathListBox: TListBox;
    PathGroupBox: TGroupBox;
    BrowseDialog: TSelectDirectoryDialog;
    procedure AddButtonClick(Sender: TObject);
    procedure AddTemplateButtonClick(Sender: TObject);
    procedure CopyMenuItemClick(Sender: TObject);
    procedure ExportMenuItemClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PasteMenuItemClick(Sender: TObject);
    procedure DeleteInvalidPathsButtonClick(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure DirectoryEditAcceptDirectory(Sender: TObject; var Value: String);
    procedure DirectoryEditChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MoveDownButtonClick(Sender: TObject);
    procedure MoveUpButtonClick(Sender: TObject);
    procedure PathListBoxDrawItem({%H-}Control: TWinControl; Index: Integer;
      ARect: TRect; {%H-}State: TOwnerDrawState);
    procedure PathListBoxKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure PathListBoxSelectionChange(Sender: TObject; {%H-}User: boolean);
    procedure ReplaceButtonClick(Sender: TObject);
    procedure ImportMenuItemClick(Sender: TObject);
    procedure TemplatesListBoxDblClick(Sender: TObject);
    procedure TemplatesListBoxSelectionChange(Sender: TObject; {%H-}User: boolean);
  private
    FBaseDirectory: string;
    FEffectiveBaseDirectory: string;
    FTemplateList: TStringList;
    procedure AddPath(aPath: String; aObject: TObject);
    function GetPath: string;
    //function GetTemplates: string;
    function BaseRelative(const APath: string): String;
    function PathAsAbsolute(const APath: string): String;
    function PathMayExist(APath: string): TObject;
    procedure ReadHelper(Paths: TStringList);
    procedure SetBaseDirectory(const AValue: string);
    procedure SetPath(const AValue: string);
    procedure SetTemplates(const AValue: string);
    procedure UpdateButtons;
    procedure WriteHelper(Paths: TStringList);
  public
    property BaseDirectory: string read FBaseDirectory write SetBaseDirectory;
    property EffectiveBaseDirectory: string read FEffectiveBaseDirectory;
    property Path: string read GetPath write SetPath;
    property Templates: string {read GetTemplates} write SetTemplates;
  end;

  TOnPathEditorExecuted = function (Context: String; var NewPath: String): Boolean of object;

  { TPathEditorButton }

  TPathEditorButton = class(TButton)
  private
    FCurrentPathEditor: TPathEditorDialog;
    FAssociatedEdit: TCustomEdit;
    FContextCaption: String;
    FTemplates: String;
    FOnExecuted: TOnPathEditorExecuted;
  protected
    procedure DoOnPathEditorExecuted;
  public
    procedure Click; override;
    property CurrentPathEditor: TPathEditorDialog read FCurrentPathEditor;
    property AssociatedEdit: TCustomEdit read FAssociatedEdit write FAssociatedEdit;
    property ContextCaption: String read FContextCaption write FContextCaption;
    property Templates: String read FTemplates write FTemplates;
    property OnExecuted: TOnPathEditorExecuted read FOnExecuted write FOnExecuted;
  end;

function PathEditorDialog: TPathEditorDialog;
procedure SetPathTextAndHint(aPath: String; aEdit: TCustomEdit);


implementation

{$R *.lfm}

var PathEditor: TPathEditorDialog;

function PathEditorDialog: TPathEditorDialog;
begin
  if PathEditor=nil then
    PathEditor:=TPathEditorDialog.Create(Application);
  Result:=PathEditor;
end;

function TextToPath(const AText: string): string;
var
  i, j: integer;
begin
  Result:=AText;
  // convert all line ends to semicolons, remove empty paths and trailing spaces
  i:=1;
  j:=1;
  while i<=length(AText) do begin
    if AText[i] in [#10,#13] then begin
      // new line -> new path
      inc(i);
      if (i<=length(AText)) and (AText[i] in [#10,#13])
      and (AText[i]<>AText[i-1]) then
        inc(i);
      // skip spaces at end of path
      while (j>1) and (Result[j-1]=' ') do
        dec(j);
      // skip empty paths
      if (j=1) or (Result[j-1]<>';') then begin
        Result[j]:=';';
        inc(j);
      end;
    end else if ord(AText[i])<32 then begin
      // skip trailing spaces
      inc(i)
    end else if AText[i]=' ' then begin
      // space -> skip spaces at beginning of path
      if (j>1) and (Result[j-1]<>';') then begin
        Result[j]:=AText[i];
        inc(j);
      end;
      inc(i);
    end else begin
      // path char -> just copy
      Result[j]:=AText[i];
      inc(j);
      inc(i);
    end;
  end;
  if (j>1) and (Result[j-1]=';') then dec(j);
  SetLength(Result,j-1);
end;

procedure SetPathTextAndHint(aPath: String; aEdit: TCustomEdit);
var
  sl: TStrings;
begin
  aEdit.Text := aPath;
  if Pos(';', aPath) > 0 then
  begin
    sl := SplitString(aPath, ';');
    aEdit.Hint := sl.Text;
    sl.Free;
  end
  else
    aEdit.Hint := lisDelimiterIsSemicolon;
end;

{ TPathEditorDialog }

function TPathEditorDialog.BaseRelative(const APath: string): String;
begin
  Result:=Trim(APath);
  if (FEffectiveBaseDirectory<>'') and FilenameIsAbsolute(FEffectiveBaseDirectory) then
    Result:=CreateRelativePath(Result, FEffectiveBaseDirectory);
end;

function TPathEditorDialog.PathAsAbsolute(const APath: string): String;
begin
  Result:=APath;
  if not TTransferMacroList.StrHasMacros(Result)  // not a template
  and (FEffectiveBaseDirectory<>'') and FilenameIsAbsolute(FEffectiveBaseDirectory) then
    Result:=CreateAbsolutePath(Result, FEffectiveBaseDirectory);
end;

function TPathEditorDialog.PathMayExist(APath: string): TObject;
// Returns 1 if path exists or contains a macro, 0 otherwise.
// Result is casted to TObject to be used for Strings.Objects.
begin
  if TTransferMacroList.StrHasMacros(APath) then
    Exit(TObject(1));
  Result:=TObject(0);
  if (FEffectiveBaseDirectory<>'') and FilenameIsAbsolute(FEffectiveBaseDirectory) then
    APath:=CreateAbsolutePath(APath, FEffectiveBaseDirectory);
  if DirectoryExists(APath) then
    Result:=TObject(1);
end;

procedure TPathEditorDialog.AddPath(aPath: String; aObject: TObject);
var
  y: integer;
begin
  y:=PathListBox.ItemIndex+1;
  if y=0 then
    y:=PathListBox.Count;
  PathListBox.Items.InsertObject(y, aPath, aObject);
  PathListBox.ItemIndex:=y;
  UpdateButtons;
end;

procedure TPathEditorDialog.AddButtonClick(Sender: TObject);
begin
  AddPath(BaseRelative(DirectoryEdit.Text), PathMayExist(DirectoryEdit.Text));
end;

procedure TPathEditorDialog.ReplaceButtonClick(Sender: TObject);
begin
  with PathListBox do begin
    Items[ItemIndex]:=BaseRelative(DirectoryEdit.Text);
    Items.Objects[ItemIndex]:=PathMayExist(DirectoryEdit.Text);
    UpdateButtons;
  end;
end;

procedure TPathEditorDialog.DeleteButtonClick(Sender: TObject);
begin
  PathListBox.Items.Delete(PathListBox.ItemIndex);
  UpdateButtons;
end;

procedure TPathEditorDialog.DirectoryEditAcceptDirectory(Sender: TObject; var Value: String);
begin
  DirectoryEdit.Text := BaseRelative(Value);
  {$IFDEF LCLCarbon}
  // Not auto-called on Mac. ToDo: fix it in the component instead of here.
  DirectoryEdit.OnChange(nil);
  {$ENDIF}
end;

procedure TPathEditorDialog.DeleteInvalidPathsButtonClick(Sender: TObject);
var
  i: Integer;
begin
  with PathListBox do
    for i:=Items.Count-1 downto 0 do
      if PtrInt(Items.Objects[i])=0 then
        Items.Delete(i);
end;

procedure TPathEditorDialog.AddTemplateButtonClick(Sender: TObject);
var
  TemplateForm: TGenericListSelectForm;
  i: Integer;
begin
  TemplateForm := TGenericListSelectForm.Create(Nil);
  try
    // Let a user select only templates which are not in the list already.
    for i := 0 to FTemplateList.Count-1 do
      if PathListBox.Items.IndexOf(FTemplateList[i]) = -1 then
        TemplateForm.ListBox.Items.Add(FTemplateList[i]);
    if TemplateForm.ShowModal = mrOK then
      with TemplateForm.ListBox do
        AddPath(Items[ItemIndex], TObject(1));
  finally
    TemplateForm.Free;
  end;
end;

procedure TPathEditorDialog.WriteHelper(Paths: TStringList);
// Helper method for writing paths. Collect paths to a StringList.
var
  i: integer;
begin
  for i := 0 to PathListBox.Count-1 do
    Paths.Add(PathAsAbsolute(PathListBox.Items[i]));
end;

procedure TPathEditorDialog.CopyMenuItemClick(Sender: TObject);
var
  Paths: TStringList;
begin
  Paths := TStringList.Create;
  try
    WriteHelper(Paths);
    Clipboard.AsText := Paths.Text;
  finally
    Paths.Free;
  end;
end;

procedure TPathEditorDialog.ExportMenuItemClick(Sender: TObject);
var
  Paths: TStringList;
begin
  if not SaveDialog1.Execute then Exit;
  Paths := TStringList.Create;
  try
    WriteHelper(Paths);
    Paths.SaveToFile(SaveDialog1.FileName);
  finally
    Paths.Free;
  end;
end;

procedure TPathEditorDialog.ReadHelper(Paths: TStringList);
// Helper method for reading paths. Insert paths from a StringList to the ListBox.
var
  s: string;
  y, i: integer;
begin
  y := PathListBox.ItemIndex;
  if y = -1 then
    y := PathListBox.Count-1;
  for i := 0 to Paths.Count-1 do
  begin
    s := Trim(Paths[i]);
    if s <> '' then
    begin
      Inc(y);
      PathListBox.Items.InsertObject(y, BaseRelative(s), PathMayExist(s));
    end;
  end;
  UpdateButtons;
end;

procedure TPathEditorDialog.PasteMenuItemClick(Sender: TObject);
var
  Paths: TStringList;
begin
  Paths := TStringList.Create;
  try
    Paths.Text := Clipboard.AsText;
    ReadHelper(Paths);
  finally
    Paths.Free;
  end;
end;

procedure TPathEditorDialog.ImportMenuItemClick(Sender: TObject);
var
  Paths: TStringList;
begin
  if not OpenDialog1.Execute then Exit;
  Paths := TStringList.Create;
  try
    Paths.LoadFromFile(OpenDialog1.FileName);
    ReadHelper(Paths);
  finally
    Paths.Free;
  end;
end;

procedure TPathEditorDialog.DirectoryEditChange(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TPathEditorDialog.PathListBoxSelectionChange(Sender: TObject; User: boolean);
Var
  FullPath : String;
begin
  with PathListBox do
    if ItemIndex>-1 then begin
      DirectoryEdit.Text:=BaseRelative(Items[ItemIndex]);
      FullPath := Items[ItemIndex];
      IDEMacros.SubstituteMacros(FullPath);
      DirectoryEdit.Directory:=PathAsAbsolute(FullPath);
    end;
  UpdateButtons;
end;

procedure TPathEditorDialog.TemplatesListBoxSelectionChange(Sender: TObject; User: boolean);
begin
  UpdateButtons;
end;

procedure TPathEditorDialog.TemplatesListBoxDblClick(Sender: TObject);
begin
  AddTemplateButtonClick(Nil);
end;

procedure TPathEditorDialog.FormCreate(Sender: TObject);
const
  Filt = 'Text file (*.txt)|*.txt|All files (*)|*';
begin
  FTemplateList := TStringList.Create;
  Caption:=dlgDebugOptionsPathEditorDlgCaption;
  PathGroupBox.Caption:=lisPathEditSearchPaths;
  MoveUpButton.Hint:=lisPathEditMovePathUp;
  MoveDownButton.Hint:=lisPathEditMovePathDown;
  ReplaceButton.Caption:=lisReplace;
  ReplaceButton.Hint:=lisPathEditorReplaceHint;
  AddButton.Caption:=lisAdd;
  AddButton.Hint:=lisPathEditorAddHint;
  DeleteButton.Caption:=lisDelete;
  DeleteButton.Hint:=lisPathEditorDeleteHint;
  DeleteInvalidPathsButton.Caption:=lisPathEditDeleteInvalidPaths;
  DeleteInvalidPathsButton.Hint:=lisPathEditorDeleteInvalidHint;
  AddTemplateButton.Caption:=lisCodeTemplAdd;
  AddTemplateButton.Hint:=lisPathEditorTemplAddHint;

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
  IDEImages.AssignImage(ReplaceButton, 'menu_reportingbug');
  IDEImages.AssignImage(AddButton, 'laz_add');
  IDEImages.AssignImage(DeleteButton, 'laz_delete');
  IDEImages.AssignImage(DeleteInvalidPathsButton, 'menu_clean');
  IDEImages.AssignImage(AddTemplateButton, 'laz_add');
end;

procedure TPathEditorDialog.FormDestroy(Sender: TObject);
begin
  FTemplateList.Free;
end;

procedure TPathEditorDialog.FormShow(Sender: TObject);
begin
  PathListBox.ItemIndex:=-1;
  UpdateButtons;
end;

procedure TPathEditorDialog.FormResize(Sender: TObject);
var
  PathGroupBoxHeight: integer;
begin
  PathGroupBoxHeight:=((ClientHeight-70)*2) div 3;
  if PathGroupBoxHeight<10 then
    PathGroupBoxHeight:=10;
  PathGroupBox.Height:=PathGroupBoxHeight;
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

procedure TPathEditorDialog.PathListBoxDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
begin
  if Index < 0 then Exit;
  with PathListBox do begin
    Canvas.FillRect(ARect);
    if PtrInt(Items.Objects[Index]) = 0 then
      Canvas.Font.Color := clGray;
    Canvas.TextRect(ARect, ARect.Left, ARect.Top, Items[Index]);
  end;
end;

procedure TPathEditorDialog.PathListBoxKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (ssCtrl in shift) and ((Key = VK_UP) or (Key = VK_DOWN)) then begin
    if Key = VK_UP then
      MoveUpButtonClick(Nil)
    else
      MoveDownButtonClick(Nil);
    Key:=VK_UNKNOWN;
  end;
end;

function TPathEditorDialog.GetPath: string;
begin
  // ToDo: Join PathListBox.Items directly without Text property.
  Result:=TextToPath(PathListBox.Items.Text);
end;

procedure TPathEditorDialog.SetPath(const AValue: string);
var
  sl: TStrings;
  i: Integer;
begin
  DirectoryEdit.Text:='';
  PathListBox.Items.Clear;
  sl := SplitString(AValue, ';');
  try
    for i:=0 to sl.Count-1 do
      PathListBox.Items.AddObject(sl[i], PathMayExist(sl[i]));
    PathListBox.ItemIndex:=-1;
  finally
    sl.Free;
  end;
end;
{
function TPathEditorDialog.GetTemplates: string;
begin
  raise Exception.Create('TPathEditorDialog.GetTemplates is called.');
  //Result := TextToPath(FTemplateList.Text);
end;
}
procedure TPathEditorDialog.SetTemplates(const AValue: string);
begin
  SplitString(GetForcedPathDelims(AValue), ';', FTemplateList, True);
  AddTemplateButton.Enabled := FTemplateList.Count > 0;
end;

procedure TPathEditorDialog.UpdateButtons;
var
  i: integer;
  InValidPathsExist: Boolean;
begin
  // Replace / add / delete / Delete Invalid Paths
  AddButton.Enabled:=(DirectoryEdit.Text<>'')
                 and (DirectoryEdit.Text<>FEffectiveBaseDirectory)
                 and (IndexInStringList(PathListBox.Items,cstCaseSensitive,
                                        BaseRelative(DirectoryEdit.Text)) = -1);
  ReplaceButton.Enabled:=AddButton.Enabled and (PathListBox.ItemIndex>-1) ;
  DeleteButton.Enabled:=PathListBox.SelCount=1; // or ItemIndex>-1; ?
  // Delete non-existent paths button. Check if there are any.
  InValidPathsExist:=False;
  for i:=0 to PathListBox.Items.Count-1 do
    if PtrInt(PathListBox.Items.Objects[i])=0 then begin
      InValidPathsExist:=True;
      Break;
    end;
  DeleteInvalidPathsButton.Enabled:=InValidPathsExist;
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
    FCurrentPathEditor.Templates := FTemplates;
    FCurrentPathEditor.Path := AssociatedEdit.Text;
    FCurrentPathEditor.ShowModal;
    DoOnPathEditorExecuted;
  finally
    FCurrentPathEditor:=nil;
  end;
end;

procedure TPathEditorButton.DoOnPathEditorExecuted;
var
  Ok: Boolean;
  NewPath: String;
begin
  NewPath := FCurrentPathEditor.Path;
  Ok := (FCurrentPathEditor.ModalResult = mrOk) and (AssociatedEdit.Text <> NewPath);
  if Ok and Assigned(OnExecuted) then
    Ok := OnExecuted(ContextCaption, NewPath);
  // Assign value only if old <> new and OnExecuted allows it.
  if Ok then
    SetPathTextAndHint(NewPath, AssociatedEdit);
end;

end.

