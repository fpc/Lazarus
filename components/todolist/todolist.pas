(***************************************************************************
                             todolist.pp
                             --------------------

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


  Author:
   Olivier GUILBAUD <golivier@free.fr>,
   Gerard Visent <gerardusmercator@gmail.com>
   Mattias Gaertner
   Alexander du Plessis
   Silvio Clecio
   Kevin Jesshope
   Juha Manninen

  Abstract:
    List all to do comments of current project and the file
    projectname.todo.
    {TODO Priority -oOwnerName -cCategoryName: Todo_text}
    {DONE Priority -oOwnerName -cCategoryName: Todo_text}
    {NOTE Priority -oOwnerName -cCategoryName: Note_text}
    {#todo Priority -oOwnerName -cCategoryName: Todo_text}
    {#done Priority -oOwnerName -cCategoryName: Todo_text}
    {#note Priority -oOwnerName -cCategoryName: Note_text}

    the Priority, -o and -c tags are optional.

    Quoted OwnerName and CategoryName are permitted.
    -o'Lazarus Dev Team' etc

    The colon before the text is optional. Anything to the right of the colon
    will be interpretted as the description text

    Sub comments in nested comments are ignored.
*)

unit ToDoList;

{$mode objfpc}{$H+}

interface

uses
  // FCL, RTL
  Classes, SysUtils, Math, AVL_Tree,
  // LCL
  LCLType, LclIntf, Forms, Controls, StdCtrls, Dialogs, ComCtrls,
  ActnList, XMLPropStorage, ExtCtrls, Menus,
  // LazUtils
  LazFileUtils, LazStringUtils, LazFileCache, LazLoggerBase, LazTracer, AvgLvlTree,
  // Codetools
  CodeToolManager, FileProcs,
  // BuildIntf
  PackageIntf, ProjectIntf,
  // IDEIntf
  LazIDEIntf, IDEImagesIntf, MenuIntf, SrcEditorIntf, IDEOptEditorIntf,
  // ToDoList
  ToDoListCore, ToDoListStrConsts;

Const
  ToDoWindowName = 'IDETodoWindow';

type
  TOnEditToDo = procedure(aTodoItem: TTodoItem; aSrcEdit: TSourceEditorInterface);

  { TIDETodoWindow }

  TIDETodoWindow = class(TForm)
    acEdit: TAction;
    acGoto: TAction;
    acRefresh: TAction;
    acExport: TAction;
    acHelp: TAction;
    acColors: TAction;
    ActionList: TActionList;
    lblCount: TLabel;
    lvTodo: TListView;
    EditMenuItem: TMenuItem;
    AllPackagesMenuItem: TMenuItem;
    FixmeMenuItem: TMenuItem;
    pnlStatistics: TPanel;
    ListedFilesMenuItem: TMenuItem;
    UsedUnitsMenuItem: TMenuItem;
    EditorFilesMenuItem: TMenuItem;
    DepPackagesMenuItem: TMenuItem;
    ToDoMenuItem: TMenuItem;
    DoneMenuItem: TMenuItem;
    NoteMenuItem: TMenuItem;
    mnuFiles: TPopupMenu;
    N1: TToolButton;
    N5: TToolButton;
    mnuList: TPopupMenu;
    mnuTypes: TPopupMenu;
    SaveDialog: TSaveDialog;
    tbEdit: TToolButton;
    tbColors: TToolButton;
    ToolBar: TToolBar;
    tbGoto: TToolButton;
    tbRefresh: TToolButton;
    tbExport: TToolButton;
    N2: TToolButton;
    N3: TToolButton;
    N4: TToolButton;
    tbHelp: TToolButton;
    tbShowTypes: TToolButton;
    tbShowFiles: TToolButton;
    XMLPropStorage: TXMLPropStorage;
    procedure acColorsExecute(Sender: TObject);
    procedure acEditExecute(Sender: TObject);
    procedure acExportExecute(Sender: TObject);
    procedure acGotoExecute(Sender: TObject);
    procedure acHelpExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var {%H-}CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; {%H-}Shift:TShiftState);
    procedure lvTodoClick(Sender: TObject);
    procedure lvTodoCompare(Sender : TObject; Item1, Item2 : TListItem;
      {%H-}Data : Integer; var Compare : Integer);
    procedure lvTodoEnter(Sender: TObject);
    procedure lvTodoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure lvTodoSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure SaveDialogShow(Sender: TObject);
    procedure ShowSomethingMenuItemClick(Sender: TObject);
    procedure ShowChange(Sender: TObject);
    procedure XMLPropStorageRestoreProperties(Sender: TObject);
    procedure XMLPropStorageRestoringProperties(Sender: TObject);
  private
    FBaseDirectory: string;
    FUpdating, FUpdateNeeded: Boolean;
    FIDEItem: string;
    FIdleConnected: boolean;
    FLoadingOptions: boolean;
    FStartFilename: String;
    FOwnerProjPack: TObject;  // Project or package owning the FStartFilename.
    FScannedFiles: TAvlTree;// tree of TTLScannedFile
    FScannedIncFiles: TStringMap;
    FOnEditItem: TOnEditToDo;
    procedure GotoTodo(aTodoItem: TTodoItem);
    procedure SetIDEItem(AValue: string);
    procedure SetIdleConnected(const AValue: boolean);
    function ProjectOpened(Sender: TObject; AProject: TLazProject): TModalResult;
    procedure UpdateStartFilename;
    procedure ResolveIDEItem(out CurOwner: TObject; out CurProject: TLazProject;
                             out CurPkg: TIDEPackage);
    procedure AddListItem(aTodoItem: TTodoItem);
    procedure ScanFile(aFileName : string);
    procedure OnIdle(Sender: TObject; var {%H-}Done: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateTodos(Immediately: boolean = false);

    property IDEItem: string read FIDEItem write SetIDEItem; // package name or empty for active project
    property BaseDirectory: string read FBaseDirectory;
    property IdleConnected: boolean read FIdleConnected write SetIdleConnected;

    property OnEditItem: TOnEditToDo read FOnEditItem write FOnEditItem;
  end;

var
  IDETodoWindow: TIDETodoWindow;


implementation

{$R *.lfm}

const
  DefaultTodoListCfgFile = 'todolistoptions.xml';

{ TIDETodoWindow }

constructor TIDETodoWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if Name<>ToDoWindowName then
    RaiseGDBException('');
  ToolBar.Images := IDEImages.Images_16;
  acEdit.ImageIndex := IDEImages.LoadImage('laz_edit');
  acGoto.ImageIndex := IDEImages.LoadImage('menu_goto_line');
  acRefresh.ImageIndex := IDEImages.LoadImage('laz_refresh');
  acExport.ImageIndex := IDEImages.LoadImage('menu_saveas');
  acColors.ImageIndex := IDEImages.LoadImage('pastel_colors');
  acHelp.ImageIndex := IDEImages.LoadImage('btn_help');
  tbShowTypes.ImageIndex := IDEImages.LoadImage('item_filter');
  tbShowFiles.ImageIndex := IDEImages.LoadImage('laz_open_unit');
  SaveDialog.Filter := dlgFilterCsv+'|*.csv';
  LazarusIDE.AddHandlerOnProjectOpened(@ProjectOpened);
end;

destructor TIDETodoWindow.Destroy;
begin
  FScannedFiles.FreeAndClear;
  FreeAndNil(FScannedFiles);
  FreeAndNil(FScannedIncFiles);
  inherited Destroy;
end;

procedure TIDETodoWindow.FormCreate(Sender: TObject);
begin
  FUpdating := False;
  FScannedFiles := TAvlTree.Create(@CompareTLScannedFiles);
  FScannedIncFiles := TStringMap.Create(False);

  Caption := lisToDoList;

  acEdit.Hint := lisTodolistEdit;
  acRefresh.Hint := lisTodolistRefresh;
  acGoto.Hint := listodoListGotoLine;
  acExport.Hint := rsExportTodoIt;
  acColors.Hint := lisColorOptions;

  acEdit.Caption := lisEdit;
  acRefresh.Caption := dlgUnitDepRefresh;
  acGoto.Caption := lisToDoGoto;
  acExport.Caption := lisToDoExport;
  acColors.Caption := lisColors;
  acHelp.Caption := lisHelp;

  // Show Types
  tbShowTypes.Caption := lisShowTypes;
  tbShowTypes.Hint := lisShowTypesHint;
  ToDoMenuItem.Caption  := lisTodo;
  FixmeMenuItem.Caption := lisFixMe;
  DoneMenuItem.Caption  := lisDone;
  NoteMenuItem.Caption  := lisNote;

  // Show Files
  tbShowFiles.Caption := lisShowFiles;
  tbShowFiles.Hint := lisShowFilesHint;
  ListedFilesMenuItem.Caption := lisToDoListed;
  //ListedFilesMenuItem.Hint := lisToDoListedHint;         Menu hints don't work!
  UsedUnitsMenuItem.Caption := lisToDoUsedUnits;
  //UsedUnitsMenuItem.Hint := lisToDoUsedUnitsHint;
  EditorFilesMenuItem.Caption := lisSourceEditor;
  //EditorFilesMenuItem.Hint := lisSourceEditorHint;
  DepPackagesMenuItem.Caption := lisPackages;
  //DepPackagesMenuItem.Hint := Format(lisPackagesHint, [lisToDoListed, lisToDoUsedUnits]);
  //AllPackagesMenuItem.Caption := ;
  //AllPackagesMenuItem.Hint := ;
  with lvTodo do
  begin
    Column[0].Caption := lisToDoLType;
    Column[1].Caption := lisToDoLDescription;
    Column[1].Width   := 700;
    Column[2].Caption := lisToDoLPriority;
    Column[3].Caption := lisToDoLFile;
    Column[4].Caption := lisToDoLLine;
    Column[5].Caption := lisToDoLOwner;
    Column[6].Caption := listToDoLCategory;
  end;

  XMLPropStorage.FileName := Concat(AppendPathDelim(LazarusIDE.GetPrimaryConfigPath),
    DefaultTodoListCfgFile);
  XMLPropStorage.Active := True;
end;

procedure TIDETodoWindow.UpdateTodos(Immediately: boolean);
var
  i: integer;
  St : String;
  Node: TAvlTreeNode;
  CurFile: TTLScannedFile;
  Units: TStrings;
  Flags: TFindUnitsOfOwnerFlags;
begin
  if FLoadingOptions then
    exit;
  if not Immediately then
  begin
    FUpdateNeeded:=true;
    IdleConnected:=true;
    lblCount.Caption := '';
    exit;
  end;
  FUpdateNeeded:=false;
  if FUpdating or (FOwnerProjPack=nil) then
    Exit;
  LazarusIDE.SaveSourceEditorChangesToCodeCache(nil);
  Screen.BeginWaitCursor;
  lvTodo.BeginUpdate;
  try
    Units:=nil;
    FUpdating:=True;
    CodeToolBoss.ActivateWriteLock;

    FScannedFiles.FreeAndClear;
    FScannedIncFiles.Clear;
    lvTodo.Items.Clear;

    if FStartFilename<>'' then begin
      // Find a '.todo' file of the main source
      St:=ChangeFileExt(FStartFilename,'.todo');
      if FileExistsCached(St) then
        ScanFile(St);
      // Scan main source file
      if FilenameIsPascalUnit(FStartFilename) then
        ScanFile(FStartFilename);
    end;

    Flags:=[];
    if ListedFilesMenuItem.Checked then
      Include(Flags, fuooListed);
    if UsedUnitsMenuItem.Checked then
      Include(Flags, fuooUsed);
    if EditorFilesMenuItem.Checked then
      Include(Flags, fuooSourceEditor);
    if DepPackagesMenuItem.Checked then
      Include(Flags, fuooPackages);

    Units:=LazarusIDE.FindUnitsOfOwner(FOwnerProjPack,Flags);
    for i:=0 to Units.Count-1 do
      ScanFile(Units[i]);

    Node:=FScannedFiles.FindLowest;
    while Node<>nil do
    begin
      CurFile:=TTLScannedFile(Node.Data);
      for i:=0 to CurFile.Count-1 do
        AddListItem(CurFile[i]);
      Node:=FScannedFiles.FindSuccessor(Node);
    end;
  finally
    Units.Free;
    CodeToolBoss.DeactivateWriteLock;
    lvTodo.EndUpdate;
    lblCount.Caption := Format(lisToDoItems, [lvTodo.Items.Count]);
    Screen.EndWaitCursor;
    FUpdating:=False;
  end;
end;

procedure TIDETodoWindow.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key=VK_ESCAPE) then
    ModalResult:=mrCancel;
end;

procedure TIDETodoWindow.lvTodoClick(Sender: TObject);
begin
  acGoto.Execute;
end;

procedure TIDETodoWindow.lvTodoCompare(Sender : TObject;
  Item1, Item2 : TListItem; Data : Integer; var Compare : Integer);
var
  Str1: String;
  Str2: String;
  Int1: Integer;
  Int2: Integer;
begin
  Case lvTodo.SortColumn of
    0, 1, 3, 5, 6 :
      begin
        if lvTodo.SortColumn = 0 then
        begin
          Str1 := TListItem(Item1).Caption;
          Str2 := TListItem(Item2).Caption;
        end else
          begin
            // Checks against Subitems.Count necessary??

            if lvTodo.SortColumn <= Item1.SubItems.Count then
              Str1 := Item1.SubItems.Strings[lvTodo.SortColumn-1]
            else
              Str1 := '';

            if lvTodo.SortColumn <= Item2.SubItems.Count then
              Str2 := Item2.SubItems.Strings[lvTodo.SortColumn-1]
            else
              Str2 := '';
          end;
        Compare := AnsiCompareText(Str1, Str2);
      end;
    2, 4  :
      begin
        if TryStrToInt(Item1.SubItems.Strings[lvTodo.SortColumn-1], Int1)
        and TryStrToInt(Item2.SubItems.Strings[lvTodo.SortColumn-1], Int2) then
          Compare := CompareValue(Int1, Int2)
        else
          Compare := 0;
      end;
    else Compare := 0;
  end;

  if lvTodo.SortDirection = sdDescending then
    Compare := -Compare;
end;

procedure TIDETodoWindow.lvTodoEnter(Sender: TObject);
begin
  acEdit.Enabled := lvTodo.Selected<>nil;
  acGoto.Enabled := acEdit.Enabled;
end;

procedure TIDETodoWindow.lvTodoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (Shift = []) then
  begin
    acGotoExecute(Sender);
    Key := 0;
  end;
end;

procedure TIDETodoWindow.lvTodoSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  lvTodoEnter(Sender);
end;

procedure TIDETodoWindow.SaveDialogShow(Sender: TObject);
begin
  SaveDialog.InitialDir := GetCurrentDirUTF8;
end;

procedure TIDETodoWindow.ShowSomethingMenuItemClick(Sender: TObject);
// Handler used for Show Types and Show Files menu items.
var
  mi: TMenuItem;
begin
  mi := Sender as TMenuItem;
  mi.Checked := not mi.Checked;
  UpdateTodos;
end;

procedure TIDETodoWindow.ShowChange(Sender: TObject);
begin
  UpdateTodos;
end;

procedure TIDETodoWindow.XMLPropStorageRestoreProperties(Sender: TObject);
begin
  FLoadingOptions := False;
  UpdateTodos;
end;

procedure TIDETodoWindow.XMLPropStorageRestoringProperties(Sender: TObject);
begin
  FLoadingOptions := True;
end;

function TIDETodoWindow.ProjectOpened(Sender: TObject; AProject: TLazProject): TModalResult;
begin
  Result:=mrOK;
  IDEItem:='';
  UpdateTodos;
end;

procedure TIDETodoWindow.UpdateStartFilename;
var
  NewStartFilename: String;
  CurProject: TLazProject;
  CurPkg: TIDEPackage;
begin
  ResolveIDEItem(FOwnerProjPack,CurProject,CurPkg);
  NewStartFilename:='';
  if CurPkg<>nil then                   // package
    NewStartFilename:=CurPkg.Filename
  else if CurProject<>nil then          // project
    NewStartFilename:=CurProject.ProjectInfoFile;
  if FStartFilename=NewStartFilename then
    exit;
  FStartFilename:=NewStartFilename;
  UpdateTodos;
end;

procedure TIDETodoWindow.ResolveIDEItem(out CurOwner: TObject;
  out CurProject: TLazProject; out CurPkg: TIDEPackage);
begin
  CurOwner:=nil;
  CurProject:=nil;
  CurPkg:=nil;
  if IsValidIdent(FIDEItem,true,true) then begin
    // package
    CurPkg:=PackageEditingInterface.FindPackageWithName(FIDEItem);
    CurOwner:=CurPkg;
    //DebugLn(['TIDETodoWindow.ResolveIDEItem: Found package ', CurPkg.Filename]);
  end else begin
    // project
    CurProject:=LazarusIDE.ActiveProject;
    CurOwner:=CurProject;
    //DebugLn(['TIDETodoWindow.ResolveIDEItem: Found project ', CurProject.MainFile.Filename]);
  end;
end;

procedure TIDETodoWindow.SetIdleConnected(const AValue: boolean);
begin
  if FIdleConnected=AValue then exit;
  FIdleConnected:=AValue;
  if IdleConnected then
    Application.AddOnIdleHandler(@OnIdle)
  else
    Application.RemoveOnIdleHandler(@OnIdle);
end;

procedure TIDETodoWindow.SetIDEItem(AValue: string);
begin
  //if FIDEItem=AValue then exit;  // No check, trigger update in any case.
  FIDEItem:=AValue;
  UpdateStartFilename;
end;

procedure TIDETodoWindow.acExportExecute(Sender: TObject);
begin
  SaveDialog.FileName:='TodoList_'+FormatDateTime('YYYY_MM_DD',now);
  if SaveDialog.Execute then
    ExtractToCSV(SaveDialog.FileName, lvTodo.Items);
end;

procedure TIDETodoWindow.GotoTodo(aTodoItem: TTodoItem);
begin
  LazarusIDE.DoOpenFileAndJumpToPos(aTodoItem.Filename, aTodoItem.StartPos,-1,-1,-1,
    [ofOnlyIfExists,ofRegularFile,ofVirtualFile,ofDoNotLoadResource]);
end;

procedure TIDETodoWindow.acEditExecute(Sender: TObject);
var
  ListItem: TListItem;
  TodoItem: TTodoItem;
  SrcEdit: TSourceEditorInterface;
begin
  Assert(Assigned(OnEditItem), 'TIDETodoWindow.acEditExecute: OnEditItem=Nil');
  ListItem := lvtodo.Selected;
  if (ListItem=nil) or (ListItem.Data=nil) then exit;
  TodoItem := TTodoItem(ListItem.Data);
  SrcEdit := SourceEditorManagerIntf.ActiveEditor;
  if (SrcEdit=nil) or SrcEdit.ReadOnly then exit;
  if (TodoItem.Filename<>SrcEdit.FileName) or (TodoItem.StartPos<>SrcEdit.CursorTextXY) then
    GotoTodo(TodoItem);          // Move to the right place if not there already.
  OnEditItem(TodoItem, SrcEdit); // Open the dialog for editing.
end;

procedure TIDETodoWindow.acGotoExecute(Sender: TObject);
var
  ListItem: TListItem;
begin
  ListItem := lvtodo.Selected;
  if Assigned(ListItem) and Assigned(ListItem.Data) then
    GotoTodo(TTodoItem(ListItem.Data));
end;

procedure TIDETodoWindow.acColorsExecute(Sender: TObject);
begin
  { #todo 1 -oJuha : Move to the actual ToDo items in the color settings list. }
  //LazarusIDE.DoOpenIDEOptions(TEditorColorOptionsFrame, 'Go to end of color options list', [TIDEEditorOptions], []);
  LazarusIDE.DoOpenIDEOptions(EditorColorOptionsEditorClass,
    lisToDoColorsAreAtTheEndOfColorList, [TIDEEditorOptions], []);
end;

procedure TIDETodoWindow.acHelpExecute(Sender: TObject);
begin
  // usual API from IdeHelpIntf don't work
  OpenURL('https://wiki.freepascal.org/IDE_Window:_ToDo_List');
end;

procedure TIDETodoWindow.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  XMLPropStorage.Save;
end;

procedure TIDETodoWindow.AddListItem(aTodoItem: TTodoItem);

  function ShowThisToDoItem: boolean;
  // Add this ToDoItem based on the cboShowWhat selection
  begin
    case aTodoItem.ToDoType of
      tdToDo:  Result := ToDoMenuItem.Checked;
      tdFixme: Result := FixmeMenuItem.Checked;
      tdDone:  Result := DoneMenuItem.Checked;
      tdNote:  Result := NoteMenuItem.Checked;
    end;
  end;

var
   aListItem: TListItem;
   aFilename: String;
begin
  Assert(Assigned(aTodoItem), 'TIDETodoWindow.AddListItem: aTodoItem=Nil');
  if ShowThisToDoItem then
  begin
    //DebugLn(['TIDETodoWindow.AddListItem ',aTodoItem.Filename,' ',aTodoItem.LineNumber]);
    aListitem := lvTodo.Items.Add;
    aListitem.Data := aTodoItem;
    aListItem.Caption := LIST_INDICATORS[aTodoItem.ToDoType];
    aListitem.SubItems.Add(TextToSingleLine(aTodoItem.Text)); // Join lines if there are many.
    aListitem.SubItems.Add(IntToStr(aTodoItem.Priority));
    aFilename:=aTodoItem.Filename;
    if (BaseDirectory<>'') and FilenameIsAbsolute(aFilename) then
      aFilename:=CreateRelativePath(aFilename,BaseDirectory);
    aListitem.SubItems.Add(aFilename);
    aListitem.SubItems.Add(IntToStr(aTodoItem.StartPos.Y));
    aListitem.SubItems.Add(aTodoItem.Owner);
    aListitem.SubItems.Add(aTodoItem.Category);
  end;
end;

procedure TIDETodoWindow.ScanFile(aFileName: string);
begin
  ToDoListCore.ScanFile(aFileName, FScannedFiles, FScannedIncFiles);
end;

procedure TIDETodoWindow.OnIdle(Sender: TObject; var Done: Boolean);
begin
  IdleConnected:=false;
  UpdateTodos(true);
end;

end.

