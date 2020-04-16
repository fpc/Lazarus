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
  Classes, SysUtils, Math, Laz_AVL_Tree,
  // LCL
  LCLType, LclIntf, Forms, Controls, StdCtrls, Dialogs, ComCtrls,
  ActnList, XMLPropStorage, ExtCtrls,
  // LazUtils
  LazFileUtils, LazStringUtils, LazFileCache, LazLoggerBase, LazTracer,
  // Codetools
  CodeToolManager, FileProcs,
  // IDEIntf
  LazIDEIntf, IDEImagesIntf, PackageIntf, ProjectIntf,
  // ToDoList
  ToDoListCore, ToDoListStrConsts;

Const
  ToDoWindowName = 'IDETodoWindow';

type
  TOnOpenFile = procedure(Sender: TObject; const Filename: string;
                          const LineNumber: integer) of object;

  { TIDETodoWindow }

  TIDETodoWindow = class(TForm)
    acGoto: TAction;
    acRefresh: TAction;
    acExport: TAction;
    acHelp: TAction;
    ActionList: TActionList;
    chkListed: TCheckBox;
    chkUsed: TCheckBox;
    chkPackages: TCheckBox;
    chkSourceEditor: TCheckBox;
    cboShowWhat: TComboBox;
    grbOptions: TGroupBox;
    lblShowWhat: TLabel;
    lvTodo: TListView;
    pnlShowWhat: TPanel;
    SaveDialog: TSaveDialog;
    ToolBar: TToolBar;
    tbGoto: TToolButton;
    tbRefresh: TToolButton;
    tbExport: TToolButton;
    N1: TToolButton;
    tbHelp: TToolButton;
    XMLPropStorage: TXMLPropStorage;
    procedure acExportExecute(Sender: TObject);
    procedure acGotoExecute(Sender: TObject);
    procedure acHelpExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var {%H-}CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; {%H-}Shift:TShiftState);
    procedure DoUpdateToDos(Sender: TObject);
    procedure lvTodoClick(Sender: TObject);
    procedure lvTodoCompare(Sender : TObject; Item1, Item2 : TListItem;
      {%H-}Data : Integer; var Compare : Integer);
    procedure SaveDialogShow(Sender: TObject);
    procedure XMLPropStorageRestoreProperties(Sender: TObject);
    procedure XMLPropStorageRestoringProperties(Sender: TObject);
  private
    FBaseDirectory: string;
    FUpdating, FUpdateNeeded: Boolean;
    FIDEItem: string;
    FIdleConnected: boolean;
    FLoadingOptions: boolean;
    FStartFilename: String;
    FOnOpenFile  : TOnOpenFile;
    FScannedFiles: TAvlTree;// tree of TTLScannedFile

    procedure SetIDEItem(AValue: string);
    procedure SetIdleConnected(const AValue: boolean);
    procedure SetStartFilename(const AValue: String);
    procedure UpdateStartFilename;
    procedure ResolveIDEItem(out CurOwner: TObject; out CurProject: TLazProject;
                             out CurPkg: TIDEPackage);

    procedure CreateToDoItem(aTLFile: TTLScannedFile;
        const aFileName: string; const SComment, EComment: string;
        const TokenString: string; LineNumber: Integer);
    procedure AddListItem(aTodoItem: TTodoItem);
    
    procedure ScanFile(aFileName : string);
    procedure OnIdle(Sender: TObject; var {%H-}Done: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure UpdateTodos(Immediately: boolean = false);

    property IDEItem: string read FIDEItem write SetIDEItem; // package name or empty for active project
    property StartFilename: String read FStartFilename write SetStartFilename; // lpi, lpk or a source file
    property BaseDirectory: string read FBaseDirectory;
    property OnOpenFile: TOnOpenFile read FOnOpenFile write FOnOpenFile;
    property IdleConnected: boolean read FIdleConnected write SetIdleConnected;
  end;

var
  IDETodoWindow: TIDETodoWindow;

implementation

{$R *.lfm}

const
  DefaultTodoListCfgFile = 'todolistoptions.xml';

function CompareTLScannedFiles(Data1, Data2: Pointer): integer;
begin
  Result:=CompareFilenames(TTLScannedFile(Data1).Filename,
                           TTLScannedFile(Data2).Filename);
end;

{ TIDETodoWindow }

constructor TIDETodoWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if Name<>ToDoWindowName then RaiseGDBException('');
  ToolBar.Images := IDEImages.Images_16;
  acGoto.ImageIndex := IDEImages.LoadImage('menu_goto_line');
  acRefresh.ImageIndex := IDEImages.LoadImage('laz_refresh');
  acExport.ImageIndex := IDEImages.LoadImage('menu_saveas');
  acHelp.ImageIndex := IDEImages.LoadImage('btn_help');

  SaveDialog.Filter:= dlgFilterCsv+'|*.csv';
end;

destructor TIDETodoWindow.Destroy;
begin
  FScannedFiles.FreeAndClear;
  FreeAndNil(FScannedFiles);
  inherited Destroy;
end;

procedure TIDETodoWindow.UpdateTodos(Immediately: boolean);
var
  i: integer;
  St : String;
  CurOwner: TObject;
  Node: TAvlTreeNode;
  CurFile: TTLScannedFile;
  Units: TStrings;
  CurProject: TLazProject;
  CurPkg: TIDEPackage;
  Flags: TFindUnitsOfOwnerFlags;
begin
  if FLoadingOptions then
    exit;

  if not Immediately then
    begin
      FUpdateNeeded:=true;
      IdleConnected:=true;
      exit;
    end;

  FUpdateNeeded:=false;
  if FUpdating then
    Exit;
  LazarusIDE.SaveSourceEditorChangesToCodeCache(nil);

  Screen.BeginWaitCursor;

  lvTodo.BeginUpdate;
  Units:=nil;
  try
    FUpdating:=True;
    CodeToolBoss.ActivateWriteLock;

    FScannedFiles.FreeAndClear;
    lvTodo.Items.Clear;

    if StartFilename<>'' then begin
      // Find a '.todo' file of the main source
      St:=ChangeFileExt(StartFilename,'.todo');
      if FileExistsCached(St) then
        ScanFile(St);
      // Scan main source file
      if FilenameIsPascalUnit(StartFilename) then
        ScanFile(StartFilename);
    end;

    ResolveIDEItem(CurOwner,CurProject,CurPkg);
    if CurOwner=nil then
      Exit;

    Flags:=[];
    if chkListed.Checked then
      Include(Flags, fuooListed);
    if chkUsed.Checked then
      Include(Flags, fuooUsed);
    if chkPackages.Checked then
      Include(Flags, fuooPackages);
    if chkSourceEditor.Checked then
      Include(Flags, fuooSourceEditor);

    Units:=LazarusIDE.FindUnitsOfOwner(CurOwner,Flags);
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
    Screen.EndWaitCursor;
    FUpdating:=False;
  end;
end;

procedure TIDETodoWindow.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key=VK_ESCAPE) then
    ModalResult:=mrCancel;
end;

procedure TIDETodoWindow.DoUpdateToDos(Sender: TObject);
begin
  UpdateTodos;
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
            else Str1 := '';

            if lvTodo.SortColumn <= Item2.SubItems.Count then
              Str2 := Item2.SubItems.Strings[lvTodo.SortColumn-1]
            else Str2 := '';
          end;
        Compare := AnsiCompareText(Str1, Str2);
      end;
    2, 4  :
      begin
        if TryStrToInt(Item1.SubItems.Strings[lvTodo.SortColumn-1], Int1)
           and TryStrToInt(Item2.SubItems.Strings[lvTodo.SortColumn-1], Int2) then
           Compare := CompareValue(Int1, Int2)
        else Compare := 0;
      end;
    else Compare := 0;
  end;

  if lvTodo.SortDirection = sdDescending then Compare := -Compare;
end;

procedure TIDETodoWindow.SaveDialogShow(Sender: TObject);
begin
  SaveDialog.InitialDir:=GetCurrentDirUTF8;
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

//Initialise the todo project and find them
procedure TIDETodoWindow.SetStartFilename(const AValue: String);
begin
  //debugln(['TIDETodoWindow.SetOwnerFilename ',AValue]);
  if FStartFilename=AValue then
    exit;
  FStartFilename:=AValue;
  UpdateTodos;
end;

procedure TIDETodoWindow.UpdateStartFilename;
var
  NewStartFilename: String;
  CurObject: TObject;
  CurProject: TLazProject;
  CurPkg: TIDEPackage;
begin
  ResolveIDEItem(CurObject,CurProject,CurPkg);
  NewStartFilename:='';
  if CurPkg<>nil then begin
    // package
    NewStartFilename:=CurPkg.Filename;
  end else if CurProject<>nil then begin
    // project
    NewStartFilename:=CurProject.ProjectInfoFile;
  end;
  StartFilename:=NewStartFilename;
end;

procedure TIDETodoWindow.ResolveIDEItem(out CurOwner: TObject;
  out CurProject: TLazProject; out CurPkg: TIDEPackage);
begin
  CurOwner:=nil;
  CurProject:=nil;
  CurPkg:=nil;
  if LazIsValidIdent(IDEItem,true) then begin
    // package
    CurPkg:=PackageEditingInterface.FindPackageWithName(IDEItem);
    CurOwner:=CurPkg;
  end else begin
    // project
    CurProject:=LazarusIDE.ActiveProject;
    CurOwner:=CurProject;
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
  if FIDEItem=AValue then exit;
  FIDEItem:=AValue;
  UpdateStartFilename;
end;

procedure TIDETodoWindow.CreateToDoItem(aTLFile: TTLScannedFile;
  const aFileName: string; const SComment, EComment: string;
  const TokenString: string; LineNumber: Integer);

begin
  TToDoListCore.CreateToDoItem(aTLFile, aFileName, SComment, EComment,TokenString, LineNumber);
end;

procedure TIDETodoWindow.FormCreate(Sender: TObject);
begin
  FUpdating := False;
  FScannedFiles := TAvlTree.Create(@CompareTLScannedFiles);

  Caption := lisToDoList;

  acRefresh.Hint := lisTodolistRefresh;
  acGoto.Hint := listodoListGotoLine;
  acExport.Hint := rsExportTodoIt;
  acRefresh.Caption := dlgUnitDepRefresh;
  acGoto.Caption := lisToDoGoto;
  acExport.Caption := lisToDoExport;
  acHelp.Caption := lisHelp;

  grbOptions.Caption := lisOptions;
  chkListed.Caption := lisToDoListed;
  chkListed.Hint := lisToDoListedHint;
  chkUsed.Caption := lisToDoUsed;
  chkUsed.Hint := lisToDoUsedHint;
  chkPackages.Caption := lisPackages;
  chkPackages.Hint := Format(lisPackagesHint, [lisToDoListed, lisToDoUsed]);
  chkSourceEditor.Caption := lisSourceEditor;
  chkSourceEditor.Hint := lisSourceEditorHint;

  with cboShowWhat do
    begin
      Items[0] := lisFilterItem0;
      Items[1] := lisFilterItem1;
      Items[2] := lisFilterItem2;
      Items[3] := lisFilterItem3;
      Items[4] := lisFilterItem4;
      Items[5] := lisFilterItem5;
      Items[6] := lisFilterItem6;

      Hint := lisShowWhatHint;
    end;

  lblShowWhat.Caption:=lisShowWhat;

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

procedure TIDETodoWindow.acGotoExecute(Sender: TObject);
var
  CurFilename: String;
  aTodoItem: TTodoItem;
  aListItem: TListItem;
  TheLine: integer;
begin
  CurFilename:='';
  aListItem:= lvtodo.Selected;
  if Assigned(aListItem) and Assigned(aListItem.Data) then
  begin
    aTodoItem := TTodoItem(aListItem.Data);
    CurFileName := aTodoItem.Filename;
    TheLine     := aTodoItem.LineNumber;
    if Assigned(OnOpenFile) then
      OnOpenFile(Self,CurFilename,TheLine)
    else
      LazarusIDE.DoOpenFileAndJumpToPos(CurFilename,Point(1,TheLine),-1,-1,-1,
        [ofOnlyIfExists,ofRegularFile,ofVirtualFile,ofDoNotLoadResource]);
  end;
end;

procedure TIDETodoWindow.acHelpExecute(Sender: TObject);
begin
  // usual API from IdeHelpIntf don't work
  OpenURL('http://wiki.freepascal.org/IDE_Window:_ToDo_List');
end;

procedure TIDETodoWindow.acExportExecute(Sender: TObject);
begin
  SaveDialog.FileName:='TodoList_'+FormatDateTime('YYYY_MM_DD',now);
  if SaveDialog.Execute then
    TToDoListCore.ExtractToCSV(lvTodo.Items, SaveDialog.FileName);
end;

procedure TIDETodoWindow.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  XMLPropStorage.Save;
end;

procedure TIDETodoWindow.AddListItem(aTodoItem: TTodoItem);

  function ShowThisToDoItem:boolean;
  // Add this ToDoItem based on the cboShowWhat selection
  begin
    case cboShowWhat.ItemIndex of
      0:Result := True;
      1..3: Result := (TToDoType(cboShowWhat.ItemIndex - 1) =  aTodoItem.ToDoType);
      4:Result := aTodoItem.ToDoType in [tdToDo, tdDone];
      5:Result := aTodoItem.ToDoType in [tdToDo, tdNote];
      6:Result := aTodoItem.ToDoType in [tdDone, tdNote];
    end;
  end;

var
   aListItem: TListItem;
   aFilename: String;

begin
  if Assigned(aTodoItem) and ShowThisToDoItem then
  begin
    //DebugLn(['TfrmTodo.AddListItem ',aTodoItem.Filename,' ',aTodoItem.LineNumber]);
    aListitem := lvTodo.Items.Add;
    aListitem.Data := aTodoItem;
    aListItem.Caption := LIST_INDICATORS[aTodoItem.ToDoType];
    aListitem.SubItems.Add(aTodoItem.Text);
    aListitem.SubItems.Add(IntToStr(aTodoItem.Priority));
    aFilename:=aTodoItem.Filename;
    if (BaseDirectory<>'') and FilenameIsAbsolute(aFilename) then
      aFilename:=CreateRelativePath(aFilename,BaseDirectory);
    aListitem.SubItems.Add(aFilename);
    aListitem.SubItems.Add(IntToStr(aTodoItem.LineNumber));
    aListitem.SubItems.Add(aTodoItem.Owner);
    aListitem.SubItems.Add(aTodoItem.Category);
  end;
end;

procedure TIDETodoWindow.ScanFile(aFileName: string);
begin
  TToDoListCore.ScanFile(aFileName, FScannedFiles);
end;

procedure TIDETodoWindow.OnIdle(Sender: TObject; var Done: Boolean);
begin
  IdleConnected:=false;
  UpdateTodos(true);
end;

end.

