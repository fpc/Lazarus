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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************


  Author:
   Olivier GUILBAUD <golivier@free.fr>,
   Gerard Visent <gerardusmercator@gmail.com>
   Mattias Gaertner
   Alexander du Plessis

  Abstract:
    List all to do comments of current project and the file
    projectname.todo.
    {TODO -oOwnerName -cCategoryName: Todo_text}
    {DONE -oOwnerName -cCategoryName: Todo_text}
    {#todo -oOwnerName -cCategoryName: Todo_text}
    {#done -oOwnerName -cCategoryName: Todo_text}

    the -o and -c tags are optional.

    If the -o and -c tags are not used, then the variant without semicolon is
    allowed too:
    {TODO Todo_text}
    {DONE Todo_text}
    {#todo Todo_text}
    {#done Todo_text}

    
    Sub comments in nested comments are ignored.
*)
{#todo options }
{#todo print an todo report }


unit TodoList;

{$mode objfpc}{$H+}

interface

uses
  // FCL, RTL, LCL
  Classes, SysUtils, Math, LCLProc, Forms, Controls, Graphics, Dialogs,
  StrUtils, ExtCtrls, ComCtrls, Menus, Buttons, GraphType, ActnList, AvgLvlTree,
  LCLIntf, LCLType,
  // Codetools
  CodeAtom, CodeCache, CodeToolManager, BasicCodeTools, FileProcs,
  // IDEIntf
  LazIDEIntf, IDEImagesIntf, PackageIntf, ProjectIntf,
  // IDE
  ToDoListStrConsts;


Const
  cTodoFlag = '#todo';
  cDoneFlag = '#done';
  cAltTodoFLag = 'todo:';
  cAltDoneFLag = 'done:';
  ToDoWindowName = 'IDETodoWindow';

type
  TOnOpenFile = procedure(Sender: TObject; const Filename: string;
                          const LineNumber: integer) of object;
  TTLScannedFile = class;

  { TTodoItem: Class to hold TODO item information }

  TTodoItem = class(TObject)
  private
    FAltNotation: boolean;
    FCategory: string;
    FDone: boolean;
    FFilename: string;
    FLineNumber: integer;
    FOwner: string;
    FPriority: integer;
    FText: string;
    FTLFile: TTLScannedFile;
    function GetAsComment: string;
    function GetAsString: string;
  public
    constructor Create(aTLFile: TTLScannedFile);
    property TLFile: TTLScannedFile read FTLFile;
    property AltNotation: boolean read FAltNotation write FAltNotation;
    property Category: string read FCategory write FCategory;
    property Done: boolean read FDone write FDone;
    property LineNumber: integer read FLineNumber write FLineNumber;
    property Filename: string read FFilename write FFilename;
    property Owner: string read FOwner write FOwner;
    property Priority: integer read FPriority write FPriority;
    property Text: string read FText write FText;
    property AsString: string read GetAsString;
    property AsComment: string read GetAsComment;
  end;
  
  { TTLScannedFiles }

  TTLScannedFile = class
    fItems: TFPList;// list of TTodoItem
  private
    function GetCount: integer;
    function GetItems(Index: integer): TTodoItem;
  public
    Filename: string; // = Tool.MainFilename
    CodeChangeStep: integer; // = Tool.Scanner.ChangeStep
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Add(Item: TTodoItem);
    property Count: integer read GetCount;
    property Items[Index: integer]: TTodoItem read GetItems; default;
  end;

  { TIDETodoWindow }

  TIDETodoWindow = class(TForm)
    acGoto: TAction;
    acRefresh: TAction;
    acExport: TAction;
    ActionList: TActionList;
    lvTodo: TListView;
    SaveDialog1: TSaveDialog;
    StatusBar: TStatusBar;
    ToolBar: TToolBar;
    tbGoto: TToolButton;
    tbOptions: TToolButton;
    tbPrint: TToolButton;
    tbRefresh: TToolButton;
    tbExport: TToolButton;
    procedure acExportExecute(Sender: TObject);
    procedure acGotoExecute(Sender: TObject);
    procedure acRefreshExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift:TShiftState);
    procedure FormShow(Sender: TObject);
    procedure lvTodoClick(Sender: TObject);
    procedure lvTodoColumnClick(Sender : TObject; Column : TListColumn);
    procedure lvTodoCompare(Sender : TObject; Item1, Item2 : TListItem;
      Data : Integer; var Compare : Integer);
    procedure SaveDialog1Show(Sender: TObject);
  private
    FBaseDirectory: string;
    fUpdating, fUpdateNeeded: Boolean;
    FIDEItem: string;
    FIdleConnected: boolean;
    fStartFilename: String;
    FOnOpenFile  : TOnOpenFile;
    fScannedFiles: TAvgLvlTree;// tree of TTLScannedFile

    procedure SetIDEItem(AValue: string);
    procedure SetIdleConnected(const AValue: boolean);
    procedure SetStartFilename(const AValue: String);
    procedure UpdateStartFilename;
    procedure ResolveIDEItem(out CurOwner: TObject; out CurProject: TLazProject;
                             out CurPkg: TIDEPackage);

    function  CreateToDoItem(aTLFile: TTLScannedFile;
        const aFileName: string; const SComment, EComment: string;
        const TokenString: string; LineNumber: Integer): TTodoItem;
    procedure AddListItem(aTodoItem: TTodoItem);
    
    procedure ScanFile(aFileName : string);
    procedure OnIdle(Sender: TObject; var Done: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure UpdateTodos(Immediately: boolean = false);

    property IDEItem: string read FIDEItem write SetIDEItem; // package name or empty for active project
    property StartFilename : String read fStartFilename write SetStartFilename; // lpi, lpk or a source file
    property BaseDirectory: string read FBaseDirectory;
    property OnOpenFile: TOnOpenFile read FOnOpenFile write FOnOpenFile;
    property IdleConnected: boolean read FIdleConnected write SetIdleConnected;
  end;

var
  IDETodoWindow: TIDETodoWindow;

implementation

{$R *.lfm}

function CompareTLScannedFiles(Data1, Data2: Pointer): integer;
begin
  Result:=CompareFilenames(TTLScannedFile(Data1).Filename,
                           TTLScannedFile(Data2).Filename);
end;

function CompareAnsiStringWithTLScannedFile(Filename, ScannedFile: Pointer): integer;
begin
  Result:=CompareFilenames(AnsiString(Filename),
                           TTLScannedFile(ScannedFile).Filename);
end;

{ TIDETodoWindow }

constructor TIDETodoWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if Name<>ToDoWindowName then RaiseGDBException('');
  ToolBar.Images := IDEImages.Images_16;
  acGoto.ImageIndex := IDEImages.LoadImage(16, 'menu_goto_line');
  acRefresh.ImageIndex := IDEImages.LoadImage(16, 'laz_refresh');
  acExport.ImageIndex := IDEImages.LoadImage(16, 'menu_saveas');
end;

destructor TIDETodoWindow.Destroy;
begin
  fScannedFiles.FreeAndClear;
  FreeAndNil(fScannedFiles);
  inherited Destroy;
end;

procedure TIDETodoWindow.UpdateTodos(Immediately: boolean);
var
  i: integer;
  St : String;
  CurOwner: TObject;
  CurProject: TLazProject;
  CurPackage: TIDEPackage;
  CurProjFile: TLazProjectFile;
  Node: TAvgLvlTreeNode;
  CurFile: TTLScannedFile;
  CurPkgFile: TLazPackageFile;
begin
  if not Immediately then begin
    fUpdateNeeded:=true;
    IdleConnected:=true;
    exit;
  end;
  fUpdateNeeded:=false;

  if fUpdating then Exit;

  DebugLn(['TfrmTodo.UpdateTodos StartFilename=',StartFilename,' IDEItem=',IDEItem]);

  LazarusIDE.SaveSourceEditorChangesToCodeCache(nil);

  Screen.Cursor:=crHourGlass;
  lvTodo.BeginUpdate;
  try
    fUpdating:=True;
    CodeToolBoss.ActivateWriteLock;

    fScannedFiles.FreeAndClear;
    lvTodo.Items.Clear;

    if StartFilename<>'' then begin
      // Find a '.todo' file of the main source
      St:=ChangeFileExt(StartFilename,'.todo');
      if FileExistsUTF8(St) then
        ScanFile(St);
      // Scan main source file
      if FilenameIsPascalUnit(StartFilename) then
        ScanFile(StartFilename);
    end;

    // find project/package
    ResolveIDEItem(CurOwner,CurProject,CurPackage);
    if CurOwner=nil then begin
      CurProject:=LazarusIDE.ActiveProject;
      CurOwner:=CurProject;
    end;

    //debugln(['TIDETodoWindow.UpdateTodos Owner=',DbgSName(CurOwner)]);
    if CurProject<>nil then begin
      // scan all units of project
      Caption:=lisToDoList+' '+CurProject.ProjectInfoFile;
      FBaseDirectory:=ExtractFilePath(CurProject.ProjectInfoFile);
      if (CurProject.MainFile<>nil) and (pfMainUnitIsPascalSource in CurProject.Flags)
      then
        ScanFile(CurProject.MainFile.Filename);
      for i:=0 to CurProject.FileCount-1 do begin
        CurProjFile:=CurProject.Files[i];
        //debugln(['TIDETodoWindow.UpdateTodos ',CurProjFile.IsPartOfProject,' ',CurProjFile.Filename]);
        if CurProjFile.IsPartOfProject
        and FilenameIsPascalUnit(CurProjFile.Filename) then
          ScanFile(CurProjFile.Filename);
      end;
    end else if CurPackage<>nil then begin
      // scan all units of package
      Caption:=lisToDoList+' '+CurPackage.Filename;
      FBaseDirectory:=ExtractFilePath(CurPackage.Filename);
      for i:=0 to CurPackage.FileCount-1 do begin
        CurPkgFile:=CurPackage.Files[i];
        if FilenameIsPascalUnit(CurPkgFile.Filename) then
          ScanFile(CurPkgFile.Filename);
      end;
    end;

    Node:=fScannedFiles.FindLowest;
    while Node<>nil do begin
      CurFile:=TTLScannedFile(Node.Data);
      for i:=0 to CurFile.Count-1 do
        AddListItem(CurFile[i]);
      Node:=fScannedFiles.FindSuccessor(Node);
    end;
  finally
    CodeToolBoss.DeactivateWriteLock;
    lvTodo.EndUpdate;
    Screen.Cursor:=crDefault;
    fUpdating:=False;
  end;
end;

procedure TIDETodoWindow.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Shift=[] then ;
  if (Key=VK_ESCAPE) then
    ModalResult:=mrCancel;
end;

procedure TIDETodoWindow.FormShow(Sender: TObject);
begin
  IdleConnected:=true;
end;

procedure TIDETodoWindow.lvTodoClick(Sender: TObject);
begin
  acGoto.Execute;
end;

procedure TIDETodoWindow.lvTodoColumnClick(Sender : TObject; Column : TListColumn);
Var
  aListItem : TListItem;
begin
  aListItem := lvTodo.Selected;

  If lvTodo.SortDirection = sdAscending then lvTodo.SortDirection := sdDescending
  Else lvTodo.SortDirection := sdAscending;

  lvTodo.SortColumn := Column.Index;

  lvTodo.Selected := nil;  // Otherwise wrong selection - bug??
  lvTodo.Selected := aListItem;

  lvTodo.Update;  // First row not redrawn?
  //lvTodo.Repaint;
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

procedure TIDETodoWindow.SaveDialog1Show(Sender: TObject);
begin
  SaveDialog1.InitialDir:=GetCurrentDirUTF8;
end;

//Initialise the todo project and find them
procedure TIDETodoWindow.SetStartFilename(const AValue: String);
begin
  //debugln(['TIDETodoWindow.SetOwnerFilename ',AValue]);
  if fStartFilename=AValue then exit;
  fStartFilename:=AValue;
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
  if (IDEItem<>'') and IsValidIdent(IDEItem) then begin
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

function TIDETodoWindow.CreateToDoItem(aTLFile: TTLScannedFile;
  const aFileName: string; const SComment, EComment: string;
  const TokenString: string; LineNumber: Integer): TTodoItem;
var
  N, Strlen: Integer;
  TempStr, ParsingString, LowerString : string;
  IsAltNotation,
  IsDone: boolean;
  aChar: char;

const
  cSemiColon  = ':';
  cWhiteSpace = ' ';
  
  Procedure SetItemFields(aItem: TTodoItem; aStr: String);
  var
     aPriority: integer;
  begin
    if aStr <> '' then
    begin
      // Category
      if pos('-c', aStr) = 1 then
        aItem.Category := Copy(aStr, 3, Length(aStr)-2)
      else
      begin
        // Owner
        if pos('-o', aStr) = 1 then
          aItem.Owner := Copy(aStr, 3, Length(aStr)-2)
        else
        begin
          // Priority
          if TryStrToInt(aStr, aPriority) then
            aItem.Priority := aPriority;
        end;
      end;
    end;
  end;

begin
  //DebugLn(['TfrmTodo.CreateToDoItem aFileName=',aFileName,' LineNumber=',LineNumber]);
  Result := nil;
  ParsingString:= TextToSingleLine(TokenString);
  // Remove the beginning comment chars from input string
  Delete(ParsingString, 1, Length(SComment));
  // Remove leading and trailing blanks from input
  ParsingString := Trim(ParsingString);
  // See if it's a TODO or DONE item
  LowerString := lowercase(ParsingString);
  if (Pos(cTodoFlag, LowerString) = 1) then
  begin
    IsDone := False;
    IsAltNotation := False;
  end
  else
  begin
    if (Pos(cAltTodoFLag, LowerString) = 1) then
    begin
      IsDone := False;
      IsAltNotation := True;
    end
    else
    begin
      if (Pos(cDoneFlag, LowerString) = 1) then
      begin
        IsDone := True;
        IsAltNotation := False;
      end
      else
      begin
        if (Pos(cAltDoneFLag, LowerString) = 1) then
        begin
          IsDone := True;
          IsAltNotation := True;
        end
        else
          // Not a Todo/Done item, leave
          Exit;
      end;
    end;
  end;
  
  // Remove the ending comment chars from input string
  if (eComment<>'')
  and (Pos(EComment, ParsingString)=Length(ParsingString)- Length(EComment)+1) then
    ParsingString := Copy(ParsingString, 1, Length(ParsingString)-Length(eComment));
  
  // Remove Todo/Done flag from input string
  if isAltNotation then
    Delete(ParsingString, 1, 4)
  else
    Delete(ParsingString, 1, 5);

  Result := TTodoItem.Create(aTLFile);
  Result.Done := IsDone;
  Result.AltNotation := IsAltNotation;
  Result.LineNumber  := LineNumber;
  Result.Filename    := aFileName;
  if aTLFile<>nil then begin
    aTLFile.Add(Result);
  end;

  if Pos(cSemiColon, ParsingString)>0 then
  begin
    // Parse priority, owner and category
    n := 1;
    TempStr := '';
    Strlen  := Length(ParsingString);

    while (n <= StrLen) and (ParsingString[n]<>cSemiColon) do
    begin

      aChar := ParsingString[n];

      // Add char to temporary string
      if (aChar<>cSemiColon) and (aChar<>cWhiteSpace) then
        TempStr := TempStr + aChar

      // Process temporary string
      else
      begin
        SetItemFields(Result, TempStr);
        TempStr := '';;
      end;

      inc(N);

    end;//while

    SetItemFields(Result, TempStr);

    Delete(ParsingString, 1, n);
  end;

  // Set item text
  Result.Text := ParsingString;
  
end;


procedure TIDETodoWindow.FormCreate(Sender: TObject);
begin
  fUpdating := False;
  fScannedFiles := TAvgLvlTree.Create(@CompareTLScannedFiles);

  Caption := lisToDoList;

  acRefresh.Hint := lisTodolistRefresh;
  acGoto.Hint := listodoListGotoLine;
  tbPrint.Hint := listodoListPrintList;
  tbOptions.Hint := lisToDoListOptions;

  tbOptions.Caption := dlgFROpts;
  tbPrint.Caption := lisPrint;
  tbRefresh.Caption := dlgUnitDepRefresh;
  tbGoto.Caption := lisToDoGoto;
  tbExport.Caption := lisToDoExport;

  with lvTodo do
  begin
    Column[0].Caption := lisToDoLDone;
    Column[1].Caption := lisToDoLDescription;
    Column[1].Width   := 700;
    Column[2].Caption := lisToDoLPriority;
    Column[3].Caption := lisToDoLFile;
    Column[4].Caption := lisToDoLLine;
    Column[5].Caption := lisToDoLOwner;
    Column[6].Caption := listToDoLCategory;
  end;
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

procedure TIDETodoWindow.acExportExecute(Sender: TObject);
var
  CommaList: TStringList;
  s,t      : string;
  todoItm  : TTodoItem;
  i        : integer;
begin
  SaveDialog1.FileName:='TodoList_'+FormatDateTime('YYYY_MM_DD',now);
  if SaveDialog1.Execute then
  begin
    CommaList:=TStringList.Create;
    try
      CommaList.Add('Done,Description,Priority,Module,Line,Owner,Category');
      i:=0;
      while i<lvTodo.Items.Count do
      begin
        todoItm:=TTodoItem(lvTodo.Items[i].Data);
        if todoItm.Done then
          s:='X,'
        else
          s:=' ,';
        t:=DelChars(todoItm.Text,',');{Strip any commas that can cause a faulty csv file}
        s:=s+t+','+IntToStr(todoItm.Priority)+','+todoItm.Filename+
           ','+IntToStr(todoItm.LineNumber)+','+todoItm.Owner+','+todoItm.Category;
        CommaList.Add(s);
        i:=i+1;
      end;
      CommaList.SaveToFile(UTF8ToSys(SaveDialog1.FileName));
    finally
      CommaList.Clear;
      CommaList.Free;
    end;
  end;
end;

procedure TIDETodoWindow.acRefreshExecute(Sender: TObject);
begin
  UpdateTodos;
end;

procedure TIDETodoWindow.AddListItem(aTodoItem: TTodoItem);
var
   aListItem: TListItem;
   aFilename: String;
begin
  if Assigned(aTodoItem) then
  begin
    //DebugLn(['TfrmTodo.AddListItem ',aTodoItem.Filename,' ',aTodoItem.LineNumber]);
    aListitem := lvTodo.Items.Add;
    aListitem.Data := aTodoItem;
    if aTodoItem.Done then
      aListItem.Caption := 'X'
    else
      aListItem.Caption := ' ';
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
var
  ExpandedFilename: String;
  AVLNode: TAvgLvlTreeNode;
  Tool: TCodeTool;
  Code: TCodeBuffer;
  CurFile: TTLScannedFile;
  Src: String;
  p: Integer;
  NestedComment: Boolean;
  CommentEnd: LongInt;
  CommentStr: String;
  CodeXYPosition: TCodeXYPosition;
begin
  //DebugLn(['TfrmTodo.ScanFile ',aFileName]);
  ExpandedFilename:=TrimFilename(aFileName);

  Code:=CodeToolBoss.LoadFile(ExpandedFilename,true,false);
  if Code=nil then begin
    debugln(['TIDETodoWindow.ScanFile failed loading ',ExpandedFilename]);
    exit;
  end;
  CodeToolBoss.Explore(Code,Tool,false,false); // ignore the result
  if (Tool=nil) or (Tool.Scanner=nil) then begin
    debugln(['TIDETodoWindow.ScanFile failed parsing ',Code.Filename]);
    exit;
  end;

  AVLNode:=fScannedFiles.FindKey(Pointer(Tool.MainFilename),
                                 @CompareAnsiStringWithTLScannedFile);
  CurFile:=nil;
  //DebugLn(['TfrmTodo.ScanFile ',Tool.MainFilename,' AVLNode=',AVLNode<>nil]);
  if AVLNode<>nil then begin
    CurFile:=TTLScannedFile(AVLNode.Data);
    // Abort if this file has already been scanned and has not changed
    if CurFile.CodeChangeStep=Tool.Scanner.ChangeStep then exit;
  end;
  //DebugLn(['TfrmTodo.ScanFile SCANNING ... ']);

  // Add file name to list of scanned files
  if CurFile=nil then begin
    CurFile:=TTLScannedFile.Create;
    CurFile.Filename:=Tool.MainFilename;
    fScannedFiles.Add(CurFile);
  end;
  // save ChangeStep
  CurFile.CodeChangeStep:=Tool.Scanner.ChangeStep;
  //DebugLn(['TfrmTodo.ScanFile saved ChangeStep ',CurFile.CodeChangeStep,' ',Tool.Scanner.ChangeStep]);
  // clear old items
  CurFile.Clear;

  // Display file name being processed
  //StatusBar.SimpleText := aFileName;
  //StatusBar.Repaint;

  Src:=Tool.Src;
  p:=1;
  NestedComment:=CodeToolBoss.GetNestedCommentsFlagForFile(Code.Filename);
  repeat
    p:=FindNextComment(Src,p);
    if p>length(Src) then break;
    CommentEnd:=FindCommentEnd(Src,p,NestedComment);
    Tool.CleanPosToCaret(p,CodeXYPosition);
    CommentStr:=copy(Src,p,CommentEnd-p);
    //DebugLn(['TfrmTodo.ScanFile CommentStr="',CommentStr,'"']);
    if Src[p]='/' then
      CreateToDoItem(CurFile,CodeXYPosition.Code.Filename, '//', '', CommentStr, CodeXYPosition.Y)
    else if Src[p]='{' then
      CreateToDoItem(CurFile,CodeXYPosition.Code.Filename, '{', '}', CommentStr, CodeXYPosition.Y)
    else if Src[p]='(' then
      CreateToDoItem(CurFile,CodeXYPosition.Code.Filename, '(*', '*)', CommentStr, CodeXYPosition.Y);
    p:=CommentEnd;
  until false;
end;

procedure TIDETodoWindow.OnIdle(Sender: TObject; var Done: Boolean);
begin
  if Done then ;
  IdleConnected:=false;
  UpdateTodos(true);
end;

{ TTodoItem }

function TTodoItem.GetAsString: string;
begin
  // Todo/Done in two notations
  if AltNotation then
  begin
   if Done then
     Result := 'DONE'
   else
     Result := 'TODO';
  end
  else
  begin
    if Done then
      Result := '#done'
    else
      Result := '#todo';
  end;
  // Priority
  if Priority > 0 then
    Result := Result + ' '+IntToStr(Priority);
  // Owner
  if Owner <> '' then
    Result := Result + ' -o'+Owner;
  // Category
  if Category <> '' then
    Result := Result + ' -c'+Category;
  // Text
  Result := Result + ' : ' + Text;
end;

constructor TTodoItem.Create(aTLFile: TTLScannedFile);
begin
  FTLFile:=aTLFile;
end;

function TTodoItem.GetAsComment: string;
begin
  Result := '{ '+AsString+' }';
end;

{ TTLScannedFile }

function TTLScannedFile.GetCount: integer;
begin
  if fItems=nil then
    Result:=0
  else
    Result:=fItems.Count;
end;

function TTLScannedFile.GetItems(Index: integer): TTodoItem;
begin
  Result:=TTodoItem(fItems[Index]);
end;

constructor TTLScannedFile.Create;
begin

end;

destructor TTLScannedFile.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TTLScannedFile.Clear;
var
  i: Integer;
begin
  if fItems<>nil then begin
    for i:=0 to fItems.Count-1 do
      TObject(fItems[i]).Free;
    FreeAndNil(fItems);
  end;
end;

procedure TTLScannedFile.Add(Item: TTodoItem);
begin
  if fItems=nil then fItems:=TFPList.Create;
  fItems.Add(Item);
end;

end.

