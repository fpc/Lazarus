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

  Abstract:
    List all to do comments of current project and the file
    projectname.todo.
    {TODO -oOwnerName -cCategoryName: Todo_text}
    {DONE -oOwnerName -cCategoryName: Todo_text}
    {#todo -oOwnerName -cCategoryName: Todo_text}
    {#done -oOwnerName -cCategoryName: Todo_text}

    the -o and -c tags are optional.
    
    Sub comments in nested comments are ignored.
*)
{#todo options }
{#todo print an todo report }


unit TodoList;

{$mode objfpc}{$H+}

interface

uses
  // FCL, LCL
  Classes, SysUtils, LCLProc, Forms, Controls, Graphics, Dialogs, LResources,
  StrUtils, ExtCtrls, ComCtrls, Menus, Buttons, GraphType, ActnList, AvgLvlTree,
  StdCtrls, LCLIntf, LCLType,
  // Codetools
  CodeAtom, CodeCache, CodeToolManager, BasicCodeTools, FileProcs,
  // IDEIntf
  LazIDEIntf, IDEImagesIntf, PackageIntf, ProjectIntf,
  // IDE
  LazarusIDEStrConsts, PackageDefs;


Const
  cTodoFlag = '#todo';
  cDoneFlag = '#done';
  cAltTodoFLag = 'TODO';
  cAltDoneFLag = 'DONE';

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

  { TfrmTodo }

  TfrmTodo = class(TForm)
    acGoto: TAction;
    acRefresh: TAction;
    ActionList: TActionList;
    lvTodo: TListView;// lvTodo.Items.Data is TTodoItem (they belong to the TTLScannedFile)
    StatusBar: TStatusBar;
    ToolBar: TToolBar;
    tbGoto: TToolButton;
    tbOptions: TToolButton;
    tbPrint: TToolButton;
    tbRefresh: TToolButton;
    procedure acGotoExecute(Sender: TObject);
    procedure acRefreshExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift:TShiftState);
    procedure lvTodoClick(Sender: TObject);
  private
    fBuild       : Boolean;
    fMainSourceFilename    : String;
    FOnOpenFile  : TOnOpenFile;
    fRootCBuffer : TCodeBuffer;
    fScannedFiles: TAvgLvlTree;// tree of TTLScannedFile

    procedure SetMainSourceFilename(const AValue: String);

    function  CreateToDoItem(aTLFile: TTLScannedFile;
        const aFileName: string; const SComment, EComment: string;
        const TokenString: string; LineNumber: Integer): TTodoItem;
    procedure AddListItem(aTodoItem: TTodoItem);
    
    procedure ScanFile(aFileName : string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property MainSourceFilename : String read fMainSourceFilename write SetMainSourceFilename;
    property OnOpenFile: TOnOpenFile read FOnOpenFile write FOnOpenFile;
  end;

var
  frmTodo: TfrmTodo;

implementation

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

{ TfrmTodo }

constructor TfrmTodo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ToolBar.Images := IDEImages.Images_16;
  acGoto.ImageIndex := IDEImages.LoadImage(16, 'menu_goto_line');
  acRefresh.ImageIndex := IDEImages.LoadImage(16, 'refresh');
end;

destructor TfrmTodo.Destroy;
begin
  fScannedFiles.FreeAndClear;
  FreeAndNil(fScannedFiles);
  inherited Destroy;
end;

procedure TfrmTodo.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key=VK_ESCAPE then
    ModalResult:=mrCancel;
end;

procedure TfrmTodo.lvTodoClick(Sender: TObject);
begin
  acGoto.Execute;
end;

//Initialise the todo project and find them
procedure TfrmTodo.SetMainSourceFilename(const AValue: String);
begin
  if fMainSourceFilename=AValue then exit;
  fMainSourceFilename:=AValue;
  Caption:=lisTodoListCaption+' '+fMainSourceFilename;
  acRefresh.Execute;
end;

function TfrmTodo.CreateToDoItem(aTLFile: TTLScannedFile;
  const aFileName: string; const SComment, EComment: string;
  const TokenString: string; LineNumber: Integer): TTodoItem;
var
  N,Strlen    : Integer;
  TempStr       : string;
  ParsingString : string;
  IsAltNotation,
  IsDone        : boolean;
  aChar         : char;
  
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

  Result := nil;
  
  ParsingString:= Trim(TokenString);
  
  // Remove the beginning comment chars from input string
  Delete(ParsingString, 1, Length(SComment));

  // Remove leading and trailing blanks from input
  ParsingString := Trim(ParsingString);

  // See if it's a TODO or DONE item
  if (Pos(cTodoFlag, ParsingString) = 1) then
  begin
    IsDone := False;
    IsAltNotation := False;
  end
  else
  begin
    if (Pos(cAltTodoFLag, ParsingString) = 1) then
    begin
      IsDone := False;
      IsAltNotation := True;
    end
    else
    begin
      if (Pos(cDoneFlag, ParsingString) = 1) then
      begin
        IsDone := True;
        IsAltNotation := False;
      end
      else
      begin
        if (Pos(cAltDoneFLag, ParsingString) = 1) then
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

  n := 1;
  TempStr := '';
  Strlen  := Length(ParsingString);
  
  // Parse priority, owner and category
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
  
  // Set item text
  Result.Text := ParsingString;
  
end;


procedure TfrmTodo.FormCreate(Sender: TObject);
begin
  fBuild:=False;
  fScannedFiles:=TAvgLvlTree.Create(@CompareTLScannedFiles);

  Caption := lisTodoListCaption;

  acRefresh.Hint  := lisTodolistRefresh;
  acGoto.Hint  := listodoListGotoLine;
  tbPrint.Hint   :=listodoListPrintList;
  tbOptions.Hint  :=  lisToDoListOptions;

  tbOptions.Caption:=dlgFROpts;
  tbPrint.Caption:=srVK_PRINT;
  tbRefresh.Caption:=dlgUnitDepRefresh;
  tbGoto.Caption:=lisToDoGoto;

  with lvTodo do
  begin
    Column[0].Caption := 'Done';
    Column[0].Width   := 45;
    Column[1].Caption := lisToDoLDescription;
    Column[1].Width   := 150;
    Column[2].Caption := lisToDoLPriority;
    Column[2].Width   := 45;
    Column[3].Caption := lisToDoLFile;
    Column[3].Width   := 80;
    Column[4].Caption := lisToDoLLine;
    Column[4].Width   := 40;
    Column[5].Caption := lisToDoLOwner;
    Column[5].Width   := 50;
    Column[6].Caption := listToDoLCategory;
    Column[6].Width   := 80;
  end;
end;

procedure TfrmTodo.acGotoExecute(Sender: TObject);
var
  CurFilename: String;
  aTodoItem: TTodoItem;
  aListItem: TListItem;
  TheLine: integer;
  UsedInterfaceFilenames: TStrings;
  UsedImplementationFilenames: TStrings;
  i: integer;
  Found: boolean;
begin
  CurFilename:='';
  aListItem:= lvtodo.Selected;
  Found:= false;
  if Assigned(aListItem) and Assigned(aListItem.Data) then
  begin
    aTodoItem := TTodoItem(aListItem.Data);
    CurFileName := aTodoItem.Filename;
    TheLine     := aTodoItem.LineNumber;
    if not FileNameIsAbsolute(CurFileName) then
    begin
      if Assigned(CodeToolBoss) then
      begin
        fRootCBuffer:=CodeToolBoss.LoadFile(fMainSourceFilename,false,false);
        if not Assigned(fRootCBuffer) then Exit;
        if CodeToolBoss.FindUsedUnitFiles(fRootCBuffer,UsedInterfaceFilenames,
                                          UsedImplementationFilenames) then
        begin
          try
            for i:=0 to UsedInterfaceFilenames.Count-1 do
            begin
              if CompareFilenames(ExtractFileName(UsedInterfaceFileNames[i]),
                                  CurFileName) = 0 then
              begin
                CurFileName:= UsedInterFaceFileNames[i];
                Found:= true;
                break;
              end;
            end;
            if not Found then
            begin
              for i:=0 to UsedImplementationFilenames.Count-1 do
              begin
                if CompareFilenames(ExtractFileName
                (UsedImplementationFilenames[i]), CurFileName) = 0 then
                begin
                  CurFileName:= UsedImplementationFilenames[i];
                  break;
                end;
              end;
            end;
          finally
            UsedImplementationFilenames.Free;
            UsedInterfaceFilenames.Free;
          end;
        end;
      end;
    end;
    if Assigned(OnOpenFile) then
      OnOpenFile(Self,CurFilename,TheLine)
    else
      LazarusIDE.DoOpenFileAndJumpToPos(CurFilename,Point(1,TheLine),-1,-1,
        [ofOnlyIfExists,ofRegularFile,ofVirtualFile,ofDoNotLoadResource]);
  end;
end;

procedure TfrmTodo.acRefreshExecute(Sender: TObject);
var
  i: integer;
  St : String;
  Owners: TFPList;
  CurOwner: TObject;
  CurProject: TLazProject;
  CurPackage: TLazPackage;
  CurProjFile: TLazProjectFile;
  CurPkgFile: TPkgFile;
begin
  if fBuild then Exit;

  //DebugLn(['TfrmTodo.acRefreshExecute MainSourceFilename=',MainSourceFilename]);

  Screen.Cursor:=crHourGlass;
  Owners:=nil;
  try
    fBuild:=True;
    lvTodo.Items.Clear;
    
    if MainSourceFilename='' then exit;
    
    // Find an '.todo' file of the main source
    St:=ChangeFileExt(MainSourceFilename,'.todo');
    if FileExists(St) then
      ScanFile(St);

    // Scan main source file
    ScanFile(MainSourceFilename);
    
    // find project/package
    CurOwner:=nil;
    CurProject:=nil;
    CurPackage:=nil;
    Owners:=PackageEditingInterface.GetOwnersOfUnit(MainSourceFilename);
    if (Owners<>nil) then begin
      i:=0;
      while i<Owners.Count do begin
        CurOwner:=TObject(Owners[0]);
        if CurOwner is TLazProject then begin
          // this file is owned by a project
          CurProject:=TLazProject(CurOwner);
          if (CurProject.MainFile<>nil)
          and (CompareFilenames(CurProject.MainFile.Filename,MainSourceFilename)=0)
          then begin
            // create the list of todo items for this project
            break;
          end;
          CurProject:=nil;
        end else if CurOwner is TLazPackage then begin
          // this file is owned by a package
          CurPackage:=TLazPackage(CurOwner);
          if (CurPackage.GetSrcFilename<>'')
          and (CompareFilenames(CurPackage.GetSrcFilename,MainSourceFilename)=0)
          then begin
            // create the list of todo items for this package
            break;
          end;
          CurPackage:=nil;
        end;
        inc(i);
      end;
      if i=Owners.Count then
        CurOwner:=nil;// no appropriate owner found
    end;

    if CurProject<>nil then begin
      // scan all units of project
      for i:=0 to CurProject.FileCount-1 do begin
        CurProjFile:=CurProject.Files[i];
        if CurProjFile.IsPartOfProject
        and FilenameIsPascalUnit(CurProjFile.Filename)then
          ScanFile(CurProjFile.Filename);
      end;
    end;
    if CurPackage<>nil then begin
      // scan all units of package
      DebugLn(['TfrmTodo.acRefreshExecute AAA1 ',CurPackage.Filename]);
      for i:=0 to CurPackage.FileCount-1 do begin
        CurPkgFile:=CurPackage.Files[i];
        DebugLn(['TfrmTodo.acRefreshExecute AAA2 ',i,' ',CurPkgFile.Filename]);
        if FilenameIsPascalUnit(CurPkgFile.Filename) then
          ScanFile(CurPkgFile.Filename);
      end;
    end;
  finally
    Owners.Free;
    Screen.Cursor:=crDefault;
    fBuild:=False;
  end;
end;

procedure TfrmTodo.AddListItem(aTodoItem: TTodoItem);
var
   aListItem: TListItem;
begin
  if Assigned(aTodoItem) then
  begin
    aListitem := lvTodo.Items.Add;
    aListitem.Data := aTodoItem;
    if aTodoItem.Done then
      aListItem.Caption := 'X'
    else
      aListItem.Caption := ' ';
    aListitem.SubItems.Add(aTodoItem.Text);
    aListitem.SubItems.Add(IntToStr(aTodoItem.Priority));
    aListitem.SubItems.Add(aTodoItem.Filename);
    aListitem.SubItems.Add(IntToStr(aTodoItem.LineNumber));
    aListitem.SubItems.Add(aTodoItem.Owner);
    aListitem.SubItems.Add(aTodoItem.Category);
  end;
end;

procedure TfrmTodo.ScanFile(aFileName: string);
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
  i: Integer;
begin
  DebugLn(['TfrmTodo.ScanFile ',aFileName]);
  ExpandedFilename:=TrimFilename(aFileName);
  if not FilenameIsPascalUnit(ExpandedFilename) then exit;

  Code:=CodeToolBoss.LoadFile(ExpandedFilename,true,false);
  if Code=nil then exit;
  CodeToolBoss.Explore(Code,Tool,false,false); // ignore the result
  if (Tool=nil) or (Tool.Scanner=nil) then exit;

  AVLNode:=fScannedFiles.FindKey(Pointer(Tool.MainFilename),
                                 @CompareAnsiStringWithTLScannedFile);
  CurFile:=nil;
  try
    if AVLNode<>nil then begin
      CurFile:=TTLScannedFile(AVLNode.Data);
      // Abort if this file has already been scanned and has not changed
      if CurFile.CodeChangeStep=Tool.Scanner.ChangeStep then exit;
    end;

    // Add file name to list of scanned files
    if CurFile=nil then begin
      CurFile:=TTLScannedFile.Create;
      CurFile.Filename:=Tool.MainFilename;
      fScannedFiles.Add(CurFile);
    end;
    // save ChangeStep
    CurFile.CodeChangeStep:=Tool.Scanner.ChangeStep;
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
      //DebugLn(['TfrmTodo.LoadFile CommentStr="',CommentStr,'"']);
      if Src[p]='/' then
        CreateToDoItem(CurFile,CodeXYPosition.Code.Filename, '//', '', CommentStr, CodeXYPosition.Y)
      else if Src[p]='{' then
        CreateToDoItem(CurFile,CodeXYPosition.Code.Filename, '{', '}', CommentStr, CodeXYPosition.Y)
      else if Src[p]='(' then
        CreateToDoItem(CurFile,CodeXYPosition.Code.Filename, '(*', '*)', CommentStr, CodeXYPosition.Y);
      p:=CommentEnd;
    until false;
  finally
    if (CurFile<>nil) then begin
      for i:=0 to CurFile.Count-1 do
        AddListItem(CurFile[i]);
    end;
  end;
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

initialization
  {$i todolist.lrs}
  
end.

