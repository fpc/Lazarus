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


  Author: Olivier GUILBAUD <golivier@free.fr>

  Abstract:
    List all to do comments of current project.
    
    the todo comments has this syntax
      {#todoxxx yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy}
      or
      //#todoxxx yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy
      or
      (.*#todoxxx yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy *.) (delete the .)
      
    where  xxx     is the priority
           yyy..yy is the text of todo
           
   you can create an file naming projectname.todo and add list of todo

*)
(*
Modified by Gerard Visent <gerardusmercator@gmail.com> on 5/11/2007
- Extended to recognize Delphi syntax.
- It works now with the folowing tags:
  #todo, #done, TODO, DONE
- Owner and Category tags are also processed and displayed
  Syntax is -oXXXXX for the owner and -cXXXXX for the category
- Info on the todo items is also stored in a TTodoItem object, for more flexibility
*)

{#todo options }
{#todo print an todo report }

unit TodoList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, Forms, Controls, Graphics, Dialogs, LResources,
  StrUtils, ExtCtrls, ComCtrls, Menus, Buttons, GraphType, ActnList,
  StdCtrls, mPasLex, LCLIntf, LCLType, FileUtil,
  CodeCache, CodeToolManager, LazarusIDEStrConsts,
  Project;


Const
  cTodoFlag = '#todo';
  cDoneFlag = '#done';
  cAltTodoFLag = 'TODO';
  cAltDoneFLag = 'DONE';

type
  TOnOpenFile = procedure(Sender: TObject; const Filename: string;
                          const LineNumber: integer) of object;


  { TTodoItem: Class to hold TODO item information }

  TTodoItem = Class(TObject)
  private
    FAltNotation: boolean;
    FCategory: string;
    FDone: boolean;
    FLineNumber: integer;
    FModule: string;
    FOwner: string;
    FPriority: integer;
    FText: string;
    function GetAsComment: string;
    function GetAsString: string;
  published
    property AltNotation: boolean read FAltNotation write FAltNotation;
    property Category: string read FCategory write FCategory;
    property Done: boolean read FDone write FDone;
    property LineNumber: integer read FLineNumber write FLineNumber;
    property Module: string read FModule write FModule;
    property Owner: string read FOwner write FOwner;
    property Priority: integer read FPriority write FPriority;
    property Text: string read FText write FText;
    property AsString: string read GetAsString;
    property AsComment: string read GetAsComment;
  end;

  { TfrmTodo }

  TfrmTodo = class(TForm)
    acGoto: TAction;
    acRefresh: TAction;
    ActionList: TActionList;
    ImageList: TImageList;
    lvTodo: TListView;
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
    { private declarations }
    fBuild       : Boolean;
    fFileName    : String;
    FOnOpenFile  : TOnOpenFile;
    fRootCBuffer : TCodeBuffer;
    fScannedFile : TStringList;

    procedure SetFileName(const AValue: String);

    Function  GetToDoItem(const aFileName: string; const SComment, EComment: string;
        const TokenString: string; LineNumber: Integer): TTodoItem ;
    procedure ParseDirective(aDirective : String);
    procedure AddListItem(aTodoItem: TTodoItem);
    
    procedure LoadFile(aFileName : string);
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property FileName : String read fFileName write SetFileName;
    property OnOpenFile: TOnOpenFile read FOnOpenFile write FOnOpenFile;
  end;

var
  frmTodo: TfrmTodo;

implementation

{ TfrmTodo }

constructor TfrmTodo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TfrmTodo.Destroy;
var
  i: integer;
begin
  fScannedFile.Free;
  for i := 0 to lvTodo.Items.Count-1 do
    if Assigned(lvTodo.Items[i].Data) then
      TTodoItem(lvTodo.Items[i].Data).Free;
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
procedure TfrmTodo.SetFileName(const AValue: String);
begin
  if fFileName=AValue then exit;
  fFileName:=AValue;
  acRefresh.Execute;
end;

function TfrmTodo.GetToDoItem(const aFileName: string; const SComment,
  EComment: string; const TokenString: string; LineNumber: Integer): TTodoItem;
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
  if (eComment<>'') and (Pos(EComment, ParsingString)=Length(ParsingString)- Length(EComment)+1) then
    ParsingString := Copy(ParsingString, 1, Length(ParsingString)-Length(eComment));
  
  // Remove Todo/Done flag from input string
  if isAltNotation then
    Delete(ParsingString, 1, 4)
  else
    Delete(ParsingString, 1, 5);

  Result := TTodoItem.Create;
  Result.Done := IsDone;
  Result.AltNotation := IsAltNotation;
  Result.LineNumber  := LineNumber;
  Result.Module      := aFileName;

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
  fScannedFile:=TStringList.Create;

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
    CurFileName := aTodoItem.Module;
    TheLine     := aTodoItem.LineNumber;
    if not FileNameIsAbsolute(CurFileName) then
    begin
      if Assigned(CodeToolBoss) then
      begin
        fRootCBuffer:=CodeToolBoss.LoadFile(fFileName,false,false);
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
    if Assigned(OnOpenFile) then OnOpenFile(Self,CurFilename,TheLine);
  end;
end;

procedure TfrmTodo.acRefreshExecute(Sender: TObject);
var
  UsedInterfaceFilenames,
  UsedImplementationFilenames: TStrings;
  i: integer;
  St : String;
begin
  if fBuild then Exit;

  Screen.Cursor:=crHourGlass;
  Try
    fBuild:=True;
    lvTodo.Items.Clear;
    fScannedFile.Clear;
    //Find an '.todo' filename
    St:=ChangeFileExt(fFileName,'.todo');
    If FileExists(St) then
      LoadFile(St);

    //Load project file
    LoadFile(fFileName);

    if Assigned(CodeToolBoss) then
    begin
      fRootCBuffer:=CodeToolBoss.LoadFile(fFileName,false,false);
      if not Assigned(fRootCBuffer) then Exit;

      if CodeToolBoss.FindUsedUnitFiles(fRootCBuffer,UsedInterfaceFilenames,
                                        UsedImplementationFilenames) then
      begin
        try
          for i:=0 to UsedInterfaceFilenames.Count-1 do
            LoadFile(UsedInterfaceFilenames[i]);
          for i:=0 to UsedImplementationFilenames.Count-1 do
            LoadFile(UsedImplementationFilenames[i]);
        finally
          UsedImplementationFilenames.Free;
          UsedInterfaceFilenames.Free;
        end;
      end;
    end;
  finally
    Screen.Cursor:=crDefault;
    fBuild:=False;
  end;
end;

//Find the {$I filename} directive. If exists, call LoadFile()
procedure TfrmTodo.ParseDirective(aDirective : String);
Var N             : Integer;
    ParsingString : string;
begin
  N:=Pos('{$I ',UpperCase(aDirective));
  if N<>0 then
  begin
    //we found a token that looks like an include directive. now extract
    //the file name
    ParsingString:=Trim(Copy(aDirective,N+Length('{$I '),MaxInt));
    ParsingString:=Trim(Copy(ParsingString,1,Pos('}',ParsingString)-1));
    {#ToDo: search in include path}
    LoadFile(ParsingString);
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
    aListitem.SubItems.Add(aTodoItem.Module);
    aListitem.SubItems.Add(IntToStr(aTodoItem.LineNumber));
    aListitem.SubItems.Add(aTodoItem.Owner);
    aListitem.SubItems.Add(aTodoItem.Category);
  end;
end;


procedure TfrmTodo.LoadFile(aFileName: string);
var
  Parser   : TmwPasLex;
  EStream  : TMemoryStream;
  ST       : String;
  aTodoItem: TTodoItem;
  
begin
  St:=ExtractFileName(aFileName);

  // Abort if this file has already been scanned
  if fScannedFile.IndexOf(St)<>-1 then
    Exit;
    
  // Add file name to list of scanned files
  fScannedFile.Add(St);

  // Display file name being processed
  //StatusBar.SimpleText := aFileName;
  //StatusBar.Repaint;


  EStream := TMemoryStream.Create;
  try
    //Echange of stream
    Try
      if not FileExists(aFileName) then
        if Assigned(Project1) then
          aFileName := AppendPathDelim(Project1.ProjectDirectory)+aFileName;
      EStream.LoadFromFile(aFileName);
      EStream.Position := EStream.Size;
      EStream.WriteByte(0); // Terminate string for TmwPasLex
    Except
      FreeAndNil(EStream);
    End;
    if not Assigned(EStream) then Exit; // Silently ignore failed reads

    EStream.Position := 0;

    Parser := TmwPasLex.Create;
    try
      Parser.Origin := EStream.Memory;
      while Parser.TokenID <> tkNull do
      begin
        { TODO 3 -oStefan -cIssue: This needs to be fixed for multiline comments;
          the strategy ought to be to read the complete comment and only then
          start parsing. Also it would be better to move deleting of the comment
          tokens out of the parser }
        aTodoItem := nil;
        case Parser.TokenID of
          tkBorComment: aTodoItem := GetToDoItem(aFileName, '{', '}', Parser.Token, Parser.LineNumber + 1);
          tkAnsiComment: aTodoItem := GetToDoItem(aFileName, '(*', '*)', Parser.Token, Parser.LineNumber + 1);
          // Slash comments in CPP files should work if they are not in a {}
          tkSlashesComment: aTodoItem := GetToDoItem(aFileName, '//', '', Parser.Token, Parser.LineNumber + 1);
          tkCompDirect : ParseDirective(Parser.Token);
        end;
        AddListItem(aTodoItem);
        Parser.Next;
      end;
    finally
      Parser.Free;
    end;
  finally
    EStream.Free;
    Self.Update;
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

function TTodoItem.GetAsComment: string;
begin
  Result := '{ '+AsString+' }';
end;

initialization
  {$i todolist.lrs}
  
end.

