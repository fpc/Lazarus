(*
/***************************************************************************
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

{#todo goto line of selected unit}
{#todo options }
{#todo print an todo report }

unit TodoList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, LResources,
  ExtCtrls, ComCtrls, Menus, Buttons, GraphType,
  StdCtrls, mPasLex, LCLIntf, LCLType,
  CodeCache, CodeToolManager, LazarusIDEStrConsts;

Const
  cTodoFlag = '#todo';
  cAltTodoFLag = 'TODO';

type
  TOnOpenFile = procedure(Sender: TObject; const Filename: string;
                          const LineNumber: integer) of object;

  { TfrmTodo }

  TfrmTodo = class(TForm)
    ImageList1: TImageList;
    lvTodo: TListView;
    StatusBar: TStatusBar;
    ToolBar: TToolBar;
    tbGoto: TToolButton;
    tbOptions: TToolButton;
    tbPrint: TToolButton;
    tbRefresh: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure tbGotoClick(Sender: TObject);
    procedure tbRefreshClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift:TShiftState);
  private
    { private declarations }
    fBuild       : Boolean;
    fFileName    : String;
    FOnOpenFile  : TOnOpenFile;
    fRootCBuffer : TCodeBuffer;
    fScannedFile : TStringList;

    procedure SetFileName(const AValue: String);

    procedure ParseComment(const aFileName: string; const SComment, EComment: string;
        const TokenString: string; LineNumber: Integer);
    procedure ParseDirective(aDirective : String);
    
    procedure LoadFile(const aFileName : string);
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

uses
  StrUtils,
  FileUtil;

{ TfrmTodo }

constructor TfrmTodo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TfrmTodo.Destroy;
begin
  fScannedFile.Free;

  inherited Destroy;
end;

//Load project main file and scan all files for find the syntax todo
procedure TfrmTodo.tbRefreshClick(Sender: TObject);
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

procedure TfrmTodo.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key=VK_ESCAPE then
    Close;
end;

//Initialise then todo project and find them
procedure TfrmTodo.SetFileName(const AValue: String);
begin
  if fFileName=AValue then exit;
  fFileName:=AValue;
  tbRefreshClick(nil);
end;

procedure TfrmTodo.tbGotoClick(Sender: TObject);
var
  CurFilename: String;
  TheItem: TListItem;
  TheLine: integer;
  UsedInterfaceFilenames: TStrings;
  UsedImplementationFilenames: TStrings;
  i: integer;
  Found: boolean;
begin
  CurFilename:='';
  TheItem:= lvtodo.Selected;
  Found:= false;
  if Assigned(TheItem) then
  begin
    CurFileName:= TheItem.SubItems[1];
    TheLine:= StrToInt(TheItem.SubItems[2]);
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
              if AnsiCompareStr(ExtractFileName(UsedInterfaceFileNames[i]),
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
                if AnsiCompareStr(ExtractFileName
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

procedure TfrmTodo.FormCreate(Sender: TObject);
begin
  fBuild:=False;
  fScannedFile:=TStringList.Create;

  Caption := lisTodoListCaption;

  tbRefresh.Hint  := lisTodolistRefresh;
  tbGoto.Hint  := listodoListGotoLine;
  tbPrint.Hint   :=listodoListPrintList;
  tbOptions.Hint  :=  lisToDoListOptions;

  tbOptions.Caption:=dlgFROpts;
  tbPrint.Caption:=srVK_PRINT;
  tbRefresh.Caption:=dlgUnitDepRefresh;
  tbGoto.Caption:=lisToDoGoto;

  with lvTodo do
  begin
    Column[0].Caption := ' !';
    Column[0].Width   := 25;
    Column[1].Caption := lisToDoLDescription;
    Column[1].Width   := 250;
    Column[2].Caption := lisToDoLFile;
    Column[2].Width := 150;
    Column[3].Caption := lisToDoLLine;
    Column[3].Width := 50;
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

//Find in comment the ToDo message
procedure TfrmTodo.ParseComment(const aFileName: string;
  const SComment, EComment: string;
  const TokenString: string; LineNumber: Integer);
Var
  N,J           : Integer;
  ParsingString : string;
  CListItem     : TListItem;
  TodoFlag      : string;
  
  function IsTodoFlag(const Flag: string): boolean;
  begin
    TodoFLag := Flag;
    Result := Pos(UpperCase(Flag),UpperCase(TokenString)) > 1;
  end;
  
begin
  if IsTodoFlag(cTodoFlag) or IsTodoFlag(cAltTodoFlag) then
  begin
    // We found a token that looks like a TODO comment. Now
    // verify that it *is* one: either a white-space or the
    // comment token need to be in front of the TODO item

    // Remove comment characters
    ParsingString := TokenString;
    Delete(ParsingString, 1, Length(SComment));

    // Remove white-space left and right
    ParsingString := Trim(ParsingString);

    // The TODO token should be at the beginning of the comment
    N:=Pos(UpperCase(TodoFlag),UpperCase(ParsingString));
    if N=1 then
    begin
      // Remove token from string
      Delete(ParsingString, 1, Length(TodoFlag));
      ParsingString := TrimRight(ParsingString);

      if EComment<>'' then
      begin
        N:=Pos(EComment,ParsingString);
        // Remove end comment from string
        Delete(ParsingString, N, Length(EComment));
        ParsingString := TrimRight(ParsingString);
      end;

      CListItem := lvToDo.Items.Add;

      // Identify numeric priority (if there is one)
      j := 0;
      while j < Length(ParsingString) do
      begin
        if not (ParsingString[j + 1] in ['0'..'9']) then
          Break;
        Inc(j);
      end;
      N:=StrToIntDef(Copy(ParsingString, 1, j), 0);
      ClistItem.Caption:=IntToStr(N);
      Delete(ParsingString, 1, j);
      ParsingString := TrimLeft(ParsingString);
      if (j=0) and AnsiStartsText(':', ParsingString) then begin
        { Remove Leading : from comment }
        Delete(ParsingString, 1, 1);
        ParsingString := TrimLeft(ParsingString);
      end;

      ClistItem.SubItems.Add(ParsingString);
      ClistItem.SubItems.Add(ExtractFileName(aFileName));
      ClistItem.SubItems.Add(IntToStr(LineNumber));
     // CListItem.ImageIndex := Ord(Info.Priority);
    end;
  end;
end;

//Load an FileName and find {#todox yyyyyy} where
// x is the priority (0 by default)
// yyyy it's the message one line only
procedure TfrmTodo.LoadFile(const aFileName: string);
var
  Parser   : TmwPasLex;
  EStream  : TMemoryStream;
  ST       : String;
begin
  St:=ExtractFileName(aFileName);
  //Quit this method if we have already scan this file
  if fScannedFile.IndexOf(St)<>-1 then Exit;
  StatusBar.SimpleText :=aFileName;// St;
  StatusBar.Repaint;
  fScannedFile.Add(St);

  EStream := TMemoryStream.Create;
  try
    //Echange of stream
    Try
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
        case Parser.TokenID of
          tkBorComment: ParseComment(aFileName, '{', '}', Parser.Token, Parser.LineNumber + 1);
          tkAnsiComment: ParseComment(aFileName, '(*', '*)', Parser.Token, Parser.LineNumber + 1);
          // Slash comments in CPP files should work if they are not in a {}
          tkSlashesComment: ParseComment(aFileName, '//', '', Parser.Token, Parser.LineNumber + 1);
          tkCompDirect : ParseDirective(Parser.Token);
        end;
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

initialization
  {$i todolist.lrs}
  
end.

