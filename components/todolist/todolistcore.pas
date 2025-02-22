{
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
}
(*
Modified by Kevin Jesshope <KevinOfOz@gmail.com> 15 Mar 2020
- Owner and Category now support basic quoted values
  -c'Lazarus ToDoList'
- Select Token style from dialog. #todo is normal (unchecked) todo is alernate (checked)
- Save Owner, Category and normal/alt selection to XMLPropStorage
- Move (some) non-presentation code to ToDoListCore
- Add Note type to ToDo and Done types

By Juha Manninen Feb. 2025
  Require a colon with "done" but not with "#done".
  Plain "done" or "note" would cause false positives. Issue #41437.
*)

unit ToDoListCore;

{$mode objfpc}{$H+}

interface

uses
  // FCL, RTL
  Classes, SysUtils, StrUtils, AVL_Tree,
  // LCL
  LCLType, LclIntf, Controls, Dialogs, ComCtrls,
  // LazUtils
  LazFileUtils, LazStringUtils, LazFileCache, LazLoggerBase, AvgLvlTree,
  // Codetools
  CodeToolManager, FileProcs, CodeCache, BasicCodeTools,
  // IDEIntf
  PackageIntf, ProjectIntf,
  // ToDoList
  ToDoListStrConsts;

type
  TToDoType = (tdToDo, tdDone, tdNote);
  TTokenStyle = (tsNormal, tsAlternate);

const
  LIST_INDICATORS : array [TToDoType] of string = ('ToDo', 'Done', 'Note');

type
  { TTodoItem: Class to hold TODO item information }

  TTodoItem = class(TObject)
  private
    FCategory: string;
    FToDoType: TToDoType;
    FTokenStyle: TTokenStyle;
    FFilename: string;
    FLineNumber: integer;
    FOwner: string;
    FPriority: integer;
    FText: string;
    function GetQuotedCategory: string;
    function GetQuotedOwner: string;
    function GetAsComment: string;
    function GetAsString: string;
    function QuotedStr(const aSrc: string; const aQuote: char): string;
    function Parse(const aTokenString: string; aRequireColon: Boolean): Boolean;
  public
    property Category: string read FCategory write FCategory;
    property QuotedCategory:string read GetQuotedCategory;
    property TokenStyle: TTokenStyle read FTokenStyle write FTokenStyle;
    property ToDoType:TToDoType read FToDoType write FToDoType;
    property LineNumber: integer read FLineNumber write FLineNumber;
    property Filename: string read FFilename write FFilename;
    property Owner: string read FOwner write FOwner;
    property QuotedOwner:string read GetQuotedOwner;
    property Priority: integer read FPriority write FPriority;
    property Text: string read FText write FText;
    property AsString: string read GetAsString;
    property AsComment: string read GetAsComment;
  end;

  { TTLScannedFile }

  TTLScannedFile = class
  private
    FItems: TFPList;          // list of TTodoItem
    FFilename: string;        // Tool.MainFilename
    FRealFilename: string;    // Can be an include file inside FFilename.
    FCommentStr: string;      // The comment where a ToDo is extracted.
    FCodeChangeStep: integer; // Tool.Scanner.ChangeStep
    FTool: TCodeTool;
    FCode: TCodeBuffer;
    FScannedIncFiles: TStringMap;
    function GetCount: integer;
    function GetItems(Index: integer): TTodoItem;
    procedure CreateToDoItem(const aStartComment, aEndComment: string;
      aLineNumber: Integer);
    procedure ScanPascalToDos;
    procedure ScanToDoFile;
  public
    constructor Create(const aFilename: string; aTool: TCodeTool; aCode: TCodeBuffer;
      aScannedIncFiles: TStringMap);
    destructor Destroy; override;
    procedure Clear;
    procedure Add(aItem: TTodoItem);
    property Count: integer read GetCount;
    property Items[Index: integer]: TTodoItem read GetItems; default;
  end;

  function CompareTLScannedFiles(Data1, Data2: Pointer): integer;
  procedure ExtractToCSV(const aFilename: string; aListItems: TListItems);
  procedure ScanFile(const aFileName: string;
    aScannedFiles: TAvlTree; aScannedIncFiles: TStringMap);


implementation

const
  TODO_TOKENS : array [TTokenStyle, TToDoType] of string
      = (('#todo', '#done', '#note'), ('TODO', 'DONE', 'NOTE'));

function CompareTLScannedFiles(Data1, Data2: Pointer): integer;
begin
  Result:=CompareFilenames(TTLScannedFile(Data1).FFilename,
                           TTLScannedFile(Data2).FFilename);
end;

function CompareAnsiStringWithTLScannedFile(Filename, ScannedFile: Pointer): integer;
begin
  Result:=CompareFilenames(AnsiString(Filename),
                           TTLScannedFile(ScannedFile).FFilename);
end;

procedure ExtractToCSV(const aFilename: string; aListItems: TListItems);
var
  lCommaList: TStringList;
  i: Integer;
  lToDoItem: TTodoItem;
  s, t: String;
begin
  lCommaList:=TStringList.Create;
  try
    lCommaList.Add(csvHeader);
    i:=0;
    while i<aListItems.Count do
      begin
        lToDoItem:=TTodoItem(aListItems[i].Data);
        s:=LIST_INDICATORS[lToDoItem.ToDoType] + ',';
        t:=DelChars(lToDoItem.Text,',');{Strip any commas that can cause a faulty csv file}
        s:=s+t+','+IntToStr(lToDoItem.Priority)+','+lToDoItem.Filename+
           ','+IntToStr(lToDoItem.LineNumber)+','+lToDoItem.Owner+','+lToDoItem.Category;
        lCommaList.Add(s);
        Inc(i);
      end;
    lCommaList.SaveToFile(aFileName);
  finally
    lCommaList.Clear;
    lCommaList.Free;
  end;
end;

procedure ScanFile(const aFileName: string;
  aScannedFiles: TAvlTree; aScannedIncFiles: TStringMap);
var
  FN: String;
  AVLNode: TAvlTreeNode;
  Tool: TCodeTool;
  Code: TCodeBuffer;
  CurFile: TTLScannedFile;
begin
  //DebugLn(['ScanFile ',aFileName]);
  FN:=TrimFilename(aFileName);
  Code:=CodeToolBoss.LoadFile(FN,true,false);
  if Code=nil then begin
    DebugLn(['ScanFile failed loading ',FN]);
    exit;
  end;
  Assert(aFilename=Code.Filename, 'ScanFile: aFileName <> Code.Filename');
  CodeToolBoss.Explore(Code,Tool,false,false); // Parse Pascal code, ignore Result
  AVLNode:=aScannedFiles.FindKey(Pointer(aFilename),
                                @CompareAnsiStringWithTLScannedFile);
  //DebugLn(['ScanFile ',aFilename,' AVLNode=',AVLNode<>nil]);
  if AVLNode<>nil then begin
    CurFile:=TTLScannedFile(AVLNode.Data);
    Assert(Assigned(CurFile), 'ScanFile: CurFile=Nil');
    // Abort if this file has already been scanned and has not changed
    if Assigned(Tool) and (CurFile.FCodeChangeStep=Tool.Scanner.ChangeStep) then
      exit;
    CurFile.Clear;       // clear old items
  end
  else begin
    // Add file name to list of scanned files
    CurFile:=TTLScannedFile.Create(aFilename, Tool, Code, aScannedIncFiles);
    aScannedFiles.Add(CurFile);
  end;
  if (Tool=nil) or (Tool.Scanner=nil) then begin
    // Not Pascal. Assume .todo textual file.
    CurFile.ScanToDoFile;
  end
  else begin
    Assert(aFileName=Tool.MainFilename, 'ScanFile: aFileName <> Tool.MainFilename');
    // save ChangeStep
    CurFile.FCodeChangeStep:=Tool.Scanner.ChangeStep;
    //DebugLn(['ScanFile saved ChangeStep ',CurFile.FCodeChangeStep,' ',Tool.Scanner.ChangeStep]);
    CurFile.ScanPascalToDos;
  end;
end;

{ TTLScannedFile }

function TTLScannedFile.GetCount: integer;
begin
  if Assigned(FItems) then
    Result:=FItems.Count
  else
    Result:=0
end;

function TTLScannedFile.GetItems(Index: integer): TTodoItem;
begin
  Result:=TTodoItem(FItems[Index]);
end;

procedure TTLScannedFile.CreateToDoItem(const aStartComment, aEndComment: string;
  aLineNumber: Integer);
var
  lParsingString, TheToken: string;
  lTokenFound: boolean;
  lTodoType, lFoundToDoType: TToDoType;
  lTokenStyle, lFoundTokenStyle: TTokenStyle;
  NewToDoItem: TTodoItem;
begin
  //DebugLn(['TTLScannedFile.CreateToDoItem FileName=',FRealFilename,' LineNumber=',aLineNumber]);
  lParsingString := TextToSingleLine(FCommentStr);
  // Remove the beginning comment chars from input string
  if aStartComment <> '' then
    Delete(lParsingString, 1, Length(aStartComment));
  // Remove leading and trailing blanks from input
  lParsingString := Trim(lParsingString);

  // Determine Token and Style
  lTokenFound := False;
  for lTokenStyle := Low(TTokenStyle) to High(TTokenStyle) do
  begin
    if lTokenFound then Break;
    for lTodoType := Low(TToDoType) to High(TToDoType) do
    begin
      TheToken := TODO_TOKENS[lTokenStyle,lTodoType];
      if LazStartsText(TheToken, lParsingString) then
      begin
        if (Length(lParsingString)=Length(TheToken)) // Don't match with 'ToDoX'
        or (lParsingString[Length(TheToken)+1] in [#9,' ',':']) then
        begin
          lTokenFound := True;       // Token match
          lFoundToDoType := lTodoType;
          lFoundTokenStyle := lTokenStyle;
          Break;
        end;
      end;
    end;
  end;

  if Not lTokenFound then
    Exit; // Not a Todo/Done item, leave

  // Remove the ending comment chars from input string
  if (aEndComment <> '') and LazEndsStr(aEndComment, lParsingString) then
    SetLength(lParsingString, Length(lParsingString)-Length(aEndComment));

  // Remove the ToDo token
  Assert(TheToken=TODO_TOKENS[lFoundTokenStyle,lFoundToDoType], 'TTLScannedFile.CreateToDoItem: TheToken');
  Delete(lParsingString, 1, Length(TheToken));
  lParsingString := Trim(lParsingString);

  // Require a colon with plain "done" but not with "#done". Prevent false positives.
  NewToDoItem:=TTodoItem.Create;
  if NewToDoItem.Parse(lParsingString, lFoundTokenStyle=tsAlternate) then
  begin
    NewToDoItem.ToDoType   := lFoundToDoType;
    NewToDoItem.TokenStyle := lFoundTokenStyle;
    NewToDoItem.LineNumber := aLineNumber;
    NewToDoItem.Filename   := FRealFilename;
    Add(NewToDoItem);            // Add to list.
  end
  else
    NewToDoItem.Free;            // Parsing failed, dispose.
end;

constructor TTLScannedFile.Create(const aFilename: string; aTool: TCodeTool;
  aCode: TCodeBuffer; aScannedIncFiles: TStringMap);
begin
  inherited Create;
  FFilename:=aFilename;
  FTool:=aTool;
  FCode:=aCode;
  FScannedIncFiles:=aScannedIncFiles;
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
  if FItems=Nil then Exit;
  for i:=0 to FItems.Count-1 do
    TObject(FItems[i]).Free;
  FreeAndNil(FItems);
end;

procedure TTLScannedFile.Add(aItem: TTodoItem);
begin
  if not Assigned(FItems) then
    FItems:=TFPList.Create;
  FItems.Add(aItem);
end;

procedure TTLScannedFile.ScanPascalToDos;
var
  Src, LocationIncTodo: String;
  p, CommentEnd: Integer;
  NestedComment: Boolean;
  CodePos: TCodeXYPosition;
begin
  Src:=FTool.Src;
  Assert(FCode.Filename=FTool.MainFilename, 'TTLScannedFile.ScanPascalToDos: aCode.Filename<>FTool.MainFilename');
  p:=1;
  NestedComment:=CodeToolBoss.GetNestedCommentsFlagForFile(FCode.Filename);
  repeat
    p:=FindNextComment(Src,p);
    if p>length(Src) then  // No more comments found, break loop
      break;
    if not FTool.CleanPosToCaret(p,CodePos) then
    begin
      ShowMessageFmt(errScanFileFailed, [ExtractFileName(FFilename)]);
      Exit;
    end;
    // Study include file names. Use heuristics, assume name ends with ".inc".
    FRealFilename:=CodePos.Code.Filename;
    if FilenameExtIs(FRealFilename, 'inc') then // Filename and location in an include file.
      LocationIncTodo:=FRealFilename+'_'+IntToStr(CodePos.Y)
    else
      LocationIncTodo:='';
    // Process a comment
    CommentEnd:=FindCommentEnd(Src,p,NestedComment);
    FCommentStr:=copy(Src,p,CommentEnd-p);
    // Process each include file location only once. Units are processed always.
    if (LocationIncTodo='') or not FScannedIncFiles.Contains(LocationIncTodo) then
    begin
      if Src[p]='/' then
        CreateToDoItem('//', '', CodePos.Y)
      else if Src[p]='{' then
        CreateToDoItem('{', '}', CodePos.Y)
      else if Src[p]='(' then
        CreateToDoItem('(*', '*)', CodePos.Y);
      if LocationIncTodo<>'' then    // Store include file location for future.
        FScannedIncFiles.Add(LocationIncTodo);
    end;
    p:=CommentEnd;
  until false;
end;

procedure TTLScannedFile.ScanToDoFile;
var
  List: TStringList;
  i: Integer;
begin
  List:=TStringList.Create;
  try
    List.Text:=FCode.Source;
    for i:=0 to List.Count-1 do
    begin
      FRealFilename:=FCode.Filename;
      FCommentStr:=List[i];
      CreateToDoItem('', '', i+1)
    end;
  finally
    List.Free;
  end;
end;

{ TTodoItem }

function TTodoItem.GetAsString: string;
begin
  Result := TODO_TOKENS[TokenStyle, ToDoType];
  // Priority
  if Priority > 0 then
    Result := Result + ' '+IntToStr(Priority);
  // Owner
  if Owner <> '' then
    Result := Result + ' -o'+QuotedOwner;
  // Category
  if Category <> '' then
    Result := Result + ' -c'+QuotedCategory;
  // Text
  Result := Result + ' : ' + Text;
end;

function TTodoItem.QuotedStr(const aSrc: string; const aQuote: char): string;
begin
  // Only quote if necessary
  if (pos(aQuote, aSrc)<>0) or (pos(' ', aSrc)<>0) then
    Result := AnsiQuotedStr(aSrc, aQuote)
  else
    Result := aSrc;
end;

function TTodoItem.GetQuotedOwner: string;
begin
  Result := QuotedStr(FOwner, '''');
end;

function TTodoItem.GetQuotedCategory: string;
begin
  Result := QuotedStr(FCategory, '''');
end;

function TTodoItem.GetAsComment: string;
begin
  Result := '{ '+AsString+' }';
end;

type
  TParseState =
    (psHunting, psGotDash, psPriority, psText, psAllDone,
     psOwnerStart, psOwnerContinue, { NOTE: Continue state must follow Start state }
     psCategoryStart, psCategoryContinue
    );

function TTodoItem.Parse(const aTokenString: string; aRequireColon: Boolean): Boolean;
// Parse a string like
//  "10 -o'Me Myself' -cMyOwnCat : Text for the item goes here."
// Returns False if the format is invalid, like a colon is missing.
var
  lParseState: TParseState;
  i, lPriorityStart: Integer;
  HasColon: Boolean;
  lTempStr, lStr: string;
  lpTemp: PChar;
begin
  lParseState := psHunting;
  HasColon := False;
  i := 1;
  while i <= Length(aTokenString) do
    case lParseState of
      psHunting:
        case aTokenString[i] of
          ' ': Inc(i);// look at the next character
          '-':
            begin
              lParseState:=psGotDash;
              Inc(i);
            end;
          '0'..'9':
            begin
              lParseState:=psPriority;
              lPriorityStart := i;
              Inc(i);
            end;
          ':':
            begin
              HasColon := True;
              lParseState:=psText;
              Inc(i);
            end;
          else // Not a special character so it must be the text
            if aRequireColon and not HasColon then
              Exit(False);
            lParseState := psText;
        end;

      psText:
        begin
          Text := Trim(Copy(aTokenString, i, MaxInt));
          lParseState := psAllDone;
        end;

      psGotDash:
        case LowerCase(aTokenString[i]) of
          'o':
            begin
              lParseState:=psOwnerStart;
              Inc(i);
            end;
          'c':
            begin
              lParseState:=psCategoryStart;
              Inc(i);
            end
          else // invalid so assume rest is text
            begin
              if aRequireColon and not HasColon then
                Exit(False);
              lParseState := psText;
              Dec(i); // wind back 1 character so we catch the - in the text
            end;
        end;

      psPriority:
        if aTokenString[i] in ['0'..'9'] then
          Inc(i)
        else begin
          Priority := StrToInt(Copy(aTokenString, lPriorityStart, i-lPriorityStart));
          lParseState := psHunting;
        end;

      psOwnerStart, psCategoryStart:
        case aTokenString[i] of
          '''':// Got a quote so extract
            begin
              lTempStr := Copy(aTokenString, i, MaxInt);
              lpTemp := PChar(lTempStr);
              lStr := AnsiExtractQuotedStr(lpTemp, '''');
              if lParseState = psOwnerStart then
                Owner := lStr
              else
                Category := lStr;
              i := i + Length(lTempStr) - Length(lpTemp);
              lParseState := psHunting;
            end;
          else
            begin
              lTempStr := aTokenString[i];
              Inc(i);
              Assert(Succ(psOwnerStart) = psOwnerContinue, 'Succ(psOwnerStart) is not psOwnerContinue.');
              Assert(Succ(psCategoryStart) = psCategoryContinue, 'Succ(psCategoryStart) is not psCategoryContinue.');
              inc(lParseState); // Assumes Continue is succ to Start
            end;
        end;

      psOwnerContinue,psCategoryContinue:
        if aTokenString[i] in [#9,' ',':'] then
        begin
          if lParseState = psOwnerContinue then
            Owner := lTempStr
          else
            Category := lTempStr;
          lParseState:=psHunting;
        end
        else begin
          lTempStr:=lTempStr + aTokenString[i];
          Inc(i);
        end;

      psAllDone:
        break;

    end;
  Result := True;
end;

end.

