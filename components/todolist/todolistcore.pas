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
  TToDoType = (tdToDo, tdFixMe, tdDone, tdNote);
  TTokenStyle = (tsNormal, tsAlternate); // Normal is with hash '#'.

const
  LIST_INDICATORS : array [TToDoType] of string = (lisToDo, lisFixMe, lisDone, lisNote);

type
  { TTodoItem: Class to hold TODO item information }

  TTodoItem = class(TObject)
  private
    FPriority: integer;
    FOwner: string;
    FIssueID: string;
    FCategory: string;
    FTokenStyle: TTokenStyle;
    FToDoType: TToDoType;
    FText: string;
    FHasColon: Boolean;
    FStartPos, FEndPos: TPoint; // Comment Column:Line in file.
    FCommentType: char;         // Character of comment start ['{','/','('].
    FFilename: string;
    function GetQuotedCategory: string;
    function GetQuotedOwner: string;
    function GetAsComment: string;
    function GetAsString: string;
    function QuotedStr(const aSrc: string; const aQuote: char): string;
    procedure FindIssueIDFromText(const aKey: string);
    function Parse(const aTokenString: string; aRequireColon: Boolean): Boolean;
  public
    property Priority: integer read FPriority write FPriority;
    property Owner: string read FOwner write FOwner;
    property IssueID: string read FIssueID write FIssueID;
    property Category: string read FCategory write FCategory;
    property QuotedCategory: string read GetQuotedCategory;
    property QuotedOwner: string read GetQuotedOwner;
    property TokenStyle: TTokenStyle read FTokenStyle write FTokenStyle;
    property ToDoType: TToDoType read FToDoType write FToDoType;
    property HasColon: Boolean read FHasColon write FHasColon;
    property StartPos: TPoint read FStartPos write FStartPos;
    property EndPos: TPoint read FEndPos write FEndPos;
    property CommentType: char read FCommentType write FCommentType;
    property Filename: string read FFilename write FFilename;
    property Text: string read FText write FText;
    property AsString: string read GetAsString;
    property AsComment: string read GetAsComment;
  end;

  { TTLScannedFile }

  TTLScannedFile = class
  private
    FItems: TFPList;          // list of TTodoItem
    FFilename: string;        // Tool.MainFilename
    FCodeChangeStep: integer; // Tool.Scanner.ChangeStep
    FTool: TCodeTool;
    FCode: TCodeBuffer;
    FScannedIncFiles: TStringMap;
    function GetCount: integer;
    function GetItems(Index: integer): TTodoItem;
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
  function FindTokenAndStyle(pComment: PChar;
    out aTodoType: TToDoType; out aTokenStyle: TTokenStyle): string;
  function CreateToDoItem(aCommentStr: string; aStartPos, aEndPos: TPoint): TTodoItem;


implementation

const
  TODO_TOKENS : array [TTokenStyle, TToDoType] of string
    = (('#todo', '#fixme', '#done', '#note'), ('TODO', 'FIXME', 'DONE', 'NOTE'));

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
        s:=s+t+','+IntToStr(lToDoItem.Priority)+
               ','+lToDoItem.Filename+
               ','+IntToStr(lToDoItem.StartPos.Y)+
               ','+lToDoItem.Owner+
               ','+lToDoItem.IssueID+
               ','+lToDoItem.Category;
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

function FindTokenAndStyle(pComment: PChar;
  out aTodoType: TToDoType; out aTokenStyle: TTokenStyle): string;
// Find a ToDo token and style. Returns the token if found.
var
  TheToken: string;
  TodoType: TToDoType;
  TokenStyle: TTokenStyle;
  pe: PChar;
begin
  Result := '';
  for TokenStyle := Low(TTokenStyle) to High(TTokenStyle) do
  begin
    for TodoType := Low(TToDoType) to High(TToDoType) do
    begin
      TheToken := TODO_TOKENS[TokenStyle,TodoType];
      pe := pComment + Length(TheToken);
      if (StrLIComp(PChar(TheToken), pComment, Length(TheToken)) = 0) and
         ( (pe^ in [#0,#9,#10,#13,' ',':','}']) or
          ((pe^='*') and (pe[1]=')'))
         )
      then begin
        aToDoType := TodoType;
        aTokenStyle := TokenStyle;
        exit(TheToken);       // Token match
      end;
    end;
  end;
end;

function CreateToDoItem(aCommentStr: string; aStartPos, aEndPos: TPoint): TTodoItem;
var
  TheToken: string;
  CT: char;  // Character starting the comment
  lStartLen, lEndLen: Integer;
  lTodoType: TToDoType;
  lTokenStyle: TTokenStyle;
begin
  //DebugLn(['CreateToDoItem Start=',aStartPos.X,':',aStartPos.Y,', End=',aEndPos.X,':',aEndPos.Y
  //         ', aCommentStr="',aCommentStr,'"']);
  Result := nil;
  Assert(aCommentStr<>'', 'CreateToDoItem: aCommentStr is empty.');
  CT := aCommentStr[1];
  // Remove beginning and ending comment characters from the string
  case CT of
    '/' : begin lStartLen := 2; lEndLen := 0; end; // //
    '{' : begin lStartLen := 1; lEndLen := 1; end; // { }
    '(' : begin lStartLen := 2; lEndLen := 2; end; // (* *)
    else  begin lStartLen := 0; lEndLen := 0; CT := #0; end; // A dedicated .todo file
  end;
  if lStartLen > 0 then begin
    while (lStartLen < Length(aCommentStr)) and (aCommentStr[lStartLen+1] in [' ',#9]) do
      Inc(lStartLen);
    Delete(aCommentStr, 1, lStartLen);
  end;
  if lEndLen > 0 then begin
    lStartLen := Length(aCommentStr);  // Reuse lStartLen.
    while (lEndLen < lStartLen) and (aCommentStr[lStartLen-lEndLen] in [' ',#9]) do
      Inc(lEndLen);
    SetLength(aCommentStr, lStartLen-lEndLen);
  end;
  TheToken := FindTokenAndStyle(PChar(aCommentStr), lTodoType, lTokenStyle);
  if TheToken = '' then Exit; // Not a Todo item, leave

  // Remove the ToDo token
  Assert(TheToken=TODO_TOKENS[lTokenStyle,lToDoType], 'CreateToDoItem: TheToken');
  lStartLen := Length(TheToken);
  while (lStartLen > Length(aCommentStr)) and (aCommentStr[lStartLen+1] in [' ',#9]) do
    Inc(lStartLen);
  Delete(aCommentStr, 1, lStartLen);  // Remove the token and whitespace.

  // Require a colon with plain "done" but not with "#done". Prevent false positives.
  Result := TTodoItem.Create;
  if Result.Parse(aCommentStr, lTokenStyle=tsAlternate) then
  begin
    Result.ToDoType   := lToDoType;
    Result.TokenStyle := lTokenStyle;
    Result.StartPos   := aStartPos;
    Result.EndPos     := aEndPos;
    Result.CommentType:= CT;
  end
  else
    FreeAndNil(Result);     // Parsing failed, dispose.
end;

{ TTLScannedFile }

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

procedure TTLScannedFile.ScanPascalToDos;
var
  Src, CommentStr, LocationIncTodo: String;
  RealFilename: string;    // Can be an include file inside FFilename.
  pStart, pEnd: Integer;
  NestedComment, B: Boolean;
  StartCaret, EndCaret: TCodeXYPosition;
  StartPos, EndPos: TPoint;
  ToDoItem: TTodoItem;
begin
  Src:=FTool.Src;
  Assert(FCode.Filename=FTool.MainFilename, 'TTLScannedFile.ScanPascalToDos: aCode.Filename<>FTool.MainFilename');
  pStart:=1;
  NestedComment:=CodeToolBoss.GetNestedCommentsFlagForFile(FCode.Filename);
  repeat
    pStart:=FindNextComment(Src,pStart);
    if pStart>length(Src) then  // No more comments found, break loop
      break;

    if not FTool.CleanPosToCaret(pStart,StartCaret) then
    begin
      ShowMessageFmt(errScanFileFailed, [ExtractFileName(FFilename)]);
      Exit;
    end;
    // Study include file names. Use heuristics, assume name ends with ".inc".
    RealFilename:=StartCaret.Code.Filename;
    if FilenameExtIs(RealFilename, 'inc') then // Filename and location in an include file.
      LocationIncTodo:=RealFilename+'_'+IntToStr(StartCaret.Y)
    else
      LocationIncTodo:='';
    // Process a comment
    pEnd:=FindCommentEnd(Src,pStart,NestedComment);
    B := FTool.CleanPosToCaret(pEnd,EndCaret);
    CommentStr:=copy(Src,pStart,pEnd-pStart);
    //Assert(B, 'TTLScannedFile.ScanPascalToDos: No comment end. "'+CommentStr+'"');
    // Process each include file location only once. Units are processed always.
    if (LocationIncTodo='') or not FScannedIncFiles.Contains(LocationIncTodo) then
    begin
      StartPos.X:=StartCaret.X;
      StartPos.Y:=StartCaret.Y;
      EndPos.X:=EndCaret.X;
      EndPos.Y:=EndCaret.Y;
      ToDoItem := CreateToDoItem(CommentStr, StartPos, EndPos);
      if Assigned(ToDoItem) then begin
        ToDoItem.Filename := RealFilename;
        Add(ToDoItem);            // Add to list.
      end;
      //AddToDoItem(CommentStr, StartPos, EndPos);
      if LocationIncTodo<>'' then    // Store include file location for future.
        FScannedIncFiles.Add(LocationIncTodo);
    end;
    pStart:=pEnd;
  until false;
end;

procedure TTLScannedFile.ScanToDoFile;
var
  List: TStringList;
  SPos, EPos: TPoint;    // Start and End.
  Line: String;
  OldItem, NewItem: TTodoItem;
  i: Integer;
begin
  OldItem := nil;
  List:=TStringList.Create;
  try
    List.Text:=FCode.Source;
    for i:=0 to List.Count-1 do
    begin
      Line:=List[i];
      if Line <> '' then begin
        SPos.X := 1;
        SPos.Y := i+1;
        EPos.X := Length(Line)+1;
        EPos.Y := SPos.Y;
        // Try creating a ToDo item. It fails if the signature is wrong.
        NewItem := CreateToDoItem(Line, SPos, EPos);
        if Assigned(NewItem) then begin
          NewItem.Filename := FCode.Filename;
          //NewItem.CommentType := #0;
          //NewItem.HasColon := True;
          if Assigned(OldItem) then
            Add(OldItem);             // Add previously created item to list.
          OldItem := NewItem;
        end
        else if Assigned(OldItem) then begin
          // Extend the text of a previous item with Line contents.
          OldItem.Text := OldItem.Text + LineEnding + Line;
          // Update EPos
          EPos.X := OldItem.EndPos.X + Length(Line)+1;
          EPos.Y := SPos.Y;
          OldItem.EndPos := EPos;
        end;
      end;
    end;
    if Assigned(OldItem) then
      Add(OldItem);             // Add previously created item to list.
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
  // Issue ID
  if IssueID <> '' then
    Result := Result + ' -#'+IssueID;
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
  case CommentType of
    #0  : Result := AsString;               // A dedicated .todo file
    '/' : Result := '// '+AsString;
    '{' : Result := '{ ' +AsString+' }';
    '(' : Result := '(* '+AsString+' *)';
    else raise Exception.Create('TTodoItem.GetAsComment: Wrong comment char "'+CommentType+'".');
  end;
end;

procedure TTodoItem.FindIssueIDFromText(const aKey: string);
// Dig out Issue IDs embedded in the Text. aKey is typically 'issue' or 'bug'.
// Syntax "aKey #12345" and "aKey 12345" are supported.
var
  I, EndI, Len: Integer;
begin
  if (Text = '') or (IssueID <> '') then exit;
  I := PosI(aKey, Text);
  if (I > 0) and (I < Length(Text)-1) and (Text[I+Length(aKey)] in [' ','=']) then
  begin
    Inc(I, Length(aKey)+1);
    if Text[I] = '#' then
      Inc(I);
    EndI := I;
    while (EndI <= Length(Text)) and (Text[EndI] in ['0'..'9']) do
      Inc(EndI);
    Len := EndI-I;
    if (Len > 0) and ((EndI > Length(Text)) or (Text[EndI] in [#9,' '])) then
      IssueID := Copy(Text, I, Len);
  end;
end;

type
  TParseState =
    (psHunting, psGotDash, psPriority, psText, psAllDone,
     psOwnerStart, psOwnerContinue, { NOTE: Continue state must follow Start state }
     psIssueStart, psIssueContinue,
     psCategoryStart, psCategoryContinue
    );

function TTodoItem.Parse(const aTokenString: string; aRequireColon: Boolean): Boolean;
// Parse a string like
//  "10 -o'Me Myself' -cMyOwnCat : Text for the item goes here."
// Returns False if the format is invalid, like a colon is missing.
var
  lParseState: TParseState;
  i, lPriorityStart: Integer;
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
        case aTokenString[i] of
          'o','O':
            begin
              lParseState:=psOwnerStart;
              Inc(i);
            end;
          '#':
            begin
              lParseState:=psIssueStart;
              Inc(i);
            end;
          'c','C':
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

      psOwnerStart, psIssueStart, psCategoryStart:
        case aTokenString[i] of
          '''':// Got a quote so extract
            begin
              lTempStr := Copy(aTokenString, i, MaxInt);
              lpTemp := PChar(lTempStr);
              lStr := AnsiExtractQuotedStr(lpTemp, '''');
              if lParseState = psOwnerStart then
                Owner := lStr
              else if lParseState = psIssueStart then
                IssueID := lStr
              else
                Category := lStr;
              i := i + Length(lTempStr) - Length(lpTemp);
              lParseState := psHunting;
            end;
          else
            begin
              lTempStr := aTokenString[i];
              Inc(i);
              //Assert(Succ(psOwnerStart) = psOwnerContinue, 'Succ(psOwnerStart) is not psOwnerContinue.');
              //Assert(Succ(psCategoryStart) = psCategoryContinue, 'Succ(psCategoryStart) is not psCategoryContinue.');
              inc(lParseState); // Assumes Continue is succ to Start
            end;
        end;

      psOwnerContinue, psIssueContinue, psCategoryContinue:
        if aTokenString[i] in [#9,' ',':'] then
        begin
          if lParseState = psOwnerContinue then
            Owner := lTempStr
          else if lParseState = psIssueContinue then
            IssueID := lTempStr
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

  // Issue IDs may be embedded in Text.
  FindIssueIDFromText('issue');
  FindIssueIDFromText('bug');
  Result := True;
end;

end.

