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
*)

unit ToDoListCore;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, Laz_AVL_Tree, ToDoListStrConsts;

type
  TToDoType = (tdToDo, tdDone, tdNote);
  TTokenStyle = (tsNormal, tsAlternate);

const
  LIST_INDICATORS : array [TToDoType] of string
      = ('ToDo', 'Done', 'Note');

  // Value names of the various parts of the todo entry
  OWNER_PART_NAME = 'Owner';
  CATEGORY_PART_NAME = 'Category';
  PRIORITY_PART_NAME = 'Priority';
  TEXT_PART_NAME = 'Text';


type
  TTLScannedFile = class;

  { TTodoItem: Class to hold TODO item information }

  TTodoItem = class(TObject)
  private
    FCategory: string;
    FToDoType:TToDoType;
    FTokenStyle:TTokenStyle;
    FFilename: string;
    FLineNumber: integer;
    FOwner: string;
    FPriority: integer;
    FText: string;
    FTLFile: TTLScannedFile;
    function GetQuotedCategory: string;
    function GetQuotedOwner: string;
    function GetAsComment: string;
    function GetAsString: string;
    function QuotedStr(const aSrc: string; const aQuote: char): string;
  public
    constructor Create(aTLFile: TTLScannedFile);
    property TLFile: TTLScannedFile read FTLFile;
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

  { TTLScannedFiles }

  TTLScannedFile = class
    FItems: TFPList;// list of TTodoItem
  private
    function GetCount: integer;
    function GetItems(Index: integer): TTodoItem;
  public
    Filename: string; // = Tool.MainFilename
    CodeChangeStep: integer; // = Tool.Scanner.ChangeStep
    destructor Destroy; override;
    procedure Clear;
    procedure Add(aItem: TTodoItem);
    property Count: integer read GetCount;
    property Items[Index: integer]: TTodoItem read GetItems; default;
  end;

  { TToDoListCore }

  (* implemented as a class of class procedures so the protected methods can be
     exposed via a class helper for unit testing purposes *)

  TToDoListCore = class(TObject)
  protected
    class procedure ParseToParts(const aTokenString:string;const aParts:TStrings);
    class procedure AddToDoItemFromParts(const aParts: TStrings;
      const aTLFile: TTLScannedFile; const aFileName: string;
      const aLineNumber: integer; const aToDoType: TToDoType;
      const aTokenStyle: TTokenStyle);
  public
    class procedure CreateToDoItem(aTLFile: TTLScannedFile;
        const aFileName: string; const aStartComment, aEndComment: string;
        const aTokenString: string; aLineNumber: Integer);
    class procedure ExtractToCSV(const aListItems: TListItems; const aFilename: string);
    class procedure ScanFile(const aFileName: string; const FScannedFiles: TAvlTree
      );
  end;

implementation

uses
  Dialogs, Controls,   // FCL, RTL
  StrUtils, LazUTF8Classes,
  // LCL
  LCLType, LclIntf, Forms, ActnList, ExtCtrls,
  // LazUtils
  LazFileUtils, LazStringUtils, LazFileCache, LazLoggerBase, LazTracer,
  // Codetools
  CodeToolManager, FileProcs, CodeCache, BasicCodeTools,
  // IDEIntf
  LazIDEIntf, IDEImagesIntf, PackageIntf, ProjectIntf;

const
  TODO_TOKENS : array [TTokenStyle, TToDoType] of string
      = (('#todo', '#done', '#note'), ('TODO', 'DONE', 'NOTE'));

function CompareAnsiStringWithTLScannedFile(Filename, ScannedFile: Pointer): integer;
begin
  Result:=CompareFilenames(AnsiString(Filename),
                           TTLScannedFile(ScannedFile).Filename);
end;

{ TToDoListCore }

type
  TParseState = (psHunting, psGotDash, psOwnerStart, psOwnerContinue, psCategoryStart,
  psCategoryContinue, psPriority, psText, psAllDone); { NOTE : Continue state must follow Start state }

class procedure TToDoListCore.ParseToParts(const aTokenString: string;
  const aParts: TStrings);

  function ParseStateToText(const aParseState:TParseState):string;
  begin
    case aParseState of
      psOwnerStart, psOwnerContinue:Result := OWNER_PART_NAME;
      psCategoryStart,psCategoryContinue:Result := CATEGORY_PART_NAME;
      psPriority:Result := PRIORITY_PART_NAME;
      psText:Result := TEXT_PART_NAME;
      else
        raise Exception.Create(excInvalidParseState);
    end;
  end;

var
  lParseState:TParseState;
  i, lPriorityStart, lBytesRemoved: Integer;
  lTempStr, lStr: string;
  lpTemp: PChar;
begin
  lParseState :=psHunting;
  i := 1;
  while i <= Length(aTokenString) do
    begin
      case lParseState of

        psHunting:
          begin
            case aTokenString[i] of
              ' ':Inc(i);// look at the next character
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
                  lParseState:=psText;
                  Inc(i);
                end;
              else // Not a special character so it must be the text
                lParseState := psText;
            end;
          end;

        psText:
          begin
            aParts.Values[ParseStateToText(lParseState)]:= Trim(Copy(aTokenString, i, MaxInt));
            lParseState := psAllDone;
          end;

        psGotDash:
          begin
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
                  lParseState := psText;
                  Dec(i); // wind back 1 character so we catch the - in the text
                end;
            end;
          end;

        psPriority:
          if (aTokenString[i] < '0') or (aTokenString[i] > '9') then
            begin
              aParts.Values[ParseStateToText(lParseState)] := Copy(aTokenString, lPriorityStart, i-lPriorityStart);
              lParseState := psHunting;
            end
          else
            Inc(i);

        psOwnerStart, psCategoryStart:
          begin
            case aTokenString[i] of
              '''':// Got a quote so extract
                begin
                  lTempStr := Copy(aTokenString, i, MaxInt);
                  lpTemp := PChar(lTempStr);
                  lStr := AnsiExtractQuotedStr(lpTemp, '''');
                  aParts.Values[ParseStateToText(lParseState)] := lStr;
                  lBytesRemoved := Length(lTempStr) - Length(lpTemp);
                  i := i + lBytesRemoved;
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
          end;

        psOwnerContinue,psCategoryContinue:
          begin
            if (aTokenString[i] = ' ') or (aTokenString[i] = ':') then
              begin
                aParts.Values[ParseStateToText(lParseState)] := lTempStr;
                lParseState:=psHunting;
              end
            else
              begin
                lTempStr:=lTempStr + aTokenString[i];
                Inc(i);
              end;
          end;

        psAllDone:
          break;

      end;
    end;
end;

class procedure TToDoListCore.AddToDoItemFromParts(const aParts: TStrings;
  const aTLFile: TTLScannedFile; const aFileName: string;
  const aLineNumber: integer; const aToDoType: TToDoType;
  const aTokenStyle: TTokenStyle);

var
  lNewToDoItem: TTodoItem;

begin
  lNewToDoItem := TTodoItem.Create(aTLFile);
  lNewToDoItem.ToDoType    := aToDoType;
  lNewToDoItem.TokenStyle  := aTokenStyle;
  lNewToDoItem.LineNumber  := aLineNumber;
  lNewToDoItem.Filename    := aFileName;

  if aParts.Values[TEXT_PART_NAME] <> '' then
    lNewToDoItem.Text:=aParts.Values[TEXT_PART_NAME];

  if aParts.Values[OWNER_PART_NAME] <> '' then
    lNewToDoItem.Owner:=aParts.Values[OWNER_PART_NAME];

  if aParts.Values[CATEGORY_PART_NAME] <> '' then
    lNewToDoItem.Category:=aParts.Values[CATEGORY_PART_NAME];

  if aParts.Values[PRIORITY_PART_NAME] <> '' then
    lNewToDoItem.Priority:=StrToInt(aParts.Values[PRIORITY_PART_NAME]);

  if Assigned(aTLFile) then
    aTLFile.Add(lNewToDoItem);
end;

class procedure TToDoListCore.CreateToDoItem(aTLFile: TTLScannedFile;
  const aFileName: string; const aStartComment, aEndComment: string;
  const aTokenString: string; aLineNumber: Integer);

var
  lParsingString, lLowerString, lTokenToCheckFor : string;
  lToDoTokenFound: boolean;
  lTodoType, lFoundToDoType: TToDoType;
  lTokenStyle, lFoundTokenStyle:TTokenStyle;
  lParts: TStringList;

begin
  //DebugLn(['TfrmTodo.CreateToDoItem aFileName=',aFileName,' LineNumber=',aLineNumber]);
  lParsingString:= TextToSingleLine(aTokenString);
  // Remove the beginning comment chars from input string
  Delete(lParsingString, 1, Length(aStartComment));
  // Remove leading and trailing blanks from input
  lParsingString := Trim(lParsingString);
  // See if it's a TODO or DONE item
  lLowerString := LowerCase(lParsingString);

  lToDoTokenFound:=False;

  // Determine token and style

  for lTokenStyle := Low(TTokenStyle) to High(TTokenStyle) do
    begin
      for lTodoType := Low(TToDoType) to High (TToDoType) do
        begin
          lTokenToCheckFor := LowerCase(TODO_TOKENS[lTokenStyle, lTodoType]);
          if (Pos(lTokenToCheckFor, lLowerString) = 1) // Token match
            and ((Length(lLowerString)=Length(lTokenToCheckFor)) // Exact match, no further chars. Should not happen?
                 or not (lLowerString[Length(lTokenToCheckFor) + 1] in ['a'..'z'])) then // Extra char is not alpha
            begin
              lToDoTokenFound := True;
              lFoundToDoType := lTodoType;
              lFoundTokenStyle := lTokenStyle;
              Break;
            end;
          if lToDoTokenFound then
            break;
        end;
    end;

  if Not lToDoTokenFound then
    Exit; // Not a Todo/Done item, leave

  // Remove the ending comment chars from input string
  if (aEndComment <> '')
    and (RightStr(lParsingString, Length(aEndComment)) = aEndComment) then
      lParsingString := Copy(lParsingString, 1, Length(lParsingString)-Length(aEndComment));

  // Remove the Token
  Delete(lParsingString, 1, Length(TODO_TOKENS[lFoundTokenStyle, lTodoType]));

  lParsingString := Trim(lParsingString);

  lParts:=TStringList.Create;
  try
    ParseToParts(lParsingString, lParts);
    AddToDoItemFromParts(lParts, aTLFile, aFileName, aLineNumber, lFoundToDoType, lFoundTokenStyle);
  finally
    lParts.Free;
  end;

end;

class procedure TToDoListCore.ExtractToCSV(const aListItems:TListItems;const aFilename:string);
var
  lCommaList: TStringListUTF8;
  i: Integer;
  lToDoItem: TTodoItem;
  s, t: String;
begin
  lCommaList:=TStringListUTF8.Create;
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

class procedure TToDoListCore.ScanFile(const aFileName: string;const FScannedFiles:TAvlTree);
var
  ExpandedFilename: String;
  AVLNode: TAvlTreeNode;
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

  AVLNode:=FScannedFiles.FindKey(Pointer(Tool.MainFilename),
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
    FScannedFiles.Add(CurFile);
  end;
  // save ChangeStep
  CurFile.CodeChangeStep:=Tool.Scanner.ChangeStep;
  //DebugLn(['TfrmTodo.ScanFile saved ChangeStep ',CurFile.CodeChangeStep,' ',Tool.Scanner.ChangeStep]);
  // clear old items
  CurFile.Clear;
  Src:=Tool.Src;
  p:=1;
  NestedComment:=CodeToolBoss.GetNestedCommentsFlagForFile(Code.Filename);

  repeat
      p:=FindNextComment(Src,p);
    if p>length(Src) then break;
      CommentEnd:=FindCommentEnd(Src,p,NestedComment);
    if not Tool.CleanPosToCaret(p,CodeXYPosition) then
      begin
        ShowMessageFmt(errScanFileFailed, [ExtractFileName(aFileName)]);
        Exit;
      end;

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

destructor TTLScannedFile.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TTLScannedFile.Clear;
var
  i: Integer;
begin
  if Assigned(FItems) then
    begin
      for i:=0 to FItems.Count-1 do
        TObject(FItems[i]).Free;
      FreeAndNil(FItems);
  end;
end;

procedure TTLScannedFile.Add(aItem: TTodoItem);
begin
  if not Assigned(FItems) then
    FItems:=TFPList.Create;

  FItems.Add(aItem);
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
  if (pos(aQuote, aSrc)<>0)
    or (pos(' ', aSrc)<>0) then
    Result := AnsiQuotedStr(aSrc, aQuote)
  else
    Result := aSrc;
end;

constructor TTodoItem.Create(aTLFile: TTLScannedFile);
begin
  FTLFile:=aTLFile;
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

end.
