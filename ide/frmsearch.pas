{
/***************************************************************************
                                frmSearch.pas
                             -------------------

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
}
unit frmSearch;

{$mode objfpc}{$H+}

interface

uses
  // LCL
  Classes, SysUtils, LCLProc, LResources, LCLType, LCLIntf, Forms, Controls,
  Graphics, Dialogs, ExtCtrls, StdCtrls, Buttons, FileUtil,
  // synedit, codetools
  SynRegExpr, SourceLog, KeywordFuncLists,
  // IDEIntf
  LazIDEIntf, SrcEditorIntf,
  // ide
  LazarusIDEStrConsts, InputHistory, FindInFilesDlg, SearchResultView;


type

  { TSearchForm }

  TSearchForm = class(TForm)
    btnCancel: TBUTTON;
    MatchesLabel: TLABEL;
    SearchingLabel: TLABEL;
    SearchTextLabel: TLABEL;
    lblMatches: TLABEL;
    lblProgress: TLABEL;
    lblSearchText: TLABEL;
    Panel2: TPANEL;
    procedure SearchFormCREATE(Sender: TObject);
    procedure SearchFormDESTROY(Sender: TObject);
    procedure btnAbortCLICK(Sender: TObject);
  private
    fAbort: boolean;
    fAborting: boolean;
    fAbortString: string;
    fCaseSensitive: Boolean;
    fmask: string;
    fMatches: longint;
    fPad: string;
    fParsedMasks: TStringList; //Holds the parsed masks.
    fRecursive: boolean;
    fRegExp: Boolean;
    FReplaceText: string;
    fResultsList: TStrings;
    fResultsWindow: integer;
    fSearchFileList: TStringList;
    fSearchFiles: boolean;
    fSearchFor: String;
    fSearchOpen: boolean;
    fSearchProject: boolean;
    fTheDirectory: string;
    fWholeWord: Boolean;
    fReplace: boolean;
    fReplaceAll: boolean;
    procedure SearchFile(TheFileName: string);
    procedure DoFindInFiles(TheFileName: string);
    procedure DoFindInSearchList;
    procedure ParseMask;
    procedure UpdateMatches;
    procedure UpdateProgress(FileName: string);
    function PadAndShorten(FileName: string): string;
    procedure SetOptions(TheOptions: TLazFindInFileSearchOptions);
    function GetOptions: TLazFindInFileSearchOptions;
  public
    Procedure DoSearch;
    property SearchDirectory: string read fTheDirectory write fTheDirectory;
    property SearchText: string read fSearchFor write fSearchFor;
    property ReplaceText: string read FReplaceText write FReplaceText;
    property SearchOptions: TLazFindInFileSearchOptions read GetOptions
                                                        write SetOptions;
    property SearchFileList: TStringList read fSearchFileList
                                         write fSearchFileList;
    property ResultsList: TStrings read fResultsList write fResultsList;
    property SearchMask: string read fMask write fMask;
    property Pad: string read fPad write fPad;
    property ResultsWindow: integer read fResultsWindow write fResultsWindow;
  end;

var
  SearchForm: TSearchForm;

implementation

{ TSearchForm }

procedure TSearchForm.btnAbortCLICK(Sender: TObject);
begin
  fAbort:= true;
end;

procedure TSearchForm.SearchFormCREATE(Sender: TObject);
begin
  //Set Defaults
  MatchesLabel.Caption:=lissMatches;
  SearchingLabel.Caption:=lissSearching;
  SearchTextLabel.Caption:=lissSearchText;
  btnCancel.Caption:=dlgCancel;
  Caption:=dlgSearchCaption;

  fWholeWord:= false;
  fReplace:=false;
  fReplaceAll:=false;
  fCaseSensitive:= false;
  fRecursive:= True;
  fAbort:= false;
  fAbortString:= dlgSearchAbort;
  fPad:= '...';
  fRegExp:= false;
  fAborting:= false;
  fSearchProject:= false;
  fSearchOpen:= false;
  fSearchFiles:= false;
  self.Caption:= dlgSearchCaption;
end;//SearchFormCreate

procedure TSearchForm.SearchFormDESTROY(Sender: TObject);
begin
  FreeAndNil(fParsedMasks);
end;

procedure TSearchForm.SetOptions(TheOptions: TLazFindInFileSearchOptions);
begin
  fWholeWord:= (fifWholeWord in TheOptions);
  fReplace:=(fifReplace in TheOptions);
  fReplaceAll:=(fifReplaceAll in TheOptions);
  fCaseSensitive:= (fifMatchCase in TheOptions);
  fRegExp := (fifRegExpr in TheOptions);
  frecursive:= (fifIncludeSubDirs in TheOptions);
  fSearchProject:= (fifSearchProject in TheOptions);
  fSearchOpen:= (fifSearchOpen in TheOptions);
  fSearchFiles:= (fifSearchDirectories in TheOptions);
end;//SetOptions

function TSearchForm.GetOptions: TLazFindInFileSearchOptions;
begin
  Result:=[];
  if fWholeWord then include(Result,fifWholeWord);
  if fCaseSensitive then include(Result,fifMatchCase);
  if fReplace then include(Result,fifReplace);
  if fReplaceAll then include(Result,fifReplaceAll);
  if fRegExp then include(Result,fifRegExpr);
  if fRecursive then include(Result,fifIncludeSubDirs);
  if fRecursive then include(Result,fifIncludeSubDirs);
  if fSearchProject then include(Result, fifSearchProject);
  if fSearchOpen then include(Result,fifSearchOpen);
  if fSearchFiles then include(Result,fifSearchDirectories);
end;//GetOptions

procedure TSearchForm.DoSearch;
begin
  lblSearchText.Caption:= fSearchFor;
  fMatches:= 0;
  ParseMask;
  if Assigned(fResultsList) then
  begin
    if fSearchFiles then
    begin
      DoFindInFiles(fTheDirectory);
    end;//if
    if fSearchProject or fSearchOpen then
      DoFindInSearchList;
  end;//if
  if Assigned(fResultsList) and (fResultsList.Count = 0) then
    fResultsList.Add(lisFileNotFound);
  Self.Close;
end;//DoSearch

procedure TSearchForm.SearchFile(TheFileName: string);
  const
    WordBreakChars = [#0..#31,'.', ',', ';', ':', '"', '''', '!', '?', '[', ']',
               '(', ')', '{', '}', '^', '-', '=', '+', '*', '/', '\', '|', ' '];
    WhiteSpaceChars = [' ',#10,#13,#9];

  function SearchInLine(const SearchStr: string; SrcLog: TSourceLog;
    LineNumber: integer; WholeWords: boolean; StartInLine: integer;
    var MatchStartInLine: integer): boolean;
  // search SearchStr in SrcLog line
  // returns MatchStartInLine=1 for start of line
  var
    LineRange: TLineRange;
    Src: String;
    StartPos: PChar;
    EndPos: PChar;
    i: Integer;
    SearchLen: Integer;
    LineStartPos: PChar;
    FirstChar: Char;
    Found: Boolean;
    CharInFront: PChar;
    CharBehind: PChar;
  begin
    Result:=false;
    if SearchStr='' then exit;
    SrcLog.GetLineRange(LineNumber,LineRange);
    Src:=SrcLog.Source;
    SearchLen:=length(SearchStr);
    LineStartPos:=@Src[LineRange.StartPos];
    StartPos:=LineStartPos+StartInLine-1;
    EndPos:=@Src[LineRange.EndPos-SearchLen+1];
    FirstChar:=SearchStr[1];
    while (StartPos<EndPos) do begin
      if FirstChar=StartPos^ then begin
        i:=1;
        while (i<=SearchLen) and (StartPos[i-1]=SearchStr[i]) do
          inc(i);
        if i>SearchLen then begin
          Found:=true;
          MatchStartInLine:=StartPos-LineStartPos+1;
          if WholeWords then begin
            CharInFront:=StartPos-1;
            CharBehind:=StartPos+SearchLen;
            if ((MatchStartInLine=1)
                or (CharInFront^ in WordBreakChars))
            and ((StartPos+SearchLen=@Src[LineRange.EndPos])
                 or (CharBehind^ in WordBreakChars))
            then begin
              // word start and word end
            end else begin
              // not whole word
              Found:=false;
            end;
          end;
          if Found then begin
            Result:=true;
            exit;
          end;
        end;
      end;
      inc(StartPos);
    end;
  end;
  
  function TrimLineAndMatch(const Line: string; var APosition: integer): string;
  var
    StartPos: Integer;
    EndPos: Integer;
  begin
    StartPos:=1;
    while (StartPos<=length(Line)) and (Line[StartPos] in WhiteSpaceChars) do
      inc(StartPos);
    EndPos:=length(Line)+1;
    while (EndPos>=StartPos) and (Line[EndPos-1] in WhiteSpaceChars) do
      dec(EndPos);
    dec(APosition,StartPos-1);
    Result:=copy(Line,StartPos,EndPos-StartPos);
  end;
  
{Start SearchFile ============================================================}
var
  ThisFile: TSourceLog;   //The original File being searched
  UpperFile: TSourceLog;  //The working File being searched
  Line:      integer;    //Loop Counter
  Match:      integer;    //Position of match in line.
  TempSearch: string;     //Temp Storage for the search string.
  MatchLen: integer;
  CurLine: String;
  RE: TRegExpr;
  Found: Boolean;
  TrimmedMatch: LongInt;
  LastMatchStart: LongInt;
  LastMatchEnd: Integer;
  WorkLine: String;
  TrimmedCurLine: String;
  SearchAllHitsInLine: boolean;
  PromptOnReplace: boolean;

  procedure DoReplaceLine;
  var
    ASearch: String;
    AReplace: String;
    Action: TSrcEditReplaceAction;
  begin
    // ask the user
    if PromptOnReplace then begin
      // open the place in the source editor
      if LazarusIDE.DoOpenFileAndJumpToPos(TheFileName,Point(Match,Line),-1,-1,
                  [ofUseCache,ofDoNotLoadResource,ofVirtualFile,ofRegularFile])
      <>mrOk then
      begin
        fAbort:=true;
        exit;
      end;
      ASearch:=copy(CurLine,Match,MatchLen);
      AReplace:=ReplaceText;
      SourceEditorWindow.ActiveEditor.AskReplace(Self,ASearch,AReplace,
                                                 Line,Match,Action);
      case Action of
      seraSkip: exit;
      seraReplace: ;
      seraReplaceAll: PromptOnReplace:=false;
      else
        fAbort:=true;
        exit;
      end;
    end;
    // TODO: create change text
  end;

begin
  if fAbort then exit;
  
  ThisFile:=nil;
  UpperFile:=nil;
  RE:=nil;
  PromptOnReplace:=fReplace;
  SearchAllHitsInLine:=fReplace;

  fResultsList.BeginUpdate;
  try
    MatchLen:= Length(fSearchFor);
    TempSearch:= fSearchFor;

    ThisFile:= TSourceLog.Create('');
    ThisFile.LoadFromFile(TheFileName);
    if fCaseSensitive then begin
      UpperFile:=ThisFile;
    end else begin
      UpperFile:=TSourceLog.Create(UpperCaseStr(ThisFile.Source));
      TempSearch:=UpperCaseStr(TempSearch);
    end;

    if fRegExp then begin
      //Set up the regular expression search engine.
      RE:= TRegExpr.Create;
      With RE do
      begin
        Expression:= fSearchFor;
        ModifierI:= not fCaseSensitive;
        ModifierM:= False;  //for now
      end;
    end;

    //writeln('TheFileName=',TheFileName,' len=',ThisFile.SourceLength,' Cnt=',ThisFile.LineCount,' TempSearch=',TempSearch);
    Application.ProcessMessages;
    CurLine:='';
    for Line:= 0 to ThisFile.LineCount -1 do
    begin
      if (Line and $fff)=0 then
        Application.ProcessMessages;
      Match:=1;
      MatchLen:=0;
      repeat
        LastMatchStart:=Match;
        LastMatchEnd:=Match+MatchLen;
        
        // search
        Found:=false;
        if fRegExp then begin
          // search every line for regular expression
          if LastMatchStart=LastMatchEnd then begin
            CurLine:=ThisFile.GetLine(Line);
            WorkLine:=CurLine;
          end else begin
            WorkLine:=copy(CurLine,LastMatchEnd,length(CurLine));
          end;
          if RE.Exec(WorkLine) then begin
            Found:=true;
            Match:= RE.MatchPos[0]+LastMatchEnd-1;
            MatchLen:= Re.MatchLen[0];
          end;
        end else begin
          Found:=SearchInLine(TempSearch,UpperFile,Line,
                              fWholeWord,LastMatchEnd,Match);
          if Found then begin
            CurLine:=ThisFile.GetLine(Line);
            MatchLen:=length(TempSearch);
          end;
        end;
        
        // add found place
        if Found then begin
          //DebugLn('TSearchForm.SearchFile CurLine="',CurLine,'" Found=',dbgs(Found),' Match=',dbgs(Match),' MatchLen=',dbgs(MatchLen),' Line=',dbgs(Line));
          if fReplace then
            DoReplaceLine;
          TrimmedMatch:=Match;
          TrimmedCurLine:=TrimLineAndMatch(CurLine,TrimmedMatch);
          SearchResultsView.AddMatch(fResultsWindow,
                                     TheFileName,Point(Match,Line+1),
                                     TrimmedCurLine, TrimmedMatch, MatchLen);
          UpdateMatches;
        end else
          break;
      until fAbort or (not SearchAllHitsInLine) or (MatchLen<1);
      
      // check abort
      if fAbort and not fAborting then
      begin
        fResultsList.Insert(0,fAbortString);
        fAborting:= True;
        break;
      end
      else if fAbort then
      begin
        break;
      end;//if
      
    end;//for
  finally
    if ThisFile=UpperFile then
      UpperFile:=nil;
    FreeAndNil(ThisFile);
    FreeAndNil(UpperFile);
    FreeAndNil(RE);
    fResultsList.EndUpdate;
  end;
end;//SearchFile


procedure TSearchForm.DoFindInFiles(TheFileName: string);
var
  //Loop counter
  i:        integer;
  //Result of FindFirst, FindNext
  FileInfo: TSearchRec;
  //Temp Storage for The search Directoru
  TempDir: string;
begin
  //if we have a list and a valid directory
  if (DirPathExists(TheFileName)) then
  begin //make sure path ends with delimiter
    TempDir:= AppendPathDelim(TheFileName);
    for i:= 0 to fParsedMasks.Count -1 do
    begin
      if SysUtils.FindFirst(TempDir + fParsedMasks[i],
                            faAnyFile,FileInfo)=0 then
      begin
        repeat
          // check if special file, skip directories this time
          if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='')
          or ((faDirectory and FileInfo.Attr)>0) then continue;
          //Make sure this is a text file as it will be searched
          if FileIsReadable(TempDir + FileInfo.Name)
          and FileIsText(TempDir + FileInfo.Name) then
          begin
            UpdateProgress(TempDir + FileInfo.Name);
            SearchFile(TempDir + FileInfo.Name);
          end;//if
          if fAbort and not fAborting then
          begin
            fAborting:= True;
            fResultsList.Insert(0,fAbortString);
            break;
          end
          else if fAbort then
          begin
            break;
          end;
        until SysUtils.FindNext(FileInfo)<>0;
      end;//if
      SysUtils.FindClose(FileInfo);
    end;//for
    //If selected then Look for and search subdirectories
    if (frecursive) then begin
      if (SysUtils.FindFirst(TempDir+GetAllFilesMask,faAnyFile,FileInfo)=0) then
      begin
        repeat
          // check if directory and not special file
          if ((faDirectory and FileInfo.Attr)>0)
            and (FileInfo.Name<>'.') and (FileInfo.Name<>'..')
            and (FileInfo.Name<>'')
            then
              DoFindInFiles(TempDir + FileInfo.Name);
          if fAbort and not fAborting then
          begin
            fAborting:= True;
            fResultsList.Insert(0,fAbortString);
            break;
          end
          else if fAbort then
          begin
            break;
          end;
        until SysUtils.FindNext(FileInfo)<>0;
      end;//if
      SysUtils.FindClose(FileInfo);
    end;//if
  end;//if
end;//DoFindInFiles

procedure TSearchForm.DoFindInSearchList;
var
  i: integer;
begin
  if Assigned(fSearchFileList) then
  begin
    for i:= 0 to fSearchFileList.Count -1 do
    begin
      UpdateProgress(fSearchFileList[i]);
      SearchFile(fSearchFileList[i]);
    end;//for
  end;//if
end;//DoFindInSearchList

procedure TSearchForm.ParseMask;
var
  //Position Tracking within the string.
  curpos,startpos: integer;
  //Used as mask seperator
const
  MaskSeperator = ';';
begin
  if not Assigned(fParsedMasks) then
    fParsedMasks:= TStringList.Create;
  if fmask<>'' then
  begin
    fParsedMasks.Clear;
    //do we have multiple masks
    if (pos(MaskSeperator,fMask)>0) then
    begin
      startpos:=1;
      curpos:=1;
      repeat //loop through the string and get the masks.
        while (curpos<=length(fmask)) and (fmask[curpos] <> MaskSeperator) do
          inc(curpos);
        //add the mask to the list
        fParsedMasks.Add(copy(fmask,startpos,curpos-startpos));
        inc(curpos);//skip the seperator
        startpos:= curpos;//start on next mask
      until curpos > length(fmask);
    end//if
    else
    begin
      fParsedMasks.Add(fmask);
    end;//else
  end//if
  else
  begin
    fParsedMasks.Add(GetAllFilesMask) //OS Independent Mask
  end;//else
end;//ParseMask


procedure TSearchForm.UpdateMatches;
begin
  inc(fMatches);
  lblMatches.Caption:= IntToStr(fMatches);
end;//UpdateMatches


procedure TSearchForm.UpdateProgress(FileName: string);
var
  DisplayFileName: string;
begin
  DisplayFileName := FileName;
  lblProgress.Caption:= DisplayFileName;
  while (lblProgress.Left + lblProgress.Width)> lblProgress.Parent.ClientWidth-12 do
  begin
    DisplayFileName:= PadAndShorten(DisplayFileName);
    lblProgress.Caption := DisplayFileName;
  end;//while
end;//UpdateProgress

function TSearchForm.PadAndShorten(FileName: string): string;
var
  FoundAt: integer;
begin
  result:= '';
  FoundAt:= pos(PathDelim,FileName);
  inc(FoundAt);
  result:= copy(FileName,FoundAt,Length(FileName));
  result:= fPad + result;
end;//PadAndShorten

initialization
  {$I frmsearch.lrs}

end.

