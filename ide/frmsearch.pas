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
    FPromptOnReplace: boolean;
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
    property PromptOnReplace: boolean read FPromptOnReplace write FPromptOnReplace;
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
  PromptOnReplace:=true;
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
  Close;
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
  OriginalFile: TSourceLog;  // The original File being searched
  CaseFile: TSourceLog;  // The working File being searched
  Line:      integer;    // Loop Counter
  Match:      integer;   // Position of match in line.
  CurLine: String;
  CurLineReplaceOffset: integer; // e.g. if in the current line 'ABC'
                         // was replaced by 'a', then CurLineReplaceOffset is -2
  TempSearch: string;    // Temp Storage for the search string.
  MatchLen: integer;
  RE: TRegExpr;
  SearchAllHitsInLine: boolean;

  SrcEditValid: Boolean;// true if SrcEdit is valid
  SrcEdit: TSourceEditorInterface;
  PaintLockEnabled: Boolean;

  ReplacedText: PChar;
  ReplacedTextCapacity: integer;
  ReplacedTextLength: integer;
  ReplacedTextOriginalPos: integer;// 1-based. e.g. 2 bytes has been replaced => ReplacedTextOriginalPos=3.

  function FileIsOpenInSourceEditor: boolean;
  begin
    if not SrcEditValid then
      SrcEdit:=SourceEditorWindow.SourceEditorIntfWithFilename(TheFileName);
    SrcEditValid:=true;
    Result:=SrcEdit<>nil;
  end;
  
  procedure GrowNewText(NewLength: integer);
  var
    NewCapacity: Integer;
  begin
    if NewLength<=ReplacedTextCapacity then exit;
    // grow
    // first double
    NewCapacity:=ReplacedTextCapacity*2;
    if NewLength>NewCapacity then begin
      // double is not enough, use the original size as minimum
      if NewCapacity<1 then
        NewCapacity:=OriginalFile.SourceLength+1000;
      if NewLength>NewCapacity then begin
        // still not enough -> grow to new length
        NewCapacity:=NewLength;
      end;
    end;
    ReplacedTextCapacity:=NewCapacity;
    ReAllocMem(ReplacedText,ReplacedTextCapacity);
  end;
  
  procedure EnablePaintLock;
  begin
    if (not PaintLockEnabled) and FileIsOpenInSourceEditor then begin
      PaintLockEnabled:=true;
      SrcEdit.BeginUpdate;
    end;
  end;
  
  procedure DisablePaintLock;
  begin
    if PaintLockEnabled then
      SrcEdit.EndUpdate;
    PaintLockEnabled:=false;
  end;
  
  procedure EndLocks;
  begin
    DisablePaintLock;
    SrcEditValid:=false;
  end;
  
  procedure DoReplaceLine;
  var
    ASearch: String;
    AReplace: String;
    Action: TSrcEditReplaceAction;
    OriginalTextPos: integer; // 1-based
    GapLength: Integer;
    NewLength: Integer;
  begin
    // create replacement
    AReplace:=ReplaceText;
    if fRegExp then
      AReplace:=RE.Substitute(AReplace);

    // ask the user
    if PromptOnReplace then begin
      // open the place in the source editor
      EndLocks;
      if LazarusIDE.DoOpenFileAndJumpToPos(TheFileName,Point(Match,Line+1),
             -1,-1,[ofUseCache,ofDoNotLoadResource,ofVirtualFile,ofRegularFile])
      <>mrOk then
      begin
        fAbort:=true;
        exit;
      end;
      ASearch:=copy(CurLine,Match,MatchLen);
      // select found text
      if not FileIsOpenInSourceEditor then
        RaiseGDBException('inconsistency');
      SrcEdit.SelectText(Line+1,Match,Line+1,Match+MatchLen);
      SrcEdit.AskReplace(Self,ASearch,AReplace,Line,Match,Action);
      case Action of
        seraSkip: exit;
        seraReplace: ;
        seraReplaceAll: PromptOnReplace:=false;
      else
        fAbort:=true;
        exit;
      end;
    end;

    if FileIsOpenInSourceEditor then begin
      // change text in source editor
      EnablePaintLock;
      SrcEdit.SelectText(Line+1,Match+CurLineReplaceOffset,
                         Line+1,Match+CurLineReplaceOffset+MatchLen);
      SrcEdit.Selection:=AReplace;
      // adjust CurLine and MatchLen for next search
      DebugLn('DoReplaceLine CurLine="',CurLine,'" Match=',dbgs(Match),' MatchLen=',dbgs(MatchLen));
    end else begin
      // change text in memory/disk
      OriginalFile.LineColToPosition(Line+1,Match,OriginalTextPos);
      GapLength:=OriginalTextPos-ReplacedTextOriginalPos;
      NewLength:=ReplacedTextLength+GapLength+length(AReplace);
      GrowNewText(NewLength);
      //writeln('DoReplaceLine Line=',Line,' Match=',Match,' OriginalTextPos=',OriginalTextPos,' ReplacedTextOriginalPos=',ReplacedTextOriginalPos,' GapLength=',GapLength,' NewLength=',NewLength,' "',AReplace,'" ReplacedTextCapacity=',ReplacedTextCapacity);
      // copy the text between the last and this replacement
      if GapLength>0 then begin
        System.Move(OriginalFile.Source[ReplacedTextOriginalPos],
                    ReplacedText[ReplacedTextLength],GapLength);
        inc(ReplacedTextLength,GapLength);
      end;
      // copy the replacement
      if AReplace<>'' then begin
        System.Move(AReplace[1],ReplacedText[ReplacedTextLength],length(AReplace));
        inc(ReplacedTextLength,length(AReplace));
      end;
      // save original position behind found position
      OriginalFile.LineColToPosition(Line+1,Match+MatchLen,ReplacedTextOriginalPos);
    end;
    // adjust replace offset
    inc(CurLineReplaceOffset,length(AReplace)-MatchLen);
  end;
  
  procedure CommitChanges;
  var
    GapLength: Integer;
    NewLength: Integer;
    NewText: string;
    CurResult: TModalResult;
  begin
    EndLocks;
    if ReplacedText<>nil then begin
      if not fAbort then begin
        GapLength:=OriginalFile.SourceLength+1-ReplacedTextOriginalPos;
        NewLength:=ReplacedTextLength+GapLength;
        GrowNewText(NewLength);
        // copy the text between the last and this replacement
        if GapLength>0 then begin
          System.Move(OriginalFile.Source[ReplacedTextOriginalPos],
                      ReplacedText[ReplacedTextLength],GapLength);
          inc(ReplacedTextLength,GapLength);
        end;
        SetLength(NewText,ReplacedTextLength);
        if NewText<>'' then
          System.Move(ReplacedText[0],NewText[1],length(NewText));
        OriginalFile.Source:=NewText;
        if not OriginalFile.SaveToFile(TheFileName) then begin
          CurResult:=MessageDlg('Write error',
                                'Error writing file "'+TheFileName+'"',
                                mtError,[mbCancel,mbAbort],0);
          if CurResult=mrAbort then fAbort:=true;
        end;
      end;
      FreeMem(ReplacedText);
    end;
  end;

var
  WorkLine: String;
  TrimmedCurLine: String;
  TrimmedMatch: LongInt;
  LastMatchStart: LongInt;
  LastMatchEnd: Integer;
  Found: Boolean;
begin
  if fAbort then exit;
  
  OriginalFile:=nil;
  CaseFile:=nil;
  RE:=nil;
  SearchAllHitsInLine:=fReplace;
  SrcEdit:=nil;
  SrcEditValid:=false;
  PaintLockEnabled:=false;
  ReplacedText:=nil;
  ReplacedTextCapacity:=0;
  ReplacedTextLength:=0;
  ReplacedTextOriginalPos:=1;

  fResultsList.BeginUpdate;
  try
    MatchLen:= Length(fSearchFor);
    TempSearch:= fSearchFor;

    // load text (do not use CodeToolBoss cache system to save memory)
    if FileIsOpenInSourceEditor then begin
      OriginalFile:=TSourceLog.Create(SrcEdit.GetText(false));
    end else begin
      OriginalFile:=TSourceLog.Create('');
      OriginalFile.LoadFromFile(TheFileName);
    end;
    
    // convert case
    if fCaseSensitive then begin
      CaseFile:=OriginalFile;
    end else begin
      CaseFile:=TSourceLog.Create(UpperCaseStr(OriginalFile.Source));
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

    //writeln('TheFileName=',TheFileName,' len=',OriginalFile.SourceLength,' Cnt=',OriginalFile.LineCount,' TempSearch=',TempSearch);
    Application.ProcessMessages;
    CurLine:='';
    for Line:= 0 to OriginalFile.LineCount -1 do begin
      if (Line and $fff)=0 then begin
        EndLocks;
        Application.ProcessMessages;
      end;
      Match:=1;
      MatchLen:=0;
      CurLineReplaceOffset:=0;
      repeat
        LastMatchStart:=Match;
        LastMatchEnd:=Match+MatchLen;
        
        // search
        Found:=false;
        if fRegExp then begin
          // search every line for regular expression
          if LastMatchStart=LastMatchEnd then begin
            CurLine:=OriginalFile.GetLine(Line);
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
          Found:=SearchInLine(TempSearch,CaseFile,Line,
                              fWholeWord,LastMatchEnd,Match);
          if Found then begin
            if (LastMatchStart=LastMatchEnd) then
              CurLine:=OriginalFile.GetLine(Line);
            MatchLen:=length(TempSearch);
          end;
        end;
        
        // add found place
        if Found then begin
          //DebugLn('TSearchForm.SearchFile CurLine="',CurLine,'" Found=',dbgs(Found),' Match=',dbgs(Match),' MatchLen=',dbgs(MatchLen),' Line=',dbgs(Line));
          if fReplace then begin
            DoReplaceLine
          end else begin
            TrimmedMatch:=Match;
            TrimmedCurLine:=TrimLineAndMatch(CurLine,TrimmedMatch);
            SearchResultsView.AddMatch(fResultsWindow,
                                       TheFileName,Point(Match,Line+1),
                                       TrimmedCurLine, TrimmedMatch, MatchLen);
            UpdateMatches;
          end;
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
    CommitChanges;
    if OriginalFile=CaseFile then
      CaseFile:=nil;
    FreeAndNil(OriginalFile);
    FreeAndNil(CaseFile);
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

