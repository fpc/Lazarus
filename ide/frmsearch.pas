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
    procedure OnAddMatch(const Filename: string; const StartPos,
      EndPos: TPoint; const Lines: string);
    procedure Panel2Click(Sender: TObject);
    procedure SearchFormCREATE(Sender: TObject);
    procedure SearchFormDESTROY(Sender: TObject);
    procedure btnAbortCLICK(Sender: TObject);
  private
    fFlags: TSrcEditSearchOptions;
    fAbortString: string;
    fMask: string;
    fMatches: longint;
    fPad: string;
    fParsedMasks: TStringList; //Holds the parsed masks.
    FProgress: TIDESearchInTextProgress;
    fPromptOnReplace: boolean;
    fRecursive: boolean;
    FReplaceText: string;
    fResultsList: TStrings;
    fResultsWindow: integer;
    fSearchFileList: TStringList;
    fSearchFiles: boolean;
    fSearchFor: String;
    fSearchOpen: boolean;
    fSearchProject: boolean;
    fTheDirectory: string;
    fAborting: boolean;
    procedure DoFindInFiles(TheFileName: string);
    procedure DoFindInSearchList;
    procedure ParseMask;
    procedure UpdateMatches;
    procedure UpdateProgress(FileName: string);
    function PadAndShorten(FileName: string): string;
    procedure SetOptions(TheOptions: TLazFindInFileSearchOptions);
    function GetOptions: TLazFindInFileSearchOptions;
    procedure SearchFile(const aFilename: string);
    procedure SetFlag(Flag: TSrcEditSearchOption; AValue: boolean);
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
    property PromptOnReplace: boolean read fPromptOnReplace write fPromptOnReplace;// this is asked once and can be changed when prompting
    property Progress: TIDESearchInTextProgress read FProgress;
  end;

var
  SearchForm: TSearchForm;
  
function SearchInText(const TheFileName: string;
  var TheText: string;// if TheFileName='' then use TheText
  SearchFor, ReplaceText: string;
  Flags: TSrcEditSearchOptions; var Prompt: boolean;
  Progress: TIDESearchInTextProgress = nil
  ): TModalResult;


implementation


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

function SearchInText(const TheFileName: string;
  var TheText: string;// if TheFileName='' then use TheText
  SearchFor, ReplaceText: string;
  Flags: TSrcEditSearchOptions; var Prompt: boolean;
  Progress: TIDESearchInTextProgress = nil
  ): TModalResult;
var
  OriginalFile: TSourceLog;// The original File being searched
  CaseFile: TSourceLog;  // The working File being searched
  Line: integer;         // Loop Counter
  Match: integer;        // Position of match in line.
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
  
  procedure DoAbort;
  begin
    if Progress<>nil then
      Progress.Abort:=true;
    Result:=mrAbort;
  end;
  
  procedure ProcessMessages;
  begin
    Application.ProcessMessages;
    if (Progress<>nil) and Progress.Abort then
      Result:=mrAbort;
  end;
  
  function FileIsOpenInSourceEditor: boolean;
  begin
    if not SrcEditValid then begin
      if (TheFileName<>'') then
        SrcEdit:=SourceEditorWindow.SourceEditorIntfWithFilename(TheFileName)
      else
        SrcEdit:=nil;
      SrcEditValid:=true;
    end;
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
    if sesoRegExpr in Flags then
      AReplace:=RE.Substitute(AReplace);

    // ask the user
    if Prompt and (TheFileName<>'') then begin
      // open the place in the source editor
      EndLocks;
      if LazarusIDE.DoOpenFileAndJumpToPos(TheFileName,Point(Match,Line+1),
             -1,-1,[ofUseCache,ofDoNotLoadResource,ofVirtualFile,ofRegularFile])
      <>mrOk then
      begin
        DoAbort;
        exit;
      end;
      ASearch:=copy(CurLine,Match,MatchLen);
      // select found text
      if not FileIsOpenInSourceEditor then
        RaiseGDBException('inconsistency');
      SrcEdit.SelectText(Line+1,Match,Line+1,Match+MatchLen);
      SrcEdit.AskReplace(nil,ASearch,AReplace,Line,Match,Action);
      case Action of
        seraSkip: exit;
        seraReplace: ;
        seraReplaceAll: Prompt:=false;
      else
        DoAbort;
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
      if SearchInText<>mrAbort then begin
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
        if (TheFileName<>'') then begin
          OriginalFile.Source:=NewText;
          if (not OriginalFile.SaveToFile(TheFileName)) then begin
            CurResult:=MessageDlg('Write error',
                                  'Error writing file "'+TheFileName+'"',
                                  mtError,[mbCancel,mbAbort],0);
            if CurResult=mrAbort then DoAbort;
          end;
        end else begin
          TheText:=NewText;
        end;
      end;
      FreeMem(ReplacedText);
    end;
  end;

var
  WorkLine: String;
  LastMatchStart: LongInt;
  LastMatchEnd: Integer;
  Found: Boolean;
begin
  if (Progress<>nil) and Progress.Abort then exit(mrAbort);
  Result:=mrOk;

  OriginalFile:=nil;
  CaseFile:=nil;
  RE:=nil;
  SearchAllHitsInLine:=sesoReplace in Flags;
  SrcEdit:=nil;
  SrcEditValid:=false;
  PaintLockEnabled:=false;
  ReplacedText:=nil;
  ReplacedTextCapacity:=0;
  ReplacedTextLength:=0;
  ReplacedTextOriginalPos:=1;

  try
    MatchLen:= Length(SearchFor);
    TempSearch:= SearchFor;

    // load text (to save memory, do not use codetools cache system)
    if FileIsOpenInSourceEditor then begin
      OriginalFile:=TSourceLog.Create(SrcEdit.GetText(false));
    end else if TheFileName<>'' then begin
      OriginalFile:=TSourceLog.Create('');
      OriginalFile.LoadFromFile(TheFileName);
    end else begin
      OriginalFile:=TSourceLog.Create('');
      OriginalFile.Source:=TheText;
    end;

    // convert case
    if sesoMatchCase in Flags then begin
      CaseFile:=OriginalFile;
    end else begin
      CaseFile:=TSourceLog.Create(UpperCaseStr(OriginalFile.Source));
      TempSearch:=UpperCaseStr(TempSearch);
    end;

    if sesoRegExpr in Flags then begin
      //Set up the regular expression search engine.
      RE:= TRegExpr.Create;
      With RE do
      begin
        Expression:= SearchFor;
        ModifierI:= not (sesoMatchCase in Flags);
        ModifierM:= False;  //for now
      end;
    end;

    //writeln('TheFileName=',TheFileName,' len=',OriginalFile.SourceLength,' Cnt=',OriginalFile.LineCount,' TempSearch=',TempSearch);
    ProcessMessages;

    CurLine:='';
    for Line:= 0 to OriginalFile.LineCount -1 do begin
      if (Line and $fff)=0 then begin
        EndLocks;
        ProcessMessages;
      end;
      Match:=1;
      MatchLen:=0;
      CurLineReplaceOffset:=0;
      repeat
        LastMatchStart:=Match;
        LastMatchEnd:=Match+MatchLen;

        // search
        Found:=false;
        if sesoRegExpr in Flags then begin
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
                              sesoWholeWord in Flags,LastMatchEnd,Match);
          if Found then begin
            if (LastMatchStart=LastMatchEnd) then
              CurLine:=OriginalFile.GetLine(Line);
            MatchLen:=length(TempSearch);
          end;
        end;

        // add found place
        if Found then begin
          //DebugLn('TSearchForm.SearchFile CurLine="',CurLine,'" Found=',dbgs(Found),' Match=',dbgs(Match),' MatchLen=',dbgs(MatchLen),' Line=',dbgs(Line));
          if sesoReplace in Flags then begin
            DoReplaceLine
          end else begin
            if (Progress<>nil)
            and (Progress.OnAddMatch<>nil) then
              Progress.OnAddMatch(TheFileName,Point(Match,Line+1),
                                  Point(Match+MatchLen,Line+1),CurLine);
          end;
        end else
          break;
      until (Result=mrAbort) or (not SearchAllHitsInLine) or (MatchLen<1);

      // check abort
      if (Result=mrAbort) then begin
        exit;
      end;

    end;//for
  finally
    CommitChanges;
    if OriginalFile=CaseFile then
      CaseFile:=nil;
    FreeAndNil(OriginalFile);
    FreeAndNil(CaseFile);
    FreeAndNil(RE);
  end;
end;//SearchFile


{ TSearchForm }

procedure TSearchForm.btnAbortCLICK(Sender: TObject);
begin
  Progress.Abort:= true;
end;

procedure TSearchForm.SearchFormCREATE(Sender: TObject);
begin
  //Set Defaults
  MatchesLabel.Caption:=lissMatches;
  SearchingLabel.Caption:=lissSearching;
  SearchTextLabel.Caption:=lissSearchText;
  btnCancel.Caption:=dlgCancel;
  Caption:=dlgSearchCaption;

  fProgress:=TIDESearchInTextProgress.Create;
  FProgress.OnAddMatch:=@OnAddMatch;

  fFlags:=[];
  fPromptOnReplace:=true;
  fRecursive:= True;
  Progress.Abort:= false;
  fAbortString:= dlgSearchAbort;
  fPad:= '...';
  fSearchProject:= false;
  fSearchOpen:= false;
  fSearchFiles:= false;
  Caption:= dlgSearchCaption;
end;

procedure TSearchForm.Panel2Click(Sender: TObject);
begin

end;

procedure TSearchForm.OnAddMatch(const Filename: string; const StartPos,
  EndPos: TPoint; const Lines: string);
var
  MatchLen: Integer;
  TrimmedMatch: LongInt;
  TrimmedCurLine: String;
begin
  TrimmedMatch:=StartPos.X;
  TrimmedCurLine:=TrimLineAndMatch(Lines,TrimmedMatch);
  MatchLen:=EndPos.X-StartPos.X;
  if MatchLen<1 then MatchLen:=1;
  SearchResultsView.AddMatch(fResultsWindow,
                                       FileName,StartPos,
                                       TrimmedCurLine, TrimmedMatch, MatchLen);
  UpdateMatches;
end;

procedure TSearchForm.SearchFormDESTROY(Sender: TObject);
begin
  FreeAndNil(fParsedMasks);
  FreeAndNil(fProgress);
end;

procedure TSearchForm.SetOptions(TheOptions: TLazFindInFileSearchOptions);
begin
  SetFlag(sesoWholeWord,fifWholeWord in TheOptions);
  SetFlag(sesoReplace,fifReplace in TheOptions);
  SetFlag(sesoReplaceAll,fifReplaceAll in TheOptions);
  SetFlag(sesoMatchCase,fifMatchCase in TheOptions);
  SetFlag(sesoRegExpr,fifRegExpr in TheOptions);
  fRecursive:= (fifIncludeSubDirs in TheOptions);
  fSearchProject:= (fifSearchProject in TheOptions);
  fSearchOpen:= (fifSearchOpen in TheOptions);
  fSearchFiles:= (fifSearchDirectories in TheOptions);
end;//SetOptions

function TSearchForm.GetOptions: TLazFindInFileSearchOptions;
begin
  Result:=[];
  if sesoWholeWord in fFlags then include(Result,fifWholeWord);
  if sesoMatchCase in fFlags then include(Result,fifMatchCase);
  if sesoReplace in fFlags then include(Result,fifReplace);
  if sesoReplaceAll in fFlags then include(Result,fifReplaceAll);
  if sesoRegExpr in fFlags then include(Result,fifRegExpr);
  if fRecursive then include(Result,fifIncludeSubDirs);
  if fSearchProject then include(Result, fifSearchProject);
  if fSearchOpen then include(Result,fifSearchOpen);
  if fSearchFiles then include(Result,fifSearchDirectories);
end;//GetOptions

procedure TSearchForm.DoSearch;
begin
  PromptOnReplace:=true;
  fAborting:=false;
  Progress.Abort:=false;
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
          if Progress.Abort and not fAborting then
          begin
            fAborting:= True;
            fResultsList.Insert(0,fAbortString);
            break;
          end
          else if Progress.Abort then
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
          if Progress.Abort and not fAborting then
          begin
            fAborting:= True;
            fResultsList.Insert(0,fAbortString);
            break;
          end
          else if Progress.Abort then
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

procedure TSearchForm.SearchFile(const aFilename: string);
var
  Src: String;
begin
  fResultsList.BeginUpdate;
  try
    Src:='';
    SearchInText(aFilename,Src,fSearchFor,FReplaceText,FFlags,
                 fPromptOnReplace,Progress);
  finally
    fResultsList.EndUpdate;
  end;
end;

procedure TSearchForm.SetFlag(Flag: TSrcEditSearchOption; AValue: boolean);
begin
  if AValue then
    Include(fFlags,Flag)
  else
    Exclude(fFlags,Flag);
end;

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

