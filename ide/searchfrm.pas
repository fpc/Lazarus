{
/***************************************************************************
                                SearchFrm.pas
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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************
}
unit SearchFrm;

{$mode objfpc}{$H+}

interface

uses
  // RTL + FCL
  Classes, SysUtils, types, RegExpr,
  // LCL
  LCLIntf, Forms, Controls, ComCtrls, Dialogs, ExtCtrls, StdCtrls, Buttons,
  // CodeTools
  SourceLog, KeywordFuncLists, BasicCodeTools, FileProcs,
  // LazUtils
  FileUtil, LazFileUtils, LazFileCache, LazLoggerBase,
  // IDEIntf
  IDEWindowIntf, LazIDEIntf, SrcEditorIntf, IDEDialogs,
  // ide
  LazarusIDEStrConsts, InputHistory, IDEProcs, SearchResultView, Project;

type

  { TSearchProgressForm }

  TSearchProgressForm = class(TForm)
    btnCancel: TBitBtn;
    MatchesLabel: TLABEL;
    SearchingLabel: TLABEL;
    SearchTextLabel: TLABEL;
    lblMatches: TLABEL;
    lblProgress: TLABEL;
    lblSearchText: TLABEL;
    Panel2: TPANEL;
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure OnAddMatch(const Filename: string; const StartPos, EndPos: TPoint;
                         const Lines: string);
    procedure SearchFormCREATE(Sender: TObject);
    procedure SearchFormDESTROY(Sender: TObject);
    procedure btnAbortCLICK(Sender: TObject);
  private
    fFlags: TSrcEditSearchOptions;
    fAbortString: string;
    fMask: string;
    fMatches: longint;
    fPad: string;
    FProgress: TIDESearchInTextProgress;
    fPromptOnReplace: boolean;
    fRecursive: boolean;
    FReplaceText: string;
    fResultsListUpdating: boolean;
    fResultsList: TStrings;
    fResultsWindow: TTabSheet;
    fSearchFileList: TStringList;
    fSearchFiles: boolean;
    fSearchFor: String;
    fDirectories: string;
    fSearchOpen: boolean;
    fSearchActive: boolean;
    fSearchProject: boolean;
    fAborting: boolean;
    fLastUpdateProgress: DWORD;
    fWasActive: boolean;
    procedure DoFindInFiles(ADirectories: string);
    procedure DoFindInSearchList;
    procedure SetResultsList(const AValue: TStrings);
    procedure UpdateMatches;
    procedure UpdateProgress(FileName: string);
    function PadAndShorten(FileName: string): string;
    procedure SetOptions(TheOptions: TLazFindInFileSearchOptions);
    function GetOptions: TLazFindInFileSearchOptions;
    procedure SearchFile(const aFilename: string);
    procedure SetFlag(Flag: TSrcEditSearchOption; AValue: boolean);
    procedure DoSearchAndAddToSearchResults;
    function DoSearch: integer;
  public
    procedure DoSearchOpenFiles;
    procedure DoSearchActiveFile;
    procedure DoSearchDir;
    procedure DoSearchProject(AProject: TProject);
  public
    property SearchDirectories: string read fDirectories write fDirectories;
    property SearchText: string read fSearchFor write fSearchFor;
    property ReplaceText: string read FReplaceText write FReplaceText;
    property SearchOptions: TLazFindInFileSearchOptions read GetOptions
                                                        write SetOptions;
    property SearchFileList: TStringList read fSearchFileList
                                         write fSearchFileList;
    property ResultsList: TStrings read fResultsList write SetResultsList;
    property SearchMask: string read fMask write fMask;
    property Pad: string read fPad write fPad;
    property ResultsWindow: TTabSheet read fResultsWindow write fResultsWindow;
    property PromptOnReplace: boolean read fPromptOnReplace write fPromptOnReplace;// this is asked once and can be changed when prompting
    property Progress: TIDESearchInTextProgress read FProgress;
  end;

var
  SearchProgressForm: TSearchProgressForm;
  
function SearchInText(const TheFileName: string;
  var TheText: string;// if TheFileName='' then use TheText
  SearchFor, ReplaceText: string;
  Flags: TSrcEditSearchOptions; var Prompt: boolean;
  Progress: TIDESearchInTextProgress = nil
  ): TModalResult;
function TrimLinesAndAdjustPos(const Lines: string; var APosition: integer): string;
function SearchInLine(const SearchStr: string; SrcLog: TSourceLog;
  LineNumber: integer; WholeWords: boolean; StartInLine: integer;
  out MatchStartInLine: integer): boolean;


implementation

{$R *.lfm}

const
  WordBreakChars = [#0..#31,'.', ',', ';', ':', '"', '''', '!', '?', '[', ']',
               '(', ')', '{', '}', '^', '-', '=', '+', '*', '/', '\', '|', ' '];
  WhiteSpaceChars = [' ',#10,#13,#9];

function SearchInLine(const SearchStr: string; SrcLog: TSourceLog;
  LineNumber: integer; WholeWords: boolean; StartInLine: integer;
  out MatchStartInLine: integer): boolean;
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
  SrcLog.GetLineRange(LineNumber-1,LineRange);
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

function TrimLinesAndAdjustPos(const Lines: string;
  var APosition: integer): string;
var
  StartPos: Integer;
  EndPos: Integer;
begin
  if Lines='' then begin
    Result:='';
    exit;
  end;
  if LineEndCount(Lines)=0 then begin
    StartPos:=1;
    while (StartPos<=length(Lines)) and (Lines[StartPos] in WhiteSpaceChars) do
      inc(StartPos);
    if (APosition>0) and (StartPos>APosition) then
      StartPos:=APosition;
    EndPos:=length(Lines)+1;
    while (EndPos>=StartPos) and (Lines[EndPos-1] in WhiteSpaceChars) do
      dec(EndPos);
    dec(APosition,StartPos-1);
    Result:=copy(Lines,StartPos,EndPos-StartPos);
  end else
    Result:=Lines;
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
  FoundStartPos: TPoint; // Position of match in line. 1 based.
  FoundEndPos: TPoint;
  ReplaceLineOffset: integer;// number of lines added/deleted by replacement.
  LastReplaceLine: integer;  // last changed line by replace. 1 based
  LastReplaceColOffset: integer;// bytes added/deleted by replace in last line
  TempSearch: string;    // Temp Storage for the search string.
  RE: TRegExpr;
  Lines: String;

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
    if Application<>nil then Application.ProcessMessages;
    if (Progress<>nil) and Progress.Abort then
      Result:=mrAbort;
  end;
  
  function FileIsOpenInSourceEditor: boolean;
  begin
    if not SrcEditValid then begin
      if (TheFileName<>'') and (SourceEditorManagerIntf<>nil) then
        SrcEdit:=SourceEditorManagerIntf.SourceEditorIntfWithFilename(TheFileName)
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
    AReplace: String;
    Action: TSrcEditReplaceAction;
    OriginalTextPos: integer; // 1-based
    GapLength: Integer;
    NewLength: Integer;
    SrcEditPosValid: boolean;
    SrcEditStartPos, SrcEditEndPos: TPoint;
    aLastLineLength: integer;
    aLineCount: integer;
    i: integer;
    
    procedure GetSrcEditPos;
    begin
      if not SrcEditPosValid then begin
        SrcEditStartPos:=FoundStartPos;
        SrcEditEndPos:=FoundEndPos;
        // FoundStart/EndPos contain the original position
        // add the changes due to replacement to SrcEditStart/EndPos
        if SrcEditStartPos.Y=LastReplaceLine then
          inc(SrcEditStartPos.X,LastReplaceColOffset);
        if SrcEditStartPos.Y>=LastReplaceLine then
          inc(SrcEditStartPos.Y,ReplaceLineOffset);
        if SrcEditEndPos.Y=LastReplaceLine then
          inc(SrcEditEndPos.X,LastReplaceColOffset);
        if SrcEditEndPos.Y>=LastReplaceLine then
          inc(SrcEditEndPos.Y,ReplaceLineOffset);
        SrcEditPosValid:=true;
      end;
    end;
    
  begin
    // create replacement
    AReplace:=ReplaceText;
    if sesoRegExpr in Flags then
      AReplace:=RE.Substitute(AReplace);
    //DebugLn(['DoReplaceLine Replace with "',AReplace,'"']);
      
    SrcEditPosValid:=false;
      
    // ask the user
    if Prompt and (TheFileName<>'') then begin
      // open the place in the source editor
      EndLocks;

      // update windows
      ProcessMessages;
      if Result=mrAbort then exit;
      
      GetSrcEditPos;
      if LazarusIDE.DoOpenFileAndJumpToPos(TheFileName,SrcEditStartPos,
             -1,-1,-1,[ofUseCache,ofDoNotLoadResource,ofVirtualFile,ofRegularFile])
      <>mrOk then
      begin
        DoAbort;
        exit;
      end;
      // select found text
      if not FileIsOpenInSourceEditor then
        RaiseGDBException('inconsistency');
      SrcEdit.SelectText(SrcEditStartPos.Y,SrcEditStartPos.X,
                         SrcEditEndPos.Y,SrcEditEndPos.X);
      SrcEdit.AskReplace(nil,SrcEdit.Selection,AReplace,
                         SrcEditStartPos.Y,SrcEditStartPos.X,Action);
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
      GetSrcEditPos;
      SrcEdit.SelectText(SrcEditStartPos.Y,SrcEditStartPos.X,
                         SrcEditEndPos.Y,SrcEditEndPos.X);
      SrcEdit.Selection:=AReplace;

      // count total replacements and adjust offsets
      aLineCount:=LineEndCount(AReplace,aLastLineLength);
      //debugln(['DoReplaceLine Replace="',dbgstr(AReplace),'" aLineCount=',aLineCount,' aLastLineLength=',aLastLineLength]);
      if aLineCount>0 then begin
        // replaced with multiple lines
        LastReplaceColOffset:=aLastLineLength+1-FoundEndPos.X;
      end else begin
        if FoundStartPos.Y<>LastReplaceLine then
          LastReplaceColOffset:=0;
        // replaced with some words
        if FoundStartPos.Y=FoundEndPos.Y then begin
          // replaced some words with some words
          inc(LastReplaceColOffset,
                               aLastLineLength-(FoundEndPos.X-FoundStartPos.X));
        end else begin
          // replaced several lines with some words
          inc(LastReplaceColOffset,FoundStartPos.X+aLastLineLength-FoundEndPos.X);
        end;
      end;
      LastReplaceLine:=FoundEndPos.Y;

      Lines := '';
      for i := SrcEditStartPos.Y to SrcEditStartPos.Y + aLineCount do
        Lines := Lines + SrcEdit.Lines[i-1] + LineEnding;
      Lines:=ChompOneLineEndAtEnd(Lines);
      if (Progress<>nil)
      and (Progress.OnAddMatch<>nil) then begin
        Progress.OnAddMatch(TheFileName,
          Point(FoundStartPos.x, FoundStartPos.y + ReplaceLineOffset),
          SrcEdit.CursorTextXY,Lines);
      end;

      inc(ReplaceLineOffset,aLineCount-(FoundEndPos.Y-FoundStartPos.Y));
      //DebugLn(['DoReplaceLine FoundStartPos=',dbgs(FoundStartPos),' FoundEndPos=',dbgs(FoundEndPos),' aLastLineLength=',aLastLineLength,' LastReplaceLine=',LastReplaceLine,' LastReplaceColOffset=',LastReplaceColOffset,' ReplaceLineOffset=',ReplaceLineOffset]);
    end else begin
      // change text in memory/disk
      OriginalFile.LineColToPosition(FoundStartPos.Y,FoundStartPos.X,
                                     OriginalTextPos);
      GapLength:=OriginalTextPos-ReplacedTextOriginalPos;
      NewLength:=ReplacedTextLength+GapLength+length(AReplace);
      GrowNewText(NewLength);
      // copy the text between the last replacement and this replacement
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
      OriginalFile.LineColToPosition(FoundEndPos.Y,FoundEndPos.X,
                                     ReplacedTextOriginalPos);

      Lines:=copy(OriginalFile.GetLines(FoundStartPos.Y,FoundStartPos.Y), 1, FoundStartPos.X - 1) +
             AReplace +
             copy(OriginalFile.GetLines(FoundEndPos.Y,FoundEndPos.Y), FoundEndPos.x, MaxInt);
      Lines:=ChompOneLineEndAtEnd(Lines);
      aLineCount:=LineEndCount(AReplace,aLastLineLength);
      if aLineCount = 0 then aLastLineLength := aLastLineLength + FoundStartPos.X;
      if (Progress<>nil)
      and (Progress.OnAddMatch<>nil) then begin
        Progress.OnAddMatch(TheFileName,
          Point(FoundStartPos.x, FoundStartPos.y + ReplaceLineOffset),
          Point(aLastLineLength, FoundStartPos.Y + aLineCount + ReplaceLineOffset),
          Lines);
      end;

      inc(ReplaceLineOffset,aLineCount-(FoundEndPos.Y-FoundStartPos.Y));
    end;
  end;

  procedure CommitChanges;
  var
    GapLength: Integer;
    NewLength: Integer;
    NewText: string;
    CurResult: TModalResult;
  begin
    EndLocks;
    if (ReplacedText<>nil) then begin
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
            CurResult:=MessageDlg(lisCodeToolsDefsWriteError,
                                  Format(lisErrorWritingFile, [TheFileName]),
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
  Found: Boolean;
  Src: String;
  NewMatchStartPos: PtrInt;
  NewMatchEndPos: PtrInt;
begin
  //debugln(['SearchInText TheFileName=',TheFileName,' SearchFor=',SearchFor,'" ReplaceText=',ReplaceText,'"']);

  if (Progress<>nil) and Progress.Abort then exit(mrAbort);
  Result:=mrOk;

  OriginalFile:=nil;
  CaseFile:=nil;
  RE:=nil;
  SrcEdit:=nil;
  SrcEditValid:=false;
  PaintLockEnabled:=false;
  ReplacedText:=nil;
  ReplacedTextCapacity:=0;
  ReplacedTextLength:=0;
  ReplacedTextOriginalPos:=1;

  ReplaceLineOffset:=0;
  LastReplaceLine:=0;
  LastReplaceColOffset:=0;

  try
    FoundEndPos:= Point(0,0);
    TempSearch:= SearchFor;

    // load text (to save memory, do not use codetools cache system)
    if FileIsOpenInSourceEditor then begin
      OriginalFile:=TSourceLog.Create(SrcEdit.GetText(false));
    end else if TheFileName<>'' then begin
      OriginalFile:=TSourceLog.Create('');
      OriginalFile.LoadFromFile(TheFileName);
    end else begin
      OriginalFile:=TSourceLog.Create(TheText);
    end;
    if OriginalFile.Source='' then exit;

    CaseFile:=nil;

    if sesoRegExpr in Flags then begin
      // Setup the regular expression search engine
      RE:=TRegExpr.Create;
      RE.ModifierI:=not (sesoMatchCase in Flags);
      RE.ModifierM:=true;
      RE.ModifierS:=sesoMultiLine in Flags;
      Src:=OriginalFile.Source;
      if sesoWholeWord in Flags then
        RE.Expression:='\b'+SearchFor+'\b'
      else
        RE.Expression:=SearchFor;
    end else begin
      // convert case if necessary
      if not (sesoMatchCase in Flags) then begin
        CaseFile:=TSourceLog.Create(UpperCaseStr(OriginalFile.Source));
        TempSearch:=UpperCaseStr(TempSearch);
        Src:=CaseFile.Source;
      end else
        Src:=OriginalFile.Source;
    end;

    //debugln(['TheFileName=',TheFileName,' len=',OriginalFile.SourceLength,' Cnt=',OriginalFile.LineCount,' TempSearch=',TempSearch]);
    ProcessMessages;

    NewMatchEndPos:=1;
    repeat
      Found:=false;
      if sesoRegExpr in Flags then begin
        // search the text for regular expression
        RE.InputString:=Src;
        if RE.ExecPos(NewMatchEndPos) then begin
          Found:=true;
          NewMatchStartPos:=RE.MatchPos[0];
          NewMatchEndPos:=NewMatchStartPos+RE.MatchLen[0];
        end;
      end else begin
        // search for normal text
        if SearchNextInText(PChar(TempSearch),length(TempSearch),
                            PChar(Src),length(Src),
                            NewMatchEndPos-1,NewMatchStartPos,NewMatchEndPos,
                            sesoWholeWord in Flags,sesoMultiLine in Flags)
        then begin
          Found:=true;
          inc(NewMatchStartPos);
          inc(NewMatchEndPos);
        end;
      end;
      
      if Found then begin
        // found => convert position, report and/or replace
        OriginalFile.AbsoluteToLineCol(NewMatchStartPos,
                                       FoundStartPos.Y,FoundStartPos.X);
        OriginalFile.AbsoluteToLineCol(NewMatchEndPos,
                                       FoundEndPos.Y,FoundEndPos.X);
        //DebugLn(['SearchInText NewMatchStartPos=',NewMatchStartPos,' NewMatchEndPos=',NewMatchEndPos,' FoundStartPos=',dbgs(FoundStartPos),' FoundEndPos=',dbgs(FoundEndPos),' Found="',dbgstr(copy(Src,NewMatchStartPos,NewMatchEndPos-NewMatchStartPos)),'" Replace=',sesoReplace in Flags]);
        if sesoReplace in Flags then begin
          DoReplaceLine
        end else begin
          if (Progress<>nil)
          and (Progress.OnAddMatch<>nil) then begin
            Lines:=OriginalFile.GetLines(FoundStartPos.Y,FoundEndPos.Y);
            Lines:=ChompOneLineEndAtEnd(Lines);
            if (Progress<>nil)
            and (Progress.OnAddMatch<>nil) then begin
              Progress.OnAddMatch(TheFileName,FoundStartPos,FoundEndPos,Lines);
            end;
          end;
        end;
      end else begin
        // not found
        break;
      end;

      // check abort
      if (Result=mrAbort) then begin
        exit;
      end;
      
    until false;
  finally
    CommitChanges;
    if OriginalFile=CaseFile then
      CaseFile:=nil;
    FreeAndNil(OriginalFile);
    FreeAndNil(CaseFile);
    FreeAndNil(RE);
  end;
end;//SearchFile


{ TSearchProgressForm }

procedure TSearchProgressForm.btnAbortCLICK(Sender: TObject);
begin
  Progress.Abort:= true;
end;

procedure TSearchProgressForm.SearchFormCREATE(Sender: TObject);
  Function MaxWidth(Labs : array of TLabel) : integer;
  var i,w : integer;
  begin
    Result:=0;
    for i:=low(Labs) to high(Labs) do
      begin
          w:=Canvas.TextWidth(Labs[i].Caption);
          if Result<w then
             Result:=w;
      end;
  end;

var NewX : integer;
begin
  //Set Defaults
  MatchesLabel.Caption:=lissMatches;
  SearchingLabel.Caption:=lissSearching;
  SearchTextLabel.Caption:=lissSearchText;
  NewX:=MatchesLabel.Left+MaxWidth([MatchesLabel,SearchingLabel,SearchTextLabel])+10;
  lblMatches.Left:=NewX;
  lblProgress.Left:=NewX;
  lblSearchText.Left:=NewX;

  Caption:=dlgSearchCaption;
  btnCancel.Caption:=lisCancel;

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
  fWasActive:= false;
end;

procedure TSearchProgressForm.OnAddMatch(const Filename: string; const StartPos,
  EndPos: TPoint; const Lines: string);
var
  MatchLen: Integer;
  TrimmedMatch: LongInt;
  TrimmedLines: String;
  LastLineLen: integer;
begin
  LineEndCount(Lines,LastLineLen);
  MatchLen:=length(Lines)-(LastLineLen+1-EndPos.X)-StartPos.X+1;
  if MatchLen<1 then MatchLen:=1;
  //DebugLn(['TSearchForm.OnAddMatch length(Lines)=',length(Lines),' LastLineLen=',LastLineLen,' MatchLen=',MatchLen]);
  TrimmedMatch:=StartPos.X;
  TrimmedLines:=TrimLinesAndAdjustPos(Lines,TrimmedMatch);
  //DebugLn(['TSearchForm.OnAddMatch StartPos=',dbgs(StartPos),' EndPos=',dbgs(EndPos),' Lines="',Lines,'" Trimmed="',TrimmedLines,'" TrimmedMatch=',TrimmedMatch]);
  SearchResultsView.AddMatch(fResultsWindow.PageIndex,FileName,StartPos,EndPos,
                             TrimmedLines, TrimmedMatch, MatchLen);
  UpdateMatches;
end;

procedure TSearchProgressForm.FormClose(Sender: TObject; var CloseAction:
  TCloseAction);
begin
  fWasActive:= Active;
end;

procedure TSearchProgressForm.FormShow(Sender: TObject);
begin
  fWasActive:= true;
end;

procedure TSearchProgressForm.SearchFormDESTROY(Sender: TObject);
begin
  FreeAndNil(fProgress);
end;

procedure TSearchProgressForm.SetOptions(TheOptions: TLazFindInFileSearchOptions);
begin
  SetFlag(sesoWholeWord,fifWholeWord in TheOptions);
  SetFlag(sesoReplace,fifReplace in TheOptions);
  SetFlag(sesoReplaceAll,fifReplaceAll in TheOptions);
  SetFlag(sesoMatchCase,fifMatchCase in TheOptions);
  SetFlag(sesoRegExpr,fifRegExpr in TheOptions);
  SetFlag(sesoMultiLine,fifMultiLine in TheOptions);
  fRecursive:= (fifIncludeSubDirs in TheOptions);
  fSearchProject:= (fifSearchProject in TheOptions);
  fSearchOpen:= (fifSearchOpen in TheOptions);
  fSearchActive:= (fifSearchActive in TheOptions);
  fSearchFiles:= (fifSearchDirectories in TheOptions);
end;//SetOptions

function TSearchProgressForm.GetOptions: TLazFindInFileSearchOptions;
begin
  Result:=[];
  if sesoWholeWord in fFlags then include(Result,fifWholeWord);
  if sesoMatchCase in fFlags then include(Result,fifMatchCase);
  if sesoReplace in fFlags then include(Result,fifReplace);
  if sesoReplaceAll in fFlags then include(Result,fifReplaceAll);
  if sesoRegExpr in fFlags then include(Result,fifRegExpr);
  if sesoMultiLine in fFlags then include(Result,fifMultiLine);
  if fRecursive then include(Result,fifIncludeSubDirs);
  if fSearchProject then include(Result, fifSearchProject);
  if fSearchOpen then include(Result,fifSearchOpen);
  if fSearchActive then include(Result,fifSearchActive);
  if fSearchFiles then include(Result,fifSearchDirectories);
end;//GetOptions

function TSearchProgressForm.DoSearch: integer;
// Search in all files and then return the number of found items.
begin
  Result:= 0;
  PromptOnReplace:=true;
  fAborting:=false;
  Progress.Abort:=false;
  lblSearchText.Caption:= fSearchFor;
  fMatches:= 0;
  if Assigned(fResultsList) then
  begin
    if not fResultsListUpdating then begin
      fResultsList.BeginUpdate;
      fResultsListUpdating:=true;
    end;
    try
      if fSearchFiles then
        DoFindInFiles(fDirectories);
      if fSearchProject or fSearchOpen or fSearchActive then
        DoFindInSearchList;
      if Assigned(fResultsList) then begin
        Result:=fResultsList.Count;     // Return the real item count.
        if fResultsList.Count = 0 then  // Add a note to the list if no items found.
          fResultsList.Add(Format(lisUESearchStringNotFound,[dbgstr(fSearchFor)]));
      end;
    finally
      if fResultsListUpdating then begin
        fResultsListUpdating:=false;
        fResultsList.EndUpdate;
      end;
    end;
  end;//if
  Close;
end;//DoSearch

type

  { TLazFileSearcher }

  TLazFileSearcher = class(TFileSearcher)
  private
    FParent: TSearchProgressForm;
    procedure CheckAbort;
  protected
    procedure DoDirectoryEnter; override;
    procedure DoDirectoryFound; override;
    procedure DoFileFound; override;
  public
    constructor Create(AParent: TSearchProgressForm);
    destructor Destroy; override;
  end;

{ TLazFileSearcher }

procedure TLazFileSearcher.CheckAbort;
begin
  if FParent.Progress.Abort then
  begin
    if not FParent.FAborting then
    begin
      FParent.FAborting := True;
      FParent.FResultsList.Insert(0, FParent.FAbortString);
    end;

    Stop;
  end;
end;

procedure TLazFileSearcher.DoDirectoryEnter;
begin
  CheckAbort;
end;

procedure TLazFileSearcher.DoDirectoryFound;
begin
  CheckAbort;
end;

procedure TLazFileSearcher.DoFileFound;
var
  F: String;
begin
  F := FileName;
  if FileIsTextCached(F) then
  begin
    FParent.UpdateProgress(F);
    FParent.SearchFile(F);
  end;
  CheckAbort;
end;

constructor TLazFileSearcher.Create(AParent: TSearchProgressForm);
begin
  inherited Create;
  FParent := AParent;
end;

destructor TLazFileSearcher.Destroy;
begin
  FParent:=nil;
  inherited Destroy;
end;

{ TSearchProgressForm }

procedure TSearchProgressForm.DoFindInFiles(ADirectories: string);
var
  Searcher: TLazFileSearcher;
  SearchPath: String;
  p: Integer;
  Dir: String;
begin
  // if we have a list and a valid directory
  SearchPath:='';
  p:=1;
  repeat
    Dir:=GetNextDirectoryInSearchPath(ADirectories,p);
    if Dir='' then break;
    if DirPathExists(Dir) then
      SearchPath:=MergeSearchPaths(SearchPath,Dir);
  until false;
  if SearchPath='' then
    exit;
  Searcher := TLazFileSearcher.Create(Self);
  try
    Searcher.Search(SearchPath, FMask, FRecursive);
  finally
    Searcher.Free;
  end;
end;

procedure TSearchProgressForm.DoFindInSearchList;
var
  i: integer;
begin
  if Assigned(fSearchFileList) then
  begin
    for i:= 0 to fSearchFileList.Count -1 do
    begin
      UpdateProgress(fSearchFileList[i]);
      SearchFile(fSearchFileList[i]);
    end;
  end;
end;

procedure TSearchProgressForm.SetResultsList(const AValue: TStrings);
begin
  if fResultsList=AValue then exit;
  if fResultsListUpdating then
  begin
    fResultsList.EndUpdate;
    fResultsListUpdating:=false;
  end;
  fResultsList:=AValue;
end;

procedure TSearchProgressForm.UpdateMatches;
begin
  inc(fMatches);
  //DebugLn(['TSearchForm.UpdateMatches ',lblMatches.Caption]);
  lblMatches.Caption:=IntToStr(fMatches);
end;

procedure TSearchProgressForm.UpdateProgress(FileName: string);
const
  UpdateAfterTicks = 200; // update not more than 5 times per second
var
  DisplayFileName: string;
  ShorterFileName: String;
  CurTick: DWORD;
begin
  CurTick:=GetTickCount;
  if Abs(int64(CurTick)-int64(fLastUpdateProgress))<UpdateAfterTicks then
    exit;
  fLastUpdateProgress:=CurTick;

  DisplayFileName := FileName;
  //DebugLn(['TSearchForm.UpdateProgress DisplayFileName="',dbgstr(DisplayFileName),'"']);
  lblProgress.Caption:= DisplayFileName;
  while (lblProgress.Left + lblProgress.Width)> lblProgress.Parent.ClientWidth-12 do
  begin
    ShorterFileName:= PadAndShorten(DisplayFileName);
    if ShorterFileName=DisplayFileName then break;
    DisplayFileName:=ShorterFileName;
    //DebugLn(['TSearchForm.UpdateProgress Padded DisplayFileName="',dbgstr(DisplayFileName),'"']);
    lblProgress.Caption := DisplayFileName;
  end;
end;

procedure TSearchProgressForm.SearchFile(const aFilename: string);
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

procedure TSearchProgressForm.SetFlag(Flag: TSrcEditSearchOption; AValue: boolean);
begin
  if AValue then
    Include(fFlags,Flag)
  else
    Exclude(fFlags,Flag);
end;

procedure TSearchProgressForm.DoSearchAndAddToSearchResults;
var
  ListPage: TTabSheet;
  Cnt: integer;
  State: TIWGetFormState;
begin
  Cnt:= 0;
  LazarusIDE.DoShowSearchResultsView(iwgfShow);
  ListPage:=SearchResultsView.AddSearch(SearchText,SearchText,
                            ReplaceText,SearchDirectories,SearchMask,SearchOptions);
  try
    (* BeginUpdate prevents ListPage from being closed,
      other pages can still be closed or inserted, so PageIndex can change *)
    SearchResultsView.BeginUpdate(ListPage.PageIndex);
    ResultsList:= SearchResultsView.Items[ListPage.PageIndex];
    ResultsList.Clear;
    ResultsWindow:= ListPage;
    try
      Show; // floating window, not dockable
      Cnt:= DoSearch;
    except
      on E: ERegExpr do
        IDEMessageDialog(lisUEErrorInRegularExpression, E.Message,mtError,
                   [mbCancel]);
    end;
  finally
    ListPage.Caption:= Format('%s (%d)',[ListPage.Caption,Cnt]);
    SearchResultsView.EndUpdate(ListPage.PageIndex);
    // show, but bring to front only if Search Progress dialog was active
    if fWasActive then
      State:=iwgfShowOnTop
    else
      State:=iwgfShow;
    LazarusIDE.DoShowSearchResultsView(State);
  end;
end;

procedure TSearchProgressForm.DoSearchOpenFiles;
var
  i: integer;
  TheFileList: TStringList;
  SrcEdit: TSourceEditorInterface;
begin
  try
    TheFileList:= TStringList.Create;
    for i:= 0 to SourceEditorManagerIntf.UniqueSourceEditorCount -1 do
    begin
      //only if file exists on disk
      SrcEdit := SourceEditorManagerIntf.UniqueSourceEditors[i];
      if FilenameIsAbsolute(SrcEdit.FileName)
      and (not FileExistsCached(SrcEdit.FileName)) then
        continue;
      TheFileList.Add(SrcEdit.FileName);
    end;
    SearchFileList:= TheFileList;
    DoSearchAndAddToSearchResults;
  finally
    FreeAndNil(TheFileList);
  end;
end;

procedure TSearchProgressForm.DoSearchActiveFile;
var
  TheFileList: TStringList;
begin
  try
    TheFileList:= TStringList.Create;      // Add a single file to the list
    TheFileList.Add(SourceEditorManagerIntf.ActiveEditor.FileName);
    SearchFileList:= TheFileList;
    DoSearchAndAddToSearchResults;
  finally
    FreeAndNil(TheFileList);
  end;
end;

procedure TSearchProgressForm.DoSearchDir;
begin
  SearchFileList:= Nil;
  DoSearchAndAddToSearchResults;
end;

procedure TSearchProgressForm.DoSearchProject(AProject: TProject);
var
  AnUnitInfo:  TUnitInfo;
  TheFileList: TStringList;
begin
  try
    TheFileList:= TStringList.Create;
    AnUnitInfo:=AProject.FirstPartOfProject;
    while AnUnitInfo<>nil do begin
      //Only if file exists on disk.
      if FilenameIsAbsolute(AnUnitInfo.FileName)
      and FileExistsCached(AnUnitInfo.FileName) then
        TheFileList.Add(AnUnitInfo.FileName);
      AnUnitInfo:=AnUnitInfo.NextPartOfProject;
    end;
    SearchFileList:= TheFileList;
    DoSearchAndAddToSearchResults;
  finally
    FreeAndNil(TheFileList);
  end;
end;

function TSearchProgressForm.PadAndShorten(FileName: string): string;
var
  FoundAt: integer;
begin
  FoundAt:= System.Pos(PathDelim,FileName);
  if FoundAt<1 then begin
    Result := Filename;
  end else begin
    Result:= fPad + copy(FileName,FoundAt+1,Length(FileName));
  end;
end;//PadAndShorten

end.

