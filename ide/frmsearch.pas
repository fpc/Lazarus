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
  Classes, SysUtils, LResources, LCLType, LCLIntf, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, StdCtrls, Buttons, FileCtrl,
  // synedit
  SynRegExpr,
  // ide
  LazarusIDEStrConsts, InputHistory, FindInFilesDlg, SearchResultView;


type
  TSearchForm = class(TForm)
    btnCancel: TBUTTON;
    Label1: TLABEL;
    Label2: TLABEL;
    Label4: TLABEL;
    lblMatches: TLABEL;
    lblProgress: TLABEL;
    lblSearchText: TLABEL;
    Panel2: TPANEL;
    procedure SearchFormCREATE(Sender: TObject);
    procedure SearchFormDESTROY(Sender: TObject);
    procedure btnAbortCLICK(Sender: TObject);
  private
    { Private declarations }
    fSearchFor: String;
    fWholeWord: Boolean;
    fCaseSensitive: Boolean;
    fRegExp: Boolean;
    fTheDirectory: string;
    fmask: string;
    frecursive: boolean;
    fParsedMasks: TStringList; //Holds the parsed masks.
    fMatches: longint;
    fAbort: boolean;
    fAbortString: string;
    fAborting: boolean;
    fCharWidth: longint;
    fSearchProject: boolean;
    fSearchOpen: boolean;
    fSearchFileList: TStringList;
    fSearchFiles: boolean;
    fResultsList: TStrings;
    fResultsWindow: integer;
    fPad: string;
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

const
  {$IFDEF Win32}
  FindMask = '*.*';
  {$ELSE}
  FindMask = '*';
  {$ENDIF}

{ TSearchForm }

procedure TSearchForm.btnAbortCLICK(Sender: TObject);
begin
   fAbort:= true;
end;

procedure TSearchForm.SearchFormCREATE(Sender: TObject);
var
  FormFont: THandle;
  tm : TTextmetric;
  DC: HDC;
begin
  //Set Defaults
  fWholeWord:= false;
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
  try
    DC:= GetDC(0);
    FormFont:= SelectObject(DC, Font.Handle);
    GetTextMetrics(DC, tm);
    fCharWidth:= tm.tmAveCharWidth;
  finally
    SelectObject(DC, FormFont);
    ReleaseDC(0, DC);
  end;//finally
end;//SearchFormCreate

procedure TSearchForm.SearchFormDESTROY(Sender: TObject);
begin
  FreeAndNil(fParsedMasks);
end;

procedure TSearchForm.SetOptions(TheOptions: TLazFindInFileSearchOptions);
begin
  fWholeWord:= (fifWholeWord in TheOptions);
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
  if fRegExp then include(Result,fifRegExpr);
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
  {DoNormalSearch is called if the search is not a Regular expression}
  procedure DoNormalSearch;
  var
    ThisFile: TStringList;   //The File being searched
    Lines:      integer;     //Loop Counter
    Match:      integer;     //Position of match in line.
    StartWord:  boolean;     //Does the word start with a sperator charater?
    EndWord:    boolean;     //Does the word end with a seperator charater?
    TheLine:    string;      //Temp Storage for the current line in the file.
    TempSearch: string;      //Temp Storage for the search string.
    TheHeader: string;
    MatchLen: integer;

 const
  WordBreakChars = ['.', ',', ';', ':', '"', '''', '!', '?', '[', ']', '(',
                ')', '{', '}', '^', '-', '=', '+', '*', '/', '\', '|', ' '];
  begin
    try
      ThisFile:= TStringList.Create;
      MatchLen:= Length(fSearchFor);
      //if this is not a regular expression search
      if (Not fCaseSensitive) then
        TempSearch:= UpperCase(fSearchFor)
      else
        TempSearch:= fSearchFor;
      ThisFile.LoadFromFile(TheFileName);
      for Lines:= 0 to ThisFile.Count -1 do
      begin
        Application.ProcessMessages;
        TheLine:= Trim(ThisFile.Strings[Lines]);
        if not fCaseSensitive then
          TheLine:= UpperCase(TheLine);
        Match:= pos(TempSearch,TheLine);
        //look at the char before and after the match to see if they are in
        //our list of word seperator charaters.
        if fWholeWord and (Match > 0) then
        begin //is this the first word on the line or does the word start with
              //one of the word seperator charaters.
          if (Match = 1) or (TheLine[Match-1] in WordBreakChars) then
            StartWord := True
          else
            StartWord := False;
          EndWord:= False;
          if StartWord then // evaluate end only if start is true.
          begin
            if (Match + length(TempSearch) >= length(TheLine)) or
                (TheLine[Match + Length(TempSearch)] in WordBreakChars) then
              EndWord:= True
          end;//if
          if StartWord And EndWord then
          begin
            TheHeader:= TheFileName +'('+IntToStr(lines+1)+ ','+ IntToStr(match)
                        +')' + ' ';
            SearchResultsView.AddMatch(fResultsWindow,
                                      TheHeader + Trim(ThisFile.Strings[Lines]),
                                      match + Length(TheHeader),
                                      MatchLen);
            UpdateMatches;
          end;//if
        end;//if
        if not fWholeWord and (Match > 0) then
        begin
          TheHeader:= TheFileName +'('+IntToStr(lines+1)+ ','+ IntToStr(match)
                      +')' + ' ';
          SearchResultsView.AddMatch(fResultsWindow,
                                     TheHeader + Trim(ThisFile.Strings[Lines]),
                                     match + Length(TheHeader),
                                     MatchLen);
          UpdateMatches;
        end;//if
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
      FreeAndNil(ThisFile);
    end;//Try-finally
  end;//DoNormalSearch

  {DoRegExpSearch is called if the search is a regular expression}
  procedure DoRegExpSearch;
  var
    ThisFile:   TStringList;
    Lines:      integer;     //Loop Counter
    Match:      integer;     //Position of match in line.
    MatchLen:   integer;
    TheLine:    string;      //Temp Storage for the current line in the file.
    TheHeader:  string;
    RE:         TRegExpr;    //Regular expression search engin
  begin
     try
      ThisFile:= TStringList.Create;
      RE:= TRegExpr.Create;
      //Set up the search engine.
      With RE do
      begin
        Expression:= fSearchFor;
        ModifierI:= not fCaseSensitive;
        ModifierM:= False;  //for now
      end;//with
      ThisFile.LoadFromFile(TheFileName);
      for Lines:= 0 to ThisFile.Count - 1 do
      begin
        Application.ProcessMessages;
        TheLine:= Trim(ThisFile[Lines]);
        if RE.Exec(TheLine) then
        begin
          Match:= RE.MatchPos[0];
          MatchLen:= Re.MatchLen[0];
          
          TheHeader:= TheFileName +'('+IntToStr(lines+1)+ ','+ IntToStr(match)
                      +')' + ' ';
          SearchResultsView.AddMatch(fResultsWindow,TheHeader + TheLine,
                                     match + Length(TheHeader),
                                     MatchLen);
          UpdateMatches;
        end;//if
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
      FreeAndNil(ThisFile);
      FreeAndNil(RE);
    end;//try-finally
  end;//DoRegExpSearch

{Start SearchFile ============================================================}
begin
  try
    fResultsList.BeginUpdate;
    if not fRegExp then
      DoNormalSearch
    else
      DoRegExpSearch;
  finally
    fResultsList.EndUpdate;
  end;//finally
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
          if (FileInfo.Name='.') or (FileInfo.Name='..')
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
      if (SysUtils.FindFirst(TempDir+FindMask,faAnyFile,FileInfo)=0) then
      begin
        repeat
          // check if directory and not special file
          if ((faDirectory and FileInfo.Attr)>0)
            and (FileInfo.Name<>'.') and (FileInfo.Name<>'..')
            then DoFindInFiles(TempDir + FileInfo.Name);
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
  //Position Tracking wihtin the string.
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
    fParsedMasks.Add(FindMask) //OS Independent Mask
  end;//else
end;//ParseMask


procedure TSearchForm.UpdateMatches;
begin
  Application.ProcessMessages;
  inc(fMatches);
  lblMatches.Caption:= IntToStr(fMatches);
end;//UpdateMatches


procedure TSearchForm.UpdateProgress(FileName: string);
var
  DisplayFileName: string;
begin
  DisplayFileName:= FileName;
  While ((Length(DisplayFileName) * fCharWidth) >= lblProgress.Width) do
  begin
    DisplayFileName:= PadAndShorten(DisplayFileName);
  end;//while
  lblProgress.Caption:= DisplayFileName;
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

