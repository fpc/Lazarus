{
 /***************************************************************************
                       searchresultviewView.pp - SearchResult view
                       -------------------------------------------
                   TSearchResultsView is responsible for displaying the
                   Search Results of a find operation.


                   Initial Revision  : Sat Nov 8th 2003


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
unit SearchResultView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, Buttons, LCLType,
  IDEOptionDefs, LazarusIDEStrConsts, EnvironmentOpts, InputHistory,
  IDEProcs, FindInFilesDlg, Project, MainIntf;

type
  { TLazSearchMatchPos }
  
  TLazSearchMatchPos = class(TObject)
  private
    FFileEndPos: TPoint;
    FFilename: string;
    FFileStartPos: TPoint;
    fMatchStart: integer;
    fMatchLen: integer;
    FShownFilename: string;
    FTheText: string;
  public
    property MatchStart: integer read fMatchStart write fMatchStart;// start in TheText
    property MatchLen: integer read fMatchLen write fMatchLen; // length in TheText
    property Filename: string read FFilename write FFilename;
    property FileStartPos: TPoint read FFileStartPos write FFileStartPos;
    property FileEndPos: TPoint read FFileEndPos write FFileEndPos;
    property TheText: string read FTheText write FTheText;
    property ShownFilename: string read FShownFilename write FShownFilename;
  end;//TLazSearchMatchPos


  { TLazSearch }

  TLazSearch = Class(TObject)
  private
    FReplaceText: string;
    fSearchString: string;
    fSearchOptions: TLazFindInFileSearchOptions;
    fSearchDirectory: string;
    fSearchMask: string;
  public
    property SearchString: string read fSearchString write fSearchString;
    property ReplaceText: string read FReplaceText write FReplaceText;
    property SearchOptions: TLazFindInFileSearchOptions read fSearchOptions
                                                        write fSearchOptions;
    property SearchDirectory: string read fSearchDirectory
                                     write fSearchDirectory;
    property SearchMask: string read fSearchMask write fSearchMask;
  end;//TLazSearch


  { TLazSearchResultLB }

  TLazSearchResultLB = class(TCustomListBox)
  private
    fSearchObject: TLazSearch;
    FSkipped: integer;
    fUpdateStrings: TStrings;
    fBackUpStrings: TStrings;
    fUpdating: boolean;
    fUpdateCount: integer;
    fShortenPathNeeded: boolean;
    FSearchInListPhrases: string;
    fFiltered: Boolean;
    procedure SetSkipped(const AValue: integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property SearchObject: TLazSearch read fSearchObject write fSearchObject;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure ShortenPaths;
    procedure FreeObjects(var slItems: TStrings);
    function BeautifyLine(const Filename: string; X, Y: integer;
                          const Line: string): string;
    function BeautifyLine(SearchPos: TLazSearchMatchPos): string;
    property BackUpStrings: TStrings read fBackUpStrings write fBackUpStrings;
    property Filtered: Boolean read fFiltered write fFiltered;
    property SearchInListPhrases: string read FSearchInListPhrases write FSearchInListPhrases;
    property UpdateItems: TStrings read fUpdateStrings write fUpdateStrings;
    property UpdateState: boolean read fUpdating;
    property Skipped: integer read FSkipped write SetSkipped;
  end;


  { TSearchResultsView }

  TSearchResultsView = class(TForm)
    btnSearchAgain: TButton;
    ClosePageButton: TButton;
    ResultsNoteBook: TNotebook;
    gbSearchPhraseInList: TGroupBox;
    edSearchInList: TEdit;
    bnForwardSearch: TButton;
    bnResetResults: TButton;
    bnFilter: TButton;
    procedure ClosePageButtonClick(Sender: TObject);
    procedure Form1Create(Sender: TObject);
    procedure ListBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ResultsNoteBookClosetabclicked(Sender: TObject);
    procedure SearchResultsViewDestroy(Sender: TObject);
    procedure btnSearchAgainClick(Sender: TObject);
    procedure ListboxDrawitem(Control: TWinControl; Index: Integer;
                              ARect: TRect; State: TOwnerDrawState);
    procedure LazLBShowHint(Sender: TObject; HintInfo: PHintInfo);
    procedure LazLBMousemove(Sender: TObject; Shift: TShiftState;
                             X, Y: Integer);
    Procedure LazLBMouseWheel(Sender: TObject; Shift: TShiftState;
                   WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure edSearchInListChange(Sender: TObject );
    procedure ResultsNoteBookPageChanged (Sender: TObject );
    procedure bnForwardSearchClick(Sender: TObject );
    procedure bnResetResultsClick(Sender: TObject );
    procedure edSearchInListKeyDown(Sender: TObject; var Key: Word;
                                    Shift: TShiftState );
    procedure bnFilterClick (Sender: TObject );
  private
    FMaxItems: integer;
    function BeautifyPageName(const APageName: string): string;
    function PageExists(const APageName: string): boolean;
    function GetPageIndex(const APageName: string): integer;
    function GetListBox(APageIndex: integer): TLazSearchResultLB;
    procedure ListBoxClicked(Sender: TObject);
    procedure ListBoxDoubleClicked(Sender: TObject);
    procedure SetItems(Index: Integer; Value: TStrings);
    function GetItems(Index: integer): TStrings;
                      fOnSelectionChanged: TNotifyEvent; fListBoxFont: TFont;
                      fMouseOverIndex: integer;
    procedure SetMaxItems(const AValue: integer);
  public
    function AddSearch(const ResultsName: string;
                       const SearchText: string;
                       const ReplaceText: string;
                       const ADirectory: string;
                       const AMask: string;
                       const TheOptions: TLazFindInFileSearchOptions): integer;
    function GetSourcePositon: TPoint;
    function GetSourceFileName: string;
    function GetSelectedText: string;
    function GetSelectedMatchPos: TLazSearchMatchPos;
    procedure BringResultsToFront(const APageName: string);
    procedure AddMatch(const APageIndex: integer;
                       const Filename: string; const StartPos, EndPos: TPoint;
                       const TheText: string;
                       const MatchStart: integer; const MatchLen: integer);
    procedure BeginUpdate(APageIndex: integer);
    procedure EndUpdate(APageIndex: integer);
    procedure Parse_Search_Phrases(var slPhrases: TStrings);
    procedure ClosePage(PageIndex: integer);
    property ListBoxFont: TFont read fListBoxFont write fListBoxFont;
    property OnSelectionChanged: TNotifyEvent read fOnSelectionChanged
                                              write fOnSelectionChanged;
    property Items[Index: integer]: TStrings read GetItems write SetItems;
    property MaxItems: integer read FMaxItems write SetMaxItems;
  end; 

var
  SearchResultsView: TSearchResultsView;

implementation

{ TSearchResultsView }

const
  MaxTextLen = 80;
  
function CopySearchMatchPos(var Src, Dest: TLazSearchMatchPos): Boolean;
begin
  Result := False;
  if ((Src = nil) or (Dest = nil)) then Exit;
  Dest.MatchStart := Src.MatchStart;
  Dest.MatchLen := Src.MatchLen;
  Dest.Filename := Src.Filename;
  Dest.FileStartPos := Src.FileStartPos;
  Dest.FileEndPos := Src.FileEndPos;
  Dest.TheText := Src.TheText;
  Dest.ShownFilename := Src.ShownFilename;
  Result := True;
end;
  
procedure TSearchResultsView.Form1Create(Sender: TObject);
var
  ALayout: TIDEWindowLayout;
begin
  FMaxItems:=500;
  
  ResultsNoteBook.Options:= ResultsNoteBook.Options+[nboShowCloseButtons];
  ResultsNoteBook.Update;

  Caption:=lisMenuViewSearchResults;
  btnSearchAgain.Caption:=lisSearchAgain;
  ClosePageButton.Caption:=lisSRClosePage;

  Name := NonModalIDEWindowNames[nmiwSearchResultsViewName];
  ALayout:=EnvironmentOptions.IDEWindowLayoutList.
                                          ItemByEnum(nmiwSearchResultsViewName);
  ALayout.Form:=TForm(Self);
  ALayout.Apply;
  fListBoxFont:= TFont.Create;
  fListBoxFont.Name:= 'courier';
  fListBoxFont.Height:= 12;
  fListBoxFont.Style:= [];
  fOnSelectionChanged:= nil;
  ShowHint:= True;
  fMouseOverIndex:= -1;
end;//Create

procedure TSearchResultsView.ClosePageButtonClick(Sender: TObject);
begin
  ClosePage(ResultsNoteBook.PageIndex);
end;

{Keeps track of the Index of the Item the mouse is over, Sets ShowHint to true
if the Item length is longer than the Listbox client width.}
procedure TSearchResultsView.LazLBMousemove(Sender: TObject; Shift: TShiftState;
                                       X, Y: Integer);
begin
  if Sender is TLazSearchResultLB then
  begin
    with Sender as TLazSearchResultLB do
    begin
      fMouseOverIndex:= GetIndexAtY(Y);
      if (fMouseOverIndex > -1) and (fMouseOverIndex < Items.Count) then
      begin
        if (Canvas.TextWidth(Items[fMouseOverIndex]) > Width) then
          ShowHint:= True
        else
          ShowHint:= False;
      end;//if
    end;//with
  end;//
end;//LazLBMousemove

{Keep track of the mouse position over the list box when the wheel is used}
procedure TSearchResultsView.LazLBMouseWheel(Sender: TObject;
                                             Shift: TShiftState;
                                             WheelDelta: Integer;
                                             MousePos: TPoint;
                                             var Handled: Boolean);
begin
  LazLBMouseMove(Sender,Shift,MousePos.X, MousePos.Y);
  Handled:= false;
end;//LazLBMouseWheel

procedure TSearchResultsView.edSearchInListChange (Sender: TObject );
var CurrentLB: TLazSearchResultLB;
begin
 CurrentLB := GetListBox(ResultsNoteBook.PageIndex);
 if Assigned(CurrentLB) then
  CurrentLB.SearchInListPhrases := edSearchInList.Text;
end;

procedure TSearchResultsView.ResultsNoteBookPageChanged (Sender: TObject );
var CurrentLB: TLazSearchResultLB;
begin
 CurrentLB := GetListBox(ResultsNoteBook.PageIndex);
 if Assigned(CurrentLB) then
  edSearchInList.Text := CurrentLB.SearchInListPhrases;
end;

procedure TSearchResultsView.bnForwardSearchClick (Sender: TObject );
var CurrentLB: TLazSearchResultLB;
    slPhrases: TStrings;
    i, j, iCurrentIndex: Integer;
    S: string;
begin
 CurrentLB := GetListBox(ResultsNoteBook.PageIndex);
 if Assigned(CurrentLB) then
  begin
   if (Length(edSearchInList.Text) = 0) then Exit;//No Search Phrases specified.
   if (CurrentLB.Items.Count <= 0) then Exit;
   slPhrases := TStringList.Create;
   try
    iCurrentIndex := CurrentLB.ItemIndex + 1;
    if (iCurrentIndex > CurrentLB.Items.Count) then iCurrentIndex := CurrentLB.Items.Count;
    if (iCurrentIndex < 0) then iCurrentIndex := 0;//Set to 1st list-item if none are selected
    //Parse Phrases
    Parse_Search_Phrases(slPhrases);
    if (slPhrases.Count > 0) then
     begin
      for i:=iCurrentIndex to CurrentLB.Items.Count-1 do
       begin
        S := UpperCase(CurrentLB.Items[i]);//for case-insensitive search
        for j:=0 to slPhrases.Count-1 do
         begin
          if (Pos(slPhrases[j], S) <> 0) then
           begin
            CurrentLB.ItemIndex := i;//Set listbox's itemindex
            CurrentLB.MakeCurrentVisible;
            Exit;//Found what we looking for, exit
           end;//End if (Pos(slPhrases[j], CurrentLB.Items[i]) <> 0)
         end;//End for-loop j
       end;//End for-loop i
     end;//End if if (slPhrases.Count > 0)
   finally
    FreeAndNil(slPhrases);
    edSearchInList.SetFocus;
   end;//End try-finally
  end;//End if Assigned(CurrentLB)
end;

procedure TSearchResultsView.bnResetResultsClick (Sender: TObject );
var i: Integer;
    oObject: TObject;
    CurrentLB: TLazSearchResultLB;
    mpMatchPos, mpOrgMatchPos: TLazSearchMatchPos;
begin
 CurrentLB := GetListBox(ResultsNoteBook.PageIndex);
 try
  if assigned(CurrentLB) and CurrentLB.Filtered then
   begin
    if (CurrentLB.Items.Count > 0) then
     begin
      CurrentLB.FreeObjects(CurrentLB.Items);//Free the objects
      CurrentLB.Items.Clear;
     end;//End if (CurrentLB.Items.Count > 0)

    if (CurrentLB.BackUpStrings.Count > 0) then
     begin
      for i:=0 to CurrentLB.BackUpStrings.Count-1 do
       begin
        oObject := CurrentLB.BackUpStrings.Objects[i];
        if not (oObject is TLazSearchMatchPos) then Continue;
        mpOrgMatchPos := TLazSearchMatchPos(oObject);
        if Assigned(mpOrgMatchPos) then
         begin
          mpMatchPos := TLazSearchMatchPos.Create;
          if CopySearchMatchPos(mpOrgMatchPos, mpMatchPos) then
           CurrentLB.Items.AddObject(CurrentLB.BackUpStrings[i], mpMatchPos);
         end;//End if Assigned(mpOrgMatchPos)
       end;//End for-loop i
     end;//End if (CurrentLB.BackUpStrings.Count > 0)
    CurrentLB.Filtered := False;
   end;//End if CurrentLB.Filtered
 finally
  edSearchInList.SetFocus;
 end;//End try-finally
end;

procedure TSearchResultsView.edSearchInListKeyDown (Sender: TObject;
 var Key: Word; Shift: TShiftState );
begin
 if (Key = VK_RETURN) then
  bnForwardSearchClick(bnForwardSearch);
end;

procedure TSearchResultsView.bnFilterClick (Sender: TObject );
var CurrentLB: TLazSearchResultLB;
    mpMatchPos, mpOrgMatchPos: TLazSearchMatchPos;
    slPhrases: TStrings;
    i, j: Integer;
    S: string;
    oObject: TObject;
begin
 CurrentLB := GetListBox(ResultsNoteBook.PageIndex);
 if Assigned(CurrentLB) then
  begin
   if (Length(edSearchInList.Text) = 0) then Exit;//No Filter Phrases specified.
   slPhrases := TStringList.Create;
   try
    //Parse Phrases
    Parse_Search_Phrases(slPhrases);
    //BackUp Result List
    if not (CurrentLB.Filtered or (CurrentLB.BackUpStrings.Count > 0)) then
     begin
      if (CurrentLB.Items.Count <= 1) then Exit;
      for i:=0 to CurrentLB.Items.Count-1 do
       begin
        oObject := CurrentLB.Items.Objects[i];
        if not (oObject is TLazSearchMatchPos) then Continue;
        mpOrgMatchPos := TLazSearchMatchPos(CurrentLB.Items.Objects[i]);
        if Assigned(mpOrgMatchPos) then
         begin
          mpMatchPos := TLazSearchMatchPos.Create;
          if CopySearchMatchPos(mpOrgMatchPos, mpMatchPos) then
           CurrentLB.BackUpStrings.AddObject(CurrentLB.Items[i], mpMatchPos);
         end;//End if Assigned(mpOrgMatchPos)
       end;//End for-loop i
     end;//End if not (CurrentLB.Filtered or (CurrentLB.BackUpStrings.Count > 0))
     
    if (CurrentLB.BackUpStrings.Count <= 0) then Exit;//Empty list

    if (CurrentLB.Items.Count > 0) then
     begin
      CurrentLB.FreeObjects(CurrentLB.Items);//Free the objects
      CurrentLB.Items.Clear;//Clear the list
      //Clear update items as their objects are freed together with CurrentLB.Items
      CurrentLB.UpdateItems.Clear;
     end;//End if (CurrentLB.Items.Count > 0)

    if (slPhrases.Count > 0) then
     begin
      for i:=0 to CurrentLB.BackUpStrings.Count-1 do
       begin
        S := UpperCase(CurrentLB.BackUpStrings[i]);//for case-insensitive search
        for j:=0 to slPhrases.Count-1 do
         begin
          if (Pos(slPhrases[j], S) <> 0) then
           begin
            oObject := CurrentLB.BackUpStrings.Objects[i];
            if not (oObject is TLazSearchMatchPos) then Continue;
            mpOrgMatchPos := TLazSearchMatchPos(CurrentLB.BackUpStrings.Objects[i]);
            if Assigned(mpOrgMatchPos) then
             begin
              mpMatchPos := TLazSearchMatchPos.Create;
              if CopySearchMatchPos(mpOrgMatchPos, mpMatchPos) then
               CurrentLB.Items.AddObject(CurrentLB.BackUpStrings[i], mpMatchPos);
             end;//End if Assigned(mpOrgMatchPos)
            Break;
           end;//End if (Pos(slPhrases[j], S) <> 0)
         end;//End for-loop j
       end;//End for-loop i
      CurrentLB.Filtered := True;
     end;//End if if (slPhrases.Count > 0)
   finally
    FreeAndNil(slPhrases);
    edSearchInList.SetFocus;
    if (CurrentLB.Items.Count > 0) then CurrentLB.ItemIndex := 0;//Goto first item
   end;//End try-finally
  end;//End if Assigned(CurrentLB)
end;

function TSearchResultsView.BeautifyPageName(const APageName: string): string;
const
  MaxPageName = 25;
begin
  Result:=SpecialCharsToHex(APageName);
  if UTF8Length(Result)>MaxPageName then
    Result:=UTF8Copy(Result,1,15)+'...';
end;

procedure TSearchResultsView.AddMatch(const APageIndex: integer;
  const Filename: string; const StartPos, EndPos: TPoint;
  const TheText: string;
  const MatchStart: integer; const MatchLen: integer);
var
  CurrentLB: TLazSearchResultLB;
  SearchPos: TLazSearchMatchPos;
  ShownText: String;
begin
  CurrentLB:=GetListBox(APageIndex);
  if Assigned(CurrentLB) then
  begin
    if CurrentLB.UpdateState then begin
      if CurrentLB.UpdateItems.Count>=MaxItems then begin
        CurrentLB.Skipped:=CurrentLB.Skipped+1;
        exit;
      end;
    end else begin
      if CurrentLB.Items.Count>=MaxItems then begin
        CurrentLB.Skipped:=CurrentLB.Skipped+1;
        exit;
      end;
    end;
    SearchPos:= TLazSearchMatchPos.Create;
    SearchPos.MatchStart:=MatchStart;
    SearchPos.MatchLen:=MatchLen;
    SearchPos.Filename:=Filename;
    SearchPos.FileStartPos:=StartPos;
    SearchPos.FileEndPos:=EndPos;
    SearchPos.TheText:=TheText;
    SearchPos.ShownFilename:=SearchPos.Filename;
    ShownText:=CurrentLB.BeautifyLine(SearchPos);
    if CurrentLB.UpdateState then
      CurrentLB.UpdateItems.AddObject(ShownText, SearchPos)
    else
      CurrentLB.Items.AddObject(ShownText, SearchPos);
    CurrentLB.ShortenPaths;
  end;//if
end;//AddMatch

procedure TSearchResultsView.SearchResultsViewDestroy(Sender: TObject);
begin
  fListBoxFont.free;
end;//SearchResulstViewDestroy

Procedure TSearchResultsView.BeginUpdate(APageIndex: integer);
var
  CurrentLB: TLazSearchResultLB;
begin
  CurrentLB:= GetListBox(APageIndex);
  if Assigned(CurrentLB) then
    CurrentLB.BeginUpdate;
end;//BeginUpdate

procedure TSearchResultsView.EndUpdate(APageIndex: integer);
var
  CurrentLB: TLazSearchResultLB;
begin
  CurrentLB:= GetListBox(APageIndex);
  if Assigned(CurrentLB) then
  begin
    CurrentLB.EndUpdate;
    if CurrentLB.Items.Count>0 then begin
      CurrentLB.ItemIndex:= 0;
      CurrentLB.TopIndex:= 0;
    end;
  end;
end;

procedure TSearchResultsView.Parse_Search_Phrases(var slPhrases: TStrings);
var i, iLength: Integer;
    sPhrases, sPhrase: string;
begin
 //Parse Phrases
 sPhrases := edSearchInList.Text;
 iLength := Length(sPhrases);
 sPhrase := '';
 for i:=1 to iLength do
  begin
   if ((sPhrases[i] = ' ') or (sPhrases[i] = ',') or (i = iLength)) then
    begin
     if not ((sPhrases[i] = ' ') or (sPhrases[i] = ',')) then
      sPhrase := sPhrase + sPhrases[i];
     if (sPhrase > ' ') then
      slPhrases.Add(UpperCase(sPhrase));//End of phrase, add to phrase list
     sPhrase := '';//Reset sPhrase
    end else
    begin
     if (sPhrases[i] > ' ') then
      sPhrase := sPhrase + sPhrases[i];
    end;//End if ((sPhrases[i] = ' ') or (sPhrases[i] = ','))
  end;//End for-loop i
end;

procedure TSearchResultsView.ClosePage(PageIndex: integer);
begin
  if (PageIndex<0) or (PageIndex>=ResultsNoteBook.Pages.Count) then exit;
  ResultsNoteBook.Pages.Delete(PageIndex);
  if ResultsNoteBook.Pages.Count = 0 then
    Hide;
end;

{Brings the results tab named APageName to front.
 If APageName does not exist, does nothing}
procedure TSearchResultsView.BringResultsToFront(const APageName: string);
begin
  if PageExists(APageName) then
  begin
    ResultsNoteBook.PageIndex:= GetPageIndex(APageName);
  end;//if
end;//BringResultsToFront

{Sets the Items from the list box on the currently selected page in the
 TNoteBook}
procedure TSearchResultsView.SetItems(Index: integer; Value: TStrings);
var
  CurrentLB: TLazSearchResultLB;
begin
  if Index > -1 then
  begin
    CurrentLB:= GetListBox(Index);
    if Assigned(CurrentLB) then
    begin
      if CurrentLB.UpdateState then
        CurrentLB.UpdateItems.Assign(Value)
      else
        CurrentLB.Items.Assign(Value);
      CurrentLB.Skipped:=0;
    end;//if
  end//if
end;//SetItems

function TSearchResultsView.GetItems(Index: integer): TStrings;
var
  CurrentLB: TLazSearchResultLB;
begin
  result:= nil;
  CurrentLB:= GetListBox(Index);
  if Assigned(CurrentLB) then
  begin
    if CurrentLB.UpdateState then
      result:= CurrentLB.UpdateItems
    else
      result:= CurrentLB.Items;
  end;//if
end;//GetItems

procedure TSearchResultsView.SetMaxItems(const AValue: integer);
begin
  if FMaxItems=AValue then exit;
  FMaxItems:=AValue;
end;

procedure TSearchResultsView.ResultsNoteBookCloseTabclicked(Sender: TObject);
begin
  if (Sender is TPage) then
  begin
    with Sender as TPage do
    begin
      ClosePage(PageIndex);
    end;//with
  end;//if
end;//ResultsNoteBookClosetabclicked

procedure TSearchResultsView.btnSearchAgainClick(Sender: TObject);
var
  CurrentLB: TLazSearchResultLB;
  SearchObj: TLazSearch;
begin
  CurrentLB:= GetListBox(ResultsNoteBook.PageIndex);
  if not Assigned(CurrentLB) then begin
    MainIDEInterface.FindInFilesPerDialog(Project1);
  end
  else begin
    SearchObj:= CurrentLB.SearchObject;
    if Assigned(FindInFilesDialog) then
    begin
      with FindInFilesDialog do
      begin
        DirectoryComboBox.Text:= SearchObj.SearchDirectory;
        Options:= SearchObj.SearchOptions;
        FileMaskComboBox.Text:= SearchObj.SearchMask;
      end;//with
      MainIDEInterface.FindInFiles(Project1, SearchObj.SearchString);
    end;//if
  end;
end;

{Searched the notebook control for a page with APageName name, returns true if
 found}
function TSearchResultsView.PageExists(const APageName: string): boolean;
var
  i: integer;
  CurPagename: String;
begin
  Result:= false;
  CurPagename:=BeautifyPageName(APageName);
  for i:= 0 to ResultsNoteBook.Pages.Count - 1 do
  begin
    if (ResultsNoteBook.Pages[i] = CurPageName) then
    begin
      Result:= true;
      exit;
    end;//if
  end;//for
end;//PageExists

procedure TSearchResultsView.ListBoxKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then begin
    Key:=VK_UNKNOWN;
    Close;
  end;
end;

{ Add Result will create a tab in the Results view window with an new
  list box or focus an existing listbox and update it's searchoptions.}
function TSearchResultsView.AddSearch(const ResultsName: string;
  const SearchText: string;
  const ReplaceText: string;
  const ADirectory: string;
  const AMask: string;
  const TheOptions: TLazFindInFileSearchOptions): integer;
var
  NewListBox: TLazSearchResultLB;
  NewPage: LongInt;
  i: integer;
  SearchObj: TLazSearch;
  NewPageName: String;
begin
  Result:= -1;
  if Assigned(ResultsNoteBook) then
  begin
    NewPageName:=BeautifyPageName(ResultsName);
    //DebugLn(['TSearchResultsView.AddSearch NewPageName=',dbgstr(NewPageName),' ResultsName="',dbgstr(ResultsName),'"']);
    with ResultsNoteBook do
    begin
      i:= GetPageIndex(NewPageName);
      if i>=0 then
      begin
        NewListBox:= GetListBox(i);
        ResultsNoteBook.PageIndex:= i;
        //Free backup objects and list since its a new search with the same listbox
        NewListBox.FreeObjects(NewListBox.BackUpStrings);
        NewListBox.BackUpStrings.Clear;
        NewListBox.Filtered := False;
      end//if
      else
      begin
        NewPage:= Pages.Add(NewPageName);
        ResultsNoteBook.PageIndex:= NewPage;
        ResultsNoteBook.Page[ResultsNoteBook.PageIndex].OnKeyDown := @ListBoxKeyDown;
        if NewPage > -1 then
        begin
          NewListBox:= TLazSearchResultLB.Create(Page[NewPage]);
          with NewListBox do
          begin
            Parent:= Page[NewPage];
            Align:= alClient;
            BorderSpacing.Around := 6;
            ClickOnSelChange:=false;
            OnClick:= @ListBoxClicked;
            OnDblClick:= @ListBoxDoubleClicked;
            OnKeyDown := @ListBoxKeyDown;
            Style:= lbOwnerDrawFixed;
            OnDrawItem:= @ListBoxDrawItem;
            OnShowHint:= @LazLBShowHint;
            OnMouseMove:= @LazLBMousemove;
            OnMouseWheel:= @LazLBMouseWheel;
            Font.Name:=fListBoxFont.Name;
            Font.Height:=fListBoxFont.Height;
            ShowHint:= true;
            NewListBox.Canvas.Color:= clWhite;
          end;//with
        end;//if
      end;//else
    end;//with
    SearchObj:=NewListBox.SearchObject;
    if SearchObj<>nil then begin
      SearchObj.SearchString:= SearchText;
      SearchObj.ReplaceText := ReplaceText;
      SearchObj.SearchDirectory:= ADirectory;
      SearchObj.SearchMask:= AMask;
      SearchObj.SearchOptions:= TheOptions;
    end;
    NewListBox.Skipped:=0;
    Result:= ResultsNoteBook.PageIndex;
    edSearchInList.Clear;
  end;//if
end;//AddResult

procedure TSearchResultsView.LazLBShowHint(Sender: TObject;
  HintInfo: PHintInfo);
var
  MatchPos: TLazSearchMatchPos;
  HintStr: string;
begin
  if Sender is TLazSearchResultLB then
  begin
    With Sender as TLazSearchResultLB do
    begin
      if (fMouseOverIndex >= 0) and (fMouseOverIndex < Items.Count) then
      begin
        if Items.Objects[fMouseOverIndex] is TLazSearchMatchPos then
          MatchPos:= TLazSearchMatchPos(Items.Objects[fMouseOverIndex])
        else
          MatchPos:= nil;
        if MatchPos<>nil then
          HintStr:=MatchPos.Filename
                   +' ('+IntToStr(MatchPos.FileStartPos.Y)
                   +','+IntToStr(MatchPos.FileStartPos.X)+')'
                   +' '+MatchPos.TheText
        else
          HintStr:=Items[fMouseOverIndex];
        Hint:= HintStr;
      end;//if
    end;//with
  end;//if
end;//LazLBShowHint

procedure TSearchResultsView.ListboxDrawitem(Control: TWinControl;
                                             Index: Integer; ARect: TRect;
                                             State: TOwnerDrawState);
var
  FirstPart: string;
  BoldPart: string;
  LastPart: string;
  TheText: string;
  TheTop: integer;
  MatchPos: TLazSearchMatchPos;
  TextEnd: integer;
begin
  With Control as TLazSearchResultLB do
  begin
    Canvas.FillRect(ARect);
    if Items.Objects[Index] is TLazSearchMatchPos then
      MatchPos:= TLazSearchMatchPos(Items.Objects[Index])
    else
      MatchPos:= nil;

    if Assigned(MatchPos) then
    begin
      TheTop:= ARect.Top;

      FirstPart:=MatchPos.ShownFilename+' ('+IntToStr(MatchPos.FileStartPos.Y)
          +','+IntToStr(MatchPos.FileStartPos.X)+') '
          +SpecialCharsToHex(copy(MatchPos.TheText,1,MatchPos.MatchStart-1));
      BoldPart:=SpecialCharsToHex(
                  copy(MatchPos.TheText,MatchPos.MatchStart,MatchPos.MatchLen));
      LastPart:=SpecialCharsToHex(
                   copy(MatchPos.TheText, MatchPos.MatchStart+MatchPos.MatchLen,
                        Length(MatchPos.TheText)));
      if UTF8Length(BoldPart)>MaxTextLen then
        BoldPart:=UTF8Copy(BoldPart,1,MaxTextLen)+'...';
      //DebugLn(['TSearchResultsView.ListboxDrawitem FirstPart="',FirstPart,'" BoldPart="',BoldPart,'" LastPart="',LastPart,'"']);
      Canvas.TextOut(ARect.Left, TheTop, FirstPart);
      TextEnd:= ARect.Left + Canvas.TextWidth(FirstPart);
      Canvas.Font.Style:= Canvas.Font.Style + [fsBold];
      Canvas.TextOut(TextEnd, TheTop, BoldPart);
      TextEnd:= TextEnd + Canvas.TextWidth(BoldPart);
      Canvas.Font.Style:=Canvas.Font.Style - [fsBold];
      Canvas.TextOut(TextEnd, TheTop, LastPart);
    end//if
    else
    begin
      TheText:=Items[Index];
      Canvas.TextOut(ARect.Left, ARect.Top, TheText);
    end;//else
  end;//with
end;//ListBoxDrawItem

procedure TSearchResultsView.ListBoxClicked(Sender: TObject);
begin
  if EnvironmentOptions.MsgViewDblClickJumps then exit;
  if Assigned(fOnSelectionChanged) then
    fOnSelectionChanged(Self)
end;//ListBoxClicked

procedure TSearchResultsView.ListBoxDoubleClicked(Sender: TObject);
begin
  if not EnvironmentOptions.MsgViewDblClickJumps then exit;
  if Assigned(fOnSelectionChanged) then
    fOnSelectionChanged(Self)
end;//ListBoxDoubleClicked

{Returns the Position within the source file from a properly formated search
 result}
function TSearchResultsView.GetSourcePositon: TPoint;
var
  MatchPos: TLazSearchMatchPos;
begin
  Result.x:= -1;
  Result.y:= -1;
  MatchPos:=GetSelectedMatchPos;
  if MatchPos=nil then exit;
  Result:=MatchPos.FileStartPos;
end;//GetSourcePositon

{Returns The file name portion of a properly formated search result}
function TSearchResultsView.GetSourceFileName: string;
var
  MatchPos: TLazSearchMatchPos;
begin
  MatchPos:=GetSelectedMatchPos;
  if MatchPos=nil then
    Result:=''
  else
    Result:=MatchPos.Filename;
end;//GetSourceFileName

{Returns the selected text in the currently active listbox.}
function TSearchResultsView.GetSelectedText: string;
var
  ThePage: TPage;
  TheListBox: TLazSearchResultLB;
  i: integer;
begin
  result:= '';
  i:= ResultsNoteBook.PageIndex;
  if i > -1 then
  begin
    ThePage:= ResultsNoteBook.Page[i];
    if Assigned(ThePage) then
    begin
      TheListBox:= GetListBox(ThePage.PageIndex);
      if Assigned(TheListBox) then
      begin
        i:= TheListBox.ItemIndex;
        if i > -1 then
          result:= TheListBox.Items[i];
      end;//if
    end;//if
  end;//if
end;//GetSelectedText

function TSearchResultsView.GetSelectedMatchPos: TLazSearchMatchPos;
var
  ThePage: TPage;
  TheListBox: TLazSearchResultLB;
  i: integer;
  AnObject: TObject;
begin
  Result:= nil;
  i:= ResultsNoteBook.PageIndex;
  if i > -1 then
  begin
    ThePage:= ResultsNoteBook.Page[i];
    if Assigned(ThePage) then
    begin
      TheListBox:= GetListBox(ThePage.PageIndex);
      if Assigned(TheListBox) then
      begin
        i:= TheListBox.ItemIndex;
        if i > -1 then begin
          AnObject:=TheListBox.Items.Objects[i];
          if AnObject is TLazSearchMatchPos then
            Result:=TLazSearchMatchPos(AnObject);
        end;
      end;//if
    end;//if
  end;//if
end;

function TSearchResultsView.GetPageIndex(const APageName: string): integer;
var
  i: integer;
  CurPagename: String;
begin
  Result:= -1;
  CurPagename:=BeautifyPageName(APageName);
  for i:= 0 to ResultsNoteBook.Pages.Count - 1 do
  begin
    if (ResultsNoteBook.Pages[i] = CurPageName) then
    begin
      Result:= i;
      break;
    end;//if
  end;//for
end;//GetPageIndex

{Returns a the listbox control from a Tab if both the page and the listbox
 exist else returns nil}
function TSearchResultsView.GetListBox(APageIndex: integer): TLazSearchResultLB;
var
  i: integer;
  ThePage: TPage;
begin
  Result:= nil;
  if (APageIndex > -1) and (APageIndex < ResultsNoteBook.Pages.Count) then
  begin
    ThePage:= ResultsNoteBook.Page[APageIndex];
    if Assigned(ThePage) then
    begin
      for i:= 0 to ThePage.ComponentCount - 1 do
      begin
        if ThePage.Components[i] is TLazSearchResultLB then
        begin
          result:= TLazSearchResultLB(ThePage.Components[i]);
          break;
        end;//if
      end;//for
    end;//if
  end;//if
end;//GetListBox

procedure TLazSearchResultLB.SetSkipped(const AValue: integer);
var
  SrcList: TStrings;
  s: String;
  HasSkippedLine: Boolean;
  SkippedLine: String;
begin
  if FSkipped=AValue then exit;
  FSkipped:=AValue;
  s:='Found, but not listed here: ';
  if fUpdating then
    SrcList:=fUpdateStrings
  else
    SrcList:=Items;
  if (SrcList.Count>0) and (copy(SrcList[SrcList.Count-1],1,length(s))=s) then
    HasSkippedLine:=true
  else
    HasSkippedLine:=false;
  SkippedLine:=s+IntToStr(FSkipped);
  if FSkipped>0 then begin
    if HasSkippedLine then begin
      SrcList[SrcList.Count-1]:=SkippedLine;
    end else begin
      SrcList.add(SkippedLine);
    end;
  end else begin
    if HasSkippedLine then
      SrcList.Delete(SrcList.Count-1);
  end;
end;

{******************************************************************************
  TLazSearchResultLB
******************************************************************************}
Constructor TLazSearchResultLB.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fSearchObject:= TLazSearch.Create;
  fUpdating:= false;
  fUpdateCount:= 0;
  fUpdateStrings:= TStringList.Create;
  fBackUpStrings := TStringList.Create;
  FSearchInListPhrases := '';
  fFiltered := False;
end;//Create

Destructor TLazSearchResultLB.Destroy;
begin
  if Assigned(fSearchObject) then
    FreeAndNil(fSearchObject);
  //if UpdateStrings is empty,
  //means the objects are stored in Items due to filtering
  //filtering clears UpdateStrings
  if (fUpdateStrings.Count = 0) then
   FreeObjects(Items);
  if Assigned(fUpdateStrings) then
  begin
    FreeObjects(fUpdateStrings);
    FreeAndNil(fUpdateStrings);
  end;//if
  if Assigned(fBackUpStrings) then
  begin
   FreeObjects(fBackUpStrings);
   FreeAndNil(fBackUpStrings);
  end;//End if Assigned(fBackUpStrings)
  inherited Destroy;
end;//Destroy

procedure TLazSearchResultLB.BeginUpdate;
begin
  inc(fUpdateCount);
  if (fUpdateCount = 1) then
  begin
    if Assigned(Items) then
      fUpdateStrings.Assign(Items);
    fUpdating:= true;
  end;//if
end;//BeginUpdate

procedure TLazSearchResultLB.EndUpdate;
begin
  if (fUpdateCount = 0) then
    RaiseGDBException('TLazSearchResultLB.EndUpdate');
  dec(fUpdateCount);
  if (fUpdateCount = 0) then
  begin
    ShortenPaths;
    fUpdating:= false;
    FreeObjects(Items);
    Items.Assign(fUpdateStrings);
  end;//if
end;//EndUpdate

procedure TLazSearchResultLB.ShortenPaths;
var
  i: Integer;
  AnObject: TObject;
  SharedPath: String;
  MatchPos: TLazSearchMatchPos;
  SrcList: TStrings;
  SharedLen: Integer;
  ShownText: String;
begin
  if fUpdateCount>0 then begin
    fShortenPathNeeded:=true;
    exit;
  end;
  fShortenPathNeeded:=false;
  
  if fUpdating then
    SrcList:=fUpdateStrings
  else
    SrcList:=Items;
  
  // find shared path (the path of all filenames, that is the same)
  SharedPath:='';
  for i:=0 to SrcList.Count-1 do begin
    AnObject:=SrcList.Objects[i];
    if AnObject is TLazSearchMatchPos then begin
      MatchPos:=TLazSearchMatchPos(AnObject);
      if i=0 then
        SharedPath:=ExtractFilePath(MatchPos.Filename)
      else if (SharedPath<>'') then begin
        SharedLen:=0;
        while (SharedLen<length(MatchPos.Filename))
        and (SharedLen<length(SharedPath))
        and (MatchPos.Filename[SharedLen+1]=SharedPath[SharedLen+1])
        do
          inc(SharedLen);
        while (SharedLen>0) and (SharedPath[SharedLen]<>PathDelim) do
          dec(SharedLen);
        if SharedLen<>length(SharedPath) then
          SharedPath:=copy(SharedPath,1,SharedLen);
      end;
    end;
  end;
  
  // shorten shown paths
  SharedLen:=length(SharedPath);
  for i:=0 to SrcList.Count-1 do begin
    AnObject:=SrcList.Objects[i];
    if AnObject is TLazSearchMatchPos then begin
      MatchPos:=TLazSearchMatchPos(AnObject);
      MatchPos.ShownFilename:=copy(MatchPos.Filename,SharedLen+1,
                                   length(MatchPos.Filename));
      ShownText:=BeautifyLine(MatchPos);
      SrcList[i]:=ShownText;
      SrcList.Objects[i]:=MatchPos;
    end;
  end;
end;

procedure TLazSearchResultLB.FreeObjects(var slItems: TStrings);
var i: Integer;
begin
 if (slItems.Count <= 0) then Exit;
 for i:=0 to slItems.Count-1 do
  begin
   if Assigned(slItems.Objects[i]) then
    slItems.Objects[i].Free;
  end;//End for-loop
end;

function TLazSearchResultLB.BeautifyLine(const Filename: string; X, Y: integer;
  const Line: string): string;
begin
  Result:=SpecialCharsToHex(Line);
  if UTF8Length(Result)>MaxTextLen then
    Result:=UTF8Copy(Result,1,MaxTextLen)+'...';
  Result:=Filename
          +' ('+IntToStr(Y)
          +','+IntToStr(X)+')'
          +' '+Result;
end;

function TLazSearchResultLB.BeautifyLine(SearchPos: TLazSearchMatchPos
  ): string;
begin
  Result:=BeautifyLine(SearchPos.ShownFilename,SearchPos.FileStartPos.X,
                       SearchPos.FileStartPos.Y,SearchPos.TheText);
end;

initialization
  {$I searchresultview.lrs}

end.

