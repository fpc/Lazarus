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
  Classes, SysUtils, Math, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, Buttons, LCLType,
  IDEOptionDefs, LazarusIDEStrConsts, EnvironmentOpts, InputHistory,
  FindInFilesDlg, Project;


 {TLazSearchMatchPos}
type
  TLazSearchMatchPos = class(TObject)
  private
    fMatchStart: integer;
    fMatchLen: integer;
  public
    property MatchStart: integer read fMatchStart write fMatchStart;
    property MatchLen: integer read fMatchLen write fMatchLen;
end;//TLazSearchMatchPos

 {TLazSearch}
type
  TLazSearch = Class(TObject)
  private
    fSearchString: string;
    fSearchOptions: TLazFindInFileSearchOptions;
    fSearchDirectory: string;
    fSearchMask: string;
  public
    property SearchString: string read fSearchString write fSearchString;
    property SearchOptions: TLazFindInFileSearchOptions read fSearchOptions
                                                        write fSearchOptions;
    property SearchDirectory: string read fSearchDirectory
                                     write fSearchDirectory;
    property SearchMask: string read fSearchMask write fSearchMask;
end;//TLazSearch

 {TLazSearchResultLB}
type
  TLazSearchResultLB = Class(TCustomListBox)
  private
    fSearchObject: TLazSearch;
    fUpdateStrings: TStrings;
    fUpdating: boolean;
    fUpdateCount: integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property SearchObject: TLazSearch read fSearchObject write fSearchObject;
    procedure BeginUpdate;
    procedure EndUpdate;
    property UpdateItems: TStrings read fUpdateStrings write fUpdateStrings;
    property UpdateState: boolean read fUpdating;
end;

{TSearchResultsView}
type
  TSearchResultsView = class(TForm)
    btnSearchAgain: TBUTTON;
    ResultsNoteBook: TNOTEBOOK;
    procedure Form1Create(Sender: TObject);
    procedure ResultsNoteBookChangebounds(Sender: TObject);
    procedure ResultsNoteBookClosetabclicked(Sender: TObject);
    procedure SearchResultsViewDestroy(Sender: TObject);
    procedure btnSearchAgainClick(Sender: TObject);
    procedure ListboxDrawitem(Control: TWinControl; Index: Integer;
                              ARect: TRect; State: TOwnerDrawState);
    procedure LazLBShowHint(Sender: TObject; HintInfo: Pointer);
    procedure LazLBMousemove(Sender: TObject; Shift: TShiftState;
                             X, Y: Integer);
    Procedure LazLBMouseWheel(Sender: TObject; Shift: TShiftState;
                   WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private
    { private declarations }
    function PageExists(const APageName: string): boolean;
    function GetPageIndex(APageName: string): integer;
    function GetListBox(APageIndex: integer): TLazSearchResultLB;
    procedure ListBoxClicked(Sender: TObject);
    procedure ListBoxDoubleClicked(Sender: TObject);
    procedure SetItems(Index: Integer; Value: TStrings);
    function GetItems(Index: integer): TStrings;
    fOnSelectionChanged: TNotifyEvent;
    fListBoxFont: TFont;
    fMouseOverIndex: integer;
  public
    { public declarations }
    function AddResult(const ResultsName: string;
                        const SearchText: string;
                        const ADirectory: string;
                        const AMask: string;
                        const TheOptions: TLazFindInFileSearchOptions): integer;
    function GetSourcePositon: TPoint;
    function GetSourceFileName: string;
    function GetSelectedText: string;
    procedure BringResultsToFront(const APageName: string);
    procedure AddMatch(const AIndex: integer; const TheText: string;
                       const MatchStart: integer; const MatchLen: integer);
    procedure BeginUpdate(AIndex: integer);
    procedure EndUpdate(AIndex: integer);
    property ListBoxFont: TFont read fListBoxFont write fListBoxFont;
    property OnSelectionChanged: TNotifyEvent read fOnSelectionChanged
                                              write fOnSelectionChanged;
    property Items[Index: integer]: TStrings read GetItems write SetItems;
  end; 

var
  SearchResultsView: TSearchResultsView;

implementation
uses
  MainBar;
  
{ TSearchResultsView }

const
  SPACE = ' ';
  
procedure TSearchResultsView.Form1Create(Sender: TObject);
var
  ALayout: TIDEWindowLayout;
begin
  ResultsNoteBook.Options:= ResultsNoteBook.Options+[nboShowCloseButtons];
  ResultsNoteBook.Update;

  if LazarusResources.Find(ClassName)=nil then
    Caption:=lisMenuViewSearchResults;
    
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
  self.ShowHint:= True;
  fMouseOverIndex:= -1;
end;//Create

procedure TSearchResultsView.ResultsNoteBookChangebounds(Sender: TObject);
begin

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

procedure TSearchResultsView.AddMatch(const AIndex: integer;
                                      const TheText: string;
                                      const MatchStart: integer;
                                      const MatchLen: integer);
var
  CurrentLB: TLazSearchResultLB;
  SearchPos: TLazSearchMatchPos;
begin
  CurrentLB:= GetListBox(AIndex);
  if Assigned(CurrentLB) then
  begin
    SearchPos:= TLazSearchMatchPos.Create;
    SearchPos.MatchStart:= MatchStart;
    SearchPos.MatchLen:= MatchLen;
    if CurrentLB.UpdateState then
      CurrentLB.UpdateItems.AddObject(TheText, SearchPos)
    else
      CurrentLB.Items.AddObject(TheText, SearchPos);
  end;//if
end;//AddMatch

procedure TSearchResultsView.SearchResultsViewDestroy(Sender: TObject);
begin
  fListBoxFont.free;
end;//SearchResulstViewDestroy

Procedure TSearchResultsView.BeginUpdate(AIndex: integer);
var
  CurrentLB: TLazSearchResultLB;
begin
  CurrentLB:= GetListBox(AIndex);
  if Assigned(CurrentLB) then
    CurrentLB.BeginUpdate;
end;//BeginUpdate

procedure TSearchResultsView.EndUpdate(AIndex: integer);
var
  CurrentLB: TLazSearchResultLB;
begin
  CurrentLB:= GetListBox(AIndex);
  if Assigned(CurrentLB) then
  begin
    CurrentLB.EndUpdate;
    CurrentLB.ItemIndex:= 0;
    CurrentLB.TopIndex:= 0;
  end;//if
end;//EndUpdate

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

procedure TSearchResultsView.ResultsNoteBookClosetabclicked(Sender: TObject);
begin
  if (Sender is TPage) then
  begin
    with sender as TPage do
    begin
      ResultsNoteBook.Pages.Delete(PageIndex);
    end;//with
  end;//if
  if ResultsNoteBook.Pages.Count = 0 then
    Self.Hide;
end;//ResultsNoteBookClosetabclicked

procedure TSearchResultsView.btnSearchAgainClick(Sender: TObject);
var
  CurrentLB: TLazSearchResultLB;
  SearchObj: TLazSearch;
begin
  CurrentLB:= GetListBox(ResultsNoteBook.PageIndex);
  if not Assigned(CurrentLB) then exit;
  SearchObj:= CurrentLB.SearchObject;
  if Assigned(FindInFilesDialog) then
  begin
    with FindInFilesDialog do
    begin
      DirectoryComboBox.Text:= SearchObj.SearchDirectory;
      FindText:= SearchObj.SearchString;
      Options:= SearchObj.SearchOptions;
      FileMaskComboBox.Text:= SearchObj.SearchMask;
    end;//with
    SourceNotebook.FindInFiles(Project1);
  end;//if
end;

{Searched the notebook control for a page with APageName name, returns true if
 found}
function TSearchResultsView.PageExists(const APageName: string): boolean;
var
  i: integer;
begin
  result:= false;
  for i:= 0 to ResultsNoteBook.Pages.Count - 1 do
  begin
    if (ResultsNoteBook.Pages[i] = APageName + SPACE) then
    begin
      result:= true;
      break;
    end;//if
  end;//for
end;//PageExists

{Add Result will create a tab in the Results view window with an new
 list box or focus an existing listbox and update it's searchoptions.}
function TSearchResultsView.AddResult(const ResultsName: string;
                        const SearchText: string;
                        const ADirectory: string;
                        const AMask: string;
                        const TheOptions: TLazFindInFileSearchOptions): integer;
var
  NewListBox: TLazSearchResultLB;
  NewPage: LongInt;
  i: integer;
begin
  result:= -1;
  if Assigned(ResultsNoteBook) then
  begin
    With ResultsNoteBook do
    begin
      i:= GetPageIndex(ResultsName);
      if i > -1 then
      begin
        NewListBox:= GetListBox(i);
        ResultsNoteBook.PageIndex:= i;
      end//if
      else
      begin
        NewPage:= Pages.Add(ResultsName + SPACE);
        ResultsNoteBook.PageIndex:= NewPage;
        if NewPage > -1 then
        begin
          NewListBox:= TLazSearchResultLB.Create(Page[NewPage]);
          with NewListBox do
          begin
            Parent:= Page[NewPage];
            Align:= alClient;
            OnClick:= @ListBoxClicked;
            OnDblClick:= @ListBoxDoubleClicked;
            Style:= lbOwnerDrawFixed;
            OnDrawItem:= @ListBoxDrawItem;
            Font.Name:= fListBoxFont.Name;
            Font.Height:= fListBoxFont.Height;
            OnShowHint:= @LazLBShowHint;
            OnMouseMove:= @LazLBMousemove;
            OnMouseWheel:= @LazLBMouseWheel;
            ShowHint:= true;
            NewLIstBox.Canvas.Color:= clWhite;
          end;//with
        end;//if
      end;//else
    end;//with
    with NewListBox.SearchObject do
    begin
      SearchString:= SearchText;
      SearchDirectory:= ADirectory;
      SearchMask:= AMask;
      SearchOptions:= TheOptions;
    end;//with
    result:= ResultsNoteBook.PageIndex;
  end;//if
end;//AddResult


procedure TSearchResultsView.LazLBShowHint(Sender: TObject; HintInfo: Pointer);
begin
  if Sender is TLazSearchResultLB then
  begin
    With Sender as TLazSearchResultLB do
    begin
      if (fMouseOverIndex >= 0) and (fMouseOverIndex < Items.Count) then
      begin
        Hint:= Items[fMouseOverIndex];
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
  BoldLen: integer;
  TheText: string;
  TheTop: integer;
  MatchPos: TLazSearchMatchPos;
begin
  With Control as TLazSearchResultLB do
  begin
    Canvas.FillRect(ARect);
    TheText:= Items[Index];
    if Items.Objects[Index] is TLazSearchMatchPos then
      MatchPos:= TLazSearchMatchPos(Items.Objects[Index])
    else
      MatchPos:= nil;
      
    if Assigned(MatchPos) then
    begin
      TheTop:= ARect.Top;
      BoldLen:= MatchPos.MatchLen;
      FirstPart:= copy(TheText,1,MatchPos.MatchStart - 1);
      BoldPart:= copy(TheText,MatchPos.MatchStart ,BoldLen);
      LastPart:= copy(TheText, MatchPos.MatchStart + BoldLen,
                      Length(TheText) - (MatchPos.MatchStart + BoldLen) + 2);
      Canvas.TextOut(ARect.Left, TheTop, FirstPart);
      Canvas.Font.Style:= Canvas.Font.Style + [fsBold];
      {TODO: Find out why bold is 1 pixel off}
      Canvas.TextOut(ARect.Left + Canvas.TextWidth(FirstPart),
                     TheTop - 1, BoldPart);
      Canvas.Font.Style:= Canvas.Font.Style  - [fsBold];
      Canvas.TextOut(ARect.Left + Canvas.TextWidth(FirstPart + BoldPart),
                     TheTop, LastPart);
    end//if
    else
    begin
      Canvas.TextOut(ARect.Left, ARect.Top + 1, TheText);
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
 reslut}
function TSearchResultsView.GetSourcePositon: TPoint;
var
  i: integer;
  strTemp: string;
  strResults: string;
begin
  strResults:= GetSelectedText;
  result.x:= -1;
  result.y:= -1;
  i:= pos('(',strResults);
  if i > 0 then
  begin
    inc(i);
    While (i < length(strResults)) and (strResults[i] <> ',') do
    begin
      strTemp:= StrTemp + strResults[i];
      inc(i);
    end;//while
    if (i < Length(StrResults)) and (strResults[i] = ',') then
    begin
      result.y:= StrToInt(strTemp);
      inc(i);
      strTemp:= '';
      While (i < length(strResults)) and (strResults[i] <> ')') do
      begin
        strTemp:= strResults[i];
        inc(i);
      end;//while
      if (i < Length(strResults)) and (strResults[i] = ')' ) then
       result.x:= StrToInt(strTemp);
    end;//if
  end;//if
end;//GetSource Positon

{Returns The file name portion of a properly formated search result}
function TSearchResultsView.GetSourceFileName: string;
var
  strResults: string;
  i: integer;
begin
  strResults:= GetSelectedText;
  i:= pos('(', strResults);
  dec(i);
  if i > 0 then
  begin
    result:= copy(strResults, 1, i);
  end
  else
  begin
    result:= '';
  end;
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

function TSearchResultsView.GetPageIndex(APageName: string): integer;
var
  i: integer;
begin
  result:= -1;
  for i:= 0 to ResultsNoteBook.Pages.Count - 1 do
  begin
    if (ResultsNoteBook.Pages[i] = APageName + SPACE) then
    begin
      result:= i;
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
end;//Create

Destructor TLazSearchResultLB.Destroy;
var
  i: integer;
begin
  if Assigned(fSearchObject) then
    FreeAndNil(fSearchObject);
  if Assigned(fUpdateStrings) then
  begin
    for i:= 0 to fUpdateStrings.Count -1 do
    begin
      if Assigned(fUpdateStrings.Objects[i]) then
        fUpdateStrings.Objects[i].free;
    end;//for
    FreeAndNil(fUpdateStrings);
  end;//if
  
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
var
  i: integer;
begin
  dec(fUpdateCount);
  if (fUpdateCount < 0) then
    fUpdateCount:= 0;
  if (fUpdateCount = 0) then
  begin
    fUpdating:= false;
    for i:= 0 to Items.Count -1 do
    begin
      try
        if Assigned(Items.Objects[i]) then
        begin
          Items.Objects[i].free;
        end;//if
      except
        writeln('Exception in TLazSearchResultLB.EndUpdate,' +
                ' Pointer assigned free failed');
      end;//except
    end;//for
    Items.Assign(fUpdateStrings);
  end;//if
end;//EndUpdate


initialization
  {$I searchresultview.lrs}

end.

