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
  IDEOptionDefs, LazarusIDEStrConsts, EnvironmentOpts;

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
  private
    { private declarations }
    fUpdating: boolean;
    fSearchObjectList: TStringList;
    function PageExists(APageName: string): boolean;
    function GetPageIndex(APageName: string): integer;
    function GetListBox(APageIndex: integer): TListBox;
    procedure ListBoxClicked(Sender: TObject);
    procedure ListBoxDoubleClicked(Sender: TObject);
    fOnSelectionChanged: TNotifyEvent;
  public
    { public declarations }
    function AddResult(const ResultsName: string; SearchText: string): TStrings;
    function GetSourcePositon: TPoint;
    function GetSourceFileName: string;
    function GetSelectedText: string;
    procedure BeginUpdate;
    procedure EndUpdate;
    property OnSelectionChanged: TNotifyEvent read fOnSelectionChanged
                                              write fOnSelectionChanged;
  end; 

type
  TLazSearch = Class(TObject)
  private
    fSearchString: string;
  public
    property SearchString: string read fSearchString write fSearchString;
  end;

var
  SearchResultsView: TSearchResultsView;

implementation

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
  fSearchObjectList:= TStringList.Create;
  fOnSelectionChanged:= nil;
end;//Create

procedure TSearchResultsView.ResultsNoteBookChangebounds(Sender: TObject);
begin

end;

procedure TSearchResultsView.SearchResultsViewDestroy(Sender: TObject);
var
  i: integer;
  TheObject: TObject;
begin
  try
    if not Assigned(fSearchObjectList) then
      Writeln('fSearchObject is not assigned');
    for i:= 0 to fSearchObjectList.Count - 1 do
    begin
      if Assigned(fSearchObjectList.Objects[i]) then
      begin
        TheObject:= fSearchObjectList.Objects[i];
        FreeAndNil(TheObject);
      end;//if
    end;//for
  except
    writeln('Exception in form destroy!');
  end;//except
  FreeAndNil(fSearchObjectList);
end;//SearchResulstViewDestroy

Procedure TSearchResultsView.BeginUpdate;
begin
  fUpDating:= true;
end;//BeginUpdate

procedure TSearchResultsView.EndUpdate;
begin
  fUpdating:= false;
end;//EndUpdate

procedure TSearchResultsView.ResultsNoteBookClosetabclicked(Sender: TObject);
var
  TheObject: TObject;
  i: integer;
begin
  if (Sender is TPage) then
  begin
    with sender as TPage do
    begin
      i:= fSearchObjectList.IndexOf(Caption);
      TheObject:= fSearchObjectList.Objects[i];
      FreeAndNil(TheObject);
      fSearchObjectList.Delete(i);
      ResultsNoteBook.Pages.Delete(PageIndex);
    end;//with
  end;//if
  if ResultsNoteBook.Pages.Count = 0 then
    Self.Hide;
end;//ResultsNoteBookClosetabclicked

procedure TSearchResultsView.btnSearchAgainClick(Sender: TObject);
begin
  MessageDlg('Working On it!', mtInformation,[mbOk],0);
end;

{Searched the notebook control for a page with APageName name, returns true if
 found}
function TSearchResultsView.PageExists(APageName: string): boolean;
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
 list box and return the Items from the new listbox to be filled in
 by the search routine.}
function TSearchResultsView.AddResult(const ResultsName: string;
                                      SearchText: string ): TStrings;
var
  NewListBox: TListBox;
  NewPage: LongInt;
  i: integer;
  SearchObj: TLazSearch;
begin
  result:= nil;
  SearchObj:= TLazSearch.Create;
  SearchObj.SearchString:= SearchText;
  if Assigned(ResultsNoteBook) then
  begin
    With ResultsNoteBook do
    begin
      i:= GetPageIndex(ResultsName);
      if i > -1 then
      begin
        NewListBox:= GetListBox(i);
      end//if
      else
      begin
        NewPage:= Pages.Add(ResultsName + SPACE);
        fSearchObjectList.AddObject(ResultsName + SPACE, SearchObj);
        if NewPage > -1 then
        begin
          NewListBox:= TListBox.Create(Page[NewPage]);
          NewListBox.Parent:= Page[NewPage];
          NewListBox.Align:= alClient;
          NewListBox.OnClick:= @ListBoxClicked;
          NewListBox.OnDblClick:= @ListBoxDoubleClicked;
          //NewListBox.Style:= lbOwnerDrawFixed;
          //NewListBox.OnDrawItem:= @ListBoxDrawItem;
          //NewListBox.Font.Name:= 'courier';
          //NewListBox.Font.Height:= 12;
          //NewListBox.ItemHeight:= 2 * NewListBox.Canvas.TextHeight('0');
        end;//if
      end;//else
    end;//
  end;//if
  result:= NewListBox.Items;
end;//AddResult

procedure TSearchResultsView.ListboxDrawitem(Control: TWinControl;
                                             Index: Integer; ARect: TRect;
                                             State: TOwnerDrawState);
var
  FirstPart: string;
  BoldPart: string;
  LastPart: string;
  i: integer;
  BoldLen: integer;
  SearchObj: TLazSearch;
  TheText: string;
  SearchText: string;
  TheTop: integer;
begin
  //if not fUpdating then
  //begin
    With Control as TListBox do
    begin
      Canvas.FillRect(ARect);
      TheText:= Items[Index];
      if Items.Count > 0 then
      begin
        i:= fSearchObjectList.IndexOf(ResultsNoteBook.ActivePage);
        SearchObj:= TLazSearch(fSearchObjectList.Objects[i]);
      end;
      if Assigned(SearchObj) then
      begin
        SearchText:= SearchObj.SearchString;
        TheText:= Items[Index];
        i:= pos(SearchText,TheText);
        if i > 0 then
        begin
          TheTop:= ARect.Top + 1;
          BoldLen:= Length(SearchText);
          FirstPart:= copy(TheText,1,i-1);
          BoldPart:= copy(TheText,i,BoldLen + 1);
          LastPart:= copy(TheText, i + BoldLen +1, Length(TheText) -
                          (i + BoldLen));
          Canvas.TextOut(ARect.Left, TheTop, FirstPart);
          Canvas.Font.Style:= Canvas.Font.Style + [fsBold];
          Canvas.TextOut(ARect.Left + Canvas.TextWidth(FirstPart),
                         TheTop, BoldPart);
          Canvas.Font.Style:= Canvas.Font.Style  - [fsBold];
          Canvas.TextOut(ARect.Left + Canvas.TextWidth(FirstPart + BoldPart),
                         TheTop, LastPart);
        end//if
        else
        begin
          Canvas.TextOut(ARect.Left, ARect.Top + 1, TheText);
        end;//else
      end
      else
      begin
        Canvas.TextOut(ARect.Left, ARect.Top + 1, TheText);
      end;//else
    end;//with
  //end;//if
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
  TheListBox: TListBox;
  i: integer;
begin
  result:= '';
  i:= ResultsNoteBook.PageIndex;
  if i > -1 then
  begin
    ThePage:= ResultsNoteBook.Page[i];
  end;//if
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
function TSearchResultsView.GetListBox(APageIndex: integer): TListBox;
var
  i: integer;
  ThePage: TPage;
begin
  result:= nil;
  if (APageIndex > -1) and (APageIndex < ResultsNoteBook.Pages.Count) then
  begin
    ThePage:= ResultsNoteBook.Page[APageIndex];
    if Assigned(ThePage) then
    begin
      for i:= 0 to ThePage.ComponentCount - 1 do
      begin
        if ThePage.Components[i] is TListBox then
        begin
          result:= TListBox(ThePage.Components[i]);
          break;
        end;//if
      end;//for
    end;//if
  end;//if
end;//GetListBox

initialization
  { $I searchresultview.lrs}

end.

