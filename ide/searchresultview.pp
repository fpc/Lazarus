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
  FindInFilesDlg, Project, MainIntf;

type
  { TLazSearchMatchPos }
  
  TLazSearchMatchPos = class(TObject)
  private
    FFilename: string;
    FFilePosition: TPoint;
    fMatchStart: integer;
    fMatchLen: integer;
    FShownFilename: string;
    FTheText: string;
  public
    property MatchStart: integer read fMatchStart write fMatchStart;
    property MatchLen: integer read fMatchLen write fMatchLen;
    property Filename: string read FFilename write FFilename;
    property FilePosition: TPoint read FFilePosition write FFilePosition;
    property TheText: string read FTheText write FTheText;
    property ShownFilename: string read FShownFilename write FShownFilename;
  end;//TLazSearchMatchPos


  { TLazSearch }

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


  { TLazSearchResultLB }

  TLazSearchResultLB = Class(TCustomListBox)
  private
    fSearchObject: TLazSearch;
    fUpdateStrings: TStrings;
    fUpdating: boolean;
    fUpdateCount: integer;
    fShortenPathNeeded: boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property SearchObject: TLazSearch read fSearchObject write fSearchObject;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure ShortenPaths;
    property UpdateItems: TStrings read fUpdateStrings write fUpdateStrings;
    property UpdateState: boolean read fUpdating;
  end;


  { TSearchResultsView }

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
    function PageExists(const APageName: string): boolean;
    function GetPageIndex(const APageName: string): integer;
    function GetListBox(APageIndex: integer): TLazSearchResultLB;
    procedure ListBoxClicked(Sender: TObject);
    procedure ListBoxDoubleClicked(Sender: TObject);
    procedure SetItems(Index: Integer; Value: TStrings);
    function GetItems(Index: integer): TStrings;
    fOnSelectionChanged: TNotifyEvent;
    fListBoxFont: TFont;
    fMouseOverIndex: integer;
  public
    function AddResult(const ResultsName: string;
                        const SearchText: string;
                        const ADirectory: string;
                        const AMask: string;
                        const TheOptions: TLazFindInFileSearchOptions): integer;
    function GetSourcePositon: TPoint;
    function GetSourceFileName: string;
    function GetSelectedText: string;
    function GetSelectedMatchPos: TLazSearchMatchPos;
    procedure BringResultsToFront(const APageName: string);
    procedure AddMatch(const AIndex: integer;
                       const Filename: string; const FilePosition: TPoint;
                       const TheText: string;
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
  ShowHint:= True;
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
  const Filename: string; const FilePosition: TPoint;
  const TheText: string;
  const MatchStart: integer; const MatchLen: integer);
var
  CurrentLB: TLazSearchResultLB;
  SearchPos: TLazSearchMatchPos;
  ShownText: String;
begin
  CurrentLB:= GetListBox(AIndex);
  if Assigned(CurrentLB) then
  begin
    SearchPos:= TLazSearchMatchPos.Create;
    SearchPos.MatchStart:= MatchStart;
    SearchPos.MatchLen:= MatchLen;
    SearchPos.Filename:=Filename;
    SearchPos.FilePosition:=FilePosition;
    SearchPos.TheText:=TheText;
    SearchPos.ShownFilename:=SearchPos.Filename;
    ShownText:=SearchPos.ShownFilename
                   +' ('+IntToStr(SearchPos.FilePosition.Y)
                   +','+IntToStr(SearchPos.FilePosition.X)+')'
                   +' '+SearchPos.TheText;
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
    if CurrentLB.Items.Count>0 then begin
      CurrentLB.ItemIndex:= 0;
      CurrentLB.TopIndex:= 0;
    end;
  end;
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

procedure TSearchResultsView.ResultsNoteBookCloseTabclicked(Sender: TObject);
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
      Options:= SearchObj.SearchOptions;
      FileMaskComboBox.Text:= SearchObj.SearchMask;
    end;//with
    MainIDEInterface.FindInFiles(Project1, SearchObj.SearchString);
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
            OnShowHint:= @LazLBShowHint;
            OnMouseMove:= @LazLBMousemove;
            OnMouseWheel:= @LazLBMouseWheel;
            Font.Name:=fListBoxFont.Name;
            Font.Height:=fListBoxFont.Height;
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
                   +' ('+IntToStr(MatchPos.FilePosition.Y)
                   +','+IntToStr(MatchPos.FilePosition.X)+')'
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
  BoldLen: integer;
  TheText: string;
  TheTop: integer;
  MatchPos: TLazSearchMatchPos;
  TextEnd: integer;
  ShownMatchStart: LongInt;
begin
  With Control as TLazSearchResultLB do
  begin
    Canvas.FillRect(ARect);
    if Items.Objects[Index] is TLazSearchMatchPos then
      MatchPos:= TLazSearchMatchPos(Items.Objects[Index])
    else
      MatchPos:= nil;
    TheText:= Items[Index];

    if Assigned(MatchPos) then
    begin
      TheTop:= ARect.Top;
      BoldLen:= MatchPos.MatchLen;
      ShownMatchStart:=length(TheText)-length(MatchPos.TheText)
                       +MatchPos.MatchStart;
      FirstPart:= copy(TheText,1,ShownMatchStart - 1);
      BoldPart:= copy(TheText,ShownMatchStart ,BoldLen);
      LastPart:= copy(TheText, ShownMatchStart + BoldLen,
                      Length(TheText) - (ShownMatchStart + BoldLen) + 2);
      Canvas.TextOut(ARect.Left, TheTop, FirstPart);
      TextEnd:= ARect.Left + Canvas.TextWidth(FirstPart);
      Canvas.Font.Style:= Canvas.Font.Style + [fsBold];
      {TODO: Find out why bold is 1 pixel off in gtk}
      Canvas.TextOut(TextEnd, TheTop, BoldPart);
      TextEnd:= TextEnd + Canvas.TextWidth(BoldPart);
      Canvas.Font.Style:= Canvas.Font.Style  - [fsBold];
      Canvas.TextOut(TextEnd, TheTop, LastPart);
    end//if
    else
    begin
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
  Result:=MatchPos.FilePosition;
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
  if (fUpdateCount = 0) then
    RaiseGDBException('TLazSearchResultLB.EndUpdate');
  dec(fUpdateCount);
  if (fUpdateCount = 0) then
  begin
    ShortenPaths;
    fUpdating:= false;
    for i:= 0 to Items.Count -1 do
    begin
      if Assigned(Items.Objects[i]) then
      begin
        Items.Objects[i].free;
      end;//if
    end;//for
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
      ShownText:=MatchPos.ShownFilename
                     +' ('+IntToStr(MatchPos.FilePosition.Y)
                     +','+IntToStr(MatchPos.FilePosition.X)+')'
                     +' '+MatchPos.TheText;
      SrcList[i]:=ShownText;
      SrcList.Objects[i]:=MatchPos;
    end;
  end;
end;

initialization
  {$I searchresultview.lrs}

end.

