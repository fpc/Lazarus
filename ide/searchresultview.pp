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
  Classes, SysUtils, Controls, StdCtrls, Forms, LResources, IDEProcs,
  IDEOptionDefs, EnvironmentOpts, LazarusIDEStrConsts;

type
  TSearchResultsView = class(TForm)
    SearchResultView : TListBox;
    procedure SearchResultViewDblClicked(Sender: TObject);
    Procedure SearchResultViewClicked(sender : TObject);
  private
    FDirectories: TStringList;
    FLastLineIsProgress: boolean;
    FOnSelectionChanged: TNotifyEvent;
    function GetDirectory: string;
    Function GetSearchResult: String;
    procedure SetLastLineIsProgress(const AValue: boolean);
  protected
    fBlockCount: integer;
    Function GetSelectedLineIndex: Integer;
    procedure SetSelectedLineIndex(const AValue: Integer);
    procedure SetMsgDirectory(Index: integer; const CurDir: string);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Add(const Msg, CurDir: String; ProgressLine: boolean);
    procedure AddMsg(const Msg, CurDir: String);
    procedure AddProgress(const Msg, CurDir: String);
    procedure AddSeparator;
    procedure ClearTillLastSeparator;
    procedure ShowTopSearchResult;
    function MsgCount: integer;
    procedure Clear;
    procedure GetSearchResultAt(Index: integer; var Msg, MsgDirectory: string);
    procedure BeginBlock;
    procedure EndBlock;
  public
    property LastLineIsProgress: boolean read FLastLineIsProgress
                                         write SetLastLineIsProgress;
    property SearchResult: String read GetSearchResult;
    property Directory: string read GetDirectory;
    property SelectedSearchResultIndex: Integer read GetSelectedLineIndex
                                           write SetSelectedLineIndex;
    property OnSelectionChanged: TNotifyEvent read FOnSelectionChanged
                                              write FOnSelectionChanged;
  end;

var
  SearchResultsView: TSearchResultsView;


implementation

const SeparatorLine = '---------------------------------------------';

{ TSearchResultsView }


{------------------------------------------------------------------------------
  TSearchResultsView.Create
------------------------------------------------------------------------------}
constructor TSearchResultsView.Create(TheOwner : TComponent);
var ALayout: TIDEWindowLayout;
Begin
  inherited Create(TheOwner);
  if LazarusResources.Find(ClassName)=nil then begin
    Caption:=lisMenuViewSearchResults;
    SearchResultView := TListBox.Create(Self);
    With SearchResultView do Begin
      Parent:= Self;
      Align:= alClient;
    end;
  end;
  Name := NonModalIDEWindowNames[nmiwSearchResultsViewName];
  ALayout:=EnvironmentOptions.IDEWindowLayoutList.
                                               ItemByEnum(nmiwSearchResultsViewName);
  ALayout.Form:=TForm(Self);
  ALayout.Apply;
end;

destructor TSearchResultsView.Destroy;
begin
  FreeAndNil(FDirectories);
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  TSearchResultsView.Add
------------------------------------------------------------------------------}
Procedure TSearchResultsView.Add(const Msg, CurDir: String; ProgressLine: boolean);
var
  i: Integer;
Begin
  if FLastLineIsProgress then begin
    SearchResultView.Items[SearchResultView.Items.Count-1]:=Msg;
  end else begin
    SearchResultView.Items.Add(Msg);
  end;
  FLastLineIsProgress:=ProgressLine;
  i:=SearchResultView.Items.Count-1;
  SetMsgDirectory(i,CurDir);
  SearchResultView.TopIndex:=SearchResultView.Items.Count-1;
end;

procedure TSearchResultsView.AddMsg(const Msg, CurDir: String);
begin
  Add(Msg,CurDir,false);
end;

procedure TSearchResultsView.AddProgress(const Msg, CurDir: String);
begin
  Add(Msg,CurDir,true);
end;

Procedure TSearchResultsView.AddSeparator;
begin
  Add(SeparatorLine,'',false);
end;

procedure TSearchResultsView.ClearTillLastSeparator;
var LastSeparator: integer;
begin
  with SearchResultView do begin
    LastSeparator:=Items.Count-1;
    while (LastSeparator>=0) and (Items[LastSeparator]<>SeparatorLine) do
      dec(LastSeparator);
    if LastSeparator>=0 then begin
      while (Items.Count>LastSeparator) do
        Items.Delete(Items.Count-1);
      FLastLineIsProgress:=false;
    end;
  end;
end;

procedure TSearchResultsView.ShowTopSearchResult;
begin
  if SearchResultView.Items.Count>0 then
    SearchResultView.TopIndex:=0;
end;

function TSearchResultsView.MsgCount: integer;
begin
  Result:=SearchResultView.Items.Count;
end;

{------------------------------------------------------------------------------
  TSearchResultsView.Clear
------------------------------------------------------------------------------}
Procedure  TSearchResultsView.Clear;
Begin
  if fBlockCount>0 then exit;
  SearchResultView.Clear;
  FLastLineIsProgress:=false;
  if not Assigned(SearchResultsView.SearchResultView.OnClick) then
    SearchResultView.OnClick := @SearchResultViewClicked;
  if not Assigned(SearchResultsView.SearchResultView.OnDblClick) then
    SearchResultView.OnDblClick :=@SearchResultViewDblClicked;
end;

procedure TSearchResultsView.GetSearchResultAt(Index: integer;
  var Msg, MsgDirectory: string);
begin
  // consistency checks
  if (Index<0) then
    RaiseException('TSearchResultsView.GetSearchResultAt');
  if SearchResultView.Items.Count<=Index then
    RaiseException('TSearchResultsView.GetSearchResultAt');
  if (FDirectories=nil) then
    RaiseException('TSearchResultsView.GetSearchResultAt');
  if (FDirectories.Count<=Index) then
    RaiseException('TSearchResultsView.GetSearchResultAt');
  Msg:=SearchResultView.Items[Index];
  MsgDirectory:=FDirectories[Index];
end;

procedure TSearchResultsView.BeginBlock;
begin
  Clear;
  inc(fBlockCount);
end;

procedure TSearchResultsView.EndBlock;
begin
  if fBlockCount<=0 then RaiseException('TSearchResultsView.EndBlock Internal Error');
  dec(fBlockCount);
end;

{------------------------------------------------------------------------------
  TSearchResultsView.GetSearchResult
------------------------------------------------------------------------------}
Function TSearchResultsView.GetSearchResult: String;
Begin
  Result := '';
  if (SearchResultView.Items.Count > 0) and (SearchResultView.SelCount > 0) then
    Result := SearchResultView.Items.Strings[GetSelectedLineIndex];
end;

procedure TSearchResultsView.SearchResultViewDblClicked(Sender: TObject);
begin
  if not EnvironmentOptions.MsgViewDblClickJumps then exit;
  if (SearchResultView.Items.Count > 0) and (SearchResultView.SelCount > 0) then Begin
    If Assigned(OnSelectionChanged) then
      OnSelectionChanged(self);
  end;
end;

Procedure TSearchResultsView.SearchResultViewClicked(sender : TObject);
begin
  if EnvironmentOptions.MsgViewDblClickJumps then exit;
  if (SearchResultView.Items.Count > 0) and (SearchResultView.SelCount > 0) then Begin
    If Assigned(OnSelectionChanged) then
      OnSelectionChanged(self);
  end;
end;

function TSearchResultsView.GetDirectory: string;
var
  i: Integer;
begin
  Result := '';
  i:=GetSelectedLineIndex;
  if (FDirectories<>nil) and (FDirectories.Count>i) then
    Result := FDirectories[i];
end;

Function TSearchResultsView.GetSelectedLineIndex : Integer;
var
  I : Integer;
Begin
  Result := -1;
  if (SearchResultView.Items.Count > 0) and (SearchResultView.SelCount > 0) then Begin
    for i := 0 to SearchResultView.Items.Count-1 do
    Begin
      if SearchResultView.Selected[I] then
        Begin
	  Result := I;
          Break;
        end;
    end;
  end;
end;

procedure TSearchResultsView.SetLastLineIsProgress(const AValue: boolean);
begin
  if FLastLineIsProgress=AValue then exit;
  if FLastLineIsProgress then
    SearchResultView.Items.Delete(SearchResultView.Items.Count-1);
  FLastLineIsProgress:=AValue;
end;

procedure TSearchResultsView.SetSelectedLineIndex(const AValue: Integer);
begin
  SearchResultView.ItemIndex:=AValue;
  SearchResultView.TopIndex:=SearchResultView.ItemIndex;
end;

procedure TSearchResultsView.SetMsgDirectory(Index: integer; const CurDir: string);
begin
  if FDirectories=nil then FDirectories:=TStringList.Create;
  while FDirectories.Count<=Index do FDirectories.Add('');
  FDirectories[Index]:=CurDir;
end;

initialization
  SearchResultsView:=nil;
  { $I msgview.lrs}


end.


