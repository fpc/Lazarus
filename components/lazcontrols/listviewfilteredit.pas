{ ListViewFilterEdit

  Copyright (C) 2012 Lazarus team

  This library is free software; you can redistribute it and/or modify it
  under the same terms as the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.

}
unit ListViewFilterEdit;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, Math, fgl,
  // LCL
  LCLType, Graphics, ComCtrls, EditBtn,
  // LazUtils
  LazUTF8;

type

  //TImageIndexEvent = function (Str: String; Data: TObject;
  //                             var IsEnabled: Boolean): Integer of object;
  TStringArray = array of string;

  { TListViewDataItem }

  TListViewDataItem = record
    Data: Pointer;
    StringArray: TStringArray;
    //constructor Initialize(ColumnCount: integer);   Does not work. Why?
    class operator =(a,b : TListViewDataItem) : Boolean;
  end;
  TListViewDataList = specialize TFPGList<TListViewDataItem>;

  { TListViewFilterEdit }

  TListViewFilterEdit = class(TCustomControlFilterEdit)
  private
    // A control showing the (filtered) data.
    fFilteredListview: TCustomListView;
    // All fields / Just the first field, caption.
    fByAllFields: Boolean;
    // Store/restore the old selections here.
    fSelectionList: TStringList;
    // Data supplied by caller through Items property.
    fOriginalData: TListViewDataList;
    // Data sorted for viewing.
    fFilteredData: TListViewDataList;
    function MatchesFilter(aData: TListViewDataItem; const AFilter: string): Boolean;
    procedure SetFilteredListview(const AValue: TCustomListView);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetLastSelectedIndex: Integer;
    procedure MoveTo(AIndex: Integer; ASelect: Boolean);
    procedure MoveNext(ASelect: Boolean = False); override;
    procedure MovePrev(ASelect: Boolean = False); override;
    procedure MovePageUp(ASelect: Boolean = False); override;
    procedure MovePageDown(ASelect: Boolean = False); override;
    procedure MoveHome(ASelect: Boolean = False); override;
    procedure MoveEnd(ASelect: Boolean = False); override;
    function ReturnKeyHandled: Boolean; override;
    procedure SortAndFilter; override;
    procedure ApplyFilterCore; override;
    function GetDefaultGlyphName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure StoreSelection; override;
    procedure RestoreSelection; override;
  public
    property SelectionList: TStringList read fSelectionList;
    property Items: TListViewDataList read fOriginalData;
  published
    property FilteredListview: TCustomListView read fFilteredListview write SetFilteredListview;
    property ByAllFields: Boolean read fByAllFields write fByAllFields default False;
  end;

implementation

{ TListViewDataItem }
{
constructor TListViewDataItem.Initialize(ColumnCount: integer);
begin
  Data := Nil;
  SetLength(StringArray, ColumnCount);
end;
}
class operator TListViewDataItem. = (a, b: TListViewDataItem): Boolean;
begin
  Result := (a.Data=b.Data) and (a.StringArray=b.StringArray);
end;

{ TListViewFilterEdit }

constructor TListViewFilterEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fOriginalData:=TListViewDataList.Create;
  fSelectionList:=TStringList.Create;
  fFilteredData:=TListViewDataList.Create;
end;

destructor TListViewFilterEdit.Destroy;
begin
  fFilteredData.Free;
  fSelectionList.Free;
  fOriginalData.Free;
  inherited Destroy;
end;

function TListViewFilterEdit.GetDefaultGlyphName: string;
begin
  Result := 'btnfiltercancel';
end;

function TListViewFilterEdit.GetLastSelectedIndex: Integer;
begin
  if fFilteredListview.LastSelected<>nil then
    Result := fFilteredListview.LastSelected.Index
  else
    Result := -1;
end;

function ListItem2Data(AItem: TListItem): TListViewDataItem;
var
  i: Integer;
begin
  Result.Data := AItem.Data;
  SetLength(Result.StringArray, AItem.SubItems.Count+1);
  Result.StringArray[0] := AItem.Caption;
  for i := 0 to AItem.SubItems.Count-1 do
    Result.StringArray[i+1] := AItem.SubItems[i];
end;

procedure Data2ListItem(AData: TListViewDataItem; AItem: TListItem);
var
  i: Integer;
begin
  AItem.Data := AData.Data;
  AItem.Caption := AData.StringArray[0];
  for i := 1 to Length(AData.StringArray)-1 do
    AItem.SubItems.Add(AData.StringArray[i]);
end;

procedure TListViewFilterEdit.SetFilteredListview(const AValue: TCustomListView);
var
  i: Integer;
begin
  if fFilteredListview = AValue then Exit;
  fFilteredListview:=AValue;
  if Assigned(fFilteredListview) then
  begin
    Filter:=Text;
    for i := 0 to fFilteredListview.Items.Count-1 do
      fOriginalData.Add(ListItem2Data(fFilteredListview.Items[i]));
  end;
end;

procedure TListViewFilterEdit.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) and (FilteredListview=AComponent) then
  begin
    IdleConnected:=False;
    fNeedUpdate:=False;
    fFilteredListview:=nil;
  end;
end;

function TListViewFilterEdit.MatchesFilter(aData: TListViewDataItem;
  const AFilter: string): Boolean;
var
  i, EndInd: Integer;
begin
  if fByAllFields then
    EndInd := Pred(Length(aData.StringArray))
  else
    EndInd := 0;
  for i := 0 to EndInd do begin
    if DoFilterItem(aData.StringArray[i], AFilter, aData.Data) then
      Exit(True);
  end;
  Result := False;
end;

procedure TListViewFilterEdit.MoveEnd(ASelect: Boolean);
begin
  if (fFilteredListview = nil) or (fFilteredListview.Items.Count = 0) then Exit;
  MoveTo(fFilteredListview.Items.Count-1, ASelect);
end;

procedure TListViewFilterEdit.MoveHome(ASelect: Boolean);
begin
  if (fFilteredListview = nil) or (fFilteredListview.Items.Count = 0) then Exit;
  MoveTo(0, ASelect);
end;

procedure TListViewFilterEdit.SortAndFilter;
// Copy data from fOriginalData to fSortedData in sorted order
var
  Origi: Integer;
  Data: TListViewDataItem;
begin
  fFilteredData.Clear;
  for Origi:=0 to fOriginalData.Count-1 do begin
    Data:=fOriginalData[Origi];
    if MatchesFilter(Data, Filter) then
      fFilteredData.Add(Data);
  end;
end;

procedure TListViewFilterEdit.ApplyFilterCore;
var
  i: Integer;
begin
  if fFilteredListview = nil then exit;
  fFilteredListview.Clear;
  fFilteredListview.Items.BeginUpdate;
  for i:=0 to fFilteredData.Count-1 do
    Data2ListItem(fFilteredData[i], fFilteredListview.Items.Add);
  fFilteredListview.AlphaSort;         // Always sorted.
  fFilteredListview.Items.EndUpdate;
end;

procedure TListViewFilterEdit.StoreSelection;
var
  i: Integer;
begin
  if fFilteredListview = nil then exit;
  fSelectionList.Clear;
  if fFilteredListview.SelCount > 0 then
    for i := 0 to fFilteredListview.Items.Count-1 do
      if fFilteredListview.Items[i].Selected then
        fSelectionList.Add(fFilteredListview.Items[i].Caption);
end;

procedure TListViewFilterEdit.RestoreSelection;
var
  i: Integer;
begin
  if (fSelectionList.Count > 0) and Assigned(fFilteredListview) then
    for i := 0 to fFilteredListview.Items.Count-1 do
      if fSelectionList.IndexOf(fFilteredListview.Items[i].Caption) > -1 then
        fFilteredListview.Items[i].Selected:=True;
end;

procedure TListViewFilterEdit.MoveNext(ASelect: Boolean);
var
  i: Integer;
begin
  if (fFilteredListview = nil) or (fFilteredListview.Items.Count = 0) then Exit;
  i := GetLastSelectedIndex + 1;
  if i >= fFilteredListview.Items.Count then
    i := fFilteredListview.Items.Count-1;
  MoveTo(i, ASelect);
end;

procedure TListViewFilterEdit.MovePageDown(ASelect: Boolean);
var
  I: Integer;
begin
  if (fFilteredListview = nil) or (fFilteredListview.Items.Count = 0) then Exit;
  I := GetLastSelectedIndex + fFilteredListview.VisibleRowCount;
  if (I < 0) or (I >= fFilteredListview.Items.Count) then
    I := fFilteredListview.Items.Count-1;
  MoveTo(I, ASelect);
end;

procedure TListViewFilterEdit.MovePageUp(ASelect: Boolean);
var
  I: Integer;
begin
  if (fFilteredListview = nil) or (fFilteredListview.Items.Count = 0) then Exit;
  I := GetLastSelectedIndex - fFilteredListview.VisibleRowCount;
  if (I < 0) or (I >= fFilteredListview.Items.Count) then
    I := 0;
  MoveTo(I, ASelect);
end;

procedure TListViewFilterEdit.MovePrev(ASelect: Boolean);
var
  i: Integer;
begin
  if (fFilteredListview = nil) or (fFilteredListview.Items.Count = 0) then Exit;
  i := GetLastSelectedIndex - 1;
  if i < 0 then
    i := 0;
  MoveTo(i, ASelect);
end;

procedure TListViewFilterEdit.MoveTo(AIndex: Integer; ASelect: Boolean);
var
  I: Integer;
begin
  fFilteredListview.BeginUpdate;
  try
    if ASelect and fFilteredListview.MultiSelect then
    begin
      if fFilteredListview.ItemIndex < AIndex then
        for I := Max(0, fFilteredListview.ItemIndex) to AIndex do
          fFilteredListview.Items[I].Selected := True
      else
        for I := Max(0, fFilteredListview.ItemIndex) downto AIndex do
          fFilteredListview.Items[I].Selected := True;
      fFilteredListview.ItemIndex := AIndex;
    end else
    begin
      fFilteredListview.ClearSelection;
      fFilteredListview.ItemIndex := AIndex;
    end;

    if fFilteredListview.LastSelected<>nil then
      fFilteredListview.LastSelected.MakeVisible(False);
  finally
    fFilteredListview.EndUpdate;
  end;
end;

function TListViewFilterEdit.ReturnKeyHandled: Boolean;
// Retuns true if the Return press was forwarded to the ListView
var
  Key: Char;
begin
  if fFilteredListview = nil then
    exit(false);
  Key:=Char(VK_RETURN);
  Result:=Assigned(fFilteredListview.OnKeyPress);
  if Result then
    fFilteredListview.OnKeyPress(fFilteredListview, Key);
end;

end.

