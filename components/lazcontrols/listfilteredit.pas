unit ListFilterEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, LResources, Graphics, Controls, StdCtrls,
  LCLProc, EditBtn, FileUtil;

type

  TImageIndexEvent = function (Str: String; Data: TObject;
                               var IsEnabled: Boolean): Integer of object;

  { TListFilterEdit }

  TListFilterEdit = class(TCustomControlFilterEdit)
  private
    fFilteredListbox: TCustomListbox; // A control showing the (filtered) data.
    fSelectionList: TStringList;      // Store/restore the old selections here.
    // Data supplied by caller through Data property.
    fOriginalData: TStringList;
    // Data sorted for viewing.
    fSortedData: TStringList;
    function CompareFNs(AFilename1,AFilename2: string): integer;
    function GetFirstSelected: Integer;
    procedure SetFilteredListbox(const AValue: TCustomListBox);
    procedure UnselectAll;
  protected
    procedure MoveNext; override;
    procedure MovePrev; override;
    procedure SortAndFilter; override;
    procedure ApplyFilterCore; override;
    function GetDefaultGlyph: TBitmap; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure StoreSelection; override;
    procedure RestoreSelection; override;
  public
    property SelectionList: TStringList read fSelectionList;
    property Data: TStringList read fOriginalData;
  published
    property FilteredListbox: TCustomListBox read fFilteredListbox write SetFilteredListbox;
  end;

var
  ListFilterGlyph: TBitmap;

procedure Register;

implementation

uses CheckLst;

procedure Register;
begin
  {$I listfilteredit_icon.lrs}
  RegisterComponents('LazControls',[TListFilterEdit]);
end;

{ TListBoxFilterEdit }

constructor TListFilterEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fOriginalData:=TStringList.Create;
  fSelectionList:=TStringList.Create;
  fSortedData:=TStringList.Create;
end;

destructor TListFilterEdit.Destroy;
begin
  fSortedData.Free;
  fSelectionList.Free;
  fOriginalData.Free;
  inherited Destroy;
end;

function TListFilterEdit.GetDefaultGlyph: TBitmap;
begin
  Result := ListFilterGlyph;
end;

procedure TListFilterEdit.SetFilteredListbox(const AValue: TCustomListBox);
begin
  if fFilteredListbox = AValue then Exit;
  fFilteredListbox:=AValue;
  if AValue = nil then Exit;
  fOriginalData.Assign(fFilteredListbox.Items);
end;

function TListFilterEdit.CompareFNs(AFilename1,AFilename2: string): integer;
begin
  if SortData then
    Result:=CompareFilenames(AFilename1, AFilename2)
  else
    Result:=0;
end;

procedure TListFilterEdit.SortAndFilter;
// Copy data from fOriginalData to fSortedData in sorted order
var
  Origi, i: Integer;
  s: string;
  Pass, Done: Boolean;
begin
  Done:=False;
  fSortedData.Clear;
  for Origi:=0 to fOriginalData.Count-1 do begin
    s:=fOriginalData[Origi];
    // Filter with event handler if there is one.
    Pass:=False;
    if Assigned(OnFilterItem) then
      Pass:=OnFilterItem(fOriginalData.Objects[Origi], Done);
    // Filter by item's title text if needed.
    if not (Pass or Done) then
      Pass:=(Filter='') or (Pos(Filter,lowercase(s))>0);
    if Pass then begin
      i:=fSortedData.Count-1;
      while i>=0 do begin
        if CompareFNs(s,fSortedData[i])>=0 then break;
        dec(i);
      end;
      fSortedData.InsertObject(i+1, s, fOriginalData.Objects[Origi]);
    end;
  end;
end;

procedure TListFilterEdit.ApplyFilterCore;
var
  i, j: Integer;
  s: string;
  clb: TCustomCheckListBox;
begin
  clb:=Nil;
  if fFilteredListbox is TCustomCheckListBox then
    clb:=TCustomCheckListBox(fFilteredListbox);
  fFilteredListbox.Clear;
  fFilteredListbox.Items.BeginUpdate;
  for i:=0 to fSortedData.Count-1 do begin
    s:=fSortedData[i];
    j:=fFilteredListbox.Items.AddObject(s, fSortedData.Objects[i]);
    if Assigned(fSelectedPart) then
      fFilteredListbox.Selected[i]:=fSelectedPart=fSortedData.Objects[i];
    if Assigned(clb) and Assigned(OnCheckItem) then
      clb.Checked[j]:=OnCheckItem(fSortedData.Objects[i]);
  end;
  fFilteredListbox.Items.EndUpdate;
end;

procedure TListFilterEdit.StoreSelection;
var
  i: Integer;
begin
  fSelectionList.Clear;
  for i := 0 to fFilteredListbox.Count-1 do
    if fFilteredListbox.Selected[i] then
      fSelectionList.Add(fFilteredListbox.Items[i]);
end;

procedure TListFilterEdit.RestoreSelection;
var
  i: Integer;
begin
  for i := 0 to fFilteredListbox.Count-1 do
    if fSelectionList.IndexOf(fFilteredListbox.Items[i])>0 then
      fFilteredListbox.Selected[i]:=True;
end;

function TListFilterEdit.GetFirstSelected: Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to fFilteredListbox.Count - 1 do
    if fFilteredListbox.Selected[i] then
      Exit(i);
end;

procedure TListFilterEdit.UnselectAll;
var
  i: Integer;
begin
  for i := 0 to fFilteredListbox.Count - 1 do
    fFilteredListbox.Selected[i] := False;
end;

procedure TListFilterEdit.MoveNext;
var
  i: Integer;
begin
  i := GetFirstSelected + 1;
  if i < fFilteredListbox.Count then
  begin
    UnselectAll;
    fFilteredListbox.Selected[i] := True;
  end;
end;

procedure TListFilterEdit.MovePrev;
var
  i: Integer;
begin
  i := GetFirstSelected - 1;
  if i >= 0 then
  begin
    UnselectAll;
    fFilteredListbox.Selected[i] := True;
  end;
end;

end.

