unit ListFilterEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, LResources, Graphics, StdCtrls, ComCtrls, EditBtn;

resourcestring
  lisCEFilter = '(Filter)';

type

  TImageIndexEvent = function (Str: String; Data: TObject): Integer of object;

  { TListFilterEdit }

  TListFilterEdit = class(TCustomEditButton)
    procedure FilterEditChange(Sender: TObject);
    procedure FilterEditEnter(Sender: TObject);
    procedure FilterEditExit(Sender: TObject);
    procedure OnIdle(Sender: TObject; var Done: Boolean);
  private
    fFilter: string;
    fNeedUpdate: boolean;
    fIdleConnected: boolean;
    // Data to be filtered. Objects property can contain data, too.
    fData: TStringList;
    fFilteredTreeview: TTreeview;
    fFilteredListbox: TListbox;
    fOnGetImageIndex: TImageIndexEvent;
    procedure SetFilter(const AValue: string);
    procedure SetFilteredTreeview(const AValue: TTreeview);
    procedure SetFilteredListbox(const AValue: TListBox);
    function PassesFilter(Entry: string): boolean;
    procedure ApplyFilterToListBox(AListBox: TListBox);
    procedure ApplyFilterToTreeview(ATreeView: TTreeView);
    procedure SetIdleConnected(const AValue: boolean);
  protected
    function GetDefaultGlyph: TBitmap; override;
    function GetDefaultGlyphName: String; override;
    procedure DoButtonClick (Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ApplyFilter(Immediately: Boolean = False);
    procedure Invalidate;
  public
    property Filter: string read fFilter write SetFilter;
    property IdleConnected: boolean read fIdleConnected write SetIdleConnected;
    property Data: TStringList read fData;
  published
    // TListFilterEdit properties.
    property FilteredTreeview: TTreeview read fFilteredTreeview write SetFilteredTreeview;
    property FilteredListbox: TListBox read fFilteredListbox write SetFilteredListbox;
    property OnGetImageIndex: TImageIndexEvent read fOnGetImageIndex write fOnGetImageIndex;
    // TEditButton properties.
    property ButtonWidth;
    property DirectInput;
    property ButtonOnlyWhenFocused;
    // property Glyph;
    property NumGlyphs;
    property Flat;
    // Other properties
    property Align;
    property Anchors;
    property BidiMode;
    property BorderSpacing;
    property BorderStyle;
    property AutoSize;
    property AutoSelect;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property MaxLength;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnUTF8KeyPress;
  end;

var
  ListFilterGlyph: TBitmap;

const
  ResBtnListFilter = 'btnfiltercancel';

procedure Register;

implementation

procedure Register;
begin
  {$I listfilteredit_icon.lrs}
  RegisterComponents('LazControls',[TListFilterEdit]);
end;

{ TListBoxFilterEdit }

constructor TListFilterEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fData:=TStringList.Create;
  Button.Enabled:=False;
  Font.Color:=clBtnShadow;
  Text:=lisCEFilter;
  OnChange:=@FilterEditChange;
  OnEnter:=@FilterEditEnter;
  OnExit:=@FilterEditExit;
end;

destructor TListFilterEdit.Destroy;
begin
  fData.Free;
  inherited Destroy;
end;

procedure TListFilterEdit.FilterEditChange(Sender: TObject);
begin
  Filter:=Text;
end;

procedure TListFilterEdit.FilterEditEnter(Sender: TObject);
begin
  if Text=lisCEFilter then
    Text:='';
end;

procedure TListFilterEdit.FilterEditExit(Sender: TObject);
begin
  Filter:=Text;
end;

procedure TListFilterEdit.DoButtonClick(Sender: TObject);
begin
  Filter:='';
end;

procedure TListFilterEdit.SetFilter(const AValue: string);
var
  NewValue: String;
begin
  NewValue:=LowerCase(AValue);
  if AValue=lisCEFilter then
    NewValue:='';
  fFilter:=NewValue;
  Button.Enabled:=Filter<>'';
  if (Filter='') and not Focused then begin
    Text:=lisCEFilter;
    Font.Color:=clBtnShadow;
  end
  else begin
    Text:=Filter;
    Font.Color:=clDefault;
  end;
  ApplyFilter;
end;

procedure TListFilterEdit.OnIdle(Sender: TObject; var Done: Boolean);
begin
  if fNeedUpdate then
    ApplyFilter(true);
  IdleConnected:=false;
end;

procedure TListFilterEdit.SetIdleConnected(const AValue: boolean);
begin
  if fIdleConnected=AValue then exit;
  fIdleConnected:=AValue;
  if fIdleConnected then
    Application.AddOnIdleHandler(@OnIdle)
  else
    Application.RemoveOnIdleHandler(@OnIdle);
end;

function TListFilterEdit.GetDefaultGlyph: TBitmap;
begin
  Result := ListFilterGlyph;
end;

function TListFilterEdit.GetDefaultGlyphName: String;
begin
  Result := ResBtnListFilter;
end;

procedure TListFilterEdit.SetFilteredTreeview(const AValue: TTreeview);
begin
  if Assigned(fFilteredListbox) then
    raise Exception.Create('Sorry, both Treeview and ListBox should not be assigned.');
  fFilteredTreeview := AValue;
end;

procedure TListFilterEdit.SetFilteredListbox(const AValue: TListBox);
begin
  if Assigned(fFilteredTreeview) then
    raise Exception.Create('Sorry, both Treeview and ListBox should not be assigned.');
  fFilteredListbox := AValue;
end;

function TListFilterEdit.PassesFilter(Entry: string): boolean;
begin
  Result:=(Filter='') or (System.Pos(Filter,lowercase(Entry))>0);
end;

procedure TListFilterEdit.ApplyFilterToListBox(AListBox: TListBox);
begin
  raise Exception.Create('Under construction.');
end;

procedure TListFilterEdit.ApplyFilterToTreeview(ATreeView: TTreeView);
var
  TVNode: TTreeNode;
  ImgIndex, i: Integer;
begin
  fNeedUpdate:=false;
  ImgIndex:=-1;
  fData.Sorted:=True;
  ATreeView.BeginUpdate;
  ATreeView.Items.Clear;
  for i:=0 to fData.Count-1 do
    if PassesFilter(fData[i]) then begin
      TVNode:=ATreeView.Items.Add(nil,fData[i]);
      if Assigned(OnGetImageIndex) then
        ImgIndex:=OnGetImageIndex(fData[i], fData.Objects[i]);
      TVNode.ImageIndex:=ImgIndex;
      TVNode.SelectedIndex:=ImgIndex;
    end;
  ATreeView.EndUpdate;
end;

procedure TListFilterEdit.ApplyFilter(Immediately: Boolean);
begin
  if Immediately then begin
    if Assigned(fFilteredTreeview) then
      ApplyFilterToTreeview(fFilteredTreeview)
    else
      ApplyFilterToListBox(fFilteredListbox);
  end
  else begin
    if csDestroying in ComponentState then exit;
    Invalidate;
  end;
end;

procedure TListFilterEdit.Invalidate;
begin
  fNeedUpdate:=true;
  IdleConnected:=true;
end;


end.

