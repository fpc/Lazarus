unit object_inspector;
{
  Author: Mattias Gaertner

  Abstract:
   This unit defines the TObjectInspector which is a descendent of TCustomForm.
   It uses TOIPropertyGrid and TOIPropertyGridRow which are also defined in this
   unit. The object inspector uses property editors (see TPropertyEditor) to
   display and control properties, thus the object inspector is merely an
   object viewer than an editor. The property editors do the real work.

  ToDo:
   - connect to TFormEditor
   - TCustomComboBox has a bug: it can not store objects
   - MouseDown is always fired two times -> workaround
   - clipping (almost everywhere)
   - scrolling with TrackBar
   - ScrollBar instead of TrackBar
   - TCustomComboBox don't know custom draw yet
   - improve TextHeight function
   - combobox can't sort (exception)
   - TEdit.OnChange and TEdit.OnExit don't work
   - TEdit has no ReadOnly and Maxlength
   - Events
   - backgroundcolor=clNone

   - a lot more ...
}

{$MODE OBJFPC}

interface

uses
  Forms, SysUtils, Buttons, Classes, Graphics, StdCtrls, LCLLinux, Controls,
  ComCtrls, ExtCtrls, Prop_Edits, TypInfo;

type
  TOIPropertyGrid = class;

  TOIPropertyGridRow = class
  private
    FTop:integer;
    FHeight:integer;
    FLvl:integer;
    FName:string;
    FExpanded: boolean;
    FTree:TOIPropertyGrid;
    FChildCount:integer;
    FPriorBrother,
    FFirstChild,
    FLastChild,
    FNextBrother,
    FParent:TOIPropertyGridRow;
    FEditor: TPropertyEditor;
    procedure GetLvl;
  public
    Index:integer;
    property Editor:TPropertyEditor read FEditor;
    property Top:integer read FTop write FTop;
    property Height:integer read FHeight write FHeight;
    function Bottom:integer;
    property Lvl:integer read FLvl;
    property Name:string read FName;
    property Expanded:boolean read FExpanded;
    property Tree:TOIPropertyGrid read FTree;
    property Parent:TOIPropertyGridRow read FParent;
    property ChildCount:integer read FChildCount;
    property FirstChild:TOIPropertyGridRow read FFirstChild;
    property LastChild:TOIPropertyGridRow read FFirstChild;
    property NextBrother:TOIPropertyGridRow read FNextBrother;
    property PriorBrother:TOIPropertyGridRow read FPriorBrother;
    constructor Create(PropertyTree:TOIPropertyGrid;  PropEditor:TPropertyEditor;
       ParentNode:TOIPropertyGridRow);
    destructor Destroy; override;
  end;

  //----------------------------------------------------------------------------
  TOIPropertyGrid = class(TCustomControl)
  private
    FComponentList: TComponentSelectionList;
    FFilter: TTypeKinds;
    FItemIndex:integer;
    FChangingItemIndex:boolean;
    FRows:TList;
    FExpandingRow:TOIPropertyGridRow;
    FTopY:integer;
    FDefaultItemHeight:integer;
    FSplitterX:integer;
    FIndent:integer;
    FBackgroundColor:TColor;
    FNameFont,FValueFont:TFont;
    FCurrentEdit:TWinControl;  // nil or ValueEdit or ValueComboBox
    FCurrentButton:TWinControl; // nil or ValueButton
    FDragging:boolean;
    FOldMouseDownY:integer;  // XXX workaround

    function GetRow(Index:integer):TOIPropertyGridRow;
    function GetRowCount:integer;
    procedure ClearRows;
    procedure SetItemIndex(NewIndex:integer);

    procedure SetItemsTops;
    procedure AlignEditComponents;
    procedure EndDragSplitter;
    procedure SetSplitterX(const NewValue:integer);
    procedure SetTopY(const NewValue:integer);

    function GetTreeIconX(Index:integer):integer;
    function RowRect(ARow:integer):TRect;
    procedure PaintRow(ARow:integer);

    procedure SetSelections(const NewSelections:TComponentSelectionList);

    procedure AddPropertyEditor(PropEditor: TPropertyEditor);
    procedure AddStringToComboBox(const s:string);
    procedure ExpandRow(Index:integer);
    procedure ShrinkRow(Index:integer);
    procedure AddSubEditor(PropEditor:TPropertyEditor);

    procedure SetRowValue;
    procedure ValueEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ValueEditExit(Sender: TObject);
    procedure ValueEditChange(Sender: TObject);
    procedure ValueComboBoxExit(Sender: TObject);
    procedure ValueComboBoxChange(Sender: TObject);
    procedure ValueButtonClick(Sender: TObject);
    procedure TrackBarChange(Sender:TObject);
  public
    ValueEdit:TEdit;
    ValueComboBox:TComboBox;
    ValueButton:TButton;
    TrackBar:TTrackBar;

    property Selections:TComponentSelectionList read FComponentList write SetSelections;
    procedure BuildPropertyList;

    property RowCount:integer read GetRowCount;
    property Rows[Index:integer]:TOIPropertyGridRow read GetRow;

    property TopY:integer read FTopY write SetTopY;
    function GridHeight:integer;
    property DefaultItemHeight:integer read FDefaultItemHeight write FDefaultItemHeight;
    property SplitterX:integer read FSplitterX write SetSplitterX;
    property Indent:integer read FIndent write FIndent;
    property BackgroundColor:TColor read FBackgroundColor write FBackgroundColor;
    property NameFont:TFont read FNameFont write FNameFont;
    property ValueFont:TFont read FValueFont write FValueFont;
    property ItemIndex:integer read FItemIndex write SetItemIndex;

    procedure MouseDown(Button:TMouseButton; Shift:TShiftState; X,Y:integer);  override;
    procedure MouseMove(Shift:TShiftState; X,Y:integer);  override;
    procedure MouseUp(Button:TMouseButton; Shift:TShiftState; X,Y:integer);  override;
    function MouseToIndex(y:integer;MustExist:boolean):integer;

    procedure SetBounds(aLeft,aTop,aWidth,aHeight:integer); override;
    procedure Paint;  override;
    procedure Clear;
    constructor Create(AOwner:TComponent; TypeFilter:TTypeKinds);
    destructor Destroy;  override;
  end;

  //============================================================================
  TObjectInspector = class (TCustomForm)
  private
    FComponentList: TComponentSelectionList;
    FRootComponent:TComponent;
    function ComponentToString(c:TComponent):string;
    procedure SetRootComponent(Value:TComponent);
    procedure SetSelections(const NewSelections:TComponentSelectionList);
    procedure AvailComboBoxChange(Sender:TObject);
  public
    AvailCompsComboBox : TComboBox;
    NoteBook:TNoteBook;
    PropertyGrid:TOIPropertyGrid;
    EventGrid:TOIPropertyGrid;
    procedure SetBounds(aLeft,aTop,aWidth,aHeight:integer); override;
    property Selections:TComponentSelectionList read FComponentList write SetSelections;
    procedure RefreshSelections;
    procedure FillComponentComboBox;
    property RootComponent:TComponent read FRootComponent write SetRootComponent;
    procedure DoInnerResize;
    procedure Paint;  override;
    constructor Create(AOwner: TComponent); override;
  end;

//******************************************************************************

implementation

{ TOIPropertyGrid }

constructor TOIPropertyGrid.Create(AOwner:TComponent; TypeFilter:TTypeKinds);
begin
  inherited Create(AOwner);
  SetBounds(1,1,200,300);
  Visible:=false;
  ControlStyle:=ControlStyle+[csAcceptsControls];

  FComponentList:=TComponentSelectionList.Create;
  FFilter:=TypeFilter;
  FItemIndex:=-1;
  FChangingItemIndex:=false;
  FRows:=TList.Create;
  FExpandingRow:=nil;
  FDragging:=false;

  // visible values
  FTopY:=0;
  FSplitterX:=100;
  FIndent:=9;
  FBackgroundColor:=clBtnFace;
  FNameFont:=TFont.Create;
  FNameFont.Color:=clWindowText;
  FValueFont:=TFont.Create;
  FValueFont.Color:=clActiveCaption;
  BorderWidth:=1;

  // create sub components
  FCurrentEdit:=nil;
  FCurrentButton:=nil;

  TrackBar:=TTrackBar.Create(AOwner);
  with TrackBar do begin
    Parent:=Self;
  	Orientation:=trVertical;
    Align:=alRight;
    OnChange:=@TrackBarChange;
    Visible:=true;
    Enabled:=true;
  end;

  ValueEdit:=TEdit.Create(Self);
  with ValueEdit do begin
    Parent:=Self;
    OnKeyDown:=@ValueEditKeyDown;
    OnExit:=@ValueEditExit;
    OnChange:=@ValueEditChange;
    Visible:=false;
    Enabled:=false;
  end;

  ValueComboBox:=TComboBox.Create(Self);
  with ValueComboBox do begin
    OnExit:=@ValueComboBoxExit;
    OnChange:=@ValueComboBoxChange;
    Visible:=false;
    Enabled:=false;
    Parent:=Self;
  end;

  ValueButton:=TButton.Create(Self);
  with ValueButton do begin
    Visible:=false;
    Enabled:=false;
    OnClick:=@ValueButtonClick;
    Parent:=Self;
  end;

  FDefaultItemHeight:=ValueComboBox.Height-3;

  BuildPropertyList;
end;

destructor TOIPropertyGrid.Destroy;
var a:integer;
begin
  for a:=0 to FRows.Count-1 do Rows[a].Free;
  FRows.Free;
  FComponentList.Free;
  FValueFont.Free;
  FNameFont.Free;
  inherited Destroy;
end;

procedure TOIPropertyGrid.SetSelections(
  const NewSelections:TComponentSelectionList);
var a:integer;
begin
  ItemIndex:=-1;
  for a:=0 to FRows.Count-1 do
    Rows[a].Free;
  FRows.Clear;
  FComponentList.Assign(NewSelections);
  BuildPropertyList;
end;

procedure TOIPropertyGrid.SetRowValue;
var CurrRow:TOIPropertyGridRow;
  NewValue:string;
begin
  if (FChangingItemIndex=false) and (FCurrentEdit<>nil)
  and (FItemIndex>=0) and (FItemIndex<FRows.Count) then begin
    CurrRow:=Rows[FItemIndex];
      if FCurrentEdit=ValueEdit then
        NewValue:=ValueEdit.Text
      else
        NewValue:=ValueComboBox.Text;
    if NewValue<>CurrRow.Editor.GetVisualValue then begin
      try
        CurrRow.Editor.SetValue(NewValue);
      except
      end;
    end;
  end;
end;

procedure TOIPropertyGrid.ValueEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=VK_Return then
    SetRowValue;
end;

procedure TOIPropertyGrid.ValueEditExit(Sender: TObject);
begin
  SetRowValue;
end;

procedure TOIPropertyGrid.ValueEditChange(Sender: TObject);
var CurrRow:TOIPropertyGridRow;
begin
  if (FCurrentEdit<>nil) and (FItemIndex>=0) and (FItemIndex<FRows.Count) then
  begin
    CurrRow:=Rows[FItemIndex];
    if paAutoUpdate in CurrRow.Editor.GetAttributes then
      SetRowValue;
  end;
end;

procedure TOIPropertyGrid.ValueComboBoxExit(Sender: TObject);
begin
  SetRowValue;
end;

procedure TOIPropertyGrid.ValueComboBoxChange(Sender: TObject);
begin
  SetRowValue;
end;

procedure TOIPropertyGrid.ValueButtonClick(Sender: TObject);
var CurrRow:TOIPropertyGridRow;
begin
  if (FCurrentEdit<>nil) and (FItemIndex>=0) and (FItemIndex<FRows.Count) then
  begin
    CurrRow:=Rows[FItemIndex];
    if paDialog in CurrRow.Editor.GetAttributes then begin
      CurrRow.Editor.Edit;
      if FCurrentEdit=ValueEdit then
        ValueEdit.Text:=CurrRow.Editor.GetVisualValue
      else
        ValueComboBox.Text:=CurrRow.Editor.GetVisualValue;
    end;
  end;
end;

procedure TOIPropertyGrid.TrackBarChange(Sender:TObject);
begin
  if TrackBar.Position<>FTopY then begin
    FTopY:=TrackBar.Position;
    Invalidate;
  end;
end;

procedure TOIPropertyGrid.SetItemIndex(NewIndex:integer);
var NewRow:TOIPropertyGridRow;
  NewValue:string;
begin
  // XXX
  SetRowValue;
  FChangingItemIndex:=true;
  if (FItemIndex<>NewIndex) then begin
    if (FItemIndex>=0) and (FItemIndex<FRows.Count) then
      Rows[FItemIndex].Editor.Deactivate;
    FItemIndex:=NewIndex;
    if FCurrentEdit<>nil then begin
      FCurrentEdit.Visible:=false;
      FCurrentEdit.Enabled:=false;
      FCurrentEdit:=nil;
    end;
    if FCurrentButton<>nil then begin
      FCurrentButton.Visible:=false;
      FCurrentButton.Enabled:=false;
      FCurrentButton:=nil;
    end;
    if (NewIndex>=0) and (NewIndex<FRows.Count) then begin
      NewRow:=Rows[NewIndex];
      NewRow.Editor.Activate;
      if paDialog in NewRow.Editor.GetAttributes then begin
        FCurrentButton:=ValueButton;
        FCurrentButton.Visible:=true;
      end;
      NewValue:=NewRow.Editor.GetVisualValue;
      if paValueList in NewRow.Editor.GetAttributes then begin
        FCurrentEdit:=ValueComboBox;
        ValueComboBox.MaxLength:=NewRow.Editor.GetEditLimit;
        ValueComboBox.Items.BeginUpdate;
        ValueComboBox.Items.Text:='';
        // XXX
        //ValueComboBox.Sorted:=paSortList in Node.Director.GetAttributes;
        NewRow.Editor.GetValues(@AddStringToComboBox);
        ValueComboBox.Text:=NewValue;
        ValueComboBox.Items.EndUpdate;
        ValueComboBox.Visible:=true;
      end else begin
        FCurrentEdit:=ValueEdit;
        // XXX
        //ValueEdit.ReadOnly:=paReadOnly in NewRow.Editor.GetAttributes;
        //ValueEdit.MaxLength:=NewRow.Editor.GetEditLimit;
        ValueEdit.Text:=NewValue;
        ValueEdit.Visible:=true;
      end;
      AlignEditComponents;
      if FCurrentEdit<>nil then begin
        FCurrentEdit.Enabled:=true;
        if (FDragging=false) and (FCurrentEdit.Showing) then
          FCurrentEdit.SetFocus;
      end;
      if FCurrentButton<>nil then
        FCurrentButton.Enabled:=true;
    end;
  end;
  FChangingItemIndex:=false;
end;

function TOIPropertyGrid.GetRowCount:integer;
begin
  Result:=FRows.Count;
end;

procedure TOIPropertyGrid.BuildPropertyList;
var a:integer;
begin
  ItemIndex:=-1;
  for a:=0 to FRows.Count-1 do Rows[a].Free;
  FRows.Clear;
  GetComponentProperties(FComponentList,FFilter,@AddPropertyEditor);
  SetItemsTops;
  Invalidate;
end;

procedure TOIPropertyGrid.AddPropertyEditor(PropEditor: TPropertyEditor);
var NewRow:TOIPropertyGridRow;
begin
  NewRow:=TOIPropertyGridRow.Create(Self,PropEditor,nil);
  FRows.Add(NewRow);
  if FRows.Count>1 then begin
    NewRow.FPriorBrother:=Rows[FRows.Count-2];
    NewRow.FPriorBrother.FNextBrother:=NewRow;
  end;
end;

procedure TOIPropertyGrid.AddStringToComboBox(const s:string);
var NewIndex:integer;
begin
  NewIndex:=ValueComboBox.Items.Add(s);
  if ValueComboBox.Text=s then
    ValueComboBox.ItemIndex:=NewIndex;
end;

procedure TOIPropertyGrid.ExpandRow(Index:integer);
begin
  FExpandingRow:=Rows[Index];
  if (FExpandingRow.Expanded)
  or (not (paSubProperties in FExpandingRow.Editor.GetAttributes))
  then begin
    FExpandingRow:=nil;
    exit;
  end;
  FExpandingRow.Editor.GetProperties(@AddSubEditor);
  SetItemsTops;
  FExpandingRow.FExpanded:=true;
  FExpandingRow:=nil;
  Invalidate;
end;

procedure TOIPropertyGrid.ShrinkRow(Index:integer);
var CurrRow:TOIPropertyGridRow;
  StartIndex,EndIndex,a:integer;
begin
  CurrRow:=Rows[Index];
  if (not CurrRow.Expanded) then exit;
  if CurrRow.NextBrother=nil then StartIndex:=FRows.Count-1
  else StartIndex:=CurrRow.NextBrother.Index-1;
  EndIndex:=CurrRow.Index+1;
  for a:=StartIndex downto EndIndex do begin
    Rows[a].Free;
    FRows.Delete(a);
  end;
  SetItemsTops;
  CurrRow.FExpanded:=false;
  Parent.Invalidate;
  Invalidate;
end;

procedure TOIPropertyGrid.AddSubEditor(PropEditor:TPropertyEditor);
var NewRow:TOIPropertyGridRow;
  NewIndex:integer;
begin
  NewRow:=TOIPropertyGridRow.Create(Self,PropEditor,FExpandingRow);
  NewIndex:=FExpandingRow.Index+1+FExpandingRow.ChildCount;
  FRows.Insert(NewIndex,NewRow);
  if FExpandingRow.FFirstChild=nil then
    FExpandingRow.FFirstChild:=NewRow;
  NewRow.FPriorBrother:=FExpandingRow.FLastChild;
  FExpandingRow.FLastChild:=NewRow;
  if NewRow.FPriorBrother<>nil then NewRow.FPriorBrother.FNextBrother:=NewRow;
  inc(FExpandingRow.FChildCount);
end;

function TOIPropertyGrid.MouseToIndex(y:integer;MustExist:boolean):integer;
var l,r,m:integer;
begin
  l:=0;
  r:=FRows.Count-1;
  inc(y,FTopY);
  while (l<=r) do begin
    m:=(l+r) shr 1;
    if Rows[m].Top>y then begin
      r:=m-1;
    end else if Rows[m].Bottom<=y then begin
      l:=m+1;
    end else begin
      Result:=m;  exit;
    end;
  end;
  if (MustExist=false) and (FRows.Count>0) then begin
    if y<0 then Result:=0
    else Result:=FRows.Count-1;
  end else Result:=-1;
end;

procedure TOIPropertyGrid.MouseDown(Button:TMouseButton;  Shift:TShiftState;
  X,Y:integer);
var IconX,Index:integer;
  PointedRow:TOIpropertyGridRow;
begin
  //ShowMessageDialog('X'+IntToStr(X)+',Y'+IntToStr(Y));
  //inherited MouseDown(Button,Shift,X,Y);
  // XXX
  // the MouseDown event is fired two times
  // this is a workaround
  if FOldMouseDownY=Y then begin
    FOldMouseDownY:=-1;
    exit;
  end else FOldMouseDownY:=Y;

  if Button=mbLeft then begin
    if Cursor=crHSplit then begin
      FDragging:=true;
    end else begin
      Index:=MouseToIndex(Y,false);
      ItemIndex:=Index;
      if (Index>=0) and (Index<FRows.Count) then begin
        IconX:=GetTreeIconX(Index);
        if (X>=IconX) and (X<=IconX+FIndent) then begin
          PointedRow:=Rows[Index];
          if paSubProperties in PointedRow.Editor.GetAttributes then begin
            if PointedRow.Expanded then
              ShrinkRow(Index)
            else
              ExpandRow(Index);
          end;
        end;
      end;
    end;
  end;
end;

procedure TOIPropertyGrid.MouseMove(Shift:TShiftState;  X,Y:integer);
var SplitDistance:integer;
begin
  //inherited MouseMove(Shift,X,Y);
  SplitDistance:=X-SplitterX;
  if FDragging then begin
    if ssLeft in Shift then begin
      SplitterX:=SplitterX+SplitDistance;
    end else begin
      EndDragSplitter;
    end;
  end else begin
    if (abs(SplitDistance)<=2) then begin
      Cursor:=crHSplit;
    end else begin
      Cursor:=crDefault;
    end;
  end;
end;

procedure TOIPropertyGrid.MouseUp(Button:TMouseButton;  Shift:TShiftState;
X,Y:integer);
begin
  //inherited MouseUp(Button,Shift,X,Y);
  if FDragging then EndDragSplitter;
end;

procedure TOIPropertyGrid.EndDragSplitter;
begin
  if FDragging then begin
    Cursor:=crDefault;
    FDragging:=false;
    if FCurrentEdit<>nil then FCurrentEdit.SetFocus;
  end;
end;

procedure TOIPropertyGrid.SetSplitterX(const NewValue:integer);
begin
  if FSplitterX<>NewValue then begin
    FSplitterX:=NewValue;
    AlignEditComponents;
    Invalidate;
  end;
end;

procedure TOIPropertyGrid.SetTopY(const NewValue:integer);
begin
  if FTopY<>NewValue then begin
    FTopY:=NewValue;
    if FTopY<0 then FTopY:=0;
    if FTopY>TrackBar.Max then FTopY:=TrackBar.Max;
    TrackBar.Position:=FTopY;
    Invalidate;
  end;
end;

procedure TOIPropertyGrid.SetBounds(aLeft,aTop,aWidth,aHeight:integer);
var scrollmax:integer;
begin
  inherited SetBounds(aLeft,aTop,aWidth,aHeight);
  if Visible then begin
    AlignEditComponents;
    scrollmax:=GridHeight-Height;
    if scrollmax<10 then scrollmax:=10;
    TrackBar.Max:=scrollmax;
  end;
end;

function TOIPropertyGrid.GetTreeIconX(Index:integer):integer;
begin
  Result:=Rows[Index].Lvl*Indent+2;
end;

function TOIPropertyGrid.GridHeight:integer;
begin
  if FRows.Count>0 then
    Result:=Rows[FRows.Count-1].Bottom
  else
    Result:=0;
end;

procedure TOIPropertyGrid.AlignEditComponents;
var RRect,EditCompRect,EditBtnRect:TRect;

  function CompareRectangles(r1,r2:TRect):boolean;
  begin
    Result:=(r1.Left=r2.Left) and (r1.Top=r2.Top) and (r1.Right=r2.Right)
       and (r1.Bottom=r2.Bottom);
  end;

// AlignEditComponents
begin
  if ItemIndex>=0 then begin
    RRect:=RowRect(ItemIndex);
    EditCompRect:=RRect;
    EditCompRect.Top:=EditCompRect.Top-1;
    EditCompRect.Left:=RRect.Left+SplitterX;
    if FCurrentButton<>nil then begin
      // edit dialog button
      with EditBtnRect do begin
        Top:=RRect.Top;
        Left:=RRect.Right-20;
        Bottom:=RRect.Bottom;
        Right:=RRect.Right;
        EditCompRect.Right:=Left;
      end;
      if not CompareRectangles(FCurrentButton.BoundsRect,EditBtnRect) then begin
        FCurrentButton.BoundsRect:=EditBtnRect;
        //FCurrentButton.Invalidate;
      end;
    end;
    if FCurrentEdit<>nil then begin
      // edit component
      EditCompRect.Left:=EditCompRect.Left-1;
      // XXX
      //
      EditCompRect.Bottom:=EditCompRect.Top+FCurrentEdit.Height;
      if not CompareRectangles(FCurrentEdit.BoundsRect,EditCompRect) then begin
        FCurrentEdit.BoundsRect:=EditCompRect;
        FCurrentEdit.Invalidate;
      end;
    end;
  end;
end;

procedure TOIPropertyGrid.PaintRow(ARow:integer);
var ARowRect,NameRect,NameIconRect,NameTextRect,ValueRect:TRect;
  IconX,IconY:integer;
  CurrRow:TOIPropertyGridRow;
  DrawState:TPropEditDrawState;
  OldFont:TFont;

  procedure DrawTreeIcon(x,y:integer;Plus:boolean);
  begin
    with Canvas do begin
      Brush.Color:=clWhite;
      Pen.Color:=clBlack;
      Rectangle(x,y,x+8,y+8);
      MoveTo(x+2,y+4);
      LineTo(x+7,y+4);
      if Plus then begin
        MoveTo(x+4,y+2);
        LineTo(x+4,y+7);
      end;
    end;
  end;

// PaintRow
begin
  CurrRow:=Rows[ARow];
  ARowRect:=RowRect(ARow);
  NameRect:=ARowRect;
  ValueRect:=ARowRect;
  NameRect.Right:=SplitterX;
  ValueRect.Left:=SplitterX;
  IconX:=GetTreeIconX(ARow);
  IconY:=((NameRect.Bottom-NameRect.Top-9) div 2)+NameRect.Top;
  NameIconRect:=NameRect;
  NameIconRect.Right:=IconX+Indent;
  NameTextRect:=NameRect;
  NameTextRect.Left:=NameIconRect.Right;
  DrawState:=[];
  if ARow=FItemIndex then Include(DrawState,pedsSelected);
  with Canvas do begin
    // draw name background
    if FBackgroundColor<>clNone then begin
      Brush.Color:=FBackgroundColor;
      FillRect(NameIconRect);
      FillRect(NameTextRect);
    end;
    // draw icon
    if paSubProperties in CurrRow.Editor.GetAttributes then begin
      DrawTreeIcon(IconX,IconY,not CurrRow.Expanded);
    end;
    // draw name
    OldFont:=Font;
    Font:=FNameFont;
    CurrRow.Editor.PropDrawName(Canvas,NameTextRect,DrawState);
    Font:=OldFont;
    // draw frame
    Pen.Color:=cl3DDkShadow;
    MoveTo(NameRect.Left,NameRect.Bottom-1);
    LineTo(NameRect.Right-1,NameRect.Bottom-1);
    LineTo(NameRect.Right-1,NameRect.Top-1);
    if ARow=FItemIndex then begin
      MoveTo(NameRect.Left,NameRect.Bottom-1);
      LineTo(NameRect.Left,NameRect.Top);
      LineTo(NameRect.Right-1,NameRect.Top);
    end;
    // draw value background
    if FBackgroundColor<>clNone then begin
      Brush.Color:=FBackgroundColor;
      FillRect(ValueRect);
    end;
    // draw value
    if ARow<>ItemIndex then begin
      OldFont:=Font;
      Font:=FValueFont;
      CurrRow.Editor.PropDrawValue(Canvas,ValueRect,DrawState);
      Font:=OldFont;
    end;
    // draw frame
    Pen.Color:=cl3DDkShadow;
    MoveTo(ValueRect.Left,ValueRect.Bottom-1);
    LineTo(ValueRect.Right,ValueRect.Bottom-1);
    Pen.Color:=cl3DLight;
    MoveTo(ValueRect.Left,ValueRect.Bottom-1);
    LineTo(ValueRect.Left,ValueRect.Top);
  end;
end;

procedure TOIPropertyGrid.Paint;
var a:integer;
  SpaceRect:TRect;
begin
  inherited Paint;
  with Canvas do begin
    // draw properties
    for a:=0 to FRows.Count-1 do begin
      PaintRow(a);
    end;
    // draw unused space below rows
    SpaceRect:=Rect(BorderWidth,BorderWidth,TrackBar.Left-1,Height-BorderWidth);
    Brush.Color:=FBackgroundColor;
    if FRows.Count>0 then
      SpaceRect.Top:=Rows[FRows.Count-1].Bottom-FTopY+BorderWidth;
    FillRect(SpaceRect);
    // draw border
    Pen.Color:=cl3DDkShadow;
    for a:=0 to BorderWidth-1 do begin
      MoveTo(a,Self.Height-1-a);
      LineTo(a,a);
      LineTo(Self.Width-1-a,a);
    end;
    Pen.Color:=cl3DLight;
    for a:=0 to BorderWidth-1 do begin
      MoveTo(Self.Width-1-a,a);
      LineTo(Self.Width-1-a,Self.Height-1-a);
      LineTo(a,Self.Height-1-a);
    end;
  end;
end;

function TOIPropertyGrid.RowRect(ARow:integer):TRect;
begin
  Result.Left:=BorderWidth;
  Result.Top:=Rows[ARow].Top-FTopY+BorderWidth;
  Result.Right:=TrackBar.Left-1;
  Result.Bottom:=Rows[ARow].Bottom-FTopY+BorderWidth;
end;

procedure TOIPropertyGrid.SetItemsTops;
// compute row tops from row heights
// set indices of all rows
var a,scrollmax:integer;
begin
  for a:=0 to FRows.Count-1 do
    Rows[a].Index:=a;
  if FRows.Count>0 then
    Rows[0].Top:=0;
  for a:=1 to FRows.Count-1 do
    Rows[a].FTop:=Rows[a-1].Bottom;
  if FRows.Count>0 then
    scrollmax:=Rows[FRows.Count-1].Bottom-Height
  else
    scrollmax:=10;
  if scrollmax<10 then scrollmax:=10;
  TrackBar.Max:=scrollmax;
end;

procedure TOIPropertyGrid.ClearRows;
var a:integer;
begin
  for a:=0 to FRows.Count-1 do begin
    Rows[a].Free;
  end;
  FRows.Clear;
end;

procedure TOIPropertyGrid.Clear;
begin
  ClearRows;
end;

function TOIPropertyGrid.GetRow(Index:integer):TOIPropertyGridRow;
begin
  Result:=TOIPropertyGridRow(FRows[Index]);
end;

//------------------------------------------------------------------------------

{ TOIPropertyGridRow }

constructor TOIPropertyGridRow.Create(PropertyTree:TOIPropertyGrid;
PropEditor:TPropertyEditor;  ParentNode:TOIPropertyGridRow);
begin
  inherited Create;
  // tree pointer
  FTree:=PropertyTree;
  FParent:=ParentNode;
  FNextBrother:=nil;
  FPriorBrother:=nil;
  FExpanded:=false;
  // child nodes
  FChildCount:=0;
  FFirstChild:=nil;
  FLastChild:=nil;
  // director
  FEditor:=PropEditor;
  GetLvl;
  FName:=FEditor.GetName;
  FTop:=0;
  FHeight:=FTree.DefaultItemHeight;
  Index:=-1;
end;

destructor TOIPropertyGridRow.Destroy;
begin
  if FPriorBrother<>nil then FPriorBrother.FNextBrother:=FNextBrother;
  if FNextBrother<>nil then FNextBrother.FPriorBrother:=FPriorBrother;
  if FParent<>nil then begin
    if FParent.FFirstChild=Self then FParent.FFirstChild:=FNextBrother;
    if FParent.FLastChild=Self then FParent.FLastChild:=FPriorBrother;
    dec(FParent.FChildCount);
  end;
  if FEditor<>nil then FEditor.Free;
  inherited Destroy;
end;

procedure TOIPropertyGridRow.GetLvl;
var n:TOIPropertyGridRow;
begin
  FLvl:=0;
  n:=FParent;
  while n<>nil do begin
    inc(FLvl);
    n:=n.FParent;
  end;
end;

function TOIPropertyGridRow.Bottom:integer;
begin
  Result:=FTop+FHeight;
end;

//==============================================================================

{ TObjectInspector }

constructor TObjectInspector.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Caption := 'Object Inspector';
  FRootComponent:=nil;
  FComponentList:=TComponentSelectionList.Create;

  // combobox at top (filled with available components)
  AvailCompsComboBox := TComboBox.Create (Self);
  with AvailCompsComboBox do begin
    Name:='AvailCompsComboBox';
    Parent:=self;
    Style:=csDropDown;
    OnChange:=@AvailComboBoxChange;
    //Sorted:=true;
    Show;
  end;

  // NoteBook
  NoteBook:=TNoteBook.Create(Self);
  with NoteBook do begin
    Name:='NoteBook';
    Parent:=Self;
    Pages.Strings[0]:='Properties';
    Pages.Add('Events');
    Show;
  end;

  // property grid
  PropertyGrid:=TOIPropertyGrid.Create(Self,
     [tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkSet{, tkMethod}
      , tkSString, tkLString, tkAString, tkWString, tkVariant
      {, tkArray, tkRecord, tkInterface}, tkClass, tkObject, tkWChar, tkBool
      , tkInt64, tkQWord]);
  with PropertyGrid do begin
    Name:='PropertyGrid';
    Parent:=NoteBook.Page[0];
    TrackBar.Parent:=Parent;
    ValueEdit.Parent:=Parent;
    ValueComboBox.Parent:=Parent;
    ValueButton.Parent:=Parent;
    Selections:=Self.FComponentList;
    Align:=alClient;
    Show;
  end;

  // event grid
  EventGrid:=TOIPropertyGrid.Create(Self,
     [tkMethod]);
  with EventGrid do begin
    Name:='EventGrid';
    Parent:=NoteBook.Page[1];
    TrackBar.Parent:=Parent;
    ValueEdit.Parent:=Parent;
    ValueComboBox.Parent:=Parent;
    ValueButton.Parent:=Parent;
    Selections:=Self.FComponentList;
    Align:=alClient;
    Show;
  end;
end;

procedure TObjectInspector.DoInnerResize;
var MaxX,MaxY,NewTop:integer;
begin
  if Visible=false then exit;
  MaxX:=ClientWidth;
  MaxY:=ClientHeight-20;

  // combobox at top (filled with available components)
  AvailCompsComboBox.SetBounds(0,0,MaxX-4,20);

  // notebook
  NewTop:=AvailCompsComboBox.Top+AvailCompsComboBox.Height+2;
  NoteBook.SetBounds(0,NewTop,MaxX-4,MaxY-NewTop);
end;

procedure TObjectinspector.Paint;
begin
  with Canvas do begin
  end;
end;

procedure TObjectinspector.SetRootComponent(Value:TComponent);
begin
  if FRootComponent<>Value then begin
    FRootComponent:=Value;
    FillComponentComboBox;
    // select root component
    FComponentList.Clear;
    if FRootComponent<>nil then FComponentList.Add(FRootComponent);
    RefreshSelections;
  end;
end;

function TObjectinspector.ComponentToString(c:TComponent):string;
begin
  Result:=c.GetNamePath+': '+c.ClassName;
end;

procedure TObjectinspector.FillComponentComboBox;
var a:integer;
begin
  AvailCompsComboBox.Items.Clear;
  if FRootComponent<>nil then begin
    AvailCompsComboBox.Items.AddObject(
      ComponentToString(FRootComponent),FRootComponent);
    AvailCompsComboBox.Text:=ComponentToString(FRootComponent);
    for a:=0 to FRootComponent.ComponentCount-1 do begin
      AvailCompsComboBox.Items.AddObject(
        ComponentToString(FRootComponent.Components[a]),
        FRootComponent.Components[a]);
    end;
  end;
end;

procedure TObjectinspector.SetSelections(
 const NewSelections:TComponentSelectionList);
var a:integer;
begin
  FComponentList.Clear;
  FComponentList.Capacity:=NewSelections.Count;
  for a:=0 to NewSelections.Count-1 do
    FComponentList.Add(NewSelections[a]);
  RefreshSelections;
end;

procedure TObjectinspector.RefreshSelections;
begin
  if FComponentList.Count=1 then begin
    AvailCompsComboBox.Text:=ComponentToString(FComponentList[0]);
  end else begin
    AvailCompsComboBox.Text:='';
  end;
  PropertyGrid.Selections:=FComponentList;
  EventGrid.Selections:=FComponentList;
end;

procedure TObjectinspector.SetBounds(aLeft,aTop,aWidth,aHeight:integer);
begin
  inherited SetBounds(aLeft,aTop,aWidth,aHeight);
  DoInnerResize;
end;

procedure TObjectinspector.AvailComboBoxChange(Sender:TObject);
var NewComponent:TComponent;
  a:integer;
begin
  if FRootComponent=nil then exit;
  if AvailCompsComboBox.Text=ComponentToString(FRootComponent) then begin
    FComponentList.Clear;
    FComponentList.Add(FRootComponent);
    RefreshSelections;
  end else begin
    for a:=0 to FRootComponent.ComponentCount-1 do begin
      NewComponent:=FRootComponent.Components[a];
      if AvailCompsComboBox.Text=ComponentToString(NewComponent) then begin
        FComponentList.Clear;
        FComponentList.Add(NewComponent);
        RefreshSelections;
        break;
      end;
    end;
  end;
end;

end.

