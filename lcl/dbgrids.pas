{ $Id$}
{
 /***************************************************************************
                               DBGrids.pas
                               -----------
                     An interface to DB aware Controls
                     Initial Revision : Sun Sep 14 2003


 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
{
TDBGrid and TComponentDataLink for Lazarus
Copyright (C) 2003  Jesus Reyes Aguilar.
email: jesusrmx@yahoo.com.mx

todo: credit who created the TComponentDatalink idea (Johana ...)

}
unit DBGrids;

{$mode objfpc}{$H+}
{.$define protodbgrid}
interface

uses
  Classes, LCLProc, Graphics, SysUtils, LCLType, stdctrls, DB, LMessages, Grids,
  Controls;

Type
  TDataSetScrolledEvent = Procedure(DataSet: TDataSet; Distance: Integer) of Object;

Type
  TComponentDataLink=Class(TDatalink)
  private
    FDataSet: TDataSet;
    FDataSetName: String;
    FModified: Boolean;
    FOnDatasetChanged: TDatasetNotifyEvent;
    fOnDataSetClose: TDataSetNotifyEvent;
    fOnDataSetOpen: TDataSetNotifyEvent;
    FOnDataSetScrolled: TDataSetScrolledEvent;
    fOnInvalidDataSet: TDataSetNotifyEvent;
    fOnInvalidDataSource: TDataSetNotifyEvent;
    fOnNewDataSet: TDataSetNotifyEvent;
    FOnRecordChanged: TFieldNotifyEvent;
    function GetDataSetName: String;
    function GetFields(Index: Integer): TField;
    procedure SetDataSetName(const AValue: String);
  Protected
    procedure RecordChanged(Field: TField); override;
    Procedure DataSetChanged; Override;
    procedure ActiveChanged; override;
    procedure LayoutChanged; override;
    procedure DataSetScrolled(Distance: Integer); override;
    procedure FocusControl(Field: TFieldRef); override;
    // Testing Events
    procedure CheckBrowseMode; Override;
    procedure EditingChanged; Override;
    procedure UpdateData; Override;
    function  MoveBy(Distance: Integer): Integer; Override;
  Public
    Procedure Modified;
    Property OnRecordChanged: TFieldNotifyEvent Read FOnRecordChanged Write FOnRecordChanged;
    Property OnDataSetChanged: TDatasetNotifyEvent Read FOnDatasetChanged Write FOnDataSetChanged;
    property OnNewDataSet: TDataSetNotifyEvent read fOnNewDataSet write fOnNewDataSet;
    property OnDataSetOpen: TDataSetNotifyEvent read fOnDataSetOpen write fOnDataSetOpen;
    property OnInvalidDataSet: TDataSetNotifyEvent read fOnInvalidDataSet write fOnInvalidDataSet;
    property OnInvalidDataSource: TDataSetNotifyEvent read fOnInvalidDataSource write fOnInvalidDataSource;
    property OnDataSetClose: TDataSetNotifyEvent read fOnDataSetClose write fOnDataSetClose;
    Property OnDataSetScrolled: TDataSetScrolledEvent Read FOnDataSetScrolled Write FOnDataSetScrolled;
    Property DataSetName:String Read GetDataSetName Write SetDataSetName;
    Property Fields[Index: Integer]: TField read GetFields;
  End;


  TCustomDbGrid=Class(TCustomGrid)
  Private
    FDataLink: TComponentDataLink;
    FKeepInBuffer: Boolean;
    FOnColEnter: TNotifyEvent;
    FOnColExit: TNotifyEvent;
    FReadOnly: Boolean;
    FColEnterPending: Boolean;
    FSelfScroll: Boolean;
    FLayoutChanging: Boolean;
    FVisualLock: Boolean;
    FNumRecords: Integer;
    function GetDataSource: TDataSource;
    Procedure OnRecordChanged(Field:TField);
    Procedure OnDataSetChanged(aDataSet: TDataSet);
    Procedure OnDataSetOpen(aDataSet: TDataSet);
    Procedure OnDataSetClose(aDataSet: TDataSet);
    Procedure OnInvalidDataSet(aDataSet: TDataSet);
    Procedure OnInvalidDataSource(aDataSet: TDataset);
    Procedure OnNewDataSet(aDataSet: TDataset);
    Procedure OnDataSetScrolled(aDataSet:TDataSet; Distance: Integer);
    procedure SetDataSource(const AValue: TDataSource);
    Procedure UpdateBufferCount;
    // Temporal
    Function DefaultFieldColWidth(FieldType: TFieldType): Integer;

  Protected
    procedure LinkActive(Value: Boolean); virtual;
    Procedure LayoutChanged; Virtual;
    Property ReadOnly: Boolean Read FReadOnly Write FReadOnly;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    Procedure DrawByRows; Override;
    Procedure DrawRow(ARow: Integer); Override;
    Procedure DrawCell(aCol,aRow: Integer; aRect: TRect; aState:TGridDrawState); Override;
    
    {$Ifdef protodbgrid}
    Function BeyondRowCount(Count: Integer):Boolean; Override;
    Function BelowFirstRow(Count: Integer):Boolean; Override;
    procedure UpdateGridScrollPosition(DCol,DRow: Integer; InvAll: Boolean); Override;
    {$endif protodbgrid}
    Procedure MoveSelection; Override;
    Procedure BeforeMoveSelection(Const DCol,DRow: Integer); Override;
    procedure HeaderClick(IsColumn: Boolean; index: Integer); Override;
    procedure KeyDown(var Key : Word; Shift : TShiftState); Override;
    
    Procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    function  ScrollBarAutomatic(Which: TScrollStyle): boolean; override;
    {
    Procedure MouseMove(Shift: TShiftState; X,Y: Integer);Override;
    Procedure MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    }
    
    Procedure VisualChange; Override;

    Procedure WMHScroll(var Message : TLMHScroll); message LM_HScroll;
    Procedure WMVScroll(var Message : TLMVScroll); message LM_VScroll;

    procedure UpdateActive;
    property OnColEnter: TNotifyEvent read FOnColEnter write FOnColEnter;
    property OnColExit: TNotifyEvent read FOnColExit write FOnColExit;
    Property KeepInBuffer: Boolean read FKeepInBuffer write FKeepInBuffer;
  Public
    Constructor Create(AOwner: TComponent); Override;
    Destructor Destroy; Override;
  End;
  
  TdbGrid=Class(TCustomDbGrid)
  public
    property Canvas;
    //property SelectedRows;
  published
  
    property Align;
    property Anchors;
    //property BiDiMode;
    //property BorderStyle;
    property Color;
    //property Columns stored False; //StoreColumns;
    //property Constraints;
    //property Ctl3D;
    property DataSource;
    property DefaultDrawing;
    //property DragCursor;
    //property DragKind;
    //property DragMode;
    property Enabled;
    property FixedColor;
    property Font;
    //property ImeMode;
    //property ImeName;
    //property Options;
    //property ParentBiDiMode;
    property ParentColor;
    //property ParentCtl3D;
    property ParentFont;
    //property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    //property TitleFont;
    property Visible;
    //property OnCellClick;
    property OnColEnter;
    property OnColExit;
    //property OnColumnMoved;
    //property OnDrawDataCell;  { obsolete }
    //property OnDrawColumnCell;
    property OnDblClick;
    //property OnDragDrop;
    //property OnDragOver;
    //property OnEditButtonClick;
    //property OnEndDock;
    //property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    //property OnStartDock;
    //property OnStartDrag;
    //property OnTitleClick;
  End;

Procedure Register;
  
implementation

procedure Register;
begin
  RegisterComponents('Data Controls',[TDBGrid]);
end;


{ TCustomdbGrid }

procedure TCustomDbGrid.OnRecordChanged(Field: TField);
begin
  {$IfDef dbgdbgrid}
  DBGOut('(',name,') ','TCustomDBGrid.OnRecordChanged(Field=');
  If Field=nil Then DebugLn('nil)')
  Else              DebugLn(Field.FieldName,')');
  {$Endif}
end;

function TCustomDbGrid.GetDataSource: TDataSource;
begin
  Result:= FDataLink.DataSource;
end;

procedure TCustomDbGrid.OnDataSetChanged(aDataSet: TDataSet);
begin
  {$Ifdef dbgdbgrid}
  DBGOut('(',name,') ','TCustomDBDrid.OnDataSetChanged(aDataSet=');
  If aDataSet=nil Then DebugLn('nil)')
  Else DebugLn(aDataSet.Name,')');
  {$endif}
  UpdateActive;
end;

procedure TCustomDbGrid.OnDataSetOpen(aDataSet: TDataSet);
begin
  {$Ifdef dbgdbgrid}
  DebugLn('(',name,') ','TCustomDBGrid.OnDataSetOpen');
  {$endif}
  LinkActive(True);
  UpdateActive;
end;

procedure TCustomDbGrid.OnDataSetClose(aDataSet: TDataSet);
begin
  {$ifdef dbgdbgrid}
  DebugLn('(',name,') ','TCustomDBGrid.OnDataSetClose');
  {$endif}
  LinkActive(False);
end;

procedure TCustomDbGrid.OnInvalidDataSet(aDataSet: TDataSet);
begin
  {$ifdef dbgdbgrid}
  DebugLn('(',name,') ','TCustomDBGrid.OnInvalidDataSet');
  {$endif}
  LinkActive(False);
end;

procedure TCustomDbGrid.OnInvalidDataSource(aDataSet: TDataset);
begin
  {$ifdef dbgdbgrid}
  DebugLn('(',name,') ','TCustomDBGrid.OnInvalidDataSource');
  {$endif}
  LinkActive(False);
end;

procedure TCustomDbGrid.OnNewDataSet(aDataSet: TDataset);
begin
  {$ifdef dbgdbgrid}
  DebugLn('(',name,') ','TCustomDBGrid.OnNewDataSet');
  {$endif}
  LinkActive(True);
  UpdateActive;
end;

procedure TCustomDbGrid.OnDataSetScrolled(aDataset: TDataSet; Distance: Integer);
begin
  {$ifdef dbgdbgrid}
  DebugLn(ClassName, ' (',name,')', '.OnDataSetScrolled(',Distance,'), Invalidating');
  {$endif}
  UpdateActive;
  If Distance<>0 Then Invalidate;
end;

procedure TCustomDbGrid.SetDataSource(const AValue: TDataSource);
begin
  if AValue = FDatalink.Datasource then Exit;
  FDataLink.DataSource := AValue;
  UpdateActive;
end;

procedure TCustomDbGrid.UpdateBufferCount;
begin
  If FDataLink.Active Then begin
    //if FGCache.ValidGrid Then
      FDataLink.BufferCount:= ClientHeight div DefaultRowHeight - 1;
    //Else
    //  FDataLink.BufferCount:=0;
    {$ifdef dbgdbgrid}
    DebugLn(ClassName, ' (',name,')', ' FdataLink.BufferCount=',Fdatalink.BufferCount);
    {$endif}
  End;
end;

procedure TCustomDbGrid.WMHScroll(var Message: TLMHScroll);
begin
  inherited;
end;

procedure TCustomDbGrid.WMVScroll(var Message: TLMVScroll);
Var
  Num: Integer;
  C, TL: Integer;
begin
  Inherited;
  if Not GCache.ValidGrid Then Exit;
  DebugLn('VSCROLL: Code=',dbgs(Message.ScrollCode),' Position=', dbgs(Message.Pos));
  
  exit;
  C:=Message.Pos+GCache.Fixedheight;
  Num:=(FNumRecords + FixedRows) * DefaultRowHeight;
  TL:= Num div C;
  GCache.TLRowOff:= C - TL*DefaultRowHeight;
  DebugLn('---- Offset=',dbgs(C), ' ScrollTo=> TL=',dbgs(TL), ' TLRowOFf=', dbgs(GCache.TLRowOff));
end;


Function TCustomDbGrid.DefaultFieldColWidth(FieldType: TFieldType): Integer;
begin
  Case FieldType of
    ftString: Result:=150;
    ftSmallInt..ftBoolean: Result:=60;
    Else Result:=DefaultColWidth;
  End;
end;

procedure TCustomDbGrid.LinkActive(Value: Boolean);
begin
  //BeginUpdate;
  FVisualLock:= Value; // If Not Active Call Inherited visualchange y Active dont call it
  If Not Value Then FDataLink.BufferCount:=0;
  Clear; // This will call VisualChange and Finally -> LayoutChanged
  //If Value Then LayoutChanged;
  //EndUpdate(uoFull);
end;

procedure TCustomDbGrid.LayoutChanged;
var
  i: Integer;
  FDefs: TFieldDefs;
begin
  If FDataLink.Active Then begin

    FNumRecords:= FDataLink.DataSet.RecordCount;
    {$ifdef dbgdbgrid}
    DebugLn('(',name,') ','TCustomGrid.LayoutChanged INIT');
    DebugLn('DataLink.DataSet.recordcount: ',FNumRecords);
    {$endif}

    FLayoutChanging:=True; // Avoid infinit loop
    FVisualLock:=True; // Avoid Calling Inherited visualchange
    UpdateBufferCount;
    ColCount:= FDataLink.DataSet.FieldCount + 1;
    RowCount:= FDataLink.RecordCount + 1;
    FixedRows:=1;
    FixedCols:=1;
    ColWidths[0]:=12;
    FDefs:=FDataLink.DataSet.FieldDefs;
    For i:=0 to FDefs.Count-1 do Begin
      //DebugLn('Field ',FDefs[i].Name, ' Size= ',FDefs[i].Size);
      ColWidths[i+1]:= DefaultFieldColWidth(FDefs[i].DataType);
    End;
    FVisualLock:=False;
    VisualChange; // Now Call Visual Change
    // Update Scrollbars

    ScrollBarRange(SB_HORZ, GridWidth + 2);
    ScrollBarRange(SB_VERT, (FNumRecords + FixedRows) * DefaultRowHeight + 2);
    
    //HorzScrollBar.Range:= GridWidth+2;
    //VertScrollBar.Range:= (FNumRecords + FixedRows) * DefaultRowHeight + 2;
    {
    For i:=1 to ColCount-1 do begin
      F:=FDataLink.Fields[i];
      If F<>nil Then Begin
        W:=F.DisplayWidth;
        If W<0 Then W:=0;
        If W=0 Then W:=F.GetDefaultwidth;
        DebugLn('Field ',F.FieldName,' DisplayWidth=', W);
      End;
    End;
    }
    {$ifdef dbgdbgrid}
    DebugLn('(',name,') ','TCustomGrid.LayoutChanged - DONE');
    {$endif}
    FLayoutChanging:=False;
  End;
end;
{$IfDef Protodbgrid}
Function TCustomDbGrid.BeyondRowCount(Count: Integer): Boolean;
Var
  i: integer;
  InMaxRow: Boolean;
begin
  With FDataLink do begin
    Result:=Active;
    {$ifdef dbgdbgrid}
    DebugLn('(',name,') ',
      'BeyondRowCount Hitted here: Count=',Count,
      ' FDataLink.Active=', Result,
      ' FDataLink.EOF=',EOF);
    {$Endif}
    If Not result Then Exit;

    If EOF And DataSet.CanModify And Not ReadOnly Then
      Dataset.Append
    Else
      If not EOF Then begin
        I:=MoveBy(Count);
        {$Ifdef dbgdbgrid}
        DebugLn('Scrolled by ',I);
        {$Endif}
      End;
  End;
end;

Function TCustomDbGrid.BelowFirstRow(Count: Integer):Boolean;
var
  i: Integer;
begin
  With FDataLink do Begin
    Result:=Active;
    {$ifdef dbgdbgrid}
    DebugLn('(',name,') ',
      'BelowFirstRow Hitted here: Count=',Count,
      ' FDataLink.Active=', Result,
      ' FDataLink.BOF=',BOF);
    {$Endif}
    If Result And Not BOF Then begin
      If KeepInBuffer And (ActiveRecord<>0) Then
        Result:=Inherited BelowFirstRow(Count)
      Else begin
        I:=MoveBy(-Count);
        {$Ifdef dbgdbgrid}
        DebugLn('Scrolled By ', I);
        {$Endif}
      End;
    End;
  End;
end;

procedure TCustomDbGrid.UpdateGridScrollPosition(DCol, DRow: Integer; InvAll: Boolean);
begin
  If DCol<>Col Then inherited;
end;
{$Endif Protodbgrid}

Procedure TCustomDbGrid.BeforeMoveSelection(Const DCol,DRow: Integer);
begin
  Inherited BeforeMoveSelection(DCol, DRow);
  
  FDatalink.UpdateData;
  If DCol<>Col Then begin
     // Its a Column Movement
     If assigned(OnColExit) Then OnColExit(Self);
     FColEnterPending:=True;
  End;
  {
  Exit;
  If (DRow<>Row) Then Begin
    // Its a Row Movement
    D:= DRow - Row;
    FDatalink.MoveBy(D);
  End;
  }
end;

procedure TCustomDbGrid.HeaderClick(IsColumn: Boolean; index: Integer);
begin
  inherited HeaderClick(IsColumn, index);
end;

procedure TCustomDbGrid.KeyDown(var Key: Word; Shift: TShiftState);
  Procedure MoveBy(Delta: Integer);
  Begin
    FSelfScroll:=True;
    FDatalink.MoveBy(Delta);
    FSelfScroll:=False;
  end;
begin
  // inherited KeyDown(Key, Shift); // Fully override old KeyDown handler
  Case Key of
    VK_DOWN: MoveBy(1);
    VK_UP: MoveBy(-1);
    VK_NEXT: MoveBy( VisibleRowCount );
    VK_PRIOR: MoveBy( -VisibleRowCount );
    else Inherited;
  End;
end;

procedure TCustomDbGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
Var
  Gz: TGridZone;
  P: TPoint;
begin
  If csDesigning in componentState Then Exit;
  If Not GCache.ValidGrid Then Exit;
  
  Gz:=MouseToGridZone(X,Y, False);
  Case Gz of
    gzFixedRows, gzFixedCols: inherited MouseDown(Button, Shift, X, Y);
    else
      Begin
        P:=MouseToCell(Point(X,Y));
        If P.Y=Row Then Inherited MouseDown(Button, Shift, X, Y)
        Else Begin
          BeginUpdate;
          FDatalink.MoveBy(P.Y - Row);
          Col:=P.X;
          EndUpdate(uoQuick);
        End;
      End;
  End;
end;

function TCustomDbGrid.ScrollBarAutomatic(Which: TScrollStyle): boolean;
begin
  if Which=ssHorizontal then
    Result:=True
  else
    Result:=inherited ScrollBarAutomatic(Which);
end;

procedure TCustomDbGrid.MoveSelection;
begin
  inherited MoveSelection;
  If FColEnterPending And Assigned(OnColEnter) Then OnColEnter(Self);
  FColEnterPending:=False;
  UpdateActive;
end;

procedure TCustomDbGrid.DrawByRows;
Var
  CurActiveRecord: Integer;
begin
  If FDataLink.ACtive Then Begin
    CurActiveRecord:=FDataLink.ActiveRecord;
    //PrimerRecord:=FDataLink.FirstRecord;
  End;
  Try
    inherited DrawByRows;
  Finally
    if FDataLink.Active Then FDataLink.ActiveRecord:=CurActiveRecord;
  End;
end;
// 33 31 21 29 80 90 4 3
procedure TCustomDbGrid.DrawRow(ARow: Integer);
begin
  If Arow>=FixedRows then FDataLink.ActiveRecord:=ARow-FixedRows;
  inherited DrawRow(ARow);
end;

procedure DrawArrow(Canvas: TCanvas; R: TRect; Opt: TDataSetState);
var
  dx,dy, x, y: Integer;
begin
  Case Opt of
    dsBrowse:
      begin //
          Canvas.Brush.Color:=clBlack;
          Canvas.Pen.Color:=clBlack;
          Dx:=6;
          Dy:=6;
          y:= R.top+ (R.Bottom-R.Top) div 2;
          x:= R.Left+2;
          Canvas.Polygon([point(x,y-dy),point(x+dx,y),point(x, y+dy),point(x,y-dy)]);
       End;
    dsEdit:
      begin // Normal
          Canvas.Brush.Color:=clRed;
          Canvas.Pen.Color:=clRed;
          Dx:=6;
          Dy:=6;
          y:= R.top+ (R.Bottom-R.Top) div 2;
          x:= R.Left+2;
          Canvas.Polygon([point(x,y-dy),point(x+dx,y),point(x, y+dy),point(x,y-dy)]);
       End;
    dsInsert:
      begin // Normal
          Canvas.Brush.Color:=clGreen;
          Canvas.Pen.Color:=clGreen;
          Dx:=6;
          Dy:=6;
          y:= R.top+ (R.Bottom-R.Top) div 2;
          x:= R.Left+2;
          Canvas.Polygon([point(x,y-dy),point(x+dx,y),point(x, y+dy),point(x,y-dy)]);
       End;
   End;
End;

procedure TCustomDbGrid.DrawCell(aCol, aRow: Integer; aRect: TRect;
  aState: TGridDrawState);
Var
  F: TField;
begin
  // Draw appropiated attributes
  inherited DrawCell(aCol, aRow, aRect, aState);
  
  If Not FDataLink.Active then Exit;
  
  // Draw text When needed
  If gdFixed in aState Then begin
    if (aRow=0)And(ACol>=FixedCols) Then begin
      // draw column headers
      F:=FDataLink.Fields[aCol-FixedCols];
      If F<>nil then Canvas.TextOut(Arect.Left+2,ARect.Top+2, F.FieldName);
    End Else
    If (aCol=0)And(aRow=Row) Then
      // draw row headers (selected/editing/* record)
      DrawArrow(Canvas, aRect, FDataLink.Dataset.State)
  End Else begin
    // Draw the other cells
    F:=FDataLink.Fields[Acol-FixedCols];
    If F<>nil then Canvas.TextOut(aRect.Left+2,ARect.Top+2, F.AsString);
  End;
end;

procedure TCustomDbGrid.UpdateActive;
{
var
  LastRow: Integer;
  lastEditor: TWinControl;
  WasVisible: Boolean;
}
begin
  With FDataLink do begin
    If Not GCache.ValidGrid then Exit;
    If DataSource=nil Then Exit;
    DebugLn('(',Name,') ActiveRecord=', dbgs(ActiveRecord), ' FixedRows=',dbgs(FixedRows), ' Row=', dbgs(Row));
    Row:= FixedRows + ActiveRecord;
    {
    LastRow:=Row;
    LastEditor:= Editor;
    WasVisible:= (Lasteditor<>nil)And(LastEditor.Visible);
    FRow:=FixedRows + ActiveRecord;
    If LastRow<>FRow Then
      ProcessEditor(LastEditor,Col,LastRow,WasVisible);
    }
  End;
  Invalidate;
end;

procedure TCustomDbGrid.VisualChange;
begin
  If FDataLink=nil Then Exit;
  If not FVisualLock Then begin
    inherited VisualChange;
  End;
  If Not FLayoutChanging Then begin
    LayoutChanged;
  End;
end;

constructor TCustomDbGrid.Create(AOwner: TComponent);
begin
  DragDx:=5;
  inherited Create(AOwner);
  
  FDataLink := TComponentDataLink.Create;//(Self);
  FDataLink.OnRecordChanged:=@OnRecordChanged;
  FDataLink.OnDatasetChanged:=@OnDataSetChanged;
  FDataLink.OnDataSetOpen:=@OnDataSetOpen;
  FDataLink.OnDataSetClose:=@OnDataSetClose;
  FDataLink.OnNewDataSet:=@OnNewDataSet;
  FDataLink.OnInvalidDataSet:=@OnInvalidDataset;
  FDataLink.OnInvalidDataSource:=@OnInvalidDataSource;
  FDataLink.OnDataSetScrolled:=@OnDataSetScrolled;
  FKeepInBuffer:=False;

  FReadOnly:=True;
  Options:=Options + [goColSizing, goDrawFocusSelected];
  // What a dilema!, we need ssAutoHorizontal and ssVertical!!!
  ScrolLBars:=ssBoth;
  FVisualLock:=False;
  Clear;
end;

destructor TCustomDbGrid.Destroy;
begin
  FDataLink.OnDataSetChanged:=nil;
  FDataLink.OnRecordChanged:=nil;
  FDataLink.Free;
  Inherited Destroy;
end;

{ TComponentDataLink }

function TComponentDataLink.GetFields(Index: Integer): TField;
begin
  If (index>=0)And(index<DataSet.FieldCount) Then result:=DataSet.Fields[index];
end;

function TComponentDataLink.GetDataSetName: String;
begin
  Result:=FDataSetName;
  If DataSet<>nil Then Result:=DataSet.Name;
end;

procedure TComponentDataLink.SetDataSetName(const AValue: String);
begin
  If FDataSetName<>AValue then FDataSetName:=AValue;
end;

procedure TComponentDataLink.RecordChanged(Field: TField);
begin
  {$ifdef dbgdbgrid}
  DebugLn('TComponentDataLink.RecordChanged');
  {$endif}
  If Assigned(OnRecordChanged) Then OnRecordChanged(Field);
end;

procedure TComponentDataLink.DataSetChanged;
begin
  {$ifdef dbgdbgrid}
  DebugLn('TComponentDataLink.DataSetChanged');
  {$Endif}
  If Assigned(OnDataSetChanged) Then OnDataSetChanged(DataSet);
end;

procedure TComponentDataLink.ActiveChanged;
begin
  {$ifdef dbgdbgrid}
  DebugLn('TComponentDataLink.ActiveChanged');
  {$endif}
  if Active then begin
    fDataSet := DataSet;
    if DataSetName <> fDataSetName then begin
      fDataSetName := DataSetName;
      if Assigned(fOnNewDataSet) then fOnNewDataSet(DataSet);
    end else
      if Assigned(fOnDataSetOpen) then fOnDataSetOpen(DataSet);
  end else begin
    if (DataSource = nil)then begin
      if Assigned(fOnInvalidDataSource) then fOnInvalidDataSource(fDataSet);
      fDataSet := nil;
      fDataSetName := '[???]';
    end else begin
      if (DataSet=nil)or(csDestroying in DataSet.ComponentState) then begin
        if Assigned(fOnInvalidDataSet) then fOnInvalidDataSet(fDataSet);
        fDataSet := nil;
        fDataSetName := '[???]';
      end else begin
        if Assigned(fOnDataSetClose) then fOnDataSetClose(DataSet);
        if DataSet <> nil then FDataSetName := DataSetName;
      end;
    end;
  end;
end;

procedure TComponentDataLink.LayoutChanged;
begin
  Inherited LayoutChanged;
  {$ifdef dbgdbgrid}
  DebugLn('TComponentDataLink.LayoutChanged');
  {$endif}
end;

procedure TComponentDataLink.DataSetScrolled(Distance: Integer);
begin
  {$ifdef dbgdbgrid}
  DebugLn('TComponentDataLink.DataSetScrolled(',Distance,')');
  {$endif}
  if Assigned(OnDataSetScrolled) Then OnDataSetScrolled(DataSet, Distance);
end;

procedure TComponentDataLink.FocusControl(Field: TFieldRef);
begin
  {$ifdef dbgdbgrid}
  DebugLn('TComponentDataLink.FocusControl');
  {$endif}
end;

procedure TComponentDataLink.CheckBrowseMode;
begin
  (*
  {$ifdef dbgdbgrid}
  DebugLn(ClassName,'.CheckBrowseMode');
  {$endif}
  *)
  inherited CheckBrowseMode;
end;

procedure TComponentDataLink.EditingChanged;
begin
  {$ifdef dbgdbgrid}
  DebugLn(ClassName,'.EditingChanged');
  {$endif}
  inherited EditingChanged;
end;

procedure TComponentDataLink.UpdateData;
begin
  (*
  {$ifdef dbgdbgrid}
  DebugLn(ClassName,'.UpdateData');
  {$endif}
  *)
  inherited UpdateData;
end;

function TComponentDataLink.MoveBy(Distance: Integer): Integer;
begin
  (*
  {$ifdef dbgdbgrid}
  DebugLn(ClassName,'.MoveBy  INIT: Distance=',Distance);
  {$endif}
  *)
  Result:=inherited MoveBy(Distance);
  (*
  {$ifdef dbgdbgrid}
  DebugLn(ClassName,'.MoveBy  END: Distance=',Distance);
  {$endif}
  *)
end;

procedure TComponentDataLink.Modified;
begin
  {$ifdef dbgdbgrid}
  DebugLn(ClassName,'.Modified');
  {$Endif}
  FModified:=True;
end;
end.

{
  the_log:
  
}
