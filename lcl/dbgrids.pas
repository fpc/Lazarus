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
interface

uses
  Classes, LCLProc, Graphics, SysUtils, LCLType, stdctrls, DB, LMessages, Grids,
  Controls;

type
  TDataSetScrolledEvent = procedure(DataSet: TDataSet; Distance: Integer) of object;

type
  TComponentDataLink=class(TDatalink)
  private
    FDataSet: TDataSet;
    FDataSetName: string;
    FModified: Boolean;
    FOnDatasetChanged: TDatasetNotifyEvent;
    fOnDataSetClose: TDataSetNotifyEvent;
    fOnDataSetOpen: TDataSetNotifyEvent;
    FOnDataSetScrolled: TDataSetScrolledEvent;
    fOnInvalidDataSet: TDataSetNotifyEvent;
    fOnInvalidDataSource: TDataSetNotifyEvent;
    fOnNewDataSet: TDataSetNotifyEvent;
    FOnRecordChanged: TFieldNotifyEvent;
    function GetDataSetName: string;
    function GetFields(Index: Integer): TField;
    procedure SetDataSetName(const AValue: string);
  protected
    procedure RecordChanged(Field: TField); override;
    procedure DataSetChanged; override;
    procedure ActiveChanged; override;
    procedure LayoutChanged; override;
    procedure DataSetScrolled(Distance: Integer); override;
    procedure FocusControl(Field: TFieldRef); override;
    // Testing Events
    procedure CheckBrowseMode; override;
    procedure EditingChanged; override;
    procedure UpdateData; override;
    function  MoveBy(Distance: Integer): Integer; override;
  public
    procedure Modified;
    Property OnRecordChanged: TFieldNotifyEvent read FOnRecordChanged write FOnRecordChanged;
    Property OnDataSetChanged: TDatasetNotifyEvent read FOnDatasetChanged write FOnDataSetChanged;
    property OnNewDataSet: TDataSetNotifyEvent read fOnNewDataSet write fOnNewDataSet;
    property OnDataSetOpen: TDataSetNotifyEvent read fOnDataSetOpen write fOnDataSetOpen;
    property OnInvalidDataSet: TDataSetNotifyEvent read fOnInvalidDataSet write fOnInvalidDataSet;
    property OnInvalidDataSource: TDataSetNotifyEvent read fOnInvalidDataSource write fOnInvalidDataSource;
    property OnDataSetClose: TDataSetNotifyEvent read fOnDataSetClose write fOnDataSetClose;
    Property OnDataSetScrolled: TDataSetScrolledEvent read FOnDataSetScrolled write FOnDataSetScrolled;
    Property DataSetName:string read GetDataSetName write SetDataSetName;
    Property Fields[Index: Integer]: TField read GetFields;
  end;


  TCustomDbGrid=class(TCustomGrid)
  private
    FDataLink: TComponentDataLink;
    FKeepInBuffer: Boolean;
    FOnColEnter: TNotifyEvent;
    FOnColExit: TNotifyEvent;
    FReadOnly: Boolean;
    FColEnterPending: Boolean;
    //FSelfScroll: Boolean;
    FLayoutChanging: Boolean;
    FVisualLock: Boolean;
    FNumRecords: Integer;
    function GetDataSource: TDataSource;
    procedure OnRecordChanged(Field:TField);
    procedure OnDataSetChanged(aDataSet: TDataSet);
    procedure OnDataSetOpen(aDataSet: TDataSet);
    procedure OnDataSetClose(aDataSet: TDataSet);
    procedure OnInvalidDataSet(aDataSet: TDataSet);
    procedure OnInvalidDataSource(aDataSet: TDataset);
    procedure OnNewDataSet(aDataSet: TDataset);
    procedure OnDataSetScrolled(aDataSet:TDataSet; Distance: Integer);
    procedure SetDataSource(const AValue: TDataSource);
    procedure UpdateBufferCount;
    // Temporal
    function DefaultFieldColWidth(FieldType: TFieldType): Integer;

  protected
    procedure LinkActive(Value: Boolean); virtual;
    procedure LayoutChanged; virtual;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DrawByRows; override;
    procedure DrawRow(ARow: Integer); override;
    procedure DrawCell(aCol,aRow: Integer; aRect: TRect; aState:TGridDrawState); override;
    
    procedure MoveSelection; override;
    procedure BeforeMoveSelection(const DCol,DRow: Integer); override;
    procedure HeaderClick(IsColumn: Boolean; index: Integer); override;
    procedure KeyDown(var Key : Word; Shift : TShiftState); override;
    
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    function  ScrollBarAutomatic(Which: TScrollStyle): boolean; override;
    {
    procedure MouseMove(Shift: TShiftState; X,Y: Integer);override;
    procedure MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    }
    
    procedure VisualChange; override;

    procedure WMVScroll(var Message : TLMVScroll); message LM_VScroll;

    procedure UpdateActive;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    Property KeepInBuffer: Boolean read FKeepInBuffer write FKeepInBuffer;
    Property ReadOnly: Boolean read FReadOnly write FReadOnly;
    property OnColEnter: TNotifyEvent read FOnColEnter write FOnColEnter;
    property OnColExit: TNotifyEvent read FOnColExit write FOnColExit;
  public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
  end;
  
  TdbGrid=class(TCustomDbGrid)
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
  end;

procedure Register;
  
implementation

procedure Register;
begin
  RegisterComponents('Data Controls',[TDBGrid]);
end;


{ TCustomdbGrid }

procedure TCustomDbGrid.OnRecordChanged(Field: TField);
begin
  {$IfDef dbgdbgrid}
  DBGOut('('+name+') ','TCustomDBGrid.OnRecordChanged(Field=');
  if Field=nil then DebugLn('nil)')
  else              DebugLn(Field.FieldName,')');
  {$Endif}
end;

function TCustomDbGrid.GetDataSource: TDataSource;
begin
  Result:= FDataLink.DataSource;
end;

procedure TCustomDbGrid.OnDataSetChanged(aDataSet: TDataSet);
begin
  {$Ifdef dbgdbgrid}
  DBGOut('('+name+') ','TCustomDBDrid.OnDataSetChanged(aDataSet=');
  if aDataSet=nil then DebugLn('nil)')
  else DebugLn(aDataSet.Name,')');
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
  DebugLn(ClassName, ' (',name,')', '.OnDataSetScrolled(',IntToStr(Distance),'), Invalidating');
  {$endif}
  UpdateActive;
  if Distance<>0 then Invalidate;
end;

procedure TCustomDbGrid.SetDataSource(const AValue: TDataSource);
begin
  if AValue = FDatalink.Datasource then Exit;
  FDataLink.DataSource := AValue;
  UpdateActive;
end;

procedure TCustomDbGrid.UpdateBufferCount;
begin
  if FDataLink.Active then begin
    //if FGCache.ValidGrid then
      FDataLink.BufferCount:= ClientHeight div DefaultRowHeight - 1;
    //else
    //  FDataLink.BufferCount:=0;
    {$ifdef dbgdbgrid}
    DebugLn(ClassName, ' (',name,')', ' FdataLink.BufferCount=' + IntToStr(Fdatalink.BufferCount));
    {$endif}
  end;
end;

procedure TCustomDbGrid.WMVScroll(var Message: TLMVScroll);
var
  Num: Integer;
  C, TL: Integer;
begin
  inherited;
  if not GCache.ValidGrid then Exit;
  {$ifdef dbgdbgrid}
  DebugLn('VSCROLL: Code=',dbgs(Message.ScrollCode),' Position=', dbgs(Message.Pos));
  {$endif}
  exit;
  C:=Message.Pos+GCache.Fixedheight;
  Num:=(FNumRecords + FixedRows) * DefaultRowHeight;
  TL:= Num div C;
  GCache.TLRowOff:= C - TL*DefaultRowHeight;
  DebugLn('---- Offset=',dbgs(C), ' ScrollTo=> TL=',dbgs(TL), ' TLRowOFf=', dbgs(GCache.TLRowOff));
end;


function TCustomDbGrid.DefaultFieldColWidth(FieldType: TFieldType): Integer;
begin
  case FieldType of
    ftString: Result:=150;
    ftSmallInt..ftBoolean: Result:=60;
    else Result:=DefaultColWidth;
  end;
end;

procedure TCustomDbGrid.LinkActive(Value: Boolean);
begin
  //BeginUpdate;
  FVisualLock:= Value; // if not Active Call inherited visualchange y Active dont call it
  if not Value then FDataLink.BufferCount:=0;
  Clear; // This will call VisualChange and Finally -> LayoutChanged
  //if Value then LayoutChanged;
  //EndUpdate(uoFull);
end;

procedure TCustomDbGrid.LayoutChanged;
var
  i: Integer;
  FDefs: TFieldDefs;
begin
  if FDataLink.Active then begin

    FNumRecords:= FDataLink.DataSet.RecordCount;
    {$ifdef dbgdbgrid}
    DebugLn('(',name,') ','TCustomGrid.LayoutChanged INIT');
    DebugLn('DataLink.DataSet.recordcount: ', IntToStr(FNumRecords));
    {$endif}

    FLayoutChanging:=True; // Avoid infinit loop
    FVisualLock:=True; // Avoid Calling inherited visualchange
    UpdateBufferCount;
    ColCount:= FDataLink.DataSet.FieldCount + 1;
    RowCount:= FDataLink.RecordCount + 1;
    FixedRows:=1;
    FixedCols:=1;
    ColWidths[0]:=12;
    FDefs:=FDataLink.DataSet.FieldDefs;
    for i:=0 to FDefs.Count-1 do begin
      //DebugLn('Field ',FDefs[i].Name, ' Size= ',FDefs[i].Size);
      ColWidths[i+1]:= DefaultFieldColWidth(FDefs[i].DataType);
    end;
    FVisualLock:=False;
    VisualChange; // Now Call Visual Change
    // Update Scrollbars

    ScrollBarRange(SB_HORZ, GridWidth + 2);
    ScrollBarRange(SB_VERT, (FNumRecords + FixedRows) * DefaultRowHeight + 2);
    
    //HorzScrollBar.Range:= GridWidth+2;
    //VertScrollBar.Range:= (FNumRecords + FixedRows) * DefaultRowHeight + 2;
    {
    for i:=1 to ColCount-1 do begin
      F:=FDataLink.Fields[i];
      if F<>nil then begin
        W:=F.DisplayWidth;
        if W<0 then W:=0;
        if W=0 then W:=F.GetDefaultwidth;
        DebugLn('Field ',F.FieldName,' DisplayWidth=', W);
      end;
    end;
    }
    {$ifdef dbgdbgrid}
    DebugLn('(',name,') ','TCustomGrid.LayoutChanged - DONE');
    {$endif}
    FLayoutChanging:=False;
  end;
end;

procedure TCustomDbGrid.DefineProperties(Filer: TFiler);
begin
end;

procedure TCustomDbGrid.BeforeMoveSelection(const DCol,DRow: Integer);
begin
  inherited BeforeMoveSelection(DCol, DRow);
  
  FDatalink.UpdateData;
  if DCol<>Col then begin
     // Its a Column Movement
     if assigned(OnColExit) then OnColExit(Self);
     FColEnterPending:=True;
  end;
  {
  Exit;
  if (DRow<>Row) then begin
    // Its a Row Movement
    D:= DRow - Row;
    FDatalink.MoveBy(D);
  end;
  }
end;

procedure TCustomDbGrid.HeaderClick(IsColumn: Boolean; index: Integer);
begin
  inherited HeaderClick(IsColumn, index);
end;

procedure TCustomDbGrid.KeyDown(var Key: Word; Shift: TShiftState);
  procedure MoveBy(Delta: Integer);
  begin
    //FSelfScroll:=True;
    FDatalink.MoveBy(Delta);
    //FSelfScroll:=False;
  end;
begin
  // inherited KeyDown(Key, Shift); // Fully override old KeyDown handler
  case Key of
    VK_DOWN: MoveBy(1);
    VK_UP: MoveBy(-1);
    VK_NEXT: MoveBy( VisibleRowCount );
    VK_PRIOR: MoveBy( -VisibleRowCount );
    else inherited;
  end;
end;

procedure TCustomDbGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  Gz: TGridZone;
  P: TPoint;
begin
  if csDesigning in componentState then Exit;
  if not GCache.ValidGrid then Exit;
  
  Gz:=MouseToGridZone(X,Y, False);
  case Gz of
    gzFixedRows, gzFixedCols: inherited MouseDown(Button, Shift, X, Y);
    else
      begin
        P:=MouseToCell(Point(X,Y));
        if P.Y=Row then inherited MouseDown(Button, Shift, X, Y)
        else begin
          BeginUpdate;
          FDatalink.MoveBy(P.Y - Row);
          Col:=P.X;
          EndUpdate(uoQuick);
        end;
      end;
  end;
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
  if FColEnterPending and Assigned(OnColEnter) then OnColEnter(Self);
  FColEnterPending:=False;
  UpdateActive;
end;

procedure TCustomDbGrid.DrawByRows;
var
  CurActiveRecord: Integer;
begin
  if FDataLink.ACtive then begin
    CurActiveRecord:=FDataLink.ActiveRecord;
    //PrimerRecord:=FDataLink.FirstRecord;
  end;
  try
    inherited DrawByRows;
  finally
    if FDataLink.Active then FDataLink.ActiveRecord:=CurActiveRecord;
  end;
end;
// 33 31 21 29 80 90 4 3
procedure TCustomDbGrid.DrawRow(ARow: Integer);
begin
  if Arow>=FixedRows then FDataLink.ActiveRecord:=ARow-FixedRows;
  inherited DrawRow(ARow);
end;

procedure DrawArrow(Canvas: TCanvas; R: TRect; Opt: TDataSetState);
var
  dx,dy, x, y: Integer;
begin
  case Opt of
    dsBrowse:
      begin //
          Canvas.Brush.Color:=clBlack;
          Canvas.Pen.Color:=clBlack;
          Dx:=6;
          Dy:=6;
          y:= R.top+ (R.Bottom-R.Top) div 2;
          x:= R.Left+2;
          Canvas.Polygon([point(x,y-dy),point(x+dx,y),point(x, y+dy),point(x,y-dy)]);
       end;
    dsEdit:
      begin // Normal
          Canvas.Brush.Color:=clRed;
          Canvas.Pen.Color:=clRed;
          Dx:=6;
          Dy:=6;
          y:= R.top+ (R.Bottom-R.Top) div 2;
          x:= R.Left+2;
          Canvas.Polygon([point(x,y-dy),point(x+dx,y),point(x, y+dy),point(x,y-dy)]);
       end;
    dsInsert:
      begin // Normal
          Canvas.Brush.Color:=clGreen;
          Canvas.Pen.Color:=clGreen;
          Dx:=6;
          Dy:=6;
          y:= R.top+ (R.Bottom-R.Top) div 2;
          x:= R.Left+2;
          Canvas.Polygon([point(x,y-dy),point(x+dx,y),point(x, y+dy),point(x,y-dy)]);
       end;
   end;
end;

procedure TCustomDbGrid.DrawCell(aCol, aRow: Integer; aRect: TRect;
  aState: TGridDrawState);
var
  F: TField;
  S: string;
begin
  // Draw appropiated attributes
  inherited DrawCell(aCol, aRow, aRect, aState);
  
  if not FDataLink.Active then
    Exit;
  
  // Draw text When needed
  if gdFixed in aState then begin
    if (aRow=0)and(ACol>=FixedCols) then begin
      // draw column headers
      F:=FDataLink.Fields[aCol-FixedCols];
      if F<>nil then
        Canvas.TextOut(Arect.Left+2,ARect.Top+2, F.FieldName);
    end else
    if (aCol=0)and(aRow=Row) then
      // draw row headers (selected/editing/* record)
      DrawArrow(Canvas, aRect, FDataLink.Dataset.State)
  end else begin
    // Draw the other cells
    try
      F:=FDataLink.Fields[Acol-FixedCols];
      if F<>nil then
        S := F.AsString
      else
        S := '';
    except
      S := 'Error!';
    end;
    Canvas.TextOut(aRect.Left+2,ARect.Top+2, S);
  end;
end;

procedure TCustomDbGrid.UpdateActive;
{
var
  LastRow: Integer;
  lastEditor: TWinControl;
  WasVisible: Boolean;
}
begin
  with FDataLink do begin
    if not GCache.ValidGrid then Exit;
    if DataSource=nil then Exit;
    DebugLn('(',Name,') ActiveRecord=', dbgs(ActiveRecord), ' FixedRows=',dbgs(FixedRows), ' Row=', dbgs(Row));
    Row:= FixedRows + ActiveRecord;
    {
    LastRow:=Row;
    LastEditor:= Editor;
    WasVisible:= (Lasteditor<>nil)and(LastEditor.Visible);
    FRow:=FixedRows + ActiveRecord;
    if LastRow<>FRow then
      ProcessEditor(LastEditor,Col,LastRow,WasVisible);
    }
  end;
  Invalidate;
end;

procedure TCustomDbGrid.VisualChange;
begin
  if FDataLink=nil then Exit;
  if not FVisualLock then begin
    inherited VisualChange;
  end;
  if not FLayoutChanging then begin
    LayoutChanged;
  end;
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
  inherited Destroy;
end;

{ TComponentDataLink }

function TComponentDataLink.GetFields(Index: Integer): TField;
begin
  if (index>=0)and(index<DataSet.FieldCount) then result:=DataSet.Fields[index];
end;

function TComponentDataLink.GetDataSetName: string;
begin
  Result:=FDataSetName;
  if DataSet<>nil then Result:=DataSet.Name;
end;

procedure TComponentDataLink.SetDataSetName(const AValue: string);
begin
  if FDataSetName<>AValue then FDataSetName:=AValue;
end;

procedure TComponentDataLink.RecordChanged(Field: TField);
begin
  {$ifdef dbgdbgrid}
  DebugLn('TComponentDataLink.RecordChanged');
  {$endif}
  if Assigned(OnRecordChanged) then OnRecordChanged(Field);
end;

procedure TComponentDataLink.DataSetChanged;
begin
  {$ifdef dbgdbgrid}
  DebugLn('TComponentDataLink.DataSetChanged');
  {$Endif}
  if Assigned(OnDataSetChanged) then OnDataSetChanged(DataSet);
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
  inherited LayoutChanged;
  {$ifdef dbgdbgrid}
  DebugLn('TComponentDataLink.LayoutChanged');
  {$endif}
end;

procedure TComponentDataLink.DataSetScrolled(Distance: Integer);
begin
  {$ifdef dbgdbgrid}
  DebugLn('TComponentDataLink.DataSetScrolled(',IntToStr(Distance),')');
  {$endif}
  if Assigned(OnDataSetScrolled) then OnDataSetScrolled(DataSet, Distance);
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
