
{*****************************************}
{                                         }
{             FastReport v2.3             }
{            Report DB dataset            }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit LR_DBSet;

interface

{$I LR_Vers.inc}

uses
  SysUtils, Classes, LR_DBRel, LR_DSet,
  DB;

type

  { TfrDBDataSet }

  TfrDBDataSet = class(TfrDataset)
  private
    FDataSet: TDataSet;
    FDataSource: TDataSource;

    FOpenDataSource, FCloseDataSource: Boolean;
    FOnOpen, FOnClose: TNotifyEvent;
    FBookmark: TfrBookmark;
    FEof: Boolean;
    function GetSafeDataset: TDataSet;
    procedure SetDataSet(Value: TDataSet);
    procedure SetDataSource(Value: TDataSource);
  protected
    procedure Notification(AComponent: TComponent;
                           Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Init; override;
    procedure Exit; override;
    procedure First; override;
    procedure Next; override;
    procedure Refresh; override;
    procedure Open;
    procedure Close;
    function Eof: Boolean; override;
    function GetDataSet: TfrTDataSet;
    
    function GetBookMark : Pointer; override;
    procedure GotoBookMark(BM : Pointer); override;
    procedure FreeBookMark(BM : Pointer); override;
    procedure DisableControls; override;
    procedure EnableControls; override;
    
  published
    property CloseDataSource: Boolean read FCloseDataSource write FCloseDataSource default False;
    property DataSet: TDataSet read GetSafeDataset write SetDataSet;
    property DataSource: TDataSource read FDataSource write SetDataSource;
    property OpenDataSource: Boolean read FOpenDataSource write FOpenDataSource default True;
    property RangeBegin;
    property RangeEnd;
    property RangeEndCount;
    property OnCheckEOF;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnFirst;
    property OnNext;
    property OnOpen: TNotifyEvent read FOnOpen write FOnOpen;
  end;


implementation

uses LR_Class;

type
  EDSError = class(Exception);

constructor TfrDBDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOpenDataSource := True;
end;

procedure TfrDBDataSet.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = FDataSource then
      FDataSource := nil
    else if AComponent = FDataSet then
      FDataSet := nil
end;

procedure TfrDBDataSet.SetDataSet(Value: TDataSet);
begin
  FDataSet := Value;
  FDataSource := nil;
end;

function TfrDBDataSet.GetSafeDataset: TDataSet;
begin
  if FDataSource<>nil then
    Result := FDataSource.DataSet
  else
    Result := nil;
  if Result=nil then
    Result := FDataset;
end;

procedure TfrDBDataSet.SetDataSource(Value: TDataSource);
begin
  FDataSource := Value;
  if Value <> nil then
    FDataSet := nil;
end;

function TfrDBDataSet.GetDataSet: TfrTDataSet;
begin
  if (FDataSource <> nil) and (FDataSource.DataSet <> nil) then
    Result := TfrTDataSet(FDataSource.DataSet)
  else if FDataSet <> nil then
    Result := TfrTDataSet(FDataSet)
  else
  begin
    raise EDSError.Create('Unable to open dataset ' + Name);
    Result := nil;
  end;
end;

function TfrDBDataSet.GetBookMark: Pointer;
var
  ds: TDataset;
begin
  Result:=inherited GetBookMark;
  ds := DataSet;
  if Assigned(ds) then
    TBookMark(Result):=ds.GetBookmark; //increases refcount of bookmark
end;

procedure TfrDBDataSet.GotoBookMark(BM: Pointer);
var
  ds: TDataset;
begin
  ds := DataSet;
  if Assigned(ds) then
    ds.GotoBookmark(TBookMark(BM));
end;

procedure TfrDBDataSet.FreeBookMark(BM: Pointer);
var
  ds: TDataset;
begin
  {$IFNDEF noautomatedbookmark}
  SetLength(TBookMark(BM),0);  //decreases refcount of bookmark
  {$ENDIF noautomatedbookmark}
  ds := DataSet;
  if Assigned(ds) and Assigned(BM) then
    ds.FreeBookmark(BM);
end;

procedure TfrDBDataSet.DisableControls;
var
  ds: TDataset;
begin
  ds := DataSet;
  if Assigned(ds) then
    ds.DisableControls;
end;

procedure TfrDBDataSet.EnableControls;
var
  ds: TDataset;
begin
  ds := DataSet;
  if Assigned(ds) then
    ds.EnableControls;
end;

procedure TfrDBDataSet.Init;
begin
  Open;
  FBookmark := frGetBookmark(TfrTDataSet(GetDataSet));
  
  FEof := False;
end;

procedure TfrDBDataSet.Exit;
var
  ds: TDataset;
begin
  if FBookMark <> frEmptyBookmark then
  begin
    ds := GetDataSet;
    if (FRangeBegin = rbCurrent) or (FRangeEnd = reCurrent) then
      frGotoBookmark(TfrTDataSet(ds), FBookmark);
    frFreeBookmark(TfrTDataSet(ds), FBookmark);
  end;
  FBookMark := frEmptyBookmark;
  Close;
end;

procedure TfrDBDataSet.First;
begin
  if FRangeBegin = rbFirst then
    GetDataSet.First
  else if FRangeBegin = rbCurrent then
    frGotoBookmark(GetDataSet, FBookmark);
  FEof := False;
  inherited First;
end;

procedure TfrDBDataSet.Next;
var
  b: TfrBookmark;
  ds: TDataset;
begin
  FEof := False;
  ds := GetDataSet;
  if FRangeEnd = reCurrent then
  begin
    b := frGetBookmark(TfrTDataSet(ds));
    if frIsBookmarksEqual(TfrTDataSet(ds), b, FBookmark) then
      FEof := True;
    frFreeBookmark(TfrTDataSet(ds), b);
    System.Exit;
  end;
  ds.Next;
  inherited Next;
end;

procedure TfrDBDataSet.Refresh;
var
  ds: TDataset;
begin
  ds := GetDataSet;
  if ds<>nil then
    ds.Refresh;
end;

function TfrDBDataSet.Eof: Boolean;
begin
  Result := inherited Eof or GetDataSet.Eof or FEof;
end;

procedure TfrDBDataSet.Open;
begin
  if FOpenDataSource then GetDataSet.Open;
  if Assigned(FOnOpen) then FOnOpen(Self);
end;

procedure TfrDBDataSet.Close;
begin
  if Assigned(FOnClose) then FOnClose(Self);
  if FCloseDataSource then GetDataSet.Close;
end;



end.
