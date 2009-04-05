
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
begin
  Result:=inherited GetBookMark;
  
  if Assigned(Dataset) then
    Result:=Dataset.GetBookmark;
end;

procedure TfrDBDataSet.GotoBookMark(BM: Pointer);
begin
  if Assigned(Dataset) then
    Dataset.GotoBookmark(BM);
end;

procedure TfrDBDataSet.FreeBookMark(BM: Pointer);
begin
  if Assigned(Dataset) and Assigned(BM) then
    Dataset.FreeBookmark(BM);
end;

procedure TfrDBDataSet.DisableControls;
begin
  if Assigned(Dataset) then
    Dataset.DisableControls;
end;

procedure TfrDBDataSet.EnableControls;
begin
  if Assigned(Dataset) then
    Dataset.EnableControls;
end;

procedure TfrDBDataSet.Init;
begin
  Open;
  FBookmark := frGetBookmark(TfrTDataSet(GetDataSet));
  
  FEof := False;
end;

procedure TfrDBDataSet.Exit;
begin
  if FBookMark <> frEmptyBookmark then
  begin
    if (FRangeBegin = rbCurrent) or (FRangeEnd = reCurrent) then
      frGotoBookmark(TfrTDataSet(GetDataSet), FBookmark);
    frFreeBookmark(TfrTDataSet(GetDataSet), FBookmark);
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
begin
  FEof := False;
  if FRangeEnd = reCurrent then
  begin
    b := frGetBookmark(GetDataSet);
    if frIsBookmarksEqual(GetDataSet, b, FBookmark) then
      FEof := True;
    frFreeBookmark(GetDataSet, b);
    System.Exit;
  end;
  GetDataSet.Next;
  inherited Next;
end;

procedure TfrDBDataSet.Refresh;
begin
  if GetDataset<>nil then
    GetDataset.Refresh;
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
