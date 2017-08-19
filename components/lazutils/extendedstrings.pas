{
 /***************************************************************************
                             extendedstrings.pas
                             -------------------

 ***************************************************************************/

 *****************************************************************************
  This file is part of LazUtils.

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Mattias Gaertner
  
  TExtendedStrings is a normal TStringList, except that the Objects can hold
  any type of records.
}
unit ExtendedStrings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;
  
type
  TExtStringsOption = (
    esoClearRecordsOnCreate,
    esoFreeObjectsOnDelete
    );
  TExtStringsOptions = set of TExtStringsOption;

  TExtendedStringList = class(TStringList)
  private
    FOptions: TExtStringsOptions;
    FRecordSize: integer;
    function GetRecords(Index: integer): pointer;
    procedure SetOptions(const AValue: TExtStringsOptions);
    procedure SetRecords(Index: integer; const AValue: pointer);
    procedure SetRecordSize(const AValue: integer);
    procedure DoResizeRecord(Index, OldSize, NewSize: integer);
  protected
    procedure ResizeRecord(var ARecord: Pointer;
                           {%H-}Index, {%H-}OldSize, NewSize: integer); virtual;
    function GetObject(Index: Integer): TObject; override;
    procedure PutObject(Index: Integer; AnObject: TObject); override;
  public
    constructor Create(InitialRecordSize: integer);
    destructor Destroy; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure CreateRecord(Index: integer); virtual;
    procedure FreeRecord(Index: integer); virtual;
    procedure FreeAllRecords; virtual;
    function RecordAllocated(Index: integer): boolean;
    property Records[Index: integer]: pointer read GetRecords write SetRecords;
    property RecordSize: integer read FRecordSize write SetRecordSize;
    property Options: TExtStringsOptions read FOptions write SetOptions;
  end;

implementation

{ TExtendedStringList }

function TExtendedStringList.GetRecords(Index: integer): pointer;
begin
  if not RecordAllocated(Index) then CreateRecord(Index);
  Result:=inherited GetObject(Index);
end;

procedure TExtendedStringList.SetOptions(const AValue: TExtStringsOptions);
begin
  if FOptions=AValue then exit;
  FOptions:=AValue;
end;

procedure TExtendedStringList.SetRecords(Index: integer; const AValue: pointer);
begin
  FreeRecord(Index);
  inherited PutObject(Index,TObject(AValue));
end;

procedure TExtendedStringList.SetRecordSize(const AValue: integer);
var
  i: integer;
begin
  if FRecordSize=AValue then exit;
  for i:=0 to Count-1 do
    DoResizeRecord(i,FRecordSize,AValue);
  FRecordSize:=AValue;
end;

procedure TExtendedStringList.DoResizeRecord(Index, OldSize, NewSize: integer);
var
  CurRecord: Pointer;
begin
  CurRecord:=inherited GetObject(Index);
  if CurRecord=nil then exit;
  ResizeRecord(CurRecord,Index,OldSize,NewSize);
  inherited PutObject(Index,TObject(CurRecord));
end;

procedure TExtendedStringList.CreateRecord(Index: integer);
var
  NewRecord: Pointer;
begin
  GetMem(NewRecord,RecordSize);
  if (esoClearRecordsOnCreate in Options) then
    FillChar(NewRecord^,RecordSize,0);
  inherited PutObject(Index,TObject(NewRecord));
end;

procedure TExtendedStringList.FreeRecord(Index: integer);
var
  OldRecord: pointer;
  OldObject: TObject;
begin
  OldRecord:=inherited GetObject(Index);
  if OldRecord<>nil then begin
    if (esoFreeObjectsOnDelete in Options) then begin
      OldObject:=Objects[Index];
      if OldObject<>nil then begin
        OldObject.Free;
      end;
    end;
    FreeMem(OldRecord);
    inherited PutObject(Index,nil);
  end;
end;

procedure TExtendedStringList.FreeAllRecords;
var
  i: integer;
begin
  for i:=0 to Count-1 do
    FreeRecord(i);
end;

function TExtendedStringList.RecordAllocated(Index: integer): boolean;
begin
  Result:=(inherited GetObject(Index))<>nil;
end;

procedure TExtendedStringList.ResizeRecord(var ARecord: Pointer; Index, OldSize,
  NewSize: integer);
begin
  ReAllocMem(ARecord,NewSize);
end;

function TExtendedStringList.GetObject(Index: Integer): TObject;
var
  ARecord: Pointer;
begin
  ARecord:=inherited GetObject(Index);
  if ARecord<>nil then
    Result:=TObject(ARecord^)
  else
    Result:=nil;
end;

procedure TExtendedStringList.PutObject(Index: Integer; AnObject: TObject);
var
  ARecord: Pointer;
begin
  ARecord:=Records[Index];
  if ARecord=nil then
  begin
    CreateRecord(Index);
    ARecord:=Records[Index];
  end;
  TObject(ARecord^):=AnObject;
end;

constructor TExtendedStringList.Create(InitialRecordSize: integer);
begin
  inherited Create;
  FOptions:=[esoClearRecordsOnCreate];
  FRecordSize:=InitialRecordSize;
end;

destructor TExtendedStringList.Destroy;
begin
  FreeAllRecords;
  inherited Destroy;
end;

procedure TExtendedStringList.Clear;
begin
  FreeAllRecords;
  inherited Clear;
end;

procedure TExtendedStringList.Delete(Index: Integer);
begin
  FreeRecord(Index);
  inherited Delete(Index);
end;

end.
