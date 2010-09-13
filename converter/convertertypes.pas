unit ConverterTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs;

type

  { TopOffset }

  // Used when fixing top coordinates of controls inside a visual container.
  TSrcPropOffset = class
  private
    fParentType: string;
    fChildType: string;
    fPropName: string;
    fStartPos: integer;
  public
    constructor Create(aParentType, aChildType, aPropName: string; aStartPos: integer);
    destructor Destroy; override;
    property ParentType: string read fParentType;
    property ChildType: string read fChildType;
    property PropName: string read fPropName;
    property StartPos: integer read fStartPos;
  end;

  { TVisualOffset }

  // User defined settings.
  TVisualOffset = class
  private
    fParentType: string;
    fTop: Integer;
    fLeft: Integer;
  public
    constructor Create(const aParentType: string; aTop, aLeft: Integer);
    destructor Destroy; override;
    function ByProperty(aPropName: string): Integer;
  public
    property ParentType: string read fParentType;
    property Top: Integer read fTop;
    property Left: Integer read fLeft;
  end;

  { TVisualOffsets }

  TVisualOffsets = class(TObjectList)
  private
    function GetVisualOffset(Index: Integer): TVisualOffset;
    procedure SetVisualOffset(Index: Integer; const AValue: TVisualOffset);
  public
    constructor Create;
    destructor Destroy; override;
    function Find(aParentType: string; var Index: Integer): Boolean;
    function AddVisualOffset(const aParentType: string; aTop, aLeft: Integer): integer;
    property Items[Index: Integer]: TVisualOffset read GetVisualOffset
                                                 write SetVisualOffset; default;
  end;

  // types for errors

  { EConverterError }

  EDelphiConverterError = class(Exception)
    constructor Create(const AMessage: string);
  end;


implementation

{ EConverterError }

constructor EDelphiConverterError.Create(const AMessage: string);
begin
  inherited Create('Converter: '+AMessage);
end;


{ TopOffset }

constructor TSrcPropOffset.Create(aParentType, aChildType, aPropName: string; aStartPos: integer);
begin
  fParentType:=aParentType;
  fChildType:=aChildType;
  fPropName:=aPropName;
  fStartPos:=aStartPos;
end;

destructor TSrcPropOffset.Destroy;
begin
  inherited Destroy;
end;


{ TVisualOffset }

constructor TVisualOffset.Create(const aParentType: string; aTop, aLeft: Integer);
begin
  fParentType:=aParentType;
  fTop:=aTop;
  fLeft:=aLeft;
end;

destructor TVisualOffset.Destroy;
begin
  inherited Destroy;
end;

function TVisualOffset.ByProperty(aPropName: string): Integer;
begin
  if aPropName='Top' then
    Result:=Top
  else if aPropName='Left' then
    Result:=Left
  else
    Result:=0
end;


{ TVisualOffsets }

constructor TVisualOffsets.Create;
begin
  inherited Create;
end;

destructor TVisualOffsets.Destroy;
begin
  inherited Destroy;
end;

function TVisualOffsets.Find(aParentType: string; var Index: Integer): Boolean;
var
  i: Integer;
begin
  Result:=False;
  Index:=-1;
  for i:=0 to Count-1 do
    if Items[i].fParentType = aParentType then begin
      Result:=True;
      Index:=i;
      Break;
    end;
end;

function TVisualOffsets.AddVisualOffset(const aParentType: string; aTop, aLeft: Integer): integer;
// This is called when settings are read or when user made changes in GUI.
// Returns index for the added object, or -1 if not added (duplicate).
var
  x: integer;
begin
  Result:=-1;
  if (aParentType<>'') and not Find(aParentType, x) then
    Result:=Add(TVisualOffset.Create(aParentType, aTop, aLeft));
end;

// Getter / Setter :

function TVisualOffsets.GetVisualOffset(Index: Integer): TVisualOffset;
begin
  Result:=Inherited Items[Index] as TVisualOffset;
end;

procedure TVisualOffsets.SetVisualOffset(Index: Integer; const AValue: TVisualOffset);
begin
  Inherited Items[Index]:=AValue;
end;


end.

