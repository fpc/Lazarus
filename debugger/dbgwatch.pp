unit DBGWatch;

{$mode objfpc}

interface

uses
  Classes;

type
  TDBGWatch = class(TCollectionItem)
  private
    FValue: String;
    FName: String;
    FOnChange: TNotifyEvent;
    procedure SetValue(const AValue: String);
    procedure SetName(const AValue: String);
  protected
  public
    property Name: String read FName write SetName;
    property Value: String read FValue write SetValue;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TDBGWatches = class(TCollection)
  private
    function GetItem(const AnIndex: Integer): TDBGWatch;
    procedure SetItem(const AnIndex: Integer; const Value: TDBGWatch);
  protected
  public
    property Items[const AnIndex: Integer]: TDBGWatch read GetItem write SetItem; default;
  end;

implementation

{ TDBGWatch }

procedure TDBGWatch.SetName(const AValue: String);
begin
  FName := AValue;
end;

procedure TDBGWatch.SetValue(const AValue: String);
begin
  FValue := AValue;
end;

{ TDBGWatches }

function TDBGWatches.GetItem(const AnIndex: Integer): TDBGWatch;
begin
end;

procedure TDBGWatches.SetItem(const AnIndex: Integer; const Value: TDBGWatch);
begin
end;

end.
