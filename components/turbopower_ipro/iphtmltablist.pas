unit IpHtmlTabList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TIpHtmlTabList }

  TIpHtmlTabList = class(TFPList)
  private
    FIndex: Integer;
    function GetTabItem: TObject;
  public
    constructor Create;
    property Index: Integer read FIndex write FIndex;
    property TabItem: TObject read GetTabItem;
  end;

implementation

{ TIpHtmlTabList }

function TIpHtmlTabList.GetTabItem: TObject;
begin
  if (FIndex = -1) or (FIndex >= Count) then
    Exit(nil)
  else
    Result := TObject(Items[FIndex]);
end;

constructor TIpHtmlTabList.Create;
begin
  Inherited Create;
  FIndex:=-1;
end;

end.

