unit fpDbgSymTable;

{$mode objfpc}{$H+}

interface

uses
  DbgIntfBaseTypes,
  fgl, Classes;

type

  { TfpSymbolList }

  TfpSymbolList= class(specialize TFPGMap<String, TDBGPtr>)
  private
    FHighAddr: TDBGPtr;
    FLowAddr: TDBGPtr;
  public
    procedure SetAddressBounds(ALowAddr, AHighAddr: TDBGPtr);
    property LowAddr: TDBGPtr read FLowAddr;
    property HighAddr: TDBGPtr read FHighAddr;
  end;

implementation

{ TfpSymbolList }

procedure TfpSymbolList.SetAddressBounds(ALowAddr, AHighAddr: TDBGPtr);
begin
  FLowAddr := ALowAddr;
  FHighAddr := AHighAddr;
end;

end.

