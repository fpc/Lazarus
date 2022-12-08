unit TransferMacrosIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MacroDefIntf, MacroIntf, FileProcs, CodeToolManager;

type

  { TTransferMacroList }

  TTransferMacroListClass = class of TTransferMacroListIntf;

  { TTransferMacroListIntf }

  TTransferMacroListIntf = class
  protected
    function GetItems(Index: integer): TTransferMacro; virtual; abstract;
    procedure SetItems(Index: integer; AValue: TTransferMacro); virtual; abstract;
  public
    class function StrHasMacros(const s: string): boolean; virtual;
  public
    procedure Clear; virtual; abstract;
    function Count: integer; virtual; abstract;
    procedure Delete(Index: integer); virtual; abstract;
    procedure Add(NewMacro: TTransferMacro); virtual; abstract;
    function FindByName(const MacroName: string): TTransferMacro; virtual; abstract;
    procedure SetValue(const MacroName, NewValue: string); virtual; abstract;
    function SubstituteStr(var s: string; const Data: PtrInt = 0;
      Depth: integer = 0): boolean;  virtual; abstract;
    procedure ExecuteMacro(const MacroName: string;
      var MacroParam: string; const Data: PtrInt; out Handled, Abort: boolean;
      Depth: integer); virtual; abstract;

    property Items[Index: integer]: TTransferMacro
       read GetItems write SetItems; default;
  end;


procedure IncreaseCompilerParseStamp;

var
  GlobalMacroListClass: TTransferMacroListClass = TTransferMacroListIntf;
  GlobalMacroList: TTransferMacroListIntf = nil;

  CompilerParseStamp: integer = 0; // TimeStamp of base value for macros

implementation

{ TTransferMacroListIntf }

class function TTransferMacroListIntf.StrHasMacros(const s: string): boolean;
begin
  Result := False;
end;

procedure IncreaseCompilerParseStamp;
begin
  if IDEMacros<>nil then
    IDEMacros.IncreaseBaseStamp;
  CTIncreaseChangeStamp(TransferMacrosIntf.CompilerParseStamp);
  CodeToolBoss.DefineTree.ClearCache;
  //if Assigned(CompilerParseStampIncreased) then
  //  CompilerParseStampIncreased();
end;

end.

