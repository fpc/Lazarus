unit DebuggerPropertiesBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, PropEdits;

type
  { TXmlConfStringList }

  TXmlConfStringList = class(TStringList)
  private
    function TextStored: boolean;
  published
    property Text stored TextStored;
  end;

  { TXmlConfStringsPropertyEditor }

  TXmlConfStringsPropertyEditor = class(TStringsPropertyEditor)
  public
    function GetValue: ansistring; override;
  end;

implementation

{ TXmlConfStringList }

function TXmlConfStringList.TextStored: boolean;
begin
  Result := Text <> '';
end;

{ TXmlConfStringsPropertyEditor }

function TXmlConfStringsPropertyEditor.GetValue: ansistring;
var
  s: TStrings;
  i: Integer;
begin
  Result := '';
  s := TStrings(GetObjectValue);
  for i := 0 to s.Count - 1 do begin
    if i > 0 then Result := Result + ' / ';
    Result := Result + s[i];
  end;
end;

initialization
  RegisterPropertyEditor(TypeInfo(TXmlConfStringList), nil, '', TXmlConfStringsPropertyEditor);

end.

