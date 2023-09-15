unit PList2JSon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Laz2_DOM, fpjson;

function PListXml2Json(AnXml: TXMLDocument): TJSONData;

implementation

function Xml2Json(AnNode: TDOMNode): TJSONData; forward;

function XmlDict2Json(AnNode: TDOMNode): TJSONData;
var
  c, i: integer;
  key: DOMString;
  val: TJSONData;
  keyNd: TDOMNode;
begin
  c := AnNode.GetChildCount;
  i := 0;

  Result := CreateJSONObject([]);
  try
    while i < c do begin
      keyNd := AnNode.ChildNodes[i];
      if LowerCase(keyNd.NodeName) <> 'key' then
        raise Exception.Create('Expected <key>, but got ' + keyNd.NodeName);
      key := Trim(keyNd.TextContent);
      inc(i);

      if i = c then
        raise Exception.Create('Expected value for key '+key);

      val := Xml2Json(AnNode.ChildNodes[i]);
      inc(i);

      TJSONObject(Result).Add(key, val)
    end;
  except
    Result.Free;
    raise;
  end;
end;

function XmlArray2Json(AnNode: TDOMNode): TJSONData;
var
  CN: TDOMNode;
begin
  Result := CreateJSONArray([]);
  try
    CN := AnNode.FirstChild;
    while CN <> nil do begin
      TJSONArray(Result).Add(Xml2Json(CN));
      CN := CN.NextSibling;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function Xml2Json(AnNode: TDOMNode): TJSONData;
var
  n: DOMString;
  i64: int64;
  f: Double;
  b: Boolean;
  d: TDateTime;
begin
  n := LowerCase(AnNode.NodeName);
  if (n = 'dict') or (n = 'dictionary') then begin
    Result := XmlDict2Json(AnNode);
  end
  else
  if n = 'array' then begin
    Result := XmlArray2Json(AnNode);
  end
  else
  if n = 'string' then begin
    Result := CreateJSON(AnNode.TextContent);
  end
  else
  if n = 'number' then begin
    if TryStrToInt64(trim(AnNode.TextContent), i64) then begin
      if (i64 >= low(integer)) and (i64 <= high(integer)) then
        Result := CreateJSON(integer(i64))
      else
        Result := CreateJSON(i64);
    end
    else begin
      if TryStrToFloat(trim(AnNode.TextContent), f) then
        Result := CreateJSON(f)
      else
        raise Exception.Create('Unknown number '+AnNode.TextContent);
    end;
  end
  else
  if n = 'boolean' then begin
    if TryStrToBool(trim(AnNode.TextContent), b) then
      Result := CreateJSON(b)
    else
      raise Exception.Create('Unknown bool '+AnNode.TextContent);
  end
  else
  if n = 'date' then begin
    if TryStrToDate(trim(AnNode.TextContent), d) then
      Result := CreateJSON(d)
    else
      raise Exception.Create('Unknown date'+AnNode.TextContent);
  end
  else
  if n = 'data' then begin
    raise Exception.Create('DATA not supported');
  end
  else
    raise Exception.Create('Unknown key '+n);
end;


function PListXml2Json(AnXml: TXMLDocument): TJSONData;
var
  CN: TDOMNode;
begin
  Result := nil;
  if AnXml = nil then
    raise Exception.Create('Missing XML');

  CN := AnXml.FirstChild;
  if CN = nil then
    raise Exception.Create('Missing XML content');

  if CN is TDOMDocumentType then
    CN := CN.NextSibling;

  if LowerCase(CN.NodeName) <> 'plist' then
    raise Exception.Create('Expected <plist> ');
  if CN.NextSibling <> nil then
    raise Exception.Create('Trailing extra data');

  if CN.GetChildCount <> 1 then
    raise Exception.Create('Expected <dict> or <array> ');

  CN := CN.ChildNodes[0];
  if (LowerCase(CN.NodeName) = 'dict') or (LowerCase(CN.NodeName) = 'dictionary') then
    Result := XmlDict2Json(CN)
  else
  if LowerCase(CN.NodeName) = 'CN' then
    Result := XmlArray2Json(AnXml)
  else
    raise Exception.Create('Expected <dict> or <array> ');
end;

end.

