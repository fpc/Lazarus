{%MainUnit ../lclintf.pas}

// Open a given URL with the default browser
function OpenURL(AURL: String): Boolean;
var
  cf: CFStringRef;
  url: CFURLRef;
begin
  if AURL = '' then
    Exit(False);
  url := CFURLCreateWithBytes(nil, @AURL[1], Length(AURL), kCFStringEncodingUTF8, nil);
  if not Assigned(url) then
    Exit(False);
  Result := LSOpenCFURLRef(url, nil) = 0;
  CFRelease(url);
end;

// Open a document with the default application associated with it in the system
function OpenDocument(APath: String): Boolean;
var
  ResultingPath: string;
begin
  Result := True;
  if not FileExistsUTF8(APath) then exit(false);
  // Paths with spaces need to be quoted, see bug 21651
  if (APath<>'') and (APath[1]<>'''') then
    ResultingPath:=QuotedStr(APath)
  else
    ResultingPath:=APath;
  RunCmdFromPath('open',ResultingPath);
end;
