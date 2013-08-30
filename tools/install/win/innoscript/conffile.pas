
type
  TCfgFileState = (csNoFile, csUnreadable, csParsedOk);

var
  NewCFGFile: TStringList;

function LoadCFGFile(AFolder: String; var AList: TStringList): Boolean;
var
  cfgfile: String;
begin
  if AList = nil then
    AList := TStringList.Create
  else
    AList.Clear;

  cfgfile := AddBackslash(AFolder) + 'lazarus.cfg';
  Result := FileExists(cfgfile);
  if not Result then
    exit;
  AList.LoadFromFile(cfgfile);
end;

procedure CreateCFGFile(APCP: String; var AList: TStringList);
var
  cfgfile: String;
begin
  if AList = nil then
    AList := TStringList.Create
  else
    AList.Clear;
  AList.add('--primary-config-path=' + APCP);
end;

function ParseCFGFile(AFolder: String; var APrimConfDir: String): TCfgFileState;
var
  s, cfgfile: String;
  i: Integer;
  l: TStringList;
begin
  cfgfile := AddBackslash(AFolder) + 'lazarus.cfg';

  Result := csNoFile;
  if not FileExists(cfgfile) then begin
    Log('ParseCFGFile not existent');
    exit;
  end;

  Result := csUnreadable;
  l := TStringList.Create;
  l.LoadFromFile(cfgfile);
  for i := 0 to l.Count - 1 do
    if copy(l[i], 1, 6) = '--pcp=' then
      s := copy(l[i], 7, length(l[i]))
    else
    if copy(l[i], 1, 22) = '--primary-config-path=' then
      s := copy(l[i], 23, length(l[i]));
  l.Free;

  if s = '' then
    exit;

  if (s[1] = '"') and (s[length(s)] = '"') then
    s := copy(s, 2, length(s)-2)
  else
  if (s[1] = '''') and (s[length(s)] = '''') then
    s := copy(s, 2, length(s)-2)

  if s = '' then
    exit;

  if (not FileExists(AddBackslash(s) + 'environmentoptions.xml')) and
     (not IsDirEmpty(s))
  then begin
    Log('ParseCFGFile unreadable');
    exit;
  end;

  Result := csParsedOk;
  APrimConfDir := s;
  Log('ParseCFGFile OK');
end;

