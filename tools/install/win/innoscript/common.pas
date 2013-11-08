
function dbgsBool(b: Boolean): String; begin Result := 'False'; if b then Result := 'True'; end;

function GetAppId(param:string): String;
var
  s: String;
begin
  if (IsSecondaryCheckBoxChecked) or IsSecondaryUpdate then
  begin
	// Secondary
    s := RemoveBackslashUnlessRoot(Lowercase(WizardDirValue));
    Result := 'lazarus_sec_'+GetSHA1OfString(s) + '_' + IntToStr(length(s));
  end
  else
    Result := 'lazarus';
  if ForcePrimaryAppId then
    Result := 'lazarus';
  Log('App-Id='+Result);
end;

function GetPCPForDelete(param:string): String;
// Used by [InstallDelete]
// Name: {code:GetPCPForDelete}*.xml; Type: files; Tasks: delusersettings
// ... delete primary conf
begin
  if (IsSecondaryCheckBoxChecked) or IsSecondaryUpdate then 
  begin
    if SecondPCP = '' then
      Result := AddBackslash(WizardDirValue) // some fallback
    else
      Result := AddBackslash(SecondPCP);
  end
  else
    Result := ExpandConstant('{localappdata}\lazarus\');
  Log('PrimConf for Delete='+Result);
end;

function IsDirEmpty(s: String): Boolean;
var
	FindRec: TFindRec;
begin
  Result := not DirExists(s);
  if Result then exit;
  SetCurrentDir(s);
  Result := not FindFirst('*', FindRec);
  if Result then exit;
  if (FindRec.Name = '.') or (FindRec.Name = '..') then Result := not FindNext(FindRec);
  if (not Result) and ((FindRec.Name = '.') or (FindRec.Name = '..')) then Result := not FindNext(FindRec);
  FindClose(FindRec);
end;

function SaveCustomMessage(AMsgId, ADefaulText: String): String;
begin
  try
    Result := CustomMessage(AMsgId);
  except
    Result := ADefaulText;
  end;
end;

function GetDefDir( def: String ) : String;
// Used by [SETUP]
// DefaultDirName={code:GetDefDir|{sd}\lazarus}
begin
  if Pos( ' ', def ) > 0 then
  begin
    def := Copy( def, 1, Pos( ' ', def ) - 1 ) + '\NoFolderSpace';
  end;
  Result := def;
end;

procedure UpdateEnvironmentOptions();
// used by [FILES]
// Source: environmentoptions.xml; DestDir: {app}; AfterInstall: UpdateEnvironmentOptions; DestName: environmentoptions.xml
var
  FileName, Content: string;
  s: Ansistring;
begin
  FileName := ExpandConstant(CurrentFileName);
  LoadStringFromFile(FileName, s);
  Content := s;
  StringChange(Content, '%Temp%', GetTempDir);
  StringChange(Content, '%LazDir%', ExpandConstant('{app}'));
  StringChange(Content, '%FpcBinDir%', ExpandConstant('{app}\fpc\{#FPCVersion}\bin\{#FPCFullTarget}\'));
  SaveStringToFile(FileName, Content, False);
end;

function IsHKLMWriteable(): boolean;
begin
  Result := IsAdminLoggedOn or IsPowerUserLoggedOn;
end;

function IsHKLMNotWriteable: boolean;
begin
  Result := not IsHKLMWriteable();
end;

function GetAssociateDesc(const ext: string): string;
var
  AmpersandPos: integer;
begin
  Result := FmtMessage(CustomMessage('AssocFileExtension'), ['Lazarus',ext]);
  AmpersandPos := pos('&', Result);
  if AmpersandPos>0 then
    Delete(Result, AmpersandPos, 1);
end;

// ALeftDistance, ARightDistance: 0 = align with AControlAbove
//            other = distance to parent
// AHeight: negative = distance to parent bottom
//          0 = ignore
Procedure AddComponentToPage(AControl, AControlAbove: TControl;
  ATopDistance, ALeftDistance, ARightDistance, AHeight: Integer);
begin
  AControl.Parent := AControlAbove.Parent;
  AControl.Top    :=  AControlAbove.Top + AControlAbove.Height + ATopDistance;
  if ALeftDistance = 0 then
    AControl.Left :=  AControlAbove.Left
  else
    AControl.Left := ALeftDistance;
  if ARightDistance = 0 then
    AControl.Width :=  AControlAbove.Left + AControlAbove.Width - AControl.Left
  else
    AControl.Width :=  AControlAbove.Width - AControl.Left - ARightDistance;
  if AHeight < 0 then
    AControl.Height := AControlAbove.Parent.Height - AControl.Top + AHeight;
  if AHeight > 0 then
    AControl.Height := AHeight;
end;


