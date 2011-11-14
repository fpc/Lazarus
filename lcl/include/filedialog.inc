{%MainUnit ../dialogs.pp}

{******************************************************************************
                                  TFileDialog
 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

{------------------------------------------------------------------------------}
{  TFileDialog Create                                                          }
{------------------------------------------------------------------------------}
constructor TFileDialog.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  fCompStyle := csFileDialog;
  FFiles := TStringList.Create;
  FHistoryList:=TStringList.Create;
  FFilterIndex := 1;
  FInternalFilterIndex := 1;
end;

{------------------------------------------------------------------------------}
{  TFileDialog Destroy                                                         }
{------------------------------------------------------------------------------}
destructor TFileDialog.Destroy;
begin
  FHistoryList.Free;
  FFiles.Free;
  inherited Destroy;
end;


{------------------------------------------------------------------------------}
{  TFileDialog DoOnTypeChange                                                         }
{------------------------------------------------------------------------------}
procedure TFileDialog.DoTypeChange;
begin
  if Assigned(FOnTypeChange) then
    OnTypeChange(Self);
end;

{------------------------------------------------------------------------------}
{  TFileDialog Execute                                                         }
{------------------------------------------------------------------------------}
function TFileDialog.Execute : boolean;
begin
  Result:=inherited Execute;
end;

{------------------------------------------------------------------------------
  TFileDialog DoExecute
------------------------------------------------------------------------------}
procedure TFileDialog.SetHistoryList(const AValue: TStrings);
begin
  FHistoryList.Assign(AValue);
end;

procedure TFileDialog.IntfFileTypeChanged(NewFilterIndex: Integer);
begin
  if FilterIndex <> NewFilterIndex then
  begin
    FInternalFilterIndex := NewFilterIndex;
    DoTypeChange;
  end;
end;

class function TFileDialog.FindMaskInFilter(aFilter, aMask: string): integer;
{ The filter is a list of pairs of captions and masks separated by pipe |
  The masks are separated by semicolon ;
  For example:
    Pascal Files|*.pas;*.pp|All files|*.*
}
var
  p: PChar;
  MaskStart: PChar;
begin
  Result:=0;
  if (aFilter='') or (aMask='') then exit;
  p:=PChar(aFilter);
  while p^<>#0 do
  begin
    // skip caption
    while not (p^ in ['|',#0]) do inc(p);
    if p^=#0 then break;
    // parse masks
    repeat
      inc(p);
      MaskStart:=p;
      while not (p^ in [';','|',#0]) do inc(p);
      //debugln(['TFileDialog.ExtractAllFilterMasks Mask=',copy(aFilter,MaskStart-PChar(aFilter)+1,p-MaskStart)]);
      if (p>MaskStart)
      and (CompareFilenames(MaskStart,p-MaskStart,PChar(aMask),length(aMask),false)=0)
      then
        exit(MaskStart-PChar(aFilter)+1);
      if p^='|' then break;
    until p^=#0;
    inc(p);
  end;
end;

class function TFileDialog.ExtractAllFilterMasks(aFilter: string;
  SkipAllFilesMask: boolean): string;
{ The filter is a list of pairs of captions and masks separated by pipe |
  The masks are separated by semicolon ;
  For example:
    Pascal Files|*.pas;*.pp|Include Files|*.inc|All files|*.*
    Result: *.pas;*.pp;*.inc
}
var
  AllFilesMask: string;

  procedure AddMask(MaskStart: PChar; MaskLen: integer; var Masks: string);
  var
    p: PChar;
    NewMask: string;
    Start: PChar;
  begin
    if (AllFilesMask<>'')
    and (CompareFilenames(@AllFilesMask[1],length(AllFilesMask),MaskStart,MaskLen,false)=0)
    then exit; // skip all files mask

    if Masks<>'' then
    begin
      p:=PChar(Masks);
      repeat
        Start:=p;
        while not (p^ in [#0,';']) do inc(p);
        if (CompareFilenames(Start,p-Start,MaskStart,MaskLen,false)=0) then
          exit;
        if p^=#0 then break;
        inc(p);
      until false;
    end;
    if Masks<>'' then
      Masks:=Masks+';';
    SetLength(NewMask,MaskLen);
    System.Move(MaskStart^,NewMask[1],length(NewMask));
    Masks:=Masks+NewMask;
  end;

var
  p: PChar;
  MaskStart: PChar;
begin
  Result:='';
  if aFilter='' then exit;
  if SkipAllFilesMask then
    AllFilesMask:=GetAllFilesMask
  else
    AllFilesMask:='';
  //debugln(['TFileDialog.ExtractAllFilterMasks Filter=',aFilter]);
  p:=PChar(aFilter);
  while p^<>#0 do
  begin
    // skip caption
    while not (p^ in ['|',#0]) do inc(p);
    if p^=#0 then break;
    // parse masks
    repeat
      inc(p);
      MaskStart:=p;
      while not (p^ in [';','|',#0]) do inc(p);
      //debugln(['TFileDialog.ExtractAllFilterMasks Mask=',copy(aFilter,MaskStart-PChar(aFilter)+1,p-MaskStart)]);
      if p>MaskStart then
        AddMask(MaskStart,p-MaskStart,Result);
      if p^='|' then break;
    until p^=#0;
    inc(p);
  end;
end;

{------------------------------------------------------------------------------
  procedure TFileDialog.SetDefaultExt(const AValue: string);
------------------------------------------------------------------------------}
procedure TFileDialog.SetDefaultExt(const AValue: string);
begin
  FDefaultExt:=AValue;
  if (FDefaultExt<>'') and (FDefaultExt[1]<>'.') then
    FDefaultExt:='.'+FDefaultExt;
end;

procedure TFileDialog.SetFilterIndex(const AValue: Integer);
begin
  FFilterIndex := AValue;
  if FHandle = 0 then
    FInternalFilterIndex := AValue;
end;

class procedure TFileDialog.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterFileDialog;
end;

{------------------------------------------------------------------------------
  TFileDialog DoExecute
------------------------------------------------------------------------------}
function TFileDialog.DoExecute : boolean;
begin
  Result:= inherited DoExecute;
end;

{------------------------------------------------------------------------------
  TFileDialog GetFilterIndex
------------------------------------------------------------------------------}
function TFileDialog.GetFilterIndex: Integer;
begin
  Result := FInternalFilterIndex;
end;

{------------------------------------------------------------------------------}
{  TFileDialog SetFilter                                                       }
{------------------------------------------------------------------------------}
procedure TFileDialog.SetFilter(const value : string);
begin
  FFilter := Value;  // make sure this is defined first before the CNSendMessage
end;

{------------------------------------------------------------------------------}
{  TFileDialog SetFileName                                                     }
{------------------------------------------------------------------------------}
procedure TFileDialog.SetFileName(const value : string);
begin
  if FFilename=Value then exit;
  FFileName := Value;  // make sure this is defined first before the CNSendMessage
end;

{******************************************************************************
                                  TOpenDialog
 ******************************************************************************}

class procedure TOpenDialog.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterOpenDialog;
end;

procedure TOpenDialog.DereferenceLinks;
var i: integer;
begin
  if Filename<>'' then
    Filename:=TryReadAllLinks(FileName);
  if Files<>nil then begin
    for i:=0 to Files.Count-1 do begin
      if Files[i]<>'' then
        Files[i]:=TryReadAllLinks(Files[i]);
    end;
  end;
end;


//Helper function
function GetExtensionFromFilterAtIndex(Filter: String; Index: Integer): String;
{
  Returns a file extension from a filter as used in TOpen/TSaveDialog
  - it will return the extension (including the leading period) that matches the index (index starts at 1)
  - it will return an empty string if the extension contains a wildcard, or on any failure
  - filters have the format of:
    'Text files (*.txt)|*.txt|'+
    'Pascal files (*.pp;*.pas)|*.pp;*.pas|'+
    'All files (*.*)|*.*'
  - if a given extension is a composite (like '*.pp;*.pas') it will return the first one from the list
}
var
  p, pipe: Integer;
begin
  Result := '';
  if Index < 1 then Exit;
  p := 0;
  pipe := 0;
  //Find where the filter for the given index starts
  while (p < Length(Filter)) do
  begin
    Inc(p);
    //Debugln('p = ',dbgs(p),' Filter[',dbgs(p),'] = ',Filter[p]);
    if Filter[p] = '|' then Inc(pipe);
    if (pipe = 2 * (Index - 1)) then break;
  end;
  //debugln('p = ',dbgs(p),' pipe = ',dbgs(pipe));
  if (p = length(Filter)) then exit;
  System.Delete(Filter,1,p);
  //Find the | that splits the filter name and the filter extension
  p := Pos('|',Filter);
  if (p = 0) then exit;
  System.Delete(Filter,1,p);
  Filter := Copy(Filter,1,MaxInt);
  //debugln('Filter now = ',filter);

  //The associated extension ends at the first ; or |, or at the end of the string
  p := Pos(';',Filter);
  pipe := Pos('|',Filter);
  //Debugln('Pos(;/|,Filter) = ',dbgs(p),' ',dbgs(pipe));
  if (pipe < p) or (p = 0) then p := pipe;
  if (p > 0) then System.Delete(Filter,p,Length(Filter) - p + 1);
  //debugln('Filter now = ',filter);

  //Get the associated extension
  Filter := ExtractFileExt(Filter);
  //debugln('Filter now = ',filter);
  //if an extension at this point contains a wildcard, reject it
  if (Pos('?',Filter) > 0) or (Pos('*',Filter) > 0) then exit;
  Result := Filter;
end;

function TOpenDialog.CheckFile(var AFilename: string): boolean;
var
  Dir, Ext: string;
begin
  Result:=true;
  if (DefaultExt<>'') and (ExtractFileExt(AFilename)='')
  and (not FileExistsUTF8(AFilename)) then begin
    Ext := GetExtensionFromFilterAtIndex(Filter, FilterIndex);
    if (Length(Ext) > 0) then
      AFileName := AFileName + Ext
    else
      AFilename:=AFilename+DefaultExt;
  end;
  //ofOverwritePrompt -> is done in the interface
  if (ofPathMustExist in Options)
  and (not DirPathExists(ExtractFileDir(AFilename))) then begin
    Result:=false;
    MessageDlg(rsfdPathMustExist,
               Format(rsfdPathNoExist,[ExtractFileDir(AFilename)]),
               mtError,[mbCancel],0);
    exit;
  end;
  if (ofFileMustExist in Options)
  and (not CheckFileMustExist(AFileName)) then begin
    // CheckFileMustExists shows message dialog
    Result:=false;
    exit;
  end;
  if ofNoReadOnlyReturn in Options then begin
    if FileExistsUTF8(AFilename) then
      Result := FileIsWritable(AFilename)
    else begin { File does not exist - check directory }
      Dir := ExtractFileDir(AFilename);
      if Dir = '' then
        Dir := '.';
      Result := FileIsWritable(Dir);
    end;
    if not Result then begin
        MessageDlg(rsfdFileReadOnlyTitle,
                   Format(rsfdFileReadOnly,[AFileName]),
                   mtError,[mbCancel],0);
        exit;
    end;
  end;
end;

function TOpenDialog.CheckFileMustExist(const AFileName: string): boolean;
begin
  if not FileExistsUTF8(AFilename) then begin
    Result:=false;
    MessageDlg(rsfdFileMustExist,
               Format(rsfdFileNotExist,[AFileName]),mtError,
               [mbCancel],0);
  end else
    Result:=true;
end;

function TOpenDialog.CheckAllFiles: boolean;
var
  AFilename: String;
  i: Integer;
begin
  Result:=true;

  AFilename:=Filename;
  if (AFilename<>'')
  or (not (ofAllowMultiSelect in Options)) then begin
    Result:=CheckFile(AFilename);
    Filename:=AFilename;
    if not Result then exit;
  end;

  if ofAllowMultiSelect in Options then begin
    for i:=0 to Files.Count-1 do begin
      AFilename:=Files[i];
      Result:=CheckFile(AFilename);
      Files[i]:=AFilename;
      if not Result then exit;
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method: TOpenDialog.DoExecute
  Params:  none
  Returns: true if valid was selected

  Starts dialogs and lets user choose a filename.
 ------------------------------------------------------------------------------}
function TOpenDialog.DoExecute: boolean;
begin
  Result:=inherited DoExecute;
  if (not (ofNoDereferenceLinks in Options)) then begin
    DereferenceLinks;
  end;
  if (not (ofNoChangeDir in Options)) then begin
    if (ExtractFilePath(Filename)<>'') then
      InitialDir:=ExtractFilePath(Filename)
    else if (Files.Count>0) and (ExtractFilePath(Files[0])<>'') then
      InitialDir:=ExtractFilePath(Files[0]);
  end;
  if not Result then exit;
  Result:=CheckAllFiles;
end;

function TOpenDialog.DefaultTitle: string;
begin
  Result:= rsfdOpenFile;
end;

{------------------------------------------------------------------------------
  Method: TOpenDialog.Create
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TOpenDialog.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  fCompStyle:=csOpenFileDialog;
  FOptions := DefaultOpenDialogOptions;
end;

procedure TOpenDialog.DoFolderChange;
begin
  if Assigned(OnFolderChange) then OnFolderChange(Self);
end;

procedure TOpenDialog.DoSelectionChange;
var
  CurFilename: String;
begin
  CurFilename:=Filename;
  if FLastSelectionChangeFilename=CurFilename then exit;
  FLastSelectionChangeFilename:=CurFilename;
  if Assigned(OnSelectionChange) then OnSelectionChange(Self);
end;

procedure TOpenDialog.IntfSetOption(const AOption: TOpenOption;
  const AValue: Boolean);
begin
  if AValue then
    Include(FOptions, AOption)
  else
    Exclude(FOptions, AOption);
end;



class procedure TSaveDialog.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterSaveDialog;
end;

function TSaveDialog.DefaultTitle: string;
begin
  Result:=rsfdFileSaveAs;
end;

{******************************************************************************
                                  TSaveDialog
 ******************************************************************************}
{------------------------------------------------------------------------------
  Method: TSaveDialog.Create
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TSaveDialog.Create (AOwner : TComponent);
begin
  inherited Create(AOwner);
  fCompStyle:=csSaveFileDialog;
end;

{******************************************************************************
                             TSelectDirectoryDialog
 ******************************************************************************}

{ TSelectDirectoryDialog }

constructor TSelectDirectoryDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCompStyle:=csSelectDirectoryDialog;
end;

class procedure TSelectDirectoryDialog.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterSelectDirectoryDialog;
end;

function TSelectDirectoryDialog.CheckFileMustExist(const AFilename: string): boolean;
begin
  if not DirPathExists(AFilename) then begin
    Result:=false;
    MessageDlg(rsfdDirectoryMustExist,
               Format(rsfdDirectoryNotExist,[AFileName]),mtError,
               [mbCancel],0);
  end else
    Result:=true;
end;

function TSelectDirectoryDialog.DefaultTitle: string;
begin
  Result:=rsfdSelectDirectory;
end;


