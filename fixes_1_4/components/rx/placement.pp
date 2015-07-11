{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1995, 1996 AO ROSNO             }
{         Copyright (c) 1997 Master-Bank                }
{                                                       }
{*******************************************************}

{$mode objfpc}
{$h+}

unit Placement;

interface

uses Controls, Classes, FileUtil, Forms, IniFiles, Dialogs, RTTIUtils;

  
type
  TPlacementOption = (fpState, fpPosition, fpActiveControl);
  TPlacementOptions = set of TPlacementOption;
  TPlacementOperation = (poSave, poRestore);

  TIniLink       = Class;
  TFormPlacement = Class;
  TStoredValue   = Class;
  TStoredValues  = Class;
  

{ TStoredValue }

{$ifdef storevariant}
  TStoredType = Variant;
{$else}
  TStoredType = AnsiString;
{$endif}  

  TStoredValueEvent = procedure(Sender: TStoredValue; var Value: TStoredType) of object;

  TStoredValue = class(TCollectionItem)
  private
    FName: string;
    FValue: TStoredType;
    FKeyString: string;
    FOnSave: TStoredValueEvent;
    FOnRestore: TStoredValueEvent;
    function IsValueStored: Boolean;
    function GetStoredValues: TStoredValues;
  protected
    function GetDisplayName: string; override;
    procedure SetDisplayName(const Value: string); override;
  public
    constructor Create(ACollection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    procedure Save; virtual;
    procedure Restore; virtual;
    property StoredValues: TStoredValues read GetStoredValues;
  published
    property Name: string read FName write SetDisplayName;
    property Value: TStoredType read FValue write FValue stored IsValueStored;
    property KeyString: string read FKeyString write FKeyString;
    property OnSave: TStoredValueEvent read FOnSave write FOnSave;
    property OnRestore: TStoredValueEvent read FOnRestore write FOnRestore;
  end;

{ TStoredValues }

  TStoredValues = class(TOwnedCollection)
  private
    FStorage: TFormPlacement;
    function GetValue(const AName: string): TStoredValue;
    procedure SetValue(const AName: string; StoredValue: TStoredValue);
    function GetStoredValue(const AName: string): TStoredType;
    procedure SetStoredValue(const AName: string; Value: TStoredType);
    function GetItem(Index: Integer): TStoredValue;
    procedure SetItem(Index: Integer; StoredValue: TStoredValue);
  public
    constructor Create(AOwner: TPersistent);
    function IndexOf(const AName: string): Integer;
    procedure SaveValues; virtual;
    procedure RestoreValues; virtual;
    property Storage: TFormPlacement read FStorage write FStorage;
    property Items[Index: Integer]: TStoredValue read GetItem write SetItem; default;
    property Values[const Name: string]: TStoredValue read GetValue write SetValue;
    property StoredValue[const Name: string]: TStoredType read GetStoredValue write SetStoredValue;
  end;

{ TFormPlacement }

  TFormPlacement = class(TComponent)
  private
    FActive: Boolean;
    FIniFileName: String;
    FIniSection: String;
    FIniFile: TCustomIniFile;
    FLinks: TList;
    FOptions: TPlacementOptions;
    FVersion: Integer;
    FSaved: Boolean;
    FRestored: Boolean;
    FDestroying: Boolean;
    //FDefMaximize: Boolean;
    FSaveFormShow: TNotifyEvent;
    FSaveFormDestroy: TNotifyEvent;
    FSaveFormCloseQuery: TCloseQueryEvent;
    FOnSavePlacement: TNotifyEvent;
    FOnRestorePlacement: TNotifyEvent;
    procedure SetEvents;
    procedure RestoreEvents;
    function  GetIniSection: string;
    procedure SetIniSection(const Value: string);
    function  GetIniFileName: string;
    procedure SetIniFileName(const Value: string);
    procedure AddLink(ALink: TIniLink);
    procedure NotifyLinks(Operation: TPlacementOperation);
    procedure RemoveLink(ALink: TIniLink);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDestroy(Sender: TObject);
    function GetForm: TForm;
  protected
    procedure IniNeeded(ReadOnly: Boolean);Virtual;
    procedure IniFree;Virtual;
    procedure Loaded; override;
    procedure Save; dynamic;
    procedure Restore; dynamic;
    procedure SavePlacement; virtual;
    procedure RestorePlacement; virtual;
    function  DoReadString(const Section, Ident, Default: string): string; virtual;
    procedure DoWriteString(const Section, Ident, Value: string); virtual;
    property  Form: TForm read GetForm;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SaveFormPlacement;
    procedure RestoreFormPlacement;
    function ReadString(const Ident, Default: string): string;
    procedure WriteString(const Ident, Value: string);
    function ReadInteger(const Ident: string; Default: Longint): Longint;
    procedure WriteInteger(const Ident: string; Value: Longint);
    procedure EraseSections;
    property IniFile: TCustomIniFile read FIniFile;
  published
    property Active: Boolean read FActive write FActive default True;
    property IniFileName: string read GetIniFileName write SetIniFileName;
    property IniSection: string read GetIniSection write SetIniSection;
    property Options: TPlacementOptions read FOptions write FOptions default [fpState, fpPosition];
    property Version: Integer read FVersion write FVersion default 0;
    property OnSavePlacement: TNotifyEvent read FOnSavePlacement write FOnSavePlacement;
    property OnRestorePlacement: TNotifyEvent read FOnRestorePlacement  write FOnRestorePlacement;
  end;

{ TFormStorage }

  TFormStorage = class(TFormPlacement)
  private
    FStoredProps: TStrings;
    FStoredValues: TStoredValues;
    procedure SetStoredProps(Value: TStrings);
    procedure SetStoredValues(Value: TStoredValues);
    function GetStoredValue(const AName: string): TstoredType;
    procedure SetStoredValue(const AName: string; Value: TStoredType);
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SavePlacement; override;
    procedure RestorePlacement; override;
    procedure SaveProperties; virtual;
    procedure RestoreProperties; virtual;
    procedure WriteState(Writer: TWriter); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetNotification;
    property StoredValue[const AName: string]: TStoredType read GetStoredValue write SetStoredValue;
  published
    property StoredProps: TStrings read FStoredProps write SetStoredProps;
    property StoredValues: TStoredValues read FStoredValues write SetStoredValues;
  end;

{ TIniLink }

  TIniLink = class(TPersistent)
  private
    FStorage: TFormPlacement;
    FOnSave: TNotifyEvent;
    FOnLoad: TNotifyEvent;
    function GetIniObject: TCustomIniFile;
    function GetRootSection: string;
    procedure SetStorage(Value: TFormPlacement);
  protected
    procedure SaveToIni; virtual;
    procedure LoadFromIni; virtual;
  public
    destructor Destroy; override;
    property IniObject: TCustomInifile read GetIniObject;
    property Storage: TFormPlacement read FStorage write SetStorage;
    property RootSection: string read GetRootSection;
    property OnSave: TNotifyEvent read FOnSave write FOnSave;
    property OnLoad: TNotifyEvent read FOnLoad write FOnLoad;
  end;


implementation

uses SysUtils, AppUtils, RTLConsts;

const
{ The following strings should not be localized }
  siActiveCtrl = 'ActiveControl';
  siVisible = 'Visible';
  siVersion = 'FormVersion';

function XorEncode(const Key, Source: string): string;
var
  I: Integer;
  C: Byte;
begin
  Result := '';
  for I := 1 to Length(Source) do begin
    if Length(Key) > 0 then
      C := Byte(Key[1 + ((I - 1) mod Length(Key))]) xor Byte(Source[I])
    else
      C := Byte(Source[I]);
    Result := Result + AnsiLowerCase(IntToHex(C, 2));
  end;
end;

function XorDecode(const Key, Source: string): string;
var
  I: Integer;
  C: Char;
  
begin
  Result := '';
  for I := 0 to Length(Source) div 2 - 1 do begin
    C := Chr(StrToIntDef('$' + Copy(Source, (I * 2) + 1, 2), Ord(' ')));
    if Length(Key) > 0 then
      C := Chr(Byte(Key[1 + (I mod Length(Key))]) xor Byte(C));
    Result := Result + C;
  end;
end;
                            

Function GetDefaultIniName : String;

begin
{$ifdef unix}
  Result:=IncludeTrailingPathDelimiter(GetEnvironmentVariableUTF8('HOME'))
          +'.'+ExtractFileName(Application.ExeName)

{$else}
  Result:=ChangeFileExt(Application.ExeName,'.ini');
{$endif}
end;

function FindPart(const HelpWilds, InputStr: string): Integer;

var
  I, J: Integer;
  Diff: Integer;
  
begin
  I := Pos('?', HelpWilds);
  if I = 0 then begin
    { if no '?' in HelpWilds }
    Result := Pos(HelpWilds, InputStr);
    Exit;
  end;
  { '?' in HelpWilds }
  Diff := Length(InputStr) - Length(HelpWilds);
  if Diff < 0 then begin
    Result := 0;
    Exit;
  end;
  { now move HelpWilds over InputStr }
  for I := 0 to Diff do begin
    for J := 1 to Length(HelpWilds) do begin
      if (InputStr[I + J] = HelpWilds[J]) or
        (HelpWilds[J] = '?') then
      begin
        if J = Length(HelpWilds) then begin
          Result := I + 1;
          Exit;
        end;
      end
      else Break;
    end;
  end;
  Result := 0;
end;



function IsWild(InputStr, Wilds: string; IgnoreCase: Boolean): Boolean;

 function SearchNext(var Wilds: string): Integer;
 { looking for next *, returns position and string until position }
 begin
   Result := Pos('*', Wilds);
   if Result > 0 then Wilds := Copy(Wilds, 1, Result - 1);
 end;

var
  CWild, CInputWord: Integer; { counter for positions }
  I, LenHelpWilds: Integer;
  MaxInputWord, MaxWilds: Integer; { Length of InputStr and Wilds }
  HelpWilds: string;
begin
  if Wilds = InputStr then begin
    Result := True;
    Exit;
  end;
  repeat { delete '**', because '**' = '*' }
    I := Pos('**', Wilds);
    if I > 0 then
      Wilds := Copy(Wilds, 1, I - 1) + '*' + Copy(Wilds, I + 2, MaxInt);
  until I = 0;
  if Wilds = '*' then begin { for fast end, if Wilds only '*' }
    Result := True;
    Exit;
  end;
  MaxInputWord := Length(InputStr);
  MaxWilds := Length(Wilds);
  if IgnoreCase then begin { upcase all letters }
    InputStr := AnsiUpperCase(InputStr);
    Wilds := AnsiUpperCase(Wilds);
  end;
  if (MaxWilds = 0) or (MaxInputWord = 0) then begin
    Result := False;
    Exit;
  end;
  CInputWord := 1;
  CWild := 1;
  Result := True;
  repeat
    if InputStr[CInputWord] = Wilds[CWild] then begin { equal letters }
      { goto next letter }
      Inc(CWild);
      Inc(CInputWord);
      Continue;
    end;
    if Wilds[CWild] = '?' then begin { equal to '?' }
      { goto next letter }
      Inc(CWild);
      Inc(CInputWord);
      Continue;
    end;
    if Wilds[CWild] = '*' then begin { handling of '*' }
      HelpWilds := Copy(Wilds, CWild + 1, MaxWilds);
      I := SearchNext(HelpWilds);
      LenHelpWilds := Length(HelpWilds);
      if I = 0 then begin
        { no '*' in the rest, compare the ends }
        if HelpWilds = '' then Exit; { '*' is the last letter }
        { check the rest for equal Length and no '?' }
        for I := 0 to LenHelpWilds - 1 do begin
          if (HelpWilds[LenHelpWilds - I] <> InputStr[MaxInputWord - I]) and
            (HelpWilds[LenHelpWilds - I]<> '?') then
          begin
            Result := False;
            Exit;
          end;
        end;
        Exit;
      end;
      { handle all to the next '*' }
      Inc(CWild, 1 + LenHelpWilds);
      I := FindPart(HelpWilds, Copy(InputStr, CInputWord, MaxInt));
      if I= 0 then begin
        Result := False;
        Exit;
      end;
      CInputWord := I + LenHelpWilds;
      Continue;
    end;
    Result := False;
    Exit;
  until (CInputWord > MaxInputWord) or (CWild > MaxWilds);
  { no completed evaluation }
  if CInputWord <= MaxInputWord then Result := False;
  if (CWild <= MaxWilds) and (Wilds[MaxWilds] <> '*') then Result := False;
end;

{ TFormPlacement }

constructor TFormPlacement.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActive := True;
  if (AOwner is TForm) then
    FOptions := [fpState, fpPosition]
  else
    FOptions := [];
  FLinks := TList.Create;
end;

destructor TFormPlacement.Destroy;
begin
  IniFree;
  while FLinks.Count > 0 do
    RemoveLink(TiniLink(FLinks.Last));
  FreeAndNil(FLinks);
  if not (csDesigning in ComponentState) then
    RestoreEvents;
  inherited Destroy;
end;

procedure TFormPlacement.Loaded;
var
  IsLoading: Boolean;
begin
  IsLoading := csLoading in ComponentState;
  inherited Loaded;
  if not (csDesigning in ComponentState) then
    begin
    if IsLoading then
      SetEvents;
    end;
end;

procedure TFormPlacement.AddLink(ALink: TIniLink);
begin
  FLinks.Add(ALink);
  ALink.FStorage := Self;
end;

procedure TFormPlacement.NotifyLinks(Operation: TPlacementOperation);
var
  I: Integer;
begin
  for I := 0 to FLinks.Count - 1 do
    with TIniLink(FLinks[I]) do
      case Operation of
        poSave: SaveToIni;
        poRestore: LoadFromIni;
      end;
end;

procedure TFormPlacement.RemoveLink(ALink: TIniLink);
begin
  ALink.FStorage := nil;
  FLinks.Remove(ALink);
end;

function TFormPlacement.GetForm: TForm;
begin
  if (Owner is TCustomForm) then
    Result := TForm(Owner as TCustomForm)
  else
    Result := nil;
end;

procedure TFormPlacement.SetEvents;
begin
  if (Owner is TCustomForm) then
    begin
    with TForm(Form) do
      begin
      FSaveFormShow := OnShow;
      OnShow := @FormShow;
      FSaveFormCloseQuery := OnCloseQuery;
      OnCloseQuery := @FormCloseQuery;
      FSaveFormDestroy := OnDestroy;
      OnDestroy := @FormDestroy;
      end;
    end;
end;

procedure TFormPlacement.RestoreEvents;
begin
  if (Owner <> nil) and (Owner is TCustomForm) then
    with TForm(Form) do
      begin
      OnShow := FSaveFormShow;
      OnCloseQuery := FSaveFormCloseQuery;
      OnDestroy := FSaveFormDestroy;
      end;
end;


procedure TFormPlacement.FormShow(Sender: TObject);
begin
  if Active then
    try
      RestoreFormPlacement;
    except
      Application.HandleException(Self);
    end;
  if Assigned(FSaveFormShow) then FSaveFormShow(Sender);
end;

procedure TFormPlacement.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if Assigned(FSaveFormCloseQuery) then
    FSaveFormCloseQuery(Sender, CanClose);
  if CanClose and Active and (Owner is TCustomForm) and (Form.Handle <> 0) then
    try
      SaveFormPlacement;
    except
      Application.HandleException(Self);
    end;
end;

procedure TFormPlacement.FormDestroy(Sender: TObject);
begin
  if Active and not FSaved then
    begin
    FDestroying := True;
    try
      SaveFormPlacement;
    except
      Application.HandleException(Self);
    end;
    FDestroying := False;
    end;
  if Assigned(FSaveFormDestroy) then
    FSaveFormDestroy(Sender);
end;



function TFormPlacement.GetIniFileName: string;
begin
  Result := FIniFileName;
  if (Result = '') and not (csDesigning in ComponentState) then
    Result := GetDefaultIniName;
end;

procedure TFormPlacement.SetIniFileName(const Value: string);
begin
  FIniFileName:=Value;
end;

function TFormPlacement.GetIniSection: string;
begin
  Result := FIniSection;
  if (Result = '') and not (csDesigning in ComponentState) then
    Result := GetDefaultSection(Owner);
end;

procedure TFormPlacement.SetIniSection(const Value: string);
begin
  FIniSection:=Value;
end;

procedure TFormPlacement.Save;
begin
  if Assigned(FOnSavePlacement) then
    FOnSavePlacement(Self);
end;

procedure TFormPlacement.Restore;
begin
  if Assigned(FOnRestorePlacement) then FOnRestorePlacement(Self);
end;

procedure TFormPlacement.SavePlacement;
begin
  if (Owner is TCustomForm) then
    begin
    if (Options * [fpState, fpPosition] <> []) then
      begin
      WriteFormPlacement(Form, IniFile, IniSection);
      IniFile.WriteBool(IniSection, siVisible, FDestroying);
      end;
    if (fpActiveControl in Options) and (Form.ActiveControl <> nil) then
      IniFile.WriteString(IniSection, siActiveCtrl, Form.ActiveControl.Name);
    end;
  NotifyLinks(poSave);
end;

procedure TFormPlacement.RestorePlacement;
begin
  if Owner is TCustomForm then
    ReadFormPlacement(Form, IniFile, IniSection, fpState in Options, fpPosition in Options);
  NotifyLinks(poRestore);
end;

procedure TFormPlacement.IniNeeded(ReadOnly: Boolean);
begin
  if ReadOnly then ;
  if IniFile = nil then
    FIniFile := TIniFile.Create(UTF8ToSys(IniFileName));
end;

procedure TFormPlacement.IniFree;
begin
  if IniFile <> nil then
    FreeAndNil(FIniFile);
end;

function TFormPlacement.DoReadString(const Section, Ident,
  Default: string): string;
begin
  if IniFile <> nil then
    Result := IniFile.ReadString(Section, Ident, Default)
  else
    begin
    IniNeeded(True);
    try
      Result := Inifile.ReadString(Section, Ident, Default);
    finally
      IniFree;
    end;
  end;
end;

function TFormPlacement.ReadString(const Ident, Default: string): string;
begin
  Result := DoReadString(IniSection, Ident, Default);
end;

procedure TFormPlacement.DoWriteString(const Section, Ident, Value: string);
begin
  if IniFile<>nil then
    IniFile.WriteString(Section, Ident, Value)
  else begin
    IniNeeded(False);
    try
      IniFile.WriteString(Section, Ident, Value);
    finally
      IniFree;
    end;
  end;
end;

procedure TFormPlacement.WriteString(const Ident, Value: string);
begin
  DoWriteString(IniSection, Ident, Value);
end;

function TFormPlacement.ReadInteger(const Ident: string; Default: Longint): Longint;
begin
  if (IniFile<>nil) then
    Result := IniFile.ReadInteger(IniSection, Ident, Default)
  else
    begin
    IniNeeded(True);
    try
      Result := Inifile.ReadInteger(IniSection, Ident, Default);
    finally
      IniFree;
    end;
  end;
end;

procedure TFormPlacement.WriteInteger(const Ident: string; Value: Longint);
begin
  if IniFile<>nil then
    IniFile.WriteInteger(IniSection, Ident, Value)
  else begin
    IniNeeded(False);
    try
      Inifile.WriteInteger(IniSection, Ident, Value);
    finally
      IniFree;
    end;
  end;
end;


procedure TFormPlacement.EraseSections;
var
  Lines: TStrings;
  I: Integer;
begin
  if IniFile= nil then begin
    IniNeeded(False);
    try
      Lines := TStringList.Create;
      try
        Inifile.ReadSections(Lines);
        for I := 0 to Lines.Count - 1 do begin
          if (Lines[I] = IniSection) or
            (IsWild(Lines[I], IniSection + '.*', False) or
            IsWild(Lines[I], IniSection + '\*', False)) then
            Inifile.EraseSection(Lines[I]);
        end;
      finally
        Lines.Free;
      end;
    finally
      IniFree;
    end;
  end;
end;

procedure TFormPlacement.SaveFormPlacement;
begin
  if FRestored or not Active then begin
    IniNeeded(False);
    try
      WriteInteger(siVersion, FVersion);
      SavePlacement;
      Save;
      FSaved := True;
    finally
      IniFree;
    end;
  end;
end;

procedure TFormPlacement.RestoreFormPlacement;
var
  cActive: TComponent;
begin
  FSaved := False;
  IniNeeded(True);
  try
    if ReadInteger(siVersion, 0) >= FVersion then begin
      RestorePlacement;
      FRestored := True;
      Restore;
      if (fpActiveControl in Options) and (Owner is TCustomForm) then
        begin
        cActive := Form.FindComponent(Inifile.ReadString(IniSection, siActiveCtrl, ''));
        if (cActive <> nil) and (cActive is TWinControl) and
          TWinControl(cActive).CanFocus then
            Form.ActiveControl := TWinControl(cActive);
      end;
    end;
    FRestored := True;
  finally
    IniFree;
  end;
end;

{ TFormStorage }

constructor TFormStorage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStoredProps:=TStringList.Create;
  FStoredValues:=TStoredValues.Create(Self);
  FStoredValues.Storage := Self;
end;

destructor TFormStorage.Destroy;
begin
  FreeAndNil(FStoredValues);
  FreeAndNil(FStoredProps);
  inherited Destroy;
end;

procedure TFormStorage.SetNotification;
var
  I: Integer;
  Component: TComponent;
begin
  for I := FStoredProps.Count - 1 downto 0 do begin
    Component := TComponent(FStoredProps.Objects[I]);
    if Component <> nil then Component.FreeNotification(Self);
  end;
end;

procedure TFormStorage.SetStoredProps(Value: TStrings);
begin
  FStoredProps.Assign(Value);
  SetNotification;
end;

procedure TFormStorage.SetStoredValues(Value: TStoredValues);
begin
  FStoredValues.Assign(Value);
end;
  
function TFormStorage.GetStoredValue(const AName: string): TStoredType;
begin
  Result := StoredValues.StoredValue[AName];
end;
    
procedure TFormStorage.SetStoredValue(const AName: string; Value: TStoredType);
begin
  StoredValues.StoredValue[AName] := Value;
end;
      

procedure TFormStorage.Loaded;
begin
  inherited Loaded;
  UpdateStoredList(Owner, FStoredProps, True);
end;

procedure TFormStorage.WriteState(Writer: TWriter);
begin
  UpdateStoredList(Owner, FStoredProps, False);
  inherited WriteState(Writer);
end;

procedure TFormStorage.Notification(AComponent: TComponent; Operation: TOperation);
var
  I: Integer;
  Component: TComponent;
begin
  inherited Notification(AComponent, Operation);
  if not (csDestroying in ComponentState) and (Operation = opRemove) and
    (FStoredProps <> nil) then
    for I := FStoredProps.Count - 1 downto 0 do begin
      Component := TComponent(FStoredProps.Objects[I]);
      if Component = AComponent then FStoredProps.Delete(I);
    end;
end;

procedure TFormStorage.SaveProperties;
begin
  with TPropsStorage.Create do
  try
    Section := IniSection;
    OnWriteString := @DoWriteString;
    OnEraseSection := @IniFile.EraseSection;
    StoreObjectsProps(Owner, FStoredProps);
  finally
    Free;
  end;
end;

procedure TFormStorage.RestoreProperties;
begin
  with TPropsStorage.Create do
  try
    Section := IniSection;
    OnReadString := @DoReadString;
    try
      LoadObjectsProps(Owner, FStoredProps);
    except
      { ignore any exceptions }
    end;
  finally
    Free;
  end;
end;

procedure TFormStorage.SavePlacement;
begin
  inherited SavePlacement;
  SaveProperties;
{$IFDEF RX_D3}
  StoredValues.SaveValues;
{$ENDIF}
end;

procedure TFormStorage.RestorePlacement;
begin
  inherited RestorePlacement;
  FRestored := True;
  RestoreProperties;
{$IFDEF RX_D3}
  StoredValues.RestoreValues;
{$ENDIF}
end;

{ TIniLink }

destructor TIniLink.Destroy;
begin
  FOnSave := nil;
  FOnLoad := nil;
  SetStorage(nil);
  inherited Destroy;
end;

function TIniLink.GetIniObject: TCustomInifile;
begin
  if Assigned(FStorage) then
    Result := FStorage.IniFile
  else Result := nil;
end;

function TIniLink.GetRootSection: string;
begin
  if Assigned(FStorage) then
     Result := FStorage.FIniSection
  else
    Result := '';
  if Result <> '' then
    Result := Result + '\';
end;

procedure TIniLink.SetStorage(Value: TFormPlacement);
begin
  if FStorage <> Value then
    begin
    if FStorage <> nil then
      FStorage.RemoveLink(Self);
    if Value <> nil then
      Value.AddLink(Self);
  end;
end;

procedure TIniLink.SaveToIni;
begin
  if Assigned(FOnSave) then FOnSave(Self);
end;

procedure TIniLink.LoadFromIni;
begin
  if Assigned(FOnLoad) then FOnLoad(Self);
end;

{ TStoredValue }

constructor TStoredValue.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
{$ifdef storevariant}
  FValue := Unassigned;
{$else}
  FValue:='';
{$endif}
end;

procedure TStoredValue.Assign(Source: TPersistent);
begin
  if (Source is TStoredValue) and (Source <> nil) then
    begin
{$ifdef storevariant}
    if VarIsEmpty(TStoredValue(Source).FValue) then
      Clear
    else
{$endif}
      Value := TStoredValue(Source).FValue;
    Name := TStoredValue(Source).Name;
    KeyString := TStoredValue(Source).KeyString;
    end;
end;

function TStoredValue.GetDisplayName: string;
begin
  if FName = '' then
    Result := inherited GetDisplayName
  else
    Result := FName;
end;

procedure TStoredValue.SetDisplayName(const Value: string);
begin
  if (Value <> '') and (AnsiCompareText(Value, FName) <> 0) and
    (Collection is TStoredValues) and (TStoredValues(Collection).IndexOf(Value) >= 0) then
    raise Exception.Create(SDuplicateString);
  FName := Value;
  inherited;
end;

function TStoredValue.GetStoredValues: TStoredValues;
begin
  if Collection is TStoredValues then
    Result := TStoredValues(Collection)
  else
    Result := nil;
end;

procedure TStoredValue.Clear;
begin
{$ifdef storevariant}
  FValue := Unassigned;
{$else}
  FValue := '';
{$endif}
end;

function TStoredValue.IsValueStored: Boolean;
begin
{$ifdef storevariant}
  Result := not VarIsEmpty(FValue);
{$else}
  Result := (FValue<>'');
{$endif}
end;

procedure TStoredValue.Save;
var
  SaveValue: TStoredType;
  SaveStrValue: string;
begin
  SaveValue := Value;
  if Assigned(FOnSave) then
    FOnSave(Self, SaveValue);
{$ifdef storevariant}
  SaveStrValue := VarToStr(SaveValue);
{$else}
  SaveStrValue := SaveValue;
{$endif}
  if KeyString <> '' then
    SaveStrValue := XorEncode(KeyString, SaveStrValue);
  StoredValues.Storage.WriteString(Name, SaveStrValue);
end;

procedure TStoredValue.Restore;
var
  RestoreValue: TStoredType;
  RestoreStrValue, DefaultStrValue: string;
begin
{$ifdef storevariant}
  DefaultStrValue := VarToStr(Value);
{$else}
  DefaultStrValue := Value;
{$endif}
  if KeyString <> '' then
    DefaultStrValue := XorEncode(KeyString, DefaultStrValue);
  RestoreStrValue := StoredValues.Storage.ReadString(Name, DefaultStrValue);
  if KeyString <> '' then
    RestoreStrValue := XorDecode(KeyString, RestoreStrValue);
  RestoreValue := RestoreStrValue;
  if Assigned(FOnRestore) then
    FOnRestore(Self, RestoreValue);
  Value := RestoreValue;
end;

{ TStoredValues }

constructor TStoredValues.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TStoredValue);
end;

function TStoredValues.IndexOf(const AName: string): Integer;
begin
  for Result := 0 to Count - 1 do
    if AnsiCompareText(Items[Result].Name, AName) = 0 then Exit;
  Result := -1;
end;

function TStoredValues.GetItem(Index: Integer): TStoredValue;
begin
  Result := TStoredValue(inherited Items[Index]);
end;

procedure TStoredValues.SetItem(Index: Integer; StoredValue: TStoredValue);
begin
  inherited SetItem(Index, TCollectionItem(StoredValue));
end;

function TStoredValues.GetStoredValue(const AName: string): TStoredType;
var
  AStoredValue: TStoredValue;
begin
  AStoredValue := GetValue(AName);
  if AStoredValue = nil then
{$ifdef storevariant}
    Result := Null
{$else}
    Result := ''
{$endif}
  else
    Result := AStoredValue.Value;
end;

procedure TStoredValues.SetStoredValue(const AName: string; Value: TStoredType);
var
  AStoredValue: TStoredValue;
begin
  AStoredValue := GetValue(AName);
  if AStoredValue = nil then begin
    AStoredValue := TStoredValue(Add);
    AStoredValue.Name := AName;
    AStoredValue.Value := Value;
  end
  else AStoredValue.Value := Value;
end;

function TStoredValues.GetValue(const AName: string): TStoredValue;
var
  I: Integer;
begin
  I := IndexOf(AName);
  if I < 0 then
    Result := nil
  else
    Result := Items[I];
end;

procedure TStoredValues.SetValue(const AName: string; StoredValue: TStoredValue);
var
  I: Integer;
begin
  I := IndexOf(AName);
  if I >= 0 then
    Items[I].Assign(StoredValue);
end;

procedure TStoredValues.SaveValues;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Save;
end;

procedure TStoredValues.RestoreValues;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Restore;
end;

end.
