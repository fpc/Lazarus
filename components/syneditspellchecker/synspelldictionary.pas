{
 *****************************************************************************
  This file is part of the SynEditSpellChecker package from the Lazarus IDE.

  This content of this file is licensed: Modified LGPL-2
  Or at the users choice: Modified LGPL-3
  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.

  Alternatively, the contents of this file may be used under the terms of the
  Mozilla Public License Version 1.1 or 2.0 http://www.mozilla.org/MPL/

  A copy used under either License can have the other Licenses removed from this
  header. A note should be added that the original file is available with the
  above choice of License.
  Used units may have to be amended according to the choice.
 *****************************************************************************
}
unit SynSpellDictionary;

{$mode objfpc}{$H+}

interface

uses
  SynASpellDef, LazClasses, LCLProc, SysUtils, Classes;

type
  TStringArray = array of String;

  TSuggestionCheckProc = function(const ASuggestion: String): boolean of object;

  TSynSpellDictionaryFeature = (
    ssfSuggest,
    ssfPersonal
  );
  TSynSpellDictionaryFeatures = set of TSynSpellDictionaryFeature;

  { TSynSpellDictionary }

  TSynSpellDictionary = class(specialize TRefCountedGeneric<TPersistent>)
  strict private
    FOnChanged: TNotifyEvent;
    FChangedHandlers: TMethodList;
    FUpdateLock: integer;
    FUpdChanged: boolean;
    function GetIsUpdating: boolean;
  protected
    procedure DoChanged;
    function GetIsLoaded: boolean; virtual; abstract;
    function GetLastError: String; virtual; abstract;
    procedure AssignFrom(AnOther: TSynSpellDictionary); virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(AnOther: TSynSpellDictionary); reintroduce;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure RegisterChangedHandler(AHandler: TNotifyEvent);
    procedure UnregisterChangedHandler(AHandler: TNotifyEvent);

    procedure Load; virtual; abstract;
    procedure UnLoad; virtual; abstract;

    function  CheckWord(AWord: PChar; AWordLen: integer): boolean; virtual; abstract;
    function  GetSuggestions(AWord: PChar; AWordLen: integer; AMaxSuggestions: Integer = MaxInt; ACheckProc: TSuggestionCheckProc = nil): TStringArray; virtual; abstract;

    procedure AddToPersonal(AWord: String); overload;
    procedure AddToPersonal(AWord: PChar; AWordLen: integer); virtual; overload;
    procedure SavePersonal; virtual;

    function  Features: TSynSpellDictionaryFeatures; virtual;
    property  IsLoaded: boolean read GetIsLoaded;
    property  LastError: String read GetLastError;
    property IsUpdating: boolean read GetIsUpdating;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

  { TSynSpellDictionaryASpell }

  TSynSpellDictionaryASpell = class(TSynSpellDictionary)
  private
    FASpeller: PAspellSpeller;
    FDictionaryDir: string;
    FLanguage: string;
    FLastError: String;
    FLibraryName: string;
    FPersonalWordsFile: string;
    procedure SetDictionaryDir(AValue: string);
    procedure SetLanguage(AValue: string);
    procedure SetLibraryName(AValue: string);
    procedure SetPersonalWordsFile(AValue: string);
  protected
    function GetIsLoaded: boolean; override;
    function GetLastError: String; override;

    procedure CreateSpeller;
    procedure FreeSpeller;
    procedure AssignFrom(AnOther: TSynSpellDictionary); override;
  public
    //constructor Create;
    destructor Destroy; override;
    procedure Load; override;
    procedure UnLoad; override;
    function  CheckWord(AWord: PChar; AWordLen: integer): boolean; override;
    function  GetSuggestions(AWord: PChar; AWordLen: integer; AMaxSuggestions: Integer = MaxInt; ACheckProc: TSuggestionCheckProc = nil): TStringArray; override;
    procedure AddToPersonal(AWord: PChar; AWordLen: integer); override;
    procedure SavePersonal; override;
    function Features: TSynSpellDictionaryFeatures; override;
  published
    property LibraryName: string read FLibraryName write SetLibraryName;
    property DictionaryDir: string read FDictionaryDir write SetDictionaryDir;
    property Language: string read FLanguage write SetLanguage;
    property PersonalWordsFile: string read FPersonalWordsFile write SetPersonalWordsFile;
  end;

  { TSynSpellDictionaryWordList }

  TSynSpellDictionaryWordList = class(TSynSpellDictionary)
  private
    FList: TStringList;
    function GetList: TStrings;
  protected
    function GetIsLoaded: boolean; override;
    function GetLastError: String; override;
    procedure AssignFrom(AnOther: TSynSpellDictionary); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load; override;
    procedure UnLoad; override;
    function CheckWord(AWord: PChar; AWordLen: integer): boolean; override;
    function GetSuggestions(AWord: PChar; AWordLen: integer; AMaxSuggestions: Integer = MaxInt;
      ACheckProc: TSuggestionCheckProc = nil): TStringArray; override;
    function Features: TSynSpellDictionaryFeatures; override;
    property Words: TStrings read GetList;
  end;

implementation

{ TSynSpellDictionary }

function TSynSpellDictionary.GetIsUpdating: boolean;
begin
  Result := FUpdateLock > 0;
end;

procedure TSynSpellDictionary.DoChanged;
begin
  if IsUpdating then begin
    FUpdChanged := True;
    exit;
  end;
  FUpdChanged := False;

  if FOnChanged <> nil then
    FOnChanged(Self);
  FChangedHandlers.CallNotifyEvents(Self);
end;

constructor TSynSpellDictionary.Create;
begin
  FChangedHandlers := TMethodList.Create;
  inherited Create;
  AddReference;
end;

destructor TSynSpellDictionary.Destroy;
begin
  DoDestroy;
  inherited Destroy;
  FChangedHandlers.Free;
end;

procedure TSynSpellDictionary.Assign(AnOther: TSynSpellDictionary);
begin
  AssignFrom(AnOther);
  DoChanged;
end;

procedure TSynSpellDictionary.BeginUpdate;
begin
  inc(FUpdateLock);
end;

procedure TSynSpellDictionary.EndUpdate;
begin
  if FUpdateLock > 0 then
    dec(FUpdateLock);
  if (FUpdateLock = 0) and FUpdChanged then
    DoChanged;
end;

procedure TSynSpellDictionary.RegisterChangedHandler(AHandler: TNotifyEvent);
begin
  FChangedHandlers.Add(TMethod(AHandler));
end;

procedure TSynSpellDictionary.UnregisterChangedHandler(AHandler: TNotifyEvent);
begin
  FChangedHandlers.Remove(TMethod(AHandler));
end;

procedure TSynSpellDictionary.AddToPersonal(AWord: String);
begin
  AddToPersonal(PChar(AWord), Length(AWord));
end;

procedure TSynSpellDictionary.AddToPersonal(AWord: PChar; AWordLen: integer);
begin
  //
end;

procedure TSynSpellDictionary.SavePersonal;
begin
  //
end;

function TSynSpellDictionary.Features: TSynSpellDictionaryFeatures;
begin
  Result := [];
end;

{ TSynSpellDictionaryASpell }

procedure TSynSpellDictionaryASpell.SetLanguage(AValue: string);
begin
  if FLanguage = AValue then Exit;
  FLanguage := AValue;
  FreeSpeller;
  DoChanged;
end;

procedure TSynSpellDictionaryASpell.SetDictionaryDir(AValue: string);
begin
  if FDictionaryDir = AValue then Exit;
  FDictionaryDir := AValue;
  if not IsLoaded then ;
  FreeSpeller;
  DoChanged;
end;

procedure TSynSpellDictionaryASpell.SetLibraryName(AValue: string);
begin
  if FLibraryName = AValue then Exit;
  FLibraryName := AValue;
  FreeSpeller;
  DoChanged;
end;

procedure TSynSpellDictionaryASpell.SetPersonalWordsFile(AValue: string);
begin
  if FPersonalWordsFile = AValue then Exit;
  FPersonalWordsFile := AValue;
  FreeSpeller;
  DoChanged;
end;

function TSynSpellDictionaryASpell.GetIsLoaded: boolean;
begin
  Result := FASpeller <> nil;
end;

function TSynSpellDictionaryASpell.GetLastError: String;
begin
  Result := FLastError;
end;

procedure TSynSpellDictionaryASpell.CreateSpeller;
var
  Conf: PAspellConfig;
  ErrSpeller: Paspellcanhaveerror;
begin
  FreeSpeller;
  FLastError := '';
  //if not aspell_loaded then
  //  aspell_init('D:\__ASPELL\Aspell\bin\msys-aspell-15.dll');

  if not aspell_loaded then begin
    if (FLibraryName <> '') then
      aspell_init(FLibraryName)
    else
      aspell_init(libaspell);
    if not aspell_loaded then begin
      FLastError := 'Unable to load library: ' + IntToStr(GetLastOSError);
      exit;
    end;
  end;

  Conf := new_aspell_config();
  aspell_config_replace(Conf, 'encoding', 'utf-8');
  //aspell_config_replace(Conf, 'mode', '');
  if FDictionaryDir <> '' then
    aspell_config_replace(Conf, 'dict-dir', PChar(FDictionaryDir));
  if FLanguage <> '' then
    aspell_config_replace(Conf, 'lang', PChar(FLanguage));
  if FPersonalWordsFile <> '' then
    aspell_config_replace(Conf, 'personal', PChar(FPersonalWordsFile));

  aspell_config_replace(Conf, 'ignore-case', PChar('true'));

  ErrSpeller := new_aspell_speller(Conf);
  delete_aspell_config(Conf);

  if aspell_error_number(ErrSpeller) <> 0 then begin
    FLastError :=  aspell_error_message(ErrSpeller);
    if FLastError = '' then FLastError := 'Unknown error setting config';
    delete_aspell_can_have_error(ErrSpeller);
    exit;
  end;

  FASpeller := to_aspell_speller(ErrSpeller);
end;

procedure TSynSpellDictionaryASpell.FreeSpeller;
begin
  if (FASpeller <> nil) and aspell_loaded then
    delete_aspell_speller(FASpeller);
  FASpeller := nil;
  FLastError := '';
end;

procedure TSynSpellDictionaryASpell.AssignFrom(AnOther: TSynSpellDictionary);
var
  TheOther: TSynSpellDictionaryASpell absolute AnOther;
begin
  FreeSpeller;
  if not (AnOther is TSynSpellDictionaryASpell) then
    exit;
  FLibraryName       := TheOther.FLibraryName;
  FDictionaryDir     := TheOther.FDictionaryDir;
  FLanguage          := TheOther.FLanguage;
  FPersonalWordsFile := TheOther.FPersonalWordsFile;
end;

destructor TSynSpellDictionaryASpell.Destroy;
begin
  FreeSpeller;
  inherited Destroy;
end;

procedure TSynSpellDictionaryASpell.Load;
begin
  if FASpeller = nil then
    CreateSpeller;
end;

procedure TSynSpellDictionaryASpell.UnLoad;
begin
  FreeSpeller;
end;

function TSynSpellDictionaryASpell.CheckWord(AWord: PChar; AWordLen: integer): boolean;
begin
  Result := FASpeller = nil;
  if Result then exit;

  Result := aspell_speller_check(FASpeller, AWord, AWordLen) <> 0;
end;

function TSynSpellDictionaryASpell.GetSuggestions(AWord: PChar; AWordLen: integer;
  AMaxSuggestions: Integer; ACheckProc: TSuggestionCheckProc): TStringArray;
var
  WList: Paspellwordlist;
  Enum: Paspellstringenumeration;
  wrd: pChar;
  s: string;
  i: Integer = 0;
begin
  Result := nil;
  SetLength(Result, 30);
  WList := aspell_speller_suggest(FASpeller, AWord, AWordLen);
  Enum := aspell_word_list_elements(WList);
  i := 0;
  wrd := aspell_string_enumeration_next(Enum);
  while wrd <> nil do begin
    s := wrd;
    if (ACheckProc = nil) or ACheckProc(s) then begin
      if i >= Length(Result) then
        SetLength(Result, i + 10);
      Result[i] := s;
      inc(i);
      if i >= AMaxSuggestions then
        break;
    end;
    wrd := aspell_string_enumeration_next(Enum);
  end;
  delete_aspell_string_enumeration(Enum);
  SetLength(Result, i);
end;

procedure TSynSpellDictionaryASpell.AddToPersonal(AWord: PChar; AWordLen: integer);
begin
  if FASpeller = nil then exit;
  aspell_speller_add_to_personal(FASpeller, AWord, AWordLen);
  DoChanged;
end;

procedure TSynSpellDictionaryASpell.SavePersonal;
begin
  if FASpeller = nil then exit;
  aspell_speller_save_all_word_lists(FASpeller);
end;

function TSynSpellDictionaryASpell.Features: TSynSpellDictionaryFeatures;
begin
  Result := [ssfSuggest];
  if FPersonalWordsFile <> '' then
    Result := Result + [ssfPersonal];
end;

{ TSynSpellDictionaryWordList }

function TSynSpellDictionaryWordList.GetList: TStrings;
begin
  Result := FList;
end;

function TSynSpellDictionaryWordList.GetIsLoaded: boolean;
begin
  Result := True;
end;

function TSynSpellDictionaryWordList.GetLastError: String;
begin
  Result := '';
end;

constructor TSynSpellDictionaryWordList.Create;
begin
  inherited Create;
  FList := TStringList.Create;
  FList.CaseSensitive := False;
end;

procedure TSynSpellDictionaryWordList.AssignFrom(AnOther: TSynSpellDictionary);
var
  TheOther: TSynSpellDictionaryWordList absolute AnOther;
begin
  if not (AnOther is TSynSpellDictionaryWordList) then
    exit;
  FList.Assign(TheOther.FList);
end;

destructor TSynSpellDictionaryWordList.Destroy;
begin
  inherited Destroy;
  FList.Free;
end;

procedure TSynSpellDictionaryWordList.Load;
begin
  //
end;

procedure TSynSpellDictionaryWordList.UnLoad;
begin
  //
end;

function TSynSpellDictionaryWordList.Features: TSynSpellDictionaryFeatures;
begin
  Result := [];
end;

function TSynSpellDictionaryWordList.CheckWord(AWord: PChar; AWordLen: integer): boolean;
var
  s: string;
begin
  SetString(s, AWord, AWordLen);
  Result := FList.IndexOf(s) >= 0;
end;

function TSynSpellDictionaryWordList.GetSuggestions(AWord: PChar; AWordLen: integer;
  AMaxSuggestions: Integer; ACheckProc: TSuggestionCheckProc): TStringArray;
begin
  Result := nil;
end;

end.

