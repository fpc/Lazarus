unit CommandLineOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs;

type

  { TOption }

  TOption = class
    Names: array of String;
    Values: array of String;
    Identifier: Integer;
    HasArg: Boolean;
    Present: Boolean;
    Help: String;
    MultipleValues: Boolean;
    function LongestName: String;
    function Value: String;
    procedure AddValue(AValue: String);
  end;
  TCommandLineOptions = class;


  TOptionReadError = procedure(Sender: TObject; ErrorMessage: String) of object;

  { TCommandLineOptions }

  TCommandLineOptions = class
  private
    FOnError: TOptionReadError;
    FOptions: TObjectList;
    FUnassignedArgs: TStringList;
    FStopReading: Boolean;
    function FindOptionByName(AName: String): TOption;
    function FindOptionByIdentifier(AIdentifier: Integer): TOption;
    procedure DoError(ErrorMessage: String); virtual;
  public
    // first setup options
    procedure SetOptions(ShortOptions: String; LongOptions: array of String);
    procedure AddOption(OptionNames: array of String; HasArg: Boolean = False; Help: String = ''; CanUseMultipleTimes: Boolean = False; Identifier: Integer = -1);
    // read from commandline
    procedure ReadOptions;

    // string based
    function HasOption(AName: String): Boolean;
    function OptionValue(AName:String): String;
    function OptionValues(AName: String): TStrings;

    // tag based
    function HasOption(AIdentifier: Integer): Boolean;
    function OptionValue(AIdentifier: Integer): String;
    function OptionValues(AIdentifier: Integer): TStrings;

    constructor Create;
    destructor Destroy; override;
    function PrintHelp(MaxLineWidth: Integer): TStrings; virtual;
    property OnError: TOptionReadError read FOnError write FOnError;
    property OptionsMalformed: Boolean read FStopReading;
  end;
resourcestring
   ErrUnknownOption        = 'Option unknown:  "%s"';
   ErrArgNeededNotPossible = 'Option "%s" requires an argument but an argument is not possible. (Hint: Use "%s" as last option in group "-%s" or use long option --%s)';
   ErrArgumentNeeded       = 'Option "%s" requires an argument';
   ErrOptionHasNoArgument  = 'Option "%s" does not accept arguments';
   ErrOnlyOneInstance      = 'Option "%s" cannot be used more than once';
   ErrNoEqualsAllowed      = 'Symbol "=" not allowed in argument group "-%s"';
implementation

{ TOption }

function TOption.LongestName: String;
var
  N: String;
begin
  Result := '';
  for N in Names do
  begin
    if Length(N) > Length(Result) then
      Result := N;
  end;
end;

function TOption.Value: String;
begin
  if Length(Values) > 0 then
    Exit(Values[0])
  else
    Result := '';
end;

procedure TOption.AddValue(AValue: String);
begin
  SetLength(Values, Length(Values)+1);
  Values[High(Values)] := AValue;
end;

{ TCommandLineOptions }

function TCommandLineOptions.FindOptionByName(AName: String): TOption;
var
  Opt: TOption;
  N: String;
begin
  Result := Nil;
  for Pointer(Opt) in FOptions do
  begin
    for N in Opt.Names do
      if AName = N then
        Exit(Opt)
  end;
end;

function TCommandLineOptions.FindOptionByIdentifier(AIdentifier: Integer
  ): TOption;
begin
  Result := Nil;
end;

procedure TCommandLineOptions.DoError(ErrorMessage: String);
begin
  FStopReading:=True;
  if Assigned(FOnError) then
    FOnError(Self, ErrorMessage)
  else
    WriteLn(ErrorMessage);
end;

procedure TCommandLineOptions.SetOptions(ShortOptions: String;
  LongOptions: array of String);
var
  L: String;
  S: String;
  HasArg: Boolean;
  P,
  E: PChar;
begin
  P:= PChar(ShortOptions);
  E := P + Length(ShortOptions);

  for L in LongOptions do
  begin
    S := P[0];
    if P+1 < E then
      HasArg:=P[1] = ':';
    Inc(P, 1+Ord(HasArg));
    AddOption([S, L], HasArg);
  end;
end;

procedure TCommandLineOptions.AddOption(OptionNames: array of String;
  HasArg: Boolean; Help: String; CanUseMultipleTimes: Boolean; Identifier: Integer);
var
  Opt: TOption;
  C: Integer;
begin
  Opt := TOption.Create;
  C := Length(OptionNames);
  SetLength(Opt.Names, C);
  for C := Low(OptionNames) to High(OptionNames) do
    Opt.Names[C] := OptionNames[C];
  Opt.HasArg:=HasArg;
  Opt.Identifier:=Identifier;
  Opt.MultipleValues:=CanUseMultipleTimes;
  Opt.Help:=Help;
  FOptions.Add(Opt);
end;

procedure TCommandLineOptions.ReadOptions;
var
  OptIndex: Integer;
          procedure ReadOption(S, G: String; OptionPossible: Boolean);
          var
            Opt: TOption;
            Arg: String;
            HasEq: Integer = 0;
          begin
            HasEq := Pos('=', S);
            if HasEq > 0 then
            begin
               Arg := Copy(S, HasEq+1, Length(S));
               S := Copy(S,1, HasEq-1);
            end;

            Opt := FindOptionByName(S);
            if Opt = Nil then
            begin
              DoError(Format(ErrUnknownOption, [S]));
              Exit;
            end;
            if Opt.HasArg and not OptionPossible then
            begin
              DoError(Format(ErrArgNeededNotPossible, [S, S, G, Opt.LongestName]));
              Exit;
            end;

            if Opt.HasArg then
            begin
              if (OptIndex = Paramcount) and (HasEq = 0) then
              begin
                DoError(Format(ErrArgumentNeeded, [S]));
                Exit;
              end;
              if Opt.Present and not Opt.MultipleValues then
              begin
                DoError(Format(ErrOnlyOneInstance, [S]));
                Exit;
              end;

              // Verify???

              if HasEq = 0 then
              begin
                Arg := ParamStr(OptIndex+1);
                Inc(OptIndex);
              end;
              Opt.AddValue(Arg);
            end
            else if HasEq > 0 then
            begin
              DoError(Format(ErrOptionHasNoArgument, [S]));
            end;

            Opt.Present:=True;
          end;

          procedure ReadSingleOptions(S: String);
          var
            I: Integer;
          begin
            if S[1] = '-' then // its a long option with 2 dashes : --option
              ReadOption(Copy(S,2,Length(S)), '', True)
            else // short options put together :  -abcdefg
            begin
              if Pos('=', S) > 0 then
              begin
                DoError(Format(ErrNoEqualsAllowed,[S]));
                Exit;
              end;
              for I := 1 to Length(S) do
                ReadOption(S[I], S, I = Length(S));
            end;
          end;
var
  RawOpt: String;
begin
  OptIndex:=0;
  while OptIndex < Paramcount do
  begin
    if FStopReading then
      Exit;
    Inc(OptIndex);
    RawOpt := ParamStr(OptIndex);
    if (RawOpt[1] = '-') and (RawOpt <> '-') then // '-' is treated as an unassigned arg.
      ReadSingleOptions(Copy(RawOpt,2,Length(RawOpt)))
    else
      FUnassignedArgs.Add(RawOpt);
  end;
end;

function TCommandLineOptions.HasOption(AName: String): Boolean;
var
  Opt: TOption;
begin
  Result := True;
  Opt := FindOptionByName(AName);
  if (Opt = nil) or not(Opt.Present) then
    Result := False;
end;

function TCommandLineOptions.OptionValue(AName: String): String;
var
  Opt: TOption;
begin
  Opt := FindOptionByName(AName);
  Result := Opt.Value;
end;

function TCommandLineOptions.OptionValues(AName: String): TStrings;
var
  Opt: TOption;
  S: String;
begin
  Opt := FindOptionByName(AName);
  Result := TStringList.Create;
  if Opt = nil then
    Exit;
  for S in Opt.Values do
    Result.Add(S);
end;

function TCommandLineOptions.HasOption(AIdentifier: Integer): Boolean;
var
  Opt: TOption;
begin
  Result := False;
  Opt := FindOptionByIdentifier(AIdentifier);
  if Opt = nil then
    Exit;

  Result := Opt.Present;
end;

function TCommandLineOptions.OptionValue(AIdentifier: Integer): String;
var
  Opt: TOption;
begin
  Result := '';
  Opt := FindOptionByIdentifier(AIdentifier);
  if Opt = nil then
    Exit;

  Result := Opt.Value;
end;

function TCommandLineOptions.OptionValues(AIdentifier: Integer): TStrings;
var
  Opt: TOption;
  Tmp: String;
begin
  Result := TStringList.Create;
  Opt := FindOptionByIdentifier(AIdentifier);
  if Opt = nil then
    Exit;

  for Tmp in Opt.Values do
    Result.Add(Tmp);
end;

constructor TCommandLineOptions.Create;
begin
  FOptions := TObjectList.create(True);
  FUnassignedArgs := TStringList.Create;
end;

destructor TCommandLineOptions.Destroy;
begin
  FOptions.Clear;
  FOptions.Free;
  FUnassignedArgs.Free;
  inherited Destroy;
end;

function TCommandLineOptions.PrintHelp(MaxLineWidth: Integer): TStrings;
var
  Padding: array [0..255] of char;
   function Space(Orig: String; LengthNeeded: Integer; Before: Boolean = False): String;
   begin
     if not Before then
       Result := Orig+Copy(Padding,0,LengthNeeded-Length(Orig))
     else
       Result := Copy(Padding,0,LengthNeeded-Length(Orig))+Orig;
   end;

var
  Opt: TOption;
  Tmp: String;
  Line: String;
  LinePart: String;
  I, J: Integer;
  S,L,D: TStringList; // short opt, long opt, description
  SL, LL: String; // short line, long line
  SLL, LLL: Integer; //short line length, long line length
  LineSize: Integer;
  Gap: Integer;
begin
  FillChar(Padding, 256, ' ');
  S := TStringList.Create;
  L := TStringList.Create;
  D := TStringList.Create;
  Result := TStringList.Create;
  for I := 0 to FOptions.Count-1 do
  begin
    SL := '';
    LL := '';
    Line := '';
    Opt := TOption(FOptions.Items[I]);
    for Tmp in Opt.Names do
      if Length(Tmp) = 1 then
        SL := SL + ' -' + Tmp
      else
        LL := LL + ' --' + Tmp;

    S.Add(SL);
    L.Add(LL);
    D.Add(Opt.Help);
  end;
  SLL := 0;
  LLL := 0;

  for Tmp in S do
    if Length(Tmp) > SLL then
      SLL := Length(Tmp);

  for Tmp in L do
    if Length(Tmp) > LLL then
      LLL := Length(Tmp);


  for I := 0 to S.Count-1 do
  begin
    LinePart := '';
    SL := Space(S[I], SLL);
    LL := Space(L[I], LLL);
    Line := SL + ' ' + LL + '    '+ D[I];
    if Length(Line) > MaxLineWidth then
    begin
      LineSize:=MaxLineWidth;
      Gap := 0;
      repeat
        J := LineSize;
        //if J > Length(Line) then J := Length(Line);
        while (J > 0){ and (Length(Line) > 0)} do
        begin
          if (Line[J] = ' ') or (J = 1) then
          begin
            LinePart := Copy(Line, 1, J);
            LinePart := Space(LinePart, Length(LinePart)+Gap, True);
            Delete(Line,1,J);
            Result.Add(LinePart);
            break;
          end;
          Dec(J);
        end;
        Gap := SLL+1+LLL+4;
        LineSize := MaxLineWidth-(Gap);
      until Length(Line) = 0;
    end
    else
      Result.Add(Line);
  end;
  S.Free;
  L.Free;
  D.Free;

end;

end.

