unit ObjCTemplate;

interface

{$ifdef fpc}{$mode delphi}{$h+}{$endif}

uses
  Classes, SysUtils, ObjCParserTypes, ObjCParserUtils;

type
  TTemplateList = class(TObject)
  public
    Params    : TStringList;
    SubLists  : TList;
    Owner     : TTemplateList;
    Name      : AnsiString;
    constructor Create(AOwner: TTemplateList);
    destructor Destroy; override;
  end;

  TTemplateValues = class(TObject)
  public
    procedure ProcSpecial(const Special: AnsiString; var ReplaceText: AnsiString); virtual;
  end;

  TTemplateProc = class(TObject)
  private
    fTemplate : AnsiString;

    fRoot     : TTemplateList;
    fStack    : TList;

    fValues   : TTemplateValues;
    fText     : AnsiString;

  protected
    function CurrentList: TTEmplateList;

    function GetReplace(const tmp: AnsiString; var AtIndex: Integer): AnsiString;
    function ParseNextProc(var Proc: AnsiString; var idx: Integer): AnsiString;

    procedure DoParse(var idx: Integer; const EndTmp: AnsiString);
  public
    function Parse(const Template: AnsiString; RootList: TTemplateList; AValues: TTemplateValues): AnsiString;
  end;


  TPascalValues = class(TTemplateValues)
  private
    fEndsCount  : Integer;

    ClassEnd     : Boolean;
    ClassSection : AnsiString;
  public
    procedure ProcSpecial(const Special: AnsiString; var ReplaceText: AnsiString); override;
  end;


implementation

const
  PreProcChar : AnsiChar = '%';
  PreProcMark : TCharSet = ['%'];

function GetTmpParam(const temp: AnsiString; var i: Integer): AnsiString;
begin
  ScanWhile(temp, i, [#13, #10, #32, #9]);
  Result := ScanTo(temp, i, [#32, #9, #13, #10]);
end;

{ TTemplate }

function TTemplateProc.CurrentList: TTemplateList;
begin
  Result := TTemplateList(fStack[fStack.Count-1])
end;

procedure TTemplateProc.DoParse(var idx: Integer; const EndTmp: AnsiString);
var
  prc     : AnsiString;
  isValue : Boolean;
  t       : AnsiString;
begin
  while idx <= length(fTemplate) do begin
    fText := fText + ParseNextProc(prc, idx);
    isValue := false;
    if (EndTmp <> '') and (prc = EndTmp) then Exit;
    if prc = '' then Continue;

    if prc[1] = '_' then begin
      prc := Copy(prc, 2, length(prc)-1);
      isValue := true;
    end;
    if prc = '' then Continue;

    if isValue then begin
      if Assigned(fValues) then begin
        fValues.ProcSpecial(AnsiLowerCase(prc), t);
        fText := fText + t
      end;
    end else begin
      //inc(idx);
      fText := fText + GetReplace(AnsiLowerCase(prc), idx);
    end;
  end;
end;

function TTemplateProc.GetReplace(const tmp: AnsiString; var AtIndex: Integer): AnsiString;
var
  i   : Integer;
  nm  : AnsiString;
  j   : Integer;
  l   : TTemplateList;
  ExitIndex:Integer;
  idx : Integer;
begin
  Result := '';
  i := 1;
  if AnsiLowerCase(GetTmpParam(tmp, i)) = 'foreach' then begin
    nm := GetTmpParam(tmp, i);
    ExitIndex := AtIndex;
    for j := 0 to CurrentList.SubLists.Count - 1 do begin
      l := TTemplateList(CurrentList.SubLists[j]);
      idx := AtIndex;
      if l.Name = nm then begin
        fStack.Add(l);
        DoParse(idx, 'end');
        fStack.Delete(fStack.Count-1);
        ExitIndex := idx;
      end;
    end;
    AtIndex := ExitIndex;
  end else begin
    Result := CurrentList.Params.Values[tmp];
  end;

end;

function TTemplateProc.Parse(const Template: String;
  RootList: TTemplateList; AValues: TTemplateValues): AnsiString;
var
  i : integer;
begin
  fTemplate := Template;
  fRoot := RootList;
  fValues := AValues;

  fStack := TList.Create;
  try
    i := 1;
    fStack.Add(RootList);
    DoParse(i, '');
  finally
    fStack.Free;
  end;

  Result := fText;
end;

function TTemplateProc.ParseNextProc(var Proc: string; var idx: Integer): AnsiString;
var
  canQuit : Boolean;
begin
  canQuit := false; // just don't like: repeat until false;
  Result := '';
  repeat
    Result := Result + ScanTo(fTemplate, idx, PreProcMark);

    if idx > length(fTemplate) then begin
      Proc := '';
      canQuit := true;
    end else begin
      if (idx < length(fTemplate)) and (fTemplate[idx+1] = PreProcChar) then begin
        Result := Result + PreProcChar;
        inc(idx,2);
      end else begin
        inc(idx);
        Proc := ScanTo(fTemplate, idx, PreProcMark);
        CanQuit := true;
        inc(idx);
      end;
    end;
  until canQuit;

end;

{ TTemplateList }

constructor TTemplateList.Create(AOwner: TTemplateList);
begin
  Owner := AOwner;
  SubLists := TList.Create;
  Params := TStringList.Create;
end;

destructor TTemplateList.Destroy;
begin
  SubLists.Free;
  Params.Free;
  inherited;
end;

{ TPascalValues }

procedure TPascalValues.ProcSpecial(const Special: AnsiString;
  var ReplaceText: AnsiString);
begin
  ReplaceText := '';

  if (Special = 'pasprivate') or (Special = 'pasprotected') or (Special = 'paspublic') then begin

    if ClassSection = '' then begin
      ClassEnd := true;
      inc(fEndsCount);
    end;

    if ClassSection <> Special then
      ReplaceText := Copy(Special, 4, length(Special) - 3); //removed 'pas' prefix
    ClassSection := Special;

  end else if (Special = 'pasend') or (Special = 'pasend.') or (Special = 'pasend;')then begin
    if fEndsCount > 0 then begin
      ReplaceText := Copy(Special, 4, length(Special) - 3); //removed 'pas' prefix
      dec(fEndsCount);       
      if classEnd then begin
        classEnd := false;
        ClassSection := '';
      end;
    end;
  end;
  
end;

{ TTemplateValues }

procedure TTemplateValues.ProcSpecial(const Special: AnsiString;
  var ReplaceText: AnsiString);
begin
  ReplaceText := '';
end;

end.
