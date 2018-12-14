{%MainUnit ../osprinters.pas}
{$IFDEF DebugCUPS}
{$DEFINE LogPrintoutFile}
{$ENDIF}

{**************************************************************
Implementation for cupsprinter
***************************************************************}
uses
  {%H-}udlgSelectPrinter, // used to compile it on this target
  {%H-}udlgpropertiesprinter, // used to compile it on this target
  FileUtil, LazFileUtils;

//Return always 72 because, PostScript it's 72 only
function TCUPSPrinter.GetXDPI: Integer;
begin
  Result:=InternalGetResolution(True);
end;

//Return always 72 because, PostScript it's 72 only
function TCUPSPrinter.GetYDPI: Integer;
begin
  Result:=InternalGetResolution(False);
end;

procedure TCUPSPrinter.DoEnumBins(Lst: TStrings);
var
  choice: Pppd_choice_t;
  Option: Pppd_option_t;
  c: Integer;
begin
  Lst.Clear;
  if CupsPPD<>nil then
  begin
    Option := ppdFindOption(CupsPPD, PChar('InputSlot'));
    if Option<>nil then
    begin
      Choice := Option^.choices;
      c := 0;
      while (Choice<>nil) and (c<Option^.num_choices) do
      begin
        lst.AddObject(Choice^.text, TObject(Choice));
        inc(choice);
        inc(c);
      end;
    end;
  end;
end;

function TCUPSPrinter.DoGetDefaultBinName: string;
var
  Option: Pppd_option_t;
  Choice: pppd_choice_t;
begin
  Result:=inherited DoGetDefaultBinName;

  if CupsPPD<>nil then
  begin
    Option := ppdFindOption(CupsPPD, 'InputSlot');
    if Option<>nil then
    begin
      choice := PPDOptionChoiceFrom('InputSlot', Option^.defchoice, true);
      if choice<>nil then
        result := choice^.text;
    end;
  end;
end;

function TCUPSPrinter.DoGetBinName: string;
var
  Choice: pppd_choice_t;
begin
  result := cupsGetOption('InputSlot');
  if result<>'' then
  begin
    Choice := PPDOptionChoiceFrom('InputSlot', result, true);
    if Choice<>nil then
      result := Choice^.text
    else
      result := '';
  end;

  if result='' then
    result := doGetDefaultBinName
end;

procedure TCUPSPrinter.DoSetBinName(aName: string);
var
  Choice: pppd_choice_t;
begin
  Choice := PPDOptionChoiceFrom('InputSlot', aName, false);
  if Choice<>nil then
    cupsAddOption('InputSlot', choice^.choice)
  else
    inherited doSetBinName(aName); // handle input slot not found
end;

//write count bytes from buffer to raw mode stream
function TCUPSPrinter.Write(const Buffer; Count: Integer; out Written: Integer
  ): Boolean;
begin
  result := False;
  CheckRawMode(True);
  if not Assigned(FRawModeStream) then
    FRawModeStream := TMemoryStream.Create;
  Written := FRawModeStream.Write(Buffer, Count);
  Result := True;
end;

constructor TCUPSPrinter.Create;
begin
  inherited Create;

  fcupsPrinters:=nil;
  fcupsPrinter :=nil;
  fcupsHttp    :=nil;
  fcupsPPD     :=nil;
  fcupsOptions :=nil;
  fcupsNumOpts :=0;
  
  FRawModeStream   := nil;
  FCupsPapersCount := -1;
end;

procedure TCUPSPrinter.DoDestroy;
begin
  if assigned(fRawModeStream) then
    fRawModeStream.Free;

  FreeOptions;

  if Assigned(fcupsHttp) then
    httpClose(fcupsHttp);

  inherited DoDestroy;
end;

procedure TCUPSPrinter.FreeOptions;
begin
  if Assigned(fcupsOptions) then
    cupsFreeOptions(fcupsNumOpts,fcupsOptions);

  fcupsNumOpts:=0;
  fcupsOptions:=nil;
  FStates := [];
end;

procedure TCUPSPrinter.cupsAddOption(aName,aValue: string);
begin
  if not CUPSLibInstalled then Exit;
  fcupsNumOpts:=cupsdyn.cupsAddOption(PChar(aName),PChar(aValue),fcupsNumOpts,
                                                                @fcupsOptions);
  if (AName='PageSize') then
  begin
    Exclude(FStates,cpsPaperNameValid);
    Exclude(FStates,cpsPaperRectValid);
  end;

  {$IFDEF DebugCUPS}
  DebugLn('TCUPSPrinter.cupsAddOption AName=%s AValue=%s',[AName,AValue]);
  {$ENDIF}
end;

//Return the value of option set for the selected printer
function TCUPSPrinter.cupsGetOption(aKeyWord: string): String;
begin
  Result:='';
  if not CUPSLibInstalled then Exit;
  if (Printers.Count>0) then
  begin
    if not Assigned(fcupsOptions) then
         SetOptionsOfPrinter;

    Result:=cupsdyn.cupsGetOption(PChar(aKeyWord),fcupsNumOpts,fcupsOptions);
  end;
end;

function TCUPSPrinter.CopyOptions(out AOptions: Pcups_option_t): Integer;
var
  i: Integer;
begin
  AOptions := nil;
  Result := 0;
  for i:=0 to fcupsNumOpts-1 do
    Result := cupsdyn.cupsAddOption(fcupsOptions[i].name,fcupsOptions[i].value,
               Result,@AOptions);
end;

procedure TCUPSPrinter.MergeOptions(const AOptions:Pcups_option_t; const n:Integer);
var
  i: Integer;
begin
  for i:=0 to n-1 do
    if
      // always merge some known options
      (strcomp('job-sheets', AOptions[i].name)=0) or
      // check if ppd option value is valid
      IsOptionValueValid(AOptions[i].name, AOptions[i].value)
    then
      cupsAddOption(AOptions[i].name, AOptions[i].value);
  cupsFreeOptions(n, AOptions);
end;

function TCUPSPrinter.GetResolutionOption: string;
var
  L1,L2: TStringlist;
  i: Integer;
begin
  Result := Self.cupsGetOption('Resolution');
  if Result='' then
  begin
    // get resolution from ppd
    Result := GetPPDAttribute('DefaultResolution');
    if Result='' then
    begin
      // try grouped options
      L1 := TStringList.Create;
      L2 := TStringList.Create;
      try
        i := EnumPPDChoice(L1,'Resolution',L2);
        if i>=0 then
          Result := L2[i]
      finally
        L2.Free;
        L1.Free;
      end;
    end;
  end;
end;

procedure TCUPSPrinter.DebugOptions(AOPtions:Pcups_option_t=nil; n:Integer=0);
var
  i: Integer;
begin
  if (Printers.Count>0) and CUPSLibInstalled and (fcupsPrinter<>nil) then
  begin
    DebugLn('**************************************************');
    if AOptions=nil then
    begin
      AOptions:= fcupsOptions;
      n := fcupsNumOpts;
    end;
    DebugLn('Printer "%s" Number of Options %d',[fcupsPrinter^.Name,n]);
    for i:=0 to n-1 do
      DebugLn('name="%s" value="%s"',[AOptions[i].name,AOptions[i].value]);
    DebugLn('**************************************************');
  end else
    DebugLn('DebugOptions: There are no valid printers');
end;

procedure TCUPSPrinter.DoCupsConnect;
begin
  if not assigned(fcupsHttp) then
  begin
    if not CUPSLibInstalled then Exit;
    fcupsHttp:=httpConnect(cupsServer(),ippPort());
    if not Assigned(fcupsHttp) then
       raise Exception.Create('Unable to contact server: '+GetLastError);
  end;
end;

function TCUPSPrinter.CupsPapersListValid: boolean;
var
  Lst: TStringlist;
begin
  if fCupsPapersCount<=0 then begin
    // paper list no exists or
    // paper list is not enumerated yet, try it now.
    Lst := TStringlist.Create;
    try
      DoEnumPapers(Lst);
    finally
      Lst.Free;
    end;
  end;
  result := fCupsPapersCount>0;
end;

function TCUPSPrinter.InternalGetResolution(ForX: boolean): Integer;

  procedure ParseResolution(s:string);
  var
    a,b: Integer;
  begin
    if s<>'' then begin
      s := uppercase(s);
      a := pos('X', S);
      b := pos('D', S);
      if b=0 then
        b := Length(S)
      else
        dec(b);
      if a>0 then begin
        // NNNXMMMDPI (or NNN X MMM DPI)
        FCachedResolution.x := StrToIntDef(trim(copy(S,1,a-1)), 0);
        FCAchedResolution.y := StrToIntDef(trim(copy(S,a+1,b)), 0);
      end else begin
        // NNNDPI (or NNN DPI);
        FCachedResolution.x := StrToIntDef(trim(copy(S,1,b)), 0);
        FCachedResolution.y := FCachedResolution.x;
      end;
    end;
  end;

begin
  if not (cpsResolutionValid in FStates) then begin
    // check user defined resolution
    FCachedResolution.x := 0;
    FCachedResolution.y := 0;

    ParseResolution(GetResolutionOption);

    if (FCachedResolution.x=0) or (FCachedResolution.y=0) then
    begin
      FCachedResolution.x := 300;
      FCachedResolution.y := 300;
    end;

    include(FStates, cpsResolutionValid);
  end;
  if ForX then
    result := FCachedResolution.X
  else
    result := FCachedResolution.Y;
end;

{$IFDEF DebugCUPS}
procedure TCUPSPrinter.DebugCapabilities;
var
  flags: Integer;

  procedure DumpCap(const aFlag: integer; const flagMsg, Desc: string; invert: boolean=false);
  begin
    if (invert and (aFlag and Flags=0)) or (not invert and (aFlag and Flags<>0)) then
      DebugLn(flagMsg, '(',Desc,')');
  end;
begin
  flags := GetAttributeInteger('printer-type',CUPS_PRINTER_LOCAL);
  DebugLn('=== CAPABILITIES ===');
  DebugLn;
  DumpCap(CUPS_PRINTER_CLASS or CUPS_PRINTER_REMOTE, 'CUPS_PRINTER_LOCAL    ', 'Local printer or class    ', true);
  DumpCap(CUPS_PRINTER_CLASS     , 'CUPS_PRINTER_CLASS    ', 'Printer class             ');
  DumpCap(CUPS_PRINTER_REMOTE    , 'CUPS_PRINTER_REMOTE   ', 'Remote printer or class   ');
  DumpCap(CUPS_PRINTER_BW        , 'CUPS_PRINTER_BW       ', 'Can do B&W printing       ');
  DumpCap(CUPS_PRINTER_COLOR     , 'CUPS_PRINTER_COLOR    ', 'Can do color printing     ');
  DumpCap(CUPS_PRINTER_DUPLEX    , 'CUPS_PRINTER_DUPLEX   ', 'Can do duplexing          ');
  DumpCap(CUPS_PRINTER_STAPLE    , 'CUPS_PRINTER_STAPLE   ', 'Can staple output         ');
  DumpCap(CUPS_PRINTER_COPIES    , 'CUPS_PRINTER_COPIES   ', 'Can do copies             ');
  DumpCap(CUPS_PRINTER_COLLATE   , 'CUPS_PRINTER_COLLATE  ', 'Can collage copies        ');
  DumpCap(CUPS_PRINTER_PUNCH     , 'CUPS_PRINTER_PUNCH    ', 'Can punch output          ');
  DumpCap(CUPS_PRINTER_COVER     , 'CUPS_PRINTER_COVER    ', 'Can cover output          ');
  DumpCap(CUPS_PRINTER_BIND      , 'CUPS_PRINTER_BIND     ', 'Can bind output           ');
  DumpCap(CUPS_PRINTER_SORT      , 'CUPS_PRINTER_SORT     ', 'Can sort output           ');
  DumpCap(CUPS_PRINTER_SMALL     , 'CUPS_PRINTER_SMALL    ', 'Can do Letter/Legal/A4    ');
  DumpCap(CUPS_PRINTER_MEDIUM    , 'CUPS_PRINTER_MEDIUM   ', 'Can do Tabloid/B/C/A3/A2  ');
  DumpCap(CUPS_PRINTER_LARGE     , 'CUPS_PRINTER_LARGE    ', 'Can do D/E/A1/A0          ');
  DumpCap(CUPS_PRINTER_VARIABLE  , 'CUPS_PRINTER_VARIABLE ', 'Can do variable sizes     ');
  DumpCap(CUPS_PRINTER_IMPLICIT  , 'CUPS_PRINTER_IMPLICIT ', 'Implicit class            ');
  DumpCap(CUPS_PRINTER_DEFAULT   , 'CUPS_PRINTER_DEFAULT  ', 'Default printer on network');
end;

procedure TCUPSPrinter.DebugPPD;
const
  arruitypes:array[ppd_ui_t] of string[9] = ('boolean','pickone','pickmany');
  arrsection:array[ppd_section_t] of string[9] = ('any','document','exit','jcl','page','prolog');
var
  i,j,k: Integer;
  AttrRoot  : Ppppd_attr_t;
  Attr      : Pppd_attr_t;
  Group     : pppd_group_t;
  Option    : Pppd_option_t;
  choices   : Pppd_choice_t;

  function markchar(const AMark:char):char;
  begin
    if AMark=#1 then
      result := '*'
    else
      result := ' ';
  end;
begin
  DebugLn;
  DebugLn('DebugPPD: ppdfile=',fCupsPPDName);
  if fcupsPPD=nil then
  begin
    DebugLn('No valid ppd file found');
    exit;
  end;

  DebugLn('=== HEADER ===');
  DebugLn;
  DebugLn('  model          : %s', [fcupsPPD^.modelname]);
  DebugLn('  modelNumber    : %d', [fcupsPPD^.model_number]);
  DebugLn('  manufacturer   : %s', [fcupsPPD^.manufacturer]);
  DebugLn('  nickname       : %s', [fcupsPPD^.nickname]);
  DebugLn('  shortnickname  : %s', [fcupsPPD^.shortnickname]);
  DebugLn('  product        : %s', [fcupsPPD^.product]);
  DebugLn('  attributes     : %d Current=%d', [fcupsPPD^.num_attrs,fcupsPPD^.cur_attr]);
  DebugLn('  language_level : %d', [fcupsPPD^.language_level]);
  DebugLn('  lang_version   : %s', [fcupsPPD^.lang_version]);
  DebugLn('  lang_encoding  : %s', [fcupsPPD^.lang_encoding]);
  DebugLn('  landscape      : %d', [fcupsPPD^.landscape]);
  DebugLn('  UI groups      : %d', [fcupsPPD^.num_groups]);
  DebugLn('  Num Papers     : %d', [fcupsPPD^.num_sizes]);
  DebugLn('  Num Attributes : %d', [fcupsPPD^.num_attrs]);
  DebugLn('  Num Constrains : %d', [fcupsPPD^.num_consts]);
  DebugLn;
  DebugLn('=== CUSTOM PAPER SUPPORT ===');
  DebugLn;
  DebugLn('  Custom Min 0   : %.2f',[fcupsPPD^.custom_min[0]]);
  DebugLn('  Custom Min 1   : %.2f',[fCupsPPD^.custom_min[1]]);
  DebugLn('  Custom Max 0   : %.2f',[fcupsPPD^.custom_max[0]]);
  DebugLn('  Custom Max 1   : %.2f',[fcupsPPD^.custom_max[1]]);

  with fcupsPPD^ do
  DebugLn('  Custom Margins : %.2f %.2f %.2f %.2f',
    [custom_margins[0],custom_margins[1],custom_margins[2],custom_margins[3]]);
  DebugLn;
  if fcupsPPD^.num_groups>0 then
  begin
    DebugLn('=== GROUPS ===');
    i := 0;
    Group := fCupsPPD^.groups;
    while (Group<>nil) and (i<fcupsPPD^.num_groups) do
    begin
      DebugLn('Group %d Name="%s" Text="%s" Options=%d SubGroups=%d',
           [i,Group^.name,Group^.text,Group^.num_options,Group^.num_subgroups]);
      j := 0;
      Option := group^.options;
      while j< group^.num_options do
      begin
        with Option^ do
        DebugLn('    Option %d Key="%s" Def="%s" Text="%s" UIType="%s" section="%s" Choices=%d',
           [j,keyword,defchoice,text,arruitypes[ui],arrsection[section],num_choices]);
        k := 0;
        Choices := Option^.choices;
        while k<Option^.num_choices do
        begin
          DebugLn('        Choice %2d %s Choice=%-20s Text="%s"',
            [k,MarkChar(Choices^.marked),Choices^.Choice,Choices^.Text]);
          inc(Choices);
          inc(k);
        end;
        inc(Option);
        inc(j);
      end;
      inc(Group);
      inc(i);
    end;
  end;

  DebugLn;
  if fcupsPPD^.num_attrs>0 then
  begin
    DebugLn('=== Attributes ===');
    i := 0;
    AttrRoot := fCupsPPD^.attrs;
    while (AttrRoot<>nil) and (i<fcupsPPD^.num_attrs) do
    begin
      Attr := AttrRoot^;
      if attr<>nil then
        DebugLn('    i=%d Name=%s Spec=%s Value=%s',[i,Attr^.Name,Attr^.Spec,Attr^.Value]);
      inc(i);
      inc(AttrRoot);
    end;
  end;
end;
{$ENDIF}

//Print the file aFileName with a selected printer and options
function TCUPSPrinter.PrintFile(aFileName: String): longint;
var
  aPrinterName : string;
begin
  Result:=-1;
  //debugln(['TCUPSPrinter.PrintFile START ',aFileName]);
  if aFileName='' then
    raise Exception.Create('TCUPSPrinter.PrintFile missing Filename');
  if not CUPSLibInstalled then Exit;
  aFileName:=ExpandFileNameUTF8(aFileName);

  if (Printers.Count>0) then
  begin
    if not Assigned(fcupsOptions) then
      SetOptionsOfPrinter;

    if Assigned(fcupsPrinter) then
      aPrinterName:=fcupsPrinter^.Name
    else
      aPrinterName:='';

    {$IFDEF DebugCUPS}
    DebugOptions;
    debugln(['TCUPSPrinter.PrintFile aPrinterName="',aPrinterName,'" aFileName="',aFileName,'" Size=',FileSizeUtf8(aFileName)]);
    {$ENDIF}

    Result:=cupsdyn.cupsPrintFile(PChar(aPrinterName),PChar(aFileName),
                                  PChar(Self.Title),
                                  fcupsNumOpts,fcupsOptions);
  end;
end;

function TCUPSPrinter.GetLastError: string;
begin
  Result:=ippErrorString(cupsdyn.cupsLastError());
end;

function TCUPSPrinter.IsOptionValueValid(AKeyword, AValue: pchar): boolean;
var
  Option: pppd_option_t;
  i: Integer;
begin
  result := false;
  if (fcupsPrinter=nil) or (fcupsppd=nil) then
    exit;
  Option := ppdFindOption(fcupsppd, AKeyword);
  if Option=nil then
    exit;

  i:=0;
  while i<Option^.num_choices do
  begin
    if strcomp(Option^.choices[i].choice, AValue)=0 then
    begin
      result := true;
      break;
    end;
    inc(i);
  end;

end;

function TCUPSPrinter.PPDOptionChoiceFrom(OptionStr, aKeyOrValue: string;
  IsKey:boolean): pppd_choice_t;
var
  i: Integer;
  option: pppd_option_t;
  p: pchar;
begin
  result := nil;

  if (fcupsPrinter=nil) or (fcupsppd=nil) then
    exit;

  option := ppdFindOption(fcupsppd, pchar(OptionStr));
  if option=nil then
    exit;

  for i:=0 to option^.num_choices-1 do
  begin
    if IsKey then
      p := @option^.choices[i].choice
    else
      p := @option^.choices[i].text;
    if strcomp(p, pchar(aKeyOrValue))=0 then
    begin
      result := @option^.choices[i];
      break;
    end;
  end;
end;


//Set State of Job
procedure TCUPSPrinter.SetJobState(aJobId : LongInt; aOp : ipp_op_t);
var Request,R : Pipp_t;                         //IPP Request
    Language  : Pcups_lang_t;                   //Default Language
    URI       : Array[0..HTTP_MAX_URI] of Char; //Printer URI
begin
  if not CUPSLibInstalled then Exit;
  if (Printers.Count>0) then
  begin
    if Assigned(fcupsPrinter) then
    begin
      R:=nil;
      DoCupsConnect;
      Request:=ippNew();
      Language:=cupsLangDefault();

      ippAddString(Request, IPP_TAG_OPERATION, IPP_TAG_CHARSET,
               'attributes-charset', '', cupsLangEncoding(language));

      ippAddString(Request, IPP_TAG_OPERATION, IPP_TAG_LANGUAGE,
               'attributes-natural-language', '', Language^.language);

      URI:=Format('http://%s:%d/jobs/%d',[cupsServer,ippPort,aJobId]);

      ippAddString(Request,IPP_TAG_OPERATION,IPP_TAG_URI,'job-uri','',URI);
      ippAddString(Request,IPP_TAG_OPERATION,IPP_TAG_NAME,'requesting-user-name','',cupsUser());

      Request^.request.op.operation_id := aOp;
      Request^.request.op.request_id   := 1;

      //Do the request and get back a response...
      R:=cupsDoRequest(fcupsHttp, Request, '/jobs/');
      if Assigned(R) then
      begin
        if (R^.request.status.status_code>IPP_OK_CONFLICT) then
          ippDelete(R);
      end;
    end;
  end;
end;

function TCUPSPrinter.GetCupsRequest : Pipp_t;
var Request   : Pipp_t; //IPP Request
    Language  : Pcups_lang_t;     //Default Language
    URI       : Array[0..HTTP_MAX_URI] of Char; //Printer URI
begin
  Result:=Nil;
  if not CUPSLibInstalled then Exit;
  if (Printers.Count>0) then
  begin
    if Assigned(fcupsPrinter) then
    begin
      DoCupsConnect;
      Request:=ippNew();
      {Build an IPP_GET_PRINTER_ATTRIBUTES request,
      which requires the following attributes:
        attributes-charset
        attributes-natural-language
        printer-uri}
      Request^.request.op.operation_id := IPP_GET_PRINTER_ATTRIBUTES;
      Request^.request.op.request_id   := 1;
      Language:=cupsLangDefault;

      ippAddString(Request, IPP_TAG_OPERATION, IPP_TAG_CHARSET,
               'attributes-charset', '', cupsLangEncoding(language));

      ippAddString(Request, IPP_TAG_OPERATION, IPP_TAG_LANGUAGE,
               'attributes-natural-language', '', Language^.language);

      // or this syntax >>
      //URI:=Format('http://%s:%d/printers/%s',[cupsServer,ippPort,fcupsPrinter^.name]);
      URI:=Format('ipp://localhost/printers/%s',[fcupsPrinter^.name]);
      ippAddString(Request,IPP_TAG_OPERATION,IPP_TAG_URI,'printer-uri','',URI);

      //Do the request and get back a response...
      Result:=cupsDoRequest(fcupsHttp, Request, '/');
      if Assigned(Result) then
      begin
        if (Result^.request.status.status_code>IPP_OK_CONFLICT) then
        begin
          ippDelete(Result);
          Result:=nil;
        end;
      end;
    end;
  end;
end;

//Initialize the options with the default options of selected printer
procedure TCUPSPrinter.SetOptionsOfPrinter;
Var Opts : Pcups_option_t;
    Opt  : Pcups_option_t;
    i    : Integer;
begin
  //if not CUPSLibInstalled then
    Exit;
  if (Printers.Count>0) then
  begin
    if Assigned(fcupsPrinter) then
    begin
      Opts := fcupsPrinter^.Options;
      for i:=0 to fcupsPrinter^.num_options-1 do
      begin
        Opt:=@Opts[i];
        cupsAddOption(Opt^.Name,Opt^.Value);
      end;
    end;
  end;
end;

//Enum all options associed with aKeyWord
function TCUPSPrinter.EnumPPDChoice(Lst : TStrings;
  const aKeyWord : string; OptNames: TStrings = nil) : Integer;
var i         : integer;
    Option    : Pppd_option_t;
    Choice    : Pppd_choice_t;
begin
  Result:=-1;
  if not CUPSLibInstalled then Exit;
  if not Assigned(Lst) then Exit;
  Lst.Clear;

  if (Printers.Count>0) then
  begin
    if Assigned(fcupsPrinter) then
    begin
      if Assigned(fcupsPPD) then
      begin
        Option:=nil;
        Option:=ppdFindOption(fcupsPPD,PChar(aKeyWord));

        If Assigned(Option) then
        begin
          for i:=0 to Option^.num_choices-1 do
          begin
            Choice:=@Option^.choices[i];
            if Choice^.marked=#1 then
              Result:=i;

            Lst.Add(Choice^.text);
            if Assigned(OptNames) then
              OptNames.Add(Choice^.choice);
          end;

          //Not marked choice then the choice is default
          if (Result<0) and (Lst.Count>0) then begin
            Result:=Lst.IndexOf(OPtion^.defchoice);
            if (Result<0)and Assigned(OptNames) then
              Result := OptNames.IndexOf(Option^.DefChoice);
          end;
        end;
      end;
    end;
  end;
end;

function TCUPSPrinter.GetPPDAttribute(const aName: string): string;
var
  i         : integer;
  AttrRoot  : PPppd_attr_t;
  Attr      : Pppd_attr_t;
begin
  Result:='';
  if not CUPSLibInstalled then
    Exit;

  if (Printers.Count>0) and (fcupsPrinter<>nil) and (fcupsPPD<>nil) then
  begin
    i := 0;
    AttrRoot := fCupsPPD^.attrs;
    while (AttrRoot<>nil) and (i<fcupsPPD^.num_attrs) do
    begin
      Attr := AttrRoot^;
      if attr<>nil then
      begin
        if (StrComp(pchar(AName), Attr^.name)=0) then
        begin
          result := attr^.value;
          break;
        end;
      end;
      inc(i);
      inc(AttrRoot);
    end;
  end;
end;

procedure TCUPSPrinter.GetEnumAttributeString(aName: PChar; Lst: TStrings);
var
  Reponse   : Pipp_t; //IPP Reponse
  Attribute : Pipp_attribute_t; //Current attribute
  i         : Integer;
begin
  if not assigned(Lst) then
    raise Exception.Create('Lst must be assigned');
  if not CUPSLibInstalled then begin
    DebugLn(['TCUPSPrinter.GetEnumAttributeString CUPSLibInstalled not installed']);
    Exit;
  end;
  
  Reponse:=GetCupsRequest;
  if not Assigned(Reponse) then begin
    DebugLn(['TCUPSPrinter.GetEnumAttributeString no Response']);
  end else begin
    try
      Attribute:=ippFindAttribute(Reponse,aName, IPP_TAG_ZERO);
      if Assigned(Attribute) then begin
        for i:=0 to Attribute^.num_values-1 do
        begin
          if Attribute^.value_tag=IPP_TAG_INTEGER then
            Lst.add(IntToStr(Pipp_value_t(@Attribute^.values)[i].aInteger))
          else
            Lst.add(Pipp_value_t(@Attribute^.values)[i]._string.text);
        end;
      end else begin
        DebugLn(['TCUPSPrinter.GetEnumAttributeString Attribute not found: ',aName]);
      end;
    finally
      ippDelete(Reponse);
    end;
  end;
end;

function TCUPSPrinter.GetAttributeInteger(aName: PChar; DefaultValue : Integer): Integer;
var
  Reponse   : Pipp_t; //IPP Reponse
  Attribute : Pipp_attribute_t; //Current attribute
begin
  Result:=DefaultValue;
  if not CUPSLibInstalled then Exit;

  Reponse:=GetCupsRequest;
  if Assigned(Reponse) then
  begin
    try
      Attribute:=ippFindAttribute(Reponse,aName, IPP_TAG_ZERO);
      if Assigned(Attribute) then
        Result:=Attribute^.values[0].aInteger;
    finally
      ippDelete(Reponse);
    end;
  end;
end;

function TCUPSPrinter.GetAttributeString(aName: PChar;
  const DefaultValue : string): string;
var
  Reponse   : Pipp_t; //IPP Reponse
  Attribute : Pipp_attribute_t; //Current attribute
begin
  Result:=DefaultValue;
  if not CUPSLibInstalled then Exit;
  Reponse:=GetCupsRequest;
  if Assigned(Reponse) then
  begin
    try
      Attribute:=ippFindAttribute(Reponse,aName, IPP_TAG_ZERO);
      if Assigned(Attribute) then
        Result:=Attribute^.values[0]._string.text
      else begin
        DebugLn(['TCUPSPrinter.GetAttributeString failed: aName="',aName,'"']);
      end;
    finally
      ippDelete(Reponse);
    end;
  end;
end;

function TCUPSPrinter.GetAttributeBoolean(aName: PChar;
  DefaultValue : Boolean): Boolean;
var
  Reponse   : Pipp_t; //IPP Reponse
  Attribute : Pipp_attribute_t; //Current attribute
begin
  Result:=DefaultValue;
  if not CUPSLibInstalled then Exit;
  Reponse:=GetCupsRequest;
  if Assigned(Reponse) then
  begin
    try
      Attribute:=ippFindAttribute(Reponse,aName, IPP_TAG_ZERO);
      if Assigned(Attribute) then
        Result:=(Attribute^.values[0].aBoolean=#1);
    finally
      ippDelete(Reponse);
    end;
  end;
end;

//Override this methode for assign an
//file name at Canvas
procedure TCUPSPrinter.DoBeginDoc;
var
  NewPath: String;
  fs: TFileStream;

  function TryTemporaryPath(const Path: string): Boolean;
  var
    CurPath: String;
  begin
    Result:=false;
    CurPath:=CleanAndExpandDirectory(Path);
    if CurPath='' then exit(false);
    if not DirectoryIsWritable(CurPath) then exit;
    NewPath:=CurPath;
    Result:=true;
  end;

begin
  if FBeginDocCount>0 then
    raise Exception.Create('TCUPSPrinter.DoBeginDoc already called. Maybe you forgot an EndDoc?');
  inherited DoBeginDoc;
  inc(FBeginDocCount);

  if (not TryTemporaryPath('~/tmp/'))
  and (not TryTemporaryPath('/tmp/'))
  and (not TryTemporaryPath('/var/tmp/')) then
    NewPath:='';

  FOutputFileName := AppendPathDelim(NewPath)+
    'OutPrinter_'+FormatDateTime('yyyymmmddd-hhnnss',Now);

  if RawMode then
    FOutputFileName := FOutputFileName + '.raw'
  else begin
    FOutputFileName := FOutputFileName + '.ps';
    TFilePrinterCanvas(Canvas).OutputFileName := FOutputFileName;
  end;

  // test writing, on error this raises exception showing the user the filename
  fs:=TFileStream.Create(FOutputFilename,fmCreate);
  try
    fs.Write(FOutputFilename[1],1);
  finally
    fs.free;
  end;
  DeleteFileUTF8(FOutputFilename);
end;

//If not aborted, send PostScript file to printer.
//After, delete this file.
procedure TCUPSPrinter.DoEndDoc(aAborted: Boolean);
var
  CupsResult: LongInt;
begin
  inherited DoEndDoc(aAborted);
  dec(FBeginDocCount);
  Exclude(FStates,cpsPaperRectValid);

  if RawMode then begin

    if not aAborted and (FRawModeStream<>nil)
      and (FRawModeStream.Size>0) then
    begin
      try
        FRawModeStream.SaveToFile(FOutputFileName);
      finally
        FRawModeStream.Clear;
      end;
    end;
    
  end else
    TFilePrinterCanvas(Canvas).OutputFileName:='';

  if not aAborted then begin
    if not FileExistsUTF8(FOutputFileName) then
      raise Exception.Create('Unable to write to "'+FOutputFileName+'"');
    {$IFDEF LogPrintoutFile}
    CopyFile(FOutputFileName, 'printjob'+ExtractFileExt(FOutputFileName));
    {$ENDIF}
    try
      {$IFNDEF DoNotPrint}
      if Filename<>'' then
        CopyFile(FOutputFileName, FileName)
      else begin
        CupsResult:=PrintFile(FOutputFileName);
        if CupsResult<=0 then
          raise Exception.Create('CUPS printing: '+GetLastError);
      end;
      {$ENDIF}
    finally
      DeleteFileUTF8(FOutputFilename);
    end;
  end;
end;

procedure TCUPSPrinter.DoNewPage;
begin
  // just to flag that we want the old 'newpage' pagination way
  // instead of the new 'beginpage/endpage'
end;

procedure TCUPSPrinter.DoResetPrintersList;
begin
  if Assigned(fcupsPPD) then
  begin
    ppdClose(fcupsPPD);
    fcupsPPD:=nil;
  end;

  if fcupsPPDName<>'' then
  begin
    DeleteFileUTF8(fcupsPPDName);
    fcupsPPDName:='';
  end;

  FreeOptions;
  if Assigned(fcupsPrinters) and CUPSLibInstalled then begin
    cupsFreeDests(Printers.Count,fcupsPrinters);
    fCupsPrinter := nil;
  end;

  inherited DoResetPrintersList;
end;

procedure TCUPSPrinter.DoEnumPrinters(Lst: TStrings);
Var i,Num   : Integer;
    P       : Pcups_dest_t;
begin
  inherited DoEnumPrinters(Lst);
  {$IFDEF NOPRINTERS}
  Lst.Clear;
  Exit;
  {$ENDIF}
  if not CUPSLibInstalled then Exit;

  Num:=cupsGetDests(@fcupsPrinters);
  For i:=0 to Num-1 do
  begin
    P:=nil;
    P:=@fcupsPrinters[i];
    if Assigned(P) then
    begin
      if P^.is_default<>0 then
        Lst.Insert(0,P^.name)
      else
        Lst.Add(P^.name);
    end;
  end;
end;

procedure TCUPSPrinter.DoEnumPapers(Lst: TStrings);
var
  choice: Pppd_choice_t;
  Option: Pppd_option_t;
  c: Integer;
begin
  //DebugLn(['TCUPSPrinter.DoEnumPapers ',dbgsName(Self)]);

  //TODO: note that we are returning here the list of paper "keys"
  //      not the human readable paper names. Modify cups support
  //      to return human readable paper names.

  Lst.Clear;
  FCupsDefaultPaper := '';
  if CupsPPD<>nil then
  begin
    Option := ppdFindOption(CupsPPD, PChar('PageSize'));
    Choice := Option^.choices;
    fCupsDefaultPaper := Option^.defchoice;
    c := 0;
    while (Choice<>nil) and (c<Option^.num_choices) do
    begin
      lst.AddObject(Choice^.Choice, TObject(Choice));
      inc(choice);
      inc(c);
    end;
  end;

  fCupsPapersCount := lst.Count;
end;

function TCUPSPrinter.DoSetPrinter(aName: string): Integer;
Var i  : Integer;
    P  : Pcups_dest_t;
    Fn : String;
begin
  //debugln('TCUPSPrinter.DoSetPrinter aName="',aName,'"');
  Result:=inherited DoSetPrinter(aName);
  if not CUPSLibInstalled then Exit;
  //debugln('TCUPSPrinter.DoSetPrinter B Printers.Count=',dbgs(Printers.Count));

  //Set the current printer. If aName='' then use a default Printer (index 0)
  If (Printers.Count>0) then
  begin
    if (aName<>'') and Assigned(fcupsPPD) then
    begin
      //Printer changed ?
      i:=Printers.IndexOf(aName);
      if i=PrinterIndex then
      begin
        Result:=PrinterIndex;
        //debugln('TCUPSPrinter.DoSetPrinter no change');
        Exit;
      end;
    end;

    //Clear all existing options
    FreeOptions;

    if Assigned(fcupsPPD) then
    begin
      ppdClose(fcupsPPD);
      fcupsPPD:=nil;

      if fcupsPPDName<>'' then
      begin
        DeleteFileUTF8(fcupsPPDName);
        fcupsPPDName:='';
      end;
    end;


    if aName='' then
      i:=0
    else
      i:=Printers.IndexOf(aName);

    if i>-1 then
    begin
      Result:=i;
      
      P:=nil;
      P:=cupsGetDest(PChar(aName),nil,Printers.Count,fcupsPrinters);
      if not Assigned(P) then
        raise Exception.Create(Format('"%s" is not a valid printer.',[aName]));
      fcupsPrinter:=P;

      //Open linked ppdfile
      Fn:=cupsGetPPD(PChar(aName));
      fcupsPPD:=ppdOpenFile(PChar(Fn));
      fcupsPPDName:=Fn;
      {$IFDEF DebugCUPS}
      DebugPPD;
      DebugCapabilities;
      {$ENDIF}
    end;
  end
  else
  begin
    PrinterIndex:=-1;
    fcupsPPD:=nil;
  end;
end;

function TCUPSPrinter.DoGetCopies: Integer;
begin
  if not (cpsCopiesValid in FStates) then begin
    fCachedCopies:=inherited DoGetCopies;

    //Get default value if defined
    fCachedCopies:=GetAttributeInteger('copies-default',fCachedCopies);
    //Get Copies in options or return default value
    fCachedCopies:=StrToIntdef(cupsGetOption('copies'),fCachedCopies);
    {$IFDEF UseCache}
    Include(FStates,cpsCopiesValid);
    {$ENDIF}
  end;
  Result:=fCachedCopies;
end;

procedure TCUPSPrinter.DoSetCopies(aValue: Integer);
var i : Integer;
begin
  {$IFDEF UseCache}
  if aValue=DoGetCopies then exit;
  Exclude(FStates,cpsCopiesValid);
  {$ENDIF}
  inherited DoSetCopies(aValue);

  if Printers.Count>0 then
  begin
    if not Assigned(fcupsOptions) then
      SetOptionsOfPrinter;
    i:=aValue;
    if i<1 then i:=1;
    cupsAddOption('copies',IntToStr(i));
  end;
end;

function TCUPSPrinter.DoGetOrientation: TPrinterOrientation;
var i  : Integer;
begin
  if not (cpsOrientationValid in FStates) then begin
    if Printers.Count>0 then
    begin
      //Default orientation value
      i:=GetAttributeInteger('orientation-requested-default',3);
      // check if rotation is automatic or out-of-range
      if not (i in [3,4,5,6]) then
        i:=3; // yep, then for us this means portait
      fCachedOrientation:=TPrinterOrientation(i-3);
    end;
    Include(FStates,cpsOrientationValid);
  end;
  Result:=fCachedOrientation;
  {$IFDEF DebugCUPS}
  DebugLn('DoGetOrientation: result=%d',[ord(Result)]);
  {$ENDIF}
end;

procedure TCUPSPrinter.DoSetOrientation(aValue: TPrinterOrientation);
begin
  if aValue=DoGetOrientation then
    exit;
  Exclude(FStates,cpsPaperRectValid);
  inherited DoSetOrientation(aValue);
  fcachedOrientation := AValue;
  Include(FStates,cpsOrientationValid);
end;

function TCUPSPrinter.DoGetDefaultPaperName: string;
begin
  if not (cpsDefaultPaperNameValid in FStates) then begin
    fCachedGetDefaultPaperName:='';
    if not CupsPapersListValid then
      FCachedGetDefaultPaperName:=PaperSize.DefaultPaperName
    else begin
      if FCupsDefaultPaper<>'' then
        fCachedGetDefaultPaperName:= FCupsDefaultPaper
      else
        fCachedGetDefaultPaperName:=
                 GetAttributeString('media-default',fCachedGetDefaultPaperName);
      {$IFDEF UseCache}
      Include(FStates,cpsDefaultPaperNameValid);
      {$ENDIF}
    end;
  end;
  Result:=fCachedGetDefaultPaperName;
end;

function TCUPSPrinter.DoGetPaperName: string;
begin
  if not (cpsPaperNameValid in FStates) then begin
    // paper is not yet retrieved for first time
    // first try to see if there is a list of papers available
    if not CupsPapersListValid then
      FCachedPaperName := PaperSize.PaperName
    else begin
      fCachedPaperName := cupsGetOption('PageSize');
      {$IFDEF UseCache}
      Include(FStates,cpsPaperNameValid);
      {$ENDIF}
    end;
  end;
  Result:=fCachedPaperName;
end;

procedure TCUPSPrinter.DoSetPaperName(aName: string);
begin
  {$IFDEF UseCache}
  if aName=DoGetPaperName then exit;
  Exclude(FStates,cpsPaperNameValid);
  {$ENDIF}
  inherited DoSetPaperName(aName);
  
  if FCupsPapersCount<=0 then
    PaperSize.PaperName:=AName
  else
    cupsAddOption('PageSize',aName)
end;

//Initialise aPaperRc with the aName paper rect
//Result : -1 no result
//          0 aPaperRc.WorkRect is a margins
//          1 aPaperRc.WorkRect is really the work rect
function TCUPSPrinter.DoGetPaperRect(aName: string;
  var aPaperRc: TPaperRect): Integer;
  
var
  P : Pppd_size_t;
  Ky,Kx: Double;
begin
  if (not (cpsPaperRectValid in FStates)) or
    (fCachePaperRectName<>aName) then
  begin
    fCachePaperRectName:=aName;
    FillChar(fCachePaperRect,SizeOf(fCachePaperRect),0);
    fCachePaperRectResult:=inherited DoGetPaperRect(aName, aPaperRc);
    {$IFDEF UseCache}
    Include(FStates,cpsPaperRectValid);
    {$ENDIF}

    P:=nil;
    if CUPSLibInstalled and Assigned(fcupsPPD) then
    begin
      P:=ppdPageSize(fcupsPPD,PChar(aName));
      if Assigned(P) then
      begin
        fCachePaperRectResult:=1; //CUPS return margins

        // Margins.
        //
        // Cups gives dimensions based on postcript language
        // user space coordinates system which is something like
        //
        //  +y                                 +--> +x
        //   ^           but our system is     |
        //   |                                 v
        //   +--> +x                           +y
        //
        //  so values in x are the same, but we need to invert values in y,
        //  the given bottom value is the margin size at the bottom, we need
        //  to re-calc. our bottom offset, and the given top value is offset
        //  top value of imageable area, we need to re-calc. our top offset,
        //  which is the margin size at the top of the page.
        //
        //  The current implementation assumes that paper is fed short-edge-first
        //  either in portrait orientation, or in landscape orientation.
        //
        //  In landscape orientation, printable margins should preserved.
        //  It's based on a 90 degree counterclock wise paper rotation
        //
        //     FEED DIRECTION             FEED DIRECTION
        //
        //           /\                         /\
        //          /  \                       /  \
        //           ||                         ||
        //           ||                         ||
        //
        //     PORTRAIT                   LANDSCAPE
        //     +-----------------+        +-----------------+
        //     |        t        |        |        t        |
        //     |   +---------+   |        |   +---------+   |
        //     |   |   ( )   |   |        |   |   |   / |   |
        //     | l |  --+--  | r |        | l |()-+---  | r |
        //     |   |   / \   |   |        |   |   |   \ |   |
        //     |   +---------+   |        |   +---------+   |
        //     |        b        |        |        b        |
        //     +-----------------+        +-----------------+
        //
        //     REVERSE PORTRAIT           REVERSE LANDSCAPE
        //     +-----------------+        +-----------------+
        //     |        t        |        |        t        |
        //     |   +---------+   |        |   +---------+   |
        //     |   |   \ /   |   |        |   | \   |   |   |
        //     | l |  --+--  | r |        | l |  ---+-()| r |
        //     |   |   ( )   |   |        |   | /   |   |   |
        //     |   +---------+   |        |   +---------+   |
        //     |        b        |        |        b        |
        //     +-----------------+        +-----------------+
        //
        Kx := Printer.XDPI/72;
        Ky := Printer.YDPI/72;
        if Orientation in [poPortrait, poReversePortrait] then begin
          fCachePaperRect.PhysicalRect.Right:=Round(P^.Width*Kx);
          fCachePaperRect.PhysicalRect.Bottom:=Round(P^.Length*Ky);
          fCachePaperRect.WorkRect.Left:=Round(P^.Left*Kx);
          fCachePaperRect.WorkRect.Right:=Round(P^.Right*Kx);
          fCachePaperRect.WorkRect.Top:=Round((P^.Length-P^.Top)*Ky);
          fCachePaperRect.WorkRect.Bottom:=Round((P^.Length-P^.Bottom)*Ky);
        end else begin
          FCachePaperRect.PhysicalRect.Right:=Round(P^.Length*Kx);
          FCachePaperRect.PhysicalRect.Bottom:=Round(P^.Width*Ky);
          FCachePaperRect.WorkRect.Left:=Round((P^.Length-P^.Top)*Kx);
          FCachePaperRect.WorkRect.Right:=Round((P^.Length-P^.Bottom)*Kx);
          FCachePaperRect.WorkRect.Top:=Round((P^.Width-P^.Right)*Ky);
          FCachePaperRect.WorkRect.Bottom:=Round((p^.width - P^.left)*Ky);
        end;

        {$IFDEF DebugCUPS}
        with P^ do
        DebugLn('ORG: Width=%f Length=%f Left=%f Right=%f Top=%f Bottom=%f Name=%s',
          [Width,Length,Left,Right,Top,Bottom,string(Name)]);
        with FCachePaperRect do
        DebugLn('NEW: Width=%d Length=%d Left=%d Top=%d Right=%d Bottom=%d ml=%d mt=%d mr=%d mb=%d',
          [PhysicalRect.Right,PhysicalRect.Bottom,WorkRect.Left,WorkRect.Top,WorkRect.Right,WorkRect.Bottom,
           WorkRect.Left,WorkRect.Top,PhysicalRect.Right-WorkRect.Right,
           PhysicalRect.Bottom-WorkRect.Bottom]);
        {$ENDIF}
      end;
    end;
    
    if P=nil then begin
      FCachePaperRect := PaperSize.PaperRectOf[AName];
      fCachePaperRectResult:=1
    end;
    
  end;
  Result:=fCachePaperRectResult;
  aPaperRc:=fCachePaperRect;
end;


function TCUPSPrinter.DoGetPrinterState: TPrinterState;
var //Request   : Pipp_t; //IPP Request
    //Reponse   : Pipp_t; //IPP Reponse
    //Attribute : Pipp_attribute_t; //Current attribute
    //Language  : Pcups_lang_t;     //Default Language
    aState    : ipp_pstate_t;     //Printer state
    //URI       : Array[0..HTTP_MAX_URI] of Char; //Printer URI
begin
  Result:=inherited DoGetPrinterState;

  aState:=ipp_pstate_t(GetAttributeInteger('printer-state',0));
  Case aState of
    IPP_PRINTER_IDLE       : Result:=psReady;
    IPP_PRINTER_PROCESSING : Result:=psPrinting;
    IPP_PRINTER_STOPPED    : Result:=psStopped;
  end;
end;

function TCUPSPrinter.DoGetDefaultCanvasClass: TPrinterCanvasRef;
begin
  {$IFDEF UseCairo}
  Result := TCairoPsCanvas;
  {$ELSE}
  Result := TPostscriptPrinterCanvas;
  {$ENDIF}
end;

function TCUPSPrinter.GetPrinterType: TPrinterType;
Var i : Integer;
begin
  Result:=inherited GetPrinterType;
  i:=GetAttributeInteger('printer-type',CUPS_PRINTER_LOCAL);
  If (i and CUPS_PRINTER_REMOTE)=CUPS_PRINTER_REMOTE then
    Result:=ptNetWork;
end;

function TCUPSPrinter.GetCanPrint: Boolean;
begin
  Result:=inherited GetCanPrint;
  Result:=GetAttributeBoolean('printer-is-accepting-jobs',Result)
end;

initialization
  if Assigned(Printer) then
    Printer.Free;

  Printer:=TCUPSPrinter.Create;

FINALIZATION
  // Free the printer before unloading library
  Printer.Free;
  Printer:=nil;
  //Unload CUPSLib if loaded
  FinalizeCups;

END.

