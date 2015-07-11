{%MainUnit GtkInt.pp}

{------------------------------------------------------------------------------
  Function: ExtractFilterList
  Params: const Filter: string; var FilterIndex: integer;
          var ListOfPFileSelFilterEntry: TStringList
  Returns: -

  Converts a Delphi file filter of the form
  'description1|mask1|description2|mask2|...'
  into a TFPList of PFileSelFilterEntry(s).
  Multi masks:
    - multi masks like '*.pas;*.pp' are converted into multiple entries.
    - if the masks are found in the description they are adjusted
    - if the mask is not included in the description it will be concatenated
    For example:
      'Pascal files (*.pas;*.pp)|*.pas;*.lpr;*.pp;
      is converted to three filter entries:
        'Pascal files (*.pas)' + '*.pas'
        'Pascal files (*.pp)'  + '*.pp'
        'Pascal files (*.lpr)' + '*.lpr'
 ------------------------------------------------------------------------------}
procedure ExtractFilterList(const Filter: string;
  out ListOfFileSelFilterEntry: TFPList;
  SplitMultiMask: boolean);
var
  Masks: TStringList;
  CurFilterIndex: integer;

  procedure ExtractMasks(const MultiMask: string);
  var CurMaskStart, CurMaskEnd: integer;
    s: string;
  begin
    if Masks=nil then
      Masks:=TStringList.Create
    else
      Masks.Clear;
    CurMaskStart:=1;
    while CurMaskStart<=length(MultiMask) do begin
      CurMaskEnd:=CurMaskStart;
      if SplitMultiMask then begin
        while (CurMaskEnd<=length(MultiMask)) and (MultiMask[CurMaskEnd]<>';')
        do
          inc(CurMaskEnd);
      end else begin
        CurMaskEnd:=length(MultiMask)+1;
      end;
      s:=Trim(copy(MultiMask,CurMaskStart,CurMaskEnd-CurMaskStart));
      Masks.Add(s);
      CurMaskStart:=CurMaskEnd+1;
    end;
  end;

  procedure AddEntry(const Desc, Mask: string);
  var NewFilterEntry: TFileSelFilterEntry;
  begin
    NewFilterEntry:=TFileSelFilterEntry.Create(Desc,Mask);
    NewFilterEntry.FilterIndex:=CurFilterIndex;
    ListOfFileSelFilterEntry.Add(NewFilterEntry);
  end;

  // remove all but one masks from description string
  function RemoveOtherMasks(const Desc: string; MaskIndex: integer): string;
  var i, StartPos, EndPos: integer;
  begin
    Result:=Desc;
    for i:=0 to Masks.Count-1 do begin
      if i=MaskIndex then continue;
      StartPos:=Pos(Masks[i],Result);
      EndPos:=StartPos+length(Masks[i]);
      if StartPos<1 then continue;
      while (StartPos>1) and (Result[StartPos-1] in [' ',#9,';']) do
        dec(StartPos);
      while (EndPos<=length(Result)) and (Result[EndPos] in [' ',#9]) do
        inc(EndPos);
      if (StartPos>1) and (Result[StartPos-1]='(')
      and (EndPos<=length(Result)) then begin
        if (Result[EndPos]=')') then begin
          dec(StartPos);
          inc(EndPos);
        end else if Result[EndPos]=';' then begin
          inc(EndPos);
        end;
      end;
      System.Delete(Result,StartPos,EndPos-StartPos);
    end;
  end;

  procedure AddEntries(const Desc: string; MultiMask: string);
  var i: integer;
    CurDesc: string;
  begin
    ExtractMasks(MultiMask);
    for i:=0 to Masks.Count-1 do begin
      CurDesc:=RemoveOtherMasks(Desc,i);
      if (Masks.Count>1) and (Pos(Masks[i],CurDesc)<1) then begin
        if (CurDesc='') or (CurDesc[length(CurDesc)]<>' ') then
          CurDesc:=CurDesc+' ';
        CurDesc:=CurDesc+'('+Masks[i]+')';
      end;
      //debugln('AddEntries ',CurDesc,' ',Masks[i]);
      AddEntry(CurDesc,Masks[i]);
    end;
    inc(CurFilterIndex);
  end;

var
  CurDescStart, CurDescEnd, CurMultiMaskStart, CurMultiMaskEnd: integer;
  CurDesc, CurMultiMask: string;
begin
  ListOfFileSelFilterEntry:=TFPList.Create;
  Masks:=nil;
  CurFilterIndex:=0;
  CurDescStart:=1;
  while CurDescStart<=length(Filter) do begin
    // extract next filter description
    CurDescEnd:=CurDescStart;
    while (CurDescEnd<=length(Filter)) and (Filter[CurDescEnd]<>'|') do
      inc(CurDescEnd);
    CurDesc:=copy(Filter,CurDescStart,CurDescEnd-CurDescStart);
    // extract next filter multi mask
    CurMultiMaskStart:=CurDescEnd+1;
    CurMultiMaskEnd:=CurMultiMaskStart;
    while (CurMultiMaskEnd<=length(Filter)) and (Filter[CurMultiMaskEnd]<>'|') do
      inc(CurMultiMaskEnd);
    CurMultiMask:=copy(Filter,CurMultiMaskStart,CurMultiMaskEnd-CurMultiMaskStart);
    if CurDesc='' then CurDesc:=CurMultiMask;
    // add filter(s)
    if (CurMultiMask<>'') or (CurDesc<>'') then
      AddEntries(CurDesc,CurMultiMask);
    // next filter
    CurDescStart:=CurMultiMaskEnd+1;
  end;
  Masks.Free;
end;

procedure FreeListOfFileSelFilterEntry(ListOfFileSelFilterEntry: TFPList);
var
  i: Integer;
begin
  if ListOfFileSelFilterEntry=nil then exit;
  for i:=0 to ListOfFileSelFilterEntry.Count-1 do
    TObject(ListOfFileSelFilterEntry[i]).Free;
  ListOfFileSelFilterEntry.Free;
end;
