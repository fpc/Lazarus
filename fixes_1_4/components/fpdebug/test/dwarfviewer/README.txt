This tool can be used to generate the dwarf setup data for the testcase.

Warning: Very slow



The output can be processed in the IDE with the below pas script macro.
NOTE: avoid variables of the same name in different scopes. 


const
  AddressLen = 8; // chars in hex = 4 bytes
  VarNamePrefix = 'GLOBTESTSETUP1';
  VarNamePrefixRepl = 'GlobTestSetup1.Var';
  VarParamPrefix = '';
  VarParamPrefixRepl = 'TestStackFrame.';
  VarParamOffset = 'TestStackFrame.EndPoint';


const  HexDigits= '0123456789ABCDEF';

function IntToHex(Value: int64; Digits: integer): string;
var i: integer;
begin
 If Digits=0 then
   Digits:=1;
 SetLength(result, digits);
 for i := 0 to digits - 1 do
  begin
   result[digits - i] := HexDigits[(value and 15+1)];
   value := value shr 4;
  end ;
 while value <> 0 do begin
   result := HexDigits[(value and 15+1)] + result;
   value := value shr 4;
 end;
end ;

function GetCurrentLineId: String;
var   p : TPoint;
begin
  p := Caller.CaretXY;
  ecLineStart;
  caller.SelectWord;
  Result := caller.SelText;
  Caller.CaretXY := p;
end;

function GetCurrentLineIndent: String;
var   p : TPoint;
begin
  Result := '';
  if copy(caller.LineAtCaret, 1,1) <> ' ' then exit;
  p := Caller.CaretXY;
  Caller.CaretX := 1;
  ecSelWordRight;
  Result := caller.SelText;
  Caller.CaretXY := p;
end;

function FindTAG(ATag: String; var StartPoint: TPoint; out ID: String): Boolean;
begin
  Result := Caller.SearchReplaceEx('.Tag := '+ATag+';', '', [], StartPoint) > 0;
  if not Result Then exit;
  StartPoint := Caller.CaretXY;
  StartPoint.y := StartPoint.y + 1;
  ID := GetCurrentLineId;
end;

function FindTAGEx(ATag, AnIndent: String; var StartPoint: TPoint; out ID: String): Boolean;
begin
  repeat
    Result := Caller.SearchReplaceEx('.Tag := '+ATag+';', '', [], StartPoint) > 0;
    if not Result Then exit;
    if (Copy(caller.LineAtCaret, 1, length(AnIndent)) = AnIndent) and
       (Copy(caller.LineAtCaret, length(AnIndent),1) <> ' ')
    then exit;
  until not result;
end;

function FindAttr(AnAttr, AnID: String; FindInLine: String): Boolean;
var i: Integer;
begin
  Result := Caller.SearchReplaceEx('^ *'+AnId+'\..*\('+AnAttr+' *,', '', [ssoRegExpr], Point(1,1)) > 0;
  if (not Result) or (FindInLine='') then exit;
  i := pos(FindInLine, caller.LineAtCaret);
  Result := i > 0;
  if not result then exit;
  Caller.LogicalCaretXY := Point(i, Caller.CaretY);
end;

function FindName(AnID: String): String;
begin
  Result := AnId;
  if not FindAttr('DW_AT_name', ANId, '+#0') then exit;
  while (Caller.LogicalCaretX > 1) and (caller.LineAtCaret[caller.LogicalCaretX] <> '''') do ecLeft;
  while (Caller.LogicalCaretX > 1) and (caller.LineAtCaret[caller.LogicalCaretX-1] <> '''') do ecSelLeft;
  Result := caller.SelText;
end;

function PrefIxIdParagraph(AnId, ANewLine: String): Boolean;
begin
  Result := Caller.SearchReplaceEx(AnId+' :=', '', [ssoWholeWord], Point(1,1)) > 0;
  if not Result Then exit;
  Caller.CaretX := 1; ecSelWordRight;
  if copy(caller.LineAtCaret, 1,1) = ' ' then
    ANewLine := caller.SelText+ ANewLine;
  Caller.CaretX := 1;
  caller.SelText := ANewLine + #13+#10;
end;

function ReplaceHexAddrInLine(NewText: String): Boolean;
var   p,p2: TPoint;
begin
  p := Caller.CaretXY;
  p2 := p; p2.x := 1;
  Result := Caller.SearchReplaceEx('\$[0-9a-f]+', '', [ssoRegExpr], p2) > 0;
  Result := Result and (Caller.CaretY = p.y);
  if Result then
    caller.SelText := NewText;
  Caller.CaretXY := p;
end;

function ReplaceSLEBInLine(NewText: String): Boolean;
var   p,p2: TPoint;
begin
  p := Caller.CaretXY;
  p2 := p; p2.x := 1;
  Result := Caller.SearchReplaceEx('SLEB\(-?[0-9]+\)', '', [ssoRegExpr], p2) > 0;
  Result := Result and (Caller.CaretY = p.y);
  if Result then
    caller.SelText := 'SLEB('+NewText+')';
  Caller.CaretXY := p;
end;

function ReplacePreFix(AText, APrefix, ANew: String): String;
begin
  Result := AText;
  if copy(Result,1,length(APrefix))=APrefix
  then Result := ANew+copy(Result,1+length(APrefix),length(Result));
end;

var
  MainComment: String;
  UnitAddr, UnitIncr: Int64;
  UnitFoundPos : TPoint;
  UnitId, UnitName: string;

  ProcAddr, ProcIncr: Int64;
  ProcFoundPos : TPoint;
  ProcEndLine: Integer;
  ProcId, ProcName: string;

  VarFoundPos : TPoint;
  VarId, VarName, VarNewName: string;

  p : TPoint;
  s:String;
begin
  UnitAddr := $400000;
  UnitIncr  := $100000;
  ProcAddr := $400000;
  ProcIncr  := $001000;
  UnitFoundPos:=Point(1,1);

  if FindTag('DW_TAG_compile_unit', UnitFoundPos, UnitId) then begin
    UnitName:=FindName(UnitId);


    if FindAttr('DW_AT_low_pc', UnitId, '') then begin
      if ReplaceHexAddrInLine('$'+inttohex(UnitAddr, AddressLen))
      then MainComment := MainComment + '// '+UnitName+' DW_AT_low_pc at $'+ inttohex(UnitAddr, AddressLen)+ #13+#10
      else MainComment := MainComment + '// '+UnitName+' DW_AT_low_pc at Addr NOT found'+ #13+#10;
      UnitAddr := UnitAddr + UnitIncr;
    end;
    if FindAttr('DW_AT_high_pc', UnitId, '') then begin
      ReplaceHexAddrInLine('$'+inttohex(UnitAddr-1, AddressLen));
    end;


    ProcFoundPos:=Point(1,1);
    while FindTag('DW_TAG_subprogram', ProcFoundPos, ProcId) do begin
      ProcName := FindName(ProcID);
      if FindAttr('DW_AT_low_pc', ProcId, '') then begin
        if ReplaceHexAddrInLine('$'+inttohex(ProcAddr, AddressLen))
        then MainComment := MainComment + '// '+ProcName+' DW_AT_low_pc at $'+ inttohex(ProcAddr, AddressLen)+ #13+#10
        else MainComment := MainComment + '// '+ProcName+' DW_AT_low_pc at Addr NOT found'+ #13+#10;
        ProcAddr := ProcAddr + ProcIncr;
      end;
      if FindAttr('DW_AT_high_pc', ProcId, '') then begin
        ReplaceHexAddrInLine('$'+inttohex(ProcAddr-1, AddressLen));
      end;

    end;


    VarFoundPos:=Point(1,1);
    while FindTag('DW_TAG_variable', VarFoundPos, VarId) do begin
      VarName := FindName(VarID);
      if not( (VarName='this') or (VarName='result') or (VarName='self') or (VarName='vmt') ) then begin
        if FindAttr('DW_AT_location', VarId, 'SLEB') then begin
          // local var
          VarNewName  := ReplacePreFix(VarName, VarParamPrefix, VarParamPrefixRepl);
          s := 'StackOffs := @'+VarNewName+' - @'+VarParamOffset+';'
          if ReplaceSLEBInLine('StackOffs')
          then MainComment := MainComment + '// param '+UnitName+'.'+VarName+' = ' + VarNewName+ #13+#10
          else MainComment := MainComment + '// param '+UnitName+'.'+VarName+' Addr NOT FOUND'+ #13+#10;
          PrefIxIdParagraph(VarID, s);
        end
        else
        if FindAttr('DW_AT_location', VarId, '$') then begin
          // Global var
          VarNewName  := ReplacePreFix(VarName, VarNamePrefix, VarNamePrefixRepl);
          if ReplaceHexAddrInLine('@'+VarNewName)
          then MainComment := MainComment + '// var '+UnitName+'.'+VarName+' = ' + VarNewName+ #13+#10
          else MainComment := MainComment + '// var '+UnitName+'.'+VarName+' Addr NOT FOUND'+ #13+#10;
        end
        else
          MainComment := MainComment + '// var '+UnitName+'.'+VarName+' DW_AT_location NOT FOUND'+ #13+#10;
      end;
    end;



    VarFoundPos:=Point(1,1);
    while FindTag('DW_TAG_formal_parameter', VarFoundPos, VarId) do begin
      VarName := FindName(VarID);
      if not( (VarName='this') or (VarName='result') or (VarName='self') or (VarName='vmt') ) then begin
        if FindAttr('DW_AT_location', VarId, '') then begin
          VarNewName  := ReplacePreFix(VarName, VarParamPrefix, VarParamPrefixRepl);
          s := 'StackOffs := @'+VarNewName+' - @'+VarParamOffset+';'
          if ReplaceSLEBInLine('StackOffs')
          then MainComment := MainComment + '// param '+UnitName+'.'+VarName+' = ' + VarNewName+ #13+#10
          else MainComment := MainComment + '// param '+UnitName+'.'+VarName+' Addr NOT FOUND'+ #13+#10;
          PrefIxIdParagraph(VarID, s);
        end
        else
          MainComment := MainComment + '// param '+UnitName+'.'+VarName+' DW_AT_location NOT FOUND'+ #13+#10;
      end;
    end;


  end;


  Caller.LogicalCaretXY := Point(1,1);
  caller.SelText:=MainComment;
end.