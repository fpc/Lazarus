{
/***************************************************************************
                             sourceeditprocs.pas
                             -------------------

 ***************************************************************************/

 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Support functions and types for the source editor.

}
unit SourceEditProcs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LCLType, GraphType, Graphics, Controls, SynEdit,
  SynEditHighlighter, SynRegExpr, SynCompletion, BasicCodeTools, CodeTree,
  CodeAtom, CodeCache, SourceChanger, CustomCodeTool, CodeToolManager,
  PascalParserTool, KeywordFuncLists, FileProcs, IdentCompletionTool,
  PascalReaderTool, SourceLog,
  LazIDEIntf, TextTools, IDETextConverter, DialogProcs, MainIntf, EditorOptions,
  IDEImagesIntf, CodeToolsOptions;

type

  { TLazTextConverterToolClasses }

  TLazTextConverterToolClasses = class(TTextConverterToolClasses)
  public
    function GetTempFilename: string; override;
    function SupportsType(aTextType: TTextConverterType): boolean; override;
    function LoadFromFile(Converter: TIDETextConverter; const AFilename: string;
                          UpdateFromDisk, Revert: Boolean): Boolean; override;
    function SaveCodeBufferToFile(Converter: TIDETextConverter;
                           const AFilename: string): Boolean; override;
    function GetCodeBufferSource(Converter: TIDETextConverter;
                                 out Source: string): boolean; override;
    function CreateCodeBuffer(Converter: TIDETextConverter;
                              const Filename, NewSource: string;
                              out CodeBuffer: Pointer): boolean; override;
    function LoadCodeBufferFromFile(Converter: TIDETextConverter;
                                   const Filename: string;
                                   UpdateFromDisk, Revert: Boolean;
                                   out CodeBuffer: Pointer): boolean; override;
    procedure AssignCodeToolBossError(Target: TCustomTextConverterTool); override;
  end;
  
procedure SetupTextConverters;
procedure FreeTextConverters;

type
  TCompletionType = (
    ctNone, ctWordCompletion, ctTemplateCompletion, ctIdentCompletion);
  TIdentComplValue = (
    icvIdentifier,
    icvProcWithParams,
    icvIndexedProp,
    icvCompleteProcDeclaration,
    icvUnitName,
    icvNone
    );

// completion form and functions
function PaintCompletionItem(const AKey: string; ACanvas: TCanvas;
  X, Y, MaxX: integer; ItemSelected: boolean; Index: integer;
  aCompletion : TSynCompletion; CurrentCompletionType: TCompletionType;
  Highlighter: TSrcIDEHighlighter; MeasureOnly: Boolean = False): TPoint;

function GetIdentCompletionValue(aCompletion : TSynCompletion;
  AddChar: TUTF8Char;
  out ValueType: TIdentComplValue; out CursorToLeft: integer): string;
function BreakLinesInText(const s: string; MaxLineLength: integer): string;

implementation

var
  SynREEngine: TRegExpr;

procedure SetupTextConverters;
begin
  TextConverterToolClasses:=TLazTextConverterToolClasses.Create;
  TextConverterToolClasses.RegisterClass(TTextReplaceTool);
end;

procedure FreeTextConverters;
begin
  FreeAndNil(TextConverterToolClasses);
end;

function PaintCompletionItem(const AKey: string; ACanvas: TCanvas;
  X, Y, MaxX: integer; ItemSelected: boolean; Index: integer;
  aCompletion : TSynCompletion; CurrentCompletionType: TCompletionType;
  Highlighter: TSrcIDEHighlighter; MeasureOnly: Boolean): TPoint;

const
  HintModifierImage: array[TPascalHintModifier] of String = (
 { phmDeprecated    } 'ce_deprecated',
 { phmPlatform      } 'ce_platform',
 { phmLibrary       } 'ce_library',
 { phmUnimplemented } 'ce_unimplemented',
 { phmExperimental  } 'ce_experimental'
  );

var
  BGRed: Integer;
  BGGreen: Integer;
  BGBlue: Integer;
  TokenStart: Integer;
  BackgroundColor: TColorRef;
  ForegroundColor: TColorRef;

  procedure SetFontColor(NewColor: TColor);
  
    {procedure IncreaseDiff(var Value: integer; BaseValue: integer);
    begin
      if Value<BaseValue then begin
        dec(Value,$80);
      end else begin
        inc(Value,$80);
      end;
      if (Value<0) or (Value>$ff) then begin
        if BaseValue<$80 then
          Value:=$ff
        else
          Value:=0;
      end;
    end;}
  
  var
    FGRed: Integer;
    FGGreen: Integer;
    FGBlue: Integer;
    RedDiff: integer;
    GreenDiff: integer;
    BlueDiff: integer;
  begin
    NewColor := TColor(ColorToRGB(NewColor));
    FGRed:=(NewColor shr 16) and $ff;
    FGGreen:=(NewColor shr 8) and $ff;
    FGBlue:=NewColor and $ff;
    RedDiff:=Abs(FGRed-BGRed);
    GreenDiff:=Abs(FGGreen-BGGreen);
    BlueDiff:=Abs(FGBlue -BGBlue);
    if RedDiff*RedDiff + GreenDiff*GreenDiff + BlueDiff*BlueDiff<30000 then
    begin
      NewColor:=InvertColor(NewColor);
      {IncreaseDiff(FGRed,BGRed);
      IncreaseDiff(FGGreen,BGGreen);
      IncreaseDiff(FGBlue,BGBlue);
      NewColor:=(FGRed shl 16) or (FGGreen shl 8) or FGBlue;}
    end;
    ACanvas.Font.Color:=NewColor;
  end;
  
  procedure WriteToken(var TokenStart, TokenEnd: integer);
  var
    CurToken: String;
  begin
    if TokenStart>=1 then begin
      CurToken:=copy(AKey,TokenStart,TokenEnd-TokenStart);
      if MeasureOnly then
        Inc(Result.X, ACanvas.TextWidth(CurToken))
      else
        ACanvas.TextOut(x+1, y, CurToken);
      x := x + ACanvas.TextWidth(CurToken);
      //debugln('Paint A Text="',CurToken,'" x=',dbgs(x),' y=',dbgs(y),' "',ACanvas.Font.Name,'" ',dbgs(ACanvas.Font.Height),' ',dbgs(ACanvas.TextWidth(CurToken)));
      TokenStart:=0;
    end;
  end;

  procedure PaintHighlighted(s: string);
  var
    sToken: PChar;
    nTokenLen: integer;
    Attr: TSynHighlightElement;
    CurForeground: TColor;
  begin
    if MeasureOnly then begin
      Inc(Result.X,ACanvas.TextWidth(s));
      exit;
    end;
    if (Highlighter<>nil) and (not ItemSelected) then begin
      Highlighter.ResetRange;
      Highlighter.SetLine(s,0);
      while not Highlighter.GetEol do begin
        Highlighter.GetTokenEx(sToken,nTokenLen);
        SetLength(s,nTokenLen);
        if nTokenLen>0 then begin
          System.Move(sToken^,s[1],nTokenLen);
          attr := Highlighter.GetTokenAttribute;
          CurForeground:=Attr.Foreground;
          if CurForeground=clNone then
            CurForeground:=TColor(ForegroundColor);
          SetFontColor(CurForeground);
          ACanvas.TextOut(x,y,s);
          inc(x,ACanvas.TextWidth(s));
        end;
        Highlighter.Next;
      end;
    end else begin
      SetFontColor(ForegroundColor);
      ACanvas.TextOut(x+1,y,s);
    end;
  end;

var
  i: Integer;
  s: string;
  IdentItem: TIdentifierListItem;
  AColor: TColor;
  ANode: TCodeTreeNode;
  ItemNode: TCodeTreeNode;
  SubNode: TCodeTreeNode;
  IsReadOnly: boolean;
  ImageIndex: longint;
  HintModifiers: TPascalHintModifiers;
  HintModifier: TPascalHintModifier;
begin
  ForegroundColor := ColorToRGB(ACanvas.Font.Color);
  Result.X := 0;
  Result.Y := ACanvas.TextHeight('W');
  if CurrentCompletionType=ctIdentCompletion then begin
    // draw
    IdentItem:=CodeToolBoss.IdentifierList.FilteredItems[Index];
    if IdentItem=nil then begin
      if not MeasureOnly then
        ACanvas.TextOut(x+1, y, 'PaintCompletionItem: BUG in codetools or misuse of PaintCompletionItem');
      exit;
    end;
    BackgroundColor:=ColorToRGB(ACanvas.Brush.Color);
    BGRed:=(BackgroundColor shr 16) and $ff;
    BGGreen:=(BackgroundColor shr 8) and $ff;
    BGBlue:=BackgroundColor and $ff;
    ImageIndex:=-1;

    // first write the type
    // var, procedure, property, function, type, const
    case IdentItem.GetDesc of

    ctnVarDefinition, ctnRecordCase:
      begin
        AColor:=clMaroon;
        s:='var';
      end;

    ctnTypeDefinition, ctnEnumerationType:
      begin
        AColor:=clLime;
        s:='type';
      end;

    ctnConstDefinition,ctnConstant:
      begin
        AColor:=clOlive;
        s:='const';
      end;
      
    ctnProcedure:
      begin
        if IdentItem.IsFunction then
        begin
          AColor:=clTeal;
          s:='function';
        end
        else
        begin
          AColor:=clNavy;
          if IdentItem.IsContructor then
            s := 'constructor'
          else
          if IdentItem.IsDestructor then
            s := 'destructor'
          else
            s:='procedure';
        end;
        if IdentItem.TryIsAbstractMethod then
          AColor:=clRed;
      end;
      
    ctnProperty:
      begin
        AColor:=clPurple;
        s:='property';
        IsReadOnly:=IdentItem.IsPropertyReadOnly;
        if IsReadOnly then
          ImageIndex:=IDEImages.LoadImage(16,'ce_property_readonly');
      end;
      
    ctnEnumIdentifier:
      begin
        AColor:=clOlive;
        s:='enum';
      end;
      
    ctnUnit, ctnUseUnit:
      begin
        AColor:=clBlack;
        s:='unit';
      end;

    ctnNone:
      if iliKeyword in IdentItem.Flags then begin
        AColor:=clBlack;
        s:='keyword';
      end else begin
        AColor:=clGray;
        s:='';
      end;

    else
      AColor:=clGray;
      s:='';
    end;

    SetFontColor(AColor);
    if MeasureOnly then
      Inc(Result.X, ACanvas.TextWidth('constructor '))
    else
      ACanvas.TextOut(x+1,y,s);
    inc(x,ACanvas.TextWidth('constructor '));
    if x>MaxX then exit;

    // paint the identifier
    SetFontColor(ForegroundColor);
    ACanvas.Font.Style:=ACanvas.Font.Style+[fsBold];
    s:=IdentItem.Identifier;
    with CodeToolBoss.SourceChangeCache.BeautifyCodeOptions do
      WordExceptions.CheckExceptions(s);
    if MeasureOnly then
      Inc(Result.X, 1+ACanvas.TextWidth(s))
    else begin
      //DebugLn(['PaintCompletionItem ',x,',',y,' ',s]);
      ACanvas.TextOut(x+1,y,s);
      inc(x,ACanvas.TextWidth(s));
      if x>MaxX then exit;
    end;
    ACanvas.Font.Style:=ACanvas.Font.Style-[fsBold];

    if ImageIndex <= 0 then
    begin
      HintModifiers := IdentItem.GetHintModifiers;
      for HintModifier in HintModifiers do
      begin
        ImageIndex := IDEImages.LoadImage(16, HintModifierImage[HintModifier]);
        break;
      end;
    end;

    // paint icon
    if ImageIndex>=0 then begin
      if MeasureOnly then
        Inc(Result.X, 18)
      else begin
        IDEImages.Images_16.Draw(ACanvas,x+1,y+(Result.Y-16) div 2,ImageIndex);
        inc(x,18);
        if x>MaxX then exit;
      end;
    end;
    
    // finally paint the type/value/parameters
    s:='';
    ItemNode:=IdentItem.Node;
    if ItemNode<>nil then begin
      case ItemNode.Desc of

      ctnProcedure:
        begin
          s:=IdentItem.Tool.ExtractProcHead(ItemNode,
            [phpWithoutClassName,phpWithoutName,phpWithVarModifiers,
             phpWithParameterNames,phpWithDefaultValues,phpWithResultType,
             phpWithOfObject,phpWithoutSemicolon]);
        end;

      ctnProperty:
        begin
          s:=IdentItem.Tool.ExtractProperty(ItemNode,
            [phpWithoutName,phpWithVarModifiers,
             phpWithParameterNames,phpWithDefaultValues,phpWithResultType]);
        end;

      ctnVarDefinition:
        begin
          ANode:=IdentItem.Tool.FindTypeNodeOfDefinition(ItemNode);
          s:=' : '+IdentItem.Tool.ExtractNode(ANode,[]);
        end;

      ctnTypeDefinition:
        begin
          ANode:=IdentItem.Tool.FindTypeNodeOfDefinition(ItemNode);
          s:=' = ';
          if (ANode<>nil) then begin
            case ANode.Desc of
            ctnClass,ctnObject,ctnObjCClass,ctnObjCCategory,
            ctnCPPClass,
            ctnClassInterface,ctnObjCProtocol,ctnDispinterface:
              begin
                case ANode.Desc of
                ctnClass: s:=s+'class';
                ctnObject: s:=s+'object';
                ctnObjCClass: s:=s+'objcclass';
                ctnObjCCategory: s:=s+'objccategory';
                ctnCPPClass: s:=s+'cppclass';
                ctnClassInterface: s:=s+'interface';
                ctnObjCProtocol: s:=s+'objcprotocol';
                ctnDispinterface: s:=s+'dispinterface';
                end;
                try
                  IdentItem.Tool.BuildSubTree(ANode);
                except
                  on ECodeToolError do ;
                end;
                SubNode:=IdentItem.Tool.FindInheritanceNode(ANode);
                if SubNode<>nil then
                  s:=s+IdentItem.Tool.ExtractNode(SubNode,[]);
              end;
            ctnRecordType:
              s:=s+'record';
            else
              s:=s+IdentItem.Tool.ExtractNode(ANode,[]);
            end;
          end else
            s:=s+'?';
        end;

      ctnConstDefinition:
        begin
          ANode:=IdentItem.Tool.FindTypeNodeOfDefinition(ItemNode);
          if ANode<>nil then
            s:=' = '+IdentItem.Tool.ExtractNode(ANode,[])
          else begin
            s:=IdentItem.Tool.ExtractCode(ItemNode.StartPos
                            +GetIdentLen(@IdentItem.Tool.Src[ItemNode.StartPos]),
                            ItemNode.EndPos,[]);
          end;
          s:=copy(s,1,50);
        end;

      ctnRecordCase:
        begin
          s:=' : '+IdentItem.Tool.ExtractRecordCaseType(ItemNode);
        end;

      end;
    end else begin
      // IdentItem.Node=nil
      case IdentItem.GetDesc of
      ctnProcedure:
        // predefined procedure (e.g. length)
        begin
          s:=IdentItem.ParamNameList;
          if s<>'' then
            s:='('+s+')';
          if IdentItem.IsFunction then
            s := s + ':' + IdentItem.ResultType;
          s:=s+';'
        end;
      end;
    end;
    
    if s<>'' then begin
      inc(x);
      PaintHighlighted(s);
    end;

  end else begin
    // parse AKey for text and style
    i := 1;
    TokenStart:=0;
    while i <= Length(AKey) do begin
      case AKey[i] of
      #1, #2:
        begin
          WriteToken(TokenStart,i);
          // set color
          ACanvas.Font.Color := (Ord(AKey[i + 3]) shl 8
                        + Ord(AKey[i + 2])) shl 8
                        + Ord(AKey[i + 1]);
          inc(i, 4);
        end;
      #3:
        begin
          WriteToken(TokenStart,i);
          // set style
          case AKey[i + 1] of
          'B': ACanvas.Font.Style := ACanvas.Font.Style + [fsBold];
          'b': ACanvas.Font.Style := ACanvas.Font.Style - [fsBold];
          'U': ACanvas.Font.Style := ACanvas.Font.Style + [fsUnderline];
          'u': ACanvas.Font.Style := ACanvas.Font.Style - [fsUnderline];
          'I': ACanvas.Font.Style := ACanvas.Font.Style + [fsItalic];
          'i': ACanvas.Font.Style := ACanvas.Font.Style - [fsItalic];
          end;
          inc(i, 2);
        end;
      else
        if TokenStart<1 then TokenStart:=i;
        inc(i);
      end;
    end;
    WriteToken(TokenStart,i);
  end;
end;

function FindUnitName(IdentList: TIdentifierList;
  IdentItem: TIdentifierListItem): string;
var
  CodeBuf: TCodeBuffer;
begin
  Result:=IdentItem.Identifier;
  CodeBuf:=CodeToolBoss.FindUnitSource(IdentList.StartContextPos.Code,Result,'');
  if CodeBuf=nil then exit;
  Result:=CodeToolBoss.GetSourceName(CodeBuf,true);
  if Result='' then
    Result:=IdentItem.Identifier;
end;

function GetIdentCompletionValue(aCompletion : TSynCompletion;
  AddChar: TUTF8Char;
  out ValueType: TIdentComplValue; out CursorToLeft: integer): string;
var
  Index: Integer;
  IdentItem: TIdentifierListItem;
  IdentList: TIdentifierList;
  CursorAtEnd: boolean;
  ProcModifierPos: LongInt;
  ProcHeadFlags: TProcHeadAttributes;
  CanAddSemicolon: Boolean;
  CanAddComma: Boolean;
  ClassNode: TCodeTreeNode;
  IsReadOnly: Boolean;
  Line: string;
  Indent: LongInt;
  StartContextPos: TCodeXYPosition;
begin
  Result:='';
  CursorToLeft:=0;
  CursorAtEnd:=true;
  ValueType:=icvIdentifier;
  Index:=aCompletion.Position;
  IdentList:=CodeToolBoss.IdentifierList;

  IdentItem:=IdentList.FilteredItems[Index];
  if IdentItem=nil then begin
    ValueType := icvNone;
    exit;
  end;

  CodeToolBoss.IdentItemCheckHasChilds(IdentItem);

  CanAddSemicolon:=CodeToolsOpts.IdentComplAddSemicolon and (AddChar<>';');
  CanAddComma:=(AddChar<>',');
  IsReadOnly:=false;

  Result:=IdentItem.Identifier;
  with CodeToolBoss.SourceChangeCache.BeautifyCodeOptions do
    WordExceptions.CheckExceptions(Result);

  case IdentItem.GetDesc of

    ctnProcedure:
    begin
      if (ilcfCanProcDeclaration in IdentList.ContextFlags)
      and (IdentItem.Node<>nil) then begin
        //DebugLn(['GetIdentCompletionValue icvCompleteProcDeclaration']);
        ValueType:=icvCompleteProcDeclaration;
      end else if IdentItem.IsProcNodeWithParams then
        ValueType:=icvProcWithParams;
    end;

    ctnProperty:
      begin
        if IdentItem.IsPropertyWithParams then
          ValueType:=icvIndexedProp;
        IsReadOnly:=IdentItem.IsPropertyReadOnly;
      end;

    ctnUnit, ctnPackage, ctnLibrary:
      ValueType:=icvUnitName;
  end;

  case ValueType of
  
    icvProcWithParams:
      // add brackets for parameter lists
      if (AddChar='')
      and CodeToolsOpts.IdentComplAddParameterBrackets
      and (ilcfStartInStatement in IdentList.ContextFlags)
      and (not IdentList.StartUpAtomBehindIs('('))
      and (not IdentList.StartUpAtomInFrontIs('@'))
      and (IdentItem.ParamNameList<>'') then begin
        Result:=Result+'()';
        inc(CursorToLeft);
        CursorAtEnd:=false;
      end;

    icvIndexedProp:
      // add brackets for parameter lists
      if (AddChar='')
      and CodeToolsOpts.IdentComplAddParameterBrackets
      and (ilcfStartInStatement in IdentList.ContextFlags)
      and (not IdentList.StartUpAtomBehindIs('[')) then begin
        Result:=Result+'[]';
        inc(CursorToLeft);
        CursorAtEnd:=false;
      end;
      
    icvCompleteProcDeclaration:
      // create complete procedure declaration
      if (AddChar='')
      and (IdentList.StartAtomBehind.Flag in [cafEnd,cafWord,cafSemicolon])
      and (ilcfEndOfLine in IdentList.ContextFlags)
      and (IdentItem.Node<>nil) then begin
        ProcHeadFlags:=[phpWithStart,phpWithVarModifiers,phpWithParameterNames,
           phpWithDefaultValues,phpWithResultType,phpWithCallingSpecs,
           phpWithProcModifiers];
        if IdentList.StartUpAtomInFrontIs('PROCEDURE')
        or IdentList.StartUpAtomInFrontIs('FUNCTION')
        or IdentList.StartUpAtomInFrontIs('CONSTRUCTOR')
        or IdentList.StartUpAtomInFrontIs('DESTRUCTOR')
        then
          Exclude(ProcHeadFlags,phpWithStart);
        Result:=IdentItem.Tool.ExtractProcHead(IdentItem.Node,ProcHeadFlags);
        ClassNode:=IdentItem.Tool.FindClassOrInterfaceNode(IdentItem.Node);
        if (ClassNode<>nil)
        and (ClassNode.Desc in [ctnClass,ctnObjCClass]) then begin
          // replace virtual and dynamic with override
          ProcModifierPos:=System.Pos('VIRTUAL;',UpperCaseStr(Result));
          if ProcModifierPos<1 then
            ProcModifierPos:=System.Pos('DYNAMIC;',UpperCaseStr(Result));
          if ProcModifierPos>0 then
            Result:=copy(Result,1,ProcModifierPos-1)+'override;'
                    +copy(Result,ProcModifierPos+8,length(Result));
        end;
        // remove abstract
        ProcModifierPos:=System.Pos('ABSTRACT;',UpperCaseStr(Result));
        if ProcModifierPos>0 then
          Result:=copy(Result,1,ProcModifierPos-1)
                  +copy(Result,ProcModifierPos+9,length(Result));
        StartContextPos:=CodeToolBoss.IdentifierList.StartContextPos;
        Line:=StartContextPos.Code.GetLine(StartContextPos.Y-1);
        Indent:=StartContextPos.X;
        //debugln(['GetIdentCompletionValue ',Indent,' "',dbgstr(Line),'" ',GetLineIndent(Line,1),' empty=',InEmptyLine(Line,1),' ',DbgsCXY(StartContextPos)]);
        if not InEmptyLine(Line,1) then
          Indent:=GetLineIndent(Line,1);
        Result:=TrimLeft(CodeToolBoss.SourceChangeCache
          .BeautifyCodeOptions.BeautifyProc(Result,Indent,false));
        //debugln(['GetIdentCompletionValue ',dbgstr(Result),' LineLen=',CodeToolBoss.SourceChangeCache.BeautifyCodeOptions.LineLength]);
        CanAddSemicolon:=false;
      end;

    icvUnitName:
      begin
        Result:=FindUnitName(IdentList,IdentItem);
      end;
  end;

  if CursorAtEnd then ;

  // add assignment operator :=
  //debugln(['GetIdentCompletionValue CursorToLeft=',CursorToLeft,' AddChar=',AddChar,' ilcfStartOfStatement=',ilcfStartOfStatement in IdentList.ContextFlags,' ilcfEndOfLine=',ilcfEndOfLine in IdentList.ContextFlags]);
  if (CursorToLeft=0)
  and (AddChar='')
  and (ilcfStartOfStatement in IdentList.ContextFlags)
  and ((ilcfEndOfLine in IdentList.ContextFlags) or IdentList.StartUpAtomBehindIs(';'))
  and (not IdentItem.HasChilds)
  and (not IdentItem.HasIndex)
  and (not IsReadOnly)
  and (not IdentList.StartUpAtomBehindIs(':='))
  and (not IdentList.StartUpAtomBehindIs('('))
  and (IdentItem.CanBeAssigned)
  and CodeToolsOpts.IdentComplAddAssignOperator then begin
    if (atIdentifier in CodeToolsOpts.DoInsertSpaceAfter)
    or (atSymbol in CodeToolsOpts.DoInsertSpaceInFront) then
      Result:=Result+' ';
    Result:=Result+':=';
    if (atSymbol in CodeToolsOpts.DoInsertSpaceAfter) then
      Result:=Result+' ';
  end;

  // add last typed character (that ended the identifier completion and starts a new token)
  if AddChar<>'' then
    Result:=Result+AddChar;

  if CanAddComma
  and (ilcfNeedsEndComma in IdentList.ContextFlags) then
  begin
    Result:=Result+',';
  end;

  if IdentItem.GetDesc=ctnUseUnit then begin
    Result:=Result+'.';
  end;

  // add semicolon for statement ends
  //debugln(['GetIdentCompletionValue CanAddSemicolon=',CanAddSemicolon,' ilcfNoEndSemicolon=',ilcfNoEndSemicolon in IdentList.ContextFlags,' ']);
  if CanAddSemicolon
  and (not (ilcfNoEndSemicolon in IdentList.ContextFlags))
  then begin
    if (ilcfNeedsEndSemicolon in IdentList.ContextFlags)
    or ((ilcfStartInStatement in IdentList.ContextFlags)
        and (IdentItem.GetDesc=ctnProcedure))
    then begin
      Result:=Result+';';
      if (CursorToLeft=0) and (IdentItem.GetDesc=ctnProcedure)
      and (not IdentItem.IsFunction) then begin
        // a procedure call without parameters
        // => put cursor behind semicolon
      end else begin
        // keep cursor in front of semicolon
        inc(CursorToLeft);
      end;
    end;
  end;

  //DebugLn(['GetIdentCompletionValue END Result="',Result,'"']);
end;

function BreakLinesInText(const s: string; MaxLineLength: integer): string;
begin
  Result:=BreakString(s,MaxLineLength,GetLineIndent(s,1));
end;

procedure InitSynREEngine;
begin
  if SynREEngine=nil then
    SynREEngine:=TRegExpr.Create;
end;

function SynREMatches(const TheText, RegExpr, ModifierStr: string;
  StartPos: integer): boolean;
begin
  InitSynREEngine;
  SynREEngine.ModifierStr:=ModifierStr;
  SynREEngine.Expression:=RegExpr;
  SynREEngine.InputString:=TheText;
  Result:=SynREEngine.ExecPos(StartPos);
end;

function SynREVar(Index: Integer): string;
begin
  if SynREEngine<>nil then
    Result:=SynREEngine.Match[Index]
  else
    Result:='';
end;

procedure SynREVarPos(Index: Integer; out MatchStart, MatchLength: integer);
begin
  if SynREEngine<>nil then begin
    MatchStart:=SynREEngine.MatchPos[Index];
    MatchLength:=SynREEngine.MatchLen[Index];
  end else begin
    MatchStart:=-1;
    MatchLength:=-1;
  end;
end;

function SynREVarCount: Integer;
begin
  if SynREEngine<>nil then
    Result:=SynREEngine.SubExprMatchCount
  else
    Result:=0;
end;

function SynREReplace(const TheText, FindRegExpr, ReplaceRegExpr: string;
  UseSubstutition: boolean; const ModifierStr: string): string;
begin
  InitSynREEngine;
  SynREEngine.ModifierStr:=ModifierStr;
  SynREEngine.Expression:=FindRegExpr;
  Result:=SynREEngine.Replace(TheText,ReplaceRegExpr,UseSubstutition);
end;

procedure SynRESplit(const TheText, SeparatorRegExpr: string; Pieces: TStrings;
  const ModifierStr: string);
begin
  InitSynREEngine;
  SynREEngine.ModifierStr:=ModifierStr;
  SynREEngine.Expression:=SeparatorRegExpr;
  SynREEngine.Split(TheText,Pieces);
end;

{ TLazTextConverterToolClasses }

function TLazTextConverterToolClasses.GetTempFilename: string;
var
  BaseDir: String;
begin
  BaseDir:='';
  if LazarusIDE.ActiveProject<>nil then
    BaseDir:=ExtractFilePath(LazarusIDE.ActiveProject.ProjectInfoFile);
  if BaseDir='' then
    BaseDir:=LazarusIDE.GetTestBuildDirectory;
  if BaseDir='' then
    BaseDir:=GetCurrentDirUTF8;
  BaseDir:=CleanAndExpandDirectory(BaseDir);
  Result:=FileProcs.GetTempFilename(BaseDir,'convert_');
end;

function TLazTextConverterToolClasses.LoadFromFile(
  Converter: TIDETextConverter; const AFilename: string; UpdateFromDisk,
  Revert: Boolean): Boolean;
var
  TheFilename: String;
  CodeBuf: TCodeBuffer;
  TargetCodeBuffer: TCodeBuffer;
begin
  TheFilename:=TrimAndExpandFilename(AFilename);
  if TheFilename='' then exit(false);
  CodeBuf:=CodeToolBoss.FindFile(TheFilename);
  if CodeBuf=nil then begin
    // it is not in cache
    // to save memory do not load it into the cache and use the default way
    //DebugLn(['TLazTextConverterToolClasses.LoadFromFile not in cache, using default ...']);
    Result:=Converter.LoadFromFile(AFilename,false,UpdateFromDisk,Revert);
  end else begin
    // use cache
    //DebugLn(['TLazTextConverterToolClasses.LoadFromFile using cache']);
    CodeBuf:=CodeToolBoss.LoadFile(TheFilename,UpdateFromDisk,Revert);
    if CodeBuf=nil then
      exit(false);
    Result:=true;
    //DebugLn(['TLazTextConverterToolClasses.LoadFromFile Converter.CurrentType=',ord(Converter.CurrentType)]);
    case Converter.CurrentType of
    tctSource:
      Converter.Source:=CodeBuf.Source;
    tctFile:
      Result:=SaveStringToFile(Converter.Filename,CodeBuf.Source,[])=mrOk;
    tctStrings:
      CodeBuf.AssignTo(Converter.Strings,true);
    tctCodeBuffer:
      begin
        if Converter.CodeBuffer=nil then
          Converter.CodeBuffer:=CodeBuf
        else begin
          TargetCodeBuffer:=(TObject(Converter.CodeBuffer) as TCodeBuffer);
          if TargetCodeBuffer<>CodeBuf then
            TargetCodeBuffer.Source:=CodeBuf.Source;
        end;
      end;
    end;
  end;
end;

function TLazTextConverterToolClasses.SaveCodeBufferToFile(
  Converter: TIDETextConverter; const AFilename: string): Boolean;
begin
  Result:=(TObject(Converter.CodeBuffer) as TCodeBuffer).SaveToFile(AFilename);
end;

function TLazTextConverterToolClasses.GetCodeBufferSource(
  Converter: TIDETextConverter; out Source: string): boolean;
begin
  Result:=true;
  Source:=(TObject(Converter.CodeBuffer) as TCodeBuffer).Source;
end;

function TLazTextConverterToolClasses.CreateCodeBuffer(
  Converter: TIDETextConverter; const Filename, NewSource: string; out
  CodeBuffer: Pointer): boolean;
begin
  CodeBuffer:=CodeToolBoss.CreateFile(Filename);
  if CodeBuffer<>nil then begin
    TCodeBuffer(CodeBuffer).Source:=NewSource;
    Result:=true;
  end else
    Result:=false;
end;

function TLazTextConverterToolClasses.LoadCodeBufferFromFile(
  Converter: TIDETextConverter; const Filename: string;
  UpdateFromDisk, Revert: Boolean; out CodeBuffer: Pointer): boolean;
begin
  CodeBuffer:=CodeToolBoss.LoadFile(Filename,UpdateFromDisk,Revert);
  Result:=CodeBuffer<>nil;
end;

procedure TLazTextConverterToolClasses.AssignCodeToolBossError(
  Target: TCustomTextConverterTool);
begin
  Target.ErrorMsg:=CodeToolBoss.ErrorMessage;
  Target.ErrorLine:=CodeToolBoss.ErrorLine;
  Target.ErrorColumn:=CodeToolBoss.ErrorColumn;
  Target.ErrorTopLine:=CodeToolBoss.ErrorTopLine;
  if CodeToolBoss.ErrorCode<>nil then
    Target.ErrorFilename:=CodeToolBoss.ErrorCode.Filename
  else
    Target.ErrorFilename:='';
end;

function TLazTextConverterToolClasses.SupportsType(aTextType: TTextConverterType
  ): boolean;
begin
  Result:=true;
end;

initialization
  REException:=ERegExpr;
  REMatchesFunction:=@SynREMatches;
  REVarFunction:=@SynREVar;
  REVarPosProcedure:=@SynREVarPos;
  REVarCountFunction:=@SynREVarCount;
  REReplaceProcedure:=@SynREReplace;
  RESplitFunction:=@SynRESplit;

finalization
  FreeAndNil(SynREEngine);

end.

