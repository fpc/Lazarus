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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  Support functions and types for the source editor.

}
unit SourceEditProcs;

{$mode objfpc}{$H+}

{$I ide.inc}

interface

uses
  Classes, SysUtils, RegExpr, LCLProc, LCLType, Graphics, Controls,
  SynCompletion, BasicCodeTools, CodeTree,
  CodeAtom, CodeCache, SourceChanger, CustomCodeTool, CodeToolManager,
  PascalParserTool, KeywordFuncLists, FileProcs, IdentCompletionTool,
  PascalReaderTool,
  LazIDEIntf, IDEImagesIntf, TextTools, IDETextConverter,
  DialogProcs, LazFileUtils, EditorOptions, CodeToolsOptions;

type

  { TLazTextConverterToolClasses }

  TLazTextConverterToolClasses = class(TTextConverterToolClasses)
  public
    function GetTempFilename: string; override;
    function SupportsType({%H-}aTextType: TTextConverterType): boolean; override;
    function LoadFromFile(Converter: TIDETextConverter; const AFilename: string;
                          UpdateFromDisk, Revert: Boolean): Boolean; override;
    function SaveCodeBufferToFile(Converter: TIDETextConverter;
                           const AFilename: string): Boolean; override;
    function GetCodeBufferSource(Converter: TIDETextConverter;
                                 out Source: string): boolean; override;
    function CreateCodeBuffer({%H-}Converter: TIDETextConverter;
                              const Filename, NewSource: string;
                              out CodeBuffer: Pointer): boolean; override;
    function LoadCodeBufferFromFile({%H-}Converter: TIDETextConverter;
                                   const Filename: string;
                                   UpdateFromDisk, Revert: Boolean;
                                   out CodeBuffer: Pointer): boolean; override;
    procedure AssignCodeToolBossError(Target: TCustomTextConverterTool); override;
  end;

  TLazIdentifierListItem = class(TIdentifierListItem)
  private
    FBeautified: Boolean;
  public
    procedure BeautifyIdentifier({%H-}IdentList: TIdentifierList); override;
  end;

  TLazUnitNameSpaceIdentifierListItem = class(TUnitNameSpaceIdentifierListItem)
  private
    FBeautified: Boolean;
  public
    procedure BeautifyIdentifier(IdentList: TIdentifierList); override;
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

  TPaintCompletionItemColors = record
    BackgroundColor: TColor;
    BackgroundSelectedColor: TColor;
    TextColor: TColor;
    TextSelectedColor: TColor;
    TextHilightColor: TColor;
  end;
  PPaintCompletionItemColors = ^TPaintCompletionItemColors;

// completion form and functions
function PaintCompletionItem(const AKey: string; ACanvas: TCanvas;
  X, Y, MaxX: integer; ItemSelected: boolean; Index: integer;
  {%H-}aCompletion : TSynCompletion; CurrentCompletionType: TCompletionType;
  Highlighter: TSrcIDEHighlighter; Colors: PPaintCompletionItemColors;
  MeasureOnly: Boolean = False): TPoint;

function GetIdentCompletionValue(aCompletion : TSynCompletion;
  AddChar: TUTF8Char;
  out ValueType: TIdentComplValue; out CursorToLeft: integer): string;
function BreakLinesInText(const s: string; MaxLineLength: integer): string;

const
  ctnWord = ctnUser + 1;
  WordCompatibility = icompUnknown;

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

function PaintCompletionItem(const AKey: string; ACanvas: TCanvas; X, Y,
  MaxX: integer; ItemSelected: boolean; Index: integer;
  aCompletion: TSynCompletion; CurrentCompletionType: TCompletionType;
  Highlighter: TSrcIDEHighlighter; Colors: PPaintCompletionItemColors;
  MeasureOnly: Boolean): TPoint;

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
  BackgroundColor: TColor;
  ForegroundColor: TColor;
  TextHilightColor: TColor;
  AllowFontColor: Boolean;

  procedure SetFontColor(NewColor: TColor; Force: boolean = false);
  
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
    if (not AllowFontColor) and (not Force) then
      Exit;

    NewColor := TColor(ColorToRGB(NewColor));
    FGRed:=(NewColor shr 16) and $ff;
    FGGreen:=(NewColor shr 8) and $ff;
    FGBlue:=NewColor and $ff;
    RedDiff:=Abs(FGRed-BGRed);
    GreenDiff:=Abs(FGGreen-BGGreen);
    BlueDiff:=Abs(FGBlue-BGBlue);
    {if ItemSelected then
      writeln('SetFontColor ',RedDiff,'=',FGRed,'-',BGRed,' ',
         GreenDiff,'=',FGGreen,'-',BGGreen,' ',
         BlueDiff,'=',FGBlue,'-',BGBlue);}
    if RedDiff*RedDiff + GreenDiff*GreenDiff + BlueDiff*BlueDiff<30000 then
    begin
      NewColor:=InvertColor(NewColor);
      {IncreaseDiff(FGRed,BGRed);
      IncreaseDiff(FGGreen,BGGreen);
      IncreaseDiff(FGBlue,BGBlue);
      NewColor:=(FGRed shl 16) or (FGGreen shl 8) or FGBlue;}
    end;
    ACanvas.Font.Color:=NewColor;
    //debugln(['SetFontColor ',NewColor,' ',ACanvas.Font.Color]);
  end;
  
  procedure WriteToken(var TokenStart, TokenEnd: integer);
  var
    CurToken: String;
  begin
    if TokenStart>=1 then begin
      CurToken:=copy(AKey,TokenStart,TokenEnd-TokenStart);
      if MeasureOnly then
        Inc(Result.X, ACanvas.TextWidth(CurToken))
      else begin
        //debugln(['WriteToken ',CurToken,' ',ACanvas.Font.Color]);
        ACanvas.TextOut(x+1, y, CurToken);
      end;
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
    LeftText: string;
  begin
    if MeasureOnly then begin
      Inc(Result.X,ACanvas.TextWidth(s));
      exit;
    end;
    if (Highlighter<>nil) and AllowFontColor then begin
      LeftText := '';
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
          ACanvas.TextOut(x+1+ACanvas.TextWidth(LeftText),y,s);
          LeftText += s;
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
  UseImages: boolean;
  ImageIndex, ImageIndexCC: longint;
  Token: String;
  PrefixPosition: Integer;
  HintModifiers: TPascalHintModifiers;
  HintModifier: TPascalHintModifier;
  HelperForNode: TCodeTreeNode;
begin
  if Colors<>nil then
  begin
    if ItemSelected then
    begin
      AllowFontColor := Colors^.TextSelectedColor=clNone;
      if AllowFontColor then
        ForegroundColor := ColorToRGB(Colors^.TextColor)
      else
        ForegroundColor := ColorToRGB(Colors^.TextSelectedColor);
      BackgroundColor:=ColorToRGB(Colors^.BackgroundSelectedColor);
    end else
    begin
      ForegroundColor := ColorToRGB(Colors^.TextColor);
      AllowFontColor := True;
      BackgroundColor:=ColorToRGB(Colors^.BackgroundColor);
    end;
    TextHilightColor:=ColorToRGB(Colors^.TextHilightColor);
  end else
  begin
    ForegroundColor := clBlack;
    AllowFontColor := True;
    BackgroundColor:=ColorToRGB(ACanvas.Brush.Color);
    TextHilightColor := clWhite;
  end;

  BGRed:=(BackgroundColor shr 16) and $ff;
  BGGreen:=(BackgroundColor shr 8) and $ff;
  BGBlue:=BackgroundColor and $ff;
  ForegroundColor := ColorToRGB(ForegroundColor);
  SetFontColor(ForegroundColor,true);

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
    IdentItem.BeautifyIdentifier(CodeToolBoss.IdentifierList);
    ItemNode:=IdentItem.Node;
    ImageIndex:=-1;
    ImageIndexCC := -1;
    UseImages := CodeToolsOpts.IdentComplShowIcons;

    // first write the type
    // var, procedure, property, function, type, const
    case IdentItem.GetDesc of

    ctnVarDefinition, ctnRecordCase:
      begin
        if UseImages then
          ImageIndexCC := IDEImages.LoadImage('cc_variable', 16)
        else begin
          AColor:=clMaroon;
          s:='var';
        end;
      end;

    ctnTypeDefinition, ctnEnumerationType:
      begin
        if UseImages then
        begin
          if ItemNode <> nil then
            begin
              ANode := IdentItem.Tool.FindTypeNodeOfDefinition(ItemNode);
              case ANode.Desc of
                ctnClass:
                  ImageIndexCC := IDEImages.LoadImage('cc_class', 16);
                ctnRecordType:
                  ImageIndexCC := IDEImages.LoadImage('cc_record', 16);
                ctnEnumerationType:
                  ImageIndexCC := IDEImages.LoadImage('cc_enum', 16);
                else
                  ImageIndexCC := IDEImages.LoadImage('cc_type', 16);
              end;
            end
          else
            ImageIndexCC := IDEImages.LoadImage('cc_type', 16);
        end
        else
        begin
          AColor:=clLime;
          s:='type';
        end;
      end;

    ctnConstDefinition,ctnConstant:
      begin
        AColor:=clOlive;
        s:='const';
        if UseImages then
          ImageIndexCC := IDEImages.LoadImage('cc_constant', 16);
      end;
      
    ctnProcedure:
      begin
        if UseImages then
        begin
          if IdentItem.IsFunction then
            ImageIndexCC := IDEImages.LoadImage('cc_function', 16)
          else if IdentItem.IsConstructor then
            ImageIndexCC := IDEImages.LoadImage('cc_constructor', 16)
          else if IdentItem.IsDestructor then
            ImageIndexCC := IDEImages.LoadImage('cc_destructor', 16)
          else
            ImageIndexCC := IDEImages.LoadImage('cc_procedure', 16);
        end
        else
        begin
          if IdentItem.IsFunction then
            begin
              AColor:=clTeal;
              s:='function';
            end
          else
            begin
              AColor:=clNavy;
              if IdentItem.IsConstructor then
                s := 'constructor'
              else if IdentItem.IsDestructor then
                s := 'destructor'
              else
                s:='procedure';
            end;
          if IdentItem.TryIsAbstractMethod then
            AColor:=clRed;
          if iliHasLowerVisibility in IdentItem.Flags then
            AColor:=clGray;
        end;
      end;
      
    ctnProperty,ctnGlobalProperty:
      begin
        IsReadOnly:=IdentItem.IsPropertyReadOnly;
        if UseImages then
          begin
            if IsReadOnly then
              ImageIndexCC := IDEImages.LoadImage('cc_property_ro', 16)
            else
              ImageIndexCC := IDEImages.LoadImage('cc_property', 16);
          end
        else
          begin
            AColor:=clPurple;
            s:='property';
            if IsReadOnly then
              ImageIndex:=IDEImages.LoadImage('ce_property_readonly');
          end;
      end;

    ctnEnumIdentifier:
      begin
        if UseImages then
          ImageIndexCC := IDEImages.LoadImage('cc_enum', 16)
        else
          begin
            AColor:=clOlive;
            s:='enum';
          end;
      end;
      
    ctnLabel:
      begin
        if UseImages then
          ImageIndexCC := IDEImages.LoadImage('cc_label', 16)
        else
          begin
            AColor:=clOlive;
            s:='label';
          end;
      end;

    ctnUnit, ctnUseUnitClearName:
      begin
        if UseImages then
          ImageIndexCC := IDEImages.LoadImage('cc_unit', 16)
        else
          begin
            AColor:=clBlack;
            s:='unit';
          end;
      end;

    ctnUseUnitNamespace:
      begin
        if UseImages then
          ImageIndexCC := IDEImages.LoadImage('cc_namespace', 16)
        else
          begin
            AColor:=clBlack;
            s:='namespace';
          end;
      end;

    ctnWord:
      begin
        AColor:=clGray;
        s:='text';
      end;

    ctnNone:
      if not UseImages then
      begin
        if iliKeyword in IdentItem.Flags then begin
          AColor:=clBlack;
          s:='keyword';
        end else begin
          AColor:=clGray;
          s:='';
        end;
      end;

    else
      AColor:=clGray;
      s:='';
    end;

    if UseImages then
    begin
     // drawing type image
      if MeasureOnly then
        Inc(Result.X, IDEImages.Images_16.Width + round(IDEImages.Images_16.Width / 4))
      else
        begin
          if ImageIndexCC >= 0 then
            IDEImages.Images_16.Draw(ACanvas, x+1, y+(Result.Y-IDEImages.Images_16.Height) div 2, ImageIndexCC);
        end;
      Inc(x,IDEImages.Images_16.Width + round(IDEImages.Images_16.Width / 4));
    end
    else
    begin
      SetFontColor(AColor);
      if MeasureOnly then
        Inc(Result.X, ACanvas.TextWidth('constructor '))
      else
        ACanvas.TextOut(x+1,y,s);
      inc(x,ACanvas.TextWidth('constructor '));
    end;

    if x>MaxX then exit;

    // paint the identifier
    SetFontColor(ForegroundColor);
    ACanvas.Font.Style:=ACanvas.Font.Style+[fsBold];
    s:=IdentItem.Identifier;
    if MeasureOnly then
      Inc(Result.X, 1+ACanvas.TextWidth(s))
    else begin
      //DebugLn(['PaintCompletionItem ',x,',',y,' ',s]);
      // highlighting the prefix
      if (Colors<>nil) and (TextHilightColor<>clNone)
      and (aCompletion.CurrentString<>'') then
      begin
        PrefixPosition := Pos(LowerCase(aCompletion.CurrentString), LowerCase(s));
        if PrefixPosition > 0 then
        begin
          // paint before prefix
          Token := Copy(s, 1, PrefixPosition-1);
          ACanvas.TextOut(x+1,y,Token);
          // paint highlight prefix
          SetFontColor(TextHilightColor);
          Token := Copy(s, PrefixPosition, Length(aCompletion.CurrentString));
          ACanvas.TextOut(x+1+ACanvas.TextWidth(Copy(s, 1, PrefixPosition-1)),y,Token);
          // paint after prefix
          SetFontColor(ForegroundColor);
          Token := Copy(s, PrefixPosition+Length(aCompletion.CurrentString), High(Integer));
          ACanvas.TextOut(x+1+ACanvas.TextWidth(Copy(s, 1, PrefixPosition-1+Length(aCompletion.CurrentString))),y,Token);
        end else
          ACanvas.TextOut(x+1,y,s);
      end else
        ACanvas.TextOut(x+1,y,s);
      inc(x,ACanvas.TextWidth(s)+1);
      if x>MaxX then exit;
    end;
    SetFontColor(ForegroundColor);
    ACanvas.Font.Style:=ACanvas.Font.Style-[fsBold];

    if ImageIndex <= 0 then
    begin
      HintModifiers := IdentItem.GetHintModifiers;
      for HintModifier in HintModifiers do
      begin
        ImageIndex := IDEImages.LoadImage(HintModifierImage[HintModifier]);
        break;
      end;
    end;

    // paint icon
    if not UseImages then
    begin
      if ImageIndex>=0 then begin
        if MeasureOnly then
          Inc(Result.X, 18)
        else begin
          IDEImages.Images_16.Draw(ACanvas,x+1,y+(Result.Y-16) div 2,ImageIndex);
          inc(x,18);
          if x>MaxX then exit;
        end;
      end;
    end;

    // finally paint the type/value/parameters
    s:='';
    if ItemNode<>nil then begin
      case ItemNode.Desc of

      ctnProcedure:
        begin
          s:=IdentItem.Tool.ExtractProcHead(ItemNode,
            [phpWithoutClassName,phpWithoutName,phpWithVarModifiers,
             phpWithParameterNames,phpWithDefaultValues,phpWithResultType,
             phpWithOfObject,phpWithoutSemicolon]);
        end;

      ctnProperty,ctnGlobalProperty:
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
            ctnClassInterface,ctnObjCProtocol,ctnDispinterface,
            ctnClassHelper,ctnRecordHelper,ctnTypeHelper:
              begin
                case ANode.Desc of
                ctnClass: s:=s+'class';
                ctnClassHelper: s:=s+'class helper';
                ctnRecordHelper: s:=s+'record helper';
                ctnTypeHelper: s:=s+'type helper';
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
                if ANode.Desc in [ctnClassHelper, ctnRecordHelper, ctnTypeHelper] then
                  HelperForNode := IdentItem.Tool.FindHelperForNode(ANode)
                else
                  HelperForNode := nil;
                SubNode:=IdentItem.Tool.FindInheritanceNode(ANode);
                if SubNode<>nil then
                  s:=s+IdentItem.Tool.ExtractNode(SubNode,[]);
                if HelperForNode<>nil then
                  s:=s+' '+IdentItem.Tool.ExtractNode(HelperForNode,[]);
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
    //debugln(['PaintCompletionItem WordCompletion:']);
    i := 1;
    TokenStart:=0;
    while i <= Length(AKey) do begin
      case AKey[i] of
      #1, #2:
        begin
          WriteToken(TokenStart,i);
          // set color
          ForegroundColor:=(Ord(AKey[i + 3]) shl 8
                          + Ord(AKey[i + 2])) shl 8
                          + Ord(AKey[i + 1]);
          SetFontColor(ForegroundColor);
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
  //debugln(['PaintCompletionItem END']);
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
  s: String;
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

  IdentItem.BeautifyIdentifier(IdentList);
  CodeToolBoss.IdentItemCheckHasChilds(IdentItem);

  CanAddSemicolon:=CodeToolsOpts.IdentComplAddSemicolon and (AddChar<>';');
  CanAddComma:=CodeToolsOpts.IdentComplAddSemicolon and (AddChar<>',');
  IsReadOnly:=false;

  Result:=IdentItem.Identifier;

  //debugln(['GetIdentCompletionValue IdentItem.GetDesc=',NodeDescriptionAsString(IdentItem.GetDesc),' IdentList.ContextFlags=',dbgs(IdentList.ContextFlags),' IdentItem.Node=',IdentItem.Node<>nil]);

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

    ctnUnit, ctnPackage, ctnLibrary, ctnUseUnitNamespace:
      ValueType:=icvUnitName;
  end;

  //Add the '&' character to prefixed identifiers
  if (iliNeedsAmpersand in IdentItem.Flags) then
    Result := '&' + Result;

  case ValueType of
  
    icvProcWithParams:
      // add brackets for parameter lists
      if (AddChar='')
      and CodeToolsOpts.IdentComplAddParameterBrackets
      and (ilcfStartInStatement in IdentList.ContextFlags)
      and (not IdentList.StartUpAtomBehindIs('('))
      and (not IdentList.StartUpAtomInFrontIs('@'))
      and (IdentItem.ParamNameList<>'') then begin
        Result+='()';
        inc(CursorToLeft);
        CursorAtEnd:=false;
      end;

    icvIndexedProp:
      // add brackets for parameter lists
      if (AddChar='')
      and CodeToolsOpts.IdentComplAddParameterBrackets
      and (ilcfStartInStatement in IdentList.ContextFlags)
      and (not IdentList.StartUpAtomBehindIs('[')) then begin
        Result+='[]';
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
        Line:=StartContextPos.Code.GetLine(StartContextPos.Y-1,false);
        Indent:=StartContextPos.X;
        //debugln(['GetIdentCompletionValue ',Indent,' "',dbgstr(Line),'" ',GetLineIndent(Line,1),' empty=',InEmptyLine(Line,1),' ',DbgsCXY(StartContextPos)]);
        if not InEmptyLine(Line,1) then
          Indent:=GetLineIndent(Line,1);
        Result:=TrimLeft(CodeToolBoss.SourceChangeCache
          .BeautifyCodeOptions.BeautifyProc(Result,Indent,false));
        //debugln(['GetIdentCompletionValue ',dbgstr(Result),' LineLen=',CodeToolBoss.SourceChangeCache.BeautifyCodeOptions.LineLength]);
        CanAddSemicolon:=false;
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
      Result+=' ';
    Result+=':=';
    if (atSymbol in CodeToolsOpts.DoInsertSpaceAfter) then
      Result+=' ';
  end;

  // add last typed character (that ended the identifier completion and starts a new token)
  if AddChar<>'' then
    Result+=AddChar;

  if CanAddComma
  and (ilcfNeedsEndComma in IdentList.ContextFlags) then
  begin
    Result+=',';
  end;

  if CodeToolsOpts.IdentComplAddSemicolon and
     (IdentItem.GetDesc in [ctnUseUnitNamespace,ctnUseUnitClearName]) and (AddChar<>'.') and
     not IdentList.StartUpAtomBehindIs('.')//check if there is already a point
  then
    Result+='.';

  // add 'do'
  if CodeToolsOpts.IdentComplAddDo and (AddChar='')
  and (ilcfNeedsDo in IdentList.ContextFlags) then begin
    s:=' '+CodeToolBoss.SourceChangeCache.BeautifyCodeOptions.BeautifyKeyWord('do');
    Result+=s;
    inc(CursorToLeft,length(s));
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
      Result+=';';
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

{ TLazIdentifierListItem }

procedure TLazIdentifierListItem.BeautifyIdentifier(IdentList: TIdentifierList);
begin
  if FBeautified then
    Exit;

  CodeToolBoss.SourceChangeCache.BeautifyCodeOptions.WordExceptions.CheckExceptions(Identifier);
  FBeautified:=True;
end;

{ TLazUnitNameSpaceIdentifierListItem }

procedure TLazUnitNameSpaceIdentifierListItem.BeautifyIdentifier(
  IdentList: TIdentifierList);
var
  CodeBuf: TCodeBuffer;
  LastPointPos: Integer;
  NewIdentifier: string;
  WordExc: TWordPolicyExceptions;
begin
  if FBeautified then
    Exit;

  NewIdentifier:=Identifier;
  WordExc:=CodeToolBoss.SourceChangeCache.BeautifyCodeOptions.WordExceptions;
  if not WordExc.CheckExceptions(NewIdentifier) then
  begin
    CodeBuf:=CodeToolBoss.FindUnitSource(IdentList.StartContextPos.Code,FileUnitName,'');
    if CodeBuf=nil then Exit;

    NewIdentifier:=Copy(CodeToolBoss.GetSourceName(CodeBuf,true),
                        IdentifierStartInUnitName, Length(Identifier));

    LastPointPos := LastDelimiter('.', NewIdentifier);
    if LastPointPos > 0 then
      NewIdentifier := Copy(NewIdentifier, LastPointPos+1, length(NewIdentifier));
    if NewIdentifier='' then
      NewIdentifier:=Identifier;
  end;
  Identifier := NewIdentifier;
  FBeautified := True;
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
  CIdentifierListItem:=TLazIdentifierListItem;
  CUnitNameSpaceIdentifierListItem:=TLazUnitNameSpaceIdentifierListItem;

finalization
  FreeAndNil(SynREEngine);

end.

