unit ConvCodeTool;

{$mode objfpc}{$H+}

interface

uses
  // LCL+FCL
  Classes, SysUtils, FileProcs, Forms, Controls, DialogProcs, Dialogs,
  // IDE
  LazarusIDEStrConsts, LazIDEIntf,
  // codetools
  CodeToolManager, StdCodeTools, CodeTree, CodeAtom,
  FindDeclarationTool, PascalReaderTool, PascalParserTool,
  CodeBeautifier, ExprEval, KeywordFuncLists, BasicCodeTools, LinkScanner,
  CodeCache, SourceChanger, CustomCodeTool, CodeToolsStructs, EventCodeTool,
  ConvertSettings;

type

  { TConvDelphiCodeTool }

  TConvDelphiCodeTool = class // (TStandardCodeTool)
  private
    fCodeTool: TEventsCodeTool;
    fCode: TCodeBuffer;
    fSrcCache: TSourceChangeCache;
    fAsk: Boolean;
    fHasFormFile: boolean;
    fFormFileRename: boolean;
    fLowerCaseRes: boolean;
    fTarget: TConvertTarget;
    // List of units to remove.
    fUnitsToRemove: TStringList;
    // Units to rename. Map of unit name -> real unit name.
    fUnitsToRename: TStringToStringTree;
    // List of units to add.
    fUnitsToAdd: TStringList;
    // List of units to be commented.
    fUnitsToComment: TStringList;
    function AddDelphiAndLCLSections: boolean;
    function AddModeDelphiDirective: boolean;
    function RenameResourceDirectives: boolean;
    function RemoveUnits: boolean;
    function RenameUnits: boolean;
    function AddUnits: boolean;
    function CommentOutUnits: boolean;
    function HandleCodetoolError: TModalResult;
  public
    constructor Create(Code: TCodeBuffer);
    destructor Destroy; override;
    function Convert: TModalResult;
  public
    property Ask: Boolean read fAsk write fAsk;
    property FormFileRename: boolean read fFormFileRename write fFormFileRename;
    property HasFormFile: boolean read fHasFormFile write fHasFormFile;
    property LowerCaseRes: boolean read fLowerCaseRes write fLowerCaseRes;
    property Target: TConvertTarget read fTarget write fTarget;
    property UnitsToRemove: TStringList read fUnitsToRemove write fUnitsToRemove;
    property UnitsToRename: TStringToStringTree read fUnitsToRename write fUnitsToRename;
    property UnitsToAdd: TStringList read fUnitsToAdd write fUnitsToAdd;
    property UnitsToComment: TStringList read fUnitsToComment write fUnitsToComment;
  end;

implementation

{ TConvDelphiCodeTool }

constructor TConvDelphiCodeTool.Create(Code: TCodeBuffer);
begin
  fCode:=Code;
  // Default values for vars.
  fAsk:=true;
  fLowerCaseRes:=false;
  fFormFileRename:=false;
  fTarget:=ctLazarus;
  fUnitsToComment:=nil;
  fUnitsToRename:=nil;
  // Initialize codetools. (Copied from TCodeToolManager.)
  if not CodeToolBoss.InitCurCodeTool(fCode) then exit;
  try
    fCodeTool:=CodeToolBoss.CurCodeTool;
    fSrcCache:=CodeToolBoss.SourceChangeCache;
    fSrcCache.MainScanner:=fCodeTool.Scanner;
  except
    on e: Exception do
      CodeToolBoss.HandleException(e);
  end;
end;

destructor TConvDelphiCodeTool.Destroy;
begin
  inherited Destroy;
end;

function TConvDelphiCodeTool.HandleCodetoolError: TModalResult;
// returns mrOk or mrAbort
const
  CodetoolsFoundError='The codetools found an error in unit %s:%s%s%s';
var
  ErrMsg: String;
begin
  ErrMsg:=CodeToolBoss.ErrorMessage;
  LazarusIDE.DoJumpToCodeToolBossError;
  if fAsk then begin
    Result:=QuestionDlg(lisCCOErrorCaption,
      Format(CodetoolsFoundError, [ExtractFileName(fCode.Filename), #13, ErrMsg, #13]),
      mtWarning, [mrIgnore, lisIgnoreAndContinue, mrAbort], 0);
    if Result=mrIgnore then Result:=mrOK;
  end else begin
    Result:=mrOK;
  end;
end;

function TConvDelphiCodeTool.Convert: TModalResult;
// add {$mode delphi} directive
// remove {$R *.dfm} or {$R *.xfm} directive
// Change {$R *.RES} to {$R *.res} if needed
// TODO: fix delphi ambiguousities like incomplete proc implementation headers
begin
  Result:=mrCancel;
  try
    fSrcCache.BeginUpdate;
    try
      // these changes can be applied together without rescan
      if not AddModeDelphiDirective then exit;
      if not RenameResourceDirectives then exit;
      if not fSrcCache.Apply then exit;
    finally
      fSrcCache.EndUpdate;
    end;
    // This adds units to add, remove and rename if Delphi compat is not required.
    if not AddDelphiAndLCLSections then exit;
    if not RemoveUnits then exit;
    if not RenameUnits then exit;
    if not AddUnits then exit;
    if not CommentOutUnits then exit;
    if not fCodeTool.FixUsedUnitCase(fSrcCache) then exit;
    if not fSrcCache.Apply then exit;
    Result:=mrOK;
  except
    on e: Exception do begin
      CodeToolBoss.HandleException(e);
      Result:=HandleCodetoolError;
    end;
  end;
end;

function TConvDelphiCodeTool.AddDelphiAndLCLSections: boolean;
// add, remove and rename units for desired target.
var
  WinOnlyUnits: TStringList;  // Windows and LCL specific units.
  LclOnlyUnits: TStringList;
  UsesNode: TCodeTreeNode;
  Junk: TAtomPosition;
  IsWinUnit, IsVariantUnit: Boolean;
  s, nl: string;
  InsPos, i: Integer;
begin
  Result:=false;
  WinOnlyUnits:=TStringList.Create;
  LclOnlyUnits:=TStringList.Create;
  try
  fCodeTool.BuildTree(true);
  fSrcCache.MainScanner:=fCodeTool.Scanner;
  UsesNode:=fCodeTool.FindMainUsesSection;
  if UsesNode<>nil then begin
    fCodeTool.MoveCursorToUsesStart(UsesNode);
    InsPos:=fCodeTool.CurPos.StartPos;
    IsWinUnit:=fCodeTool.FindUnitInUsesSection(UsesNode,'WINDOWS',Junk,Junk);
    IsVariantUnit:=fCodeTool.FindUnitInUsesSection(UsesNode,'VARIANTS',Junk,Junk);
    case fTarget of
      ctLazarus: begin
        // One way conversion: just add, replace and remove units.
        if IsWinUnit then begin
          fUnitsToRemove.Append('WINDOWS');
          fUnitsToAdd.Append('LCLIntf');
          fUnitsToAdd.Append('LCLType');
          fUnitsToAdd.Append('LMessages');
        end;
        if IsVariantUnit then
          fUnitsToRemove.Append('VARIANTS');
       end;
      ctLazarusWin: begin
        // Don't do anything. Delphi units work for Lazarus under Windows.
       end;
      ctLazarusAndDelphi: begin
        // Make separate sections for LCL and Windows units.
        if IsWinUnit then begin
          WinOnlyUnits.Append('Windows');
          LclOnlyUnits.Append('LCLIntf');
          LclOnlyUnits.Append('LCLType');
          LclOnlyUnits.Append('LMessages');
          fCodeTool.RemoveUnitFromUsesSection(UsesNode, 'WINDOWS', fSrcCache);
        end;
        if IsVariantUnit then begin
          WinOnlyUnits.Append('Variants');
          fCodeTool.RemoveUnitFromUsesSection(UsesNode, 'VARIANTS', fSrcCache);
        end;
        if (LclOnlyUnits.Count>0) or (WinOnlyUnits.Count>0) then begin
          // Add Windows and LCL sections for output.
          nl:=fSrcCache.BeautifyCodeOptions.LineEnd;
          s:='{$IFDEF LCL}'+nl+'  ';
          for i:=0 to LclOnlyUnits.Count-1 do
            s:=s+LclOnlyUnits[i]+', ';
          s:=s+nl+'{$ELSE}'+nl+'  ';
          for i:=0 to WinOnlyUnits.Count-1 do
            s:=s+WinOnlyUnits[i]+', ';
          s:=s+nl+'{$ENDIF}';
          // Now add the lines using codetools.
          if not fSrcCache.Replace(gtEmptyLine,gtNewLine,InsPos,InsPos,s) then exit;
        end;
       end;
    end;
  end;
  Result:=true;
  finally
    LclOnlyUnits.Free;
    WinOnlyUnits.Free;
  end;
end;

function TConvDelphiCodeTool.AddModeDelphiDirective: boolean;
var
  ModeDirectivePos: integer;
  InsertPos: Integer;
  nl: String;
begin
  Result:=false;
  with fCodeTool do begin
    BuildTree(true);
    if not FindModeDirective(false,ModeDirectivePos) then begin
      // add {$MODE Delphi} behind source type
      if Tree.Root=nil then exit;
      MoveCursorToNodeStart(Tree.Root);
      ReadNextAtom; // 'unit', 'program', ..
      ReadNextAtom; // name
      ReadNextAtom; // semicolon
      InsertPos:=CurPos.EndPos;
      nl:=fSrcCache.BeautifyCodeOptions.LineEnd;
      if fTarget=ctLazarusAndDelphi then
        fSrcCache.Replace(gtEmptyLine,gtEmptyLine,InsertPos,InsertPos,
          '{$IFDEF LCL}'+nl+'  {$MODE Delphi}'+nl+'{$ENDIF}')
      else
        fSrcCache.Replace(gtEmptyLine,gtEmptyLine,InsertPos,InsertPos,
          '{$MODE Delphi}');
    end;
    // changing mode requires rescan
    BuildTree(false);
  end;
  Result:=true;
end;

function TConvDelphiCodeTool.RenameResourceDirectives: boolean;
// rename {$R *.dfm} directive to {$R *.lfm}, or lowercase it.
// lowercase {$R *.RES} to {$R *.res}
var
  ParamPos: Integer;
  ACleanPos: Integer;
  Key, LowKey, NewKey: String;
begin
  Result:=false;
  // find $R directive
  ACleanPos:=1;
  with fCodeTool do
    repeat
      ACleanPos:=FindNextCompilerDirectiveWithName(Src,ACleanPos,'R',
        fCodeTool.Scanner.NestedComments,ParamPos);
      if (ACleanPos<1) or (ACleanPos>SrcLen) or (ParamPos>SrcLen-6) then break;
      NewKey:='';
      if (Src[ACleanPos]='{') and
         (Src[ParamPos]='*') and (Src[ParamPos+1]='.') and
         (Src[ParamPos+5]='}')
      then begin
        Key:=copy(Src,ParamPos+2,3);
        LowKey:=LowerCase(Key);

        // Form file resource rename or lowercase:
        if (LowKey='dfm') or (LowKey='xfm') then begin
          if fFormFileRename then
            NewKey:='lfm'
          else if Key<>LowKey then
            NewKey:=LowKey;
        end

        // lowercase {$R *.RES} to {$R *.res}
        else if (Key='RES') and fLowerCaseRes then
          NewKey:=LowKey;

        // Now change code.
        if NewKey<>'' then
          if not fSrcCache.Replace(gtNone,gtNone,ParamPos+2,ParamPos+5,NewKey) then exit;
      end;
      ACleanPos:=FindCommentEnd(Src,ACleanPos,fCodeTool.Scanner.NestedComments);
    until false;
  Result:=true;
end;

function TConvDelphiCodeTool.RemoveUnits: boolean;
// Remove units
var
  i: Integer;
begin
  Result:=false;
  if Assigned(fUnitsToRemove) then begin
    for i := 0 to fUnitsToRemove.Count-1 do
      if not fCodeTool.RemoveUnitFromAllUsesSections(fUnitsToRemove[i], fSrcCache) then
        exit;
  end;
  Result:=true;
end;

function TConvDelphiCodeTool.RenameUnits: boolean;
// Rename units
begin
  Result:=false;
  if Assigned(fUnitsToRename) then
    if not fCodeTool.ReplaceUsedUnits(fUnitsToRename, fSrcCache) then
      exit;
  Result:=true;
end;

function TConvDelphiCodeTool.AddUnits: boolean;
// Add units
var
  i: Integer;
begin
  Result:=false;
  if Assigned(fUnitsToAdd) then
    for i := 0 to fUnitsToAdd.Count-1 do
    if not fCodeTool.AddUnitToMainUsesSection(fUnitsToAdd[i],'',fSrcCache) then
      exit;
  Result:=true;
end;

function TConvDelphiCodeTool.CommentOutUnits: boolean;
// Comment out missing units
begin
  Result:=false;
  if Assigned(fUnitsToComment) and (fUnitsToComment.Count>0) then
    if not fCodeTool.CommentUnitsInUsesSections(fUnitsToComment, fSrcCache) then
      exit;
//      IDEMessagesWindow.AddMsg('Error="'+CodeToolBoss.ErrorMessage+'"','',-1);
  Result:=true;
end;


end.

