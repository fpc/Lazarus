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

  // For future, when .dfm form file can be used for both Delphi and Lazarus.
{  TFormFileAction = (faUseDfm, faRenameToLfm, faUseBothDfmAndLfm); }

  { TConvDelphiCodeTool }

  TConvDelphiCodeTool = class // (TStandardCodeTool)
  private
    fCodeTool: TEventsCodeTool;
    fCode: TCodeBuffer;
    fSrcCache: TSourceChangeCache;
    fAsk: Boolean;
    fHasFormFile: boolean;
    fUseBothDfmAndLfm: boolean;
    fLowerCaseRes: boolean;
    fDfmDirectiveStart: integer;
    fDfmDirectiveEnd: integer;
    fTarget: TConvertTarget;
    // List of units to remove.
    fUnitsToRemove: TStringList;
    // Units to rename. Map of unit name -> real unit name.
    fUnitsToRename: TStringToStringTree;
    // List of units to add.
    fUnitsToAdd: TStringList;
    // List of units to be commented.
    fUnitsToComment: TStringList;
    // Map of class member object types to be renamed in ReplaceMemberTypes.
    fMemberTypesToRename: TStringToStringTree;
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
    function ReplaceMemberTypes(AClassName: string): boolean;
  public
    property Ask: Boolean read fAsk write fAsk;
    property UseBothDfmAndLfm: boolean read fUseBothDfmAndLfm write fUseBothDfmAndLfm;
    property HasFormFile: boolean read fHasFormFile write fHasFormFile;
    property LowerCaseRes: boolean read fLowerCaseRes write fLowerCaseRes;
    property Target: TConvertTarget read fTarget write fTarget;
    property UnitsToRemove: TStringList read fUnitsToRemove write fUnitsToRemove;
    property UnitsToRename: TStringToStringTree read fUnitsToRename write fUnitsToRename;
    property UnitsToAdd: TStringList read fUnitsToAdd write fUnitsToAdd;
    property UnitsToComment: TStringList read fUnitsToComment write fUnitsToComment;
    property MemberTypesToRename: TStringToStringTree read fMemberTypesToRename
                                                     write fMemberTypesToRename;
  end;

implementation

{ TConvDelphiCodeTool }

constructor TConvDelphiCodeTool.Create(Code: TCodeBuffer);
begin
  fCode:=Code;
  // Default values for vars.
  fAsk:=true;
  fLowerCaseRes:=false;
  fUseBothDfmAndLfm:=false;
  fTarget:=ctLazarus;
  fUnitsToComment:=nil;
  fUnitsToRename:=nil;
  fMemberTypesToRename:=nil;
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
  DelphiOnlyUnits: TStringList;  // Delphi specific units.
  LclOnlyUnits: TStringList;     // LCL specific units.
  UsesNode: TCodeTreeNode;
  Junk: TAtomPosition;
  IsWinUnit, IsVariantUnit: Boolean;
  s, nl: string;
  InsPos, i: Integer;
begin
  Result:=false;
  DelphiOnlyUnits:=TStringList.Create;
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
          fUnitsToAdd.Append('LCLIntf');
          fUnitsToAdd.Append('LCLType');
          fUnitsToAdd.Append('LMessages');
          fUnitsToRemove.Append('WINDOWS');
        end;
        if IsVariantUnit then
          fUnitsToRemove.Append('VARIANTS');
       end;
      ctLazarusWin: begin
        // Don't do anything. Delphi units work for Lazarus under Windows.
       end;
      ctLazarusAndDelphi: begin
        // Make separate sections for LCL and Delphi units.
        if IsWinUnit then begin
          DelphiOnlyUnits.Append('Windows');
          LclOnlyUnits.Append('LCLIntf');
          LclOnlyUnits.Append('LCLType');
          LclOnlyUnits.Append('LMessages');
          fCodeTool.RemoveUnitFromUsesSection(UsesNode, 'WINDOWS', fSrcCache);
        end;
        if IsVariantUnit then begin
          DelphiOnlyUnits.Append('Variants');
          fCodeTool.BuildTree(true);
          UsesNode:=fCodeTool.FindMainUsesSection;
          fCodeTool.MoveCursorToUsesStart(UsesNode);
          fCodeTool.RemoveUnitFromUsesSection(UsesNode, 'VARIANTS', fSrcCache);
        end;
        // Now the missing units are not commented but used by Delphi instead.
        for i:=0 to fUnitsToComment.Count-1 do begin
          s:=UpperCaseStr(fUnitsToComment[i]);
          fCodeTool.BuildTree(true);
          UsesNode:=fCodeTool.FindMainUsesSection;
          fCodeTool.MoveCursorToUsesStart(UsesNode);
          fCodeTool.RemoveUnitFromUsesSection(UsesNode, s, fSrcCache);
        end;
        DelphiOnlyUnits.AddStrings(fUnitsToComment);
        if (LclOnlyUnits.Count>0) or (DelphiOnlyUnits.Count>0) then begin
          // Add LCL and Delphi sections for output.
          nl:=fSrcCache.BeautifyCodeOptions.LineEnd;
          s:='{$IFDEF FPC}'+nl+'  ';
          for i:=0 to LclOnlyUnits.Count-1 do
            s:=s+LclOnlyUnits[i]+', ';
          s:=s+nl+'{$ELSE}'+nl+'  ';
          for i:=0 to DelphiOnlyUnits.Count-1 do
            s:=s+DelphiOnlyUnits[i]+', ';
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
    DelphiOnlyUnits.Free;
  end;
end;

function TConvDelphiCodeTool.AddModeDelphiDirective: boolean;
var
  ModeDirectivePos: integer;
  InsertPos: Integer;
  s, nl: String;
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
        s:='{$IFDEF FPC}'+nl+'  {$MODE Delphi}'+nl+'{$ENDIF}'
      else
        s:='{$MODE Delphi}';
      fSrcCache.Replace(gtEmptyLine,gtEmptyLine,InsertPos,InsertPos,s);
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
  s, nl: string;
  AlreadyIsLfm: Boolean;
begin
  Result:=false;
  AlreadyIsLfm:=false;
  fDfmDirectiveStart:=-1;
  fDfmDirectiveEnd:=-1;
  ACleanPos:=1;
  // find $R directive
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
          // Lowercase existing key. (Future, when the same dfm file can be used)
//          faUseDfm: if Key<>LowKey then NewKey:=LowKey;
          if fUseBothDfmAndLfm then begin
            // Later IFDEF will be added so that Delphi can still use .dfm.
            fDfmDirectiveStart:=ACleanPos;
            fDfmDirectiveEnd:=ParamPos+6;
          end
          else       // Change .dfm to .lfm.
            NewKey:='lfm';
        end

        // If there already is .lfm, prevent adding IFDEF for .dfm / .lfm.
        else if LowKey='lfm' then begin
          AlreadyIsLfm:=true;
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
  // if there is already .lfm file, don't add IFDEF later for .dfm / .lfm.
  if fUseBothDfmAndLfm and (fDfmDirectiveStart<>-1) and not AlreadyIsLfm then
  begin
    // Add IFDEF for .lfm and .dfm allowing Delphi to use .dfm.
    nl:=fSrcCache.BeautifyCodeOptions.LineEnd;
    s:='{$IFDEF FPC}'+nl+
       '  {$R *.lfm}'+nl+
       '{$ELSE}'+nl+
       '  {$R *.dfm}'+nl+
       '{$ENDIF}';         // gtEmptyLine,gtNewLine,
    Result:=fSrcCache.Replace(gtNone,gtNone,fDfmDirectiveStart,fDfmDirectiveEnd,s);
  end;
  Result:=true;
end;

function TConvDelphiCodeTool.RemoveUnits: boolean;
// Remove units
var
  i: Integer;
begin
  Result:=false;
  if Assigned(fUnitsToRemove) then begin
    for i:=0 to fUnitsToRemove.Count-1 do
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
    for i:=0 to fUnitsToAdd.Count-1 do
    if not fCodeTool.AddUnitToMainUsesSection(fUnitsToAdd[i],'',fSrcCache) then
      exit;
  Result:=true;
end;

function TConvDelphiCodeTool.CommentOutUnits: boolean;
// Comment out missing units
begin
  // If units are used by Delphi (IFDEF block) -> don't comment.
  if fTarget<>ctLazarusAndDelphi then begin
    Result:=false;
    if Assigned(fUnitsToComment) and (fUnitsToComment.Count>0) then
      if not fCodeTool.CommentUnitsInUsesSections(fUnitsToComment, fSrcCache) then
        exit;
//      IDEMessagesWindow.AddMsg('Error="'+CodeToolBoss.ErrorMessage+'"','',-1);
  end;
  Result:=true;
end;

function TConvDelphiCodeTool.ReplaceMemberTypes(AClassName: string): boolean;
// Replace types of class object members.
begin
//  CodeToolBoss.RetypeClassVariables();
  Result:=fCodeTool.RetypeClassVariables(AClassName, fMemberTypesToRename,
                                         false, fSrcCache);
end;


end.

