{  $Id$  }
{
 /***************************************************************************
                            MissingPropertiesDlg.pas
                            ------------------------

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
}
unit MissingPropertiesDlg;

{$mode objfpc}{$H+}

interface

uses
  // FCL+LCL
  Classes, SysUtils, contnrs,
  LCLProc, Forms, Controls, Grids, LResources, Dialogs, Buttons, StdCtrls, ExtCtrls,
  // LazUtils
  LazFileUtils, LazUTF8Classes, LazUTF8, AvgLvlTree,
  // components
  SynHighlighterLFM, SynEdit, SynEditMiscClasses,
  // codetools
  CodeCache, CodeToolManager, CodeCompletionTool, LFMTrees,
  // IdeIntf
  IDEExternToolIntf, ComponentReg, IDEImagesIntf,
  // IDE
  LazarusIDEStrConsts, EditorOptions, CheckLFMDlg, Project, SourceMarks,
  // Converter
  ConverterTypes, ConvertSettings, ReplaceNamesUnit,
  ConvCodeTool, FormFileConv, UsedUnits;

type

  { TDFMConverter }

  // Encapsulates some basic form file conversions.
  TDFMConverter = class
  private
    fSettings: TConvertSettings;
    fOrigFormat: TLRSStreamOriginalFormat;
  public
    constructor Create;
    destructor Destroy; override;
    function ConvertDfmToLfm(const aFilename: string): TModalResult;
    function Convert(const DfmFilename: string;
      DisplaySuccessMessage: Boolean): TModalResult;
  public
    property Settings: TConvertSettings read fSettings write fSettings;
  end;

  { TLfmFixer }

  TLFMFixer = class(TLFMChecker)
  private
    fCTLink: TCodeToolLink;
    fSettings: TConvertSettings;
    fUsedUnitsTool: TUsedUnitsTool;
    // List of property values which need to be adjusted.
    fHasMissingProperties: Boolean;         // LFM file has unknown properties.
    fHasMissingObjectTypes: Boolean;        // LFM file has unknown object types.
    // References to controls in UI:
    fPropReplaceGrid: TStringGrid;
    fTypeReplaceGrid: TStringGrid;
    function ReplaceAndRemoveAll: TModalResult;
    function ReplaceTopOffsets(aSrcOffsets: TList): TModalResult;
    function AddNewProps(aNewProps: TList): TModalResult;
    // Fill StringGrids with missing properties and types from fLFMTree.
    procedure FillReplaceGrids;
    function ShowConvertLFMWizard: TModalResult;
  protected
    function FixMissingComponentClasses(aMissingTypes: TStringList): TModalResult; override;
    procedure LoadLFM;
  public
    constructor Create(ACTLink: TCodeToolLink; ALFMBuffer: TCodeBuffer);
    destructor Destroy; override;
    function ConvertAndRepair: TModalResult;
  public
    property Settings: TConvertSettings read fSettings write fSettings;
    property UsedUnitsTool: TUsedUnitsTool read fUsedUnitsTool write fUsedUnitsTool;
  end;


  { TFixLFMDialog }

  TFixLFMDialog = class(TForm)
    CancelButton: TBitBtn;
    ErrorsGroupBox: TGroupBox;
    ErrorsListBox: TListBox;
    TypeReplaceGrid: TStringGrid;
    PropertyReplaceGroupBox: TGroupBox;
    NoteLabel: TLabel;
    LFMGroupBox: TGroupBox;
    LFMSynEdit: TSynEdit;
    BtnPanel: TPanel;
    ReplaceAllButton: TBitBtn;
    Splitter1: TSplitter;
    PropReplaceGrid: TStringGrid;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    PropertiesText: TStaticText;
    TypesText: TStaticText;
    SynLFMSyn1: TSynLFMSyn;
    procedure ErrorsListBoxClick(Sender: TObject);
    procedure ReplaceAllButtonClick(Sender: TObject);
    procedure LFMSynEditSpecialLineMarkup(Sender: TObject;
      Line: integer; var Special: boolean; AMarkup: TSynSelectedColor);
    procedure CheckLFMDialogCREATE(Sender: TObject);
  private
    fLfmFixer: TLFMFixer;
  public
    constructor Create(AOwner: TComponent; ALfmFixer: TLFMFixer); reintroduce;
    destructor Destroy; override;
  end;


implementation


{$R *.lfm}

function IsMissingType(LFMError: TLFMError): boolean;
begin
  with LFMError do
    Result:=(ErrorType in [lfmeIdentifierNotFound,lfmeMissingRoot])
        and (Node is TLFMObjectNode)
        and (TLFMObjectNode(Node).TypeName<>'');
end;

function FixWideString(aInStream, aOutStream: TMemoryStream): TModalResult;
// Convert Windows WideString syntax (#xxx) to UTF8

  function UnicodeNumber(const InS: string; var Ind: integer): string;
  // Convert the number to UTF8
  var
    Start, c: Integer;
  begin
    Inc(Ind);                            // Skip '#'
    Start:=Ind;
    while InS[Ind] in ['0'..'9'] do
      Inc(Ind);                          // Collect numbers
    c:=StrToInt(Copy(InS, Start, Ind-Start));
    if c>255 then
      Result:=UnicodeToUTF8(c)
    else
      Result:=SysToUTF8(chr(c));
  end;

  function FixControlChars(const s:string): string;
  var
    i: Integer;
    InControl: boolean;
  begin
    Result := '';
    InControl := false;
    for i:=1 to Length(s) do begin
      if s[i] < #32 then begin
        if not InControl then
          result := result + '''';
        result := result + '#' + IntToStr(ord(s[i]));
        InControl := true;
      end else begin
        if InControl then begin
          result := result + '''';
          InControl := false;
        end;
        Result := Result + s[i];
      end;
    end;
  end;

  function CollectString(const InS: string; var Ind: integer): string;
  // Collect a string composed of quoted strings and unicode numbers like #xxx
  var
    InQuote: Boolean;
    ch: Char;
  begin
    Result:='';
    InQuote:=False;
    repeat
      ch:=InS[Ind];
      if ch in [#13,#10] then Break;
      if ch = '''' then begin
        InQuote:=not InQuote;            // Toggle quote
        Inc(Ind);
      end
      else if InQuote then begin
        Result:=Result+ch;               // Inside quotes copy characters as is.
        Inc(Ind);
      end
      else if ch = '#' then
        Result:=Result+UnicodeNumber(InS, Ind)
      else
        Break;
    until False;
    Result:=FixControlChars(QuotedStr(Result));
  end;

var
  InS, OutS: string;
  i: Integer;
begin
  Result:=mrOk;
  OutS:='';
  aInStream.Position:=0;
  SetLength(InS, aInStream.Size);
  aInStream.Read(InS[1],length(InS));
  i := 1;
  while i <= Length(InS) do begin
    if InS[i] in ['''', '#'] then
      OutS:=OutS+CollectString(InS, i)
    else begin
      OutS:=OutS+InS[i];
      Inc(i);
    end;
  end;
  // Write data to a new stream.
  aOutStream.Write(OutS[1], Length(OutS));
end;

{ TDFMConverter }

constructor TDFMConverter.Create;
begin
  inherited Create;
end;

destructor TDFMConverter.Destroy;
begin
  inherited Destroy;
end;

function TDFMConverter.Convert(const DfmFilename: string;
  DisplaySuccessMessage: Boolean): TModalResult;
var
  s: String;
  Urgency: TMessageLineUrgency;
begin
  Result:=ConvertDfmToLfm(DfmFilename);
  if Result=mrOK then begin
    if fOrigFormat=sofBinary then begin
      if DisplaySuccessMessage then
        s:=Format(lisFileSIsConvertedToTextFormat, [DfmFilename])
      else
        s:='';
      Urgency:=mluHint;
    end
    else begin
      s:=Format(lisFileSHasIncorrectSyntax, [DfmFilename]);
      Urgency:=mluError;
    end;
    if s <> '' then begin
      if Assigned(fSettings) then
        fSettings.AddLogLine(Urgency, s, DfmFilename)
      else
        ShowMessage(s);
    end;
  end;
end;

function TDFMConverter.ConvertDfmToLfm(const aFilename: string): TModalResult;
var
  DFMStream, LFMStream, Utf8LFMStream: TMemoryStreamUTF8;
begin
  Result:=mrOk;
  DFMStream:=TMemoryStreamUTF8.Create;
  LFMStream:=TMemoryStreamUTF8.Create;
  Utf8LFMStream:=TMemoryStreamUTF8.Create;
  try
    // Note: The file is copied from DFM file earlier. Load it.
    try
      DFMStream.LoadFromFile(aFilename);
    except
      on E: Exception do begin
        Result:=QuestionDlg(lisCodeToolsDefsReadError, Format(
          lisUnableToReadFileError, [aFilename, LineEnding, E.Message]),
          mtError,[mrIgnore,mrAbort],0);
        if Result=mrIgnore then // The caller will continue like nothing happened.
          Result:=mrOk;
        exit;
      end;
    end;
    fOrigFormat:=TestFormStreamFormat(DFMStream);
    // converting dfm file, without renaming unit -> keep case...
    try
      FormDataToText(DFMStream, LFMStream, fOrigFormat);
    except
      on E: Exception do begin
        Result:=QuestionDlg(lisFormatError,
          Format(lisUnableToConvertFileError, [aFilename, LineEnding, E.Message]),
          mtError,[mrIgnore,mrAbort],0);
        if Result=mrIgnore then
          Result:=mrOk;
        exit;
      end;
    end;
    // Convert Windows WideString syntax (#xxx) to UTF8
    FixWideString(LFMStream, Utf8LFMStream);
    // Save the converted file.
    try
      Utf8LFMStream.SaveToFile(ChangeFileExt(aFilename, '.lfm'));
    except
      on E: Exception do begin
        Result:=MessageDlg(lisCodeToolsDefsWriteError,
          Format(lisUnableToWriteFileError, [aFilename, LineEnding, E.Message]),
          mtError,[mbIgnore,mbAbort],0);
        if Result=mrIgnore then
          Result:=mrOk;
        exit;
      end;
    end;
  finally
    Utf8LFMStream.Free;
    LFMSTream.Free;
    DFMStream.Free;
  end;
end;


{ TLFMFixer }

constructor TLFMFixer.Create(ACTLink: TCodeToolLink; ALFMBuffer: TCodeBuffer);
begin
  inherited Create(ACTLink.Code, ALFMBuffer);
  fCTLink:=ACTLink;
  fHasMissingProperties:=false;
  fHasMissingObjectTypes:=false;
end;

destructor TLFMFixer.Destroy;
begin
  inherited Destroy;
end;

function TLFMFixer.ReplaceAndRemoveAll: TModalResult;
// Replace or remove properties and types based on values in grid.
// Returns mrRetry if some types were changed and a new scan is needed,
//         mrOK if no types were changed, and mrCancel if there was an error.
var
  AutoInc: integer;

  function SolveAutoInc(AIdent: string): string;
  begin
    if Pos('$autoinc', AIdent)>0 then begin
      Inc(AutoInc);
      Result:=StringReplace(AIdent, '$autoinc', IntToStr(AutoInc), [rfReplaceAll]);
    end
    else
      Result:=AIdent;
  end;

  procedure InitClassCompletion;
  begin
    with fCTLink.CodeTool do
      if not Assigned(CodeCompleteClassNode) then begin // Do only at first time.
        CodeCompleteClassNode:=FindClassNodeInInterface(
                         TLFMObjectNode(fLFMTree.Root).TypeName,true,false,true);
        CodeCompleteSrcChgCache:=fCTLink.SrcCache;
      end;
  end;

var
  CurError: TLFMError;
  TheNode: TLFMTreeNode;
  ObjNode: TLFMObjectNode;
  // Property / Type name --> replacement name.
  PropReplacements: TStringToStringTree;
  TypeReplacements: TStringToStringTree;
  // List of TLFMChangeEntry objects.
  ChgEntryRepl: TObjectList;
  OldIdent, NewIdent: string;
  StartPos, EndPos: integer;
begin
  Result:=mrOK;
  AutoInc:=0;
  fCTLink.CodeTool.CodeCompleteClassNode:=Nil;
  ChgEntryRepl:=TObjectList.Create;
  PropReplacements:=TStringToStringTree.Create(false);
  TypeReplacements:=TStringToStringTree.Create(false);
  try
    // Collect (maybe edited) properties from StringGrid to map.
    FromGridToMap(PropReplacements, fPropReplaceGrid);
    FromGridToMap(TypeReplacements, fTypeReplaceGrid, false);
    // Replace each missing property / type or delete it if no replacement.
    CurError:=fLFMTree.LastError;
    while CurError<>nil do begin
      TheNode:=CurError.FindContextNode;
      if (TheNode<>nil) and (TheNode.Parent<>nil) then
      begin
        if CurError.ErrorType=lfmeIdentifierMissingInCode then
        begin
          // Missing component variable, must be added to pascal sources
          ObjNode:=CurError.Node as TLFMObjectNode;
          InitClassCompletion;
          NewIdent:=ObjNode.Name+':'+ObjNode.TypeName;
          fCTLink.CodeTool.AddClassInsertion(UpperCase(ObjNode.Name),
                                   NewIdent+';', ObjNode.Name, ncpPublishedVars);
          fSettings.AddLogLine(mluNote,
            Format(lisAddedMissingObjectSToPascalSource, [NewIdent]),
            fUsedUnitsTool.Filename);
        end
        else if IsMissingType(CurError) then
        begin
          // Object type
          ObjNode:=CurError.Node as TLFMObjectNode;
          OldIdent:=ObjNode.TypeName;
          NewIdent:=SolveAutoInc(TypeReplacements[OldIdent]);
          // Keep the old class name if no replacement.
          if NewIdent<>'' then begin
            StartPos:=ObjNode.TypeNamePosition;
            EndPos:=StartPos+Length(OldIdent);
            fSettings.AddLogLine(mluNote,
              Format(lisReplacedTypeSWithS, [OldIdent, NewIdent]),
              fUsedUnitsTool.Filename);
            AddReplacement(ChgEntryRepl,StartPos,EndPos,NewIdent);
            Result:=mrRetry;
          end;
        end
        else begin
          // Property
          TheNode.FindIdentifier(StartPos,EndPos);
          if StartPos>0 then begin
            OldIdent:=copy(fLFMBuffer.Source,StartPos,EndPos-StartPos);
            NewIdent:=SolveAutoInc(PropReplacements[OldIdent]);
            // Delete the whole property line if no replacement.
            if NewIdent='' then begin
              FindNiceNodeBounds(TheNode,StartPos,EndPos);
              fSettings.AddLogLine(mluNote, Format(lisRemovedPropertyS, [OldIdent]),
                fUsedUnitsTool.Filename);
            end
            else
              fSettings.AddLogLine(mluNote,
                Format(lisReplacedPropertySWithS, [OldIdent, NewIdent]));
            AddReplacement(ChgEntryRepl,StartPos,EndPos,NewIdent);
            Result:=mrRetry;
          end;
        end;
      end;
      CurError:=CurError.PrevError;
    end;
    // Apply replacements to LFM.
    if not ApplyReplacements(ChgEntryRepl) then
      exit(mrCancel);
    // Apply added variables to pascal class definition.
    with fCTLink.CodeTool do
      if Assigned(CodeCompleteClassNode) then
        if not ApplyClassCompletion(false) then
          exit(mrCancel);
    // Apply replacement types also to pascal source.
    if TypeReplacements.Tree.Count>0 then
      if not CodeToolBoss.RetypeClassVariables(fPascalBuffer,
            TLFMObjectNode(fLFMTree.Root).TypeName, TypeReplacements, false, true) then
        Result:=mrCancel;
  finally
    TypeReplacements.Free;
    PropReplacements.Free;
    ChgEntryRepl.Free;
  end;
end;

function TLFMFixer.ReplaceTopOffsets(aSrcOffsets: TList): TModalResult;
// Replace top coordinates of controls in visual containers.
var
  TopOffs: TSrcPropOffset;
  VisOffs: TVisualOffset;
  OldNum, NewNum, Len, ind, i: integer;
begin
  Result:=mrOK;
  // Add offset to top coordinates.
  for i:=aSrcOffsets.Count-1 downto 0 do begin
    TopOffs:=TSrcPropOffset(aSrcOffsets[i]);
    if fSettings.CoordOffsets.Find(TopOffs.ParentType, ind) then begin
      VisOffs:=fSettings.CoordOffsets[ind];
      Len:=0;
      while fLFMBuffer.Source[TopOffs.StartPos+Len] in ['-', '0'..'9'] do
        Inc(Len);
      try
        OldNum:=StrToInt(Copy(fLFMBuffer.Source, TopOffs.StartPos, Len));
      except on EConvertError do
        OldNum:=0;
      end;
      NewNum:=OldNum-VisOffs.ByProperty(TopOffs.PropName);
      if NewNum<0 then
        NewNum:=0;
      fLFMBuffer.Replace(TopOffs.StartPos, Len, IntToStr(NewNum));
      fSettings.AddLogLine(mluNote, Format(lisChangedSCoordOfSFromDToDInsideS,
        [TopOffs.PropName, TopOffs.ChildType, OldNum, NewNum, TopOffs.ParentType]),
        fUsedUnitsTool.Filename);
    end;
  end;
end;

function TLFMFixer.AddNewProps(aNewProps: TList): TModalResult;
// Add new property to the lfm file.
var
  Entry: TAddPropEntry;
  i: integer;
begin
  Result:=mrOK;
  for i:=aNewProps.Count-1 downto 0 do begin
    Entry:=TAddPropEntry(aNewProps[i]);
    fLFMBuffer.Replace(Entry.StartPos, Entry.EndPos-Entry.StartPos,
                       Entry.NewPrefix+Entry.NewText);
    fSettings.AddLogLine(mluNote,
      Format(lisAddedPropertySForS, [Entry.NewText, Entry.ParentType]),
      fUsedUnitsTool.Filename);
  end;
end;

procedure TLFMFixer.FillReplaceGrids;
var
  PropUpdater: TGridUpdater;
  TypeUpdater: TGridUpdater;
  CurError: TLFMError;
  OldIdent, NewIdent: string;
begin
  fHasMissingProperties:=false;
  fHasMissingObjectTypes:=false;
  // ReplaceTypes is used for properties just in case it will provide some.
  PropUpdater:=TGridUpdater.Create(fSettings.ReplaceTypes, fPropReplaceGrid);
  TypeUpdater:=TGridUpdater.Create(fSettings.ReplaceTypes, fTypeReplaceGrid);
  try
    if fLFMTree<>nil then begin
      CurError:=fLFMTree.FirstError;
      while CurError<>nil do begin
        if IsMissingType(CurError) then begin
          OldIdent:=(CurError.Node as TLFMObjectNode).TypeName;
          NewIdent:=TypeUpdater.AddUnique(OldIdent); // Add each type only once.
          if NewIdent<>'' then
            fHasMissingObjectTypes:=true;
        end
        else if fSettings.PropReplaceMode<>rlDisabled then begin
          OldIdent:=CurError.Node.GetIdentifier;
          PropUpdater.AddUnique(OldIdent);           // Add each property only once.
          fHasMissingProperties:=true;
        end;
        CurError:=CurError.NextError;
      end;
    end;
  finally
    TypeUpdater.Free;
    PropUpdater.Free;
  end;
end;

function TLFMFixer.ShowConvertLFMWizard: TModalResult;
var
  FixLFMDialog: TFixLFMDialog;
begin
  Result:=mrCancel;
  FixLFMDialog:=TFixLFMDialog.Create(nil, self);
  try
    fLFMSynEdit:=FixLFMDialog.LFMSynEdit;
    fErrorsListBox:=FixLFMDialog.ErrorsListBox;
    fPropReplaceGrid:=FixLFMDialog.PropReplaceGrid;
    fTypeReplaceGrid:=FixLFMDialog.TypeReplaceGrid;
    LoadLFM;
    if ((fSettings.PropReplaceMode=rlAutomatic) or not fHasMissingProperties)
    and ((fSettings.TypeReplaceMode=raAutomatic) or not fHasMissingObjectTypes) then
      Result:=ReplaceAndRemoveAll  // Can return mrRetry.
    else begin
      Result:=FixLFMDialog.ShowModal;
    end;
  finally
    FixLFMDialog.Free;
  end;
end;

function TLFMFixer.FixMissingComponentClasses(aMissingTypes: TStringList): TModalResult;
// This is called from TLFMChecker.FindAndFixMissingComponentClasses.
// Add needed units to uses section using methods already defined in fUsedUnitsTool.
var
  RegComp: TRegisteredComponent;
  ClassUnitInfo: TUnitInfo;
  i: Integer;
  NeededUnitName: String;
begin
  Result:=mrOK;
  if not Assigned(fUsedUnitsTool) then Exit;
  for i := 0 to aMissingTypes.Count-1 do
  begin
    RegComp:=IDEComponentPalette.FindComponent(aMissingTypes[i]);
    NeededUnitName:='';
    if (RegComp<>nil) then begin
      if RegComp.ComponentClass<>nil then begin
        NeededUnitName:=RegComp.ComponentClass.UnitName;
        if NeededUnitName='' then
          NeededUnitName:=RegComp.GetUnitName;
      end;
    end
    else begin
      ClassUnitInfo:=Project1.UnitWithComponentClassName(aMissingTypes[i]);
      if ClassUnitInfo<>nil then
        NeededUnitName:=ClassUnitInfo.GetUsesUnitName;
    end;
    if (NeededUnitName<>'')
    and fUsedUnitsTool.AddUnitImmediately(NeededUnitName) then
    begin
      Result:=mrRetry;  // Caller must check LFM validity again
      fUsedUnitsTool.MaybeAddPackageDep(NeededUnitName);
    end;
  end;
end;

procedure TLFMFixer.LoadLFM;
begin
  inherited LoadLFM;
  FillReplaceGrids;         // Fill both ReplaceGrids.
end;

function TLFMFixer.ConvertAndRepair: TModalResult;
const
  MaxLoopCount = 50;
var
  ConvTool: TConvDelphiCodeTool;
  FormFileTool: TFormFileConverter;
  SrcCoordOffs: TObjectList;
  SrcNewProps: TObjectList;
  LoopCount: integer;
begin
  Result:=mrCancel;
  fLFMTree:=DefaultLFMTrees.GetLFMTree(fLFMBuffer, true);
  if not fLFMTree.ParseIfNeeded then exit;
  // Change a type that main form inherits from to a fall-back type if needed.
  ConvTool:=TConvDelphiCodeTool.Create(fCTLink);
  try
    if not ConvTool.FixMainClassAncestor(TLFMObjectNode(fLFMTree.Root).TypeName,
                                         fSettings.ReplaceTypes) then exit;
  finally
    ConvTool.Free;
  end;
  LoopCount:=0;    // Prevent possible eternal loops with a counter
  repeat
    repeat
      DebugLn('TLFMFixer.ConvertAndRepair: Checking LFM for '+fPascalBuffer.Filename);
      if not fLFMTree.ParseIfNeeded then exit;
      if CodeToolBoss.CheckLFM(fPascalBuffer, fLFMBuffer, fLFMTree,
          fRootMustBeClassInUnit, fRootMustBeClassInIntf, fObjectsMustExist) then
        Result:=mrOk
      else                     // Rename/remove properties and types interactively.
        Result:=ShowConvertLFMWizard;  // Can return mrRetry.
      Inc(LoopCount);                  // Increment counter in inner loop
    until (Result in [mrOK, mrCancel]) or (LoopCount>MaxLoopCount);

    // Check for missing object types and add units as needed.
    if not fLFMTree.ParseIfNeeded then
      Exit(mrCancel);
    if CodeToolBoss.CheckLFM(fPascalBuffer, fLFMBuffer, fLFMTree,
               fRootMustBeClassInUnit, fRootMustBeClassInIntf, fObjectsMustExist)
    then
      Result:=mrOk
    else begin
      Result:=FindAndFixMissingComponentClasses; // Can return mrRetry.
      if Result=mrRetry then
        DebugLn('TLFMFixer.ConvertAndRepair: Added unit to uses section -> another loop');
    end;
    Inc(LoopCount);                    // Increment also in outer loop
  until (Result in [mrOK, mrAbort]) or (LoopCount>MaxLoopCount);

  // Fix top offsets of some components in visual containers
  if (Result=mrOK) and (fSettings.CoordOffsMode=rsEnabled) then
  begin
    FormFileTool:=TFormFileConverter.Create(fCTLink, fLFMBuffer);
    SrcCoordOffs:=TObjectList.Create;
    SrcNewProps:=TObjectList.Create;
    try
      FormFileTool.VisOffsets:=fSettings.CoordOffsets;
      FormFileTool.SrcCoordOffs:=SrcCoordOffs;
      FormFileTool.SrcNewProps:=SrcNewProps;
      Result:=FormFileTool.Convert;
      if Result=mrOK then begin
        Result:=ReplaceTopOffsets(SrcCoordOffs);
        if Result=mrOK then
          Result:=AddNewProps(SrcNewProps);
      end;
    finally
      SrcNewProps.Free;
      SrcCoordOffs.Free;
      FormFileTool.Free;
    end;
  end;
end;


{ TFixLFMDialog }

constructor TFixLFMDialog.Create(AOwner: TComponent; ALfmFixer: TLFMFixer);
begin
  inherited Create(AOwner);
  fLfmFixer:=ALfmFixer;
end;

destructor TFixLFMDialog.Destroy;
begin
  inherited Destroy;
end;

procedure TFixLFMDialog.CheckLFMDialogCREATE(Sender: TObject);
begin
  Caption:=lisFixLFMFile;
  Position:=poScreenCenter;
  NoteLabel.Caption:=lisLFMFileContainsInvalidProperties;
  ErrorsGroupBox.Caption:=lisErrors;
  LFMGroupBox.Caption:=lisLFMFile;
  PropertyReplaceGroupBox.Caption:=lisReplacements;
  PropertiesText.Caption:=lisProperties;
  TypesText.Caption:=lisTypes;
  ReplaceAllButton.Caption:=lisReplaceRemoveUnknown;
  IDEImages.AssignImage(ReplaceAllButton, 'laz_refresh');
  EditorOpts.GetHighlighterSettings(SynLFMSyn1);
  EditorOpts.GetSynEditSettings(LFMSynEdit);
end;

procedure TFixLFMDialog.ReplaceAllButtonClick(Sender: TObject);
begin
  ModalResult:=fLfmFixer.ReplaceAndRemoveAll;
end;

procedure TFixLFMDialog.ErrorsListBoxClick(Sender: TObject);
begin
  fLfmFixer.JumpToError(fLfmFixer.FindListBoxError);
end;

procedure TFixLFMDialog.LFMSynEditSpecialLineMarkup(Sender: TObject;
  Line: integer; var Special: boolean; AMarkup: TSynSelectedColor);
var
  CurError: TLFMError;
begin
  CurError:=fLfmFixer.fLFMTree.FindErrorAtLine(Line);
  if CurError = nil then Exit;
  Special := True;
  EditorOpts.SetMarkupColor(SynLFMSyn1, ahaErrorLine, AMarkup);
end;


end.

