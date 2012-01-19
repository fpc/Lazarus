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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}
unit MissingPropertiesDlg;

{$mode objfpc}{$H+}

interface

uses
  // FCL+LCL
  Classes, SysUtils, Math, LCLProc, Forms, Controls, Grids, LResources, LConvEncoding,
  Graphics, Dialogs, Buttons, StdCtrls, ExtCtrls, contnrs, FileUtil,
  // components
  SynHighlighterLFM, SynEdit, SynEditMiscClasses, LFMTrees,
  // codetools
  BasicCodeTools, CodeCache, CodeToolManager, CodeToolsStructs,
  // IDE
  IDEDialogs, ComponentReg, PackageIntf, IDEWindowIntf, DialogProcs,
  CustomFormEditor, LazarusIDEStrConsts, IDEProcs, OutputFilter,
  EditorOptions, CheckLFMDlg, IDEMsgIntf,
  // Converter
  ConverterTypes, ConvertSettings, ReplaceNamesUnit,
  ConvCodeTool, FormFileConv, UsedUnits;

type

  { TDFMConverter }

  // Encapsulates some basic form file conversions.
  TDFMConverter = class
  private
    fOrigFormat: TLRSStreamOriginalFormat;
    fIDEMsgWindow: TIDEMessagesWindowInterface;
    function GetLFMFilename(const DfmFilename: string; KeepCase: boolean): string;

  public
    constructor Create(aIDEMsgWindow: TIDEMessagesWindowInterface);
    destructor Destroy; override;
    function ConvertDfmToLfm(const aFilename: string): TModalResult;
    function Convert(const DfmFilename: string): TModalResult;
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
  protected
    procedure LoadLFM;
    function ShowRepairLFMWizard: TModalResult; override;
  public
    constructor Create(ACTLink: TCodeToolLink; ALFMBuffer: TCodeBuffer;
                       const AOnOutput: TOnAddFilteredLine);
    destructor Destroy; override;
    function Repair: TModalResult;
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

{ TDFMConverter }

constructor TDFMConverter.Create(aIDEMsgWindow: TIDEMessagesWindowInterface);
begin
  inherited Create;
  fIDEMsgWindow:=aIDEMsgWindow;
end;

destructor TDFMConverter.Destroy;
begin
  inherited Destroy;
end;

function TDFMConverter.Convert(const DfmFilename: string): TModalResult;
var
  s: String;
begin
  Result:=ConvertDfmToLfm(DfmFilename);
  if Result=mrOK then begin
    if fOrigFormat=sofBinary then
      s:=Format('File %s is converted to text format.', [DfmFilename]);
    if Assigned(fIDEMsgWindow) then
      IDEMessagesWindow.AddMsg(s, '', -1)
    else begin
      if s='' then
        s:=Format('File %s syntax is correct.', [DfmFilename]);
      ShowMessage(s);
    end;
  end;
end;

function TDFMConverter.GetLFMFilename(const DfmFilename: string;
  KeepCase: boolean): string;
begin
  if DfmFilename<>'' then begin
    // platform and fpc independent unitnames are lowercase, so are the lfm files
    Result:=lowercase(ExtractFilenameOnly(DfmFilename));
    if KeepCase then
      Result:=ExtractFilenameOnly(DfmFilename);
    Result:=ExtractFilePath(DfmFilename)+Result+'.lfm';
  end else
    Result:='';
end;

function TDFMConverter.ConvertDfmToLfm(const aFilename: string): TModalResult;
var
  DFMStream, LFMStream: TMemoryStream;
begin
  Result:=mrOk;
  DFMStream:=TMemoryStream.Create;
  LFMStream:=TMemoryStream.Create;
  try
    // Note: The file is copied from DFM file earlier.
    try
      DFMStream.LoadFromFile(UTF8ToSys(aFilename));
    except
      on E: Exception do begin
        Result:=QuestionDlg(lisCodeToolsDefsReadError, Format(
          lisUnableToReadFileError, ['"', aFilename, '"', #13, E.Message]),
          mtError,[mrIgnore,mrAbort],0);
        if Result=mrIgnore then // The caller will continue like nothing happened.
          Result:=mrOk;
        exit;
      end;
    end;
    fOrigFormat:=TestFormStreamFormat(DFMStream);
    try
      FormDataToText(DFMStream,LFMStream);
    except
      on E: Exception do begin
        Result:=QuestionDlg(lisFormatError,
          Format(lisUnableToConvertFileError, ['"',aFilename,'"',#13,E.Message]),
          mtError,[mrIgnore,mrAbort],0);
        if Result=mrIgnore then
          Result:=mrOk;
        exit;
      end;
    end;
    // converting dfm file, without renaming unit -> keep case...
    try
      LFMStream.SaveToFile(UTF8ToSys(aFilename));
    except
      on E: Exception do begin
        Result:=MessageDlg(lisCodeToolsDefsWriteError,
          Format(lisUnableToWriteFileError, ['"',aFilename,'"',#13,E.Message]),
          mtError,[mbIgnore,mbAbort],0);
        if Result=mrIgnore then
          Result:=mrOk;
        exit;
      end;
    end;
  finally
    LFMSTream.Free;
    DFMStream.Free;
  end;
end;


{ TLFMFixer }

constructor TLFMFixer.Create(ACTLink: TCodeToolLink; ALFMBuffer: TCodeBuffer;
  const AOnOutput: TOnAddFilteredLine);
begin
  inherited Create(ACTLink.Code, ALFMBuffer, AOnOutput);
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
      if (TheNode<>nil) and (TheNode.Parent<>nil) then begin
        if IsMissingType(CurError) then begin
          // Object type
          ObjNode:=CurError.Node as TLFMObjectNode;
          OldIdent:=ObjNode.TypeName;
          NewIdent:=TypeReplacements[OldIdent];
          // Keep the old class name if no replacement.
          if NewIdent<>'' then begin
            StartPos:=ObjNode.TypeNamePosition;
            EndPos:=StartPos+Length(OldIdent);
            AddReplacement(ChgEntryRepl,StartPos,EndPos,NewIdent);
            IDEMessagesWindow.AddMsg(Format(
                      'Replaced type "%s" with "%s".',[OldIdent, NewIdent]),'',-1);
            if Assigned(fUsedUnitsTool) then begin
              // ToDo: This is a test and will be replaced by configurable unit names.
              if NewIdent='TRichMemo' then
                fUsedUnitsTool.AddUnitIfNeeded('RichMemo');
            end;
            Result:=mrRetry;
          end;
        end
        else begin
          // Property
          TheNode.FindIdentifier(StartPos,EndPos);
          if StartPos>0 then begin
            OldIdent:=copy(fLFMBuffer.Source,StartPos,EndPos-StartPos);
            NewIdent:=PropReplacements[OldIdent];
            // Delete the whole property line if no replacement.
            if NewIdent='' then begin
              FindNiceNodeBounds(TheNode,StartPos,EndPos);
              IDEMessagesWindow.AddMsg(Format('Removed property "%s".',[OldIdent]),'',-1);
            end
            else
              IDEMessagesWindow.AddMsg(Format(
                      'Replaced property "%s" with "%s".',[OldIdent, NewIdent]),'',-1);
            AddReplacement(ChgEntryRepl,StartPos,EndPos,NewIdent);
          end;
        end;
      end;
      CurError:=CurError.PrevError;
    end;
    // Apply replacements to LFM.
    if not ApplyReplacements(ChgEntryRepl) then begin
      Result:=mrCancel;
      exit;
    end;
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
      IDEMessagesWindow.AddMsg(Format('Changed %s coord of %s from "%d" to "%d" inside %s.',
        [TopOffs.PropName, TopOffs.ChildType, OldNum, NewNum, TopOffs.ParentType]),'',-1);
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
    fLFMBuffer.Replace(Entry.StartPos, Entry.EndPos-Entry.StartPos,Entry.NewText);
    IDEMessagesWindow.AddMsg(Format('Added property "%s" for %s.',
                                   [Entry.NewText, Entry.ParentType]),'',-1);
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

procedure TLFMFixer.LoadLFM;
begin
  inherited LoadLFM;
  FillReplaceGrids;         // Fill both ReplaceGrids.
end;

function TLFMFixer.ShowRepairLFMWizard: TModalResult;
var
  FixLFMDialog: TFixLFMDialog;
  PrevCursor: TCursor;
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
      // Cursor is earlier set to HourGlass. Show normal cursor while in dialog.
      PrevCursor:=Screen.Cursor;
      Screen.Cursor:=crDefault;
      try
        Result:=FixLFMDialog.ShowModal;
      finally
        Screen.Cursor:=PrevCursor;
      end;
    end;
  finally
    FixLFMDialog.Free;
  end;
end;

function TLFMFixer.Repair: TModalResult;
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
  LoopCount:=0;
  repeat
    if CodeToolBoss.CheckLFM(fPascalBuffer,fLFMBuffer,fLFMTree,
        fRootMustBeClassInUnit,fRootMustBeClassInIntf,fObjectsMustExist) then
      Result:=mrOk
    else                     // Rename/remove properties and types interactively.
      Result:=ShowRepairLFMWizard;  // Can return mrRetry.
    Inc(LoopCount);
  until (Result in [mrOK, mrCancel]) or (LoopCount=10);
  // Show remaining errors to user.
  WriteLFMErrors;
  if (Result=mrOK) and (fSettings.CoordOffsMode=rsEnabled) then begin
    // Fix top offsets of some components in visual containers
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
const // Will be moved to LazarusIDEStrConsts
  lisLFMFileContainsInvalidProperties = 'The LFM (Lazarus form) '
    +'file contains unknown properties/classes which do not exist in LCL. '
    +'They can be replaced or removed.';
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
  ReplaceAllButton.LoadGlyphFromLazarusResource('laz_refresh');
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

