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
  Classes, SysUtils, Math, LCLProc, Forms, Controls,
  Graphics, Dialogs, Buttons, StdCtrls, contnrs,
  // components
  SynHighlighterLFM, SynEdit, SynEditMiscClasses, LFMTrees,
  // codetools
  BasicCodeTools, CodeCache, CodeToolManager, CodeToolsStructs,
  // IDE
  IDEDialogs, ComponentReg, PackageIntf, IDEWindowIntf,
  CustomFormEditor, LazarusIDEStrConsts, IDEProcs, OutputFilter,
  EditorOptions, ExtCtrls, Grids, ConvertSettings, ConvCodeTool, CheckLFMDlg;

type

  { TLfmFixer }

  TLFMFixer = class(TLFMChecker)
  private
    fSettings: TConvertSettings;
    // References to controls in UI:
    fPropReplaceGrid: TStringGrid;
    function ReplaceAndRemoveAll: TModalResult;
    // Fill StringGrid with missing properties from fLFMTree.
    procedure FillPropReplaceList;
  protected
    procedure LoadLFM;
    function ShowRepairLFMWizard: TModalResult; override;
  public
    constructor Create(APascalBuffer, ALFMBuffer: TCodeBuffer;
                       const AOnOutput: TOnAddFilteredLine);
    destructor Destroy; override;
    function Repair: TModalResult;
  public
    property Settings: TConvertSettings read fSettings write fSettings;
  end;


  { TFixLFMDialog }

  TFixLFMDialog = class(TForm)
    CancelButton: TBitBtn;
    ErrorsGroupBox: TGroupBox;
    ErrorsListBox: TListBox;
    PropertyReplaceGroupBox: TGroupBox;
    NoteLabel: TLabel;
    LFMGroupBox: TGroupBox;
    LFMSynEdit: TSynEdit;
    BtnPanel: TPanel;
    ReplaceAllButton: TBitBtn;
    Splitter1: TSplitter;
    PropertyReplaceGrid: TStringGrid;
    SynLFMSyn1: TSynLFMSyn;
    procedure ErrorsListBoxClick(Sender: TObject);
    procedure ReplaceAllButtonClick(Sender: TObject);
    procedure LFMSynEditSpecialLineMarkup(Sender: TObject;
      Line: integer; var Special: boolean; AMarkup: TSynSelectedColor);
    procedure CheckLFMDialogCREATE(Sender: TObject);
  private
    fLfmFixer: TLFMFixer;
    procedure SetupComponents;
  public
    constructor Create(AOwner: TComponent; ALfmFixer: TLFMFixer); reintroduce;
    destructor Destroy; override;
  end;


implementation

{$R *.lfm}

{ TLFMFixer }

constructor TLFMFixer.Create(APascalBuffer, ALFMBuffer: TCodeBuffer;
  const AOnOutput: TOnAddFilteredLine);
begin
  inherited Create(APascalBuffer, ALFMBuffer, AOnOutput);
end;

destructor TLFMFixer.Destroy;
begin
  inherited Destroy;
end;

function TLFMFixer.ReplaceAndRemoveAll: TModalResult;
var
  ConvTool: TConvDelphiCodeTool;
  CurError: TLFMError;
  TheNode: TLFMTreeNode;
  ObjNode: TLFMObjectNode;
  // Property name --> replacement name.
  PropNameRepl: TStringToStringTree;
  MemberTypes: TStringList;
  // List of TLFMChangeEntry objects.
  ChgEntryRepl: TObjectList;
  OldIdent, NewIdent: string;
  StartPos, EndPos: integer;
  i: Integer;
begin
  Result:=mrNone;
  ChgEntryRepl:=TObjectList.Create;
  PropNameRepl:=TStringToStringTree.Create(false);
  MemberTypes:=TStringList.Create;
  try
    // Collect (maybe edited) properties from StringGrid to PropNameRepl.
    for i:=1 to fPropReplaceGrid.RowCount-1 do begin // Skip the fixed row.
      OldIdent:=fPropReplaceGrid.Cells[0,i];
      NewIdent:=fPropReplaceGrid.Cells[1,i];
      if NewIdent<>'' then
        PropNameRepl[OldIdent]:=NewIdent;
    end;
    // Replace each missing property / type or delete it if no replacement.
    CurError:=fLFMTree.LastError;
    while CurError<>nil do begin
      TheNode:=CurError.FindContextNode;
      if (TheNode<>nil) and (TheNode.Parent<>nil) then begin
        if CurError.IsMissingObjectType then begin
          // Object type
          ObjNode:=CurError.Node as TLFMObjectNode;
          OldIdent:=ObjNode.TypeName;
          StartPos:=ObjNode.TypeNamePosition;
          EndPos:=StartPos+Length(OldIdent);
          NewIdent:=PropNameRepl[OldIdent];
          // Keep the old class name if no replacement.
          if NewIdent<>'' then begin
            AddReplacement(ChgEntryRepl,StartPos,EndPos,NewIdent);
            MemberTypes.Values[OldIdent]:=NewIdent;
          end;
        end
        else begin
          // Property
          TheNode.FindIdentifier(StartPos,EndPos);
          if StartPos>0 then begin
            OldIdent:=copy(fLFMBuffer.Source,StartPos,EndPos-StartPos);
            NewIdent:=PropNameRepl[OldIdent];
            // Delete the whole property line if no replacement.
            if NewIdent='' then
              FindNiceNodeBounds(TheNode,StartPos,EndPos);
            AddReplacement(ChgEntryRepl,StartPos,EndPos,NewIdent);
          end;
        end;
      end;
      CurError:=CurError.PrevError;
    end;
    // Apply replacements to LFM.
    if ApplyReplacements(ChgEntryRepl) then begin
      if MemberTypes.Count>0 then begin
        // Replace the object member types also to pascal source.
        ConvTool:=TConvDelphiCodeTool.Create(fPascalBuffer);
        try
          ConvTool.MemberTypesToRename:=MemberTypes;
          ConvTool.ReplaceMemberTypes(TLFMObjectNode(fLFMTree.Root).TypeName);
        finally
          ConvTool.Free;
        end;
      end;
      Result:=mrOk;
    end;
  finally
    MemberTypes.Free;
    PropNameRepl.Free;
    ChgEntryRepl.Free;
  end;
end;

procedure TLFMFixer.FillPropReplaceList;
var
  CurError: TLFMError;
  SeenPropName: TStringList;
  OldIdent, NewIdent: string;
  i: integer;
begin
  SeenPropName:=TStringList.Create;
  try
    fPropReplaceGrid.BeginUpdate;
    if fLFMTree<>nil then begin
      i:=1;
      CurError:=fLFMTree.FirstError;
      while CurError<>nil do begin
        if CurError.IsMissingObjectType then
          OldIdent:=(CurError.Node as TLFMObjectNode).TypeName
        else
          OldIdent:=CurError.Node.GetIdentifier;
        // Add only one instance of each property name.
        if SeenPropName.IndexOf(OldIdent)<0 then begin
          SeenPropName.Append(OldIdent);
          NewIdent:=fSettings.ReplaceProps[OldIdent];
          if fPropReplaceGrid.RowCount<i+1 then
            fPropReplaceGrid.RowCount:=i+1;
          fPropReplaceGrid.Cells[0,i]:=OldIdent;
          fPropReplaceGrid.Cells[1,i]:=NewIdent;
          Inc(i);
        end;
        CurError:=CurError.NextError;
      end;
    end;
    fPropReplaceGrid.EndUpdate;
  finally
    SeenPropName.Free;
  end;
end;

procedure TLFMFixer.LoadLFM;
begin
  inherited LoadLFM;
  // Fill PropertyReplaceGrid
  FillPropReplaceList;
end;

function TLFMFixer.ShowRepairLFMWizard: TModalResult;
var
  FixLFMDialog: TFixLFMDialog;
begin
  Result:=mrCancel;
  FixLFMDialog:=TFixLFMDialog.Create(nil, self);
  try
    fLFMSynEdit:=FixLFMDialog.LFMSynEdit;
    fErrorsListBox:=FixLFMDialog.ErrorsListBox;
    fPropReplaceGrid:=FixLFMDialog.PropertyReplaceGrid;
    LoadLFM;
    Result:=FixLFMDialog.ShowModal;
  finally
    FixLFMDialog.Free;
  end;
end;

function TLFMFixer.Repair: TModalResult;
begin
  Result:=inherited Repair;
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
//  IDEDialogLayoutList.ApplyLayout(Self,600,400);
  SetupComponents;
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

procedure TFixLFMDialog.SetupComponents;
const // Will be moved to LazarusIDEStrConsts
  lisLFMFileContainsInvalidProperties = 'The LFM (Lazarus form) '
    +'file contains invalid properties/classes which do not exist in LCL. '
    +'They can be replaced or removed.';
  lisReplaceAllProperties = 'Replace and remove invalid properties';
begin
  NoteLabel.Caption:=lisLFMFileContainsInvalidProperties;
  ErrorsGroupBox.Caption:=lisErrors;
  LFMGroupBox.Caption:=lisLFMFile;
  ReplaceAllButton.Caption:=lisReplaceAllProperties;
  ReplaceAllButton.LoadGlyphFromLazarusResource('laz_refresh');
  EditorOpts.GetHighlighterSettings(SynLFMSyn1);
  EditorOpts.GetSynEditSettings(LFMSynEdit);
end;


end.

