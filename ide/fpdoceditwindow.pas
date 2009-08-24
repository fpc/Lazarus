{
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

  see for todo list: http://wiki.lazarus.freepascal.org/index.php/LazDoc
}

unit FPDocEditWindow;

{$mode objfpc}{$H+}

{ $define VerboseCodeHelp}

interface

uses
  // FCL
  Classes, SysUtils, StrUtils,
  // LCL
  LCLProc, LResources, StdCtrls, Buttons, ComCtrls, Controls, Dialogs,
  LDockCtrl, ExtCtrls, Forms, Graphics, LCLType,
  // Synedit
  SynEdit,
  // codetools
  BasicCodeTools, FileProcs, CodeAtom, CodeCache, CodeToolManager,
  Laz_DOM, Laz_XMLRead, Laz_XMLWrite,
  // IDEIntf
  ProjectIntf, LazIDEIntf, IDEHelpIntf, LazHelpIntf, Menus,
  // IDE
  IDEOptionDefs, EnvironmentOpts, PackageSystem, IDEProcs, LazarusIDEStrConsts,
  FPDocSelectInherited, FPDocSelectLink, CodeHelp;

type
  TFPDocEditorFlag = (
    fpdefWriting,
    fpdefCodeCacheNeedsUpdate,
    fpdefChainNeedsUpdate,
    fpdefCaptionNeedsUpdate,
    fpdefValueControlsNeedsUpdate,
    fpdefInheritedControlsNeedsUpdate
    );
  TFPDocEditorFlags = set of TFPDocEditorFlag;
  
  { TFPDocEditor }

  TFPDocEditor = class(TForm)
    AddLinkToInheritedButton: TButton;
    BoldFormatButton: TSpeedButton;
    BrowseExampleButton: TButton;
    ControlDocker: TLazControlDocker;
    CopyFromInheritedButton: TButton;
    CreateButton: TButton;
    DescrMemo: TMemo;
    DescrTabSheet: TTabSheet;
    ErrorsMemo: TMemo;
    ErrorsTabSheet: TTabSheet;
    ExampleEdit: TEdit;
    ExampleTabSheet: TTabSheet;
    InheritedShortEdit: TEdit;
    InheritedShortLabel: TLabel;
    InheritedTabSheet: TTabSheet;
    InsertCodeTagButton: TSpeedButton;
    InsertLinkSpeedButton: TSpeedButton;
    InsertParagraphSpeedButton: TSpeedButton;
    InsertRemarkButton: TSpeedButton;
    InsertVarTagButton: TSpeedButton;
    ItalicFormatButton: TSpeedButton;
    LeftBtnPanel: TPanel;
    LinkEdit: TEdit;
    LinkLabel: TLabel;
    SaveButton: TSpeedButton;
    SeeAlsoMemo: TMemo;
    MoveToInheritedButton: TButton;
    OpenDialog: TOpenDialog;
    PageControl: TPageControl;
    SeeAlsoTabSheet: TTabSheet;
    ShortEdit: TEdit;
    ShortLabel: TLabel;
    ShortTabSheet: TTabSheet;
    InsertPrintShortSpeedButton: TSpeedButton;
    UnderlineFormatButton: TSpeedButton;
    procedure AddLinkToInheritedButtonClick(Sender: TObject);
    procedure ApplicationIdle(Sender: TObject; var Done: Boolean);
    procedure BrowseExampleButtonClick(Sender: TObject);
    procedure CopyFromInheritedButtonClick(Sender: TObject);
    procedure CopyShortToDescrMenuItemClick(Sender: TObject);
    procedure CreateButtonClick(Sender: TObject);
    procedure DescrMemoChange(Sender: TObject);
    procedure ErrorsMemoChange(Sender: TObject);
    procedure ExampleEditChange(Sender: TObject);
    procedure FormatButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure InsertLinkSpeedButtonClick(Sender: TObject);
    procedure LinkEditEditingDone(Sender: TObject);
    procedure MoveToInheritedButtonClick(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure SeeAlsoMemoEditingDone(Sender: TObject);
    procedure ShortEditEditingDone(Sender: TObject);
  private
    FCaretXY: TPoint;
    FModified: Boolean;
    FFlags: TFPDocEditorFlags;
    fUpdateLock: Integer;
    fSourceFilename: string;
    fChain: TCodeHelpElementChain;
    FOldValues: TFPDocElementValues;
    FOldVisualValues: TFPDocElementValues;
    function GetDoc: TXMLdocument;
    function GetDocFile: TLazFPDocFile;
    function GetSourceFilename: string;
    function GetFirstElement: TDOMNode;

    function GetContextTitle(Element: TCodeHelpElement): string;

    function FindInheritedIndex: integer;
    procedure Save;
    function GetGUIValues: TFPDocElementValues;
    procedure SetModified(const AValue: boolean);
    function WriteNode(Element: TCodeHelpElement; Values: TFPDocElementValues;
                       Interactive: Boolean): Boolean;
    procedure UpdateCodeCache;
    procedure UpdateChain;
    procedure UpdateCaption;
    procedure UpdateValueControls;
    procedure UpdateInheritedControls;
    procedure OnLazDocChanging(Sender: TObject; LazDocFPFile: TLazFPDocFile);
    procedure OnLazDocChanged(Sender: TObject; LazDocFPFile: TLazFPDocFile);
    procedure LoadGUIValues(Element: TCodeHelpElement);
    procedure MoveToInherited(Element: TCodeHelpElement);
    function ExtractIDFromLinkTag(const LinkTag: string; out ID, Title: string): boolean;
    function CreateElement(Element: TCodeHelpElement): Boolean;
    procedure UpdateButtons;
    function GetCurrentUnitName: string;
    function GetCurrentModuleName: string;
  public
    procedure Reset;
    procedure InvalidateChain;
    procedure UpdateFPDocEditor(const SrcFilename: string; const Caret: TPoint);
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure ClearEntry(DoSave: Boolean);
    property DocFile: TLazFPDocFile read GetDocFile;
    property Doc: TXMLdocument read GetDoc;
    property SourceFilename: string read GetSourceFilename;
    property CaretXY: TPoint read FCaretXY;
    property Modified: boolean read FModified write SetModified;
  end;

var
  FPDocEditor: TFPDocEditor = nil;

procedure DoShowFPDocEditor;

implementation

{ TFPDocEditor }

procedure DoShowFPDocEditor;
begin
  if FPDocEditor = Nil then begin
    Application.CreateForm(TFPDocEditor, FPDocEditor);
    EnvironmentOptions.IDEWindowLayoutList.ItemByEnum(nmiwFPDocEditorName).Apply;
  end;

  if not FPDocEditor.Visible then
    FPDocEditor.UpdateButtons;
  FPDocEditor.Show;
end;

function TFPDocEditor.GetFirstElement: TDOMNode;
var
  CurDocFile: TLazFPDocFile;
begin
  Result:=nil;
  CurDocFile:=DocFile;
  if CurDocFile=nil then exit;
  Result:=CurDocFile.GetFirstElement;
end;

procedure TFPDocEditor.FormCreate(Sender: TObject);
begin
  Caption := lisCodeHelpMainFormCaption;

  ShortTabSheet.Caption := lisCodeHelpShortTag;
  InheritedTabSheet.Caption := lisCodeHelpInherited;
  DescrTabSheet.Caption := lisCodeHelpDescrTag;
  ErrorsTabSheet.Caption := lisCodeHelpErrorsTag;
  SeeAlsoTabSheet.Caption := lisCodeHelpSeeAlsoTag;
  ExampleTabSheet.Caption := lisCodeHelpExampleTag;

  PageControl.PageIndex := 0;

  BoldFormatButton.Hint := lisCodeHelpHintBoldFormat;
  ItalicFormatButton.Hint := lisCodeHelpHintItalicFormat;
  UnderlineFormatButton.Hint := lisCodeHelpHintUnderlineFormat;
  InsertCodeTagButton.Hint := lisCodeHelpHintInsertCodeTag;
  InsertRemarkButton.Hint := lisCodeHelpHintRemarkTag;
  InsertVarTagButton.Hint := lisCodeHelpHintVarTag;
  InsertParagraphSpeedButton.Hint := lisCodeHelpInsertParagraphFormattingTag;
  InsertLinkSpeedButton.Hint := lisCodeHelpInsertALink;
  InsertPrintShortSpeedButton.Hint:=lisInsertPrintshortTag2;

  ShortLabel.Caption:=lisShort;
  LinkLabel.Caption:=lisLink;
  CreateButton.Caption := lisCodeHelpCreateButton;
  CreateButton.Enabled:=false;
  SaveButton.Caption := '';
  SaveButton.Enabled:=false;
  SaveButton.Hint:=lisHintSave;
  SaveButton.ShowHint:=true;

  BrowseExampleButton.Caption := lisCodeHelpBrowseExampleButton;
  
  MoveToInheritedButton.Caption:=lisLDMoveEntriesToInherited;
  CopyFromInheritedButton.Caption:=lisLDCopyFromInherited;
  AddLinkToInheritedButton.Caption:=lisLDAddLinkToInherited;

  Reset;
  
  CodeHelpBoss.AddHandlerOnChanging(@OnLazDocChanging);
  CodeHelpBoss.AddHandlerOnChanged(@OnLazDocChanged);
  Application.AddOnIdleHandler(@ApplicationIdle);
  
  Name := NonModalIDEWindowNames[nmiwFPDocEditorName];
  EnvironmentOptions.IDEWindowLayoutList.Apply(Self, Name);
  ControlDocker:=TLazControlDocker.Create(Self);
  ControlDocker.Name:='FPDocEditor';
  {$IFDEF EnableIDEDocking}
  ControlDocker.Manager:=LazarusIDE.DockingManager;
  {$ENDIF}

  BoldFormatButton.LoadGlyphFromLazarusResource('formatbold');
  UnderlineFormatButton.LoadGlyphFromLazarusResource('formatunderline');
  ItalicFormatButton.LoadGlyphFromLazarusResource('formatitalic');
  InsertVarTagButton.LoadGlyphFromLazarusResource('insertvartag');
  InsertCodeTagButton.LoadGlyphFromLazarusResource('insertcodetag');
  InsertRemarkButton.LoadGlyphFromLazarusResource('insertremark');
  SaveButton.LoadGlyphFromLazarusResource('laz_save');
end;

procedure TFPDocEditor.FormDestroy(Sender: TObject);
begin
  Reset;
  FreeAndNil(fChain);
  if assigned(CodeHelpBoss) then
    CodeHelpBoss.RemoveAllHandlersOfObject(Self);
  Application.RemoveAllHandlersOfObject(Self);
end;

procedure TFPDocEditor.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key=VK_S) and (Shift=[ssCtrl]) then begin
    Save;
    Key:=VK_UNKNOWN;
  end;
end;

procedure TFPDocEditor.FormResize(Sender: TObject);
begin

end;

procedure TFPDocEditor.FormatButtonClick(Sender: TObject);

  procedure InsertTag(const StartTag, EndTag: String);
  begin
    if PageControl.ActivePage = ShortTabSheet then
      ShortEdit.SelText := StartTag + ShortEdit.SelText + EndTag
    else if PageControl.ActivePage = DescrTabSheet then
      DescrMemo.SelText := StartTag + DescrMemo.SelText + EndTag
    else if PageControl.ActivePage = ErrorsTabSheet then
      ErrorsMemo.SelText := StartTag + ErrorsMemo.SelText + EndTag
    else
      exit;
    Modified:=true;
  end;

begin
  case TSpeedButton(Sender).Tag of
    //bold
    0:
      InsertTag('<b>', '</b>');
    //italic
    1:
      InsertTag('<i>', '</i>');
    //underline
    2:
      InsertTag('<u>', '</u>');
    //code tag
    3:
      InsertTag('<p><code>', '</code></p>');
    //remark tag
    4:
      InsertTag('<p><remark>', '</remark></p>');
    //var tag
    5:
      InsertTag('<var>', '</var>');
    //paragraph tag
    6:
      InsertTag('<p>', '</p>');
    //printshort
    7:
      if (fChain<>nil) and (fChain.Count>0) then
        InsertTag('<printshort id="'+fChain[0].ElementName+'"/>','');
  end;
end;

procedure TFPDocEditor.InsertLinkSpeedButtonClick(Sender: TObject);
var
  Link: string;
  LinkTitle: string;
  LinkSrc: String;
begin
  if (ShowFPDocLinkEditorDialog(fSourceFilename,DocFile,Link,LinkTitle)<>mrOk)
  or (Link='') then exit;
  LinkSrc:='<link id="'+Link+'"';
  if LinkTitle='' then begin
    LinkSrc:=LinkSrc+'/>';
  end else begin
    LinkSrc:=LinkSrc+'>'+LinkTitle+'</link>';
  end;
  if PageControl.ActivePage = ShortTabSheet then
    ShortEdit.SelText := LinkSrc;
  if PageControl.ActivePage = DescrTabSheet then
    DescrMemo.SelText := LinkSrc;
  if PageControl.ActivePage = SeeAlsoTabSheet then
    SeeAlsoMemo.SelText := LinkSrc;
  if PageControl.ActivePage = ErrorsTabSheet then
    ErrorsMemo.SelText := LinkSrc;
  Modified:=true;
end;

procedure TFPDocEditor.ApplicationIdle(Sender: TObject; var Done: Boolean);
begin
  if fUpdateLock>0 then
  begin
    DebugLn(['WARNING: TFPDocEditor.ApplicationIdle fUpdateLock>0']);
    exit;
  end;
  Done:=false;
  if fpdefCodeCacheNeedsUpdate in FFlags then
    UpdateCodeCache
  else if fpdefChainNeedsUpdate in FFlags then
    UpdateChain
  else if fpdefCaptionNeedsUpdate in FFlags then
    UpdateCaption
  else if fpdefValueControlsNeedsUpdate in FFlags then
    UpdateValueControls
  else if fpdefInheritedControlsNeedsUpdate in FFlags then
    UpdateInheritedControls
  else
    Done:=true;
end;

procedure TFPDocEditor.MoveToInheritedButtonClick(Sender: TObject);
var
  i: Integer;
  Element: TCodeHelpElement;
  Candidates: TFPList;
  FPDocSelectInheritedDlg: TFPDocSelectInheritedDlg;
  ShortDescr: String;
begin
  if fChain=nil then exit;
  Candidates:=nil;
  FPDocSelectInheritedDlg:=nil;
  try
    // find all entries till the first inherited entry with a description
    for i:=1 to fChain.Count-1 do begin
      Element:=fChain[i];
      if Candidates=nil then
        Candidates:=TFPList.Create;
      Candidates.Add(Element);
      if (Element.ElementNode<>nil)
      and (Element.FPDocFile.GetValueFromNode(Element.ElementNode,fpdiShort)<>'')
      then
        break;
    end;
    
    // choose one entry
    if (Candidates=nil) or (Candidates.Count=0) then exit;
    if Candidates.Count=1 then begin
      // there is only one candidate
      Element:=TCodeHelpElement(Candidates[0]);
      if (Element.ElementNode<>nil) then begin
        ShortDescr:=Element.FPDocFile.GetValueFromNode(Element.ElementNode,fpdiShort);
        if ShortDescr<>'' then begin
          // the inherited entry already contains a description.
          // ask if it should be really replaced
          if QuestionDlg(lisCodeHelpConfirmreplace,
            GetContextTitle(Element)+' already contains the help:'+#13
            +ShortDescr,
            mtConfirmation,[mrYes,lisCodeHelpReplaceButton,mrCancel],0)<>mrYes then exit;
        end;
      end;
    end else begin
      // there is more than one candidate
      // => ask which one to replace
      FPDocSelectInheritedDlg:=TFPDocSelectInheritedDlg.Create(nil);
      FPDocSelectInheritedDlg.InheritedComboBox.Items.Clear;
      for i:=0 to Candidates.Count-1 do begin
        Element:=TCodeHelpElement(Candidates[i]);
        FPDocSelectInheritedDlg.InheritedComboBox.Items.Add(
                                                      GetContextTitle(Element));
      end;
      if FPDocSelectInheritedDlg.ShowModal<>mrOk then exit;
      i:=FPDocSelectInheritedDlg.InheritedComboBox.ItemIndex;
      if i<0 then exit;
      Element:=TCodeHelpElement(Candidates[i]);
    end;

    // move the content of the current entry to the inherited entry
    MoveToInherited(Element);
  finally
    FPDocSelectInheritedDlg.Free;
    Candidates.Free;
  end;
end;

procedure TFPDocEditor.PageControlChange(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TFPDocEditor.SaveButtonClick(Sender: TObject);
begin
  Save;
end;

procedure TFPDocEditor.SeeAlsoMemoEditingDone(Sender: TObject);
begin
  if SeeAlsoMemo.Text<>FOldVisualValues[fpdiSeeAlso] then
    Modified:=true;
end;

procedure TFPDocEditor.ShortEditEditingDone(Sender: TObject);
begin
  if ShortEdit.Text<>FOldVisualValues[fpdiShort] then
    Modified:=true;
end;

function TFPDocEditor.GetContextTitle(Element: TCodeHelpElement): string;
// get codetools path. for example: TButton.Align
begin
  Result:='';
  if Element=nil then exit;
  Result:=Element.ElementName;
end;

function TFPDocEditor.GetDoc: TXMLdocument;
begin
  if DocFile<>nil then
    Result:=DocFile.Doc
  else
    Result:=nil;
end;

function TFPDocEditor.GetDocFile: TLazFPDocFile;
begin
  Result:=nil;
  if fChain=nil then exit;
  Result:=fChain.DocFile;
end;

function TFPDocEditor.GetSourceFilename: string;
begin
  Result:=fSourceFilename;
end;

procedure TFPDocEditor.UpdateCaption;
var
  strCaption: String;
  Filename: String;
begin
  if fUpdateLock>0 then begin
    Include(FFlags,fpdefCaptionNeedsUpdate);
    exit;
  end;
  Exclude(FFlags,fpdefCaptionNeedsUpdate);
  
  {$IFDEF VerboseCodeHelp}
  DebugLn(['TFPDocEditForm.UpdateCaption START']);
  {$ENDIF}
  strCaption := lisCodeHelpMainFormCaption + ' - ';

  if (fChain <> nil) and (fChain.Count>0) then
    strCaption := strCaption + GetContextTitle(fChain[0]) + ' - '
  else
    strCaption := strCaption + lisCodeHelpNoTagCaption + ' - ';

  if DocFile<>nil then begin
    Filename:=DocFile.Filename;
    if (LazarusIDE.ActiveProject<>nil) then
      LazarusIDE.ActiveProject.ShortenFilename(Filename);
    Caption := strCaption + Filename;
  end else
    Caption := strCaption + lisCodeHelpNoTagCaption;
  {$IFDEF VerboseCodeHelp}
  DebugLn(['TLazDocForm.UpdateCaption ',Caption]);
  {$ENDIF}
end;

procedure TFPDocEditor.UpdateValueControls;
var
  Element: TCodeHelpElement;
begin
  if fUpdateLock>0 then begin
    Include(FFlags,fpdefValueControlsNeedsUpdate);
    exit;
  end;
  Exclude(FFlags,fpdefValueControlsNeedsUpdate);

  {$IFDEF VerboseCodeHelp}
  DebugLn(['TFPDocEditForm.UpdateValueControls START']);
  {$ENDIF}
  Element:=nil;
  if (fChain<>nil) and (fChain.Count>0) then
    Element:=fChain[0];
  LoadGUIValues(Element);
  SaveButton.Enabled:=FModified;
end;

procedure TFPDocEditor.UpdateInheritedControls;
var
  i: LongInt;
  Element: TCodeHelpElement;
  ShortDescr: String;
begin
  if fUpdateLock>0 then begin
    Include(FFlags,fpdefInheritedControlsNeedsUpdate);
    exit;
  end;
  Exclude(FFlags,fpdefInheritedControlsNeedsUpdate);

  {$IFDEF VerboseCodeHelp}
  DebugLn(['TFPDocEditForm.UpdateInheritedControls START']);
  {$ENDIF}
  i:=FindInheritedIndex;
  if i<0 then begin
    InheritedShortEdit.Text:='';
    InheritedShortEdit.Enabled:=false;
    InheritedShortLabel.Caption:=lisCodeHelpnoinheriteddescriptionfound;
  end else begin
    Element:=fChain[i];
    ShortDescr:=Element.FPDocFile.GetValueFromNode(Element.ElementNode,fpdiShort);
    InheritedShortEdit.Text:=ShortDescr;
    InheritedShortEdit.Enabled:=true;
    InheritedShortLabel.Caption:=lisCodeHelpShortdescriptionOf+' '
                                 +GetContextTitle(Element);
  end;
  MoveToInheritedButton.Enabled:=(fChain<>nil)
                                 and (fChain.Count>1)
                                 and (ShortEdit.Text<>'');
  CopyFromInheritedButton.Enabled:=(i>=0);
  AddLinkToInheritedButton.Enabled:=(i>=0);
end;

procedure TFPDocEditor.UpdateChain;
var
  Code: TCodeBuffer;
  LDResult: TCodeHelpParseResult;
  NewChain: TCodeHelpElementChain;
  CacheWasUsed: Boolean;
begin
  FreeAndNil(fChain);
  if fUpdateLock>0 then begin
    Include(FFlags,fpdefChainNeedsUpdate);
    exit;
  end;
  Exclude(FFlags,fpdefChainNeedsUpdate);

  if (fSourceFilename='') or (CaretXY.X<1) or (CaretXY.Y<1) then exit;

  {$IFDEF VerboseCodeHelp}
  DebugLn(['TFPDocEditForm.UpdateChain START ',fSourceFilename,' ',dbgs(CaretXY)]);
  {$ENDIF}
  NewChain:=nil;
  try
    // fetch pascal source
    Code:=CodeToolBoss.LoadFile(fSourceFilename,true,false);
    if Code=nil then begin
      DebugLn(['TFPDocEditForm.UpdateChain failed loading ',fSourceFilename]);
      exit;
    end;

    // start getting the lazdoc element chain
    LDResult:=CodeHelpBoss.GetElementChain(Code,CaretXY.X,CaretXY.Y,true,
                                           NewChain,CacheWasUsed);
    case LDResult of
    chprParsing:
      begin
        Include(FFlags,fpdefChainNeedsUpdate);
        DebugLn(['TFPDocEditForm.UpdateChain ToDo: still parsing LazDocBoss.GetElementChain for ',fSourceFilename,' ',dbgs(CaretXY)]);
        exit;
      end;
    chprFailed:
      begin
        {$IFDEF VerboseLazDocFails}
        DebugLn(['TFPDocEditForm.UpdateChain failed LazDocBoss.GetElementChain for ',fSourceFilename,' ',dbgs(CaretXY)]);
        {$ENDIF}
        exit;
      end;
    else
      {$IFDEF VerboseCodeHelp}
      NewChain.WriteDebugReport;
      {$ENDIF}
      fChain:=NewChain;
      NewChain:=nil;
    end;
  finally
    NewChain.Free;
  end;
end;

procedure TFPDocEditor.OnLazDocChanging(Sender: TObject;
  LazDocFPFile: TLazFPDocFile);
begin
  if fpdefWriting in FFlags then exit;
  if (fChain<>nil) and (fChain.IndexOfFile(LazDocFPFile)>=0) then
    InvalidateChain;
end;

procedure TFPDocEditor.OnLazDocChanged(Sender: TObject;
  LazDocFPFile: TLazFPDocFile);
begin
  if fpdefWriting in FFlags then exit;

end;

procedure TFPDocEditor.LoadGUIValues(Element: TCodeHelpElement);
var
  EnabledState: Boolean;
  OldModified: Boolean;
begin
  OldModified:=FModified;
  
  EnabledState := (Element<>nil) and (Element.ElementNode<>nil);
  
  CreateButton.Enabled := (Element<>nil) and (Element.ElementNode=nil)
                          and (Element.ElementName<>'');

  if EnabledState then
  begin
    FOldValues:=Element.FPDocFile.GetValuesFromNode(Element.ElementNode);
    FOldVisualValues[fpdiShort]:=ReplaceLineEndings(FOldValues[fpdiShort],'');
    FOldVisualValues[fpdiElementLink]:=ConvertLineEndings(FOldValues[fpdiElementLink]);
    FOldVisualValues[fpdiDescription]:=ConvertLineEndings(FOldValues[fpdiDescription]);
    FOldVisualValues[fpdiErrors]:=ConvertLineEndings(FOldValues[fpdiErrors]);
    FOldVisualValues[fpdiSeeAlso]:=ConvertLineEndings(FOldValues[fpdiSeeAlso]);
    FOldVisualValues[fpdiExample]:=ConvertLineEndings(FOldValues[fpdiExample]);
    DebugLn(['TFPDocEditor.LoadGUIValues Short="',dbgstr(FOldValues[fpdiShort]),'"']);
  end
  else
  begin
    FOldVisualValues[fpdiShort]:=lisCodeHelpNoDocumentation;
    FOldVisualValues[fpdiElementLink]:=lisCodeHelpNoDocumentation;
    FOldVisualValues[fpdiDescription]:=lisCodeHelpNoDocumentation;
    FOldVisualValues[fpdiErrors]:=lisCodeHelpNoDocumentation;
    FOldVisualValues[fpdiSeeAlso]:=lisCodeHelpNoDocumentation;
    FOldVisualValues[fpdiExample]:=lisCodeHelpNoDocumentation;
  end;
  ShortEdit.Text := FOldVisualValues[fpdiShort];
  LinkEdit.Text := FOldVisualValues[fpdiElementLink];
  DescrMemo.Lines.Text := FOldVisualValues[fpdiDescription];
  SeeAlsoMemo.Text := FOldVisualValues[fpdiSeeAlso];
  ErrorsMemo.Lines.Text := FOldVisualValues[fpdiErrors];
  ExampleEdit.Text := FOldVisualValues[fpdiExample];

  ShortEdit.Enabled := EnabledState;
  LinkEdit.Enabled := EnabledState;
  DescrMemo.Enabled := EnabledState;
  SeeAlsoMemo.Enabled := EnabledState;
  ErrorsMemo.Enabled := EnabledState;
  ExampleEdit.Enabled := EnabledState;
  BrowseExampleButton.Enabled := EnabledState;

  FModified:=OldModified;
end;

procedure TFPDocEditor.MoveToInherited(Element: TCodeHelpElement);
var
  Values: TFPDocElementValues;
begin
  Values:=GetGUIValues;
  WriteNode(Element,Values,true);
end;

function TFPDocEditor.ExtractIDFromLinkTag(const LinkTag: string; out ID, Title: string
  ): boolean;
// extract id and title from example:
// <link id="TCustomControl"/>
// <link id="#lcl.Graphics.TCanvas">TCanvas</link>
var
  StartPos: Integer;
  EndPos: LongInt;
begin
  Result:=false;
  ID:='';
  Title:='';
  StartPos:=length('<link id="')+1;
  if copy(LinkTag,1,StartPos-1)<>'<link id="' then
    exit;
  EndPos:=StartPos;
  while (EndPos<=length(LinkTag)) do begin
    if LinkTag[EndPos]='"' then begin
      ID:=copy(LinkTag,StartPos,EndPos-StartPos);
      Title:='';
      Result:=true;
      // extract title
      StartPos:=EndPos;
      while (StartPos<=length(LinkTag)) and (LinkTag[StartPos]<>'>') do inc(StartPos);
      if LinkTag[StartPos-1]='\' then begin
        // no title
      end else begin
        // has title
        inc(StartPos);
        EndPos:=StartPos;
        while (EndPos<=length(LinkTag)) and (LinkTag[EndPos]<>'<') do inc(EndPos);
        Title:=copy(LinkTag,StartPos,EndPos-StartPos);
      end;
      exit;
    end;
    inc(EndPos);
  end;
end;

function TFPDocEditor.CreateElement(Element: TCodeHelpElement): Boolean;
var
  NewElement: TCodeHelpElement;
begin
  DebugLn(['TFPDocEditForm.CreateElement ']);
  if (Element=nil) or (Element.ElementName='') then exit(false);
  NewElement:=nil;
  Include(FFlags,fpdefWriting);
  try
    Result:=CodeHelpBoss.CreateElement(Element.CodeXYPos.Code,
                            Element.CodeXYPos.X,Element.CodeXYPos.Y,NewElement);
  finally
    Exclude(FFlags,fpdefWriting);
    NewElement.Free;
  end;
  Reset;
  InvalidateChain;
end;

procedure TFPDocEditor.UpdateButtons;
var
  HasEdit: Boolean;
begin
  HasEdit:=(PageControl.ActivePage = ShortTabSheet)
        or (PageControl.ActivePage = DescrTabSheet)
        or (PageControl.ActivePage = SeeAlsoTabSheet)
        or (PageControl.ActivePage = ErrorsTabSheet);
  BoldFormatButton.Enabled:=HasEdit;
  ItalicFormatButton.Enabled:=HasEdit;
  UnderlineFormatButton.Enabled:=HasEdit;
  InsertCodeTagButton.Enabled:=HasEdit;
  InsertLinkSpeedButton.Enabled:=HasEdit;
  InsertParagraphSpeedButton.Enabled:=HasEdit;
  InsertRemarkButton.Enabled:=HasEdit;
  InsertVarTagButton.Enabled:=HasEdit;
end;

function TFPDocEditor.GetCurrentUnitName: string;
begin
  if (fChain<>nil) and (fChain.Count>0) then
    Result:=fChain[0].ElementUnitName
  else
    Result:='';
end;

function TFPDocEditor.GetCurrentModuleName: string;
begin
  if (fChain<>nil) and (fChain.Count>0) then
    Result:=fChain[0].ElementOwnerName
  else
    Result:='';
end;

procedure TFPDocEditor.Reset;
begin
  FreeAndNil(fChain);

  // clear all element editors/viewers
  ShortEdit.Clear;
  LinkEdit.Clear;
  DescrMemo.Clear;
  SeeAlsoMemo.Clear;
  ErrorsMemo.Clear;
  ExampleEdit.Clear;

  Modified := False;
  CreateButton.Enabled:=false;
end;

procedure TFPDocEditor.InvalidateChain;
begin
  FreeAndNil(fChain);
  FFlags:=FFlags+[fpdefCodeCacheNeedsUpdate,fpdefChainNeedsUpdate,
      fpdefCaptionNeedsUpdate,fpdefValueControlsNeedsUpdate,
      fpdefInheritedControlsNeedsUpdate];
end;

procedure TFPDocEditor.UpdateFPDocEditor(const SrcFilename: string;
  const Caret: TPoint);
var
  NewSrcFilename: String;
begin
  // save the current changes to documentation
  Save;
  // check if visible
  if not Visible then exit;
  
  NewSrcFilename:=CleanAndExpandFilename(SrcFilename);
  if (NewSrcFilename=SourceFilename) and (CompareCaret(Caret,CaretXY)=0)
  and (fChain<>nil) and fChain.IsValid
  and (not LazarusIDE.NeedSaveSourceEditorChangesToCodeCache(-1)) then
    exit;

  FCaretXY:=Caret;
  fSourceFilename:=NewSrcFilename;
  
  Reset;
  InvalidateChain;
end;

procedure TFPDocEditor.BeginUpdate;
begin
  inc(fUpdateLock);
end;

procedure TFPDocEditor.EndUpdate;
begin
  dec(fUpdateLock);
  if fUpdateLock<0 then RaiseGDBException('');
  if fUpdateLock=0 then begin
    if fpdefCaptionNeedsUpdate in FFlags then UpdateCaption;
  end;
end;

procedure TFPDocEditor.ClearEntry(DoSave: Boolean);
begin
  Modified:=true;
  ShortEdit.Text:='';
  DescrMemo.Text:='';
  SeeAlsoMemo.Text:='';
  ErrorsMemo.Text:='';
  ExampleEdit.Text:='';
  if DoSave then Save;
end;

procedure TFPDocEditor.Save;
var
  Values: TFPDocElementValues;
begin
  //DebugLn(['TFPDocEditor.Save FModified=',FModified]);
  if not FModified then begin
    SaveButton.Enabled:=false;
    Exit; // nothing changed => exit
  end;
  FModified:=false;
  SaveButton.Enabled:=false;
  if (fChain=nil) or (fChain.Count=0) then begin
    DebugLn(['TFPDocEditor.Save failed: no chain']);
    exit;
  end;
  if not fChain.IsValid then begin
    DebugLn(['TFPDocEditor.Save failed: chain not valid']);
    exit;
  end;
  Values:=GetGUIValues;
  if not WriteNode(fChain[0],Values,true) then begin
    DebugLn(['TLazDocForm.Save WriteNode FAILED']);
  end;
end;

function TFPDocEditor.GetGUIValues: TFPDocElementValues;
var
  i: TFPDocItem;
begin
  Result[fpdiShort]:=ShortEdit.Text;
  Result[fpdiDescription]:=DescrMemo.Text;
  Result[fpdiErrors]:=ErrorsMemo.Text;
  Result[fpdiSeeAlso]:=SeeAlsoMemo.Text;
  Result[fpdiExample]:=ExampleEdit.Text;
  Result[fpdiElementLink]:=LinkEdit.Text;
  for i:=Low(TFPDocItem) to High(TFPDocItem) do
    if Trim(Result[i])=lisCodeHelpNoDocumentation then
      Result[i]:='';
end;

procedure TFPDocEditor.SetModified(const AValue: boolean);
begin
  if FModified=AValue then exit;
  FModified:=AValue;
  SaveButton.Enabled:=FModified;
end;

function TFPDocEditor.WriteNode(Element: TCodeHelpElement;
  Values: TFPDocElementValues; Interactive: Boolean): Boolean;
var
  TopNode: TDOMNode;
  CurDocFile: TLazFPDocFile;
  CurDoc: TXMLDocument;

  function Check(Test: boolean; const  Msg: string): Boolean;
  var
    CurName: String;
  begin
    Result:=Test;
    if not Test then exit;
    DebugLn(['TLazDocForm.WriteNode  ERROR ',Msg]);
    if Interactive then begin;
      if Element.FPDocFile<>nil then
        CurName:=Element.FPDocFile.Filename
      else
        CurName:=Element.ElementName;
      MessageDlg('Write error',
        'Error writing "'+CurName+'"'#13
        +Msg,mtError,[mbCancel],0);
    end;
  end;

begin
  Result:=false;
  if fpdefWriting in FFlags then begin
    DebugLn(['TFPDocEditForm.WriteNode inconsistency detected: recursive write']);
    exit;
  end;
  
  if Check(Element=nil,'Element=nil') then exit;
  CurDocFile:=Element.FPDocFile;
  if Check(CurDocFile=nil,'Element.FPDocFile=nil') then begin
    // no fpdoc file found
    DebugLn(['TFPDocEditForm.WriteNode TODO: implement creating new fpdoc file']);
    exit;
  end;
  CurDoc:=CurDocFile.Doc;
  if Check(CurDoc=nil,'Element.FPDocFile.Doc=nil') then exit;
  if Check(not Element.ElementNodeValid,'not Element.ElementNodeValid') then exit;
  TopNode:=Element.ElementNode;
  if Check(TopNode=nil,'TopNode=nil') then begin
    // no old node found
    Check(false,'no old node found. TODO: implement creating a new.');
    Exit;
  end;

  Include(FFlags,fpdefWriting);
  CurDocFile.BeginUpdate;
  try
    CurDocFile.SetChildValue(TopNode,'short',Values[fpdiShort]);
    CurDocFile.SetChildValue(TopNode,'elementlink',Values[fpdiElementLink]);
    CurDocFile.SetChildValue(TopNode,'descr',Values[fpdiDescription]);
    CurDocFile.SetChildValue(TopNode,'errors',Values[fpdiErrors]);
    CurDocFile.SetChildValue(TopNode,'seealso',Values[fpdiSeeAlso]);
    CurDocFile.SetChildValue(TopNode,'example',Values[fpdiExample]);
  finally
    CurDocFile.EndUpdate;
    fChain.MakeValid;
    Exclude(FFlags,fpdefWriting);
  end;

  if CodeHelpBoss.SaveFPDocFile(CurDocFile)<>mrOk then begin
    DebugLn(['TFPDocEditForm.WriteNode failed writing ',CurDocFile.Filename]);
    exit;
  end;
  Result:=true;
end;

procedure TFPDocEditor.UpdateCodeCache;
begin
  if fUpdateLock>0 then begin
    Include(FFlags,fpdefCodeCacheNeedsUpdate);
    exit;
  end;
  Exclude(FFlags,fpdefCodeCacheNeedsUpdate);
  LazarusIDE.SaveSourceEditorChangesToCodeCache(-1);
end;

procedure TFPDocEditor.ErrorsMemoChange(Sender: TObject);
begin
  if ErrorsMemo.Text<>FOldVisualValues[fpdiErrors] then
    Modified:=true;
end;

procedure TFPDocEditor.ExampleEditChange(Sender: TObject);
begin
  if ExampleEdit.Text<>FOldVisualValues[fpdiExample] then
    Modified:=true;
end;

function TFPDocEditor.FindInheritedIndex: integer;
// returns Index in chain of an overriden Element with a short description
// returns -1 if not found
var
  Element: TCodeHelpElement;
begin
  if (fChain<>nil) then begin
    Result:=1;
    while (Result<fChain.Count) do begin
      Element:=fChain[Result];
      if (Element.ElementNode<>nil)
      and (Element.FPDocFile.GetValueFromNode(Element.ElementNode,fpdiShort)<>'')
      then
        exit;
      inc(Result);
    end;
  end;
  Result:=-1;
end;

procedure TFPDocEditor.LinkEditEditingDone(Sender: TObject);
begin
  if LinkEdit.Text<>FOldVisualValues[fpdiElementLink] then
    Modified:=true;
end;

procedure TFPDocEditor.AddLinkToInheritedButtonClick(Sender: TObject);
var
  i: LongInt;
  Element: TCodeHelpElement;
  Link: String;
begin
  i:=FindInheritedIndex;
  if i<0 then exit;
  //DebugLn(['TFPDocEditor.AddLinkToInheritedButtonClick ']);
  Element:=fChain[i];
  Link:=Element.ElementName;
  if Element.ElementUnitName<>'' then begin
    Link:=Element.ElementUnitName+'.'+Link;
    if Element.ElementOwnerName<>'' then
      Link:='#'+Element.ElementOwnerName+'.'+Link;
  end;
  if Link<>LinkEdit.Text then begin
    LinkEdit.Text:=Link;
    Modified:=true;
  end;
end;

procedure TFPDocEditor.BrowseExampleButtonClick(Sender: TObject);
begin
  if Doc=nil then exit;
  if OpenDialog.Execute then
    ExampleEdit.Text := SetDirSeparators(ExtractRelativepath(
      ExtractFilePath(DocFile.Filename), OpenDialog.FileName));
end;

procedure TFPDocEditor.CopyFromInheritedButtonClick(Sender: TObject);
var
  i: LongInt;
begin
  i:=FindInheritedIndex;
  if i<0 then exit;
  DebugLn(['TFPDocEditForm.CopyFromInheritedButtonClick ']);
  if ShortEdit.Text<>'' then begin
    if QuestionDlg('Confirm replace',
      GetContextTitle(fChain[0])+' already contains the help:'+#13
      +ShortEdit.Text,
      mtConfirmation,[mrYes,'Replace',mrCancel],0)<>mrYes then exit;
  end;
  LoadGUIValues(fChain[i]);
  Modified:=true;
end;

procedure TFPDocEditor.CopyShortToDescrMenuItemClick(Sender: TObject);
begin
  DescrMemo.Append(ShortEdit.Text);
  Modified:=true;
end;

procedure TFPDocEditor.CreateButtonClick(Sender: TObject);
begin
  if (fChain=nil) or (fChain.Count=0) then exit;
  CreateElement(fChain[0]);
end;

procedure TFPDocEditor.DescrMemoChange(Sender: TObject);
begin
  if DescrMemo.Text<>FOldVisualValues[fpdiDescription] then
    Modified:=true;
end;

initialization
  // first include the lfm
  {$I fpdoceditwindow.lrs}
  // then the items
  {$I lazdoc.lrs}

end.
