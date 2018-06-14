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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  see for todo list: http://wiki.lazarus.freepascal.org/index.php/LazDoc
}

unit FPDocEditWindow;

{$mode objfpc}{$H+}

{off $define VerboseCodeHelp}

interface

uses
  // FCL
  Classes, SysUtils,
  // LazUtils
  Laz2_DOM, Laz2_XMLRead, LazUtilities, LazTracer,
  // LCL
  LResources, StdCtrls, Buttons, ComCtrls, Controls, Dialogs,
  ExtCtrls, Forms, Graphics, LCLType,
  // Synedit
  SynEdit, SynHighlighterXML,
  // codetools
  FileProcs, CodeCache, CodeToolManager, CTXMLFixFragment,
  // IDEIntf
  IDEWindowIntf, ProjectIntf, LazIDEIntf, IDEHelpIntf, Menus,
  SrcEditorIntf, IDEDialogs, LazFileUtils, IDEImagesIntf,
  // IDE
  IDEOptionDefs, EnvironmentOpts, PackageSystem, IDEProcs, LazarusIDEStrConsts,
  FPDocSelectInherited, FPDocSelectLink, CodeHelp;

type
  TFPDocEditorFlag = (
    fpdefReading,
    fpdefWriting,
    fpdefCodeCacheNeedsUpdate,
    fpdefChainNeedsUpdate,
    fpdefCaptionNeedsUpdate,
    fpdefValueControlsNeedsUpdate,
    fpdefInheritedControlsNeedsUpdate,
    fpdefTopicSettingUp,
    fpdefTopicNeedsUpdate,
    fpdefWasHidden
    );
  TFPDocEditorFlags = set of TFPDocEditorFlag;
  
  { TFPDocEditor }

  TFPDocEditor = class(TForm)
    AddLinkToInheritedButton: TButton;
    BoldFormatButton: TSpeedButton;
    BrowseExampleButton: TButton;
    OpenXMLButton: TButton;
    ShortPanel: TPanel;
    DescrShortEdit: TEdit;
    SynXMLSyn1: TSynXMLSyn;
    TopicShort: TEdit;
    TopicDescrSynEdit: TSynEdit;
    Panel3: TPanel;
    TopicListBox: TListBox;
    NewTopicNameEdit: TEdit;
    NewTopicButton: TButton;
    CopyFromInheritedButton: TButton;
    CreateButton: TButton;
    DescrSynEdit: TSynEdit;
    DescrTabSheet: TTabSheet;
    ErrorsSynEdit: TSynEdit;
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
    Panel1: TPanel;
    Panel2: TPanel;
    SaveButton: TSpeedButton;
    SeeAlsoSynEdit: TSynEdit;
    MoveToInheritedButton: TButton;
    OpenDialog: TOpenDialog;
    PageControl: TPageControl;
    SeeAlsoTabSheet: TTabSheet;
    ShortEdit: TEdit;
    ShortLabel: TLabel;
    ShortTabSheet: TTabSheet;
    InsertPrintShortSpeedButton: TSpeedButton;
    InsertURLTagSpeedButton: TSpeedButton;
    TopicSheet: TTabSheet;
    UnderlineFormatButton: TSpeedButton;
    procedure AddLinkToInheritedButtonClick(Sender: TObject);
    procedure ApplicationIdle(Sender: TObject; var Done: Boolean);
    procedure BrowseExampleButtonClick(Sender: TObject);
    procedure CopyFromInheritedButtonClick(Sender: TObject);
    procedure CopyShortToDescrMenuItemClick(Sender: TObject);
    procedure CreateButtonClick(Sender: TObject);
    procedure DescrSynEditChange(Sender: TObject);
    procedure ErrorsSynEditChange(Sender: TObject);
    procedure ExampleEditChange(Sender: TObject);
    procedure FormatButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure InsertLinkSpeedButtonClick(Sender: TObject);
    procedure LinkEditChange(Sender: TObject);
    procedure MoveToInheritedButtonClick(Sender: TObject);
    procedure NewTopicButtonClick(Sender: TObject);
    procedure OpenXMLButtonClick(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure SeeAlsoSynEditChange(Sender: TObject);
    procedure ShortEditChange(Sender: TObject);
    procedure TopicControlEnter(Sender: TObject);
    procedure TopicDescrSynEditChange(Sender: TObject);
    procedure TopicListBoxClick(Sender: TObject);
  private
    FCaretXY: TPoint;
    FModified: Boolean;
    FFlags: TFPDocEditorFlags;
    fUpdateLock: Integer;
    fSourceFilename: string;
    fDocFile: TLazFPDocFile;
    fChain: TCodeHelpElementChain;
    FOldValues: TFPDocElementValues;
    FOldVisualValues: TFPDocElementValues;
    function GetDoc: TXMLdocument;
    function GetDocFile: TLazFPDocFile;
    function GetSourceFilename: string;
    function GetFirstElement: TDOMNode;

    function GetContextTitle(Element: TCodeHelpElement): string;

    function FindInheritedIndex: integer;
    procedure Save(CheckGUI: boolean = false);
    function GetGUIValues: TFPDocElementValues;
    procedure SetModified(const AValue: boolean);
    function WriteNode(Element: TCodeHelpElement; Values: TFPDocElementValues;
                       Interactive: Boolean): Boolean;
    procedure UpdateCodeCache;
    procedure UpdateChain;
    procedure UpdateCaption;
    procedure UpdateValueControls;
    procedure UpdateInheritedControls;
    procedure OnFPDocChanging(Sender: TObject; FPDocFPFile: TLazFPDocFile);
    procedure OnFPDocChanged(Sender: TObject; FPDocFPFile: TLazFPDocFile);
    procedure LoadGUIValues(Element: TCodeHelpElement);
    procedure MoveToInherited(Element: TCodeHelpElement);
    function GetDefaultDocFile(CreateIfNotExists: Boolean = False): TLazFPDocFile;
    function ExtractIDFromLinkTag(const LinkTag: string; out ID, Title: string): boolean;
    function CreateElement(Element: TCodeHelpElement): Boolean;
    procedure UpdateButtons;
    function GetCurrentUnitName: string;
    function GetCurrentOwnerName: string;
    procedure JumpToError(Item : TFPDocItem; LineCol: TPoint);
    procedure OpenXML;
    function GUIModified: boolean;
    procedure DoEditorUpdate(Sender: TObject);
  private
    FFollowCursor: boolean;
    FIdleConnected: boolean;
    FLastTopicControl: TControl;
    FCurrentTopic: String;
    procedure SetFollowCursor(AValue: boolean);
    procedure SetIdleConnected(AValue: boolean);
    procedure UpdateTopicCombo;
    procedure ClearTopicControls;
    procedure UpdateTopic;
  protected
    procedure UpdateShowing; override;
    procedure Loaded; override;
  public
    procedure Reset;
    procedure InvalidateChain;
    procedure LoadIdentifierAt(const SrcFilename: string; const Caret: TPoint);
    procedure LoadIdentifierAtCursor;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure ClearEntry(DoSave: Boolean);
    property DocFile: TLazFPDocFile read GetDocFile;
    property Doc: TXMLdocument read GetDoc;
    property SourceFilename: string read GetSourceFilename;
    property CaretXY: TPoint read FCaretXY;
    property Modified: boolean read FModified write SetModified;
    property IdleConnected: boolean read FIdleConnected write SetIdleConnected;
    property FollowCursor: boolean read FFollowCursor write SetFollowCursor;
  end;

var
  FPDocEditor: TFPDocEditor = nil;

procedure DoShowFPDocEditor(State: TIWGetFormState = iwgfShowOnTop);

implementation

{$R *.lfm}
{$R lazdoc.res}

{ TFPDocEditor }

procedure DoShowFPDocEditor(State: TIWGetFormState);
begin
  if FPDocEditor = Nil then
    IDEWindowCreators.CreateForm(FPDocEditor,TFPDocEditor,
       State=iwgfDisabled,LazarusIDE.OwningComponent)
  else if State=iwgfDisabled then
    FPDocEditor.DisableAutoSizing{$IFDEF DebugDisableAutoSizing}('DoShowFPDocEditor'){$ENDIF};

  if State>=iwgfShow then
    IDEWindowCreators.ShowForm(FPDocEditor,State=iwgfShowOnTop);
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
  InsertURLTagSpeedButton.Hint:=lisInsertUrlTag;

  ShortLabel.Caption:=lisShort;
  LinkLabel.Caption:=lisLink;
  CreateButton.Caption := lisCodeHelpCreateButton;
  OpenXMLButton.Caption:=lisOpenXML;
  OpenXMLButton.Enabled:=false;
  SaveButton.Caption := '';
  SaveButton.Enabled:=false;
  SaveButton.Hint:=lisSave;
  SaveButton.ShowHint:=true;

  BrowseExampleButton.Caption := lisCodeHelpBrowseExampleButton;
  
  MoveToInheritedButton.Caption:=lisLDMoveEntriesToInherited;
  CopyFromInheritedButton.Caption:=lisLDCopyFromInherited;
  AddLinkToInheritedButton.Caption:=lisLDAddLinkToInherited;

  Reset;
  
  CodeHelpBoss.AddHandlerOnChanging(@OnFPDocChanging);
  CodeHelpBoss.AddHandlerOnChanged(@OnFPDocChanged);

  Name := NonModalIDEWindowNames[nmiwFPDocEditorName];

  IDEImages.AssignImage(BoldFormatButton, 'formatbold');
  IDEImages.AssignImage(UnderlineFormatButton, 'formatunderline');
  IDEImages.AssignImage(ItalicFormatButton, 'formatitalic');
  IDEImages.AssignImage(InsertVarTagButton, 'insertvartag');
  IDEImages.AssignImage(InsertCodeTagButton, 'insertcodetag');
  IDEImages.AssignImage(InsertRemarkButton, 'insertremark');
  IDEImages.AssignImage(InsertURLTagSpeedButton, 'formatunderline');
  IDEImages.AssignImage(SaveButton, 'laz_save');

  SourceEditorManagerIntf.RegisterChangeEvent(semEditorActivate, @DoEditorUpdate);
  SourceEditorManagerIntf.RegisterChangeEvent(semEditorStatus, @DoEditorUpdate);

  FollowCursor:=true;
  IdleConnected:=true;
end;

procedure TFPDocEditor.FormDestroy(Sender: TObject);
begin
  IdleConnected:=false;
  Reset;
  FreeAndNil(fChain);
  if assigned(CodeHelpBoss) then
    CodeHelpBoss.RemoveAllHandlersOfObject(Self);
  Application.RemoveAllHandlersOfObject(Self);
  if SourceEditorManagerIntf<>nil then begin
    SourceEditorManagerIntf.UnRegisterChangeEvent(semEditorActivate, @DoEditorUpdate);
    SourceEditorManagerIntf.UnRegisterChangeEvent(semEditorStatus, @DoEditorUpdate);
  end;
end;

procedure TFPDocEditor.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key=VK_S) and (Shift=[ssCtrl]) then begin
    Save(true);
    Key:=VK_UNKNOWN;
  end;
end;

procedure TFPDocEditor.FormShow(Sender: TObject);
begin
  DoEditorUpdate(nil);
end;

procedure TFPDocEditor.FormatButtonClick(Sender: TObject);

  procedure InsertTag(const StartTag, EndTag: String);
  begin
    if PageControl.ActivePage = ShortTabSheet then begin
      ShortEdit.SelText := StartTag + ShortEdit.SelText + EndTag;
      DescrShortEdit.Text:=ShortEdit.Text;
    end else if PageControl.ActivePage = DescrTabSheet then
      DescrSynEdit.SelText := StartTag + DescrSynEdit.SelText + EndTag
    else if PageControl.ActivePage = ErrorsTabSheet then
      ErrorsSynEdit.SelText := StartTag + ErrorsSynEdit.SelText + EndTag
    else if PageControl.ActivePage = TopicSheet then begin
      if (FLastTopicControl = TopicShort) then
        TopicShort.SelText := StartTag + TopicShort.SelText + EndTag;
      if (FLastTopicControl = TopicDescrSynEdit) then
        TopicDescrSynEdit.SelText := StartTag + TopicDescrSynEdit.SelText + EndTag;
    end
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
    //url tag
    8:
      InsertTag('<url href="">', '</url>');
  end;
end;

procedure TFPDocEditor.InsertLinkSpeedButtonClick(Sender: TObject);
var
  Link: string;
  LinkTitle: string;
  LinkSrc: String;
begin
  if ShowFPDocLinkEditorDialog(fSourceFilename,DocFile,Link,LinkTitle)<>mrOk then exit;
  if Link='' then exit;
  LinkSrc:='<link id="'+Link+'"';
  if LinkTitle='' then begin
    LinkSrc:=LinkSrc+'/>';
  end else begin
    LinkSrc:=LinkSrc+'>'+LinkTitle+'</link>';
  end;
  if PageControl.ActivePage = ShortTabSheet then begin
    ShortEdit.SelText := LinkSrc;
    DescrShortEdit.Text := ShortEdit.Text;
  end;
  if PageControl.ActivePage = DescrTabSheet then
    DescrSynEdit.SelText := LinkSrc;
  if PageControl.ActivePage = SeeAlsoTabSheet then
    SeeAlsoSynEdit.SelText := LinkSrc;
  if PageControl.ActivePage = ErrorsTabSheet then
    ErrorsSynEdit.SelText := LinkSrc;
  if PageControl.ActivePage = TopicSheet then begin
    if (FLastTopicControl = TopicShort) then
      TopicShort.SelText := LinkSrc;
    if (FLastTopicControl = TopicDescrSynEdit) then
      TopicDescrSynEdit.SelText := LinkSrc;
  end;

  Modified:=true;
end;

procedure TFPDocEditor.LinkEditChange(Sender: TObject);
begin
  if fpdefReading in FFlags then exit;
  if LinkEdit.Text<>FOldVisualValues[fpdiElementLink] then
    Modified:=true;
end;

procedure TFPDocEditor.ApplicationIdle(Sender: TObject; var Done: Boolean);
var
  ActiveForm: TCustomForm;
begin
  if (fUpdateLock>0) then
  begin
    DebugLn(['WARNING: TFPDocEditor.ApplicationIdle fUpdateLock>0']);
    exit;
  end;
  if not IsVisible then begin
    Include(FFlags,fpdefWasHidden);
    IdleConnected:=false;
    exit;
  end;
  ActiveForm:=Screen.ActiveCustomForm;
  if (ActiveForm<>nil) and (fsModal in ActiveForm.FormState) then exit;
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
  else if fpdefTopicNeedsUpdate in FFlags then
    UpdateTopicCombo
  else begin
    //debugln(['TFPDocEditor.ApplicationIdle updated']);
    Done:=true;
    IdleConnected:=false;
  end;
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
          if IDEQuestionDialog(lisCodeHelpConfirmreplace,
            GetContextTitle(Element)+' already contains the help:'+LineEnding+ShortDescr,
            mtConfirmation, [mrYes, lisReplace,
                             mrCancel]) <> mrYes then exit;
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

procedure TFPDocEditor.NewTopicButtonClick(Sender: TObject);
var
  Dfile: TLazFPDocFile;
begin
  if NewTopicNameEdit.Text = '' then exit;
  Dfile := GetDefaultDocFile(True);
  if not assigned(DFile) then exit;
  if DFile.GetModuleTopic(NewTopicNameEdit.Text) = nil then begin
    DFile.CreateModuleTopic(NewTopicNameEdit.Text);
    CodeHelpBoss.SaveFPDocFile(DFile);
  end;
  UpdateTopicCombo;
  TopicListBox.ItemIndex := TopicListBox.Items.IndexOf(NewTopicNameEdit.Text);
  TopicListBoxClick(Sender);
end;

procedure TFPDocEditor.OpenXMLButtonClick(Sender: TObject);
begin
  OpenXML;
end;

procedure TFPDocEditor.PageControlChange(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TFPDocEditor.SaveButtonClick(Sender: TObject);
begin
  Save;
  UpdateValueControls;
end;

procedure TFPDocEditor.SeeAlsoSynEditChange(Sender: TObject);
begin
  if fpdefReading in FFlags then exit;
  if SeeAlsoSynEdit.Text<>FOldVisualValues[fpdiSeeAlso] then
    Modified:=true;
end;

procedure TFPDocEditor.ShortEditChange(Sender: TObject);
// called by ShortEdit and DescrShortEdit
var
  NewShort: String;
begin
  if fpdefReading in FFlags then exit;
  //debugln(['TFPDocEditor.ShortEditChange ',DbgSName(Sender)]);
  if Sender=DescrShortEdit then
    NewShort:=DescrShortEdit.Text
  else
    NewShort:=ShortEdit.Text;
  if NewShort<>FOldVisualValues[fpdiShort] then
    Modified:=true;
  // copy to the other edit
  if Sender=DescrShortEdit then
    ShortEdit.Text:=NewShort
  else
    DescrShortEdit.Text:=NewShort;
end;

procedure TFPDocEditor.TopicControlEnter(Sender: TObject);
begin
  FLastTopicControl := TControl(Sender);
end;

procedure TFPDocEditor.TopicDescrSynEditChange(Sender: TObject);
begin
  if fpdefReading in FFlags then exit;
  if fpdefTopicSettingUp in FFlags then exit;
  Modified := True;
end;

procedure TFPDocEditor.TopicListBoxClick(Sender: TObject);
begin
  if fpdefTopicSettingUp in FFlags then exit;
  if (FCurrentTopic <> '') and Modified then
    Save;
  UpdateTopic;
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

procedure TFPDocEditor.ClearTopicControls;
var
  OldSettingUp: boolean;
begin
  OldSettingUp:=fpdefTopicSettingUp in FFlags;
  Include(FFlags, fpdefTopicSettingUp);
  try
    TopicShort.Clear;
    TopicDescrSynEdit.Clear;
    TopicShort.Enabled := False;
    TopicDescrSynEdit.Enabled := False;
  finally
    if not OldSettingUp then
      Exclude(FFlags, fpdefTopicSettingUp);
  end;
end;

function TFPDocEditor.GetDocFile: TLazFPDocFile;
begin
  Result:=nil;
  if fChain<>nil then
    Result:=fChain.DocFile
  else
    Result:=fDocFile;
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
      Filename:=LazarusIDE.ActiveProject.GetShortFilename(Filename,true);
    Caption := strCaption + Filename;
  end else
    Caption := strCaption + lisCodeHelpNoTagCaption;
  {$IFDEF VerboseCodeHelp}
  DebugLn(['TFPDocEditor.UpdateCaption ',Caption]);
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
  fDocFile:=nil;
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

    // start getting the fpdoc element chain
    LDResult:=CodeHelpBoss.GetElementChain(Code,CaretXY.X,CaretXY.Y,true,
                                           NewChain,CacheWasUsed);
    case LDResult of
    chprParsing:
      begin
        Include(FFlags,fpdefChainNeedsUpdate);
        DebugLn(['TFPDocEditForm.UpdateChain ToDo: still parsing CodeHelpBoss.GetElementChain for ',fSourceFilename,' ',dbgs(CaretXY)]);
        exit;
      end;
    chprFailed:
      begin
        {$IFDEF VerboseFPDocFails}
        DebugLn(['TFPDocEditForm.UpdateChain failed CodeHelpBoss.GetElementChain for ',fSourceFilename,' ',dbgs(CaretXY)]);
        {$ENDIF}
        exit;
      end;
    else
      {$IFDEF VerboseCodeHelp}
      NewChain.WriteDebugReport;
      {$ENDIF}
      fChain:=NewChain;
      fDocFile:=fChain.DocFile;
      NewChain:=nil;
    end;
  finally
    NewChain.Free;
  end;
  if (fDocFile=nil) then begin
    // load default docfile, needed to show syntax errors in xml and for topics
    fDocFile:=GetDefaultDocFile;
  end;
  OpenXMLButton.Enabled:=fDocFile<>nil;
  if fDocFile<>nil then
    OpenXMLButton.Hint:=fDocFile.Filename
  else
    OpenXMLButton.Hint:='';
end;

procedure TFPDocEditor.OnFPDocChanging(Sender: TObject;
  FPDocFPFile: TLazFPDocFile);
begin
  if fpdefWriting in FFlags then exit;
  if (fChain<>nil) and (fChain.IndexOfFile(FPDocFPFile)>=0) then
    InvalidateChain
  else if (fDocFile<>nil) and (fDocFile=FPDocFPFile) then
    Include(FFlags,fpdefTopicNeedsUpdate);
end;

procedure TFPDocEditor.OnFPDocChanged(Sender: TObject;
  FPDocFPFile: TLazFPDocFile);
begin
  if fpdefWriting in FFlags then exit;
  if FPDocFPFile=nil then exit;
  // maybe eventually update the editor
end;

procedure TFPDocEditor.LoadGUIValues(Element: TCodeHelpElement);
var
  EnabledState: Boolean;
  OldModified: Boolean;
begin
  if fpdefReading in FFlags then exit;
  OldModified:=FModified;

  Include(FFlags,fpdefReading);
  try
    EnabledState := (Element<>nil) and (Element.ElementNode<>nil);

    //CreateButton.Enabled := (Element<>nil) and (Element.ElementNode=nil)
    //                        and (Element.ElementName<>'');

    if EnabledState then
    begin
      FOldValues:=Element.FPDocFile.GetValuesFromNode(Element.ElementNode);
      FOldVisualValues[fpdiShort]:=ReplaceLineEndings(FOldValues[fpdiShort],'');
      FOldVisualValues[fpdiElementLink]:=ConvertLineEndings(FOldValues[fpdiElementLink]);
      FOldVisualValues[fpdiDescription]:=ConvertLineEndings(FOldValues[fpdiDescription]);
      FOldVisualValues[fpdiErrors]:=ConvertLineEndings(FOldValues[fpdiErrors]);
      FOldVisualValues[fpdiSeeAlso]:=ConvertLineEndings(FOldValues[fpdiSeeAlso]);
      FOldVisualValues[fpdiExample]:=ConvertLineEndings(FOldValues[fpdiExample]);
      //DebugLn(['TFPDocEditor.LoadGUIValues Short="',dbgstr(FOldValues[fpdiShort]),'"']);
    end
    else
    begin
      FOldVisualValues[fpdiShort]:='';
      FOldVisualValues[fpdiElementLink]:='';
      FOldVisualValues[fpdiDescription]:='';
      FOldVisualValues[fpdiErrors]:='';
      FOldVisualValues[fpdiSeeAlso]:='';
      FOldVisualValues[fpdiExample]:='';
    end;
    ShortEdit.Text := FOldVisualValues[fpdiShort];
    DescrShortEdit.Text := ShortEdit.Text;
    //debugln(['TFPDocEditor.LoadGUIValues "',ShortEdit.Text,'" "',FOldVisualValues[fpdiShort],'"']);
    LinkEdit.Text := FOldVisualValues[fpdiElementLink];
    DescrSynEdit.Lines.Text := FOldVisualValues[fpdiDescription];
    //debugln(['TFPDocEditor.LoadGUIValues DescrMemo="',dbgstr(DescrSynEdit.Lines.Text),'" Descr="',dbgstr(FOldVisualValues[fpdiDescription]),'"']);
    SeeAlsoSynEdit.Text := FOldVisualValues[fpdiSeeAlso];
    ErrorsSynEdit.Lines.Text := FOldVisualValues[fpdiErrors];
    ExampleEdit.Text := FOldVisualValues[fpdiExample];

    ShortEdit.Enabled := EnabledState;
    DescrShortEdit.Enabled := ShortEdit.Enabled;
    LinkEdit.Enabled := EnabledState;
    DescrSynEdit.Enabled := EnabledState;
    SeeAlsoSynEdit.Enabled := EnabledState;
    ErrorsSynEdit.Enabled := EnabledState;
    ExampleEdit.Enabled := EnabledState;
    BrowseExampleButton.Enabled := EnabledState;

    FModified:=OldModified;
    SaveButton.Enabled:=false;

  finally
    Exclude(FFlags,fpdefReading);
  end;
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
  //DebugLn(['TFPDocEditForm.CreateElement ']);
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
        or (PageControl.ActivePage = ErrorsTabSheet)
        or (PageControl.ActivePage = TopicSheet);
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

function TFPDocEditor.GetCurrentOwnerName: string;
begin
  if (fChain<>nil) and (fChain.Count>0) then
    Result:=fChain[0].ElementOwnerName
  else
    Result:='';
end;

procedure TFPDocEditor.JumpToError(Item: TFPDocItem; LineCol: TPoint);
begin
  case Item of
  fpdiShort: PageControl.ActivePage:=ShortTabSheet;
  fpdiElementLink: PageControl.ActivePage:=InheritedTabSheet;
  fpdiDescription:
    begin
      PageControl.ActivePage:=DescrTabSheet;
      DescrSynEdit.CaretXY:=LineCol;
    end;
  fpdiErrors: PageControl.ActivePage:=ErrorsTabSheet;
  fpdiSeeAlso: PageControl.ActivePage:=SeeAlsoTabSheet;
  fpdiExample: PageControl.ActivePage:=ExampleTabSheet;
  end;
end;

procedure TFPDocEditor.OpenXML;
var
  CurDocFile: TLazFPDocFile;
begin
  CurDocFile:=DocFile;
  if CurDocFile=nil then exit;
  if FileExistsUTF8(CurDocFile.Filename) then begin
    LazarusIDE.DoOpenEditorFile(CurDocFile.Filename,-1,-1,
      [ofOnlyIfExists,ofRegularFile,ofUseCache]);
  end;
end;

function TFPDocEditor.GUIModified: boolean;
begin
  if fpdefReading in FFlags then exit(false);
  Result:=(ShortEdit.Text<>FOldVisualValues[fpdiShort])
    or (LinkEdit.Text<>FOldVisualValues[fpdiElementLink])
    or (DescrSynEdit.Text<>FOldVisualValues[fpdiDescription])
    or (SeeAlsoSynEdit.Text<>FOldVisualValues[fpdiSeeAlso])
    or (ErrorsSynEdit.Text<>FOldVisualValues[fpdiErrors])
    or (ExampleEdit.Text<>FOldVisualValues[fpdiExample]);
  if Result then begin
    if (ShortEdit.Text<>FOldVisualValues[fpdiShort]) then
      debugln(['TFPDocEditor.GUIModified Short ',dbgstr(ShortEdit.Text),' <> ',dbgstr(FOldVisualValues[fpdiShort])]);
    if (LinkEdit.Text<>FOldVisualValues[fpdiElementLink]) then
      debugln(['TFPDocEditor.GUIModified link ',dbgstr(LinkEdit.Text),' <> ',dbgstr(FOldVisualValues[fpdiElementLink])]);
    if (DescrSynEdit.Text<>FOldVisualValues[fpdiDescription]) then
      debugln(['TFPDocEditor.GUIModified Descr ',dbgstr(DescrSynEdit.Text),' <> ',dbgstr(FOldVisualValues[fpdiDescription])]);
    if (SeeAlsoSynEdit.Text<>FOldVisualValues[fpdiSeeAlso]) then
      debugln(['TFPDocEditor.GUIModified SeeAlso ',dbgstr(SeeAlsoSynEdit.Text),' <> ',dbgstr(FOldVisualValues[fpdiSeeAlso])]);
    if (ErrorsSynEdit.Text<>FOldVisualValues[fpdiErrors]) then
      debugln(['TFPDocEditor.GUIModified Errors ',dbgstr(ErrorsSynEdit.Text),' <> ',dbgstr(FOldVisualValues[fpdiErrors])]);
    if (ExampleEdit.Text<>FOldVisualValues[fpdiExample]) then
      debugln(['TFPDocEditor.GUIModified Example ',dbgstr(ExampleEdit.Text),' <> ',dbgstr(FOldVisualValues[fpdiExample])]);
  end;
end;

procedure TFPDocEditor.DoEditorUpdate(Sender: TObject);
begin
  if FollowCursor then
    LoadIdentifierAtCursor;
end;

procedure TFPDocEditor.UpdateTopicCombo;
var
  cnt, i: LongInt;
  DFile: TLazFPDocFile;
  Topics: TStringList;
begin
  Exclude(FFlags,fpdefTopicNeedsUpdate);
  Topics:=TStringList.Create;
  Include(FFlags,fpdefTopicSettingUp);
  try
    Dfile := DocFile;
    if DFile<>nil then begin
      cnt := DFile.GetModuleTopicCount;
      for i := 0 to cnt - 1 do
        Topics.Add(DFile.GetModuleTopicName(i));
    end;
    TopicListBox.Items.Assign(Topics);
    TopicListBox.ItemIndex:=TopicListBox.Items.IndexOf(FCurrentTopic);
    UpdateTopic;
  finally
    Exclude(FFlags,fpdefTopicSettingUp);
    Topics.Free;
  end;
end;

procedure TFPDocEditor.SetIdleConnected(AValue: boolean);
begin
  if FIdleConnected=AValue then Exit;
  FIdleConnected:=AValue;
  if IdleConnected then
    Application.AddOnIdleHandler(@ApplicationIdle)
  else
    Application.RemoveOnIdleHandler(@ApplicationIdle);
end;

procedure TFPDocEditor.SetFollowCursor(AValue: boolean);
begin
  if FFollowCursor=AValue then Exit;
  FFollowCursor:=AValue;
  if FollowCursor then
    LoadIdentifierAtCursor;
end;

function TFPDocEditor.GetDefaultDocFile(CreateIfNotExists: Boolean): TLazFPDocFile;
var
  CacheWasUsed : Boolean;
  AnOwner: TObject;
  FPDocFileName: String;
begin
  Result := nil;
  if (not CreateIfNotExists) and (fDocFile<>nil) then
    exit(fDocFile);

  FPDocFileName := CodeHelpBoss.GetFPDocFilenameForSource(SourceFilename, true,
                                      CacheWasUsed, AnOwner, CreateIfNotExists);
  if (FPDocFileName = '')
  or (CodeHelpBoss.LoadFPDocFile(FPDocFileName, [chofUpdateFromDisk], Result,
                                 CacheWasUsed) <> chprSuccess)
  then
    Result := nil;
end;

procedure TFPDocEditor.Reset;
var
  i: TFPDocItem;
begin
  FreeAndNil(fChain);
  if fpdefReading in FFlags then exit;
  Include(FFlags,fpdefReading);
  try
    // clear all element editors/viewers
    ShortEdit.Clear;
    DescrShortEdit.Clear;
    LinkEdit.Clear;
    DescrSynEdit.Clear;
    SeeAlsoSynEdit.Clear;
    ErrorsSynEdit.Clear;
    ExampleEdit.Clear;
    ClearTopicControls;
    for i:=Low(TFPDocItem) to high(TFPDocItem) do
      FOldVisualValues[i]:='';

    Modified := False;
    //CreateButton.Enabled:=false;
    OpenXMLButton.Enabled:=false;
  finally
    Exclude(FFlags,fpdefReading);
  end;
end;

procedure TFPDocEditor.InvalidateChain;
begin
  FreeAndNil(fChain);
  FFlags:=FFlags+[fpdefCodeCacheNeedsUpdate,
      fpdefChainNeedsUpdate,fpdefCaptionNeedsUpdate,
      fpdefValueControlsNeedsUpdate,fpdefInheritedControlsNeedsUpdate];
  IdleConnected:=true;
end;

procedure TFPDocEditor.LoadIdentifierAt(const SrcFilename: string;
  const Caret: TPoint);
var
  NewSrcFilename: String;
begin
  //debugln(['TFPDocEditor.LoadIdentifierAt START ',SrcFilename,' ',dbgs(Caret)]);
  // save the current changes to documentation
  Save(IsVisible);

  NewSrcFilename:=TrimAndExpandFilename(SrcFilename);
  if (NewSrcFilename=SourceFilename) and (CompareCaret(Caret,CaretXY)=0)
  and (fChain<>nil) and fChain.IsValid
  and (not LazarusIDE.NeedSaveSourceEditorChangesToCodeCache(nil)) then
    exit;

  FCaretXY:=Caret;
  fSourceFilename:=NewSrcFilename;
  
  Reset;
  Include(FFlags,fpdefTopicNeedsUpdate);
  InvalidateChain;
end;

procedure TFPDocEditor.LoadIdentifierAtCursor;
var
  SrcEdit: TSourceEditorInterface;
begin
  if SourceEditorManagerIntf=nil then exit;
  if csDestroying in ComponentState then exit;
  if FFlags*[fpdefReading,fpdefWriting]<>[] then exit;
  SrcEdit:=SourceEditorManagerIntf.ActiveEditor;
  if SrcEdit=nil then
    Reset
  else
    LoadIdentifierAt(SrcEdit.FileName,SrcEdit.CursorTextXY);
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
  DescrShortEdit.Text:=ShortEdit.Text;
  DescrSynEdit.Text:='';
  SeeAlsoSynEdit.Text:='';
  ErrorsSynEdit.Text:='';
  ExampleEdit.Text:='';
  if DoSave then Save;
end;

procedure TFPDocEditor.Save(CheckGUI: boolean);
var
  Values: TFPDocElementValues;
  TopicDocFile: TLazFPDocFile;
  Node: TDOMNode;
  Child: TDOMNode;
  TopicChanged: Boolean;
begin
  //DebugLn(['TFPDocEditor.Save FModified=',FModified]);
  if fpdefReading in FFlags then exit;

  if (not FModified)
  and ((not CheckGUI) or (not GUIModified)) then
  begin
    SaveButton.Enabled:=false;
    Exit; // nothing changed => exit
  end;
  //DebugLn(['TFPDocEditor.Save FModified=',FModified,' CheckGUI=',CheckGUI,' GUIModified=',GUIModified]);
  FModified:=false;
  SaveButton.Enabled:=false;

  TopicChanged:=false;
  TopicDocFile:=DocFile;
  if FCurrentTopic <> '' then
  begin
    if fDocFile=nil then
      fDocFile := GetDefaultDocFile(True);
    TopicDocFile:=DocFile;
    if TopicDocFile <> nil then begin
      Node := TopicDocFile.GetModuleTopic(FCurrentTopic);
      if Node <> nil then begin
        Child := Node.FindNode('short');
        if (Child = nil)
        or (TopicDocFile.GetChildValuesAsString(Child)<>TopicShort.Text)
        then begin
          TopicDocFile.SetChildValue(Node, 'short', TopicShort.Text);
          TopicChanged:=true;
        end;
        Child := Node.FindNode('descr');
        if (Child = nil)
        or (TopicDocFile.GetChildValuesAsString(Child)<>TopicDescrSynEdit.Text)
        then begin
          TopicDocFile.SetChildValue(Node, 'descr', TopicDescrSynEdit.Text);
          TopicChanged:=true;
        end;
      end;
    end;
  end;
  if (fChain=nil) or (fChain.Count=0) then
  begin
    if IsVisible then
      DebugLn(['TFPDocEditor.Save failed: no chain']);
  end else if not fChain.IsValid then
  begin
    if IsVisible then
      DebugLn(['TFPDocEditor.Save failed: chain not valid']);
  end else if (fChain[0].FPDocFile <> nil) then
  begin
    Values:=GetGUIValues;
    if WriteNode(fChain[0],Values,true) then
    begin
      // write succeeded
      if fChain.DocFile=TopicDocFile then
        TopicChanged:=false;
    end else begin
      DebugLn(['TFPDocEditor.Save WriteNode FAILED']);
    end;
  end;
  if TopicChanged then begin
    Include(FFlags,fpdefWriting);
    try
      CodeHelpBoss.SaveFPDocFile(TopicDocFile);
    finally
      Exclude(FFlags,fpdefWriting);
    end;
  end;
end;

function TFPDocEditor.GetGUIValues: TFPDocElementValues;
var
  i: TFPDocItem;
begin
  Result[fpdiShort]:=ShortEdit.Text;
  Result[fpdiDescription]:=DescrSynEdit.Text;
  Result[fpdiErrors]:=ErrorsSynEdit.Text;
  Result[fpdiSeeAlso]:=SeeAlsoSynEdit.Text;
  Result[fpdiExample]:=ExampleEdit.Text;
  Result[fpdiElementLink]:=LinkEdit.Text;
  for i:=Low(TFPDocItem) to High(TFPDocItem) do
    if Trim(Result[i])='' then
      Result[i]:='';
end;

procedure TFPDocEditor.SetModified(const AValue: boolean);
begin
  if FModified=AValue then exit;
  FModified:=AValue;
  SaveButton.Enabled:=FModified;
  //debugln(['TFPDocEditor.SetModified New=',FModified]);
end;

procedure TFPDocEditor.UpdateTopic;
var
  Child: TDOMNode;
  Node: TDOMNode;
  DFile: TLazFPDocFile;
begin
  FCurrentTopic := '';
  try
    if TopicListBox.ItemIndex < 0 then exit;
    Dfile := GetDefaultDocFile(True);
    if DFile = nil then exit;

    FCurrentTopic := TopicListBox.Items[TopicListBox.ItemIndex];
    Node := DFile.GetModuleTopic(FCurrentTopic);
    if Node = nil then exit;

    Include(FFlags, fpdefTopicSettingUp);
    try
      Child := Node.FindNode('short');
      if Child <> nil then
        TopicShort.Text := DFile.GetChildValuesAsString(Child);
      Child := Node.FindNode('descr');
      if Child <> nil then
        TopicDescrSynEdit.Text := DFile.GetChildValuesAsString(Child);
      TopicShort.Enabled := True;
      TopicDescrSynEdit.Enabled := True;
      if TopicShort.IsVisible then
        TopicShort.SetFocus;
    finally
      Exclude(FFlags, fpdefTopicSettingUp);
    end;
  finally
    if FCurrentTopic='' then
      ClearTopicControls;
  end;
end;

procedure TFPDocEditor.UpdateShowing;
begin
  inherited UpdateShowing;
  if IsVisible and (fpdefWasHidden in FFlags) then begin
    Exclude(FFlags,fpdefWasHidden);
    LoadIdentifierAtCursor;
  end;
end;

procedure TFPDocEditor.Loaded;
begin
  inherited Loaded;
  DescrSynEdit.ControlStyle:=DescrSynEdit.ControlStyle+[];
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
    DebugLn(['TFPDocEditor.WriteNode ERROR ',Msg]);
    if Interactive then begin;
      if Element.FPDocFile<>nil then
        CurName:=Element.FPDocFile.Filename
      else
        CurName:=Element.ElementName;
      IDEMessageDialog(lisCodeToolsDefsWriteError,
        Format(lisFPDocErrorWriting, [CurName, LineEnding, Msg]), mtError, [mbCancel]);
    end;
  end;

  function SetValue(Item: TFPDocItem): boolean;
  var
    NewValue: String;
  begin
    Result:=false;
    NewValue:=Values[Item];
    try
      FixFPDocFragment(NewValue,
             Item in [fpdiShort,fpdiDescription,fpdiErrors,fpdiSeeAlso],
             true);
      CurDocFile.SetChildValue(TopNode,FPDocItemNames[Item],NewValue);
      Result:=true;
    except
      on E: EXMLReadError do begin
        DebugLn(['SetValue ',dbgs(E.LineCol),' Name=',FPDocItemNames[Item]]);
        JumpToError(Item,E.LineCol);
        IDEMessageDialog(lisFPDocFPDocSyntaxError,
          Format(lisFPDocThereIsASyntaxErrorInTheFpdocElement, [FPDocItemNames
            [Item], LineEnding+LineEnding, E.Message]), mtError, [mbOk], '');
      end;
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
    if SetValue(fpdiShort)
    and SetValue(fpdiElementLink)
    and SetValue(fpdiDescription)
    and SetValue(fpdiErrors)
    and SetValue(fpdiSeeAlso)
    and SetValue(fpdiExample) then
      ;
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
  LazarusIDE.SaveSourceEditorChangesToCodeCache(nil);
end;

procedure TFPDocEditor.ErrorsSynEditChange(Sender: TObject);
begin
  if fpdefReading in FFlags then exit;
  if ErrorsSynEdit.Text<>FOldVisualValues[fpdiErrors] then
    Modified:=true;
end;

procedure TFPDocEditor.ExampleEditChange(Sender: TObject);
begin
  if fpdefReading in FFlags then exit;
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
    if Element.ElementFPDocPackageName<>'' then
      Link:='#'+Element.ElementFPDocPackageName+'.'+Link;
  end;
  if Link<>LinkEdit.Text then begin
    LinkEdit.Text:=Link;
    Modified:=true;
  end;
end;

procedure TFPDocEditor.BrowseExampleButtonClick(Sender: TObject);
begin
  if Doc=nil then exit;
  InitIDEFileDialog(OpenDialog);
  OpenDialog.Title:=lisChooseAnExampleFile;
  OpenDialog.Filter:=dlgFilterPascalFile+'|*.pas;*.pp;*.p|'+dlgFilterAll+'|'+FileMask;
  OpenDialog.InitialDir:=ExtractFilePath(DocFile.Filename);
  if OpenDialog.Execute then begin
    ExampleEdit.Text := ExtractRelativepath(
      ExtractFilePath(DocFile.Filename), GetForcedPathDelims(OpenDialog.FileName));
    if ExampleEdit.Text<>FOldVisualValues[fpdiExample] then
      Modified:=true;
  end;
  StoreIDEFileDialog(OpenDialog);
end;

procedure TFPDocEditor.CopyFromInheritedButtonClick(Sender: TObject);
var
  i: LongInt;
begin
  i:=FindInheritedIndex;
  if i<0 then exit;
  //DebugLn(['TFPDocEditForm.CopyFromInheritedButtonClick ']);
  if ShortEdit.Text<>'' then begin
    if IDEQuestionDialog('Confirm replace',
      GetContextTitle(fChain[0])+' already contains the help:'+LineEnding+ShortEdit.Text,
      mtConfirmation, [mrYes,'Replace',
                       mrCancel]) <> mrYes then exit;
  end;
  LoadGUIValues(fChain[i]);
  Modified:=true;
end;

procedure TFPDocEditor.CopyShortToDescrMenuItemClick(Sender: TObject);
begin
  DescrSynEdit.Append(ShortEdit.Text);
  Modified:=true;
end;

procedure TFPDocEditor.CreateButtonClick(Sender: TObject);
begin
  if ((fChain=nil) or (fChain.Count=0))
  or (TCodeHelpElement(fChain[0]).ElementName='') then begin
    IDEMessageDialog('Invalid Declaration','Please place the editor caret on an identifier. If this is a new unit, please save the file first.',
      mtError,[mbOK]);
    exit;
  end;
  CreateElement(fChain[0]);
end;

procedure TFPDocEditor.DescrSynEditChange(Sender: TObject);
begin
  if fpdefReading in FFlags then exit;
  if DescrSynEdit.Text<>FOldVisualValues[fpdiDescription] then
    Modified:=true;
end;

end.
