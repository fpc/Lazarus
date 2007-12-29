{
/***************************************************************************
                             LazDocFrm.pas
                             -------------

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

  see for todo list: http://wiki.lazarus.freepascal.org/index.php/LazDoc
}

unit LazDocFrm;

{$mode objfpc}{$H+}

{ $define VerboseLazDoc}

interface

uses
  // FCL
  Classes, SysUtils, StrUtils,
  // LCL
  LCLProc, LResources, StdCtrls, Buttons, ComCtrls, Controls, Dialogs,
  ExtCtrls, Forms, Graphics,
  // Synedit
  SynEdit,
  // codetools
  FileProcs, CodeAtom, CodeCache, CodeToolManager,
  Laz_DOM, Laz_XMLRead, Laz_XMLWrite,
  // IDEIntf
  IDEHelpIntf, LazHelpIntf,
  // IDE
  IDEOptionDefs, EnvironmentOpts,
  IDEProcs, LazarusIDEStrConsts, LazDocSelectInherited, CodeHelp;

type
  TLazDocFormFlag = (
    ldffWriting,
    ldffChainNeedsUpdate,
    ldffCaptionNeedsUpdate,
    ldffValueControlsNeedsUpdate,
    ldffInheritedControlsNeedsUpdate,
    ldffLinkIDComboNeedsUpdate
    );
  TLazDocFormFlags = set of TLazDocFormFlag;
  
  { TLazDocEditForm }

  TLazDocEditForm = class(TForm)
    AddLinkButton: TButton;
    BrowseExampleButton: TButton;
    SaveButton: TButton;
    CreateButton: TButton;
    CopyFromInheritedButton: TButton;
    MoveToInheritedButton: TButton;
    InheritedShortEdit: TEdit;
    ExampleEdit: TEdit;
    InheritedShortLabel: TLabel;
    LinkIdComboBox: TComboBox;
    DeleteLinkButton: TButton;
    DescrMemo: TMemo;
    LinkTextEdit: TEdit;
    LinkListBox: TListBox;
    OpenDialog: TOpenDialog;
    Panel1: TPanel;
    ShortEdit: TEdit;
    ErrorsMemo: TMemo;
    PageControl: TPageControl;
    DescrTabSheet: TTabSheet;
    ErrorsTabSheet: TTabSheet;
    ShortTabSheet: TTabSheet;
    BoldFormatButton: TSpeedButton;
    ItalicFormatButton: TSpeedButton;
    InsertCodeTagButton: TSpeedButton;
    InsertRemarkButton: TSpeedButton;
    InsertVarTagButton: TSpeedButton;
    ExampleTabSheet: TTabSheet;
    InheritedTabSheet: TTabSheet;
    UnderlineFormatButton: TSpeedButton;
    SeeAlsoTabSheet: TTabSheet;
    procedure AddLinkButtonClick(Sender: TObject);
    procedure BrowseExampleButtonClick(Sender: TObject);
    procedure CopyFromInheritedButtonClick(Sender: TObject);
    procedure CreateButtonClick(Sender: TObject);
    procedure DeleteLinkButtonClick(Sender: TObject);
    procedure DocumentationTagChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormatButtonClick(Sender: TObject);
    procedure LinkChange(Sender: TObject);
    procedure LinkListBoxClick(Sender: TObject);
    procedure ApplicationIdle(Sender: TObject; var Done: Boolean);
    procedure MoveToInheritedButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private
    FCaretXY: TPoint;
    FModified: Boolean;
    FFlags: TLazDocFormFlags;
    fUpdateLock: Integer;
    fSourceFilename: string;
    fChain: TLazDocElementChain;
    function GetDoc: TXMLdocument;
    function GetDocFile: TLazFPDocFile;
    function GetSourceFilename: string;
    function GetFirstElement: TDOMNode;

    function GetContextTitle(Element: TLazDocElement): string;

    function MakeLink: String;
    function FindInheritedIndex: integer;
    procedure Save;
    function GetValues: TFPDocElementValues;
    procedure SetModified(const AValue: boolean);
    function WriteNode(Element: TLazDocElement; Values: TFPDocElementValues;
                       Interactive: Boolean): Boolean;
    procedure UpdateChain;
    procedure UpdateCaption;
    procedure UpdateLinkIdComboBox;
    procedure UpdateValueControls;
    procedure UpdateInheritedControls;
    procedure OnLazDocChanging(Sender: TObject; LazDocFPFile: TLazFPDocFile);
    procedure OnLazDocChanged(Sender: TObject; LazDocFPFile: TLazFPDocFile);
    procedure LoadGUIValues(Element: TLazDocElement);
    procedure MoveToInherited(Element: TLazDocElement);
    function CreateElement(Element: TLazDocElement): Boolean;
  public
    procedure Reset;
    procedure InvalidateChain;
    procedure UpdateLazDoc(const SrcFilename: string; const Caret: TPoint);
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
  LazDocEditForm: TLazDocEditForm = nil;

procedure DoShowLazDoc;

implementation

{ TLazDocEditForm }

procedure DoShowLazDoc;
begin
  if LazDocEditForm = Nil then begin
    Application.CreateForm(TLazDocEditForm, LazDocEditForm);
    EnvironmentOptions.IDEWindowLayoutList.ItemByEnum(nmiwLazDocName).Apply;
  end;

  LazDocEditForm.Show;
end;

function TLazDocEditForm.GetFirstElement: TDOMNode;
var
  CurDocFile: TLazFPDocFile;
begin
  Result:=nil;
  CurDocFile:=DocFile;
  if CurDocFile=nil then exit;
  Result:=CurDocFile.GetFirstElement;
end;

procedure TLazDocEditForm.UpdateLinkIdComboBox;
// fills LinkIdComboBox.Items
var
  n: TDOMNode;
  sl: TStringList;
begin
  if fUpdateLock>0 then begin
    Include(FFLags,ldffLinkIDComboNeedsUpdate);
    exit;
  end;
  Exclude(FFLags,ldffLinkIDComboNeedsUpdate);

  {$IFDEF VerboseLazDoc}
  DebugLn(['TLazDocEditForm.UpdateLinkIdComboBox START']);
  {$ENDIF}
  LinkIdComboBox.Clear;
  if Doc=nil then exit;

  // element nodes
  sl:=TStringList.Create;
  n := GetFirstElement;
  while n<>nil do
  begin
    if n.NodeName <> '#comment' then
      sl.Add(TDomElement(n)['name']);
    n := n.NextSibling;
  end;
  LinkIdComboBox.Items.Assign(sl);
  sl.Free;
end;

procedure TLazDocEditForm.FormCreate(Sender: TObject);
begin
  Caption := lisLazDocMainFormCaption;

  with PageControl do
  begin
    Page[0].Caption := lisLazDocShortTag;
    Page[1].Caption := lisLazDocDescrTag;
    Page[2].Caption := lisLazDocErrorsTag;
    Page[3].Caption := lisLazDocSeeAlsoTag;
    Page[4].Caption := lisLazDocExampleTag;
    Page[5].Caption := lisLazDocInherited;
    PageIndex := 0;
  end;

  BoldFormatButton.Hint := lisLazDocHintBoldFormat;
  ItalicFormatButton.Hint := lisLazDocHintItalicFormat;
  UnderlineFormatButton.Hint := lisLazDocHintUnderlineFormat;
  InsertCodeTagButton.Hint := lisLazDocHintInsertCodeTag;
  InsertRemarkButton.Hint := lisLazDocHintRemarkTag;
  InsertVarTagButton.Hint := lisLazDocHintVarTag;

  CreateButton.Caption := 'Create help item';
  CreateButton.Enabled:=false;
  SaveButton.Caption := 'Save';
  SaveButton.Enabled:=false;

  AddLinkButton.Caption := lisLazDocAddLinkButton;
  DeleteLinkButton.Caption := lisLazDocDeleteLinkButton;

  BrowseExampleButton.Caption := lisLazDocBrowseExampleButton;
  
  MoveToInheritedButton.Caption:=lisLDMoveEntriesToInherited;
  CopyFromInheritedButton.Caption:=lisLDCopyFromInherited;
  
  Reset;
  
  LazDocBoss.AddHandlerOnChanging(@OnLazDocChanging);
  LazDocBoss.AddHandlerOnChanged(@OnLazDocChanged);
  Application.AddOnIdleHandler(@ApplicationIdle);
  
  Name := NonModalIDEWindowNames[nmiwLazDocName];
  EnvironmentOptions.IDEWindowLayoutList.Apply(Self, Name);
end;

procedure TLazDocEditForm.FormDestroy(Sender: TObject);
begin
  Reset;
  FreeAndNil(fChain);
  LazDocBoss.RemoveAllHandlersOfObject(Self);
  Application.RemoveAllHandlersOfObject(Self);
end;

procedure TLazDocEditForm.FormResize(Sender: TObject);
begin
  LinkIdComboBox.Width := (AddLinkButton.Left - LinkIdComboBox.Left - 8) div 2;
end;

procedure TLazDocEditForm.FormatButtonClick(Sender: TObject);

  procedure InsertTag(starttag, endtag: String);
  begin
    if PageControl.ActivePage.Caption = lisLazDocDescrTag then
      DescrMemo.SelText := starttag + DescrMemo.SelText + endtag;
    if PageControl.ActivePage.Caption = lisLazDocErrorsTag then
      ErrorsMemo.SelText := starttag + ErrorsMemo.SelText + endtag;
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
    //codetag
    3:
      InsertTag('<p><code>', '</code></p>');
    //remarktag
    4:
      InsertTag('<p><remark>', '</remark></p>');
    //vartag
    5:
      InsertTag('<var>', '</var>');
  end;
end;

procedure TLazDocEditForm.LinkChange(Sender: TObject);
begin
  if LinkListBox.ItemIndex<0 then
    Exit;

  LinkListBox.Items.Strings[LinkListBox.ItemIndex] := MakeLink;
end;

procedure TLazDocEditForm.LinkListBoxClick(Sender: TObject);
var
  strTmp: String;
  intTmp: Integer;
  intStart: Integer;
  LinkIndex: LongInt;
begin
  //split the link into Id and Text
  LinkIndex := LinkListBox.ItemIndex;
  if LinkIndex = -1 then
    Exit;

  intStart := PosEx('"', LinkListBox.Items[LinkIndex], 1);

  intTmp := PosEx('"', LinkListBox.Items[LinkIndex], intStart + 1);

  LinkIdComboBox.Text := Copy(LinkListBox.Items[LinkIndex],
    intStart + 1, intTmp - intStart - 1);

  strTmp := Copy(LinkListBox.Items[LinkIndex], intTmp + 2,
    Length(LinkListBox.Items[LinkIndex]));

  if strTmp = '>' then
    LinkTextEdit.Text := ''
  else
    LinkTextEdit.Text := Copy(strTmp, 1, Length(strTmp) - Length('</link>'));
end;

procedure TLazDocEditForm.ApplicationIdle(Sender: TObject; var Done: Boolean);
begin
  Done:=false;
  if ldffChainNeedsUpdate in FFlags then
    UpdateChain
  else if ldffCaptionNeedsUpdate in FFlags then
    UpdateCaption
  else if ldffValueControlsNeedsUpdate in FFlags then
    UpdateValueControls
  else if ldffInheritedControlsNeedsUpdate in FFlags then
    UpdateInheritedControls
  else if ldffLinkIDComboNeedsUpdate in FFlags then
    UpdateLinkIdComboBox
  else
    Done:=true;
end;

procedure TLazDocEditForm.MoveToInheritedButtonClick(Sender: TObject);
var
  i: Integer;
  Element: TLazDocElement;
  Candidates: TFPList;
  LazDocSelectInheritedDlg: TLazDocSelectInheritedDlg;
  ShortDescr: String;
begin
  if fChain=nil then exit;
  Candidates:=nil;
  LazDocSelectInheritedDlg:=nil;
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
      Element:=TLazDocElement(Candidates[0]);
      if (Element.ElementNode<>nil) then begin
        ShortDescr:=Element.FPDocFile.GetValueFromNode(Element.ElementNode,fpdiShort);
        if ShortDescr<>'' then begin
          // the inherited entry already contains a description.
          // ask if it should be really replaced
          if QuestionDlg('Confirm replace',
            GetContextTitle(Element)+' already contains the help:'+#13
            +ShortDescr,
            mtConfirmation,[mrYes,'Replace',mrCancel],0)<>mrYes then exit;
        end;
      end;
    end else begin
      // there is more than one candidate
      // => ask which one to replace
      LazDocSelectInheritedDlg:=TLazDocSelectInheritedDlg.Create(nil);
      LazDocSelectInheritedDlg.InheritedComboBox.Items.Clear;
      for i:=0 to Candidates.Count-1 do begin
        Element:=TLazDocElement(Candidates[i]);
        LazDocSelectInheritedDlg.InheritedComboBox.Items.Add(
                                                      GetContextTitle(Element));
      end;
      if LazDocSelectInheritedDlg.ShowModal<>mrOk then exit;
      i:=LazDocSelectInheritedDlg.InheritedComboBox.ItemIndex;
      if i<0 then exit;
      Element:=TLazDocElement(Candidates[i]);
    end;

    // move the content of the current entry to the inherited entry
    MoveToInherited(Element);
  finally
    LazDocSelectInheritedDlg.Free;
    Candidates.Free;
  end;
end;

procedure TLazDocEditForm.SaveButtonClick(Sender: TObject);
begin
  Save;
end;

function TLazDocEditForm.GetContextTitle(Element: TLazDocElement): string;
// get codetools path. for example: TButton.Align
begin
  Result:='';
  if Element=nil then exit;
  Result:=Element.ElementName;
end;

function TLazDocEditForm.GetDoc: TXMLdocument;
begin
  if DocFile<>nil then
    Result:=DocFile.Doc
  else
    Result:=nil;
end;

function TLazDocEditForm.GetDocFile: TLazFPDocFile;
begin
  Result:=nil;
  if fChain=nil then exit;
  Result:=fChain.DocFile;
end;

function TLazDocEditForm.GetSourceFilename: string;
begin
  Result:=fSourceFilename;
end;

procedure TLazDocEditForm.UpdateCaption;
var
  strCaption: String;
begin
  if fUpdateLock>0 then begin
    Include(FFlags,ldffCaptionNeedsUpdate);
    exit;
  end;
  Exclude(FFlags,ldffCaptionNeedsUpdate);
  
  {$IFDEF VerboseLazDoc}
  DebugLn(['TLazDocEditForm.UpdateCaption START']);
  {$ENDIF}
  strCaption := lisLazDocMainFormCaption + ' - ';

  if (fChain <> nil) and (fChain.Count>0) then
    strCaption := strCaption + GetContextTitle(fChain[0]) + ' - '
  else
    strCaption := strCaption + lisLazDocNoTagCaption + ' - ';

  if DocFile<>nil then
    Caption := strCaption + DocFile.Filename
  else
    Caption := strCaption + lisLazDocNoTagCaption;
  {$IFDEF VerboseLazDoc}
  DebugLn(['TLazDocForm.UpdateCaption ',Caption]);
  {$ENDIF}
end;

procedure TLazDocEditForm.UpdateValueControls;
var
  Element: TLazDocElement;
begin
  if fUpdateLock>0 then begin
    Include(FFLags,ldffValueControlsNeedsUpdate);
    exit;
  end;
  Exclude(FFLags,ldffValueControlsNeedsUpdate);

  {$IFDEF VerboseLazDoc}
  DebugLn(['TLazDocEditForm.UpdateValueControls START']);
  {$ENDIF}
  Element:=nil;
  if (fChain<>nil) and (fChain.Count>0) then
    Element:=fChain[0];
  LoadGUIValues(Element);
  SaveButton.Enabled:=FModified;
end;

procedure TLazDocEditForm.UpdateInheritedControls;
var
  i: LongInt;
  Element: TLazDocElement;
  ShortDescr: String;
begin
  if fUpdateLock>0 then begin
    Include(FFLags,ldffInheritedControlsNeedsUpdate);
    exit;
  end;
  Exclude(FFLags,ldffInheritedControlsNeedsUpdate);

  {$IFDEF VerboseLazDoc}
  DebugLn(['TLazDocEditForm.UpdateInheritedControls START']);
  {$ENDIF}
  i:=FindInheritedIndex;
  if i<0 then begin
    InheritedShortEdit.Text:='';
    InheritedShortEdit.Enabled:=false;
    InheritedShortLabel.Caption:='no inherited description found';
  end else begin
    Element:=fChain[i];
    ShortDescr:=Element.FPDocFile.GetValueFromNode(Element.ElementNode,fpdiShort);
    InheritedShortEdit.Text:=ShortDescr;
    InheritedShortEdit.Enabled:=true;
    InheritedShortLabel.Caption:='Short description of '
                                 +GetContextTitle(Element);
  end;
  MoveToInheritedButton.Enabled:=(fChain<>nil)
                                 and (fChain.Count>1)
                                 and (ShortEdit.Text<>'');
  CopyFromInheritedButton.Enabled:=(i>=0);
end;

procedure TLazDocEditForm.UpdateChain;
var
  Code: TCodeBuffer;
  LDResult: TLazDocParseResult;
  NewChain: TLazDocElementChain;
  CacheWasUsed: Boolean;
begin
  FreeAndNil(fChain);
  if fUpdateLock>0 then begin
    Include(FFLags,ldffChainNeedsUpdate);
    exit;
  end;
  Exclude(FFLags,ldffChainNeedsUpdate);

  if (fSourceFilename='') or (CaretXY.X<1) or (CaretXY.Y<1) then exit;

  {$IFDEF VerboseLazDoc}
  DebugLn(['TLazDocEditForm.UpdateChain START']);
  {$ENDIF}
  NewChain:=nil;
  try
    // fetch pascal source
    Code:=CodeToolBoss.LoadFile(fSourceFilename,true,false);
    if Code=nil then begin
      DebugLn(['TLazDocEditForm.UpdateChain failed loading ',fSourceFilename]);
      exit;
    end;

    // start getting the lazdoc element chain
    LDResult:=LazDocBoss.GetElementChain(Code,CaretXY.X,CaretXY.Y,true,
                                         NewChain,CacheWasUsed);
    case LDResult of
    ldprParsing:
      begin
        Include(FFLags,ldffChainNeedsUpdate);
        DebugLn(['TLazDocEditForm.UpdateChain ToDo: still parsing LazDocBoss.GetElementChain for ',fSourceFilename,' ',dbgs(CaretXY)]);
        exit;
      end;
    ldprFailed:
      begin
        DebugLn(['TLazDocEditForm.UpdateChain failed LazDocBoss.GetElementChain for ',fSourceFilename,' ',dbgs(CaretXY)]);
        exit;
      end;
    else
      fChain:=NewChain;
      NewChain:=nil;
    end;
  finally
    NewChain.Free;
  end;
end;

procedure TLazDocEditForm.OnLazDocChanging(Sender: TObject;
  LazDocFPFile: TLazFPDocFile);
begin
  if ldffWriting in FFlags then exit;
  if (fChain<>nil) and (fChain.IndexOfFile(LazDocFPFile)>=0) then
    InvalidateChain;
end;

procedure TLazDocEditForm.OnLazDocChanged(Sender: TObject;
  LazDocFPFile: TLazFPDocFile);
begin
  if ldffWriting in FFlags then exit;

end;

procedure TLazDocEditForm.LoadGUIValues(Element: TLazDocElement);
var
  EnabledState: Boolean;
  Values: TFPDocElementValues;
  OldModified: Boolean;
begin
  OldModified:=FModified;
  
  EnabledState := (Element<>nil) and (Element.ElementNode<>nil);
  
  CreateButton.Enabled := (Element<>nil) and (Element.ElementNode=nil)
                          and (Element.ElementName<>'');

  if EnabledState then
  begin
    Values:=Element.FPDocFile.GetValuesFromNode(Element.ElementNode);
    ShortEdit.Text := ConvertLineEndings(Values[fpdiShort]);
    DescrMemo.Lines.Text := ConvertLineEndings(Values[fpdiDescription]);
    ErrorsMemo.Lines.Text := ConvertLineEndings(Values[fpdiErrors]);
    LinkListBox.Items.Text := ConvertLineEndings(Values[fpdiSeeAlso]);
    LinkIdComboBox.Text := '';
    LinkTextEdit.Clear;
    ExampleEdit.Text := ConvertLineEndings(Values[fpdiExample]);
  end
  else
  begin
    ShortEdit.Text := lisLazDocNoDocumentation;
    DescrMemo.Lines.Text := lisLazDocNoDocumentation;
    ErrorsMemo.Lines.Text := lisLazDocNoDocumentation;
    LinkIdComboBox.Text := lisLazDocNoDocumentation;
    LinkTextEdit.Text := lisLazDocNoDocumentation;
    LinkListBox.Clear;
    ExampleEdit.Text := lisLazDocNoDocumentation;
  end;

  ShortEdit.Enabled := EnabledState;
  DescrMemo.Enabled := EnabledState;
  ErrorsMemo.Enabled := EnabledState;
  LinkIdComboBox.Enabled := EnabledState;
  LinkTextEdit.Enabled := EnabledState;
  LinkListBox.Enabled := EnabledState;
  AddLinkButton.Enabled := EnabledState;
  DeleteLinkButton.Enabled := EnabledState;
  ExampleEdit.Enabled := EnabledState;
  BrowseExampleButton.Enabled := EnabledState;

  FModified:=OldModified;
end;

procedure TLazDocEditForm.MoveToInherited(Element: TLazDocElement);
var
  Values: TFPDocElementValues;
begin
  Values:=GetValues;
  WriteNode(Element,Values,true);
end;

function TLazDocEditForm.CreateElement(Element: TLazDocElement): Boolean;
var
  NewElement: TLazDocElement;
begin
  DebugLn(['TLazDocEditForm.CreateElement ']);
  if (Element=nil) or (Element.ElementName='') then exit(false);
  NewElement:=nil;
  Include(FFlags,ldffWriting);
  try
    Result:=LazDocBoss.CreateElement(Element.CodeXYPos.Code,
                            Element.CodeXYPos.X,Element.CodeXYPos.Y,NewElement);
  finally
    Exclude(FFlags,ldffWriting);
    NewElement.Free;
  end;
  Reset;
  InvalidateChain;
end;

procedure TLazDocEditForm.Reset;
begin
  FreeAndNil(fChain);

  // clear all element editors/viewers
  ShortEdit.Clear;
  DescrMemo.Clear;
  ErrorsMemo.Clear;
  LinkIdComboBox.Text := '';
  LinkTextEdit.Clear;
  LinkListBox.Clear;
  ExampleEdit.Clear;

  Modified := False;
  CreateButton.Enabled:=false;
end;

procedure TLazDocEditForm.InvalidateChain;
begin
  FreeAndNil(fChain);
  FFlags:=FFlags+[ldffChainNeedsUpdate,ldffCaptionNeedsUpdate,
      ldffValueControlsNeedsUpdate,ldffInheritedControlsNeedsUpdate,
      ldffLinkIDComboNeedsUpdate];
end;

procedure TLazDocEditForm.UpdateLazDoc(const SrcFilename: string;
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
  and (fChain<>nil) and fChain.IsValid then
    exit;

  FCaretXY:=Caret;
  fSourceFilename:=NewSrcFilename;
  
  Reset;
  InvalidateChain;
end;

procedure TLazDocEditForm.BeginUpdate;
begin
  inc(fUpdateLock);
end;

procedure TLazDocEditForm.EndUpdate;
begin
  dec(fUpdateLock);
  if fUpdateLock<0 then RaiseGDBException('');
  if fUpdateLock=0 then begin
    if ldffCaptionNeedsUpdate in FFlags then UpdateCaption;
  end;
end;

procedure TLazDocEditForm.ClearEntry(DoSave: Boolean);
begin
  Modified:=true;
  ShortEdit.Text:='';
  DescrMemo.Text:='';
  ErrorsMemo.Text:='';
  LinkListBox.Items.Clear;
  ExampleEdit.Text:='';
  if DoSave then Save;
end;

procedure TLazDocEditForm.Save;
var
  Values: TFPDocElementValues;
begin
  if not FModified then Exit; // nothing changed => exit
  FModified:=false;
  if Doc=nil then exit;
  if not fChain.IsValid then exit;

  Values:=GetValues;
  if not WriteNode(fChain[0],Values,true) then begin
    DebugLn(['TLazDocForm.Save FAILED']);
  end else begin
    FModified := False;
  end;
  SaveButton.Enabled:=false;
end;

function TLazDocEditForm.GetValues: TFPDocElementValues;
begin
  Result[fpdiShort]:=ShortEdit.Text;
  Result[fpdiDescription]:=DescrMemo.Text;
  Result[fpdiErrors]:=ErrorsMemo.Text;
  Result[fpdiSeeAlso]:=LinkListBox.Items.Text;
  Result[fpdiExample]:=ExampleEdit.Text;
end;

procedure TLazDocEditForm.SetModified(const AValue: boolean);
begin
  if FModified=AValue then exit;
  FModified:=AValue;
  SaveButton.Enabled:=FModified;
end;

function TLazDocEditForm.WriteNode(Element: TLazDocElement;
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

  {procedure CheckAndWriteNode(const NodeName: String; NodeText: String;
    NodeIndex: TFPDocItem);
  var
    child: TDOMNode;
    FileAttribute: TDOMAttr;
    OldNode: TDOMNode;
    NewValue: String;
  begin
    DebugLn('TLazDocForm.Save[CheckAndWriteNode]: checking element: ' +
      NodeName);

    if CurNodeName <> NodeName then exit;

    NewValue:=ToUnixLineEnding(NodeText);
    if CurNodeName = 'example' then begin
      OldNode:=Node.Attributes.GetNamedItem('file');
      NewValue:=FilenameToURLPath(NewValue);
      if (NodeText<>'')
      or (not (OldNode is TDOMAttr))
      or (TDOMAttr(OldNode).Value<>NewValue) then begin
        DebugLn(['TLazDocForm.CheckAndWriteNode Changing NodeName=',NodeName,' NodeText="',NewValue,'"']);
        // add or change example
        FileAttribute := Entry.DocFile.Doc.CreateAttribute('file');
        FileAttribute.Value := NewValue;
        OldNode:=Node.Attributes.SetNamedItem(FileAttribute);
        OldNode.Free;
      end;
    end
    else if not Assigned(Node.FirstChild) then begin
      // add node
      if NodeText<>'' then begin
        DebugLn(['TLazDocForm.CheckAndWriteNode Adding NodeName=',NodeName,' NodeText="',NewValue,'"']);
        child := Entry.DocFile.Doc.CreateTextNode(NewValue);
        Node.AppendChild(child);
      end;
    end else begin
      // change node
      if Node.FirstChild.NodeValue <> NewValue then begin
        DebugLn(['TLazDocForm.CheckAndWriteNode Changing NodeName=',NodeName,' NodeText="',NewValue,'"']);
        Node.FirstChild.NodeValue := NewValue;
      end;
    end;
    NodeWritten[NodeIndex] := True;
  end;

  procedure CheckAndWriteNode(const NodeName: String; NodeType: TFPDocItem);
  begin
    CheckAndWriteNode(NodeName,DocNode[NodeType],NodeType);
  end;

  procedure InsertNodeElement(const ElementName, ElementText: String);
  var
    child: TDOMNode;
    FileAttribute: TDOMAttr;
  begin
    DebugLn('TLazDocForm.Save[InsertNodeElement]: inserting element: ' + ElementName);
    if (ElementText='') then exit;

    DebugLn(['InsertNodeElement Adding node ElementName=',ElementName,' ElementText="',ElementText,'"']);
    child := Entry.DocFile.doc.CreateElement(ElementName);
    if ElementName='example' then begin
      FileAttribute := Entry.DocFile.Doc.CreateAttribute('file');
      FileAttribute.Value := FilenameToURLPath(ElementText);
      child.Attributes.SetNamedItem(FileAttribute);
    end
    else begin
      child.AppendChild(Entry.DocFile.Doc.CreateTextNode(
                                                ToUnixLineEnding(ElementText)));
    end;
    TopNode.AppendChild(child);
  end;}
  
begin
  Result:=false;
  if ldffWriting in FFlags then begin
    DebugLn(['TLazDocEditForm.WriteNode inconsistency detected: recursive write']);
    exit;
  end;
  
  if Check(Element=nil,'Element=nil') then exit;
  CurDocFile:=Element.FPDocFile;
  if Check(CurDocFile=nil,'Element.FPDocFile=nil') then begin
    // no fpdoc file found
    // TODO: create a new file
    DebugLn(['TLazDocEditForm.WriteNode TODO: implement creating new fpdoc file']);
    exit;
  end;
  CurDoc:=CurDocFile.Doc;
  if Check(CurDoc=nil,'Element.FPDocFile.Doc=nil') then exit;
  if Check(not Element.ElementNodeValid,'not Element.ElementNodeValid') then exit;
  TopNode:=Element.ElementNode;
  if Check(TopNode=nil,'TopNode=nil') then begin
    // no old node found
    // TODO: create a new node
    Check(false,'no old node found. TODO: implement creating a new.');
    Exit;
  end;

  Include(FFlags,ldffWriting);
  try
    CurDocFile.BeginUpdate;
    
    CurDocFile.SetChildValue(TopNode,'short',Values[fpdiShort]);
    CurDocFile.SetChildValue(TopNode,'descr',Values[fpdiDescription]);
    CurDocFile.SetChildValue(TopNode,'errors',Values[fpdiErrors]);
    CurDocFile.SetChildValue(TopNode,'seealso',Values[fpdiSeeAlso]);
    CurDocFile.SetChildValue(TopNode,'example',Values[fpdiExample]);

  finally
    CurDocFile.EndUpdate;
    fChain.MakeValid;
    Exclude(FFlags,ldffWriting);
  end;

  if LazDocBoss.SaveFPDocFile(CurDocFile)<>mrOk then begin
    DebugLn(['TLazDocEditForm.WriteNode failed writing ',CurDocFile.Filename]);
    exit;
  end;
  Result:=true;
end;

procedure TLazDocEditForm.DocumentationTagChange(Sender: TObject);
begin
  Modified := True;
end;

function TLazDocEditForm.MakeLink: String;
begin
  if Trim(LinkTextEdit.Text) = '' then
    Result := '<link id="' + Trim(LinkIdComboBox.Text) + '"/>'
  else
    Result := '<link id="' + Trim(LinkIdComboBox.Text) + '">' +
      LinkTextEdit.Text + '</link>';
end;

function TLazDocEditForm.FindInheritedIndex: integer;
// returns Index in chain of an overriden Element with a short description
// returns -1 if not found
var
  Element: TLazDocElement;
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

procedure TLazDocEditForm.AddLinkButtonClick(Sender: TObject);
begin
  if Trim(LinkIdComboBox.Text) <> '' then
  begin
    LinkListBox.Items.Add(MakeLink);
    Modified := True;
  end;
end;

procedure TLazDocEditForm.BrowseExampleButtonClick(Sender: TObject);
begin
  if Doc=nil then exit;
  if OpenDialog.Execute then
    ExampleEdit.Text := SetDirSeparators(ExtractRelativepath(
      ExtractFilePath(DocFile.Filename), OpenDialog.FileName));
end;

procedure TLazDocEditForm.CopyFromInheritedButtonClick(Sender: TObject);
var
  i: LongInt;
begin
  i:=FindInheritedIndex;
  if i<0 then exit;
  DebugLn(['TLazDocEditForm.CopyFromInheritedButtonClick ']);
  if ShortEdit.Text<>'' then begin
    if QuestionDlg('Confirm replace',
      GetContextTitle(fChain[0])+' already contains the help:'+#13
      +ShortEdit.Text,
      mtConfirmation,[mrYes,'Replace',mrCancel],0)<>mrYes then exit;
  end;
  LoadGUIValues(fChain[i]);
  Modified:=true;
end;

procedure TLazDocEditForm.CreateButtonClick(Sender: TObject);
begin
  if (fChain=nil) or (fChain.Count=0) then exit;
  CreateElement(fChain[0]);
end;

procedure TLazDocEditForm.DeleteLinkButtonClick(Sender: TObject);
begin
  if LinkListBox.ItemIndex >= 0 then begin
    LinkListBox.Items.Delete(LinkListBox.ItemIndex);
    DebugLn(['TLazDocEditForm.DeleteLinkButtonClick ']);
    Modified := True;
  end;
end;

initialization
  {$I lazdocfrm.lrs}

end.
