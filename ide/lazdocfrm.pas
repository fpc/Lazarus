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

{ $define dbgLazDoc}

interface

uses
  Classes, SysUtils, StrUtils,
  LCLProc, LResources, StdCtrls, Buttons, ComCtrls, Controls, Dialogs,
  ExtCtrls, Forms, Graphics,
  SynEdit,
  CodeAtom, CodeCache, CodeToolManager,
  Laz_DOM, Laz_XMLRead, Laz_XMLWrite,
  IDEHelpIntf, LazHelpIntf,
  IDEOptionDefs, EnvironmentOpts,
  IDEProcs, LazarusIDEStrConsts, LazDocSelectInherited, LazDoc;

type
  TFPDocItem = (
    fpdiShort,
    fpdiDescription,
    fpdiErrors,
    fpdiSeeAlso,
    fpdiExample
    );

  TFPDocNode = array [TFPDocItem] of String;
  
  TLazDocFormFlag = (
    ldffCaptionNeedsUpdate,
    ldffInheritedNeedsUpdate,
    ldffInheritedEntriesNeedUpdate
    );
  TLazDocFormFlags = set of TLazDocFormFlag;
  
  { TLazDocInheritedEntry }

  TLazDocInheritedEntry = class
  public
    SrcFilename: string;
    Caret: TPoint;
    DocFilename: string;
    DocFilenameValid: Boolean;
    DocFile: TLazFPDocFile;
    DocFileValid: Boolean;
    PascalContext: TPascalHelpContextList;
    PascalContextValid: Boolean;
    DOMNodeValid: Boolean;
    DOMNode: TDOMNode;
    ValuesValid: Boolean;
    Values: TFPDocNode;
    destructor Destroy; override;
  end;
    
  { TLazDocForm }

  TLazDocForm = class(TForm)
    AddLinkButton: TButton;
    BrowseExampleButton: TButton;
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
  private
    FCaretXY: TPoint;
    FChanged: Boolean;
    FLazDocBoss: TLazDocManager;
    FFlags: TLazDocFormFlags;
    fUpdateLock: Integer;
    fEntry: TLazDocInheritedEntry;
    fInheritedEntries: TFPList; // list of TLazDocInheritedEntry. Entry 0 is current.
    function GetValuesFromNode(Node: TDOMNode): TFPDocNode;
    function GetDoc: TXMLdocument;
    function GetDocFile: TLazFPDocFile;
    function GetFirstChildValue(n: TDOMNode): String;
    function GetFirstElement(ADoc: TXMLdocument): TDOMNode;
    function GetModuleNode(ADoc: TXMLdocument): TDOMNode;
    function GetSourceContext(const SrcFilename: string;
                                const CaretPos: TPoint): TPascalHelpContextList;
    function GetSourceFilename: string;
    function MakeLink: String;
    function NodeByPascalContext(ADoc: TXMLdocument;
                              const AContext: TPascalHelpContextList): TDOMNode;
    function GetContextTitle(const AContext: TPascalHelpContextList): string;
    procedure UpdateLinkIdComboBox;
    procedure Save;
    function WriteNode(Entry: TLazDocInheritedEntry; DocNode: TFPDocNode;
                       Interactive: Boolean): Boolean;
    procedure UpdateCaption;
    procedure UpdateValueControls;
    procedure UpdateInheritedControls;
    procedure UpdateInherited;
    procedure UpdateInheritedEntries(All: Boolean);
    procedure ClearInherited(UpdateControls: Boolean);
    function FindInheritedEntry: TLazDocInheritedEntry;
    procedure MoveToInherited(DestEntry: TLazDocInheritedEntry);
  public
    procedure Reset;
    procedure UpdateLazDoc(const SrcFilename: string; const Caret: TPoint);
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure ClearEntry(DoSave: Boolean);
    property DocFile: TLazFPDocFile read GetDocFile;
    property Doc: TXMLdocument read GetDoc;
    property LazDocBoss: TLazDocManager read FLazDocBoss;
    property SourceFilename: string read GetSourceFilename;
    property CaretXY: TPoint read FCaretXY;
  end;

var
  LazDocForm: TLazDocForm = nil;

procedure DoShowLazDoc;

implementation

{ TLazDocForm }

procedure DoShowLazDoc;
begin
  if LazDocForm = Nil then begin
    Application.CreateForm(TLazDocForm, LazDocForm);
    EnvironmentOptions.IDEWindowLayoutList.ItemByEnum(nmiwLazDocName).Apply;
  end;

  LazDocForm.Show;
end;

function TLazDocForm.GetModuleNode(ADoc: TXMLdocument): TDOMNode;
var
  n: TDOMNode;
begin
  Result:=nil;
  if ADoc=nil then exit;

  // get first node
  n := ADoc.FindNode('fpdoc-descriptions');
  if n=nil then exit;

  // proceed to package (could there be more packages in one file??)
  n := n.FirstChild;
  if n=nil then exit;

  // proceed to module  (could there be more modules in one file??)
  n := n.FirstChild;
  while (n<>nil) and (n.NodeName <> 'module') do
    n := n.NextSibling;

  Result := n;
end;

function TLazDocForm.GetFirstElement(ADoc: TXMLdocument): TDOMNode;
var
  Node: TDOMNode;
begin
  //get first module node
  Node := GetModuleNode(ADoc);

  //proceed to element
  Node := Node.FirstChild;
  while Node.NodeName <> 'element' do
    Node := Node.NextSibling;

  Result := Node;
end;

procedure TLazDocForm.UpdateLinkIdComboBox;
// fills LinkIdComboBox.Items
var
  n: TDOMNode;
begin
  LinkIdComboBox.Clear;

  if not Assigned(doc) then
  begin
    {$ifdef dbgLazDoc}
    DebugLn('TLazDocForm.GetElementList: document is not set');
    {$endif}

    Exit;
  end;

  //get first element node
  n := GetFirstElement(Doc);

  //search all elements
  while Assigned(n) do
  begin
    //showmessage(TDomElement(n)['name']);
    LinkIdComboBox.Items.Add(TDomElement(n)['name']);

    n := n.NextSibling;

    if not Assigned(n) then
      Exit;

    while n.NodeName = '#comment' do
      n := n.NextSibling;
  end;
end;

procedure TLazDocForm.FormCreate(Sender: TObject);
begin
  FLazDocBoss:=TLazDocManager.Create;
  fEntry:=TLazDocInheritedEntry.Create;
  
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

  AddLinkButton.Caption := lisLazDocAddLinkButton;
  DeleteLinkButton.Caption := lisLazDocDeleteLinkButton;

  BrowseExampleButton.Caption := lisLazDocBrowseExampleButton;
  
  MoveToInheritedButton.Caption:=lisLDMoveEntriesToInherited;
  CopyFromInheritedButton.Caption:=lisLDCopyFromInherited;
  
  Reset;
  
  Application.AddOnIdleHandler(@ApplicationIdle);
  
  Name := NonModalIDEWindowNames[nmiwLazDocName];
  EnvironmentOptions.IDEWindowLayoutList.Apply(Self, Name);
end;

procedure TLazDocForm.FormDestroy(Sender: TObject);
begin
  ClearInherited(false);
  Application.RemoveAllHandlersOfObject(Self);
  FreeAndNil(fEntry);
  FreeAndNil(FLazDocBoss);
end;

procedure TLazDocForm.FormResize(Sender: TObject);
begin
  LinkIdComboBox.Width := (AddLinkButton.Left - LinkIdComboBox.Left - 8) div 2;
end;

procedure TLazDocForm.FormatButtonClick(Sender: TObject);

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

procedure TLazDocForm.LinkChange(Sender: TObject);
begin
  if LinkListBox.ItemIndex<0 then
    Exit;

  LinkListBox.Items.Strings[LinkListBox.ItemIndex] := MakeLink;
end;

procedure TLazDocForm.LinkListBoxClick(Sender: TObject);
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

procedure TLazDocForm.ApplicationIdle(Sender: TObject; var Done: Boolean);
begin
  if ldffInheritedNeedsUpdate in FFlags then
    UpdateInherited
  else if ldffInheritedEntriesNeedUpdate in FFlags then
    UpdateInheritedEntries(false);
end;

procedure TLazDocForm.MoveToInheritedButtonClick(Sender: TObject);
var
  i: Integer;
  Entry: TLazDocInheritedEntry;
  Candidates: TFPList;
  LazDocSelectInheritedDlg: TLazDocSelectInheritedDlg;
begin
  if fInheritedEntries=nil then exit;
  Candidates:=nil;
  LazDocSelectInheritedDlg:=nil;
  try
    // find all entries till the first inherited entry with a description
    for i:=1 to fInheritedEntries.Count-1 do begin
      Entry:=TLazDocInheritedEntry(fInheritedEntries[i]);
      if Entry.ValuesValid then begin
        if Candidates=nil then
          Candidates:=TFPList.Create;
        Candidates.Add(Entry);
        if Entry.Values[fpdiShort]<>'' then break;
      end;
    end;
    
    // choose one entry
    if (Candidates=nil) or (Candidates.Count=0) then exit;
    if Candidates.Count=1 then begin
      // there is only one candidate
      Entry:=TLazDocInheritedEntry(Candidates[0]);
      if Entry.Values[fpdiShort]<>'' then begin
        // the inherited entry already contains a description.
        // ask if it should be really replacement
        if QuestionDlg('Confirm replace',
          GetContextTitle(Entry.PascalContext)+' already contains the help:'+#13
          +Entry.Values[fpdiShort],
          mtConfirmation,[mrYes,'Replace',mrCancel],0)<>mrYes then exit;
      end;
    end else begin
      // there is more than one candidate
      // => ask which one to replace
      LazDocSelectInheritedDlg:=TLazDocSelectInheritedDlg.Create(nil);
      LazDocSelectInheritedDlg.InheritedComboBox.Items.Clear;
      for i:=0 to Candidates.Count-1 do begin
        Entry:=TLazDocInheritedEntry(Candidates[i]);
        LazDocSelectInheritedDlg.InheritedComboBox.Items.Add(
                                           GetContextTitle(Entry.PascalContext));
      end;
      if LazDocSelectInheritedDlg.ShowModal<>mrOk then exit;
      i:=LazDocSelectInheritedDlg.InheritedComboBox.ItemIndex;
      if i<0 then exit;
      Entry:=TLazDocInheritedEntry(Candidates[i]);
    end;

    // move the content of the current entry to the inherited entry
    MoveToInherited(Entry);
  finally
    LazDocSelectInheritedDlg.Free;
    Candidates.Free;
  end;
end;

function TLazDocForm.NodeByPascalContext(ADoc: TXMLdocument;
  const AContext: TPascalHelpContextList): TDOMNode;
var
  Node: TDOMNode;
  ElementName: String;
begin
  Result := Nil;
  if not Assigned(ADoc) then Exit;

  // get first element node
  ElementName:=GetContextTitle(AContext);
  if ElementName='' then exit;
  //DebugLn('TLazDocForm.NodeByPascalContext ElementName="',ElementName,'"');

  // search elements for ElementName
  Node:=GetFirstElement(ADoc);
  while Node<>nil do begin
    if (Node is TDomElement)
    and (CompareText(TDomElement(Node).GetAttribute('name'),ElementName)=0) then
    begin
      break;
    end;
    Node:=Node.NextSibling;
  end;
  
  if Node=nil then begin
    // no element found
    Exit;
  end;

  {$ifdef dbgLazDoc}
  DebugLn('TLazDocForm.NodeByPascalContext: element node found where name is: ' +
    ElementName);
  {$endif}

  Result := Node;
end;

function TLazDocForm.GetContextTitle(const AContext: TPascalHelpContextList
  ): string;
// get codetools path. for fpdiExample: TButton.Align
var
  Level: Integer;
begin
  Result:='';
  if AContext=nil then exit;
  Level:=0;
  while (Level<AContext.Count) do begin
    case AContext.Items[Level].Descriptor of
    pihcProperty,pihcProcedure,pihcVariable,pihcType,pihcConst:
      begin
        if Result<>'' then Result:=Result+'.';
        Result:=Result+AContext.Items[Level].Context;
      end;
    pihcFilename: ;
    pihcSourceName: ;
    else
      DebugLn('TLazDocForm.NodeByPascalContext unsupported type: "',AContext.Items[Level].Context,'"');
      exit; // unsupported type
    end;
    inc(Level);
  end;
end;

function TLazDocForm.GetFirstChildValue(n: TDOMNode): String;
begin
  if Assigned(n.FirstChild) then
  begin
    {$ifdef dbgLazDoc}
    DebugLn('TLazDocForm.GetFirstChildValue: retrieving node ' +
      n.NodeName + '=' + n.FirstChild.NodeValue);
    {$endif}

    Result := n.FirstChild.NodeValue;
  end
  else
  begin
    {$ifdef dbgLazDoc}
    DebugLn('TLazDocForm.GetFirstChildValue: retrieving empty node ' +
      n.NodeName);
    {$endif}

    Result := '';
  end;
end;

function TLazDocForm.GetValuesFromNode(Node: TDOMNode): TFPDocNode;
var
  S: String;
begin
  Node := Node.FirstChild;
  while Assigned(Node) do
  begin
    if (Node.NodeType = ELEMENT_NODE) then
    begin
      S := Node.NodeName;

      if S = 'short' then
        Result[fpdiShort] := GetFirstChildValue(Node);

      if S = 'descr' then
        Result[fpdiDescription] := GetFirstChildValue(Node);

      if S = 'errors' then
        Result[fpdiErrors] := GetFirstChildValue(Node);

      if S = 'seealso' then
        Result[fpdiSeeAlso] := GetFirstChildValue(Node);

      if S = 'example' then begin
        Result[fpdiExample] := Node.Attributes.GetNamedItem('file').NodeValue;
        writeln('TLazDocForm.ElementFromNode example: ',Result[fpdiExample]);
      end;
    end;
    Node := Node.NextSibling;
  end;
end;

function TLazDocForm.GetDoc: TXMLdocument;
begin
  if DocFile<>nil then
    Result:=DocFile.Doc
  else
    Result:=nil;
end;

function TLazDocForm.GetDocFile: TLazFPDocFile;
begin
  Result:=fEntry.DocFile;
end;

function TLazDocForm.GetSourceContext(const SrcFilename: string;
  const CaretPos: TPoint): TPascalHelpContextList;
begin
  Result:=LazarusHelp.ConvertSourcePosToPascalHelpContext(CaretPos,SrcFilename);
  //if Result<>nil then
  //  DebugLn('TLazDocForm.GetNearestSourceElement Result=',Result.AsString);
end;

function TLazDocForm.GetSourceFilename: string;
begin
  Result:=fEntry.SrcFilename;
end;

procedure TLazDocForm.UpdateCaption;
var
  strCaption: String;
begin
  if fUpdateLock>0 then begin
    Include(FFlags,ldffCaptionNeedsUpdate);
    exit;
  end;
  Exclude(FFlags,ldffCaptionNeedsUpdate);
  
  strCaption := lisLazDocMainFormCaption + ' - ';

  if fEntry.PascalContext <> nil then
    strCaption := strCaption + GetContextTitle(fEntry.PascalContext) + ' - '
  else
    strCaption := strCaption + lisLazDocNoTagCaption + ' - ';

  if DocFile<>nil then
    Caption := strCaption + DocFile.Filename
  else
    Caption := strCaption + lisLazDocNoTagCaption;
  DebugLn(['TLazDocForm.UpdateCaption ',Caption]);
end;

procedure TLazDocForm.UpdateValueControls;
var
  EnabledState: Boolean;
begin
  EnabledState := fEntry.DOMNode<>nil;

  if Assigned(fEntry.DOMNode) then
  begin
    ShortEdit.Text := fEntry.Values[fpdiShort];
    DescrMemo.Lines.Text := ConvertLineEndings(fEntry.Values[fpdiDescription]);
    ErrorsMemo.Lines.Text := ConvertLineEndings(fEntry.Values[fpdiErrors]);
    LinkListBox.Items.Text := ConvertLineEndings(fEntry.Values[fpdiSeeAlso]);
    LinkIdComboBox.Text := '';
    LinkTextEdit.Clear;
    ExampleEdit.Text := ConvertLineEndings(fEntry.Values[fpdiExample]);
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
end;

procedure TLazDocForm.UpdateInheritedControls;
var
  Entry: TLazDocInheritedEntry;
begin
  Entry:=FindInheritedEntry;
  DebugLn(['TLazDocForm.UpdateInheritedControls ',dbgsName(Entry)]);
  if Entry=nil then begin
    InheritedShortEdit.Text:='';
    InheritedShortEdit.Enabled:=false;
    InheritedShortLabel.Caption:='no inherited description found';
  end else begin
    InheritedShortEdit.Text:=Entry.Values[fpdiShort];
    InheritedShortEdit.Enabled:=true;
    InheritedShortLabel.Caption:='Short description of '
                                 +GetContextTitle(Entry.PascalContext);
  end;
  MoveToInheritedButton.Enabled:=(fInheritedEntries<>nil)
                                 and (fInheritedEntries.Count>1)
                                 and (ShortEdit.Text<>'');
  CopyFromInheritedButton.Enabled:=(Entry<>nil);
end;

procedure TLazDocForm.UpdateInherited;
var
  ListOfPCodeXYPosition: TFPList;
  CurCodePos: PCodeXYPosition;
  i: Integer;
  CodeBuffer: TCodeBuffer;
  NewInherited: TLazDocInheritedEntry;
begin
  if fUpdateLock>0 then begin
    Include(FFlags,ldffInheritedNeedsUpdate);
    exit;
  end;
  Exclude(FFlags,ldffInheritedNeedsUpdate);
  
  ClearInherited(true);
  DebugLn(['TLazDocForm.UpdateInherited ']);
  if DocFile=nil then exit;
  if DocFile.CodeBuffer=nil then exit;
  CodeBuffer:=CodeToolBoss.LoadFile(SourceFilename,true,false);
  if CodeBuffer=nil then exit;

  ListOfPCodeXYPosition:=nil;
  try
    // get all possible declarations of this identifier
    if not CodeToolBoss.FindDeclarationAndOverload(CodeBuffer,
      CaretXY.X,CaretXY.Y,ListOfPCodeXYPosition,[])
    then
      exit;
    debugln('TLazDocForm.UpdateInherited Success Overloads=',dbgs(ListOfPCodeXYPosition.Count));
    // convert the source positions in pascal help context list
    if ListOfPCodeXYPosition=nil then exit;
    for i:=0 to ListOfPCodeXYPosition.Count-1 do begin
      CurCodePos:=PCodeXYPosition(ListOfPCodeXYPosition[i]);
      debugln('TLazDocForm.UpdateInherited  C ',CurCodePos^.Code.Filename,' X=',dbgs(CurCodePos^.X),' Y=',dbgs(CurCodePos^.Y));
      if fInheritedEntries=nil then
        fInheritedEntries:=TFPList.Create;
      NewInherited:=TLazDocInheritedEntry.Create;
      NewInherited.SrcFilename:=CurCodePos^.Code.Filename;
      NewInherited.Caret.X:=CurCodePos^.X;
      NewInherited.Caret.Y:=CurCodePos^.Y;
      fInheritedEntries.Add(NewInherited);
    end;
  finally
    FreeListOfPCodeXYPosition(ListOfPCodeXYPosition);
  end;
  Include(FFlags,ldffInheritedEntriesNeedUpdate);
end;

procedure TLazDocForm.UpdateInheritedEntries(All: Boolean);
var
  i: Integer;
  Entry: TLazDocInheritedEntry;
  CurInheritedEntry: TLazDocInheritedEntry;
begin
  if fUpdateLock>0 then begin
    Include(FFlags,ldffInheritedEntriesNeedUpdate);
    exit;
  end;

  CurInheritedEntry:=FindInheritedEntry;
  if (CurInheritedEntry=nil) and (fInheritedEntries<>nil) then begin
    for i:=0 to fInheritedEntries.Count-1 do begin
      Entry:=TLazDocInheritedEntry(fInheritedEntries[i]);
      //DebugLn(['TLazDocForm.UpdateInheritedEntries ',Entry.SrcFilename,' ',dbgs(Entry.Caret)]);
      // find fpdoc file
      if not Entry.DocFilenameValid then begin
        Entry.DocFilenameValid:=true;
        Entry.DocFilename:=
                     LazDocBoss.GetFPDocFilenameForSource(Entry.SrcFilename,true);
        //DebugLn(['TLazDocForm.UpdateInheritedEntries Source=',Entry.SrcFilename,' -> FPDoc=',Entry.DocFilename]);
        if not All then exit;
      end;
      // read fpdoc file
      if not Entry.DocFileValid then begin
        Entry.DocFileValid:=true;
        //DebugLn(['TLazDocForm.UpdateInheritedEntries Parsing ',Entry.DocFilename,' ...']);
        if (Entry.DocFilename<>'') then begin
          if not LazDocBoss.LoadFPDocFile(Entry.DocFilename,true,false,
                                          Entry.DocFile)
          then
            Entry.DocFile:=nil;
          if not All then exit;
        end;
      end;
      // get codetools path
      if not Entry.PascalContextValid then begin
        Entry.PascalContextValid:=true;
        Entry.PascalContext:=LazarusHelp.ConvertSourcePosToPascalHelpContext(
                                                 Entry.Caret,Entry.SrcFilename);
        //DebugLn(['TLazDocForm.UpdateInheritedEntries Pascal=',Entry.PascalContext.AsString]);
        if not All then exit;
      end;
      // get fpdoc values
      if (not Entry.ValuesValid)
      and Entry.PascalContextValid and (Entry.PascalContext<>nil)
      and Entry.DocFileValid and (Entry.DocFile<>nil)
      and (Entry.DocFile.Doc<>nil) then begin
        //DebugLn(['TLazDocForm.UpdateInheritedEntries get fpdoc values ',Entry.PascalContext.AsString]);
        Entry.DOMNode := NodeByPascalContext(Entry.DocFile.Doc,Entry.PascalContext);
        Entry.DOMNodeValid:=true;
        Entry.ValuesValid:=true;
        if Entry.DOMNode<>nil then begin
          Entry.Values := GetValuesFromNode(Entry.DOMNode);
          if CurInheritedEntry=nil then begin
            CurInheritedEntry:=FindInheritedEntry;
            if CurInheritedEntry<>nil then
              UpdateInheritedControls;
          end;
        end;
        if not All then exit;
      end;
    end;
  end;
  Exclude(FFlags,ldffInheritedEntriesNeedUpdate);
end;

procedure TLazDocForm.ClearInherited(UpdateControls: Boolean);
var
  i: Integer;
begin
  //DebugLn(['TLazDocForm.ClearInherited UpdateControls=',UpdateControls]);
  if fInheritedEntries<>nil then begin
    for i:=0 to fInheritedEntries.Count-1 do
      TObject(fInheritedEntries[i]).Free;
    FreeAndNil(fInheritedEntries);
  end;
  if UpdateControls then
    UpdateInheritedControls;
end;

function TLazDocForm.FindInheritedEntry: TLazDocInheritedEntry;
var
  i: Integer;
begin
  if fInheritedEntries=nil then
    exit(nil);
  for i:=1 to fInheritedEntries.Count-1 do begin
    Result:=TLazDocInheritedEntry(fInheritedEntries[i]);
    if Result.ValuesValid and (Result.Values[fpdiShort]<>'') then
      exit;
  end;
  Result:=nil;
end;

procedure TLazDocForm.MoveToInherited(DestEntry: TLazDocInheritedEntry);
begin
  DebugLn(['TLazDocForm.MoveToInherited ',DestEntry.PascalContext.AsString]);
  if not fEntry.ValuesValid then begin
    DebugLn(['TLazDocForm.MoveToInherited not fEntry.NodeValid']);
    exit;
  end;
  if DestEntry.PascalContext=nil then begin
    DebugLn(['TLazDocForm.MoveToInherited DestEntry.PascalContext=nil']);
    exit;
  end;
  if fEntry.PascalContext.IsEqual(DestEntry.PascalContext) then begin
    DebugLn(['TLazDocForm.MoveToInherited fEntry=DestEntry']);
    exit;
  end;
  DebugLn(['TLazDocForm.MoveToInherited Writing to inherited node ...']);
  if WriteNode(DestEntry,fEntry.Values,true) then begin
    DebugLn(['TLazDocForm.MoveToInherited clearing current node ...']);
    ClearEntry(true);
    UpdateInherited;
  end;
end;

procedure TLazDocForm.Reset;
begin
  ClearInherited(true);
  FreeAndNil(FEntry.PascalContext);
  FEntry.DocFile:=nil;
  FEntry.DocFileValid:=false;
  FEntry.DOMNode:=nil;
  FEntry.DOMNodeValid:=false;
  FEntry.ValuesValid:=false;

  // clear all element editors/viewers
  ShortEdit.Clear;
  DescrMemo.Clear;
  ErrorsMemo.Clear;
  LinkIdComboBox.Text := '';
  LinkTextEdit.Clear;
  LinkListBox.Clear;
  ExampleEdit.Clear;

  FChanged := False;
end;

procedure TLazDocForm.UpdateLazDoc(const SrcFilename: string;
  const Caret: TPoint);
var
  NewElement: TPascalHelpContextList;
  DocFilename: String;
  DocFileChanged: Boolean;
begin
  // save the current changes to documentation
  Save;
  BeginUpdate;
  try
    // check if visible
    if not Visible then exit;

    if (SrcFilename=SourceFilename) and (CompareCaret(Caret,CaretXY)=0) then
      exit;
    FCaretXY:=Caret;
    DocFileChanged:=false;

    if SrcFilename<>SourceFilename then begin
      fEntry.SrcFilename:=SrcFilename;

      // search the fpdoc xml file for this unit
      // Note: if this is an include file, find the unit
      DocFilename:=LazDocBoss.GetFPDocFilenameForSource(SrcFilename,true);
      if (DocFile=nil) or (CompareFilenames(DocFile.Filename,DocFilename)<>0)
      then begin
        // DocFile changed
        DebugLn(['TLazDocForm.UpdateLazDoc DocFilename=',DocFilename]);
        DocFileChanged:=true;
        Reset;
        if DocFilename<>'' then begin
          try
            //DebugLn(['TLazDocForm.UpdateLazDoc DocFilename=',DocFilename]);
            if LazDocBoss.LoadFPDocFile(DocFilename,true,false,
              fEntry.DocFile)
            then begin
              fEntry.DocFileValid:=true;
            end else begin
              DebugLn(['TLazDocForm.UpdateLazDoc FAILED DocFilename=',DocFilename]);
              fEntry.DocFile:=nil;
            end;
          except
            on E: Exception do begin
              fEntry.DocFile:=nil;
              MessageDlg('Error in LazDoc',
                'File: '+DocFilename+#13
                +'Error: '+E.Message,
                mtError,[mbCancel],0);
            end;
          end;
        end;
      end;

      UpdateCaption;
      UpdateLinkIdComboBox;
    end;

    if not Assigned(Doc) then
    begin
      { $ifdef dbgLazDoc}
      DebugLn('TLazDocForm.UpdateLazDoc: document is not set');
      { $endif}

      Exit;
    end;

    // fetch source context
    NewElement:=GetSourceContext(SrcFilename, Caret);
    DebugLn(['TLazDocForm.UpdateLazDoc ',NewElement]);
    
    // avoid circles and overhead
    if (not DocFileChanged) then begin
      if ((NewElement<>nil) and (fEntry.PascalContext<>nil)
      and (NewElement.IsEqual(fEntry.PascalContext)))
      or ((NewElement=nil) and (fEntry.PascalContext=nil)) then begin
        DebugLn(['TLazDocForm.UpdateLazDoc Same entry']);
        NewElement.Free;
        exit;
      end;
    end;

    FreeAndNil(fEntry.PascalContext);
    fEntry.PascalContext := NewElement;
    fEntry.PascalContextValid:=true;
    
    fEntry.DOMNode:=NodeByPascalContext(Doc,fEntry.PascalContext);
    fEntry.DOMNodeValid:=true;

    if Assigned(fEntry.DOMNode) then
    begin
      fEntry.Values := GetValuesFromNode(fEntry.DOMNode);
      fEntry.ValuesValid:=true;
    end else
      fEntry.ValuesValid:=false;

    UpdateCaption;

    UpdateValueControls;
    FChanged := False;

    ClearInherited(true);
    Include(FFlags,ldffInheritedNeedsUpdate);
  finally
    EndUpdate;
  end;
end;

procedure TLazDocForm.BeginUpdate;
begin
  inc(fUpdateLock);
end;

procedure TLazDocForm.EndUpdate;
begin
  dec(fUpdateLock);
  if fUpdateLock<0 then RaiseGDBException('');
  if fUpdateLock=0 then begin
    if ldffCaptionNeedsUpdate in FFlags then UpdateCaption;
  end;
end;

procedure TLazDocForm.ClearEntry(DoSave: Boolean);
begin
  FChanged:=true;
  ShortEdit.Text:='';
  DescrMemo.Text:='';
  ErrorsMemo.Text:='';
  LinkListBox.Items.Clear;
  ExampleEdit.Text:='';
  if DoSave then Save;
end;

function ToUnixLineEnding(const s: String): String;
var
  p: Integer;
begin
  Result:=s;
  p:=1;
  while (p<=length(s)) do begin
    if not (s[p] in [#10,#13]) then begin
      inc(p);
    end else begin
      // line ending
      if (p<length(s)) and (s[p+1] in [#10,#13]) and (s[p]<>s[p+1]) then begin
        // double character line ending
        Result:=copy(Result,1,p-1)+#10+copy(Result,p+2,length(Result));
      end else if s[p]=#13 then begin
        // single char line ending #13
        Result[p]:=#10;
      end;
      inc(p);
    end;
  end;
end;

procedure TLazDocForm.Save;
var
  DocNode: TFPDocNode;
begin
  // nothing changed => exit
  if not FChanged then Exit;
  if Doc=nil then exit;
  
  DocNode[fpdiShort]:=ShortEdit.Text;
  DocNode[fpdiDescription]:=DescrMemo.Text;
  DocNode[fpdiErrors]:=ErrorsMemo.Text;
  DocNode[fpdiSeeAlso]:=LinkListBox.Items.Text;
  DocNode[fpdiExample]:=ExampleEdit.Text;
  if not WriteNode(fEntry,DocNode,true) then begin
    DebugLn(['TLazDocForm.Save FAILED']);
  end else begin
    FChanged := False;
  end;
end;

function TLazDocForm.WriteNode(Entry: TLazDocInheritedEntry;
  DocNode: TFPDocNode; Interactive: Boolean): Boolean;
var
  Node: TDOMNode;
  CurNodeName: String;
  NodeWritten: array [TFPDocItem] of Boolean;
  i: TFPDocItem;
  TopNode: TDOMNode;

  function Check(Test: boolean; const  Msg: string): Boolean;
  begin
    Result:=Test;
    if not Test then exit;
    DebugLn(['TLazDocForm.WriteNode  ERROR ',Msg]);
    if Interactive then begin;
      MessageDlg('Write error',
        'Error writing "'+Entry.DocFilename+'"'#13
        +Msg,mtError,[mbCancel],0);
    end;
  end;

  procedure CheckAndWriteNode(const NodeName: String; NodeText: String;
    NodeIndex: TFPDocItem);
  var
    child: TDOMNode;
    FileAttribute: TDOMAttr;
    OldNode: TDOMNode;
    NewValue: String;
  begin
    {$ifdef dbgLazDoc}
    DebugLn('TLazDocForm.Save[CheckAndWriteNode]: checking element: ' +
      NodeName);
    {$endif}

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
    {$ifdef dbgLazDoc}
    DebugLn('TLazDocForm.Save[InsertNodeElement]: inserting element: ' +
      ElementName);
    {$endif}
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
  end;
  
begin
  Result:=false;
  if Check(Entry=nil,'Entry=nil') then exit;
  if Check(Entry.DocFile=nil,'Entry.DocFile=nil') then exit;
  if Check(Entry.DocFile.Doc=nil,'Entry.DocFile.Doc=nil') then exit;
  if Check(Entry.PascalContext=nil,'Entry.PascalContext=nil') then exit;
  if Check(not Entry.DOMNodeValid,'not Entry.DOMNodeValid') then exit;

  if Entry.DOMNode=nil then begin
    // no old node found
    // TODO: create a new node
    Check(false,'no old node found. TODO: implement creating a new.');
    Exit;
  end;

  TopNode := Entry.DOMNode;

  // reset all nodes
  for i := Low(TFPDocItem) to High(TFPDocItem) do
    NodeWritten[i] := False;

  // write all known nodes to XML
  Node := TopNode.FirstChild;
  while Assigned(Node) do
  begin
    if (Node.NodeType = ELEMENT_NODE) then
    begin
      CurNodeName := Node.NodeName;
      CheckAndWriteNode('short', fpdiShort);
      CheckAndWriteNode('descr', fpdiDescription);
      CheckAndWriteNode('errors', fpdiErrors);
      CheckAndWriteNode('seealso', fpdiSeeAlso);
      CheckAndWriteNode('example', fpdiExample);
    end;
    Node := Node.NextSibling;
  end;

  // add new nodes to XML if not already updated
  for i := Low(TFPDocItem) to High(TFPDocItem) do
    if NodeWritten[i] = False then
      case i of
        fpdiShort:
          InsertNodeElement('short', DocNode[fpdiShort]);
        fpdiDescription:
          InsertNodeElement('descr', DocNode[fpdiDescription]);
        fpdiErrors:
          InsertNodeElement('errors', DocNode[fpdiErrors]);
        fpdiSeeAlso:
          InsertNodeElement('seealso', DocNode[fpdiSeeAlso]);
        fpdiExample:
          InsertNodeElement('example', DocNode[fpdiExample]);
      end;

  // write fpdoc xml file
  try
    WriteXMLFile(Doc, DocFile.Filename);
    Result:=true;
  except
    on E: Exception do begin
      MessageDlg('Write error',
        'unable to write file '+DocFile.Filename+#13
        +E.Message,
        mtError,[mbCancel],0);
    end;
  end;
end;

procedure TLazDocForm.DocumentationTagChange(Sender: TObject);
begin
  FChanged := True;
end;

function TLazDocForm.MakeLink: String;
begin
  if Trim(LinkTextEdit.Text) = '' then
    Result := '<link id="' + Trim(LinkIdComboBox.Text) + '"/>'
  else
    Result := '<link id="' + Trim(LinkIdComboBox.Text) + '">' +
      LinkTextEdit.Text + '</link>';
end;

procedure TLazDocForm.AddLinkButtonClick(Sender: TObject);
begin
  if Trim(LinkIdComboBox.Text) <> '' then
  begin
    LinkListBox.Items.Add(MakeLink);
    FChanged := True;
  end;
end;

procedure TLazDocForm.BrowseExampleButtonClick(Sender: TObject);
begin
  if Doc=nil then exit;
  if OpenDialog.Execute then
    ExampleEdit.Text := SetDirSeparators(ExtractRelativepath(
      ExtractFilePath(DocFile.Filename), OpenDialog.FileName));
end;

procedure TLazDocForm.CopyFromInheritedButtonClick(Sender: TObject);
var
  InhEntry: TLazDocInheritedEntry;
begin
  InhEntry:=FindInheritedEntry;
  if InhEntry=nil then exit;
  if (not InhEntry.ValuesValid) then exit;
  if InhEntry.Values[fpdiShort]='' then exit;
  if ShortEdit.Text<>'' then begin
    if QuestionDlg('Confirm replace',
      GetContextTitle(fEntry.PascalContext)+' already contains the help:'+#13
      +ShortEdit.Text,
      mtConfirmation,[mrYes,'Replace',mrCancel],0)<>mrYes then exit;
  end;
  fEntry.Values:=InhEntry.Values;
  fEntry.ValuesValid:=true;

  UpdateValueControls;
  FChanged:=true;
end;

procedure TLazDocForm.DeleteLinkButtonClick(Sender: TObject);
begin
  if LinkListBox.ItemIndex >= 0 then begin
    LinkListBox.Items.Delete(LinkListBox.ItemIndex);
    FChanged := True;
  end;
end;

{ TLazDocInheritedEntry }

destructor TLazDocInheritedEntry.Destroy;
begin
  FreeAndNil(PascalContext);
  inherited Destroy;
end;

initialization
  {$I lazdocfrm.lrs}

end.
