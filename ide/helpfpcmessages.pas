{
 /***************************************************************************
                            helpfpcmessages.pas
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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
 
  Author: Mattias Gaertner
   
  Abstract:
    Help items for FPC messages.
}
unit HelpFPCMessages;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, LCLProc, Dialogs, FileUtil, TextTools, MacroIntf,
  LazarusIDEStrConsts, LazConfigStorage, HelpIntfs, IDEHelpIntf, BaseIDEIntf,
  IDEMsgIntf, IDEDialogs, LazHelpIntf, LazHelpHTML, StdCtrls, ButtonPanel,
  ExtCtrls, Forms, Controls, Graphics, LCLIntf, CodeToolsFPCMsgs, FileProcs,
  CodeToolManager, CodeCache;
  
const
  lihcFPCMessages = 'FreePascal Compiler messages';
  lihFPCMessagesURL = 'http://wiki.lazarus.freepascal.org/';

type

  { TMessageHelpAddition }

  TMessageHelpAddition = class
  public
    Name: string;
    URL: string;
    RegEx: string;
    IDs: string; // comma separated
    procedure Assign(Source: TMessageHelpAddition);
    function IsEqual(Source: TMessageHelpAddition): boolean;
    function Fits(ID: integer; Msg: string): boolean;
  end;
  TBaseMessageHelpAdditions = specialize TFPGObjectList<TMessageHelpAddition>;

  { TMessageHelpAdditions }

  TMessageHelpAdditions = class(TBaseMessageHelpAdditions)
  public
    function FindWithName(Name: string): TMessageHelpAddition;
    function IsEqual(Source: TMessageHelpAdditions): boolean;
    procedure Clone(Source: TMessageHelpAdditions);
    procedure LoadFromConfig(Cfg: TConfigStorage);
    procedure SaveToConfig(Cfg: TConfigStorage);
    procedure LoadFromFile(Filename: string);
    procedure SaveToFile(Filename: string);
  end;

  { TFPCMessagesHelpDatabase }

  TFPCMessagesHelpDatabase = class(THTMLHelpDatabase)
  private
    fAdditions: TMessageHelpAdditions;
    FAdditionsChangeStep: integer;
    FAdditionsFile: string;
    FDefaultAdditionsFile: string;
    FFoundAddition: TMessageHelpAddition;
    FFPCTranslationFile: string;
    FDefaultNode: THelpNode;
    FFoundComment: string;
    FLastMessage: string;
    FLoadedAdditionsFilename: string;
    FMsgFile: TFPCMsgFile;
    FMsgFileChangeStep: integer;
    FMsgFilename: string;
    function GetAdditions(Index: integer): TMessageHelpAddition;
    procedure SetAdditionsFile(AValue: string);
    procedure SetFPCTranslationFile(const AValue: string);
    procedure SetFoundComment(const AValue: string);
    procedure SetLastMessage(const AValue: string);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function GetNodesForMessage(const AMessage: string; MessageParts: TStrings;
                                var ListOfNodes: THelpNodeQueryList;
                                var ErrMsg: string): TShowHelpResult; override;
    function ShowHelp(Query: THelpQuery; BaseNode, NewNode: THelpNode;
                      QueryItem: THelpQueryItem;
                      var ErrMsg: string): TShowHelpResult; override;
    procedure Load(Storage: TConfigStorage); override;
    procedure Save(Storage: TConfigStorage); override;
    property DefaultNode: THelpNode read FDefaultNode;
    property LastMessage: string read FLastMessage write SetLastMessage;
    property FoundComment: string read FFoundComment write SetFoundComment;
    property FoundAddition: TMessageHelpAddition read FFoundAddition;

    // the FPC message file
    function GetMsgFile: TFPCMsgFile;
    property MsgFile: TFPCMsgFile read FMsgFile;
    property MsgFilename: string read FMsgFilename;
    property MsgFileChangeStep: integer read FMsgFileChangeStep;

    // additional help for messages (they add an URL to the FPC comments)
    function AdditionsCount: integer;
    property Additions[Index: integer]: TMessageHelpAddition read GetAdditions;
    property AdditionsChangeStep: integer read FAdditionsChangeStep;
    property DefaultAdditionsFile: string read FDefaultAdditionsFile;
    property LoadedAdditionsFilename: string read FLoadedAdditionsFilename;
    procedure ClearAdditions;
    procedure LoadAdditions;
    procedure SaveAdditions;
    function GetAdditionsFilename: string;
  published
    property AdditionsFile: string read FAdditionsFile write SetAdditionsFile;
    property FPCTranslationFile: string read FFPCTranslationFile
                                        write SetFPCTranslationFile;
  end;

  { TEditIDEMsgHelpDialog }

  TEditIDEMsgHelpDialog = class(TForm)
    AddButton: TButton;
    AdditionFitsMsgLabel: TLabel;
    AdditionsFileEdit: TEdit;
    AdditionsFileLabel: TLabel;
    ButtonPanel1: TButtonPanel;
    CurGroupBox: TGroupBox;
    CurMsgGroupBox: TGroupBox;
    CurMsgMemo: TMemo;
    DeleteButton: TButton;
    FPCMsgFileEdit: TEdit;
    FPCMsgFileLabel: TLabel;
    GlobalOptionsGroupBox: TGroupBox;
    NameEdit: TEdit;
    NameLabel: TLabel;
    OnlyFPCMsgIDsLabel: TLabel;
    OnlyFPCMsgIDsEdit: TEdit;
    OnlyRegExEdit: TEdit;
    OnlyRegExLabel: TLabel;
    Splitter1: TSplitter;
    AllGroupBox: TGroupBox;
    TestURLButton: TButton;
    URLEdit: TEdit;
    URLLabel: TLabel;
    AllListBox: TListBox;
    procedure AddButtonClick(Sender: TObject);
    procedure AllListBoxSelectionChange(Sender: TObject; User: boolean);
    procedure ButtonPanel1Click(Sender: TObject);
    procedure ButtonPanel1OKButtonClick(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FPCMsgFileEditEditingDone(Sender: TObject);
    procedure NameEditChange(Sender: TObject);
    procedure OnlyFPCMsgIDsEditChange(Sender: TObject);
    procedure OnlyRegExEditChange(Sender: TObject);
    procedure TestURLButtonClick(Sender: TObject);
    procedure URLEditChange(Sender: TObject);
  private
    fDefaultValue: string;
    procedure FillAdditionsList;
    procedure UpdateCurAddition;
    procedure UpdateCurMessage;
    procedure UpdateAdditionsFitsMsg;
    function IsIDListValid(IDs: string): boolean;
    function IsRegexValid(re: string): boolean;
    function IsURLValid(URL: string): boolean;
  public
    Additions: TMessageHelpAdditions;
    CurAddition: TMessageHelpAddition;
    CurMsg: string;
    CurFPCId: integer;
  end;

var
  FPCMsgHelpDB: TFPCMessagesHelpDatabase = nil;

function ShowMessageHelpEditor: TModalResult;

procedure CreateFPCMessagesHelpDB;

implementation

{$R *.lfm}

function ShowMessageHelpEditor: TModalResult;
var
  Editor: TEditIDEMsgHelpDialog;
begin
  Editor:=TEditIDEMsgHelpDialog.Create(nil);
  try
    Result:=Editor.ShowModal;
  finally
    Editor.Free;
  end;
end;

procedure CreateFPCMessagesHelpDB;
var
  StartNode: THelpNode;
begin
  FPCMessagesHelpDB:=HelpDatabases.CreateHelpDatabase(lihcFPCMessages,
                                                 TFPCMessagesHelpDatabase,true);
  FPCMsgHelpDB:=FPCMessagesHelpDB as TFPCMessagesHelpDatabase;
  FPCMsgHelpDB.DefaultBaseURL:=lihFPCMessagesURL;

  // HTML nodes
  StartNode:=THelpNode.CreateURLID(FPCMsgHelpDB,'FreePascal Compiler messages',
          'file://Build_messages#FreePascal_Compiler_messages',lihcFPCMessages);
  FPCMsgHelpDB.TOCNode:=THelpNode.Create(FPCMsgHelpDB,StartNode);// once as TOC
  FPCMsgHelpDB.RegisterItemWithNode(StartNode);// and once as normal page
end;

{ TMessageHelpAdditions }

function TMessageHelpAdditions.FindWithName(Name: string): TMessageHelpAddition;
var
  i: Integer;
begin
  for i:=0 to Count-1 do begin
    Result:=Items[i];
    if SysUtils.CompareText(Result.Name,Name)=0 then exit;
  end;
  Result:=nil;
end;

function TMessageHelpAdditions.IsEqual(Source: TMessageHelpAdditions): boolean;
var
  i: Integer;
begin
  Result:=false;
  if Source=nil then exit;
  if Source=Self then exit(true);
  if Count<>Source.Count then exit;
  for i:=0 to Count-1 do
    if not Items[i].IsEqual(Source[i]) then exit;
  Result:=true;
end;

procedure TMessageHelpAdditions.Clone(Source: TMessageHelpAdditions);
var
  i: Integer;
  Item: TMessageHelpAddition;
begin
  Clear;
  for i:=0 to Source.Count-1 do begin
    Item:=TMessageHelpAddition.Create;
    Item.Assign(Source[i]);
    Add(Item);
  end;
end;

procedure TMessageHelpAdditions.LoadFromConfig(Cfg: TConfigStorage);
var
  Cnt: Integer;
  i: Integer;
  Item: TMessageHelpAddition;
  SubPath: String;
begin
  Clear;
  Cfg.AppendBasePath('Additions');
  try
    Cnt:=Cfg.GetValue('Count',0);
    for i:=1 to Cnt do begin
      Item:=TMessageHelpAddition.Create;
      SubPath:='Item'+IntToStr(i)+'/';
      Item.Name:=Cfg.GetValue(SubPath+'Name','');
      if Item.Name='' then begin
        Item.Free;
      end else begin
        Add(Item);
        Item.IDs:=cfg.GetValue(SubPath+'IDs','');
        Item.RegEx:=cfg.GetValue(SubPath+'RegEx','');
        Item.URL:=cfg.GetValue(SubPath+'URL','');
      end;
    end;
  finally
    Cfg.UndoAppendBasePath;
  end;
end;

procedure TMessageHelpAdditions.SaveToConfig(Cfg: TConfigStorage);
var
  Cnt: Integer;
  i: Integer;
  Item: TMessageHelpAddition;
  SubPath: String;
begin
  Cfg.AppendBasePath('Additions');
  try
    Cnt:=0;
    for i:=0 to Count-1 do begin
      Item:=Items[i];
      if Item.Name='' then continue;
      inc(Cnt);
      SubPath:='Item'+IntToStr(Cnt)+'/';
      Cfg.SetDeleteValue(SubPath+'Name',Item.Name,'');
      cfg.SetDeleteValue(SubPath+'IDs',Item.IDs,'');
      cfg.SetDeleteValue(SubPath+'RegEx',Item.RegEx,'');
      cfg.SetDeleteValue(SubPath+'URL',Item.URL,'');
    end;
    Cfg.SetDeleteValue('Count',Cnt,0);
  finally
    Cfg.UndoAppendBasePath;
  end;
end;

procedure TMessageHelpAdditions.LoadFromFile(Filename: string);
var
  Cfg: TConfigStorage;
begin
  try
    Cfg:=GetIDEConfigStorage(Filename,true);
    try
      LoadFromConfig(Cfg);
    finally
      Cfg.Free;
    end;
  except
    on E: Exception do begin
      debugln(['TMessageHelpAdditions.LoadFromFile unable to load file "'+Filename+'": '+E.Message]);
    end;
  end;
end;

procedure TMessageHelpAdditions.SaveToFile(Filename: string);
var
  Cfg: TConfigStorage;
begin
  try
    Cfg:=GetIDEConfigStorage(Filename,false);
    try
      SaveToConfig(Cfg);
      Cfg.WriteToDisk;
    finally
      Cfg.Free;
    end;
  except
    on E: Exception do begin
      debugln(['TMessageHelpAdditions.SaveToFile unable to save file "'+Filename+'": '+E.Message]);
    end;
  end;
end;

{ TMessageHelpAddition }

procedure TMessageHelpAddition.Assign(Source: TMessageHelpAddition);
begin
  Name:=Source.Name;
  IDs:=Source.IDs;
  RegEx:=Source.RegEx;
  URL:=Source.URL;
end;

function TMessageHelpAddition.IsEqual(Source: TMessageHelpAddition): boolean;
begin
  Result:=(Name=Source.Name)
      and (IDs=Source.IDs)
      and (RegEx=Source.RegEx)
      and (URL=Source.URL);
end;

function TMessageHelpAddition.Fits(ID: integer; Msg: string): boolean;
var
  CurID: Integer;
  p: PChar;
begin
  Result:=false;
  if Msg='' then exit;
  if RegEx<>'' then begin
    try
      Result:=REMatches(Msg,RegEx,'I');
    except
    end;
    if not Result then exit;
  end;
  if IDs<>'' then begin
    Result:=false;
    p:=PChar(IDs);
    CurID:=0;
    while p^<>#0 do begin
      case p^ of
      ',':
        if (CurID>0) and (CurID=ID) then begin
          Result:=true;
          break;
        end;
      '0'..'9':
        begin
          CurID:=CurID*10+ord(p^)-ord('0');
          if CurID>100000 then exit;
        end;
      else exit;
      end;
      inc(p);
    end;
    if (CurID>0) and (CurID=ID) then Result:=true;
    if not Result then exit;
  end;
end;

{ TEditIDEMsgHelpDialog }

procedure TEditIDEMsgHelpDialog.FormCreate(Sender: TObject);
begin
  fDefaultValue:='(default)';
  Caption:='Edit additional help for messages';

  GlobalOptionsGroupBox.Caption:='Global settings';
  FPCMsgFileLabel.Caption:='FPC message file:';
  AdditionsFileLabel.Caption:='Config file of additions:';

  CurMsgGroupBox.Caption:='Selected message in messages window:';

  AllGroupBox.Caption:='Additions';
  AddButton.Caption:='Create new addition';

  NameLabel.Caption:='Name:';
  OnlyFPCMsgIDsLabel.Caption:='Only messages with these FPC IDs (comma separated):';
  OnlyRegExLabel.Caption:='Only messages fitting this regular expression:';
  URLLabel.Caption:='URL on wiki (the base url is '
              +(FPCMessagesHelpDB as THTMLHelpDatabase).GetEffectiveBaseURL+')';
  TestURLButton.Caption:='Test URL';

  DeleteButton.Caption:='Delete this addition';

  ButtonPanel1.OKButton.OnClick:=@ButtonPanel1OKButtonClick;

  // global options
  FPCMsgFileEdit.Text:=FPCMsgHelpDB.FPCTranslationFile;
  AdditionsFileEdit.Text:=FPCMsgHelpDB.AdditionsFile;

  // fetch selected message
  UpdateCurMessage;

  // list of additions
  FPCMsgHelpDB.LoadAdditions;
  Additions:=TMessageHelpAdditions.Create;
  Additions.Clone(FPCMsgHelpDB.fAdditions);
  FillAdditionsList;

  // current addition
  if AllListBox.Items.Count>0 then
    AllListBox.ItemIndex:=0;
  UpdateCurAddition;
end;

procedure TEditIDEMsgHelpDialog.ButtonPanel1Click(Sender: TObject);
begin

end;

procedure TEditIDEMsgHelpDialog.AllListBoxSelectionChange(Sender: TObject;
  User: boolean);
begin
  UpdateCurAddition;
end;

procedure TEditIDEMsgHelpDialog.AddButtonClick(Sender: TObject);
var
  i: Integer;
  Prefix: String;
  NewName: String;
  Item: TMessageHelpAddition;
begin
  if CurFPCId>=0 then
    Prefix:='Msg'+IntToStr(CurFPCId)+'_'
  else
    Prefix:='Msg';
  i:=1;
  repeat
    NewName:=Prefix+IntToStr(i);
    if Additions.FindWithName(NewName)=nil then break;
    inc(i);
  until false;
  Item:=TMessageHelpAddition.Create;
  Item.Name:=NewName;
  if CurFPCId>=0 then
    Item.IDs:=IntToStr(CurFPCId);
  Additions.Add(Item);
  FillAdditionsList;
  AllListBox.ItemIndex:=AllListBox.Items.IndexOf(Item.Name);
  UpdateCurAddition;
end;

procedure TEditIDEMsgHelpDialog.ButtonPanel1OKButtonClick(Sender: TObject);
var
  Filename: TCaption;
  HasChanged: Boolean;
begin
  HasChanged:=false;

  Filename:=FPCMsgFileEdit.Text;
  if (Filename=fDefaultValue) then
    Filename:='';
  if FPCMsgHelpDB.FPCTranslationFile<>Filename then begin
    FPCMsgHelpDB.FPCTranslationFile:=Filename;
    HasChanged:=true;
  end;

  Filename:=AdditionsFileEdit.Text;
  if (Filename=fDefaultValue)
  or (Filename=FPCMsgHelpDB.FDefaultAdditionsFile) then
    Filename:='';
  if FPCMsgHelpDB.AdditionsFile<>Filename then begin
    FPCMsgHelpDB.AdditionsFile:=Filename;
    HasChanged:=true;
  end;

  if HasChanged then begin
    // ToDo: save changes
    ShowMessage('Saving global options is not yet supported');
  end;

  if not Additions.IsEqual(FPCMsgHelpDB.fAdditions) then
  begin
    FPCMsgHelpDB.fAdditions.Clone(Additions);
    FPCMsgHelpDB.SaveAdditions;
  end;
end;

procedure TEditIDEMsgHelpDialog.DeleteButtonClick(Sender: TObject);
var
  i: LongInt;
  NewIndex: Integer;
begin
  if CurAddition=nil then exit;
  if IDEMessageDialog('Delete?',
    'Delete addition "'+CurAddition.Name+'"?',mtConfirmation,[mbYes,mbNo])<>mrYes
  then exit;
  NewIndex:=AllListBox.ItemIndex;
  i:=Additions.IndexOf(CurAddition);
  CurAddition:=nil;
  if i>=0 then
    Additions.Delete(i);
  FillAdditionsList;
  if NewIndex<0 then NewIndex:=0;
  if NewIndex>=AllListBox.Items.Count then dec(NewIndex);
  AllListBox.ItemIndex:=NewIndex;
  UpdateCurAddition;
end;

procedure TEditIDEMsgHelpDialog.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Additions);
end;

procedure TEditIDEMsgHelpDialog.FPCMsgFileEditEditingDone(Sender: TObject);
var
  Filename: TCaption;
begin
  Filename:=FPCMsgFileEdit.Text;
  if (Filename=fDefaultValue) then Filename:='';

end;

procedure TEditIDEMsgHelpDialog.NameEditChange(Sender: TObject);
var
  NewName: TCaption;
  ConflictAddition: TMessageHelpAddition;
begin
  NewName:=NameEdit.Text;
  ConflictAddition:=Additions.FindWithName(NewName);
  if (NewName='') or (CurAddition=nil)
  or ((ConflictAddition<>nil) and (Additions.FindWithName(NewName)<>CurAddition))
  then begin
    // invalid name
    NameLabel.Font.Color:=clRed;
  end else begin
    NameLabel.Font.Color:=clDefault;
    CurAddition.Name:=NewName;
    AllListBox.Items[AllListBox.ItemIndex]:=NewName;
  end;
end;

procedure TEditIDEMsgHelpDialog.OnlyFPCMsgIDsEditChange(Sender: TObject);
var
  NewIDs: TCaption;
begin
  NewIDs:=OnlyFPCMsgIDsEdit.Text;
  if (CurAddition=nil) or (not IsIDListValid(NewIDs)) then begin
    OnlyFPCMsgIDsLabel.Font.Color:=clRed;
  end else begin
    OnlyFPCMsgIDsLabel.Font.Color:=clDefault;
    CurAddition.IDs:=NewIDs;
    UpdateAdditionsFitsMsg;
  end;
end;

procedure TEditIDEMsgHelpDialog.OnlyRegExEditChange(Sender: TObject);
var
  NewRE: TCaption;
begin
  NewRE:=OnlyRegExEdit.Text;
  if (CurAddition=nil) or (not IsRegexValid(NewRE)) then begin
    OnlyRegExLabel.Font.Color:=clRed;
  end else begin
    OnlyRegExLabel.Font.Color:=clDefault;
    CurAddition.RegEx:=NewRE;
    UpdateAdditionsFitsMsg;
  end;
end;

procedure TEditIDEMsgHelpDialog.TestURLButtonClick(Sender: TObject);
var
  URL: String;
begin
  if (CurAddition=nil) or (CurAddition.URL='') then exit;
  URL:=FPCMsgHelpDB.GetEffectiveBaseURL+CurAddition.URL;
  OpenURL(URL);
end;

procedure TEditIDEMsgHelpDialog.URLEditChange(Sender: TObject);
var
  NewURL: TCaption;
begin
  NewURL:=URLEdit.Text;
  if (CurAddition=nil) or (not IsURLValid(NewURL)) then begin
    URLLabel.Font.Color:=clRed;
  end else begin
    URLLabel.Font.Color:=clDefault;
    CurAddition.URL:=NewURL;
  end;
end;

procedure TEditIDEMsgHelpDialog.FillAdditionsList;
var
  sl: TStringList;
  i: Integer;
begin
  sl:=TStringList.Create;
  try
    for i:=0 to Additions.Count-1 do
      sl.Add(Additions[i].Name);
    sl.Sort;
    AllListBox.Items.Assign(sl);
  finally
    sl.Free;
  end;
end;

procedure TEditIDEMsgHelpDialog.UpdateCurAddition;
var
  i: Integer;
begin
  i:=AllListBox.ItemIndex;
  if i>=0 then
    CurAddition:=Additions.FindWithName(AllListBox.Items[i])
  else
    CurAddition:=nil;
  if CurAddition=nil then begin
    CurGroupBox.Caption:='(None selected)';
    CurGroupBox.Enabled:=false;
    NameEdit.Text:='';
    OnlyFPCMsgIDsEdit.Text:='';
    OnlyRegExEdit.Text:='';
    URLEdit.Text:='';
    for i:=0 to CurGroupBox.ControlCount-1 do
      CurGroupBox.Controls[i].Enabled:=false;
    NameLabel.Font.Color:=clDefault;
    OnlyFPCMsgIDsEdit.Font.Color:=clDefault;
    OnlyRegExEdit.Font.Color:=clDefault;
    URLEdit.Font.Color:=clDefault;
  end else begin
    CurGroupBox.Caption:='Selected addition:';
    CurGroupBox.Enabled:=true;
    NameEdit.Text:=CurAddition.Name;
    NameLabel.Font.Color:=clDefault;
    OnlyFPCMsgIDsEdit.Text:=CurAddition.IDs;
    if not IsIDListValid(CurAddition.IDs) then
      OnlyFPCMsgIDsLabel.Font.Color:=clRed
    else
      OnlyFPCMsgIDsLabel.Font.Color:=clDefault;
    OnlyRegExEdit.Text:=CurAddition.RegEx;
    if not IsRegexValid(CurAddition.RegEx) then
      OnlyRegExLabel.Font.Color:=clRed
    else
      OnlyRegExLabel.Font.Color:=clDefault;
    URLEdit.Text:=CurAddition.URL;
    if not IsURLValid(CurAddition.URL) then
      URLLabel.Font.Color:=clRed
    else
      URLLabel.Font.Color:=clDefault;
    for i:=0 to CurGroupBox.ControlCount-1 do
      CurGroupBox.Controls[i].Enabled:=true;
  end;
  UpdateAdditionsFitsMsg;
end;

procedure TEditIDEMsgHelpDialog.UpdateCurMessage;
var
  Line: TIDEMessageLine;
  sl: TStringList;
  MsgFile: TFPCMsgFile;
  FPCMsg: TFPCMsgItem;
begin
  CurMsg:='';
  CurFPCId:=-1;
  Line:=IDEMessagesWindow.GetSelectedLine;
  if Line=nil then begin
    CurMsgMemo.Text:='(no message selected)';
    CurMsgMemo.Enabled:=false;
  end else begin
    CurMsg:=Line.Msg;
    sl:=TStringList.Create;
    try
      sl.Add('Msg='+Line.Msg);
      MsgFile:=FPCMsgHelpDB.GetMsgFile;
      if MsgFile<>nil then begin
        FPCMsg:=MsgFile.FindWithMessage(Line.Msg);
        if FPCMsg<>nil then begin
          CurFPCId:=FPCMsg.ID;
          sl.Add('FPC Msg='+FPCMsg.GetName);
        end;
      end;
      sl.Add('Directory='+Line.Directory);
      if Line.Parts<>nil then
        sl.AddStrings(Line.Parts);
      CurMsgMemo.Text:=sl.Text;
    finally
      sl.Free;
    end;
    CurMsgMemo.Enabled:=true;
  end;
end;

procedure TEditIDEMsgHelpDialog.UpdateAdditionsFitsMsg;
begin
  if (CurAddition=nil) or (CurMsg='') then
    AdditionFitsMsgLabel.Visible:=false
  else begin
    AdditionFitsMsgLabel.Visible:=true;
    if CurAddition.Fits(CurFPCId,CurMsg) then begin
      AdditionFitsMsgLabel.Caption:='Addition fits the current message';
    end else begin
      AdditionFitsMsgLabel.Caption:='Addition does not fit the current message';
    end;
  end;
end;

function TEditIDEMsgHelpDialog.IsIDListValid(IDs: string): boolean;
// comma separated decimal numbers
var
  p: PChar;
  id: Integer;
begin
  if IDs='' then exit(true);
  Result:=false;
  p:=PChar(IDs);
  id:=0;
  while p^<>#0 do begin
    case p^ of
    ',': id:=0;
    '0'..'9':
      begin
        id:=id*10+ord(p^)-ord('0');
        if id>100000 then begin
          debugln(['TEditIDEMsgHelpDialog.IsIDListValid id too big ',id]);
          exit;
        end;
      end;
    else
      debugln(['TEditIDEMsgHelpDialog.IsIDListValid invalid character ',ord(p^),'=',dbgstr(p[0])]);
      exit;
    end;
    inc(p);
  end;
  Result:=true;
end;

function TEditIDEMsgHelpDialog.IsRegexValid(re: string): boolean;
begin
  if re='' then exit(true);
  Result:=false;
  try
    REMatches('',re,'I');
    Result:=true;
  except
    on E: Exception do begin
      debugln(['TEditIDEMsgHelpDialog.IsRegexValid inalid Re="',re,'": ',E.Message]);
    end;
  end;
end;

function TEditIDEMsgHelpDialog.IsURLValid(URL: string): boolean;
var
  i: Integer;
begin
  Result:=false;
  if URL='' then exit;
  for i:=1 to length(URL) do begin
    if URL[i] in [#0..#32] then exit;
  end;
  Result:=true;
end;

{ TFPCMessagesHelpDatabase }

procedure TFPCMessagesHelpDatabase.SetFoundComment(const AValue: string);
begin
  if FFoundComment=AValue then exit;
  FFoundComment:=AValue;
end;

procedure TFPCMessagesHelpDatabase.SetFPCTranslationFile(const AValue: string);
begin
  if FFPCTranslationFile=AValue then exit;
  FFPCTranslationFile:=AValue;
end;

function TFPCMessagesHelpDatabase.GetAdditions(Index: integer
  ): TMessageHelpAddition;
begin
  Result:=fAdditions[Index];
end;

procedure TFPCMessagesHelpDatabase.SetAdditionsFile(AValue: string);
begin
  if FAdditionsFile=AValue then Exit;
  FAdditionsFile:=AValue;
  FAdditionsChangeStep:=CTInvalidChangeStamp;
  FLoadedAdditionsFilename:='';
end;

procedure TFPCMessagesHelpDatabase.SetLastMessage(const AValue: string);
begin
  if FLastMessage=AValue then exit;
  FLastMessage:=AValue;
end;

constructor TFPCMessagesHelpDatabase.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FDefaultAdditionsFile:='$(LazarusDir)/docs/additionalmsghelp.xml';
  fAdditions:=TMessageHelpAdditions.Create;
  FAdditionsChangeStep:=CTInvalidChangeStamp;
  FMsgFileChangeStep:=CTInvalidChangeStamp;
  FDefaultNode:=THelpNode.CreateURL(Self,'FPC messages: Appendix',
     'http://lazarus-ccr.sourceforge.net/fpcdoc/user/userap3.html#x81-168000C');
end;

destructor TFPCMessagesHelpDatabase.Destroy;
begin
  FreeAndNil(fAdditions);
  FreeAndNil(FDefaultNode);
  FreeAndNil(FMsgFile);
  inherited Destroy;
end;

function TFPCMessagesHelpDatabase.GetNodesForMessage(const AMessage: string;
  MessageParts: TStrings; var ListOfNodes: THelpNodeQueryList;
  var ErrMsg: string): TShowHelpResult;
var
  MsgItem: TFPCMsgItem;
  i: Integer;
  FPCID: Integer;
begin
  FFoundAddition:=nil;
  FFoundComment:='';
  FPCID:=-1;
  Result:=inherited GetNodesForMessage(AMessage, MessageParts, ListOfNodes,
                                       ErrMsg);
  if (ListOfNodes<>nil) and (ListOfNodes.Count>0) then exit;
  LastMessage:=AMessage;

  // search message in FPC message file
  GetMsgFile;
  MsgItem:=MsgFile.FindWithMessage(AMessage);
  if MsgItem<>nil then begin
    FoundComment:=MsgItem.GetTrimmedComment(true,true);
    FPCID:=MsgItem.ID;
  end;

  // search message in additions
  LoadAdditions;
  FFoundAddition:=nil;
  for i:=0 to AdditionsCount-1 do begin
    if Additions[i].Fits(FPCID,AMessage) then begin
      FFoundAddition:=Additions[i];
      break;
    end;
  end;

  if (FoundComment<>'') or (FoundAddition<>nil) then begin
    Result:=shrSuccess;
    CreateNodeQueryListAndAdd(DefaultNode,nil,ListOfNodes,true);
    //DebugLn('TFPCMessagesHelpDatabase.GetNodesForMessage ',FoundComment);
  end;
end;

function TFPCMessagesHelpDatabase.ShowHelp(Query: THelpQuery; BaseNode,
  NewNode: THelpNode; QueryItem: THelpQueryItem; var ErrMsg: string
  ): TShowHelpResult;
var
  URL: String;
begin
  Result:=shrHelpNotFound;
  if NewNode<>DefaultNode then begin
    Result:=inherited ShowHelp(Query, BaseNode, NewNode, QueryItem, ErrMsg);
  end else begin
    URL:='';
    if (FoundAddition<>nil) and (FoundAddition.URL<>'') then
      URL:=GetEffectiveBaseURL+FoundAddition.URL;
    if FoundComment<>'' then begin
      if URL='' then begin
        IDEMessageDialog(lisHFMHelpForFreePascalCompilerMessage, FoundComment,
                   mtInformation,[mbOk]);
      end else begin
        if IDEQuestionDialog(lisHFMHelpForFreePascalCompilerMessage, FoundComment
                   +#13#13'There are additional notes for this message on'#13
                   +URL,
                   mtInformation,[mrYes,'Open URL',mrClose,'Close'])
        =mrYes then begin
          if not OpenURL(URL) then
            exit(shrViewerError);
        end;
      end;
    end else if URL<>'' then begin
      if not OpenURL(URL) then
        exit(shrViewerError);
    end;
    Result:=shrSuccess;
  end;
end;

procedure TFPCMessagesHelpDatabase.Load(Storage: TConfigStorage);
begin
  inherited Load(Storage);
  FPCTranslationFile:=Storage.GetValue('FPCTranslationFile/Value','');
  AdditionsFile:=Storage.GetValue('Additions/Filename','');
end;

procedure TFPCMessagesHelpDatabase.Save(Storage: TConfigStorage);
begin
  inherited Save(Storage);
  Storage.SetDeleteValue('FPCTranslationFile/Value',FPCTranslationFile,'');
  Storage.SetDeleteValue('Additions/Filename',AdditionsFile,'');
end;

function TFPCMessagesHelpDatabase.GetMsgFile: TFPCMsgFile;
var
  Filename: String;
  FPCSrcDir: String;
  Code: TCodeBuffer;
begin
  Result:=nil;
  Filename:=FPCTranslationFile;
  if Filename<>'' then
    IDEMacros.SubstituteMacros(Filename);
  Filename:=TrimFilename(Filename);
  if Filename='' then
    FileName:='errore.msg';
  if not FilenameIsAbsolute(Filename) then begin
    FPCSrcDir:='$(FPCSrcDir)';
    IDEMacros.SubstituteMacros(FPCSrcDir);
    if (FPCSrcDir='') then exit;
    Filename:=TrimFilename(AppendPathDelim(FPCSrcDir)
              +SetDirSeparators('compiler/msg/')+Filename);
  end;
  Code:=CodeToolBoss.LoadFile(Filename,true,false);
  if Code=nil then exit;

  // load MsgFile
  if (Filename<>MsgFilename) or (Code.ChangeStep<>MsgFileChangeStep) then begin
    fMsgFilename:=Filename;
    if FMsgFile=nil then
      FMsgFile:=TFPCMsgFile.Create;
    FMsgFileChangeStep:=Code.ChangeStep;
    try
      MsgFile.LoadFromText(Code.Source);
    except
      on E: Exception do begin
        debugln(['TFPCMessagesHelpDatabase failed to parse "'+MsgFilename+'": '+E.Message]);
        exit;
      end;
    end;
  end;
  Result:=MsgFile;
end;

function TFPCMessagesHelpDatabase.AdditionsCount: integer;
begin
  Result:=fAdditions.Count;
end;

procedure TFPCMessagesHelpDatabase.ClearAdditions;
begin
  fAdditions.Clear;
  FLoadedAdditionsFilename:='';
  FAdditionsChangeStep:=CTInvalidChangeStamp;
  FFoundAddition:=nil;
end;

procedure TFPCMessagesHelpDatabase.LoadAdditions;
var
  Filename: String;
  Code: TCodeBuffer;
begin
  Filename:=GetAdditionsFilename;
  if FLoadedAdditionsFilename<>Filename then
    FAdditionsChangeStep:=CTInvalidChangeStamp;
  Code:=CodeToolBoss.LoadFile(Filename,true,false);
  if Code<>nil then begin
    if Code.ChangeStep=AdditionsChangeStep then exit;
    fAdditionsChangeStep:=Code.ChangeStep;
  end else
    fAdditionsChangeStep:=CTInvalidChangeStamp;
  ClearAdditions;
  fAdditions.LoadFromFile(Filename);
  FLoadedAdditionsFilename:=Filename;
end;

procedure TFPCMessagesHelpDatabase.SaveAdditions;
var
  Code: TCodeBuffer;
  Filename: String;
begin
  Filename:=GetAdditionsFilename;
  fAdditions.SaveToFile(Filename);
  Code:=CodeToolBoss.LoadFile(Filename,true,false);
  if Code<>nil then
    fAdditionsChangeStep:=Code.ChangeStep;
  FLoadedAdditionsFilename:=Filename;
end;

function TFPCMessagesHelpDatabase.GetAdditionsFilename: string;
var
  LazDir: String;
begin
  Result:=AdditionsFile;
  IDEMacros.SubstituteMacros(Result);
  if Result='' then begin
    Result:=FDefaultAdditionsFile;
    IDEMacros.SubstituteMacros(Result);
  end;
  Result:=TrimFilename(Result);
  if not FilenameIsAbsolute(Result) then begin
    LazDir:='$(LazarusDir)';
    IDEMacros.SubstituteMacros(LazDir);
    Result:=TrimFilename(AppendPathDelim(LazDir)+Result);
  end;
end;

end.

