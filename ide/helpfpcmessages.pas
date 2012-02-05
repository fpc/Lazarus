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
  Classes, SysUtils, LCLProc, Dialogs, FileUtil, TextTools, MacroIntf,
  LazarusIDEStrConsts, LazConfigStorage, HelpIntfs, IDEHelpIntf, LazHelpIntf,
  LazHelpHTML, StdCtrls, ButtonPanel, ExtCtrls, Forms, CodeToolsFPCMsgs,
  FileProcs, CodeToolManager, CodeCache;
  
const
  lihcFPCMessages = 'FreePascal Compiler messages';
  lihFPCMessagesURL = 'http://wiki.lazarus.freepascal.org/';

type

  { TFPCMessagesHelpDatabase }

  TFPCMessagesHelpDatabase = class(THTMLHelpDatabase)
  private
    FFPCTranslationFile: string;
    FDefaultNode: THelpNode;
    FFoundComment: string;
    FLastMessage: string;
    FMsgFile: TFPCMsgFile;
    FMsgFileChangeStep: integer;
    FMsgFilename: string;
    procedure SetFPCTranslationFile(const AValue: string);
    procedure SetFoundComment(const AValue: string);
    procedure SetLastMessage(const AValue: string);
    procedure SetMsgFilename(AValue: string);
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
    property MsgFile: TFPCMsgFile read FMsgFile;
    property MsgFilename: string read FMsgFilename write SetMsgFilename;
    property MsgFileChangeStep: integer read FMsgFileChangeStep;
  published
    property FPCTranslationFile: string read FFPCTranslationFile
                                        write SetFPCTranslationFile;
  end;

  { TEditIDEMsgHelpDialog }

  TEditIDEMsgHelpDialog = class(TForm)
    AddButton: TButton;
    ButtonPanel1: TButtonPanel;
    CurGroupBox: TGroupBox;
    CurMsgGroupBox: TGroupBox;
    CurMsgMemo: TMemo;
    DeleteButton: TButton;
    NameEdit: TEdit;
    NameLabel: TLabel;
    OnlyFPCMsgIDsLabel: TLabel;
    OnlyFPCMsgIDsEdit: TEdit;
    OnlyRegExEdit: TEdit;
    OnlyRegExLabel: TLabel;
    Splitter1: TSplitter;
    AllGroupBox: TGroupBox;
    URLEdit: TEdit;
    URLLabel: TLabel;
    URLsListBox: TListBox;
    procedure FormCreate(Sender: TObject);
  public
  end;

procedure CreateFPCMessagesHelpDB;
function AddFPCMessageHelpItem(const Title, URL, RegularExpression: string
                               ): THelpDBIRegExprMessage;

implementation

procedure CreateFPCMessagesHelpDB;
var
  FPCHelp: TFPCMessagesHelpDatabase;
  StartNode: THelpNode;
begin
  FPCMessagesHelpDB:=HelpDatabases.CreateHelpDatabase(lihcFPCMessages,
                                                 TFPCMessagesHelpDatabase,true);
  FPCHelp:=FPCMessagesHelpDB as TFPCMessagesHelpDatabase;
  FPCHelp.DefaultBaseURL:=lihFPCMessagesURL;

  // HTML nodes
  StartNode:=THelpNode.CreateURLID(FPCHelp,'FreePascal Compiler messages',
          'file://Build_messages#FreePascal_Compiler_messages',lihcFPCMessages);
  FPCHelp.TOCNode:=THelpNode.Create(FPCHelp,StartNode);// once as TOC
  FPCHelp.RegisterItemWithNode(StartNode);// and once as normal page

  // register messages
  AddFPCMessageHelpItem('Can''t find unit',
                        'FPC_message:_Can_not_find_unit',': Can''t find unit ');
  AddFPCMessageHelpItem('Wrong number of parameters specified',
                        'FPC_message:_Wrong_number_of_parameters_specified',
                        ': Wrong number of parameters specified');
  AddFPCMessageHelpItem('cannot find -l',
                        'Linker_message:_cannot_find_-l',': cannot find -l');
end;

function AddFPCMessageHelpItem(const Title, URL, RegularExpression: string
  ): THelpDBIRegExprMessage;
begin
  Result:=THelpDBIRegExprMessage.Create(
    THelpNode.CreateURL(FPCMessagesHelpDB,Title,'file://'+URL),
    RegularExpression,'I');
  FPCMessagesHelpDB.RegisterItem(Result);
end;

{ TEditIDEMsgHelpDialog }

procedure TEditIDEMsgHelpDialog.FormCreate(Sender: TObject);
begin
  Caption:='Edit additional help for FPC messages';

  CurMsgGroupBox.Caption:='Selected message in messages window:';

  AllGroupBox.Caption:='All additional';
  AddButton.Caption:='Create new item';

  NameLabel.Caption:='Name:';
  OnlyFPCMsgIDsLabel.Caption:='Only messages with these FPC IDs (comma separated):';
  OnlyRegExLabel.Caption:='Only messages fitting this regular expression:';
  URLLabel.Caption:='URL on wiki, the base url is '
    +(FPCMessagesHelpDB as THTMLHelpDatabase).GetEffectiveBaseURL;

  DeleteButton.Caption:='Delete this item';
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

procedure TFPCMessagesHelpDatabase.SetLastMessage(const AValue: string);
begin
  if FLastMessage=AValue then exit;
  FLastMessage:=AValue;
end;

procedure TFPCMessagesHelpDatabase.SetMsgFilename(AValue: string);
begin
  if FMsgFilename=AValue then Exit;
  FMsgFilename:=AValue;
  FMsgFileChangeStep:=-1;
  FreeAndNil(FMsgFile);
end;

constructor TFPCMessagesHelpDatabase.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FDefaultNode:=THelpNode.CreateURL(Self,'FPC messages: Appendix',
     'http://lazarus-ccr.sourceforge.net/fpcdoc/user/userap3.html#x81-168000C');
end;

destructor TFPCMessagesHelpDatabase.Destroy;
begin
  FreeAndNil(FDefaultNode);
  FreeAndNil(FMsgFile);
  inherited Destroy;
end;

function TFPCMessagesHelpDatabase.GetNodesForMessage(const AMessage: string;
  MessageParts: TStrings; var ListOfNodes: THelpNodeQueryList;
  var ErrMsg: string): TShowHelpResult;
var
  Filename: String;
  Code: TCodeBuffer;
  MsgItem: TFPCMsgItem;
begin
  Result:=inherited GetNodesForMessage(AMessage, MessageParts, ListOfNodes,
                                       ErrMsg);
  if (ListOfNodes<>nil) and (ListOfNodes.Count>0) then exit;

  // no node found -> add default node
  LastMessage:=AMessage;
  Filename:='$(FPCSrcDir)';
  IDEMacros.SubstituteMacros(Filename);
  //DebugLn('TFPCMessagesHelpDatabase.GetNodesForMessage Filename="',Filename,'"');
  if (Filename='') then exit;
  Filename:=AppendPathDelim(Filename)
            +SetDirSeparators('compiler/msg/');
  // TODO: use the same language as the compiler
  if FPCTranslationFile<>'' then
    Filename:=Filename+FPCTranslationFile
  else
    Filename:=Filename+'errore.msg';
  Code:=CodeToolBoss.LoadFile(Filename,true,false);
  if Code=nil then exit;

  // load MsgFile
  if (Filename<>MsgFilename) or (Code.ChangeStep<>MsgFileChangeStep) then begin
    MsgFilename:=Filename;
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
  if MsgFile=nil then exit;

  MsgItem:=MsgFile.FindWithMessage(AMessage);
  if MsgItem=nil then exit;
  FoundComment:=MsgItem.GetTrimmedComment(true,true);
  if FoundComment<>'' then begin
    Result:=shrSuccess;
    CreateNodeQueryListAndAdd(DefaultNode,nil,ListOfNodes,true);
    //DebugLn('TFPCMessagesHelpDatabase.GetNodesForMessage ',FoundComment);
  end;
end;

function TFPCMessagesHelpDatabase.ShowHelp(Query: THelpQuery; BaseNode,
  NewNode: THelpNode; QueryItem: THelpQueryItem; var ErrMsg: string
  ): TShowHelpResult;
begin
  if NewNode=DefaultNode then begin
    if FoundComment<>'' then begin
      Result:=shrSuccess;
      MessageDlg(lisHFMHelpForFreePascalCompilerMessage, FoundComment,
                 mtInformation,[mbOk],0);
    end else begin
      Result:=shrHelpNotFound;
    end;
  end else begin
    Result:=inherited ShowHelp(Query, BaseNode, NewNode, QueryItem, ErrMsg);
  end;
end;

procedure TFPCMessagesHelpDatabase.Load(Storage: TConfigStorage);
begin
  inherited Load(Storage);
  FPCTranslationFile:=Storage.GetValue('FPCTranslationFile/Value','');
end;

procedure TFPCMessagesHelpDatabase.Save(Storage: TConfigStorage);
begin
  inherited Save(Storage);
  Storage.SetDeleteValue('FPCTranslationFile/Value',FPCTranslationFile,'');
end;

end.

