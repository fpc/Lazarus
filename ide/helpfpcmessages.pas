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
  LazHelpHTML;
  
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
  published
    property FPCTranslationFile: string read FFPCTranslationFile
                                        write SetFPCTranslationFile;
  end;

var
  FPCMessagesHelpDB: THelpDatabase;
  
procedure CreateFPCMessagesHelpDB;
function AddFPCMessageHelpItem(const Title, URL, RegularExpression: string
                               ): THelpDBIRegExprMessage;

function FindFPCMessageComment(const CommentFile, Msg: string;
  ExtractText: boolean): string;
procedure ParseFPCMessagesFile(Lines: TStrings;
  const SearchMessage: string; var FoundComment: string);

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

function FindFPCMessageComment(const CommentFile, Msg: string;
  ExtractText: boolean): string;
var
  sl: TStringList;
  p: Integer;
  TagStart: LongInt;
begin
  Result:='';
  sl:=TStringList.Create;
  try
    sl.LoadFromFile(UTF8ToSys(CommentFile));
    ParseFPCMessagesFile(sl,Msg,Result);
    if ExtractText and (Result<>'') then begin
      p:=1;
      while (p<length(Result)) do begin
        case Result[p] of
        '\':
          begin
            TagStart:=p;
            inc(p);
            if (p<=length(Result)) and (Result[p]='\') then begin
              inc(p);
            end else begin
              // remove tag
              while (p<=length(Result)) and (Result[p] in ['a'..'z','A'..'Z'])
              do
                inc(p);
              Result:=copy(Result,1,TagStart-1)+copy(Result,p,length(Result));
              p:=TagStart;
            end;
          end;
        '{','}':
          begin
            // remove brackets
            Result:=copy(Result,1,p-1)+copy(Result,p+1,length(Result));
          end;
        else
          inc(p);
        end;
      end;
    end;
  finally
    sl.Free;
  end;
end;

procedure ParseFPCMessagesFile(Lines: TStrings;
  const SearchMessage: string; var FoundComment: string);
var
  i: integer;
  Line: string;

  procedure Error(const ErrMsg: string);
  begin
    raise Exception.Create('Line='+IntToStr(i+1)+': '+ErrMsg+' in "'+Line+'"');
  end;

  function CompareTextWithSearchMessage(const Command: string;
    TypeStart, TxtStart: integer): boolean;
  var
    RegularExpression: String;
    p: Integer;
  begin
    Result:=false;
    if TxtStart>length(Command) then exit;
    if TypeStart>length(Command) then exit;
    
    RegularExpression:=copy(Command,TxtStart,length(Command));
    // replace all $d variables with (.*)
    p:=length(RegularExpression);
    while (p>0) do begin
      if (RegularExpression[p]='$') then begin
        if (p<length(RegularExpression))
        and (RegularExpression[p+1] in ['1'..'9']) then begin
          RegularExpression:=copy(RegularExpression,1,p-1)+'(.*)'
                           +copy(RegularExpression,p+2,length(RegularExpression));
        end else begin
          RegularExpression:=copy(RegularExpression,1,p-1)+'\'
                           +copy(RegularExpression,p,length(RegularExpression));
        end;
      end;
      dec(p);
    end;
    case Command[TypeStart] of
    'F': RegularExpression:='Fatal: '+RegularExpression;
    'E': RegularExpression:='Error: '+RegularExpression;
    'N': RegularExpression:='Note: '+RegularExpression;
    'I': RegularExpression:='Info: '+RegularExpression;
    'H': RegularExpression:='Hint: '+RegularExpression;
    end;

    try
      Result:=REMatches(SearchMessage,RegularExpression);
    except
      on E: Exception do begin
        WriteLn('CompareTextWithSearchMessage RegExpr Error ',E.Message,' RegularExpression="'+RegularExpression+'"');
        exit;
      end;
    end;
    // debugging:
    //if System.Pos('is assigned but never used',Command)>0 then WriteLn('CompareTextWithSearchMessage "',RegularExpression,'" Result=',Result);
  end;

var
  Command: String;
  CommandLine: Integer;

  procedure ErrorInCommand(const ErrMsg: string);
  begin
    raise Exception.Create('Line='+IntToStr(CommandLine+1)+': '+ErrMsg+' in "'+Command+'"');
  end;

var
  EqualPos: LongInt;
  Comment: String;
  BracketLevel: Integer;
  x: Integer;
  PartStart: Integer;
  TypeStart: LongInt;
  TxtStart: LongInt;
begin
  FoundComment:='';
  Comment:='';
  Command:='';
  CommandLine:=0;
  BracketLevel:=0;
  for i:=0 to Lines.Count-1 do begin
    Line:=Lines[i];
    if (Trim(Line)='') or (Line[1]='#') then continue;

    // example:
    //   general_t_compilername=01000_T_Compiler: $1
    //   % When the \var{-vt} switch is used, this line tells you what compiler
    //   % is used.
    if Line[1]='%' then begin
      if BracketLevel>0 then
        Error('unclosed bracket in lines before');
      Comment:=Comment+copy(Line,2,length(Line));
    end else begin
      if BracketLevel=0 then begin
        // end old message
        if Command<>'' then begin
          if CompareByte(Command[1],'option_',7)=0 then begin
            // option
          end else begin
            // read '='
            EqualPos:=System.Pos('=',Command);
            if EqualPos<1 then ErrorInCommand('missing =');
            // read number
            PartStart:=EqualPos+1;
            TypeStart:=PartStart;
            while (TypeStart<=length(Command)) and (Command[TypeStart]<>'_') do
              inc(TypeStart);
            // read type
            inc(TypeStart);
            TxtStart:=TypeStart;
            while (TxtStart<=length(Command)) and (Command[TxtStart]<>'_') do
              inc(TxtStart);
            // read text
            inc(TxtStart);
            if SearchMessage<>'' then begin
              if CompareTextWithSearchMessage(Command,TypeStart,TxtStart) then
              begin
                FoundComment:=Trim(Comment);
                exit;
              end;
            end;
          end;
        end;

        // start a new message
        Comment:='';
        Command:=Line;
        CommandLine:=i;
      end else begin
        // continue command
        Command:=Command+Line;
      end;
      // update BracketLevel
      for x:=1 to length(Line) do begin
        case Line[x] of
        '[': inc(BracketLevel);
        ']':
          if BracketLevel>0 then
            dec(BracketLevel)
          else
            Error('closing a bracket, which was not opened.');
        end;
      end;
    end;
  end;
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

constructor TFPCMessagesHelpDatabase.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FDefaultNode:=THelpNode.CreateURL(Self,'FPC messages: Appendix',
     'http://lazarus-ccr.sourceforge.net/fpcdoc/user/userap3.html#x81-168000C');
end;

destructor TFPCMessagesHelpDatabase.Destroy;
begin
  FreeAndNil(FDefaultNode);
  inherited Destroy;
end;

function TFPCMessagesHelpDatabase.GetNodesForMessage(const AMessage: string;
  MessageParts: TStrings; var ListOfNodes: THelpNodeQueryList;
  var ErrMsg: string): TShowHelpResult;
var
  Filename: String;
begin
  Result:=inherited GetNodesForMessage(AMessage, MessageParts, ListOfNodes,
                                       ErrMsg);
  if (ListOfNodes<>nil) and (ListOfNodes.Count>0) then exit;

  // no node found -> add default node
  LastMessage:=AMessage;
  Filename:='$(FPCSrcDir)';
  IDEMacros.SubstituteMacros(Filename);
  //DebugLn('TFPCMessagesHelpDatabase.GetNodesForMessage Filename="',Filename,'"');
  if (Filename<>'') then begin
    Filename:=AppendPathDelim(Filename)
              +SetDirSeparators('compiler/msg/');
    // TODO: use the same language as the compiler
    if FPCTranslationFile<>'' then
      Filename:=Filename+FPCTranslationFile
    else
      Filename:=Filename+'errore.msg';
    if FileExistsUTF8(Filename) then begin
      FoundComment:=FindFPCMessageComment(Filename,AMessage,true);
      if FoundComment<>'' then begin
        Result:=shrSuccess;
        CreateNodeQueryListAndAdd(DefaultNode,nil,ListOfNodes,true);
        //DebugLn('TFPCMessagesHelpDatabase.GetNodesForMessage ',FoundComment);
      end;
    end;
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

