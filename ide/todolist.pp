(*
/***************************************************************************
                             todolist.pp
                             --------------------

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


  Author: Olivier GUILBAUD <golivier@free.fr>

  Abstract:
    List all to do comments of current project.
    
    the todo comments has this syntax
      {#todoxxx yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy}
      or
      //#todoxxx yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy
      or
      (.*#todoxxx yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy *.) (delete the .)
      
    where  xxx     is the priority
           yyy..yy is the text of todo
           
   you can create an file naming projectname.todo and add list of todo
*)

{#todo goto line of selected unit}
{#todo options }
{#todo print an todo report }

unit TodoList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, LResources,
  ExtCtrls, ComCtrls, Menus, Buttons, GraphType,
  StdCtrls, mPasLex, LCLLinux, LCLType,
  CodeCache, CodeToolManager, LazarusIDEStrConsts;

Const
  cTodoFlag = '#todo';
  
type
  TOnOpenFile = procedure(Sender: TObject; const Filename: string) of object;

  TfrmToDo = class(TForm)
    StatusBar:TStatusBar;
    lvTodo:TListView;
  private
    { private declarations }
    fBuild       : Boolean;
    fFileName    : String;
    FOnOpenFile  : TOnOpenFile;
    fRootCBuffer : TCodeBuffer;
    fScannedFile : TStringList;

    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift:TShiftState);
    procedure SetFileName(const AValue: String);
    
    procedure actFileRefresh(Sender : TObject);
    procedure actEditGoto(Sender : TObject);
    procedure actFilePrint(Sender : TObject);
    procedure actOptionsCfg(Sender : TObject);
    
    procedure ParseComment(const aFileName: string; const SComment, EComment: string;
        const TokenString: string; LineNumber: Integer);
    procedure ParseDirective(aDirective : String);
    
    procedure LoadFile(const aFileName : string);
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property FileName : String read fFileName write SetFileName;
    property OnOpenFile: TOnOpenFile read FOnOpenFile write FOnOpenFile;
  end;

var
  frmToDo: TfrmToDo;

implementation


Type
   PCharArray   = Array[0..16+5] of PChar;
const
  //Images of ToolBar
  cImg_ToDoRefresh : PCharArray=
  ('16 16 5 1',
   '. c None',
   '# c #303030',
   'a c #58a8ff',
   ' ',
   ' ',
   '................',
   '................',
   '................',
   '.......######...',
   '.....##aaa####..',
   '....##aa###.....',
   '....#aa#........',
   '...##aa#........',
   '...#aa#.........',
   '...#aa#.........',
   '...#aa#.........',
   '.##aaa####......',
   '..#aaaaa#.......',
   '...##a##........',
   '....###.........',
   '.....#..........');
   
  cImg_ToDoGotoLine : PCharArray=
  ('16 16 5 1',
   '. c None',
   '# c #303030',
   'b c #ff0000',
   'a c #ffffff',
   ' ',
   '................',
   '................',
   '...#######......',
   '...#aaaaaa#.....',
   '...#aaaaaaa#....',
   '...#aaaaaaaa#...',
   '...#aaaaaaaa#...',
   '...#aaaaaaaa#...',
   '.b.#aaaaaaaa#...',
   '..bbbbbbbbbbb...',
   '.b.#aaaaaaaa#...',
   '...#aaaaaaaa#...',
   '...#aaaaaaaa#...',
   '...##########...',
   '................',
   '................');
   
   cImg_ToDoOptions : PCharArray=
   ('16 16 5 1',
    '. c None',
    'a c #0058c0',
    '# c #303030',
    'c c #ff0000',
    'b c #ffffff',
    '................',
    '.##############.',
    '.#aaaaaaaaaaaa#.',
    '.##############.',
    '.#bbbbbbbbbbbb#.',
    '.#bcbbbccbbbbb#.',
    '.#bccbccbbbbbb#.',
    '.#bbcccbbbbbbb#.',
    '.#bbbcbbbbbbbb#.',
    '.#bcbbbccbbbbb#.',
    '.#bccbccbbbbbb#.',
    '.#bbcccbbbbbbb#.',
    '.#bbbcbbbbbbbb#.',
    '.#bbbbbbbbbbbb#.',
    '.##############.',
    '................');
    
    cImg_ToDoPrint : PCharArray=
    ('16 16 5 1',
     '. c None',
     'e c #00ff00',
     '# c #303030',
     'c c #808080',
     'b c #c3c3c3',
     'd c #ff0000',
     'a c #ffffff',
     '................',
     '................',
     '.......#######..',
     '......#aaaaa#...',
     '.....#aaaaa#....',
     '....#aaaaa#####.',
     '...########bb##.',
     '..#bbbbbbbbb#c#.',
     '..##########cc#.',
     '..#bdbebbb#cc##.',
     '..############..',
     '..#bbbbbbbb##...',
     '...#########....',
     '................');


{ TfrmToDo }

constructor TfrmToDo.Create(AOwner: TComponent);
var C   : TListColumn;
    Btn : TSpeedButton;
    Bar :TPanel;
    Bmp : TBitMap;
    
  procedure AssignResImg(SpdBtn: TSpeedButton; const ResName: string);
  Var PixM : TBitMap;
  begin
    if LazarusResources.Find(ResName)<>nil then
    begin
      PixM:=TBitMap.Create;
      try
        PixM.LoadFromLazarusResource(ResName);
        SpdBtn.Glyph.Assign(PixM);
      finally
        PixM.Free;
      end;
    end;
  end;

begin
  inherited Create(AOwner);
  fBuild:=False;
  fScannedFile:=TStringList.Create;
  Left := 456;
  Top := 116;
  Width := 477;
  Height := 307;
  Caption := dlgTodoListCaption;
  KeyPreview := True;
  Position := poScreenCenter;
  OnKeyDown  :={$IFDEF FPC}@{$ENDIF}FormKeyDown;

  StatusBar:=TStatusBar.Create(Self);
  with StatusBar do
  begin
    parent:=self;
    Left := 0;
    Top := 259;
    Width := 469;
    Height := 19;
    SimplePanel := True;
  end;

  //Tool bar
  Bar:=TPanel.Create(Self);
  with Bar do
  begin
    parent:=self;
    Left := 0;
    Top := 0;
    Height := 22;
    Caption := '';
    Align:=alTop;
    ParentShowHint:=False;
    BevelOuter:=bvNone;
    BevelInner:=bvNone;
  end;

  //Button refresh
  Btn:=TSpeedButton.Create(Bar);
  btn.Parent:=Bar;
  Btn.Flat  :=True;
  Btn.Width :=20;
  Btn.Height:=20;
  Btn.Left  :=0;
  Btn.Top   :=0;
  Bmp:=TBitMap.Create;
  Bmp.Handle:=CreatePixmapIndirect(@cImg_ToDoRefresh[0], GetSysColor(COLOR_BTNFACE));
  Btn.Glyph:=Bmp;
  Btn.ShowHint:=True;
  Btn.Hint  := dlgTodolistRefresh;
  Btn.OnClick:={$IFDEF FPC}@{$ENDIF}actFileRefresh;

  //button Goto
  Btn:=TSpeedButton.Create(Bar);
  btn.Parent:=Bar;
  Btn.Flat  :=True;
  Btn.Width :=20;
  Btn.Height:=20;
  Btn.Left  :=22;
  Btn.Top   :=0;
  Btn.ShowHint:=True;
  Bmp:=TBitMap.Create;
  Bmp.Handle:=CreatePixmapIndirect(@cImg_ToDoGotoLine[0], GetSysColor(COLOR_BTNFACE));
  Btn.Glyph:=Bmp;
  Btn.Hint  := dlgtodoListGotoLine;
  Btn.OnClick:={$IFDEF FPC}@{$ENDIF}actEditGoto;

  //button Print
  Btn:=TSpeedButton.Create(Bar);
  btn.Parent:=Bar;
  Btn.Flat  :=True;
  Btn.Width :=20;
  Btn.Height:=20;
  Btn.Left  :=42;
  Btn.Top   :=0;
  Btn.ShowHint:=True;
  Btn.Hint   :=dlgtodoListPrintList;
  Bmp:=TBitMap.Create;
  Bmp.Handle:=CreatePixmapIndirect(@cImg_ToDoPrint[0], GetSysColor(COLOR_BTNFACE));
  Btn.Glyph:=Bmp;
  Btn.OnClick:={$IFDEF FPC}@{$ENDIF}actFilePrint;

  //button Options
  Btn:=TSpeedButton.Create(Bar);
  btn.Parent:=Bar;
  Btn.Flat  :=True;
  Btn.Width :=20;
  Btn.Height:=20;
  Btn.Left  :=62;
  Btn.Top   :=0;
  Btn.ShowHint:=True;
  Bmp:=TBitMap.Create;
  Bmp.Handle:=CreatePixmapIndirect(@cImg_ToDoOptions[0], GetSysColor(COLOR_BTNFACE));
  Btn.Glyph:=Bmp;
  Btn.Hint  :=  dlgToDoListOptions;
  Btn.OnClick:={$IFDEF FPC}@{$ENDIF}actOptionsCfg;


  lvTodo:=TListView.Create(Self);
  with lvTodo do
  begin
    parent:=self;
    Left := 0;
    Top := 22;
    Width := 469;
    Height := 237;
    Align := alClient;
    ViewStyle:=vsReport;
    lvToDo.OnDblClick:={$IFDEF FPC}@{$ENDIF}actEditGoto;
    //priority column
    C:=Columns.Add;
    C.Caption := ' !';
    C.Width   := 15;
    //Description column
    C:=Columns.Add;
    C.Caption := 'Description';
    C.Width   := 250;
    //File column
    C:=Columns.Add;
    C.Caption := 'File';
    C.Width := 150;
    //Line column
    C:=Columns.Add;
    C.Caption := 'Line';
    C.Width := 28;
  end;

  ActiveControl := lvTodo;
end;

destructor TfrmToDo.Destroy;
begin
  fScannedFile.Free;

  inherited Destroy;
end;

//Load project and scan all files for find the syntax todo
procedure TfrmToDo.actFileRefresh(Sender: TObject);
var
  UsedInterfaceFilenames,
  UsedImplementationFilenames: TStrings;
  i: integer;
  St : String;
begin
  if fBuild then Exit;
  
  Screen.Cursor:=crHourGlass;
  Try
    fBuild:=True;
    lvTodo.Items.Clear;
    fScannedFile.Clear;
    //Find an '.todo' filename
    St:=ChangeFileExt(fFileName,'.todo');
    If FileExists(St) then
      LoadFile(St);

    //Load project file
    LoadFile(fFileName);


    if Assigned(CodeToolBoss) then
    begin
      fRootCBuffer:=CodeToolBoss.LoadFile(fFileName,false,false);
      if not Assigned(fRootCBuffer) then Exit;

      if CodeToolBoss.FindUsedUnits(fRootCBuffer,UsedInterfaceFilenames,
                                    UsedImplementationFilenames) then
      begin
        try
          for i:=0 to UsedInterfaceFilenames.Count-1 do
            LoadFile(UsedInterfaceFilenames[i]);

          for i:=0 to UsedImplementationFilenames.Count-1 do
            LoadFile(UsedImplementationFilenames[i]);
        finally
          UsedImplementationFilenames.Free;
          UsedInterfaceFilenames.Free;
        end;
      end;
    end;
  finally
    Screen.Cursor:=crDefault;
    fBuild:=False;
  end;
end;

procedure TfrmToDo.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key=VK_ESCAPE then
    Close;
end;

//Initialise then todo project and find them
procedure TfrmToDo.SetFileName(const AValue: String);
begin
  if fFileName=AValue then exit;
  fFileName:=AValue;
  actFileRefresh(nil);
end;

procedure TfrmToDo.actEditGoto(Sender: TObject);
var
  CurFilename: String;
begin
  showMessage('not implemented');
  exit;
  CurFilename:='';
  if Assigned(OnOpenFile) then OnOpenFile(Self,CurFilename);
end;

procedure TfrmToDo.actFilePrint(Sender: TObject);
begin
  //showMessage('not implemented');
end;

procedure TfrmToDo.actOptionsCfg(Sender: TObject);
begin
  //showMessage('not implemented');
end;

//Find the {$I filename} directive. If exists, call LoadFile()
procedure TfrmToDo.ParseDirective(aDirective : String);
Var N             : Integer;
    ParsingString : string;
begin
  N:=Pos('{$I ',UpperCase(aDirective));
  if N<>0 then
  begin
    //we found a token that look a include directive. now extract
    //the file name
    ParsingString:=Trim(Copy(aDirective,N+Length('{$I '),MaxInt));
    ParsingString:=Trim(Copy(ParsingString,1,Pos('}',ParsingString)-1));
    // ToDo: search in include path
    LoadFile(ParsingString);
  end;
end;

//Find in comment the ToDo message
procedure TfrmToDo.ParseComment(const aFileName: string; const SComment, EComment: string;
  const TokenString: string; LineNumber: Integer);
Var
  N,J           : Integer;
  ParsingString : string;
  CListItem     : TListItem;
begin
  N:=Pos(UpperCase(cTodoFlag),UpperCase(TokenString));
  if N>1 then
  begin
    // We found a token that looks like a TODO comment. Now
    // verify that it *is* one: either a white-space or the
    // comment token need to be right in front of the TODO item

    // Remove comment characters
    ParsingString := TokenString;
    Delete(ParsingString, 1, Length(SComment));

    // Remove white-space left and right
    ParsingString := Trim(ParsingString);

    // The TODO token should be at the beginning of the comment
    N:=Pos(UpperCase(cTodoFlag),UpperCase(ParsingString));
    if N=1 then
    begin
      // Remove token from string
      Delete(ParsingString, 1, Length(cTodoFlag));
      ParsingString := TrimRight(ParsingString);

      if EComment<>'' then
      begin
        N:=Pos(EComment,ParsingString);
        // Remove end comment from string
        Delete(ParsingString, N, Length(EComment));
        ParsingString := TrimRight(ParsingString);
      end;

      CListItem := lvToDo.Items.Add;

      // Identify numeric priority (if there is one)
      j := 0;
      while j < Length(ParsingString) do
      begin
        if not (ParsingString[j + 1] in ['0'..'9']) then
          Break;
        Inc(j);
      end;
      N:=StrToIntDef(Copy(ParsingString, 1, j), 0);
      ClistItem.Caption:=IntToStr(N);
      Delete(ParsingString, 1, j);
      ParsingString := TrimLeft(ParsingString);

      ClistItem.SubItems.Add(ParsingString);
      ClistItem.SubItems.Add(ExtractFileName(aFileName));
      ClistItem.SubItems.Add(IntToStr(LineNumber));
     // CListItem.ImageIndex := Ord(Info.Priority);
    end;
  end;
end;

//Load an FileName and find {#todox yyyyyy} where
// x is the priority (0 by default)
// yyyy it's the message one line only
procedure TfrmToDo.LoadFile(const aFileName: string);
var
  Parser   : TmwPasLex;
  FlStream : TFileStream;
  EStream  : TMemoryStream;
  Err      : Boolean;
  ST       : String;
begin
  St:=ExtractFileName(aFileName);
  //Quit this method if we have already scan this file
  if fScannedFile.IndexOf(St)<>-1 then Exit;
  StatusBar.SimpleText :=aFileName;// St;
  StatusBar.Repaint;
  fScannedFile.Add(St);

  EStream := TMemoryStream.Create;
  try
    //Echange of stream
    Try
      Err:=False;
      FlStream := TFileStream.Create(aFileName,fmOpenReadWrite);
      try
        EStream.LoadFromStream(FlStream)
      finally
        FlStream.Free;
      end;
    Except
      Err:=True;
    end;

    if Err then Exit;

    EStream.Position := 0;

    Parser := TmwPasLex.Create;
    try
      Parser.Origin := EStream.Memory;
      while Parser.TokenID <> tkNull do
      begin
        { TODO 3 -oStefan -cIssue: This needs to be fixed for multiline comments;
          the strategy ought to be to read the complete comment and only then
          start parsing. Also it would be better to move deleting of the comment
          tokens out of the parser }
        case Parser.TokenID of
          tkBorComment: ParseComment(aFileName, '{', '}', Parser.Token, Parser.LineNumber + 1);
          tkAnsiComment: ParseComment(aFileName, '(*', '*)', Parser.Token, Parser.LineNumber + 1);
          // Slash comments in CPP files should work if they are not in a {}
          tkSlashesComment: ParseComment(aFileName, '//', '', Parser.Token, Parser.LineNumber + 1);
          tkCompDirect : ParseDirective(Parser.Token);
        end;
        Parser.Next;
      end;
    finally
      Parser.Free;
    end;
  finally
    EStream.Free;
    Self.Update;
  end;
end;


initialization
  {.$I ufrmtodo.lrs}
  
end.

