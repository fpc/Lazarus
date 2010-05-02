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
}
unit frmmain;

{$MODE Delphi}

interface

uses
  LCLIntf,SysUtils, Classes, Graphics, Controls, Forms, FileUtil,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, ActnList, Menus,
  LResources, LCLType;

type
  TMainForm = class(TForm)
    MainMenu1: TMainMenu;
    ToolBar1: TToolBar;
    ActionList1: TActionList;
    AOpen: TAction;
    AOpenDir: TAction;
    AExit: TAction;
    LBFiles: TListBox;
    SPImage: TSplitter;
    File1: TMenuItem;
    MIOpen: TMenuItem;
    MIOPenDir: TMenuItem;
    N1: TMenuItem;
    MIQuit: TMenuItem;
    TBOPen: TToolButton;
    TBOpenDir: TToolButton;
    ILMain: TImageList;
    ODImage: TOpenDialog;
    AClear: TAction;
    MIOpenDirRec: TMenuItem;
    MIClear: TMenuItem;
    OpenDirRecursively: TAction;
    TBOpenDirRec: TToolButton;
    ADoubleSize: TAction;
    MImage: TMenuItem;
    D1: TMenuItem;
    AHalfSize: TAction;
    MIHalfSize: TMenuItem;
    PImage: TPanel;
    ScrollBox1: TScrollBox;
    IMain: TImage;
    ANextImage: TAction;
    APreviousImage: TAction;
    ANextImageDir: TAction;
    APrevImageDir: TAction;
    MINextImage: TMenuItem;
    PreviousImage1: TMenuItem;
    Nextimagedirectory1: TMenuItem;
    Previousimagedirectory1: TMenuItem;
    ToolButton4: TToolButton;
    TBPRev: TToolButton;
    TBNext: TToolButton;
    TBPRevDir: TToolButton;
    TBNextDir: TToolButton;
    TBDoubleSize: TToolButton;
    TBHalfSize: TToolButton;
    ToolButton3: TToolButton;
    N2: TMenuItem;
    OpenDialog1: TOpenDialog;
    procedure AOpenExecute(Sender: TObject);
    procedure LBFilesClick(Sender: TObject);
    procedure AOpenDirExecute(Sender: TObject);
    procedure AExitExecute(Sender: TObject);
    procedure OpenDirRecursivelyExecute(Sender: TObject);
    procedure AClearExecute(Sender: TObject);
    procedure ADoubleSizeExecute(Sender: TObject);
    procedure AHalfSizeExecute(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure ANextImageExecute(Sender: TObject);
    procedure APreviousImageExecute(Sender: TObject);
    procedure ANextImageDirExecute(Sender: TObject);
    procedure APrevImageDirExecute(Sender: TObject);
  private
    FImageScale : Double;
    procedure AddFile(FileName: String; ShowFile: Boolean);
    procedure ShowFile(Index: Integer);
    procedure AddDir(Directory: String; Recurse: Boolean);
    procedure RescaleImage(NewScale: Double);
    procedure NextImage;
    procedure PreviousImage;
    procedure NextImageDir;
    procedure PreviousImageDir;
    Function NextDirIndex(Direction : Integer) : Integer;
    procedure ShiftImageIndex(MoveBy: Integer);
    procedure ProcessCommandLine;
    procedure DoError(Msg: String; Args: array of const);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

Const
  ImageTypes = '.jpg.jpeg.bmp.xpm.png';

resourcestring
  SSelectImageDir = 'Select directory to add images from';
  SSelectImageDirRec = 'Select directory to recursively add images from';
  SImageViewer = 'Image viewer';
  SErrNeedArgument        = 'Option at position%d (%s) needs an argument';

{ [] }
procedure TMainForm.AOpenExecute(Sender: TObject);

Var
  I : Integer;

begin
  With ODImage do
    begin
    If Execute then
      for I:=0 to Files.Count-1 do
        AddFile(Files[I],(I=0))
    end;
end;

procedure TMainForm.AddFile(FileName :String; ShowFile : Boolean);

Var
  Index : Integer;

begin
  ShowFile:=ShowFile or (LBFiles.Items.Count=0);
  Index:=LBFiles.Items.Add(FileName);
  If ShowFile then
    self.ShowFile(Index);
end;

procedure TMainForm.ShowFile(Index : Integer);

Var
  LoadOK : Boolean;

begin
  If Index=-1 then
    begin
    IMain.Picture:=Nil;
    Caption:=SImageViewer;
    end
  else
    Repeat
      Try
        LoadOK:=False;
        IMain.Align:=AlClient;
        Imain.Stretch:=False;
        FImageScale:=1.0;
        IMain.Picture.LoadFromFile(LBFiles.Items[Index]);
        Caption:=SImageViewer+'('+LBFiles.Items[Index]+')';
        LoadOK:=True;
      Except
        If Index<LBFiles.Items.Count-1 then
          inc(Index)
        else
          Index:=-1;
      end
    Until LoadOK or (Index=-1);
  With LBFiles do
    begin
    If Index<>ItemIndex then
      LBFiles.Itemindex:=Index;
{    If Not ItemVisible(ItemIndex) then
      MakeCurrentVisible;}
    end;
end;

procedure TMainForm.LBFilesClick(Sender: TObject);
begin
  ShowFile(LBFiles.ItemIndex);
end;

procedure TMainForm.AOpenDirExecute(Sender: TObject);

Var
  Dir : String;

begin
  if SelectDirectory(SSelectImageDir,'/',Dir) then

//  if SelectDirectory(SSelectImageDir,'/',Dir,True) then
    AddDir(Dir,False);
end;

procedure TMainForm.AddDir(Directory :String; Recurse : Boolean);

Var
  Info : TSearchRec;
  Ext : String;
begin
  LBFiles.Items.BeginUpdate;
  Try
    Directory:=IncludeTrailingBackslash(Directory);
    if FindFirstUTF8(Directory+'*.*',0,Info)=0 then
      try
        Repeat
          Ext:=ExtractFileExt(Info.Name);
          If Pos(Ext,ImageTypes)<>0 then
            AddFile(Directory+Info.Name,False);
        until (FindNextUTF8(Info)<>0)
      Finally
        FindCloseUTF8(Info);
      end;
    If Recurse then
      if FindFirstUTF8(Directory+'*',faDirectory,Info)=0 then
        try
          Repeat
          If (Info.Name<>'.') and (Info.Name<>'') and (info.name<>'..') and
             ((Info.Attr and faDirectory)<>0) then
            AddDir(Directory+Info.name,True);
          until (FindNextUTF8(Info)<>0)
        finally
          FindCloseUTF8(Info);
        end;
  Finally
    LBFiles.Items.EndUpdate;
  end;
end;

procedure TMainForm.AExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.OpenDirRecursivelyExecute(Sender: TObject);

Var
  Dir : String;

begin
  if SelectDirectory(SSelectImageDirRec,'/',Dir) then
    AddDir(Dir,True);
end;

procedure TMainForm.AClearExecute(Sender: TObject);
begin
  LBFiles.ItemIndex:=-1;
  ShowFile(-1);
  LBFiles.Items.Clear;
end;

procedure TMainForm.ADoubleSizeExecute(Sender: TObject);

begin
  RescaleImage(2.0);
end;

procedure TMainForm.RescaleImage(NewScale : Double);

Var
  OrgWidth,OrgHeight : Integer;
  Rect : TRect;

begin
  OrgWidth:=IMain.Picture.Bitmap.Width;
  OrgHeight:=IMain.Picture.Bitmap.Height;
  FImageScale:=FImageScale*NewScale;
  Rect:=IMain.BoundsRect;
  Rect.Right:=Rect.Left+Round(OrgWidth*FImageScale);
  Rect.Bottom:=Rect.Top+Round(OrgHeight*FImageScale);
  Imain.Align:=AlNone;
  IMain.BoundsRect:=Rect;
  Imain.Stretch:=True;
end;

procedure TMainForm.AHalfSizeExecute(Sender: TObject);
begin
  RescaleImage(0.5);
end;

procedure TMainForm.NextImage;

begin
  ShiftImageIndex(1);
end;

procedure TMainForm.PreviousImage;

begin
  ShiftImageIndex(-1);
end;

procedure TMainForm.ShiftImageIndex(MoveBy : Integer);

Var
  ImageIndex : Integer;

begin
  ImageIndex:=LBFiles.ItemIndex;
  ImageIndex:=ImageIndex+MoveBy;
  If ImageIndex<0 then
    ImageIndex:=LBFiles.Items.Count-1;
  If ImageIndex>=LBFiles.Items.Count then
    begin
    ImageIndex:=0;
    If LBFiles.Items.Count=0 then
      ImageIndex:=-1;
    end;
  ShowFile(ImageIndex);
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (shift=[ssShift]) or (shift=[ssAlt]) then
    begin
    if (key=VK_Prior) then
      begin
      RescaleImage(2.0);
      Key:=0;
      end
    else if (key=VK_Next) then
      begin
      RescaleImage(0.5);
      Key:=0;
      end
    else if (key=VK_Left) then
      begin
      PreviousImage;
      Key:=0;
      end
    else if (key=VK_right) then
      begin
      NextImage;
      Key:=0;
      end
    end
  else if (shift=[]) then
    begin
    if Key=VK_UP then
      Previousimage
    else if Key=VK_DOWN then
      NextImage;
    end;
end;
procedure TMainForm.DoError(Msg : String; Args : Array Of const);

begin
  ShowMessage(Format(Msg,Args));
end;

procedure TMainForm.ProcessCommandLine;

  Function CheckOption(Index : Integer;Short,Long : String): Boolean;

  var
    O : String;

  begin
    O:=ParamStrUTF8(Index);
    Result:=(O='-'+short) or (copy(O,1,Length(Long)+3)=('--'+long+'='));
  end;

  Function OptionArg(Var Index : Integer) : String;

  Var
    P : Integer;

  begin
    if (Length(ParamStrUTF8(Index))>1) and (ParamStrUTF8(Index)[2]<>'-') then
      begin
      If Index<ParamCount then
        begin
        Inc(Index);
        Result:=ParamStrUTF8(Index);
        end
      else
        DoError(SErrNeedArgument,[Index,ParamStrUTF8(Index)]);
      end
    else If length(ParamStrUTF8(Index))>2 then
      begin
      P:=Pos('=',ParamStrUTF8(Index));
      If (P=0) then
        DoError(SErrNeedArgument,[Index,ParamStrUTF8(Index)])
      else
        begin
        Result:=ParamStrUTF8(Index);
        Delete(Result,1,P);
        end;
      end;
  end;

Var
  I : Integer;
  S : String;
  FRecursive : Boolean;

begin
  FRecursive:=False;
  I:=0;
  While (I<ParamCount) do
    begin
    Inc(I);
    If CheckOption(I,'r','recursive') then
      FRecursive:=True
    else
      begin
      S:=ParamStrUTF8(I);
      If DirectoryExistsUTF8(S) then
        AddDir(ExpandFileNameUTF8(S),FRecursive)
      else if FileExistsUTF8(S) then
        AddFile(ExpandFileNameUTF8(S),LBFiles.Items.Count=0);
      end;
    end;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  ProcessCommandLine;
end;

procedure TMainForm.NextImageDir;

Var
  Index : Integer;

begin
  Index:=NextDirIndex(1);
  If (Index<>-1) then
    ShowFile(Index);
end;

Function TMainForm.NextDirIndex(Direction: Integer) : integer;

Var
  Dir : String;

begin
  Result:=-1;
  If LBFiles.Itemindex=-1 then
    Exit;
  Result:=LBFiles.Itemindex;
  Dir:=ExtractFilePath(LBFiles.Items[Result]);
  Repeat
    Result:=Result+Direction;
  Until ((Result=-1) or (Result>=LBFiles.Items.Count)) or (Dir<>ExtractFilePath(LBFiles.Items[Result]));
  If Result>=LBFiles.Items.Count then
    Result:=-1;
end;

procedure TMainForm.PreviousImageDir;
Var
  Index : Integer;

begin
  Index:=NextDirIndex(-1);
  If (Index<>-1) then
    ShowFile(Index);
end;

procedure TMainForm.ANextImageExecute(Sender: TObject);
begin
  NextImage;
end;

procedure TMainForm.APreviousImageExecute(Sender: TObject);
begin
  PreviousImage
end;

procedure TMainForm.ANextImageDirExecute(Sender: TObject);
begin
  NextImageDir;
end;

procedure TMainForm.APrevImageDirExecute(Sender: TObject);
begin
  PreviousImageDir;
end;

end.
