{ Form for the spotter window

  Copyright (C) 2018  Michael van Canneyt  michael@freepascal.org

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}
unit frmspotter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazUTF8, Forms, Controls, Graphics, Dialogs,
  StdCtrls, IDECommands, LazIDEIntf, Types;

type

  { TSearchItem }
  TMatchPos = Array of Integer;
  TSearchItem = Class(TObject)
  private
    FKeyStroke: String;
    FMatchLen: TMatchPos;
    FMatchPos: TMatchPos;
    FOwnsSource: Boolean;
    FPrefix: String;
    FSource: TObject;
    procedure SetSource(AValue: TObject);
  Public
    Constructor Create(aSource : TObject;AOwnsSource : Boolean = False);
    Destructor Destroy; override;
  published
    Property OwnsSource: Boolean read FOwnsSource Write FOwnsSource;
    Property Source : TObject read FSource write FSource;
    Property KeyStroke : String Read FKeyStroke Write FKeyStroke;
    Property Prefix : String Read FPrefix Write FPrefix;
    Property MatchPos : TMatchPos Read FMatchPos Write FMatchPos;
    Property MatchLen : TMatchPos Read FMatchLen Write FMatchLen;
  end;
  { TSpotterForm }

  TSpotterForm = class(TForm)
    ECommand: TEdit;
    LBMatches: TListBox;
    procedure ECommandChange(Sender: TObject);
    procedure ECommandKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LBMatchesClick(Sender: TObject);
    procedure LBMatchesDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; {%H-}State: TOwnerDrawState);
    procedure LBMatchesKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState
      );
  private
    FKeyStrokeColor: TColor;
    FMatchColor: TColor;
    FShowCategory: Boolean;
    FSearchItems: TStringList;
    FOrgCaption : String;
    FShowShortCutKey: Boolean;
    Procedure Initialize;
    procedure ExecuteSelected;
    Procedure FillCommands;
    procedure FilterList(aSearchTerm: String);
    function GetCommandCategoryString(Cmd: TIDECommand): String;
    procedure RefreshCaption(aCount: Integer);
  public
    Property ShowCategory : Boolean Read FShowCategory Write FShowCategory;
    Property ShowShortCutKey : Boolean Read FShowShortCutKey Write FShowShortCutKey;
    property KeyStrokeColor : TColor Read FKeyStrokeColor Write FKeyStrokeColor;
    property MatchColor : TColor Read FMatchColor Write FMatchColor;
  end;

Var
  ShowCmdCategory : Boolean = True;
  ShowShortCutKey : Boolean = True;
  MatchColor : TColor = clRed;
  KeyStrokeColor : TColor = clGreen;

Procedure ShowSpotterForm;
Procedure ApplySpotterOptions;
Procedure SaveSpotterOptions;
procedure LoadSpotterOptions;

implementation

Uses BaseIDEIntf, LazConfigStorage, StrUtils, LCLIntf, LCLType, LCLProc, srceditorintf;

{$R *.lfm}

var
  SpotterForm: TSpotterForm;


Const
  IDESpotterOptsFile = 'idespotter.xml';

  KeyShowCategory = 'showcategory/value';
  KeyShowShortCut = 'showShortCut/value';
  KeyShortCutColor = 'ShortCutColor/value';
  KeyMatchColor = 'MatchColor/value';

procedure LoadSpotterOptions;
var
  Cfg: TConfigStorage;
begin
  Cfg:=GetIDEConfigStorage(IDESpotterOptsFile,true);
  try
    ShowCmdCategory:=Cfg.GetValue(KeyShowCategory,ShowCmdCategory);
    ShowShortCutKey:=Cfg.GetValue(KeyShowShortCut,ShowShortCutKey);
    KeyStrokeColor:=TColor(Cfg.GetValue(KeyShortCutColor,Ord(KeyStrokeColor)));
    MatchColor:=TColor(Cfg.GetValue(KeyMatchColor,Ord(MatchColor)));
    ApplySpotterOptions;
  finally
    Cfg.Free;
  end;
end;


procedure SaveSpotterOptions;
var
  Cfg: TConfigStorage;
begin
  Cfg:=GetIDEConfigStorage(IDESpotterOptsFile,false);
  try
    Cfg.SetValue(KeyShowCategory,ShowCmdCategory);
    Cfg.SetValue(KeyShowShortCut,ShowShortCutKey);
    Cfg.SetValue(KeyShortCutColor,Ord(KeyStrokeColor));
    Cfg.SetValue(KeyMatchColor,Ord(MatchColor));
  finally
    Cfg.Free;
  end;
end;

Procedure ApplySpotterOptions;

begin
  if Assigned(SpotterForm) then
    begin
    SpotterForm.ShowCategory:=ShowCmdCategory;
    SpotterForm.MatchColor:=MatchColor;
    SpotterForm.KeyStrokeColor:=KeyStrokeColor;
    SpotterForm.ShowShortCutKey:=ShowShortCutKey;
    SpotterForm.Initialize;
    end;
end;

Procedure ShowSpotterForm;

begin
  if SpotterForm=Nil then
    begin
    SpotterForm:=TSpotterForm.Create(Application);
    ApplySpotterOptions;
    end;
  SpotterForm.Show;
end;
{ TSearchItem }

procedure TSearchItem.SetSource(AValue: TObject);
begin
  if FSource=AValue then Exit;
  FSource:=AValue;
end;

constructor TSearchItem.Create(aSource: TObject; AOwnsSource: Boolean);
begin
  FSource:=aSource;
  FOwnsSource:=AOwnsSource;
end;

destructor TSearchItem.Destroy;
begin
  if OwnsSource then
    FreeAndNil(FSource);
  Inherited;
end;

{ TSpotterForm }

procedure TSpotterForm.ECommandChange(Sender: TObject);
begin
  FilterList(ECommand.Text);
end;

procedure TSpotterForm.ECommandKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  Case Key of
  VK_ESCAPE: Hide;
  VK_UP:
    begin
    If LBMatches.ItemIndex>0 then
      LBMatches.ItemIndex:=LBMatches.ItemIndex-1;
    Key:=0;
    ECommand.SelStart:=Length(ECommand.Text);
    end;
  VK_DOWN:
    begin
    If LBMatches.ItemIndex<LBMatches.Items.Count-1 then
      LBMatches.ItemIndex:=LBMatches.ItemIndex+1;
    Key:=0;
    ECommand.SelStart:=Length(ECommand.Text);
    end;
  VK_RETURN :
    If LBMatches.ItemIndex>=0 then
      ExecuteSelected;
  end;
end;

procedure TSpotterForm.FormActivate(Sender: TObject);
begin
  ECommand.SetFocus;
end;

procedure TSpotterForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction:=caHide;
end;

procedure TSpotterForm.RefreshCaption(aCount : Integer);

begin
  if ACount=-1 then
    Caption:=FOrgCaption+Format(' (%d)',[FSearchItems.Count])
  else
    Caption:=FOrgCaption+Format(' (%d/%d)',[aCount,FSearchItems.Count]);
end;

procedure TSpotterForm.FilterList(aSearchTerm: String);

Var
  i : Integer;
  Itm : TSearchItem;
  Words : Array of string;
  MatchPos : Array of Integer;
  MatchLen : Array of Integer;

  Function Match(S : String) : Boolean;

  Var
    I : integer;

  begin
    Result:=True;
    I:=0;
    While Result and (I<Length(Words)) do
      begin
      MatchPos[i]:=Pos(Words[i],S);
      Result:=MatchPos[i]<>0;
      inc(I);
      end;
  end;

begin
  aSearchTerm:=LowerCase(aSearchTerm);
  Setlength(Words,WordCount(aSearchTerm,[' ']));
  Setlength(MatchPos,Length(Words));
  Setlength(MatchLen,Length(Words));
  For I:=1 to Length(Words) do
    begin
    Words[I-1]:=ExtractWord(I,aSearchTerm,[' ']);
    MatchLen[I-1]:=Length(Words[I-1]);
    end;
  LBMatches.Items.BeginUpdate;
  try
    LBMatches.Items.Clear;
    For I:=0 to FSearchItems.Count-1 do
      if Match(FSearchItems[I]) then
        begin
        Itm:=FSearchItems.Objects[I] as TSearchItem;
        Itm.MatchPos:=Copy(MatchPos,0,Length(MatchPos));
        Itm.MatchLen:=Copy(MatchLen,0,Length(MatchLen));
        LBMatches.Items.AddObject(FSearchItems[I],Itm);
        end;
    RefreshCaption(LBMatches.Items.Count);
  finally
    LBMatches.Items.EndUpdate;
    LBMatches.Visible:=LBMatches.Items.Count>0;
    If LBMatches.Visible then
      LBMatches.ItemIndex:=0;
  end;
end;

procedure TSpotterForm.FormCreate(Sender: TObject);
begin
  FSearchItems:=TStringList.Create;
  FSearchItems.OwnsObjects:=True;
  FOrgCaption:=Caption;
end;

procedure TSpotterForm.FormShow(Sender: TObject);
Var
  MF : TForm;
begin
  MF:=LazarusIDE.GetMainBar;
  if MF=nil then MF:=Application.MainForm;
  if Assigned(MF) then
    begin
    Top:=MF.ClientOrigin.y+32; // Note: docked or not docked
    Left:=MF.ClientOrigin.x+(MF.Width-Width) div 2;
    end;
  ECommand.Clear;
  LBMatches.Clear;
  RefreshCaption(-1);
end;

procedure TSpotterForm.LBMatchesClick(Sender: TObject);

begin
  ExecuteSelected;
end;

procedure TSpotterForm.LBMatchesDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);

Var
  LB : TListbox;
  DS,S : String;
  Itm : TSearchItem;
  R : TRect;
  P,I,SP,W,SW : Integer;
  FC : TColor;

begin
  SW := GetSystemMetrics(SM_CXVSCROLL);
  LB:=Control as TListBox;
  LB.Canvas.FillRect(ARect);
  FC:=LB.Canvas.Font.Color;
  Itm:=LB.Items.Objects[Index] as TSearchItem;
  S:=LB.Items[Index];
  R:=ARect;
  if ShowShortCutKey and (Itm.KeyStroke<>'') then
    begin
    W:=LB.Canvas.TextWidth(Itm.KeyStroke);
    R.Right:=R.Right-W-SW;
    end;
  Inc(R.Left,2);
  SP:=1;
  For I:=0 to Length(Itm.MatchPos)-1 do
    begin
    P:=Itm.MatchPos[i];
    if (P-SP>0) then
      begin
      DS:=Copy(S,SP,P-SP);
      LB.Canvas.TextRect(R,R.Left,R.Top,DS);
      Inc(R.Left,LB.Canvas.TextWidth(DS));
      end;
    DS:=Copy(S,P,Itm.MatchLen[i]);
    LB.Canvas.Font.Color:=MatchColor;
    LB.Canvas.TextRect(R,R.Left,R.Top,DS);
    LB.Canvas.Font.Color:=FC;
    Inc(R.Left,LB.Canvas.TextWidth(DS));
    SP:=P+Itm.MatchLen[i];
    end;
  if SP<=Length(S) then
    begin
    DS:=Copy(S,SP,Length(S)-SP+1);
    LB.Canvas.TextRect(R,R.Left,R.Top,DS);
    end;
  if Itm.KeyStroke<>'' then
    begin
    R.Left:=R.Right+1;
    R.Right:=aRect.Right;
    LB.Canvas.Font.Color:=KeyStrokeColor;
    LB.Canvas.TextRect(R,R.Left,R.Top,Itm.KeyStroke);
    end;
end;

procedure TSpotterForm.ExecuteSelected;

Var
  idx: Integer;
  itm : TSearchItem;

begin
  Idx:=LBMatches.ItemIndex;
  if (Idx>=0) then
    begin
    Hide;
    Itm:=LBMatches.Items.Objects[Idx] as TSearchItem;
    if Itm.Source is TIDECommand then
      TIDECommand(Itm.Source).Execute(SourceEditorManagerIntf.ActiveEditor);
    end;
end;


procedure TSpotterForm.LBMatchesKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=VK_ESCAPE then
    Hide;
end;

procedure TSpotterForm.Initialize;
begin
  FillCommands;
end;

function TSpotterForm.GetCommandCategoryString(Cmd: TIDECommand): String;

Const
  Cmds = ' commands';

Var
  Cat: TIDECommandCategory;
  D : String;
  P : Integer;

begin
  Result:='';
  If Not ShowCategory then
    Exit;
  Cat:=Cmd.Category;
  While (Cat<>Nil) and (Cat.Parent<>Nil) do
    Cat:=Cat.Parent;
  If Cat<>Nil then
    begin
    D:=Cat.Description;
    P:=Pos(' commands',D);
    If (P>0) and (P=Length(D)-Length(Cmds)+1) then
      D:=Copy(D,1,P-1);
    Result:=D+': ';
    end;
end;

procedure TSpotterForm.FillCommands;
var
  I, J: Integer;
  Itm : TSearchItem;
  Cmd : TIDECommand;
  Ks,Pref : String;

begin
  For I:=0 to IDECommandList.CategoryCount-1 do
    for J:=0 to IDECommandList.Categories[I].Count-1 do
      begin
      if TObject(IDECommandList.Categories[I].Items[J]) is TIDECommand then
        begin
        Cmd:=TIDECommand(IDECommandList.Categories[I].Items[J]);
        Pref:=GetCommandCategoryString(Cmd);
        ks:='';
        if Cmd.ShortcutA.Key1<>VK_UNKNOWN then
          begin
          ks:=' ('+KeyAndShiftStateToKeyString(Cmd.ShortcutA.Key1,Cmd.ShortcutA.Shift1);
          if Cmd.ShortcutA.Key2<>VK_UNKNOWN then
            ks:=Ks+', '+KeyAndShiftStateToKeyString(Cmd.ShortcutA.Key2,Cmd.ShortcutA.Shift2);
          ks:=ks+')';
          end;
        Itm:=TSearchItem.Create(Cmd);
        Itm.Prefix:=Pref;
        Itm.KeyStroke:=Ks;
        FSearchItems.AddObject(UTF8LowerCase(Pref+Cmd.LocalizedName),Itm);
        end;
      end;
  FSearchItems.Sort;
  RefreshCaption(-1);
end;

end.

