{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

 Abstract:
   Under construction by Mattias
 
 
   Interface unit for IDE commands.
   IDE commands are functions like open file, save, build, ... .
   
   Every command can have up to two shortcuts. For example:
     ecCopy: two shortcuts: Ctrl+C and Ctrl+Insert
     ecDeleteChar: one shortcut: Delete
     ecInsertDateTime: no shortcut
   
   Commands are sorted into categories. For example:
     ecCopy is in the category 'Selection'.
     
   Every command can have a menu item.

}
unit IDECommands;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType, Menus;
  
type
  TCommandArea = (
    caMenu,
    caSourceEditor,
    caDesigner
    );
  TCommandAreas = set of TCommandArea;

const
  caAll = [caMenu, caSourceEditor, caDesigner];
  caMenuOnly = [caMenu];
  caSrcEdit = [caMenu,caSourceEditor];
  caSrcEditOnly = [caSourceEditor];
  caDesign = [caMenu,caDesigner];
  caDesignOnly = [caDesigner];


type
  TIDEShortCut = record
    Key1: word;
    Shift1: TShiftState;
    Key2: word;
    Shift2: TShiftState;
  end;

  //---------------------------------------------------------------------------
  // TIDECommandCategory is used to divide the commands in handy packets
  TIDECommandCategory = class(TList)
  protected
    FAreas: TCommandAreas;
    FDescription: string;
    FName: string;
    FParent: TIDECommandCategory;
  public
    property Name: string read FName;
    property Description: string read FDescription;
    property Parent: TIDECommandCategory read FParent;
    property Areas: TCommandAreas read FAreas;
    procedure Delete(Index: Integer); virtual;
  end;

  //---------------------------------------------------------------------------
  // class for storing the keys of a single command
  // (shortcut-command relationship)
  TIDECommandKeys = class
  private
    FCategory: TIDECommandCategory;
    FCommand: word;
    FLocalizedName: string;
    FName: String;
  protected
    function GetLocalizedName: string; virtual;
    procedure SetLocalizedName(const AValue: string); virtual;
    procedure SetCategory(const AValue: TIDECommandCategory); virtual;
  public
    function AsShortCut: TShortCut; virtual;
    constructor Create(TheCategory: TIDECommandCategory; const TheName: String;
      TheCommand: word; const TheKeyA, TheKeyB: TIDEShortCut);
  public
    KeyA: TIDEShortCut;
    KeyB: TIDEShortCut;
    DefaultKeyA: TIDEShortCut;
    DefaultKeyB: TIDEShortCut;
    procedure ClearKeyA;
    procedure ClearKeyB;
    function GetCategoryAndName: string;
    property Name: String read FName;
    property Command: word read FCommand;  // see the ecXXX constants above
    property LocalizedName: string read GetLocalizedName write SetLocalizedName;
    property Category: TIDECommandCategory read FCategory write SetCategory;
  end;

const
  CleanIDEShortCut: TIDEShortCut =
    (Key1: VK_UNKNOWN; Shift1: []; Key2: VK_UNKNOWN; Shift2: []);

function IDEShortCut(Key1: word; Shift1: TShiftState;
  Key2: word; Shift2: TShiftState): TIDEShortCut;


type
  TExecuteIDEShortCut = procedure(Sender: TObject;
                                  var Key: word; Shift: TShiftState;
                                  Areas: TCommandAreas) of object;
  TExecuteIDECommand = procedure(Sender: TObject; Command: word) of object;

var
  // will be set by the IDE
  OnExecuteIDEShortCut: TExecuteIDEShortCut;
  OnExecuteIDECommand: TExecuteIDECommand;

procedure ExecuteIDEShortCut(Sender: TObject; var Key: word; Shift: TShiftState;
                             Areas: TCommandAreas);
procedure ExecuteIDEShortCut(Sender: TObject; var Key: word; Shift: TShiftState);
procedure ExecuteIDECommand(Sender: TObject; Command: word);

function IDEShortCutToMenuShortCut(const IDEShortCut: TIDEShortCut): TShortCut;

implementation


function IDEShortCut(Key1: word; Shift1: TShiftState;
  Key2: word; Shift2: TShiftState): TIDEShortCut;
begin
  Result.Key1:=Key1;
  Result.Shift1:=Shift1;
  Result.Key2:=Key2;
  Result.Shift2:=Shift2;
end;

procedure ExecuteIDEShortCut(Sender: TObject; var Key: word; Shift: TShiftState;
  Areas: TCommandAreas);
begin
  if (OnExecuteIDECommand<>nil) and (Key<>VK_UNKNOWN) then
    OnExecuteIDEShortCut(Sender,Key,Shift,Areas);
end;

procedure ExecuteIDEShortCut(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  OnExecuteIDEShortCut(Sender,Key,Shift,caMenuOnly);
end;

procedure ExecuteIDECommand(Sender: TObject; Command: word);
begin
  if (OnExecuteIDECommand<>nil) and (Command<>0) then
    OnExecuteIDECommand(Sender,Command);
end;

function IDEShortCutToMenuShortCut(const IDEShortCut: TIDEShortCut): TShortCut;
begin
  if IDEShortCut.Key2=VK_UNKNOWN then
    Result:=ShortCut(IDEShortCut.Key1,IDEShortCut.Shift1)
  else
    Result:=ShortCut(VK_UNKNOWN,[]);
end;

{ TIDECommandCategory }

procedure TIDECommandCategory.Delete(Index: Integer);
begin
  inherited Delete(Index);
end;

{ TIDECommandKeys }

function TIDECommandKeys.GetLocalizedName: string;
begin
  if FLocalizedName<>'' then
    Result:=FLocalizedName
  else
    Result:=Name;
end;

procedure TIDECommandKeys.SetLocalizedName(const AValue: string);
begin
  if FLocalizedName=AValue then exit;
  FLocalizedName:=AValue;
end;

procedure TIDECommandKeys.SetCategory(const AValue: TIDECommandCategory);
begin
  if FCategory=AValue then exit;
  // unbind
  if Category<>nil then
    Category.Remove(Self);
  // bind
  fCategory:=AValue;
  if Category<>nil then
    Category.Add(Self);
end;

function TIDECommandKeys.AsShortCut: TShortCut;
var
  CurKey: TIDEShortCut;
begin
  if (KeyA.Key1<>VK_UNKNOWN) and (KeyA.Key2=VK_UNKNOWN) then
    CurKey:=KeyA
  else if (KeyB.Key1<>VK_UNKNOWN) and (KeyB.Key2=VK_UNKNOWN) then
    CurKey:=KeyB
  else
    CurKey:=CleanIDEShortCut;
  Result:=CurKey.Key1;
  if ssCtrl in CurKey.Shift1 then
    Result:=Result+scCtrl;
  if ssShift in CurKey.Shift1 then
    Result:=Result+scShift;
  if ssAlt in CurKey.Shift1 then
    Result:=Result+scAlt;
end;

constructor TIDECommandKeys.Create(TheCategory: TIDECommandCategory;
  const TheName: String; TheCommand: word;
  const TheKeyA, TheKeyB: TIDEShortCut);
begin
  fCommand:=TheCommand;
  fName:=TheName;
  KeyA:=TheKeyA;
  KeyB:=TheKeyB;
  DefaultKeyA:=KeyA;
  DefaultKeyB:=KeyB;
  Category:=TheCategory;
end;

procedure TIDECommandKeys.ClearKeyA;
begin
  KeyA:=CleanIDEShortCut;
end;

procedure TIDECommandKeys.ClearKeyB;
begin
  KeyB:=CleanIDEShortCut;
end;

function TIDECommandKeys.GetCategoryAndName: string;
begin
  Result:='"'+GetLocalizedName+'"';
  if Category<>nil then
    Result:=Result+' in "'+Category.Description+'"';
end;

end.

