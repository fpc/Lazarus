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
   Interface unit for IDE commands.
}
unit IDECommands;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType;
  
type
  TIDECommandCategory = class;

  TCommandArea = (caSourceEditor, caDesigner);
  TCommandAreas = set of TCommandArea;
  
  TIDECommandKey = record
    Key1: word;
    Shift1: TShiftState;
    Key2: word;
    Shift2: TShiftState;
  end;

  //---------------------------------------------------------------------------
  // class for storing the keys of a single command (key-command relationship)
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
      TheCommand: word; const TheKeyA, TheKeyB: TIDECommandKey);
  public
    KeyA: TIDECommandKey;
    KeyB: TIDECommandKey;
    DefaultKeyA: TIDECommandKey;
    DefaultKeyB: TIDECommandKey;
    property Name: String read FName;
    property Command: word read FCommand;  // see the ecXXX constants above
    property LocalizedName: string read GetLocalizedName write SetLocalizedName;
    property Category: TIDECommandCategory read FCategory write SetCategory;
  end;

  //---------------------------------------------------------------------------
  // TIDECommandCategory is used to divide the key commands in handy packets
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
  
const
  CleanIDECommandKey: TIDECommandKey =
    (Key1: VK_UNKNOWN; Shift1: []; Key2: VK_UNKNOWN; Shift2: []);

function IDECommandKey(Key1: word; Shift1: TShiftState;
  Key2: word; Shift2: TShiftState): TIDECommandKey;


implementation


function IDECommandKey(Key1: word; Shift1: TShiftState;
  Key2: word; Shift2: TShiftState): TIDECommandKey;
begin
  Result.Key1:=Key1;
  Result.Shift1:=Shift1;
  Result.Key2:=Key2;
  Result.Shift2:=Shift2;
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
  CurKey: TIDECommandKey;
begin
  if (KeyA.Key1<>VK_UNKNOWN) and (KeyA.Key2=VK_UNKNOWN) then
    CurKey:=KeyA
  else if (KeyB.Key1<>VK_UNKNOWN) and (KeyB.Key2=VK_UNKNOWN) then
    CurKey:=KeyB
  else
    CurKey:=CleanIDECommandKey;
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
  const TheKeyA, TheKeyB: TIDECommandKey);
begin
  fCommand:=TheCommand;
  fName:=TheName;
  KeyA:=TheKeyA;
  KeyB:=TheKeyB;
  DefaultKeyA:=KeyA;
  DefaultKeyB:=KeyB;
  Category:=TheCategory;
end;

end.

