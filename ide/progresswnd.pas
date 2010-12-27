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

  Author: Mattias Gaertner

  Abstract:
    A floating IDE window to show what long tasks are currently running in the
    background.

}
unit ProgressWnd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, FileUtil, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, LazIDEIntf, LazarusIDEStrConsts;

type
  TIDEProgressWindow = class;

  { TIDEProgressItem }

  TIDEProgressItem = class(TComponent)
  private
    FCaption: string;
    FCaptionLabel: TLabel;
    FEndPos: integer;
    FHint: string;
    FPanel: TPanel;
    FPosition: integer;
    FProgressBar: TProgressBar;
    FStartPos: integer;
    FWindow: TIDEProgressWindow;
    procedure SetCaption(const AValue: string);
    procedure SetCaptionLabel(const AValue: TLabel);
    procedure SetEndPos(const AValue: integer);
    procedure SetHint(const AValue: string);
    procedure SetPanel(const AValue: TPanel);
    procedure SetPosition(const AValue: integer);
    procedure SetProgressBar(const AValue: TProgressBar);
    procedure SetStartPos(const AValue: integer);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    property Caption: string read FCaption write SetCaption;
    property Hint: string read FHint write SetHint;
    property StartPos: integer read FStartPos write SetStartPos;
    property EndPos: integer read FEndPos write SetEndPos; // if EndPos=StartPos then unknown
    property Position: integer read FPosition write SetPosition;
    property Panel: TPanel read FPanel write SetPanel;
    property CaptionLabel: TLabel read FCaptionLabel write SetCaptionLabel;
    property ProgressBar: TProgressBar read FProgressBar write SetProgressBar;
    property Window: TIDEProgressWindow read FWindow;
  end;

  { TIDEProgressWindow }

  TIDEProgressWindow = class(TForm)
  private
    FItems: TFPList; // list of TIDEProgressItem
    function GetItems(Index: integer): TIDEProgressItem;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function Count: integer;
    property Items[Index: integer]: TIDEProgressItem read GetItems; default;
    procedure ClearItems;
    function IndexByName(AName: string): integer;
    function AddItem(AName, ACaption, AHint: string): TIDEProgressItem;
  end;

var
  IDEProgressWindow: TIDEProgressWindow = nil;

function CreateProgressItem(AName, ACaption, AHint: string): TIDEProgressItem;

implementation

function CreateProgressItem(AName, ACaption, AHint: string): TIDEProgressItem;
begin
  //debugln(['CreateProgressItem Name="',AName,'" Caption="',ACaption,'"']);
  if IDEProgressWindow=nil then
  begin
    IDEProgressWindow:=TIDEProgressWindow.Create(LazarusIDE.OwningComponent);
  end;
  Result:=IDEProgressWindow.AddItem(AName,ACaption,AHint);
end;

{$R *.lfm}

{ TIDEProgressItem }

procedure TIDEProgressItem.SetCaption(const AValue: string);
begin
  if FCaption=AValue then exit;
  FCaption:=AValue;
  if CaptionLabel<>Nil then
    CaptionLabel.Caption:=Caption;
end;

procedure TIDEProgressItem.SetCaptionLabel(const AValue: TLabel);
begin
  if FCaptionLabel=AValue then exit;
  if CaptionLabel<>nil then
    RemoveFreeNotification(CaptionLabel);
  FCaptionLabel:=AValue;
  if CaptionLabel<>nil then
  begin
    FreeNotification(CaptionLabel);
    CaptionLabel.Caption:=Caption;
    CaptionLabel.Hint:=Hint;
  end;
end;

procedure TIDEProgressItem.SetEndPos(const AValue: integer);
begin
  if FEndPos=AValue then exit;
  FEndPos:=AValue;
  if ProgressBar<>nil then
  begin
    if EndPos>StartPos then
    begin
      ProgressBar.Style:=pbstNormal;
      ProgressBar.Min:=StartPos;
      ProgressBar.Position:=Position;
      ProgressBar.Max:=EndPos;
    end else begin
      ProgressBar.Style:=pbstMarquee;
    end;
  end;
end;

procedure TIDEProgressItem.SetHint(const AValue: string);
begin
  if FHint=AValue then exit;
  FHint:=AValue;
  if ProgressBar<>Nil then
    ProgressBar.Hint:=Hint;
  if CaptionLabel<>Nil then
    CaptionLabel.Hint:=Hint;
end;

procedure TIDEProgressItem.SetPanel(const AValue: TPanel);
begin
  if FPanel=AValue then exit;
  if Panel<>nil then
    RemoveFreeNotification(Panel);
  FPanel:=AValue;
  if Panel<>nil then
    FreeNotification(Panel);
end;

procedure TIDEProgressItem.SetPosition(const AValue: integer);
begin
  if FPosition=AValue then exit;
  FPosition:=AValue;
  if ProgressBar<>nil then
    ProgressBar.Position:=Position;
end;

procedure TIDEProgressItem.SetProgressBar(const AValue: TProgressBar);
begin
  if FProgressBar=AValue then exit;
  if ProgressBar<>nil then
    RemoveFreeNotification(ProgressBar);
  FProgressBar:=AValue;
  if ProgressBar<>nil then begin
    FreeNotification(ProgressBar);
    if EndPos>StartPos then
    begin
      ProgressBar.Style:=pbstNormal;
      ProgressBar.Min:=StartPos;
      ProgressBar.Position:=Position;
      ProgressBar.Max:=EndPos;
    end else begin
      ProgressBar.Style:=pbstMarquee;
    end;
    ProgressBar.Hint:=Hint;
  end;
end;

procedure TIDEProgressItem.SetStartPos(const AValue: integer);
begin
  if FStartPos=AValue then exit;
  FStartPos:=AValue;
  if ProgressBar<>Nil then
  begin
    if EndPos>StartPos then
    begin
      ProgressBar.Style:=pbstNormal;
      ProgressBar.Min:=StartPos;
      ProgressBar.Position:=Position;
      ProgressBar.Max:=EndPos;
    end else begin
      ProgressBar.Style:=pbstMarquee;
    end;
  end;
end;

procedure TIDEProgressItem.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation=opRemove then
  begin
    if AComponent=Panel then
      FPanel:=nil;
    if AComponent=ProgressBar then
      fProgressBar:=nil;
    if AComponent=CaptionLabel then
      fCaptionLabel:=nil;
  end;
end;

{ TIDEProgressWindow }

function TIDEProgressWindow.GetItems(Index: integer): TIDEProgressItem;
begin
  Result:=TIDEProgressItem(FItems[Index]);
end;

procedure TIDEProgressWindow.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  i: Integer;
begin
  inherited Notification(AComponent, Operation);
  if Operation=opRemove then
  begin
    DisableAutoSizing;
    try
      for i:=Count-1 downto 0 do
        if Items[i]=AComponent then
        begin
          FreeAndNil(Items[i].fPanel);
          FItems.Delete(i);
        end;
      if Count=0 then
        Hide;
    finally
      EnableAutoSizing;
    end;
  end;
end;

constructor TIDEProgressWindow.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FItems:=TFPList.Create;
  Caption:=lisPDProgress;
end;

destructor TIDEProgressWindow.Destroy;
begin
  ClearItems;
  FreeAndNil(FItems);
  if IDEProgressWindow=Self then IDEProgressWindow:=nil;
  inherited Destroy;
end;

function TIDEProgressWindow.Count: integer;
begin
  Result:=FItems.Count;
end;

procedure TIDEProgressWindow.ClearItems;
var
  i: Integer;
begin
  for i:=Count-1 downto 0 do Items[i].Free;
end;

function TIDEProgressWindow.IndexByName(AName: string): integer;
begin
  Result:=Count-1;
  while (Result>=0) and (SysUtils.CompareText(AName,Items[Result].Name)<>0) do
    dec(Result);
end;

function TIDEProgressWindow.AddItem(AName, ACaption, AHint: string
  ): TIDEProgressItem;
var
  i: Integer;
  NewName: String;
begin
  //debugln(['TIDEProgressWindow.AddItem Name="',AName,'" Caption="',ACaption,'"']);
  if FindComponent(AName)<>nil then
  begin
    i:=1;
    repeat
      NewName:=AName+IntToStr(i);
    until FindComponent(NewName)=nil;
    AName:=NewName;
  end;
  Result:=TIDEProgressItem.Create(Self);
  Result.FWindow:=Self;
  Result.Name:=AName;
  Result.Caption:=ACaption;
  Result.Hint:=AHint;
  // add a panel
  Result.Panel:=TPanel.Create(Result);
  Result.Panel.Align:=alTop;
  Result.Panel.AutoSize:=true;
  Result.Panel.Constraints.MinWidth:=100;
  Result.Panel.Constraints.MinHeight:=30;
  // add a label into the panel
  Result.CaptionLabel:=TLabel.Create(Result.Panel);
  Result.CaptionLabel.Align:=alTop;
  Result.CaptionLabel.AutoSize:=true;
  Result.CaptionLabel.Parent:=Result.Panel;
  Result.CaptionLabel.ShowHint:=true;
  // add a progressbar below the label
  Result.ProgressBar:=TProgressBar.Create(Result.Panel);
  Result.ProgressBar.Align:=alTop;
  Result.ProgressBar.AutoSize:=true;
  Result.ProgressBar.Parent:=Result.Panel;
  Result.ProgressBar.ShowHint:=true;
  Result.ProgressBar.Top:=10;

  // show panel
  DisableAutoSizing;
  try
    AutoSize:=false;
    Result.Panel.Parent:=Self;
    AutoSize:=true;
    Show;
  finally
    EnableAutoSizing;
  end;
end;

end.

