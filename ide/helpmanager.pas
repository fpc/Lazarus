{  $Id$  }
{
 /***************************************************************************
                            helpmanager.pas
                            ---------------


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
}
unit HelpManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Buttons, StdCtrls, LCLProc, HelpIntf,
  IDEOptionDefs;

type
  { TBaseHelpManager }

  TBaseHelpManager = class(TComponent)
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  public
    procedure ConnectMainBarEvents;
  end;


  { TIDEHelpDatabases }

  TIDEHelpDatabases = class(THelpDatabases)
  public
    function ShowHelpSelector(Nodes: TList; var ErrMsg: string;
                              var Selection: THelpNode): TShowHelpResult; override;
  end;
  
  
  { THelpManager }

  THelpManager = class(TBaseHelpManager)
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;
  
var
  HelpBoss: TBaseHelpManager;

implementation

{ TBaseHelpManager }

constructor TBaseHelpManager.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  HelpBoss:=Self;
end;

destructor TBaseHelpManager.Destroy;
begin
  HelpBoss:=nil;
  inherited Destroy;
end;

procedure TBaseHelpManager.ConnectMainBarEvents;
begin

end;

{ THelpSelectorDialog }

type
  THelpSelectorDialog = class(TForm)
    NodesGroupBox: TGroupBox;
    NodesListBox: TListBox;
    OkButton: TButton;
    CancelButton: TButton;
    procedure HelpSelectorDialogClose(Sender: TObject;
      var CloseAction: TCloseAction);
  private
    FNodes: TList;
    procedure SetNodes(const AValue: TList);
    procedure FillNodesListBox;
  public
    constructor Create(TheOwner: TComponent); override;
    property Nodes: TList read FNodes write SetNodes;
  end;

procedure THelpSelectorDialog.HelpSelectorDialogClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure THelpSelectorDialog.SetNodes(const AValue: TList);
begin
  if FNodes=AValue then exit;
  FNodes:=AValue;
  FillNodesListBox;
end;

procedure THelpSelectorDialog.FillNodesListBox;
var
  List: TStringList;
  Node: THelpNode;
  i: Integer;
begin
  List:=TStringList.Create;
  if (Nodes<>nil) then begin
    for i:=0 to Nodes.Count-1 do begin
      Node:=TObject(Nodes[i]) as THelpNode;
      List.Add(Node.Title);
    end;
  end;
  NodesListBox.Items.Assign(List);
  List.Free;
end;

constructor THelpSelectorDialog.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Position:=poScreenCenter;
  IDEDialogLayoutList.ApplyLayout(Self,500,300);
  OnClose:=@HelpSelectorDialogClose;

  NodesGroupBox:=TGroupBox.Create(Self);
  with NodesGroupBox do begin
    Name:='NodesGroupBox';
    Parent:=Self;
    Left:=5;
    Top:=5;
    Width:=Self.ClientWidth-10;
    Height:=Self.ClientWidth-40;
    Anchors:=[akLeft,akTop,akRight,akBottom];
    Caption:='Select a help item:';
  end;
  
  NodesListBox:=TListBox.Create(Self);
  with NodesListBox do begin
    Name:='NodesListBox';
    Parent:=NodesGroupBox;
    Align:=alClient;
  end;
  
  OkButton:=TButton.Create(Self);
  with OkButton do begin
    Name:='OkButton';
    Parent:=Self;
    Left:=5;
    Top:=Self.ClientHeight-35;
    Width:=80;
    Anchors:=[akLeft,akBottom];
    Caption:='Ok';
    ModalResult:=mrOk;
  end;
  
  CancelButton:=TButton.Create(Self);
  with CancelButton do begin
    Name:='CancelButton';
    Parent:=Self;
    Left:=OkButton.Left+OkButton.Width+10;
    Top:=Self.ClientHeight-35;
    Width:=80;
    Anchors:=[akLeft,akBottom];
    Caption:='Cancel';
    ModalResult:=mrCancel;
  end;
end;

{ TIDEHelpDatabases }

function TIDEHelpDatabases.ShowHelpSelector(Nodes: TList; var ErrMsg: string;
  var Selection: THelpNode): TShowHelpResult;
var
  Dialog: THelpSelectorDialog;
  i: LongInt;
begin
  Selection:=nil;
  Result:=shrNone;
  Dialog:=THelpSelectorDialog.Create(Application);
  try
    Dialog.Nodes:=Nodes;
    if Dialog.ShowModal=mrOk then begin
      i:=Dialog.NodesListBox.ItemIndex;
      if i>=0 then begin
        Selection:=THelpNode(Nodes[i]);
        Result:=shrSuccess;
      end;
    end;
  finally
    Dialog.Free;
  end;
end;

{ THelpManager }

constructor THelpManager.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  HelpDatabases:=TIDEHelpDatabases.Create;
  HelpViewers:=THelpViewers.Create;
end;

destructor THelpManager.Destroy;
begin
  FreeThenNil(HelpDatabases);
  FreeThenNil(HelpViewers);
  inherited Destroy;
end;

end.

