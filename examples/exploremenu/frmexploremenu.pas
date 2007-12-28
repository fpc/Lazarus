{ Copyright (C) 2005  Michael Van Canneyt

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
unit frmExploreMenu;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, Buttons, menuintf, ExtCtrls;

type

  { TExploreIDEMenuForm }

  TExploreIDEMenuForm = class(TForm)
    BCLose: TBitBtn;
    LTree: TLabel;
    LPAth: TMemo;
    Splitter1: TSplitter;
    TVIDEMenu: TTreeView;
    procedure FormCreate(Sender: TObject);
    procedure TVIDEMenuChange(Sender: TObject; Node: TTreeNode);
  private
    { private declarations }
     procedure AddMenuItem(ParentNode : TTreeNode;Item : TIDEMenuItem);
  public
    { public declarations }
  end; 

var
  ExploreIDEMenuForm: TExploreIDEMenuForm;

Procedure Register;

implementation


Const
  SExploreIDEMEnu = 'ExploreIDEMEnu';
  
Resourcestring
  SExploreIDEMenUCaption = 'Explore IDE menu...';
  SSelectedPath = 'Path of selected item: ';
  
Procedure ShowMenu(Sender : TObject);

begin
  if Sender=nil then ;
  With TExploreIDEMenuForm.Create(Application) do
    try
      ShowModal;
    Finally
      Free;
    end;
end;

Procedure Register;

begin
  RegisterIDEMenuCommand(itmSecondaryTools,SExploreIDEMEnu,
                         SExploreIDEMenuCaption,nil,@ShowMenu,nil,'');
end;


{ TExploreIDEMenuForm }

procedure TExploreIDEMenuForm.FormCreate(Sender: TObject);

Var
  I : Integer;

begin
  if Sender=nil then ;
  With TVIDEMenu.Items do
    begin
    BeginUpdate;
    Try
      Clear;
      For I:=0 to IDEMenuRoots.Count-1 do
        AddMenuItem(Nil,IDEMenuRoots[I]);
    Finally
      EndUpdate;
    end;
    end;
end;

procedure TExploreIDEMenuForm.TVIDEMenuChange(Sender: TObject; Node: TTreeNode);

Var
  N : TTreeNode;

begin
  if Sender=nil then ;
  if Node=nil then ;
  N:=TVIDEMenu.Selected;
  If Assigned(N) and Assigned(N.Data) then
    With TIDEMenuItem(N.Data) do
      LPath.Text:=SSelectedPath+GetPath;
end;

procedure TExploreIDEMenuForm.AddMenuItem(ParentNode : TTreeNode;
  Item : TIDEMenuItem);

Var
  N : TTreeNode;
  I : Integer;
  Sec : TIDEMenuSection;
  
begin
  With Item do
    begin
    N:=TVIDEMenu.Items.AddChild(ParentNode,Format('%s (%s)',[Caption,Name]));
    N.Data:=Item;
    end;
  if Item is TIDEMenuSection then
    begin
    Sec:=(Item as TIDEMenuSection);
    For I:=0 to Sec.Count-1 do
      AddMenuItem(N,Sec.Items[I]);
    end;
end;

initialization
  {$I frmexploremenu.lrs}

end.

