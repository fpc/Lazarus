{
 /***************************************************************************
                            codeexplorer.pas
                            ----------------

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
    TCodeExplorerView is the form of the IDE 'Code Explorer'.
}
unit CodeExplorer;

{$mode objfpc}{$H+}

interface

{$I ide.inc}

uses
  {$IFDEF IDE_MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils, Controls, Forms, Dialogs, Buttons, ComCtrls, StdCtrls,
  CodeToolManager, CodeCache, EnvironmentOpts, LResources, IDEOptionDefs,
  LazarusIDEStrConsts, InputHistory, IDEProcs, Graphics, LCLType;

type
  TCodeExplorerView = class(TForm)
    NodeTypeImgList: TImageList;
    NodeTreeView: TTreeView;
    RefreshButton: TBitBtn;
    procedure CodeExplorerViewResize(Sender: TObject);
    procedure NodeTreeViewAdvancedCustomDrawItem(Sender: TCustomTreeView;
          Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
          var PaintImages, DefaultDraw: Boolean);
    procedure NodeTreeViewCollapsing(Sender: TObject; Node: TTreeNode;
          var AllowCollapse: Boolean);
    procedure NodeTreeViewExpanding(Sender: TObject; Node: TTreeNode;
          var AllowExpansion: Boolean);
    procedure RefreshButtonClick(Sender: TObject);
  private
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoRefresh;
  end;
  
var
  CodeExplorerView: TCodeExplorerView;


implementation


{ TCodeExplorerView }

procedure TCodeExplorerView.CodeExplorerViewResize(Sender: TObject);
begin
  with RefreshButton do begin
    SetBounds(0,0,70,Height);
  end;

  with NodeTreeView do begin
    SetBounds(0,RefreshButton.Top+RefreshButton.Height,
              Self.ClientWidth,Self.ClientHeight-Top);
  end;
end;

procedure TCodeExplorerView.NodeTreeViewAdvancedCustomDrawItem(
  Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState;
  Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
begin

end;

procedure TCodeExplorerView.NodeTreeViewCollapsing(Sender: TObject;
  Node: TTreeNode; var AllowCollapse: Boolean);
begin

end;

procedure TCodeExplorerView.NodeTreeViewExpanding(Sender: TObject;
  Node: TTreeNode; var AllowExpansion: Boolean);
begin

end;

procedure TCodeExplorerView.RefreshButtonClick(Sender: TObject);
begin
  DoRefresh;
end;

constructor TCodeExplorerView.Create(TheOwner: TComponent);

  procedure AddResImg(ImgList: TImageList; const ResName: string);
  var Pixmap: TPixmap;
  begin
    Pixmap:=TPixmap.Create;
    if LazarusResources.Find(ResName)=nil then
      writeln('TCodeExplorerView.Create: ',
        ' WARNING: icon not found: "',ResName,'"');
    Pixmap.LoadFromLazarusResource(ResName);
    ImgList.Add(Pixmap,nil)
  end;

var
  ALayout: TIDEWindowLayout;
begin
  inherited Create(TheOwner);
  if LazarusResources.Find(ClassName)=nil then begin
    Name:=DefaultCodeExplorerName;
    Caption := lisCodeExplorer;
    ALayout:=EnvironmentOptions.IDEWindowLayoutList.ItemByFormID(Name);
    ALayout.Form:=TForm(Self);
    ALayout.Apply;
    
    NodeTypeImgList:=TImageList.Create(Self);
    with NodeTypeImgList do begin
      Name:='NodeTypeImgList';
      Width:=22;
      Height:=22;
      AddResImg(SrcTypeImageList,'nodetype_unknown_22x22');            // 0
    end;
    
    RefreshButton:=TBitBtn.Create(Self);
    with RefreshButton do begin
      Name:='RefreshButton';
      Parent:=Self;
      Left:=0;
      Top:=0;
      Width:=70;
      Caption:='Refresh';
      OnClick:=@RefreshButtonClick;
    end;

    NodeTreeView:=TTreeView.Create(Self);
    with NodeTreeView do begin
      Name:='NodeTreeView';
      Parent:=Self;
      Left:=0;
      Top:=RefreshButton.Top+RefreshButton.Height;
      Width:=Self.ClientWidth;
      Height:=Self.ClientHeight-Top;
      OnExpanding:=@NodeTreeViewExpanding;
      OnCollapsing:=@NodeTreeViewCollapsing;
      Images:=NodeTypeImageList;
      OnAdvancedCustomDrawItem:=@NodeTreeViewAdvancedCustomDrawItem;
    end;
    
    OnResize:=@CodeExplorerViewResize;
  end;
end;

destructor TCodeExplorerView.Destroy;
begin
  inherited Destroy;
end;

procedure TCodeExplorerView.DoRefresh;
begin

end;

initialization
  CodeExplorerView:=nil;
  

end.

