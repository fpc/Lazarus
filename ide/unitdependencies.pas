{
/***************************************************************************
                             unitdependencies.pas
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
 
 
  Author: Mattias Gaertner
 
  Abstract:
    Defines the TUnitDependenciesView form.
 
}
unit UnitDependencies;

{$mode objfpc}{$H+}

interface

{$I ide.inc}

uses
  {$IFDEF IDE_MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils, Forms, Dialogs, Buttons, ComCtrls, StdCtrls,
  CodeToolManager, EnvironmentOpts, LResources, IDEOptionDefs,
  LazarusIDEStrConsts, InputHistory;
  
type

  { TUnitNode }

  TUnitNode = class
  private
    FFilename: string;
    FFirstChild: TUnitNode;
    FLastChild: TUnitNode;
    FNextSibling: TUnitNode;
    FParent: TUnitNode;
    FPrevSibling: TUnitNode;
    FShortFilename: string;
    FTreeNode: TTreeNode;
    procedure SetFilename(const AValue: string);
    procedure SetShortFilename(const AValue: string);
    procedure SetTreeNode(const AValue: TTreeNode);
    procedure CreateShortFilename;
  public
    constructor Create;
    destructor Destroy; override;
    procedure CreateChilds;
    property Filename: string read FFilename write SetFilename;
    property FirstChild: TUnitNode read FFirstChild;
    property LastChild: TUnitNode read FLastChild;
    property NextSibling: TUnitNode read FNextSibling;
    property PrevSibling: TUnitNode read FPrevSibling;
    property Parent: TUnitNode read FParent;
    property ShortFilename: string read FShortFilename write SetShortFilename;
    property TreeNode: TTreeNode read FTreeNode write SetTreeNode;
  end;


  { TUnitDependenciesView }

  TUnitDependenciesView = class(TForm)
    UnitHistoryList: TComboBox;
    SelectUnitButton: TBitBtn;
    UnitTreeView: TTreeView;
    RefreshButton: TBitBtn;
    procedure UnitDependenciesViewResize(Sender: TObject);
  private
    FRootFilename: string;
    FRootShortFilename: string;
    FRootValid: boolean;
    FRootNode: TUnitNode;
    procedure DoResize;
    procedure ClearTree;
    procedure RebuildTree;
    procedure SetRootFilename(const AValue: string);
    procedure SetRootShortFilename(const AValue: string);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function RootValid: boolean;
    procedure UpdateUnitTree;
    property RootFilename: string read FRootFilename write SetRootFilename;
    property RootShortFilename: string read FRootShortFilename write SetRootShortFilename;
  end;
  
const
  UnitDependenciesView: TUnitDependenciesView = nil;

implementation

{ TUnitDependenciesView }

procedure TUnitDependenciesView.UnitDependenciesViewResize(Sender: TObject);
begin
  DoResize;
end;

procedure TUnitDependenciesView.DoResize;
begin
  with UnitHistoryList do begin
    SetBounds(0,0,Parent.ClientWidth-Left,Height);
  end;

  with SelectUnitButton do begin
    SetBounds(0,UnitHistoryList.Top+UnitHistoryList.Height+2,25,Height);
  end;

  with RefreshButton do begin
    SetBounds(SelectUnitButton.Left+SelectUnitButton.Width+5,
              SelectUnitButton.Top,100,SelectUnitButton.Height);
  end;

  with UnitTreeView do begin
    SetBounds(0,SelectUnitButton.Top+SelectUnitButton.Height+2,
              Parent.ClientWidth,Parent.ClientHeight-Top);
  end;
end;

procedure TUnitDependenciesView.ClearTree;
begin
  FRootNode.Free;
  FRootNode:=nil;
end;

procedure TUnitDependenciesView.RebuildTree;
begin
  ClearTree;
  if RootFilename='' then exit;
  FRootNode:=TUnitNode.Create;
  FRootNode.Filename:=RootFilename;
  FRootNode.ShortFilename:=FRootShortFilename;
  UnitTreeView.Items.Clear;
  FRootNode.TreeNode:=UnitTreeView.Items.Add(nil,'');
  FRootNode.CreateChilds;
end;

procedure TUnitDependenciesView.SetRootFilename(const AValue: string);
begin
  if FRootFilename=AValue then exit;
  FRootFilename:=AValue;
  FRootShortFilename:=FRootFilename;
  RebuildTree;
  UpdateUnitTree;
end;

procedure TUnitDependenciesView.SetRootShortFilename(const AValue: string);
begin
  if FRootShortFilename=AValue then exit;
  FRootShortFilename:=AValue;
  if FRootNode<>nil then
    FRootNode.ShortFilename:=AValue;
end;

function TUnitDependenciesView.RootValid: boolean;
begin
  Result:=FRootValid;
end;

procedure TUnitDependenciesView.UpdateUnitTree;
begin

end;

constructor TUnitDependenciesView.Create(TheOwner: TComponent);
var
  ALayout: TIDEWindowLayout;
begin
  inherited Create(TheOwner);
  if LazarusResources.Find(ClassName)=nil then begin
    Name:=DefaultUnitDependenciesName;
    Caption := 'Unit Dependencies';
    ALayout:=EnvironmentOptions.IDEWindowLayoutList.ItemByFormID(Name);
    ALayout.Form:=TForm(Self);
    ALayout.Apply;
    
    UnitHistoryList:=TComboBox.Create(Self);
    with UnitHistoryList do begin
      Name:='UnitHistoryList';
      Parent:=Self;
      Left:=0;
      Top:=0;
      Width:=Parent.ClientWidth-Left;
      Visible:=true;
    end;
    
    SelectUnitButton:=TBitBtn.Create(Self);
    with SelectUnitButton do begin
      Name:='SelectUnitButton';
      Parent:=Self;
      Left:=0;
      Top:=UnitHistoryList.Top+UnitHistoryList.Height+2;
      Width:=25;
      Caption:='...';
      Visible:=true;
    end;
    
    RefreshButton:=TBitBtn.Create(Self);
    with RefreshButton do begin
      Name:='RefreshButton';
      Parent:=Self;
      Left:=SelectUnitButton.Left+SelectUnitButton.Width+5;
      Top:=SelectUnitButton.Top;
      Width:=100;
      Height:=SelectUnitButton.Height;
      Caption:='Refresh';
      Visible:=true;
    end;

    UnitTreeView:=TTreeView.Create(Self);
    with UnitTreeView do begin
      Name:='UnitTreeView';
      Parent:=Self;
      Left:=0;
      Top:=SelectUnitButton.Top+SelectUnitButton.Height+2;
      Width:=Parent.ClientWidth;
      Height:=Parent.ClientHeight-Top;
      Visible:=true;
    end;
    
    OnResize:=@UnitDependenciesViewResize;
  end;
end;

destructor TUnitDependenciesView.Destroy;
begin
  ClearTree;
  inherited Destroy;
end;

{ TUnitNode }

procedure TUnitNode.SetFilename(const AValue: string);
begin
  if FFilename=AValue then exit;
  FFilename:=AValue;
  CreateShortFilename;
end;

procedure TUnitNode.SetShortFilename(const AValue: string);
begin
  if ShortFilename=AValue then exit;
  FShortFilename:=AValue;
  if TreeNode<>nil then
    TreeNode.Text:=FShortFilename;
end;

procedure TUnitNode.SetTreeNode(const AValue: TTreeNode);
begin
  if FTreeNode=AValue then exit;
  FTreeNode:=AValue;
  if FTreeNode<>nil then begin
    FTreeNode.Text:=ShortFilename;

  end;
end;

procedure TUnitNode.CreateShortFilename;
begin
  ShortFilename:=Filename;
end;

constructor TUnitNode.Create;
begin

end;

destructor TUnitNode.Destroy;
begin
  inherited Destroy;
end;

procedure TUnitNode.CreateChilds;
//var
//  UsedInterfaceFilenames, UsedImplementation: TStrings;
begin

end;

end.

