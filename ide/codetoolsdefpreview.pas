{ /***************************************************************************
                 codetoolsdefpreview.pas  -  Lazarus IDE unit
                 --------------------------------------------

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
    - TCodeToolsDefinesPreview is a preview for the defines of a single
      directory, used by TCodeToolsDefinesEditor.

}
unit CodeToolsDefPreview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLLinux, Forms, Controls, Buttons, StdCtrls, ComCtrls,
  ExtCtrls, Menus, LResources, Graphics, Dialogs, ImgList, SynEdit, ExprEval,
  DefineTemplates;

type
  TCodeToolsDefinesPreview = class(TForm)
    DirectoryLabel: TLabel;
    DirectoryEdit: TEdit;
    DefListBox: TListBox;
  
    // misc
    procedure FormResize(Sender: TObject);
  private
    FDefineTree: TDefineTree;
    procedure CreateComponents;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure ShowDefines;
    property DefineTree: TDefineTree read FDefineTree write FDefineTree;
  end;

implementation


{ TCodeToolsDefinesPreview }

procedure TCodeToolsDefinesPreview.FormResize(Sender: TObject);
var MaxX, MaxY: integer;
begin
  MaxX:=ClientWidth-2;
  MaxY:=ClientHeight-2;

  DirectoryLabel.SetBounds(5,3,150,DirectoryLabel.Height);
  
  with DirectoryEdit do begin
    Left:=0;
    Top:=DirectoryLabel.Top+DirectoryLabel.Height+2;
    Width:=MaxX;
  end;
  
  with DefListBox do begin
    Left:=0;
    Top:=DirectoryEdit.Top+DirectoryEdit.Height+3;
    Width:=MaxX;
    Height:=MaxY-Top;
  end;
end;

procedure TCodeToolsDefinesPreview.CreateComponents;
begin
  DirectoryLabel:=TLabel.Create(Self);
  with DirectoryLabel do begin
    Name:='DirectoryLabel';
    Parent:=Self;
    Visible:=true;
  end;
  
  DirectoryEdit:=TEdit.Create(Self);
  with DirectoryEdit do begin
    Name:='DirectoryEdit';
    Parent:=Self;
    Visible:=true;
  end;
  
  DefListBox:=TListBox.Create(Self);
  with DefListBox do begin
    Name:='DefListBox';
    Parent:=Self;
    Visible:=true;
  end;
end;

constructor TCodeToolsDefinesPreview.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  if LazarusResources.Find(ClassName)=nil then begin
    SetBounds((Screen.Width-400) div 2,(Screen.Height-400) div 2, 420, 420);
    Caption:='CodeTools Defines Preview';
    OnResize:=@FormResize;

    CreateComponents;
  end;
  Resize;
end;

destructor TCodeToolsDefinesPreview.Destroy;
begin

  inherited Destroy;
end;

procedure TCodeToolsDefinesPreview.ShowDefines;
var
  ExprEval: TExpressionEvaluator;
  i: integer;
begin
  DefListBox.Items.BeginUpdate;
  if DefineTree<>nil then begin
    DefineTree.ClearCache;
    ExprEval:=DefineTree.GetDefinesForDirectory(DirectoryLabel.Text,false);
    if ExprEval<>nil then begin
      for i:=0 to ExprEval.Count-1 do begin
        if i<DefListBox.Items.Count then
          // replace old value
          DefListBox.Items[i]:=ExprEval.Items(i)
        else
          // add value
          DefListBox.Items.Add(ExprEval.Items(i));
      end;
      while DefListBox.Items.Count>ExprEval.Count do
        // delete old value
        DefListBox.Items.Delete(DefListBox.Items.Count-1);
    end else
      DefListBox.Items.Clear;
  end;
  DefListBox.Items.EndUpdate;
end;

end.

