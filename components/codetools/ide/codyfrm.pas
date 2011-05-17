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
    An IDE window for context sensitive codetools.
}
unit CodyFrm;

{$mode objfpc}{$H+}

{$R *.lfm}

interface

uses
  Classes, SysUtils, FileProcs, LResources, Forms, Controls, Graphics, Dialogs,
  Buttons, ComCtrls,
  // codetools
  //FileProcs, CodeToolManager, SourceLog, CodeCache, EventCodeTool,
  //LinkScanner, PascalParserTool, CodeTree,
  // IDEIntf
  LazIDEIntf, IDEWindowIntf,
  // cody
  CodyStrConsts;

const
  CodyWindowName = 'CodyWindow';
type

  { TCodyWindow }

  TCodyWindow = class(TForm)
    ImageList1: TImageList;
    ToolBar1: TToolBar;
    RefreshToolButton: TToolButton;
    OptionsToolButton: TToolButton;
    TreeView1: TTreeView;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    ImgIDRefresh: Integer;
    ImgIDOptions: Integer;
  public
  end;

var
  CodyWindow: TCodyWindow = nil;
  CodyWindowCreator: TIDEWindowCreator; // set by CodyRegistration.Register

procedure ShowCodyWindow(Sender: TObject);
procedure CreateCodyWindow(Sender: TObject; aFormName: string;
                          var AForm: TCustomForm; DoDisableAutoSizing: boolean);

implementation

procedure ShowCodyWindow(Sender: TObject);
begin
  IDEWindowCreators.ShowForm(CodyWindowCreator.FormName,true);
end;

procedure CreateCodyWindow(Sender: TObject; aFormName: string;
  var AForm: TCustomForm; DoDisableAutoSizing: boolean);
begin
  if CompareText(aFormName,CodyWindowName)<>0 then begin
    debugln(['ERROR: CreateCodyWindow: there is already a form with this name']);
    exit;
  end;
  IDEWindowCreators.CreateForm(AForm,TCodyWindow,DoDisableAutoSizing,
    LazarusIDE.OwningComponent);
  AForm.Name:=aFormName;
end;

{ TCodyWindow }

procedure TCodyWindow.FormCreate(Sender: TObject);
begin
  if CodyWindow=nil then CodyWindow:=Self;
  Caption:='Cody';

  ImgIDRefresh := Imagelist1.AddLazarusResource('laz_refresh');
  ImgIDOptions := Imagelist1.AddLazarusResource('menu_environment_options');
  ToolBar1.Images:=ImageList1;
  OptionsToolButton.Hint:=crsOptions;
  OptionsToolButton.ImageIndex:=ImgIDOptions;
  RefreshToolButton.Hint:=crsRefresh;
  RefreshToolButton.ImageIndex:=ImgIDRefresh;
end;

procedure TCodyWindow.FormDestroy(Sender: TObject);
begin

  if CodyWindow=Self then CodyWindow:=nil;
end;


end.

