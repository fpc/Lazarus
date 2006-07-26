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

  Author: Mattias Gaertner

  Abstract:
    An editor for a list of TCustomTextConverterTool.
}
unit IDETextConvListEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  IDETextConverter, ExtCtrls, ObjectInspector, Buttons;

type

  { TTTextConvListEditor }

  TTTextConvListEditor = class(TForm)
    AddToolButton: TButton;
    CloneButton: TButton;
    PasteButton: TButton;
    CopyToolButton: TButton;
    MoveToolDownButton: TButton;
    MoveToolUpButton: TButton;
    DeleteToolButton: TButton;
    ToolsSplitter: TSplitter;
    ToolsPanel: TPanel;
    ToolsListBox: TListBox;
    UpDownSplitter: TSplitter;
    ToolsLabel: TLabel;
    PropertyGrid: TCustomPropertiesGrid;
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

var
  TTextConvListEditor: TTTextConvListEditor;

implementation

{ TTTextConvListEditor }

procedure TTTextConvListEditor.FormCreate(Sender: TObject);
begin
  Caption:='Text conversion tools editor';
  ToolsLabel.Caption:='Tools:';
  
  // buttons
  AddToolButton.Caption:='Add new tool';
  CloneButton.Caption:='Add a copy';
  PasteButton.Caption:='Add from clipboard';
  CopyToolButton.Caption:='Copy tool to clipboard';
  MoveToolDownButton.Caption:='Move down';
  MoveToolUpButton.Caption:='Move up';
  DeleteToolButton.Caption:='Delete tool';

  PropertyGrid:=TCustomPropertiesGrid.Create(Self);
  PropertyGrid.Align:=alBottom;
  PropertyGrid.AnchorToNeighbour(akTop,0,UpDownSplitter);
  PropertyGrid.Parent:=Self;
end;

initialization
  {$I idetextconvlistedit.lrs}

end.

