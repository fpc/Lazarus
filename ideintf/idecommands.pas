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
  Classes, SysUtils; 
  
type
  TCommandArea = (caSourceEditor, caDesigner);
  TCommandAreas = set of TCommandArea;

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


implementation

{ TIDECommandCategory }

procedure TIDECommandCategory.Delete(Index: Integer);
begin
  inherited Delete(Index);
end;

end.

