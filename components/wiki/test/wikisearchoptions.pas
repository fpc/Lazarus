{ Search options offline wiki

  Copyright (C) 2012  Mattias Gaertner  mattias@freepascal.org

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.

}
unit WikiSearchOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, WikiHelpManager, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls;

type

  { TWikiSearchOptsWnd }

  TWikiSearchOptsWnd = class(TForm)
    LanguagesGroupBox: TGroupBox;
    LanguagesSplitter: TSplitter;
    LanguagesTreeView: TTreeView;
    procedure FormCreate(Sender: TObject);
  private
    FLanguages: string;
    procedure SetLanguages(AValue: string);
  public
    property Languages: string read FLanguages write SetLanguages;
    procedure UpdateAvailableLanguages;
    procedure UpdateSelectedLanguages;
  end;

var
  WikiSearchOptsWnd: TWikiSearchOptsWnd = nil;

implementation

{$R *.lfm}

{ TWikiSearchOptsWnd }

procedure TWikiSearchOptsWnd.FormCreate(Sender: TObject);
begin
  Caption:='Wiki Search Options';
  LanguagesGroupBox.Caption:='Languages';
end;

procedure TWikiSearchOptsWnd.SetLanguages(AValue: string);
begin
  if FLanguages=AValue then Exit;
  FLanguages:=AValue;
end;

procedure TWikiSearchOptsWnd.UpdateAvailableLanguages;
var
  Langs: TStrings;
  i: Integer;
  TVNode: TTreeNode;
begin
  Langs:=WikiHelp.CollectAllLanguages(true);
  LanguagesTreeView.BeginUpdate;
  try
    for i:=0 to Langs.Count-1 do begin
      if i<LanguagesTreeView.Items.TopLvlCount then begin
        TVNode:=LanguagesTreeView.Items.TopLvlItems[i];
        TVNode.Text:=Langs[i];
      end else begin
        TVNode:=LanguagesTreeView.Items.Add(nil,Langs[i]);
      end;
    end;
    while LanguagesTreeView.Items.TopLvlCount>Langs.Count do
      LanguagesTreeView.Items.TopLvlItems[LanguagesTreeView.Items.TopLvlCount-1].Delete;
  finally
    LanguagesTreeView.EndUpdate;
    Langs.Free;
  end;
  UpdateSelectedLanguages;
end;

procedure TWikiSearchOptsWnd.UpdateSelectedLanguages;
begin

end;

end.

