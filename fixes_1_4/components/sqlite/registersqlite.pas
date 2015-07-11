{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit registersqlite;

{$Mode ObjFpc}
{$H+}

interface

uses 
  Classes, SysUtils, LResources, LazarusPackageIntf, PropEdits,
  ComponentEditors, sqliteds, SqliteComponentEditor;
  
procedure Register;

implementation

{$R sqliteicon.res}

procedure RegisterUnitSqliteds;
begin
  RegisterComponents('Data Access',[TSqliteDataset]);
end;  

procedure Register;

begin
  RegisterUnit('sqliteds',@RegisterUnitSqliteds);
  RegisterComponentEditor(TSqliteDataset,TSqliteEditor) ;
  RegisterPropertyEditor(TypeInfo(String),TSqliteDataset,'FileName',
                         TFileNamePropertyEditor);
end; 

end.
