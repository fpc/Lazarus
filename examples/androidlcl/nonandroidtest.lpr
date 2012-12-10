program nonandroidtest;

{$mode objfpc}{$H+}
{.$define TEST_SQLITE}

uses
  Interfaces, // this includes the LCL widgetset
  Forms, mainform, secondform,
  {$ifdef TEST_SQLITE}sqliteform,{$ENDIF}
  customdrawndrawers,
  customdrawn_android;

{$R *.res}

begin
  DefaultStyle := dsAndroid;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  {$ifdef TEST_SQLITE}Application.CreateForm(TformSqlite, formSqlite);{$ENDIF}
  Application.Run;
end.

