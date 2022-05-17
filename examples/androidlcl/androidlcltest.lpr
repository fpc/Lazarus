library androidlcltest;

{$mode objfpc}{$H+}

uses
  customdrawnint, Interfaces, Forms, mainform, secondform, sqlitejniandroid,
  customdrawn_android, customdrawndrawers, sqliteform;

exports
  JNI_OnLoad name 'JNI_OnLoad',
  JNI_OnUnload name 'JNI_OnUnload';

procedure MyActivityOnCreate;
begin
  DefaultStyle := dsAndroid;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TFormSqlite, formSqlite);
  Application.Run;
end;

begin
  CDWidgetset.ActivityClassName := 'com/pascal/lcltest/LCLActivity';
  CDWidgetset.ActivityOnCreate := @MyActivityOnCreate;
end.

