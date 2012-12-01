library androidlcltest;

{$mode objfpc}{$H+}

uses
  customdrawnint, Interfaces, Forms, mainform, secondform, sqlitejniandroid,
  customdrawn_android, customdrawndrawers, sqliteform;

exports
  Java_com_pascal_lclproject_LCLActivity_LCLOnTouch name 'Java_com_pascal_lcltest_LCLActivity_LCLOnTouch',
  Java_com_pascal_lclproject_LCLActivity_LCLDrawToBitmap name 'Java_com_pascal_lcltest_LCLActivity_LCLDrawToBitmap',
  Java_com_pascal_lclproject_LCLActivity_LCLOnCreate name 'Java_com_pascal_lcltest_LCLActivity_LCLOnCreate',
  Java_com_pascal_lclproject_LCLActivity_LCLOnMessageBoxFinished name 'Java_com_pascal_lcltest_LCLActivity_LCLOnMessageBoxFinished',
  Java_com_pascal_lclproject_LCLActivity_LCLOnKey name 'Java_com_pascal_lcltest_LCLActivity_LCLOnKey',
  Java_com_pascal_lclproject_LCLActivity_LCLOnTimer name 'Java_com_pascal_lcltest_LCLActivity_LCLOnTimer',
  Java_com_pascal_lclproject_LCLActivity_LCLOnConfigurationChanged name 'Java_com_pascal_lcltest_LCLActivity_LCLOnConfigurationChanged',
  Java_com_pascal_lclproject_LCLActivity_LCLOnSensorChanged name 'Java_com_pascal_lcltest_LCLActivity_LCLOnSensorChanged',
  Java_com_pascal_lclproject_LCLActivity_LCLOnMenuAction name 'Java_com_pascal_lcltest_LCLActivity_LCLOnMenuAction',
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

