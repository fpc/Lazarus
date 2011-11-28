library androidlcltest;

{$mode objfpc}{$H+}
{$define Android}

uses
  {$ifdef Android}
  cmem,
  {$ifdef CD_Android_NativeApp}
  android_native_app_glue,
  {$else}
  customdrawnint,
  {$endif}
  {$endif}
  Interfaces,
  Forms,
  mainform;

{$ifdef CD_Android_NativeApp}
exports //android_main name 'android_main',
  ANativeActivity_onCreate name 'ANativeActivity_onCreate';
{$else}
exports
  Java_com_pascal_lclproject_LCLActivity_stringFromJNI name 'Java_com_pascal_lcltest_LCLActivity_stringFromJNI',
  Java_com_pascal_lclproject_LCLActivity_intFromJNI name 'Java_com_pascal_lcltest_LCLActivity_intFromJNI',
  Java_com_pascal_lclproject_LCLActivity_LCLDrawToBitmap name 'Java_com_pascal_lcltest_LCLActivity_LCLDrawToBitmap',
  JNI_OnLoad name 'JNI_OnLoad',
  JNI_OnUnload name 'JNI_OnUnload';
{$endif}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

