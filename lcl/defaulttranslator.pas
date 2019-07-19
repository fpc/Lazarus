unit DefaultTranslator;

{
If you need standard translation, just use this unit in your project and enable
i18n in project options. It will translate your project automatically.

If you want to set translation language yourself, use LCLTranslator unit instead
and call SetDefaultLang in your program manually.
}
{$mode objfpc}{$H+}

interface

uses
  LCLTranslator;

implementation

initialization
  //It is safe to place code here as no form is initialized before unit
  //initialization is made
  SetDefaultLang('', '', '', false);

end.
