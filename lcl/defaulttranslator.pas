unit DefaultTranslator;

{ Copyright (C) 2015 Lazarus Developers Team

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}
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
