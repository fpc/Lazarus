Lazarus Packages for the FreePascal MySQL units:
MySQL3Laz, MySQL4Laz

FreePascal supports MySQL versions 3.2.2, 3.2.3 and 4.0.
Because the MySQL interface differs too much, there are separate units.
The package mysql3laz.lpk is for version 3.2.2 and 3.2.3.
The package mysql4laz.lpk is for version 4.0.
You have to choose one of them. Trying to use both into an application will give
linking errors.
The packages defines a macro MySQL3 respectively MySQL4, so you can write the
following in your code:

{$IFDEF MySQL3}
// do something MySQL3 specific
{$ENDIF}
{$IFDEF MySQL4}
// do something MySQL4 specific
{$ENDIF}



