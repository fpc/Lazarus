
{$IFDEF FPC}
  // FreePascal must be in Delphi mode
  {$MODE delphi}
{$ELSE}
  {$DEFINE USEJCL}

  // define the appropriate DelphiXX constants

  {$IFDEF VER210}
     {$IFDEF BCB}
       {$DEFINE BCB14}
     {$ELSE}
       {$DEFINE DELPHI14}
     {$ENDIF}
  {$ENDIF}

  {$IFDEF VER200}
     {$IFDEF BCB}
       {$DEFINE BCB12}
     {$ELSE}
       {$DEFINE DELPHI12}
     {$ENDIF}
  {$ENDIF}

  {$IFDEF VER190}
     {$DEFINE DELPHI11}
  {$ENDIF}

  {$IFDEF VER180}
    {$IFDEF VER185}
      {$IFDEF BCB}
        {$DEFINE BCB11}
      {$ELSE}
        {$DEFINE DELPHI11}
      {$ENDIF}
    {$ELSE}
      {$IFDEF BCB}
        {$DEFINE BCB10}
      {$ELSE}
        {$DEFINE DELPHI10}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}

  {$IFDEF VER170}
    {$DEFINE DELPHI9}
  {$ENDIF}

  {$IFDEF VER160}
    {$DEFINE DELPHI8}
  {$ENDIF}

  {$IFDEF VER150}
    {$DEFINE DELPHI7}
  {$ENDIF}

  {$IFDEF VER140}
    {$IFDEF BCB}
      {$DEFINE BCB6}
    {$ELSE}
      {$DEFINE DELPHI6}
    {$ENDIF}
  {$ENDIF}

  {$IFDEF VER130}
    {$IFDEF BCB}
      {$DEFINE BCB5}
    {$ELSE}
      {$DEFINE DELPHI5}
    {$ENDIF}
  {$ENDIF}

  {$IFDEF VER125}
    {$DEFINE BCB4}
  {$ENDIF}

  {$IFDEF VER120}
    {$DEFINE DELPHI4}
  {$ENDIF}

  {$IFDEF VER110}
    {$DEFINE BCB3}
  {$ENDIF}

  {$IFDEF VER100}
    {$DEFINE DELPHI3}
  {$ENDIF}

  {$IFDEF VER93}
    {$DEFINE BCB1}
  {$ENDIF}

  {$IFDEF VER90}
    {$DEFINE DELPHI2}
  {$ENDIF}

  {$IFDEF VER80}
    {$DEFINE DELPHI1}
  {$ENDIF}
{$ENDIF}
