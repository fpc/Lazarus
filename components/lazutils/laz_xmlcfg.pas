{
 **********************************************************************
  This file is part of LazUtils.
  It is copied from Free Component Library and was adapted to use
  UTF8 strings instead of widestrings.

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 **********************************************************************

  Implementation of TXMLConfig class
  Copyright (c) 1999 - 2001 by Sebastian Guenther, sg@freepascal.org

  TXMLConfig enables applications to use XML files for storing their
  configuration data
}

{$MODE objfpc}
{$H+}

unit Laz_XMLCfg;

interface

uses
  Classes, sysutils, Laz2_XMLCfg;

type
  TXMLConfig = Laz2_XMLCfg.TXMLConfig;
  TRttiXMLConfig = Laz2_XMLCfg.TRttiXMLConfig;

implementation

end.
