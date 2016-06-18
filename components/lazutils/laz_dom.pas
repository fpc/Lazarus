{
 **********************************************************************
  This file is part of LazUtils.
  It is copied from Free Component Library.

  See the file COPYING.FPC, included in this distribution,
  for details about the license.
 **********************************************************************

}

unit Laz_DOM;

{$MODE objfpc}{$H+}

interface

uses
  SysUtils, Classes, laz2_DOM;

type
  TDOMImplementation = laz2_DOM.TDOMImplementation;
  TDOMDocumentFragment = laz2_DOM.TDOMDocumentFragment;
  TDOMDocument = laz2_DOM.TDOMDocument;
  TDOMNode = laz2_DOM.TDOMNode;
  TDOMNodeList = laz2_DOM.TDOMNodeList;
  TDOMNamedNodeMap = laz2_DOM.TDOMNamedNodeMap;
  TDOMCharacterData = laz2_DOM.TDOMCharacterData;
  TDOMAttr = laz2_DOM.TDOMAttr;
  TDOMElement = laz2_DOM.TDOMElement;
  TDOMText = laz2_DOM.TDOMText;
  TDOMComment = laz2_DOM.TDOMComment;
  TDOMCDATASection = laz2_DOM.TDOMCDATASection;
  TDOMDocumentType = laz2_DOM.TDOMDocumentType;
  TDOMNotation = laz2_DOM.TDOMNotation;
  TDOMEntity = laz2_DOM.TDOMEntity;
  TDOMEntityReference = laz2_DOM.TDOMEntityReference;
  TDOMProcessingInstruction = laz2_DOM.TDOMProcessingInstruction;

  DOMString = laz2_DOM.DOMString;
  DOMPChar = laz2_DOM.DOMPChar;

  EDOMError = laz2_DOM.EDOMError;

implementation

end.


