//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************



#include "pascalbind.h"

//======================  
// WideString 
//======================  

CopyUnicodeToPWideString copyUnicodeToPWideString;
UnicodeOfPWideString unicodeOfPWideString;
LengthOfPWideString lengthOfPWideString;
InitializePWideString initPWideString;
FinalizePWideString finalPWideString;


void initPWideStrings(CopyUnicodeToPWideString cutps, UnicodeOfPWideString uops,
  LengthOfPWideString lops, InitializePWideString ip, FinalizePWideString fp)
{
  copyUnicodeToPWideString = cutps;
  unicodeOfPWideString = uops;
  lengthOfPWideString = lops;
  initPWideString = ip;
  finalPWideString = fp;
}


//=========================
// QList<T> vs PtrIntArray
//=========================  

GetPtrIntArrayAddr getPtrIntArrayAddr;
GetPtrIntArrayLength getPtrIntArrayLength;
SetPtrIntArrayLength setPtrIntArrayLength;



void initializePPtrIntArray(GetPtrIntArrayAddr gaa, GetPtrIntArrayLength gal, SetPtrIntArrayLength sal)
{
  getPtrIntArrayAddr = gaa;
  getPtrIntArrayLength = gal;
  setPtrIntArrayLength = sal;
}


//===============================
// QVector<qreal> vs TQRealArray
//===============================  

GetQRealArrayAddr getQRealArrayAddr;
GetQRealArrayLength getQRealArrayLength;
SetQRealArrayLength setQRealArrayLength;

void initializeQRealArray(GetQRealArrayAddr gaa, GetQRealArrayLength gal, SetQRealArrayLength sal)
{
  getQRealArrayAddr = gaa;
  getQRealArrayLength = gal;
  setQRealArrayLength = sal;
}

