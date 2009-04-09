{%MainUnit GtkInt.pp}

procedure ExtractFilterList(const Filter: string;
  out ListOfFileSelFilterEntry: TFPList; SplitMultiMask: boolean);
procedure FreeListOfFileSelFilterEntry(ListOfFileSelFilterEntry: TFPList);
