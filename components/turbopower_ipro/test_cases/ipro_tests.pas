unit ipro_tests;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

const
  LE = LineEnding;
  
//------------------------------------------------------------------------------
//   <BR> tag
//------------------------------------------------------------------------------
const
  BRinBODY_title = 
    '<BR> between two words in BODY';
  BRinBODY_html = 
    '<html>' + LE +
    '<body>' + LE +
    '  abc<br>def' + LE +
    '</body>' + LE +
    '</html>';
  BRinBODY_descr =
    'The two words should be in separate lines, having no additional empty line between.';

  TwoBRinBODY_title = 
    'Two <BR> tags between two words in BODY';
  TwoBRinBODY_html = 
    '<html>' + LE +
    '<body>' + LE +
    '  abc<br><br>def' + LE +
    '</body>' + LE +
    '</html>';
  TwoBRinBODY_descr =
    'The two words should be in separate lines with additional empty line between.';

  BRinP_title = 
    '<BR> between two words in P nodes';
  BRinP_html = 
    '<html>' + LE +
    '<body>' + LE +
    '  <p>abc<br>def</p>' + LE +
    '</body>' + LE +
    '</html>';
  BRinP_descr =
    'The two words should be in separate lines, having no additional empty line between.';
  
  TwoBRinP_title = 
    'Two <BR> tags between two words in P nodes';
  TwoBRinP_html = 
    '<html>' + LE +
    '<body>' + LE +
    '  <p>abc<br><br>def</p>' + LE +
    '</body>' + LE +
    '</html>';
  TwoBRinP_descr =
    'The two words should be in separate lines with an additional empty line between.';

  BRinTableCell_title = 
    '<BR> between two words in a table cell';
  BRinTableCell_html = 
    '<html>' + LE +
    '<body>' + LE +
    '  <table border="1" cellspacing="0">' + LE +
    '    <tr><td>abc<br>def</td><td>ghi</td></tr>' + LE +
    '  </table>' + LE +
    '</body>' + LE +
    '</html>';
  BRinTableCell_descr =
    'The two words in the left cell should be in separate lines, having no additional empty line between.';
  
  TwoBRinTableCell_title = 
    'Two <BR> tags between two words in a table cell';
  TwoBRinTableCell_html = 
    '<html>' + LE +
    '<body>' + LE +
    '  <table border="1" cellspacing="0">' + LE +
    '    <tr><td>abc<br><br>def</td><td>ghi</td></tr>' + LE +
    '  </table>' + LE +
    '</body>' + LE +
    '</html>';
  TwoBRinTableCell_descr =
    'The two words in the left cell should be in separate lines with an additional empty line between.';

  BRbetweenTwoTables_title = 
    '<BR> between two tables';
  BRbetweenTwoTables_html = 
    '<html>' + LE +
    '<body>' + LE +
    '  <table border="1" cellspacing="0">' + LE +
    '    <tr>' + LE + 
    '      <td>abc</td><td>def</td><td>ghi</td>' + LE +
    '    </tr>' + LE +
    '  </table>' + LE +
    '  <br>' + LE +
    '  <table border="1" cellspacing="0">' + LE +
    '    <tr>' + LE + 
    '      <td>ABC</td><td>DEF</td><td>GHI</td>' + LE +
    '    </tr>' + LE +
    '  </table>' + LE +
    '</body>' + LE +
    '</html>';
  BRbetweenTwoTables_descr =
    'There should be an empty line between the two tables.';
    
  BRbetweenTwoP_title = 
    '<BR> between two <p> tags';
  BRbetweenTwoP_html = 
    '<html>' + LE +
    '<body>' + LE +
    '  <p>abc</p>' + LE +
    '  <br>' + LE +
    '  <p>ABC</p>' + LE +
    '</body>' + LE +
    '</html>';
  BRbetweenTwoP_descr =
    'There should be an empty line between the two lines.';

  
//------------------------------------------------------------------------------
//   <PRE>
//------------------------------------------------------------------------------
const
  PRE_title =
    'Formatting with <PRE> tag';
  PRE_descr =
    'All lines should have normal spacing.';
  PRE_html =
    '<html>' + LE +
    '<body>' + LE +
    '<p>Normal text before.</p>' + LE +
    '<pre>' + LE +
    'program Test;' + LE +
    'begin' + LE + 
    '  Run;' + LE +
    'end.' + LE +
    '</pre>' + LE +
    '<p>Normal text after.</p>' + LE +
    '</body>' + LE +
    '</html>';
  
  
// -----------------------------------------------------------------------------
//   Background color
//------------------------------------------------------------------------------
const
  TextWithBackgroundInBODY_title =
    'Text with background in BODY';
  TextWithBackgroundInBODY_html =
    '<html>' + LE +
    '<body>' + LE +
    '  <font style="background-color: yellow;" color="red">Testing backcolor</font>' + LE +
    '</body>' + LE +
    '</html>';
  TextWithBackgroundInBODY_descr =
    'This test should show red text on yellow background.';

  
  TextWithBackgroundInBODY_CSS_title =
    'Text with background in BODY (with CSS)';
  TextWithBackgroundInBODY_CSS_html =
    '<html>' + LE +
    '<head>' + LE +
    '  <style type="text/css">' + LE +
    '    .class1 {' + LE +
    '      color: red;' + LE +
    '      background-color: yellow;' + LE +
    '    }' + LE +
    '  </style>' + LE +
    '</head>' + LE +
    '<body>' + LE +
    '  <p class="class1">Testing backcolor</p>' + LE +
    '</body>' + LE +
    '</html>';
  TextWithBackgroundInBODY_CSS_descr =
    'This test should show red text on yellow background.';

  TextInColoredTableCell_title =
    'Text in colored table cell';
  TextInColoredTableCell_html = 
    '<html>' + LE +
    '<body>' + LE + 
    '  <table border="1" cellspacing="0">' + LE +
    '    <tr bgcolor="yellow">' + LE +
    '      <td>abc</td>' + LE +
    '      <td>def</td>' + LE +
    '      <td bgcolor="red">Test</td>' + LE +
    '    </tr>'+ LE +
    '  </table>' + LE +
    '</body>' + LE +
    '</html>';
  TextInColoredTableCell_descr =
    'The left and center cells should have yellow, the right cell uniform red background.';

//------------------------------------------------------------------------------
//   Tables: Text Alignment
//------------------------------------------------------------------------------
const
  AlignInCell_title =
    'Left-/right-aligned and centered text in a table cell';
  AlignInCell_descr =
    'The text in the left, center and right cell should be left-aligned, centered, right-aligned, respectively.';
  AlignInCell_html =
    '<html>' + LE +
    '<body>' + LE + 
    '  <table border="1" cellspacing="0">' + LE +
    '    <tr>' + LE +
    '      <th>left</td>' + LE +
    '      <th>centered</td>' + LE +
    '      <th>right</td>' + LE +
    '    </tr>'+ LE +
    '    <tr>' + LE +
    '      <td align="left">a</td>' + LE +
    '      <td align="center">b</td>' + LE +
    '      <td align="right">c</td>' + LE +
    '    </tr>'+ LE +
    '  </table>' + LE +
    '</body>' + LE +
    '</html>';

  AlignInCellBold_title =
    'Left-/right-aligned and centered BOLD text in a table cell';
  AlignInCellBold_descr =
    'The text in the left, center and right cell should be left-aligned, centered, right-aligned, respectively.';
  AlignInCellBold_html =
    '<html>' + LE +
    '<body>' + LE + 
    '  <table border="1" cellspacing="0">' + LE +
    '    <tr>' + LE +
    '      <th>left</td>' + LE +
    '      <th>centered</td>' + LE +
    '      <th>right</td>' + LE +
    '    </tr>'+ LE +
    '    <tr>' + LE +
    '      <td align="left"><b>a</b></td>' + LE +
    '      <td align="center"><b>b</b></td>' + LE +
    '      <td align="right"><b>c</b></td>' + LE +
    '    </tr>'+ LE +
    '  </table>' + LE +
    '</body>' + LE +
    '</html>';

  AlignInCell_CSS_title =
    'Left-/right-aligned and centered text in a table cell, using body style';
  AlignInCell_CSS_descr =
    'The text in the left, center and right cell should be left-aligned, centered, right-aligned, respectively.';
  AlignInCell_CSS_html =
    '<html>' + LE +
    '<head>' + LE +
    '  <style type="text/css">' + LE +
    '    body { font-size:9pt; font-family:Arial, Helvetica, sans-serif }' + LE +
    '  </style>' + LE +
    '</head>' + LE +
    '<body>' + LE + 
    '  <table border="1" cellspacing="0" style="font-size:9pt; font-family:Arial, Helvetica, sans-serif;">' + LE +
    '    <tr>' + LE +
    '      <th>left</td>' + LE +
    '      <th>centered</td>' + LE +
    '      <th>right</td>' + LE +
    '    </tr>'+ LE +
    '    <tr>' + LE +
    '      <td align="left">a</td>' + LE +
    '      <td align="center">b</td>' + LE +
    '      <td align="right">c</td>' + LE +
    '    </tr>'+ LE +
    '  </table>' + LE +
    '</body>' + LE +
    '</html>';

  AlignInCellBold_CSS_title =
    'Left-/right-aligned and centered BOLD text in a table cell, using body style';
  AlignInCellBold_CSS_descr =
    'The text in the left, center and right cell should be left-aligned, centered, right-aligned, respectively.';
  AlignInCellBold_CSS_html =
    '<html>' + LE +
    '<head>' + LE +
    '  <style type="text/css">' + LE +
    '    body { font-size:9pt; font-family:Arial, Helvetica, sans-serif }' + LE +
    '  </style>' + LE +
    '</head>' + LE +
    '<body>' + LE + 
    '  <table border="1" cellspacing="0" style="font-size:9pt; font-family:Arial, Helvetica, sans-serif;">' + LE +
    '    <tr>' + LE +
    '      <th>left</td>' + LE +
    '      <th>centered</td>' + LE +
    '      <th>right</td>' + LE +
    '    </tr>'+ LE +
    '    <tr>' + LE +
    '      <td align="left"><b>a</b></td>' + LE +
    '      <td align="center"><b>b</b></td>' + LE +
    '      <td align="right"><b>c</b></td>' + LE +
    '    </tr>'+ LE +
    '  </table>' + LE +
    '</body>' + LE +
    '</html>';

  
//------------------------------------------------------------------------------
//    Tables: Merged cells
//------------------------------------------------------------------------------
const
  TableColSpan_title = 
    'Merged columns (colspan)';
  TableColSpan_descr =
    'The last row must contain a singe (merged) cell.';
  TableColSpan_html =
    '<html>' + LE +
    '<body>' + LE + 
    '  <table border="1">' + LE +
    '    <tr>' + LE +
    '      <th>Month</th>' + LE +
    '      <th>Savings</th>' + LE +
    '    </tr>' + LE +
    '    <tr>' + LE +
    '      <td>January</td>' + LE +
    '      <td>$100</td>' + LE +
    '    </tr>' + LE +
    '    <tr>' + LE +
    '      <td>February</td>' + LE +
    '      <td>$80</td>' + LE +
    '    </tr>' + LE +
    '    <tr>' + LE +
    '      <td colspan="2">Sum: $180</td>' + LE +
    '    </tr>' + LE +
    '  </table>' + LE +
    '</body>' + LE +
    '</html>';
  
  TableRowSpan_title =
    'Merged rows (rowspan)';
  TableRowSpan_descr =
    'The last column must contain a single (merged) cell.';
  TablerowSpan_html =
    '<html>' + LE +
    '<body>' + LE +
    '  <table border="1">' + LE +
    '    <tr>' + LE +
    '      <th>Month</th>' + LE +
    '      <th>Savings</th>' + LE +
    '      <th>Savings for holiday!</th>' + LE +
    '    </tr>' + LE +
    '    <tr>' + LE +
    '      <td>January</td>' + LE +
    '      <td>$100</td>' + LE +
    '      <td rowspan="2">$50</td>' + LE +
    '    </tr>' + LE +
    '    <tr>' + LE +
    '      <td>February</td>' + LE +
    '      <td>$80</td>' + LE +
    '    </tr>' + LE +
    '  </table>' + LE +
    '</body>' + LE +
    '</html>';
  
//------------------------------------------------------------------------------
//    Tables: background color
//------------------------------------------------------------------------------
const
  TableCellBkColor_title =
    'Cell background color';
  TableCellBkColor_descr =
    'The cell text must describe the cell background color.';
  TableCellBkColor_html =
    '<html>' + LE +
    '<body>' + LE +
    '<table border="1">' + LE +
    '  <tr>' + LE +
    '    <td bgcolor="red">red</td>' + LE +
    '    <td bgcolor="yellow">yellow</td>' + LE +
    '  </tr>' + LE +
    '  <tr>' + LE +
    '    <td bgcolor="green">green</td>' + LE +
    '    <td bgcolor="skyblue">skyblue</td>' + LE +
    '  </tr>' + LE +
    '</table>' + LE +
    '</body>' + LE +
    '</html>';
  
  TableCellBkColor_style_title =
    'Cell background color (inline style)';
  TableCellBkColor_style_descr =
    'The cell text must describe the cell background color.';
  TableCellBkColor_style_html =
    '<html>' + LE +
    '<body>' + LE +
    '<table border="1">' + LE +
    '  <tr>' + LE +
    '    <td style="background-color:red">red</td>' + LE +
    '    <td style="background-color:yellow">yellow</td>' + LE +
    '  </tr>' + LE +
    '  <tr>' + LE +
    '    <td style="background-color:green">green</td>' + LE +
    '    <td style="background-color:skyblue">skyblue</td>' + LE +
    '  </tr>' + LE +
    '</table>' + LE +
    '</body>' + LE +
    '</html>';

  TableRowBkColor_title =
    'Row background color';
  TableRowBkColor_descr =
    'The cells in the 1st and 2nd row must have a yellow and an orange background, respectively.';
  TableRowBkColor_html =
    '<html>' + LE +
    '<body>' + LE +
    '<table border="1">' + LE +
    '  <tr bgcolor="yellow">' + LE +
    '    <td>Cell A</td>' + LE +
    '    <td>Cell B</td>' + LE +
    '  </tr>' + LE +
    '  <tr bgcolor="orange">' + LE +
    '    <td>Cell C</td>' + LE +
    '    <td>Cell D</td>' + LE +
    '  </tr>' + LE +
    '</table>' + LE +
    '</body>' + LE +
    '</html>';
  
  TableRowBkColor_style_title =
    'Row background color (inline style)';
  TableRowBkColor_style_descr =
    'The cells in the 1st and 2nd row must have a yellow and an orange background, respectively.';
  TableRowBkColor_style_html =
    '<html>' + LE +
    '<body>' + LE +
    '<table border="1">' + LE +
    '  <tr style="background-color:yellow">' + LE +
    '    <td>Cell A</td>' + LE +
    '    <td>Cell B</td>' + LE +
    '  </tr>' + LE +
    '  <tr style="background-color:orange">' + LE +
    '    <td>Cell C</td>' + LE +
    '    <td>Cell D</td>' + LE +
    '  </tr>' + LE +
    '</table>' + LE +
    '</body>' + LE +
    '</html>';
  
  TableColBkColor_style_title =
    'Column background color';
  TableColBkColor_style_descr = 
    'The 1st column must have red, the second column a yellow background color.';
  TableColBkColor_style_html = 
    '<html>' + LE +
    '<body>' + LE +
    '<table border="1">' + LE +
    '  <colgroup>' + LE +
    '    <col style="background-color:red">' + LE +
    '    <col style="background-color:yellow">' + LE +
    '  </colgroup>' + LE +
    '  <tr>' + LE +
    '    <th>Column A</th>' + LE +
    '    <th>Column B</th>' + LE +
    '  </tr>' + LE +
    '  <tr>' + LE +
    '    <td>Cell 1</td>' + LE +
    '    <td>Cell 2</td>' + LE +
    '  </tr>' + LE +
    '</table>' + LE +
    '</body>' + LE +
    '</html>';
  
  ColWidth_auto_title =
    'Column width auto';
  ColWidth_auto_descr =
    'Column widths should fit text width in cells';
  ColWidth_auto_html =
    '<html>' + LE +
    '<body>' + LE +
    '<table border="1">' + LE +
    '  <tr>' + LE +
    '    <th>Column A</th>' + LE +
    '    <th>Column B</th>' + LE +
    '  </tr>' + LE +
    '  <tr>' + LE +
    '    <td>Cell 1</td>' + LE +
    '    <td>Cell 2</td>' + LE +
    '  </tr>' + LE +
    '</table>' + LE +
    '</body>' + LE +
    '</html>';
    
  ColWidth_fixed_title =
    'Column widths specified, total width automatic';
  ColWidth_fixed_descr =
    'The 1st column should be 150 pixels, the 2nd column 300 pixels wide.';
  ColWidth_fixed_html =
    '<html>' + LE +
    '<body>' + LE +
    '<table border="1">' + LE +
    '  <tr>' + LE +
    '    <th width="150">Column A</th>' + LE +
    '    <th width="300">Column B</th>' + LE +
    '  </tr>' + LE +
    '  <tr>' + LE +
    '    <td>Cell 1</td>' + LE +
    '    <td>Cell 2</td>' + LE +
    '  </tr>' + LE +
    '</table>' + LE +
    '</body>' + LE +
    '</html>';
    
  ColWidth_100perc_title =
    'Table width 100%';
  ColWidth_100perc_descr =
    'Table expands over available space, equal column widths';
  ColWidth_100perc_html =
    '<html>' + LE +
    '<body>' + LE +
    '<table border="1" width="100%">' + LE +
    '  <tr>' + LE +
    '    <th>Column A</th>' + LE +
    '    <th>Column B</th>' + LE +
    '  </tr>' + LE +
    '  <tr>' + LE +
    '    <td>Cell 1</td>' + LE +
    '    <td>Cell 2</td>' + LE +
    '  </tr>' + LE +
    '</table>' + LE +
    '</body>' + LE +
    '</html>';

  ColWidth_30perc_70perc_title =
    'Table width 100%, 1st column 30%';
  ColWidth_30perc_70perc_descr =
    'Table expands over available space, 1st column has 30%';
  ColWidth_30perc_70perc_html =
    '<html>' + LE +
    '<body>' + LE +
    '<table border="1" width="100%">' + LE +
    '  <tr>' + LE +
    '    <th width="30%">Column A</th>' + LE +
    '    <th width="70%">Column B</th>' + LE +
    '  </tr>' + LE +
    '  <tr>' + LE +
    '    <td>Cell 1</td>' + LE +
    '    <td>Cell 2</td>' + LE +
    '  </tr>' + LE +
    '</table>' + LE +
    '</body>' + LE +
    '</html>';

  ColWidth_200px_total100perc_title =
    'Table width 100%, 1st column 200px';
  ColWidth_200px_total100perc_descr =
    'Table expands over available space, 1st column has 200px, 2nd column rest';
  ColWidth_200px_total100perc_html =
    '<html>' + LE +
    '<body>' + LE +
    '<table border="1" width="100%">' + LE +
    '  <tr>' + LE +
    '    <th width="200px">Column A</th>' + LE +  // "px" must be removed by parser
    '    <th>Column B</th>' + LE +
    '  </tr>' + LE +
    '  <tr>' + LE +
    '    <td>Cell 1</td>' + LE +
    '    <td>Cell 2</td>' + LE +
    '  </tr>' + LE +
    '</table>' + LE +
    '</body>' + LE +
    '</html>';

  ColGroup_ColWidth_200px_total100perc_title =
    'Table using ColGroup, table width 100%, 1st column 200px, 2nd column covers rest';
  ColGroup_ColWidth_200px_total100perc_descr =
    'Table expands over available space, 1st column has 200px, 2nd column covers rest';
  ColGroup_ColWidth_200px_total100perc_html =
    '<html>' + LE +
    '<body>' + LE +
    '<table border="1" width="100%">' + LE +
    '  <colgroup>' + LE +
    '    <col span="1" width="200px">' + LE +      // "px" must be removed by parser
    '    <col span="1">' + LE + 
    '  </colgroup>' + LE +
    '  <tbody>' + LE +
    '    <tr>' + LE +
    '      <th>Column A</th>' + LE +
    '      <th>Column B</th>' + LE +
    '    </tr>' + LE +
    '    <tr>' + LE +
    '      <td>Cell 1</td>' + LE +
    '      <td>Cell 2</td>' + LE +
    '    </tr>' + LE +
    '  </tbody>' + LE +
    '</table>' + LE +
    '</body>' + LE +
    '</html>';
  
//------------------------------------------------------------------------------
//    Lists
//------------------------------------------------------------------------------
const
  OL_title =
    'Default';
  OL_descr =
    'The lines should be numbered 1, 2, 3.';
  OL_html =
    '<html>' + LE +
    '<body>' + LE +
    '<ol>' + LE +
    '  <li>Coffee</li>' + LE + 
    '  <li>Tea</li>' + LE +
    '  <li>Milk</li>' + LE +
    '</ol>' + LE +
    '</body>' + LE +
    '</html>';
  
  OL_typeA_title =
    'Type "A"';
  OL_typeA_descr =
    'The lines should begin with an upper-case letter (A, B, C).';
  OL_typeA_html =
    '<html>' + LE +
    '<body>' + LE +
    '<ol type="A">' + LE +         // allowed: "1", "A", "a", "I", "i"
    '  <li>Coffee</li>' + LE + 
    '  <li>Tea</li>' + LE +
    '  <li>Milk</li>' + LE +
    '</ol>' + LE +
    '</body>' + LE +
    '</html>';

  OL_typeA_inline_title =
    'Type "a" as inline "style" attribute';
  OL_typeA_inline_descr =
    'The lines should begin with a lower-case letter (a, b, c).';
  OL_typeA_inline_html =
    '<html>' + LE +
    '<body>' + LE +
    '<ol style="list-style-type:lower-alpha">' + LE +
    '  <li>Coffee</li>' + LE + 
    '  <li>Tea</li>' + LE +
    '  <li>Milk</li>' + LE +
    '</ol>' + LE +
    '</body>' + LE +
    '</html>';

  OL_typeA_style_title =
    'Type "a" as in <style> node';
  OL_typeA_style_descr =
    'The lines should begin with a lower-case letter (a, b, c).';
  OL_typeA_style_html =
    '<html>' + LE +
    '  <style type="text/css">' + LE +
    '    ol { list-style-type:lower-alpha }' + LE +
    '  </style>' + LE +
    '<body>' + LE +
    '<ol>' + LE +
    '  <li>Coffee</li>' + LE + 
    '  <li>Tea</li>' + LE +
    '  <li>Milk</li>' + LE +
    '</ol>' + LE +
    '</body>' + LE +
    '</html>';

  OL_start_title =
    'Start value';
  OL_start_descr =
    'The lines should be numbered 10, 11, 12.';
  OL_start_html =
    '<html>' + LE +
    '<body>' + LE +
    '<ol start="10">' + LE +
    '  <li>Coffee</li>' + LE + 
    '  <li>Tea</li>' + LE +
    '  <li>Milk</li>' + LE +
    '</ol>' + LE +
    '</body>' + LE +
    '</html>';
  
  OL_2lev_title =
    '2 levels, default style';
  OL_2lev_descr =
    'Each level should be numbered (1, 2).';
  OL_2lev_html =
    '<html>' + LE +
    '<body>' + LE +
    '<ol>' + LE +
    '  <li>' + LE +
    '    Coffee' + LE +
    '    <ol type="a">' + LE +
    '      <li>with sugar and milk</li>' + LE +
    '      <li>with sugar, no milk</li>' + LE +
    '      <li>no sugar, with milk</li>' + LE +
    '      <li>no sugar, no milk</li>' + LE +
    '    </ol>' + LE +
    '  </li>' + LE +
    '  <li>Milk</li>' + LE +
    '</ol>' + LE +
    '</body>' + LE +
    '</html>';

  OL_3lev_title =
    '3 levels, default style';
  OL_3lev_descr =
    'Each level should be numbered (1, 2, 3).';
  OL_3lev_html =
    '<html>' + LE +
    '<body>' + LE +
    '<ol>' + LE +
    '  <li>Coffee' + LE +
    '    <ol>' + LE +
    '      <li>with sugar' + LE +
    '        <ol>'+ LE +
    '          <li>with milk</li>' + LE +
    '          <li>no milk</li>' + LE +
    '        </ol>' + LE +
    '      </li>' + LE +
    '      <li>no sugar' + LE +
    '        <ol>' + LE +
    '          <li>with milk</li>' + LE +
    '          <li>no milk</li>' + LE +
    '        </ol>' + LE +
    '      </li>' + LE +
    '    </ol>' + LE +
    '  </li>' + LE +
    '  <li>Milk</li>' + LE +
    '</ol>' + LE +
    '</body>' + LE +
    '</html>';

  UL_title =
    'Default';
  UL_descr =
    'The lines should begin with a filled circle as bullet point.';
  UL_html =
    '<html>' + LE +
    '<body>' + LE +
    '<ul>' + LE +
    '  <li>Coffee</li>' + LE + 
    '  <li>Tea</li>' + LE +
    '  <li>Milk</li>' + LE +
    '</ul>' + LE +
    '</body>' + LE +
    '</html>';
  
  UL_2lev_title =
    '2 levels, default styles';
  UL_2lev_descr =
    '1st level should have filled, 2nd level open circles as bullet symbols.';
  UL_2lev_html =
    '<html>' + LE +
    '<body>' + LE +
    '<ul>' + LE +
    '  <li>' + LE +
    '    Coffee' + LE +
    '    <ul type="circle">' + LE +
    '      <li>with sugar and milk</li>' + LE +
    '      <li>with sugar, no milk</li>' + LE +
    '      <li>no sugar, with milk</li>' + LE +
    '      <li>no sugar, no milk</li>' + LE +
    '    </ul>' + LE +
    '  </li>' + LE +
    '  <li>Milk</li>' + LE +
    '</ul>' + LE +
    '</body>' + LE +
    '</html>';

  UL_3lev_title =
    '3 levels, default styles';
  UL_3lev_descr =
    'The indented 2nd and 3rd item levels should have an open circle and a filled square as bullet symbol.';
  UL_3lev_html =
    '<html>' + LE +
    '<body>' + LE +
    '<ul>' + LE +
    '  <li>Coffee' + LE +
    '    <ul>' + LE +
    '      <li>with sugar' + LE +
    '        <ul>'+ LE +
    '          <li>with milk</li>' + LE +
    '          <li>no milk</li>' + LE +
    '        </ul>' + LE +
    '      </li>' + LE +
    '      <li>no sugar' + LE +
    '        <ul>' + LE +
    '          <li>with milk</li>' + LE +
    '          <li>no milk</li>' + LE +
    '        </ul>' + LE +
    '      </li>' + LE +
    '    </ul>' + LE +
    '  </li>' + LE +
    '  <li>Milk</li>' + LE +
    '</ul>' + LE +
    '</body>' + LE +
    '</html>';

  UL_square_title =
    'Type "square"';
  UL_square_descr =
    'The lines should begin with a filled square.';
  UL_square_html =
    '<html>' + LE +
    '<body>' + LE +
    '<ul type="square">' + LE +
    '  <li>Coffee</li>' + LE + 
    '  <li>Tea</li>' + LE +
    '  <li>Milk</li>' + LE +
    '</ul>' + LE +
    '</body>' + LE +
    '</html>';
  
  UL_inline_title =
    'Type "circle", as inline style';
  UL_inline_descr =
    'The lines should begin with an open circle.';
  UL_inline_html =
    '<html>' + LE +
    '<body>' + LE +
    '<ul style="list-style-type:circle">' + LE +
    '  <li>Coffee</li>' + LE + 
    '  <li>Tea</li>' + LE +
    '  <li>Milk</li>' + LE +
    '</ul>' + LE +
    '</body>' + LE +
    '</html>';
  
  UL_style_title =
    'Type "circle", as <style> node';
  UL_style_descr =
    'The lines should begin with an open circle.';
  UL_style_html =
    '<html>' + LE +
    '  <style type="text/css">' + LE +
    '    ul { list-style-type:circle }' + LE +
    '  </style>' + LE +
    '<body>' + LE +
    '<ul>' + LE +
    '  <li>Coffee</li>' + LE + 
    '  <li>Tea</li>' + LE +
    '  <li>Milk</li>' + LE +
    '</ul>' + LE +
    '</body>' + LE +
    '</html>';
  
  UL_individual_title =
    'Individual styles per item';
  UL_individual_descr =
    'The 1st line should begin with an open circe, the 2nd line with a filled circle, ' +
    'and the 3rd line with a filled square.';
  UL_individual_html =
    '<html>' + LE +
    '<body>' + LE +
    '<ul>' + LE +
    '  <li type="circle">Coffee</li>' + LE + 
    '  <li type="disc">Tea</li>' + LE +
    '  <li type="square">Milk</li>' + LE +
    '</ul>' + LE +
    '</body>' + LE +
    '</html>';

  OUL_3lev_title =
    '3 levels, default styles';
  OUL_3lev_descr =
    'The outer level should be numbered, the inner levels bulleted with filled and open circles.';
  OUL_3lev_html =
    '<html>' + LE +
    '<body>' + LE +
    '<ol>' + LE +
    '  <li>Coffee' + LE +
    '    <ul>' + LE +
    '      <li>with sugar' + LE +
    '        <ul>'+ LE +
    '          <li>with milk</li>' + LE +
    '          <li>no milk</li>' + LE +
    '        </ul>' + LE +
    '      </li>' + LE +
    '      <li>no sugar' + LE +
    '        <ul>' + LE +
    '          <li>with milk</li>' + LE +
    '          <li>no milk</li>' + LE +
    '        </ul>' + LE +
    '      </li>' + LE +
    '    </ul>' + LE +
    '  </li>' + LE +
    '  <li>Milk</li>' + LE +
    '</ol>' + LE +
    '</body>' + LE +
    '</html>';

  
//------------------------------------------------------------------------------
//    CSS
//------------------------------------------------------------------------------
const
  HTMLCommentInCSS_title =
    'HTML comment in CSS section';
  HTMLCommentInCSS_descr =
    'The text must be red.';
  HTMLCommentInCSS_html =
    '<html>' + LE +
    '<head>' + LE +
    '  <style type="text/css">' + LE +
    '    <!--' + LE + 
    '    body { color: red; }' + LE +
    '    -->' + LE +
    '  </style>' + LE +
    '</head>' + LE +
    '<body>' + LE +
    '  <p>abc</p>' + LE +
    '</body>' + LE +
    '</html>';
    
//------------------------------------------------------------------------------
//   Special cases in file structure
//------------------------------------------------------------------------------
const
  NoHtmlTag_title =
    'No <html> tag';
  NoHtmlTag_descr =
    'You should see an ordered list with two items "line 1" and "line 2".';
  NoHtmlTag_html = 
    '<body>' + LE +
    '  <ol>' + LE + 
    '    <li>line 1</li>' + LE +
    '    <li>line 2</li>' + LE +
    '  </ol>' + LE +
    '</body>';
  
const
  NoBodyTag_title =
    'No <body> tag';
  NoBodyTag_descr =
    'You should see an ordered list with two items "line 1" and "line 2".';
  NoBodyTag_html =
    '<ol>' + LE +
    '  <li>line 1</li>' + LE +
    '  <li>line 2</li>' + LE +
    '</ol>';

  
//------------------------------------------------------------------------------
//   Right-to-left
//------------------------------------------------------------------------------
const
  Arab_title =
    'Arabian text';
  Arab_descr =
    'Text should begin at right.';
  Arab_html =
    '<html lang="ar" dir="rtl">' + LE +
    '<head>' + LE +
    '  <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />' + LE +
    '</head>' + LE +
    '<body>' + LE +
    '  <div> لماذا ترتيب الكلمات معكوس </div> ' + LE +
    '</body>' + LE +
    '</html>';
  
  Hebrew_title =
    'Hebrew text (RTL in <html> tag)';
  Hebrew_descr =
    'Text should begin at right. Terminating period must be at left of last word.';
  Hebrew_html =
    '<html lang="he-IL" dir="rtl">' + LE +
    '<body>' + LE +
    '  <div>אנא פנאי בהבנה צרפתית גם. בה ויקי פיסיקה חפש, מפתח המשפט אתה על. כתב ב ליצירתה ויקימדיה, בדף על בקרבת ייִדיש ליצירתה. גם אנא עקרונות התפתחות פוליטיקה, יוני מיותר בחירות אנא מה. או שנורו בישול ארץ, עזרה וקשקש קצרמרים או שער. מה זאת מיזם מדעי סטטיסטיקה. זכר אל היסטוריה אקטואליה, את כלים מיזמי מונחים מדע.</div>' + LE +
    '</body>' + LE +
    '</html>';

  Hebrew_bodyRTL_title =
    'Hebrew text (RTL in <body> tag)';
  Hebrew_bodyRTL_descr =
    'Text should begin at right. Terminating period must be at left of last word.';
  Hebrew_bodyRTL_html =
    '<html lang="he-IL">' + LE +
    '<head>' + LE +
    '  <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />' + LE +
    '</head>' + LE +
    '<body dir="rtl">' + LE +
    '  <div>אנא פנאי בהבנה צרפתית גם. בה ויקי פיסיקה חפש, מפתח המשפט אתה על. כתב ב ליצירתה ויקימדיה, בדף על בקרבת ייִדיש ליצירתה. גם אנא עקרונות התפתחות פוליטיקה, יוני מיותר בחירות אנא מה. או שנורו בישול ארץ, עזרה וקשקש קצרמרים או שער. מה זאת מיזם מדעי סטטיסטיקה. זכר אל היסטוריה אקטואליה, את כלים מיזמי מונחים מדע.</div>' + LE +
    '</body>' + LE +
    '</html>';

  Hebrew_divRTL_title =
    'Hebrew text (RTL in <div> tag)';
  Hebrew_divRTL_descr =
    'Text should begin at right. Terminating period must be at left of last word.';
  Hebrew_divRTL_html =
    '<html>' + LE +
    '<head>' + LE +
    '  <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />' + LE +
    '</head>' + LE +
    '<body>' + LE +
    '  <div dir="rtl">אנא פנאי בהבנה צרפתית גם. בה ויקי פיסיקה חפש, מפתח המשפט אתה על. כתב ב ליצירתה ויקימדיה, בדף על בקרבת ייִדיש ליצירתה. גם אנא עקרונות התפתחות פוליטיקה, יוני מיותר בחירות אנא מה. או שנורו בישול ארץ, עזרה וקשקש קצרמרים או שער. מה זאת מיזם מדעי סטטיסטיקה. זכר אל היסטוריה אקטואליה, את כלים מיזמי מונחים מדע.</div>' + LE +
    '</body>' + LE +
    '</html>';

  Hebrew_pRTL_title =
    'Hebrew text (RTL in <p> tag)';
  Hebrew_pRTL_descr =
    'Text should begin at right. Terminating period must be at left of last word.';
  Hebrew_pRTL_html =
    '<html>' + LE +
    '<head>' + LE +
    '  <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />' + LE +
    '</head>' + LE +
    '<body>' + LE +
    '  <p dir="rtl">אנא פנאי בהבנה צרפתית גם. בה ויקי פיסיקה חפש, מפתח המשפט אתה על. כתב ב ליצירתה ויקימדיה, בדף על בקרבת ייִדיש ליצירתה. גם אנא עקרונות התפתחות פוליטיקה, יוני מיותר בחירות אנא מה. או שנורו בישול ארץ, עזרה וקשקש קצרמרים או שער. מה זאת מיזם מדעי סטטיסטיקה. זכר אל היסטוריה אקטואליה, את כלים מיזמי מונחים מדע.</p>' + LE +
    '</body>' + LE +
    '</html>';


implementation

end.

