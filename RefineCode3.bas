Attribute VB_Name = "Module1"
Sub DeleteRowsColumns()
    'deleting unnecessary rows and columns

    'Columns("FR").SpecialCells(xlCellTypeBlanks).EntireRow.Delete
    Rows("2:2").Select
    Selection.Delete Shift:=xlUp
    Columns("B:I").Select
    Selection.Delete Shift:=xlToLeft
    Range("I1:EY1").Select
    Selection.ClearContents
    ActiveWorkbook.Save
    
End Sub
