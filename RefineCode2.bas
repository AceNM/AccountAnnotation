Attribute VB_Name = "Module2"
Sub AdjustingDataTypes()

    'Adjusting data types
    Columns("A:A").Select
    Selection.NumberFormat = "0.00"
    Selection.NumberFormat = "0.0"
    Selection.NumberFormat = "0"
    
End Sub
