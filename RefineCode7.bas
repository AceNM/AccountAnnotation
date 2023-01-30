Attribute VB_Name = "Module3"
Sub Macro4()
Attribute Macro4.VB_ProcData.VB_Invoke_Func = " \n14"

    Dim i As Integer
    Dim nofPar As Integer
    
    Columns("L:L").Select
    Selection.Insert Shift:=xlToRight, CopyOrigin:=xlFormatFromLeftOrAbove
    Range("L1").Select
    ActiveCell.FormulaR1C1 = "ATSat"
    Columns("M:M").Select
    Selection.Insert Shift:=xlToRight, CopyOrigin:=xlFormatFromLeftOrAbove
    Range("M1").Select
    ActiveCell.FormulaR1C1 = "SatLev"
    For i = 1 To 3
    
        Range("N1").Select
        Selection.Copy
        Range("L2").Offset((i - 1) * nofPar, 0).Select
        ActiveSheet.Paste
        Range("M2").Offset((i - 1) * nofPar, 0).Select
        Selection.AutoFill Destination:=Range(Range("M2").Offset((i - 1) * nofPar, 0), Range("M2").Offset(i * nofPar - 1, 0)), Type:=xlFillDefault
        Range(Range("N2").Offset((i - 1) * nofPar, 0), Range("N2").Offset(i * nofPar - 1, 0)).Select
        Selection.Cut
        Range("M2").Select
        ActiveSheet.Paste
        Range("N:N").Select
        Selection.Delete Shift:=xlToLeft
    
    Next i
End Sub
Sub Macro5()
Attribute Macro5.VB_ProcData.VB_Invoke_Func = " \n14"
'
' Macro5 Macro
'

'
    
End Sub
