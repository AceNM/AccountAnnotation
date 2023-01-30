Attribute VB_Name = "Module1"
Sub Macro6()
Attribute Macro6.VB_ProcData.VB_Invoke_Func = " \n14"
'
' Macro6 Macro
'

'
    Columns("J:K").Select
    Selection.Cut
    Columns("FM:FM").Select
    Selection.Insert Shift:=xlToRight
End Sub
