Attribute VB_Name = "Module12"
Sub AdjustTableHeaders()

    Range("A1").Select
    ActiveCell.FormulaR1C1 = "RespondentID"
    Columns("B:B").Select
    Selection.Insert Shift:=xlToRight, CopyOrigin:=xlFormatFromLeftOrAbove
    Range("B1").Select
    ActiveCell.FormulaR1C1 = "AT"
    Columns("C:C").Select
    Selection.Insert Shift:=xlToRight, CopyOrigin:=xlFormatFromLeftOrAbove
    ActiveCell.FormulaR1C1 = "AL"
    Columns("D:D").Select
    Selection.Insert Shift:=xlToRight, CopyOrigin:=xlFormatFromLeftOrAbove
    ActiveCell.FormulaR1C1 = "PT"
    Range("E1").Select
    ActiveCell.FormulaR1C1 = "AccountTrustPre"
    Range("F1").Select
    ActiveCell.FormulaR1C1 = "AccountTrustPost"
    Range("G1").Select
    ActiveCell.FormulaR1C1 = "PostTrust"
    Range("H1").Select
    ActiveCell.FormulaR1C1 = "Like"
    Range("I1").Select
    ActiveCell.FormulaR1C1 = "Forward"
    Range("J1").Select
    ActiveCell.FormulaR1C1 = "Comment"
    Range("K1").Select
    ActiveCell.FormulaR1C1 = "CommentText"
    Range("L1").Select
    ActiveCell.FormulaR1C1 = "CASat"
    Range("M1").Select
    ActiveCell.FormulaR1C1 = "SASat"
    Range("N1").Select
    ActiveCell.FormulaR1C1 = "NASat"
    Range("O1").Select
    ActiveCell.FormulaR1C1 = "COVID-19"
    Range("P1").Select
    ActiveCell.FormulaR1C1 = "Vaccine"
    Range("Q1").Select
    ActiveCell.FormulaR1C1 = "Age"
    Range("R1").Select
    ActiveCell.FormulaR1C1 = "Degree"
    Range("S1").Select
    ActiveCell.FormulaR1C1 = "Gender"
    Range("T1").Select
    ActiveCell.FormulaR1C1 = "ColourDeficiency"
    Range("U1").Select
    ActiveCell.FormulaR1C1 = "Culture"
    Range("V1").Select
    ActiveCell.FormulaR1C1 = "PV"
     Range("W1").Select
    ActiveCell.FormulaR1C1 = "Consent"
    Range("X1").Select
    ActiveCell.FormulaR1C1 = "Resident"
   
    
End Sub
