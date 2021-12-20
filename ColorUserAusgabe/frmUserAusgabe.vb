Option Compare Text
Option Explicit On
Option Strict Off
Friend Class frmUserAusgabe
  Dim QuTable As List(Of DataTable)
  Dim radTable As List(Of radiobutton)
  Private Sub frmUserAusgabe_Load(sender As Object, e As System.EventArgs) Handles Me.Load

    radTable_00.Text = TblMenue(0).TableName
    radTable_01.Text = TblMenue(1).TableName
    radTable_02.Text = TblAnWeisng.TableName
    radTable_03.Text = TblMerk.TableName
    radTable_04.Text = TblWeisngRwerte.TableName
    radTable_05.Text = TblNormLicht.TableName
    radTable_06.Text = TblWinkel.TableName
    radTable_07.Text = TblWerte.TableName
    radTable_08.Text = TblRwerte.TableName
    radTable_09.Text = TblFarben.TableName
    radTable_10.Text = TblRezepte.TableName
    radTable_11.Text = TblMengen.TableName
    radTable_12.Text = TblWerteAllg.TableName
    radTable = New List(Of RadioButton)
    radTable.Add(radTable_00)
    radTable.Add(radTable_01)
    radTable.Add(radTable_02)
    radTable.Add(radTable_03)
    radTable.Add(radTable_04)
    radTable.Add(radTable_05)
    radTable.Add(radTable_06)
    radTable.Add(radTable_07)
    radTable.Add(radTable_08)
    radTable.Add(radTable_09)
    radTable.Add(radTable_10)
    radTable.Add(radTable_11)
    radTable.Add(radTable_12)
    '
    '
    '
    QuTable = New List(Of DataTable)
    QuTable.Add(TblMenue(0))
    QuTable.Add(TblMenue(1))
    QuTable.Add(TblAnWeisng)
    QuTable.Add(TblMerk)
    QuTable.Add(TblWeisngRwerte)
    QuTable.Add(TblNormLicht)
    QuTable.Add(TblWinkel)
    QuTable.Add(TblWerte)
    QuTable.Add(TblRwerte)
    QuTable.Add(TblFarben)
    QuTable.Add(TblRezepte)
    QuTable.Add(TblMengen)
    QuTable.Add(TblWerteAllg)
    For i = 9 To 12
      If Not IsNothing(QuTable(i)) Then
        radTable(i).Visible = True
      End If
    Next


  End Sub

  Private Sub radTable_Click(sender As Object, e As System.EventArgs) Handles radTable_00.Click, radTable_01.Click, radTable_02.Click, _
   radTable_03.Click, radTable_04.Click, radTable_05.Click, radTable_06.Click, radTable_07.Click, radTable_08.Click, radTable_09.Click, _
   radTable_10.Click, radTable_11.Click, radTable_12.Click
    Dim Index As Integer
    Index = CInt(sender.name.substring(9, 2))
    Call TdbGridUserAusgabe.SetDataBinding(QuTable(Index), "", False)
  End Sub

  Private Sub btnKOP_Click(sender As System.Object, e As System.EventArgs) Handles btnKOP.Click
    Call C1GridtoClipboard(TdbGridUserAusgabe)
  End Sub

  
 
End Class