Option Explicit On
Option Compare Text
Option Strict Off
Public Class HandleFillGrid
  Implements IDisposable
  Dim disposed As Boolean
  Dim TblGrid As DataTable
  Dim WithEvents MnflgREZ As DataGridView
  Dim WithEvents MnchkFawrt As CheckBox
  Dim MnRezGraphic As HandleRezGrafik
  Dim MnAufbaupar As AufbauParameters
  Dim FarbWrt As ValuesGrpsAssigns
  Dim FawrtRwerte As RefValuesGrp
  Dim quali As QualKontrolle
  Dim MnKsrt As ArrayList()
  Public Sub New()
    disposed = False
    TblGrid = New DataTable
    FawrtRwerte = New RefValuesGrp
    FarbWrt = New ValuesGrpsAssigns
    quali = New QualKontrolle
  End Sub

  Protected Overrides Sub Finalize()
    MyBase.Finalize()
    dispose()

  End Sub
  Sub dispose() Implements IDisposable.Dispose
    If disposed Then Exit Sub
    FawrtRwerte = Nothing
    FarbWrt = Nothing
    quali = Nothing
    TblGrid.Dispose()
    disposed = True
  End Sub
  '
  '
  '
  '
  '
  '
  'Properties
  '
  '
  '
  '
  '
  Property flgREZ() As DataGridView
    Get
      flgREZ = MnflgREZ
    End Get
    Set(ByVal Value As DataGridView)
      MnflgREZ = Value
    End Set
  End Property
  Property chkFawrt() As CheckBox
    Get
      chkFawrt = MnchkFawrt
    End Get
    Set(ByVal Value As CheckBox)
      MnchkFawrt = Value
    End Set
  End Property
  Property RezGraphic() As HandleRezGrafik
    Get
      RezGraphic = MnRezGraphic
    End Get
    Set(ByVal Value As HandleRezGrafik)
      MnRezGraphic = Value
    End Set
  End Property

  '
  '
  '
  '
  'Methods
  '
  '
  '
  '
  '
  Sub FillUeGrid()
    Dim k As Short
    Dim j As Short
    Dim i As Short
    Dim Preis As Single
    Dim RzNr As String
    Dim FaId As Integer
    Dim KeyD As String
    Dim ier As Short
    TblGrid.Clear()
    '
    FawrtRwerte.clear()


    FawrtRwerte.Add(KeyRe(0), New RefValues)
    If RezGraphic.WSOpt = 1 Then
      FawrtRwerte.Add(KeyRe(1), New RefValues)
    End If

    FarbWrt.clear()

    AufbauPar.AufbauRezeptMerk(FarbWrt, ier)


    For k = 0 To RezGraphic.Ksrt.Count - 1
      FarbWrt(0).clear()
      If FarbWrt.Count > 1 Then
        FarbWrt(1).clear()
      End If
      FawrtRwerte(0).clear()
      If FawrtRwerte.Count > 1 Then
        FawrtRwerte(1).clear()
      End If
      RzNr = KeyRe(RezGraphic.Ksrt(k))
      If Not RezGraphic.AllRezepte.Rezepte.ContainsKey(RzNr) Then
        Continue For
      End If
      FawrtRwerte(0).Add(KeyRe(0), RezGraphic.GrpRwerte(0)("V"))
      FawrtRwerte(0).Add(KeyRe(1), RezGraphic.GrpRwerte(0)(RzNr))
      If RezGraphic.WSOpt = 1 Then
        FawrtRwerte(1).Add(KeyRe(0), RezGraphic.GrpRwerte(1)("V"))
        FawrtRwerte(1).Add(KeyRe(1), RezGraphic.GrpRwerte(1)(RzNr))
      End If
      '
      '
      '
      Call quali.FarbWrtCalc(MenueParam.User.Winkel, FawrtRwerte, FarbWrt, ier)
      TblGrid.Rows.Add(TblGrid.NewRow)
      'Rezeptnummer
      TblGrid.Rows(k)(0) = RzNr
      'DE*
      TblGrid.Rows(k)(1) = FarbWrt(RezGraphic.WSOpt)(1)(RezGraphic.Knlz)(RezGraphic.Kwop)(MenueParam.Menue.StdMrkKen(15))

      'DL*
      TblGrid.Rows(k)(2) = FarbWrt(RezGraphic.WSOpt)(1)(RezGraphic.Knlz)(RezGraphic.Kwop)(MenueParam.Menue.StdMrkKen(16))
      'DC*
      TblGrid.Rows(k)(3) = FarbWrt(RezGraphic.WSOpt)(1)(RezGraphic.Knlz)(RezGraphic.Kwop)(MenueParam.Menue.StdMrkKen(17))

      'DH*
      TblGrid.Rows(k)(4) = FarbWrt(RezGraphic.WSOpt)(1)(RezGraphic.Knlz)(RezGraphic.Kwop)(MenueParam.Menue.StdMrkKen(18))

      'Metamerie
      TblGrid.Rows(k)(5) = FarbWrt(RezGraphic.WSOpt)(1)(RezGraphic.Knlz)(RezGraphic.Kwop)(MenueParam.Menue.StdMrkKen(14))

      'Preis
      Preis = 0.0#
      For i = 0 To RezGraphic.AllRezepte.Rezepte(RzNr).KF - 1
        FaId = RezGraphic.AllRezepte.Rezepte(RzNr)(i).ID
        KeyD = KeyName(FaId)
        Preis = Preis + RezGraphic.AllRezepte.Farben(KeyD).Preis * RezGraphic.AllRezepte.Rezepte(RzNr)(i).FaAmng
      Next i
      TblGrid.Rows(k)(6) = Format(Preis, "###.00")
      For i = 0 To RezGraphic.AllRezepte.Farben.FarbCount - 1
        For j = 0 To RezGraphic.AllRezepte.Rezepte(RzNr).KF - 1
          TblGrid.Rows(k)(7 + i) = " "
          If RezGraphic.AllRezepte.Rezepte(RzNr)(j).ID = RezGraphic.AllRezepte.Farben(i).ID Then
            TblGrid.Rows(k)(7 + i) = Format(RezGraphic.AllRezepte.Rezepte(RzNr)(j).BaAmng, RezGraphic.AllRezepte.Farben(i).Form).Trim
            Exit For
          End If
        Next j
      Next i
    Next k
  End Sub
  Sub TableAufbau()
    Dim i As Integer
    Dim Jbase As Integer
    Jbase = MenueParam.Menue.TexJabst
    TblGrid.Columns.Clear()
    TblGrid.Columns.Add(KeyName(0), GetType(String))
    TblGrid.Columns(KeyName(0)).Caption = Texxt(930)

    '
    'Farbdiff.
    For i = 1 To 4
      TblGrid.Columns.Add(KeyName(i), GetType(String))
      TblGrid.Columns(KeyName(i)).Caption = Trim(TexKt(Jbase + i))
    Next i
    'Metamerie
    TblGrid.Columns.Add(KeyName(5), GetType(String))
    TblGrid.Columns(KeyName(5)).Caption = Trim(TexKt(Jbase))
    'Preis
    TblGrid.Columns.Add(KeyName(6), GetType(String))
    TblGrid.Columns(KeyName(6)).Caption = Trim(Texxt(842))

    For i = 0 To RezGraphic.AllRezepte.Farben.FarbCount - 1
      TblGrid.Columns.Add(KeyName(7 + i), GetType(String))
      TblGrid.Columns(KeyName(7 + i)).Caption = Trim(RezGraphic.AllRezepte.Farben(i).Name)
    Next i

    '
    '
    '
    '
    '
    '
    MnflgREZ.DataSource = TblGrid

  End Sub
  Sub GridAufbau()
    Dim i As Short
    Dim k As Short
    Dim WiFawrt As Single
    Dim StWidth As Single
    '
    If MnflgREZ.Columns.Count = 0 Then Exit Sub
    If MnflgREZ.Columns.Count <> TblGrid.Columns.Count Then Exit Sub
    MnflgREZ.MultiSelect = False
    'MnflgREZ.AutoSizeColumnsMode = DataGridViewAutoSizeColumnsMode.Fill
    MnflgREZ.AlternatingRowsDefaultCellStyle.BackColor = Color.WhiteSmoke
    MnflgREZ.RowHeadersWidth = 20
    MnflgREZ.Columns(0).Width = 35
    WiFawrt = 375
    StWidth = (flgREZ.Width - WiFawrt) / RezGraphic.AllRezepte.Farben.FarbCount
    If StWidth < 50 Then
      StWidth = 50
    End If
    For i = 0 To TblGrid.Columns.Count - 1
      MnflgREZ.Columns(i).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
      MnflgREZ.Columns(i).HeaderCell.Style.Alignment = DataGridViewContentAlignment.MiddleLeft
      MnflgREZ.Columns(i).Width = StWidth
      MnflgREZ.Columns(i).Visible = True
    Next i
    For i = 1 To 6
      MnflgREZ.Columns(i).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
      MnflgREZ.Columns(i).HeaderCell.Style.Alignment = DataGridViewContentAlignment.MiddleCenter
      MnflgREZ.Columns(i).Width = 50
      If chkFawrt.CheckState = CheckState.Unchecked Then
        MnflgREZ.Columns(i).Visible = False
      Else
        MnflgREZ.Columns(i).Visible = True
      End If
    Next i

    For i = 0 To TblGrid.Columns.Count - 1
      MnflgREZ.Columns(i).SortMode = DataGridViewColumnSortMode.NotSortable
      MnflgREZ.Columns(i).HeaderText = TblGrid.Columns(i).Caption
      If chkFawrt.CheckState = CheckState.Unchecked Then
        If i > 0 And i < 7 Then
          MnflgREZ.Columns(i).HeaderText = ""
        End If
      End If
    Next
    If flgREZ.Rows.Count > 0 Then
      For i = 0 To flgREZ.Columns.Count - 1
        For k = 0 To flgREZ.Rows.Count - 1
          If Trim(flgREZ.Rows(k).Cells(i).Value) <> "" Then
            Exit For
          End If
        Next k
        If k = flgREZ.Rows.Count Then
          flgREZ.Columns(i).Visible = False
        End If
      Next i
    End If
    MnflgREZ.Columns(0).Frozen = True

  End Sub

  Private Sub MnchkFawrt_CheckStateChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles MnchkFawrt.CheckStateChanged
    Call GridAufbau()
  End Sub

  Private Sub MnflgREZ_Resize(ByVal sender As Object, ByVal e As System.EventArgs) Handles MnflgREZ.Resize
    Call GridAufbau()
  End Sub
End Class

