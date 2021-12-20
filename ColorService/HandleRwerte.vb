Option Strict Off
Option Explicit On
Option Compare Text

Public Class HandleRwerte
  Implements IDisposable
  Dim MncboGRP As ComboBox
  Dim MnlblGRP As Label
  Dim MnIarch As CheckBox
  Dim ViewRwert = New DataView(MenueParam.GroupTableRwert)
  Property cboGRP() As ComboBox
    Get
      cboGRP = MncboGRP
    End Get
    Set(ByVal value As ComboBox)
      MncboGRP = value
    End Set
  End Property
  Property lblGRP() As Label
    Get
      lblGRP = Mnlblgrp
    End Get
    Set(ByVal value As Label)
      Mnlblgrp = value
    End Set
  End Property
  'Inherits System.ComponentModel.Component


 
  '
  '
  Dim FormRwert As New frmHandleRwert
  Dim disposed As Boolean
  Public Event HdlGetNameID(ByRef ID As Integer, ByRef RefName As String, ByRef Banum As String)

  Property Messrefel() As RefValue
    Get
      Messrefel = FormRwert.Messrefel
    End Get
    Set(ByVal value As RefValue)
      FormRwert.Messrefel = value
    End Set
  End Property
  Public Sub GRoupList()
    ViewRwert.RowFilter = "USER_ID=" & MenueParam.UserID & " AND MESSG_ID=" & MenueParam.MessgID & " AND (DONT_SHOW=FALSE OR GROUP_ID=" & MenueParam.Messg.RwrtGID & ")"
    If ViewRwert.Count = 0 Then
      ViewRwert.rowfilter = ""
      MsgBox(Texxt(3635))
      Exit Sub
    End If

    '
    '
    '
    'Gruppe enabled/visible
    '
    '
    '
    If Not BitWrt(0, MenueParam.User.Visbl) Then
      If Not IsNothing(MnlblGRp) Then
        MnlblGRp.Visible = False
      End If
      If Not IsNothing(MncboGRP) Then
        MncboGRP.Visible = False
      End If
    Else
      If Not IsNothing(MnlblGRp) Then
        MnlblGRp.Visible = True
      End If
      If Not IsNothing(MncboGRP) Then
        MncboGRP.Visible = True
      End If
    End If
    If Not BitWrt(0, MenueParam.User.Enabl) Then
      If Not IsNothing(MncboGRP) Then
        MncboGRP.Enabled = False
      End If
    Else
      If Not IsNothing(MncboGRP) Then
        MncboGRP.Enabled = True
      End If
    End If
    '
    '
    '
    '
    '
    MncboGRP.DataSource = ViewRwert
    MncboGRP.DisplayMember = "GROUP_KBEZ"
    MncboGRP.ValueMember = "GROUP_ID"
    MncboGRP.SelectedValue = MenueParam.Messg.RwrtGID
  End Sub
  Function GlanzWrt() As ArrayList
    Dim GlWr As New ArrayList
    GlWr.Add(0.0#)
    GlWr.Add(0.01)
    GlWr.Add(0.02)
    GlWr.Add(0.025)
    GlWr.Add(0.03)
    GlWr.Add(0.0325)
    GlWr.Add(0.035)
    GlWr.Add(0.036)
    GlWr.Add(0.037)
    GlWr.Add(0.038)
    GlWr.Add(0.039)
    GlWr.Add(0.04)
    GlWr.Add(0.041)
    GlWr.Add(0.042)
    GlWr.Add(0.043)
    GlWr.Add(0.044)
    GlWr.Add(0.045)
    GlWr.Add(0.0475)
    GlWr.Add(0.05)
    GlanzWrt = GlWr
  End Function




  Public WriteOnly Property Iarch() As Short
    Set(ByVal AcIarch As Short)
      FormRwert.chkARC.CheckState = AcIarch
    End Set
  End Property

  Public Property Retr() As Short
    Get
      Retr = FormRwert.Retr
    End Get
    Set(ByVal AcRetr As Short)
      FormRwert.Retr = AcRetr
    End Set
  End Property

  Public WriteOnly Property Captext() As String
    Set(ByVal AcCaptext As String)
      FormRwert.Captext = AcCaptext
    End Set
  End Property

  Public ReadOnly Property HideSofort() As Boolean
    Get
      HideSofort = FormRwert.HideSofort
    End Get
  End Property

  
  Public WriteOnly Property Measure() As Object
    Set(ByVal value As Object)
      FormRwert.Measure = value
    End Set
  End Property

  Public Overloads Sub ReflexWerte(ByRef Ialle As Boolean)
    Try

      FormRwert.Ialle = Ialle
      If Not FormRwert.Messrefel.IVoNa Then
        FormRwert.Messrefel.Iami = -1
      End If
      FormRwert.ShowDialog()
    Catch ex As Exception

    End Try
  End Sub
  Public Overloads Sub ReflexWerte(ByRef Dialog As DialogResult)
    FormRwert.Ialle = False
    If Not FormRwert.Messrefel.IVoNa Then
      FormRwert.Messrefel.Iami = -1
    End If
    FormRwert.ShowDialog()
    Dialog = FormRwert.DialogResult
  End Sub
  '
  Public Sub RaiGetNameID(ByRef ID As Integer, ByRef RefName As String, ByRef Banum As String)
    RaiseEvent HdlGetNameID(ID, RefName, Banum)
  End Sub
  Public Sub InVisible()
    If FormRwert.Messrefel.Iami = -1 Then
      FormRwert.DialogResult = DialogResult.Abort
    Else
      FormRwert.DialogResult = DialogResult.OK
    End If
    FormRwert.Hide()
  End Sub
  Public Function MeldSpeiAllRwrt(Del As Boolean) As Boolean
    Dim Meld As Integer
    MeldSpeiAllRwrt = True
    '
    '
    'Prüfen , ob GrpInd = Read-only
    '
    '

    If MncboGRP.SelectedItem("READ_ONLY") Then
      MeldSpeiAllRwrt = False
      MsgBox(Texxt(3014))
      Exit Function
    End If

    '

    If MncboGRP.SelectedValue <> 0 Then Exit Function
    Meld = 3009
    If Del Then
      Meld = 3033
    End If
    If MessageBox.Show(RepTexxt(Texxt(Meld), MncboGRP.Text), Texxt(2000), MessageBoxButtons.YesNo) = DialogResult.No Then
      MeldSpeiAllRwrt = False
    End If
  End Function

  Sub MakeTABRefwerte(ByRef WithRecipes As Boolean, ByRef kbez As String, ByRef Winkel As AngGeos, ByRef Tabwerte As DataTable)
    Dim i As Integer
    Dim TextBem As String
    Dim TextMess As String
    TextBem = Texxt(916)
    TextMess = kbez
    If WithRecipes Then
      TextBem = TextBem & "/" & Texxt(602)
      TextMess = TextMess & "/" & Texxt(893)
    End If
    Tabwerte.Rows.Clear()
    Tabwerte.Columns.Clear()
    Tabwerte.Columns.Add(Texxt(3659))
    Tabwerte.Columns.Add(Texxt(824))
    Tabwerte.Columns.Add(TextBem)
    Tabwerte.Columns.Add(Texxt(375))
    Tabwerte.Columns.Add(TextMess)
    For i = 0 To Winkel.Wsol.Nwe - 1
      Tabwerte.Columns.Add(CStr(Winkel.Wsol.R(i)))
    Next i
  End Sub
  Sub MakeGRIDRefwerte(ByRef winkel As AngGeos, ByRef grdwrt As DataGridView)
    Dim i As Integer
    grdwrt.Columns(0).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleLeft
    grdwrt.Columns(1).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleLeft
    grdwrt.Columns(2).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    grdwrt.Columns(3).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleLeft
    grdwrt.Columns(4).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    grdwrt.Columns(0).SortMode = DataGridViewColumnSortMode.NotSortable
    grdwrt.Columns(1).SortMode = DataGridViewColumnSortMode.NotSortable
    grdwrt.Columns(2).SortMode = DataGridViewColumnSortMode.NotSortable
    grdwrt.Columns(3).SortMode = DataGridViewColumnSortMode.NotSortable
    grdwrt.Columns(4).SortMode = DataGridViewColumnSortMode.NotSortable
    grdwrt.Columns(0).Width = 50
    grdwrt.Columns(1).Width = 150
    grdwrt.Columns(2).Width = 125
    grdwrt.Columns(3).Width = 125
    grdwrt.Columns(4).Width = 100
    For i = 0 To winkel.Wsol.Nwe - 1
      grdwrt.Columns(i + 5).Width = 50
      grdwrt.Columns(i + 5).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
      grdwrt.Columns(i + 5).SortMode = DataGridViewColumnSortMode.NotSortable
    Next i
  End Sub
  Protected Overrides Sub Finalize()
    MyBase.Finalize()
    dispose()
  End Sub
  Public Sub New()
    MyBase.New()
    ViewRwert.rowfilter = ""



    ' Initialisierungen nach dem Aufruf InitializeComponent() hinzufügen
    FormRwert.HandleRwert = Me
    disposed = False
  End Sub
  Sub dispose() Implements IDisposable.Dispose
    If disposed Then Exit Sub

    FormRwert.Dispose()
    disposed = True
  End Sub



  Sub TabRefRecord(ByVal kwb As Integer, ByVal Winkel As AngGeos, ByVal Fakt As Single, ByVal RefWert As RefValue, ByRef TabWrt As DataTable, ByRef ier As Integer)
    Dim RowWrt As DataRow
    Dim i As Integer
    Dim kw As Integer
    Dim CHRM As String
    ier = 0
    RowWrt = TabWrt.NewRow
    RowWrt(0) = CStr(kwb)
    RowWrt(1) = RefWert.Name
    RowWrt(2) = RefWert.Bem
    RowWrt(3) = RefWert.DatTim

    For kw = 0 To Winkel.Km - 1
      chrm = Winkel(kw).Chrm
      '
      RowWrt(4) = CHRM
      '
      For i = 0 To Winkel.Wsol.Nwe - 1
        RowWrt(5 + i) = Format(Fakt * RefWert.RefKurv(CHRM).R(i), "###.000")
      Next i
      TabWrt.Rows.Add(RowWrt)
      RowWrt = TabWrt.NewRow
      RowWrt(0) = Space(1)
      RowWrt(1) = Space(1)
      RowWrt(2) = Space(1)
      RowWrt(3) = Space(1)
    Next kw

  End Sub

End Class