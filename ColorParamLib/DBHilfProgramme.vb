Option Strict Off
Option Explicit On
Option Compare Text

Public Class DBHilfProgramme
  Implements IDisposable
  Dim Rec As Integer
  Private Shared UseTexte As String
  Private Shared TblTexte As String
  Private Shared DrAllg As OleDbDataReader
  Private Shared CmdAllg As OleDbCommand
  Private Shared TextUse As DataTable = Nothing
  Private Shared TextTex As DataTable = Nothing
  Private Shared RowUse() As DataRow
  Private Shared RowTex() As DataRow
  Private Shared RowToolTip() As DataRow
  Private Shared MnMenueParam As AllParameters
  Private Shared MnAufbauPar As AufbauParameters
  ' Private Shared TableToolTip As DataTable = Nothing

  Shared Sub New()
    MnMenueParam = New AllParameters
    MnAufbauPar = New AufbauParameters
    Call TextBuild()
    'Call TabBuildTooltip(TableToolTip, "TBL_TEXTE_TOOLTIP")
  End Sub
  Public Shared Sub TextNewStart()
    textuse = Nothing
    texttex = Nothing
    Call TextBuild()
  End Sub
  Private Shared Sub TextBuild()
    TblTexte = GetPrivSettings("LANGUAGE", "TABLE", "TBL_TEXTE_GER", COLORFileName())
    UseTexte = "TBL_TEXTE_USE_" & TblTexte.Substring(10, 3)
    If TextUse Is Nothing And TableExists(UseTexte, Cncol) Then
      TextUse = New DataTable
      Call TabBuild(TextUse, UseTexte)
    End If
    If TextTex Is Nothing Then
      TextTex = New DataTable
      Call TabBuild(TextTex, TblTexte)
    End If

  End Sub
  Private Sub dispose() Implements IDisposable.Dispose
    'CmdAllg = Nothing
  End Sub

  Protected Overrides Sub Finalize()
    MyBase.Finalize()
    dispose()
  End Sub


  Public Shared Property MenueParam() As AllParameters
    Get
      MenueParam = MnMenueParam
    End Get
    Set(value As AllParameters)
      MnMenueParam = value
    End Set
  End Property
  Public Shared ReadOnly Property AufbauPar() As AufbauParameters
    Get
      AufbauPar = MnAufbauPar
    End Get
  End Property
  Shared Function Texxt(ByRef i As Integer) As String
    If Not IsNothing(TextUse) Then
      RowUse = TextUse.Select("TEXT_ID=" & i)
    End If
    If RowUse Is Nothing OrElse RowUse.Length = 0 OrElse IsDBNull(RowUse(0)("TEXT_LBEZ")) Then
      RowTex = TextTex.Select("TEXT_ID=" & i)
      If RowTex Is Nothing OrElse RowTex.Length = 0 OrElse IsDBNull(RowTex(0)("TEXT_LBEZ")) Then
        Texxt = CStr(i)
      Else
        Texxt = RowTex(0)("TEXT_LBEZ")
      End If
    Else
      Texxt = RowUse(0)("TEXT_LBEZ")
    End If
  End Function
  Shared Function TexKt(ByRef i As Integer) As String
    If Not IsNothing(TextUse) Then
      RowUse = TextUse.Select("TEXT_ID=" & i)
    End If
    If RowUse Is Nothing OrElse RowUse.Length = 0 OrElse IsDBNull(RowUse(0)("TEXT_KBEZ")) Then
      RowTex = TextTex.Select("TEXT_ID=" & i)
      If RowTex Is Nothing OrElse RowTex.Length = 0 OrElse IsDBNull(RowTex(0)("TEXT_KBEZ")) Then
        TexKt = CStr(i)
      Else
        TexKt = RowTex(0)("TEXT_KBEZ")
      End If
    Else
      TexKt = RowUse(0)("TEXT_KBEZ")
    End If
  End Function
  Private Shared Sub TabBuild(ByVal TextTab As DataTable, ByVal NamTab As String)
    Dim CmdTex As New OleDbCommand
    Dim DatAdaptText As New OleDbDataAdapter
    CmdTex.Connection = Cncol()
    CmdTex.CommandText = "SELECT * FROM " & NamTab
    DatAdaptText.SelectCommand = CmdTex
    If FillDatset(DatAdaptText, TextTab) Then
      TextTab.Columns("TEXT_ID").Unique = True
      CmdTex.Dispose()
      DatAdaptText.Dispose()
      Cncol.Close()
    End If
  End Sub
  Shared Function Texstr(ByRef Index As Integer) As String
    If Index = 9999 Then
      Texstr = "falsch"
    Else
      Texstr = Texxt(Index)
    End If
  End Function
  Shared Sub UpdateLangText(ByRef TABText As DataTable, StartID As Integer, KeyID As String, KeyTexkt As String, keyTexxt As String)
    Dim i As Integer
    Dim ID As Integer
    Dim KeykLength As Integer
    Dim KeylLength As Integer
    Dim KeyZwi As String
    If KeyTexkt <> "" Then
      KeykLength = TABText.Columns(KeyTexkt).MaxLength
    End If
    If keyTexxt <> "" Then
      KeylLength = TABText.Columns(keyTexxt).MaxLength
    End If
    For i = 0 To TABText.Rows.Count - 1
      ID = TABText.Rows(i)(KeyID)
      If KeyTexkt <> "" Then
        KeyZwi = TexKt(StartID + ID)
        If KeyZwi.Length > KeykLength Then
          KeyZwi = KeyZwi.Substring(0, KeykLength)
        End If
        TABText.Rows(i)(KeyTexkt) = KeyZwi
      End If
      If keyTexxt <> "" Then
        KeyZwi = Texxt(StartID + ID)
        If KeyZwi.Length > KeylLength Then
          KeyZwi = KeyZwi.Substring(0, KeylLength)
        End If
        TABText.Rows(i)(keyTexxt) = KeyZwi
      End If
    Next
  End Sub
  Shared Sub RezToTab(ByVal KeyNam As String, ByRef RezSozpt As RecipesGrp, ByRef DatTab As DataTable)
    Dim i As Integer
    Dim FarbNam As String
    Dim Row As DataRow
    DatTab.Rows.Clear()
    For i = 0 To RezSozpt.Rezepte(KeyNam).KF - 1
      Row = DatTab.NewRow
      FarbNam = KeyName(RezSozpt.Rezepte(KeyNam)(i).ID)
      If DatTab.Columns.Contains("FARBM_ID") Then
        Row("FARBM_ID") = RezSozpt.Rezepte(KeyNam)(i).ID
      End If
      If DatTab.Columns.Contains("FARBM_NAME") Then
        Row("FARBM_NAME") = RezSozpt.Farben(FarbNam).Name
      End If
      If DatTab.Columns.Contains("FARBM_MENGE") Then
        Row("FARBM_MENGE") = RezSozpt.Rezepte(KeyNam)(i).BaAmng
      End If
      If DatTab.Columns.Contains("FARBM_PROZ") Then
        Row("FARBM_PROZ") = RezSozpt.Rezepte(KeyNam)(i).Proz
      End If
      If DatTab.Columns.Contains("FARBM_PROB") Then
        Row("FARBM_PROB") = RezSozpt.Rezepte(KeyNam)(i).Prob
      End If
      If DatTab.Columns.Contains("FARBM_PREIS") Then
        Row("FARBM_PREIS") = RezSozpt.Farben(FarbNam).Preis
      End If
      If DatTab.Columns.Contains("FARBM_FST") Then
        Row("FARBM_FST") = RezSozpt.Farben(FarbNam).Fst
      End If
      If DatTab.Columns.Contains("FARBM_TOPF") Then
        Row("FARBM_TOPF") = RezSozpt.Farben(FarbNam).Kto
      End If
      If DatTab.Columns.Contains("FARBM_OPERAT") Then
        Row("FARBM_OPERAT") = RezSozpt.Farben(FarbNam).OP
      End If
      If DatTab.Columns.Contains("FARBM_LIMMNG") Then
        Row("FARBM_LIMMNG") = RezSozpt.Farben(FarbNam).BoMng
      End If
      If DatTab.Columns.Contains("FARBM_ICHF") Then
        Row("FARBM_ICHF") = RezSozpt.Farben(FarbNam).Ichf
      End If
      If DatTab.Columns.Contains("FARBM_FARBID") Then
        Row("FARBM_FARBID") = RezSozpt.Farben(FarbNam).FarbID
      End If
      DatTab.Rows.Add(Row)
    Next
    DatTab.AcceptChanges()
  End Sub
  Shared Sub TabToRez(ByVal KeyNam As String, ByRef RezSozpt As RecipesGrp, ByRef DatTab As DataTable)
    Dim i As Integer
    Dim FarbNam As String
    If KeyNam = "" Then Exit Sub
    RezSozpt.Rezepte(KeyNam).clear()
    DatTab.AcceptChanges()
    For i = 0 To DatTab.Rows.Count - 1
      FarbNam = KeyName(CInt(DatTab.Rows(i)("FARBM_ID")))
      RezSozpt.Rezepte(KeyNam).AddFaNr(KeyRe(i), New ColorAmount)
      If DatTab.Columns.Contains("FARBM_ID") Then
        RezSozpt.Rezepte(KeyNam)(i).ID = DatTab.Rows(i)("FARBM_ID")
      End If
      If DatTab.Columns.Contains("FARBM_NAME") Then
        RezSozpt.Farben(FarbNam).Name = DatTab.Rows(i)("FARBM_NAME")
      End If
      If DatTab.Columns.Contains("FARBM_MENGE") Then
        RezSozpt.Rezepte(KeyNam)(i).BaAmng = DatTab.Rows(i)("FARBM_MENGE")
      End If
      If DatTab.Columns.Contains("FARBM_PROZ") Then
        RezSozpt.Rezepte(KeyNam)(i).Proz = DatTab.Rows(i)("FARBM_PROZ")
      End If
      If DatTab.Columns.Contains("FARBM_PROB") Then
        RezSozpt.Rezepte(KeyNam)(i).Prob = DatTab.Rows(i)("FARBM_PROB")
      End If
      If DatTab.Columns.Contains("FARBM_PREIS") Then
        RezSozpt.Farben(FarbNam).Preis = DatTab.Rows(i)("FARBM_PREIS")
      End If
      If DatTab.Columns.Contains("FARBM_FST") Then
        RezSozpt.Farben(FarbNam).Fst = DatTab.Rows(i)("FARBM_FST")
      End If
      If DatTab.Columns.Contains("FARBM_TOPF") Then
        RezSozpt.Farben(FarbNam).Kto = DatTab.Rows(i)("FARBM_TOPF")
      End If
      If DatTab.Columns.Contains("FARBM_OPERAT") Then
        RezSozpt.Farben(FarbNam).OP = DatTab.Rows(i)("FARBM_OPERAT")
      End If
      If DatTab.Columns.Contains("FARBM_LIMMNG") Then
        RezSozpt.Farben(FarbNam).BoMng = DatTab.Rows(i)("FARBM_LIMMNG")
      End If
      If DatTab.Columns.Contains("FARBM_ICHF") Then
        RezSozpt.Farben(FarbNam).Ichf = DatTab.Rows(i)("FARBM_ICHF")
      End If
      If DatTab.Columns.Contains("FARBM_FARBID") Then
        RezSozpt.Farben(FarbNam).FarbID = DatTab.Rows(i)("FARBM_FARBID")
      End If
    Next i
  End Sub
  '
  Shared Sub CreateCopyDatabase(ByRef Ende As Boolean)
    Dim imsg As Short
    Dim DbName As String
    Dim FilNaa As String
    Ende = False
    DbName = Cndat.DataSource
    imsg = MsgBox(Texxt(2929), 4, Texxt(2000))

    If imsg = 7 Then
      FilNaa = ""

      FilNaa = NewFileName(Cndat.DataSource, "COLORCOPDATA.MDB")
      FilNaa = InputBox(Texxt(2035), Texxt(2000), FilNaa)
      If FilNaa = "" Then
        Ende = True
        Exit Sub
      End If

      FilNaa = InputBox(Texxt(3675), Texxt(2000), FilNaa)
      If FilNaa = "" Or InStr(FilNaa, "*") > 0 Then
        imsg = MsgBox(Texxt(3615) & Chr(13) & Texxt(1999), 4, Texxt(2000))
        If imsg = 7 Then
          Ende = True
          Exit Sub
        End If
      End If


      If Not File.Exists(FilNaa) Then
        Err.Clear()
        Try

          File.Copy(DbName, FilNaa, False)
          MsgBox(Texxt(3611))
        Catch ex As Exception
          MsgBox(Texxt(3612) & Space(2) & ex.Message)
          Exit Sub
        End Try
      Else
        MsgBox(Texxt(3616))
        Ende = True
      End If
    End If
  End Sub
  '
  '
  'Tooltip-Verwaltung
  '
  'Public Shared Sub CreateTooltip(ByVal FRM As Form, ByRef TOOLT As ToolTip)
  ' Dim i As Integer
  ' Dim Namtab As String = "TBL_TEXTE_TOOLTIP"
  '   If Not TableExists(Namtab, Cncol) Then Exit Sub
  '   If Not BitWrt(20, MenueParam.User.Sonst) Then Exit Sub
  ' Dim CmdTex As New OleDbCommand
  ' Dim DatAdaptText As New OleDbDataAdapter
  '   TOOLT = New ToolTip
  '   TOOLT.AutoPopDelay = 2000
  '   TOOLT.ShowAlways = True
  '   TableToolTip = New DataTable
  '   CmdTex.Connection = Cncol()
  '   CmdTex.CommandText = "SELECT * FROM " & Namtab & " WHERE FORM='" & FRM.Name & "'"
  '   DatAdaptText.SelectCommand = CmdTex
  '   If FillDatset(DatAdaptText, TableToolTip) Then
  '     CmdTex.Dispose()
  '     DatAdaptText.Dispose()
  '     Cncol.Close()
  '   End If
  '   Call ToolControl(FRM, FRM.Controls, TOOLT)
  '   TableToolTip.Dispose()
  'End Sub
  'Private Shared Sub ToolControl(ByVal FRM As Form, ConAll As System.Windows.Forms.Control.ControlCollection, ByRef TOOLT As ToolTip)
  ' Dim i As Integer
  ' Dim j As Integer
  '   If IsNothing(ConAll) OrElse ConAll.Count = 0 Then Exit Sub
  '   For i = 0 To ConAll.Count - 1
  '     For j = 0 To TableToolTip.Rows.Count - 1
  '       If TableToolTip.Rows(j)("CONTROL") <> "" And ConAll(i).Name = TableToolTip.Rows(j)("CONTROL") Then
  '         TOOLT.SetToolTip(ConAll(i), TableToolTip.Rows(j)("TEXT_TEXT").replace("|", vbCrLf))
  '       ElseIf BitWrt(21, MenueParam.User.Sonst) And ConAll(i).Name <> "" Then
  '         TOOLT.SetToolTip(ConAll(i), FRM.Name & " + " & ConAll(i).Name)
  '       End If
  '     Next
  '     Call ToolControl(FRM, ConAll(i).Controls, TOOLT)
  '   Next
  ' End Sub
  ' 
  '
End Class