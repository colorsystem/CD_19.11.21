Option Strict Off
Option Explicit On
Option Compare Text

Friend Module modColorBasis
  Friend FormMDI As Object
  Friend MnPrintSet As PrinterSettings
  Friend WithEvents GetPutReflex As HandleRwerte
  Friend Refel As RefValue
  Friend MnResz As Boolean
  Friend ReWrRwert As ReadWriteRwert
  '
  '
  Function AddDelBCS(ByRef TxtMess As String, ByRef TxtPrint As String, ByRef Txt0 As String, ByRef Txt1 As String, ByRef Txt2 As String) As Short
    'frmBCSMessage.DefInstance.Text = TxtMess
    'frmBCSMessage.DefInstance.lblMessage.Text = TxtPrint
    'frmBCSMessage.DefInstance.cmdAUSW(0).Text = Txt0
    'frmBCSMessage.DefInstance.cmdAUSW(1).Text = Txt1
    'frmBCSMessage.DefInstance.cmdAUSW(2).Text = Txt2
    'frmBCSMessage.DefInstance.ShowDialog()
    'AddDelBCS = frmBCSMessage.DefInstance.CmdIndex
  End Function




  Function WinUSeqMe(ByRef WinUse As AngGeos, ByRef WinMes As AngGeos) As Boolean
    Dim i As Short
    WinUSeqMe = True
    If WinUse.Km <> WinMes.Km Then
      WinUSeqMe = False
      Exit Function
    End If
    For i = 0 To WinMes.Km - 1
      If WinUse(i).Chrm <> WinMes(i).Chrm Then
        WinUSeqMe = False
        Exit Function
      End If
    Next i
  End Function





  Sub ClipboardToTable(ByVal WithHeader As Boolean, ByRef DatTab As DataTable, ByVal SplString As String, ByRef ier As Integer)
    '
    Dim Istart As Integer
    Dim i As Integer
    Dim srReadExcel As StringReader
    Dim RowNew As DataRow
    Dim arrSplitData As Array
    Dim sFormattedData As String
    ier = 0
    '
    'Daten von Clipboard übernehmen
    '
    srReadExcel = New StringReader(Clipboard.GetText)

    'Prüfen, ob Daten vorhanden
    '
    If srReadExcel.Peek < 0 Then
      ier = -1
      Exit Sub
    End If
    DatTab.Rows.Clear()
    '
    'Loop, solange Daten vorhanden
    '
    '
    Istart = 0
    While (srReadExcel.Peek() > 0)
      '
      'Zeilen lesen
      '
      '
      sFormattedData = srReadExcel.ReadLine()
      '
      '
      '
      sFormattedData = sFormattedData.Replace(SplString, Space(1))
      '
      '

      '
      '
      'TAB's werden durch SplString(z.B. = ";") ersetzt und in Array umgespeichert
      '
      arrSplitData = sFormattedData.Replace(Chr(9), SplString).Split(SplString)
      If DatTab.Columns.Count < arrSplitData.Length Then
        ier = -2
        Exit Sub
      End If
      If WithHeader And Istart = 0 Then
        For i = 0 To DatTab.Columns.Count - 1
          If i < arrSplitData.Length Then
            DatTab.Columns(i).Caption = arrSplitData.GetValue(i)
          Else
            Exit For
          End If
        Next i
      Else
        RowNew = DatTab.NewRow
        For i = 0 To DatTab.Columns.Count - 1
          If i < arrSplitData.Length Then
            RowNew(i) = arrSplitData.GetValue(i)
          Else
            Exit For
          End If
        Next
        DatTab.Rows.Add(RowNew)
      End If

      Istart = Istart + 1
    End While

    srReadExcel.Dispose()

  End Sub
  Sub SetStdQucontrol(ByRef GrpRwerte As RefValuesGrp)
    Dim k As Integer
    Dim i As Integer
    '
    For k = 0 To GrpRwerte.Count - 1
      For i = 0 To GrpRwerte(k).Count - 1
        If IsNothing(GrpRwerte(k)(i).QuControl) Then
          GrpRwerte(k)(i).QuControl = New QuControls
          If i = 0 Then
            GrpRwerte(k)(i).QuControl.Cart = "@T$" & GrpRwerte.RwArt(k)
          Else
            GrpRwerte(k)(i).QuControl.Cart = "@P$" & GrpRwerte.RwArt(k)
          End If
        End If
      Next i
    Next k
  End Sub
  Sub SpeiRwertCalc(RewrRwert As ReadWriteRwert, ByVal GrpRwerte As RefValuesGrp, Gid As Integer, ByRef ier As Integer)
    Dim k As Short
    Dim ID As Integer
    Dim i As Short
    Dim HlfText As String
    For i = 0 To 1
      If GrpRwerte(i)("R").IVoNa Then
        GrpRwerte(i)("R").Banum = ""
        GrpRwerte(i)("R").Iami = 1
        For k = 0 To MenueParam.Messg.Winkel.Km - 1
          GrpRwerte(i)("R").De(k) = 0.0#
        Next k
        GrpRwerte(i)("R").DatTim = Date.Now
        GrpRwerte(i)("R").Cme = MenueParam.Messg.Kenn
        GrpRwerte(i)("R").Iarch = 0
        GrpRwerte(i)("R").ReTr = BitInt(i, i, MenueParam.Messg.MeArtID)
        GrpRwerte(i)("R").Gid = Gid
        GrpRwerte(i)("R").MessgID = MenueParam.MessgID
        HlfText = InputBox(Texxt(370), Texxt(2000), GrpRwerte(i)("R").Name)
        If HlfText <> "" Then
          GrpRwerte(i)("R").Name = HlfText
          Call ReWrRwert.WriteRwert(ID, GrpRwerte(i)("R"), ier)
        End If
      End If
    Next i
  End Sub
End Module

