Option Strict Off
Option Explicit On
Option Compare Text
Public Class frmColorGrund
  Inherits System.Windows.Forms.Form
  Dim Lrez As Short = 0
  Dim AllFa As Short
  Dim AlStp As Short
  Dim i100 As Short
  Dim Iprn As Integer
  Dim Ipra As Integer
  Dim Nsort() As Short
  Dim Rmii As Single
  Dim Rmaa As Single
  Dim StrWS As String
  Dim KeyRez As String
  Dim KeyMenge As String
  Dim SqlStmt As String
  Dim Strgid As String
  Dim StrHilf As String
  Dim Zwisch As String
  Dim GkSperr As Short
  Dim IndexWin As Short
  Dim OldMischID As Integer = -1
  Dim ReWrFarbe As ReadWriteFarbe
  Dim ReWrGrund As ReadWriteGrund
  Dim RezSozpt As RecipesGrp
  Dim GrpRwerteM As RefValuesGrp
  Dim GrpRwerteR As RefValuesGrp
  Dim FawrtRwerte As RefValuesGrp
  Dim GesStreuAbs As Colorants
  Dim ArbFarb As Colorant
  Dim FarbCollection As Colorants
  Dim RwWrRezept As ReadWriteRezept
  Dim ReWrRwert As ReadWriteRwert
  Dim RezGraphics As HandleRezGrafik
  Dim Picauf As HandlePictures
  Dim RezCheckRad As HandleCheckRad
  Dim HandleRezept As HandleRezGeneral
  Dim HandleRwrt As HandleRwerte
  Dim GKmin() As Single = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.1, -1.0, 0.0, 0.0, 0.0, 0.0, 0.0}
  Dim GKmax() As Single = {0.05, 0.05, 0.99, 1.0, 1.0, 1.0, 1.0, 3.0, 5.0, 5.0, 1.0, 4.0, 2.0, 2.0, 2.0, 2.0}
  Dim CalcRezept As RezeptBerechnung
  Dim FarbWrt As ValuesGrpsAssigns
  Dim quali As QualKontrolle
  Dim RezGrid As HandleRezC1Screen
  Dim DatenFarb As HandleRezDsetFarb
  Dim PanGrund As List(Of Panel)
  Dim cmdSPE As List(Of Button)
  Dim cmdWei As List(Of Button)
  Dim optAnz As List(Of RadioButton)
  Dim optGrd As List(Of RadioButton)
  Dim RadUntergrund As List(Of RadioButton)
  Dim txtVarianz As List(Of TextBox)
  Dim ConzDi() As Single
  Dim ConzDiAdd() As Single
  Dim Rwert() As Single
  Dim chkPlottWin As List(Of CheckBox)
  Dim chkWinkVergleich As List(Of CheckBox)
  Dim chkGrundVergleich As List(Of CheckBox)
  Dim RadZusatz As List(Of RadioButton)
  '
  '
  '
  '
  Dim Col() As Color
  Dim Anz As Integer
  Dim AchsText(2) As String
  Dim PunktMouse() As Point
  Dim ToolText() As String
  Dim Val() As Single
  '
  '
  Dim UntID(1) As Integer
  Dim TypID(1) As Integer
  Dim SmpID(1) As Integer
  Dim KeyD As String

  Dim Kwan As Short
  Dim Chcmd As Boolean
  Dim NF As Short
  Dim Nanz As Short
  Dim Nrwrt As Short
  Dim Kfm As Short
  Dim KFFI As Short
  Dim KFVA As Short
  Dim Phi As Single
  Dim Icheck As Short
  Dim Krez As Short
  Dim Kgrd As Short
  Dim hlf() As Byte
  Dim RzID() As Integer
  Dim RwKey() As String
  Dim TooltipDreieck As ToolTip

  'Anzahl Reflexionskurven (+Zusatzinformation) QU-Kontrolle
  '
  Dim ier As Integer
  '
  Dim isw(6) As Boolean
  '
  '
  Dim Namm() As String
  Dim NPS As Short
  Dim Itext() As Integer
  '
  Dim DbAdapt As OleDbDataAdapter
  Dim DbCommand As OleDbCommand
  Dim AllgAdapt As OleDbDataAdapter
  Dim ALLGRead As OleDbDataReader
  Dim AllgCommand As OleDbCommand
  Dim TabGroup As DataTable
  Dim TabDySr As DataTable
  Dim TabDySq As DataTable
  Dim TabDySn As DataTable
  Dim TabDysm As DataTable
  Dim TabDyset As DataTable
  Dim TabFVA As DataTable
  Dim TabFFI As DataTable
  Dim TabAvail As DataTable
  Dim TblMessgeraet As DataTable
  Dim TabMix As DataTable
  Dim TabCalc As DataTable
  Dim TabAlle As DataTable
  Dim TabZusatz As DataTable
  Dim TabCopyMisch As DataTable
  Dim ViewCalc As DataView
  Dim TabMessRech As DataTable
  Dim TabRez As DataTable
  Dim TabMisch As DataTable
  Dim TabMischAll As DataTable
  Dim ViewBindWeiss As DataView
  Dim ViewBindWeissAdd As DataView

  Dim WithEvents ConnCalc As BindingSource '
  '
  ' Laufvariable
  Dim i As Integer
  Dim l As Integer
  Dim k As Integer
  Dim j As Integer
  Dim kw As Integer
  ' Zwischengroessen
  Dim nkw As Integer
  'Message Kennung
  Dim imsg As Integer 'Messagebox (Rückgabewert)
  '
  '
  Dim ChCancel As Integer
 
  Sub CalcFarbWrt()
    Dim k As Integer
    Dim ITP() As Boolean
    Dim Ivona() As Boolean
    Dim Nall As Integer
    Nall = GrpRwerteM(0).Count + GrpRwerteM(1).Count
    FawrtRwerte("F").clear()
    ReDim ITP(2 * Nall - 1)
    ReDim Ivona(2 * Nall - 1)
    For k = 0 To Nall - 1

      StrWS = RwKey(k).Substring(0, 1)
      KeyRez = RwKey(k).Substring(1, 3)
      FawrtRwerte("F").Add("M" & RwKey(k), GrpRwerteM(StrWS)(KeyRez))
      ITP(2 * k) = FawrtRwerte("F")(2 * k).Itp
      Ivona(2 * k) = FawrtRwerte("F")(2 * k).IVoNa
      FawrtRwerte("F")(2 * k).Itp = True
      FawrtRwerte("F")(2 * k).IVoNa = True
      '
      FawrtRwerte("F").Add("R" & RwKey(k), GrpRwerteR(StrWS)(KeyRez))
      ITP(2 * k + 1) = FawrtRwerte("F")(2 * k + 1).Itp
      Ivona(2 * k + 1) = FawrtRwerte("F")(2 * k + 1).IVoNa
      FawrtRwerte("F")(2 * k + 1).Itp = False
      FawrtRwerte("F")(2 * k + 1).IVoNa = True
    Next k
    FarbWrt.clear()
    AufbauPar.AufbauRezeptMerk(FarbWrt, ier)
    Call quali.FarbWrtCalc(MenueParam.Messg.Winkel, FawrtRwerte, FarbWrt, ier)
    '
    'ITP und Ivona werden zurückgesetzt
    '
    '
    For k = 0 To 2 * Nall - 1
      FawrtRwerte("F")(k).Itp = ITP(k)
      FawrtRwerte("F")(k).IVoNa = Ivona(k)
    Next k
    Erase ITP
    Erase Ivona
  End Sub

  Sub PanVisible(ByVal Ivi As Integer)
    Dim i As Integer
    For i = 0 To PanGrund.Count - 1
      PanGrund(i).Visible = False
    Next
    cmdKOP.Visible = False
    If Ivi >= 0 Then
      PanGrund(Ivi).Visible = True
    End If
    If Ivi = 0 Then
      Call ListFarbm()
    End If
    Application.DoEvents()
  End Sub
  Sub FillMesRech(ByRef Ikenn As Short, ByRef RezSozpt As RecipesGrp, ByRef GesStreuAbs As Colorants, ByRef TabMessRech As DataTable, ByRef flgMessRech As DataGridView, ByRef ier As Short)
    Dim i As Short
    Dim l As Short
    Dim j As Short
    Dim k As Short
    Dim kw As Short
    Dim WinKey As String
    Dim nwe As Short
    Dim Nanz As Short
    Dim StrRw As String
    Dim KeyNr As String
    Dim RwertHilf As RefValuesGrp
    Dim TypID(1) As Integer
    Dim SmpID(1) As Integer
    Dim UntID(1) As Integer
    Dim RowWrt As DataRow
    Dim FaId As Integer
    TabMessRech.Columns.Clear()
    TabMessRech.Rows.Clear()

    nwe = MenueParam.Messg.Winkel.Wsol.Nwe
    If Ikenn = 1 Then
      RwertHilf = GrpRwerteM
    ElseIf Ikenn = 2 Then
      RwertHilf = GrpRwerteR
    ElseIf Ikenn = 3 Then
      RwertHilf = New RefValuesGrp
      RwertHilf.Add(GrpRwerteM.RwArt(0), New RefValues)
      RwertHilf.Add(GrpRwerteM.RwArt(1), New RefValues)
      RwertHilf(0).RefUnt = GrpRwerteM(0).RefUnt
      RwertHilf(1).RefUnt = GrpRwerteM(1).RefUnt
      For l = 0 To UBound(RwKey)
        StrRw = RwKey(l).Substring(0, 1)
        KeyNr = RwKey(l).Substring(1, 3)
        RwertHilf(StrRw).Add(KeyNr, New RefValue)
        RwertHilf(StrRw)(KeyNr).Iplott = GrpRwerteM(StrRw)(KeyNr).Iplott
        RwertHilf(StrRw)(KeyNr).Gid = GrpRwerteM(StrRw)(KeyNr).Gid
        RwertHilf(StrRw)(KeyNr).Iami = GrpRwerteM(StrRw)(KeyNr).Iami
        RwertHilf(StrRw)(KeyNr).Iarch = GrpRwerteM(StrRw)(KeyNr).Iarch
        RwertHilf(StrRw)(KeyNr).ID = GrpRwerteM(StrRw)(KeyNr).ID
        RwertHilf(StrRw)(KeyNr).Itp = GrpRwerteM(StrRw)(KeyNr).Itp
        RwertHilf(StrRw)(KeyNr).IVoNa = GrpRwerteM(StrRw)(KeyNr).IVoNa
        RwertHilf(StrRw)(KeyNr).kwb = GrpRwerteM(StrRw)(KeyNr).kwb
        RwertHilf(StrRw)(KeyNr).Name = GrpRwerteM(StrRw)(KeyNr).Name
        RwertHilf(StrRw)(KeyNr).Nr = GrpRwerteM(StrRw)(KeyNr).Nr
        RwertHilf(StrRw)(KeyNr).ReTr = GrpRwerteM(StrRw)(KeyNr).ReTr
        RwertHilf(StrRw)(KeyNr).Banum = GrpRwerteM(StrRw)(KeyNr).Banum
        RwertHilf(StrRw)(KeyNr).Bem = GrpRwerteM(StrRw)(KeyNr).Bem
        RwertHilf(StrRw)(KeyNr).Cme = GrpRwerteM(StrRw)(KeyNr).Cme
        RwertHilf(StrRw)(KeyNr).DatTim = GrpRwerteM(StrRw)(KeyNr).DatTim
        '
        Call ADDCurves(RwertHilf(StrRw)(KeyNr).RefKurv)

        For k = 0 To MenueParam.Messg.Winkel.Km - 1
          'WinKey = GrpRwerteM(StrRw)(KeyNr).RefKurv.Winkey(k)
          'RwertHilf(StrRw)(KeyNr).RefKurv.Add(WinKey, New CurveRef(nwe))
          WinKey = MenueParam.Messg.Winkel(k).Chrm
          For i = 0 To nwe - 1
            RwertHilf(StrRw)(KeyNr).RefKurv(WinKey).R(i) = GrpRwerteM(StrRw)(KeyNr).RefKurv(WinKey).R(i) - GrpRwerteR(StrRw)(KeyNr).RefKurv(WinKey).R(i)
          Next i
        Next k
      Next l
    End If

    Select Case Ikenn
      Case 1, 2, 3
        '
        '
        '
        'Tabelle TabMessREch aufbauen
        '
        '
        TabMessRech.Columns.Add(Texxt(3659))
        TabMessRech.Columns.Add(Texxt(824))
        TabMessRech.Columns.Add(Texxt(916) & "/" & Texxt(602))
        TabMessRech.Columns.Add(Texxt(375))
        TabMessRech.Columns.Add(MenueParam.Messg.Kbez & "/" & Texxt(893))
        For i = 0 To MenueParam.Messg.Winkel.Wsol.Nwe - 1
          TabMessRech.Columns.Add(CStr(MenueParam.Messg.Winkel.Wsol.R(i)))
        Next i
        '
        '
        'Rows aufbauen (Untergrund)
        '
        '
        '
        '
        For i = 0 To RwertHilf.Count - 1
          If RwertHilf(i).RefUnt.IVoNa And RwertHilf(i).RefUnt.ID > -1 Then
            j = -1 * (RwertHilf(i).RefUnt.kwb + 1)
            Call HandleRwrt.TabRefRecord(j, MenueParam.Messg.Winkel, 100.0, RwertHilf(i).RefUnt, TabMessRech, ier)
            If ier > 0 Then Exit Sub
          End If
        Next i
        '
        '


        '
        For l = 0 To UBound(RwKey)
          '
          '
          'Tabelle aufbauen
          StrRw = RwKey(l).Substring(0, 1)
          KeyNr = RwKey(l).Substring(1, 3)
          '
          RowWrt = TabMessRech.NewRow
          '
          '
          RowWrt(0) = "0"
          Nanz = RwertHilf(StrRw)(KeyNr).Nr
          RowWrt(1) = RezSozpt.Rezepte(KeyNr).Name
          RowWrt(2) = RezSozpt.Rezepte(KeyNr).Bem
          RowWrt(3) = RezSozpt.Rezepte(KeyNr).DatTim
          RowWrt(4) = RezSozpt.Rezepte(KeyNr).Dicke(RwertHilf(StrRw)(KeyNr).kwb)
          TabMessRech.Rows.Add(RowWrt)

          For i = 0 To RezSozpt.Rezepte(KeyNr).KF - 1
            RowWrt = TabMessRech.NewRow
            FaId = RezSozpt.Rezepte(KeyNr)(i).ID
            RowWrt(0) = "#" & CStr(FaId)
            RowWrt(1) = RezSozpt.Farben(KeyName(FaId)).Name
            RowWrt(2) = Format(RezSozpt.Rezepte(KeyNr)(i).FaAmng, RezSozpt.Farben(KeyName(FaId)).Form)
            TabMessRech.Rows.Add(RowWrt)
          Next i
          '
          '
          'Messung/Rechnung
          '
          '
          '
          j = System.Math.Abs(RwertHilf(StrRw)(KeyNr).kwb) + 1
          Call HandleRwrt.TabRefRecord(j, MenueParam.Messg.Winkel, 100.0, RwertHilf(StrRw)(KeyNr), TabMessRech, ier)
          If ier > 0 Then Exit Sub
          '
          '
          '
        Next l
        '
        'Gridparameter
        '
        '
        '
        flgMessRech.AlternatingRowsDefaultCellStyle.BackColor = Color.WhiteSmoke
        flgMessRech.Columns(0).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleLeft
        flgMessRech.Columns(1).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleLeft
        flgMessRech.Columns(2).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
        flgMessRech.Columns(3).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleLeft
        flgMessRech.Columns(4).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
        flgMessRech.Columns(0).Width = 50
        flgMessRech.Columns(1).Width = 150
        flgMessRech.Columns(2).Width = 125
        flgMessRech.Columns(3).Width = 125
        flgMessRech.Columns(4).Width = 100
        For i = 0 To MenueParam.Messg.Winkel.Wsol.Nwe - 1
          flgMessRech.Columns(i + 5).Width = 50
          flgMessRech.Columns(i + 5).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
        Next i
        flgMessRech.Columns(0).Frozen = True
        flgMessRech.Columns(1).Frozen = True
        flgMessRech.Columns(2).Frozen = True
        flgMessRech.Columns(3).Frozen = True
        flgMessRech.Columns(4).Frozen = True
        '
        '
      Case 4
        '
        '
        '
        '
        'optische Daten
        '
        '
        '
        '
        '
        '
        'Tabelle aufbauen
        '
        '
        '
        '
        TabMessRech.Columns.Add("0", GetType(String))
        TabMessRech.Columns.Add("1", GetType(String))
        TabMessRech.Columns.Add("2", GetType(String))
        TabMessRech.Columns.Add("3", GetType(String))
        TabMessRech.Columns.Add(Trim(MenueParam.Messg.Kbez))
        flgMessRech.Columns(0).HeaderText = " "
        flgMessRech.Columns(1).HeaderText = " "
        flgMessRech.Columns(2).HeaderText = " "
        flgMessRech.Columns(3).HeaderText = " "

        For i = 0 To nwe - 1
          TabMessRech.Columns.Add(Format(MenueParam.Messg.Winkel.Wsol.R(i), "###"))
        Next i
        '
        '
        'Rows hinzufügen
        '
        '
        For k = 0 To RezSozpt.Farben.FarbCount - 1
          KeyD = KeyName(RezSozpt.Farben(k).ID)
          Call NammText(KeyD, Namm)
          For j = 0 To RezSozpt.Farben(k).OptData.Grund.Count - 1
            For kw = 0 To RezSozpt.Farben(k).OptData.Grund(j).Count - 1
              RowWrt = TabMessRech.NewRow
              RowWrt(1) = RezSozpt.Farben(k).Name
              RowWrt(2) = Namm(j)
              RowWrt(4) = RezSozpt.Farben(k).OptData.Grund(j).Winkey(kw)
              For i = 0 To nwe - 1
                RowWrt(5 + i) = HandleRezept.AutFormat(RezSozpt.Farben(k).OptData.Grund(j)(kw).R(i))
              Next i
              TabMessRech.Rows.Add(RowWrt)
            Next kw
          Next j
        Next k
        '
        '
      Case 5
        '
        '
        '
        '
        'optische Daten (gesamt)
        '
        '
        '
        '
        '
        '
        'Tabelle aufbauen
        '
        '
        '
        '
        TabMessRech.Columns.Add("0", GetType(String))
        TabMessRech.Columns.Add("1", GetType(String))
        TabMessRech.Columns.Add("2", GetType(String))
        TabMessRech.Columns.Add("3", GetType(String))
        TabMessRech.Columns.Add(Trim(MenueParam.Messg.Kbez))
        flgMessRech.Columns(0).HeaderText = " "
        flgMessRech.Columns(1).HeaderText = " "
        flgMessRech.Columns(2).HeaderText = " "
        flgMessRech.Columns(3).HeaderText = " "

        For i = 0 To nwe - 1
          TabMessRech.Columns.Add(Format(MenueParam.Messg.Winkel.Wsol.R(i), "###"))
        Next i
        '
        '
        'Rows hinzufügen
        '
        '
        For k = 0 To GesStreuAbs.FarbCount - 1
          KeyD = KeyName(GesStreuAbs(k).ID)
          Call NammText(KeyD, Namm)
          For j = 0 To GesStreuAbs(k).OptData.Grund.Count - 1
            For kw = 0 To GesStreuAbs(k).OptData.Grund(j).Count - 1
              RowWrt = TabMessRech.NewRow
              'Weiß oder schwarz
              RowWrt(0) = Texxt(891 + GesStreuAbs(k).Ibas)
              RowWrt(1) = GesStreuAbs(k).Name
              RowWrt(2) = Namm(j)
              RowWrt(3) = GesStreuAbs(k).Bem
              RowWrt(4) = GesStreuAbs(k).OptData.Grund(j).Winkey(kw)
              For i = 0 To nwe - 1
                RowWrt(5 + i) = HandleRezept.AutFormat(GesStreuAbs(k).OptData.Grund(j)(kw).R(i))
              Next i
              TabMessRech.Rows.Add(RowWrt)
            Next kw
          Next j
        Next k
        '
        '
    End Select
    '
    '
    'Gridparameter
    '
    '
    '
    'flgMessRech.Cols.Fixed = 0
    flgMessRech.Columns(0).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleLeft
    flgMessRech.Columns(1).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleLeft
    flgMessRech.Columns(2).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    flgMessRech.Columns(3).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    flgMessRech.Columns(4).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    flgMessRech.Columns(0).Width = 50
    flgMessRech.Columns(1).Width = 50
    flgMessRech.Columns(2).Width = 50
    flgMessRech.Columns(3).Width = 50
    flgMessRech.Columns(4).Width = 100
    For i = 0 To MenueParam.Messg.Winkel.Wsol.Nwe - 1
      flgMessRech.Columns(i + 5).Width = 50
      flgMessRech.Columns(i + 5).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    Next i
    '
    flgMessRech.RowHeadersVisible = False
    For i = 0 To flgMessRech.Columns.Count - 1
      flgMessRech.Columns(i).SortMode = DataGridViewColumnSortMode.NotSortable
    Next

    RwertHilf = Nothing
  End Sub











  Private Sub cboSKAL_SelectedIndexChanged(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles cboSKAL.SelectedIndexChanged
    If cboSKAL.Enabled = False Then Exit Sub
    Rmii = -1.0#
    Picauf.Refresh()

  End Sub



  Sub FillRezTab(ByRef Nla As Short, ByRef kwopt As Short, ByRef AbsValue As Boolean, ByVal TabRez As DataTable)
    Dim Ku As Short
    Dim k As Integer
    Dim j As Integer
    Dim i As Integer
    'L*,C*,h,a*,b*
    Dim ABSFarb() As String = {"BA", "BB", "BC", "BD", "BE"}
    'DE*,DL*,DC*,DH*,META
    Dim RelFarb() As String = {"BJ", "BK", "BL", "BM", "BI"}
    Dim RowWrt As DataRow
    If Nla < 0 Then Exit Sub
    If kwopt < 0 Then Exit Sub
    If IsNothing(RezSozpt.Farben) Then Exit Sub
    If RezSozpt.Farben.FarbCount = 0 Then Exit Sub
    '
    '
    TabRez.Rows.Clear()
    TabRez.Columns.Clear()
    '
    '
    'Spalten
    '
    '
    '
    If TabRez.Columns.Count = 0 Then
      TabRez.Columns.Add("NAME", GetType(String))
      If AbsValue Then
        For i = 0 To ABSFarb.Count - 1
          TabRez.Columns.Add(ABSFarb(i), GetType(String))
        Next i
      Else
        For i = 0 To RelFarb.Count - 1
          TabRez.Columns.Add(RelFarb(i), GetType(String))
        Next i
      End If
      TabRez.Columns.Add("DICK", GetType(String))
      For i = 0 To Kfm - 1
        KeyD = KeyName(RezSozpt.Farben(i).ID)
        TabRez.Columns.Add(KeyD, GetType(String))
      Next
      TabRez.Columns.Add("UNT", GetType(String))
      TabRez.Columns.Add("X", GetType(String))
      '
      'Überschrift
      '
      '
      TabRez.Columns("NAME").Caption = Texxt(931)
      If AbsValue Then
        For i = 0 To ABSFarb.Count - 1
          TabRez.Columns(ABSFarb(i)).Caption = Trim(FarbWrt(1).Merk(ABSFarb(i)).Kbez)
        Next i
      Else
        For i = 0 To RelFarb.Count - 1
          TabRez.Columns(RelFarb(i)).Caption = Trim(FarbWrt(1).Merk(RelFarb(i)).Kbez)
        Next i
      End If

      'Dicke
      TabRez.Columns("DICK").Caption = Texxt(893)
      For i = 0 To Kfm - 1
        KeyD = KeyName(RezSozpt.Farben(i).ID)
        TabRez.Columns(KeyD).Caption = Trim(RezSozpt.Farben(KeyD).Name)
      Next
      TabRez.Columns("UNT").Caption = ""
      TabRez.Columns("X").Caption = ""
    End If

    For k = 0 To GrpRwerteM(0).Count + GrpRwerteM(1).Count - 1
      RowWrt = TabRez.NewRow
      StrWS = RwKey(k).Substring(0, 1)
      KeyRez = RwKey(k).Substring(1, 3)
      RowWrt(0) = Trim(RezSozpt.Rezepte(KeyRez).Name)

      If AbsValue Then
        For i = 0 To ABSFarb.Count - 1
          RowWrt(i + 1) = FarbWrt(0)(2 * k)(Nla)(kwopt)(ABSFarb(i))
        Next i
      Else
        For i = 0 To RelFarb.Count - 1
          RowWrt(i + 1) = FarbWrt(0)(2 * k + 1)(Nla)(kwopt)(RelFarb(i))
        Next i
      End If

      Ku = GrpRwerteM(StrWS)(KeyRez).kwb
      'RowWrt(6) = Format(RezSozpt.Rezepte(KeyRez).Dicke(Ku), "###.00")
      RowWrt(6) = Format(GrpRwerteR(StrWS)(KeyRez).Dik(kwopt), "###.00")


      For i = 0 To Kfm - 1
        RowWrt(7 + i) = Space(1)
        For j = 0 To RezSozpt.Rezepte(KeyRez).KF - 1
          If RezSozpt.Rezepte(KeyRez)(j).ID = RezSozpt.Farben(i).ID Then
            RowWrt(7 + i) = Format(RezSozpt.Rezepte(KeyRez)(j).FaAmng, RezSozpt.Farben(i).Form)
          End If
        Next j
      Next i
      RowWrt(7 + Kfm) = Texxt(891 + Ku)
      RowWrt(7 + Kfm + 1) = Space(1)
      If Not GrpRwerteM(StrWS)(KeyRez).IVoNa Then
        RowWrt(7 + Kfm + 1) = "X"
      End If
      TabRez.Rows.Add(RowWrt)
    Next k
    flgREZ.AlternatingRowsDefaultCellStyle.BackColor = Color.WhiteSmoke
    For i = 0 To flgREZ.Columns.Count - 1
      flgREZ.Columns(i).HeaderText = TabRez.Columns(i).Caption
      flgREZ.Columns(i).SortMode = DataGridViewColumnSortMode.NotSortable
    Next
    flgREZ.Columns(0).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleLeft
    flgREZ.Columns(0).Width = 150
    flgREZ.Columns(1).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    flgREZ.Columns(1).Width = 50
    flgREZ.Columns(2).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    flgREZ.Columns(2).Width = 50
    flgREZ.Columns(3).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    flgREZ.Columns(3).Width = 50
    flgREZ.Columns(4).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    flgREZ.Columns(4).Width = 50
    flgREZ.Columns(5).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    flgREZ.Columns(5).Width = 50
    flgREZ.Columns(6).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    flgREZ.Columns(6).Width = 50
    For i = 0 To Kfm - 1
      flgREZ.Columns(7 + i).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
      flgREZ.Columns(7 + i).Width = 50
    Next
    flgREZ.Columns(7 + Kfm).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleCenter
    flgREZ.Columns(7 + Kfm).Width = 25
    flgREZ.Columns(8 + Kfm).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleCenter
    flgREZ.Columns(8 + Kfm).Width = 25
    flgREZ.Columns(0).Frozen = True

  End Sub

  Sub ListFarbm()
    '
    '
    'Alle Zusatzmittel werden gesucht
    '
    '
    SqlStmt = "SELECT FARBM_ID AS FARBM_ID,FARBM_NAME FROM TBL_FARBM WHERE MISCH_ID=" & MenueParam.MischID & " ORDER BY FARBM_NAME"
    TabAlle.Clear()
    DbCommand.CommandText = SqlStmt
    If Not FillDatset(DbAdapt, TabAlle) Then
      Exit Sub
    End If
    '
    '
    SqlStmt = "SELECT FARBM_ID AS FARBM_ID,FARBM_NAME FROM TBL_FARBM WHERE FARBM_ICHF=8 AND MISCH_ID=" & MenueParam.MischID & " ORDER BY FARBM_NAME"
    TabZusatz.Clear()
    DbCommand.CommandText = SqlStmt
    If Not FillDatset(DbAdapt, TabZusatz) Then
      Exit Sub
    End If



    '
    '
    'Nur Farb-/Bindemittel für die Rezepte vorliegen werden gesucht (Rezeptgruppe ohne Bedeutung)
    'Keine Restfarben (ICHF=6) und Laborrezepte (ICHF=7)
    'Auch für Zusatzstoffe (ICHF=8) müssen Rezepte vorliegen
    '
    '
    '
    '
    'Vorhandenen Farbmittel mit Rezepten
    '
    '
    
    If MenueParam.Misch.UserRzpGID = 0 Then
      SqlStmt = "SELECT DISTINCT TBL_FARBM.FARBM_ID AS FARBM_ID," _
       & " TBL_FARBM.FARBM_NAME AS FARBM_NAME,TBL_FARBM.FARBM_ICHF AS FARBM_ICHF FROM " _
       & " (TBL_FARBM INNER JOIN TBL_REZEPT_FARBM ON (TBL_FARBM.FARBM_ID=TBL_REZEPT_FARBM.FARBM_ID) AND (TBL_FARBM.MISCH_ID=TBL_REZEPT_FARBM.MISCH_ID))" _
       & " INNER JOIN TBL_REZEPT_RWERT ON (TBL_REZEPT_FARBM.REZEPT_ID=TBL_REZEPT_RWERT.REZEPT_ID) AND (TBL_REZEPT_FARBM.MISCH_ID=TBL_REZEPT_RWERT.MISCH_ID)" _
       & " WHERE TBL_REZEPT_FARBM.MISCH_ID=" & MenueParam.MischID & " AND (FARBM_ICHF<>6 AND FARBM_ICHF<>7) AND MESSGRW_ID=" & MenueParam.Messg.MessgRwID & " ORDER BY FARBM_NAME"
    Else
      SqlStmt = "SELECT DISTINCT TBL_FARBM.FARBM_ID AS FARBM_ID," _
      & " TBL_FARBM.FARBM_NAME AS FARBM_NAME,TBL_FARBM.FARBM_ICHF AS FARBM_ICHF FROM " _
      & " ((TBL_FARBM INNER JOIN TBL_REZEPT_FARBM ON (TBL_FARBM.FARBM_ID=TBL_REZEPT_FARBM.FARBM_ID) AND (TBL_FARBM.MISCH_ID=TBL_REZEPT_FARBM.MISCH_ID))" _
      & " INNER JOIN TBL_REZEPT ON (TBL_REZEPT_FARBM.REZEPT_ID=TBL_REZEPT.REZEPT_ID) AND (TBL_REZEPT_FARBM.MISCH_ID=TBL_REZEPT.MISCH_ID))" _
      & " INNER JOIN TBL_REZEPT_RWERT ON (TBL_REZEPT_FARBM.REZEPT_ID=TBL_REZEPT_RWERT.REZEPT_ID) AND (TBL_REZEPT_FARBM.MISCH_ID=TBL_REZEPT_RWERT.MISCH_ID)" _
      & " WHERE TBL_REZEPT.REZEPT_GID=" & MenueParam.Misch.UserRzpGID & " AND TBL_REZEPT_FARBM.MISCH_ID=" & MenueParam.MischID _
      & " AND (FARBM_ICHF<>6 AND FARBM_ICHF<>7) AND MESSGRW_ID=" & MenueParam.Messg.MessgRwID & " ORDER BY FARBM_NAME"
    End If
    TabAvail.Clear()
    TabFVA.Clear()
    DbCommand.CommandText = SqlStmt
    If Not FillDatset(DbAdapt, TabAvail) Then
      Exit Sub
    End If
    lstAvail.DisplayMember = "FARBM_NAME"
    lstAvail.ValueMember = "FARBM_ID"
    lstCopyNach.DisplayMember = "FARBM_NAME"
    lstCopyNach.ValueMember = "FARBM_ID"
    '

    '
    '

    'Bereits berechnete Farbmittel
    '
    SqlStmt = "SELECT DISTINCT TBL_FARBM.FARBM_ID AS FARBM_ID, TBL_FARBM.FARBM_NAME AS FARBM_NAME,FARBM_FIX,GLZGRD_ID,FARBM_GLZGRD" _
    & " FROM (TBL_FARBM INNER JOIN TBL_GRUND_FARBM ON (TBL_GRUND_FARBM.MISCH_ID=TBL_FARBM.MISCH_ID) AND (TBL_GRUND_FARBM.FARBM_ID=TBL_FARBM.FARBM_ID))" _
    & " WHERE TBL_FARBM.MISCH_ID = " & MenueParam.MischID & " AND TBL_GRUND_FARBM.GKWRT_ID=" & MenueParam.Misch.GKwrtID _
    & " AND TBL_GRUND_FARBM.MESSGRW_ID=" & MenueParam.Messg.MessgRwID & " ORDER BY FARBM_NAME"
    TabCalc.Clear()
    DbCommand.CommandText = SqlStmt
    If Not FillDatset(DbAdapt, TabCalc) Then
      Exit Sub
    End If
    TabCalc.AcceptChanges()
    lstCalc.DisplayMember = "FARBM_NAME"
    lstCalc.ValueMember = "FARBM_ID"
    lstCopyVon.DisplayMember = "FARBM_NAME"
    lstCopyVon.ValueMember = "FARBM_ID"

    ConnCalc = New BindingSource
    ConnCalc.DataSource = ViewCalc
    BindingPlott.BindingSource = ConnCalc
    cboGrundPlott.DataSource = ConnCalc
    cboGrundPlott.DisplayMember = "FARBM_NAME"
    cboGrundPlott.ValueMember = "FARBM_ID"
    dbgGridFarbmittel.DataSource = ViewCalc
    Call dbgGridFCust(dbgGridFarbmittel)
    '
    '
    '
    '
    '
    '
    'Bereits berechnete und als "fest" gekennzeichnete Farbmittel
    '
    SqlStmt = "SELECT DISTINCT TBL_FARBM.FARBM_ID AS FARBM_ID," & " TBL_FARBM.FARBM_NAME AS FARBM_NAME,FARBM_FIX" _
    & " FROM (TBL_FARBM INNER JOIN TBL_GRUND_FARBM ON (TBL_GRUND_FARBM.MISCH_ID=TBL_FARBM.MISCH_ID) AND " & " (TBL_GRUND_FARBM.FARBM_ID=TBL_FARBM.FARBM_ID))" _
    & " WHERE TBL_FARBM.MISCH_ID = " & MenueParam.MischID & " AND TBL_GRUND_FARBM.GKWRT_ID=" & MenueParam.Misch.GKwrtID _
    & " AND TBL_GRUND_FARBM.MESSGRW_ID=" & MenueParam.Messg.MessgRwID & " AND FARBM_FIX=1 ORDER BY FARBM_NAME"
    TabFFI.Clear()
    DbCommand.CommandText = SqlStmt
    If Not FillDatset(DbAdapt, TabFFI) Then
      Exit Sub
    End If
    lstFFI.DisplayMember = "FARBM_NAME"
    lstFFI.ValueMember = "FARBM_ID"
    '
    '
    'Liste der noch zu berechneten Farb-/Bindemittel
    '
    '
    '

    lstFVA.DisplayMember = "FARBM_NAME"
    lstFVA.ValueMember = "FARBM_ID"
    '
    '
    '
    '
    '
    '
    'Für Mischungspartner R(C*d)-Darstellung bzw. R-Kurven
    '
    '
    If MenueParam.Misch.UserRzpGID = 0 Then
      SqlStmt = "SELECT DISTINCT TBL_FARBM.FARBM_ID AS FARBM_ID," _
       & " TBL_FARBM.FARBM_NAME AS FARBM_NAME," & " TBL_FARBM.FARBM_ICHF AS FARBM_ICHF FROM " _
       & " (TBL_FARBM INNER JOIN " & " TBL_REZEPT_FARBM ON (TBL_FARBM.FARBM_ID=TBL_REZEPT_FARBM.FARBM_ID) AND " & " (TBL_FARBM.MISCH_ID=TBL_REZEPT_FARBM.MISCH_ID))" _
       & " WHERE TBL_REZEPT_FARBM.MISCH_ID=" & MenueParam.MischID & " AND (FARBM_ICHF=1 OR FARBM_ICHF=2 OR FARBM_ICHF=3 OR FARBM_ICHF=4) ORDER BY FARBM_NAME"
    Else
      SqlStmt = "SELECT DISTINCT TBL_FARBM.FARBM_ID AS FARBM_ID," _
      & " TBL_FARBM.FARBM_NAME AS FARBM_NAME," & " TBL_FARBM.FARBM_ICHF AS FARBM_ICHF FROM " _
      & " (TBL_FARBM INNER JOIN " & " TBL_REZEPT_FARBM ON (TBL_FARBM.FARBM_ID=TBL_REZEPT_FARBM.FARBM_ID) AND " & " (TBL_FARBM.MISCH_ID=TBL_REZEPT_FARBM.MISCH_ID))" _
      & " INNER JOIN TBL_REZEPT ON (TBL_REZEPT_FARBM.REZEPT_ID=TBL_REZEPT.REZEPT_ID) AND (TBL_REZEPT_FARBM.MISCH_ID=TBL_REZEPT.MISCH_ID)" _
      & " WHERE TBL_REZEPT.REZEPT_GID=" & MenueParam.Misch.UserRzpGID & " AND TBL_REZEPT_FARBM.MISCH_ID=" & MenueParam.MischID & " AND (FARBM_ICHF=1 OR FARBM_ICHF=2 OR FARBM_ICHF=3 OR FARBM_ICHF=4) ORDER BY FARBM_NAME"
    End If
    TabMix.Clear()
    DbCommand.CommandText = SqlStmt
    If Not FillDatset(DbAdapt, TabMix) Then
      Exit Sub
    End If

  End Sub
  Private Sub chkABS_CheckedChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles chkABS.CheckedChanged
    Call FillRezTab(cboNORML.SelectedIndex, cboWIN.SelectedIndex, chkABS.Checked, TabRez)

  End Sub
  'UPGRADE_WARNING: Event cboNORML.SelectedIndexChanged may fire when form is initialized. Click for more: 'ms-help://MS.VSExpressCC.v80/dv_commoner/local/redirect.htm?keyword="88B12AE1-6DE0-48A0-86F1-60C0686C026A"'
  Private Sub cboNORML_SelectedIndexChanged(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles cboNORML.SelectedIndexChanged
    Call FillRezTab(cboNORML.SelectedIndex, cboWIN.SelectedIndex, chkABS.Checked, TabRez)

  End Sub


  Private Sub cboWIN_SelectedIndexChanged(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles cboWIN.SelectedIndexChanged
    Call FillRezTab(cboNORML.SelectedIndex, cboWIN.SelectedIndex, chkABS.Checked, TabRez)

  End Sub


  Private Sub chkGRD_CheckStateChanged(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles chkGRD.CheckStateChanged, chkGRD.EnabledChanged
    If Not chkGRD.Enabled Then Exit Sub
    If chkGRD.CheckState = 1 Then
      picGRD.Visible = True
      picREF.Visible = False
      cboSKAL.Visible = False
      lblUNU.Visible = False
      lblUNT.Visible = False
      lblDIK.Visible = False
      lblRZN.Visible = False
      lblRWE.Visible = False
      RezGrid.TDBFar.Visible = False
      txtSUM.Visible = False
      lblDIU.Visible = False
      lblRZU.Visible = False
      lblRWU.Visible = False
      lblREZ.Visible = False
      hscREZ.Visible = False
      hscGRD.Visible = False
      hscGRD.Visible = True
      hscGRD.Value = 0
      Call hscGRD_Scroll(hscGRD, New System.Windows.Forms.ScrollEventArgs(ScrollEventType.EndScroll, 0))
    Else
      picREF.Visible = True
      picGRD.Visible = False
      cboSKAL.Visible = True
      lblUNU.Visible = True
      lblUNT.Visible = True
      lblDIK.Visible = True
      lblRZN.Visible = True
      lblRWE.Visible = True
      TDBFar.Visible = True
      txtSUM.Visible = True
      lblDIU.Visible = True
      lblRZU.Visible = True
      lblRWU.Visible = True
      lblREZ.Visible = True
      hscREZ.Visible = False
      hscGRD.Visible = False
      hscREZ.Visible = True
      hscREZ.Value = 0
      Call hscREZ_Scroll(hscREZ, New System.Windows.Forms.ScrollEventArgs(ScrollEventType.EndScroll, 0))
    End If
  End Sub




  Private Sub cmdABR_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles cmdABR.Click
    cmdVER.Enabled = True
    Lrez = Lrez + 9999
    cmdVER.PerformClick()
  End Sub

  Private Sub cmdGRI_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles cmdGRI.Click

    Call PanVisible(2)
    cmdKOP.Visible = True

    cboNORML.SelectedIndex = -1
    cboWIN.SelectedIndex = -1


    '
    '
    'Farbwerte berechnen
    '
    '
    '
    '
    Call CalcFarbWrt()
    '
    '
    '
    '
    '
    '
    '
    '
    cboNORML.SelectedIndex = 0
    cboWIN.SelectedIndex = 0


  End Sub
  Private Sub cmdKOP_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles cmdKOP.Click
    Clipboard.Clear()
    If flgMessREch.Visible Then
      '
      'Tabellen für R-Werte oder Grunddaten in Zwischenablage
      '
      '
      '
      flgMessREch.ClipboardCopyMode = DataGridViewClipboardCopyMode.EnableAlwaysIncludeHeaderText
      flgMessREch.SelectAll()

      If flgMessREch.GetCellCount(DataGridViewElementStates.Selected) > 0 Then
        Clipboard.SetDataObject(flgMessREch.GetClipboardContent)
      End If
      flgMessREch.ClearSelection()
    ElseIf flgREZ.Visible Then
      '
      'Tabellen für Farbwerte und Rezepte in Zwischenablage
      '
      '
      '
      flgREZ.RowHeadersVisible = False
      flgREZ.ClipboardCopyMode = DataGridViewClipboardCopyMode.EnableAlwaysIncludeHeaderText
      flgREZ.SelectAll()

      If flgREZ.GetCellCount(DataGridViewElementStates.Selected) > 0 Then
        Clipboard.SetDataObject(flgREZ.GetClipboardContent)
      End If
      flgREZ.ClearSelection()
      flgREZ.RowHeadersVisible = True
    End If
  End Sub

  Private Sub cmdREC_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles cmdREC.Click
    Dim i As Short
    Dim kw As Short
    Dim j As Integer
    Call PanVisible(-1)
    lblARB.Visible = True
    Application.DoEvents()
    Cursor = System.Windows.Forms.Cursors.WaitCursor
    cmdABR.Enabled = False
    cmdTAB.Enabled = False
    For i = 0 To cmdSPE.Count - 1
      cmdSPE(i).Enabled = False
    Next i
    cmdKOP.Enabled = False
    cmdVER.Enabled = False
    cmdGRI.Enabled = False
    If chkAGR.CheckState = 1 Then
      RezSozpt.Farben = RezSozpt.FarbAux.clone
    End If
    Ipra = 2 * chkGlaett.CheckState + 8 + chkAGR.CheckState
    Call CalcRezept.GrundDaten(Ipra, MenueParam.Messg.Winkel, RezSozpt, GrpRwerteM, GrpRwerteR, GesStreuAbs, ier)
    For j = 0 To RezSozpt.Farben.FarbCount - 1
      KeyD = KeyName(RezSozpt.Farben(j).ID)
      Call NammText(KeyD, Namm)
      For i = 0 To UBound(Namm)
        For kw = 0 To RezSozpt.Farben(KeyD).OptData.Grund(i).Count - 1
          RezSozpt.Farben(KeyD).OptData.Grund(i)(kw).NamPlott = Namm(i)
        Next
      Next
    Next
    Cursor = System.Windows.Forms.Cursors.Arrow
    cmdABR.Enabled = True
    cmdTAB.Enabled = True
    For i = 0 To cmdSPE.Count - 1
      cmdSPE(i).Enabled = True
    Next i
    cmdKOP.Enabled = True
    cmdVER.Enabled = True
    cmdGRI.Enabled = True
    Call PanVisible(2)
    lblARB.Visible = False
    Call CalcFarbWrt()

    Call FillRezTab(cboNORML.SelectedIndex, cboWIN.SelectedIndex, chkABS.Checked, TabRez)
    cmdKOP.Visible = True
  End Sub

  Private Sub cmdSPE_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles cmdSPE_0.Click, _
    cmdSPE_1.Click, cmdSPE_2.Click
    Dim DiaRes As DialogResult
    Dim Index As Short
    Dim i As Integer
    Dim FaID As Integer
    Dim NewName As String
    Dim AltName As String
    Dim AltID As Integer
    Dim NewID As Integer

    Index = CInt(eventSender.name.substring(7, 1))
    cmdKOP.Visible = False
    If Index < 2 Then
      If optAnz(0).Checked Then
        Chcmd = HandleRezept.AddDelP(2100)
      End If
      If Chcmd Or optAnz(1).Checked Then
        If MenueParam.Misch.GKwrtID = 0 Then
          DiaRes = MessageBox.Show(Texxt(2957), Texxt(2000), MessageBoxButtons.YesNo)
          If DiaRes = Forms.DialogResult.No Then
            Exit Sub
          End If
        End If
        For i = Lrez To Lrez + AlStp
          KeyD = KeyName(TabFVA.Rows(i)("FARBM_ID"))
          RezSozpt.Farben(KeyD).OptData.Fest = Index
          NewID = -1
          If chkFARB.CheckState = 1 Then
            NewName = InputBox(Texxt(815), Texxt(2000), RezSozpt.Farben(KeyD).Name)
            If NewName <> "" Then
              Call ReWrFarbe.ReadFarbmName(NewName, FaID, ier)
              If FaID = -1 Then
                '
                '
                '
                '
                'Neuer Name in Farb-/Bindemitteldatei
                '
                '
                '
                AltName = RezSozpt.Farben(KeyD).Name
                AltID = RezSozpt.Farben(KeyD).ID
                RezSozpt.Farben(KeyD).Name = NewName
                RezSozpt.Farben(KeyD).GlzGrdID = -1
                RezSozpt.Farben(KeyD).GlzGrd = 0.0
                Call ReWrFarbe.FarAdd(RezSozpt.Farben(KeyD), ier)
                NewID = RezSozpt.Farben(KeyD).ID
                RezSozpt.Farben(KeyD).Name = AltName
                RezSozpt.Farben(KeyD).ID = AltID
              Else
                If FaID <> NewID Then
                  'MsgBox(Texxt(2954) & ": " & NewName)
                  NewID = FaID
                End If
              End If
              'ALLGRead.Close()
            Else
              NewID = -1
            End If
          Else
            NewID = RezSozpt.Farben(KeyD).ID
          End If
          If NewID > -1 Then
            Call ReWrGrund.WriteGrund(NewID, RezSozpt.Farben(KeyD).OptData, MenueParam.Messg.Winkel, ier)
          End If
        Next i
        If MenueParam.Messg.GKsperr = 0 Then
          SqlStmt = "UPDATE TBL_GKWRT SET SPERR=1 WHERE GKWRT_ID=" & MenueParam.Misch.GKwrtID
          AllgCommand.CommandText = SqlStmt
          If SQLExeNonQuery(AllgCommand, Cncol) <> 0 Then
          End If
        End If
      End If
    End If
    Lrez = Lrez + AlStp + 1
    cmdVER.Enabled = True
    cmdVER.PerformClick()
    optGRD_0.Enabled = True
    optGRD_1.Enabled = True
  End Sub




  Private Sub cmdTAB_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles cmdTAB.Click
    Call PanVisible(3)
    cmdKOP.Visible = True
    cmdWei_1.PerformClick()
  End Sub

  Private Sub cmdVER_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles cmdVER.Click
    Dim RezID As Integer
    Dim KeyWS(1) As String
    Dim KeyRwrt(1) As String
    Dim FaID As Integer
    Dim Dicke As Single
    Dim Zero As Single = 0.0001
    Dim Lnanz As Short
    Dim i As Integer
    Dim j As Integer
    Dim k As Short
    Dim KF As Integer
    Dim jk As Integer
    Dim kw As Integer
    Dim check As Boolean
    Dim Chech As Boolean
    Dim CheFa As Boolean
    Dim ChRwrt As Boolean
    Dim KeyID As String
    If optGrd(0).Checked Then
      '
      ' Einzelverarbeitung
      '
      '
      AllFa = lstFVA.Items.Count - 1
      AlStp = 0
    Else

      '
      'Gesamtverarbeitung
      '
      AllFa = lstFVA.Items.Count - 1
      AlStp = lstFVA.Items.Count - 1
    End If
    If AllFa < 0 Then
      MsgBox(Texxt(4014))
      Exit Sub
    End If

    If Lrez > AllFa Then
      Call PanVisible(0)
      cmdGRI.Enabled = False
      cmdABR.Enabled = False
      cmdTAB.Enabled = False
      For i = 0 To 2
        cmdSPE(i).Enabled = False
      Next i
      cmdVER.Enabled = True
      optGrd(0).Enabled = True
      optGrd(1).Enabled = True
      Lrez = 0
      Exit Sub
    End If
    Cursor = Cursors.WaitCursor
    '
    '
    '
    'UPGRADE_WARNING: Couldn't resolve default property of object MDform.mnuEI. Click for more: 'ms-help://MS.VSExpressCC.v80/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
    'FormMDI.seteinst = False
    cmdVER.Enabled = False
    optGrd(0).Enabled = False
    optGrd(1).Enabled = False
    picREF.Visible = True
    Krez = 0
    '
    Erase RzID
    Erase RwKey

    cmdVER.Enabled = False
    '
    '

    '
    '
    'Tabellen für ausgewählte Farb-/Bindemittel erstellen
    '
    '
    '
    '
    'Chefa = true ====> Rezept hat gültiges Farb-/Bindemittel
    'Check=true ======> Kein Rezepteintrag mit ungültigem Farb-/Bindemittel gefunden
    'CHECH=true ======> Es ist ein variables Farb-/Bindemittel gefunden worden
    '
    'Set TblRez = Ddat.OpenRecordset(Menueparam.TblRezept, dbOpenTable, dbConsistent, dbReadOnly)
    ChCancel = 0
    '
    'Farbmittel löschen
    '
    '
    RezSozpt.Farben.clear()
    '
    '
    'Farbmittel für Zusatzstoffe hnzufügen
    '
    '
    If ConnOpen(Cndat) Then
      For i = Lrez To Lrez + AlStp
        SqlStmt = "SELECT * FROM TBL_FARBM WHERE MISCH_ID=" & MenueParam.MischID & " AND FARBM_ID=" & TabFVA.Rows(i)("FARBM_ID") _
        & " AND FARBM_ICHF=8"
        AllgCommand.CommandText = SqlStmt
        ALLGRead = DataReader(AllgCommand, CommandBehavior.SingleRow, Cndat)
        If ALLGRead.Read Then
          'Neues Farbmittel hinzufügen
          '
          '
          KeyD = KeyName(TabFVA.Rows(i)("FARBM_ID"))
          '
          RezSozpt.Farben.AddFarb(KeyD, New Colorant)
          '
          'von Tabelle TBL_FARBM einlesen
          '
          '
          ReWrFarbe.FarReaGrund(TabFVA.Rows(i)("FARBM_ID"), RezSozpt.Farben(KeyD), False, ier)
          '
          '
          '
        End If
        ALLGRead.Close()
      Next i
      Cndat.Close()
    End If

    '
    'Rezepte löschen
    '
    '
    RezSozpt.Rezepte.clear()
    '
    '
    '
    '
    'Rwerte löschen
    '
    '
    GrpRwerteM(0).clear()
    GrpRwerteM(1).clear()
    GrpRwerteR(0).clear()
    GrpRwerteR(1).clear()
    For k = 0 To 1
      GrpRwerteM(k).RefUnt.Nr = -1
      GrpRwerteM(k).RefUnt.ID = -1
      GrpRwerteM(k).RefUnt.Iplott = False
    Next k
    '
    '
    '
    '
    lblARB.Visible = True
    Call PanVisible(-1)
    'sstVis(sstGRUND, False)

    'System.Windows.Forms.Application.DoEvents()
    '
    If cboGRP.SelectedValue <= 0 Then
      Strgid = ""
    Else
      Strgid = " REZEPT_GID= " & cboGRP.SelectedValue & " AND"
    End If
    If txtSUC.Text = "" Then
      SqlStmt = "SELECT REZEPT_ID FROM TBL_REZEPT WHERE MISCH_ID=" & MenueParam.MischID & " AND " & Strgid & " REZEPT_NAME LIKE '%'"
    Else
      Zwisch = StrFil(txtSUC.Text, "'")
      SqlStmt = "SELECT REZEPT_ID FROM TBL_REZEPT WHERE MISCH_ID=" & MenueParam.MischID & " AND " & Strgid & " REZEPT_NAME LIKE '" & Zwisch & "'"
    End If
    '
    SqlStmt = "SELECT REZEPT_ID,TBL_REZEPT_FARBM.FARBM_ID, TBL_REZEPT_FARBM.FARBM_MENGE AS FARBM_MENGE, TBL_FARBM.FARBM_ICHF, TBL_FARBM.FARBM_NAME" & " FROM TBL_REZEPT_FARBM INNER JOIN TBL_FARBM ON (TBL_REZEPT_FARBM.FARBM_ID =TBL_FARBM.FARBM_ID) AND " & "(TBL_REZEPT_FARBM.MISCH_ID =TBL_FARBM.MISCH_ID)" & " WHERE TBL_REZEPT_FARBM.MISCH_ID=" & MenueParam.MischID & " AND REZEPT_ID IN (" & SqlStmt & ") ORDER BY REZEPT_ID"
    AllgCommand.CommandText = SqlStmt
    ALLGRead = DataReader(AllgCommand, CommandBehavior.Default Or CommandBehavior.CloseConnection, Cndat)
    Lnanz = 0
    j = 0
    RezID = -1
    Chech = False
    check = True
    CheFa = False
    Do While ALLGRead.Read
      If RezID <> ALLGRead("rezept_id") Then
        If check And Chech Then
          ReDim Preserve RzID(j)
          RzID(j) = RezID
          j = j + 1
        End If
        Chech = False
        check = True
        RezID = ALLGRead("rezept_id")
      End If
      If ALLGRead("Farbm_Menge") > Zero Then
        If check Then
          CheFa = False
          For i = Lrez To Lrez + AlStp
            If ALLGRead("Farbm_ID") = TabFVA.Rows(i)("FARBM_ID") Then
              CheFa = True
              Chech = True
              Exit For
            End If
          Next i
          If Not CheFa Then
            For i = 0 To lstFFI.Items.Count - 1
              If ALLGRead("Farbm_ID") = TabFFI.Rows(i)("FARBM_ID") Then
                CheFa = True
                Exit For
              End If
            Next i
          End If
          If Not CheFa Then
            check = False
          End If
        End If
      End If
    Loop
    If check And Chech Then
      If IsNothing(RzID) OrElse RezID <> RzID(j - 1) Then
        ReDim Preserve RzID(j)
        RzID(j) = RezID
        j = j + 1
      End If
    End If
    Lnanz = j
    '
    ALLGRead.Close()
    If Lnanz > 0 Or RezSozpt.Farben.FarbCount > 0 Then

      Nanz = 0
      Nrwrt = 0
      If ConnOpen(Cndat) Then
        For i = 0 To Lnanz - 1

          '
          'Farbmittel für Rezept
          '
          '
          Nanz = RezSozpt.Rezepte.RezCount
          '
          '
          'Rezept übernehmen
          '
          '
          '
          KeyRez = KeyRe(Nanz)
          RezSozpt.Rezepte.AddRez(KeyRez, New Recipe)
          RwWrRezept.ReadRezeptFarbGrund(KeyRez, RzID(i), RezSozpt, UntID, TypID, SmpID, ier)
          '
          '
          '
          'Farbmittel mit MENGE== eliminieren
          '
          '
          '
          For k = RezSozpt.Rezepte(KeyRez).KF - 1 To 0 Step -1

            If RezSozpt.Rezepte(KeyRez)(k).FaAmng < Zero Then
              RezSozpt.Rezepte(KeyRez).RemoveFaNr(KeyRe(k))
              '
              '
              'Farbmittel in Rezept verschieben
              '
              '
              KF = RezSozpt.Rezepte(KeyRez).KF
              For j = k To KF - 1
                RezSozpt.Rezepte(KeyRez).AddFaNr(KeyRe(j), RezSozpt.Rezepte(KeyRez)(KeyRe(j + 1)))
                RezSozpt.Rezepte(KeyRez).RemoveFaNr(KeyRe(j + 1))
              Next
            End If
          Next k

          '
          '
          If ier <> 0 Then
            MsgBox(Texxt(ier))
            GoTo NullGru
          End If


          If SmpID(0) < 0 And SmpID(1) < 0 Then
            imsg = MsgBox(Texxt(4017) & Chr(13) & Texxt(863) & RezSozpt.Rezepte(KeyRez).Name & Chr(13) & Texxt(1999), 4, Texxt(2000))
            If imsg = 7 Then
              Cndat.Close()
              GoTo NullGru
            End If
            RezSozpt.Rezepte.RemoveRez(KeyRez)
            Continue For
          End If
          If (RezSozpt.Rezepte(KeyRez).Dicke(0) = 0 And SmpID(0) >= 0) Then
            imsg = MsgBox(Texxt(2601) & ": " & Texxt(355) & ":  " & RezSozpt.Rezepte(KeyRez).Name & Chr(13) & Texxt(1999), 4, Texxt(2000))
            If imsg = 7 Then
              Cndat.Close()
              GoTo NullGru
            End If
            RezSozpt.Rezepte.RemoveRez(KeyRez)
            Continue For
          End If
          If (RezSozpt.Rezepte(KeyRez).Dicke(1) = 0 And SmpID(1) >= 0) Then
            imsg = MsgBox(Texxt(2601) & ": " & Texxt(356) & ":  " & RezSozpt.Rezepte(KeyRez).Name & Chr(13) & Texxt(1999), 4, Texxt(2000))
            If imsg = 7 Then
              Cndat.Close()
              GoTo NullGru
            End If
            RezSozpt.Rezepte.RemoveRez(KeyRez)
            Continue For
          End If
          If RezSozpt.Rezepte.RezCount = 0 Then
            MsgBox(Texxt(2958), 0)
            Cndat.Close()
            GoTo NextFarb
          End If
          If RezSozpt.Rezepte.ContainsKey(KeyRez) Then
            RezSozpt.Rezepte(KeyRez).Nr = Nanz
            '
            '
            '           R-Werte
            '
            ChRwrt = False
            For k = 0 To 1
              '
              '
              '             R-Werte Untergründe
              '
              '
              '
              If UntID(k) >= 0 Then
                If GrpRwerteM(k).RefUnt.ID < 0 Then
                  GrpRwerteM(k).RefUnt.ID = UntID(k)
                  ReWrRwert.ReadRwert(UntID(k), GrpRwerteM(k).RefUnt, ier)
                  GrpRwerteM(k).RefUnt.IVoNa = True
                  GrpRwerteM(k).RefUnt.Iplott = True

                Else
                  '
                  '
                  'Prüfen, ob bisherige Untergründe mit aktuellem Untergrund übereinstimmen
                  '
                  If UntID(k) <> GrpRwerteM(k).RefUnt.ID Then
                    If optAnz(0).Checked Then
                      MsgBox(Texxt(2501 + k) & Chr(13) & Texxt(863) & RezSozpt.Rezepte(KeyRez).Name, 0)
                    End If
                  End If
                End If
              End If
            Next k

            '
            '
            '
            '
            '
            '
            '
            'R-Werte Farbmittel
            '
            '
            '
            For k = 0 To 1
              If MenueParam.Misch.Transp Then
                If UntID(k) = -1 OrElse GrpRwerteM(k).RefUnt.ID = -1 OrElse UntID(k) <> GrpRwerteM(k).RefUnt.ID Then
                  If SmpID(k) >= 0 Then
                    MsgBox(Texxt(4015) & Chr(13) & Texxt(863) & RezSozpt.Rezepte(KeyRez).Name)
                    Cndat.Close()
                    GoTo NextFarb
                  Else
                    Continue For
                  End If
                End If
              End If
              Dicke = RezSozpt.Rezepte(KeyRez).Dicke(k)
              If Dicke > 0.0# And SmpID(k) >= 0 Then
                GrpRwerteM(k).Add(KeyRez, New RefValue)
                ReWrRwert.ReadRwert(SmpID(k), GrpRwerteM(k)(KeyRez), ier)
                If ier > 0 Then
                  If optAnz(0).Checked Then
                    imsg = MsgBox(Texxt(4016) & Chr(13) & Texxt(863) & RezSozpt.Rezepte(KeyRez).Name & Chr(13) & Texxt(1999), 4, Texxt(2000))
                    If imsg = 7 Then
                      Cndat.Close()
                      GoTo NullGru
                    End If
                  End If
                  RezSozpt.Rezepte.RemoveRez(KeyRez)
                  GrpRwerteM(k).Remove(KeyRez)
                Else
                  ChRwrt = True
                End If
                GrpRwerteM(k)(KeyRez).Nr = Nanz
                GrpRwerteM(k)(KeyRez).kwb = GrpRwerteM(k).RefUnt.kwb
                GrpRwerteM(k)(KeyRez).IVoNa = True
                GrpRwerteM(k)(KeyRez).Iplott = True
                ReDim Preserve RwKey(Nrwrt)
                RwKey(Nrwrt) = GrpRwerteM.RwArt(k) & KeyRez
                Nrwrt = Nrwrt + 1
              End If
            Next k
            If Not ChRwrt Then
              If optAnz(0).Checked Then
                imsg = MsgBox(Texxt(4016) & Chr(13) & Texxt(863) & RezSozpt.Rezepte(KeyRez).Name & Chr(13) & Texxt(1999), 4, Texxt(2000))
                If imsg = 7 Then
                  Cndat.Close()
                  GoTo NullGru
                End If
              End If
              RezSozpt.Rezepte.RemoveRez(KeyRez)
            End If
          Else
            RezSozpt.Rezepte.RemoveRez(KeyRez)
          End If
        Next i
        Cndat.Close()
      End If
      '
      '
      '
      'überflüssige Farbmittel eliminieren
      '
      '
      '
      CheFa = False
      For i = RezSozpt.Farben.FarbCount - 1 To 0 Step -1
        For k = 0 To RezSozpt.Rezepte.RezCount - 1
          For j = 0 To RezSozpt.Rezepte(k).KF - 1
            If RezSozpt.Rezepte(k)(j).ID = RezSozpt.Farben(i).ID Then Exit For
          Next j
          If j < RezSozpt.Rezepte(k).KF Then
            Exit For
          End If
        Next k
        If k >= RezSozpt.Rezepte.RezCount Then
          KeyID = KeyName(RezSozpt.Farben(i).ID)
          RezSozpt.Farben.RemoveFarb(KeyID)
        End If
      Next i


      '
      'Feste(KFFI) und variable(KFVA) Farb-/Bindemittel kennzeichnen
      '
      Iprn = chkAGR.CheckState
      KFFI = 0
      KFVA = 0
      For i = 0 To RezSozpt.Farben.FarbCount - 1
        If IsNothing(RezSozpt.Farben(i).OptData) Then
          Iprn = 0
          RezSozpt.Farben(i).OptData = New OpticalData
        End If
        If RezSozpt.Farben(i).Ichf = 8 Then
          '
          'Zusatzstoffe sind immer "fest"
          '
          RezSozpt.Farben(i).OptData.Fest = 1
          KFFI = KFFI + 1
        Else
          For j = 0 To TabFFI.Rows.Count - 1
            If RezSozpt.Farben(i).ID = TabFFI.Rows(j)("FARBM_ID") AndAlso RezSozpt.Farben(i).OptData.OptID >= 0 Then
              KeyD = KeyName(TabFFI.Rows(j)("FARBM_ID"))
              RezSozpt.Farben(KeyD).OptData.Fest = 1
              KFFI = KFFI + 1
              Exit For
            End If
          Next j
          For j = Lrez To Lrez + AlStp
            If RezSozpt.Farben(i).ID = TabFVA.Rows(j)("FARBM_ID") Then
              KeyD = KeyName(TabFVA.Rows(j)("FARBM_ID"))
              RezSozpt.Farben(KeyD).OptData.Fest = 0
              KFVA = KFVA + 1
              Exit For
            End If
          Next j
        End If
      Next i
      '
      '
      Cursor = Cursors.Default
      '
      '
      Kfm = KFFI + KFVA
      '
      '
      For i = 0 To RezSozpt.Rezepte.RezCount - 1
        If RezSozpt.Rezepte(i).KF > Kfm Then
          MsgBox(Texxt(2507))
          GoTo NullGru
        End If
      Next i

      '
      '
      '
      '
      '
      '
      'Prüfen, ob gleiche R-WerteID
      '
      '
      '
      If Not IsNothing(RwKey) Then
        For i = 0 To UBound(RwKey) - 1
          If GrpRwerteM(RwKey(i).Substring(0, 1))(RwKey(i).Substring(1, 3)).ID <> -1 Then
            For j = i + 1 To UBound(RwKey)
              KeyWS(0) = RwKey(i).Substring(0, 1)
              KeyRwrt(0) = RwKey(i).Substring(1, 3)
              KeyWS(1) = RwKey(j).Substring(0, 1)
              KeyRwrt(1) = RwKey(j).Substring(1, 3)
              If GrpRwerteM(KeyWS(0))(KeyRwrt(0)).ID = GrpRwerteM(KeyWS(1))(KeyRwrt(1)).ID Then
                imsg = MsgBox(Texxt(4012) & Chr(13) & RezSozpt.Rezepte(KeyRe((GrpRwerteM(KeyWS(0))(KeyRwrt(0)).Nr))).Name & Space(4) & RezSozpt.Rezepte(KeyRe((GrpRwerteM(KeyWS(1))(KeyRwrt(1)).Nr))).Name & Chr(13) & Texxt(1999), 4, Texxt(2000))
                If imsg = 7 Then
                  GoTo NextFarb
                End If
              End If
            Next j
          End If
        Next i
      End If

      '
      '
      '
      '
      '
      '
      '       Ergebnisse anzeigen; manuell abspeichern
      '
      '
      ier = 0
      '
      '
      '
      '
      '      Startwerte sind Standardwerte falls  menall.gralt= 0
      '
      'Menueparam.Misch.Gewr
      Cursor = Cursors.WaitCursor
      Application.DoEvents()
      If chkAGR.CheckState = 0 Then
        Call CalcRezept.GrundDaten(Iprn, MenueParam.Messg.Winkel, RezSozpt, GrpRwerteM, GrpRwerteR, GesStreuAbs, ier)
        If ier <> 0 Then
          Cursor = Cursors.Default
          GoTo NullGru
        End If
      Else
        RezSozpt.FarbAux = RezSozpt.Farben.clone
      End If
      Ipra = 2 * chkGlaett.CheckState + 8 + chkAGR.CheckState
      Call CalcRezept.GrundDaten(Ipra, MenueParam.Messg.Winkel, RezSozpt, GrpRwerteM, GrpRwerteR, GesStreuAbs, ier)
      Cursor = Cursors.Default
      If ier <> 0 Then
        GoTo NullGru
      End If
      For j = 0 To RezSozpt.Farben.FarbCount - 1
        KeyD = KeyName(RezSozpt.Farben(j).ID)
        Call NammText(KeyD, Namm)
        For i = 0 To UBound(Namm)
          For kw = 0 To RezSozpt.Farben(KeyD).OptData.Grund(i).Count - 1
            RezSozpt.Farben(KeyD).OptData.Grund(i)(kw).NamPlott = Namm(i)
          Next
        Next
      Next

      RezGrid.AllRezepte.Rezepte.AddRez(KeyMenge, New Recipe)
      CheFa = False
      check = False
      Chech = False

      If optAnz(1).Checked Then
        '
        '
        '
        'Automatisch abspeichern
        '
        cmdSPE_0.Enabled = True
        '
        cmdSPE_0.PerformClick()
        cmdSPE_0.Enabled = False
        Exit Sub
      End If


      lblARB.Visible = False
      Call PanVisible(1)
      Application.DoEvents()
      cmdGRI.Enabled = True
      cmdABR.Enabled = True
      cmdTAB.Enabled = True
      For i = 0 To 2
        cmdSPE(i).Enabled = True
      Next i
      '


      '
      ' Reflexionswerte d.h. Einzelrezepte anzeigen
      '
      '
      '
      '
      lblUNU.Visible = True
      lblUNT.Visible = True
      lblDIK.Visible = True
      lblRZN.Visible = True
      lblRWE.Visible = True
      RezGrid.TDBFar.Visible = True
      lblDIU.Visible = True
      lblRZU.Visible = True
      lblRWU.Visible = True
      lblREZ.Visible = True
      hscREZ.Visible = True
      hscGRD.Visible = False

      '
      '
      '          Rezept für keymenge="ZEI"
      '
      '
      hscREZ.LargeChange = 1
      hscREZ.Maximum = UBound(RwKey)
      hscREZ.Minimum = 0
      hscREZ.Value = hscREZ.Maximum
      hscREZ.Value = 0
      '
      '
      hscGRD.LargeChange = 1
      hscGRD.Maximum = Kfm - 1
      '          
      chkGRD.Enabled = True
      If GrpRwerteM.Count = 0 Then
        chkGRD.Enabled = False
        chkGRD.CheckState = CheckState.Checked
      Else
        chkGRD.CheckState = CheckState.Unchecked
      End If
      '
      '
    Else
      '
      '
      '  keine passenden Rezepte gefunden
      '
      '

      If AlStp = 0 Then
        MsgBox(Texxt(2958) & ": " & TabFVA.Rows(Lrez)("FARBM_NAME"))
      Else
        MsgBox(Texxt(2958), 0)
      End If
      GoTo NextFarb

    End If
    Exit Sub
NextFarb:
    Cursor = Cursors.Default
    lblARB.Visible = False
    cmdSPE_2.Enabled = True
    '
    cmdSPE_2.PerformClick()
    Exit Sub

    '
NullGru:
    Cursor = Cursors.Default
    lblARB.Visible = False
    cmdABR.Enabled = True
    cmdTAB.Enabled = True
    If Lrez > AllFa Then
      Exit Sub
    End If
    cmdABR.PerformClick()


  End Sub


 
  Sub NammText(ByRef KeyD As String, ByRef Namm() As String)
    Dim i As Integer
    Dim CCC As String
    Dim NST As Short
    Dim NPX As Short
    If CnzDep(MenueParam.Messg.CDE) = 1 And KeyD <> "" Then
      Call ItextGrund(MenueParam.Messg.CDE, Itext)
      NPX = NopNPX(MenueParam.Messg.CDE)
      NST = RezSozpt.Farben(KeyD).OptData.Nst
      NPS = NPNPS(MenueParam.Messg.CDE, NPX, NST)
      ReDim Namm(NPS - 1)
      For i = 0 To NPS - 2
        Namm(i) = Texxt(Itext(0)) & " (" & Format(RezSozpt.Farben(KeyD).OptData.Cst(i + 1), ".000000") & ")"
      Next i
      Namm(NPS - 1) = Texxt(Itext(1))
    ElseIf CnzDep(MenueParam.Messg.CDE) = 2 And KeyD <> "" Then
      Call ItextGrund(MenueParam.Messg.CDE, Itext)
      NPX = NopNPX(MenueParam.Messg.CDE)
      NST = RezSozpt.Farben(KeyD).OptData.Nst
      NPS = NPNPS(MenueParam.Messg.CDE, NPX, NST)
      ReDim Namm(NPS - 1)

      For i = 0 To NPS - 1
        If i <= NPS / 2 - 1 Then
          Namm(i) = Texxt(Itext(0)) & " (" & Format(RezSozpt.Farben(KeyD).OptData.Cst(i + 1), ".000000") & ")"
        Else
          Namm(i) = Texxt(Itext(1)) & " (" & Format(RezSozpt.Farben(KeyD).OptData.Cst(i + 1 - NPS / 2), ".000000") & ")"
        End If
      Next i
    Else
      CCC = MenueParam.Messg.CDE
      If KeyD = "" Then
        CCC = "T0"
      End If
      Call ItextGrund(CCC, Itext)
      ReDim Namm(UBound(Itext))
      For i = 0 To UBound(Itext)
        Namm(i) = Texxt(Itext(i))
      Next i
    End If
  End Sub
  Private Sub cmdWEI_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles cmdWei_0.Click, cmdWei_1.Click, cmdWei_2.Click, cmdWei_3.Click, cmdWei_4.Click, cmdWei_5.Click
    Dim Index As Short
    Index = CInt(eventSender.name.substring(7, 1))
    If Index = 0 Then
      Call PanVisible(2)
      Call CalcFarbWrt()
      Call FillRezTab(cboNORML.SelectedIndex, cboWIN.SelectedIndex, chkABS.Checked, TabRez)
      Exit Sub
    End If
    Call FillMesRech(Index, RezSozpt, GesStreuAbs, TabMessRech, flgMessREch, ier)
  End Sub

  Private Sub cmdZUR_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles cmdZUR.Click
    lblARB.Visible = False
    Call PanVisible(1)
  End Sub




  Private Sub flgREZ_RowHeaderMouseClick(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewCellMouseEventArgs) Handles flgREZ.RowHeaderMouseClick

    Dim ROwi As Integer
    ROwi = flgREZ.CurrentRow.Index
    hscREZ.Value = ROwi

    lblARB.Visible = False
    Call PanVisible(1)
  End Sub

  Private Sub flgREZ_CellClick(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles flgREZ.CellClick
    Dim RowI As Integer
    If e.RowIndex < 0 Then Exit Sub
    RowI = e.RowIndex
    If RowI < 0 Then Exit Sub
    StrWS = RwKey(RowI).Substring(0, 1)
    KeyRez = RwKey(RowI).Substring(1, 3)
    If RowI - 1 > UBound(RwKey) Then Exit Sub
    If RowI <= UBound(RwKey) + 1 And RowI >= 0 Then
      If flgREZ.CurrentCell.ColumnIndex = flgREZ.Columns.Count - 1 Then
        GrpRwerteM(StrWS)(KeyRez).IVoNa = Not GrpRwerteM(StrWS)(KeyRez).IVoNa
        If GrpRwerteM(StrWS)(KeyRez).IVoNa Then
          TabRez.Rows(RowI)("X") = " "
        Else
          TabRez.Rows(RowI)("X") = "X"
        End If
        'GrpRwerte("M")((flgRez.Row)).kwb = -1 * GrpRwerte("M")((flgRez.Row)).kwb
      End If
    End If

  End Sub
  Private Sub frmCalcGrundDaten_Load(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles MyBase.Load
    Dim i As Integer
    '
    HandleRezept = New HandleRezGeneral
    HandleRwrt = New HandleRwerte
    ArbFarb = New Colorant
    '
    '
    Call HandleRezept.SetSpecial()
    '
    '
    For i = 0 To MenueParam.Menue.Pallg.Count - 1
      MenueParam.Menue.Pallg(i) = MenueParam.Messg.Winkel(0).GK(2)
    Next
    '
    '
    Krez = 0
    '
    '
    '
    '
    '
    Me.Text = Texxt(1852) & Space(1) & "{" & MenueParam.User.Name & "}" & Space(1) & "{" & MenueParam.Messg.Kbez & "}"
    Me.lblARB.Text = Texxt(2002)
    Me.lblAVA.Text = Texxt(925)
    Me.lblBER.Text = Texxt(899)
    Me.lblDIK.Text = Texxt(997)
    Me.lblDIU.Text = Texxt(893)
    Me.lblFIX.Text = Texxt(898)
    Me.lblGRP.Text = Texxt(386)
    Me.lblMSH.Text = Texxt(422)
    Me.lblREZ.Text = Texxt(888)
    Me.lblRWE.Text = Texxt(997)
    Me.lblRWU.Text = Texxt(889)
    Me.lblRZU.Text = Texxt(888)
    Me.lblSEL.Text = Texxt(926)
    Me.lblSUC.Text = Texxt(369)
    Me.lblUNT.Text = Texxt(997)
    Me.lblUNU.Text = Texxt(997)
    Me.lblVergl.Text = Texxt(3969)
    Me.lblCopy.Text = Texxt(3966)
    Me.lblSource.Text = Texxt(389) & Space(1) & Texxt(3967)
    Me.lblTarget.Text = Texxt(389) & Space(1) & Texxt(3968)
    Me.optANZ_0.Text = Texxt(894)
    Me.optANZ_1.Text = Texxt(895)
    Me.optGRD_0.Text = Texxt(896)
    Me.optGRD_1.Text = Texxt(897)
    Me.cmdABR.Text = Texxt(379)
    Me.cmdGRI.Text = Texxt(1989)
    Me.cmdKOP.Text = Texxt(389)
    Me.cmdREC.Text = Texxt(886)
    Me.cmdSPE_0.Text = Texxt(1993)
    Me.cmdSPE_1.Text = Texxt(1994)
    Me.cmdSPE_2.Text = Texxt(1992)
    Me.cmdTAB.Text = Texxt(679)
    Me.cmdVER.Text = Texxt(360)
    Me.cmdWei_1.Text = Texxt(680)
    Me.cmdWei_2.Text = Texxt(681)
    Me.cmdWei_3.Text = Texxt(682)
    Me.cmdWei_4.Text = Texxt(683)
    Me.cmdWei_5.Text = Texxt(4691)
    Me.cmdWei_0.Text = Texxt(159)
    Me.cmdZUR.Text = Texxt(159)
    Me.chkAGR.Text = Texxt(882)
    Me.chkFARB.Text = Texxt(881)
    Me.chkGRD.Text = Texxt(887)
    Me.chkGlaett.Text = Texxt(840)
    Me.btnGrundPlott.Text = Texxt(3994)
    Me.btnGrundVerw.Text = Texxt(3995)
    Me.btnGrundCalc.Text = Texxt(3996)
    Me.btnMessZeig.Text = Texxt(3981)
    Me.btnMischZeig.Text = Texxt(3981)
    Me.btnCopyVonNach.Text = Texxt(389)
    Me.btnZeigVergleich.Text = Texxt(3981)
    Me.btnPlottVergleich.Text = Texxt(3979)
    Me.btnZurück.Text = Texxt(4617)
    Me.btnZurückVergleich.Text = Texxt(4617)
    Me.btnSelect.Text = Texxt(3670)
    Me.btnSelectFarbm.Text = Texxt(3670)
    Me.btnMischSelect.Text = Texxt(3670)
    Me.btnZeigCopy.Text = Texxt(3981)
    Me.lblMessgeraet.Text = Texxt(204)
    Me.lblOtherMeas.Text = Texxt(3984)
    Me.lblMischGrund.Text = Texxt(3985)
    Me.lblGrundMSH.Text = Texxt(422)
    Me.btnGrundDelete.Text = Texxt(3986)
    Me.lblGlanzGrad.Text = Texxt(870)
    Me.lblFarbBind.Text = Texxt(801)
    Me.lblZusatz.Text = Texxt(988)
    Me.radZusatz_0.Text = Texxt(988)
    Me.radZusatz_1.Text = Texxt(801)
    Me.lblCopyMischSelect.Text = Texxt(213)
    Me.lblCopyMisch.Text = Texxt(3827)
    Me.btnCopyMisch.Text = Texxt(3829)
    Me.btnZeigMisch.Text = Texxt(3826)
    Me.btnMischCalc.Text = Texxt(3996)
    Me.btnGrundVergleich.Text = Texxt(3965)
    '
    '
    RadZusatz = New List(Of RadioButton)
    RadZusatz.Add(radZusatz_0)
    RadZusatz.Add(radZusatz_1)
    '
    '
    '
    'Winkel für Grunddaten
    '
    chkPlottWin = New List(Of CheckBox)
    chkPlottWin.Clear()
    chkPlottWin.Add(chkPlottWIN_00)
    chkPlottWin.Add(chkPlottWIN_01)
    chkPlottWin.Add(chkPlottWIN_02)
    chkPlottWin.Add(chkPlottWIN_03)
    chkPlottWin.Add(chkPlottWIN_04)
    chkPlottWin.Add(chkPlottWIN_05)
    chkPlottWin.Add(chkPlottWIN_06)
    chkPlottWin.Add(chkPlottWIN_07)
    chkPlottWin.Add(chkPlottWIN_08)

    '
    '
    '
    'Winkel für Grunddaten-Vergleich
    '
    chkWinkVergleich = New List(Of CheckBox)
    chkWinkVergleich.Clear()
    chkWinkVergleich.Add(chkWinkVergleich_0)
    chkWinkVergleich.Add(chkWinkVergleich_1)
    chkWinkVergleich.Add(chkWinkVergleich_2)
    chkWinkVergleich.Add(chkWinkVergleich_3)
    chkWinkVergleich.Add(chkWinkVergleich_4)
    chkWinkVergleich.Add(chkWinkVergleich_5)
    chkWinkVergleich.Add(chkWinkVergleich_6)
    chkWinkVergleich.Add(chkWinkVergleich_7)
    chkWinkVergleich.Add(chkWinkVergleich_8)

    '
    '
    '
    '
    'Absorption und Streuung für Grunddaten-Vergleich
    '
    chkGrundVergleich = New List(Of CheckBox)
    chkGrundVergleich.Clear()
    chkGrundVergleich.Add(chkGrundVergleich_0)
    chkGrundVergleich.Add(chkGrundVergleich_1)
    chkGrundVergleich.Add(chkGrundVergleich_2)
    chkGrundVergleich.Add(chkGrundVergleich_3)
    chkGrundVergleich.Add(chkGrundVergleich_4)
    chkGrundVergleich.Add(chkGrundVergleich_5)
    chkGrundVergleich.Add(chkGrundVergleich_6)
    chkGrundVergleich.Add(chkGrundVergleich_7)
    chkGrundVergleich.Add(chkGrundVergleich_8)
    chkGrundVergleich.Add(chkGrundVergleich_9)


    '
    '
    TooltipDreieck = New ToolTip
    ReWrFarbe = New ReadWriteFarbe
    RezSozpt = New RecipesGrp
    GrpRwerteM = New RefValuesGrp
    GrpRwerteR = New RefValuesGrp
    RwWrRezept = New ReadWriteRezept
    ReWrRwert = New ReadWriteRwert
    ReWrGrund = New ReadWriteGrund
    CalcRezept = New RezeptBerechnung
    FawrtRwerte = New RefValuesGrp
    FarbWrt = New ValuesGrpsAssigns
    quali = New QualKontrolle
    Picauf = New HandlePictures
    RezGrid = New HandleRezC1Screen
    RezGraphics = New HandleRezGrafik
    Picauf.Add("REF", picREF)
    Picauf.Add("GRD", picGRD)
    Picauf.PicGraphic = RezGraphics
    RezGraphics.AllRezepte = RezSozpt
    RezCheckRad = New HandleCheckRad
    RezCheckRad.PicGraphic = RezGraphics
    RezGraphics.WeSc(0) = True
    RezGraphics.WeSc(1) = True
    RezGraphics.Kwop = 0
    RezGrid.Picauf = Picauf
    '
    For i = 0 To MenueParam.Messg.Winkel.Km - 1
      chkPlottWin(i).Visible = True
      chkPlottWin(i).Checked = True
      chkPlottWin(i).Text = MenueParam.Messg.Winkel(i).Chrm
    Next i
    '
    '
    '
    '
    For i = 0 To MenueParam.Messg.Winkel.Km - 1
      chkWinkVergleich(i).Visible = True
      chkWinkVergleich(i).Checked = False
      chkWinkVergleich(i).Text = MenueParam.Messg.Winkel(i).Chrm
    Next i
    chkWinkVergleich(0).Checked = True
    '
    '
    '
    '
    '
    '
    Call ItextGrund(MenueParam.Messg.CDE, Itext)
    For i = 0 To Itext.Count - 1
      chkGrundVergleich(i).Visible = True
      chkGrundVergleich(i).Checked = True
      chkGrundVergleich(i).Text = Texxt(Itext(i))
    Next i
    '
    '
    '
    '
    '
    picREF.Visible = False
    'Call Aend(Me)
    'Call AendMDChild()
    '
    AllgCommand = New OleDbCommand("", Cncol)
    AllgAdapt = New OleDbDataAdapter
    AllgAdapt.SelectCommand = AllgCommand
    '
    '
    '
    SqlStmt = "SELECT TBL_MESSG.MESSG_ID AS MESSG_ID,MESSG_KBEZ FROM " _
   & "(TBL_MESSG INNER JOIN TBL_USER_MESSG ON (TBL_MESSG.MESSG_ID=TBL_USER_MESSG.MESSG_ID)) " _
    & " INNER JOIN TBL_MISCH_MESSG ON (TBL_MESSG.MESSG_ID=TBL_MISCH_MESSG.MESSG_ID)" _
     & " WHERE TBL_MESSG.MESSG_ID=MESSGRW_ID AND USER_ID=" & MenueParam.UserID & " AND MISCH_ID=" & MenueParam.MischID
    TblMessgeraet = New DataTable
    TblMessgeraet.Clear()
    AllgCommand.CommandText = SqlStmt
    'AllgAdapt.MissingSchemaAction = MissingSchemaAction.Ignore
    If Not FillDatset(AllgAdapt, TblMessgeraet) Then
      Exit Sub
    End If
    TblMessgeraet.AcceptChanges()
    cboMessgeraet.DataSource = TblMessgeraet
    cboMessgeraet.DisplayMember = "MESSG_KBEZ"
    cboMessgeraet.ValueMember = "MESSG_ID"
    '

    DbAdapt = New OleDbDataAdapter
    DbCommand = New OleDbCommand("", Cndat)
    DbAdapt.SelectCommand = DbCommand
    TabMisch = New DataTable
    TabGroup = New DataTable
    TabRez = New DataTable
    flgREZ.DataSource = TabRez
    TabMessRech = New DataTable
    flgMessREch.DataSource = TabMessRech
    TabAlle = New DataTable
    TabZusatz = New DataTable
    TabAvail = New DataTable
    TabCalc = New DataTable
    TabFVA = New DataTable
    TabFFI = New DataTable
    TabMix = New DataTable
    ViewCalc = New DataView(TabCalc)
    TblMessgeraet = New DataTable
    ViewBindWeiss = New DataView(TabMix)
    ViewBindWeissAdd = New DataView(TabMix)

    '
    '
    lstAvail.DataSource = TabAvail
    lstCalc.DataSource = TabCalc
    lstCopyNach.DataSource = TabZusatz
    lstCopyVon.DataSource = TabCalc

    lstFVA.DataSource = TabFVA
    lstFFI.DataSource = TabFFI
    TabFVA.Columns.Add("FARBM_ID", GetType(Integer))
    TabFVA.Columns.Add("FARBM_NAME", GetType(String))
    TabFFI.Columns.Add("FARBM_ID", GetType(Integer))
    TabFFI.Columns.Add("FARBM_NAME", GetType(String)) '
    '
    'GK-Optimierung
    '
    '
    '
    RadUntergrund = New List(Of RadioButton)
    RadUntergrund.Clear()
    '
    txtVarianz = New List(Of TextBox)
    '
    '
    '
    '


    '
    PanGrund = New List(Of Panel)
    PanGrund.Add(PanGrund_0)
    PanGrund.Add(PanGrund_1)
    PanGrund.Add(PanGrund_2)
    PanGrund.Add(PanGrund_3)

    '
    '
    'chkFAR.CheckState = CheckState.Checked
    RezCheckRad.cboSKAL = cboSKAL
    RezGraphics.cboSKAL = cboSKAL
    RezCheckRad.cboSKAL.Enabled = False

    '
    '
    RezCheckRad.AddchkWIN = chkWIN_0
    RezCheckRad.AddchkWIN = chkWIN_1
    RezCheckRad.AddchkWIN = chkWIN_2
    RezCheckRad.AddchkWIN = chkWIN_3
    RezCheckRad.AddchkWIN = chkWIN_4
    RezCheckRad.AddchkWIN = chkWIN_5
    RezCheckRad.AddchkWIN = chkWIN_6
    RezCheckRad.AddchkWIN = chkWIN_7
    RezCheckRad.AddchkWIN = chkWIN_8
    '
    '
    '
    cmdSPE = New List(Of Button)
    cmdSPE.Add(cmdSPE_0)
    cmdSPE.Add(cmdSPE_1)
    cmdSPE.Add(cmdSPE_2)
    '
    '
    '
    cmdWei = New List(Of Button)
    cmdWei.Add(cmdWei_0)
    cmdWei.Add(cmdWei_1)
    cmdWei.Add(cmdWei_2)
    cmdWei.Add(cmdWei_3)
    cmdWei.Add(cmdWei_4)
    cmdWei.Add(cmdWei_5)
    '
    '
    '
    optAnz = New List(Of RadioButton)
    optAnz.Add(optANZ_0)
    optAnz.Add(optANZ_1)
    '
    '
    '
    '
    optGrd = New List(Of RadioButton)
    optGrd.Add(optGRD_0)
    optGrd.Add(optGRD_1)



    ChCancel = 1
    '
    'ID's für Menü,B-wert u.ä.m.
    '
    '
    '
    '
    'Aufbau TDBFar
    DatenFarb = New HandleRezDsetFarb

    RezGrid.AllRezepte = RezSozpt
    RezGrid.DatenFarb = DatenFarb
    RezGrid.txtFooter = txtSUM
    RezGrid.TDBFar = TDBFar
    RezGrid.TDBFar.AllowAddNew = False
    RezGrid.TDBFar.AllowUpdate = False
    RezGrid.TDBFar.AllowDelete = False
    'RezGrid.TDBFar.AllowUserToAddRows = False
    'RezGrid.TDBFar.ReadOnly = True
    'RezGrid.TDBFar.AllowUserToDeleteRows = False
    RezGrid.TDBDropFar = TDBDropFar
    RezGrid.TDBDropPre = TDBDropPre
    RezGrid.TDBDropPrb = TDBDropPrb
    RezGrid.TDBDropPro = TDBDropPro





    '
    '######
    '
    RezGrid.AllRezepte = RezSozpt
    'RezGrid.DatenFarb = DatenFarb
    'RezGrid.CalcRezept = CalcRezept

    KeyMenge = "ZEI"
    DatenFarb.DsetFarbCreate()



    RezGrid.GrpRwerte = FawrtRwerte

    '
    '


    '
    'Messungen über weiß/scwarz
    '
    '
    'Gruppe Rezepte
    '
    '
    HandleRezept.cboGRP = cboGRP
    HandleRezept.lblGRP = lblGRP


    '
    'R-Werte für Farbwerte
    '
    '
    '
    FawrtRwerte.Add("F", New RefValues)
    '
    RezCheckRad.cboSKAL = cboSKAL
    '
    If MenueParam.Messg.Winkel.Km = 0 Then
      cmdVER.Enabled = False
    Else
      cmdVER.Enabled = True
    End If
    cmdGRI.Enabled = False
    For i = 0 To RezCheckRad.chkWIN.Count - 1
      RezCheckRad.chkWIN(i).Visible = False
    Next i
    For i = 0 To MenueParam.Messg.Winkel.Km - 1
      RezCheckRad.chkWIN(i).Visible = True
      RezCheckRad.chkWIN(i).Text = MenueParam.Messg.Winkel(i).Chrm
    Next i
    lblDIU.Text = Texxt(893)
    Call PanVisible(0)


    lblARB.Visible = False

    For i = 0 To cmdSPE.Count - 1
      cmdSPE(i).Enabled = False
    Next i
    cmdABR.Enabled = False
    cmdTAB.Enabled = False
    '
    'Mischsysteme
    '
    '
    '
    'Mischsystem Enabled / Visible
    '
    '
    If BitWrt(23, MenueParam.User.Enabl) Then
      cboMSH.Enabled = True
    Else
      cboMSH.Enabled = False
    End If
    If BitWrt(23, MenueParam.User.Visbl) Then
      cboMSH.Visible = True
      lblMSH.Visible = True
    Else
      cboMSH.Visible = False
      lblMSH.Visible = False
    End If

    '
    '
    'Skalierungswerte für R-Kurven
    '
    '
    '
    '
    '
    'Normlichtarten für alle Rezepte
    cboNORML.Items.Clear()
    For i = 0 To MenueParam.Normfa.Nlz - 1
      cboNORML.Items.Add(MenueParam.Normfa(i).NormNama)
    Next i
    If cboNORML.Items.Count = 0 Then Exit Sub
    cboNORML.SelectedIndex = 0
    cboWIN.Items.Clear()
    For i = 0 To MenueParam.Messg.Winkel.Km - 1
      cboWIN.Items.Add(MenueParam.Messg.Winkel(i).Chrm)
    Next i
    cboWIN.Text = cboWIN.Items(0)

    For i = 0 To MenueParam.Messg.Winkel.Km - 1
      'cboWIN.Items.Add(New Menueparam.Messg.Winkel(i).Chrm, Menueparam.Messg.Winkel(i).Lkm)
    Next i
    '
    Cursor = Cursors.WaitCursor
    TabMisch.Clear()
    cboMSH.Enabled = False
    cboMSH.DataSource = TabMisch
    If MenueParam.MischID <> -1 Then
      AllgAdapt.SelectCommand.CommandText = HandleRezept.MischSelectCommand()
      If Not FillDatset(AllgAdapt, TabMisch) Then
        Exit Sub
      End If
      cboMSH.DisplayMember = "MISCH_KBEZ"
      cboMSH.ValueMember = "MISCH_ID"
      cboMSH.SelectedValue = -1
      cboMSH.Enabled = True
      cboMSH.SelectedValue = MenueParam.MischID

    Else
      MsgBox(Texxt(3601))
      Call PanVisible(-1)
    End If
    Call PanVisible(0)


    Cursor = Cursors.Arrow
    '

    ''
  End Sub



  Private Sub frmCalcGrundDaten_FormClosed(ByVal eventSender As System.Object, ByVal eventArgs As System.Windows.Forms.FormClosedEventArgs) Handles Me.FormClosed
    Try
      ReWrFarbe.dispose()
      RezSozpt.dispose()
      RwWrRezept.dispose()
      ReWrRwert.dispose()
      ReWrGrund.dispose()
      CalcRezept.dispose()
      FawrtRwerte.dispose()
      FarbWrt.dispose()
      quali.dispose()
      RezGrid.dispose()
      RezGraphics.dispose()
      Me.Dispose()
    Catch
    End Try
    AufbauPar.MethID = -1
  End Sub
  Private Sub frmColorGrundDaten_Resize(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Resize
    Call ResizeChild(Me)
  End Sub














  Private Sub hscREZ_Scroll(ByVal sender As Object, ByVal e As System.Windows.Forms.ScrollEventArgs) Handles hscREZ.Scroll
    Dim jk As Integer
    Dim j As Integer
    If e.Type <> ScrollEventType.EndScroll Then Exit Sub
    Application.DoEvents()
    hscREZ.Enabled = False
    If IsNothing(RezSozpt) Then Exit Sub
    If hscREZ.Value < 0 Then Exit Sub
    If IsNothing(RezSozpt.Rezepte) Then Exit Sub
    If RezSozpt.Rezepte.RezCount = 0 Then Exit Sub
    If Not hscREZ.Visible Then Exit Sub
    Cursor = Cursors.WaitCursor
    'hscREZ.Enabled = False
    jk = hscREZ.Value
    If jk > UBound(RwKey) Then Exit Sub
    StrWS = RwKey(jk).Substring(0, 1)
    KeyRez = RwKey(jk).Substring(1, 3)
    j = GrpRwerteM(StrWS)(KeyRez).kwb
    lblUNU.Text = Texxt(890) & Texxt(891 + j)
    lblUNT.Text = GrpRwerteM(j).RefUnt.Name
    lblDIK.Text = CStr(RezSozpt.Rezepte(KeyRez).Dicke(j))
    lblRZN.Text = RezSozpt.Rezepte(KeyRez).Name
    lblRWE.Text = GrpRwerteM(StrWS)(KeyRez).Name
    RezGrid.AllRezepte.Rezepte.RemoveRez(KeyMenge)
    'UPGRADE_WARNING: Couldn't resolve default property of object RezGrid.AllRezepte.rezepte.AddRez(). Click for more: 'ms-help://MS.VSExpressCC.v80/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
    RezGrid.AllRezepte.Rezepte.AddRez(KeyMenge, RezSozpt.Rezepte(KeyRez))
    RezGrid.TDBFarRezStart(ier)
    RezGraphics.PlotRwerte.clear()
    RezGraphics.PlotRwerte.Add("M", GrpRwerteM(StrWS)(KeyRez).clone)
    RezGraphics.PlotRwerte.Add("R", GrpRwerteR(StrWS)(KeyRez).clone)
    'UPGRADE_WARNING: Couldn't resolve default property of object PlottRwerte.Add(). Click for more: 'ms-help://MS.VSExpressCC.v80/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
    'ReflName = PlottRwerte("R").Name
    RezGraphics.PlotRwerte("R").Name = Texxt(886)
    RezGraphics.Text = ""


    RezGraphics.Rmax = -1.0
    RezGraphics.Rmin = -1.0

    Picauf.Refresh()
    RezGraphics.Skalier()

    'Call ReflPlot(0, Rmii, Rmaa, WeSc, kwo, Menueparam.Messg.Winkel, "", PlottRwerte, picREF, 0, ier)
    'Application.DoEvents()
    'hscREZ.Enabled = True
    Cursor = Cursors.Arrow
    Application.DoEvents()
    hscREZ.Enabled = True

  End Sub
  Private Sub hscREZ_VisibleChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles hscREZ.VisibleChanged
    Call hscREZ_Scroll(sender, New System.Windows.Forms.ScrollEventArgs(ScrollEventType.EndScroll, 0))
  End Sub
  Private Sub hscGRD_Scroll(ByVal sender As Object, ByVal e As System.Windows.Forms.ScrollEventArgs) Handles hscGRD.Scroll
    Dim kgrd As Integer
    Application.DoEvents()
    If IsNothing(RezSozpt) Then Exit Sub
    If RezSozpt.Farben.FarbCount = 0 Then Exit Sub
    If Not hscGRD.Visible Then Exit Sub
    If e.Type <> ScrollEventType.EndScroll Then Exit Sub

    Cursor = Cursors.WaitCursor

    'hscGRD.Enabled = False
    kgrd = hscGRD.Value
    If kgrd >= RezSozpt.Farben.FarbCount Then Exit Sub
    '
    '
    KeyD = KeyName(RezSozpt.Farben(kgrd).ID)
    RezGraphics.Text = RezSozpt.Farben(KeyD).Name
    RezGraphics.OptKonst = RezSozpt.Farben(KeyD).OptData
    RezGraphics.Rmax = -1.0
    RezGraphics.Rmin = -1.0

    Picauf.Refresh()

    'Application.DoEvents()

    'hscGRD.Enabled = True
    Cursor = Cursors.Arrow
    Application.DoEvents()
  End Sub
  Private Sub hscGRD_VisibleChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles hscGRD.VisibleChanged
    Dim ee As New System.Windows.Forms.ScrollEventArgs(ScrollEventType.EndScroll, 0)
    Call hscGRD_Scroll(sender, ee)

  End Sub

  Private Sub PanGrund_0_Enter(ByVal sender As Object, ByVal e As System.EventArgs) Handles PanGrund_0.Enter
    Lrez = 0
  End Sub
  Private Sub PanGrund_0_VisibleChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles PanGrund_0.VisibleChanged
    If sender.visible Then
      FormMDI.seteinst = True
    End If
  End Sub
  Private Sub lstCalc_click(ByVal sender As Object, ByVal e As System.EventArgs) Handles lstCalc.Click
    Dim i As Integer
    Dim NewRow As DataRow
    '
    'Prüfen, ob bereits in TabFFI vorhanden
    '
    If TabCalc.Rows.Count = 0 Then Exit Sub
    For i = 0 To TabFFI.Rows.Count - 1
      If lstCalc.SelectedValue = TabFFI.Rows(i)("FARBM_ID") Then
        Exit Sub
      End If
    Next

    '
    'Prüfen, ob bereits in TabFVA enthalten
    For i = 0 To TabFVA.Rows.Count - 1
      If lstCalc.SelectedValue = TabFVA.Rows(i)("FARBM_ID") Then
        Exit Sub
      End If
    Next
    '
    '
    '
    '
    'Übernehmen
    '
    NewRow = TabFFI.NewRow
    NewRow("FARBM_ID") = TabCalc.Rows(lstCalc.SelectedIndex)("FARBM_ID")
    NewRow("FARBM_NAME") = TabCalc.Rows(lstCalc.SelectedIndex)("FARBM_NAME")
    TabFFI.Rows.Add(NewRow)
    TabFFI.AcceptChanges()
  End Sub

  Private Sub lstFFI_DoubleClick(ByVal sender As Object, ByVal e As System.EventArgs) Handles lstFFI.DoubleClick
    Dim ev As New KeyEventArgs(Keys.Delete)
    Call lstFFI_KeyDown(sender, ev)
  End Sub

  Private Sub lstFFI_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles lstFFI.KeyDown
    If e.KeyCode = 46 Then
      If IsNothing(lstFFI.SelectedValue) Or lstFFI.SelectedIndex < 0 Then
        Exit Sub
      End If
      'SqlStmt = "UPDATE TBL_GRUND_FARBM SET FARBM_FIX=0 WHERE MISCH_ID=" & MenueParam.MischID & " AND FARBM_ID=" & lstFFI.SelectedValue _
      '& " AND GKWRT_ID=" & MenueParam.Misch.GKwrtID
      'AllgCommand.CommandText = SqlStmt
      'If SQLExeNonQuery(AllgCommand, Cndat) <> 0 Then
      ' Exit Sub
      'End If
      TabFFI.Rows(lstFFI.SelectedIndex).Delete()
      TabFFI.AcceptChanges()
    End If
  End Sub
  Private Sub lstAvail_click(ByVal sender As Object, ByVal e As System.EventArgs) Handles lstAvail.Click
    Dim i As Integer
    Dim NewRow As DataRow
    '
    'Prüfen, ob bereits in TabFFI vorhanden
    '
    If TabAvail.Rows.Count = 0 Then Exit Sub
    For i = 0 To TabFFI.Rows.Count - 1
      If lstAvail.SelectedValue = TabFFI.Rows(i)("FARBM_ID") Then
        Exit Sub
      End If
    Next

    '
    'Prüfen, ob bereits in TabFVA enthalten
    For i = 0 To TabFVA.Rows.Count - 1
      If lstAvail.SelectedValue = TabFVA.Rows(i)("FARBM_ID") Then
        Exit Sub
      End If
    Next
    '
    '
    '
    '
    'Übernehmen
    '
    NewRow = TabFVA.NewRow
    NewRow("FARBM_ID") = TabAvail.Rows(lstAvail.SelectedIndex)("FARBM_ID")
    NewRow("FARBM_NAME") = TabAvail.Rows(lstAvail.SelectedIndex)("FARBM_NAME")
    TabFVA.Rows.Add(NewRow)
    TabFVA.AcceptChanges()
  End Sub

  Private Sub lstFVA_DoubleClick(ByVal sender As Object, ByVal e As System.EventArgs) Handles lstFVA.DoubleClick
    Dim ev As New KeyEventArgs(Keys.Delete)
    Call lstFVA_KeyDown(sender, ev)
  End Sub

  Private Sub lstFVA_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles lstFVA.KeyDown
    If e.KeyCode = 46 Then
      If IsNothing(lstFVA.SelectedValue) Or lstFVA.SelectedIndex = -1 Then
        Exit Sub
      End If
      TabFVA.Rows(lstFVA.SelectedIndex).Delete()
      TabFVA.AcceptChanges()
    End If
  End Sub
  Private Sub lstCalc_DoubleClick(ByVal sender As Object, ByVal e As System.EventArgs) Handles lstCalc.DoubleClick
    Dim ev As New KeyEventArgs(Keys.Delete)
    Call lstCalc_KeyDown(sender, ev)
  End Sub

  Private Sub lstCalc_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles lstCalc.KeyDown
    Dim i As Integer
    Dim diares As DialogResult
    If e.KeyCode = 46 Then
      If IsNothing(lstCalc.SelectedValue) Or lstCalc.SelectedIndex = -1 Then
        Exit Sub
      End If
      diares = MessageBox.Show(Texxt(2974), Texxt(2000), MessageBoxButtons.YesNo)
      If diares = Forms.DialogResult.No Then
        Exit Sub
      End If

      SqlStmt = "DELETE * FROM TBL_GRUND_FARBM WHERE MISCH_ID=" & MenueParam.MischID & " AND FARBM_ID=" & lstCalc.SelectedValue _
      & " AND GKWRT_ID=" & MenueParam.Misch.GKwrtID & " AND MESSGRW_ID=" & MenueParam.Messg.MessgRwID
      AllgCommand.CommandText = SqlStmt
      If SQLExeNonQuery(AllgCommand, Cndat) <> 0 Then
        Exit Sub
      End If
      For i = 0 To TabFFI.Rows.Count - 1
        If TabFFI.Rows(i)("FARBM_ID") = lstCalc.SelectedValue Then
          TabFFI.Rows(i).Delete()
          Exit For
        End If
      Next i
      TabCalc.Rows(lstCalc.SelectedIndex).Delete()
      TabFFI.AcceptChanges()
      TabCalc.AcceptChanges()
    End If
  End Sub

  Private Sub cboMSH_SelectedValueChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles cboMSH.SelectedValueChanged
    Dim i As Short
    If Not sender.enabled Then Exit Sub
    If IsNothing(sender.selectedvalue) Then Exit Sub
    If Not IsNumeric(sender.selectedvalue) Then Exit Sub
    If sender.Selectedvalue = -1 Then Exit Sub
    If Not IsNumeric(sender.Selectedvalue) Then Exit Sub
    If sender.SelectedValue = OldMischID Then Exit Sub
    OldMischID = sender.SelectedValue
    AufbauPar.MischID = sender.SelectedValue
    '
    '
    RezGrid.TDBFarGridRezept(ier)
    '
    '
    lblOldMisch.Text = MenueParam.Misch.Kbez
    '
    '
    GesStreuAbs = New Colorants
    '
    '
    '
    '


    For i = 0 To 1
      UntID(i) = -1
      TypID(i) = -1
      SmpID(i) = -1
    Next i
    '
    lblGKText.Text = MenueParam.Messg.GKBez

    DatenFarb.DsetFarbFill(0, False)
    RezGrid.NewKeynam("ZEI")


    'RezGrid.TDBFar.AutoSizeRowsMode = DataGridViewAutoSizeRowsMode.AllCells
    RezGrid.TDBFar.Splits(0).ExtendRightColumn = True

    '
    '
    '
    '
    '
    '
    '
    '


    '
    '
    '
    '
    'Aufbau der Gruppen für Rezepte
    '
    '

    '
    '
    HandleRezept.GroupList()
    '
    '
    '
    '
    '
    '
    '
    '
    '
    '
    '
    '
    '
    '
    '
    GrpRwerteM.clear()
    GrpRwerteR.clear()
    If IsNothing(RezSozpt.Rezepte) Then
      Exit Sub
    End If

    RezSozpt.Rezepte.clear()
    RezSozpt.Farben.clear()
    '
    If MenueParam.Misch.Vert = 0 Then
      GrpRwerteM.Add("W", New RefValues)
      GrpRwerteM.Add("S", New RefValues)
      GrpRwerteR.Add("W", New RefValues)
      GrpRwerteR.Add("S", New RefValues)
    Else
      GrpRwerteM.Add("S", New RefValues)
      GrpRwerteM.Add("W", New RefValues)
      GrpRwerteR.Add("S", New RefValues)
      GrpRwerteR.Add("W", New RefValues)
    End If
    '
    GrpRwerteM(0).RefUnt = New RefValue
    GrpRwerteM(1).RefUnt = New RefValue
    GrpRwerteR(0).RefUnt = New RefValue
    GrpRwerteR(1).RefUnt = New RefValue

    If GrpRwerteM.RwArt(0) = "W" Then
      RezSozpt.Rezepte.kwb(0) = 0
      RezSozpt.Rezepte.kwb(1) = 1
      GrpRwerteM(0).RefUnt.kwb = 0
      GrpRwerteM(1).RefUnt.kwb = 1
      GrpRwerteR(0).RefUnt.kwb = 0
      GrpRwerteR(1).RefUnt.kwb = 1
      RezGraphics.Vkwb(0) = 0
      RezGraphics.Vkwb(1) = 1
    Else
      RezSozpt.Rezepte.kwb(0) = 1
      RezSozpt.Rezepte.kwb(1) = 0
      GrpRwerteM(0).RefUnt.kwb = 1
      GrpRwerteM(1).RefUnt.kwb = 0
      GrpRwerteR(0).RefUnt.kwb = 1
      GrpRwerteR(1).RefUnt.kwb = 0
      RezGraphics.Vkwb(0) = 1
      RezGraphics.Vkwb(1) = 0
    End If


    '
    '
    '
    '
    '
    '
    '
    '
    '

    '
    '
    'Winkel und Lichtart einstellen
    '
    RezGraphics.Winkel = MenueParam.Messg.Winkel
    '
    '
    '
    '
    '
    'RezCheckRad.Picauf = PicAufbau

    If RezCheckRad.radNLA.Count > 0 Then
      For i = 0 To RezCheckRad.radNLA.Count - 1
        RezCheckRad.radNLA(i).Visible = False
        RezCheckRad.radNLA(i).checked = False
      Next i
      For i = 0 To MenueParam.Normfa.Nlz - 1
        RezCheckRad.radNLA(i).Visible = True
        RezCheckRad.radNLA(i).enabled = False
        RezCheckRad.radNLA(i).Text = MenueParam.Normfa(i).NormKenn
      Next i
      '
      RezCheckRad.radNLA(0).checked = True
    End If
    '
    '
    '
    If RezCheckRad.chkUUU.Count > 0 Then
      For i = 0 To RezCheckRad.chkUUU.Count - 1
        RezCheckRad.chkUUU(i).Text = Texxt(804 + i)
        RezCheckRad.chkUUU(i).Enabled = False
        RezCheckRad.chkUUU(i).Visible = False
        RezCheckRad.chkUUU(i).CheckState = CheckState.Unchecked
        RezCheckRad.radUUU(i).Visible = False
        RezCheckRad.radUUU(i).checked = False
        RezCheckRad.radUUU(i).enabled = False
      Next i
      For i = 0 To RezCheckRad.chkUUU.Count - 1
        RezCheckRad.chkUUU(i).Visible = True
        RezCheckRad.chkUUU(i).Text = Texxt(804 + i)
      Next i
      RezCheckRad.chkUUU(0).Visible = True
      RezCheckRad.chkUUU(0).Enabled = False
      RezCheckRad.chkUUU(0).Checked = True
    End If
    If RezCheckRad.chkWIN.Count > 0 Then
      For i = 0 To RezCheckRad.chkWIN.Count - 1
        RezCheckRad.chkWIN(i).Visible = False
        RezCheckRad.chkWIN(i).checkstate = CheckState.Unchecked
      Next i
      For i = 0 To MenueParam.Messg.Winkel.Km - 1
        RezCheckRad.chkWIN(i).Visible = True
        RezCheckRad.chkWIN(i).Enabled = True
        RezCheckRad.chkWIN(i).Text = LTrim(MenueParam.Messg.Winkel(i).Chrm)
        RezGraphics.kwopt(i) = RezCheckRad.chkWIN(i).checked
      Next i
      '
      RezCheckRad.chkWIN(0).checkstate = CheckState.Checked
    End If
    '

    '
    '
    '
    '
    '
    'Winkel/Messgeometrie für z.B. Lab-Berechnung
    '
    cboWIN.Items.Clear()
    For i = 0 To MenueParam.Messg.Winkel.Km - 1
      cboWIN.Items.Add(MenueParam.Messg.Winkel(i).Chrm)
    Next i
    cboWIN.Text = cboWIN.Items(0)

    '
    '
    Cursor = Cursors.WaitCursor
    RezGrid.Picauf.PicGraphic.DataChanged = False
    '
    '
    TabFVA.Rows.Clear()
    TabFFI.Rows.Clear()
    '
    '
    '
    Call ListFarbm()
    '
    '
    '
    'grafische Darstellung R(c*d) oder R-Kurven
    '
    '
    txtGrundMsh.Text = MenueParam.Misch.Kbez
    '
    '
    Cursor = Cursors.Arrow
    '
    '
    '

    '
    ''

  End Sub

  Private Sub chkWIN_0_CheckStateChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles chkWIN_0.CheckStateChanged, chkWIN_1.CheckStateChanged, _
  chkWIN_2.CheckStateChanged, chkWIN_3.CheckStateChanged, chkWIN_4.CheckStateChanged, chkWIN_5.CheckStateChanged, _
  chkWIN_6.CheckStateChanged, chkWIN_7.CheckStateChanged, chkWIN_8.CheckStateChanged
    Dim index As Short
    If IsNothing(Picauf) Then Exit Sub
    If Not (Picauf.PicRezGrd Is picGRD) Then Exit Sub
    If sender.name = "" Then Exit Sub
    index = CInt(sender.name.ToString.Substring(7, 1))
    RezGraphics.kwopt(index) = RezCheckRad.chkWIN(index).checkstate
    If chkGRD.Checked Then
      Call hscGRD_Scroll(hscGRD, New System.Windows.Forms.ScrollEventArgs(ScrollEventType.EndScroll, 0))
    Else
      Call hscREZ_Scroll(hscREZ, New System.Windows.Forms.ScrollEventArgs(ScrollEventType.EndScroll, 0))
    End If


  End Sub

  Private Sub chkPlottWIN_0_CheckStateChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles chkPlottWIN_00.CheckStateChanged, chkPlottWIN_01.CheckStateChanged, _
 chkPlottWIN_02.CheckStateChanged, chkPlottWIN_03.CheckStateChanged, chkPlottWIN_04.CheckStateChanged, chkPlottWIN_05.CheckStateChanged, _
 chkPlottWIN_06.CheckStateChanged, chkPlottWIN_07.CheckStateChanged, chkPlottWIN_08.CheckStateChanged
    Dim index As Short
    If IsNothing(Picauf) Then Exit Sub

    If Not (Picauf.PicRezGrd Is picGrundPlott) Then Exit Sub
    If sender.name = "" Then Exit Sub
    If IsNothing(chkPlottWin) Then Exit Sub
    index = CInt(sender.name.ToString.Substring(13, 1))
    RezGraphics.kwopt(index) = chkPlottWin(index).CheckState

    Picauf.Refresh()

  End Sub


























  Private Sub cboGRP_SelectedValueChanged(sender As Object, e As System.EventArgs) Handles cboGRP.SelectedValueChanged
    If Not IsNumeric(cboGRP.SelectedValue) Then Exit Sub
    MenueParam.Misch.UserRzpGID = cboGRP.SelectedValue
    Call ListFarbm()
  End Sub
  Private Sub btnGrundverw_Click(sender As Object, e As System.EventArgs) Handles btnGrundVerw.Click
    Dim i As Integer
    If lstCalc.Items.Count = 0 Then Exit Sub
    SplitConGrund.Visible = False
    splGrundVerwaltung.Visible = True
    panPlottWIN.Visible = True
    cmdABR.Visible = False
    cmdSPE_0.Visible = False
    cmdSPE_1.Visible = False
    cmdSPE_2.Visible = False
    cmdVER.Visible = False
    cmdTAB.Visible = False
    cmdGRI.Visible = False
    cboSKAL.Visible = False
    btnGrundPlott.Enabled = True
    BindingPlott.Visible = True
    BindingPlott.Enabled = False
    Picauf.RemoveAt(1)
    Picauf.Add("GRD", picGrundPlott)
    For i = 0 To chkPlottWin.Count - 1
      RezGraphics.kwopt(i) = chkPlottWin(i).Checked
    Next
    FarbCollection = Nothing
    ViewCalc.RowFilter = ""
    ConnCalc.DataSource = ViewCalc
    btnZurück.Visible = True
  End Sub





  Private Sub btnZurück_Click(sender As Object, e As System.EventArgs) Handles btnZurück.Click, btnZurückVergleich.Click
    splGrundVerwaltung.Visible = False
    splGrundVergleiche.Visible = False
    SplitConGrund.Visible = True
    cmdABR.Visible = True
    cmdSPE_0.Visible = True
    cmdSPE_1.Visible = True
    cmdSPE_2.Visible = True
    cmdVER.Visible = True
    cmdTAB.Visible = True
    cmdGRI.Visible = True
    cboSKAL.Visible = True
    picGrundPlott.Visible = False
    dbgGridFarbmittel.Visible = False
    Picauf.RemoveAt(1)
    Picauf.Add("GRD", picGRD)

    For i = 0 To RezCheckRad.chkWIN.Count - 1
      RezGraphics.kwopt(i) = RezCheckRad.chkWIN(i).Checked
    Next
    btnZurück.Visible = False
    btnZurückVergleich.Visible = False
    panCopy.Visible = False
    dbgGrundVergleich.Visible = False
    ChartGrundVergleich.Visible = False
  End Sub
  Private Sub txtNum_Validating(sender As Object, e As System.ComponentModel.CancelEventArgs) Handles txtSUM.Validating
    If Not IsNumeric(sender.text) Then
      e.Cancel = True
      MsgBox(Texxt(60))
    End If
  End Sub


  Private Sub ConnCalc_CurrentChanged(sender As Object, e As System.EventArgs) Handles ConnCalc.CurrentChanged
    Dim keyd As String
    Dim FarbID As Integer
    Dim i As Integer
    Dim kw As Integer
    Application.DoEvents()


    If IsNothing(ConnCalc.Current) Then Exit Sub

    Cursor = Cursors.WaitCursor

    'hscGRD.Enabled = False
    If IsNothing(FarbCollection) OrElse FarbCollection.FarbCount = 0 Then
      '
      FarbID = ConnCalc.Current("FARBM_ID")
      '
      Call ReWrFarbe.FarReaGrund(FarbID, ArbFarb, False, ier)
    Else
      FarbID = dbgGridFarbmittel.SelectedRows(ConnCalc.Position).Cells("FARBM_ID").Value
      ArbFarb = FarbCollection(ConnCalc.Position)
    End If

    If IsNothing(ArbFarb.OptData) Then Exit Sub
    keyd = KeyName(FarbID)
    Call NammText(keyd, Namm)
    For i = 0 To UBound(Namm)
      For kw = 0 To ArbFarb.OptData.Grund(i).Count - 1
        ArbFarb.OptData.Grund(i)(kw).NamPlott = Namm(i)
      Next
    Next
    RezGraphics.Text = ArbFarb.Name
    ToolStripFarbName.Text = ArbFarb.Name
    RezGraphics.OptKonst = ArbFarb.OptData
    RezGraphics.Rmax = -1.0
    RezGraphics.Rmin = -1.0

    Picauf.Refresh()

    'Application.DoEvents()

    'hscGRD.Enabled = True
    Cursor = Cursors.Arrow
    Application.DoEvents()
  End Sub


  Private Sub btnGrundPlott_Click(sender As System.Object, e As System.EventArgs) Handles btnGrundPlott.Click
    picGrundPlott.Visible = True
    dbgGridFarbmittel.Visible = False
    BindingPlott.Enabled = True
    cboGrundPlott.Enabled = True
    btnGrundCalc.Enabled = False
    btnSelect.Enabled = False
    FarbCollection = Nothing
  End Sub
  Private Sub btnMischZeig_Click(sender As Object, e As System.EventArgs) Handles btnMischZeig.Click
    Dim StrFarb As String
    dbgGridFarbmittel.Visible = True

    picGrundPlott.Visible = False
    BindingPlott.Enabled = False
    cboGrundPlott.Enabled = False
    btnGrundCalc.Enabled = True
    btnMischSelect.Enabled = True
    ViewCalc.RowFilter = "NOT FARBM_ID=GLZGRD_ID"
    StrFarb = StrLin(ViewCalc, "GLZGRD_ID")
    ViewCalc.RowFilter = "FARBM_ID IN " & StrFarb
    btnMischCalc.Enabled = True
    btnGrundPlott.Enabled = False
    dbgGridFarbmittel.DataSource = ViewCalc
    Call dbgGridFCust(dbgGridFarbmittel)
  End Sub
  Private Sub btnMischCalc_Click(sender As Object, e As System.EventArgs) Handles btnMischCalc.Click
    Dim i As Integer
    Dim ier As Integer
    Dim FarbID As Integer
    Dim ArbFarb As Colorant
    Dim GlzGrd As Single
    Cursor = Cursors.WaitCursor
    FarbCollection = New Colorants
    GlzGrd = txtGlanzGrad.Text
    For i = 0 To dbgGridFarbmittel.SelectedRows.Count - 1
      FarbID = dbgGridFarbmittel.SelectedRows(i).Cells("FARBM_ID").Value
      ArbFarb = New Colorant
      ReWrFarbe.FarReaGrund(FarbID, ArbFarb, 0.0#, ier)
      Call HandleRezept.GrundGlzGrd(MenueParam.MischID, FarbID, ArbFarb, GlzGrd, ier)
      If ier <> 0 Then
        Cursor = Cursors.Default
        Exit Sub
      End If
      FarbCollection.AddFarb(KeyName(FarbID), ArbFarb)
    Next
    ConnCalc.DataSource = dbgGridFarbmittel.SelectedRows
    picGrundPlott.Visible = True
    dbgGridFarbmittel.Visible = False
    cboGrundPlott.Visible = False
    BindingPlott.Enabled = True

    Cursor = Cursors.Default
  End Sub

  Private Sub btnMessZeig_Click(sender As System.Object, e As System.EventArgs) Handles btnMessZeig.Click
    dbgGridFarbmittel.Visible = True
    picGrundPlott.Visible = False
    BindingPlott.Enabled = False
    cboGrundPlott.Enabled = False
    btnGrundCalc.Enabled = True
    btnSelect.Enabled = True
    FarbCollection = Nothing
    ViewCalc.RowFilter = ""
    dbgGridFarbmittel.DataSource = ViewCalc
    Call dbgGridFCust(dbgGridFarbmittel)
  End Sub
  Sub dbgGridFCust(ByRef dbgFarbmittel As DataGridView)
    Dim i As Integer
    For i = 0 To dbgFarbmittel.Columns.Count - 1
      dbgFarbmittel.Columns(i).Visible = False
    Next
    If dbgFarbmittel.Columns.Count = 0 Then Exit Sub
    dbgFarbmittel.Columns("FARBM_NAME").Visible = True
    dbgFarbmittel.Columns("FARBM_NAME").HeaderText = Texxt(815)
    dbgFarbmittel.Columns("FARBM_NAME").Width = 300
    dbgFarbmittel.Columns("FARBM_ID").Visible = False
    dbgFarbmittel.AllowUserToAddRows = False
    dbgFarbmittel.ReadOnly = True
  End Sub

  Private Sub btnGrundCalc_Click(sender As Object, e As System.EventArgs) Handles btnGrundCalc.Click
    Dim i As Integer
    Dim j As Integer
    Dim kw As Integer
    Dim RefTraAlt As String
    Dim MessgIDAlt As Integer
    Dim WinkelAlt As AngGeos
    Dim MessgIDNeu As Integer
    Dim FarbID As Integer
    Dim WsolAlt() As Single
    Dim comND As OleDbCommand
    Dim FarbList As List(Of Colorant)
    FarbList = New List(Of Colorant)
    '
    '
    '
    'Alte Farbmittel zwischenspeichern
    '
    '
    '
    FarbList.Clear()
    For i = 0 To dbgGridFarbmittel.SelectedRows.Count - 1
      FarbID = dbgGridFarbmittel.SelectedRows(i).Cells("FARBM_ID").Value
      Call ReWrFarbe.FarReaGrund(FarbID, ArbFarb, False, ier)
      If ier <> 0 Then
        MsgBox(Texxt(801) & Space(1) & Texxt(4701) & ": " & ArbFarb.Name)
        Exit Sub
      End If
      FarbList.Add(ArbFarb.clone)
    Next
    '

    '
    WinkelAlt = MenueParam.Messg.Winkel.clone
    MessgIDAlt = MenueParam.MessgID
    RefTraAlt = MenueParam.Messg.RefTra
    ReDim WsolAlt(MenueParam.Messg.Winkel.Wsol.Nwe - 1)
    For i = 0 To MenueParam.Messg.Winkel.Wsol.Nwe - 1
      WsolAlt(i) = MenueParam.Messg.Winkel.Wsol.R(i)
    Next
    MessgIDNeu = cboMessgeraet.SelectedValue
    '
    '
    '
    '
    'GKWRTID für neues Gerät wird gleich der GKWRTID des alten Gerätes gesetzt
    '
    '
    '
    comND = New OleDbCommand("UPDATE TBL_USER_MISCH_MESSG SET GKWRT_ID=" & MenueParam.Misch.GKwrtID _
                             & " WHERE USER_ID=" & MenueParam.UserID _
                             & " AND MISCH_ID=" & MenueParam.MischID _
                             & " AND MESSG_ID=" & MessgIDNeu, Cncol)
    If SQLNonQuery(comND, Cncol) <> 0 Then
      Exit Sub
    End If

    AufbauPar.MessgID = MessgIDNeu
    '
    '
    If RefTraAlt <> MenueParam.Messg.RefTra Then
      MessageBox.Show(Texxt(4500) & vbCrLf & "MESSG_REFTRA: " & MenueParam.Messg.RefTra & "," & RefTraAlt & vbCrLf _
            , Texxt(2004), MessageBoxButtons.OK)
      GoTo ColERR
    End If
    '
    '
    '
    '
    '
    'Berechnung der Grunddaten für neuen Wellenlängen-Bereich
    '
    '
    '
    For i = 0 To FarbList.Count - 1
      ArbFarb = FarbList(i).clone
      '
      'z.B. j=0 absorption, j=1 Streuung usw.
      '
      For j = 0 To ArbFarb.OptData.Grund.Count - 1
        ArbFarb.OptData.Grund(j).clear()
      Next j

      Call CalcRezept.GrundUmrechnung(WinkelAlt, FarbList(i), MenueParam.Messg.Winkel, ArbFarb, ier)
      If ier <> 0 Then
        MessageBox.Show(Texxt(3024) & ": " & FarbList(0).OptData.Grund.Count & "," & MenueParam.Messg.Winkel.Km, Texxt(2004), MessageBoxButtons.OK)
        GoTo colerr
      End If
      Call ReWrGrund.WriteGrund(ArbFarb.ID, ArbFarb.OptData, MenueParam.Messg.Winkel, ier)
    Next i
    '
    '
    '
    MsgBox(Texxt(3982) & ": MessgRwID= " & MenueParam.Messg.MessgRwID & ", GKwrtID: " & MenueParam.Misch.GKwrtID)
    '
    '
    '
    '
    '
ColERR:
    AufbauPar.MessgID = MessgIDAlt
  End Sub

  Private Sub btnGrundDelete_Click(sender As Object, e As System.EventArgs) Handles btnGrundDelete.Click
    Dim imsg As Integer
    Dim cmdDelete As OleDbCommand
    If MessageBox.Show(Texxt(3987), Texxt(422) & ": " & txtGrundMsh.Text, MessageBoxButtons.YesNo) = Forms.DialogResult.No Then
      Exit Sub
    End If
    cmdDelete = New OleDbCommand("", Cndat)
    cmdDelete.CommandText = "DELETE * FROM TBL_GRUND_FARBM WHERE MISCH_ID=" & MenueParam.MischID

    imsg = SQLNonQuery(cmdDelete, Cndat)
    MessageBox.Show(Texxt(3980), Texxt(2000), MessageBoxButtons.OK)
    cmdDelete.Dispose()
    Call ListFarbm()
    btnZurück.PerformClick()

  End Sub


  Private Sub btnZeigVergleich_Click(sender As Object, e As System.EventArgs) Handles btnZeigVergleich.Click
    dbgGrundVergleich.Visible = True
    ChartGrundVergleich.Visible = False
    panCopy.Visible = False
    ViewCalc.RowFilter = ""
    dbgGrundVergleich.DataSource = ViewCalc
    Call dbgGridFCust(dbgGrundVergleich)
    btnPlottVergleich.Enabled = True
    btnCopyVonNach.Enabled = False
  End Sub

  Private Sub btnGrundVergleich_Click(sender As Object, e As System.EventArgs) Handles btnGrundVergleich.Click
    Dim i As Integer
    If lstCalc.Items.Count = 0 Then Exit Sub
    SplitConGrund.Visible = False
    splGrundVerwaltung.Visible = False
    splGrundVergleiche.Visible = True
    panGrundAbsStrVergleich.Visible = True
    panGrundWinkelVergleich.Visible = True
    btnZurückVergleich.Visible = True
    ChartGrundVergleich.Visible = False
    dbgGrundVergleich.Visible = False
    btnPlottVergleich.Enabled = False
    btnCopyVonNach.Enabled = False
  End Sub

  Private Sub btnPlottVergleich_Click(sender As Object, e As System.EventArgs) Handles btnPlottVergleich.Click
    Dim i As Integer
    Dim j As Integer
    Dim k As Integer
    Dim l As Integer
    Dim kw As Integer
    Dim Farbid As Integer
    Dim Graphtext As String
    Dim nanz As Integer
    Dim Nwe As Integer
    Dim Kurvtext() As String
    Dim Xchart() As Single
    Dim Ychart As List(Of Single())

    If dbgGrundVergleich.SelectedRows.Count = 0 Then
      MsgBox(Texxt(4014), MsgBoxStyle.OkOnly, Texxt(2000))
      Exit Sub
    End If
    ChartGrundVergleich.Visible = True
    dbgGrundVergleich.Visible = False
    Nwe = MenueParam.Messg.Winkel.Wsol.Nwe
    ReDim Xchart(Nwe - 1)
    For i = 0 To Nwe - 1
      Xchart(i) = MenueParam.Messg.Winkel.Wsol.R(i)
    Next
    Ychart = New List(Of Single())
    '
    '
    '
    '
    l = 0
    For k = 0 To dbgGrundVergleich.SelectedRows.Count - 1
      Farbid = dbgGrundVergleich.SelectedRows(k).Cells("FARBM_ID").Value
      ArbFarb = New Colorant
      ReWrFarbe.FarReaGrund(Farbid, ArbFarb, 0.0#, ier)
      If ier <> 0 Then
        Cursor = Cursors.Default
        Exit Sub
      End If
      '
      'Schleife über Grunddaten
      '
      For j = 0 To ArbFarb.OptData.Grund.Count - 1
        If chkGrundVergleich(j).Checked Then
          '
          'Schleife über Winkel
          '
          For kw = 0 To ArbFarb.OptData.Grund(j).Count - 1
            If chkWinkVergleich(kw).Checked Then
              ReDim Preserve Kurvtext(l)
              Ychart.Add(Xchart.Clone)
              Kurvtext(l) = "(" & chkGrundVergleich(j).Text & ") (" & chkWinkVergleich(kw).Text & ")" & ArbFarb.Name
              For i = 0 To Nwe - 1
                Ychart(l)(i) = ArbFarb.OptData.Grund(j)(kw).R(i)
              Next i
              l = l + 1
            End If
          Next kw
        End If
      Next j
    Next k
    nanz = l
    Graphtext = MenueParam.Misch.Kbez & "  " & MenueParam.Messg.Kbez
    Call PlottMultiCurve(Graphtext, ChartGrundVergleich, nanz, Kurvtext, Nwe, Xchart, Ychart)

  End Sub
  Sub PlottMultiCurve(GraphText As String, ChartXY As C1.Win.C1Chart.C1Chart, Nanz As Integer, KurvText() As String, Nwe As Integer, XChart() As Single, ychart As List(Of Single()))
    Dim i As Integer
    Dim k As Integer
    Dim j As Integer
    Dim Ymax As Single
    Dim Ymin As Single
    Dim ColAlt As Color
    Dim s As C1.Win.C1Chart.ChartDataSeries
    If Not ChartXY.Visible Then Exit Sub
    ChartXY.ToolTip.Enabled = False
    'Rmax berechnen


    '

    '
    '
    Ymax = 0.0
    For k = 0 To ychart.Count - 1
      For i = 0 To ychart(k).Count - 1
        If ychart(k)(i) > Ymax Then
          Ymax = ychart(k)(i)
        End If
      Next i
    Next k
    Ymin = 1.0E+30
    For k = 0 To ychart.Count - 1
      For i = 0 To ychart(k).Count - 1
        If ychart(k)(i) < Ymin Then
          Ymin = ychart(k)(i)
        End If
      Next i
    Next k
    For i = -5 To 10
      If Ymin < 10 ^ i Then
        Ymin = 10 ^ (i - 1)
        Exit For
      End If
    Next
    For i = 10 To -3 Step -1
      If Ymax > 10 ^ i Then
        Ymax = 10 ^ (i + 1)
        Ymin = Max(10 ^ (i - 5), Ymin)
        Exit For
      End If
    Next
    '
    '
    'Y-Kurven
    '
    '

    ChartXY.ChartGroups.Group0.ChartType = C1.Win.C1Chart.Chart2DTypeEnum.XYPlot


    ChartXY.Reset()

    'ChartXY.ChartGroups(0).ChartData.
    ChartXY.ChartArea.AxisX.AutoMax = False
    ChartXY.ChartArea.AxisX.Max = XChart(Nwe - 1)
    ChartXY.ChartArea.AxisX.Min = XChart(0)
    '
    '
    ChartXY.ChartArea.AxisY.IsLogarithmic = True
    ChartXY.ChartArea.AxisY.AutoMax = False
    ChartXY.ChartArea.AxisY.Max = Ymax
    ChartXY.ChartArea.AxisY.Min = Ymin
    ChartXY.BackColor = Color.LightYellow
    ChartXY.ChartGroups.Group0.ChartData.SeriesList.Clear()
    ChartXY.ChartArea.PlotArea.BackColor = Color.GreenYellow
    ChartXY.ChartArea.Style.BackColor = Color.Fuchsia
    ChartXY.Footer.Text = GraphText
    ChartXY.Legend.Visible = True
    ChartXY.Legend.Style.BackColor = ChartXY.ChartArea.PlotArea.BackColor
    ChartXY.Legend.Compass = C1.Win.C1Chart.CompassEnum.East
    '
    '
    ' ChartXY.ChartGroups.Group0.
    '
    '
    ColAlt = Color.Black
    For k = 0 To ychart.Count - 1
      '
      'Daten
      '
      '

      s = New C1.Win.C1Chart.ChartDataSeries
      s.PointData.Clear()
      s.SymbolStyle.Shape = C1.Win.C1Chart.SymbolShapeEnum.Tri
      s.FitType = C1.Win.C1Chart.FitTypeEnum.Spline
      s.LineStyle.Thickness = 1
      s.LineStyle.Color = s.SymbolStyle.OutlineColor
      s.Label = KurvText(k)
      s.TooltipText = KurvText(k)
      s.TooltipTextLegend = KurvText(k)
      s.LegendEntry = True
      For i = 0 To Nwe - 1
        s.X.Add(XChart(i))
        s.Y.Add(ychart(k)(i))
        If s.Y(i) < Ymin Then
          s.Y(i) = Ymin
        End If
      Next i
      ChartXY.ChartGroups.Group0.ChartData.SeriesList.Add(s)
      s.Display = C1.Win.C1Chart.SeriesDisplayEnum.Show

      ColAlt = s.SymbolStyle.Color
      s.LineStyle.Color = s.SymbolStyle.Color

    Next k

    '
    '
    '
    'Tooltip
    ' 
    '
    ChartXY.ToolTip.Enabled = True
    ChartXY.ToolTip.PlotElement = C1.Win.C1Chart.PlotElementEnum.Points
    ChartXY.ToolTip.SelectAction = C1.Win.C1Chart.SelectActionEnum.MouseOver
    '
    '
    '
    '

    Application.DoEvents()
    '
    '
    '
  End Sub


  Private Sub ChartGrundVergleich_ShowTooltip(sender As Object, e As C1.Win.C1Chart.ShowTooltipEventArgs) Handles ChartGrundVergleich.ShowTooltip
    If e.PointIndex < 0 Or e.SeriesIndex < 0 Then Exit Sub
    e.TooltipText = ChartGrundVergleich.ChartGroups.Group0.ChartData(e.SeriesIndex).Y(e.PointIndex)
  End Sub

  Private Sub btnZeigCopy_Click(sender As Object, e As System.EventArgs) Handles btnZeigCopy.Click
    panCopy.Visible = True
    btnCopyVonNach.Enabled = True
    dbgGrundVergleich.Visible = False
    ChartGrundVergleich.Visible = False
    btnPlottVergleich.Enabled = False
    lstCopyNach.SelectedItems.Clear()
    lstCopyVon.SelectedItems.Clear()

  End Sub

  Private Sub btnCopyVonNach_Click(sender As Object, e As System.EventArgs) Handles btnCopyVonNach.Click
    If lstCopyVon.SelectedItems.Count = 0 Then
      MsgBox(Texxt(4014), MsgBoxStyle.OkOnly, Texxt(2000))
      Exit Sub
    End If
    If lstCopyNach.SelectedItems.Count = 0 Then
      MsgBox(Texxt(4014), MsgBoxStyle.OkOnly, Texxt(2000))
      Exit Sub
    End If
    Call ReWrGrund.ReadGrund(lstCopyVon.SelectedValue, ArbFarb.OptData, ier)
    ArbFarb.OptData.OptID = lstCopyNach.SelectedValue
    Call ReWrGrund.WriteGrund(lstCopyNach.SelectedValue, ArbFarb.OptData, MenueParam.Messg.Winkel, ier)
    MsgBox(Texxt(3983) & Space(1) & Texxt(3968) & ": " & Space(1) & lstCopyNach.Text, MsgBoxStyle.OkOnly, Texxt(2000))

  End Sub

  Private Sub chkGrundWinkVergleich_Click(sender As Object, e As System.EventArgs) Handles chkGrundVergleich_0.Click, chkGrundVergleich_1.Click, chkGrundVergleich_2.Click, chkGrundVergleich_3.Click, _
    chkGrundVergleich_4.Click, chkGrundVergleich_5.Click, chkGrundVergleich_6.Click, chkWinkVergleich_7.Click, chkGrundVergleich_8.Click, chkGrundVergleich_9.Click, _
    chkWinkVergleich_0.Click, chkWinkVergleich_1.Click, chkWinkVergleich_2.Click, chkWinkVergleich_3.Click, chkWinkVergleich_4.Click, chkWinkVergleich_5.Click, chkWinkVergleich_6.Click, _
     chkWinkVergleich_7.Click, chkWinkVergleich_8.Click
    btnPlottVergleich.PerformClick()

  End Sub

  Private Sub btnZeigMisch_Click(sender As Object, e As System.EventArgs) Handles btnZeigMisch.Click
    TabMischAll = New DataTable
    AllgAdapt.SelectCommand.CommandText = "SELECT TBL_MISCH.MISCH_ID AS MISCH_ID,MISCH_KBEZ FROM TBL_MISCH INNER JOIN TBL_MISCH_MESSG ON TBL_MISCH.MISCH_ID=TBL_MISCH_MESSG.MISCH_ID" _
                                        & " WHERE MESSG_ID=" & MenueParam.MessgID & " AND NOT TBL_MISCH.MISCH_ID=" & MenueParam.MischID
    If Not FillDatset(AllgAdapt, TabMischAll) Then
      Exit Sub
    End If
    TabMischAll.AcceptChanges()
    cboCopyMisch.DataSource = TabMischAll
    If TabMischAll.Rows.Count = 0 Then
      MessageBox.Show(Texxt(3828), Texxt(2000), MessageBoxButtons.OK)
      Exit Sub
    End If
    cboCopyMisch.DisplayMember = "MISCH_KBEZ"
    cboCopyMisch.ValueMember = "MISCH_ID"
    cboCopyMisch.SelectedIndex = -1
    cboCopyMisch.BackColor = Color.Red
   
    '
    '
    panCopy.Visible = False
    ChartGrundVergleich.Visible = False
    dbgGrundVergleich.Visible = False
    btnSelectFarbm.Enabled = False
    btnCopyMisch.Enabled = False
  End Sub

 
  Private Sub btnCopyMisch_Click(sender As Object, e As System.EventArgs) Handles btnCopyMisch.Click
    Dim i As Integer
    Dim ier As Integer
    Dim IDFarbTarget As Integer
    Dim IDFarbSource As Integer
    If dbgGrundVergleich.SelectedRows.Count = 0 Then Exit Sub
    Cursor = Cursors.WaitCursor
    If ConnOpen(Cndat) Then
      IDFarbTarget = MaxDBTableID("TBL_FARBM", "FARBM_ID", {"MISCH_ID"}, {cboCopyMisch.SelectedValue}, Cndat)
      For i = 0 To dbgGrundVergleich.SelectedRows.Count - 1
        IDFarbTarget = IDFarbTarget + 1
        IDFarbSource = dbgGrundVergleich.SelectedRows(i).Cells("FARBM_ID").Value
        Call CopyMixingsystem(MenueParam.MischID, IDFarbSource, cboCopyMisch.SelectedValue, IDFarbTarget, MenueParam.Messg.MessgRwID, ier)
        If ier <> 0 Then
          Cursor = Cursors.Default
          Exit Sub
        End If
      Next i
      Cndat.Close()
    End If
    Cursor = Cursors.Default
    lblCopyMisch.Enabled = False
    cboCopyMisch.Enabled = False
    btnCopyMisch.Enabled = False
    btnSelectFarbm.Enabled = False
  End Sub
  Sub CopyMixingsystem(IDMischSource As Integer, IDFarbSource As Integer, IDMischTarget As Integer, IDFarbTarget As Integer, MessgRwID As Integer, ByRef ier As Integer)
    Dim TabNamen() As String = {"TBL_FARBM", "TBL_FARBM_PREIS", "TBL_FARBM_PROZ", "TBL_FARBM_PROB", "TBL_GRUND_FARBM"}

    '

    '
    '

    Dim i As Integer
    Dim j As Integer
    Dim k As Integer
    Dim AdaptMisch As OleDbDataAdapter
    Dim CommandMisch As OleDbCommand
    Dim TablMisch As DataTable
    ier = 0
    CommandMisch = New OleDbCommand("", Cndat)
    AdaptMisch = New OleDbDataAdapter
    AdaptMisch.SelectCommand = New OleDbCommand("", Cndat)
    '
    '
    'Prüfen, ob Source und Target verschieden
    '
    '
    If IDMischSource = IDMischTarget Then
      Exit Sub
    End If
    '
    '

    '
    'Übertrage die Einträge von IDMischSource nach IDMischTarget
    '
    For i = 0 To TabNamen.Count - 1
      TablMisch = New DataTable
      If TabNamen(i) = "TBL_GRUND_FARBM" Then
        AdaptMisch.SelectCommand.CommandText = "SELECT * FROM " & TabNamen(i) & " WHERE MISCH_ID=" & IDMischSource _
                                     & " AND FARBM_ID=" & IDFarbSource & " AND MESSGRW_ID=" & MessgRwID
      Else
        AdaptMisch.SelectCommand.CommandText = "SELECT * FROM " & TabNamen(i) & " WHERE MISCH_ID=" & IDMischSource _
                                     & " AND FARBM_ID=" & IDFarbSource
      End If
      If Not FillDatset(AdaptMisch, TablMisch) Then
        Cndat.Close()
        Exit Sub
      End If
      TablMisch.AcceptChanges()
      '
      '
      '
      '
      'IDSource durch IDTarget ersetzen
      '
      '
      '
      AdaptMisch.InsertCommand = OleDBInsertCmd(TabNamen(i), Cndat)
      For j = 0 To TablMisch.Rows.Count - 1
        TablMisch.Rows(j)("MISCH_ID") = IDMischTarget
        TablMisch.Rows(j)("FARBM_ID") = IDFarbTarget
        If TabNamen(i) = "TBL_FARBM" Then
          TablMisch.Rows(j)("GLZGRD_ID") = IDFarbTarget
        End If
        For k = 0 To TablMisch.Columns.Count - 1
          AdaptMisch.InsertCommand.Parameters(k).Value = TablMisch.Rows(j)(k)
        Next
        ier = SQLNonQuery(AdaptMisch.InsertCommand, Cndat)
        If ier <> 0 Then
          MsgBox("error IDMischSource " & IDMischSource & " IDFarbSource " & IDFarbSource & " IDMischTarget " & IDMischTarget & " IDFarbTarget " & IDFarbTarget)
          Exit Sub
        End If
      Next j
      '
      '
      '
      '
      TablMisch.Dispose()
    Next i
    AdaptMisch.Dispose()
  End Sub

  Private Sub radZusatz_CheckedChanged(sender As Object, e As System.EventArgs) Handles radZusatz_0.CheckedChanged, radZusatz_1.CheckedChanged
    Dim index As Integer
    If sender.name = "" Then Exit Sub
    index = CInt(sender.name.substring(10, 1))
    lblZusatz.Text = RadZusatz(index).Text
    If index = 0 Then
      lstCopyNach.DataSource = TabZusatz
    Else
      lstCopyNach.DataSource = TabAlle
    End If
  End Sub

  Private Sub cboCopyMisch_DropDown(sender As Object, e As System.EventArgs) Handles cboCopyMisch.DropDown
    cboCopyMisch.BackColor = Color.White
  End Sub

  Private Sub cboCopyMisch_SelectedValueChanged(sender As Object, e As System.EventArgs) Handles cboCopyMisch.SelectedValueChanged
    If Not IsNumeric(cboCopyMisch.SelectedValue) Then Exit Sub
    If cboCopyMisch.SelectedIndex = -1 Then Exit Sub
    TabCopyMisch = New DataTable
    TabCopyMisch.Clear()
    DbCommand.CommandText = "SELECT TBL_FARBM.FARBM_ID AS FARBM_ID,FARBM_NAME FROM" _
                          & " TBL_FARBM INNER JOIN TBL_GRUND_FARBM ON (TBL_FARBM.FARBM_ID=TBL_GRUND_FARBM.FARBM_ID) AND (TBL_FARBM.MISCH_ID=TBL_GRUND_FARBM.MISCH_ID)" _
                          & " WHERE TBL_FARBM.MISCH_ID=" & MenueParam.MischID & " AND TBL_GRUND_FARBM.MESSGRW_ID=" & MenueParam.Messg.MessgRwID & " AND GKWRT_ID=" & MenueParam.Misch.GKwrtID _
                          & " AND FARBM_NAME NOT IN (SELECT FARBM_NAME FROM TBL_FARBM WHERE MISCH_ID=" & cboCopyMisch.SelectedValue & ")"
    If Not FillDatset(DbAdapt, TabCopyMisch) Then
      Exit Sub
    End If
    dbgGrundVergleich.DataSource = TabCopyMisch
    Call dbgGridFCust(dbgGrundVergleich)
    lblCopyMisch.Enabled = True
    cboCopyMisch.Enabled = True
    btnCopyMisch.Enabled = True
    btnSelectFarbm.Enabled = True
    dbgGrundVergleich.Visible = True
  End Sub

  Private Sub btnSelect_Click(sender As System.Object, e As System.EventArgs) Handles btnSelect.Click, btnMischSelect.Click
    dbgGridFarbmittel.SelectAll()
  End Sub

  Private Sub btnSelectFarbm_Click(sender As Object, e As System.EventArgs) Handles btnSelectFarbm.Click
    dbgGrundVergleich.SelectAll()
  End Sub
End Class
