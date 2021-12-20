Option Compare Text
Option Strict Off
Option Explicit On
Public Class frmMankiewRezept
  Inherits System.Windows.Forms.Form
  Friend WithEvents cboSortiment As ComboBox
  '
  Dim ier As Integer
  Dim KeySor As String
  Dim KeyBas As String
  Dim TypID(1) As Integer
  Dim UntID(1) As Integer
  Dim SmpID() As Integer
  Dim Nrs() As Integer
  Dim kzl As Integer
  Dim RzNr As String
  Dim RzId As Integer
  Dim NormMengeAlt As Single
  Dim RestfarbenTabelle As DataTable
  Dim FarbTabelle As DataTable
  Dim FarbAlleTabelle As DataTable
  Dim NeueMischTable As DataTable
  Dim DataAdapter As OleDbDataAdapter
  Dim DataMischAdapter As OleDbDataAdapter
  Dim Picauf As HandlePictures
  Dim Rzkey() As String

  '
  '
  '
  '
  '
  '
  Dim radwinkel As List(Of RadioButton)
  Dim chkwinkel As List(Of CheckBox)
  Dim lblwinkel As List(Of Label)
  Dim panwinkel As Panel
  Dim radLichtart As List(Of RadioButton)
  Dim chkLichtart As List(Of CheckBox)
  Dim lblLichtart As List(Of Label)
  Dim panLichtart As Panel


  '
  '
  '
  Dim GrpRwerte As RefValuesGrp
  Dim FarbWrtStd As ValuesGrpsAssigns
  Dim RezSozpt As RecipesGrp
  Dim CalcRezept As RezeptBerechnung
  Dim HandleRezept As HandleRezGeneral
  Dim Umr As RezeptUmrechnung
  Dim RezTab As DataTable
  Dim RezAlleTab As DataTable
  Dim PicGraphic As HandleRezGrafik
  Dim RezDruck As HandlePlottDruck
  Dim ReWrSorti As ReadWriteSortiment
  Dim ReWrRezept As ReadWriteRezept
  Dim ReWrFarbe As ReadWriteFarbe
  Dim ReWrGrund As ReadWriteGrund
  Dim lblGewNL As List(Of Label)
  Dim txtGewNL As List(Of TextBox)
  '
  Dim RestFarb As Colorant
  Dim ArbFarb As Colorant
  Dim WriteRestFarbe As ReadWriteFarbe
  Dim WriteRestGrund As ReadWriteGrund
  Dim txtSUM As List(Of TextBox)

  '
  'Dim FormReport As Form
  Dim ppv As PrintPreviewDialog
  Sub AllPanelHide()
    PrintPrev.Hide()
    SplitContainRezept.Hide()
    SplitContainAlleRezepte.Hide()
    SplitContainPicture.Hide()
    SplitContainSortiment.Hide()
    SplitContainRestfarbe.Hide()
  End Sub
  Sub DisplayAllRezept(ByRef RezAlleTab As DataTable)
    Dim i As Integer
    Dim k As Integer
    Dim Ifarb As Integer
    Dim KeyR As String
    Dim KeyID As String
    Dim Preis As Single
    Dim Rnr As Integer
    Dim Knlz As Short
    Dim Kwop As Short
    Dim Jbase As Integer
    Jbase = 10137 + 192 * MenueParam.Menue.JABST
    Kwop = PicGraphic.Kwop
    Knlz = PicGraphic.Knlz
    '
    '
    '
    '
    '
    'Tabelle für alle Rezepte
    ' 
    '
    '
    '
    RezAlleTab.Rows.Clear()
    RezAlleTab.Columns.Clear()
    ' 
    '
    '
    RezAlleTab.Columns.Add("LFDNR")
    RezAlleTab.Columns("LFDNR").Caption = "lfdNr."
    RezAlleTab.Columns.Add("DE")
    RezAlleTab.Columns("DE").Caption = TexKt(Jbase + 1).Trim
    RezAlleTab.Columns.Add("DL")
    RezAlleTab.Columns("DL").Caption = TexKt(Jbase + 2).Trim
    RezAlleTab.Columns.Add("Da")
    RezAlleTab.Columns("Da").Caption = TexKt(Jbase + 5).Trim
    RezAlleTab.Columns.Add("Db")
    RezAlleTab.Columns("Db").Caption = TexKt(Jbase + 6).Trim
    RezAlleTab.Columns.Add("DC")
    RezAlleTab.Columns("DC").Caption = TexKt(Jbase + 3).Trim
    RezAlleTab.Columns.Add("DH")
    RezAlleTab.Columns("DH").Caption = TexKt(Jbase + 4).Trim
    RezAlleTab.Columns.Add("META")
    RezAlleTab.Columns("META").Caption = TexKt(Jbase).Trim
    RezAlleTab.Columns.Add("PRE")
    RezAlleTab.Columns("PRE").Caption = TexKt(10016).Trim
    Ifarb = RezAlleTab.Columns.Count
    For i = 0 To RezSozpt.Farben.FarbCount - 1
      If RezSozpt.Farben(i).Kto <> "-" Then
        KeyID = KeyName(RezSozpt.Farben(i).ID)
        RezAlleTab.Columns.Add(KeyID)
        RezAlleTab.Columns(KeyID).Caption = RezSozpt.Farben(i).Name.Trim
      End If
    Next

    '
    '
    'Grid für alle Rezepte füllen (GridTable)
    '
    '
    txtRezepte.Text = GrpRwerte(0)("V").Name.Trim
    For k = 0 To kzl - 1
      KeyR = KeyRe(Nrs(k))
      Preis = 0.0
      For i = 0 To RezSozpt.Rezepte(KeyR).KF - 1
        KeyID = KeyName(RezSozpt.Rezepte(KeyR)(i).ID)
        Preis = Preis + RezSozpt.Rezepte(KeyR)(i).BaAmng * RezSozpt.Farben(KeyID).Preis
      Next
      RezAlleTab.Rows.Add(RezAlleTab.NewRow)
      RezAlleTab.Rows(k)("LFDNR") = KeyR
      RezAlleTab.Rows(k)("DE") = FarbWrtStd(0)(KeyR)(Knlz)(Kwop)(MenueParam.Menue.StdMrkKen(15))
      RezAlleTab.Rows(k)("DL") = FarbWrtStd(0)(KeyR)(Knlz)(Kwop)(MenueParam.Menue.StdMrkKen(16))
      RezAlleTab.Rows(k)("Da") = FarbWrtStd(0)(KeyR)(Knlz)(Kwop)(MenueParam.Menue.StdMrkKen(19))
      RezAlleTab.Rows(k)("Db") = FarbWrtStd(0)(KeyR)(Knlz)(Kwop)(MenueParam.Menue.StdMrkKen(20))
      RezAlleTab.Rows(k)("DC") = FarbWrtStd(0)(KeyR)(Knlz)(Kwop)(MenueParam.Menue.StdMrkKen(17))
      RezAlleTab.Rows(k)("DH") = FarbWrtStd(0)(KeyR)(Knlz)(Kwop)(MenueParam.Menue.StdMrkKen(18))
      RezAlleTab.Rows(k)("META") = FarbWrtStd(0)(KeyR)(Knlz)(Kwop)(MenueParam.Menue.StdMrkKen(14))
      RezAlleTab.Rows(k)("PRE") = Format(Preis, "#######.00")
      For i = 0 To RezSozpt.Rezepte(KeyR).KF - 1
        KeyID = KeyName(RezSozpt.Rezepte(KeyR)(i).ID)
        If RezSozpt.Farben(KeyID).Kto <> "-" Then
          RezAlleTab.Rows(k)(KeyID) = Format(RezSozpt.Rezepte(KeyR)(i).BaAmng, RezSozpt.Farben(KeyID).Form.Trim)
        End If
      Next
    Next
    '
 
    '
  End Sub
  Sub GetSortiment()
    Dim i As Integer
    Dim j As Integer
    Dim k As Integer
    Dim ier As Integer
    Dim MiID As Integer
    Dim MiAltID As Integer
    Dim FaID As Integer
    Dim HilfID As Integer
    Dim KeyID As String
    Dim KeyNeuID As String
    Dim ArbFarb As Colorant
    Dim IsChanged As Boolean
    If KeySor <> "" Then
      '
      '
      MiAltID = MenueParam.MischID
      For i = 0 To FarbTabelle.Rows.Count - 1
        If FarbTabelle.Rows(i).RowState = DataRowState.Added Then
          '
          'Prüfen, ob Farb-/Bindemittel bereits vorhanden
          '
          '
          HilfID = FarbTabelle.Rows(i)("FARBM_ID")
          If Not RezSozpt.Farben.ContainsFarb(KeyName(HilfID)) Then
            MiID = HilfID / 100000
            If MiID = 0 Then
              MiID = MiAltID
            End If
            FaID = HilfID Mod 100000
            AufbauPar.MischID = MiID
            ArbFarb = New Colorant
            ReWrFarbe.FarReaGrund(FaID, ArbFarb, True, ier)
            ArbFarb.ID = HilfID
            ArbFarb.OptData.OptID = HilfID
            RezSozpt.Farben.AddFarb(KeyName(HilfID), ArbFarb)
          End If
        ElseIf FarbTabelle.Rows(i).RowState = DataRowState.Deleted Then
          FarbTabelle.Rows(i).RejectChanges()
          HilfID = FarbTabelle.Rows(i)("FARBM_ID")
          RezSozpt.Farben.RemoveFarb(KeyName(HilfID))
          FarbTabelle.Rows(i).Delete()

        End If
      Next
      Cndat.Close()
      AufbauPar.MischID = MiAltID
      IsChanged = DatTabChange(FarbTabelle)
      Call TabToRez(KeySor, RezSozpt, FarbTabelle)
      ' 
      Umr.CalcFamng(KeySor, RezSozpt, ier)
      If RezSozpt.Rezepte(KeySor).Dicke(0) <> Singl(txtDickeSor.Text) Then
        RezSozpt.Rezepte(KeySor).Dicke(0) = Singl(txtDickeSor.Text)
        IsChanged = True
      End If
      If UntID(0) <> GrpRwerte(0).RefUnt.ID Then
        UntID(0) = GrpRwerte(0).RefUnt.ID
        IsChanged = True
      End If
      txtDickeRez.Text = txtDickeSor.Text
      If IsChanged Then
        For i = 0 To FarbTabelle.Rows.Count - 1
          HilfID = FarbTabelle.Rows(i)("FARBM_ID")
          KeyID = KeyName(HilfID)
          MiID = HilfID / 100000
          If MiID = 0 Then
            MiID = MiAltID
          End If
          FaID = HilfID Mod 100000
          '
          '
          '
          If MiID <> MiAltID Then
            '
            'Farbmittel nach MiAltID übernehmen
            '
            RezSozpt.Farben(KeyID).Name = RezSozpt.Farben(KeyID).Name & " (" & CStr(MiID) & ")"
            FarbTabelle.Rows(i)("FARBM_NAME") = RezSozpt.Farben(KeyID).Name
            RezSozpt.Farben(KeyID).Ichf = 6
            RezSozpt.Farben(KeyID).GlzGrdID = -1
            RezSozpt.Farben(KeyID).GlzGrd = 0.0
            ReWrFarbe.FarAdd(RezSozpt.Farben(KeyID), ier)
            ReWrGrund.WriteGrund(RezSozpt.Farben(KeyID).ID, RezSozpt.Farben(KeyID).OptData, MenueParam.Messg.Winkel, ier)
            KeyNeuID = KeyName(RezSozpt.Farben(KeyID).ID)
            RezSozpt.Farben.AddFarb(KeyNeuID, RezSozpt.Farben(KeyID))
            RezSozpt.Farben.RemoveFarb(KeyID)
            'ID's Tabelle ändern
            '
            '
            FarbTabelle.Rows(i)("FARBM_ID") = RezSozpt.Farben(KeyNeuID).ID
            '
            'ID's aller Rezepte korrigieren
            '
            '

            For j = 0 To RezSozpt.Rezepte.RezCount - 1
              For k = 0 To RezSozpt.Rezepte(j).KF - 1
                If RezSozpt.Rezepte(j)(k).ID = HilfID Then
                  RezSozpt.Rezepte(j)(k).ID = RezSozpt.Farben(KeyNeuID).ID
                End If
              Next k
            Next j
          End If
          FarbTabelle.Rows(i).AcceptChanges()
        Next i
        If BitWrt(30, MenueParam.User.Writ) Then
          For i = 0 To FarbTabelle.Rows.Count - 1
            flgFarbmittel.Rows(i).DefaultCellStyle.BackColor = Color.FromArgb(FarbTabelle.Rows(i)("FARBM_FARBID"))
          Next i
        End If
        '
        '
        'BitWrt(5, MenueParam.User.Writ) = Speichern ja/nein (überschreiben erlaubt)
        'BitWrt(20, MenueParam.User.sonst) = Speichern ja/nein (überschreiben nicht erlaubt)

        'Call SorAbsp(KeySor, cboSortiment.SelectedValue, ier)
        If BitWrt(20, MenueParam.User.Sonst) Then
          If Not BitWrt(11, MenueParam.User.Writ) Or MenueParam.UserID = cboSortiment.DataSource.rows(cboSortiment.SelectedIndex)("USER_ID") Then
            ReWrSorti.UpdSortim(KeySor, cboSortiment.SelectedValue, RezSozpt, UntID, TypID, ier)
            MsgBox(Texxt(4666))
          End If
        End If
      End If
    End If
    If BitWrt(30, MenueParam.User.Writ) Then
      For i = 0 To FarbTabelle.Rows.Count - 1
        flgFarbmittel.Rows(i).DefaultCellStyle.BackColor = Color.FromArgb(FarbTabelle.Rows(i)("FARBM_FARBID"))
      Next i
    End If
  End Sub
  '
  '
  'Dieses Programm wird z.Zt. nicht verwendet
  '
  '
  Sub SorAbsp(RzNr As String, olduserID As Integer, ByRef ier As Integer)
    Dim Rezname As String
    Dim RzID As Integer
    'Sortiment abspeichern
    '
    '
    '
    ier = 0
    For i = 0 To 1
      TypID(i) = GrpRwerte(i)("V").ID
      UntID(i) = GrpRwerte(i).RefUnt.ID
    Next
    If Not BitWrt(5, MenueParam.User.Writ) Then Exit Sub
    Rezname = InputBox("", Texxt(4635), RezSozpt.Rezepte(RzNr).Name)
    If Rezname <> "" Then

      Call ReWrSorti.ReadSortiName(Rezname, RzID, ier)
      RezSozpt.Rezepte(RzNr).Name = Rezname
      If RzID = -1 Then
        '
        'Name alt <> Name neu
        '
        If MessageBox.Show(Texxt(799) & "?", Texxt(2000), MessageBoxButtons.YesNo) = DialogResult.Yes Then
          Call ReWrRezept.AddRezept(RzNr, RzID, RezSozpt, UntID, TypID, SmpID, ier)
          MsgBox(Texxt(888) & ": " & Rezname & " " & Texxt(13), MsgBoxStyle.OkOnly, Texxt(2000))
          Rezname = RezSozpt.Rezepte("MNG").Name
          'lblRezept.Text = Rezname
          Exit Sub
        End If
      Else
        If Not BitWrt(20, MenueParam.User.Sonst) Then
          '
          'Rezept bereits vorhanden
          '
          '
          '
          MsgBox(Texxt(2956), MsgBoxStyle.OkOnly, Texxt(2000))
          Exit Sub
        Else
          '
          '
          'Userabhängig
          '
          '
          If BitWrt(11, MenueParam.User.Writ) Then
            '
            '
            If MenueParam.UserID <> olduserID Then
              'User verschieden
              MsgBox(Texxt(3103), MsgBoxStyle.OkOnly, Texxt(2000))
              Exit Sub
            End If
          End If
          '
          'Überschreiben
          '
          If MessageBox.Show(Texxt(2955) & vbCrLf & Texxt(3034), Texxt(2000), MessageBoxButtons.YesNo) = DialogResult.Yes Then
            Call ReWrRezept.UpdateRezept(RzNr, RzID, RezSozpt, UntID, TypID, SmpID, ier)
            MsgBox(Texxt(888) & ": " & Rezname & " " & Texxt(13), MsgBoxStyle.OkOnly, Texxt(2000))
          End If
        End If
      End If
    End If
  End Sub
  Sub GridRezept(ByRef RzNR As String, ByRef RezSozpt As RecipesGrp)
    Dim j As Integer
    Dim Sum As Single
    Dim Height As Integer
    Dim KeyId As String
    If RzNR = "" Then Exit Sub
    RezTab.Rows.Clear()



    Height = 20
    Sum = 0.0
    For j = 0 To RezSozpt.Rezepte(RzNR).KF - 1
      KeyId = KeyName(RezSozpt.Rezepte(RzNR)(j).ID)
      RezTab.Rows.Add(RezTab.NewRow)
      RezTab.Rows(j)(0) = RezSozpt.Farben(KeyId).ID
      RezTab.Rows(j)(1) = Trim(RezSozpt.Farben(KeyId).Name)
      RezTab.Rows(j)(2) = Format(RezSozpt.Rezepte(RzNR)(j).BaAmng, RezSozpt.Farben(KeyId).Form)
      RezTab.Rows(j)(3) = Format(RezSozpt.Farben(KeyId).Fst, "###.00")
      RezTab.Rows(j)(4) = Format(RezSozpt.Rezepte(RzNR)(j).Proz, "###.00")
      RezTab.Rows(j)(5) = Format(RezSozpt.Rezepte(RzNR)(j).Prob, "###.00")
      Sum = Sum + RezSozpt.Rezepte(RzNR)(j).BaAmng
    Next
    txtSUM(1).Text = Texxt(825)
    txtSUM(2).Text = Format(Sum, "#####.00")
    '
    '
    RezTab.AcceptChanges()
    '
    '
    '
    For j = 0 To RezSozpt.Rezepte(RzNR).KF - 1
      TDBRezept.Rows(j).Height = Height
    Next
    TDBRezept.AllowUserToAddRows = False
    TDBRezept.AllowUserToResizeColumns = False
    TDBRezept.AllowUserToOrderColumns = False
    TDBRezept.AllowUserToDeleteRows = False
    TDBRezept.ReadOnly = True
    TDBRezept.AutoSizeRowsMode = DataGridViewAutoSizeRowsMode.AllCells
    TDBRezept.ScrollBars = ScrollBars.Vertical
    TDBRezept.BorderStyle = BorderStyle.None
    TDBRezept.RowHeadersVisible = True
    TDBRezept.RowHeadersWidth = 4
    TDBRezept.Columns(1).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    TDBRezept.Columns(2).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    TDBRezept.Columns(3).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    TDBRezept.Columns(4).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    TDBRezept.Columns(5).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight

    '
    '
    TDBRezept.Columns(3).DefaultCellStyle.Format = "##0.00"
    TDBRezept.Columns(4).DefaultCellStyle.Format = "##0.00"
    TDBRezept.Columns(5).DefaultCellStyle.Format = "##0.00"

    '
    '
    '
    TDBRezept.Columns(1).HeaderText = Texxt(394)
    TDBRezept.Columns(2).HeaderText = Texxt(602)
    TDBRezept.Columns(3).HeaderText = TexKt(21014)
    TDBRezept.Columns(4).HeaderText = Texxt(820)
    TDBRezept.Columns(5).HeaderText = Texxt(821)

    TDBRezept.Columns(0).Visible = False
    TDBRezept.Columns(1).Width = TDBRezept.Width * 0.4
    TDBRezept.Columns(2).Width = TDBRezept.Width * 0.2
    TDBRezept.Columns(3).Width = TDBRezept.Width * 0.2
    TDBRezept.Columns(4).Width = TDBRezept.Width * 0.2
    TDBRezept.Columns(5).Width = TDBRezept.Width * 0.2

    TDBRezept.Columns(1).HeaderCell.Style.Alignment = TDBRezept.Columns(1).DefaultCellStyle.Alignment
    TDBRezept.Columns(2).HeaderCell.Style.Alignment = TDBRezept.Columns(2).DefaultCellStyle.Alignment
    TDBRezept.Columns(3).HeaderCell.Style.Alignment = TDBRezept.Columns(2).DefaultCellStyle.Alignment
    TDBRezept.Columns(4).HeaderCell.Style.Alignment = TDBRezept.Columns(2).DefaultCellStyle.Alignment
    TDBRezept.Columns(5).HeaderCell.Style.Alignment = TDBRezept.Columns(2).DefaultCellStyle.Alignment

    TDBRezept.Columns(1).AutoSizeMode = DataGridViewAutoSizeColumnMode.Fill
    TDBRezept.Columns(1).SortMode = SortOrder.None
    TDBRezept.Columns(2).SortMode = SortOrder.None
    TDBRezept.Columns(3).SortMode = SortOrder.None
    TDBRezept.Columns(4).SortMode = SortOrder.None
    TDBRezept.Columns(3).ReadOnly = True
    TDBRezept.Columns(4).ReadOnly = True
    TDBRezept.Columns(5).ReadOnly = True
    TDBRezept.Columns(5).Visible = False
    TDBRezept.Columns(3).SortMode = SortOrder.None

    '
    '
    '
    TDBRezept.FirstDisplayedScrollingRowIndex = 0

  End Sub

  Sub CalcNewRezepte()
    '
    '
    'Gravimetrische Rezepte berechnen (u. U.unter Beachtung der Rundung DOSMIN)
    '
    '
    '
    Dim i As Integer
    Dim Rkey As String
    Dim OptHilf As OpticalData
    OptHilf = New OpticalData
    'Mengen als Gewichte berechnen
    RezSozpt.IVOL = 0
    For i = 0 To RezSozpt.Rezepte.RezCount - 1
      Rkey = RezSozpt.Rezepte.RezKey(i)
      CalcRezept.MischRezept(0, MenueParam.User.Winkel, Rkey, RezSozpt, GrpRwerte, OptHilf, ier)
    Next
    OptHilf.dispose()
  End Sub


  
  
   


  Private Sub frmMankiewRezept_Deactivate(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Deactivate
    Call DeActivat(sender, e)
  End Sub

  Private Sub frmMankiewRezept_Disposed(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Disposed
  End Sub



 

  
  Private Sub frmMankiewRezept_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
    Dim i As Integer
    '
    '
    FormMDI.Icon = My.Resources.berechnung
    FormMDI.Text = MenueParam.User.Name & " - " & MenueParam.Messg.Kbez & " - " & MenueParam.Menue.MethBez
    '
    radwinkel = FormMDI.radwinkel
    chkwinkel = FormMDI.chkWinkel
    lblwinkel = FormMDI.lblWinkel
    panwinkel = FormMDI.panWinkel
    radLichtart = FormMDI.radLichtart
    chkLichtart = FormMDI.chkLichtart
    lblLichtart = FormMDI.lblLichtart
    panLichtart = FormMDI.panLichtart
    '
    '
    '
    btnAlleRezepte.Text = Texxt(4622)
    btnAlleRezZurück.Text = Texxt(4617)
    btnDruckAlle.Text = Texxt(4624)
    btnDrucken.Text = Texxt(4608)
    btnFarbmetrik.Text = Texxt(4615)
    btnPictureDrucken.Text = Texxt(397)
    btnPictureZurück.Text = Texxt(4617)
    btnPrevzurück.Text = Texxt(4617)
    btnRezCalc.Text = Texxt(4618)
    btnRwerteUntergrund.Text = Texxt(890)
    btnRwerteVorlage.Text = Texxt(861)
    btnSpeicher.Text = Texxt(4614)
    btnSortiverw.Text = Texxt(4609)
    btnSortimentZurück.Text = Texxt(4617)
    'chkVol.Text = Texxt(830)
    lblAnzahl.Text = Texxt(3689)
    lblGewFakt.Text = Texxt(446)
    lblMeta.Text = Texxt(841)
    lblPreis.Text = Texxt(4633)
    lblRezepte.Text = Texxt(4623)
    lblUntergrund.Text = Texxt(4619)
    'lblDosis.Text = Texxt(4611)
    'lblMenge.Text = Texxt(602)
    lblPicVorlage.Text = Texxt(861)
    lblPicNachstellung.Text = Texxt(786)
    lblMngBis.Text = Texxt(377)
    lblProzBis.Text = Texxt(377)
    lblDickeRez.Text = Texxt(832)
    lblDickeSor.Text = Texxt(832)
    lblProRezept.Text = Texxt(4660)
    lblMin.Text = Texxt(813)
    lblMax.Text = Texxt(814)
    btnRestfarbeZurück.Text = Texxt(4617)
    btnRestfarbe.Text = Texxt(4625)
    btnCalcRestfarbe.Text = Texxt(853)
    btnCopyFarbmittel.Text = Texxt(2508)
    btnRestVorlage.Text = Texxt(4626)
    btnRestUntergrund.Text = Texxt(890)
    txtRestfarbe.Text = Texxt(4627)
    btnElimRestfarbe.Text = Texxt(4653)
    btnShowRestfarbe.Text = Texxt(4683)
    btnSelectRestfarbe.Text = Texxt(4684)
    btnUserProg.Text = Texxt(392)
    lblRestUntergrund.Text = Texxt(4619)
    chkRwertAngleich.Text = Texxt(4708)
    lblGlzGrd.Text = Texxt(870)

    cboMin.Items.Clear()
    cboMax.Items.Clear()
    For i = 0 To 13
      cboMin.Items.Add(CStr(i + 1))
      cboMax.Items.Add(CStr(i + 1))
    Next i
    '
    '
    txtSUM = New List(Of TextBox)
    txtSUM.Add(txtSUM_0)
    txtSUM.Add(txtSUM_1)
    txtSUM.Add(txtSUM_2)
    

    '
    Application.DoEvents()
    '
    '
    DataAdapter = New OleDbDataAdapter
    DataAdapter.SelectCommand = New OleDbCommand
    DataAdapter.SelectCommand.Connection = Cndat()
    '
    '
    '
    DataMischAdapter = New OleDbDataAdapter
    DataMischAdapter.SelectCommand = New OleDbCommand
    DataMischAdapter.SelectCommand.Connection = Cncol()
    '
    lblGewNL = New List(Of Label)
    txtGewNL = New List(Of TextBox)
    'Gewichtsfaktoren
    '
    '
    lblGewNL.Clear()
    txtGewNL.Clear()

    For i = 0 To 4
      Select Case i
        Case 0
          lblGewNL.Add(lblGewNL_0)
          txtGewNL.Add(txtGewNL_0)
        Case 1
          lblGewNL.Add(lblGewNL_1)
          txtGewNL.Add(txtGewNL_1)
        Case 2
          lblGewNL.Add(lblGewNL_2)
          txtGewNL.Add(txtGewNL_2)
        Case 3
          lblGewNL.Add(lblGewNL_3)
          txtGewNL.Add(txtGewNL_3)
        Case 4
          lblGewNL.Add(lblGewNL_4)
          txtGewNL.Add(txtGewNL_4)
        Case 5
          lblGewNL.Add(lblGewNL_5)
          txtGewNL.Add(txtGewNL_5)
      End Select
    Next
    '
    'Art der Normierungen
    '
    '
    cboMNG.Items.Clear()
    For i = 0 To 18
      cboMNG.Items.Add(RepTexxt(Texxt(580 + i), Texxt(602)))
    Next i
    '
    '
    'Art der Mengenverhältnisse
    '
    '
    cboPROZZae.Items.Clear()
    cboPROZNen.Items.Clear()
    For i = 0 To 18
      cboPROZZae.Items.Add(RepTexxt(Texxt(580 + i), Texxt(602)))
      cboPROZNen.Items.Add(RepTexxt(Texxt(580 + i), Texxt(602)))
    Next i
    '
    '
    '
    '
    'Grid Sortiment
    '
    '
    FarbTabelle.Clear()
    '
    flgFarbmittel.Columns(0).HeaderText = "ID"
    flgFarbmittel.Columns(0).Visible = False
    flgFarbmittel.Columns(0).Width = 0
    flgFarbmittel.Columns(0).ReadOnly = True
    FarbTabelle.Columns.Add(flgFarbmittel.Columns(0).DataPropertyName, GetType(Integer))
    FarbTabelle.Columns(0).DefaultValue = -1
    '
    'Name
    '
    flgFarbmittel.Columns(1).HeaderText = Texxt(394)
    flgFarbmittel.Columns(1).Width = flgFarbmittel.Width - 600
    flgFarbmittel.Columns(1).Visible = True
    flgFarbmittel.Columns(1).ReadOnly = True
    flgFarbmittel.Columns(1).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleLeft
    FarbTabelle.Columns.Add(flgFarbmittel.Columns(1).DataPropertyName, GetType(String))
    FarbTabelle.Columns(1).DefaultValue = Space(1)

    '
    '
    'Menge
    '

    flgFarbmittel.Columns(2).HeaderText = Texxt(4630)
    flgFarbmittel.Columns(2).Width = 75
    flgFarbmittel.Columns(2).ReadOnly = False
    flgFarbmittel.Columns(2).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    flgFarbmittel.Columns(2).DefaultCellStyle.Format = "###0.00"
    flgFarbmittel.Columns(2).HeaderCell.Style.Alignment = DataGridViewContentAlignment.MiddleRight
    flgFarbmittel.Columns(2).HeaderCell.SortGlyphDirection = SortOrder.None
    FarbTabelle.Columns.Add(flgFarbmittel.Columns(2).DataPropertyName, GetType(Single))
    FarbTabelle.Columns(2).DefaultValue = -1


    '
    '
    '
    '
    'Farbstärke
    ' 
    flgFarbmittel.Columns(3).HeaderText = TexKt(21014)
    flgFarbmittel.Columns(3).Width = 75
    flgFarbmittel.Columns(3).ReadOnly = True
    flgFarbmittel.Columns(3).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    flgFarbmittel.Columns(3).DefaultCellStyle.Format = "###0.00"
    flgFarbmittel.Columns(3).HeaderCell.Style.Alignment = DataGridViewContentAlignment.MiddleRight
    flgFarbmittel.Columns(3).HeaderCell.SortGlyphDirection = SortOrder.None
    FarbTabelle.Columns.Add(flgFarbmittel.Columns(3).DataPropertyName, GetType(Single))
    FarbTabelle.Columns(3).DefaultValue = 100

    '
    '
    '
    'PROZ
    '
    '
    flgFarbmittel.Columns(4).HeaderText = Texxt(820)
    flgFarbmittel.Columns(4).Width = 75
    flgFarbmittel.Columns(4).ReadOnly = False
    flgFarbmittel.Columns(4).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    flgFarbmittel.Columns(4).DefaultCellStyle.Format = "###0.00"
    flgFarbmittel.Columns(4).HeaderCell.Style.Alignment = DataGridViewContentAlignment.MiddleRight
    flgFarbmittel.Columns(4).HeaderCell.SortGlyphDirection = SortOrder.None
    FarbTabelle.Columns.Add(flgFarbmittel.Columns(4).DataPropertyName, GetType(Single))
    FarbTabelle.Columns(4).DefaultValue = 100

    '
    '
    '
    'PROB
    '
    '
    flgFarbmittel.Columns(5).HeaderText = Texxt(821)
    flgFarbmittel.Columns(5).Width = 1
    flgFarbmittel.Columns(5).Visible = False
    flgFarbmittel.Columns(5).ReadOnly = False
    flgFarbmittel.Columns(5).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    flgFarbmittel.Columns(5).DefaultCellStyle.Format = "###0.00"
    flgFarbmittel.Columns(5).HeaderCell.Style.Alignment = DataGridViewContentAlignment.MiddleRight
    flgFarbmittel.Columns(5).HeaderCell.SortGlyphDirection = SortOrder.None
    FarbTabelle.Columns.Add(flgFarbmittel.Columns(5).DataPropertyName, GetType(Single))
    FarbTabelle.Columns(5).DefaultValue = 100

    '
    'Preis
    ' 
    flgFarbmittel.Columns(6).HeaderText = Texxt(822)
    flgFarbmittel.Columns(6).Width = 75
    flgFarbmittel.Columns(6).ReadOnly = False
    flgFarbmittel.Columns(6).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    flgFarbmittel.Columns(6).DefaultCellStyle.Format = "###0.00"
    flgFarbmittel.Columns(6).HeaderCell.Style.Alignment = DataGridViewContentAlignment.MiddleRight
    flgFarbmittel.Columns(6).HeaderCell.SortGlyphDirection = SortOrder.None
    FarbTabelle.Columns.Add(flgFarbmittel.Columns(6).DataPropertyName, GetType(Single))
    FarbTabelle.Columns(6).DefaultValue = 0

    '
    
    '
    'TOPF
    '
    '
    '
    '
    flgFarbmittel.Columns(7).HeaderText = "Topf"
    flgFarbmittel.Columns(7).Width = 60
    flgFarbmittel.Columns(7).ReadOnly = False
    flgFarbmittel.Columns(7).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    flgFarbmittel.Columns(7).HeaderCell.Style.Alignment = DataGridViewContentAlignment.MiddleRight
    flgFarbmittel.Columns(7).HeaderCell.SortGlyphDirection = SortOrder.None
    FarbTabelle.Columns.Add(flgFarbmittel.Columns(7).DataPropertyName, GetType(String))
    FarbTabelle.Columns(7).DefaultValue = Space(1)

    '
    '
    'Operator
    '
    '
    '

    flgFarbmittel.Columns(8).HeaderText = "OP"
    flgFarbmittel.Columns(8).Width = 60
    flgFarbmittel.Columns(8).ReadOnly = False
    flgFarbmittel.Columns(8).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    flgFarbmittel.Columns(8).HeaderCell.Style.Alignment = DataGridViewContentAlignment.MiddleRight
    flgFarbmittel.Columns(8).HeaderCell.SortGlyphDirection = SortOrder.None
    FarbTabelle.Columns.Add(flgFarbmittel.Columns(8).DataPropertyName, GetType(String))
    FarbTabelle.Columns(8).DefaultValue = Space(1)

    '
    'Limitierungsmenge
    '
    flgFarbmittel.Columns(9).HeaderText = Texxt(4637)
    flgFarbmittel.Columns(9).Width = 90
    flgFarbmittel.Columns(9).ReadOnly = False
    flgFarbmittel.Columns(9).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
    flgFarbmittel.Columns(9).DefaultCellStyle.Format = "###0.00"
    flgFarbmittel.Columns(9).HeaderCell.Style.Alignment = DataGridViewContentAlignment.MiddleRight
    flgFarbmittel.Columns(9).HeaderCell.SortGlyphDirection = SortOrder.None
    FarbTabelle.Columns.Add(flgFarbmittel.Columns(9).DataPropertyName, GetType(Single))
    FarbTabelle.Columns(9).DefaultValue = 100

    '
    '
    'ICHF
    '
    '
    flgFarbmittel.Columns(10).HeaderText = "ICHF"
    flgFarbmittel.Columns(10).Visible = False
    flgFarbmittel.Columns(10).Width = 0
    flgFarbmittel.Columns(10).ReadOnly = True
    FarbTabelle.Columns.Add(flgFarbmittel.Columns(10).DataPropertyName, GetType(Integer))
    FarbTabelle.Columns(10).DefaultValue = 0

    '
    '
    'FARBID
    '
    '
    flgFarbmittel.Columns(11).HeaderText = "FARBID"
    flgFarbmittel.Columns(11).Visible = False
    flgFarbmittel.Columns(11).Width = 0
    flgFarbmittel.Columns(11).ReadOnly = True
    FarbTabelle.Columns.Add(flgFarbmittel.Columns(11).DataPropertyName, GetType(Integer))
    FarbTabelle.Columns(11).DefaultValue = -1
    '
    For i = 0 To FarbTabelle.Columns.Count - 1
      FarbTabelle.Columns(i).AllowDBNull = False
    Next

    '
    '
    '
    '
    lstRestfarbe.DataSource = RestfarbenTabelle
    flgFarbmittel.DataSource = FarbTabelle
    flgFarbmittel.MultiSelect = False
    lstFarbmittel.DataSource = FarbAlleTabelle
    'lstFarbmittel.DisplayMember = "FARBM_NAME"
    'lstFarbmittel.ValueMember = "FARBM_ID"
    

    FarbWrtStd = New ValuesGrpsAssigns
    GrpRwerte = New RefValuesGrp
    RezSozpt = New RecipesGrp
    CalcRezept = New RezeptBerechnung
    HandleRezept = New HandleRezGeneral
    ReWrSorti = New ReadWriteSortiment
    ReWrRezept = New ReadWriteRezept
    ReWrFarbe = New ReadWriteFarbe
    ReWrGrund = New ReadWriteGrund
    Umr = New RezeptUmrechnung
    WriteRestFarbe = New ReadWriteFarbe
    WriteRestGrund = New ReadWriteGrund
    RestFarb = New Colorant
    ArbFarb = New Colorant



    RzNr = ""
    cboSortiment = FormMDI.cboSortiment
    KeySor = ""
    ReDim SmpID(1)
    SmpID(0) = -1
    SmpID(1) = -1
    '
    '
    'Tabelle für Einzelrezepte
    '
    '
    RezTab = New DataTable
    RezTab.Columns.Clear()
    RezTab.Columns.Add("FAID", GetType(Integer))
    RezTab.Columns.Add("FNAME", GetType(String))
    RezTab.Columns.Add("GEWICHT", GetType(Single))
    RezTab.Columns.Add("FST", GetType(Single))
    RezTab.Columns.Add("PROZ", GetType(Single))
    RezTab.Columns.Add("PROB", GetType(Single))

    TDBRezept.DataSource = RezTab

    '
    '
    'Tabelle für alle Rezepte
    '
    RezAlleTab = New DataTable

    PrintPrev.Hide()
    SplitContainRezept.Hide()
    SplitContainAlleRezepte.Show()
    SplitContainRezept.Show()
    SplitContainPicture.Hide()
    '
    '
    '
    '
    '
    PicGraphic = New HandleRezGrafik
    RezDruck = New HandlePlottDruck
    RezDruck.printset = pd.PrinterSettings
    Picauf = New HandlePictures
    Picauf.Add("REF", PictureBoxKurve)
    Picauf.Add("REZ", PictureBoxRezept)
    Picauf.Add("LAB", PictureBoxLAB)
    Picauf.Add("XYZ", pictureBoxFarbe)
    Picauf.Add("FRB", PictureBoxFarbWerte)
    Picauf.PicGraphic = PicGraphic
    RezDruck.JProz = True
    RezDruck.JProb = True
    RezDruck.JStae = True
    RezDruck.JSpez = True


    RezDruck.Plott = PicGraphic
    RezDruck.AllRezepte = RezSozpt
    RezDruck.GrpRwerte = GrpRwerte
    RezDruck.Winkel = MenueParam.User.Winkel

    RezDruck.AllRezepte = RezSozpt
    RezDruck.GrpRwerte = GrpRwerte
    ppv = New PrintPreviewDialog
    PicGraphic.GrpRwerte = GrpRwerte
    PicGraphic.AllRezepte = RezSozpt
    If MenueParam.MischID = -1 Then
      MsgBox(Texxt(3601))
      Exit Sub
    End If
    '
    If BitWrt(22, MenueParam.User.Sonst) Then
      lblGlzGrd.Visible = True
      txtGlzGrd.Visible = True
    End If



    '
    '
    '
    'Call LoadChild(Me)
    Call ShowMe(sender, e)
    '
    '
    '
    '
    ''
  End Sub

  Private Sub btnRezCalc_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnRezCalc.Click
    Dim i As Integer
    Dim Keyr As String
    Dim KFIT As Integer
    Dim GrpHilfRwerte As RefValuesGrp
    Cursor = Cursors.WaitCursor
    SplitContainAlleRezepte.Hide()
    SplitContainPicture.Hide()
    PrintPrev.Hide()
    hscRezepte.Enabled = False
    btnRezCalc.Enabled = False
    btnRwerteVorlage.Enabled = False
    RezSozpt.IVOL = 0
    RezSozpt.MngMax = Singl(txtMngMax.Text)
    RezSozpt.MngMin = Singl(txtMngMin.Text)
    RezSozpt.ProzMax = 0.01 * Singl(txtProzMax.Text)
    RezSozpt.ProzMin = 0.01 * Singl(txtProzMin.Text)
    RezSozpt.Rezepte(KeySor).RezMin = cboMin.SelectedIndex + 1
    RezSozpt.Rezepte(KeySor).RezMax = cboMax.SelectedIndex + 1

    RezSozpt.Rezepte(KeySor).Dicke(0) = CSng(txtDickeRez.Text)
    '
    RezSozpt.Rezepte(KeySor).GlzGrd = CSng(txtGlzGrd.Text)
    '
    'Gewichtsfaktoren
    '
    MenueParam.Menue.Lgew = CSng(txtGewDL.Text)
    MenueParam.Menue.Cgew = CSng(txtGewDC.Text)
    MenueParam.Menue.Hgew = CSng(txtGewDH.Text)
    For i = 0 To Min(MenueParam.Normfa.Nlz - 1, lblGewNL.Count - 1)
      MenueParam.Normfa(i).NormGew = CSng(txtGewNL(i).Text)
    Next


    RzNr = ""

    '
    '
    'Erstrezepte berechnen
    '
    '
    If MenueParam.Misch.MGGE = 0 Then
      MenueParam.Misch.Gge = 0
    End If
    '
    '
    KFIT = 1
    If chkRwertAngleich.Checked Then
      KFIT = 2
    End If

    If BitWrt(22, MenueParam.User.Sonst) Then
      Cursor = Cursors.WaitCursor
      For i = 0 To RezSozpt.Farben.FarbCount - 1
        Call HandleRezept.GrundGlzGrd(MenueParam.MischID, RezSozpt.Farben(i).ID, RezSozpt.Farben(i), RezSozpt.Rezepte(KeySor).GlzGrd, ier)
      Next i
      Cursor = Cursors.Default
    End If


    CalcRezept.BasisRezept(KFIT + 8, MenueParam.User.Winkel, KeySor, KeyBas, RezSozpt, GrpRwerte, ier)
    If ier <> 0 Then
      Cursor = Cursors.Arrow
      hscRezepte.Enabled = True
      btnRezCalc.Enabled = True
      btnRwerteVorlage.Enabled = True
      Exit Sub
    End If
    RezSozpt.Rezepte("SOR").RezMax = cboMax.SelectedIndex + 1
    RezSozpt.Rezepte("SOR").RezMin = cboMin.SelectedIndex + 1
    MenueParam.Misch.MaxFarb = RezSozpt.Rezepte("SOR").RezMax
    MenueParam.Misch.MinFarb = RezSozpt.Rezepte("SOR").RezMin
    CalcRezept.ErstRezept(KFIT + 8, MenueParam.User.Winkel, KeySor, KeyBas, RezSozpt, GrpRwerte, ier)
    RezSozpt.IVOL = 0
    If ier <> 0 Then
      Cursor = Cursors.Arrow
      hscRezepte.Enabled = True
      btnRezCalc.Enabled = True
      btnRwerteVorlage.Enabled = True
      Exit Sub
    End If
    Call CalcNewRezepte()

    '
    '
    '
    'Sortieren
    '
    CalcRezept.SorRez(MenueParam.Misch.Isor, MenueParam.Misch.MaxFarb, kzl, Nrs, MenueParam.Misch.DeGut, RezSozpt, ier)
    '
    '
    '
    '
    '
    '
    FarbWrtStd.clear()
    AufbauPar.AufbauRezeptMerk(FarbWrtStd, ier)
    Call LichtArtText()
    GrpHilfRwerte = New RefValuesGrp
    GrpHilfRwerte.Add(GrpRwerte.RwArt(0), New RefValues)
    GrpHilfRwerte.Add(GrpRwerte.RwArt(1), New RefValues)
    GrpHilfRwerte(0).Add("V", GrpRwerte(0)("V"))
    GrpHilfRwerte(1).Add("V", GrpRwerte(1)("V"))
    For i = 0 To kzl - 1
      Keyr = KeyRe(Nrs(i))
      GrpHilfRwerte(0).Add(Keyr, GrpRwerte(0)(Keyr))
      GrpHilfRwerte(1).Add(Keyr, GrpRwerte(1)(Keyr))
    Next
    Call Quali.FarbWrtCalc(MenueParam.User.Winkel, GrpHilfRwerte, FarbWrtStd, ier)
    GrpHilfRwerte.dispose()
    '
    Call DisplayAllRezept(RezAlleTab)
    '
    '
    '
    '
    '
    'Auswahl Rezept
    '
    '
    hscRezepte.Minimum = 0
    hscRezepte.Maximum = kzl - 1

    hscRezepte.SmallChange = 1
    hscRezepte.LargeChange = 1
    '

    'Visible
    '
    '
    Call ControlsVisible(True)
    SplitContainRezept.Show()
    Cursor = Cursors.Arrow
    hscRezepte.Enabled = True
    Application.DoEvents()
    Call hscRezepte_Scroll(sender, New System.Windows.Forms.ScrollEventArgs(ScrollEventType.EndScroll, 0))

    btnRezCalc.Enabled = True
    btnRwerteVorlage.Enabled = True

  End Sub
  Sub ControlsVisible(ByVal Vis As Boolean)
    pictureBoxFarbe.Visible = Vis
    lblPicVorlage.Visible = Vis
    lblPicNachstellung.Visible = Vis
    panTDBRezept.Visible = Vis
    btnDrucken.Visible = Vis
    btnFarbmetrik.Visible = vis
    btnSpeicher.Visible = Vis
    btnAlleRezepte.Visible = Vis
    hscRezepte.Visible = Vis
    lblRezepte.Visible = vis
    lblDEStern.Visible = Vis
    lblDLStern.Visible = vis
    lblDaStern.Visible = Vis
    lblDbStern.Visible = Vis
    lblDCStern.Visible = Vis
    lblDHStern.Visible = Vis
    txtDEStern.Visible = Vis
    txtDLStern.Visible = Vis
    txtDaStern.Visible = Vis
    txtDbStern.Visible = Vis
    txtDCStern.Visible = Vis
    txtDHStern.Visible = Vis
    lblMeta.Visible = Vis
    txtMETA.Visible = Vis
    lblPreis.Visible = Vis
    txtPreis.Visible = Vis
    If BitWrt(20, MenueParam.User.Drum) And Vis Then
      btnUserProg.Visible = True
    Else
      btnUserProg.Visible = False
    End If

  End Sub





  Private Sub btnDrucken_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnDrucken.Click
    Dim Rezname As String
    Rezname = InputBox("", Texxt(4635), RezSozpt.Rezepte(RzNr).Name)
    If Rezname <> "" Then
      RezSozpt.Rezepte(RzNr).Name = Rezname
      Call AllPanelHide()
      PrintPrev.Show()



      Dim Printdoc As New PrintDocument
      Printdoc.PrinterSettings = pd.PrinterSettings
      ppv.Document = Printdoc

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
      'Wiegeschein
      '
      RezDruck.Clear()
      RezDruck.KeyNam = RzNr
      Erase RezDruck.RzName
      ReDim RezDruck.RzName(0)
      RezDruck.RzName(0) = GrpRwerte(0)("V").Name
      '
      '
      Erase RezDruck.RefNr
      ReDim RezDruck.RefNr(1)
      RezDruck.RefNr(0) = "V"
      RezDruck.RefNr(1) = RzNr
      '
      '
      '
      RezAusgabe.pldr = RezDruck
      AddHandler Printdoc.PrintPage, AddressOf RezAusgabe.MankDruckWiegeschein
      Printdoc.DefaultPageSettings.Landscape = False
      ppv.WindowState = FormWindowState.Maximized
      'Preview anzeigen 
      ppv.ShowDialog()

      RemoveHandler Printdoc.PrintPage, AddressOf RezAusgabe.MankDruckWiegeschein
      Printdoc.Dispose()
      SplitContainRezept.Show()
    End If
  End Sub

  Private Sub btnAlleRezepte_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnAlleRezepte.Click
    Dim i As Integer
    txtAnzahl.Text = kzl
    Call AllPanelHide()
    SplitContainAlleRezepte.Show()
    '
    '
    '
    '
    GridAlleRezepte.DataSource = RezAlleTab

    For i = 0 To RezAlleTab.Columns.Count - 1
      GridAlleRezepte.Columns(i).Width = 50
      GridAlleRezepte.Columns(i).HeaderText = RezAlleTab.Columns(i).Caption
      GridAlleRezepte.Columns(i).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
      GridAlleRezepte.Columns(i).HeaderCell.Style.Alignment = DataGridViewContentAlignment.MiddleRight
    Next
    GridAlleRezepte.Columns("Da").Visible = False
    GridAlleRezepte.Columns("Db").Visible = False
    GridAlleRezepte.AutoSizeColumnsMode = DataGridViewAutoSizeColumnsMode.Fill
    For i = 0 To GridAlleRezepte.Columns.Count - 1
      'GridAlleRezepte.Columns(i).HeaderCell.SortGlyphDirection = SortOrder.None
      GridAlleRezepte.Columns(i).SortMode = DataGridViewColumnSortMode.NotSortable
    Next i
  End Sub

  Private Sub btnZurück_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnAlleRezZurück.Click, btnPrevzurück.Click, btnPictureZurück.Click
    Call AllPanelHide()
    SplitContainRezept.Show()

  End Sub
  Private Sub btnFarbmetrik_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnFarbmetrik.Click
    Call AllPanelHide()
    SplitContainPicture.Show()
  End Sub
  Private Sub CBOSortiment_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles cboSortiment.SelectedIndexChanged
    Call ShowMe(sender, e)
  End Sub
  Sub ShowMe(ByVal sender As Object, ByVal e As System.EventArgs)
    Dim i As Integer
    Dim KeyID As String
    Dim index As Integer
    If Not Me.Visible Then Exit Sub
    pictureBoxFarbe.Visible = False
    lblPicVorlage.Visible = False
    lblPicNachstellung.Visible = False
    panTDBRezept.Visible = False
    btnDrucken.Visible = False
    btnFarbmetrik.Visible = False
    btnSpeicher.Visible = False
    btnAlleRezepte.Visible = False
    hscRezepte.Visible = False
    lblRezepte.Visible = False
    lblDEStern.Visible = False
    lblDLStern.Visible = False
    lblDaStern.Visible = False
    lblDbStern.Visible = False
    lblDCStern.Visible = False
    lblDHStern.Visible = False
    txtDEStern.Visible = False
    txtDLStern.Visible = False
    txtDaStern.Visible = False
    txtDbStern.Visible = False
    txtDCStern.Visible = False
    txtDHStern.Visible = False
    lblMeta.Visible = False
    txtMETA.Visible = False
    lblPreis.Visible = False
    txtPreis.Visible = False
    btnRwerteVorlage.Enabled = False
    btnRwerteUntergrund.Enabled = False
    btnRezCalc.Enabled = False
    If IsNothing(RezSozpt) Then Exit Sub
    If MenueParam.MischID = -1 Then
      Exit Sub
    End If
    If IsNothing(cboSortiment) Then Exit Sub
    If cboSortiment.SelectedIndex = -1 Then
      Exit Sub
    Else
      If cboSortiment.DataSource.rows.count = 0 Then
        MsgBox(Texxt(4661))
        Exit Sub
      End If
      KeySor = "SOR"
      Cursor = Cursors.WaitCursor
      AufbauPar.MethID = 53
      '
      '
      '
      'Winkel
      '
      '
      For i = 0 To chkwinkel.Count - 1
        RemoveHandler chkwinkel(i).Click, AddressOf LichtartWinkel_click
        chkwinkel(i).Visible = False
        radwinkel(i).Visible = False
        lblwinkel(i).Visible = False
      Next
      '
      '
      '
      panwinkel.Visible = True
      panLichtart.Visible = True
      For i = 0 To MenueParam.User.Winkel.Km - 1
        chkwinkel(i).Visible = True
        radwinkel(i).Visible = True
        lblwinkel(i).Visible = True
        lblwinkel(i).Text = MenueParam.User.Winkel(i).IhrmBez
        AddHandler chkwinkel(i).Click, AddressOf LichtartWinkel_click
        AddHandler radwinkel(i).Click, AddressOf LichtartWinkel_click

      Next
      '
      '
      'Lichtarten
      '
      '
      For i = 0 To radLichtart.Count - 1
        RemoveHandler radLichtart(i).Click, AddressOf LichtartWinkel_click
        radLichtart(i).Visible = False
        chkLichtart(i).Visible = False
        lblLichtart(i).Visible = False

      Next
      '
      '
      '
      panLichtart.Visible = True
      For i = 0 To MenueParam.Normfa.Nlz - 1
        radLichtart(i).Visible = True
        'chkLichtart(i).Visible = True
        lblLichtart(i).Visible = True
        lblLichtart(i).Text = MenueParam.Normfa(i).NormNama
        AddHandler radLichtart(i).Click, AddressOf LichtartWinkel_click

      Next
      '
      '
      '
      '
      '
      '
      '
      '
      'Measure.Umspeich(MenueParam.Messg, MenueParam.Normfa(0))
      cboMin.SelectedIndex = MenueParam.Misch.MinFarb - 1
      cboMax.SelectedIndex = MenueParam.Misch.MaxFarb - 1
      '
      '
      '
      '
      'Unvisible
      '
      '
      '
      'Unenabled
      '
      'chkVol.Enabled = True
      'txtMenge.Enabled = False
      btnRwerteVorlage.Enabled = True
      btnRwerteUntergrund.Enabled = True
      btnRezCalc.Enabled = True

      '
      '
      '
      '
      GrpRwerte.clear()
      RezTab.Rows.Clear()
      '
      If MenueParam.Misch.Vert = 0 Then
        GrpRwerte.Add("W", New RefValues)
        GrpRwerte.Add("S", New RefValues)
      Else
        GrpRwerte.Add("S", New RefValues)
        GrpRwerte.Add("W", New RefValues)
      End If
      GrpRwerte(0).Add("V", New RefValue)
      GrpRwerte(0).Add("R", New RefValue)
      GrpRwerte(1).Add("V", New RefValue)
      GrpRwerte(1).Add("R", New RefValue)
      GrpRwerte(0).RefUnt = New RefValue
      GrpRwerte(1).RefUnt = New RefValue
      GrpRwerte(0)("V").Itp = True
      GrpRwerte(1)("V").Itp = True
      '
      '
      '
      '
      '
      '
      '
      If GrpRwerte.RwArt(0) = "W" Then
        GrpRwerte(0)("V").kwb = 0
        GrpRwerte(1)("V").kwb = 1
        GrpRwerte(0)("R").kwb = 0
        GrpRwerte(1)("R").kwb = 1
        GrpRwerte(0).RefUnt.kwb = 0
        GrpRwerte(1).RefUnt.kwb = 1
        RezSozpt.Rezepte.kwb(0) = 0
        RezSozpt.Rezepte.kwb(1) = 1
      Else
        GrpRwerte(0)("V").kwb = 1
        GrpRwerte(1)("V").kwb = 0
        GrpRwerte(0)("R").kwb = 1
        GrpRwerte(1)("R").kwb = 0
        GrpRwerte(0).RefUnt.kwb = 1
        GrpRwerte(1).RefUnt.kwb = 0
        RezSozpt.Rezepte.kwb(0) = 1
        RezSozpt.Rezepte.kwb(1) = 0
      End If
      For i = 0 To 1
        GrpRwerte(i).RefUnt.Name = ""
        GrpRwerte(0)(i).Name = ""
        GrpRwerte(1)(i).Name = ""
      Next i
      PicGraphic.Winkel = MenueParam.User.Winkel
      Call SetPicgraphics()
      PicGraphic.Rmin = 0
      PicGraphic.Rmax = 100
      '
      '
      'Sortiment einlesen
      '
      '
      '
      '
      For i = 0 To 1
        TypID(i) = -1
        UntID(i) = -1
      Next i
      index = cboSortiment.SelectedIndex
      If index < 0 Then
        RezSozpt.Rezepte(KeySor).Dicke(0) = MenueParam.Misch.Dicke
        RezSozpt.Rezepte(KeySor).Dicke(1) = MenueParam.Misch.Dicke
      Else
        '
        '
        '
        '
        'Einlesen Sortiment 
        '
        '
        ClearRezepte(RezSozpt)
        RezSozpt.Rezepte.clear()
        RezSozpt.Rezepte.AddRez(KeySor, New Recipe)
        KeyBas = "BAS"
        RezSozpt.Rezepte.AddRez(KeyBas, New Recipe)
        SortID = SortiTable.Rows(index)("SORTI_ID")
        '
        RwWrSortim.ReadSortimFarbGrund(KeySor, SortID, RezSozpt, UntID, TypID, ier)
        '
        cboMin.SelectedIndex = RezSozpt.Rezepte(KeySor).RezMin - 1
        cboMax.SelectedIndex = RezSozpt.Rezepte(KeySor).RezMax - 1
        For i = 0 To RezSozpt.Rezepte(KeySor).KF - 1
          KeyID = KeyName(RezSozpt.Rezepte(KeySor)(i).ID)
          RezSozpt.Rezepte(KeySor)(i).BaAmng = RezSozpt.Farben(KeyID).Smenge
          RezSozpt.Rezepte(KeySor)(i).Prob = RezSozpt.Farben(KeyID).Prb(0)
          RezSozpt.Rezepte(KeySor)(i).Proz = RezSozpt.Farben(KeyID).Prf(0)
        Next i
        RzNr = ""
        'txtMenge.Text = CStr(RezSozpt.MngMax)
        'txtDosis.Text = CStr(MenueParam.Misch.MinDos)
        Umr.CalcBamng(KeySor, RezSozpt, ier)
        'chkVol.Checked = False
        'If RezSozpt.IVOL = 1 Then
        ' chkVol.Checked = True
        'End If
        GrpRwerte(0).RefUnt.ID = UntID(0)
        GrpRwerte(1).RefUnt.ID = UntID(1)
        GrpRwerte(0)("V").ID = TypID(0)
        GrpRwerte(1)("V").ID = TypID(1)
        '
        '
        '
        'Einlesen R-Werte (Untergrund)
        '
        '
        '
        For i = 0 To GrpRwerte.Count - 1
          GrpRwerte(i).RefUnt.IVoNa = False
          If GrpRwerte(i).RefUnt.ID >= 0 Then
            ReWrRwert.ReadRwert(GrpRwerte(i).RefUnt.ID, GrpRwerte(i).RefUnt, ier)
          End If
        Next i
        GrpRwerte(1).RefUnt.IVoNa = False

        '
        '
        '
        'Einlesen R-Werte (Vorlage)
        '
        '
        '
        For i = 0 To GrpRwerte.Count - 1
          GrpRwerte(i)("V").IVoNa = False
          If GrpRwerte(i)("V").ID >= 0 Then
            ReWrRwert.ReadRwert(GrpRwerte(i)("V").ID, GrpRwerte(i)("V"), ier)
          End If
        Next i
        GrpRwerte(1)("V").IVoNa = False
      End If

      txtRezept.Text = GrpRwerte(0)("V").Name.Trim
      txtRestfarbe.Text = GrpRwerte(0)("V").Name.Trim
      '
      '


      '
      '
      'Plausibilitätsprüfungen für Rezepte
      '
      '
      '


      If RezSozpt.MngMin < 0.0# Then RezSozpt.MngMin = 0
      If RezSozpt.MngMax < RezSozpt.MngMin Then RezSozpt.MngMax = RezSozpt.MngMin
      If RezSozpt.ProzMin < 0 Then RezSozpt.ProzMin = 0
      If RezSozpt.ProzMax < RezSozpt.ProzMin Then RezSozpt.ProzMax = RezSozpt.ProzMin
      If RezSozpt.Rezepte(KeySor).Dicke(0) < 0 Then RezSozpt.Rezepte(KeySor).Dicke(0) = 0
      If RezSozpt.Rezepte(KeySor).Dicke(1) < 0 Then RezSozpt.Rezepte(KeySor).Dicke(1) = 0
      If RezSozpt.Rezepte(KeySor).Dicke(0) <> RezSozpt.Rezepte(KeySor).Dicke(1) Then
        RezSozpt.Rezepte(KeySor).Dicke(1) = RezSozpt.Rezepte(KeySor).Dicke(0)
      End If
      btnRwerteUntergrund.Visible = False
      lblUntergrund.Visible = False
      txtUntergrund.Visible = False
      txtUntergrund.Text = ""
      lblRestUntergrund.Visible = False
      txtRestUntergrund.Visible = False
      txtRestUntergrund.Text = ""
      If MenueParam.Misch.Transp Then
        btnRwerteUntergrund.Visible = True
        lblUntergrund.Visible = True
        txtUntergrund.Visible = True
        lblRestUntergrund.Visible = True
        txtRestUntergrund.Visible = True
        If UntID(0) > -1 Then
          txtUntergrund.Text = GrpRwerte(0).RefUnt.Name.Trim
          txtRestUntergrund.Text = GrpRwerte(0).RefUnt.Name.Trim
        End If
      End If
      '
      '
      '
      'Neues Mischsystem
      '
      DataMischAdapter.SelectCommand.CommandText = "SELECT DISTINCT TBL_MISCH.MISCH_ID AS MISCH_ID,TBL_MISCH.MISCH_KBEZ AS MISCH_KBEZ" _
         & " FROM (TBL_MISCH INNER JOIN TBL_USER_MISCH ON TBL_MISCH.MISCH_ID = TBL_USER_MISCH.MISCH_ID)" _
         & " INNER JOIN TBL_MISCH_MESSG ON TBL_MISCH.MISCH_ID = TBL_MISCH_MESSG.MISCH_ID" _
         & " WHERE ((TBL_USER_MISCH.USER_ID=" & MenueParam.UserID & ") AND (TBL_MISCH_MESSG.MESSG_ID=" _
         & MenueParam.MessgID & ") AND (MISCH_SET=YES)) ORDER BY MISCH_KBEZ;"
      If Not FillDatset(DataMischAdapter, NeueMischTable) Then
        Exit Sub
      End If
      NeueMischTable.AcceptChanges()
      cboNeuesMischsystem.Enabled = False
      cboNeuesMischsystem.DataSource = NeueMischTable
      cboNeuesMischsystem.DisplayMember = "MISCH_KBEZ"
      cboNeuesMischsystem.ValueMember = "MISCH_ID"
      cboNeuesMischsystem.Enabled = True
      cboNeuesMischsystem.SelectedIndex = -1
      For i = 0 To NeueMischTable.Rows.Count - 1
        If MenueParam.MischID = NeueMischTable.Rows(i)("MISCH_ID") Then
          cboNeuesMischsystem.SelectedIndex = i
          Exit For
        End If
      Next
      If Transp(MenueParam.Messg.CDE) Then
        cboNeuesMischsystem.Visible = True
      Else
        cboNeuesMischsystem.Visible = False
      End If

      Call RezToTab(KeySor, RezSozpt, FarbTabelle)
      If BitWrt(30, MenueParam.User.Writ) Then
        For i = 0 To FarbTabelle.Rows.Count - 1
          flgFarbmittel.Rows(i).DefaultCellStyle.BackColor = Color.FromArgb(FarbTabelle.Rows(i)("FARBM_FARBID"))
        Next i
      End If
      txtMngMin.Text = RezSozpt.MngMin
      txtMngMax.Text = RezSozpt.MngMax
      txtProzMin.Text = RezSozpt.ProzMin
      txtProzMax.Text = RezSozpt.ProzMax
      cboMNG.SelectedIndex = RezSozpt.INO
      cboPROZZae.SelectedIndex = RezSozpt.INP
      cboPROZNen.SelectedIndex = RezSozpt.INQ
      '
      '
      'Schichtdicke
      txtDickeRez.Text = CStr(RezSozpt.Rezepte(KeySor).Dicke(0))
      '
      '
      'Glanzgrad
      '
      '
      txtGlzGrd.Text = RezSozpt.Rezepte(KeySor).GlzGrd
      '
      '
      'Gewichtfaktoren
      '
      lblGewDL.Text = "DL"
      lblGewDC.Text = "DC"
      lblGewDH.Text = "DH"
      '
      '
      txtGewDL.Text = Format(MenueParam.Menue.Lgew, "###.00")
      txtGewDC.Text = Format(MenueParam.Menue.Cgew, "###.00")
      txtGewDH.Text = Format(MenueParam.Menue.Hgew, "###.00")
      For i = 0 To lblGewNL.Count - 1
        lblGewNL(i).Visible = False
        txtGewNL(i).Visible = False
      Next
      For i = 0 To Min(MenueParam.Normfa.Nlz - 1, lblGewNL.Count - 1)
        lblGewNL(i).Text = MenueParam.Normfa(i).NormKenn
        txtGewNL(i).Text = Format(MenueParam.Normfa(i).NormGew, "###.00")
        lblGewNL(i).Visible = True
        txtGewNL(i).Visible = True
      Next
      Cursor = Cursors.Arrow
    End If

  End Sub
  Sub DeActivat(ByVal sender As Object, ByVal e As System.EventArgs)
    KeySor = ""
    AufbauPar.MethID = -1
  End Sub

  Private Sub hscRezepte_Scroll(ByVal sender As Object, ByVal e As System.Windows.Forms.ScrollEventArgs) Handles hscRezepte.Scroll


    Dim i As Integer
    Dim Rnr As Integer
    Dim Preis As Single
    Dim KeyId As String
    Application.DoEvents()

    If e.Type <> ScrollEventType.EndScroll Then Exit Sub
    If Not hscRezepte.Enabled Then Exit Sub
    If IsNothing(Nrs) Then Exit Sub
    If Not IsNothing(TDBRezept.CurrentCell) AndAlso TDBRezept.CurrentCell.IsInEditMode Then
      'TDBRezept.EndEdit()
      'Durch nachfolgende Befehle wird CellEndEdit Ereignis ausgelöst!
      TDBRezept.Enabled = False
      TDBRezept.Enabled = True
    End If

    '
    'Aktuelles Rezept in Grid
    '
    Rnr = e.NewValue
    hscRezepte.Value = Rnr
    RzNr = KeyRe(Nrs(Rnr))
    Call GridRezept(RzNr, RezSozpt)
    'MsgBox(hscRezepte.Maximum)
    If BitWrt(30, MenueParam.User.Writ) Then
      For i = 0 To TDBRezept.Rows.Count - 1
        KeyId = KeyName(RezSozpt.Rezepte(RzNr)(i).ID)
        TDBRezept.Rows(i).DefaultCellStyle.BackColor = Color.FromArgb(RezSozpt.Farben(KeyId).FarbID)
      Next i
    End If
    '
    '
    'Picgraphic mit aktuellen Daten versorgen
    '
    '
    '
    '
    'R-Werte zur grafischen Darstellung
    '
    '
    '
    '
    PicGraphic.Text = GrpRwerte(0)("V").Name
    PicGraphic.PlotRwerte.clear()
    PicGraphic.PlotRwerte.Add("V", GrpRwerte(0)("V"))
    PicGraphic.PlotRwerte.Add(RzNr, GrpRwerte(0)(RzNr))
    '
    '
    '
    'R-Werte zur Berechnung von FarbWerten
    '
    '
    '
    PicGraphic.FawrtRwerte(0).clear()
    PicGraphic.FawrtRwerte(1).clear()
    PicGraphic.FawrtRwerte(0).Add("V", GrpRwerte(0)("V"))
    PicGraphic.FawrtRwerte(0).Add(RzNr, GrpRwerte(0)(RzNr))
    '
    '
    '
    'Rezepte
    '
    '
    '
    '
    PicGraphic.RzName(0) = RzNr
    '
    '


    '
    txtDEStern.Text = Format(CSng(RezAlleTab.Rows(Rnr)("DE")), "##0.00")
    txtMETA.Text = Format(CSng(RezAlleTab.Rows(Rnr)("META")), "##0.00")
    txtDLStern.Text = Format(CSng(RezAlleTab.Rows(Rnr)("DL")), "##0.00")
    txtDaStern.Text = Format(CSng(RezAlleTab.Rows(Rnr)("Da")), "##0.00")
    txtDbStern.Text = Format(CSng(RezAlleTab.Rows(Rnr)("Db")), "##0.00")
    txtDCStern.Text = Format(CSng(RezAlleTab.Rows(Rnr)("DC")), "##0.00")
    txtDHStern.Text = Format(CSng(RezAlleTab.Rows(Rnr)("DH")), "##0.00")

    Preis = 0.0
    For i = 0 To RezSozpt.Rezepte(RzNr).KF - 1
      KeyId = KeyName(RezSozpt.Rezepte(RzNr)(i).ID)
      Preis = Preis + RezSozpt.Rezepte(RzNr)(i).BaAmng * RezSozpt.Farben(KeyId).Preis
    Next
    txtPreis.Text = Format(Preis, "#######.00")
    PicGraphic.Rmax = -1.0
    PicGraphic.Rmin = -1.0
    pictureBoxFarbe.Refresh()
  End Sub




  Private Sub PictureBoxfarbe_Paint(ByVal sender As Object, ByVal e As System.Windows.Forms.PaintEventArgs) Handles pictureBoxFarbe.Paint
    If IsNothing(PicGraphic) Then Exit Sub
    Try
      e.Graphics.Clear(Color.White)
      PicGraphic.GraphBounds = New RectangleF(0.0 * sender.width, 0.0 * sender.height, 1.0 * sender.width, 1.0 * sender.height)
      PicGraphic.CalcFarbWrt()
      PicGraphic.PicXYZGraph(sender, e.Graphics)
    Finally
    End Try
  End Sub

  

  Private Sub GridAlleRezepte_CellClick(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles GridAlleRezepte.CellClick
    If e.RowIndex < 0 Then Exit Sub
    If e.ColumnIndex <> 0 Then Exit Sub
    Call hscRezepte_Scroll(hscRezepte, New System.Windows.Forms.ScrollEventArgs(ScrollEventType.EndScroll, e.RowIndex))
    PrintPrev.Hide()
    SplitContainPicture.Hide()
    SplitContainAlleRezepte.Hide()
    SplitContainRezept.Show()
  End Sub





  Private Sub btnDruckAlle_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnDruckAlle.Click
    Dim i As Integer
    Dim KzlDr As Integer
    SplitContainRezept.Hide()
    SplitContainPicture.Hide()
    SplitContainAlleRezepte.Hide()



    Dim Printdoc As New PrintDocument
    Printdoc.PrinterSettings = pd.PrinterSettings
    ppv.Document = Printdoc

    '
    '
    '
    '
    '
    '
    '
    '
    'OldINF = RezSozpt.INF
    '
    '
    'Bamng für Gesamtmenge (wg. Batchkonzentrationen
    '
    'RezSozpt.INF = 0
    'If index = 1 Then
    'Call RezDruck.MngUmrech("BAS")
    'Else
    'Call RezDruck.MngUmrech("")
    'End If

    '
    '

    '
    '

    '
    '
    'Alle Rezepte
    '
    RezDruck.Clear()
    RezDruck.KeyNam = RzNr
    ReDim RezDruck.RzName(0)
    RezDruck.RzName(0) = GrpRwerte(0)("V").Name
    '
    '
    ReDim RezDruck.RefNr(1)
    RezDruck.RefNr(0) = "V"
    RezDruck.RefNr(1) = RzNr
    '
    KzlDr = CInt(txtAnzahl.Text)
    RezDruck.Plott.Ksrt.Clear()
    For i = 0 To KzlDr - 1
      RezDruck.Plott.Ksrt.Add(Nrs(i))
    Next
    '
    '
    RezAusgabe.pldr = RezDruck
    AddHandler Printdoc.PrintPage, AddressOf RezAusgabe.MankDruckRezAlle
    'AddHandler Printdoc.PrintPage, AddressOf RezDruck.DruckRezAlle

    Printdoc.DefaultPageSettings.Landscape = False
    ppv.WindowState = FormWindowState.Maximized
    'Preview anzeigen 
    ppv.ShowDialog()

    RemoveHandler Printdoc.PrintPage, AddressOf RezAusgabe.MankDruckRezAlle
    'RemoveHandler Printdoc.PrintPage, AddressOf RezDruck.DruckRezAlle

    Printdoc.Dispose()
    SplitContainRezept.Show()
  End Sub

  Private Sub btnSpeicher_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnSpeicher.Click
    Dim RezName As String
    Dim i As Integer
    '
    'Prüfen, ob FarbmittelID > 100000 (=anderes Mischsystem)
    '
    '
    '
    For i = 0 To RezSozpt.Rezepte(RzNr).KF - 1
      If RezSozpt.Rezepte(RzNr)(i).ID > 100000 Then
        MsgBox(Texxt(4644))
        Exit Sub
      End If
    Next
    RezName = InputBox("", Texxt(4635), RezSozpt.Rezepte(RzNr).Name)
    If RezName <> "" Then
      Call ReWrRezept.ReadRezeptName(RezName, RzId, ier)
      RezSozpt.Rezepte(RzNr).Name = RezName
      For i = 0 To 1
        TypID(i) = GrpRwerte(i)("V").ID
        UntID(i) = GrpRwerte(i).RefUnt.ID
        SmpID(i) = -1
      Next
      RezSozpt.Rezepte(RzNr).Gid = MenueParam.Misch.UserRzpGID
      If RzId = -1 Then
        Call ReWrRezept.AddRezept(RzNr, RzId, RezSozpt, UntID, TypID, SmpID, ier)
      Else
        If MessageBox.Show(Texxt(2955), Texxt(2000), MessageBoxButtons.YesNo) = DialogResult.Yes Then
          Call ReWrRezept.UpdateRezept(RzNr, RzId, RezSozpt, UntID, TypID, SmpID, ier)
        End If
      End If
    End If
  End Sub





  Private Sub btnPictureDrucken_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnPictureDrucken.Click
    'FormShow.ShowDialog()
    'RezDruck.Text0 = FormShow.txtEIN_0.Text
    'RezDruck.Text1 = FormShow.txtEIN_1.Text
    'RezDruck.Text2 = FormShow.txtEIN_2.Text
    RezDruck.PicNr = 1
    RezDruck.KeyNam = RzNr
    RezDruck.RzName(0) = RezSozpt.Rezepte(RzNr).Name
    printdoc = New PrintDocument
    printdoc.PrinterSettings = pd.PrinterSettings
    ppv.Document = printdoc
    AddHandler printdoc.PrintPage, AddressOf RezDruck.DruckPicturebox
    printdoc.DefaultPageSettings.Landscape = True
    ppv.WindowState = FormWindowState.Maximized
    'Preview anzeigen 
    ppv.ShowDialog()
    RemoveHandler printdoc.PrintPage, AddressOf RezDruck.DruckPicturebox
    printdoc.Dispose()
  End Sub
  Private Sub btnRwerteVorlage_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnRwerteVorlage.Click, btnRestVorlage.Click
    Dim Dialo As DialogResult
    Call ControlsVisible(False)
    hscRezepte.Enabled = False
    '
    'Vorlage
    '
    '
    Erase Nrs
    GrpRwerte(0)("V").IVoNa = False
    GetRwerte.Messrefel = GrpRwerte(0)("V")
    Me.Enabled = False
    GetRwerte.Iarch = 0
    GetRwerte.Captext = Texxt(861)
    Call GetRwerte.ReflexWerte(Dialo)

    Me.Enabled = True
    If Dialo <> DialogResult.OK Then Exit Sub
    '
    hscRezepte.Enabled = False

    If GrpRwerte(0)("V").IVoNa Then
      Cursor = Cursors.WaitCursor

      txtRezept.Text = GrpRwerte(0)("V").Name
      txtRezepte.Text = GrpRwerte(0)("V").Name
      txtRestfarbe.Text = GrpRwerte(0)("V").Name
      If sender.name = btnRwerteVorlage.Name Then
        Application.DoEvents()
        btnRezCalc.PerformClick()
      End If
      Cursor = Cursors.Arrow
    End If
    hscRezepte.Enabled = True

  End Sub
  Private Sub btnRwerteUntergrund_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnRwerteUntergrund.Click, btnRestUntergrund.Click
    Call ControlsVisible(False)
    '
    'Untergrund
    '
    '
    Erase Nrs
    GrpRwerte(0).RefUnt.IVoNa = False
    GetRwerte.Messrefel = GrpRwerte(0).RefUnt
    Me.Enabled = False
    GetRwerte.Iarch = 0
    GetRwerte.Captext = Texxt(890)
    Call GetRwerte.ReflexWerte(False)

    Me.Enabled = True
    hscRezepte.Enabled = False

    If GrpRwerte(0).RefUnt.IVoNa Then
      Cursor = Cursors.WaitCursor

      btnRezCalc.Enabled = False
      txtUntergrund.Text = GrpRwerte(0).RefUnt.Name.Trim
      txtRestUntergrund.Text = GrpRwerte(0).RefUnt.Name.Trim
      If sender.name = btnRwerteUntergrund.Name Then
        btnRezCalc.Enabled = True

        btnRezCalc.PerformClick()
      End If
      Cursor = Cursors.Arrow
    End If
    hscRezepte.Enabled = True
  End Sub

  Public Sub New()

    ' This call is required by the Windows Form Designer.
    InitializeComponent()

    ' Add any initialization after the InitializeComponent() call.
    FarbTabelle = New DataTable
    FarbAlleTabelle = New DataTable
    NeueMischTable = New DataTable
    RestfarbenTabelle = New DataTable

  End Sub



  Private Sub TDBRezept_CellFormatting(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewCellFormattingEventArgs) Handles TDBRezept.CellFormatting
    If RzNr = "" Then Exit Sub
    If e.ColumnIndex = 2 AndAlso e.RowIndex >= 0 AndAlso e.RowIndex < RezSozpt.Rezepte(RzNr).KF Then
      If Not IsDBNull(e.Value) Then
        If RezSozpt.Farben.ContainsFarb(KeyName(RezSozpt.Rezepte(RzNr)(e.RowIndex).ID)) Then
          e.Value = Format(e.Value, RezSozpt.Farben(KeyName(RezSozpt.Rezepte(RzNr)(e.RowIndex).ID)).Form)
        End If
      End If
    End If
  End Sub
  Private Sub TDBRezept_CellEndEdit(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles TDBRezept.CellEndEdit
    Dim NewMenge As Single
    Dim ivol As Integer
    ReDim Rzkey(0)
    Rzkey(0) = RzNr
    If RezTab.Rows.Count = 0 Then Exit Sub
    If e.ColumnIndex = 2 And e.RowIndex = RezSozpt.Rezepte(RzNr).KF Then
      NewMenge = RezTab.Rows(e.RowIndex)("GEWICHT")
      ivol = RezSozpt.IVOL
      Erase Rzkey
      For i = 0 To RezSozpt.Rezepte.RezCount - 1
        If IsNumeric(RezSozpt.Rezepte.RezKey(i)) Then
          If IsNothing(Rzkey) Then
            ReDim Rzkey(0)
          Else
            ReDim Preserve Rzkey(Rzkey.Count)
          End If
          Rzkey(Rzkey.Count - 1) = RezSozpt.Rezepte.RezKey(i)
        End If
      Next
      Call RezeptNewMenge(ivol, NewMenge, Umr, Rzkey, RezSozpt, ier)
      Call Umr.CalcFamng(RzNr, RezSozpt, ier)
      Call GridRezept(RzNr, RezSozpt)
    End If
  End Sub

  Private Sub TDBRezept_CellEnter(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles TDBRezept.CellEnter
    If e.ColumnIndex = 2 And e.RowIndex = RezSozpt.Rezepte(RzNr).KF Then
      TDBRezept.ReadOnly = False
    End If
  End Sub

  Private Sub TDBRezept_CellLeave(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles TDBRezept.CellLeave
    TDBRezept.ReadOnly = True
  End Sub
  Private Sub TDBRezept_DataError(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewDataErrorEventArgs) Handles TDBRezept.DataError
   
  End Sub
  Private Sub TDBRezept_ColumnWidthChanged(sender As Object, e As System.Windows.Forms.DataGridViewColumnEventArgs) Handles TDBRezept.ColumnWidthChanged
    Dim i As Integer
    Dim WholeWidth As Integer
    WholeWidth = TDBRezept.RowHeadersWidth
    For i = 1 To txtSUM.Count - 1
      txtSUM(i).Width = TDBRezept.Columns(i).Width
      txtSUM(i).Left = WholeWidth
      WholeWidth = WholeWidth + TDBRezept.Columns(i).Width
    Next
  End Sub

  Private Sub frmMankiewRezept_Resize(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Resize
    If Not Me.Visible Then Exit Sub

    Call ResizeChild(Me)
  End Sub




  Private Sub btnSortiverw_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnSortiverw.Click
    txtDickeSor.Text = txtDickeRez.Text
    Call AllPanelHide()
    SplitContainSortiment.Show()
    Application.DoEvents()
  End Sub

  Function DatTabChange(ByRef DatTab As DataTable) As Boolean
    Dim Roo As DataRow
    Dim imsg As Integer
    If IsNothing(DatTab) Then Exit Function
    If DatTab.Rows.Count = 0 Then Exit Function
    DatTabChange = False
    For Each Roo In DatTab.Rows
      If Roo.RowState <> DataRowState.Unchanged OrElse Roo("FARBM_ID") > 100000 Then
        imsg = MessageBox.Show(Texxt(4638), Texxt(2000), MessageBoxButtons.YesNo)
        If imsg = 6 Then
          DatTabChange = True
        End If
        Exit For
      End If
    Next
  End Function
  Private Sub btnSortimentZurück_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnSortimentZurück.Click

    Call ControlsVisible(False)
    '
    Call GetSortiment()
    '
    Call AllPanelHide()
    SplitContainRezept.Show()
    btnRezCalc.Enabled = True
  End Sub



  Private Sub cboNeuesMischsystem_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles cboNeuesMischsystem.SelectedIndexChanged
    If cboNeuesMischsystem.SelectedIndex = -1 Then Exit Sub
    If Not cboNeuesMischsystem.Enabled Then Exit Sub
    Dim OneGlzGrd As String
    Dim WithMessg As String
   
    OneGlzGrd = " AND TBL_FARBM.FARBM_ID=GLZGRD_ID"

    WithMessg = "TBL_GRUND_FARBM.MESSGRW_ID=" & MenueParam.Messg.MessgRwID

    'Liste der auswählbaren Farb-/Bindemittel
    '
    FarbAlleTabelle.Clear()
    lstFarbmittel.DataSource = FarbAlleTabelle
    DataAdapter.SelectCommand.CommandText = "SELECT TBL_FARBM.FARBM_ID, TBL_FARBM.FARBM_NAME,FARBM_ICHF,FARBM_FST,FARBM_SMENGE,FARBM_FARBID " _
    & "FROM TBL_FARBM INNER JOIN TBL_GRUND_FARBM ON (TBL_FARBM.FARBM_ID = TBL_GRUND_FARBM.FARBM_ID) AND (TBL_FARBM.MISCH_ID = TBL_GRUND_FARBM.MISCH_ID)" _
    & "WHERE (((TBL_GRUND_FARBM.GKWRT_ID)=" & MenueParam.Misch.GKwrtID & ") AND ((TBL_FARBM.MISCH_ID)=" & cboNeuesMischsystem.SelectedValue & "))" & " AND " & WithMessg & OneGlzGrd & " ORDER BY FARBM_NAME;"
    FarbAlleTabelle.Clear()
    If Not FillDatset(DataAdapter, FarbAlleTabelle) Then
      Exit Sub
    End If
    FarbAlleTabelle.AcceptChanges()
    lstFarbmittel.DisplayMember = "FARBM_NAME"
    lstFarbmittel.ValueMember = "FARBM_ID"


  End Sub

  Private Sub lstFarbmittel_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles lstFarbmittel.Click
    Dim i As Integer
    Dim row As DataRow
    Dim FaID As Integer
    Dim HilfID As Integer
    Dim Mialt As Integer
    Dim Mineu As Integer
    '
    '
    '
    Mialt = MenueParam.MischID
    Mineu = cboNeuesMischsystem.SelectedValue
    If cboNeuesMischsystem.SelectedValue = MenueParam.MischID Then
      FaID = FarbAlleTabelle.Rows(lstFarbmittel.SelectedIndex)("FARBM_ID")
    Else
      FaID = FarbAlleTabelle.Rows(lstFarbmittel.SelectedIndex)("FARBM_ID") + 100000 * Mineu
    End If
    '
    '
    '
    '
    'Prüfen, ob Farbmittel bereits vorhanden
    '
    '
    For i = 0 To FarbTabelle.Rows.Count - 1
      If Not FarbTabelle.Rows(i).RowState = DataRowState.Deleted Then
        If CInt(FarbTabelle.Rows(i)("FARBM_ID")) = FaID Then
          Exit Sub
        End If
      End If
    Next
    '
    '
    '
    row = FarbTabelle.NewRow
    row("FARBM_ID") = FaID
    '
    '
    'Prüfen, ob Farbmittel von neuem Mischsystem
    '
    '
    HilfID = FaID
    If Mialt = Mineu Then
      FaID = HilfID
    Else
      FaID = HilfID Mod 100000
      AufbauPar.MischID = Mineu
    End If
    '
    '
    ReWrFarbe.FarRea(FaID, ArbFarb, True, ier)
    '
    'Mischsystem zurücksetzen
    '
    '
    '
    AufbauPar.MischID = Mialt
    '
    '
    '
    row("FARBM_NAME") = FarbAlleTabelle.Rows(lstFarbmittel.SelectedIndex)("FARBM_NAME")
    row("FARBM_MENGE") = FarbAlleTabelle.Rows(lstFarbmittel.SelectedIndex)("FARBM_SMENGE")
    row("FARBM_FST") = FarbAlleTabelle.Rows(lstFarbmittel.SelectedIndex)("FARBM_FST")
    row("FARBM_PROZ") = ArbFarb.Prf(0)
    row("FARBM_PROB") = ArbFarb.Prb(0)
    row("FARBM_PREIS") = ArbFarb.Pre(0)
    row("FARBM_TOPF") = " "
    row("FARBM_OPERAT") = " "
    row("FARBM_LIMMNG") = 0.0
    row("FARBM_ICHF") = FarbAlleTabelle.Rows(lstFarbmittel.SelectedIndex)("FARBM_ICHF")
    row("FARBM_FARBID") = FarbAlleTabelle.Rows(lstFarbmittel.SelectedIndex)("FARBM_FARBID")

    FarbTabelle.Rows.Add(row)
    If BitWrt(30, MenueParam.User.Writ) Then
      For i = 0 To FarbTabelle.Rows.Count - 1
        flgFarbmittel.Rows(i).DefaultCellStyle.BackColor = Color.FromArgb(FarbTabelle.Rows(i)("FARBM_FARBID"))
      Next i
    End If
  End Sub



 

  Private Sub cboMNG_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles cboMNG.SelectedIndexChanged
    RezSozpt.INO = sender.selectedindex
  End Sub

  Private Sub cboPROZNen_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles cboPROZNen.SelectedIndexChanged
    RezSozpt.INQ = sender.selectedindex
  End Sub

  Private Sub cboPROZZae_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles cboPROZZae.SelectedIndexChanged
    RezSozpt.INP = sender.selectedindex
  End Sub

  Private Sub flgFarbmittel_RowHeaderMouseDoubleClick(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewCellMouseEventArgs) Handles flgFarbmittel.RowHeaderMouseDoubleClick
    flgFarbmittel.Rows.RemoveAt(e.RowIndex)
  End Sub
  Sub GetNameID(ByRef ID As Integer, ByRef Name As String, ByRef Banum As String)
    If GetRwerte.HideSofort Then
      GetRwerte.InVisible()
    End If
  End Sub
  Private Sub flgFarbmittel_DataError(sender As Object, e As System.Windows.Forms.DataGridViewDataErrorEventArgs) Handles flgFarbmittel.DataError
    e.Cancel = False
  End Sub

  '
  '
  '
  'Restfarbe
  '
  '
  '
  '
  '
  Private Sub btnCalcRestfarbe_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnCalcRestfarbe.Click
    Call AllPanelHide()
    SplitContainRestfarbe.Show()
    Call GetSortiment()
    lstRestfarbe.Visible = False
    btnSelectRestfarbe.Visible = False
    btnElimRestfarbe.Visible = False
  End Sub
  Private Sub btnRestfarbe_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnRestfarbe.Click
    Dim SqlStmt As String
    Dim LimMenge As String
    Dim CmdDys As OleDbCommand
    Cursor = Cursors.WaitCursor
    lstRestfarbe.Visible = False
    btnSelectRestfarbe.Visible = False
    btnElimRestfarbe.Visible = False
    btnRestUntergrund.Enabled = False
    '
    '
    'Restfarbe berechnen 
    '
    '
    '
    '
    CalcRezept.Restfarbe(1, KeySor, "BAS", RezSozpt, GrpRwerte, RestFarb, ier)
    btnRestUntergrund.Enabled = True
    Cursor = Cursors.Arrow
    If ier <> 0 Then
      MsgBox(Texxt(ier))
      Exit Sub
    End If
    '
    '
    'Restfarbe abspeichern
    '
    '
    '
    '
    RestFarb.Name = InputBox(Texxt(4655) & String.Format("{0:\ #dd/MM/yyyy\ HH:mm}", Now()), Texxt(986), GrpRwerte(0)("V").Name)
    '
    '
    '
    If RestFarb.Name <> "" Then
      RestFarb.PrNr = GetPrivSettings("MANKIEWICZ", "PRODNR", " ", COLORFileName)
      RestFarb.Name = "#" & RestFarb.Name & " (" & String.Format("{0:dd/MM/yyyy\ HH:mm}", Now()) & ")"

      RestFarb.GlzGrdID = -1
      RestFarb.GlzGrd = 0.0
      RestFarb.Aname = RestFarb.Name
      WriteRestFarbe.FarAdd(RestFarb, ier)
      WriteRestGrund.WriteGrund(RestFarb.ID, RestFarb.OptData, MenueParam.Messg.Winkel, ier)
      '
      '
      '
      If BitWrt(5, MenueParam.User.Writ) Then

        'Sortiment ergänzen
        '
        '
        CmdDys = New OleDbCommand

        LimMenge = InputBox(Texxt(4656), Texxt(4657), CStr(RezSozpt.MngMax))
        If LimMenge <> "" Then
          SqlStmt = "INSERT INTO TBL_SORTI_FARBM ([SORTI_ID],[MISCH_ID],[FARBM_ID],[FARBM_MENGE],[FARBM_LIMMNG],[FARBM_OPERAT],[FARBM_TOPF]," _
          & "[FARBM_PREIS],[FARBM_PROZ],[FARBM_PROB],[FARBM_IPOS])" _
          & " VALUES(" & SortID & "," & MenueParam.MischID & "," & RestFarb.ID & ",0.0," & SQLpunkt(LimMenge) _
          & ",'<',' ',0.0,100.0,100.0," & CByte(RezSozpt.Rezepte(KeySor).KF + 1) & ")"
          CmdDys.CommandText = SqlStmt
          If SQLExeNonQuery(CmdDys, Cndat) <> 0 Then
            ier = -1
          End If
          CmdDys.Dispose()
        End If
      End If
      'MsgBox(Texxt(986) & ": " & RestFarb.Name & Space(1) & Texxt(4634))
    End If
  End Sub


  Private Sub btnShowRestfarbe_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnShowRestfarbe.Click
    Dim oledbcommandrest As OleDbCommand
    Dim DataHlfAdapter As OleDbDataAdapter
    DataHlfAdapter = New OleDbDataAdapter
    oledbcommandrest = New OleDbCommand
    oledbcommandrest.Connection = Cndat()
    oledbcommandrest.CommandText = "SELECT * FROM TBL_FARBM WHERE MISCH_ID=" & MenueParam.MischID & " AND FARBM_ICHF=6"
    DataHlfAdapter.SelectCommand = oledbcommandrest
    If Not FillDatset(DataHlfAdapter, RestfarbenTabelle) Then
      Exit Sub
    End If
    RestfarbenTabelle.AcceptChanges()
    lstRestfarbe.DisplayMember = "FARBM_NAME"
    lstRestfarbe.ValueMember = "FARBM_ID"
    oledbcommandrest.Dispose()
    DataHlfAdapter.Dispose()
    lstRestfarbe.Visible = True
    btnSelectRestfarbe.Visible = True
    btnElimRestfarbe.Visible = True

  End Sub
  Private Sub btnSelectRestfarbe_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnSelectRestfarbe.Click
    For i = 0 To lstRestfarbe.Items.Count - 1
      lstRestfarbe.SetSelected(i, True)
    Next
  End Sub
  Private Sub btnElimRestfarbe_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnElimRestfarbe.Click
    Dim Darow As DataRow
    For i = 0 To lstRestfarbe.SelectedItems.Count - 1
      RestFarb.ID = lstRestfarbe.SelectedItems(i)("FARBM_ID")
      RestFarb.Name = lstRestfarbe.SelectedItems(i)("FARBM_NAME")
      WriteRestFarbe.FarDel(RestFarb, False, ier)
      For Each Darow In FarbTabelle.Rows
        If Not Darow.RowState = DataRowState.Deleted Then
          If RestFarb.ID = Darow("FARBM_ID") Then
            Darow.Delete()
            Exit For
          End If
        End If
      Next

    Next i
    btnShowRestfarbe.PerformClick()
    Call TabToRez(KeySor, RezSozpt, FarbTabelle)
    Umr.CalcFamng(KeySor, RezSozpt, ier)
    FarbTabelle.AcceptChanges()
    RestfarbenTabelle.AcceptChanges()
  End Sub


  Private Sub btnRestfarbeZurück_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnRestfarbeZurück.Click
    cboNeuesMischsystem.SelectedIndex = -1
    cboNeuesMischsystem.SelectedValue = MenueParam.MischID
    Call AllPanelHide()
    SplitContainSortiment.Show()
  End Sub

  Sub LichtartWinkel_click(ByVal sender As Object, ByVal e As System.EventArgs)
    If Not Me.Visible Then Exit Sub
    Call SetPicgraphics()
    Call LichtArtText()
    Picauf.Refresh()
    'PictureBoxLab.Refresh()
    'PictureBoxFarbWerte.Refresh()
    'PictureBoxRezept.Refresh()
    'PictureBoxKurve.Refresh()
    Call DisplayAllRezept(RezAlleTab)
    Call hscRezepte_Scroll(hscRezepte, New System.Windows.Forms.ScrollEventArgs(ScrollEventType.EndScroll, hscRezepte.Value))
    Call pictureBoxFarbe.Refresh()
  End Sub
  Sub LichtArtText()
    Dim i As Integer
    Dim Ilicht As Integer
    For i = 0 To radLichtart.Count - 1
      If radLichtart(i).Checked Then
        Ilicht = i
        Exit For
      End If
    Next
    lblDEStern.Text = FarbWrtStd(0).Merk(MenueParam.Menue.StdMrkKen(15)).Kbez.Trim & "(" & MenueParam.Normfa(Ilicht).NormKenn & ")"
    lblDLStern.Text = FarbWrtStd(0).Merk(MenueParam.Menue.StdMrkKen(16)).Kbez.Trim & "(" & MenueParam.Normfa(Ilicht).NormKenn & ")"
    lblDaStern.Text = FarbWrtStd(0).Merk(MenueParam.Menue.StdMrkKen(19)).Kbez.Trim & "(" & MenueParam.Normfa(Ilicht).NormKenn & ")"
    lblDbStern.Text = FarbWrtStd(0).Merk(MenueParam.Menue.StdMrkKen(20)).Kbez.Trim & "(" & MenueParam.Normfa(Ilicht).NormKenn & ")"
    lblDCStern.Text = FarbWrtStd(0).Merk(MenueParam.Menue.StdMrkKen(17)).Kbez.Trim & "(" & MenueParam.Normfa(Ilicht).NormKenn & ")"
    lblDHStern.Text = FarbWrtStd(0).Merk(MenueParam.Menue.StdMrkKen(18)).Kbez.Trim & "(" & MenueParam.Normfa(Ilicht).NormKenn & ")"

  End Sub

  Sub SetPicgraphics()
    PicGraphic.WeSc(0) = True
    PicGraphic.WeSc(1) = False
    For i = 0 To radLichtart.Count - 1
      If radLichtart(i).Checked Then
        PicGraphic.Knlz = i
      End If
    Next i
    For i = 0 To radwinkel.Count - 1
      If radwinkel(i).Checked Then
        PicGraphic.Kwop = i
      End If
    Next i
    For i = 0 To chkwinkel.Count - 1
      PicGraphic.kwopt(i) = False
      If chkwinkel(i).Checked Then
        PicGraphic.kwopt(i) = True
      End If
    Next i
  End Sub




  Private Sub btnUserProg_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnUserProg.Click
    Call UserDosieranlage(RzNr, RezSozpt)
  End Sub

 
  
  Private Sub frmMankiewRezept_Shown(sender As Object, e As System.EventArgs) Handles Me.Shown
    Call AllPanelHide()
    SplitContainRezept.Show()
    Call ShowMe(sender, e)
  End Sub

  Private Sub txt_Enter(sender As Object, e As System.EventArgs) Handles txtDaStern.Enter, txtDbStern.Enter, txtDLStern.Enter, txtDCStern.Enter, txtDHStern.Enter, txtDEStern.Enter, txtDickeRez.Enter, txtDickeSor.Enter, txtGewDL.Enter, txtGewDC.Enter, txtGewDH.Enter, txtGewNL_0.Enter, txtGewNL_1.Enter, txtGewNL_2.Enter, txtGewNL_3.Enter, txtGewNL_4.Enter, txtGewNL_5.Enter, txtMETA.Enter, txtMngMax.Enter, txtMngMin.Enter, txtProzMax.Enter, txtProzMin.Enter
    sender.tag = sender.text
  End Sub

  Private Sub txt_TextChanged(sender As Object, e As System.EventArgs) Handles txtDaStern.TextChanged, txtDbStern.TextChanged, txtDLStern.TextChanged, txtDCStern.TextChanged, txtDHStern.TextChanged, txtDEStern.TextChanged, _
    txtDickeRez.TextChanged, txtDickeSor.TextChanged, txtGewDL.TextChanged, txtGewDC.TextChanged, txtGewDH.TextChanged, _
    txtGewNL_0.TextChanged, txtGewNL_1.TextChanged, txtGewNL_2.TextChanged, txtGewNL_3.TextChanged, txtGewNL_4.TextChanged, txtGewNL_5.TextChanged, _
    txtMETA.TextChanged, txtMngMax.TextChanged, txtMngMin.TextChanged, txtProzMax.TextChanged, txtProzMin.TextChanged, txtGlzGrd.TextChanged
    If Not IsNumeric(sender.text) Then
      sender.text = sender.tag
    End If
  End Sub


  Private Sub txtSUM_2_KeyPress(sender As Object, e As System.Windows.Forms.KeyPressEventArgs) Handles txtSUM_2.KeyPress
    If e.KeyChar = vbCr Then
      sender.enabled = False
      sender.enabled = True
    End If
  End Sub
  Private Sub txtSUM_2_Leave(sender As Object, e As System.EventArgs) Handles txtSUM_2.Leave
    Dim i As Integer
    Dim NewMenge As Single
    Dim ivol As Integer
    ivol = RezSozpt.IVOL
    If Not IsNumeric(txtSUM_2.Text) Then Exit Sub
    NewMenge = CSng(txtSUM(2).Text)
    Erase Rzkey
    For i = 0 To RezSozpt.Rezepte.RezCount - 1
      If IsNumeric(RezSozpt.Rezepte.RezKey(i)) Then
        If IsNothing(Rzkey) Then
          ReDim Rzkey(0)
        Else
          ReDim Preserve Rzkey(Rzkey.Count)
        End If
        Rzkey(Rzkey.Count - 1) = RezSozpt.Rezepte.RezKey(i)
      End If
    Next
    Call RezeptNewMenge(ivol, NewMenge, Umr, Rzkey, RezSozpt, ier)
    Call Umr.CalcFamng(RzNr, RezSozpt, ier)
    Call GridRezept(RzNr, RezSozpt)
  End Sub
  Private Sub txtNum_Validating(sender As Object, e As System.ComponentModel.CancelEventArgs) Handles txtMngMin.Validating, txtMngMax.Validating, txtProzMin.Validating, txtProzMax.Validating, _
    txtDickeRez.Validating, txtDickeSor.Validating, txtSUM_2.Validating, txtDaStern.Validating, txtDbStern.Validating, txtDCStern.Validating, txtDHStern.Validating, txtDEStern.Validating, _
    txtDLStern.Validating, txtGewDC.Validating, txtGewDH.Validating, txtGewDL.Validating, _
    txtGewNL_0.Validating, txtGewNL_1.Validating, txtGewNL_2.Validating, txtGewNL_3.Validating, txtGewNL_4.Validating, txtGewNL_5.Validating, _
    txtMETA.Validating, txtPreis.Validating
    If Not IsNumeric(sender.text) Then
      e.Cancel = True
      MsgBox(Texxt(60))
    End If
  End Sub

  
  Private Sub btnCopyFarbmittel_Click(sender As Object, e As System.EventArgs) Handles btnCopyFarbmittel.Click
    Dim ID As Integer
    Dim ier As Integer
    Dim Fast As String
    If flgFarbmittel.SelectedRows.Count = 0 Then Exit Sub
    ID = flgFarbmittel.Rows(flgFarbmittel.SelectedRows(0).Index).Cells("FARBM_ID").Value
    ReWrFarbe.FarReaGrund(ID, ArbFarb, False, ier)
    '
    '
    'Neuer Farbname
    '
    '
    '
    ArbFarb.Name = InputBox(Texxt(815), Texxt(900), ArbFarb.Name)
    If ArbFarb.Name = "" Then
      Exit Sub
    End If

    '
    '
    ReWrFarbe.ReadFarbmName(ArbFarb.Name, ID, ier)
    If ID > -1 Then
      MsgBox(Texxt(2954))
      Exit Sub
    End If
    '
    'Farbstärke
    '
    Fast = InputBox(Texxt(909), Texxt(735), 100)
    If Fast = "" OrElse Not IsNumeric(Fast) Then
      Exit Sub
    End If
    '
    ArbFarb.Fst = CSng(Fast)
    '
    '
    '
    ArbFarb.GlzGrdID = -1
    ArbFarb.GlzGrd = 0.0
    ReWrFarbe.FarAdd(ArbFarb, ier)
    If ier <> 0 Then Exit Sub
    '
    '
    '
    'Grunddaten speichern
    '
    '
    '
    '
    ReWrGrund.WriteGrund(ArbFarb.ID, ArbFarb.OptData, MenueParam.Messg.Winkel, ier)
    '
    '
    '
    '
    FarbAlleTabelle.Rows.Clear()
    If Not FillDatset(DataAdapter, FarbAlleTabelle) Then
      Exit Sub
    End If
    FarbAlleTabelle.AcceptChanges()
  End Sub
End Class