Public Class HandleRezGeneral

  Dim WithEvents Mncbogrp As ComboBox
  Dim Mnlblgrp As Label
  Dim HandleRwrt As HandleRwerte

  Dim ViewRezepte As New DataView(MenueParam.GroupTableRezept)
  Private Property KeyMenge As String
  Function AutFormat(ByRef wert As Single) As String
    Dim Forr As String
    Dim i As Short
    Dim ijv As Short
    Dim ijn As Short
    ijv = 3
    ijn = 3
    If wert < 0.001 Then
      AutFormat = Format(wert, "0.000")
    ElseIf wert > 1000 Then
      AutFormat = Format(wert, "########.")
    Else
      i = CShort(Log10(wert))
      ijv = 1
      ijn = 5
      ijn = ijn - i
      ijv = ijv + i
      Forr = Sline(ijv, "#") & "." & Sline(ijn, "0")
      AutFormat = Format(wert, Forr)
    End If
  End Function
  Function AddDelP(ByRef nform As Short) As Boolean
    AddDelP = AddModDelP(nform)
  End Function
  Sub ClearRezepte(ByRef Rezpte As RecipesGrp)
    Dim i As Short
    '
    '
    'Vorhandene Farbmittel in Rezepten Löschen
    '
    '
    For i = 0 To Rezpte.Rezepte.RezCount - 1
      Rezpte.Rezepte(i).clear()
    Next i
    '
    'Vorhandene Farb-/Bindemittel löschen
    '
    '
    ''
  End Sub
  Function MischSelectCommand() As String
    MischSelectCommand = "SELECT DISTINCT TBL_MISCH.MISCH_ID AS MISCH_ID,TBL_MISCH.MISCH_KBEZ AS MISCH_KBEZ" _
    & " FROM (TBL_MISCH INNER JOIN TBL_USER_MISCH ON TBL_MISCH.MISCH_ID = TBL_USER_MISCH.MISCH_ID)" _
    & " INNER JOIN TBL_MISCH_MESSG ON TBL_MISCH.MISCH_ID = TBL_MISCH_MESSG.MISCH_ID" _
    & " WHERE ((TBL_USER_MISCH.USER_ID = " & MenueParam.UserID & ") And (TBL_MISCH_MESSG.MESSG_ID=" _
    & MenueParam.MessgID & ") AND (MISCH_SET=YES)) ORDER BY MISCH_KBEZ;"
  End Function

 
  Sub SetSpecial()
    'MenueParam.Messg.CDE = "U0"
  End Sub
  Sub GrundGlzGrd(MischID As Integer, Farbid As Integer, ByRef Arbfarb As Colorant, GlzGrd As Single, ByRef ier As Integer)
    Dim i As Integer
    Dim FarbHilfID As Integer
    Dim ArbFarbHilf As Colorant
    Dim TabHilfsFarb As New DataTable
    Dim AdaptHilfsFarb As New OleDbDataAdapter
    Dim FarbHilfsCollection As New Colorants
    Dim RewrFarbe As New ReadWriteFarbe
    Dim Calcrezept As New RezeptBerechnung
    ier = 0
    AdaptHilfsFarb.SelectCommand = New OleDbCommand("SELECT * FROM TBL_FARBM WHERE MISCH_ID=" & MischID & " AND GLZGRD_ID=" & Farbid, Cndat)
    If Not FillDatset(AdaptHilfsFarb, TabHilfsFarb) Then
      Exit Sub
    End If
    If TabHilfsFarb.Rows.Count < 2 Then
      '
      'zu wenig Farbmitel mit gleicher GLZGRD_ID gefunden; ARBFARB wird nicht geändert
      '
      '
      Exit Sub
    End If
    For i = 0 To TabHilfsFarb.Rows.Count - 1
      FarbHilfID = TabHilfsFarb.Rows(i)("FARBM_ID")
      ArbFarbHilf = New Colorant
      '
      'Farbmittel und optische Daten werden eingelesen
      '
      '
      Call RewrFarbe.FarReaGrund(FarbHilfID, ArbFarbHilf, 0.0, ier)
      FarbHilfsCollection.AddFarb(KeyRe(i), ArbFarbHilf)
    Next
    For i = 0 To FarbHilfsCollection.FarbCount - 1
      If FarbHilfsCollection(i).ID = FarbHilfsCollection(i).GlzGrdID Then
        Arbfarb.Name = FarbHilfsCollection(i).Name
        Exit For
      End If
    Next
    If i > FarbHilfsCollection.FarbCount - 1 Then
      ier = -2
      Exit Sub
    End If
    Call Calcrezept.GrundGlzGrdCalc(FarbHilfsCollection, MenueParam.Messg.Winkel, Arbfarb, GlzGrd, ier)
    Arbfarb.Name = Arbfarb.Name & Space(1) & "G(" & Format(GlzGrd, "###.0") & ")"
    ArbFarbHilf.dispose()
    TabHilfsFarb.Dispose()
    AdaptHilfsFarb.Dispose()
    FarbHilfsCollection.dispose()
    RewrFarbe.dispose()
    Calcrezept.dispose()
  End Sub
  Sub RezSpeiObs(ByVal Iarch As Integer, ByVal RzNr As String, ByVal RezSozpt As RecipesGrp, ByVal Untid() As Integer, ByVal Typid() As Integer, ByVal Smpid() As Integer, ByVal ier As Integer)
    Dim RezID As Long
    Dim StrName As String
    Dim RwWrRezept As New ReadWriteRezept
    ier = 0
    '
    '
    '
    'Aktuelles Rezept abspeichern
    '
    '
    '
    '
    '
    RezSozpt.Rezepte(RzNr).Gid = Mncbogrp.SelectedValue
    RezSozpt.Rezepte(RzNr).Iarch = Iarch
    If MeldSpeiAllRzp(False) Then
      StrName = InputBox(Texxt(2104), Texxt(2000), Trim(RezSozpt.Rezepte(RzNr).Name))
      If StrName <> "" Then
        RezSozpt.Rezepte(RzNr).Name = StrName
        Call RwWrRezept.AddRezept(RzNr, RezID, RezSozpt, Untid, Typid, Smpid, ier)
      End If
    End If

    RwWrRezept = Nothing
  End Sub
  
  Sub TabRezRecord(ByVal Winkel As AngGeos, ByVal RzNr As String, ByRef RezSozpt As RecipesGrp, ByVal RefWertUNT As List(Of RefValue), ByVal RefWertTYP As List(Of RefValue), ByVal RefWertSMP As List(Of RefValue), ByRef TabWrt As DataTable, ByRef ier As Integer)
    Dim i As Integer
    Dim FaID As Integer
    Dim RowWrt As DataRow


    '
    '
    'Rezepte einlesen
    '
    RowWrt = TabWrt.NewRow
    RowWrt(0) = "0"
    RowWrt(1) = RezSozpt.Rezepte(RzNr).Name
    RowWrt(2) = RezSozpt.Rezepte(RzNr).Bem
    RowWrt(3) = RezSozpt.Rezepte(RzNr).DatTim
    RowWrt(4) = RezSozpt.Rezepte(RzNr).Dicke(0) & "|" & RezSozpt.Rezepte(RzNr).Dicke(1)
    TabWrt.Rows.Add(RowWrt)
    '
    '
    'Farbmittel
    '
    '
    For i = 0 To RezSozpt.Rezepte(RzNr).KF - 1
      FaID = RezSozpt.Rezepte(RzNr)(i).ID
      RowWrt = TabWrt.NewRow
      RowWrt(0) = "#" & CStr(FaID)
      RowWrt(1) = RezSozpt.Farben(KeyName(FaID)).Name
      RowWrt(2) = Format(RezSozpt.Rezepte(RzNr)(i).FaAmng, RezSozpt.Farben(KeyName(FaID)).Form)
      TabWrt.Rows.Add(RowWrt)
    Next i

    '
    '

    '
    '
    '
    'Vorlage über weiß
    '
    '
    If Not IsNothing(RefWertTYP) AndAlso Not IsNothing(RefWertTYP(0)) AndAlso RefWertTYP(0).ID <> -1 Then
      Call HandleRwrt.TabRefRecord(11, Winkel, 100.0, RefWertTYP(0), TabWrt, ier)
    End If
    '
    'Vorlage über schwarz
    '
    '
    '
    If Not IsNothing(RefWertTYP) AndAlso RefWertTYP.Count > 1 AndAlso Not IsNothing(RefWertTYP(1)) AndAlso RefWertTYP(1).ID <> -1 Then
      Call HandleRwrt.TabRefRecord(12, Winkel, 100.0, RefWertTYP(1), TabWrt, ier)
    End If


    '
    'Nachstellung über weiß
    '
    '
    If Not IsNothing(RefWertSMP) AndAlso Not IsNothing(RefWertSMP(0)) AndAlso RefWertSMP(0).ID <> -1 Then
      Call HandleRwrt.TabRefRecord(1, Winkel, 100.0, RefWertSMP(0), TabWrt, ier)
    End If
    '
    '
    'Nachstellung über schwarz
    '
    '
    '
    If Not IsNothing(RefWertSMP) AndAlso RefWertSMP.Count > 1 AndAlso Not IsNothing(RefWertSMP(1)) AndAlso RefWertSMP(1).ID <> -1 Then
      Call HandleRwrt.TabRefRecord(2, Winkel, 100.0, RefWertSMP(1), TabWrt, ier)
    End If
    '
    'Untergrund weiß
    '
    '
    '
    If Not IsNothing(RefWertUNT) AndAlso Not IsNothing(RefWertUNT(0)) AndAlso RefWertUNT(0).ID <> -1 Then
      Call HandleRwrt.TabRefRecord(-1, Winkel, 100.0, RefWertUNT(0), TabWrt, ier)
    End If
    '
    '
    'Untergrund schwarz
    '
    '
    '
    If Not IsNothing(RefWertUNT) AndAlso RefWertUNT.Count > 1 AndAlso Not IsNothing(RefWertUNT(1)) AndAlso RefWertUNT(1).ID <> -1 Then
      Call HandleRwrt.TabRefRecord(-2, Winkel, 100.0, RefWertUNT(1), TabWrt, ier)
    End If




  End Sub
  Function IVorNa(ByVal CRef As String, GrpRwerte As RefValuesGrp, chkunt As List(Of CheckBox), chkKDE As CheckBox, chkNAS As CheckBox) As Boolean
    IVorNa = True

    Select Case CRef

      'Aktivieren R-Werte Vorlage
      '
      Case "V"
        '
        If GrpRwerte(0)("V").ID >= 0 Then
          GrpRwerte(0)("V").IVoNa = True
          GrpRwerte(0)("V").Iplott = False
        Else
          GrpRwerte(0)("V").IVoNa = False
        End If
        If chkunt(0).Checked Then
          If GrpRwerte(0)("V").ID >= 0 Then
            GrpRwerte(0)("V").IVoNa = True
            GrpRwerte(0)("V").Iplott = True
          Else
            MsgBox(Texxt(2960))
            IVorNa = False
          End If
        End If
        If GrpRwerte(1)("V").ID >= 0 Then
          GrpRwerte(1)("V").IVoNa = True
          GrpRwerte(1)("V").Iplott = False
        Else
          GrpRwerte(1)("V").IVoNa = False
        End If
        If MenueParam.Misch.Transp And chkunt(1).Checked Then
          If GrpRwerte(1)("V").ID >= 0 Then
            GrpRwerte(1)("V").IVoNa = True
            GrpRwerte(1)("V").Iplott = True
          Else
            MsgBox(Texxt(2961))
            IVorNa = False
          End If
        Else
          GrpRwerte(1)("V").IVoNa = False
          GrpRwerte(1)("V").Iplott = False
        End If
        '
        '
      Case "N"

        'Aktivieren R-Werte Nachstellung
        '
        '
        If GrpRwerte(0)("N").ID >= 0 Then
          GrpRwerte(0)("N").IVoNa = True
          GrpRwerte(0)("N").Iplott = False
        Else
          GrpRwerte(0)("N").IVoNa = False
        End If
        If chkunt(0).Checked Then
          If GrpRwerte(0)("N").ID >= 0 Then
            GrpRwerte(0)("N").IVoNa = True
            GrpRwerte(0)("N").Iplott = True
          Else
            MsgBox(Texxt(2962))
            IVorNa = False
          End If
        End If
        If GrpRwerte(1)("N").ID >= 0 Then
          GrpRwerte(1)("N").IVoNa = True
          GrpRwerte(1)("N").Iplott = False
        Else
          GrpRwerte(1)("N").IVoNa = False
        End If
        If MenueParam.Misch.Transp And _
         (chkunt(1).Checked Or (Not IsNothing(chkNAS) AndAlso chkNAS.Checked)) Then
          If GrpRwerte(1)("N").ID >= 0 Then
            GrpRwerte(1)("N").IVoNa = True
            GrpRwerte(1)("N").Iplott = True
          Else
            MsgBox(Texxt(2963))
            IVorNa = False
          End If
        Else
          GrpRwerte(1)("N").IVoNa = False
          GrpRwerte(1)("N").Iplott = False
        End If
        '
        '

      Case "Z"
        '
        '
        'Aktivieren R-Werte Untergrund
        '
        '
        '
        If GrpRwerte(0).RefUnt.ID >= 0 Then
          GrpRwerte(0).RefUnt.IVoNa = True
          GrpRwerte(0).RefUnt.Iplott = False
        Else
          GrpRwerte(0).RefUnt.IVoNa = False
        End If
        If MenueParam.Misch.Transp Then
          If GrpRwerte(0).RefUnt.ID >= 0 Then
            GrpRwerte(0).RefUnt.IVoNa = True
            GrpRwerte(0).RefUnt.Iplott = True
          Else
            MsgBox(Texxt(2964))
            IVorNa = False
          End If
        End If
        If GrpRwerte(1).RefUnt.ID >= 0 AndAlso chkunt.Count > 0 AndAlso (chkKDE.Checked Or chkunt(1).Checked) Then
          GrpRwerte(1).RefUnt.IVoNa = True
          GrpRwerte(1).RefUnt.Iplott = False
        Else
          GrpRwerte(1).RefUnt.IVoNa = False
        End If
        If MenueParam.Misch.Transp And _
        ((chkunt.Count > 0 AndAlso Not IsNothing(chkunt(1)) AndAlso chkunt(1).Checked) Or (Not IsNothing(chkKDE) AndAlso chkKDE.Checked) Or (Not IsNothing(chkNAS) AndAlso chkNAS.Checked)) Then
          If GrpRwerte(1).RefUnt.ID >= 0 Then
            GrpRwerte(1).RefUnt.IVoNa = True
            GrpRwerte(1).RefUnt.Iplott = True
          Else
            MsgBox(Texxt(2965))
            IVorNa = False
          End If
        Else
          GrpRwerte(1).RefUnt.IVoNa = False
        End If
      Case Else

        'Aktivieren R-Werte Nachstellung
        '
        '
        If GrpRwerte(0)(CRef).ID >= 0 Then
          GrpRwerte(0)(CRef).IVoNa = True
          GrpRwerte(0)(CRef).Iplott = False
        Else
          GrpRwerte(0)(CRef).IVoNa = False
        End If
        If chkunt(0).Checked Then
          If GrpRwerte(0)(CRef).ID >= 0 Then
            GrpRwerte(0)(CRef).IVoNa = True
            GrpRwerte(0)(CRef).Iplott = True
          Else
            MsgBox(Texxt(2962))
            IVorNa = False
          End If
        End If
        If GrpRwerte(1)(CRef).ID >= 0 Then
          GrpRwerte(1)(CRef).IVoNa = True
          GrpRwerte(1)(CRef).Iplott = False
        Else
          GrpRwerte(1)(CRef).IVoNa = False
        End If
        If MenueParam.Misch.Transp And _
         (chkunt(1).Checked Or (Not IsNothing(chkNAS) AndAlso chkNAS.Checked) Or (Not IsNothing(chkKDE) AndAlso chkKDE.Checked)) Then
          If GrpRwerte(1)(CRef).ID >= 0 Then
            GrpRwerte(1)(CRef).IVoNa = True
            GrpRwerte(1)(CRef).Iplott = True
          Else
            MsgBox(Texxt(2963))
            IVorNa = False
          End If
        Else
          GrpRwerte(1)(CRef).IVoNa = False
          GrpRwerte(1)(CRef).Iplott = False
        End If
        '
        '
    End Select
  End Function
 

  Property cboGRP() As ComboBox
    Get
      cboGRP = Mncbogrp
    End Get
    Set(ByVal value As ComboBox)
      Mncbogrp = value
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
  Public Function MeldSpeiAllRzp(Del As Boolean) As Boolean
    Dim Meld As Integer
    MeldSpeiAllRzp = True
    '
    '
    'Prüfen , ob GrpInd = Read-only
    '
    '
    If IsNothing(Mncbogrp.SelectedItem) Then
      MeldSpeiAllRzp = False
      Exit Function
    End If

    If Mncbogrp.SelectedItem("READ_ONLY") Then
      MeldSpeiAllRzp = False
      MsgBox(Texxt(3014))
      Exit Function
    End If
    '

    If Mncbogrp.SelectedValue <> 0 Then Exit Function
    Meld = 3009
    If Del Then
      Meld = 3033
    End If
    If MessageBox.Show(RepTexxt(Texxt(Meld), Mncbogrp.Text), Texxt(2000), MessageBoxButtons.YesNo) = DialogResult.No Then
      MeldSpeiAllRzp = False
    End If
  End Function
  Public Sub GroupList()
    Dim UserRzpGID As Integer
    '
    '
    '
    '
    'Aufbau der Gruppen für Rezepte
    '
    '
    '
    UserRzpGID = MenueParam.Misch.UserRzpGID
    '
    '
    '
    Mncbogrp.Enabled = False

    '
    '
    '
    '

    '
    '
    '
    ViewRezepte.RowFilter = "USER_ID=" & MenueParam.UserID & " AND MISCH_ID=" & MenueParam.MischID & " AND (DONT_SHOW=FALSE OR GROUP_ID=" & MenueParam.Misch.UserRzpGID & ")"
    If ViewRezepte.Count = 0 Then
      MsgBox(Texxt(3674))
      ViewRezepte.RowFilter = ""
      Exit Sub
    End If
    Mncbogrp.DataSource = ViewRezepte
    Mncbogrp.DisplayMember = "GROUP_KBEZ"
    Mncbogrp.ValueMember = "GROUP_ID"
    Mncbogrp.Enabled = True
    Mncbogrp.SelectedValue = UserRzpGID
    '
    'Gruppe Enabled / Visible
    '
    '
    If BitWrt(24, MenueParam.User.Enabl) Then
      If Not IsNothing(Mncbogrp) Then
        Mncbogrp.Enabled = True
      End If
      If Not IsNothing(Mnlblgrp) Then
        Mnlblgrp.Enabled = True
      End If
    Else
      If Not IsNothing(Mncbogrp) Then
        Mncbogrp.Enabled = False
      End If
      If Not IsNothing(Mnlblgrp) Then
        Mnlblgrp.Enabled = False
      End If
    End If
    If BitWrt(24, MenueParam.User.Visbl) Then
      If Not IsNothing(Mncbogrp) Then
        Mncbogrp.Visible = True
      End If
      If Not IsNothing(Mnlblgrp) Then
        Mnlblgrp.Visible = True
      End If
    Else
      If Not IsNothing(Mncbogrp) Then
        Mncbogrp.Visible = False
      End If
      If Not IsNothing(Mnlblgrp) Then
        Mnlblgrp.Visible = False
      End If
    End If
    ' '
  End Sub
  Sub MncboGRP_SelectedvalueChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles Mncbogrp.SelectedValueChanged
    If Not Mncbogrp.Enabled Then Exit Sub
    If Not IsNumeric(sender.selectedvalue) OrElse sender.selectedindex = -1 Then Exit Sub
    '

    If Not IsDBNull(Mncbogrp.SelectedItem("MESSG_ID")) AndAlso Mncbogrp.SelectedItem("MESSG_ID") <> -1 Then
      MenueParam.Messg.RwrtGID = Mncbogrp.SelectedItem("MESSG_GID")
    End If
    MenueParam.Misch.UserRzpGID = sender.selectedvalue
  End Sub
  Sub SorSpei(ByRef Datachanged As Boolean, ByRef lstsor As ListBox, ByRef RzNr As String, ByRef RezSozpt As RecipesGrp, _
            ByRef OleAdSorti As OleDbDataAdapter, ByRef Tblsorti As DataTable, ByRef RwWrSortim As ReadWriteSortiment, ByRef UntID() As Integer, ByRef TypID() As Integer, ByRef ier As Integer)
    Dim SoHlf As String
    Dim SorName As String
    Dim diares As DialogResult
    Static ID As Integer
Newrange:
    ier = 0
    If RzNr <> "SOR" Then Exit Sub
    If RezSozpt.Rezepte(RzNr).ID > -1 AndAlso ID = RezSozpt.Rezepte(RzNr).ID Then
      GoTo sortend
    End If
    If Not BitWrt(5, MenueParam.User.Writ) Or Not Datachanged Then Exit Sub
    SorName = RezSozpt.Rezepte(RzNr).Name
    If RezSozpt.Rezepte(RzNr).ID>=0 then
      ID = RezSozpt.Rezepte(RzNr).ID
    End If
    If SorName <> "" Then
      SoHlf = InputBox(Texxt(2101) & Chr(10) & Texxt(2102), Texxt(2000), SorName)
      SoHlf = Trim(SoHlf)
      If SoHlf <> "" Then
        Call RwWrSortim.ReadSortiName(SoHlf, ID, ier)

        If ID <> -1 And lstsor.SelectedValue > -1 And Not IsNothing(lstsor.SelectedValue) Then
          If Not BitWrt(20, MenueParam.User.Sonst) Then
            diares = MessageBox.Show(Texxt(2956), Texxt(2000), MessageBoxButtons.OK)
            GoTo sortend
          Else
            diares = MessageBox.Show(Texxt(2956) & vbCrLf & Texxt(3034), Texxt(2000), MessageBoxButtons.OKCancel)
          End If
          If diares = DialogResult.Cancel Then
            GoTo sortend
          Else
            RezSozpt.Rezepte(RzNr).Name = SoHlf
            If Tblsorti.Rows(lstsor.SelectedIndex)("USER_ID") = MenueParam.UserID Or Not BitWrt(11, MenueParam.User.Writ) Then
              '
              '
              RwWrSortim.UpdSortim(RzNr, ID, RezSozpt, UntID, TypID, ier)
              '
              '
            Else
              diares = MessageBox.Show(Texxt(3607), Texxt(2000), MessageBoxButtons.OKCancel)
              If diares = DialogResult.Cancel Then
                GoTo sortend
              Else
                GoTo Newrange
              End If

            End If
          End If
        Else
          RezSozpt.Rezepte(RzNr).Name = SoHlf
          '
          RwWrSortim.AddSortim(RzNr, ID, RezSozpt, UntID, TypID, ier)
          '
          If ier = 0 Then
            Datachanged = False
            lstsor.SelectedItem = -1
            If Not FillDatset(OleAdSorti, Tblsorti) Then
              GoTo sortend
            End If
            If ID >= 0 Then
              lstsor.SelectedValue = ID
            End If
          End If
        End If
      Else
        GoTo sortend
      End If
    Else
      SorName = ""
      SoHlf = InputBox(Texxt(2101) & Chr(10) & Texxt(2102), Texxt(2000), SorName)
      SoHlf = Trim(SoHlf)
      If SoHlf = "" Then
        GoTo sortend
      End If
      RezSozpt.Rezepte(RzNr).Name = SoHlf
      '
      '
      '
      RwWrSortim.AddSortim(RzNr, ID, RezSozpt, UntID, TypID, ier)
      '
      '
      '
      Datachanged = False
      If Not FillDatset(OleAdSorti, Tblsorti) Then
        GoTo sortend
      End If
      lstsor.SelectedValue = Tblsorti.Rows(Tblsorti.Rows.Count - 1)("SORTI_ID")
    End If
SortEnd:
    Datachanged = False
    ID = -1
    ''
  End Sub
  '
  Sub KorSpei(ByRef Keymenge As String, ByRef ID As Integer, ByRef OldUserID As Integer, ByRef Iarch As Integer, ByRef GID As Integer, ByRef RwWrRezept As ReadWriteRezept, ByRef RezGraphics As HandleRezGrafik, ByRef RezSozpt As RecipesGrp, ByRef UntID() As Integer, ByRef TypID() As Integer, ByRef SmpID() As Integer, ByRef ier As Integer)
    Dim KoHlf As String
    Dim KorName As String
    Dim DiaRes As DialogResult


NewRezept:
    ier = 0
    If Keymenge <> "NEU" And Keymenge <> "MEN" And Keymenge <> "KOR" And Keymenge <> "RZP" And Keymenge <> "KOA" Then Exit Sub
    If Not BitWrt(7, MenueParam.User.Writ) Or Not RezGraphics.DataChanged Then Exit Sub
    KorName = RezSozpt.Rezepte(Keymenge).Name
    If KorName <> "" Then
      If Not MeldSpeiAllRzp(False) Then Exit Sub
      KoHlf = InputBox(Texxt(2103) & Chr(10) & Texxt(2104), Texxt(2000), Trim(KorName))
      KoHlf = Trim(KoHlf)
      If KoHlf <> "" Then
        'If Trim(KorName) = KoHlf Then
        Call RwWrRezept.ReadRezeptName(KoHlf, ID, ier)
        If ID <> -1 Then
          '
          'Rezept bereits vorhanden
          '
          '
          If Not BitWrt(21, MenueParam.User.Sonst) Then
            DiaRes = MessageBox.Show(Texxt(2955), Texxt(2000), MessageBoxButtons.OK)
            RezGraphics.DataChanged = False
            Exit Sub
          End If
          '
          'Userabhängig
          '
          '

          If BitWrt(9, MenueParam.User.Writ) Then
            If MenueParam.UserID <> OldUserID Then
              MessageBox.Show(Texxt(3103), Texxt(2000), MessageBoxButtons.OK)
              RezGraphics.DataChanged = False
              Exit Sub
            End If
          End If

          DiaRes = MessageBox.Show(Texxt(2955) & vbCrLf & Texxt(3034), Texxt(2000), MessageBoxButtons.YesNoCancel)
          If DiaRes = DialogResult.Cancel Then
            RezGraphics.DataChanged = False
            Exit Sub
          ElseIf DiaRes = DialogResult.Yes Then
            '              Update nur falls nicht archiviert
            If RezSozpt.Rezepte(Keymenge).Iarch = 1 Then
              MsgBox(Texxt(3557))
            Else
              RezSozpt.Rezepte(Keymenge).Name = KoHlf
              RezSozpt.Rezepte(Keymenge).Iarch = Iarch
              RezSozpt.Rezepte(Keymenge).Gid = GID
              RwWrRezept.UpdateRezept(Keymenge, ID, RezSozpt, UntID, TypID, SmpID, ier)
            End If
          ElseIf DiaRes = DialogResult.No Then
            GoTo RezADD
          End If

        Else
RezADD:
          RezSozpt.Rezepte(Keymenge).Name = KoHlf
          RezSozpt.Rezepte(Keymenge).Iarch = Iarch
          RezSozpt.Rezepte(Keymenge).Gid = GID

          RwWrRezept.AddRezept(Keymenge, ID, RezSozpt, UntID, TypID, SmpID, ier)
          If ier = 0 Then
            RezGraphics.DataChanged = False
          End If
        End If
      Else
        RezGraphics.DataChanged = False
        Exit Sub
      End If
    Else
      KorName = ""
      KoHlf = InputBox(Texxt(2103) & Chr(10) & Texxt(2104), Texxt(2000), Trim(KorName))
      KoHlf = Trim(KoHlf)
      If KoHlf = "" Then
        RezGraphics.DataChanged = False
        Exit Sub
      End If
      RezSozpt.Rezepte(Keymenge).Name = KoHlf
      RezSozpt.Rezepte(Keymenge).Iarch = Iarch
      RezSozpt.Rezepte(Keymenge).Gid = GID
      RwWrRezept.AddRezept(Keymenge, ID, RezSozpt, UntID, TypID, SmpID, ier)

    End If
    RezGraphics.DataChanged = False
    '
  End Sub
  Function SqlRezeptColth(IndFarbSelect As Integer, GID As Integer, DatExist As Boolean, DatVon As Date, DatBis As Date, WithVor As Boolean, WithNach As Boolean, SuString As String, ByRef ListID() As Integer, ByRef RezHlfAvailID As List(Of Integer), ByRef TableRez As DataTable) As Boolean
    Dim i As Integer
    Dim j As Integer
    Dim Jzaehl As Integer
    Dim SQLNoGlzGrd As String
    Dim SQLGID As String
    Dim SqlUser As String
    Dim FilterString As String
    Dim TblFarbRezept As New DataTable
    Dim AdaptFarbrezept As New OleDbDataAdapter
    Dim CmdFarbRezept As New OleDbCommand("", Cndat)
    Dim ViewFarbRezept As New DataView(TblFarbRezept)
    Dim StrRezNoGLZGRD As String
    Dim Tabrez As DataTable
    Dim AdaptRezept As New OleDbDataAdapter
    Dim CmdRezept As New OleDbCommand("", Cndat)
    Dim StrRezepte As String
    AdaptRezept.SelectCommand = CmdRezept
    AdaptFarbrezept.SelectCommand = CmdFarbRezept
    '
    '
    'Rezepte für FARBM_ID <> GLZGRD_ID
    '
    Tabrez = New DataTable
    If Not SqlRezeptNoGlzGrd(GID, DatExist, DatVon, DatBis, SuString, Tabrez) Then
      Exit Function
    End If
    '
    '
    StrRezNoGLZGRD = StrLin(Tabrez, "REZEPT_ID")
    '
    '
    TableRez.Rows.Clear()
    If Not IsNothing(RezHlfAvailID) Then
      RezHlfAvailID.Clear()
    End If
    '
    '
    'Rezepte mit Grunddaten

    '
    '
    If GID = 0 Then
      SQLGID = "REZEPT_IARCH<2 AND "
    Else
      SQLGID = "(REZEPT_IARCH<2 AND REZEPT_GID = " & GID & ") AND "
    End If
    SqlUser = ""
    If BitWrt(8, MenueParam.User.Writ) Then
      SqlUser = "USER_ID=" & MenueParam.UserID & " AND "
    End If
    SQLNoGlzGrd = "TBL_REZEPT.REZEPT_ID NOT IN " & StrRezNoGLZGRD & " AND "

    If WithNach And WithVor Then
      '
      'Mit Nachstellungen und mit Vorlagen
      '
      CmdRezept.CommandText = "SELECT TBL_REZEPT.REZEPT_ID AS REZEPT_ID,REZEPT_DATTIM,REZEPT_NAME,USER_ID FROM TBL_REZEPT " _
      & " INNER JOIN TBL_REZEPT_RWERT ON (TBL_REZEPT.REZEPT_ID=TBL_REZEPT_RWERT.REZEPT_ID AND TBL_REZEPT.MISCH_ID=TBL_REZEPT_RWERT.MISCH_ID)" _
      & " WHERE " & SQLGID & SqlUser & SQLNoGlzGrd & " TBL_REZEPT.MISCH_ID=" & MenueParam.MischID & " AND MESSGRW_ID=" & MenueParam.Messg.MessgRwID _
      & " AND RWERT_KWB=1 AND TBL_REZEPT.REZEPT_ID NOT IN " & StrRezNoGLZGRD & " AND TBL_REZEPT_RWERT.REZEPT_ID IN" _
      & " (SELECT REZEPT_ID FROM TBL_REZEPT_RWERT WHERE MISCH_ID=" & MenueParam.MischID & " AND MESSGRW_ID=" & MenueParam.Messg.MessgRwID & " AND RWERT_KWB=11)"
    ElseIf WithVor Then
      '
      'Mit Vorlagen
      '
      CmdRezept.CommandText = "SELECT TBL_REZEPT.REZEPT_ID AS REZEPT_ID,REZEPT_DATTIM,REZEPT_NAME,USER_ID FROM TBL_REZEPT " _
      & " INNER JOIN TBL_REZEPT_RWERT ON (TBL_REZEPT.REZEPT_ID=TBL_REZEPT_RWERT.REZEPT_ID AND TBL_REZEPT.MISCH_ID=TBL_REZEPT_RWERT.MISCH_ID)" _
      & " WHERE " & SQLGID & SqlUser & SQLNoGlzGrd & " TBL_REZEPT.MISCH_ID=" & MenueParam.MischID & " AND MESSGRW_ID=" & MenueParam.Messg.MessgRwID & " AND RWERT_KWB=11"
    ElseIf WithNach Then
      '
      'Mit Nachstellungen
      '
      CmdRezept.CommandText = "SELECT TBL_REZEPT.REZEPT_ID AS REZEPT_ID,REZEPT_DATTIM,REZEPT_NAME,USER_ID FROM TBL_REZEPT " _
      & " INNER JOIN TBL_REZEPT_RWERT ON (TBL_REZEPT.REZEPT_ID=TBL_REZEPT_RWERT.REZEPT_ID AND TBL_REZEPT.MISCH_ID=TBL_REZEPT_RWERT.MISCH_ID)" _
      & " WHERE " & SQLGID & SqlUser & SQLNoGlzGrd & " TBL_REZEPT.MISCH_ID=" & MenueParam.MischID & " AND MESSGRW_ID=" & MenueParam.Messg.MessgRwID & " AND RWERT_KWB=1"

    Else
      CmdRezept.CommandText = "SELECT REZEPT_ID,REZEPT_DATTIM,REZEPT_NAME,USER_ID FROM TBL_REZEPT WHERE " & SQLGID & SqlUser & SQLNoGlzGrd & " MISCH_ID=" & MenueParam.MischID
    End If
      If DatExist Then
        CmdRezept.CommandText = CmdRezept.CommandText & " AND REZEPT_DATTIM BETWEEN ? AND ? "
      End If
      If Trim(SuString) <> "" Then
        CmdRezept.CommandText = CmdRezept.CommandText & " AND " & StrSelct("REZEPT_NAME", AddHkomE(SuString))
      End If
      CmdRezept.CommandText = CmdRezept.CommandText & " ORDER BY REZEPT_DATTIM DESC"
      CmdRezept.Parameters.Clear()
      CmdRezept.Parameters.Add(New OleDbParameter("DATVON", OleDbType.Date))
      CmdRezept.Parameters.Add(New OleDbParameter("DATBIS", OleDbType.Date))
      CmdRezept.Parameters("DATVON").Value = DatVon
      CmdRezept.Parameters("DATBIS").Value = DatBis
      AdaptRezept.SelectCommand = CmdRezept
    If Not FillDatset(AdaptRezept, TableRez) Then
      Exit Function
    End If

      StrRezepte = StrLin(TableRez, "REZEPT_ID")
      '
      '
      'Zugehörige Farbmittel
      '
      '
    TblFarbRezept.Rows.Clear()
    CmdFarbRezept.CommandText = "SELECT REZEPT_ID,FARBM_ID FROM TBL_REZEPT_FARBM WHERE MISCH_ID=" & MenueParam.MischID & " AND REZEPT_ID IN " & StrRezepte
    If Not FillDatset(AdaptFarbrezept, TblFarbRezept) Then
      Exit Function
    End If









      If ListID.Count <> 0 Then
        Select Case IndFarbSelect
          '
          '
          Case 0
            '
            '
            'Rezepte, die mindestens alle ausgewähltes Farbmittel aus FarbID enthalten
            '
            '
            FilterString = "REZEPT_ID="
            For i = 0 To TableRez.Rows.Count - 1
              Jzaehl = 0
              For j = 0 To ListID.Count - 1
                ViewFarbRezept.RowFilter = FilterString & TableRez.Rows(i)("REZEPT_ID") & " AND FARBM_ID=" & ListID(j)
                If ViewFarbRezept.Count = 1 Then
                  Jzaehl = Jzaehl + 1
                End If
              Next j
              ViewFarbRezept.RowFilter = FilterString & TableRez.Rows(i)("REZEPT_ID")
              If Jzaehl < ListID.Count Then
                TableRez.Rows(i).Delete()
              End If
            Next i
            '
            '
          Case 1
            '
            '
            'Rezepte, die genau alle ausgewähltes Farbmittel aus FarbID enthalten
            '
            '
            FilterString = "REZEPT_ID="
            For i = 0 To TableRez.Rows.Count - 1
              ViewFarbRezept.RowFilter = FilterString & TableRez.Rows(i)("REZEPT_ID")
              If ViewFarbRezept.Count = ListID.Count Then
                For j = 0 To ListID.Count - 1
                  ViewFarbRezept.RowFilter = FilterString & TableRez.Rows(i)("REZEPT_ID") & " AND FARBM_ID=" & ListID(j)
                  If ViewFarbRezept.Count = 0 Then
                    Exit For
                  End If
                Next j
                If j = ListID.Count Then
                  Continue For
                End If
              End If
              TableRez.Rows(i).Delete()
            Next i
            '
            '
          Case 2
            '
            '
            'Rezepte, die höchstzens alle ausgewähltes Farbmittel aus FarbID enthalten
            '
            '
            FilterString = "REZEPT_ID="
            For i = 0 To TableRez.Rows.Count - 1
              ViewFarbRezept.RowFilter = FilterString & TableRez.Rows(i)("REZEPT_ID")
              For j = 0 To ViewFarbRezept.Count - 1
                If Not ListID.Contains(ViewFarbRezept(j)("FARBM_ID")) Then
                  TableRez.Rows(i).Delete()
                  Exit For
                End If
              Next j
            Next i
            '
            '
          Case 3
            '
            '
            'Rezepte, die mindestens ein ausgewähltes Farbmittel aus FarbID enthalten
            '
            '
            FilterString = "("
            For j = 0 To ListID.Count - 1
              If j > 0 Then
                FilterString = FilterString & " OR "
              End If
              FilterString = FilterString & "FARBM_ID=" & ListID(j)
            Next j
            FilterString = FilterString & ")"
            For i = 0 To TableRez.Rows.Count - 1
              ViewFarbRezept.RowFilter = FilterString & " AND REZEPT_ID=" & TableRez.Rows(i)("REZEPT_ID")
              If ViewFarbRezept.Count = 0 Then
                TableRez.Rows(i).Delete()
              End If
            Next i
            '
            '
            '
            '
          Case 4
            '
            '
            'Rezepte, die genau ein ausgewähltes Farbmittel aus FarbID enthalten
            '
            '

            FilterString = "("
            For j = 0 To ListID.Count - 1
              If j > 0 Then
                FilterString = FilterString & " OR "
              End If
              FilterString = FilterString & "FARBM_ID=" & ListID(j)
            Next j
            FilterString = FilterString & ")"
            For i = 0 To TableRez.Rows.Count - 1
              ViewFarbRezept.RowFilter = FilterString & " AND REZEPT_ID=" & TableRez.Rows(i)("REZEPT_ID")
              If ViewFarbRezept.Count <> 1 Then
                TableRez.Rows(i).Delete()
              End If
            Next i
            '
            '
            '
          Case 5
            '
            '
            '
            'Rezepte, die höchstens ein ausgewähltes Farbmittel aus FarbID enthalten
            '
            '

            FilterString = "REZEPT_ID="
            For i = 0 To TableRez.Rows.Count - 1
              Jzaehl = 0
              ViewFarbRezept.RowFilter = FilterString & TableRez.Rows(i)("REZEPT_ID")
              For j = 0 To ViewFarbRezept.Count - 1
                If ListID.Contains(ViewFarbRezept(j)("FARBM_ID")) Then
                  Jzaehl = Jzaehl + 1
                End If
              Next j
              If Jzaehl > 1 Then
                TableRez.Rows(i).Delete()
              End If
            Next i
            '
            '
            '
          Case 6
            '
            '
            '
            'Rezepte, die kein ausgewähltes Farbmittel aus FarbID enthalten
            '
            '

            FilterString = "("
            For j = 0 To ListID.Count - 1
              If j > 0 Then
                FilterString = FilterString & " OR "
              End If
              FilterString = FilterString & "FARBM_ID=" & ListID(j)
            Next j
            FilterString = FilterString & ")"
            For i = 0 To TableRez.Rows.Count - 1
              ViewFarbRezept.RowFilter = FilterString & " AND REZEPT_ID=" & TableRez.Rows(i)("REZEPT_ID")
              If ViewFarbRezept.Count > 0 Then
                TableRez.Rows(i).Delete()
              End If
            Next i
        End Select
      End If

      TableRez.AcceptChanges()
      '
      '

      SqlRezeptColth = True
      TblFarbRezept.Dispose()
      AdaptFarbrezept.Dispose()
      CmdFarbRezept.Dispose()
      ViewFarbRezept.Dispose()
    CmdRezept.Dispose()
      AdaptRezept.Dispose()
    Tabrez.Dispose()
  End Function

  Function SqlRezeptKorr(ByRef GID As Integer, DatExist As Boolean, VorlExist As Boolean, DatVon As Date, DatBis As Date, SuString As String, ByRef RezHlfAvailID As List(Of Integer), ByRef TableRez As DataTable) As Boolean
    Dim i As Integer
    Dim StrRezNoGrund As String
    Dim StrRezNoGLZGRD As String
    Dim SqlUser As String
    Dim SQLDat As String
    Dim SQLGid As String
    Dim SQLAdd As String
    Dim SqlWith As String

    Dim Tabrez As DataTable
    Dim AdaptRezept As New OleDbDataAdapter
    Dim CmdRezept As New OleDbCommand("", Cndat)
    AdaptRezept.SelectCommand = CmdRezept

    SqlRezeptKorr = False
    '
    '
    TableRez.Rows.Clear()
    '
    Tabrez = New DataTable
    Tabrez.Clear()
    '
    '
    'Rezepte ohne Grunddaten
    '
    '
    If Not SqlRezeptNoGrund(GID, DatExist, DatVon, DatBis, SuString, Tabrez) Then
      Exit Function
    End If

    StrRezNoGrund = StrLin(Tabrez, "REZEPT_ID")
    '
    '
    'Rezepte für FARBM_ID <> GLZGRD_ID
    '
    Tabrez.Clear()
    If Not SqlRezeptNoGlzGrd(GID, DatExist, DatVon, DatBis, SuString, Tabrez) Then
      Exit Function
    End If
    '
    '
    StrRezNoGLZGRD = StrLin(Tabrez, "REZEPT_ID")
    '
    '
    SQLDat = ""

    If DatExist Then
      '
      '
      SQLDat = " AND (REZEPT_DATTIM BETWEEN " & Sqldati(DatVon) & " AND " & Sqldati(DatBis) & ")"

    End If
    '
    '
    '
    '
    If GID = 0 Then
      SQLGid = "REZEPT_IARCH<2 AND "
    Else
      SQLGid = "(REZEPT_IARCH<2 AND REZEPT_GID = " & GID & ") AND "
    End If
    '
    SqlUser = ""
    If BitWrt(8, MenueParam.User.Writ) Then
      SqlUser = "USER_ID=" & MenueParam.UserID & " AND "
    End If
    '
    SQLAdd = ""
    If Trim(SuString) <> "" Then
      SQLAdd = " AND " & StrSelct("REZEPT_NAME", AddHkomE(SuString))
    End If
    SQLAdd = SQLAdd & " ORDER BY REZEPT_DATTIM DESC"
    '
    SqlWith = ""
    '
    '
    '
    If Not IsNothing(RezHlfAvailID) Then

      '
      '
      'Rezepte mit Nachstellungen über weißem Untergrund
      '
      '
     
      '
      CmdRezept.CommandText = "SELECT TOP " & MenueParam.Misch.Top & " TBL_REZEPT.REZEPT_ID AS REZEPT_ID,REZEPT_DATTIM,REZEPT_NAME,REZEPT_BEM,RWERT_ID FROM TBL_REZEPT " _
     & " INNER JOIN TBL_REZEPT_RWERT ON (TBL_REZEPT.REZEPT_ID=TBL_REZEPT_RWERT.REZEPT_ID AND TBL_REZEPT.MISCH_ID=TBL_REZEPT_RWERT.MISCH_ID)" _
     & " WHERE " & SQLGid & SqlUser & " TBL_REZEPT.MISCH_ID=" & MenueParam.MischID & " AND MESSGRW_ID=" & MenueParam.Messg.MessgRwID _
     & " AND RWERT_KWB =1" & SQLDat & " AND TBL_REZEPT.REZEPT_ID NOT IN " & StrRezNoGLZGRD
      '
      '

      '
      CmdRezept.CommandText = CmdRezept.CommandText & SQLAdd
      '
      '
      AdaptRezept.SelectCommand = CmdRezept
      Tabrez.Clear()
      If Not FillDatset(AdaptRezept, Tabrez) Then
        Exit Function
      End If
      Tabrez.AcceptChanges()
      RezHlfAvailID.Clear()
      For i = 0 To Tabrez.Rows.Count - 1
        RezHlfAvailID.Add(Tabrez.Rows(i)("REZEPT_ID"))
      Next
    End If
    '
    'Mit vorlagen über weißem Untergrund
    '
    SqlWith = ""
    If VorlExist Then
      SqlWith = " AND MESSGRW_ID=" & MenueParam.Messg.MessgRwID & " AND RWERT_KWB =11"
    End If





    'Suche alle Rezepte für MISCHID, bei denen für alle Farb-/Bindemittel Grunddaten bereitstehen
    '1. Schritt: Liste aller Farb-/Bindemittel für Grunddaten
    '2. Schritt: Alle Rezepte suchen, die Farb-/Bindemittel aus Liste (1.Schritt) enthalten
    '3. Schritt: Alle Rezepte, die nicht in Liste (2. Schritt) enthalten sind und R-Werte(Vorlage kwb=11) enthalten usw. 
    '
    'Rezepte mit Vorlagen
    '
    '
    '
    If SqlWith = "" Then
      CmdRezept.CommandText = "SELECT DISTINCTROW TOP " & MenueParam.Misch.Top & " TBL_REZEPT.REZEPT_ID AS REZEPT_ID,REZEPT_DATTIM,REZEPT_NAME,REZEPT_BEM,USER_ID FROM TBL_REZEPT " _
      & " LEFT JOIN TBL_REZEPT_RWERT ON (TBL_REZEPT.REZEPT_ID=TBL_REZEPT_RWERT.REZEPT_ID AND TBL_REZEPT.MISCH_ID=TBL_REZEPT_RWERT.MISCH_ID)" _
      & " WHERE " & SQLGid & SqlUser & " TBL_REZEPT.MISCH_ID=" & MenueParam.MischID _
      & SqlWith & SQLDat & " AND TBL_REZEPT.REZEPT_ID NOT IN " & StrRezNoGrund & " AND TBL_REZEPT.REZEPT_ID NOT IN " & StrRezNoGLZGRD
    Else
      CmdRezept.CommandText = "SELECT DISTINCTROW TOP " & MenueParam.Misch.Top & " TBL_REZEPT.REZEPT_ID AS REZEPT_ID,REZEPT_DATTIM,REZEPT_NAME,REZEPT_BEM,USER_ID,RWERT_ID FROM TBL_REZEPT " _
      & " INNER JOIN TBL_REZEPT_RWERT ON (TBL_REZEPT.REZEPT_ID=TBL_REZEPT_RWERT.REZEPT_ID AND TBL_REZEPT.MISCH_ID=TBL_REZEPT_RWERT.MISCH_ID)" _
      & " WHERE " & SQLGid & SqlUser & " TBL_REZEPT.MISCH_ID=" & MenueParam.MischID _
      & SqlWith & SQLDat & " AND TBL_REZEPT.REZEPT_ID NOT IN " & StrRezNoGrund & " AND TBL_REZEPT.REZEPT_ID NOT IN " & StrRezNoGLZGRD
    End If

    '
    CmdRezept.CommandText = CmdRezept.CommandText & SQLAdd
    '
    '
    AdaptRezept.SelectCommand = CmdRezept

    Application.DoEvents()
    If Not FillDatset(AdaptRezept, TableRez) Then
      Exit Function
    End If
    TableRez.AcceptChanges()

    SqlRezeptKorr = True
    Tabrez.Dispose()
    AdaptRezept.Dispose()
    CmdRezept.Dispose()

  End Function
  Function SqlRezeptNoGlzGrd(GID As Integer, DatExist As Boolean, DatVon As Date, DatBis As Date, SuString As String, ByRef TableRez As DataTable) As Boolean
    Dim SqlGID As String
    Dim SqlDat As String
    Dim SqlUser As String
    Dim Withmessg As String
    Dim AdaptRezept As New OleDbDataAdapter
    Dim CmdRezept As New OleDbCommand("", Cndat)
    AdaptRezept.SelectCommand = CmdRezept
    SqlRezeptNoGlzGrd = False
    '
    '
    TableRez.Rows.Clear()
    '
    '
    'Rezepte, deren GLZGRD_ID nicht mit FARBM_ID übereinstimmt
    '
    '
    SqlDat = ""

    If DatExist Then
      '
      '
      SqlDat = " AND (REZEPT_DATTIM BETWEEN " & Sqldati(DatVon) & " AND " & Sqldati(DatBis) & ")"

    End If
    '
    '
    '
    '
    If GID = 0 Then
      SqlGID = "REZEPT_IARCH<2 AND "
    Else
      SqlGID = "(REZEPT_IARCH<2 AND REZEPT_GID = " & GID & ") AND "
    End If
    SqlUser = ""
    If BitWrt(8, MenueParam.User.Writ) Then
      SqlUser = "USER_ID=" & MenueParam.UserID & " AND "
    End If
    '

    '
    ' If BitWrt(27, MenueParam.User.Writ) Then
    Withmessg = "TBL_GRUND_FARBM.MESSGRW_ID=" & MenueParam.Messg.MessgRwID
    'Else
    'Withmessg = "TBL_GRUND_FARBM.MESSG_ID=" & MenueParam.Messg.MessgID
    'End If
    '
    '
    CmdRezept.CommandText = "SELECT DISTINCT TBL_REZEPT_FARBM.REZEPT_ID AS REZEPT_ID FROM TBL_REZEPT_FARBM " _
   & " INNER JOIN TBL_REZEPT ON TBL_REZEPT_FARBM.REZEPT_ID=TBL_REZEPT.REZEPT_ID" _
   & " WHERE " & SqlGID & SqlUser & " TBL_REZEPT_FARBM.MISCH_ID=" & MenueParam.MischID & SqlDat & " AND FARBM_ID NOT IN " _
   & " (SELECT DISTINCT FARBM_ID FROM TBL_FARBM WHERE MISCH_ID=" & MenueParam.MischID _
   & " AND FARBM_ID=GLZGRD_ID)"
    '
    If Trim(SuString) <> "" Then
      CmdRezept.CommandText = CmdRezept.CommandText & " AND " & StrSelct("REZEPT_NAME", AddHkomE(SuString))
    End If '


    '
    '
    '
    AdaptRezept.SelectCommand = CmdRezept

    If Not FillDatset(AdaptRezept, TableRez) Then
      Exit Function
    End If
    TableRez.AcceptChanges()



    SqlRezeptNoGlzGrd = True
    CmdRezept.Dispose()
    AdaptRezept.Dispose()

  End Function


  Function SqlRezeptNoGrund(GID As Integer, DatExist As Boolean, DatVon As Date, DatBis As Date, SuString As String, ByRef TableRez As DataTable) As Boolean
    Dim SqlGID As String
    Dim SqlDat As String
    Dim SqlUser As String
    Dim Withmessg As String
    Dim AdaptRezept As New OleDbDataAdapter
    Dim CmdRezept As New OleDbCommand("", Cndat)
    AdaptRezept.SelectCommand = CmdRezept
    SqlRezeptNoGrund = False
    '
    '
    TableRez.Rows.Clear()
    '
    '
    'Rezepte, die keine Grunddaten enthalten
    '
    '
    SqlDat = ""

    If DatExist Then
      '
      '
      SqlDat = " AND (REZEPT_DATTIM BETWEEN " & Sqldati(DatVon) & " AND " & Sqldati(DatBis) & ")"

    End If
    '
    '
    '
    '
    If GID = 0 Then
      SqlGID = "REZEPT_IARCH<2 AND "
    Else
      SqlGID = "(REZEPT_IARCH<2 AND REZEPT_GID = " & GID & ") AND "
    End If
    SqlUser = ""
    If BitWrt(8, MenueParam.User.Writ) Then
      SqlUser = "USER_ID=" & MenueParam.UserID & " AND "
    End If
    '
    
    '
    ' If BitWrt(27, MenueParam.User.Writ) Then
    Withmessg = "TBL_GRUND_FARBM.MESSGRW_ID=" & MenueParam.Messg.MessgRwID
    'Else
    'Withmessg = "TBL_GRUND_FARBM.MESSG_ID=" & MenueParam.Messg.MessgID
    'End If
    '
    '
    CmdRezept.CommandText = "SELECT DISTINCT TBL_REZEPT_FARBM.REZEPT_ID AS REZEPT_ID FROM TBL_REZEPT_FARBM " _
   & " INNER JOIN TBL_REZEPT ON TBL_REZEPT_FARBM.REZEPT_ID=TBL_REZEPT.REZEPT_ID" _
   & " WHERE " & SqlGID & SqlUser & " TBL_REZEPT_FARBM.MISCH_ID=" & MenueParam.MischID & SqlDat & " AND FARBM_ID NOT IN " _
   & " (SELECT DISTINCT FARBM_ID FROM TBL_GRUND_FARBM WHERE MISCH_ID=" & MenueParam.MischID _
   & " AND TBL_GRUND_FARBM.GKWRT_ID=" & MenueParam.Misch.GKwrtID & " AND " & Withmessg & ")"
    '
    If Trim(SuString) <> "" Then
      CmdRezept.CommandText = CmdRezept.CommandText & " AND " & StrSelct("REZEPT_NAME", AddHkomE(SuString))
    End If '


    '
    '
    '
    AdaptRezept.SelectCommand = CmdRezept

    If Not FillDatset(AdaptRezept, TableRez) Then
      Exit Function
    End If
    TableRez.AcceptChanges()



    SqlRezeptNoGrund = True
    CmdRezept.Dispose()
    AdaptRezept.Dispose()

  End Function
  Public Sub New()
    HandleRwrt = New HandleRwerte
  End Sub

  Protected Overrides Sub Finalize()
    MyBase.Finalize()
  End Sub
End Class
