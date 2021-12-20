Public Class RezSpezial
  Implements IDisposable

  '
  Dim i As Integer
  Dim k As Integer
  Dim kw As Integer
  Dim j As Integer
  Dim l As Integer
  Dim kmw As Integer
  Dim FaID As Integer
  Dim KeyID As String
  Dim NLQ As Integer
  Dim KFM As Integer
  Dim KfmL As Integer
  Dim IprnL As Integer
  Dim KffiL As Integer
  Dim KfvaL As Integer
  Dim NlqL As Integer
  Dim Nlqd As Integer
  Dim NfL As Integer
  Dim KfL As Integer
  Dim Rzkey As String
  Dim IGX As Integer
  Dim NM As Integer
  Dim NE As Integer
  Dim NEQ As Integer
  Dim NEM As Integer
  Dim NPQ As Integer
  Dim AGR() As Single
  Dim IGR() As Integer
  Dim BL() As Single
  Dim BU() As Single
  Dim IND() As Integer
  Dim R0() As Single

  '
  '
  '
  '
  Dim Iprn As Integer
  Dim NUV As Integer
  Dim NUN As Integer
  Dim NUU As Integer
  Dim NWE As Integer
  Dim KM As Integer
  Dim Retr(1) As Integer
  Dim KWB(1) As Integer
  Dim DICKE(1) As Single
  Dim RUNT(,,) As Single
  Dim RVOR(,,) As Single
  Dim RNAC(,,) As Single
  Dim RBER(,,) As Single
  Dim GRNDALL(,,,) As Single
  Dim RHLFN(,,) As Single
  Dim RHLFV(,,) As Single
  Dim SorKrit(1, 15) As Single

  '
  '
  Dim Ihlf As Integer
  Dim NF As Integer
  Dim LK() As Integer
  Dim KFIT As Integer
  Dim LKid(,) As Integer
  Dim NFR() As Integer
  Dim ID() As Integer
  Dim IDfarb() As Integer
  Dim SozMng() As Single
  Dim BasMng() As Single
  Dim RezMng(,) As Single
  Dim MischMng() As Single
  Dim ZwiMng() As Single
  Dim RezAlt() As Single
  Dim KorMng() As Single
  Dim HlfMng() As Single
  Dim Proz() As Single
  Dim Prob() As Single
  Dim Spz() As Single
  Dim Eff() As Single
  Dim Ichf() As Integer
  Dim NST As Integer
  Dim CSTges(,) As Single
  Dim Gruges(,,,) As Single
  Dim Ivol As Integer
  Dim Inf As Integer
  Dim Sploe As Single
  Dim DosMin As Single

  Sub MehrSchichtCalc(P As Integer, QM As Integer, N As Integer, IFANR(,,) As Integer, Anteil(,,) As Single, NF As Integer, MENG() As Single, A() As Single, S() As Single, ST() As Single, D As Single, KW As Integer, _
                    RKR As Single, RKT As Single, ByRef RG As Single, ByRef TG As Single, NeugeSim As Boolean, ByRef IER As Integer)
    Dim RER As Single
    Dim RET As Single
    Call GetWinkNormGk("MHR", KW, MenueParam.Messg.Winkel, IER)
    RER = TRUKORR(RKR, KW, 0)
    RET = TRUKORR(RKT, KW, 1)

    Call MEHRMHR(P, QM, N, IFANR(0, 0, 0), Anteil(0, 0, 0), NF, MENG(0), A(0), S(0), ST(0), D, KW, RER, RET, RG, TG, NeugeSim, IER)
  End Sub

  Sub GrundDatenMSH(ByRef Iprn As Integer, PS As Integer, QMS As Integer, NS As Integer, ICHX() As Integer, Anteil(,,) As Single, Neuge As Boolean, ByRef Winkel As AngGeos, ByRef Rezsozpt As RecipesGrp, ByRef GrpRwertM As RefValuesGrp, ByRef GrpRwertR As RefValuesGrp, ByVal GesStreuAbs As Colorants, ByRef Ier As Integer)
    Dim i As Integer
    Dim j As Integer
    Dim k As Integer
    Dim kw As Integer
    Dim l As Integer
    Dim NR As Integer
    Dim KF As Integer
    Dim NPS As Integer
    Dim NFG As Integer
    Dim NPX As Integer
    Dim KFI As Integer
    Dim KFU As Integer
    Dim KFO As Integer
    Dim KVA As Integer
    Dim MMX As Integer
    Dim FU(1) As Single
    Dim EXPO As Single
    Dim GewR As Single
    Dim FTO As Single
    Dim PHHI As Single
    Dim JTM As Integer
    Dim Iglae As Integer
    Dim DikAlt() As Single
    Dim DikNeu(,) As Single
    Dim Eff() As Single
    Dim AKOWRT() As Single
    Dim GRSTR(,) As Single
    Dim GRDAE(,) As Single
    Dim GRCCQ(,) As Byte
    Dim KWBUN() As Integer
    Dim RETUN() As Integer
    Dim WS() As Integer
    Dim NrRwrt() As Integer
    Dim KWB() As Integer
    Dim IRT() As Integer
    Dim KeyRwrt As String
    Dim Grund(,,,) As Single
    Dim GruGrz(,,,) As Single
    Dim Cst(,,) As Single
    Dim NST() As Integer
    Dim ICHF() As Integer
    Dim NABSTR As Integer
    Dim JABST As Integer
    Dim GewDik As Single
    Dim Rezcount As Integer
    '
    '

    'IPRN
    '
    '****BIT 0****
    '
    'Fix(Iprn) Mod 2 = 0 für nicht feste Farb-/Bindemittel (KFI+1....NFG) werden Startwerte festgelegt s. GRDGRUN (KFU=KFI+1)
    'Fix(Iprn) Mod 2 = 1 für alle Farb-/Bindemittel werden alte Grunddaten als Startwerte übernommen s.GRDGRUN(KFU=NFG+1)
    '
    '****BIT 1****
    '
    'Iglae=Fix(Iprn / 2) Mod 2 =0
    'Iglae=0 Grunddaten werden für jede Wellenlänge separat berechnet
    'Iglae=1 Grunddaten werden für alle Wellenlängen simultan berechnet (u.U. mit entsprechender Kopplung. s. GRDAE)
    '
    '****BIT 2****
    '
    'Fix(Iprn / 4) Mod 2 = 0 (Grunddaten für Farb-/Bindemittel (KFI+1 .. NFG)  werden neu berechnet (GRDCALC) 
    'Fix(Iprn / 4) Mod 2 = 1 nur Reflexionswerte werden aus den vorgegebenen Grunddaten berechnmet (GRDREFL)
    '
    '****BIT 3****
    '
    'Limitierung = möglichst gute Anpassung an 
    'Fix(Iprn / 8) Mod 2 = 0 möglichst gute Anpassung an alte Grunddaten gewichtet mit Grdae(i,j) (nur falls Fix(Iprn) Mod 2 = 1)
    'Fix(Iprn / 8) Mod 2 = 1 möglichst gute Anpassung an Standardgrenzen (Grstr(i, j)) gewichtet mit Grdae(i,j)
    '
    '
    '
    '
    '
    '
    '
    KM = Winkel.Km
    NWE = Winkel.Wsol.Nwe
    NPX = MenueParam.Misch.Npx
    JABST = MenueParam.Menue.JABST
    NME = 2

    '
    'Anzahl Farb-/Bindemittel
    '
    '
    NFG = Rezsozpt.Farben.FarbCount
    '
    'Anzahl Rezepte (Remissions-(Transmissions-)kurven
    '
    '
    '

    MMX = Rezsozpt.Rezepte.RezCount + GrpRwertM(1).Count + 1
    '
    '
    If MMX = 0 Then
      Ier = 2953
      MsgBox(Texxt(2953))
      Exit Sub
    End If

    '
    ReDim DikAlt(MMX - 1)
    ReDim DikNeu(MMX - 1, KM - 1)
    ReDim RUNT(NME - 1, KM - 1, NWE - 1)
    ReDim RNAC(MMX - 1, KM - 1, NWE - 1)
    ReDim RBER(MMX - 1, KM - 1, NWE - 1)
    ReDim GRNDALL(MMX - 1, NPX - 1, KM - 1, NWE - 1)
    ReDim RezMng(MMX - 1, NFG - 1)
    ReDim ZwiMng(NFG - 1)
    ReDim Eff(NFG - 1)
    ReDim ID(NFG - 1)
    ReDim KWBUN(NME - 1)
    ReDim RETUN(NME - 1)
    ReDim KWB(MMX - 1)
    ReDim IRT(MMX - 1)
    ReDim WS(MMX - 1)
    ReDim NrRwrt(MMX - 1)
    ReDim IDfarb(NFG - 1)
    ReDim ICHF(NFG - 1)
    '
    '
    '
    '
    For N = 0 To GrpRwertM.Count - 1
      KWBUN(N) = N + 1
      RETUN(N) = 0
      If Not IsNothing(GrpRwertM(N).RefUnt) Then
        KWBUN(N) = GrpRwertM(N).RefUnt.kwb + 1
        RETUN(N) = GrpRwertM(N).RefUnt.ReTr
        If GrpRwertM(N).RefUnt.ID >= 0 Then
          Call GetRwerte(NWE, KM, N, RUNT, GrpRwertM(N).RefUnt)
        End If
      End If
    Next N
    '
    'IDFarb so aufbauen, dass feste Farbmittel (KFI) zuerst und dann variable Farbmittel (KVA) stehen
    '
    '
    'feste Farb-/Bindemittel
    '
    KFI = 0
    For i = 0 To Rezsozpt.Farben.FarbCount - 1
      If Rezsozpt.Farben(i).OptData.Fest = 1 Then
        IDfarb(KFI) = Rezsozpt.Farben(i).ID
        KFI = KFI + 1
      End If
    Next
    '
    'variable Farbmittel
    '
    KVA = 0
    For i = 0 To Rezsozpt.Farben.FarbCount - 1
      If Rezsozpt.Farben(i).OptData.Fest <> 1 Then
        IDfarb(KFI + KVA) = Rezsozpt.Farben(i).ID
        KVA = KVA + 1
      End If
    Next

    '
    Rezcount = Rezsozpt.Rezepte.RezCount
    i = 0
    For l = 0 To Rezcount - 1
      KF = Rezsozpt.Rezepte(l).KF
      If IsNumeric(Rezsozpt.Rezepte.RezKey(l)) Then
        Call GetFamng(KeyRe(l), Rezsozpt, Rezsozpt.Rezepte(l).KF, ID, ZwiMng, Ier)

        NR = Rezsozpt.Rezepte(l).Nr
        '
        '
        '
        'Remissionswerte
        '
        '
        '
        For N = 0 To GrpRwertM.Count - 1
          If GrpRwertM(N).ContainsKey(KeyRe(NR)) Then
            KWB(i) = GrpRwertM(N)(KeyRe(NR)).kwb + 1
            If Not GrpRwertM(N)(KeyRe(NR)).IVoNa Then
              KWB(i) = -KWB(i)
            End If
            IRT(i) = GrpRwertM(N)(KeyRe(NR)).ReTr
            WS(i) = N
            NrRwrt(i) = NR
            Call GetRwerte(NWE, KM, i, RNAC, GrpRwertM(N)(KeyRe(NR)))
            DikAlt(i) = Rezsozpt.Rezepte(l).Dicke(N)
            For k = 0 To KM - 1
              DikNeu(i, k) = DikAlt(i)
            Next k
            For k = 0 To NFG - 1
              RezMng(i, k) = 0.0#
            Next k
            For j = 0 To KF - 1
              For k = 0 To NFG - 1
                If ID(j) = IDfarb(k) Then
                  RezMng(i, k) = ZwiMng(j)
                End If
              Next k
            Next j
            i = i + 1
          End If
        Next N
      End If
    Next l
    MMX = i
    '
    '
    '
    '
    NPQ1 = 0
    If CnzDep(MenueParam.Messg.CDE) <> 0 Then
      For j = 0 To 8
        NPQ1 = Max(NPQ1, CInt(MenueParam.Misch.GrStr(2, j) + 0.5))
      Next
      NPQ1 = NPQ1 + 1
      NABSTR = NPQ1
    Else
      NABSTR = MenueParam.Misch.Npx
    End If
    NPQ1 = NPNPS(MenueParam.Messg.CDE, MenueParam.Misch.Npx, NPQ1)
    Call GRDENDMHR(FEHL)
    Ier = FEHL.Ifeh
    If FehlDLL(FEHL, BitInt(28, 29, MenueParam.User.Writ)) Then
      If Ier = 0 Then Ier = FEHL.Iwarn
      Exit Sub
    End If
    Call GRDBEGMHR(NWE, KM, NME, NFG, NPQ1, MMX, FEHL)
    Ier = FEHL.Ifeh
    If FehlDLL(FEHL, BitInt(28, 29, MenueParam.User.Writ)) Then
      If Ier = 0 Then Ier = FEHL.Iwarn
      Exit Sub
    End If
    '
    '
    'Normlichtarten,Winkel,GK-Werte
    '
    '
    '
    '
    Call GetWinkNormGk("MHR", MenueParam.Menue.Kwj, Winkel, Ier)


    If Ier <> 0 Then Exit Sub
    '
    '
    '
    'Menuparameter
    '
    '
    '
    '
    For i = 0 To NME - 1
      FU(i) = MenueParam.Menue.Fu(i)
    Next i
    EXPO = MenueParam.Menue.EXPO
    GewR = MenueParam.Misch.Gewr
    GewDik = MenueParam.Menue.SPR
    FTO = MenueParam.Menue.Fto
    JTM = Max(MenueParam.Menue.Itm, 50)
    '
    '
    Iglae = Fix(Iprn / 2) Mod 2
    '
    If Iglae > 0 And CnzDep(MenueParam.Messg.CDE) <> 0 And KM = 1 Then
      Iglae = 1
    End If
    '
    '
    '
    Call GRDMENMHR(FU(0), EXPO, GewR, FTO, GewDik, JTM, Iglae, FEHL)
    Ier = FEHL.Ifeh
    If FehlDLL(FEHL, BitInt(28, 29, MenueParam.User.Writ)) Then
      If Ier = 0 Then Ier = FEHL.Iwarn
      Exit Sub
    End If
    '
    '
    'Mengen und Remissions-(Transmissions-)werte übernehmen
    '
    '
    '
    ReDim AKOWRT(9)
    ReDim GRSTR(9, 5)
    ReDim GRDAE(9, 5)
    ReDim GRCCQ(9, 5)
    '
    For j = 0 To 9
      AKOWRT(j) = MenueParam.Misch.Kowrt(j)
      For i = 0 To 5
        GRSTR(j, i) = MenueParam.Misch.GrStr(i, j)
        GRDAE(j, i) = MenueParam.Misch.GrDae(i, j)
        GRCCQ(j, i) = Asc(MenueParam.Misch.GrKop(i, j))
      Next i
    Next j

    '
    '
    ReDim Eff(NFG - 1)

    For k = 0 To NFG - 1
      KeyID = KeyName(IDfarb(k))
      Eff(k) = Rezsozpt.Farben(KeyID).Eff
      ICHF(k) = Rezsozpt.Farben(KeyID).Ichf
    Next k
    Call SCHICHT(NFG, ICHF(0), PS, QMS, NS, ICHX(0), Anteil(0, 0, 0), Neuge, Ier)


    Call GRDMGRFMHR(NWE, KM, NME, NFG, 6, MMX, _
    ICHF(0), AKOWRT(0), GRSTR(0, 0), GRDAE(0, 0), GRCCQ(0, 0), _
    DikAlt(0), RezMng(0, 0), _
    KWBUN(0), RETUN(0), RUNT(0, 0, 0), NrRwrt(0), KWB(0), IRT(0), RNAC(0, 0, 0), FEHL)
    Ier = FEHL.Ifeh
    If FehlDLL(FEHL, BitInt(28, 29, MenueParam.User.Writ)) Then
      If Ier = 0 Then Ier = FEHL.Iwarn
      Exit Sub
    End If
    '
    '
    '
    '
    'Grunddaten
    '
    '
    '
    '
    '
    '
    '
    'Alte Grunddaten übernehmen
    '
    '
    '
    '
    KFO = KFI
    If Iprn Mod 2 = 1 Then
      KFO = KFI + KVA
    End If

    ReDim Grund(NFG - 1, NPQ1 - 1, KM - 1, NWE - 1)
    '
    '
    '
    'CST(Anzahl Stützstellen=NST-1,Anzahl Farbmittel-1,)
    '
    ReDim NST(NFG - 1)

    ReDim Cst(NFG - 1, NPQ1 - 1, KM - 1)
    '
    '
    '
    ReDim GruGrz(NFG - 1, NPQ1 - 1, KM - 1, NWE - 1)
    '
    'Standardstartwerte
    '
    Call GRDGRUN(1, NFG, NWE, KM, NPQ1, NST(0), Cst(0, 0, 0), Grund(0, 0, 0, 0), FEHL)
    Call GRDGRUN(1, NFG, NWE, KM, NPQ1, NST(0), Cst(0, 0, 0), GruGrz(0, 0, 0, 0), FEHL)
    '    '
    '
    '
    '
    '
    KFU = KFO + 1
    '
    '


    '
    '
    'Umspeichern
    '
    'Alte Grunddaten werden übernommen
    '
    '
    '
    '
    For k = 0 To KFO - 1
      KeyID = KeyName(IDfarb(k))
      'Nst(k) = Resozpt.Farbm(KyId).OptData.Nst
      NST(k) = Rezsozpt.Farben(KeyID).OptData.Nst
      For i = 0 To NST(k) - 1
        For kw = 0 To KM - 1
          Cst(k, i, kw) = Rezsozpt.Farben(KeyID).OptData.Cst(i + 1)
        Next kw
      Next i
      'Nps = NPNPS(Resozpt.Farben(KeyId).OptData.NPX, Nst(k))
      NPS = NPNPS(MenueParam.Messg.CDE, MenueParam.Misch.Npx, NST(k))
      If NPS > NPQ1 Then
        Ier = 4011
        MsgBox(Texxt(Ier))
        Exit Sub
      End If
      If Rezsozpt.Farben(KeyID).OptData.Grund.Count = 0 And Rezsozpt.Farben(KeyID).Ichf <> 8 Then
        If k <= KFI - 1 Then
          Ier = 4164
          MsgBox(Texxt(4164))
          Exit Sub
        End If
        KFU = KFI + 1
        Exit For
      End If
      Call GetGrundDat(k, NWE, KM, NPS, Grund, Rezsozpt.Farben(KeyID).OptData)
      If Fix(Iprn / 8) Mod 2 = 1 Then
        Call GetGrundDat(k, NWE, KM, NPS, GruGrz, Rezsozpt.FarbAUX(KeyID).OptData)
      End If
    Next
    '
    '
    'Startwerte für neue Grunddaten (s. auch AKOWRT, GRSTR, GRDAE, GRCCQ) werden für die Farb-/Bindemittel K=KFU...NFG berechnet
    '
    '
    '
    '
    Call GRDGRUNMHR(KFU, NFG, NWE, KM, NPQ1, NST(0), Cst(0, 0, 0), Grund(0, 0, 0, 0), FEHL)
    Ier = FEHL.Ifeh
    If FehlDLL(FEHL, BitInt(28, 29, MenueParam.User.Writ)) Then
      If Ier = 0 Then Ier = FEHL.Iwarn
      Exit Sub
    End If
    '
    If Fix(Iprn / 4) Mod 2 = 0 Then
      '
      '
      '
      '
      'Grunddaten neu berechnen
      '
      '
      'Limitierungen festlegen
      '
      '
      '
      '
      Call GRDLIMMHR(KFI, KVA, KM, NPQ1, NST(0), Cst(0, 0, 0), FEHL)
      Ier = FEHL.Ifeh
      If FehlDLL(FEHL, BitInt(28, 29, MenueParam.User.Writ)) Then
        If Ier = 0 Then Ier = FEHL.Iwarn
        Exit Sub
      End If
      '
      'Drucken
      '
      '
      'Call GRDDRU(FEHL)

      '
      '
      'Neue Grunddaten berechnen
      '
      '
      '
      '
      'If fix(Iprn / 8) Mod 2 = 1 Then
      '
      'Standardgrenzen GruGrz werden als Limitierung verwendet 
      '
      'Call GRDCALCMHR(NWE, KM, NPQ1, NST(0), Cst(0, 0, 0), DikNeu(0, 0), Grund(0, 0, 0, 0), GruGrz(0, 0, 0, 0), PHHI, FEHL)
      'Else
      '
      'Alte Grunddaten Grund werden als Limitierung verwendet
      '
      '
      Call GRDCALCMHR(NWE, KM, NPQ1, NST(0), Cst(0, 0, 0), DikNeu(0, 0), Grund(0, 0, 0, 0), GruGrz(0, 0, 0, 0), PHHI, FEHL)
      'End If
      Ier = FEHL.Ifeh
      If FehlDLL(FEHL, BitInt(28, 29, MenueParam.User.Writ)) Then
        If Ier = 0 Then Ier = FEHL.Iwarn
        Exit Sub
      End If
      '
      '
      '
      '
      '
      '
    End If
    '
    '
    '
    'Neue Remissionswerte berechnen
    '
    '
    '
    '
    Call GRDREFLMHR(NWE, KM, NPQ1, NPX, NST(0), Cst(0, 0, 0), Grund(0, 0, 0, 0), MMX, NFG, _
              KWB(0), DikNeu(0, 0), RezMng(0, 0), RBER(0, 0, 0), GRNDALL(0, 0, 0, 0), FEHL)
    Ier = FEHL.Ifeh
    If FehlDLL(FEHL, BitInt(28, 29, MenueParam.User.Writ)) Then
      If Ier = 0 Then Ier = FEHL.Iwarn
      Exit Sub
    End If
    Call GRDENDMHR(FEHL)
    Ier = FEHL.Ifeh
    If FehlDLL(FEHL, BitInt(28, 29, MenueParam.User.Writ)) Then
      If Ier = 0 Then Ier = FEHL.Iwarn
      Exit Sub
    End If

    '
    '
    'Grunddaten umspeichern
    '
    For j = 0 To NFG - 1
      KeyID = KeyName(IDfarb(j))
      Rezsozpt.Farben(KeyID).OptData.Grund.clear()
      Rezsozpt.Farben(KeyID).OptData.Nst = NST(j)
      For i = 0 To NST(j) - 1
        Rezsozpt.Farben(KeyID).OptData.Cst(i + 1) = Cst(j, i, 0)
      Next i
      NPS = NPNPS(MenueParam.Messg.CDE, MenueParam.Misch.Npx, NST(j))
      For k = 0 To NPS - 1
        Rezsozpt.Farben(KeyID).OptData.Grund.Add(KeyRe(k), New CurvesRef)
        For kw = 0 To KM - 1
          Rezsozpt.Farben(KeyID).OptData.Grund(k).Add(Winkel(kw).Chrm, New CurveRef(NWE))
        Next kw
      Next k
      Call LetGrundDat(j, NWE, KM, NPS, Grund, Rezsozpt.Farben(KeyID).OptData)
    Next j
    '
    '
    '
    '
    '
    '
    'Streu- und Absorptionswerte (gesamt) umspeichern
    '
    '
    '
    '
    '
    If Not IsNothing(GesStreuAbs) Then
      GesStreuAbs.clear()
      For i = 0 To MMX - 1
        KeyRwrt = KeyRe(NrRwrt(i))
        KeyID = KeyRe(i)
        NPX = NopNPX(MenueParam.Messg.CDE)
        GesStreuAbs.AddFarb(KeyID, New Colorant)

        GesStreuAbs(KeyID).Name = GrpRwertM(WS(i))(KeyRwrt).Name
        GesStreuAbs(KeyID).Bem = GrpRwertM(WS(i))(KeyRwrt).Bem
        GesStreuAbs(KeyID).Ibas = GrpRwertM(WS(i))(KeyRwrt).kwb
        GesStreuAbs(KeyID).ID = GrpRwertM(WS(i))(KeyRwrt).ID
        GesStreuAbs(KeyID).OptData = New OpticalData
        For k = 0 To NPX - 1
          GesStreuAbs(KeyID).OptData.Grund.Add(KeyRe(k), New CurvesRef)
          For kw = 0 To KM - 1
            GesStreuAbs(KeyID).OptData.Grund(k).Add(Winkel(kw).Chrm, New CurveRef(NWE))
          Next kw
        Next k
        Call LetGrundDat(i, NWE, KM, NPX, GRNDALL, GesStreuAbs(KeyID).OptData)
      Next i
    End If
    '
    '
    '
    '
    '
    'Berechnete R-Werte umspeichern
    '
    '
    '

    For N = 0 To GrpRwertR.Count - 1
      GrpRwertR(N).clear()
      GrpRwertR(N).RefUnt = GrpRwertM(N).RefUnt
    Next N
    For i = 0 To MMX - 1


      '
      '
      'Remissionswerte  für ersten Untergründe
      '
      '
      KeyRwrt = KeyRe(NrRwrt(i))
      If GrpRwertM(WS(i)).ContainsKey(KeyRwrt) And Not GrpRwertR(WS(i)).ContainsKey(KeyRwrt) Then
        GrpRwertR(WS(i)).Add(KeyRwrt, New RefValue)
        GrpRwertR(WS(i))(KeyRwrt).ID = GrpRwertM(WS(i))(KeyRwrt).ID
        Call LetCalcRwert(Winkel, GrpRwertM(WS(i))(KeyRwrt), GrpRwertR(WS(i))(KeyRwrt), _
        GrpRwertM(WS(i))(KeyRwrt).Nr, False, GrpRwertM(WS(i))(KeyRwrt).IVoNa, GrpRwertM(WS(i))(KeyRwrt).Cme, _
        GrpRwertM(WS(i))(KeyRwrt).Name, GrpRwertM(WS(i))(KeyRwrt).Bem, i, RBER, Ier)
        For k = 0 To KM - 1
          GrpRwertR(WS(i))(KeyRwrt).Dik(k) = DikNeu(i, k)
        Next k
      End If
    Next i

    '
    '
    '
    '
    '
    Erase RUNT
    Erase RNAC
    Erase RBER
    Erase GRNDALL
    Erase RezMng
    Erase ZwiMng
    Erase Eff
    Erase ID
    Erase KWBUN
    Erase RETUN
    Erase KWB
    Erase IRT
    Erase AKOWRT
    Erase GRSTR
    Erase GRDAE
    Erase GRCCQ
    Erase DikNeu
    Erase DikAlt
    Erase Eff
    Erase ICHF
    Erase Cst
    Erase NST
    Erase Grund
    Erase GruGrz

  End Sub
  Sub dispose() Implements IDisposable.Dispose
    If Not IsNothing(umr) Then
      umr.dispose()
    End If
  End Sub
  Protected Overrides Sub Finalize()
    MyBase.Finalize()
    dispose()
  End Sub
End Class
