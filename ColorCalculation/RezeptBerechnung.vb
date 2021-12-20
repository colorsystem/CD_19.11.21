Option Strict Off
Option Explicit On
Option Compare Text
Imports ColorGeneral.colorallglib
Imports ColorGeneral.colorhilflib
Imports Colorparamlib.dbhilfprogramme
Imports ColorParamLIB.FrbHilfProgramme

Public Class RezeptBerechnung
  Implements IDisposable

  '
  '
  '
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
  '
  'Strteu- und Absorptionswerte (gesamt)
  '
  '


  Sub GrundDaten(ByRef Iprn As Integer, ByRef Winkel As AngGeos, ByRef Rezsozpt As RecipesGrp, ByRef GrpRwertM As RefValuesGrp, ByRef GrpRwertR As RefValuesGrp, ByRef GesStreuAbs As Colorants, ByRef Ier As Integer)
    Dim i As Integer
    Dim j As Integer
    Dim k As Integer
    Dim kw As Integer
    Dim l As Integer
    Dim N As Integer
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
    Dim ICH() As Integer
    Dim FST() As Single
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
    Dim NABSTR As Integer
    Dim JABST As Integer
    Dim GewDik As Single
    Dim Rezcount As Integer
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
    Ier = 0
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
    If NFG = 0 Then
      Ier = 2922
      MsgBox(Texxt(Ier))
      Exit Sub
    End If
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
          Call GetRwerte(Winkel, N, RUNT, GrpRwertM(N).RefUnt)
        Else
          If MenueParam.Misch.Transp And N = 0 Then
            Ier = 4050
            MsgBox(Texxt(Ier), MsgBoxStyle.OkOnly, Texxt(2000))
            Exit Sub
          End If
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
            Call GetRwerte(Winkel, i, RNAC, GrpRwertM(N)(KeyRe(NR)))
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
          Else
            '
            '
            'für nicht numerische Rezepte wird Zähler zurückgesetzt
            Rezcount = Rezcount - 1
          End If
        Next N
      End If
    Next l
    MMX = i
    If MMX < Rezcount Then
      MsgBox(Texxt(3956))
      Ier = 3956
      Exit Sub
    End If
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
    Call GRDEND(FEHL)
    Ier = FEHL.Ifeh
    If FehlDLL(FEHL, BitInt(28, 29, MenueParam.User.Writ)) Then
      If Ier = 0 Then Ier = FEHL.Iwarn
      Exit Sub
    End If
    Call GRDBEG(NWE, KM, NME, NFG, NPQ1, MMX, FEHL)
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
    Call GetWinkNormGk("GRD", MenueParam.Menue.Kwj, Winkel, Ier)
    If Ier <> 0 Then
      MsgBox(Texxt(Ier), MsgBoxStyle.OkOnly, Texxt(2000))
      Exit Sub
    End If

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
    Call GRDMEN(FU(0), EXPO, GewR, FTO, GewDik, JTM, Iglae, FEHL)

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
    ReDim ICH(NFG - 1)
    ReDim FST(NFG - 1)

    For k = 0 To NFG - 1
      KeyID = KeyName(IDfarb(k))
      Eff(k) = Rezsozpt.Farben(KeyID).Eff
      ICH(k) = Rezsozpt.Farben(KeyID).Ichf
      FST(k) = Rezsozpt.Farben(KeyID).Fst
    Next k
    Call GRDMGRF(NWE, KM, NME, NFG, 6, MMX, _
    ICH(0), FST(0), AKOWRT(0), GRSTR(0, 0), GRDAE(0, 0), GRCCQ(0, 0), _
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
    '
    '
    '
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

      Call GetGrundDat(k, Winkel, NPS, Grund, Rezsozpt.Farben(KeyID).OptData)
      If Fix(Iprn / 8) Mod 2 = 1 Then
        Call GetGrundDat(k, Winkel, NPS, GruGrz, Rezsozpt.Farben(KeyID).OptData)
      End If
    Next
    '
    '
    'Startwerte für neue Grunddaten (s. auch AKOWRT, GRSTR, GRDAE, GRCCQ) werden für die Farb-/Bindemittel K=KFU...NFG berechnet
    '
    '
    '
    '
    Call GRDGRUN(KFU, NFG, NWE, KM, NPQ1, NST(0), Cst(0, 0, 0), Grund(0, 0, 0, 0), FEHL)
    


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
      Call GRDLIM(KFI, KVA, KM, NPQ1, NST(0), Cst(0, 0, 0), FEHL)
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
      'Call GRDCALC(NWE, KM, NPQ1, NST(0), Cst(0, 0, 0), DikNeu(0, 0), Grund(0, 0, 0, 0), GruGrz(0, 0, 0, 0), PHHI, FEHL)
      'Else
      '
      'Alte Grunddaten Grund werden als Limitierung verwendet
      '
      '
      Call GRDCALC(NWE, KM, NPQ1, NST(0), Cst(0, 0, 0), DikNeu(0, 0), Grund(0, 0, 0, 0), GruGrz(0, 0, 0, 0), PHHI, FEHL)
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
    End If
    '
    '
    '
    'Neue Remissionswerte berechnen
    '
    '
    '
    '
    Call GRDREFL(NWE, KM, NPQ1, NPX, NST(0), Cst(0, 0, 0), Grund(0, 0, 0, 0), MMX, NFG, _
              KWB(0), DikNeu(0, 0), RezMng(0, 0), RBER(0, 0, 0), GRNDALL(0, 0, 0, 0), FEHL)

    Ier = FEHL.Ifeh
    If FehlDLL(FEHL, BitInt(28, 29, MenueParam.User.Writ)) Then
      If Ier = 0 Then Ier = FEHL.Iwarn
      Exit Sub
    End If
    Call GRDEND(FEHL)
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
        Call ADDCurves(Rezsozpt.Farben(KeyID).OptData.Grund(k))
        'For kw = 0 To KM - 1

        'Rezsozpt.Farben(KeyID).OptData.Grund(k).Add(Winkel(kw).Chrm, New CurveRef(NWE))
        'Next kw
      Next k
      Call LetGrundDat(j, Winkel, NPS, Grund, Rezsozpt.Farben(KeyID).OptData)
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
          Call ADDCurves(GesStreuAbs(KeyID).OptData.Grund(k))
        Next k
        Call LetGrundDat(i, Winkel, NPX, GRNDALL, GesStreuAbs(KeyID).OptData)
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
      'Remissionswerte  für ersten Untergrund
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
    Erase FST
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
    Erase ICH
    Erase Cst
    Erase NST
    Erase Grund
    Erase GruGrz

  End Sub
  Sub MischRezept(ByRef Iprn As Integer, ByRef Winkel As AngGeos, ByRef KeyMenge As String, ByRef Rezsozpt As RecipesGrp, ByRef GrpRwerte As RefValuesGrp, ByRef OptGesamt As OpticalData, ByRef Ier As Integer)
    Dim i As Integer
    Dim k As Integer
    Dim kw As Integer
    Dim NPS As Integer
    Dim MngRei As Single
    '
    '
    Ier = 0
    '
    '
    '
    '
    '
    Call RezEnde("RZP", Ier)
    Call RezBeginn("RZP", Winkel, Rezsozpt.Rezepte(KeyMenge).KF, 0, KeyMenge, Rezsozpt, Ier)
    If Ier <> 0 Then
      Exit Sub
    End If
    '
    '
    '
    '
    '
    '
    '
    '
    'Normlichtarten,Winkel,GK-Werte
    '
    '
    '
    '
    '
    '
    '
    '
    Call GetWinkNormGk("RZP", MenueParam.Menue.Kwj, Winkel, Ier)
    If Ier <> 0 Then Exit Sub
    '
    '
    '
    'Call REZDRU(Fehl)

    '
    '
    '
    '
    '
    'Parameter für Rezeptberechnung
    '
    '
    '
    '
    '
    '
    '
    Call GetRezeptMenu("RZP", Ier)
    If Ier > 0 Then Exit Sub

    '
    NF = Rezsozpt.Rezepte(KeyMenge).KF
    ReDim MischMng(NF - 1)
    ReDim LK(NF - 1)
    ReDim ID(NF - 1)
    For i = 0 To NF - 1
      LK(i) = i + 1
    Next
    Call GetFamng(KeyMenge, Rezsozpt, NF, ID, MischMng, Ier)
    For i = 0 To 1
      DICKE(i) = Rezsozpt.Rezepte(KeyMenge).Dicke(i)
    Next i

    '
    '
    'Grunddaten
    '
    '
    '
    '.DLL-Aufruf für Übernahme der Grunddaten (Farbmittel von KeyMenge)
    '
    MngRei = umr.MngINR(KeyMenge, Rezsozpt, Ier)
    If Rezsozpt.INO = 0 Then
      MngRei = Max(MngRei, umr.MngINL(KeyMenge, Rezsozpt, Ier))
    Else
      MngRei = Max(MngRei, Rezsozpt.MngMax)
    End If
    Call GetGrundDaten("RZP", Winkel, Rezsozpt, MngRei, NF, ID, Ier)

    If Ier <> 0 Then
      Exit Sub
    End If
    '
    '
    '
    '
    '
    'Rezept umspeichern nach Rezmng
    '
    '
    '
    '
    'Call REZDRU(FEHL)
    '
    '
    NWE = Winkel.Wsol.Nwe
    KM = Winkel.Km
    ReDim RUNT(1, KM - 1, NWE - 1)
    ReDim RVOR(1, KM - 1, NWE - 1)
    ReDim RNAC(1, KM - 1, NWE - 1)
    '
    '
    'Reflexionswerte für Untergründe
    '
    '
    '
    NUU = 0
    For i = 0 To GrpRwerte.Count - 1
      If Not IsNothing(GrpRwerte(i).RefUnt) Then
        KWB(i) = GrpRwerte(i).RefUnt.kwb + 1
        Retr(i) = GrpRwerte(i).RefUnt.ReTr
        If GrpRwerte(i).RefUnt.ID = -1 And MenueParam.Misch.Transp And i = 0 Then
          Ier = 4050
          MsgBox(Texxt(Ier), MsgBoxStyle.OkOnly, Texxt(2000))
          Exit Sub
        End If
        If GrpRwerte(i).RefUnt.RefKurv.Count > 0 AndAlso GrpRwerte(i).RefUnt.IVoNa Then
          Call GetRwerte(Winkel, i, RUNT, GrpRwerte(i).RefUnt)
          NUU = NUU + 1
        End If
      End If
    Next i
    '
    '
    '
    '
    'Reflexionswerte für Vorlagen (hier nicht unbedingt erforderlich (SORKRIT=0.0 falls NUV=0 und NUU=0))
    '
    '
    '
    '
    '
    NUV = 0
    'Reflexionswerte für Vorlagen
    '
    '
    '
    '
    For i = 0 To GrpRwerte.Count - 1
      If GrpRwerte(i).ContainsKey("V") AndAlso Not IsNothing(GrpRwerte(i)("V")) Then
        If Not IsNothing(GrpRwerte(i)("V").RefKurv) AndAlso GrpRwerte(i)("V").RefKurv.Count > 0 AndAlso GrpRwerte(i)("V").IVoNa Then
          If KWB(i) - 1 <> GrpRwerte(i)("V").kwb Then
            Ier = 4077
            MsgBox(Texxt(Ier))
            Exit Sub
          End If
          If Retr(i) <> GrpRwerte(i)("V").ReTr Then
            Ier = 4148
            MsgBox(Texxt(Ier))
            Exit Sub
          End If
          Call GetRwerte(Winkel, i, RVOR, GrpRwerte(i)("V"))
          NUV = NUV + 1
        End If
      End If
    Next i
    If NUU = 0 And NUV = 0 Then NUU = 1

    '
    NUN = 0
    '
    '
    '
    '

    '
    '      '
    '
    '
    'Mengen und Reflexionskurven übernehmen
    '
    '
    '
    '
    '
    '
    Call REZMGRF(1, NUU, NUV, NUN, NWE, KM, Retr(0), KWB(0), _
    RUNT(0, 0, 0), RVOR(0, 0, 0), RNAC(0, 0, 0), NF, MischMng(0), DICKE(0), FEHL)
    Ier = FEHL.Ifeh
    If FehlDLL(FEHL, BitInt(28, 29, MenueParam.User.Writ)) Then
      If Ier = 0 Then Ier = FEHL.Iwarn
      Exit Sub
    End If

    '
    '
    ReDim RBER(1, KM - 1, NWE - 1)
    ReDim Gruges(0, NPNPS(MenueParam.Messg.CDE, MenueParam.Misch.Npx, 1) - 1, KM - 1, NWE - 1)
    ReDim CSTges(0, KM - 1)
    ReDim Proz(NF - 1)
    ReDim Prob(NF - 1)
    ReDim Spz(NF - 1)
    ReDim Ichf(NF - 1)
    ReDim Eff(NF - 1)

    Ivol = Rezsozpt.IVOL
    Inf = Rezsozpt.INF
    Sploe = Rezsozpt.SpLoe
    DosMin = MenueParam.Misch.MinDos
    '
    '
    '
    '
    '
    If DosMin > 0.0# Then
      Call GetProzProbSpz(KeyMenge, Rezsozpt, NF, Proz, Prob, Spz, Ier)
      Call GetIchfEff(Rezsozpt.Farben, NF, ID, Ichf, Eff, Ier)
      Call REZANGL(NF, Ivol, Inf, Sploe, LK(0), MischMng(0), _
                   Proz(0), Prob(0), Eff(0), Spz(0), Ichf(0), DosMin, FEHL)
      Ier = FEHL.Ifeh
      If FehlDLL(FEHL, BitInt(28, 29, MenueParam.User.Writ)) Then
        If Ier = 0 Then Ier = FEHL.Iwarn
        Exit Sub
      End If
      '
    End If
    '
    Call LetMengen(KeyMenge, KeyMenge, Rezsozpt, Rezsozpt.Rezepte(KeyMenge).Nr, _
    Rezsozpt.Rezepte(KeyMenge).Name, Rezsozpt.Rezepte(KeyMenge).Bem, NF, LK, MischMng, DICKE, Ier)
    Call umr.CalcBamng(KeyMenge, Rezsozpt, Ier)
    'Call REZDRU(FEHL)
    '
    '
    'IPRN=0
    'Reflexionswerte RBER aus Mengen ZwiMng berechnen
    'Iprn=1
    'zusätzlich Gruges = neue Grunddaten 
    'Iprn=2
    'Grunddaten werden nur aus Farbmitteln mit OP<>"=" berechnet
    '
    '
    Call MSHREZ(Iprn, NWE, KM, RBER(0, 0, 0), _
    NF, LK(0), MischMng(0), DICKE(0), _
    NST, CSTges(0, 0), Gruges(0, 0, 0, 0), SorKrit(0, 0), FEHL)
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
    '
    'Berechnete Reflexionswerte RBER umspeichern
    '
    '
    Call RueckSpei(NUU, NUV, Winkel, GrpRwerte, Ier)



    '
    '
    '
    'Sortierkriterien umspeichern
    '
    '
    Call GetSorkrit(Rezsozpt.Rezepte(KeyMenge), SorKrit)

    '
    '

    '
    'Neue Grunddaten Umspeichern
    '
    '
    '
    OptGesamt.Grund.clear()
    OptGesamt.Nst = NST
    For i = 0 To NST - 1
      OptGesamt.Cst(i + 1) = CSTges(i, 0)
    Next i
    NPS = NPNPS(MenueParam.Messg.CDE, MenueParam.Misch.Npx, NST)
    For k = 0 To NPS - 1
      OptGesamt.Grund.Add(KeyRe(k), New CurvesRef)
      Call ADDCurves(OptGesamt.Grund(k))

      'For kw = 0 To KM - 1
      ' OptGesamt.Grund(k).Add(Winkel(kw).Chrm, New CurveRef(NWE))
      'Next kw
    Next k
    '
    '
    '
    Call LetGrundDat(0, Winkel, NPS, Gruges, OptGesamt)
    '
    '

    '
    '
    Call RezEnde("RZP", Ier)
    '
    '
    '
    '
    '
    Erase RezMng
    Erase LK
    Erase RBER
    Erase Gruges
    Erase Proz
    Erase Prob
    Erase Spz
    Erase Ichf
    Erase Eff
    '
    '
    '
    ''
  End Sub
  '
  '
  '
  '
  '
  '
  Sub BasisRezept(ByVal KFIT As Integer, ByRef Winkel As AngGeos, _
  ByRef KeySort As String, ByRef KeyBasis As String, ByRef Rezsozpt As RecipesGrp, _
  ByRef GrpRwerte As RefValuesGrp, ByRef Ier As Integer)
    Dim iee As Integer
    '
    '
    Ier = 0
    Call RezBeginn("RZP", Winkel, Rezsozpt.Rezepte(KeySort).KF, 0, KeySort, Rezsozpt, Ier)
    If Ier <> 0 Then
      MsgBox(Texxt(Ier))
      Call RezEnde("RZP", iee)
      Exit Sub
    End If

    '
    '
    '
    'Basisrezept berechnen
    '
    '
    '
    '
    Call BaseRezept(KFIT, Winkel, KeySort, KeyBasis, Rezsozpt, BasMng, GrpRwerte, Ier)
    If Ier <> 0 Then
      Call RezEnde("RZP", iee)
      Exit Sub
    End If

    '
    '
    '
    '
    '
    Call RezEnde("RZP", Ier)
    Erase BasMng
    Erase ID
    Erase LK
  End Sub

  Private Sub RueckSpei(ByVal NUU As Integer, ByVal NUV As Integer, ByVal Winkel As AngGeos, ByRef GrpRwerte As RefValuesGrp, ByRef Ier As Integer)
    Dim RefDummy As RefValue
    Dim kw As Integer
    If GrpRwerte.Count > 0 Then
      GrpRwerte(1)("R").IVoNa = False
    End If
    If NUV > 0 And GrpRwerte(0)("V").RefKurv.Count > 0 Then
      '
      '
      'Vorlage 1 vorhanden
      '
      '
      Call LetCalcRwert(Winkel, GrpRwerte(0)("V"), GrpRwerte(0)("R"), _
      GrpRwerte(0)("V").Nr, False, True, "  ", Texxt(860), "  ", 0, RBER, Ier)

    ElseIf NUU > 0 And GrpRwerte(0).RefUnt.RefKurv.Count > 0 Then
      Call LetCalcRwert(Winkel, GrpRwerte(0).RefUnt, GrpRwerte(0)("R"), _
      GrpRwerte(0).RefUnt.Nr, False, True, "  ", Texxt(860) & " (1)", "  ", 0, RBER, Ier)
    Else
      RefDummy = New RefValue
      For kw = 0 To Winkel.Km - 1
        RefDummy.RefKurv.Add(Winkel(kw).Chrm, New CurveRef(Winkel.Wsol.Nwe))
      Next kw
      RefDummy.kwb = 0
      RefDummy.IVoNa = True

      Call LetCalcRwert(Winkel, RefDummy, GrpRwerte(0)("R"), _
      -1, False, True, "  ", Texxt(860) & " (1)", "  ", 0, RBER, Ier)
    End If
    If NUV > 1 AndAlso GrpRwerte(1).ContainsKey("V") AndAlso GrpRwerte(1)("V").RefKurv.Count > 0 Then
      '
      '
      'Vorlage 2 vorhanden
      '
      '
      Call LetCalcRwert(Winkel, GrpRwerte(1)("V"), GrpRwerte(1)("R"), _
      GrpRwerte(1)("V").Nr, False, True, "  ", Texxt(860) & " (2)", "  ", 1, RBER, Ier)
    ElseIf NUU = 2 AndAlso GrpRwerte(1).RefUnt.RefKurv.Count > 0 Then
      Call LetCalcRwert(Winkel, GrpRwerte(1).RefUnt, GrpRwerte(1)("R"), _
      GrpRwerte(1).RefUnt.Nr, False, True, "  ", Texxt(860) & " (2)", "  ", 1, RBER, Ier)
    End If

  End Sub

  Sub ErstRezept(ByVal KFIT As Integer, ByRef Winkel As AngGeos, _
  ByRef KeySort As String, ByRef KeyBasis As String, ByRef Rezsozpt As RecipesGrp, _
  ByRef GrpRwerte As RefValuesGrp, ByRef Ier As Integer)
    Dim k As Integer
    Dim i As Integer
    Dim RzNr As String
    Dim KFM As Integer
    Dim KZAL As Integer
    Dim KZL As Integer
    Dim iee As Integer
    Dim Kfitt As Integer
    '
    '
    Ier = 0
    Call RezBeginn("RZP", Winkel, Rezsozpt.Rezepte(KeySort).KF, 0, KeySort, Rezsozpt, Ier)
    If Ier <> 0 Then
      Exit Sub
    End If
    '
    '
    '
    'Basisrezept berechnen
    '
    '
    '
    '
    Call BaseRezept(KFIT, Winkel, KeySort, KeyBasis, Rezsozpt, BasMng, GrpRwerte, Ier)
    If Ier <> 0 Then
      Call RezEnde("RZP", iee)
      Exit Sub
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
    'Erstrezepte mit maximal KFM Farb-Bindemittel berechnen.
    '
    '

    NF = Rezsozpt.Rezepte(KeyBasis).KF
    KFM = Max(MenueParam.Misch.MaxFarb, 3)
    KZAL = MenueParam.Misch.Rzp
    NWE = Winkel.Wsol.Nwe
    KM = Winkel.Km

    ReDim LKid(KZAL, KFM - 1)
    ReDim NFR(KZAL)
    ReDim RezMng(KZAL, KFM - 1)
    '
    '
    '
    '
    KZL = KZAL
    Kfitt = KFIT Mod 8
    If Kfitt = 3 Then
      Call REZREZ(Kfitt, NF, BasMng(0), KZL, KFM, NFR(0), LKid(0, 0), RezMng(0, 0), MenueParam.Messg.RefGew.R(0), FEHL)
    Else
      Call REZREZ(Kfitt, NF, BasMng(0), KZL, KFM, NFR(0), LKid(0, 0), RezMng(0, 0), MenueParam.Messg.RefOne.R(0), FEHL)
    End If
    '
    Ier = FEHL.Ifeh
    If FehlDLL(FEHL, BitInt(28, 29, MenueParam.User.Writ)) Then
      If Ier = 0 Then Ier = FEHL.Iwarn
      Call RezEnde("RZP", Ier)
      Exit Sub
    End If

   
    '
    ReDim RBER(1, KM - 1, NWE - 1)
    ReDim Gruges(0, NPNPS(MenueParam.Messg.CDE, MenueParam.Misch.Npx, 1) - 1, KM - 1, NWE - 1)
    ReDim CSTges(0, KM - 1)
    ReDim ZwiMng(KFM - 1)
    ReDim LK(KFM - 1)
    ReDim Proz(KFM - 1)
    ReDim Prob(KFM - 1)
    ReDim Spz(KFM - 1)
    ReDim Ichf(KFM - 1)
    ReDim Eff(KFM - 1)
    ReDim ID(KFM - 1)
    Ivol = Rezsozpt.IVOL
    Inf = Rezsozpt.INF
    Sploe = Rezsozpt.SpLoe
    DosMin = MenueParam.Misch.MinDos
    '
    '
    For i = 0 To 1
      DICKE(i) = Rezsozpt.Rezepte(KeySort).Dicke(i)
    Next i
    '
    '
    'Alte Einträge löschen
    '
    'Außer "SOR","BAS" werden ale Rezepte gelöscht
    '
    '
    'Außer "V" und "R" werden alle Reflexionswerte gelöscht
    '
    '
    '
    Call DelRezRef({"SOR", "BAS"}, Rezsozpt, {"V", "R"}, GrpRwerte)


    For k = 0 To KZL - 1
      RzNr = KeyRe(k)

      '
      '
      'Rezepte umspeichern
      '
      For i = 0 To KFM - 1
        LK(i) = 0
        ZwiMng(i) = 0.0
      Next
      For i = 0 To NFR(k) - 1
        LK(i) = LKid(k, i)
        ZwiMng(i) = RezMng(k, i)
      Next
      '
      '
      ' 
      '
      '
      '
      'Rezepte und R-Werte umspeichern
      '
      '
      '
      'Mengen umspeichern
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
      Rezsozpt.Rezepte.AddRez(RzNr, New Recipe)
      Call LetMengen(KeyBasis, RzNr, Rezsozpt, k, _
      GrpRwerte(0)("V").Name, GrpRwerte(0)("V").Bem, NFR(k), LK, ZwiMng, DICKE, Ier)
      Call umr.CalcBamng(RzNr, Rezsozpt, Ier)
      '
      If DosMin > 0.0# Then
        For i = 0 To NFR(k) - 1
          ID(i) = Rezsozpt.Farben(LK(i) - 1).ID
        Next
        Call GetIchfEff(Rezsozpt.Farben, NFR(k), ID, Ichf, Eff, Ier)
        Call GetProzProbSpz(RzNr, Rezsozpt, NFR(k), Proz, Prob, Spz, Ier)

        Call REZANGL(NFR(k), Ivol, Inf, Sploe, LK(0), ZwiMng(0), _
                 Proz(0), Prob(0), Eff(0), Spz(0), Ichf(0), DosMin, FEHL)
        Ier = FEHL.Ifeh
        If FehlDLL(FEHL, BitInt(28, 29, MenueParam.User.Writ)) Then
          If Ier = 0 Then Ier = FEHL.Iwarn
          Call RezEnde("RZP", Ier)
          Exit Sub
        End If
        '
        '
        Call LetMengen(KeyBasis, RzNr, Rezsozpt, k, _
        GrpRwerte(0)("V").Name, GrpRwerte(0)("V").Bem, NFR(k), LK, ZwiMng, DICKE, Ier)
        Call umr.CalcBamng(RzNr, Rezsozpt, Ier)
      End If

      'Reflexionswerte RBER aus Mengen ZwiMng berechnen
      '
      Call MSHREZ(0, NWE, KM, RBER(0, 0, 0), _
      NFR(k), LK(0), ZwiMng(0), DICKE(0), _
      NST, CSTges(0, 0), Gruges(0, 0, 0, 0), SorKrit(0, 0), FEHL)
      Ier = FEHL.Ifeh
      If FehlDLL(FEHL, BitInt(28, 29, MenueParam.User.Writ)) Then
        If Ier = 0 Then Ier = FEHL.Iwarn
        Call RezEnde("RZP", Ier)
        Exit Sub
      End If

      '
      '
      '
      '
      '
      '
      '
      'Berechnete Reflexionswerte RBER umspeichern
      '
      '
      For i = 0 To 1
        GrpRwerte(i).Add(RzNr, New RefValue)
      Next
      For i = 0 To NUV - 1
        If i = 0 Then
          Call LetCalcRwert(Winkel, GrpRwerte(i)("V"), GrpRwerte(i)(RzNr), _
           k, False, True, "  ", Texxt(860), "  ", i, RBER, Ier)
        Else
          Call LetCalcRwert(Winkel, GrpRwerte(i)("V"), GrpRwerte(i)(RzNr), _
           k, False, True, "  ", Texxt(860) & " (2)", "  ", i, RBER, Ier)
        End If
      Next i
      If NUU = 2 And NUV = 1 Then
        'Berechnete Werte, falls Deckvermögen berechnet wurde
        Call LetCalcRwert(Winkel, GrpRwerte(1).RefUnt, GrpRwerte(1)(RzNr), _
         k, False, True, "  ", Texxt(860) & " (2)", "  ", 1, RBER, Ier)

      End If
      '
      '
      '
      'Sortierkriterien umspeichern
      '
      Call GetSorkrit(Rezsozpt.Rezepte(RzNr), SorKrit)

    Next k     '
    '
    '
    Call RezEnde("RZP", Ier)
    '
    '
    '
    '
    '
    Erase BasMng
    Erase LKid
    Erase NFR
    Erase RezMng
    Erase ZwiMng
    Erase LK
    Erase RBER
    Erase Gruges
    Erase Proz
    Erase Prob
    Erase Spz
    Erase Ichf
  End Sub

  Private Sub BaseRezept(ByVal KFIT As Integer, ByRef Winkel As AngGeos, _
  ByRef KeySort As String, ByRef KeyBasis As String, ByRef Rezsozpt As RecipesGrp, ByRef Basmng() As Single, _
  ByRef GrpRwerte As RefValuesGrp, ByRef Ier As Integer)
    Dim i As Integer
    Dim MngRei As Single
    Dim Ummng As Single
    Dim BasMNG2() As Single
    Dim Basmng5() As Single
    Dim DE2 As Single
    Dim DE5 As Single
    Dim Kfitt As Integer
    '
    '
    '
    '
    '
    'Normlichtarten,Winkel,GK-Werte
    '
    '
    '
    '
    '
    '
    '
    '
    Call GetWinkNormGk("RZP", MenueParam.Menue.Kwj, Winkel, Ier)
    If Ier <> 0 Then Exit Sub
    '
    '
    '
    'Call REZDRU(Fehl)

    '
    '
    '
    '
    '
    'Parameter für Rezeptberechnung
    '
    '
    '
    '
    '
    '
    '
    Call GetRezeptMenu("RZP", Ier)
    If Ier > 0 Then Exit Sub

    '
    '
    '
    '
    '
    '
    'Rezept umspeichern
    '
    '
    '
    '
    'Call REZDRU(FEHL)

    NF = Rezsozpt.Rezepte(KeySort).KF
    ReDim ID(NF - 1)
    ReDim SozMng(NF - 1)
    ReDim Basmng(NF - 1)
    ReDim BasMNG2(NF - 1)
    ReDim Basmng5(NF - 1)
    ReDim HlfMng(NF - 1)
    ReDim LK(NF - 1)
    ReDim Proz(NF - 1)
    ReDim Prob(NF - 1)
    ReDim Spz(NF - 1)
    For i = 0 To NF - 1
      LK(i) = i + 1
    Next
    Call GetFamng(KeySort, Rezsozpt, NF, ID, SozMng, Ier)
    For i = 0 To 1
      DICKE(i) = Rezsozpt.Rezepte(KeySort).Dicke(i)
    Next i
    '
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
    MngRei = umr.MngINR(KeySort, Rezsozpt, Ier)
    If Rezsozpt.INO = 0 Then
      MngRei = Max(MngRei, umr.MngINL(KeySort, Rezsozpt, Ier))
    Else
      MngRei = Max(MngRei, Rezsozpt.MngMax)
    End If
    Call GetGrundDaten("RZP", Winkel, Rezsozpt, MngRei, NF, ID, Ier)
    If Ier <> 0 Then
      Exit Sub
    End If
    '
    '
    '
    '
    '
    '
    '
    '
    'Reflexionswerte für Untergründe
    '
    '
    '
    '
    NWE = Winkel.Wsol.Nwe
    KM = Winkel.Km
    ReDim RUNT(1, KM - 1, NWE - 1)
    ReDim RVOR(1, KM - 1, NWE - 1)
    ReDim RNAC(1, KM - 1, NWE - 1)
    ReDim RBER(1, KM - 1, NWE - 1)
    '
    '
    '
    'Reflexionswerte Untergrund
    NUU = 0
    For i = 0 To GrpRwerte.Count - 1
      KWB(i) = i + 1
      If Not IsNothing(GrpRwerte(i).RefUnt) Then
        KWB(i) = GrpRwerte(i).RefUnt.kwb + 1
        Retr(i) = GrpRwerte(i).RefUnt.ReTr
        If GrpRwerte(i).RefUnt.ID = -1 And MenueParam.Misch.Transp And i = 0 Then
          Ier = 4050
          MsgBox(Texxt(Ier), MsgBoxStyle.OkOnly, Texxt(2000))
          Exit Sub
        End If
        If GrpRwerte(i).RefUnt.RefKurv.Count > 0 AndAlso GrpRwerte(i).RefUnt.IVoNa Then
          Call GetRwerte(Winkel, i, RUNT, GrpRwerte(i).RefUnt)
          NUU = NUU + 1
        End If
      End If
    Next i



    '
    '
    '
    '
    'Reflexionswerte für Vorlagen
    '
    '
    '
    '
    NUV = 0
    For i = 0 To GrpRwerte.Count - 1
      If GrpRwerte(i).ContainsKey("V") AndAlso Not IsNothing(GrpRwerte(i)("V")) Then
        If Not IsNothing(GrpRwerte(i)("V").RefKurv) AndAlso GrpRwerte(i)("V").RefKurv.Count > 0 AndAlso GrpRwerte(i)("V").IVoNa Then
          If KWB(i) - 1 <> GrpRwerte(i)("V").kwb Then
            Ier = 4077
            MsgBox(Texxt(Ier))
            Exit Sub
          End If
          If Retr(i) <> GrpRwerte(i)("V").ReTr Then
            Ier = 4148
            MsgBox(Texxt(Ier))
            Exit Sub
          End If
          Call GetRwerte(Winkel, i, RVOR, GrpRwerte(i)("V"))
          NUV = NUV + 1
        End If
      End If
    Next i

    NUN = 0
    '
    '
    '
    '
    '
    'Sortimentsmengen und Reflexionskurven übernehmen
    '
    '
    '
    '
    '
    '
    Call REZMGRF(2, NUU, NUV, NUN, NWE, KM, Retr(0), KWB(0), _
    RUNT(0, 0, 0), RVOR(0, 0, 0), RNAC(0, 0, 0), NF, SozMng(0), DICKE(0), FEHL)
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
    'Limitierungen für Farb-/Bindemittelmengen
    '
    '
    '
    '
    '
    '
    '
    Ummng = Rezsozpt.Rezepte(KeySort).UmMng
    For i = 0 To NF - 1
      Basmng(i) = Rezsozpt.Rezepte(KeySort)(i).FaAmng
    Next
    Call GetProzProbSpz(KeySort, Rezsozpt, NF, Proz, Prob, Spz, Ier)
    Call GetRezLimit("RZP", Ummng, Rezsozpt, NF, ID, HlfMng, Basmng, Proz, Prob, Spz, Ier)
   
    If Ier <> 0 Then
      Exit Sub
    End If
    '
    '

    '
    '
    'Basisrezept berechnen
    '
    '
    '
    '
    '
    '
    '
    '
    '
    'Angleich Reflexionskurve
    '
    '
    '
    'Call REZDRU(FEHL)
    Kfitt = KFIT Mod 8
    If Kfitt <> 2 And KFIT >= 8 Then
      For i = 0 To NF - 1
        BasMNG2(i) = Basmng(i)
        Basmng5(i) = Basmng(i)
      Next
      '
      'Verfahren Schittkowski
      '
      '
      Call BASREZ(2, NF, BasMNG2(0), NWE, KM, RBER(0, 0, 0), SorKrit(0, 0), MenueParam.Messg.RefOne.R(0), FEHL)
      Ier = FEHL.Ifeh
      If FehlDLL(FEHL, BitInt(28, 29, MenueParam.User.Writ)) Then
        If Ier = 0 Then Ier = FEHL.Iwarn
        Exit Sub
      End If
      DE2 = SorKrit(0, 4)
      '
      '
      'Verfahren Hansen
      '
      '
      Call BASREZ(5, NF, Basmng5(0), NWE, KM, RBER(0, 0, 0), SorKrit(0, 0), MenueParam.Messg.RefOne.R(0), FEHL)
      Ier = FEHL.Ifeh
      If FehlDLL(FEHL, BitInt(28, 29, MenueParam.User.Writ)) Then
        If Ier = 0 Then Ier = FEHL.Iwarn
        Exit Sub
      End If
      DE5 = SorKrit(0, 4)
      If DE2 < DE5 Or Ier <> 0 Then
        For i = 0 To NF - 1
          Basmng(i) = BasMNG2(i)
        Next
      Else
        For i = 0 To NF - 1
          Basmng(i) = Basmng5(i)
        Next
      End If
    End If
    '
    '
    '
    '
    'Angleich gemäß KFIT
    '
    'Call REZDRU(FEHL)
    '
    '
    If Kfitt = 3 Then
      Call BASREZ(Kfitt, NF, Basmng(0), NWE, KM, RBER(0, 0, 0), SorKrit(0, 0), MenueParam.Messg.RefGew.R(0), FEHL)
    Else
      Call BASREZ(Kfitt, NF, Basmng(0), NWE, KM, RBER(0, 0, 0), SorKrit(0, 0), MenueParam.Messg.RefOne.R(0), FEHL)
    End If
    Ier = FEHL.Ifeh
    If FehlDLL(FEHL, BitInt(28, 29, MenueParam.User.Writ)) Then
      If Ier = 0 Then Ier = FEHL.Iwarn
      Exit Sub
    End If
    '
    '
    '
    'Rezepte umspeichern
    '
    '
    '
    Call LetMengen(KeySort, KeyBasis, Rezsozpt, 1, _
    GrpRwerte(0)("V").Name, GrpRwerte(0)("V").Bem, NF, LK, Basmng, DICKE, Ier)
    Call umr.CalcBamng(KeyBasis, Rezsozpt, Ier)
    '
    '
    'berechnete R-Werte umspeichern
    '
    '
    '
    Call RueckSpei(NUU, NUV, Winkel, GrpRwerte, Ier)
    '
    'Sortierkriterien umspeichern
    '
    '
    '
    Call GetSorkrit(Rezsozpt.Rezepte(KeyBasis), SorKrit)

    '
    '
    '
    Erase BasMNG2
    Erase Basmng5
    Erase HlfMng
    Erase LK
    Erase Proz
    Erase Prob
    Erase Spz
    Erase SozMng
    Erase RUNT
    Erase RVOR
    Erase RNAC
    Erase RBER
  End Sub
  Sub DelRezRef(NotDelRez() As String, ByRef Rezsozpt As RecipesGrp, NotDelRef() As String, ByRef GrpRwerte As RefValuesGrp)
    Dim l As Integer
    Dim k As Integer
    Dim i As Integer
    Dim RzName As String
    Dim Refname As String
    Dim Rzcount As Integer
    Dim Refcount As Integer
    '
    '
    '
    If Not IsNothing(Rezsozpt) Then
      Rzcount = Rezsozpt.Rezepte.RezCount
      For k = Rzcount - 1 To 0 Step -1
        RzName = Rezsozpt.Rezepte.RezKey(k)
        For i = 0 To NotDelRez.Count - 1
          If RzName = NotDelRez(i) Then
            Exit For
          End If
        Next
        If i = NotDelRez.Count Then
          Rezsozpt.Rezepte.RemoveRez(RzName)
        End If
      Next k
    End If
    '
    '
    For l = 0 To 1
      If Not IsNothing(GrpRwerte) AndAlso Not IsNothing(GrpRwerte(l)) Then
        Refcount = GrpRwerte(l).Count
        For k = Refcount - 1 To 0 Step -1
          Refname = GrpRwerte(l).RwKey(k)
          For i = 0 To NotDelRez.Count - 1
            If Refname = NotDelRef(i) Then
              Exit For
            End If
          Next
          If i = NotDelRef.Count Then
            GrpRwerte(l).Remove(Refname)
          End If
        Next k
      End If
    Next l

  End Sub





  Sub KorrekturRezept(ByVal KFIT As Integer, ByRef Winkel As AngGeos, ByRef KeyMenge As String, ByRef KeyLet As String, ByRef Rezsozpt As RecipesGrp, ByRef GrpRwerte As RefValuesGrp, ByRef Ier As Integer)
    Dim i As Integer
    Dim IPRG As Integer
    Dim Ummng As Single
    Dim GGEN As Single
    Dim Kfitt As Integer
    Dim FarbOld As Colorants

    'KFIT MOD 8        1 Farbangleich Schittkowski
    '                  2 R-Angleich Schittkowski
    '                  3 Farbangleich + R-Angleich Schittkowski
    '                  4 Farbangleich Levenberg-Marquard (Hansen)
    '                  5 R-Angleich Levenberg-Marquard (Hansen)
    '                  6 Farbangleich + R-Angleich Levenberg-Marquard (Hansen)
    'FIX(KFIT/8) MOD 2 vor Farbangleich immer R-Angleich durchführen
    'FIX(KFIT/16) MOD 2 Grunddaten werden mit Hilfe der Nachstellung und der Hilfskorrekturen korrigiert
    '
    '
    '
    'Remissionswerte und Rezepte umspeichern
    '
    '
    IPRG = 6
    '
    FarbOld = Rezsozpt.Farben.clone
    '
    '
    If Fix(KFIT / 16) Mod 2 = 1 Then
      Call KorrekturGrund(1, KeyMenge, Rezsozpt, GrpRwerte, Ier)
    End If
    If Ier > 0 Then
      Exit Sub
    End If
    '
    '
    Call KorrekturStart(IPRG, Winkel, KeyMenge, Rezsozpt, NF, ID, RezAlt, GrpRwerte, Ier)
    If Ier > 0 Then
      Exit Sub
    End If
    '
    'Limitierungen für Farb-/Bindemittelmengen
    '
    ReDim KorMng(NF - 1)
    ReDim HlfMng(NF - 1)
    ReDim LK(NF - 1)
    ReDim Proz(NF - 1)
    ReDim Prob(NF - 1)
    ReDim Spz(NF - 1)
    For i = 0 To NF - 1
      KorMng(i) = RezAlt(i)
      LK(i) = i + 1
    Next

    '

    '
    '
    Call GetProzProbSpz(KeyMenge, Rezsozpt, NF, Proz, Prob, Spz, Ier)
    Ummng = Rezsozpt.Rezepte(KeyMenge).UmMng
    
    Call GetRezLimit("KOR", Ummng, Rezsozpt, NF, ID, HlfMng, KorMng, Proz, Prob, Spz, Ier)

    If Ier <> 0 Then
      Exit Sub
    End If
    '
    '
    For i = 0 To 1
      DICKE(i) = Rezsozpt.Rezepte(KeyLet).Dicke(i)
    Next i
    '
    '
    '
    '
    'Korrektur berechnen
    '
    Kfitt = Fix(KFIT Mod 8)
    If Kfitt <> 2 And (Fix(KFIT / 8) Mod 2) = 1 Then
      Call REZKOR(2, NF, KorMng(0), DICKE(0), GGEN, MenueParam.Messg.RefOne.R(0), FEHL)
      Ier = FEHL.Ifeh
      If FehlDLL(FEHL, BitInt(28, 29, MenueParam.User.Writ)) Then
        If Ier = 0 Then Ier = FEHL.Iwarn
        Exit Sub
      End If
    End If
    If Kfitt = 3 Then
      Call REZKOR(Kfitt, NF, KorMng(0), DICKE(0), GGEN, MenueParam.Messg.RefGew.R(0), FEHL)
    Else
      Call REZKOR(Kfitt, NF, KorMng(0), DICKE(0), GGEN, MenueParam.Messg.RefOne.R(0), FEHL)
    End If
    Ier = FEHL.Ifeh
    If FehlDLL(FEHL, BitInt(28, 29, MenueParam.User.Writ)) Then
      If Ier = 0 Then Ier = FEHL.Iwarn
      Exit Sub
    End If
    '
    If MenueParam.Misch.Igx = 6 Then
      MenueParam.Misch.Gge = GGEN
    End If
    '
    '
    '
    '
    '
    '
    'Rezept umspeichern
    '
    '
    '
    Call LetMengen(KeyMenge, KeyLet, Rezsozpt, Rezsozpt.Rezepte(KeyMenge).Nr, _
    Rezsozpt.Rezepte(KeyLet).Name, Rezsozpt.Rezepte(KeyLet).Bem, NF, LK, KorMng, DICKE, Ier)
    Call umr.CalcBamng(KeyLet, Rezsozpt, Ier)
    '
    '
    '
    '
    '
    '
    Call RezEnde("KOR", Ier)
    '
    '
    '
    Rezsozpt.Farben = FarbOld

    OptGesamt = New OpticalData

    Call MischKorrektur(KFIT, Winkel, KeyMenge, KeyLet, Rezsozpt, GrpRwerte, OptGesamt, Ier)
    OptGesamt.dispose()
    Erase RUNT
    Erase RNAC
    Erase RVOR
    Erase ID
    Erase RezAlt
    Erase LK
    Erase Proz
    Erase Prob
    Erase Spz
    Erase KorMng
    Erase HlfMng

  End Sub


  Sub MischKorrektur(ByRef Iprn As Integer, ByRef Winkel As AngGeos, ByRef KeyMenge As String, ByRef KeyLet As String, ByRef Rezsozpt As RecipesGrp, ByRef GrpRwerte As RefValuesGrp, ByRef OptGesamt As OpticalData, ByRef Ier As Integer)
    Dim k As Integer
    Dim kw As Integer
    Dim i As Integer
    Dim NPS As Integer
    Dim RezZuw() As Single
    Dim RezMzu() As Single
    Dim IPRG As Integer
    Dim INF As Integer
    Dim FarbOld As Colorants

    '
    '
    '
    'KFIT MOD 8   1 Farbangleich Schittkowski
    '             2 R-Angleich Schittkowski
    '             3 Farbangleich + R-Angleich Schittkowski
    '             4 Farbangleich Levenberg-Marquard (Hansen)
    '             5 R-Angleich Levenberg-Marquard (Hansen)
    '             6 Farbangleich + R-Angleich Levenberg-Marquard (Hansen)
    'KFIT>=8      vor Farbangleich immer R-Angleich durchführen
    'KFIT>=16     Grunddaten werden mit Hilfe der Nachstellung und der Hilfskorrekturen korrigiert
    '
    '
    '
    'Remissionswerte und Rezepte umspeichern
    '
    '
    Ier = 0
    IPRG = 4
    '
    '
    '
    '
    FarbOld = Rezsozpt.Farben.clone
    If Fix(Iprn / 16) Mod 2 = 1 Then
      Call KorrekturGrund(1, KeyMenge, Rezsozpt, GrpRwerte, Ier)
    End If
    If Ier > 0 Then
      Exit Sub
    End If
    '
    Call KorrekturStart(IPRG, Winkel, KeyMenge, Rezsozpt, NF, ID, RezAlt, GrpRwerte, Ier)

    '

    '
    '
    '
    'Rezept umspeichern
    '
    '
    '
    ReDim KorMng(NF - 1)
    For i = 0 To NF - 1
      For k = 0 To Rezsozpt.Rezepte(KeyLet).KF - 1
        If Rezsozpt.Rezepte(KeyLet)(k).ID = Rezsozpt.Rezepte(KeyMenge)(i).ID Then
          Exit For
        End If
      Next k
      If k = Rezsozpt.Rezepte(KeyLet).KF Then
        Rezsozpt.Rezepte(KeyLet).AddFaNr(KeyRe(i), New ColorAmount)
        Rezsozpt.Rezepte(KeyLet)(KeyRe(i)).ID = Rezsozpt.Rezepte(KeyMenge)(KeyRe(i)).ID
        Rezsozpt.Rezepte(KeyLet)(KeyRe(i)).Prob = Rezsozpt.Rezepte(KeyMenge)(KeyRe(i)).Prob
        Rezsozpt.Rezepte(KeyLet)(KeyRe(i)).Proz = Rezsozpt.Rezepte(KeyMenge)(KeyRe(i)).Proz
        Rezsozpt.Rezepte(KeyLet)(KeyRe(i)).BaAmng = 0.0
        Rezsozpt.Rezepte(KeyLet)(KeyRe(i)).FaAmng = 0.0
      End If
    Next i
    For i = 0 To NF - 1
      If Rezsozpt.Rezepte(KeyLet)(KeyRe(i)).ID <> Rezsozpt.Rezepte(KeyMenge)(KeyRe(i)).ID Then
        MsgBox(Texxt(3102))
        Exit Sub
      End If
    Next
    Call GetFamng(KeyLet, Rezsozpt, NF, ID, KorMng, Ier)
    For i = 0 To 1
      DICKE(i) = Rezsozpt.Rezepte(KeyLet).Dicke(i)
    Next i

    '
    '
    NPS = NPNPS(MenueParam.Messg.CDE, MenueParam.Misch.Npx, 1)
    NWE = Winkel.Wsol.Nwe
    KM = Winkel.Km
    ReDim RBER(1, KM - 1, NWE - 1)
    ReDim Gruges(0, NPS - 1, KM - 1, NWE - 1)
    ReDim CSTges(0, KM - 1)

    ReDim Proz(NF - 1)
    ReDim Prob(NF - 1)
    ReDim Spz(NF - 1)
    ReDim Ichf(NF - 1)
    ReDim Eff(NF - 1)
    ReDim LK(NF - 1)
    For i = 0 To NF - 1
      LK(i) = i + 1
    Next
    '
    '
    '
    '

    Ivol = Rezsozpt.IVOL
    Inf = Rezsozpt.INF
    Sploe = Rezsozpt.SpLoe
    DosMin = MenueParam.Misch.MinDos
    '
    '
    '
    '
    '
    If DosMin > 0.0# Then
      Call GetProzProbSpz(KeyLet, Rezsozpt, NF, Proz, Prob, Spz, Ier)
      Call GetIchfEff(Rezsozpt.Farben, NF, ID, Ichf, Eff, Ier)
      Call REZANGL(NF, Ivol, Inf, Sploe, LK(0), KorMng(0), _
                   Proz(0), Prob(0), Eff(0), Spz(0), Ichf(0), DosMin, FEHL)
      Ier = FEHL.Ifeh
      If FehlDLL(FEHL, BitInt(28, 29, MenueParam.User.Writ)) Then
        If Ier = 0 Then Ier = FEHL.Iwarn
        Exit Sub
      End If
      '
      '
    End If
    Call LetMengen(KeyLet, KeyLet, Rezsozpt, Rezsozpt.Rezepte(KeyLet).Nr, _
    Rezsozpt.Rezepte(KeyLet).Name, Rezsozpt.Rezepte(KeyLet).Bem, NF, LK, KorMng, DICKE, Ier)
    Call umr.CalcBamng(KeyLet, Rezsozpt, Ier)

    'Reflexionswerte RBER aus Mengen KorMng berechnen
    '

    Call MSHKOR(0, NWE, KM, RBER(0, 0, 0), _
    NF, LK(0), KorMng(0), DICKE(0), _
    NST, CSTges(0, 0), Gruges(0, 0, 0, 0), SorKrit(0, 0), FEHL)
    Ier = FEHL.Ifeh
    If FehlDLL(FEHL, BitInt(28, 29, MenueParam.User.Writ)) Then
      If Ier = 0 Then Ier = FEHL.Iwarn
      Exit Sub
    End If
    '

    '
    '
    'Ende Rechnung für DLL-Programme
    '
    '
    '
    Call RezEnde("KOR", Ier)
    '
    '
    Rezsozpt.Farben = FarbOld
    '
    '
    '
    '
    '
    '
    '
    '
    'Berechnete Reflexionswerte RBER umspeichern
    '
    '
    If NUV > 0 Then
      Call LetCalcRwert(Winkel, GrpRwerte(0)("V"), GrpRwerte(0)("R"), _
      GrpRwerte(0)("V").Nr, False, True, "  ", Texxt(860), GrpRwerte(0)("V").Bem, 0, RBER, Ier)
    Else
      Ier = 2960
      Exit Sub
    End If
    If NUV > 1 Then
      Call LetCalcRwert(Winkel, GrpRwerte(1)("V"), GrpRwerte(1)("R"), _
      GrpRwerte(1)("V").Nr, False, True, "  ", Texxt(860) & " (2)", GrpRwerte(1)("V").Bem, 1, RBER, Ier)
    ElseIf NUN > 1 Then
      Call LetCalcRwert(Winkel, GrpRwerte(1)("N"), GrpRwerte(1)("R"), _
      GrpRwerte(1)("N").Nr, False, True, "  ", Texxt(860) & " (2)", GrpRwerte(1)("N").Bem, 1, RBER, Ier)
    ElseIf NUU > 1 Then
      Call LetCalcRwert(Winkel, GrpRwerte(1).RefUnt, GrpRwerte(1)("R"), _
      GrpRwerte(1).RefUnt.Nr, False, True, "  ", Texxt(860) & " (2)", "  ", 1, RBER, Ier)
    End If
    '
    '
    '
    '
    'Sortierkriterien umspeichern
    '
    '
    '
    '
    Call GetSorkrit(Rezsozpt.Rezepte(KeyLet), SorKrit)

    '
    '
    '
    'Grunddaten Umspeichern
    '
    '
    '
    '
    'Optische Konstanten (Summe) übernehmen
    '
    '
    OptGesamt.Grund.clear()
    For k = 0 To NPS - 1
      OptGesamt.Grund.Add(KeyRe(k), New CurvesRef)
      For kw = 0 To Winkel.Km - 1
        OptGesamt.Grund(k).Add(Winkel(kw).Chrm, New CurveRef(Winkel.Wsol.Nwe))
      Next kw
    Next k
    OptGesamt.Nst = NST
    For i = 0 To NST - 1
      OptGesamt.Cst(i + 1) = CSTges(i, 0)
    Next i
    Call LetGrundDat(0, Winkel, NPS, Gruges, OptGesamt)
    '
    '
    '
    'Berechnung der Zuwaage für reine Mengen
    '
    '
    If Rezsozpt.Rezepte.ContainsKey("ZUW") Then
      '
      'Kennung für Batches bzw. Pasten (Mengen gesamt)
      '
      For i = 0 To Rezsozpt.Rezepte(KeyMenge).KF - 1
        KorMng(i) = Rezsozpt.Rezepte(KeyLet)(i).FaAmng
        RezAlt(i) = Rezsozpt.Rezepte(KeyMenge)(i).FaAmng
      Next
      
      '
      Rezsozpt.Rezepte("ZUW").clear()      '
      '
      ReDim RezZuw(NF - 1)
      ReDim RezMzu(NF - 1)
      '
      '
      Call RCHZUW(NF, KorMng(0), RezAlt(0), RezZuw(0), FEHL)
      '
      '
      Ier = FEHL.Ifeh
      'If FehlDLL(FEHL, BitInt(28, 29, Menueparam.User.Writ)) Then
      If FehlDLL(FEHL, 0) Then
        If Ier = 0 Then Ier = FEHL.Iwarn
        Exit Sub
      End If
      Ier = 0


      For i = 0 To NF - 1
        If Abs(RezZuw(i) + 999.99) <= 0.001 Then
          RezMzu(i) = KorMng(i)
          RezZuw(i) = Single.NaN
        Else
          RezMzu(i) = RezAlt(i) + RezZuw(i)
        End If
      Next i






      '
      Ier = 0
      Call LetMengen(KeyLet, "ZUW", Rezsozpt, Rezsozpt.Rezepte(KeyLet).Nr, Rezsozpt.Rezepte(KeyLet).Name, Rezsozpt.Rezepte(KeyLet).Bem, _
      NF, LK, RezZuw, DICKE, Ier)
      Call umr.CalcBamng("ZUW", Rezsozpt, Ier)
      '
      If Rezsozpt.Rezepte("ZUW").KF > 0 Then
        Call GetSorkrit(Rezsozpt.Rezepte("ZUW"), SorKrit)
      End If

      '
      '
      If Rezsozpt.Rezepte.ContainsKey("MZU") Then
        Rezsozpt.Rezepte("MZU").clear()      '
        Call LetMengen(KeyLet, "MZU", Rezsozpt, Rezsozpt.Rezepte(KeyLet).Nr, Rezsozpt.Rezepte(KeyLet).Name, Rezsozpt.Rezepte(KeyLet).Bem, _
        NF, LK, RezMzu, DICKE, Ier)
        Call umr.CalcBamng("MZU", Rezsozpt, Ier)
        If Rezsozpt.Rezepte("MZU").KF > 0 Then
          Call GetSorkrit(Rezsozpt.Rezepte("MZU"), SorKrit)
        End If
      End If
      '
      
      '
    End If

    '
    '
    '
    '
    'Normierung KOR und MNG anpassen, fall entsprechendes Bit gesetzt
    '
    '
    '
    '
    '
    'If Menueparam.Misch.Igx <> 3 And Menueparam.Misch.Igx <> 5 Then
    ' If BitInt(0, 0, MenueParam.User.Sonst) = 1 And Rezsozpt.INO = 1 Then
    ' NorNeu = umr.MngINF(KeyMenge, Rezsozpt, Ier)
    ' Call umr.MNGBamngNorm(KeyLet, Rezsozpt, NorNeu, Ier)
    ' umr.CalcFamng(KeyLet, Rezsozpt, Ier)
    ' End If
    ' End If
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
    Erase KorMng
    Erase RezAlt
    Erase RezMng
    Erase RezZuw
    Erase RezMzu
    Erase LK
    Erase RBER
    Erase Gruges
    Erase Proz
    Erase Prob
    Erase Spz
    Erase Ichf
    Erase Eff
    Erase ZwiMng
    Erase ID
  End Sub
  '
  '

  Private Sub KorrekturStart(ByRef IPRG As Integer, ByRef Winkel As AngGeos, ByRef KeyMenge As String, ByRef Rezsozpt As RecipesGrp, ByRef NF As Integer, ByRef ID() As Integer, ByRef RezAlt() As Single, ByRef GrpRwerte As RefValuesGrp, ByRef Ier As Integer)
    Dim IH As Integer
    Dim i As Integer
    Dim k As Integer
    Dim Keyhlf As String
    Dim FaNr As String
    Dim Mngrei As Single
    Dim KF As Integer
    Dim IDH() As Integer
    Dim RezHlf() As Single
    '
    '
    'Ihlf bestimmen
    '
    For i = 0 To 1
      If GrpRwerte(1)("N").ID = -1 Then
        GrpRwerte(1)("N").IVoNa = False
      End If
    Next i
    '
    '
    'NF bestimmen
    '
    '
    '
    Ihlf = 0
    For k = 0 To Rezsozpt.Rezepte.RezCount - 1
      If IsNumeric(Rezsozpt.Rezepte.RezKey(k)) Then
        Ihlf = Ihlf + 1
      End If
    Next k
    '
    For IH = 1 To Ihlf
      '
      '
      '
      '
      '
      'Rezepte für IH-te Hilfskorrektur
      '
      '
      Keyhlf = KeyRe(IH)
      '
      '
      '====================================================================================
      'zu beachten sind u.U. die unterschiedliche Art und Reihenfolge der Farbmittel-ID's
      '====================================================================================
      '
      '
      KF = Rezsozpt.Rezepte(Keyhlf).KF
      ReDim IDH(KF - 1)
      ReDim ZwiMng(KF - 1)
      Call GetFamng(Keyhlf, Rezsozpt, KF, IDH, ZwiMng, Ier)
      NF = Rezsozpt.Rezepte(KeyMenge).KF
      For i = 0 To KF - 1
        For k = 0 To NF - 1
          If IDH(i) = Rezsozpt.Rezepte(KeyMenge)(k).ID Then
            Exit For
          End If
        Next k
        If k >= NF Then
          FaNr = KeyRe(Rezsozpt.Rezepte(KeyMenge).KF)
          Rezsozpt.Rezepte(KeyMenge).AddFaNr(FaNr, New ColorAmount)
          Rezsozpt.Rezepte(KeyMenge)(FaNr).ID = IDH(i)
          Rezsozpt.Rezepte(KeyMenge)(FaNr).BaAmng = 0.0
          Rezsozpt.Rezepte(KeyMenge)(FaNr).FaAmng = 0.0
          Rezsozpt.Rezepte(KeyMenge)(FaNr).Proz = Rezsozpt.Rezepte(Keyhlf)(i).Proz
          Rezsozpt.Rezepte(KeyMenge)(FaNr).Prob = Rezsozpt.Rezepte(Keyhlf)(i).Prob
        End If
      Next i
    Next IH
    NF = Rezsozpt.Rezepte(KeyMenge).KF
    For i = 0 To NF - 1
      If Not Rezsozpt.Farben.ContainsFarb(KeyName(Rezsozpt.Rezepte(KeyMenge)(i).ID)) Then
        MsgBox(KeyName(Rezsozpt.Rezepte(KeyMenge)(i).ID) & ": " & Texxt(2984))
        Ier = 2984
        Exit Sub
      End If
    Next
    ReDim RezAlt(NF - 1)
    ReDim RezHlf(NF - 1)
    Call RezBeginn("KOR", Winkel, NF, Ihlf, KeyMenge, Rezsozpt, Ier)
    If Ier <> 0 Then
      Exit Sub
    End If

    '
    'Normlichtarten,Winkel,GK-Werte
    '
    '
    '
    '
    '
    '
    '
    '
    Call GetWinkNormGk("RZP", MenueParam.Menue.Kwj, Winkel, Ier)
    If Ier <> 0 Then Exit Sub
    '
    '
    '
    'Call REZDRU(Fehl)

    '
    '
    '
    '
    '
    'Parameter für Rezeptberechnung
    '
    '
    '
    '
    '
    '
    '
    Call GetRezeptMenu("KOR", Ier)
    If Ier > 0 Then Exit Sub

    '
    '
    '
    '
    'Rezept umspeichern
    '
    '
    '
    '
    'Call REZDRU(FEHL)
    NF = Rezsozpt.Rezepte(KeyMenge).KF
    ReDim ID(NF - 1)
    ReDim ZwiMng(NF - 1)
    Call GetFamng(KeyMenge, Rezsozpt, NF, ID, ZwiMng, Ier)
    For k = 0 To NF - 1
      RezAlt(k) = ZwiMng(k)
    Next k

    For i = 0 To 1
      DICKE(i) = Rezsozpt.Rezepte(KeyMenge).Dicke(i)
    Next i
    '
    '
    '
    '
    '
    '
    'Grunddaten
    '
    '
    'Bei Vorhandensein von Hilfskorrekturen wird Rezept KeyMenge um die u.U. zusätzlich vorhandenen Farb-/Bindemittel
    '(ID und Famng=0.) ergänzt)
    '
    '
    '
    Mngrei = umr.MngINR(KeyMenge, Rezsozpt, Ier)
    If Rezsozpt.INO = 0 Then
      Mngrei = Max(Mngrei, umr.MngINL(KeyMenge, Rezsozpt, Ier))
    Else
      Mngrei = Max(Mngrei, Rezsozpt.MngMax)
    End If

    '
    'Falls Grunddaten mit SUB KORREKTURGRUND neu berechnet wurden, werden die neuen Werte hier übernommen
    '
    '
  
    '
    Call GetGrundDaten("KOR", Winkel, Rezsozpt, Mngrei, NF, ID, Ier)
    If Ier <> 0 Then
      Exit Sub
    End If
    '
    '
    '
    '
    '
    '
    'Reflexionswerte für Untergründe
    '
    '
    '
    '
    NWE = Winkel.Wsol.Nwe
    KM = Winkel.Km
    ReDim RUNT(1, KM - 1, NWE - 1)
    ReDim RVOR(1, KM - 1, NWE - 1)
    ReDim RNAC(1, KM - 1, NWE - 1)
    ReDim RHLFN(1, KM - 1, NWE - 1)
    ReDim RHLFV(1, KM - 1, NWE - 1)
    '
    '
    '
    'Untergrund
    '
    '
    '
    '
    NUU = 0
    For i = 0 To GrpRwerte.Count - 1
      KWB(i) = GrpRwerte(i).RefUnt.kwb + 1
      Retr(i) = GrpRwerte(i).RefUnt.ReTr
      If GrpRwerte(i).RefUnt.ID = -1 And MenueParam.Misch.Transp And i = 0 Then
        Ier = 4050
        MsgBox(Texxt(Ier), MsgBoxStyle.OkOnly, Texxt(2000))
        Exit Sub
      End If
      If GrpRwerte(i).RefUnt.RefKurv.Count > 0 AndAlso GrpRwerte(i).RefUnt.IVoNa Then
        Call GetRwerte(Winkel, i, RUNT, GrpRwerte(i).RefUnt)
        NUU = NUU + 1
      End If
    Next i
    If IPRG = 4 And NUU = 0 Then
      NUU = 1
    End If
    '
    '
    '
    '
    'Reflexionswerte für Vorlagen
    '
    '
    '
    '
    NUV = 0
    For i = 0 To GrpRwerte.Count - 1
      If GrpRwerte(i)("V").RefKurv.Count > 0 AndAlso GrpRwerte(i)("V").IVoNa Then
        If KWB(i) - 1 <> GrpRwerte(i)("V").kwb Then
          Ier = 4077
          MsgBox(Texxt(Ier))
          Exit Sub
        End If
        If Retr(i) <> GrpRwerte(i)("V").ReTr Then
          Ier = 4148
          MsgBox(Texxt(Ier))
          Exit Sub
        End If
        Call GetRwerte(Winkel, i, RVOR, GrpRwerte(i)("V"))
        NUV = NUV + 1
      End If
    Next i
    '
    '
    '
    '
    '
    'Reflexionswerte für Nachstellung
    '
    '
    '
    '
    NUN = 0
    For i = 0 To GrpRwerte.Count - 1
      If GrpRwerte(i)("N").RefKurv.Count > 0 AndAlso GrpRwerte(i)("N").IVoNa Then
        If KWB(i) - 1 <> GrpRwerte(i)("N").kwb Then
          Ier = 4077
          MsgBox(Texxt(Ier))
          Exit Sub
        End If
        If Retr(i) <> GrpRwerte(i)("N").ReTr Then
          Ier = 4148
          MsgBox(Texxt(Ier))
          Exit Sub
        End If
        Call GetRwerte(Winkel, i, RNAC, GrpRwerte(i)("N"))
        NUN = NUN + 1
      End If
    Next i
    '
    '
    '
    '

    '
    '
    'Mengen und Reflexionskurven übernehmen
    '
    '
    '
    '
    '
    '

    Call REZMGRF(IPRG, NUU, NUV, NUN, NWE, KM, Retr(0), KWB(0), _
    RUNT(0, 0, 0), RVOR(0, 0, 0), RNAC(0, 0, 0), NF, RezAlt(0), DICKE(0), FEHL)
    Ier = FEHL.Ifeh
    If FehlDLL(FEHL, BitInt(28, 29, MenueParam.User.Writ)) Then
      If Ier = 0 Then Ier = FEHL.Iwarn
      Exit Sub
    End If
    '
    '
    '
    'Zusätzliche Farb-/Bindemittelmengen und Remissionskurven für Hilfskorrekturen
    '
    '
    '
    For IH = 1 To Ihlf
      '
      '
      '
      '
      '
      'Rezepte für IH-te Hilfskorrektur
      '
      '
      Keyhlf = KeyRe(IH)
      '
      '
      '====================================================================================
      'zu beachten sind u.U. die unterschiedliche Art und Reihenfolge der Farbmittel-ID's
      '====================================================================================
      '
      '
      KF = Rezsozpt.Rezepte(Keyhlf).KF
      ReDim IDH(KF - 1)
      ReDim ZwiMng(KF - 1)
      Call GetFamng(Keyhlf, Rezsozpt, KF, IDH, ZwiMng, Ier)
      For k = 0 To NF - 1
        RezHlf(k) = 0.0
        For i = 0 To KF - 1
          If IDH(i) = ID(k) Then
            RezHlf(k) = ZwiMng(i)
            Exit For
          End If
        Next i
      Next k
      For i = 0 To 1
        DICKE(i) = Rezsozpt.Rezepte(Keyhlf).Dicke(i)
      Next i
      '
      '
      'Reflexionswerte für IH-te Hilfskorrektur (Vorlage)
      '
      '
      '
      '
      For i = 0 To NUV - 1
        If GrpRwerte(i)("V" & Keyhlf).IVoNa Then
          If KWB(i) - 1 <> GrpRwerte(i)("V" & Keyhlf).kwb Then
            Ier = 4077
            MsgBox(Texxt(Ier))
            Exit Sub
          End If
          If Retr(i) <> GrpRwerte(i)("V" & Keyhlf).ReTr Then
            Ier = 4148
            MsgBox(Texxt(Ier))
            Exit Sub
          End If
          Call GetRwerte(Winkel, i, RHLFV, GrpRwerte(i)("V" & Keyhlf))
        End If
      Next i
      '
      '
      'Reflexionswerte für IH-te Hilfskorrektur (Nachstellung)
      '
      '
      '
      '
      For i = 0 To NUN - 1
        If GrpRwerte(i)("N" & Keyhlf).IVoNa Then
          If KWB(i) - 1 <> GrpRwerte(i)("N" & Keyhlf).kwb Then
            Ier = 4077
            MsgBox(Texxt(Ier))
            Exit Sub
          End If
          If Retr(i) <> GrpRwerte(i)("N" & Keyhlf).ReTr Then
            Ier = 4148
            MsgBox(Texxt(Ier))
            Exit Sub
          End If
          Call GetRwerte(Winkel, i, RHLFN, GrpRwerte(i)("N" & Keyhlf))
        End If
      Next i
      Call REZHILF(IH, NWE, KM, RHLFV(0, 0, 0), RHLFN(0, 0, 0), NF, RezHlf(0), DICKE(0), FEHL)
    Next IH
    '
    '
    '
    '
    'Hier können die Grunddaten mit Hilfe der Rezepte und den Reflexionswerten der Unteergründe,
    'der Nachstellung und der Hilskorrekturen korrigiert werden. REFKOR kann dann entfallen.
    '
    '
    '
    'Korrektur und Korrekturmatrix aus Remissionswerten und Mengen der Nachstellung und u.U.
    'der Hilfskorrekturen berechnen
    '
    '
    '
    Call REFKOR(NWE, KM, RNAC(0, 0, 0), FEHL)
    Ier = FEHL.Ifeh
    If FehlDLL(FEHL, BitInt(28, 29, MenueParam.User.Writ)) Then
      If Ier = 0 Then Ier = FEHL.Iwarn
      Exit Sub
    End If
    If NUU > 1 Then
      If NUN = 1 Then
        '
        'Rechnerisch ermittelte Nachstellung über schwarzem Untergrund
        '
        '
        Call LetCalcRwert(Winkel, GrpRwerte(0)("N"), GrpRwerte(1)("N"), _
        GrpRwerte(0)("N").Nr, False, 1, "  ", GrpRwerte(0)("N").Name, GrpRwerte(0)("N").Name, 1, RNAC, Ier)
        GrpRwerte(1)("N").kwb = GrpRwerte(1).RefUnt.kwb
        GrpRwerte(1)("N").ID = -1
      End If
    End If

    ''
    Erase ZwiMng
    Erase RUNT
    Erase RVOR
    Erase RNAC
    Erase RHLFN
    Erase RHLFV
    Erase IDH
    Erase RezHlf
    '
    '
  End Sub
  '
  '
  Sub SorRez(ByRef IPGM As Integer, ByRef ImaxFrb As Integer, ByRef Kzl As Integer, ByRef Nrs() As Integer, ByRef DG0 As Single, ByRef RezSozpt As RecipesGrp, ByRef ier As Integer)
    Dim Dcv As Single
    Dim Dcq As Single
    Dim Dcr As Single
    Dim Dcb As Single
    Dim Dg As Single
    Dim Iso(8) As Integer
    Dim i As Integer
    Dim j As Integer
    Dim m As Integer
    Dim m1 As Integer
    Dim kzal1 As Integer
    Dim Nrh As Integer
    Dim Imin As Integer
    Dim Kzal As Integer
    '
    '
    '     SORTIEREN DER REZEPTE
    ' IPGG
    '   0     SORTIEREN NACH METAMERIE
    '   1     SORTIEREN NACH PREIS
    '   2     SORTIEREN NACH KORRIGIERBARKEIT
    '   3     SORTIEREN NACH FARBABSTAND FUER ERSTE NORMLICHTART
    '   4     SORTIEREN NACH FARBABSTAND FUER ZWEITE NORMLICHTART
    '   5     SORTIEREN NACH SUMME UEBER ALLE FARBABSTAENDE
    '   6     SORTIEREN NACH FLAECHENDIFFERENZ DER R-KURVEN
    '   7     SORTIEREN NACH WEISS-SCHWARZ KONTRAST
    '   8     SORTIEREN NACH Farbabstand Summe über alle Winkel

    '
    '
    '     sokrit(0)=    DE erste Notmlichtart
    '     sokrit(1)=    Metamerie
    '     sokrit(2)=    Preis
    '     sokrit(3)=    Korrigierbarkeit
    '     sokrit(4)=    Kontrast De
    '     sokrit(5)=    De 1. Normlichtart
    '     sokrit(6)=    De 2. Normlichtart
    '     sokrit(7)=    De 3. Normlichtart
    '     sokrit(8)=    De 4. Normlichtart
    '     sokrit(9)=    De 5. Normlichtart
    '     sokrit(10)=   Summe De über alle Normlichtarten
    '     sokrit(11)=   Flächendifferenz (R-Kurven)
    '     sokrit(12)=   Sensibilität
    '     sokrit(13)=   FEXP(-Korrigierbarkeit)
    '     sokrit(14)=   Standardabweichung R-Werte KWI-ter Winkel
    '     sokrit(15)=   Mittelwert Farbabstand erste Lichtart über alle Winkel
    '
    'Kzal(bestimmen)
    '
    Kzal = 0
    For i = 0 To RezSozpt.Rezepte.RezCount - 1
      If IsNumeric(RezSozpt.Rezepte.RezKey(i)) Then
        Kzal = Kzal + 1
      End If
    Next
    '
    '
    '
    '
    If Kzal = 0 Then Exit Sub
    Iso(0) = 1
    Iso(1) = 2
    Iso(2) = 3
    Iso(3) = 0
    Iso(4) = 6
    Iso(5) = 10
    Iso(6) = 11
    Iso(7) = 4
    Iso(8) = 15
    '
    ReDim Nrs(Kzal - 1)
    For i = 0 To Kzal - 1
      Nrs(i) = i
    Next i
    If IPGM < 0 Or IPGM > 9 Then IPGM = 3
    Dg = Abs(DG0)
    kzal1 = Kzal - 1
    For m = 0 To kzal1
      Imin = m
      Dcv = RezSozpt.Rezepte(KeyRe(Nrs(m))).Sorkrit(0)(KeyRe(0))
      Dcr = RezSozpt.Rezepte(KeyRe(Nrs(m))).Sorkrit(0)(KeyRe(Iso(IPGM)))
      m1 = m + 1
      For i = m1 To Kzal - 1
        Dcq = RezSozpt.Rezepte(KeyRe(Nrs(i))).Sorkrit(0)(KeyRe(Iso(IPGM)))
        Dcb = RezSozpt.Rezepte(KeyRe(Nrs(i))).Sorkrit(0)(KeyRe(0))
        If Not ((Dcr <= Dcq And (Dcb > Dg Or Dcv < Dg)) Or (Dcb > Dg And Dcv < Dg)) Then
          If Not (Dcv < Dg Or Dcb > Dg) Then
            If Dcb <= Dg Then Dcv = Dcb
          End If
          Dcr = Dcq
          Imin = i
          Nrh = Nrs(i)
        End If
      Next i
      '
      If m <> Imin Then
        Nrs(Imin) = Nrs(m)
        Nrs(m) = Nrh
      End If
    Next m
    Kzl = Kzal
    If DG0 < 0 Then
      kzal1 = Kzal
      For m = 0 To kzal1 - 1
        Dcv = RezSozpt.Rezepte(KeyRe(Nrs(m))).Sorkrit(0)(KeyRe(0))
        If Dcv > Dg Then
          Kzl = m
          Exit Sub
        End If
      Next m
    End If
    m1 = Kzl
    Kzl = 0
    For j = 0 To m - 1
      If RezSozpt.Rezepte(KeyRe(Nrs(j))).KF <= ImaxFrb Then
        Nrs(Kzl) = Nrs(j)
        Kzl = Kzl + 1
      End If
    Next j
  End Sub
  Sub Restfarbe(ByVal Iprn As Integer, ByVal KeyMenge As String, ByVal KeyCalc As String, ByVal RezSozpt As RecipesGrp, ByVal GrpRwerte As RefValuesGrp, ByRef RestFarb As Colorant, ByRef ier As Integer)
    Dim i As Integer
    Dim j As Integer
    Dim kw As Integer
    Dim GewAlt As Single
    Dim DosAlt As Single
    Dim Tdae As Single
    Dim Gew As Single
    Dim Vol As Single
    Dim GrpRwerteHilfM As RefValuesGrp
    Dim GrpRwerteHilfR As RefValuesGrp
    Dim RezSozptHilf As RecipesGrp
    Dim GesStreuAbs As Colorants
    Dim quali As QualKontrolle
    Dim RB() As Single
    Dim RP() As Single
    Dim DE As Single
    Dim DL As Single
    Dim DC As Single
    Dim DH As Single
    Dim DA As Single
    Dim DB As Single
    Dim KeyID As String
    Dim Fiy() As Integer
    Dim Irest As Integer
    Dim Kfit As Integer
    Dim Keylet As String
    Dim IvaID As Integer
    Dim Ifix As Integer
    '
    '
    '
    '
    'Anzahl,Reihenfolge und Art der Winkel (Messgeometrien) von Messgerät und Benutzer müssen übereinstimmem
    'da die berechneten Grunddaten für die Restfarbe abgespeichert werden
    '
    '
    '
    '
    'If Not WinUSeqMe(MenueParam.User.Winkel, MenueParam.Messg.Winkel) Then
    ' ier = 3605
    ' MsgBox(Texxt(ier))
    ' Exit Sub
    ' End If
    '
    ier = 0
    Kfit = Iprn Mod 4
    Irest = Iprn / 4 + 1
    Tdae = MenueParam.Menue.Delta
    '
    '
    ReDim Fiy(RezSozpt.Farben.FarbCount - 1)
    For i = 0 To RezSozpt.Farben.FarbCount - 1
      Fiy(i) = RezSozpt.Farben(i).OptData.Fest
    Next
    GrpRwerteHilfM = New RefValuesGrp
    GrpRwerteHilfR = New RefValuesGrp
    RezSozptHilf = New RecipesGrp
    RestFarb.OptData = New OpticalData
    quali = New QualKontrolle


    DosAlt = MenueParam.Misch.MinDos
    MenueParam.Misch.MinDos = 0.0
    '
    '
    '
    '
    '
    'Falls irest=2 und nur ein variables Farbmittel vorhanden, werden dessen Grunddaten als Basis für Restfarbenrechnung verwendet
    '
    IvaID = -1
    Ifix = 0
    If Irest = 2 Then
      For i = 0 To RezSozpt.Rezepte(KeyMenge).KF - 1
        KeyID = KeyName(RezSozpt.Rezepte(KeyMenge)(i).ID)
        If RezSozpt.Farben(KeyID).OP <> "=" Then
          IvaID = RezSozpt.Rezepte(KeyMenge)(i).ID
        Else
          Ifix = Ifix + 1
        End If
      Next
      If IvaID = -1 Then
        MsgBox(Texxt(4047))
        ier = 4047
        Exit Sub
      End If
    End If
    '

    If RezSozpt.Rezepte(KeyMenge).KF = Ifix + 1 Then
      '
      'nur ein variables farbmittel vorhanden
      '
      '
      Keylet = KeyMenge
      RestFarb.OptData = RezSozpt.Farben(KeyName(IvaID)).OptData.clone
    Else
      Keylet = KeyCalc
      '
      '
      '
      '
      'Basisrezept berechnen
      '
      '
      '
      '
      Call BasisRezept(Kfit + 8, MenueParam.Messg.Winkel, KeyMenge, Keylet, RezSozpt, GrpRwerte, ier)
      '
      '

      If ier <> 0 Then Exit Sub
      '
      '
      '
      'Optische Konstanten für Mischung berechnen
      '
      '
      '
      '
      Call MischRezept(Irest, MenueParam.Messg.Winkel, Keylet, RezSozpt, GrpRwerte, RestFarb.OptData, ier)
      If ier <> 0 Then Exit Sub
      '
    End If
    '
    '
    '
    '
    '
    'Umspeichern + spez. Gewicht berechnen
    '
    '
    RezSozptHilf.Farben.clear()
    RezSozptHilf.Rezepte.clear()
    RezSozptHilf.Rezepte.AddRez(KeyRe(0), New Recipe) '
    Gew = 0.0
    Vol = 0.0
    For i = 0 To RezSozpt.Rezepte(Keylet).KF - 1
      KeyID = KeyName(RezSozpt.Rezepte(Keylet)(i).ID)
      If Irest = 2 And RezSozpt.Farben(KeyID).OP = "=" Then
        '
        'Iprn=2
        'Restfarbenberechnung aus Mischungen
        'Alle Farbmittel mit OP = "=" werden festgehalten;
        'Die Grunddaten werden nur für die verbleibenden Farb-/Bindemittel berechnet
        'Damit können die Grunddaten der Restfarben auch aus Mischungen von z.B. Restfarbe und Weißpigment
        'ermittelt werden. Das Weißpigment hat dann OP="="
        '
        '
        'Festgehaltene Farbmittel nach RezsozptHilf übernehmen
        '
        j = RezSozptHilf.Farben.FarbCount
        RezSozptHilf.Farben.AddFarb(KeyID, RezSozpt.Farben(KeyID))
        RezSozptHilf.Farben(KeyID).OptData.Fest = 1
        RezSozptHilf.Rezepte(0).AddFaNr(KeyRe(j), RezSozpt.Rezepte(Keylet)(i))
        '
        '
        '
        '
        '
      Else
        Gew = Gew + RezSozpt.Rezepte(Keylet)(i).FaAmng
        Vol = Vol + RezSozpt.Rezepte(Keylet)(i).FaAmng / RezSozpt.Farben(KeyID).Spz
      End If
    Next
    '
    '
    'Restfarbe aufbauen
    '
    '
    RestFarb.Spz = Gew / Vol
    '
    'Kennung für Restfarbe
    '
    '
    RestFarb.Ichf = 0   ' zunächst für Farbmittel
    RestFarb.Ibas = 0
    For i = 0 To 0
      RestFarb.Pre(i) = 0
    Next i
    For i = 0 To 0
      RestFarb.Prf(i) = 100
    Next i
    For i = 0 To 0
      RestFarb.Prb(i) = 100
    Next i
    RestFarb.Fst = 100.0#
    RestFarb.Bel = 1.0#
    RestFarb.Eff = 1
    RestFarb.ID = -1
    RestFarb.Bem = ""
    RestFarb.Aname = ""
    RestFarb.Form = "####.000"
    RestFarb.GlzGrd = 0.0
    RestFarb.GlzGrdID = -1

    '
    'Produktnummer
    '
    RestFarb.PrNr = ""
    RestFarb.Smenge = 0.001
    j = RezSozptHilf.Farben.FarbCount
    '
    'Restfarbe nach RezsozptHilf übernehmen
    '
    RezSozptHilf.Farben.AddFarb(KeyName(RestFarb.ID), RestFarb)
    RezSozptHilf.INF = RezSozpt.INF
    RezSozptHilf.INM = RezSozpt.INM
    RezSozptHilf.INO = RezSozpt.INO
    RezSozptHilf.INP = RezSozpt.INP
    RezSozptHilf.INQ = RezSozpt.INQ
    RezSozptHilf.IVOL = RezSozpt.IVOL
    '
    RezSozptHilf.Rezepte(0).AddFaNr(KeyRe(j), New ColorAmount)
    RezSozptHilf.Rezepte(0)(j).FaAmng = Gew
    RezSozptHilf.Rezepte(0)(j).BaAmng = Gew
    RezSozptHilf.Rezepte(0)(j).ID = RestFarb.ID
    RezSozptHilf.Rezepte(0)(j).Proz = 100
    RezSozptHilf.Rezepte(0)(j).Prob = 100

    '
    '
    'Umspeichern
    '
    '
    'Gemessene Werte
    '
    GrpRwerteHilfM.Add(GrpRwerte.RwArt(0), New RefValues)
    GrpRwerteHilfM.Add(GrpRwerte.RwArt(1), New RefValues)
    '
    '
    'berechnete Werte
    '
    '
    GrpRwerteHilfR.Add(GrpRwerte.RwArt(0), New RefValues)
    GrpRwerteHilfR.Add(GrpRwerte.RwArt(1), New RefValues)

    '
    For i = 0 To 1
      If Not IsNothing(GrpRwerte(i)) Then
        If Not IsNothing(GrpRwerte(i).RefUnt) Then
          GrpRwerteHilfM(i).RefUnt = GrpRwerte(i).RefUnt
          RezSozptHilf.Rezepte.kwb(i) = GrpRwerte(i).RefUnt.kwb
        End If
        If GrpRwerte(i).ContainsKey("V") AndAlso Not IsNothing(GrpRwerte(i)("V")) AndAlso GrpRwerte(i)("V").IVoNa Then
          GrpRwerteHilfM(i).Add(KeyRe(0), GrpRwerte(i)("V"))
          GrpRwerteHilfM(i)(KeyRe(0)).Nr = i
          RezSozptHilf.Rezepte(0).Dicke(i) = RezSozpt.Rezepte(Keylet).Dicke(i)
        End If
      End If
    Next i

    For j = 0 To 9
      For i = 0 To 5
        MenueParam.Misch.GrDae(i, j) = MenueParam.Misch.GrDae(i, j) + Tdae
      Next i
    Next j
    GewAlt = MenueParam.Misch.Gewr
    MenueParam.Misch.Gewr = 100

    '
    '
    'Grunddaten für Mischung neu berechnen, mit möglichst kleiner Abweichung zu den in Optgesamt stehenden Werten
    '
    '
    RestFarb.OptData.GKID = MenueParam.Misch.GKwrtID
    RestFarb.OptData.Npx = MenueParam.Misch.Npx
    RestFarb.OptData.Fest = 0
    'MsgBox(RezSozptHilf.Rezepte(0).Name)
    'MsgBox(RezSozptHilf.Farben(0).OptData.Grund)
    '
    RezSozptHilf.FarbAux = RezSozptHilf.Farben.clone
    Call GrundDaten(9, MenueParam.Messg.Winkel, RezSozptHilf, GrpRwerteHilfM, GrpRwerteHilfR, GesStreuAbs, ier)
    If ier <> 0 Then Exit Sub
    '
    'Test für ersten Winkel/Messgeometrie ==>(Keyre(0))
    '

    ReDim RB(MenueParam.Messg.Winkel.Wsol.Nwe - 1)
    ReDim RP(MenueParam.Messg.Winkel.Wsol.Nwe - 1)
    For kw = 0 To MenueParam.Messg.Winkel.Km - 1
      For i = 0 To MenueParam.Messg.Winkel.Wsol.Nwe - 1
        RB(i) = GrpRwerteHilfM(0)(KeyRe(0)).RefKurv(kw).R(i)
        RP(i) = GrpRwerteHilfR(0)(KeyRe(0)).RefKurv(kw).R(i)
      Next i
      '
      '
      Call quali.FarbDifferenzRef(0, kw, 0, MenueParam.Messg.Winkel, RB, RP, DE, DL, DC, DH, DA, DB, ier)
      If DE > MenueParam.Misch.DeGut Then
        MsgBox(Texxt(3977))
      End If
    Next kw
    '
    '

    For j = 0 To 9
      For i = 0 To 5
        MenueParam.Misch.GrDae(i, j) = MenueParam.Misch.GrDae(i, j) - Tdae
      Next i
    Next j
    RestFarb.Ichf = 6 'Kennung für Restfarbe
    MenueParam.Misch.Gewr = GewAlt
    MenueParam.Misch.MinDos = DosAlt
    For i = 0 To RezSozpt.Farben.FarbCount - 1
      RezSozpt.Farben(i).OptData.Fest = Fix(i)
    Next

    GrpRwerteHilfM.dispose()
    GrpRwerteHilfR.dispose()
    RezSozptHilf.dispose()

  End Sub


  Sub KorrekturGrund(ByRef KFIT As Integer, _
  ByRef Keymenge As String, ByRef Rezsozpt As RecipesGrp, _
  ByRef GrpRwerte As RefValuesGrp, ByRef Ier As Integer)
    Dim i As Integer
    Dim j As Integer
    Dim IHLF As Integer
    Dim Tdae As Single
    Dim GewAlt As Single
    Dim GrpRwerteHilfM As RefValuesGrp
    Dim GrpRwerteHilfR As RefValuesGrp
    Dim RezSozptHilf As RecipesGrp
    Dim GesStreuAbs As Colorants
    Dim RzKey As String
    Dim Ifest() As Short
    Ier = 0
    Tdae = MenueParam.Menue.Delta
    '
    'If Not WinUSeqMe(MenueParam.User.Winkel, MenueParam.Messg.Winkel) Then
    'Ier = 3605
    'MsgBox(Texxt(Ier))
    'Exit Sub
    'End If
    '
    '
    '
    '
    '
    '
    GrpRwerteHilfM = New RefValuesGrp
    GrpRwerteHilfR = New RefValuesGrp
    RezSozptHilf = New RecipesGrp
    RezSozptHilf.Farben = Rezsozpt.Farben
    ReDim Ifest(RezSozptHilf.Farben.FarbCount - 1)
    For i = 0 To Ifest.Count - 1
      Ifest(i) = RezSozptHilf.Farben(i).OptData.Fest
      RezSozptHilf.Farben(i).OptData.Fest = 0
    Next
    '
    '
    '
    '
    '
    '
    'Umspeichern
    '
    '
    'Gemessene Werte
    '
    GrpRwerteHilfM.Add(GrpRwerte.RwArt(0), New RefValues)
    GrpRwerteHilfM.Add(GrpRwerte.RwArt(1), New RefValues)
    '
    '
    'berechnete Werte
    '
    '
    GrpRwerteHilfR.Add(GrpRwerte.RwArt(0), New RefValues)
    GrpRwerteHilfR.Add(GrpRwerte.RwArt(1), New RefValues)
    '
    'Untergründe übernehmen
    '
    '
    For i = 0 To 1
      GrpRwerteHilfM(i).RefUnt = GrpRwerte(i).RefUnt
    Next i
    '
    '
    'Rezepte und R-Werte übernehmen
    '
    '
    '
    '
    'Nachstellung
    '
    '
    Rezsozpt.Rezepte(Keymenge).Nr = 0
    RezSozptHilf.Rezepte.AddRez(KeyRe(0), Rezsozpt.Rezepte(Keymenge))
    If GrpRwerte(0)("N").IVoNa Then
      GrpRwerte(0)("N").Nr = 0
      GrpRwerteHilfM(0).Add(KeyRe(0), GrpRwerte(0)("N"))
    End If
    If GrpRwerte.Count > 1 AndAlso GrpRwerte(1).Count > 0 AndAlso GrpRwerte(1)("N").IVoNa Then
      GrpRwerte(1)("N").Nr = 0
      GrpRwerteHilfM(1).Add(KeyRe(0), GrpRwerte(1)("N"))
    End If
    '
    '
    'Anzahl Hilfkorrekturen IHLF bestimmen
    '
    '
    IHLF = 0
    For i = 0 To Rezsozpt.Rezepte.RezCount - 1
      If IsNumeric(Rezsozpt.Rezepte.RezKey(i)) Then
        IHLF = IHLF + 1
      End If
    Next
    '
    '
    '
    For i = 1 To IHLF
      '
      'Hilfskorrekturen
      '
      '
      RzKey = KeyRe(i)
      Rezsozpt.Rezepte(RzKey).Nr = i
      RezSozptHilf.Rezepte.AddRez(RzKey, Rezsozpt.Rezepte(RzKey))
      If GrpRwerte(0)("N").IVoNa Then
        GrpRwerte(0)("N").Nr = i
        GrpRwerteHilfM(0).Add(RzKey, GrpRwerte(0)("N" & RzKey))
      End If
      If GrpRwerte(1)("N").IVoNa Then
        GrpRwerte(1)("N").Nr = i
        GrpRwerteHilfM(1).Add(RzKey, GrpRwerte(1)("N" & RzKey))
      End If


    Next i

    For j = 0 To 9
      For i = 0 To 5
        MenueParam.Misch.GrDae(i, j) = MenueParam.Misch.GrDae(i, j) + Tdae
      Next i
    Next j
    GewAlt = MenueParam.Misch.Gewr
    MenueParam.Misch.Gewr = 100

    '
    '
    'Grunddaten für Mischung neu berechnen, mit möglichst kleiner Abweichung zu den in Farben.Optdata stehenden Werten
    '
    '
    Call GrundDaten(9, MenueParam.Messg.Winkel, RezSozptHilf, GrpRwerteHilfM, GrpRwerteHilfR, GesStreuAbs, Ier)
    If Ier <> 0 Then Exit Sub


    '
    '

    For j = 0 To 9
      For i = 0 To 5
        MenueParam.Misch.GrDae(i, j) = MenueParam.Misch.GrDae(i, j) - Tdae
      Next i
    Next j
    MenueParam.Misch.Gewr = GewAlt
    '
    For i = 0 To Ifest.Count - 1
      RezSozptHilf.Farben(i).OptData.Fest = Ifest(i)
    Next
    '
    '
    GrpRwerteHilfM.dispose()
    GrpRwerteHilfR.dispose()
    RezSozptHilf.dispose()

  End Sub
  Sub RaumRezept(IPRN As Integer, ByRef IanzHell As Integer, ByRef IanzWink As Integer, Winkel As AngGeos, KeyMenge As String, ByRef RezSozpt As RecipesGrp, ByRef GrpRwerte As RefValuesGrp, ByRef Ier As Integer)
    Dim IanH As Integer
    Dim IanW As Integer
    Dim i As Integer
    Dim k As Integer
    Dim l As Integer
    Dim KZL As Integer
    Dim kk As Integer
    Dim RzNr As String
    Dim KWW As Integer
    Dim NLicht As Integer
    Dim KU As Integer
    Dim NF As Integer
    Dim RzCount As Integer
    Dim Refcount As Integer
    Dim RzName As String
    Dim RefName As String
    Dim RezMng(,) As Single
    Dim RCAL(,,,) As Single
    Dim AL As Single
    Dim WIU As Single
    Dim WIO As Single
    Dim WIS As Single
    Dim MngRei As Single
    Dim Ummng As Single
    Dim SorKrit(,) As Single
    NF = RezSozpt.Rezepte(KeyMenge).KF
    Call RezEnde("RAU", Ier)
    Call RezBeginn("RAU", Winkel, NF, 0, KeyMenge, RezSozpt, Ier)
    If Ier <> 0 Then
      Call RezEnde("RAU", Ier)
      Exit Sub
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
    'Normlichtarten,Winkel,GK-Werte
    '
    '
    '
    '
    '
    '
    '
    '
    Call GetWinkNormGk("RAU", MenueParam.Menue.Kwj, Winkel, Ier)
    If Ier <> 0 Then Exit Sub
    '
    '
    '
    'Call REZDRU(Fehl)

    'Parameter für Rezeptberechnung
    '
    '
    '
    '
    '
    '
    '
    Call GetRezeptMenu("RAU", Ier)
    If Ier > 0 Then Exit Sub

    '
    '
    NF = RezSozpt.Rezepte(KeyMenge).KF
    '
    ReDim ID(NF - 1)
    ReDim SozMng(NF - 1)
    ReDim HlfMng(NF - 1)
    ReDim BasMng(NF - 1)
    ReDim LK(NF - 1)
    ReDim Proz(NF - 1)
    ReDim Prob(NF - 1)
    ReDim Spz(NF - 1)
    For i = 0 To NF - 1
      LK(i) = i + 1
    Next
    Call GetFamng(KeyMenge, RezSozpt, NF, ID, SozMng, Ier)
    For i = 0 To 1
      DICKE(i) = RezSozpt.Rezepte(KeyMenge).Dicke(i)
    Next i

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
    MngRei = umr.MngINR(KeyMenge, RezSozpt, Ier)
    If RezSozpt.INO = 0 Then
      MngRei = Max(MngRei, umr.MngINL(KeyMenge, RezSozpt, Ier))
    Else
      MngRei = Max(MngRei, RezSozpt.MngMax)
    End If
    Call GetGrundDaten("RAU", Winkel, RezSozpt, MngRei, NF, ID, Ier)
    If Ier <> 0 Then
      Exit Sub
    End If
    '
    '
    '
    '
    '
    'Rezept umspeichern
    '
    '
    '
    '


    '
    '
    '
    'Reflexionswerte für Untergründe
    '
    '
    '
    '
    NWE = Winkel.Wsol.Nwe
    KM = Winkel.Km
    NME = 2
    ReDim RUNT(NME - 1, KM - 1, NWE - 1)
    ReDim RVOR(NME - 1, KM - 1, NWE - 1)
    ReDim RNAC(NME - 1, KM - 1, NWE - 1)

    NUU = 0
    For i = 0 To GrpRwerte.Count - 1
      KWB(i) = GrpRwerte(i).RefUnt.kwb + 1
      Retr(i) = GrpRwerte(i).RefUnt.ReTr
      If GrpRwerte(i).RefUnt.IVoNa Then
        'Call GetTyrwert(Winkel.Wsol.Nwe, GrpRwerte("Z")(i), RwUnt(i))
        Call GetRwerte(Winkel, i, RUNT, GrpRwerte(i).RefUnt)
        NUU = NUU + 1
      End If
    Next i


    '
    '
    'Reflexionswerte für Vorlagen
    '
    '
    '
    '
    NUV = 0
    NUN = 0
    '
    '
    '
    '
    '
    'Sortimentsmengen und Reflexionskurven übernehmen
    '
    '
    '
    '
    '
    '
    'Call REZDRURAU(FEHL)

    Call REZMGRFRAU(1, NUU, NUV, NUN, NWE, KM, Retr(0), KWB(0), _
    RUNT(0, 0, 0), RVOR(0, 0, 0), RNAC(0, 0, 0), NF, SozMng(0), DICKE(0), FEHL)
    'Call REZDRURAU(FEHL)
    Ier = FEHL.Ifeh
    If FehlDLL(FEHL, BitInt(28, 29, MenueParam.User.Writ)) Then
      If Ier = 0 Then Ier = FEHL.Iwarn
      Exit Sub
    End If

    '
    '
    '
    '
    RzCount = RezSozpt.Rezepte.RezCount
    For k = RzCount - 1 To 0 Step -1
      RzName = RezSozpt.Rezepte.RezKey(k)
      If IsNumeric(RzName) Then
        RezSozpt.Rezepte.RemoveRez(RzName)
      End If
    Next k
    '
    'Außer "V" und "R" werden alle Reflexionswerte gelöscht
    '
    For k = 0 To GrpRwerte.Count - 1
      Refcount = GrpRwerte(k).Count
      For i = Refcount - 1 To 0 Step -1
        RefName = GrpRwerte(k).RwKey(i)
        If IsNumeric(RefName) Then
          GrpRwerte(k).Remove(RefName)
        End If
      Next i
    Next k
    '
    '
    '
    '
    '
    'Limitierungen für Farb-/Bindemittelmengen
    '
    '
    '
    '
    Ummng = RezSozpt.Rezepte(KeyMenge).UmMng
    umr.CalcBamng(KeyMenge, RezSozpt, Ier)
    For i = 0 To NF - 1
      HlfMng(i) = RezSozpt.Rezepte(KeyMenge)(i).BaAmng
      BasMng(i) = RezSozpt.Rezepte(KeyMenge)(i).FaAmng
    Next




    Call GetProzProbSpz(KeyMenge, RezSozpt, NF, Proz, Prob, Spz, Ier)
    Call GetRezLimit("RAU", Ummng, RezSozpt, NF, ID, HlfMng, BasMng, Proz, Prob, Spz, Ier)
    If Ier <> 0 Then
      Exit Sub
    End If
    '
    '
    'Call REZDRURAU(FEHL)
    Call GRZRAUM(FEHL)
    '
    '
    'Call REZDRURAU(FEHL)

    '
    '
    '
    'Farbräume berechnen
    '
    '
    '
    '
    NLicht = MenueParam.Menue.NLausw + 1
    KWW = MenueParam.Misch.Kwh + 1
    KU = 1
    IanH = Int((MenueParam.Menue.Pallg(2) - MenueParam.Menue.Pallg(1)) / MenueParam.Menue.Pallg(3)) + 1
    IanW = Int((MenueParam.Menue.Pallg(5) - MenueParam.Menue.Pallg(4)) / MenueParam.Menue.Pallg(6)) + 1
    ReDim RezMng(IanW - 1, NF - 1)
    ReDim ZwiMng(NF - 1)
    ReDim RCAL(IanW - 1, NME - 1, KM - 1, NWE - 1)
    ReDim RBER(NME - 1, KM - 1, NWE - 1)
    ReDim SorKrit(IanW - 1, 15)

    WIU = MenueParam.Menue.Pallg(4)
    WIO = MenueParam.Menue.Pallg(5)
    WIS = MenueParam.Menue.Pallg(6)
    KZL = IanW
    '
    '
    'Startwerte
    '
    '
    '
    AL = MenueParam.Menue.Pallg(1)

    Call REZRAUM(1, KWW, NLicht, KU, NWE, KM, NME, NF, KZL, BasMng(0), RezMng(0, 0), RCAL(0, 0, 0, 0), AL, WIU, WIO, WIS, SorKrit(0, 0), FEHL)
    Ier = FEHL.Ifeh
    If FehlDLL(FEHL, BitInt(28, 29, MenueParam.User.Writ)) Then
      If Ier = 0 Then Ier = FEHL.Iwarn
      Exit Sub
    End If
    For l = 0 To IanH - 1
      For i = 0 To NF - 1
        BasMng(i) = RezMng(KZL - 1, i)
      Next
      '
      '
      '
      '

      Call REZRAUM(1, KWW, NLicht, KU, NWE, KM, NME, NF, KZL, BasMng(0), RezMng(0, 0), RCAL(0, 0, 0, 0), AL, WIU, WIO, WIS, SorKrit(0, 0), FEHL)
      Ier = FEHL.Ifeh
      If FehlDLL(FEHL, BitInt(28, 29, MenueParam.User.Writ)) Then
        If Ier = 0 Then Ier = FEHL.Iwarn
        Exit Sub
      End If
      '
      '
      'Rezepte umspeichern
      '
      For k = 0 To KZL - 1
        For i = 0 To NF - 1
          ZwiMng(i) = RezMng(k, i)
        Next
        '
        '
        '
        '
        'Mengen umspeichern
        '
        '
        '
        '
        '
        '
        kk = k
        RzNr = KeyRe(l) & KeyRe(kk)
        '
        '
        '
        '
        RezSozpt.Rezepte.AddRez(RzNr, New Recipe)
        Call LetMengen(KeyMenge, RzNr, RezSozpt, k, _
        RezSozpt.Rezepte(KeyMenge).Name, RezSozpt.Rezepte(KeyMenge).Bem, NF, LK, ZwiMng, DICKE, Ier)
        Call umr.CalcBamng(RzNr, RezSozpt, Ier)
        '
        '
        '
        '
        '
        '
        '
        '
        'Berechnete Reflexionswerte RBER umspeichern
        '
        '
        GrpRwerte(KU - 1).Add(RzNr, New RefValue)
        For i = 0 To NWE - 1
          RBER(KU - 1, KWW - 1, i) = RCAL(k, KU - 1, KWW - 1, i)
        Next i
        Call LetCalcRwert(Winkel, GrpRwerte(0)(RzNr), GrpRwerte(0)(RzNr), _
        GrpRwerte(0)(RzNr).Nr, False, GrpRwerte(0)(RzNr).IVoNa, GrpRwerte(0)(RzNr).Cme, _
        GrpRwerte(0)(RzNr).Name, GrpRwerte(0)(RzNr).Bem, 0, RBER, Ier)
        '
        '
        '
        'Sortierkriterien umspeichern
        '

        RezSozpt.Rezepte(RzNr).Sorkrit(0).clear()


        For i = 0 To 15
          RezSozpt.Rezepte(RzNr).Sorkrit(0).Add(KeyRe(CInt(i)), New Double)
          RezSozpt.Rezepte(RzNr).Sorkrit(0)(KeyRe(CInt(i))) = SorKrit(k, i)
        Next i

        '
        '
      Next k
      AL = AL + MenueParam.Menue.Pallg(3)
    Next l
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
    IanzHell = IanH
    IanzWink = IanW
    Call RezEnde("RAU", Ier)
    Erase RCAL
    Erase RBER
    Erase RUNT
    Erase RezMng
    Erase SozMng
    Erase RVOR
    Erase RNAC
    '
    '
    '
    '
  End Sub

  Public Sub New()
    MyBase.New()
    umr = New RezeptUmrechnung
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
  Sub GrundGlzGrdCalc(Farben As Colorants, Winkel As AngGeos, ByRef Arbfarb As Colorant, GlzGrd As Single, ByRef ier As Integer)
    Dim Ifarb As Integer
    Dim kw As Integer
    Dim i As Integer
    Dim k As Integer
    Dim l As Integer
    Dim m As Integer
    Dim NPS As Integer
    Dim NST As Integer
    Dim Grund(,,,) As Single
    Dim MNF As Integer
    Dim KM As Integer
    Dim NWE As Integer
    Dim NPQ1 As Integer
    Dim NPQ As Integer
    Dim YP1 As Double = 0.0
    Dim YPN As Double = 0.0
    Dim XSI As Double = 10.0
    Dim YSDX As Double
    Dim Abl2() As Double
    Dim Rinput() As Double
    Dim GlzVek() As Double
    Dim GlzDoub As Double
    Dim Result As Double
    Dim GruResult(,,,) As Single
    Dim GlzHilf As Double
    Dim Ksort() As Integer
    Dim Khilf As Integer
    KM = Winkel.Km
    NWE = Winkel.Wsol.Nwe
    MNF = Farben.FarbCount
    NPQ1 = 10
    GlzDoub = GlzGrd
    '


    ReDim Grund(MNF - 1, NPQ1 - 1, KM - 1, NWE - 1)
    ReDim GruResult(0, NPQ1 - 1, KM - 1, NWE - 1)

    '
    '
    NPQ = Farben(0).OptData.Nst
    NPQ = NPNPS(MenueParam.Messg.CDE, MenueParam.Misch.Npx, NPQ)
    For Ifarb = 0 To Farben.FarbCount - 1
      NST = Farben(Ifarb).OptData.Nst
      NPS = NPNPS(MenueParam.Messg.CDE, MenueParam.Misch.Npx, NST)
      If NPS <> NPQ Then
        MsgBox(Texxt(3840))
        ier = 3840
        Exit Sub
      End If
      Call GetGrundDat(Ifarb, Winkel, NPS, Grund, Farben(Ifarb).OptData)
    Next
    '
    '
    ReDim GlzVek(MNF - 1)
    ReDim Rinput(MNF - 1)
    ReDim Abl2(MNF - 1)
    ReDim Ksort(MNF - 1)
    For Ifarb = 0 To MNF - 1
      Ksort(Ifarb) = Ifarb
      GlzVek(Ifarb) = Farben(Ifarb).GlzGrd
    Next
    '
    '
    'Sortieren
    '
    '
    For Ifarb = 0 To MNF - 1
      For m = Ifarb + 1 To MNF - 1
        If GlzVek(Ifarb) > GlzVek(m) Then
          GlzHilf = GlzVek(Ifarb)
          Khilf = Ksort(Ifarb)
          GlzVek(Ifarb) = GlzVek(m)
          Ksort(Ifarb) = Ksort(m)
          GlzVek(m) = GlzHilf
          Ksort(m) = Khilf
        End If
      Next m
    Next Ifarb
    For k = 0 To NPQ - 1
      For kw = 0 To KM - 1
        For i = 0 To NWE - 1
          For Ifarb = 0 To MNF - 1
            Rinput(Ifarb) = Grund(Ksort(Ifarb), k, kw, i)
          Next Ifarb
          '
          If MNF = 1 Or (MNF = 2 And Abs(GlzVek(0) - GlzVek(1)) < 0.0001) Then
            '
            '
            Result = Rinput(0)
          ElseIf MNF = 2 Then
            'lineare interpolation
            '
            '
            Result = (Rinput(1) * (GlzVek(0) - GlzDoub) + Rinput(0) * (GlzDoub - GlzVek(1))) / (GlzVek(0) - GlzVek(1))
          Else
            '
            'Splineberechnung
            '



            Call SPLINDLL(MNF, GlzVek(0), Rinput(0), Abl2(0), XSI, YP1, YPN)
            Call SPLITDLL(MNF, GlzVek(0), Rinput(0), Abl2(0), GlzDoub, Result, XSI, YSDX)
          End If
          GruResult(0, k, kw, i) = Max(0.0#, Result)
        Next i
      Next kw
    Next k
    '
    '
    Call LetGrundDat(0, Winkel, NPQ, GruResult, Arbfarb.OptData)
    '
  End Sub
  Sub GrundUmrechnung(WinkelAlt As AngGeos, AltFarben As Colorant, Winkelneu As AngGeos, NeuFarben As Colorant, ByRef ier As Integer)
    Dim i As Integer
    Dim j As Integer
    Dim kw As Integer
    Dim WsolAltD() As Double
    Dim WsolNeuD() As Double
    Dim Abl2() As Double
    Dim Result() As Double
    Dim Rinput() As Double
    Dim YP1 As Double = 0.0
    Dim YPN As Double = 0.0
    Dim XSI As Double = 10.0
    Dim Rcalc As Double
    Dim YSDX As Double
    Dim NWNeu As Integer
    Dim NWAlt As Integer
    Dim EPS As Double = 1.0E-30
    '
    '
    ier = 0
    NWAlt = WinkelAlt.Wsol.Nwe
    NWNeu = Winkelneu.Wsol.Nwe
    '



    ''
    '
    ReDim WsolAltD(NWAlt - 1)
    ReDim Abl2(NWAlt - 1)
    ReDim Rinput(NWAlt - 1)
    '
    '
    ReDim WsolNeuD(NWNeu - 1)
    ReDim Result(NWNeu - 1)

    For i = 0 To NWAlt - 1
      WsolAltD(i) = WinkelAlt.Wsol.R(i)
    Next
    For i = 0 To NWNeu - 1
      WsolNeuD(i) = Winkelneu.Wsol.R(i)
    Next
    '
    '
    '
    '
    'Schleife über Streu-/Abskonstanten
    '
    For j = 0 To AltFarben.OptData.Grund.Count - 1
      '
      'Schleife über Winkel
      '
      NeuFarben.OptData.Grund(j).clear()
      For kw = 0 To Winkelneu.Km - 1
        '
        'Prüfen, ob Winkel in Winkelalt enthalten
        '
        '
        If WinkelAlt.ContainsWink(Winkelneu(kw).Chrm) Then
          '
          'Berechnung der2.Ableitung Abl2
          '
          For i = 0 To NWAlt - 1
            Rinput(i) = AltFarben.OptData.Grund(j)(Winkelneu(kw).Chrm).R(i)
          Next i

          '
          Call SPLINDLL(NWAlt, WsolAltD(0), Rinput(0), Abl2(0), XSI, YP1, YPN)
          '
          '
          '
          NeuFarben.OptData.Grund(j).Add(Winkelneu(kw).Chrm, New CurveRef(NWNeu))
          For i = 0 To NWNeu - 1
            Call SPLITDLL(NWAlt, WsolAltD(0), Rinput(0), Abl2(0), WsolNeuD(i), Rcalc, XSI, YSDX)
            NeuFarben.OptData.Grund(j)(kw).R(i) = Rcalc
          Next
        End If
      Next kw
      If NeuFarben.OptData.Grund(j).Count <> Winkelneu.Km Then
        ier = 3024
      End If
    Next j


  End Sub

  Private Sub ttt()
    Throw New NotImplementedException
  End Sub

End Class