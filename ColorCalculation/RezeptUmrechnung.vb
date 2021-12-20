Option Strict Off
Option Explicit On
Option Compare Text
Imports ColorGeneral.colorallglib
Imports ColorGeneral.colorhilflib
Imports Colorparamlib.dbhilfprogramme
Imports Colorparamlib.frbhilfprogramme

Public Class RezeptUmrechnung
  Implements IDisposable
  '
  '
  '
  '
  Dim MnLogwarn As Integer

  '
  '
  Dim Ivol As Integer
  Dim Inf As Integer
  Dim Ino As Integer
  Dim INP As Integer
  Dim INQ As Integer
  Dim Vkwb(0) As Integer
  'Normierungsmengen aus Reinen Farb-/Bindemittelmengen berechnen
  '
  'MngRei: Reine Gesamtmenge
  'MngAkt: Normierungsmenge gemäß INF
  'MngZae: Normierungsmenge für Zähler gemäß INP für Prozentigkeit
  'MngNen: Normierungsmenge für Nenner gemäß INQ für Prozentigkeit
  Dim MngAkt As Single
  Dim MngRei As Single
  Dim MngBA As Single
  Dim MngNen As Single
  Dim MngZae As Single
  Dim MngLi As Single
  'spezifisches Gewicht Lösemittel
  Dim Sploe As Single
  'Normierungsmengen gemäß INF
  Dim BaMng() As Single
  'reine Mengen
  Dim FaMng() As Single
  'Prozentigkeit für Farb-/Bindemittel
  Dim Proz() As Single
  'Prozentigkeit für Bindemittel
  Dim Prob() As Single
  'spezifisches Gewicht
  Dim Spz() As Single
  'Art des Farb-./Bindemittels
  Dim Ichf() As Integer
  'Limitierungen für Farb-/Bindemittel
  Dim OP() As Byte
  'Topfkennungen für Farb-/Bindemittel
  Dim KTO() As Byte
  'Faktor für Mengen (z.B. 1./spez. Gewicht)
  Dim Eff() As Single
  'Farb-/Bindemittel-ID
  Dim ID() As Integer

  '
  '
  '
  Dim KF As Long
  Public Function MngINO(ByRef Rzkey As String, ByRef Rezpte As RecipesGrp, ByRef Ier As Integer) As Single
    Call CalcRezepte(Rzkey, Rezpte, MngAkt, MngRei, MngBA, MngLi, MngNen, MngZae, Ier)
    MngINO = MngAkt
  End Function
  Public Function MngINQ(ByRef Rzkey As String, ByRef Rezpte As RecipesGrp, ByRef Ier As Integer) As Single
    Call CalcRezepte(Rzkey, Rezpte, MngAkt, MngRei, MngBA, MngLi, MngNen, MngZae, Ier)
    MngINQ = MngAkt
  End Function
  Public Function MngINP(ByRef Rzkey As String, ByRef Rezpte As RecipesGrp, ByRef Ier As Integer) As Single
    Call CalcRezepte(Rzkey, Rezpte, MngAkt, MngRei, MngBA, MngLi, MngNen, MngZae, Ier)
    MngINP = MngZae
  End Function
  Public Function MngINR(ByRef Rzkey As String, ByRef Rezpte As RecipesGrp, ByRef Ier As Integer) As Single
    If Rzkey = "" Then Exit Function
    If Rezpte.Rezepte(Rzkey).KF = 0 Then
      MngINR = Single.NaN
    Else
      Call CalcRezepte(Rzkey, Rezpte, MngAkt, MngRei, MngBA, MngLi, MngNen, MngZae, Ier)
      MngINR = MngRei
    End If
  End Function
  Public Function MngINF(ByRef Rzkey As String, ByRef Rezpte As RecipesGrp, ByRef Ier As Integer) As Single
    Dim k As Integer
    If Rzkey = "" Then Exit Function
    For k = 0 To Rezpte.Rezepte(Rzkey).KF - 1
      If Single.IsNaN(Rezpte.Rezepte(Rzkey)(k).FaAmng) Then
        Exit For
      End If
    Next k
    If k < Rezpte.Rezepte(Rzkey).KF Then
      MngINF = 0.0
    Else
      Call CalcRezepte(Rzkey, Rezpte, MngAkt, MngRei, MngBA, MngLi, MngNen, MngZae, Ier)
      MngINF = MngBA
    End If
  End Function
  Public Function MngINL(ByRef Rzkey As String, ByRef Rezpte As RecipesGrp, ByRef Ier As Integer) As Single
    Call CalcRezepte(Rzkey, Rezpte, MngAkt, MngRei, MngBA, MngLi, MngNen, MngZae, Ier)
    MngINL = MngLi
  End Function







  Sub CalcBamng(ByRef Rzkey As String, ByRef Rezpte As RecipesGrp, ByRef Ier As Integer)
    Dim i As Integer
    If Rzkey = "" Then Exit Sub
    If IsNothing(Rezpte.Farben) Then Exit Sub
    '
    'Gesamtmengen aus reinen Mengen (FaMng) berechnen
    '
    '
    '
    '
    Ier = 0
    Sploe = Rezpte.SpLoe
    If Rezpte.Farben.FarbCount = 0 Then Exit Sub
    If Rezpte.Rezepte(Rzkey).KF = 0 Then Exit Sub
    '
    '
    '
    'Prüfen, ob alle Faamng =NaN
    '
    '
    '
    For i = 0 To Rezpte.Rezepte(Rzkey).KF - 1
      If Not Single.IsNaN(Rezpte.Rezepte(Rzkey)(i).FaAmng) Then
        Exit For
      End If
    Next i
    If i >= Rezpte.Rezepte(Rzkey).KF Then
      For i = 0 To Rezpte.Rezepte(Rzkey).KF - 1
        Rezpte.Rezepte(Rzkey)(i).BaAmng = Rezpte.Rezepte(Rzkey)(i).FaAmng
      Next i
      Exit Sub
    End If
    '
    '
    '
    '
    '
    '
    KF = Rezpte.Rezepte(Rzkey).KF
    Ivol = Rezpte.IVOL
    Inf = Rezpte.INF
    ReDim BaMng(KF - 1)
    ReDim FaMng(KF - 1)
    ReDim Proz(KF - 1)
    ReDim Prob(KF - 1)
    ReDim Ichf(KF - 1)
    ReDim Spz(KF - 1)
    ReDim KTO(KF - 1)
    ReDim OP(KF - 1)
    ReDim Eff(KF - 1)
    ReDim ID(KF - 1)
    Call GetFamng(Rzkey, Rezpte, KF, ID, FaMng, Ier)
    Call GetIchfEff(Rezpte.Farben, KF, ID, Ichf, Eff, Ier)
    Call GetProzProbSpz(Rzkey, Rezpte, KF, Proz, Prob, Spz, Ier)


    Call RCHMNG(KF, Ivol, Inf, Sploe, BaMng(0), FaMng(0), Proz(0), Prob(0), Eff(0), Spz(0), Ichf(0), FEHL)
    Ier = FEHL.Ifeh
    If FehlDLL(FEHL, Logwarn) Then
    End If
    '
    '
    '
    '
    'Bamng zurückspeichern
    '
    '
    For i = 0 To KF - 1
      'If Ier = 0 Then
      Rezpte.Rezepte(Rzkey)(i).BaAmng = BaMng(i)
      'Else
      'Rezpte.Rezepte(Rzkey)(i).BaAmng = -999
      'End If
    Next i
    '
    Erase BaMng
    Erase FaMng
    Erase Proz
    Erase Prob
    Erase Ichf
    Erase Spz
    Erase OP
    Erase KTO
  End Sub

  Sub CalcFamng(ByRef Rzkey As String, ByRef Rezpte As RecipesGrp, ByRef Ier As Integer)
    Dim i As Integer
    '
    If Rzkey = "" Then Exit Sub
    'Reine Mengen (FaMng) aus Normierungsmengen (Bamng) berechnen
    '
    '
    '
    '
    '
    Ier = 0
    Sploe = Rezpte.SpLoe
    If Rezpte.Farben.FarbCount = 0 Then
      Ier = -1
      Exit Sub
    End If
    If Rezpte.Rezepte(Rzkey).KF = 0 Then Exit Sub
    '
    '
    '
    'Prüfen, ob alle Faamng =NaN
    '
    '
    '
    For i = 0 To Rezpte.Rezepte(Rzkey).KF - 1
      If Not Single.IsNaN(Rezpte.Rezepte(Rzkey)(i).BaAmng) Then
        Exit For
      End If
    Next i
    If i > Rezpte.Rezepte(Rzkey).KF Then
      For i = 0 To Rezpte.Rezepte(Rzkey).KF - 1
        Rezpte.Rezepte(Rzkey)(i).FaAmng = Rezpte.Rezepte(Rzkey)(i).BaAmng
      Next i
      Exit Sub
    End If
    '
    '
    '
    '
    '
    '
    KF = Rezpte.Rezepte(Rzkey).KF
    Ivol = Rezpte.IVOL
    Inf = Rezpte.INF
    ReDim BaMng(KF - 1)
    ReDim FaMng(KF - 1)
    ReDim Proz(KF - 1)
    ReDim Prob(KF - 1)
    ReDim Ichf(KF - 1)
    ReDim Eff(KF - 1)
    ReDim Spz(KF - 1)
    ReDim KTO(KF - 1)
    ReDim OP(KF - 1)
    ReDim ID(KF - 1)
    Call GetBaMng(Rzkey, Rezpte, KF, ID, BaMng, Ier)
    Call GetIchfEff(Rezpte.Farben, KF, ID, Ichf, Eff, Ier)
    Call GetProzProbSpz(Rzkey, Rezpte, KF, Proz, Prob, Spz, Ier)
    Call GetOpKto(Rezpte.Farben, KF, ID, OP, KTO, Ier)
    Call RCHREI(KF, Ivol, Inf, Sploe, BaMng(0), FaMng(0), Proz(0), Prob(0), Eff(0), Spz(0), Ichf(0), FEHL)
    Ier = FEHL.Ifeh
    If FehlDLL(FEHL, Logwarn) Then
      If Ier <> 4105 Then Exit Sub
    End If
    '
    '
    '
    '
    'Famng zurückspeichern
    '
    '
    For i = 0 To KF - 1
      Rezpte.Rezepte(Rzkey)(i).FaAmng = FaMng(i)
    Next i
    Erase BaMng
    Erase FaMng
    Erase Proz
    Erase Prob
    Erase Ichf
    Erase Spz
    Erase OP
    Erase KTO
    '
  End Sub


  Sub CalcRezepte(ByRef Rzkey As String, ByRef Rezpte As RecipesGrp, _
  ByRef MngAkt As Single, ByRef MngRei As Single, ByRef MngBA As Single, ByRef MngLi As Single, ByRef MngNen As Single, ByRef MngZae As Single, ByRef Ier As Integer)
    Dim i As Integer
    '
    '
    '
    '
    '
    '
    Ier = 0
    Sploe = Rezpte.SpLoe
    If Rezpte.Farben.FarbCount = 0 Then Exit Sub
    If Rezpte.Rezepte(Rzkey).KF = 0 Then Exit Sub
    '
    '
    '
    '
    '
    '
    '
    '
    KF = Rezpte.Rezepte(Rzkey).KF
    Ivol = Rezpte.IVOL
    Ino = Rezpte.INO
    Inf = Rezpte.INF
    INP = Rezpte.INP
    INQ = Rezpte.INQ
    ReDim BaMng(KF - 1)
    ReDim FaMng(KF - 1)
    ReDim Proz(KF - 1)
    ReDim Prob(KF - 1)
    ReDim Ichf(KF - 1)
    ReDim Spz(KF - 1)
    ReDim OP(KF - 1)
    ReDim KTO(KF - 1)
    ReDim Eff(KF - 1)
    ReDim ID(KF - 1)
    'Reine Mengen (FaMng) aus Normierungsmengen (Bamng) berechnen
    Call GetFamng(Rzkey, Rezpte, KF, ID, FaMng, Ier)
    Call GetIchfEff(Rezpte.Farben, KF, ID, Ichf, Eff, Ier)
    Call GetProzProbSpz(Rzkey, Rezpte, KF, Proz, Prob, Spz, Ier)
    Call GetOpKto(Rezpte.Farben, KF, ID, OP, KTO, Ier)
    '
    'MngRei reine Farbmittelmenge
    'MngBA  gesamte Batchmenge
    'MngAkt Menge gemäß Normierung
    'MngZae Menge Zähler Prozentigkeit
    'MngNen Menge Nenner Prozentigkeit
    'MngLi  Menge Limitierung
    '
    '
    MngLi = 0.0
    For i = 0 To Rezpte.Farben.FarbCount - 1
      MngLi = MngLi + Rezpte.Farben(i).BoMng
    Next
    '
    Call RCHAMX(KF, Ivol, Inf, Ino, INP, INQ, Sploe, _
    FaMng(0), Proz(0), Prob(0), Eff(0), Spz(0), Ichf(0), OP(0), _
    MngRei, MngBA, MngAkt, MngZae, MngNen, FEHL)
    Ier = FEHL.Ifeh
    If FehlDLL(FEHL, Logwarn) Then
      If Ier <> 0 Then Exit Sub
    End If
    '
    '
    '
    '
    'Famng zurückspeichern
    '
    '
    Erase BaMng
    Erase FaMng
    Erase Proz
    Erase Prob
    Erase Ichf
    Erase Spz
    Erase OP
    Erase KTO
    '
  End Sub


  Property Logwarn() As Integer
    Get
      Logwarn = MnLogwarn

    End Get
    Set(ByVal value As Integer)
      MnLogwarn = value
    End Set
  End Property

  Sub MNGFamngNorm(ByRef Rzkey As String, ByRef Rezepte As RecipesGrp, ByRef NorNeu As Single, ByRef Ier As Integer)
    Dim i As Integer
    Dim Fakt As Single
    Dim Noralt As Single
    '
    If NorNeu = 0 Then
      Ier = -1
      Exit Sub
    End If
    Noralt = MngINR(Rzkey, Rezepte, Ier)
    Fakt = NorNeu / Noralt
    For i = 0 To Rezepte.Rezepte(Rzkey).KF - 1
      Rezepte.Rezepte(Rzkey)(i).FaAmng = Fakt * Rezepte.Rezepte(Rzkey)(i).FaAmng
    Next i
  End Sub
  Sub MNGBamngNorm(ByRef Rzkey As String, ByRef Rezepte As RecipesGrp, ByRef NorNeu As Single, ByRef Ier As Integer)
    Dim i As Integer
    Dim Fakt As Single
    Dim Noralt As Single
    If Rzkey = "" Then Exit Sub
    '
    If NorNeu = 0 Then
      Ier = -1
      Exit Sub
    End If
    Noralt = MngINF(Rzkey, Rezepte, Ier)
    Fakt = NorNeu / Noralt
    For i = 0 To Rezepte.Rezepte(Rzkey).KF - 1
      Rezepte.Rezepte(Rzkey)(i).BaAmng = Fakt * Rezepte.Rezepte(Rzkey)(i).BaAmng
    Next i
  End Sub





  Public Sub New()
    MyBase.New()
    MnLogwarn = 0
    Vkwb(0) = 1
    Vkwb(0) = 2
  End Sub
  Protected Overrides Sub Finalize()
    MyBase.Finalize()
    dispose()
  End Sub
  Sub dispose() Implements IDisposable.Dispose
  End Sub

End Class