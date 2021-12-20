Option Strict Off
Option Explicit On
Option Compare Text
Public Class MenuParameters
  Implements IDisposable
  Implements ICloneable
  '
  '
  '
  '
  'Allgemeine Menüparameter
  '
  '
  '
  '
  '
  Private MnJABST As Integer ' 0 DIN6174; 1 DIN6176
  Private MnMethBez As String 'Name des Menüs
  Private MnMethKbez As String 'Kurzbezeichnung des Menüs
  Private MnAbsID As Short 'Absolutwerte beim Typ werden ausgegeben
  Private MnNrIhrmDE As Short 'Nr. des Winkels für Kontrastfarbabstand
  Private MnKDEWS As Single 'Kontrastfarbabstand
  Private MnGewDEWS As Single 'zug. Gewicht
  Private MnVerWS As Single 'Pigment-Verhältnis Weißpigm:Schwarzpigment
  Private MnLgew As Single 'Gewicht für Helligkeit
  Private MnCgew As Single 'Gewicht für Chroma
  Private MnHgew As Single 'Gewicht für Hue
  Private MnLcmc As Single 'L-Gewicht für CMC-Formel
  Private MnCcmc As Single 'C-Gewicht für CMC-Formel
  Private MnSchwell As Single 'Schwellenwert für Farbabstand (z.B. Messung-Rechnung)
  Private MnGNAX As Single 'Genauigkeit für X-Koordinate
  Private MnGNAY As Single 'Genauigkeit für Y-Koordinate
  Private MnPallg(15) As Single 'Allg. Parameter
  Private MnFu(1) As Single 'Gewicht für Untergründe
  Private MnNLausw As Short 'Nummer für Standard-Normlichtart
  Private MnGXD As Single 'frei
  Private MnDEL As Single 'Genauigkeitsparameter 1 (z.B. zur Berechnung von Ableitungen)
  Private MnDELTA As Single 'Genauigkeitsparameter 1 (z.B. zur Berechnung von Ableitungen)
  Private MnMVAL As Short 'frei
  Private MnSPR As Single 'Relatives Gewicht für Minimierung (WT in SBOCLS)
  Private MnEXPO As Single 'Exponent für R-Werte zur Berechnung der Grunddaten
  Private MnITMAX As Short 'frei
  Private MnTOLIND As Single 'frei
  Private MnTOLSZE As Single 'frei
  Private MnFAC As Single 'frei
  Private MnWT As Single 'frei
  Private MnFto As Single 'Genauigkeit Fehlerquadratsumme bei Iteration
  Private MnAto As Single 'Genauigkeit Konzentration bei Iteration
  Private MnItm As Short 'Max. Anzahl Iterationsschritte
  Private MnFopt As Single 'Umrechnungsfaktor für optische Dichte
  Private MnKwj As Short 'Nr. Winkel für Farbabstandsberechnung (0=Mittelwert über alle Winkel)
  Private MnBwertID As Integer 'B-Wert Nr
  Private MnBwertName(8) As String 'Name für die verschiedenen Farbtiefen
  Private MnFstaeID As Short 'Art der Farbstärkeberechnung
  Private MnFstaeWel As Single 'Wellenlänge zur Farbstärkeberechnung
  Private MnGMAX As Single   'Maximalwert für Extinktion,K/S usw.
  Private MnALP As Single    'Daempfungsparameter
  Private MnIDaeArt As Integer   'Art der Daempfung (!=Least Square Daempfung; 2 = Max. Entropie)
  '
  '
  Property MethBez() As String
    Get
      MethBez = MnMethBez
    End Get
    Set(ByVal Value As String)
      MnMethBez = Value
    End Set
  End Property
  Property MethKbez() As String
    Get
      MethKbez = MnMethKbez
    End Get
    Set(ByVal Value As String)
      MnMethKbez = Value
    End Set
  End Property

  '
  Property AbsID() As Short
    Get
      AbsID = MnAbsID
    End Get
    Set(ByVal Value As Short)
      MnAbsID = Value
    End Set
  End Property
  Property NrIhrmDE() As Short
    Get
      NrIhrmDE = MnNrIhrmDE
    End Get
    Set(ByVal Value As Short)
      MnNrIhrmDE = Value
    End Set
  End Property
  Property NLausw() As Short
    Get
      NLausw = MnNLausw
    End Get
    Set(ByVal Value As Short)
      MnNLausw = Value
    End Set
  End Property
  Property Kwj() As Short
    Get
      Kwj = MnKwj
    End Get
    Set(ByVal Value As Short)
      MnKwj = Value
    End Set
  End Property
  Property JABST() As Integer
    Get
      JABST = MnJABST
    End Get
    Set(ByVal Value As Integer)
      MnJABST = Value
    End Set
  End Property
  Property BwertID() As Integer
    Get
      BwertID = MnBwertID
    End Get
    Set(ByVal Value As Integer)
      MnBwertID = Value
    End Set
  End Property
  ReadOnly Property BwertName() As String()
    Get
      BwertName = MnBwertName
    End Get
  End Property


  Property FstaeID() As Short
    Get
      FstaeID = MnFstaeID
    End Get
    Set(ByVal Value As Short)
      MnFstaeID = Value
    End Set
  End Property
  Property Mval() As Short
    Get
      Mval = MnMVAL
    End Get
    Set(ByVal Value As Short)
      MnMVAL = Value
    End Set
  End Property
  Property ItMax() As Short
    Get
      ItMax = MnITMAX
    End Get
    Set(ByVal Value As Short)
      MnITMAX = Value
    End Set
  End Property
  Property KdeWS() As Single
    Get
      KdeWS = MnKDEWS
    End Get
    Set(ByVal Value As Single)
      MnKDEWS = Value
    End Set
  End Property
  Property VerWS() As Single
    Get
      VerWS = MnVerWS
    End Get
    Set(ByVal Value As Single)
      MnVerWS = Value
    End Set
  End Property
  Property GewDEWS() As Single
    Get
      GewDEWS = MnGewDEWS
    End Get
    Set(ByVal Value As Single)
      MnGewDEWS = Value
    End Set
  End Property
  Property Lgew() As Single
    Get
      Lgew = MnLgew
    End Get
    Set(ByVal Value As Single)
      MnLgew = Value
    End Set
  End Property
  Property Cgew() As Single
    Get
      Cgew = MnCgew
    End Get
    Set(ByVal Value As Single)
      MnCgew = Value
    End Set
  End Property
  Property Hgew() As Single
    Get
      Hgew = MnHgew
    End Get
    Set(ByVal Value As Single)
      MnHgew = Value
    End Set
  End Property
  Property Lcmc() As Single
    Get
      Lcmc = MnLcmc
    End Get
    Set(ByVal Value As Single)
      MnLcmc = Value
    End Set
  End Property
  Property Ccmc() As Single
    Get
      Ccmc = MnCcmc
    End Get
    Set(ByVal Value As Single)
      MnCcmc = Value
    End Set
  End Property
  Property Schwell() As Single
    Get
      Schwell = MnSchwell
    End Get
    Set(ByVal Value As Single)
      MnSchwell = Value
    End Set
  End Property
  Property Gnax() As Single
    Get
      Gnax = MnGNAX
    End Get
    Set(ByVal Value As Single)
      MnGNAX = Value
    End Set
  End Property
  Property Gnay() As Single
    Get
      Gnay = MnGNAY
    End Get
    Set(ByVal Value As Single)
      MnGNAY = Value
    End Set
  End Property
  Property Gxd() As Single
    Get
      Gxd = MnGXD
    End Get
    Set(ByVal Value As Single)
      MnGXD = Value
    End Set
  End Property
  Property Del() As Single
    Get
      Del = MnDEL
    End Get
    Set(ByVal Value As Single)
      MnDEL = Value
    End Set
  End Property
  Property Delta() As Single
    Get
      Delta = MnDELTA
    End Get
    Set(ByVal Value As Single)
      MnDELTA = Value
    End Set
  End Property
  Property TolInd() As Single
    Get
      TolInd = MnTOLIND
    End Get
    Set(ByVal Value As Single)
      MnTOLIND = Value
    End Set
  End Property
  Property Tolsze() As Single
    Get
      Tolsze = MnTOLSZE
    End Get
    Set(ByVal Value As Single)
      MnTOLSZE = Value
    End Set
  End Property
  Property Fac() As Single
    Get
      Fac = MnFAC
    End Get
    Set(ByVal Value As Single)
      MnFAC = Value
    End Set
  End Property
  Property Wt() As Single
    Get
      Wt = MnWT
    End Get
    Set(ByVal Value As Single)
      MnWT = Value
    End Set
  End Property
  Property SPR() As Single
    Get
      SPR = MnSPR
    End Get
    Set(ByVal Value As Single)
      MnSPR = Value
    End Set
  End Property
  Property EXPO() As Single
    Get
      EXPO = MnEXPO
    End Get
    Set(ByVal Value As Single)
      MnEXPO = Value
    End Set
  End Property
  Property Fto() As Single
    Get
      Fto = MnFto
    End Get
    Set(ByVal Value As Single)
      MnFto = Value
    End Set
  End Property
  Property Ato() As Single
    Get
      Ato = MnAto
    End Get
    Set(ByVal Value As Single)
      MnAto = Value
    End Set
  End Property
  Property Itm() As Short
    Get
      Itm = MnItm
    End Get
    Set(ByVal Value As Short)
      MnItm = Value
    End Set
  End Property
  Property GMAX() As Single
    Get
      GMAX = MnGMAX
    End Get
    Set(ByVal Value As Single)
      MnGMAX = Value
    End Set
  End Property
  Property ALP() As Single
    Get
      ALP = MnALP
    End Get
    Set(ByVal Value As Single)
      MnALP = Value
    End Set
  End Property
  Property IDaeArt() As Integer
    Get
      IDaeArt = MnIDaeArt
    End Get
    Set(ByVal Value As Integer)
      MnIDaeArt = Value
    End Set
  End Property
  Property Fopt() As Single
    Get
      Fopt = MnFopt
    End Get
    Set(ByVal Value As Single)
      MnFopt = Value
    End Set
  End Property
  Property FstaeWel() As Single
    Get
      FstaeWel = MnFstaeWel
    End Get
    Set(ByVal Value As Single)
      MnFstaeWel = Value
    End Set
  End Property
  ReadOnly Property Fu() As Single()
    Get
      Fu = MnFu
    End Get
  End Property
  ReadOnly Property Pallg() As Single()
    Get
      Pallg = MnPallg
    End Get
  End Property
  '
  '
  Function StdMrkKen(i As Integer) As String
    '                        Preis Fl.Diff. Korr.Sensi. X     Y     Z     x     y     L     C     h     a     b    Meta   DE    DL    DC    DH    da    db    K-DE
    '                           0     1     2     3     4     5     6     7     8     9     10    11    12    13    14    15    16    17    18    19    20    21 
    Dim DIN6174() As String = {"@P", "@Q", "@R", "@S", "AA", "AB", "AC", "AD", "AE", "BA", "BB", "BC", "BD", "BE", "BI", "BJ", "BK", "BL", "BM", "BN", "BO", "BT"}
    Dim DIN6176() As String = {"@P", "@Q", "@R", "@S", "AA", "AB", "AC", "AD", "AE", "EA", "EB", "EC", "ED", "EE", "EI", "EJ", "EK", "EL", "EM", "EN", "EO", "ET"}
    Dim CMCCOLL() As String = {"@P", "@Q", "@R", "@S", "AA", "AB", "AC", "AD", "AE", "GA", "GB", "GC", "GD", "GE", "GI", "GJ", "GK", "GL", "GM", "GN", "GO", "GT"}
    Select Case MnJABST
      Case 0
        StdMrkKen = DIN6174(i)
      Case 1
        StdMrkKen = DIN6176(i)
      Case 2
        StdMrkKen = CMCCOLL(i)
      Case Else
        StdMrkKen = DIN6174(i)
    End Select
  End Function
  Function TexJabst() As Integer
    Select Case MnJABST
      Case 0
        'DIN6174
        TexJabst = 10137
      Case 1
        'DIN6176
        TexJabst = 10329
      Case 2
        'CMC(Colli)
        TexJabst = 10457
      Case Else
        TexJabst = 10137
    End Select
  End Function

  Public Sub New()
    MyBase.New()

    '
    '
    'Startwerte
    '
    '
    '
    '
    ''
    MnAbsID = 0
    MnNrIhrmDE = 1
    MnKDEWS = 1.0#
    MnGewDEWS = 10
    MnVerWS = 50
    MnLgew = 1.0#
    MnCgew = 1.0#
    MnHgew = 1
    MnLcmc = 2.0#
    MnCcmc = 1
    MnSchwell = 1.0#
    MnGNAX = 0.0001
    MnGNAY = 0.0001
    MnFu(0) = 1.0#
    MnFu(1) = 1.0#
    MnNLausw = 0
    MnGXD = 0.0#
    MnDEL = 0.0001
    MnDELTA = 0.0001
    MnMVAL = 1
    MnITMAX = 10
    MnTOLIND = 0.1
    MnTOLSZE = 1.0#
    MnFAC = 1.0#
    MnWT = 1.0#
    MnFto = 0.002
    MnAto = 0.0001
    MnItm = 15
    MnFopt = 0.86858896 '=2./ln(10.)
    MnKwj = 1
    MnBwertID = 0
    MnFstaeID = 4
    MnFstaeWel = 0.0#
    MnSPR = 0.00001
    MnEXPO = 0.0#
    MnGMAX = 1000000
    MnALP = 0.000001
    MnIDaeArt = 0
    MnJABST = 0
  End Sub
  Protected Overrides Sub Finalize()
    MyBase.Finalize()
    dispose()
  End Sub
  Sub dispose() Implements IDisposable.Dispose
  End Sub
  '

  '
  '
  '
  Private Function CloneMe() As Object Implements ICloneable.Clone
    Return clone()
  End Function
  Function clone() As MenuParameters
    clone = CType(Me.MemberwiseClone, MenuParameters)
  End Function
End Class