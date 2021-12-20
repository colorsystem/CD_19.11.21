Option Strict Off
Option Explicit On
Option Compare Text

<ComClass(UserRefWerte.ClassId, UserRefWerte.InterfaceId, UserRefWerte.EventsId)> _
Public Class UserRefWerte
  Implements IDisposable

#Region "COM-GUIDs"
  ' Diese GUIDs stellen die COM-Identität für diese Klasse 
  ' und ihre COM-Schnittstellen bereit. Wenn Sie sie ändern, können vorhandene 
  ' Clients nicht mehr auf die Klasse zugreifen.
  Public Const ClassId As String = "dbfc96e1-fd05-416a-b0e2-73a8ff9703ea"
  Public Const InterfaceId As String = "358ce091-490f-4dd9-aa0c-785eb3455e2b"
  Public Const EventsId As String = "b6359434-1fdb-4672-8c6d-6f10e27fa56d"
#End Region

  ' Eine erstellbare COM-Klasse muss eine Public Sub New() 
  ' ohne Parameter aufweisen. Andernfalls wird die Klasse 
  ' nicht in der COM-Registrierung registriert und kann nicht 
  ' über CreateObject erstellt werden.
  Public Sub New()
    MyBase.New()
    MnIer = 0
    DriverAlt = ""
    NormFaAlt = ""

  End Sub
  '
  '
  '
  'Dieses Programm dient zur Messwertübernahme von Reflexions- oder Transmissionswerten
  'für die Programme des BASF-COLOR-SYSTEM's. Der Name der Active-X-Komponente
  '(UserRefWerte) und der Name der Klasse (MessenRefWerte) sind frei wählbar.
  'Die Namen und Listenparameter, der Eigenschaften und Methoden müssen unbedingt
  'gemäß nachfolgender Beschreibung verwendet werden.
  '
  '                         Property Get Messg()as MessgParameter
  '                         Property Set Messg(AcMessg as MessgParameter)
  '                         Property Get Normfa()as Normfarbe
  '                         Property Set Normfa(AcNormfa as Normfarbe)
  '                         Property Get Ier()as Integer
  '                         Property Let Ier(AcIer as Integer)
  '                         MeasDeviceNkalib()
  '                         MeasDeviceSond()
  '                         MeasKalibrieren()
  '                         MeasMessen(Refel as Rwert)
  '                         MeasDeviceInit()
  '                         MeasDeviceKalib()
  '                         MeasDeviceMess(Refel as Rwert)
  '                         MeasDeviceManu(Refel as Rwert)
  '
  'Mit Hilfe des Programms
  '
  'BCSEinst
  '   Start (mit Administratorname und Passwort)/OK
  '   Initialisieren
  '
  'kann der Name der Active-X-Komponenten + Klasse (z.B. UserRefWerte.MessenRefWerte)
  'unter
  '
  '   Active-X-Komponete:Messgerät
  '
  'eingetragen werden. Nach
  '
  '   Fortfahren
  '
  'werden die Einträge in der Datei: BCSPrgm.INI abgespeichert
  '
  'Programmierhinweis:
  'Das zur Klasse "MessenRefwerte" gehörende Active-X Projekt muss einen Verweis
  '(Project/Verweise) auf die Active-X-DLL: "BCSWINParamStruct"
  'haben.
  '
  'Folgende Eigenschaften stehen zur Verfügung :
  '
  '
  '**************************
  '**************************
  'Messg as MessgParameter
  '
  'Property Get Messg()as MessgParameter
  'Property Set Messg(AcMessg as MessgParameter)
  '**************************
  '**************************
  '
  '
  'In dieser Klasse stehen die für das Messgerät relevanten Größen wie z.B.
  '
  'Messg.MessgID          ID-des Messgerätes
  '                       (s. Tabelle TBL_MESSG in der Datenbank BCSWIN.MDB)
  'Messg.Kbez             Kurzbezeichnung
  'Messg.Driver           Kennung für Messgerät(3 Zeichen)
  'Messg.Prog             Zusätzliche Kennung für Messgerät (32 Zeichen)
  'Messg.Baud             Baudrate
  'Messg.Par              Parität (1 Zeichen)
  'Messg.Length           Anzahl Bits
  'Messg.Stopbit          Anzahl Stopbits
  'Messg.Hands            Handshake (0 = kein Handshake)
  'Messg.Winkel.Wsol.Nwe  Anzahl Wellenlängen
  'Messg.Winkel.Wsol.r(i) Wellenlängen (z.B. 400,410 usw.)
  '                       i=1 to Messg.Winkel.Wsol.Nwe
  'Messg.Winkel.Km        Anzahl Winkel bzw. Messgeometrien
  'Messg.Winkel(kw).CHRM  Kennung für Winkel bzw. Messgeometrie
  '                       kw=1 to Messg.Winkel.Km
  'Messg.Ikal             Anzahl Messungen für Mittelwert (Kalibrierung)
  'Messg.Imes             Anzahl Messungen für Mittelwert (Messung)
  'Messg.Sond             =0 Menü "SonderProgramm" (MeasDeviceSond) wird nicht angezeigt
  'Messg.Nkal             =0 Menü "Nullkalibrieren"(MeasDeviceNkalib) wird nicht angezeigt
  'Messg.Kein             =0 Menü "Kein Messgerät" wird nicht angezeigt
  'Messg.Mes              =0 Menü "Messen" wird nicht angezeigt
  'Messg.Ini              =0 Menü "Initialisieren" (MeasDeviceInit) wird nicht angezeigt
  'Messg.Kal              =0 Menü "Kalibrieren" (MeasDeviceKalib) wird nicht angezeigt
  'u.a.
  '
  '
  '
  '
  'Mit Hilfe des Programms
  '   BCSEinst
  '   Start (Administratorname,Passwort)/OK
  '   Initialisieren/Fortfahren
  '   Systemeinstellungen/Messgeräte
  'können die meisten der oben genannten Parameter festgelegt werden.
  '
  '
  '
  '
  '
  '******Es dürfen keine Änderungen an Messg vorgenommen werden außer an
  '      Messg.Com
  '      Messg.Baud
  '      Messg.Par
  '      Messg.Length
  '      Messg.Stopbit
  '      Messg.Hands
  '      Messg.QW(kw).r(i)  Absolutwert für Weißstandard
  '      Messg.QS(kw).r(i)  Absolutwert für Schwarzstandard
  '      Messg.QG(kw).r(i)  Absolutwert für Graustandard
  '
  '
  '      Messg.MW(kw).r(i)  Messwert für Weißstandard bei Weißkalibrierung
  '      Messg.MS(kw).r(i)  Messwert für Schwarzstandard bei Schwarzkalibrierung
  '      Messg.MG(kw).r(i)  Messwert für Graustandard bei Graukalibrierung
  '
  '                         jeweils für kw-ten Winkel (bzw. Messgeometrie)
  '                         und der i-ten Wellenlänge
  '
  '
  '
  '
  '**************************
  '**************************
  'Normfa as NormFarbe
  'Property Get Normfa()as Normfarbe
  'Property Set Normfa(AcMessg as Normfarbe)
  '**************************
  '**************************
  '
  '
  'Hier stehen die für die Normlichtart erforderlichen Größen zur Verfügung.
  '(nur interessant, falls farbmetrische Werte während des Messablaufs
  'berechnet werden sollen)
  '
  '
  'Normfa.NormNama              Name der Normlichtart
  'Normfa.NormKenn              Kennung der Normlichtart
  'Normfa.NormFakt(j)           Faktor für die Tristimuluswerte  X,Y,Z
  'Normfa.Normkurven("X").r(i)  Gewichtsfaktoren für Tristimuluswert X
  '                             i=1 to Messg.Winkel.Wsol.Nwe
  'Normfa.Normkurven("Y").r(i)  Gewichtsfaktoren für Tristimuluswert Y
  'Normfa.Normkurven("Z").r(i)  Gewichtsfaktoren für Tristimuluswert Z
  '
  'u.a.
  '
  '******Es dürfen keine Änderungen an Normfa vorgenommen werden
  '
  '**************************
  '**************************
  'Refel as Rwert
  '**************************
  '**************************
  'Refel.Name                   Name der Reflexionskurve
  'Refel.Bem                    Bemerkung
  'Refel.Iami                   Anzahl Messungen für Mittelwertbildung
  'Refel.DE(kw)                 Farbabstand (oder vergleichbarer Wert)
  '                             zur Charakterisierung der Messgenauigkeit
  '                             des kw-ten Winkels (bzw. Messgeometrie)
  'Refel.ReflKurven(kw).r(i)    Reflexionswert für kw-ten Winkel
  '                             (bzw. Messgeometrie) und i-te Wellenlänge
  '
  '**************************
  '**************************
  'ier as integer
  'Property Get Ier()as Integer
  'Property Let Ier(AcIer as Integer)
  '**************************
  '**************************
  '
  '
  'Fehlercode
  'ier<0                        Fehler
  '
  '
  '
  'Folgende Methoden stehen zur Verfügung:
  '
  '
  '**************************
  '**************************
  'MeasDeviceNkalib()
  '**************************
  '**************************
  '
  'Dient bei manchen Messgeräten zur Nullkalibrierung
  '
  '**************************
  '**************************
  'MeasDeviceSond()
  '**************************
  '**************************
  '
  'Sonderprogramm (wird in BCSWIN nicht verwendet)
  '
  '
  '
  '**************************
  '**************************
  'MeasKalibrieren()
  '**************************
  '**************************
  '
  'Spezialprogramm (wird in BCSWIN nicht verwendet)
  '
  '
  '
  '
  '**************************
  '**************************
  'MeasMessen(Refel as Rwert)
  '**************************
  '**************************
  '
  'Spezialprogramm (wird in BCSWIN nicht verwendet)
  '(s. MeasDeviceMess)
  '
  '**************************
  '**************************
  'MeasDeviceInit()
  '**************************
  '**************************
  '
  '
  'Hiermit wird das Messgerät initialisiert.
  'Falls die Absolutwete zur Berechnung der Reflexionswerte erforderlich sind,
  '(s.MeasDeviceMess) werden sie für den Weiß- Schwarz- und eventuell Graustandard
  'eingelesen und in geeigneter Form abgespeichert (Messg.QW, Messg.QS und Messg.QG).
  '
  '
  '
  '
  '**************************
  '**************************
  'MeasDeviceKalib()
  '**************************
  '**************************
  '
  '
  'Hiermit wird das Messgerät für alle dem Messgerät zugeordneten
  'Winkel bzw. Messgeometrien kalibriert.
  'Die Kalibrierung erfolgt für den Weiß- und eventuell Schwarz-
  'und eventuell Graustandard)
  'Die Werte werden in Messg.MW, Messg.MS bzw. Messg.MG abgespeichert,
  'falls sie für die Berechnung der Reflexionswerte erforderlich sind (s.MeasDeviceMess).
  '
  'Die Werte für Messg.QW, Messg.QS bzw. Messg.QG (Absolutwerte für die Standards)
  'werden eingelesen, falls sie zur Berechnung der Reflexionswerte erforderlich sind
  '(s.MeasDeviceMess).
  '
  '
  '
  '
  '**************************
  '**************************
  'MeasDeviceMess(Refel as Rwert)
  '**************************
  '**************************
  '
  '
  'Hiermit werden Messwerte (Reflexions- oder Transmissionswerte )
  'für alle Winkel bzw. Messgeometrien und alle Wellenlängen
  'von einem Messgerät übernommen.
  '
  '
  'Refel as Rwert
  'Nach Aufruf von MeasDeviceMess gesetzte Größen
  '
  '             Typ
  'Refel.Name   String       Name der Reflexions- bzw. Transmissionskurve (max. 40 Zeichen)
  '                          Hinweis:
  '                          Bei "" wird der Name vom rufenden Programm gesetzt
  'Refel.Bem    String       Bemerkung (max. 40 Zeichen)
  '                          s. Hinweis bei Refel.Name
  'Refel.Banum  String       Kennung (max. 20 Zeichen)
  '                          s. Hinweis bei Refel.Name
  'Refel.Dat    Date         Datum
  'Refel.tim    Date         Uhrzeit
  '
  'Zu setzende Größen
  '
  'Refel.Iami                Anzahl Messungen
  'Refel.De(kw)              Farbabstände für kw-ten Winkel (bzw. Messgeometrie)
  '                          (z.B. Standardabweichung zum Mittelwert)
  'Refel.ReflKurven(kw).r(i) Reflexionswert für kw-ten Winkel (bzw. Messgeometrie)
  '                          und i-te Wellenlänge
  '
  '
  '
  '**************************
  '**************************
  'MeasDeviceManu(Refel as Rwert)
  '**************************
  '**************************
  '
  '
  'Mit Hilfe dieser Methode können Reflexionswewrte für alle Winkel(Messgeometrien)
  'und alle Wellenlängen manuell eingegeben werden (s. MeasDeviceMess)
  '
  '
  'Beispielprogramm ohne Verwendung eines Messgerätes
  '
  '
  '
  '
  '
  'Hilfsvariable
  '
  '
  '
  Dim nkw As Integer
  Dim kw As Integer
  Dim i As Integer
  '
  Dim MnIer As Integer
  Dim DriverAlt As String
  Dim NormFaAlt As String
  Dim Measure As MeasureRefl
  Dim UserProgram As String
  Dim UserFile As String
  Dim iret As Integer
  Dim Sr As StreamReader

  '
  '0,1 kein Fehler
  'ier<0 Fehler


  '
  '
  '
  '
  Sub GetRwertFromFile(ByRef Refel As RefValue, ByRef ier As Integer)
    Dim kw As Integer
    Dim i As Integer
    Dim nkw As Integer
    Dim ReLine As String

    Sr = File.OpenText(UserFile)
    Refel.RefKurv.clear()
    Refel.Name = ""
    Refel.Bem = ""
    Refel.Banum = ""
    Refel.Cme = MessgKenn
    Refel.RefKurv.clear()
    Refel.Iami = iami
    Refel.Gid = MessgGID



    For kw = 0 To MessgKM - 1
      ReLine = Sr.ReadLine()
      If ReLine.Trim <> MessgChrm(kw) Then
        ier = -2
        Exit Sub
      End If
      If Sr.Peek = -1 Then
        ier = -4
        Exit Sub
      End If
      Refel.De(kw) = 0.0
      Refel.RefKurv.Add(MessgChrm(kw), New CurveRef(MessgNwe))
      nkw = kw * MessgNwe
      For i = 0 To MessgNwe - 1
        ReLine = Sr.ReadLine()
        If CSng(ReLine.Substring(0, 3)) <> MessgWsol(nkw + i) Then
          ier = -3
          Exit Sub
        End If
        Refel.RefKurv(MessgChrm(kw)).R(i) = Singl(ReLine.Substring(4))
        If Sr.Peek = -1 And i < MessgNwe - 1 Then
          ier = -4
          Exit Sub
        End If
      Next i
    Next kw
  End Sub


  Sub Umspeich(ByRef Messg As MeasParameters, ByRef Normfa As NormIlluminat, ByRef ier As Integer)
    '
    '

    ier = 0
    Measure = New MeasureRefl(Messg, Normfa)
    ier = Measure.ier
    UserProgram = PrivSettings("COMCLASS", "MEASURE", "", COLORFileName())
    If UserProgram = "" Then
      Exit Sub
    End If
    UserFile = UserProgram
    Mid(UserFile, Len(UserFile) - 3, 4) = ".TXT"    '
    '

  End Sub
  Public Sub MeasDevManu(ByVal Refel As RefValue, ByRef ier As Integer)
    '
    '
    '
    'Manuelle Eingabe von Reflexionswerten (s. MeasDeviceMess)
    '
    '
    '
    '
  End Sub
  Public Sub MessDevMess(ByVal Refel As RefValue, ByRef ier As Integer)
    '
    '
    If File.Exists(UserProgram) Then
      iret = Shell(UserProgram & " MES", AppWinStyle.NormalFocus, True)
      Call GetRwertFromFile(Refel, ier)
    Else
      MsgBox(Texxt(392) & ": " & UserProgram & " " & Texxt(2984))
      ier = -1
    End If

    '
    '
    '

    '
    '

  End Sub

  Public Sub MeasDevKalib(ByRef ier As Integer)
    '
    '
    If File.Exists(UserProgram) Then
      'iret = Shell(UserProgram & " KAL", AppWinStyle.NormalFocus, True)
    Else
      MsgBox(Texxt(392) & ": " & UserProgram & " " & Texxt(2984))
      ier = -1
    End If


    '
  End Sub
  Public Sub MeasDevNkalib(ByRef ier As Integer)
    '
    '
    'Spezielle Methode
    'Normalerweise Nur Aufruf erforderlich
    '
    '
    '
  End Sub

  Public Sub MeasDevInit()
    '
    '
    'Messgerät initialisieren
    '

    '
    If File.Exists(UserProgram) Then
      iret = Shell(UserProgram & " INI", AppWinStyle.NormalFocus, True)
    Else
      MsgBox(Texxt(392) & ": " & UserProgram & " " & Texxt(2984))
      ier = -1
    End If
    '
   
    '
    '
  End Sub
  

  Public Sub MeasDevSond(ByRef ier As Integer)
    '
    '
    'Spezielle Methode
    'Normalerweise Nur Aufruf erforderlich
    '
    '
    '
  End Sub



  Sub dispose() Implements IDisposable.Dispose
    Measure.dispose()
  End Sub

  Protected Overrides Sub Finalize()
    MyBase.Finalize()
  End Sub

 


End Class


