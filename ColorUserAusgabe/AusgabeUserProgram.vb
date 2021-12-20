Option Explicit On
Option Strict Off
Option Compare Text
<ComClass(AusgabeUserProgram.ClassId, AusgabeUserProgram.InterfaceId, AusgabeUserProgram.EventsId)> _
Public Class AusgabeUserProgram

#Region "COM-GUIDs"
  ' Diese GUIDs stellen die COM-Identität für diese Klasse 
  ' und ihre COM-Schnittstellen bereit. Wenn Sie sie ändern, können vorhandene 
  ' Clients nicht mehr auf die Klasse zugreifen.
  Public Const ClassId As String = "cef72900-311e-4c92-98ef-ab1074acc984"
  Public Const InterfaceId As String = "b681620f-a850-4911-bf85-d7a77df47a5c"
  Public Const EventsId As String = "9004184d-8d5f-44b7-a3cf-67a69b1eeafb"
#End Region

  ' Eine erstellbare COM-Klasse muss eine Public Sub New() 
  ' ohne Parameter aufweisen. Andernfalls wird die Klasse 
  ' nicht in der COM-Registrierung registriert und kann nicht 
  ' über CreateObject erstellt werden.


 
  Public Sub New()
    MyBase.New()

    'CrReWrDat = New CreateReadWriteDaten
    '
    '
    'Tabelle für User,Messgerät,Methode, Mischsystem
    '
    '
 
    '
    '
   
    '
    '
    '
    '
    '
    'Tabelle für Farb-/Bindemittel
    '
    '
    '
    'TblFarben = "TBL_Farben"
    Call CreateFarben("TBL_Farben", TblFarben, MnIer)
    '
    '
    '
    'Tabelle für Rezepte
    '
    '
    '
    'TblRezepte = "TBL_Rezepte"
    Call CreateRezepte("TBL_Rezepte", TblRezepte, MnIer)
    '
    '
    '
    'Tabelle für Farb-/Bindemittelmengen
    '
    '
    '
    'TblMengen = "TBL_Mengen"

    Call CreateMengen("TBL_Mengen", TblMengen, MnIer)
    '
    'Tabelle für Farb-/Bindemittelmengen
    '
    '
    '
    'TblWerteAllg = "TBL_WrtAllg"
    '
    Call CreateWerteAllg("TBL_WrtAllg", TblWerteAllg, MnIer)
    '
    '
    ReDim TblMenue(1)
    Call CreateMenueTables({"TBL_UsMesMetMis", "TBL_NormKbez"}, TblMenue, MnIer)
    '
    '
    '
    '
    '
    'Tabellen für Merkmale der Anweisungen
    '
    '
    Call CreateAnweisng("TBL_ANWEISUNGEN", TblAnWeisng, MnIer)
    Call CreateMerk("TBL_MERK", TblMerk, MnIer)
    Call CreateWeisngRwerte("TBL_WEISNGRWERTE", TblWeisngRwerte, MnIer)
    Call CreateNormLicht("TBL_NORMLICHT", TblNormLicht, MnIer)
    Call CreateWinkel("TBL_WINKEL", TblWinkel, MnIer)
    Call CreateWerte("TBL_WERTE", TblWerte, MnIer)
    '
    'Tabelle für R-Werte
    '
    '
    Call CreateRwerte("TBL_RWERTE", TblRwerte, MnIer)


  End Sub
 
  

  Sub Qualita(Menueparam As AllParameters, ByRef ParamAlle As ValuesGrpsAssigns, ByRef GrpRwerte As RefValuesGrp)
    Dim Aform As New frmUserAusgabe
    '
    '
    '
    '
    '
    'UserID,MessgID,MethID,MischID
    'Name Normlichtarten
    '


    Call WriteMenueTables(TblMenue, Menueparam, ParamAlle, MnIer)
    '
   
    '
    'Merkmale der Anweisungen
    '
    '
    '
    Call WriteAnweisng(TblAnWeisng, ParamAlle, MnIer)
    Call WriteMerk(TblMerk, ParamAlle, MnIer)
    Call WriteWeisngRwerte(TblWeisngRwerte, ParamAlle, MnIer)
    Call WriteNormLicht(TblNormLicht, ParamAlle, MnIer)
    Call WriteWinkel(TblWinkel, ParamAlle, MnIer)
    Call WriteWerte(TblWerte, ParamAlle, MnIer)
    '
    '
    '
    'R-Werte
    '
    '
    '
    '
    '
    Call WriteRwerte(TblRwerte, GrpRwerte, MnIer)


    Call WriteWerteAllg(Menueparam, TblWerteAllg, ParamAlle, MnIer)

    Aform.ShowDialog()

  End Sub
  Sub Wiegesc(Menueparam As AllParameters, Rezsozpt As RecipesGrp, ParamAlle As ValuesGrpsAssigns, GrpRwerte As RefValuesGrp)
    Dim Aform As New frmUserAusgabe
    '
    '
    '
    '
    '
    'UserID,MessgID,MethID,MischID
    'Name Normlichtarten
    '
    Call WriteMenueTables(TblMenue, Menueparam, ParamAlle, MnIer)
    '
    '


    '
    'Merkmale der Anweisungen
    '
    '
    '
    Call WriteAnweisng(TblAnWeisng, ParamAlle, MnIer)
    Call WriteMerk(TblMerk, ParamAlle, MnIer)
    Call WriteWeisngRwerte(TblWeisngRwerte, ParamAlle, MnIer)
    Call WriteNormLicht(TblNormLicht, ParamAlle, MnIer)
    Call WriteWinkel(TblWinkel, ParamAlle, MnIer)
    Call WriteWerte(TblWerte, ParamAlle, MnIer)
    '
    '
    '
    'R-Werte
    '
    '
    'Tabelle für R-Werte
    '
    '

    '
    Call WriteRwerte(TblRwerte, GrpRwerte, MnIer)

    '

    '
    '
    'Farb-/Bindemittel
    '
    '
    '

    Call WriteFarben(TblFarben, Rezsozpt, MnIer)
    '
    '
    '
    'Rezepte
    '
    '
    '

    Call WriteRezepte(TblRezepte, Rezsozpt, MnIer)
    '
    '
    '
    'Mengen
    '

    '
    '
    Call WriteMengen(TblMengen, Rezsozpt, MnIer)
    '
    '
    'WerteAllg
    '

    '
    '
    Call WriteWerteAllg(Menueparam, TblWerteAllg, ParamAlle, MnIer)
    Aform.ShowDialog()
  End Sub
  'Sub Korrekt(Menueparam As AlleParameter, RzNr As String, Rezsozpt As Rezeptes, ParamAlle As WertWeisngs, GrpRwerte As RwerteGrp)
  Sub Korrekt(Menueparam As AllParameters, Rezsozpt As RecipesGrp, ParamAlle As ValuesGrpsAssigns, GrpRwerte As RefValuesGrp)
    Dim Aform As New frmUserAusgabe
    '
    '
    '
    '
    '
    'UserID,MessgID,MethID,MischID
    'Name Normlichtarten
    '
    Call WriteMenueTables(TblMenue, Menueparam, ParamAlle, MnIer)
    '
    '
    '
  
    '

    '
    'Merkmale der Anweisungen
    '
    '
    '
    Call WriteAnweisng(TblAnWeisng, ParamAlle, MnIer)
    Call WriteMerk(TblMerk, ParamAlle, MnIer)
    Call WriteWeisngRwerte(TblWeisngRwerte, ParamAlle, MnIer)
    Call WriteNormLicht(TblNormLicht, ParamAlle, MnIer)
    Call WriteWinkel(TblWinkel, ParamAlle, MnIer)
    Call WriteWerte(TblWerte, ParamAlle, MnIer)
    '
    '
    '
    'R-Werte
    '
    '
    'Tabelle für R-Werte
    '
    '

    '
    Call WriteRwerte(TblRwerte, GrpRwerte, MnIer)

    '
    '
    'Farb-/Bindemittel
    '
    '
    '

    Call WriteFarben(TblFarben, Rezsozpt, MnIer)
    '
    '
    '
    'Rezepte
    '
    '
    '

    Call WriteRezepte(TblRezepte, Rezsozpt, MnIer)
    '
    'Mengen
    '
    '
    '
    Call WriteMengen(TblMengen, Rezsozpt, MnIer)
    '
    '
    '
    'WerteAllg
    '

    '
    '
    Call WriteWerteAllg(Menueparam, TblWerteAllg, ParamAlle, MnIer)
    Aform.ShowDialog()
  End Sub

  'Sub Rezalle(Menueparam As AlleParameter, Kzl As Integer, Kzal() As Integer, Rezsozpt As Rezeptes, ParamAlle As WertWeisngs, GrpRwerte As RwerteGrp)
  Sub Rezalle(Menueparam As AllParameters, Rezsozpt As RecipesGrp, ParamAlle As ValuesGrpsAssigns, GrpRwerte As RefValuesGrp)
    Dim Aform As New frmUserAusgabe
    '
    '
    '
    '
    '
    'UserID,MessgID,MethID,MischID
    'Name Normlichtarten
    '
    Call WriteMenueTables(TblMenue, Menueparam, ParamAlle, MnIer)
    '
    '
    '
    'Tabellen für Merkmale der Anweisungen
    '
    '
 

    '
    'Merkmale der Anweisungen
    '
    '
    '
    Call WriteAnweisng(TblAnWeisng, ParamAlle, MnIer)
    Call WriteMerk(TblMerk, ParamAlle, MnIer)
    Call WriteWeisngRwerte(TblWeisngRwerte, ParamAlle, MnIer)
    Call WriteNormLicht(TblNormLicht, ParamAlle, MnIer)
    Call WriteWinkel(TblWinkel, ParamAlle, MnIer)
    Call WriteWerte(TblWerte, ParamAlle, MnIer)
    '
    '
    '
    'R-Werte
    '
    '
    'Tabelle für R-Werte
    '
    '

    '
    Call WriteRwerte(TblRwerte, GrpRwerte, MnIer)

    '
    '
    '
    'Farb-/Bindemittel
    '
    '

    Call WriteFarben(TblFarben, Rezsozpt, MnIer)
    '
    '
    '
    'Rezepte
    '
    '
    '

    Call WriteRezepte(TblRezepte, Rezsozpt, MnIer)
    '
    '
    '
    'Mengen
    '
    '
    '
    Call WriteMengen(TblMengen, Rezsozpt, MnIer) '
    '
    '
    '
    'WerteAllgemein (Werte mit zugehörigen Texten
    '
    '
    '
    Call WriteWerteAllg(Menueparam, TblWerteAllg, ParamAlle, MnIer) '
    Aform.ShowDialog()
  End Sub


  Sub FARBMIT(Menueparam As AllParameters)

  End Sub
  Sub SORTIME(Menueparam As AllParameters)

  End Sub
  Sub REZEPTE(Menueparam As AllParameters)

  End Sub
  Sub REWERTE(Menueparam As AllParameters)

  End Sub
  Sub ALLGEME(Menueparam As AllParameters)

  End Sub
End Class


