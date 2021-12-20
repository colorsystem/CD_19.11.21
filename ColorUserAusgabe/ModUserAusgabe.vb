Option Compare Text
Option Explicit On
Option Strict Off
Module ModUserAusgabe
  Public MnIer As Integer
  Public TblMenue() As DataTable
  Public TblAnWeisng As DataTable
  Public TblWeisngRwerte As DataTable
  Public TblMerk As DataTable
  Public TblNormLicht As DataTable
  Public TblWinkel As DataTable
  Public TblWerte As DataTable
  Public TblFarben As DataTable
  Public TblRezepte As DataTable
  Public TblMengen As DataTable
  Public TblWerteAllg As DataTable
  '
  '
  Public TblRwerte As DataTable


  Dim Rwrt As Object
  Dim KeyD As String
  Dim FaID As String
  Dim ID As Long
  Dim Rhilf() As Single
  Dim hlf As Object
  Const WertLae = 16
  Sub main()

  End Sub


  Sub CreateFarben(TblName As String, ByRef TblFarben As DataTable, ier As Integer)
    TblFarben = New DataTable
    TblFarben.TableName = TblName
    TblFarben.Columns.Add("Farbm_ID", GetType(Integer))
    TblFarben.Columns.Add("Farbm_Name", GetType(String))
    TblFarben.Columns.Add("Farbm_Bem", GetType(String))
    TblFarben.Columns.Add("Farbm_Aname", GetType(String))
    TblFarben.Columns.Add("Farbm_Form", GetType(String))
    TblFarben.Columns.Add("Farbm_Kto", GetType(String))
    TblFarben.Columns.Add("Farbm_OP", GetType(String))
    TblFarben.Columns.Add("Farbm_Bumng", GetType(Single))
    TblFarben.Columns.Add("Farbm_BoMng", GetType(Single))
    TblFarben.Columns.Add("Farbm_PrNr", GetType(String))
    TblFarben.Columns.Add("Farbm_Fst", GetType(Single))
    TblFarben.Columns.Add("Farbm_Ichf", GetType(Integer))
    TblFarben.Columns.Add("Farbm_Spz", GetType(Single))
    TblFarben.Columns.Add("Farbm_Eff", GetType(Single))
    TblFarben.Columns.Add("Farbm_Ibas", GetType(Integer))
    TblFarben.Columns.Add("Farbm_Preis", GetType(Single))
    TblFarben.Columns.Add("Farbm_FarbID", GetType(Integer))
    TblFarben.Columns.Add("Farbm_Glzgrd", GetType(Single))
    TblFarben.Columns.Add("Glzgrd_ID", GetType(Integer))

  End Sub

  Sub CreateMenueTables(TblName() As String, ByRef TblMenue() As DataTable, ier As Integer)
    Call CreateUsMesMetMisID(TblName(0), TblMenue(0), ier)
    Call CreateNormKbez(TblName(1), TblMenue(1), ier)

  End Sub

  Sub WriteFarben(ByRef TblFarben As DataTable, Rezsozpt As RecipesGrp, ier As Integer)
    Dim i As Integer
    Dim RRow As DataRow
    ier = 0
    TblFarben.Rows.Clear()
    For i = 0 To Rezsozpt.Farben.FarbCount - 1
      RRow = TblFarben.NewRow
      RRow("Farbm_ID") = Rezsozpt.Farben(i).ID
      RRow("Farbm_name") = Rezsozpt.Farben(i).Name
      RRow("Farbm_Aname") = Rezsozpt.Farben(i).Aname
      RRow("Farbm_Bem") = Rezsozpt.Farben(i).Bem
      RRow("Farbm_Form") = Rezsozpt.Farben(i).Form
      RRow("Farbm_Kto") = Rezsozpt.Farben(i).Kto
      RRow("Farbm_OP") = Rezsozpt.Farben(i).OP
      RRow("Farbm_BuMng") = Rezsozpt.Farben(i).BuMng
      RRow("Farbm_BoMng") = Rezsozpt.Farben(i).BoMng
      RRow("Farbm_PrNr") = Rezsozpt.Farben(i).PrNr
      RRow("Farbm_Fst") = Rezsozpt.Farben(i).Fst
      RRow("Farbm_Ichf") = Rezsozpt.Farben(i).Ichf
      RRow("Farbm_Spz") = Rezsozpt.Farben(i).Spz
      RRow("Farbm_Eff") = Rezsozpt.Farben(i).Eff
      RRow("Farbm_Ibas") = Rezsozpt.Farben(i).Ibas
      RRow("Farbm_Preis") = Rezsozpt.Farben(i).Preis
      RRow("Farbm_FarbID") = Rezsozpt.Farben(i).FarbID
      RRow("Farbm_Glzgrd") = Rezsozpt.Farben(i).GlzGrd
      RRow("Glzgrd_ID") = Rezsozpt.Farben(i).GlzGrdID
      TblFarben.Rows.Add(RRow)
    Next i
  End Sub
  Sub WriteUsMesMetMisID(ByRef TblUsMesMetMis As DataTable, Menueparam As AllParameters, ParamAlle As ValuesGrpsAssigns, ier As Integer)
    Dim i As Integer
    Dim RRow As DataRow
    ier = 0
    TblUsMesMetMis.Rows.Clear()
    RRow = TblUsMesMetMis.NewRow
    RRow("UserID") = Menueparam.UserID
    RRow("MessgID") = Menueparam.MessgID
    RRow("MethID") = Menueparam.MethID
    RRow("MischID") = Menueparam.MischID
    RRow("PruefmID") = ParamAlle.PruefID
    RRow("ProArtID") = ParamAlle.ProArtID
    RRow("Komment") = ParamAlle.Kommentar
    TblUsMesMetMis.Rows.Add(RRow)
  End Sub
  Sub WriteMenueTables(ByRef TblMenue() As DataTable, Menueparam As AllParameters, ParamAlle As ValuesGrpsAssigns, ier As Integer)
    Call WriteUsMesMetMisID(TblMenue(0), Menueparam, ParamAlle, ier)
    Call WriteNormKbez(TblMenue(1), Menueparam, ier)
  End Sub
 

 

  Sub CreateRezepte(TblName As String, ByRef TblRezepte As DataTable, ier As Integer)
    TblRezepte = New DataTable
    TblRezepte.TableName = TblName
    TblRezepte.Columns.Add("Rezept_ID", GetType(Integer))
    TblRezepte.Columns.Add("Rezept_Name", GetType(String))
    TblRezepte.Columns.Add("Rezept_Bem", GetType(String))
    TblRezepte.Columns.Add("Rezept_KeyID", GetType(String))
    TblRezepte.Columns.Add("Rezept_Nr", GetType(Integer))
    TblRezepte.Columns.Add("Rezept_Dattim", GetType(Date))
    TblRezepte.Columns.Add("Rezept_Dik1", GetType(Single))
    TblRezepte.Columns.Add("Rezept_Dik2", GetType(Single))
    TblRezepte.Columns.Add("Rezept_Gid", GetType(Integer))
    TblRezepte.Columns.Add("Rezept_Iarch", GetType(Integer))
  End Sub

  Sub WriteRezepte(ByRef TblRezepte As DataTable, Rezsozpt As RecipesGrp, ier As Integer)
    Dim i As Integer
    Dim RRow As DataRow
    ier = 0
    TblRezepte.Rows.Clear()
    For i = 0 To Rezsozpt.Rezepte.RezCount - 1
      RRow = TblRezepte.NewRow
      RRow("Rezept_ID") = Rezsozpt.Rezepte(i).ID
      RRow("Rezept_KeyID") = Rezsozpt.Rezepte.RezKey(i)
      RRow("Rezept_Nr") = Rezsozpt.Rezepte(i).Nr
      RRow("Rezept_Name") = Rezsozpt.Rezepte(i).Name
      RRow("Rezept_Bem") = Rezsozpt.Rezepte(i).Bem
      RRow("Rezept_Dattim") = Rezsozpt.Rezepte(i).DatTim
      RRow("Rezept_Dik1") = Rezsozpt.Rezepte(i).Dicke(0)
      RRow("Rezept_Dik2") = Rezsozpt.Rezepte(i).Dicke(1)
      RRow("Rezept_Gid") = Rezsozpt.Rezepte(i).Gid
      RRow("Rezept_Iarch") = Rezsozpt.Rezepte(i).Iarch
      TblRezepte.Rows.Add(RRow)
    Next i
  End Sub
 
  Sub CreateMengen(TblName As String, ByRef TblMengen As DataTable, ier As Integer)
    TblMengen = New DataTable
    TblMengen.TableName = TblName
    TblMengen.Columns.Add("Rezept_KeyID", GetType(String))
    TblMengen.Columns.Add("Farbm_ID", GetType(Integer))
    TblMengen.Columns.Add("Farbm_FaAmng", GetType(Single))
    TblMengen.Columns.Add("Farbm_BaAmng", GetType(Single))
    TblMengen.Columns.Add("Farbm_Prob", GetType(Single))
    TblMengen.Columns.Add("Farbm_Proz", GetType(Single))
  End Sub

  Sub WriteMengen(ByRef TblMengen As DataTable, Rezsozpt As RecipesGrp, ier As Integer)
    Dim i As Integer
    Dim k As Integer
    Dim RRow As DataRow
    ier = 0
    TblMengen.Rows.Clear()
    For k = 0 To Rezsozpt.Rezepte.RezCount - 1
      For i = 0 To Rezsozpt.Rezepte(k).KF - 1
        RRow = TblMengen.NewRow
        RRow("Rezept_KeyID") = Rezsozpt.Rezepte.RezKey(k)
        RRow("Farbm_ID") = Rezsozpt.Rezepte(k)(i).ID
        RRow("Farbm_FaAmng") = Rezsozpt.Rezepte(k)(i).FaAmng
        RRow("Farbm_BaAmng") = Rezsozpt.Rezepte(k)(i).BaAmng
        RRow("Farbm_Prob") = Rezsozpt.Rezepte(k)(i).Prob
        RRow("Farbm_Proz") = Rezsozpt.Rezepte(k)(i).Proz
        TblMengen.Rows.Add(RRow)
      Next i
    Next k
  End Sub
 

  Sub CreateRwerte(TblName As String, ByRef TblRwerte As DataTable, ier As Integer)
    TblRwerte = New DataTable
    TblRwerte.TableName = TblName
    TblRwerte.Columns.Add("Rwert_KeyID", GetType(String))
    TblRwerte.Columns.Add("Rwert_ID", GetType(Integer))
    TblRwerte.Columns.Add("Rwert_StrRw", GetType(String))
    TblRwerte.Columns.Add("Rwert_Name", GetType(String))
    TblRwerte.Columns.Add("Rwert_Bem", GetType(String))
    TblRwerte.Columns.Add("Rwert_Banum", GetType(String))
    TblRwerte.Columns.Add("Rwert_Cme", GetType(String))
    TblRwerte.Columns.Add("Rwert_St", GetType(Integer))
    TblRwerte.Columns.Add("Rwert_Gid", GetType(Integer))
    TblRwerte.Columns.Add("Rwert_Dattim", GetType(Date))
    TblRwerte.Columns.Add("Rwert_Itp", GetType(Boolean))
    TblRwerte.Columns.Add("Rwert_kwb", GetType(Integer))
    TblRwerte.Columns.Add("Rwert_Nr", GetType(Integer))
    TblRwerte.Columns.Add("Rwert_IVoNa", GetType(Boolean))
    TblRwerte.Columns.Add("Rwert_Chrm", GetType(String))
    TblRwerte.Columns.Add("Rwert_Rwert", GetType(Byte()))
  End Sub

  Sub WriteRwerte(ByRef TblRwerte As DataTable, GrpRwerte As RefValuesGrp, ier As Integer)
    Dim i As Integer
    Dim j As Integer
    Dim kw As Integer
    Dim RRow As DataRow
    ier = 0
    TblRwerte.Rows.Clear()
    For i = 0 To GrpRwerte.Count - 1
      For j = 0 To GrpRwerte(i).Count - 1
        For kw = 0 To GrpRwerte(i)(j).RefKurv.Count - 1
          If GrpRwerte(i)(j).ID >= 0 Then
            RRow = TblRwerte.NewRow
            RRow("Rwert_KeyID") = GrpRwerte(i).RwKey(j)
            RRow("Rwert_ID") = GrpRwerte(i)(j).ID
            RRow("Rwert_StrRw") = GrpRwerte(i)(j).Nr
            RRow("Rwert_Name") = GrpRwerte(i)(j).Name
            RRow("Rwert_Bem") = GrpRwerte(i)(j).Bem
            RRow("Rwert_Banum") = GrpRwerte(i)(j).Banum
            RRow("Rwert_Cme") = GrpRwerte(i)(j).Cme
            RRow("Rwert_St") = GrpRwerte(i)(j).ReTr
            RRow("Rwert_Gid") = GrpRwerte(i)(j).Gid
            RRow("Rwert_Dattim") = GrpRwerte(i)(j).DatTim
            RRow("Rwert_Itp") = GrpRwerte(i)(j).Itp
            RRow("Rwert_Ivona") = GrpRwerte(i)(j).IVoNa
            RRow("Rwert_Nr") = GrpRwerte(i)(j).Nr
            RRow("Rwert_kwb") = GrpRwerte(i)(j).kwb
            RRow("Rwert_Chrm") = GrpRwerte(i)(j).RefKurv.Winkey(kw)
            ' RRow("Rwert_rwert") = GrpRwerte(i)(j).RefKurv(kw).R
            TblRwerte.Rows.Add(RRow)
          End If
        Next kw
      Next j
    Next i
  End Sub
  
  Sub CreateAnweisng(TblName As String, ByRef TblAnWeisng As DataTable, ier As Integer)
    TblAnWeisng = New DataTable
    TblAnWeisng.TableName = TblName
    TblAnWeisng.Columns.Add("AnwsgID", GetType(Integer))
    TblAnWeisng.Columns.Add("AufgArt", GetType(String))
    TblAnWeisng.Columns.Add("AufgName", GetType(String))
  End Sub
  '
  Sub CreateUsMesMetMisID(TblName As String, ByRef TblUsMesMetMis As DataTable, ier As Integer)
    TblUsMesMetMis = New DataTable
    TblUsMesMetMis.TableName = TblName
    TblUsMesMetMis.Columns.Add("UserID", GetType(Integer))
    TblUsMesMetMis.Columns.Add("MessgID", GetType(Integer))
    TblUsMesMetMis.Columns.Add("MethID", GetType(Integer))
    TblUsMesMetMis.Columns.Add("MischID", GetType(Integer))
    TblUsMesMetMis.Columns.Add("PruefmID", GetType(Integer))
    TblUsMesMetMis.Columns.Add("ProartID", GetType(Integer))
    TblUsMesMetMis.Columns.Add("Komment", GetType(String))
  End Sub

  Sub WriteAnweisng(ByRef TblAnWeisng As DataTable, ParamAlle As ValuesGrpsAssigns, ier As Integer)
    Dim i As Integer
    Dim ianwsg As Integer
    Dim RRow As DataRow
    ier = 0
    TblAnWeisng.Rows.Clear()
    For Ianwsg = 0 To ParamAlle.Count - 1
      RRow = TblAnWeisng.NewRow
      RRow("AnwsgID") = ParamAlle(Ianwsg).AnwsgID
      RRow("AufgArt") = ParamAlle(Ianwsg).AufgArt
      RRow("AufgName") = ParamAlle(Ianwsg).AnwsgName
      TblAnWeisng.Rows.Add(RRow)
    Next Ianwsg
  End Sub
  

  Sub CreateMerk(TblName As String, ByRef TblMerk As DataTable, ier As Integer)
    TblMerk = New DataTable
    TblMerk.TableName = TblName
    TblMerk.Columns.Add("AnwsgID", GetType(Integer))
    TblMerk.Columns.Add("Ipos", GetType(Integer))
    TblMerk.Columns.Add("Merk_ID", GetType(Integer))
    TblMerk.Columns.Add("Merk_ken", GetType(String))
    TblMerk.Columns.Add("Merk_Kbez", GetType(String))
    TblMerk.Columns.Add("Merk_Form", GetType(String))
    TblMerk.Columns.Add("Merk_Fakt", GetType(Single))
    TblMerk.Columns.Add("Merk_Typ", GetType(String))
  End Sub

  Sub WriteMerk(ByRef TblMerk As DataTable, ParamAlle As ValuesGrpsAssigns, ier As Integer)
    Dim i As Integer
    Dim Ianwsg As Integer
    Dim RRow As DataRow
    ier = 0
    TblMerk.Rows.Clear()
    For Ianwsg = 0 To ParamAlle.Count - 1
      For i = 0 To ParamAlle(Ianwsg).CountMerk - 1
        RRow = TblMerk.NewRow
        RRow("AnwsgID") = ParamAlle(Ianwsg).AnwsgID
        RRow("ipos") = i
        RRow("Merk_ID") = ParamAlle(Ianwsg).Merk(i).ID
        RRow("Merk_ken") = ParamAlle(Ianwsg).Merk(i).Ken
        RRow("Merk_Kbez") = ParamAlle(Ianwsg).Merk(i).Kbez
        RRow("Merk_Form") = ParamAlle(Ianwsg).Merk(i).Form
        RRow("Merk_Fakt") = ParamAlle(Ianwsg).Merk(i).Fakt
        RRow("Merk_Typ") = ParamAlle(Ianwsg).Merk(i).Typ
        TblMerk.Rows.Add(RRow)
      Next i
    Next Ianwsg
  End Sub

 
  Sub CreateWeisngRwerte(TblName As String, ByRef TblRwerte As DataTable, ier As Integer)
    TblRwerte = New DataTable
    TblRwerte.TableName = TblName
    TblRwerte.Columns.Add("AnwsgID", GetType(Integer))
    TblRwerte.Columns.Add("Rwert_Ipos", GetType(Integer))
    TblRwerte.Columns.Add("Rwert_Nr", GetType(Integer))
    TblRwerte.Columns.Add("Rwert_StrRw", GetType(String))
    TblRwerte.Columns.Add("Rwert_Name", GetType(String))
    TblRwerte.Columns.Add("Rwert_Itp", GetType(Boolean))
    TblRwerte.Columns.Add("Rwert_Dattim", GetType(Date))
  End Sub

  Sub WriteWeisngRwerte(ByRef TblWeisngRwerte As DataTable, ParamAlle As ValuesGrpsAssigns, ier As Integer)
    Dim i As Integer
    Dim Ianwsg As Integer
    Dim RRow As DataRow
    ier = 0
    TblWeisngRwerte.Rows.Clear()
    For Ianwsg = 0 To ParamAlle.Count - 1
      For i = 0 To ParamAlle(Ianwsg).Count - 1
        RRow = TblWeisngRwerte.NewRow
        RRow("AnwsgID") = ParamAlle(Ianwsg).AnwsgID
        RRow("Rwert_ipos") = i
        RRow("Rwert_Nr") = ParamAlle(Ianwsg)(i).Nr
        RRow("Rwert_StrRw") = ParamAlle(Ianwsg)(i).Nr
        RRow("Rwert_Name") = ParamAlle(Ianwsg)(i).Name
        RRow("Rwert_Itp") = ParamAlle(Ianwsg)(i).Itp
        RRow("Rwert_Dattim") = ParamAlle(Ianwsg)(i).Dattim
        TblWeisngRwerte.Rows.Add(RRow)
      Next i
    Next Ianwsg
  End Sub
 
  Sub CreateNormLicht(TblName As String, ByRef TblNormLicht As DataTable, ier As Integer)
    TblNormLicht = New DataTable
    TblNormLicht.TableName = TblName
    TblNormLicht.Columns.Add("Kpos", GetType(Integer))
    TblNormLicht.Columns.Add("Norm_KeyID", GetType(String))
  End Sub
  Sub CreateNormKbez(TblName As String, ByRef TblNormKbez As DataTable, ier As Integer)
    TblNormKbez = New DataTable
    TblNormKbez.TableName = TblName
    TblNormKbez.Columns.Add("Norm_KeyID", GetType(String))
    TblNormKbez.Columns.Add("Norm_Kbez", GetType(String))
  End Sub

  Sub WriteNormLicht(ByRef TblNormLicht As DataTable, ParamAlle As ValuesGrpsAssigns, ier As Integer)
    Dim i As Integer
    Dim k As Integer
    TblNormLicht.Rows.Clear()
    Dim RRow As DataRow
    ier = 0
    If ParamAlle.Count = 0 Then
      ier = 1
      Exit Sub
    End If
    If ParamAlle(0)(0).Count = 0 Then
      ier = 1
      Exit Sub
    End If
    For k = 0 To ParamAlle(0)(0).Count - 1
      RRow = TblNormLicht.NewRow
      RRow("Kpos") = k
      RRow("norm_KeyID") = ParamAlle(0)(0).IllKey(k)
      TblNormLicht.Rows.Add(RRow)
    Next k
  End Sub
  Sub WriteNormKbez(ByRef TblNormKbez As DataTable, Menueparam As AllParameters, ier As Integer)
    Dim k As Integer
    Dim RRow As DataRow
    TblNormKbez.Rows.Clear()
    ier = 0
    If Menueparam.Normfa.Nlz = 0 Then
      ier = 1
      Exit Sub
    End If
    RRow = TblNormKbez.NewRow
    For k = 0 To Menueparam.Normfa.Nlz - 1
      RRow = TblNormKbez.NewRow
      RRow("norm_KeyID") = KeyRe(Menueparam.Normfa(k).LichtID)
      RRow("norm_Kbez") = Menueparam.Normfa(k).NormNama
      TblNormKbez.Rows.Add(RRow)
    Next k
  End Sub

 

  Friend Sub CreateWinkel(TblName As String, ByRef TblWinkel As DataTable, ier As Integer)
    TblWinkel = New DataTable
    TblWinkel.TableName = TblName
    TblWinkel.Columns.Add("Kwpos", GetType(Integer))
    TblWinkel.Columns.Add("Winkel_KeyID", GetType(String))
  End Sub
  Friend Sub WriteWinkel(ByRef TblWinkel As DataTable, ParamAlle As ValuesGrpsAssigns, ier As Integer)
    Dim kw As Integer
    Dim RRow As DataRow
    ier = 0
    TblWinkel.Rows.Clear()
    If ParamAlle.Count = 0 Then
      ier = 1
      Exit Sub
    End If
    If ParamAlle(0)(0)(0).Count = 0 Then
      ier = 1
      Exit Sub
    End If

    For kw = 0 To ParamAlle(0)(0)(0).Count - 1
      RRow = TblWinkel.NewRow
      RRow("Kwpos") = kw
      RRow("Winkel_KeyID") = ParamAlle(0)(0)(0).Chrm(kw)
      TblWinkel.Rows.Add(RRow)
    Next kw
  End Sub
 
  Friend Sub CreateWerte(TblName As String, ByRef TblWerte As DataTable, ier As Integer)
    TblWerte = New DataTable
    TblWerte.TableName = TblName
    TblWerte.Columns.Add("AnwsgID", GetType(Integer))
    TblWerte.Columns.Add("Ipos", GetType(Integer))
    TblWerte.Columns.Add("Kpos", GetType(Integer))
    TblWerte.Columns.Add("Kwpos", GetType(Integer))
    TblWerte.Columns.Add("Wert_Ken", GetType(String))
    TblWerte.Columns.Add("Wert_Wert", GetType(String))
  End Sub
  
  Friend Sub WriteWerte(ByRef TblWerte As DataTable, ParamAlle As ValuesGrpsAssigns, ier As Integer)
    Dim i As Integer
    Dim k As Integer
    Dim j As Integer
    Dim Value As Object
    Dim kw As Integer
    Dim Ianwsg As Integer
    Dim RRow As DataRow
    ier = 0
    '
    'Werte
    '
    '
    '
    TblWerte.Rows.Clear()
    For Ianwsg = 0 To ParamAlle.Count - 1
      For i = 0 To ParamAlle(Ianwsg).Count - 1
        For k = 0 To ParamAlle(Ianwsg)(i).Count - 1
          For kw = 0 To ParamAlle(Ianwsg)(i)(k).Count - 1
            For j = 0 To ParamAlle(Ianwsg)(i)(k)(kw).Count - 1
              RRow = TblWerte.NewRow
              RRow("AnwsgID") = ParamAlle(Ianwsg).AnwsgID
              RRow("ipos") = i 'Nummer R-Werte
              RRow("Kpos") = k 'Nummer Lichtart
              RRow("Kwpos") = kw 'Nummer Winkel
              RRow("Wert_ken") = ParamAlle(Ianwsg).Merk(j).Ken
              Value = ParamAlle(Ianwsg)(i)(k)(kw)(ParamAlle(Ianwsg).Merk(j).Ken)
              If Not IsDBNull(ParamAlle(Ianwsg)(i)(k)(kw)(ParamAlle(Ianwsg).Merk(j).Ken)) AndAlso Not Double.IsNaN(Value) Then
                If Value <> "" And Trim(ParamAlle(Ianwsg).Merk(j).Ken) <> Trim(CStr(j)) Then
                  If ParamAlle(Ianwsg).Merk(RRow("Wert_ken")).Typ = "N" Then
                    If Len(ParamAlle(Ianwsg)(i)(k)(kw)(ParamAlle(Ianwsg).Merk(j).Ken)) < WertLae Then
                      ' RRow("wert_wert") = Format(ParamAlle(Ianwsg)(i)(k)(kw)(ParamAlle(Ianwsg).Merk(j).Ken), ParamAlle(Ianwsg).Merk(RRow("Wert_ken")).Form)
                      RRow("wert_wert") = ParamAlle(Ianwsg)(i)(k)(kw)(ParamAlle(Ianwsg).Merk(j).Ken)

                    Else
                      RRow("wert_wert") = ""
                    End If
                  Else
                    RRow("wert_wert") = ParamAlle(Ianwsg)(i)(k)(kw)(ParamAlle(Ianwsg).Merk(j).Ken)
                  End If
                Else
                  RRow("wert_wert") = ParamAlle(Ianwsg)(i)(k)(kw)(ParamAlle(Ianwsg).Merk(j).Ken)
                End If
              End If
              TblWerte.Rows.Add(RRow)
            Next j
          Next kw
        Next k
      Next i
    Next Ianwsg
  End Sub
  Friend Sub CreateWerteAllg(TblName As String, ByRef TblWerteAllg As DataTable, ier As Integer)
    TblWerteAllg = New DataTable
    TblWerteAllg.TableName = TblName
    TblWerteAllg.Columns.Add("AnwsgName", GetType(String))
    TblWerteAllg.Columns.Add("RwName", GetType(String))
    TblWerteAllg.Columns.Add("Licht", GetType(String))
    TblWerteAllg.Columns.Add("Winkel", GetType(Integer))
    TblWerteAllg.Columns.Add("Wert_Kbez", GetType(String))
    TblWerteAllg.Columns.Add("Wert_Wert", GetType(String))
  End Sub
  Friend Sub WriteWerteAllg(Menueparam As AllParameters, ByRef TblWerteAllg As DataTable, ParamAlle As ValuesGrpsAssigns, ier As Integer)
    Dim i As Integer
    Dim k As Integer
    Dim j As Integer
    Dim Value As Object
    Dim kw As Integer
    Dim Ianwsg As Integer
    Dim RRow As DataRow
    ier = 0
    '
    'Werte
    '
    TblWerteAllg.Rows.Clear()
    '
    '
    For Ianwsg = 0 To ParamAlle.Count - 1
      For i = 0 To ParamAlle(Ianwsg).Count - 1
        For k = 0 To ParamAlle(Ianwsg)(i).Count - 1
          For kw = 0 To ParamAlle(Ianwsg)(i)(k).Count - 1
            For j = 0 To ParamAlle(Ianwsg)(i)(k)(kw).Count - 1
              RRow = TblWerteAllg.NewRow
              RRow("AnwsgName") = ParamAlle(Ianwsg).AnwsgName
              RRow("RwName") = "(" & ParamAlle(Ianwsg)(i).Nr & ") " & ParamAlle(Ianwsg)(i).Name
              RRow("Licht") = Menueparam.Normfa(k).NormKenn 'Lichtart
              RRow("Winkel") = Menueparam.User.Winkel(kw).Chrm
              RRow("Wert_Kbez") = ParamAlle(Ianwsg).Merk(j).Kbez
              Value = ParamAlle(Ianwsg)(i)(k)(kw)(ParamAlle(Ianwsg).Merk(j).Ken)
              If Not IsDBNull(ParamAlle(Ianwsg)(i)(k)(kw)(ParamAlle(Ianwsg).Merk(j).Ken)) AndAlso Not Double.IsNaN(Value) Then
                If Value <> "" And Trim(ParamAlle(Ianwsg).Merk(j).Ken) <> Trim(CStr(j)) Then
                  If ParamAlle(Ianwsg).Merk(j).Typ = "N" Then
                    If Len(ParamAlle(Ianwsg)(i)(k)(kw)(ParamAlle(Ianwsg).Merk(j).Ken)) < WertLae Then
                      ' RRow("wert_wert") = Format(ParamAlle(Ianwsg)(i)(k)(kw)(ParamAlle(Ianwsg).Merk(j).Ken), ParamAlle(Ianwsg).Merk(RRow("Wert_ken")).Form)
                      RRow("wert_wert") = ParamAlle(Ianwsg)(i)(k)(kw)(ParamAlle(Ianwsg).Merk(j).Ken)
                    Else
                      RRow("wert_wert") = ""
                    End If
                  Else
                    RRow("wert_wert") = ParamAlle(Ianwsg)(i)(k)(kw)(ParamAlle(Ianwsg).Merk(j).Ken)
                  End If
                Else
                  RRow("wert_wert") = ParamAlle(Ianwsg)(i)(k)(kw)(ParamAlle(Ianwsg).Merk(j).Ken)
                End If
              End If
              TblWerteAllg.Rows.Add(RRow)
            Next j
          Next kw
        Next k
      Next i
    Next Ianwsg
  End Sub
  Sub C1GridtoClipboard(C1TrueGrid As C1TrueDBGrid)
    Dim row As Integer
    Dim col As Integer
    Dim strTemp As String
    strTemp = ""
    For row = 0 To C1TrueGrid.DataSource.rows.count - 1
      For col = 0 To C1TrueGrid.DataSource.columns.count - 1
        strTemp = strTemp & C1TrueGrid.DataSource.rows(row)(col).ToString & vbTab
      Next col
      strTemp = strTemp.Substring(0, strTemp.Length - 1) & vbCrLf
    Next row
    System.Windows.Forms.Clipboard.SetDataObject(strTemp, False)
  End Sub
End Module
