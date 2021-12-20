Option Strict Off
Option Explicit On
Option Compare Text

'
Public Class AusgabeCreateObj
  Implements IDisposable
  Public Sub New()
    MyBase.New()
    disposed = False
  End Sub
  Protected Overrides Sub Finalize()
    MyBase.Finalize()
    dispose()
  End Sub

  Sub dispose() Implements IDisposable.Dispose
    If disposed Then Exit Sub
    quali = Nothing
    disposed = True

  End Sub

  Dim i As Short
  Dim quali As QualKontrolle
  Dim Ausgabeobject As Object
  Dim ObjectString As String
  Dim disposed As Boolean
 


  Sub AusgabeRZPALLE(ByRef RezSozpt As RecipesGrp, ByRef FarbWrt As ValuesGrpsAssigns, ByRef GrpRwerte As RefValuesGrp)
    ObjectString = Trim(PrivSettings("COMCLASS", "RZPALLE", "", COLORFileName()))
    If ObjectString <> "" Then
      Try


        '
        '
        '
        Ausgabeobject = CreateObject(ObjectString)
      Catch ex As Exception


        MsgBox(ex.Message & ": " & ObjectString)
        Ausgabeobject = Nothing
        Exit Sub
      End Try
      '
      '
      Try

        '
        '
        Ausgabeobject.Rezalle(MenueParam, RezSozpt, FarbWrt, GrpRwerte)

      Catch ex As Exception


        MsgBox(ex.Message & ": " & ObjectString)
        Ausgabeobject = Nothing
      End Try
    Else
      MsgBox(Texxt(2948))
    End If
  End Sub
  Sub AusgabeWIEGESC(ByRef RezSozpt As RecipesGrp, ByRef FarbWrt As ValuesGrpsAssigns, ByRef GrpRwerte As RefValuesGrp)
    ObjectString = Trim(PrivSettings("COMCLASS", "WIEGESC", "", COLORFileName()))
    If ObjectString <> "" Then
      Try


        '
        '
        Ausgabeobject = CreateObject(ObjectString)
      Catch ex As Exception

        MsgBox(ex.Message & ": " & ObjectString)
        Ausgabeobject = Nothing
        Exit Sub
      End Try
      '
      '
      '
      Try

        Call Ausgabeobject.Wiegesc(menueparam, RezSozpt, FarbWrt, GrpRwerte)

      Catch ex As Exception

        MsgBox(ex.Message & ": " & ObjectString)
        Ausgabeobject = Nothing

      End Try
    Else
      MsgBox(Texxt(2948))
    End If
  End Sub
  Sub AusgabeALLGEME()
    ObjectString = Trim(PrivSettings("COMCLASS", "ALLGEME", "", COLORFileName()))
    If ObjectString <> "" Then
      Try

        '
        '
        Ausgabeobject = CreateObject(ObjectString)

      Catch ex As Exception

        MsgBox(ex.Message & ": " & ObjectString)
        Ausgabeobject = Nothing
        Exit Sub
      End Try
      '
      '
      '
      '
      Try


        '
        Ausgabeobject.ALLGEME(MenueParam)
      Catch ex As Exception
        MsgBox(ex.Message & ": " & ObjectString)
        Ausgabeobject = Nothing
      End Try
    Else
      MsgBox(Texxt(2948))
    End If
  End Sub
  Sub AusgabeQUALITA(ByRef paramalle As ValuesGrpsAssigns, ByRef GrpRwerte As RefValuesGrp)
    ObjectString = Trim(PrivSettings("COMCLASS", "QUALITA", "", COLORFileName()))
    '
    'ObjectString = "BCSWINUserProject.SchreibeDaten"
    If ObjectString <> "" Then
      Try


        '
        '
        '
        'ObjectString = "BCSWINUsertesten.SchreibeDaten"

        Ausgabeobject = CreateObject(ObjectString)
      Catch ex As Exception
        MsgBox(ex.Message & ": " & ObjectString)
        Ausgabeobject = Nothing
        Exit Sub
      End Try
      '
      '
      '
      '
      Try

        Ausgabeobject.qualita(MenueParam, paramalle, GrpRwerte)
      Catch ex As Exception
        MsgBox(ex.Message & ": " & ObjectString)
        Ausgabeobject = Nothing
      End Try
    Else
      MsgBox(Texxt(2948))
    End If
  End Sub
  Sub AusgabeKORREKT(ByRef RezSozpt As RecipesGrp, ByRef FarbWrt As ValuesGrpsAssigns, ByRef GrpRwerte As RefValuesGrp)
    ObjectString = Trim(PrivSettings("COMCLASS", "KORREKT", "", COLORFileName()))
    If ObjectString <> "" Then
      Try

        '
        '
        Ausgabeobject = CreateObject(ObjectString)
      Catch ex As Exception
        MsgBox(ex.Message & ": " & ObjectString)
        Ausgabeobject = Nothing
        Exit Sub
      End Try

      '
      '
      Try

        '
        Ausgabeobject.KORREKT(MenueParam, RezSozpt, FarbWrt, GrpRwerte)
      Catch ex As Exception
        MsgBox(ex.Message & ": " & ObjectString)
        Ausgabeobject = Nothing
      End Try
    Else
      MsgBox(Texxt(2948))
    End If
  End Sub
  Sub AusgabeFARBMIT()
    ObjectString = Trim(PrivSettings("COMCLASS", "FARBMIT", "", COLORFileName()))
    If ObjectString <> "" Then
      Try

        '
        '
        '
        Ausgabeobject = CreateObject(ObjectString)

      Catch ex As Exception

        MsgBox(ex.Message & ": " & ObjectString)
        Ausgabeobject = Nothing
        Exit Sub
      End Try
      '
      '
      '
      '
      Try


        Ausgabeobject.FARBMIT(MenueParam)
      Catch ex As Exception
        MsgBox(ex.Message & ": " & ObjectString)
        Ausgabeobject = Nothing
      End Try
    Else
      MsgBox(Texxt(2948))
    End If
  End Sub
  Sub AusgabeSORTIME()
    ObjectString = Trim(PrivSettings("COMCLASS", "SORTIME", "", COLORFileName()))
    If ObjectString <> "" Then
      Try


        '
        '
        Ausgabeobject = CreateObject(ObjectString)
      Catch ex As Exception
        '
        MsgBox(ex.Message & ": " & ObjectString)
        Ausgabeobject = Nothing
        Exit Sub

      End Try
      '
      '
      '
      '
      Try


        Ausgabeobject.SORTIME(MenueParam)
      Catch ex As Exception
        MsgBox(ex.Message & ": " & ObjectString)
        Ausgabeobject = Nothing
      End Try
    Else
      MsgBox(Texxt(2948))
    End If
  End Sub
  Sub AusgabeREZEPTE()
    ObjectString = Trim(PrivSettings("COMCLASS", "REZEPTE", "", COLORFileName()))
    If ObjectString <> "" Then
      Try
        '
        '
        Ausgabeobject = CreateObject(ObjectString)
      Catch ex As Exception

        MsgBox(ex.Message & ": " & ObjectString)
        Ausgabeobject = Nothing
        Exit Sub

      End Try
      '
      '
      '
      '
      Try


        Ausgabeobject.Rezepte(MenueParam)
      Catch ex As Exception
        MsgBox(ex.Message & ": " & ObjectString)
        Ausgabeobject = Nothing
      End Try
    Else
      MsgBox(Texxt(2948))
    End If
  End Sub
  Sub AusgabeREWERTE()
    ObjectString = Trim(PrivSettings("COMCLASS", "REWERTE", "", COLORFileName()))
    If ObjectString <> "" Then
      Try

        '
        '
        Ausgabeobject = CreateObject(ObjectString)

      Catch ex As Exception

        MsgBox(ex.Message & ": " & ObjectString)
        Ausgabeobject = Nothing
        Exit Sub

      End Try
      '
      '
      '
      '
      Try

        Ausgabeobject.REWERTE(MenueParam)
      Catch ex As Exception
        MsgBox(ex.Message & ": " & ObjectString)
        Ausgabeobject = Nothing
      End Try
    Else
      MsgBox(Texxt(2948))
    End If
  End Sub

  Sub Ausgabemerkmale(ByRef paramalle As ValuesGrpsAssigns, ByRef GrpRwerte As RefValuesGrp)
    'frmDRQ.DefInstance.LetParameter(  GrpRwerte, paramalle)
    'frmDRQ.DefInstance.Show()
  End Sub




End Class
Public Class CreateMessgeraet
  Sub CreateMeasure(ByRef Measure As Object)
    Dim ObjName As String
    Dim ObjType As Type
    '
    '
    'COMCLASS-X Komponenten Messger‰t
    '
    '
    '
    ObjName = Trim(PrivSettings("COMCLASS", "MEASURE", "", COLORFileName()))
    If ObjName = "" Then
      '
      '
      'Bei fehlendem Eintrag wird Standardmeﬂprogramm verwendet
      '
      '
      '
      '
      ObjName = "COLORMeasure.MeasureReflex"

    End If
    '
    '
    ObjType = Type.GetType(ObjName)
    Measure = Activator.CreateInstance(ObjType)
  End Sub
End Class
