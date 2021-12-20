Option Strict Off
Option Explicit On
Option Compare Text


<ComClass(MeasureReflex.ClassId, MeasureReflex.InterfaceId, MeasureReflex.EventsId)> _
Public Class MeasureReflex
  Implements IDisposable

#Region "COM-GUIDs"
    ' Diese GUIDs stellen die COM-Identität für diese Klasse 
    ' und ihre COM-Schnittstellen bereit. Wenn Sie sie ändern, können vorhandene 
    ' Clients nicht mehr auf die Klasse zugreifen.
    Public Const ClassId As String = "f84ddff3-b5ca-46d0-8332-3a53e30e2822"
    Public Const InterfaceId As String = "f4e83134-02a8-400f-aa01-60f0ad75ce40"
    Public Const EventsId As String = "cb53554e-34a8-4567-8bf4-e48f3f8432bb"
#End Region
  ' Eine erstellbare COM-Klasse muss eine Public Sub New() 
  ' ohne Parameter aufweisen. Andernfalls wird die Klasse 
  ' nicht in der COM-Registrierung registriert und kann nicht 
  ' über CreateObject erstellt werden.


  Dim Formmessen As frmMeasDev


  Dim Measure As MeasureRefl
  Dim MnIer As Integer
  Public Sub New()
    MyBase.New()
    Formmessen = New frmMeasDev
  End Sub


  Sub MessDevMess(ByRef messg As MeasParameters, ByRef Refel As RefValue)
    MnIer = 0
    If IsNothing(clasm) Then
      MnIer = -5
      Formmessen.DialogResult = Windows.Forms.DialogResult.Abort
      Exit Sub
    End If
    Call Measure.UmspeiUseMethMessg(messg)

    Select Case MessgDriver
      Case Else
        Formmessen.MessgMenu = 0
        Formmessen.Refel = Refel
        Formmessen.ShowDialog()
        MnIer = Measure.ier
        If MnIer = 0 Then
          If Formmessen.DialogResult <> Windows.Forms.DialogResult.OK Then
            MnIer = -1
          End If
        End If
    End Select

  End Sub
    Sub MessDevKalib()
        MnIer = 0
    If IsNothing(clasm) Then
      MnIer = -5
      Formmessen.DialogResult = Windows.Forms.DialogResult.Abort
      Exit Sub
    End If
        Formmessen.MessgMenu = 1
        Formmessen.ShowDialog()
        MnIer = Measure.ier
        If MnIer = 0 Then
            If Formmessen.DialogResult <> Windows.Forms.DialogResult.OK And Formmessen.DialogResult <> Windows.Forms.DialogResult.Yes Then
                MnIer = -1
            End If
        End If
    End Sub

    Sub MessDevNkalib()
        MnIer = 0
    If IsNothing(clasm) Then
      MnIer = -5
      Formmessen.DialogResult = Windows.Forms.DialogResult.Abort
      Exit Sub
    End If
        Formmessen.MessgMenu = 2
        Formmessen.ShowDialog()
        MnIer = Measure.ier
        If MnIer = 0 Then
            If Formmessen.DialogResult <> Windows.Forms.DialogResult.OK Then
                MnIer = -1
            End If
        End If
    End Sub
    Sub MessDevInit()
    MnIer = 0
    If IsNothing(clasm) Then
      MnIer = -5
      Formmessen.DialogResult = Windows.Forms.DialogResult.Abort
      Exit Sub
    End If
        Formmessen.MessgMenu = 3
        Formmessen.ShowDialog()
        MnIer = Measure.ier
        If MnIer = 0 Then
            If Formmessen.DialogResult <> Windows.Forms.DialogResult.OK Then
                MnIer = -1
            End If
        End If
    End Sub

    Sub MessDevSond()
        MnIer = 0
    If IsNothing(clasm) Then
      MnIer = -5
      Formmessen.DialogResult = Windows.Forms.DialogResult.Abort
      Exit Sub
    End If
        Formmessen.MessgMenu = 4
        Formmessen.ShowDialog()
        MnIer = Measure.ier
        If MnIer = 0 Then
            If Formmessen.DialogResult <> Windows.Forms.DialogResult.OK Then
                MnIer = -1
            End If
        End If
    End Sub
    Sub MessDevManu(ByRef Refel As RefValue)
    MnIer = 0
    'If IsNothing(clasm) Then
    'MnIer = -5
    'Formmessen.DialogResult = Windows.Forms.DialogResult.Abort
    'Exit Sub
    'End If
    Formmessen.MessgMenu = 6
    Formmessen.Refel = Refel
    Formmessen.ShowDialog()
    MnIer = Measure.ier
    If MnIer = 0 Then
      If Formmessen.DialogResult <> Windows.Forms.DialogResult.OK Then
        MnIer = -1
      End If
    End If
    End Sub


    Public Sub Umspeich(ByRef Messg As MeasParameters, ByRef Norfa As NormIlluminat)
    If Not isnothing(measure) Then Exit Sub
    MnIer = 0
    WithClasm = True
    TableKalib = "TBL_KALIB"
    TblMeasDevice = "TBL_DEVICE"
    CmdKal = New OleDbCommand
    TblKal = New DataTable
    TblKal.TableName = "KalibName"
    MscMes = New SerialPort
    Measure = New MeasureRefl(Messg, Norfa)
    '        MscMes.Encoding() = System.Text.Encoding.ASCII

    MnIer = Measure.ier

    Call Measure.MesClasm()
    '
    '
    Formmessen.Measure = Measure

    If Measure.ier <> 0 Then
      MnIer = Measure.ier
      Exit Sub
    End If

    If MnIer = 0 Then
      MnIer = Measure.ier
    End If
  End Sub
    ReadOnly Property ier() As Integer
        Get
            ier = MnIer
        End Get
    End Property
    Sub dispose() Implements IDisposable.Dispose
        Measure.dispose()
        Formmessen.Dispose()
    End Sub

    Protected Overrides Sub Finalize()
        MyBase.Finalize()
    End Sub
End Class


