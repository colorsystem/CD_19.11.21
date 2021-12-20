Option Strict Off
Option Explicit On
Option Compare Text

Friend Class frmColorMDI
  Inherits System.Windows.Forms.Form
#Region "Vom Windows Form-Designer generierter Code "
  Public Sub New()
    MyBase.New()
    'Dieser Aufruf ist für den Windows Form-Designer erforderlich.
    InitializeComponent()
  End Sub
  'Das Formular überschreibt den Löschvorgang, um die Komponentenliste zu bereinigen.
  Protected Overloads Overrides Sub Dispose(ByVal Disposing As Boolean)
    Try
      If Disposing Then
        If Not components Is Nothing Then
          components.Dispose()
        End If
      End If
      PrintDialogMDI.Dispose()
      MyBase.Dispose(Disposing)
    Catch ex As Exception

    End Try
  End Sub
  'Wird vom Windows Form-Designer benötigt.
  Private components As System.ComponentModel.IContainer
  Friend WithEvents mnuQQ_00 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuQQ_01 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuQQ_02 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuQQ_03 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuQQ_04 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuQQ_05 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuQQ_06 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuQQ_07 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuQQ_08 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuQQ_09 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuQQ_10 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuQQ_11 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuQQ_12 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuQQ_13 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuQQ_14 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuQQ_15 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuQQ_16 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuQQ_17 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuQQ_18 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuQQ_19 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuQQ_20 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuQQ_21 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuQQ_22 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuQQ_23 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuQQ_24 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuQQ_25 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuQQ_26 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuQQ_27 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuQQ_28 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuQQ_29 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuQQ_30 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuQQ_31 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuQQ_32 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuQQ_33 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuQQ_34 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuQQ_35 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuQQ_36 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuQQ_37 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuQQ_38 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuQQ_39 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuQQ_40 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuQQ_41 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuQQ_42 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuQQ_43 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuQQ_44 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuQQ_45 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuQQ_46 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuQQ_47 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuQQ_48 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuQQ_49 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuRR_00 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuRR_01 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuRR_02 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuRR_03 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuRR_04 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuRR_05 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuRR_06 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuRR_07 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuRR_08 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuRR_09 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuRR_10 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuRR_11 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuRR_12 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuRR_13 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuRR_14 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuRR_15 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuRR_16 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuRR_17 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuRR_18 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuRR_19 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuRR_20 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuRR_21 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuRR_22 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuRR_23 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuRR_24 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuRR_25 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuRR_26 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuRR_27 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuRR_28 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuRR_29 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuRR_30 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuRR_31 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuRR_32 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuRR_33 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuRR_34 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuRR_35 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuRR_36 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuRR_37 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuRR_38 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuRR_39 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuRR_40 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuRR_41 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuRR_42 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuRR_43 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuRR_44 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuRR_45 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuRR_46 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuRR_47 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuRR_48 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuRR_49 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuSPZ_00 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuSPZ_01 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuSPZ_02 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuSPZ_03 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuSPZ_04 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuSPZ_05 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuSPZ_06 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuSPZ_07 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuSPZ_08 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuSPZ_09 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuSPZ_10 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuSPZ_11 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuSPZ_12 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuSPZ_13 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuSPZ_14 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuSPZ_15 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuSPZ_16 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuSPZ_17 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuSPZ_18 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuSPZ_19 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuSPZ_20 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuSPZ_21 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuSPZ_22 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuSPZ_23 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuSPZ_24 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuSPZ_25 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuSPZ_26 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuSPZ_27 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuSPZ_28 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuSPZ_29 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuSPZ_30 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuSPZ_31 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuSPZ_32 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuSPZ_33 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuSPZ_34 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuSPZ_35 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuSPZ_36 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuSPZ_37 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuSPZ_38 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuSPZ_39 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuSPZ_40 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuSPZ_41 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuSPZ_42 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuSPZ_43 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuSPZ_44 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuSPZ_45 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuSPZ_46 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuSPZ_47 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuSPZ_48 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuSPZ_49 As System.Windows.Forms.MenuItem
  'Hinweis: Die folgende Prozedur wird vom Windows Form-Designer benötigt.
  'Das Verändern mit dem Windows Form-Designer ist nicht möglich.
  'Das Verändern mit dem Code-Editor ist nicht möglich.
  Public WithEvents MainUsMethEinst As System.Windows.Forms.MainMenu
  Friend WithEvents mnuFenster As System.Windows.Forms.MenuItem
  Friend WithEvents PrintDialogMDI As System.Windows.Forms.PrintDialog
  Friend WithEvents mnuQC As System.Windows.Forms.MenuItem
  Friend WithEvents mnuRZ As System.Windows.Forms.MenuItem
  Friend WithEvents mnuSPE As System.Windows.Forms.MenuItem
  Friend WithEvents mnuUSE As System.Windows.Forms.MenuItem
  Friend WithEvents mnuDRU As System.Windows.Forms.MenuItem
  Public WithEvents mnuEI As System.Windows.Forms.MenuItem
  Friend WithEvents mnuEI_0 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuEI_1 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuEI_2 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuEI_3 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuEI_4 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuEI_5 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuEI_6 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuMSG As System.Windows.Forms.MenuItem
  Friend WithEvents mnuMSG_0 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuMSG_1 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuMSG_2 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuMSG_3 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuMSG_4 As System.Windows.Forms.MenuItem
  Friend WithEvents mnuMSG_5 As System.Windows.Forms.MenuItem
  Friend WithEvents menCon_0 As System.Windows.Forms.MenuItem
  Friend WithEvents menCon_1 As System.Windows.Forms.MenuItem
  Friend WithEvents menCon_2 As System.Windows.Forms.MenuItem
  Friend WithEvents menContext As System.Windows.Forms.ContextMenu
  Friend WithEvents mnuEN As System.Windows.Forms.MenuItem



  <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
    Me.components = New System.ComponentModel.Container()
    Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(frmColorMDI))
    Me.MainUsMethEinst = New System.Windows.Forms.MainMenu(Me.components)
    Me.mnuQC = New System.Windows.Forms.MenuItem()
    Me.mnuQQ_00 = New System.Windows.Forms.MenuItem()
    Me.mnuQQ_01 = New System.Windows.Forms.MenuItem()
    Me.mnuQQ_02 = New System.Windows.Forms.MenuItem()
    Me.mnuQQ_03 = New System.Windows.Forms.MenuItem()
    Me.mnuQQ_04 = New System.Windows.Forms.MenuItem()
    Me.mnuQQ_05 = New System.Windows.Forms.MenuItem()
    Me.mnuQQ_06 = New System.Windows.Forms.MenuItem()
    Me.mnuQQ_07 = New System.Windows.Forms.MenuItem()
    Me.mnuQQ_08 = New System.Windows.Forms.MenuItem()
    Me.mnuQQ_09 = New System.Windows.Forms.MenuItem()
    Me.mnuQQ_10 = New System.Windows.Forms.MenuItem()
    Me.mnuQQ_11 = New System.Windows.Forms.MenuItem()
    Me.mnuQQ_12 = New System.Windows.Forms.MenuItem()
    Me.mnuQQ_13 = New System.Windows.Forms.MenuItem()
    Me.mnuQQ_14 = New System.Windows.Forms.MenuItem()
    Me.mnuQQ_15 = New System.Windows.Forms.MenuItem()
    Me.mnuQQ_16 = New System.Windows.Forms.MenuItem()
    Me.mnuQQ_17 = New System.Windows.Forms.MenuItem()
    Me.mnuQQ_18 = New System.Windows.Forms.MenuItem()
    Me.mnuQQ_19 = New System.Windows.Forms.MenuItem()
    Me.mnuQQ_20 = New System.Windows.Forms.MenuItem()
    Me.mnuQQ_21 = New System.Windows.Forms.MenuItem()
    Me.mnuQQ_22 = New System.Windows.Forms.MenuItem()
    Me.mnuQQ_23 = New System.Windows.Forms.MenuItem()
    Me.mnuQQ_24 = New System.Windows.Forms.MenuItem()
    Me.mnuQQ_25 = New System.Windows.Forms.MenuItem()
    Me.mnuQQ_26 = New System.Windows.Forms.MenuItem()
    Me.mnuQQ_27 = New System.Windows.Forms.MenuItem()
    Me.mnuQQ_28 = New System.Windows.Forms.MenuItem()
    Me.mnuQQ_29 = New System.Windows.Forms.MenuItem()
    Me.mnuQQ_30 = New System.Windows.Forms.MenuItem()
    Me.mnuQQ_31 = New System.Windows.Forms.MenuItem()
    Me.mnuQQ_32 = New System.Windows.Forms.MenuItem()
    Me.mnuQQ_33 = New System.Windows.Forms.MenuItem()
    Me.mnuQQ_34 = New System.Windows.Forms.MenuItem()
    Me.mnuQQ_35 = New System.Windows.Forms.MenuItem()
    Me.mnuQQ_36 = New System.Windows.Forms.MenuItem()
    Me.mnuQQ_37 = New System.Windows.Forms.MenuItem()
    Me.mnuQQ_38 = New System.Windows.Forms.MenuItem()
    Me.mnuQQ_39 = New System.Windows.Forms.MenuItem()
    Me.mnuQQ_40 = New System.Windows.Forms.MenuItem()
    Me.mnuQQ_41 = New System.Windows.Forms.MenuItem()
    Me.mnuQQ_42 = New System.Windows.Forms.MenuItem()
    Me.mnuQQ_43 = New System.Windows.Forms.MenuItem()
    Me.mnuQQ_44 = New System.Windows.Forms.MenuItem()
    Me.mnuQQ_45 = New System.Windows.Forms.MenuItem()
    Me.mnuQQ_46 = New System.Windows.Forms.MenuItem()
    Me.mnuQQ_47 = New System.Windows.Forms.MenuItem()
    Me.mnuQQ_48 = New System.Windows.Forms.MenuItem()
    Me.mnuQQ_49 = New System.Windows.Forms.MenuItem()
    Me.mnuRZ = New System.Windows.Forms.MenuItem()
    Me.mnuRR_00 = New System.Windows.Forms.MenuItem()
    Me.mnuRR_01 = New System.Windows.Forms.MenuItem()
    Me.mnuRR_02 = New System.Windows.Forms.MenuItem()
    Me.mnuRR_03 = New System.Windows.Forms.MenuItem()
    Me.mnuRR_04 = New System.Windows.Forms.MenuItem()
    Me.mnuRR_05 = New System.Windows.Forms.MenuItem()
    Me.mnuRR_06 = New System.Windows.Forms.MenuItem()
    Me.mnuRR_07 = New System.Windows.Forms.MenuItem()
    Me.mnuRR_08 = New System.Windows.Forms.MenuItem()
    Me.mnuRR_09 = New System.Windows.Forms.MenuItem()
    Me.mnuRR_10 = New System.Windows.Forms.MenuItem()
    Me.mnuRR_11 = New System.Windows.Forms.MenuItem()
    Me.mnuRR_12 = New System.Windows.Forms.MenuItem()
    Me.mnuRR_13 = New System.Windows.Forms.MenuItem()
    Me.mnuRR_14 = New System.Windows.Forms.MenuItem()
    Me.mnuRR_15 = New System.Windows.Forms.MenuItem()
    Me.mnuRR_16 = New System.Windows.Forms.MenuItem()
    Me.mnuRR_17 = New System.Windows.Forms.MenuItem()
    Me.mnuRR_18 = New System.Windows.Forms.MenuItem()
    Me.mnuRR_19 = New System.Windows.Forms.MenuItem()
    Me.mnuRR_20 = New System.Windows.Forms.MenuItem()
    Me.mnuRR_21 = New System.Windows.Forms.MenuItem()
    Me.mnuRR_22 = New System.Windows.Forms.MenuItem()
    Me.mnuRR_23 = New System.Windows.Forms.MenuItem()
    Me.mnuRR_24 = New System.Windows.Forms.MenuItem()
    Me.mnuRR_25 = New System.Windows.Forms.MenuItem()
    Me.mnuRR_26 = New System.Windows.Forms.MenuItem()
    Me.mnuRR_27 = New System.Windows.Forms.MenuItem()
    Me.mnuRR_28 = New System.Windows.Forms.MenuItem()
    Me.mnuRR_29 = New System.Windows.Forms.MenuItem()
    Me.mnuRR_30 = New System.Windows.Forms.MenuItem()
    Me.mnuRR_31 = New System.Windows.Forms.MenuItem()
    Me.mnuRR_32 = New System.Windows.Forms.MenuItem()
    Me.mnuRR_33 = New System.Windows.Forms.MenuItem()
    Me.mnuRR_34 = New System.Windows.Forms.MenuItem()
    Me.mnuRR_35 = New System.Windows.Forms.MenuItem()
    Me.mnuRR_36 = New System.Windows.Forms.MenuItem()
    Me.mnuRR_37 = New System.Windows.Forms.MenuItem()
    Me.mnuRR_38 = New System.Windows.Forms.MenuItem()
    Me.mnuRR_39 = New System.Windows.Forms.MenuItem()
    Me.mnuRR_40 = New System.Windows.Forms.MenuItem()
    Me.mnuRR_41 = New System.Windows.Forms.MenuItem()
    Me.mnuRR_42 = New System.Windows.Forms.MenuItem()
    Me.mnuRR_43 = New System.Windows.Forms.MenuItem()
    Me.mnuRR_44 = New System.Windows.Forms.MenuItem()
    Me.mnuRR_45 = New System.Windows.Forms.MenuItem()
    Me.mnuRR_46 = New System.Windows.Forms.MenuItem()
    Me.mnuRR_47 = New System.Windows.Forms.MenuItem()
    Me.mnuRR_48 = New System.Windows.Forms.MenuItem()
    Me.mnuRR_49 = New System.Windows.Forms.MenuItem()
    Me.mnuSPE = New System.Windows.Forms.MenuItem()
    Me.mnuSPZ_00 = New System.Windows.Forms.MenuItem()
    Me.mnuSPZ_01 = New System.Windows.Forms.MenuItem()
    Me.mnuSPZ_02 = New System.Windows.Forms.MenuItem()
    Me.mnuSPZ_03 = New System.Windows.Forms.MenuItem()
    Me.mnuSPZ_04 = New System.Windows.Forms.MenuItem()
    Me.mnuSPZ_05 = New System.Windows.Forms.MenuItem()
    Me.mnuSPZ_06 = New System.Windows.Forms.MenuItem()
    Me.mnuSPZ_07 = New System.Windows.Forms.MenuItem()
    Me.mnuSPZ_08 = New System.Windows.Forms.MenuItem()
    Me.mnuSPZ_09 = New System.Windows.Forms.MenuItem()
    Me.mnuSPZ_10 = New System.Windows.Forms.MenuItem()
    Me.mnuSPZ_11 = New System.Windows.Forms.MenuItem()
    Me.mnuSPZ_12 = New System.Windows.Forms.MenuItem()
    Me.mnuSPZ_13 = New System.Windows.Forms.MenuItem()
    Me.mnuSPZ_14 = New System.Windows.Forms.MenuItem()
    Me.mnuSPZ_15 = New System.Windows.Forms.MenuItem()
    Me.mnuSPZ_16 = New System.Windows.Forms.MenuItem()
    Me.mnuSPZ_17 = New System.Windows.Forms.MenuItem()
    Me.mnuSPZ_18 = New System.Windows.Forms.MenuItem()
    Me.mnuSPZ_19 = New System.Windows.Forms.MenuItem()
    Me.mnuSPZ_20 = New System.Windows.Forms.MenuItem()
    Me.mnuSPZ_21 = New System.Windows.Forms.MenuItem()
    Me.mnuSPZ_22 = New System.Windows.Forms.MenuItem()
    Me.mnuSPZ_23 = New System.Windows.Forms.MenuItem()
    Me.mnuSPZ_24 = New System.Windows.Forms.MenuItem()
    Me.mnuSPZ_25 = New System.Windows.Forms.MenuItem()
    Me.mnuSPZ_26 = New System.Windows.Forms.MenuItem()
    Me.mnuSPZ_27 = New System.Windows.Forms.MenuItem()
    Me.mnuSPZ_28 = New System.Windows.Forms.MenuItem()
    Me.mnuSPZ_29 = New System.Windows.Forms.MenuItem()
    Me.mnuSPZ_30 = New System.Windows.Forms.MenuItem()
    Me.mnuSPZ_31 = New System.Windows.Forms.MenuItem()
    Me.mnuSPZ_32 = New System.Windows.Forms.MenuItem()
    Me.mnuSPZ_33 = New System.Windows.Forms.MenuItem()
    Me.mnuSPZ_34 = New System.Windows.Forms.MenuItem()
    Me.mnuSPZ_35 = New System.Windows.Forms.MenuItem()
    Me.mnuSPZ_36 = New System.Windows.Forms.MenuItem()
    Me.mnuSPZ_37 = New System.Windows.Forms.MenuItem()
    Me.mnuSPZ_38 = New System.Windows.Forms.MenuItem()
    Me.mnuSPZ_39 = New System.Windows.Forms.MenuItem()
    Me.mnuSPZ_40 = New System.Windows.Forms.MenuItem()
    Me.mnuSPZ_41 = New System.Windows.Forms.MenuItem()
    Me.mnuSPZ_42 = New System.Windows.Forms.MenuItem()
    Me.mnuSPZ_43 = New System.Windows.Forms.MenuItem()
    Me.mnuSPZ_44 = New System.Windows.Forms.MenuItem()
    Me.mnuSPZ_45 = New System.Windows.Forms.MenuItem()
    Me.mnuSPZ_46 = New System.Windows.Forms.MenuItem()
    Me.mnuSPZ_47 = New System.Windows.Forms.MenuItem()
    Me.mnuSPZ_48 = New System.Windows.Forms.MenuItem()
    Me.mnuSPZ_49 = New System.Windows.Forms.MenuItem()
    Me.mnuUSE = New System.Windows.Forms.MenuItem()
    Me.mnuDRU = New System.Windows.Forms.MenuItem()
    Me.mnuFenster = New System.Windows.Forms.MenuItem()
    Me.mnuEI = New System.Windows.Forms.MenuItem()
    Me.mnuEI_0 = New System.Windows.Forms.MenuItem()
    Me.mnuEI_1 = New System.Windows.Forms.MenuItem()
    Me.mnuEI_2 = New System.Windows.Forms.MenuItem()
    Me.mnuEI_3 = New System.Windows.Forms.MenuItem()
    Me.mnuEI_4 = New System.Windows.Forms.MenuItem()
    Me.mnuEI_5 = New System.Windows.Forms.MenuItem()
    Me.mnuEI_6 = New System.Windows.Forms.MenuItem()
    Me.mnuMSG = New System.Windows.Forms.MenuItem()
    Me.mnuMSG_0 = New System.Windows.Forms.MenuItem()
    Me.mnuMSG_1 = New System.Windows.Forms.MenuItem()
    Me.mnuMSG_2 = New System.Windows.Forms.MenuItem()
    Me.mnuMSG_3 = New System.Windows.Forms.MenuItem()
    Me.mnuMSG_4 = New System.Windows.Forms.MenuItem()
    Me.mnuMSG_5 = New System.Windows.Forms.MenuItem()
    Me.mnuEN = New System.Windows.Forms.MenuItem()
    Me.PrintDialogMDI = New System.Windows.Forms.PrintDialog()
    Me.menCon_0 = New System.Windows.Forms.MenuItem()
    Me.menCon_1 = New System.Windows.Forms.MenuItem()
    Me.menCon_2 = New System.Windows.Forms.MenuItem()
    Me.menContext = New System.Windows.Forms.ContextMenu()
    Me.SuspendLayout()
    '
    'MainUsMethEinst
    '
    Me.MainUsMethEinst.MenuItems.AddRange(New System.Windows.Forms.MenuItem() {Me.mnuQC, Me.mnuRZ, Me.mnuSPE, Me.mnuUSE, Me.mnuDRU, Me.mnuFenster, Me.mnuEI, Me.mnuMSG, Me.mnuEN})
    '
    'mnuQC
    '
    Me.mnuQC.Enabled = False
    Me.mnuQC.Index = 0
    Me.mnuQC.MenuItems.AddRange(New System.Windows.Forms.MenuItem() {Me.mnuQQ_00, Me.mnuQQ_01, Me.mnuQQ_02, Me.mnuQQ_03, Me.mnuQQ_04, Me.mnuQQ_05, Me.mnuQQ_06, Me.mnuQQ_07, Me.mnuQQ_08, Me.mnuQQ_09, Me.mnuQQ_10, Me.mnuQQ_11, Me.mnuQQ_12, Me.mnuQQ_13, Me.mnuQQ_14, Me.mnuQQ_15, Me.mnuQQ_16, Me.mnuQQ_17, Me.mnuQQ_18, Me.mnuQQ_19, Me.mnuQQ_20, Me.mnuQQ_21, Me.mnuQQ_22, Me.mnuQQ_23, Me.mnuQQ_24, Me.mnuQQ_25, Me.mnuQQ_26, Me.mnuQQ_27, Me.mnuQQ_28, Me.mnuQQ_29, Me.mnuQQ_30, Me.mnuQQ_31, Me.mnuQQ_32, Me.mnuQQ_33, Me.mnuQQ_34, Me.mnuQQ_35, Me.mnuQQ_36, Me.mnuQQ_37, Me.mnuQQ_38, Me.mnuQQ_39, Me.mnuQQ_40, Me.mnuQQ_41, Me.mnuQQ_42, Me.mnuQQ_43, Me.mnuQQ_44, Me.mnuQQ_45, Me.mnuQQ_46, Me.mnuQQ_47, Me.mnuQQ_48, Me.mnuQQ_49})
    Me.mnuQC.MergeOrder = 2
    Me.mnuQC.Text = "97"
    '
    'mnuQQ_00
    '
    Me.mnuQQ_00.Index = 0
    Me.mnuQQ_00.Text = "1800"
    '
    'mnuQQ_01
    '
    Me.mnuQQ_01.Index = 1
    Me.mnuQQ_01.Text = "1801"
    '
    'mnuQQ_02
    '
    Me.mnuQQ_02.Index = 2
    Me.mnuQQ_02.Text = "1802"
    '
    'mnuQQ_03
    '
    Me.mnuQQ_03.Index = 3
    Me.mnuQQ_03.Text = "1803"
    '
    'mnuQQ_04
    '
    Me.mnuQQ_04.Index = 4
    Me.mnuQQ_04.Text = "1804"
    '
    'mnuQQ_05
    '
    Me.mnuQQ_05.Index = 5
    Me.mnuQQ_05.Text = "1805"
    '
    'mnuQQ_06
    '
    Me.mnuQQ_06.Index = 6
    Me.mnuQQ_06.Text = "1806"
    '
    'mnuQQ_07
    '
    Me.mnuQQ_07.Index = 7
    Me.mnuQQ_07.Text = "1807"
    '
    'mnuQQ_08
    '
    Me.mnuQQ_08.Index = 8
    Me.mnuQQ_08.Text = "1808"
    '
    'mnuQQ_09
    '
    Me.mnuQQ_09.Index = 9
    Me.mnuQQ_09.Text = "1809"
    '
    'mnuQQ_10
    '
    Me.mnuQQ_10.Index = 10
    Me.mnuQQ_10.Text = "1810"
    '
    'mnuQQ_11
    '
    Me.mnuQQ_11.Index = 11
    Me.mnuQQ_11.Text = "1811"
    '
    'mnuQQ_12
    '
    Me.mnuQQ_12.Index = 12
    Me.mnuQQ_12.Text = "1812"
    '
    'mnuQQ_13
    '
    Me.mnuQQ_13.Index = 13
    Me.mnuQQ_13.Text = "1813"
    '
    'mnuQQ_14
    '
    Me.mnuQQ_14.Index = 14
    Me.mnuQQ_14.Text = "1814"
    '
    'mnuQQ_15
    '
    Me.mnuQQ_15.Index = 15
    Me.mnuQQ_15.Text = "1815"
    '
    'mnuQQ_16
    '
    Me.mnuQQ_16.Index = 16
    Me.mnuQQ_16.Text = "1816"
    '
    'mnuQQ_17
    '
    Me.mnuQQ_17.Index = 17
    Me.mnuQQ_17.Text = "1817"
    '
    'mnuQQ_18
    '
    Me.mnuQQ_18.Index = 18
    Me.mnuQQ_18.Text = "1818"
    '
    'mnuQQ_19
    '
    Me.mnuQQ_19.Index = 19
    Me.mnuQQ_19.Text = "1819"
    '
    'mnuQQ_20
    '
    Me.mnuQQ_20.Index = 20
    Me.mnuQQ_20.Text = "1820"
    '
    'mnuQQ_21
    '
    Me.mnuQQ_21.Index = 21
    Me.mnuQQ_21.Text = "1821"
    '
    'mnuQQ_22
    '
    Me.mnuQQ_22.Index = 22
    Me.mnuQQ_22.Text = "1822"
    '
    'mnuQQ_23
    '
    Me.mnuQQ_23.Index = 23
    Me.mnuQQ_23.Text = "1823"
    '
    'mnuQQ_24
    '
    Me.mnuQQ_24.Index = 24
    Me.mnuQQ_24.Text = "1824"
    '
    'mnuQQ_25
    '
    Me.mnuQQ_25.Index = 25
    Me.mnuQQ_25.Text = "1825"
    '
    'mnuQQ_26
    '
    Me.mnuQQ_26.Index = 26
    Me.mnuQQ_26.Text = "1826"
    '
    'mnuQQ_27
    '
    Me.mnuQQ_27.Index = 27
    Me.mnuQQ_27.Text = "1827"
    '
    'mnuQQ_28
    '
    Me.mnuQQ_28.Index = 28
    Me.mnuQQ_28.Text = "1828"
    '
    'mnuQQ_29
    '
    Me.mnuQQ_29.Index = 29
    Me.mnuQQ_29.Text = "1829"
    '
    'mnuQQ_30
    '
    Me.mnuQQ_30.Index = 30
    Me.mnuQQ_30.Text = "1830"
    '
    'mnuQQ_31
    '
    Me.mnuQQ_31.Index = 31
    Me.mnuQQ_31.Text = "1831"
    '
    'mnuQQ_32
    '
    Me.mnuQQ_32.Index = 32
    Me.mnuQQ_32.Text = "1832"
    '
    'mnuQQ_33
    '
    Me.mnuQQ_33.Index = 33
    Me.mnuQQ_33.Text = "1833"
    '
    'mnuQQ_34
    '
    Me.mnuQQ_34.Index = 34
    Me.mnuQQ_34.Text = "1834"
    '
    'mnuQQ_35
    '
    Me.mnuQQ_35.Index = 35
    Me.mnuQQ_35.Text = "1835"
    '
    'mnuQQ_36
    '
    Me.mnuQQ_36.Index = 36
    Me.mnuQQ_36.Text = "1836"
    '
    'mnuQQ_37
    '
    Me.mnuQQ_37.Index = 37
    Me.mnuQQ_37.Text = "1837"
    '
    'mnuQQ_38
    '
    Me.mnuQQ_38.Index = 38
    Me.mnuQQ_38.Text = "1838"
    '
    'mnuQQ_39
    '
    Me.mnuQQ_39.Index = 39
    Me.mnuQQ_39.Text = "1839"
    '
    'mnuQQ_40
    '
    Me.mnuQQ_40.Index = 40
    Me.mnuQQ_40.Text = "1840"
    '
    'mnuQQ_41
    '
    Me.mnuQQ_41.Index = 41
    Me.mnuQQ_41.Text = "1841"
    '
    'mnuQQ_42
    '
    Me.mnuQQ_42.Index = 42
    Me.mnuQQ_42.Text = "1842"
    '
    'mnuQQ_43
    '
    Me.mnuQQ_43.Index = 43
    Me.mnuQQ_43.Text = "1843"
    '
    'mnuQQ_44
    '
    Me.mnuQQ_44.Index = 44
    Me.mnuQQ_44.Text = "1844"
    '
    'mnuQQ_45
    '
    Me.mnuQQ_45.Index = 45
    Me.mnuQQ_45.Text = "1845"
    '
    'mnuQQ_46
    '
    Me.mnuQQ_46.Index = 46
    Me.mnuQQ_46.Text = "1846"
    '
    'mnuQQ_47
    '
    Me.mnuQQ_47.Index = 47
    Me.mnuQQ_47.Text = "1847"
    '
    'mnuQQ_48
    '
    Me.mnuQQ_48.Index = 48
    Me.mnuQQ_48.Text = "1848"
    '
    'mnuQQ_49
    '
    Me.mnuQQ_49.Index = 49
    Me.mnuQQ_49.Text = "1849"
    '
    'mnuRZ
    '
    Me.mnuRZ.Enabled = False
    Me.mnuRZ.Index = 1
    Me.mnuRZ.MenuItems.AddRange(New System.Windows.Forms.MenuItem() {Me.mnuRR_00, Me.mnuRR_01, Me.mnuRR_02, Me.mnuRR_03, Me.mnuRR_04, Me.mnuRR_05, Me.mnuRR_06, Me.mnuRR_07, Me.mnuRR_08, Me.mnuRR_09, Me.mnuRR_10, Me.mnuRR_11, Me.mnuRR_12, Me.mnuRR_13, Me.mnuRR_14, Me.mnuRR_15, Me.mnuRR_16, Me.mnuRR_17, Me.mnuRR_18, Me.mnuRR_19, Me.mnuRR_20, Me.mnuRR_21, Me.mnuRR_22, Me.mnuRR_23, Me.mnuRR_24, Me.mnuRR_25, Me.mnuRR_26, Me.mnuRR_27, Me.mnuRR_28, Me.mnuRR_29, Me.mnuRR_30, Me.mnuRR_31, Me.mnuRR_32, Me.mnuRR_33, Me.mnuRR_34, Me.mnuRR_35, Me.mnuRR_36, Me.mnuRR_37, Me.mnuRR_38, Me.mnuRR_39, Me.mnuRR_40, Me.mnuRR_41, Me.mnuRR_42, Me.mnuRR_43, Me.mnuRR_44, Me.mnuRR_45, Me.mnuRR_46, Me.mnuRR_47, Me.mnuRR_48, Me.mnuRR_49})
    Me.mnuRZ.MergeOrder = 3
    Me.mnuRZ.Text = "96"
    '
    'mnuRR_00
    '
    Me.mnuRR_00.Index = 0
    Me.mnuRR_00.Text = "1850"
    '
    'mnuRR_01
    '
    Me.mnuRR_01.Index = 1
    Me.mnuRR_01.Text = "1851"
    '
    'mnuRR_02
    '
    Me.mnuRR_02.Index = 2
    Me.mnuRR_02.Text = "1852"
    '
    'mnuRR_03
    '
    Me.mnuRR_03.Index = 3
    Me.mnuRR_03.Text = "1853"
    '
    'mnuRR_04
    '
    Me.mnuRR_04.Index = 4
    Me.mnuRR_04.Text = "1854"
    '
    'mnuRR_05
    '
    Me.mnuRR_05.Index = 5
    Me.mnuRR_05.Text = "1855"
    '
    'mnuRR_06
    '
    Me.mnuRR_06.Index = 6
    Me.mnuRR_06.Text = "1856"
    '
    'mnuRR_07
    '
    Me.mnuRR_07.Index = 7
    Me.mnuRR_07.Text = "1857"
    '
    'mnuRR_08
    '
    Me.mnuRR_08.Index = 8
    Me.mnuRR_08.Text = "1858"
    '
    'mnuRR_09
    '
    Me.mnuRR_09.Index = 9
    Me.mnuRR_09.Text = "1859"
    '
    'mnuRR_10
    '
    Me.mnuRR_10.Index = 10
    Me.mnuRR_10.Text = "1860"
    '
    'mnuRR_11
    '
    Me.mnuRR_11.Index = 11
    Me.mnuRR_11.Text = "1861"
    '
    'mnuRR_12
    '
    Me.mnuRR_12.Index = 12
    Me.mnuRR_12.Text = "1862"
    '
    'mnuRR_13
    '
    Me.mnuRR_13.Index = 13
    Me.mnuRR_13.Text = "1863"
    '
    'mnuRR_14
    '
    Me.mnuRR_14.Index = 14
    Me.mnuRR_14.Text = "1864"
    '
    'mnuRR_15
    '
    Me.mnuRR_15.Index = 15
    Me.mnuRR_15.Text = "1865"
    '
    'mnuRR_16
    '
    Me.mnuRR_16.Index = 16
    Me.mnuRR_16.Text = "1866"
    '
    'mnuRR_17
    '
    Me.mnuRR_17.Index = 17
    Me.mnuRR_17.Text = "1867"
    '
    'mnuRR_18
    '
    Me.mnuRR_18.Index = 18
    Me.mnuRR_18.Text = "1868"
    '
    'mnuRR_19
    '
    Me.mnuRR_19.Index = 19
    Me.mnuRR_19.Text = "1869"
    '
    'mnuRR_20
    '
    Me.mnuRR_20.Index = 20
    Me.mnuRR_20.Text = "1870"
    '
    'mnuRR_21
    '
    Me.mnuRR_21.Index = 21
    Me.mnuRR_21.Text = "1871"
    '
    'mnuRR_22
    '
    Me.mnuRR_22.Index = 22
    Me.mnuRR_22.Text = "1872"
    '
    'mnuRR_23
    '
    Me.mnuRR_23.Index = 23
    Me.mnuRR_23.Text = "1873"
    '
    'mnuRR_24
    '
    Me.mnuRR_24.Index = 24
    Me.mnuRR_24.Text = "1874"
    '
    'mnuRR_25
    '
    Me.mnuRR_25.Index = 25
    Me.mnuRR_25.Text = "1875"
    '
    'mnuRR_26
    '
    Me.mnuRR_26.Index = 26
    Me.mnuRR_26.Text = "1876"
    '
    'mnuRR_27
    '
    Me.mnuRR_27.Index = 27
    Me.mnuRR_27.Text = "1877"
    '
    'mnuRR_28
    '
    Me.mnuRR_28.Index = 28
    Me.mnuRR_28.Text = "1878"
    '
    'mnuRR_29
    '
    Me.mnuRR_29.Index = 29
    Me.mnuRR_29.Text = "1879"
    '
    'mnuRR_30
    '
    Me.mnuRR_30.Index = 30
    Me.mnuRR_30.Text = "1880"
    '
    'mnuRR_31
    '
    Me.mnuRR_31.Index = 31
    Me.mnuRR_31.Text = "1881"
    '
    'mnuRR_32
    '
    Me.mnuRR_32.Index = 32
    Me.mnuRR_32.Text = "1882"
    '
    'mnuRR_33
    '
    Me.mnuRR_33.Index = 33
    Me.mnuRR_33.Text = "1883"
    '
    'mnuRR_34
    '
    Me.mnuRR_34.Index = 34
    Me.mnuRR_34.Text = "1884"
    '
    'mnuRR_35
    '
    Me.mnuRR_35.Index = 35
    Me.mnuRR_35.Text = "1885"
    '
    'mnuRR_36
    '
    Me.mnuRR_36.Index = 36
    Me.mnuRR_36.Text = "1886"
    '
    'mnuRR_37
    '
    Me.mnuRR_37.Index = 37
    Me.mnuRR_37.Text = "1887"
    '
    'mnuRR_38
    '
    Me.mnuRR_38.Index = 38
    Me.mnuRR_38.Text = "1888"
    '
    'mnuRR_39
    '
    Me.mnuRR_39.Index = 39
    Me.mnuRR_39.Text = "1889"
    '
    'mnuRR_40
    '
    Me.mnuRR_40.Index = 40
    Me.mnuRR_40.Text = "1890"
    '
    'mnuRR_41
    '
    Me.mnuRR_41.Index = 41
    Me.mnuRR_41.Text = "1891"
    '
    'mnuRR_42
    '
    Me.mnuRR_42.Index = 42
    Me.mnuRR_42.Text = "1892"
    '
    'mnuRR_43
    '
    Me.mnuRR_43.Index = 43
    Me.mnuRR_43.Text = "1893"
    '
    'mnuRR_44
    '
    Me.mnuRR_44.Index = 44
    Me.mnuRR_44.Text = "1894"
    '
    'mnuRR_45
    '
    Me.mnuRR_45.Index = 45
    Me.mnuRR_45.Text = "1895"
    '
    'mnuRR_46
    '
    Me.mnuRR_46.Index = 46
    Me.mnuRR_46.Text = "1896"
    '
    'mnuRR_47
    '
    Me.mnuRR_47.Index = 47
    Me.mnuRR_47.Text = "1897"
    '
    'mnuRR_48
    '
    Me.mnuRR_48.Index = 48
    Me.mnuRR_48.Text = "1898"
    '
    'mnuRR_49
    '
    Me.mnuRR_49.Index = 49
    Me.mnuRR_49.Text = "1899"
    '
    'mnuSPE
    '
    Me.mnuSPE.Enabled = False
    Me.mnuSPE.Index = 2
    Me.mnuSPE.MenuItems.AddRange(New System.Windows.Forms.MenuItem() {Me.mnuSPZ_00, Me.mnuSPZ_01, Me.mnuSPZ_02, Me.mnuSPZ_03, Me.mnuSPZ_04, Me.mnuSPZ_05, Me.mnuSPZ_06, Me.mnuSPZ_07, Me.mnuSPZ_08, Me.mnuSPZ_09, Me.mnuSPZ_10, Me.mnuSPZ_11, Me.mnuSPZ_12, Me.mnuSPZ_13, Me.mnuSPZ_14, Me.mnuSPZ_15, Me.mnuSPZ_16, Me.mnuSPZ_17, Me.mnuSPZ_18, Me.mnuSPZ_19, Me.mnuSPZ_20, Me.mnuSPZ_21, Me.mnuSPZ_22, Me.mnuSPZ_23, Me.mnuSPZ_24, Me.mnuSPZ_25, Me.mnuSPZ_26, Me.mnuSPZ_27, Me.mnuSPZ_28, Me.mnuSPZ_29, Me.mnuSPZ_30, Me.mnuSPZ_31, Me.mnuSPZ_32, Me.mnuSPZ_33, Me.mnuSPZ_34, Me.mnuSPZ_35, Me.mnuSPZ_36, Me.mnuSPZ_37, Me.mnuSPZ_38, Me.mnuSPZ_39, Me.mnuSPZ_40, Me.mnuSPZ_41, Me.mnuSPZ_42, Me.mnuSPZ_43, Me.mnuSPZ_44, Me.mnuSPZ_45, Me.mnuSPZ_46, Me.mnuSPZ_47, Me.mnuSPZ_48, Me.mnuSPZ_49})
    Me.mnuSPE.MergeOrder = 4
    Me.mnuSPE.Text = "94"
    '
    'mnuSPZ_00
    '
    Me.mnuSPZ_00.Index = 0
    Me.mnuSPZ_00.Text = "1900"
    '
    'mnuSPZ_01
    '
    Me.mnuSPZ_01.Index = 1
    Me.mnuSPZ_01.Text = "1901"
    '
    'mnuSPZ_02
    '
    Me.mnuSPZ_02.Index = 2
    Me.mnuSPZ_02.Text = "1902"
    '
    'mnuSPZ_03
    '
    Me.mnuSPZ_03.Index = 3
    Me.mnuSPZ_03.Text = "1903"
    '
    'mnuSPZ_04
    '
    Me.mnuSPZ_04.Index = 4
    Me.mnuSPZ_04.Text = "1904"
    '
    'mnuSPZ_05
    '
    Me.mnuSPZ_05.Index = 5
    Me.mnuSPZ_05.Text = "1905"
    '
    'mnuSPZ_06
    '
    Me.mnuSPZ_06.Index = 6
    Me.mnuSPZ_06.Text = "1906"
    '
    'mnuSPZ_07
    '
    Me.mnuSPZ_07.Index = 7
    Me.mnuSPZ_07.Text = "1907"
    '
    'mnuSPZ_08
    '
    Me.mnuSPZ_08.Index = 8
    Me.mnuSPZ_08.Text = "1908"
    '
    'mnuSPZ_09
    '
    Me.mnuSPZ_09.Index = 9
    Me.mnuSPZ_09.Text = "1909"
    '
    'mnuSPZ_10
    '
    Me.mnuSPZ_10.Index = 10
    Me.mnuSPZ_10.Text = "1910"
    '
    'mnuSPZ_11
    '
    Me.mnuSPZ_11.Index = 11
    Me.mnuSPZ_11.Text = "1911"
    '
    'mnuSPZ_12
    '
    Me.mnuSPZ_12.Index = 12
    Me.mnuSPZ_12.Text = "1912"
    '
    'mnuSPZ_13
    '
    Me.mnuSPZ_13.Index = 13
    Me.mnuSPZ_13.Text = "1913"
    '
    'mnuSPZ_14
    '
    Me.mnuSPZ_14.Index = 14
    Me.mnuSPZ_14.Text = "1914"
    '
    'mnuSPZ_15
    '
    Me.mnuSPZ_15.Index = 15
    Me.mnuSPZ_15.Text = "1915"
    '
    'mnuSPZ_16
    '
    Me.mnuSPZ_16.Index = 16
    Me.mnuSPZ_16.Text = "1916"
    '
    'mnuSPZ_17
    '
    Me.mnuSPZ_17.Index = 17
    Me.mnuSPZ_17.Text = "1917"
    '
    'mnuSPZ_18
    '
    Me.mnuSPZ_18.Index = 18
    Me.mnuSPZ_18.Text = "1918"
    '
    'mnuSPZ_19
    '
    Me.mnuSPZ_19.Index = 19
    Me.mnuSPZ_19.Text = "1919"
    '
    'mnuSPZ_20
    '
    Me.mnuSPZ_20.Index = 20
    Me.mnuSPZ_20.Text = "1920"
    '
    'mnuSPZ_21
    '
    Me.mnuSPZ_21.Index = 21
    Me.mnuSPZ_21.Text = "1921"
    '
    'mnuSPZ_22
    '
    Me.mnuSPZ_22.Index = 22
    Me.mnuSPZ_22.Text = "1922"
    '
    'mnuSPZ_23
    '
    Me.mnuSPZ_23.Index = 23
    Me.mnuSPZ_23.Text = "1923"
    '
    'mnuSPZ_24
    '
    Me.mnuSPZ_24.Index = 24
    Me.mnuSPZ_24.Text = "1924"
    '
    'mnuSPZ_25
    '
    Me.mnuSPZ_25.Index = 25
    Me.mnuSPZ_25.Text = "1925"
    '
    'mnuSPZ_26
    '
    Me.mnuSPZ_26.Index = 26
    Me.mnuSPZ_26.Text = "1926"
    '
    'mnuSPZ_27
    '
    Me.mnuSPZ_27.Index = 27
    Me.mnuSPZ_27.Text = "1927"
    '
    'mnuSPZ_28
    '
    Me.mnuSPZ_28.Index = 28
    Me.mnuSPZ_28.Text = "1928"
    '
    'mnuSPZ_29
    '
    Me.mnuSPZ_29.Index = 29
    Me.mnuSPZ_29.Text = "1929"
    '
    'mnuSPZ_30
    '
    Me.mnuSPZ_30.Index = 30
    Me.mnuSPZ_30.Text = "1930"
    '
    'mnuSPZ_31
    '
    Me.mnuSPZ_31.Index = 31
    Me.mnuSPZ_31.Text = "1931"
    '
    'mnuSPZ_32
    '
    Me.mnuSPZ_32.Index = 32
    Me.mnuSPZ_32.Text = "1932"
    '
    'mnuSPZ_33
    '
    Me.mnuSPZ_33.Index = 33
    Me.mnuSPZ_33.Text = "1933"
    '
    'mnuSPZ_34
    '
    Me.mnuSPZ_34.Index = 34
    Me.mnuSPZ_34.Text = "1934"
    '
    'mnuSPZ_35
    '
    Me.mnuSPZ_35.Index = 35
    Me.mnuSPZ_35.Text = "1935"
    '
    'mnuSPZ_36
    '
    Me.mnuSPZ_36.Index = 36
    Me.mnuSPZ_36.Text = "1936"
    '
    'mnuSPZ_37
    '
    Me.mnuSPZ_37.Index = 37
    Me.mnuSPZ_37.Text = "1937"
    '
    'mnuSPZ_38
    '
    Me.mnuSPZ_38.Index = 38
    Me.mnuSPZ_38.Text = "1938"
    '
    'mnuSPZ_39
    '
    Me.mnuSPZ_39.Index = 39
    Me.mnuSPZ_39.Text = "1939"
    '
    'mnuSPZ_40
    '
    Me.mnuSPZ_40.Index = 40
    Me.mnuSPZ_40.Text = "1940"
    '
    'mnuSPZ_41
    '
    Me.mnuSPZ_41.Index = 41
    Me.mnuSPZ_41.Text = "1941"
    '
    'mnuSPZ_42
    '
    Me.mnuSPZ_42.Index = 42
    Me.mnuSPZ_42.Text = "1942"
    '
    'mnuSPZ_43
    '
    Me.mnuSPZ_43.Index = 43
    Me.mnuSPZ_43.Text = "1943"
    '
    'mnuSPZ_44
    '
    Me.mnuSPZ_44.Index = 44
    Me.mnuSPZ_44.Text = "1944"
    '
    'mnuSPZ_45
    '
    Me.mnuSPZ_45.Index = 45
    Me.mnuSPZ_45.Text = "1945"
    '
    'mnuSPZ_46
    '
    Me.mnuSPZ_46.Index = 46
    Me.mnuSPZ_46.Text = "1946"
    '
    'mnuSPZ_47
    '
    Me.mnuSPZ_47.Index = 47
    Me.mnuSPZ_47.Text = "1947"
    '
    'mnuSPZ_48
    '
    Me.mnuSPZ_48.Index = 48
    Me.mnuSPZ_48.Text = "1948"
    '
    'mnuSPZ_49
    '
    Me.mnuSPZ_49.Index = 49
    Me.mnuSPZ_49.Text = "1949"
    '
    'mnuUSE
    '
    Me.mnuUSE.Index = 3
    Me.mnuUSE.MergeOrder = 1
    Me.mnuUSE.Text = "200"
    '
    'mnuDRU
    '
    Me.mnuDRU.Enabled = False
    Me.mnuDRU.Index = 4
    Me.mnuDRU.MergeOrder = 7
    Me.mnuDRU.Text = "341"
    '
    'mnuFenster
    '
    Me.mnuFenster.Index = 5
    Me.mnuFenster.MdiList = True
    Me.mnuFenster.MergeOrder = 8
    Me.mnuFenster.Text = "89"
    Me.mnuFenster.Visible = False
    '
    'mnuEI
    '
    Me.mnuEI.Enabled = False
    Me.mnuEI.Index = 6
    Me.mnuEI.MenuItems.AddRange(New System.Windows.Forms.MenuItem() {Me.mnuEI_0, Me.mnuEI_1, Me.mnuEI_2, Me.mnuEI_3, Me.mnuEI_4, Me.mnuEI_5, Me.mnuEI_6})
    Me.mnuEI.Text = "302"
    '
    'mnuEI_0
    '
    Me.mnuEI_0.Index = 0
    Me.mnuEI_0.Text = "411"
    '
    'mnuEI_1
    '
    Me.mnuEI_1.Index = 1
    Me.mnuEI_1.Text = "412"
    '
    'mnuEI_2
    '
    Me.mnuEI_2.Index = 2
    Me.mnuEI_2.Text = "413"
    '
    'mnuEI_3
    '
    Me.mnuEI_3.Index = 3
    Me.mnuEI_3.Text = "414"
    '
    'mnuEI_4
    '
    Me.mnuEI_4.Index = 4
    Me.mnuEI_4.Text = "415"
    '
    'mnuEI_5
    '
    Me.mnuEI_5.Index = 5
    Me.mnuEI_5.Text = "416"
    '
    'mnuEI_6
    '
    Me.mnuEI_6.Index = 6
    Me.mnuEI_6.Text = "417"
    '
    'mnuMSG
    '
    Me.mnuMSG.Enabled = False
    Me.mnuMSG.Index = 7
    Me.mnuMSG.MenuItems.AddRange(New System.Windows.Forms.MenuItem() {Me.mnuMSG_0, Me.mnuMSG_1, Me.mnuMSG_2, Me.mnuMSG_3, Me.mnuMSG_4, Me.mnuMSG_5})
    Me.mnuMSG.Text = "204"
    '
    'mnuMSG_0
    '
    Me.mnuMSG_0.Index = 0
    Me.mnuMSG_0.Text = "205"
    '
    'mnuMSG_1
    '
    Me.mnuMSG_1.Index = 1
    Me.mnuMSG_1.Text = "206"
    '
    'mnuMSG_2
    '
    Me.mnuMSG_2.Index = 2
    Me.mnuMSG_2.Text = "207"
    '
    'mnuMSG_3
    '
    Me.mnuMSG_3.Index = 3
    Me.mnuMSG_3.Text = "208"
    '
    'mnuMSG_4
    '
    Me.mnuMSG_4.Index = 4
    Me.mnuMSG_4.Text = "209"
    '
    'mnuMSG_5
    '
    Me.mnuMSG_5.Index = 5
    Me.mnuMSG_5.Text = "210"
    '
    'mnuEN
    '
    Me.mnuEN.Index = 8
    Me.mnuEN.MergeOrder = 9
    Me.mnuEN.Text = "150"
    '
    'menCon_0
    '
    Me.menCon_0.Index = 0
    Me.menCon_0.Text = "3922"
    '
    'menCon_1
    '
    Me.menCon_1.Index = 1
    Me.menCon_1.Text = "3923"
    '
    'menCon_2
    '
    Me.menCon_2.Index = 2
    Me.menCon_2.Text = "3925"
    '
    'menContext
    '
    Me.menContext.MenuItems.AddRange(New System.Windows.Forms.MenuItem() {Me.menCon_0, Me.menCon_1, Me.menCon_2})
    '
    'frmColorMDI
    '
    Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
    Me.BackColor = System.Drawing.SystemColors.AppWorkspace
    Me.ClientSize = New System.Drawing.Size(984, 708)
    Me.Icon = CType(resources.GetObject("$this.Icon"), System.Drawing.Icon)
    Me.IsMdiContainer = True
    Me.Location = New System.Drawing.Point(12, 35)
    Me.Menu = Me.MainUsMethEinst
    Me.Name = "frmColorMDI"
    Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
    Me.Tag = "frmColorMDI"
    Me.Text = "99"
    Me.ResumeLayout(False)

  End Sub
#End Region
#Region "Aktualisierungssupport "
  'Private Shared m_vb6FormDefInstance As frmUfoColorMDI
  'Private Shared m_InitializingDefInstance As Boolean

#End Region
  Dim MnNoMess As Boolean = False


  Dim i As Short
  Dim ier As Integer

  Dim imsg As Short   'Messagebox (Rückgabewert)

  Dim UserID As Integer
  Dim MischID As Integer
  Dim MessgID As Integer
  Dim MethID As Integer
  Dim Einst As New ColSettings

 


  Private Sub frmColorMDI_Load(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles MyBase.Load
    Dim i As Short
    FormMDI = Me
    '
    '
    '
    Me.Text = Texxt(99) & " {" & COLORFileName() & "}"
    Me.mnuEN.Text = Texxt(150)
    Me.mnuUSE.Text = Texxt(200)
    Me.mnuDRU.Text = Texxt(341)
    Me.mnuFenster.Text = Texxt(89)


    Me.mnuQC.Text = Texxt(97)
    For i = 0 To Me.mnuQC.MenuItems.Count - 1
      Me.mnuQC.MenuItems(i).Text = Texxt(1800 + i)
    Next i
    Me.mnuRZ.Text = Texxt(96)
    For i = 0 To Me.mnuRZ.MenuItems.Count - 1
      Me.mnuRZ.MenuItems(i).Text = Texxt(1850 + i)
    Next i

    Me.mnuSPE.Text = Texxt(94)
    For i = 0 To Me.mnuSPE.MenuItems.Count - 1
      Me.mnuSPE.MenuItems(i).Text = Texxt(1900 + i)
    Next i
    'Call Aend(Me, Me.ToolTipMDI)
    mnuDRU.Visible = True
    If PrinterSettings.InstalledPrinters.Count = 0 Then
      mnuDRU.Visible = False
    End If
    mnuQC.Enabled = False
    mnuRZ.Enabled = False
    mnuSPE.Enabled = False
    '
    Me.mnuMSG.Text = Texxt(204)
    For i = 0 To Me.mnuMSG.MenuItems.Count - 1
      Me.mnuMSG.MenuItems(i).Text = Texxt(205 + i)
    Next i
    '
    '
    '
    '

    ''
    '
    '
    '
    '


    Me.mnuEI.Text = Texxt(302)
    For i = 0 To Me.mnuEI.MenuItems.Count - 1
      Me.mnuEI.MenuItems(i).Text = Texxt(411 + i)
    Next i
    '
    '
    '
    'Contextmenue
    '
    '
    menContext.MenuItems(0).Text = Texxt(3922)
    menContext.MenuItems(1).Text = Texxt(3923)
    menContext.MenuItems(2).Text = Texxt(3925)


    '
    '
    '
    '
    'Menueparameter übernehmen
    '
    '
    '
    '
    '
    '
    '
    '
    'VisibleMenu
    '
    '
    '
    mnuUSE.Visible = True
    '

    '
    '
    '
    '
    '
    '
    '
    ''
    If CShort(GetPrivSettings("STARTUP", "STAUTO", "1", COLORFileName())) = 1 Then
      mnuUSE.PerformClick()
    End If


  End Sub



  Private Sub frmColorMDI_FormClosing(sender As Object, e As System.Windows.Forms.FormClosingEventArgs) Handles Me.FormClosing
    If MessageBox.Show(Texxt(2992), Texxt(2000), MessageBoxButtons.YesNo) = Windows.Forms.DialogResult.No Then
      e.Cancel = True
      Exit Sub
    End If

  End Sub
  Private Sub frmColorMDI_FormClosed(sender As Object, e As System.Windows.Forms.FormClosedEventArgs) Handles Me.FormClosed

  End Sub




  Public Sub mnuEN_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles mnuEN.Click
    Me.Close()
  End Sub









  Public Sub mnuQQ_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles mnuQQ_00.Click, mnuQQ_01.Click, mnuQQ_02.Click, mnuQQ_03.Click, mnuQQ_04.Click, mnuQQ_05.Click, mnuQQ_06.Click, mnuQQ_07.Click, mnuQQ_08.Click, mnuQQ_09.Click, mnuQQ_10.Click, mnuQQ_11.Click, mnuQQ_12.Click, mnuQQ_13.Click, mnuQQ_14.Click, mnuQQ_15.Click, mnuQQ_16.Click, mnuQQ_17.Click, mnuQQ_18.Click, mnuQQ_19.Click, mnuQQ_20.Click, mnuQQ_21.Click, mnuQQ_22.Click, mnuQQ_23.Click, mnuQQ_24.Click, mnuQQ_25.Click, mnuQQ_26.Click, mnuQQ_27.Click, mnuQQ_28.Click, mnuQQ_29.Click, mnuQQ_30.Click, mnuQQ_31.Click, mnuQQ_32.Click, mnuQQ_33.Click, mnuQQ_34.Click, mnuQQ_35.Click, mnuQQ_36.Click, mnuQQ_37.Click, mnuQQ_38.Click, mnuQQ_39.Click, mnuQQ_40.Click, mnuQQ_41.Click, mnuQQ_42.Click, mnuQQ_43.Click, mnuQQ_44.Click, mnuQQ_45.Click, mnuQQ_46.Click, mnuQQ_47.Click, mnuQQ_48.Click, mnuQQ_49.Click
    Dim i As Integer


    Dim Index As Short
    If Not IsNothing(Aform) AndAlso TypeOf (Aform) Is Form Then
      Try
        Aform.close()
        Aform.hide()
        Aform.Dispose()
        Aform = Nothing
      Catch
      End Try
    End If
    Index = eventSender.index
    If Index = 40 Then
      '
      'Vergleich von Farbwerten
      '
      '
      Aform = New frmQualControl
    ElseIf Index = 47 Then
      Aform = New frmNassTrocken
    Else
      Aform = New frmQualControl
    End If
    Aform.MdiParent = Me
    AufbauPar.MethID = -1
    MethID = Index
    Try
      For i = 0 To 1
        AufbauPar.MethID = MethID
        If AufbauPar.ier <> 0 Then
          If AufbauPar.ier = -10 Then
            Call Einst.EinstellForm(0, MenueParam.UserID, MenueParam.MessgID, MenueParam.MethID, MenueParam.MischID, False, False, Nothing)
          ElseIf AufbauPar.ier = -20 Then
            Call Einst.EinstellForm(1, MenueParam.UserID, MenueParam.MessgID, MenueParam.MethID, MenueParam.MischID, False, False, Nothing)
          Else
            mnuEI.Enabled = True
            Exit Sub
          End If
        End If
      Next i
      mnuRZ.Enabled = False
      mnuSPE.Enabled = False
      mnuQC.Enabled = False
      Aform.Show()
      mnuRZ.Enabled = True
      mnuSPE.Enabled = True
      mnuQC.Enabled = True
      Call VisMenEinst()
      Me.Text = CStr(MenueParam.MethID) & Space(1) & Texxt(99)
    Catch ex As Exception
      Aform = Nothing
    End Try

  End Sub
  Public Sub mnuRR_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles mnuRR_00.Click, mnuRR_01.Click, mnuRR_02.Click, mnuRR_03.Click, mnuRR_04.Click, mnuRR_05.Click, mnuRR_06.Click, mnuRR_07.Click, mnuRR_08.Click, mnuRR_09.Click, mnuRR_10.Click, mnuRR_11.Click, mnuRR_12.Click, mnuRR_13.Click, mnuRR_14.Click, mnuRR_15.Click, mnuRR_16.Click, mnuRR_17.Click, mnuRR_18.Click, mnuRR_19.Click, mnuRR_20.Click, mnuRR_21.Click, mnuRR_22.Click, mnuRR_23.Click, mnuRR_24.Click, mnuRR_25.Click, mnuRR_26.Click, mnuRR_27.Click, mnuRR_28.Click, mnuRR_29.Click, mnuRR_30.Click, mnuRR_31.Click, mnuRR_32.Click, mnuRR_33.Click, mnuRR_34.Click, mnuRR_35.Click, mnuRR_36.Click, mnuRR_37.Click, mnuRR_38.Click, mnuRR_39.Click, mnuRR_40.Click, mnuRR_41.Click, mnuRR_42.Click, mnuRR_43.Click, mnuRR_44.Click, mnuRR_45.Click, mnuRR_46.Click, mnuRR_47.Click, mnuRR_48.Click, mnuRR_49.Click
    Dim i As Integer
    Dim Index As Short
    If Not IsNothing(Aform) AndAlso TypeOf (Aform) Is Form Then
      Try
        Aform.close()
        Aform.hide()
        Aform.Dispose()
        Aform = Nothing
      Catch
      End Try
    End If
    Index = eventSender.index

    If MischID < 0 Then
      MsgBox(Texxt(3601))
      Exit Sub
    End If
    MischID = MenueParam.MischID
    AufbauPar.MethID = -1
    AufbauPar.MischID = -1

    MethID = Index + 50
    Application.DoEvents()
    Try
      For i = 0 To 1
        AufbauPar.MethID = MethID
        If AufbauPar.ier <> 0 Then
          If AufbauPar.ier = -10 Then
            Call Einst.EinstellForm(0, MenueParam.UserID, MenueParam.MessgID, MenueParam.MethID, MenueParam.MischID, False, False, Nothing)
          ElseIf AufbauPar.ier = -20 Then
            Call Einst.EinstellForm(1, MenueParam.UserID, MenueParam.MessgID, MenueParam.MethID, MenueParam.MischID, False, False, Nothing)
          Else
            mnuEI.Enabled = True
            Exit Sub
          End If
        End If
      Next i
    Catch ex As Exception
      MsgBox(ex.Message & " ier= " & AufbauPar.ier)
    End Try
    Try

      AufbauPar.MischID = MischID
      mnuRZ.Enabled = False
      mnuSPE.Enabled = False
      mnuQC.Enabled = False

    Catch ex As Exception

    End Try
    Select Case Index
      Case 0
        '
        '
        'FARB-/BINDEMITTEL
        '
        '
        '
        '
        '
        Aform = New frmFarbmittelEing
      Case 1
        '
        '
        'COLORTHEK/EICHREIHEN
        '
        '
        '
        '
        '
        Aform = New frmColorthekEing

      Case 2
        '
        '
        'Grunddatenberechnung
        '
        '
        '
        '
        '
        'ColorBasisProg.frmColorGrunddaten.GKOptGraph = True
        'Aform = New frmColorGrundAdv
        'Aform.gkoptgraph = True
        Aform = New frmColorGrund
      Case 3
        '
        '
        'Erstrezeptberechnung
        '
        '
        '
        '
        '

        Aform = New frmColorRezepte

        'Aform = New frmColorRezAllg
        'Aform = New frmColorMessEinst

        '
      Case 4
        '
        'Rezeptkorrektur
        '
        '
        '
        Aform = New frmColorSuchKorr(False)


        '
      Case 5
        '
        'Colortheksuche + Korrekturberechnung
        '
        Aform = New frmColorSuchKorr(True)
        '
        '
      Case 6
        '
        'Farbmittelaustausch
        '
        Aform = New frmChargenNuancieren
        '
        '
      Case 7
        '
        'Mischsystem Konvertieren
        '
        '
        '
        Aform = New frmConvertMisch
      Case 10
        '
        '
        '
        '
      Case 11
        '
        '

        '
        '
      Case 12
        '
        '

        '
      Case 20
        '
        '
        Aform = New frmColorRaumBerechnung
      Case 21
        '
        'Blockverarbeitung
        '
        Aform = New frmColorBlockverarbeitung
        '
        '
      Case 22
        '
        'Blockverarbeitung (Korrektur
        '
        Aform = New frmBlockverarbeitungKorr
        '
        '
      Case 23
        '
        'Blockverarbeitung(R-Werte aus Rezepten
        '
        Aform = New frmBlockverarbeitungRefl
        '
        '

    End Select
    If AufbauPar.ier <> 0 Then
      mnuEI.Enabled = True
      Exit Sub
    End If
    Try
      Aform.MdiParent = Me
      Call VisMenEinst()

      Application.DoEvents()
      Aform.Show()
      Call VisMainMen()
      mnuRZ.Enabled = True
      mnuSPE.Enabled = True
      mnuQC.Enabled = True
      Me.Text = CStr(MenueParam.MethID) & Space(1) & Texxt(99)
      Exit Sub
    Catch ex As Exception
      Aform.dispose()
      Aform = Nothing
      'For i = MdiChildren.Count - 1 To 0 Step -1
      'Try
      'MdiChildren(i).Dispose()
      'Catch ey As Exception

      'End Try

      'Next
    End Try
  End Sub
  Sub VisMainMen()
    MenueParam.User.Qual = mnuQC.Visible
    MenueParam.User.Rezpt = mnuRZ.Visible
    MenueParam.User.Extr = mnuSPE.Visible
  End Sub
  Public Sub mnuSPZ_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles mnuSPZ_00.Click, mnuSPZ_01.Click, mnuSPZ_02.Click, mnuSPZ_03.Click, mnuSPZ_04.Click, mnuSPZ_05.Click, mnuSPZ_06.Click, mnuSPZ_07.Click, mnuSPZ_08.Click, mnuSPZ_09.Click, mnuSPZ_10.Click, mnuSPZ_11.Click, mnuSPZ_12.Click, mnuSPZ_13.Click, mnuSPZ_14.Click, mnuSPZ_15.Click, mnuSPZ_16.Click, mnuSPZ_17.Click, mnuSPZ_18.Click, mnuSPZ_19.Click, mnuSPZ_20.Click, mnuSPZ_21.Click, mnuSPZ_22.Click, mnuSPZ_23.Click, mnuSPZ_24.Click, mnuSPZ_25.Click, mnuSPZ_26.Click, mnuSPZ_27.Click, mnuSPZ_28.Click, mnuSPZ_29.Click, mnuSPZ_30.Click, mnuSPZ_31.Click, mnuSPZ_32.Click, mnuSPZ_33.Click, mnuSPZ_34.Click, mnuSPZ_35.Click, mnuSPZ_36.Click, mnuSPZ_37.Click, mnuSPZ_38.Click, mnuSPZ_39.Click, mnuSPZ_40.Click, mnuSPZ_41.Click, mnuSPZ_42.Click, mnuSPZ_43.Click, mnuSPZ_44.Click, mnuSPZ_45.Click, mnuSPZ_46.Click, mnuSPZ_47.Click, mnuSPZ_48.Click, mnuSPZ_49.Click
    Dim Index As Short
    Dim i As Integer
    If Not IsNothing(Aform) AndAlso TypeOf (Aform) Is Form Then
      Try
        Aform.close()
        Aform.hide()
        Aform.Dispose()
        Aform = Nothing
      Catch
      End Try
    End If
    Index = eventSender.index
    MethID = Index + 100
    Try
      For i = 0 To 1
        AufbauPar.MethID = MethID
        If AufbauPar.ier <> 0 Then
          If AufbauPar.ier = -10 Then
            Call Einst.EinstellForm(0, MenueParam.UserID, MenueParam.MessgID, MenueParam.MethID, MenueParam.MischID, False, False, Nothing)
          ElseIf AufbauPar.ier = -20 Then
            Call Einst.EinstellForm(1, MenueParam.UserID, MenueParam.MessgID, MenueParam.MethID, MenueParam.MischID, False, False, Nothing)
          Else
            mnuEI.Enabled = True
            Exit Sub
          End If
        End If
      Next i
    Catch ex As Exception
    End Try
    Select Case Index
      Case 0
        '
        '

      Case 1

      Case 2
        '
        '
      Case 3
        Aform = New frmFarbUmrechnung(False)
        '
      Case 4
        '
        '
      Case 5
        '
        '
      Case 6
        'Druckausgabe
        '
        'Cursor = System.Windows.Forms.Cursors.WaitCursor
        'AusgabeUser.AusgabeREWERTE(Menueparam)
        'Cursor = System.Windows.Forms.Cursors.Arrow
        Aform = New frmColorDrucken
      Case 7


      Case 8
        '
        'R-Werte(Löschen/Korrigieren)
        '

        Aform = New frmDARF
        '
      Case 9
        '
        'Rezepte (löschen/korrigieren)
        '
        If MenueParam.MischID < 0 Then
          MsgBox(Texxt(3601))
          Exit Sub
        End If
        Aform = New frmDARZ()
      Case 10
        '
        'Tabellen Exportieren
        '
        '
        Aform = New frmDBExportCOL
        '
      Case 11
        '
        'Tabellen importieren
        '
        Aform = New frmDBImportCOL
        '

    End Select
    Try
      If AufbauPar.ier <> 0 Then
        mnuEI.Enabled = True
        Exit Sub
      End If
      Aform.MdiParent = Me
      Call VisMenEinst()
      Application.DoEvents()
      mnuRZ.Enabled = False
      mnuSPE.Enabled = False
      mnuQC.Enabled = False
      Aform.Show()
      mnuRZ.Enabled = True
      mnuSPE.Enabled = True
      mnuQC.Enabled = True
      Aform.WindowState = FormWindowState.Maximized
      Call VisMainMen()
      Me.Text = CStr(MenueParam.MethID) & Space(1) & Texxt(99)
    Catch ex As Exception
      Aform = Nothing
    End Try

  End Sub


  Public Sub mnuUSE_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles mnuUSE.Click
    Dim FormUse As New frmColorUse
    Dim imsg As Short
    Dim IerMess As Short
    FormUse.Location = Me.Location
    FormUse.Size = Me.Size
    If Not IsNothing(Aform) AndAlso TypeOf (Aform) Is Form Then
      Aform.hide()
      Aform.close()
      Aform.Dispose()
    End If
    mnuQC.Enabled = False
    mnuRZ.Enabled = False
    mnuSPE.Enabled = False
    mnuEI.Enabled = False
    mnuMSG.Enabled = False
    AufbauPar.MethID = -1
    FormUse.ShowDialog()
    If FormUse.DialogResult <> System.Windows.Forms.DialogResult.OK Then
      Exit Sub
    End If
    AufbauPar.UserID = -1
    AufbauPar.MessgID = -1
    AufbauPar.MischID = -1
    UserID = FormUse.UserID
    MischID = FormUse.MischID
    MessgID = FormUse.MessgID
    If UserID = -1 And MessgID = -1 Then Exit Sub
    AufbauPar.UserID = UserID
    AufbauPar.MessgID = MessgID
    AufbauPar.MischID = MischID
    '
    '
    '
    '
    'Einstellungen visible,enabled
    '
    '

    '


    '
    '
    '
    Cursor = System.Windows.Forms.Cursors.WaitCursor


    '
    '
    '
    '
    '
    'Kalibrier- und Initialisier-Menü für Messgerät festlegen
    '
    '
    '

    Call VisMenMess()

    '
    If MenueParam.Messg.Winkel.Km = 0 Then
      MsgBox(Texxt(3603))
      Cursor = System.Windows.Forms.Cursors.Arrow
      Exit Sub
    End If
    '
    '
    If MenueParam.Normfa.Nlz = 0 Then
      MsgBox(Texxt(2994))
      Cursor = System.Windows.Forms.Cursors.Arrow
      Exit Sub
    End If
    '
    'Messgerät konfigurieren
    '
    '
    '
    GetPutReflex = New HandleRwerte()

    BasisProg = New clsColorBasis
    BasisProg.GetPutRef = GetPutReflex
    Measure = New ColorMeasure.MeasureReflex
    GetPutReflex.Iarch = CheckState.Unchecked
    GetPutReflex.Captext = "TESTEN"
    GetPutReflex.Retr = -1
    MenueParam.Messg.ReTr = GetPutReflex.Retr
    'GetPutReflex.ReflexWerte(False)
    Measure.Umspeich(MenueParam.Messg, MenueParam.Normfa(0))
    MenueParam.Messg.Exists = Not MnNoMess And Not MenueParam.Messg.Kal = 0
    'CrMeas = New CreateMessgeraet()
    'Call CrMeas.CreateMeasure(Measure)
    MenueParam.Messg.Exists = False
    If MenueParam.Messg.Exists Then

      IerMess = Measure.ier
      '
      '
      'Initialisierung fehlt
      '
      '
      If IerMess = -3 Or IerMess = -4 Then
        If MenueParam.Messg.Ini = 1 Then
          imsg = MsgBox(Texxt(3512) & Chr(13) & Texxt(2307) & "?", MsgBoxStyle.YesNo, Texxt(2000))
          If imsg <> 7 Then
            Measure.MessDevInit()
          End If
        End If
      End If
      '
      '
      'Kalibrierung fehlt
      '
      '
      '
      If IerMess = -2 Or IerMess = -4 Then
        If MenueParam.Messg.Kal = 1 Then
          imsg = MsgBox(Texxt(3513) & Chr(13) & Texxt(2308) & "?", MsgBoxStyle.YesNo, Texxt(2000))
          If imsg <> 7 Then
            Measure.MessDevKalib()
          End If
        End If
      End If
      '
      '
      'Kalibrierung zu alt
      '
      '
      '
      '
      If IerMess = -1 Then
        imsg = MsgBox(Texxt(3516) & CStr(MenueParam.Messg.KalInt) & Texxt(3517) & Chr(13) & Texxt(2308) & "?", MsgBoxStyle.YesNo, Texxt(2000))
        If imsg <> 7 Then
          Measure.MessDevKalib()
        End If
      End If
      If Measure.ier = 0 Or Measure.ier = 1 Then
        MenueParam.Messg.Exists = True
      End If
    End If
    '
    ' 
    ' 
    ' 


    GetPutReflex.Measure = Measure

    BasisKlasse.GetPutRef = GetPutReflex
    BasisKlasse.MDIForm = FormMDI
    BasisKlasse.PrintSet = Printset

    '
    '
    Cursor = System.Windows.Forms.Cursors.Arrow
    FormUse.Dispose()

  End Sub


  Private Sub frmColorMDI_Resize(ByVal sender As Object, ByVal e As System.EventArgs) Handles MyBase.Resize
    'Me.SetDesktopBounds(0.5 * (Screen.PrimaryScreen.WorkingArea.Width - Me.Size.Width), 0.5 * (Screen.PrimaryScreen.WorkingArea.Height - Me.Size.Height), Me.Size.Width, Me.Size.Height)
  End Sub




  Private Sub mnuSchließen_Click(ByVal sender As Object, ByVal e As System.EventArgs)
    If Not IsNothing(Me.ActiveMdiChild) Then
      Me.ActiveMdiChild.Close()
    End If
  End Sub

  Private Sub mnuDRU_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles mnuDRU.Click

  End Sub
  Public Sub mnuDRR_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
    Dim i As Integer
    Dim index As Integer
    index = CInt(sender.name.substring(7, 2))
    index = sender.index
    For i = 0 To mnuDRU.MenuItems.Count - 1
      If mnuDRU.MenuItems(i).Text.Substring(0, 1) = "*" Then
        mnuDRU.MenuItems(i).Text = mnuDRU.MenuItems(i).Text.Substring(1)
      End If
    Next i
    mnuDRU.MenuItems(index).Text = "*" & mnuDRU.MenuItems(index).Text
    Printset.PrinterName = PrinterSettings.InstalledPrinters.Item(index)
  End Sub


  Protected Overrides Sub Finalize()
    MyBase.Finalize()
    Dispose()
  End Sub

  Sub VisMenEinst()
    Dim i As Short
    Dim Isum As Short
    '
    '
    '
    'Menüs für Einstellungen
    '
    '
    '
    Isum = 0
    mnuEI.Visible = True
    For i = 0 To mnuEI.MenuItems.Count - 1
      Select Case i
        Case 3, 4
          If MenueParam.MethID < 50 Then
            'Isum = Isum + BitWrt(13 + i, MenueParam.User.Sonst)
            Isum = Isum + BitInt(CShort(13 + i), CShort(13 + i), MenueParam.User.Sonst)

          End If
        Case 2, 6
          If MenueParam.MethID > 49 And MenueParam.MethID < 100 Then
            'Isum = Isum + BitWrt(13 + i, MenueParam.User.Sonst)
            Isum = Isum + BitInt(CShort(13 + i), CShort(13 + i), MenueParam.User.Sonst)
          End If
        Case 0, 1, 5
          'Isum = Isum + BitWrt(13 + i, MenueParam.User.Sonst)
          Isum = Isum + BitInt(CShort(13 + i), CShort(13 + i), MenueParam.User.Sonst)
      End Select
    Next i
    If Isum = 0 Or Not BitWrt(1, MenueParam.User.Writ) Then
      mnuEI.Visible = False
    Else
      For i = 0 To mnuEI.MenuItems.Count - 1
        Select Case i
          'Einstellungen Merkmale, Parameter
          Case 3, 4
            If MenueParam.MethID < 50 And BitWrt(13 + i, MenueParam.User.Sonst) Then
              mnuEI.MenuItems(i).Visible = True
              mnuEI.MenuItems(i).Enabled = True
            Else
              mnuEI.MenuItems(i).Visible = False
            End If
            'Einstellungen Mischsysteme oder Gruppe Rezepte
          Case 2, 6
            If ((MenueParam.MethID > 49 And MenueParam.MethID < 100) _
                Or MenueParam.MethID = 106 Or MenueParam.MethID = 109 _
                Or MenueParam.MethID = 110 Or MenueParam.MethID = 111) _
               And BitWrt(13 + i, MenueParam.User.Sonst) Then
              mnuEI.MenuItems(i).Visible = True
              mnuEI.MenuItems(i).Enabled = True
            Else
              mnuEI.MenuItems(i).Visible = False
            End If
          Case 0, 1, 5
            'Einstellungen Messgeräte, Methoden, Gruppe R-Werte
            If BitWrt(13 + i, MenueParam.User.Sonst) Then
              mnuEI.MenuItems(i).Visible = True
              mnuEI.MenuItems(i).Enabled = True
            Else
              mnuEI.MenuItems(i).Visible = False
            End If
        End Select
      Next i
      mnuEI.Visible = True
      mnuEI.Enabled = True
    End If

  End Sub
  Sub VisMenMess()
    Dim i As Integer
    If BitWrt(3, MenueParam.User.Writ) Then
      Me.mnuMSG.Visible = True
      For i = 0 To Me.mnuMSG.MenuItems.Count - 1
        Me.mnuMSG.MenuItems(i).Visible = True
      Next i
      Me.mnuMSG.Visible = False

      '
      'MESSEN
      '
      '
      If MenueParam.Messg.Mes = 0 Then
        Me.mnuMSG.MenuItems(0).Visible = False
      Else
        Me.mnuMSG.Visible = True
      End If
      '
      'Kalibrieren
      '
      '
      If MenueParam.Messg.Kal = 0 Then
        Me.mnuMSG.MenuItems(1).Visible = False
      Else
        Me.mnuMSG.Visible = True
      End If

      '
      'Null-Kalibrieren
      '
      '
      If MenueParam.Messg.Nkal = 0 Then
        Me.mnuMSG.MenuItems(2).Visible = False
      Else
        Me.mnuMSG.Visible = True
      End If
      '
      'Initialisieren
      '
      '
      If MenueParam.Messg.Ini = 0 Then
        Me.mnuMSG.MenuItems(3).Visible = False
      Else
        Me.mnuMSG.Visible = True
      End If
      '
      '  kein Sonderprogramm
      '
      '
      If MenueParam.Messg.Sond = 0 Then
        Me.mnuMSG.MenuItems(4).Visible = False
      Else
        Me.mnuMSG.Visible = True
      End If
      '
      ' kein Messen'
      '
      '
      If Me.mnuMSG.Visible = True Then
        If MenueParam.Messg.Kein = 0 Then
          Me.mnuMSG.MenuItems(5).Visible = False
        End If
      End If
    Else
      Me.mnuMSG.Visible = False
      Me.mnuMSG_5.PerformClick()
    End If
  End Sub
  '
  '
  '
  '
  'Properies
  '
  '
  '
  '
  WriteOnly Property NoMess() As Boolean
    Set(ByVal AcNoMess As Boolean)
      MnNoMess = AcNoMess
    End Set
  End Property

  Private Sub mnuMSG_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles mnuMSG_0.Click, mnuMSG_1.Click, mnuMSG_2.Click, mnuMSG_3.Click, mnuMSG_4.Click, mnuMSG_5.Click
    Dim Index As Short
    Index = sender.index
    Dim Refel As RefValue
    Me.Enabled = False
  
    '
    Select Case Index
      Case 0
        '
        'Messen
        '
        Measure.MessDevMess(MenueParam.Messg, Refel)

        Refel = Nothing
      Case 1
        '
        'Kalibrieren
        '
        Measure.MessDevKalib()
        If Measure.ier = 0 Or Measure.ier = 1 Then
          MenueParam.Messg.Exists = True
        End If

      Case 2
        '
        'Null-Kalibriren
        '
        Measure.MessDevNkalib()

        'Measure.MeasDeviceNkalib()



      Case 3
        '
        'Initialisieren
        '
        '
        Measure.MessDevInit()
        

        'Measure.MeasDeviceInit()

      Case 4

        '
        'Sonderprogramm
        '
        Measure.MessDevSond()

        'Measure.MeasDeviceSond()
      Case 5
        '
        '
        'kein Messgerät
        '
        '
        '
        '




        MenueParam.Messg.Exists = False
    End Select
    FormMDI.mnuQC.Enabled = True
    FormMDI.mnuRZ.Enabled = True
    FormMDI.mnuSPE.Enabled = True
    Me.Enabled = True
  End Sub

  Private Sub mnuEI_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles mnuEI_0.Click, mnuEI_1.Click, mnuEI_2.Click, mnuEI_3.Click, mnuEI_4.Click, mnuEI_5.Click, mnuEI_6.Click
    Dim Index As Integer
    Dim BOOL As Boolean = False
    Index = sender.index
    Try
      If Not IsNothing(Aform) Then
        Aform.close()
        Aform.hide()
        Aform.dispose()
      End If
    Catch
    End Try
    AufbauPar.MethID = MethID
    Call Einst.EinstellForm(Index, MenueParam.UserID, MenueParam.MessgID, MenueParam.MethID, MenueParam.MischID, BOOL, BOOL, Nothing)
    AufbauPar.MethID = -1
    AufbauPar.MethID = MethID
    AufbauPar.UserID = MenueParam.UserID
    AufbauPar.MessgID = MenueParam.MessgID
    AufbauPar.MischID = MenueParam.MischID
    If AufbauPar.ier = 0 Then
      If MenueParam.MethID < 50 Then
        mnuQC.MenuItems(MenueParam.MethID).PerformClick()
      ElseIf MenueParam.MethID < 100 Then
        mnuRZ.MenuItems(MenueParam.MethID - 50).PerformClick()
      Else
        mnuSPE.MenuItems(MenueParam.MethID - 100).PerformClick()
      End If
    End If

  End Sub
  WriteOnly Property SetEinst() As Boolean
    Set(ByVal value As Boolean)
      mnuEI.Enabled = value
    End Set
  End Property

  
  
End Class